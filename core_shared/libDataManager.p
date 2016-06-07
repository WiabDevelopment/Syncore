/*------------------------------------------------------------------------------------------------------------------------------------------
Program : libDataManager.p
Purpose : This is the main Data Library that holds the procedures that manipulate Creates, Updates and Deletes from the temp Db. It is run
          persistently at the start of a character session or on the web at the start of any update programs and then it's internal 
          procedures are referenced later on.
Author  : BG
Date    : 17th May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
12/03/2013 BG  DayMen     Took out the mass definition of dynamic TempTables and buffers - now only creates them as needed. Also added a new
                          pGetCreatedRecord proc which allows us to create a new record in one procedure but them retrieve it before its
                          committed in another procedure. Changed the sequence in which records are committed and also logging changes.
08/08/2013 BG  DayMen     Added a piece to blanktimestamps after CommitAll. Also only now populates the timestamp for Updates if there is no
                          previously timestamp for a create. This means that when we do a create and anupdate on the same record in the same
                          Trans then the create gets committed first with all of the other creates (because we may need a record to have 
                          been created already to fulfill a mandatory link to another record) and the update gets committed later.
10/01/2014 CS  CR1021     Added error log removal to pCommitAll if the file size is zero.  This is to prevent scheduled CronEvents from 
                          leaving blank files out in the logs directory.       
12/03/2014 CS  CR1021     Added chrReturnValue and logic to prevent unknown value being appended to the error message in pCommitAll and 
                          blanking out the error message.  Also fixed a debug message where invalid handle was being used.
20/03/2014 MN  CR1049     Adding core. database prefix for Data Migration Tool
26/08/2014 BG  Ghd        Took the NO-ERROR off the BUFFER-VALIDATE method, doesn't capture errors properly.
10/11/2015 CS  CanonTlb   Added locking log removal to pKillUpdates if file size is zero and set to logs directory so they can purge.
08/01/2016 CS  NextelBr   Added unique UserTransactionID to allow record tracking from each unique libDataManager call within the same 
                          session.
11/01/2016 CS  NextelBr   Removed UserTransactionID from SessionValue. 
14/01/2016 CS  NextelBr   Changed logic in pKillUpdates to only use TransactionID if it is not SessionVariable.                        
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER chrSessionID AS CHARACTER   NO-UNDO.

/* Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE}
{fncLockingFunctions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncServerFunctions.i}

DEFINE VARIABLE hdlQuery                        AS HANDLE      NO-UNDO.
DEFINE VARIABLE chrWhereClause                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logValidQuery                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hdlTTBuffer                     AS HANDLE      NO-UNDO.
DEFINE VARIABLE logFound                        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE logReleased                     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hdlCloneTableHandle             AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlCloneTableBuffer             AS HANDLE      NO-UNDO.
DEFINE VARIABLE chrShortSessionID               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTableName                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory             AS CHARACTER   NO-UNDO.
DEFINE NEW SHARED VARIABLE intTransactionID     AS INTEGER     NO-UNDO.


/* Link table from real Db Tables and the either Temp or Real (In the temp Db) tables that we using to update them */
DEFINE TEMP-TABLE ttTableClone
   FIELD DbTableName     AS CHARACTER
   FIELD TempTableHandle AS HANDLE
   FIELD BufferHandle    AS HANDLE
   FIELD Created         AS CHARACTER
   FIELD UserTransactionID   AS INTEGER /* this should go on the temp table eventually so will need to be removed here */
   INDEX DbTableName IS UNIQUE DbTableName
   INDEX Created Created.

/* Streams */
DEFINE STREAM sToLogfile.
DEFINE STREAM sLocks.


/* We use this to tokenise JavaScript and CSS files so if they're updated it won't read the old version form the cache */
chrShortSessionID = SUBSTRING(chrSessionID,LENGTH(chrSessionID) - 15,LENGTH(chrSessionID)).

/* Set the UserTransactionID for use in Db Triggers and to track the ttTableClone and Temp Database Tables to make sure we are scoping to this
   transaction only */
intGblTransactionID = NEXT-VALUE(TransactionID,core).
THIS-PROCEDURE:PRIVATE-DATA = STRING(intGblTransactionID).
intTransactionID = intGblTransactionID.

FIND Config NO-LOCK.

/* libCreateTableClone.i creates either a dynamic TempTable for the Table (gun & batch) or a real buffer for Table in temp Db (web)      */
/* and it then creates a static ttTeableClone record to hold a link between the buffer handle for each one and the actual table name     */
/* Always need a ttCloneTable record for SessionVariable - its the only one that must be created at the start - all other on demand only */
chrTableName = "SessionVariable".
{libCreateTableClone.i}

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

/* Don't think this is actually used anywhere */
FUNCTION fReleaseHandle RETURNS HANDLE(INPUT hdlHandleName AS HANDLE,
                                       INPUT chrTableName  AS CHARACTER):
   
   IF VALID-HANDLE(hdlHandleName) THEN
   DO:
      /* This release will fire the Write trigger for the previous record and can sometimes throw errors- we can't trap them */
      hdlHandleName:BUFFER-RELEASE.
      IF hdlHandleName:TABLE <> chrTableName THEN 
      DO:
         DELETE OBJECT hdlHandleName NO-ERROR.
         CREATE BUFFER hdlHandleName FOR TABLE chrTableName NO-ERROR. 
      END.
   END.
   ELSE 
   DO:
      CREATE BUFFER hdlHandleName FOR TABLE chrTableName NO-ERROR.
   END.
   
   RETURN hdlHandleName.
   
END FUNCTION. /*fReleaseHandle*/


/* Procedures */
PROCEDURE pCreateRecord:
   
   DEFINE INPUT  PARAMETER chrTableName AS CHARACTER.
   DEFINE OUTPUT PARAMETER newRecord    AS newRecord.
   
   FIND FIRST ttTableClone
      WHERE ttTableClone.DbTableName = chrTableName NO-ERROR.
   IF NOT AVAILABLE ttTableClone THEN
   DO:
      {libCreateTableClone.i}
   END.
   
   /* Timestamp on Created records should be the current one as we want them to be committed first */
   ttTableClone.Created = fTimestamp(NOW).

   newRecord = NEW newRecord(INPUT ttTableClone.BufferHandle,
                             INPUT chrSessionID,
                             INPUT ttTableClone.UserTransactionID).

   newRecord:createRecord().

END PROCEDURE.


PROCEDURE pGetCreatedRecord:
   
   DEFINE INPUT  PARAMETER chrTableName AS CHARACTER.
   DEFINE INPUT  PARAMETER intUniqueID  AS INTEGER.
   DEFINE OUTPUT PARAMETER newRecord    AS newRecord.
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = chrTableName NO-ERROR.
   
   newRecord = NEW newRecord(INPUT ttTableClone.BufferHandle,
                             INPUT chrSessionID,
                             INPUT ttTableClone.UserTransactionID).
   
   newRecord:getCreatedRecord(INPUT intUniqueID).
   
END PROCEDURE.


PROCEDURE pCreateCustRecord:
   
   DEFINE INPUT  PARAMETER chrTableName AS CHARACTER.
   DEFINE INPUT  PARAMETER intUniqueID  AS INTEGER.
   DEFINE OUTPUT PARAMETER newRecord    AS newRecord.
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = chrTableName NO-ERROR.

   IF NOT AVAILABLE ttTableClone THEN
   DO:
      {libCreateTableClone.i}
   END.
   
   /* Timestamp on Created records should be the current one as we want them to be committed first */
   ttTableClone.Created = fTimestamp(NOW).
   
   newRecord = NEW newRecord(INPUT ttTableClone.BufferHandle,
                             INPUT chrSessionID,
                             INPUT ttTableClone.UserTransactionID).
   
   newRecord:createCustRecord(INPUT intUniqueID).
   
END PROCEDURE.


PROCEDURE pUpdateRecord:
   
   DEFINE INPUT  PARAMETER chrTableName AS CHARACTER.
   DEFINE INPUT  PARAMETER intUniqueID  AS INTEGER.
   DEFINE OUTPUT PARAMETER updRecord    AS updRecord.

   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = chrTableName NO-ERROR.
   IF NOT AVAILABLE ttTableClone THEN
   DO:
      {libCreateTableClone.i}
   END.
   
   /* Only populate Timestamp if its blank - otherwise there's a previous create for the same Table whose timestamp should take priority */
   /* Timestamp on Updated records should be "9" + the current one as we want them to be committed second */
   IF ttTableClone.Created = "" THEN
      ttTableClone.Created = "9" + fTimestamp(NOW).
   
   updRecord = NEW updRecord(INPUT ttTableClone.BufferHandle,
                             INPUT chrSessionID,
                             INPUT ttTableClone.UserTransactionID).
   
   updRecord:getRecord(INPUT intUniqueID).

END PROCEDURE.


PROCEDURE pPendingUpdate:
   
   DEFINE INPUT  PARAMETER chrTableName  AS CHARACTER NO-UNDO.     
   DEFINE OUTPUT PARAMETER pendingUpdate AS pendingUpdate.  
      
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = chrTableName NO-ERROR.
   
   IF NOT AVAILABLE ttTableClone THEN
   DO:
      {libCreateTableClone.i}
   END.
      
   pendingUpdate = NEW pendingUpdate(INPUT ttTableClone.BufferHandle,
                                     INPUT chrSessionID,
                                     INPUT ttTableClone.UserTransactionID).  
      
END PROCEDURE.


PROCEDURE pDeleteRecord:
   
   DEFINE INPUT  PARAMETER chrTableName AS CHARACTER.
   DEFINE INPUT  PARAMETER intUniqueID  AS INTEGER.
   DEFINE OUTPUT PARAMETER delRecord    AS delRecord.
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = chrTableName NO-ERROR.
   
   IF NOT AVAILABLE ttTableClone THEN
   DO:
      {libCreateTableClone.i}
   END.
   
   /* Only populate Timestamp if its blank - otherwise there's a previous update for the same Table whose timestamp should take priority */
   /* Timestamp on Deleted records should be "99" + the current one as we want them to be committed last */
   IF ttTableClone.Created = "" THEN
      ttTableClone.Created = "99" + fTimestamp(NOW).
   
   delRecord = NEW delRecord(INPUT ttTableClone.BufferHandle,
                             INPUT chrSessionID,
                             INPUT ttTableClone.UserTransactionID).
   
   delRecord:deleteRecord(INPUT intUniqueID).
   
END PROCEDURE.


PROCEDURE pSetSessionRecord:
   
   DEFINE INPUT  PARAMETER chrTableName  AS CHARACTER NO-UNDO.  
   DEFINE INPUT  PARAMETER recRecordID   AS RECID     NO-UNDO.  
   DEFINE OUTPUT PARAMETER sessionRecord AS sessionRecord.  
      
   sessionRecord = NEW sessionRecord(INPUT TEMP-TABLE ttSessionField:DEFAULT-BUFFER-HANDLE,
                                     INPUT chrTableName,
                                     INPUT recRecordID,
                                     INPUT chrSessionID,
                                     INPUT intTransactionID).
      
END PROCEDURE.


PROCEDURE pGetSessionRecord:
   
   DEFINE INPUT  PARAMETER chrTableName  AS CHARACTER NO-UNDO.  
   DEFINE INPUT  PARAMETER recRecordID   AS RECID     NO-UNDO.  
   DEFINE OUTPUT PARAMETER sessionRecord AS sessionRecord.  

   sessionRecord = NEW sessionRecord(INPUT TEMP-TABLE ttSessionField:DEFAULT-BUFFER-HANDLE,
                                     INPUT chrTableName,
                                     INPUT recRecordID,
                                     INPUT chrSessionID,
                                     INPUT intTransactionID).
   
END PROCEDURE.


PROCEDURE pSetSessionValue:
   
   DEFINE INPUT  PARAMETER chrSessionValueName AS CHARACTER NO-UNDO.  
   DEFINE INPUT  PARAMETER intProcessID        AS INTEGER   NO-UNDO.  
   DEFINE OUTPUT PARAMETER sessionValue        AS sessionValue.  
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = "SessionVariable" NO-ERROR.
   
   sessionValue = NEW sessionValue(INPUT ttTableClone.BufferHandle,
                                   INPUT chrSessionID).
   
   sessionValue:setSessionValue(INPUT chrSessionValueName,
                                INPUT intProcessID).
   
END PROCEDURE.


PROCEDURE pGetSessionValue:
   
   DEFINE INPUT  PARAMETER chrSessionValueName AS CHARACTER NO-UNDO.  
   DEFINE OUTPUT PARAMETER sessionValue        AS sessionValue.
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = "SessionVariable" NO-ERROR.
   
   sessionValue = NEW sessionValue(INPUT ttTableClone.BufferHandle,
                                   INPUT chrSessionID).
   
   sessionValue:getSessionValue(INPUT chrSessionValueName).  
   
END PROCEDURE.


/* Procedure to Clear a All sessionValues per Session */
PROCEDURE pClearSessionValues:
   
   DEFINE INPUT  PARAMETER intProcessID AS INTEGER NO-UNDO.  
   DEFINE OUTPUT PARAMETER logSuccess   AS LOGICAL NO-UNDO.  
   
   DEFINE VARIABLE hdlSessionQuery AS HANDLE      NO-UNDO.
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = "SessionVariable" NO-ERROR.
   
   CREATE QUERY hdlSessionQuery.
   hdlSessionQuery:ADD-BUFFER(ttTableClone.BufferHandle).
   
   IF intProcessID <> 0 THEN 
   DO:  
      hdlSessionQuery:QUERY-PREPARE("FOR EACH ssnSessionVariable EXCLUSIVE-LOCK" +  /*idx=SessionIDProcessID*/
                                    "   WHERE ssnSessionVariable.SessionID = '" + chrGblSessionID + "'" + 
                                    "   AND   ssnSessionVariable.ProcessID = " + STRING(intProcessID)).
   END.
   ELSE 
   DO:
      hdlSessionQuery:QUERY-PREPARE("FOR EACH ssnSessionVariable EXCLUSIVE-LOCK" + /*idx=SessionIDProcessID*/
                                    "   WHERE ssnSessionVariable.SessionID = '" + chrGblSessionID + "'").
   END.
   
   /* Open the Query and Delete the records */
   DO TRANSACTION ON ERROR UNDO:  
      
      hdlSessionQuery:QUERY-OPEN().
      hdlSessionQuery:GET-FIRST() NO-ERROR.
      
      REPEAT:
         IF hdlSessionQuery:QUERY-OFF-END THEN LEAVE.
         
         ttTableClone.BufferHandle:BUFFER-DELETE().
         
         hdlSessionQuery:GET-NEXT().
      END.

      /* Clear the Session Record Fields */      
      FOR EACH ttSessionField EXCLUSIVE-LOCK:
         DELETE ttSessionField.
      END.

   END.
   
   /* Clean Up */
   DELETE OBJECT hdlSessionQuery NO-ERROR.
   
END PROCEDURE. /*pClearSessionValues*/


/* Procedure to Clear a Single Parameter per Session */
PROCEDURE pClearSessionValue:
   
   DEFINE INPUT  PARAMETER chrSessionValue AS CHARACTER NO-UNDO.  
   DEFINE OUTPUT PARAMETER logSuccess      AS LOGICAL   NO-UNDO.  
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = "SessionVariable" NO-ERROR.    
   
   DO TRANSACTION ON ERROR UNDO:
      
      ttTableClone.BufferHandle:FIND-FIRST(" WHERE ssnSessionVariable.SessionID = '" + chrGblSessionID + "'" + /*idx=SessionIDVariableName*/
                                           " AND   ssnSessionVariable.VariableName = '" + chrSessionValue + "'", EXCLUSIVE-LOCK) NO-ERROR.
      IF ttTableClone.BufferHandle:AVAILABLE THEN
      DO:
         ttTableClone.BufferHandle:BUFFER-DELETE().
      END.
   END.
   
END PROCEDURE. /*pClearSessionValue*/


/* Procedure to Extract all Current Session Values */
PROCEDURE pExtractSessionValues:
   
   DEFINE OUTPUT PARAMETER chrValues AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE hdlSessionQuery AS HANDLE      NO-UNDO.
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = "SessionVariable" NO-ERROR.
   
   CREATE QUERY hdlSessionQuery.
   hdlSessionQuery:ADD-BUFFER(ttTableClone.BufferHandle).
   
   hdlSessionQuery:QUERY-PREPARE("FOR EACH  ssnSessionVariable NO-LOCK " + /*idx=SessionIDProcessID*/
                                 "    WHERE ssnSessionVariable.SessionID = '" + chrGblSessionID + "'").
   hdlSessionQuery:QUERY-OPEN().
   hdlSessionQuery:GET-FIRST().
   
   REPEAT:
      IF hdlSessionQuery:QUERY-OFF-END THEN 
         LEAVE.
      
      IF chrValues = "" THEN
      DO: 
         chrValues = ttTableClone.BufferHandle:BUFFER-FIELD("VariableName"):BUFFER-VALUE + " = " + 
                        ttTableClone.BufferHandle:BUFFER-FIELD("VariableValue"):BUFFER-VALUE + CHR(10).
      END.
      ELSE 
      DO:
         chrValues = chrValues + ttTableClone.BufferHandle:BUFFER-FIELD("VariableName"):BUFFER-VALUE + " = " + 
                        ttTableClone.BufferHandle:BUFFER-FIELD("VariableValue"):BUFFER-VALUE + CHR(10).
      END.
      hdlSessionQuery:GET-NEXT().
   END.
   
   /* Clean Up */
   DELETE OBJECT hdlSessionQuery NO-ERROR.
   
END PROCEDURE. /*pExtractsessionValues*/


/* Get the ttClone Handle */
PROCEDURE pGetCloneHandle:
   
   DEFINE INPUT PARAMETER  chrTableName     AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER hdlBufferHandle  AS HANDLE      NO-UNDO.
   
   FIND FIRST ttTableClone NO-LOCK
      WHERE ttTableClone.DbTableName = chrTableName NO-ERROR.
   
   IF AVAILABLE ttTableClone THEN
   DO:
      hdlBufferHandle = ttTableClone.BufferHandle.
   END.
   
END PROCEDURE. /*pGetCloneHandle*/


/* Delete all ttClone Handle data */
PROCEDURE pFlushCloneTables:
   
   FOR EACH ttTableClone EXCLUSIVE-LOCK:
      
      ttTableClone.BufferHandle:EMPTY-TEMP-TABLE().

   END.
   
   /* Clear the Session Fields */   
   FOR EACH ttSessionField EXCLUSIVE-LOCK:
      DELETE ttSessionField.
   END.

END PROCEDURE. /*pFlushCloneTables*/


/* Delete all ttClone Handle data */
PROCEDURE pFlushCloneTablesExceptSession:

   FOR EACH ttTableClone EXCLUSIVE-LOCK
      WHERE ttTableClone.DBTableName <> "SessionVariable":
      
      ttTableClone.BufferHandle:EMPTY-TEMP-TABLE().

   END.

END.


/* Main proc which commits all or nothing */
PROCEDURE pCommitAll:
   
   DEFINE INPUT  PARAMETER chrErrorIn   AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER logAllWentOK AS LOGICAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER chrErrorOut  AS CHARACTER   NO-UNDO.
   
   DEFINE VARIABLE chrDbTable           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hdlDbTable           AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlDbField           AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlCallingProc       AS HANDLE      NO-UNDO.
   DEFINE VARIABLE chrCallingProc       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intErrorCount        AS INTEGER     NO-UNDO.
   DEFINE VARIABLE logRecordCreated     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE logRecordUpdated     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE logRecordDeleted     AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrLockErrorOut      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrUpdateErrorOut    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrProcessMode       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intUniqueValue       AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrUpdates           AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intField             AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrExceptList        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrWriteList         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrReason            AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrTablesToSkip      AS CHARACTER   NO-UNDO INITIAL "AdminTable,SessionVariable,WebLock,WebRecordLock".
   DEFINE VARIABLE chrLastTableName     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrLastProcessMode   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrFileLine          AS CHARACTER   NO-UNDO.

   /* Set the log file destination directory  */
   chrNewAgedDirectory = fGetAgedDirectory("../logs/", 2).
   IF chrNewAgedDirectory BEGINS "Error" THEN
      chrLogFileDirectory = "../logs/".
   ELSE
      chrLogFileDirectory = chrNewAgedDirectory.
   
   /* Output default Stream to File to catch Progress Errors that we're not expecting. */
   OUTPUT TO VALUE(chrLogFileDirectory + "Err_" + chrShortSessionID + "_" + STRING(intTransactionID) + ".log").
   
   /* Get the calling Proc */
   hdlCallingProc = THIS-PROCEDURE:INSTANTIATING-PROCEDURE NO-ERROR.
   chrCallingProc = hdlCallingProc:NAME NO-ERROR.
   
   IF logGblDebugging THEN
   DO:
      OUTPUT STREAM sToLogFile TO VALUE(chrLogFileDirectory + "Log_" + chrShortSessionID + "_" + STRING(intTransactionID) + ".log") APPEND.
         fLog("Called from:" + chrCallingProc).
         fLog("Error In:" + chrErrorIn).
   END.
   
   TransBlk:
   DO TRANSACTION ON ERROR UNDO, LEAVE:
      
      IF chrErrorIn <> "" THEN
         LEAVE TransBlk.
      
      UpdateBlk:
      /* This sequence should do Creates first, then Updates and then Deletes */
      FOR EACH ttTableClone BY ttTableClone.Created:
         
         IF VALID-HANDLE(ttTableClone.TempTableHandle) AND 
            NOT ttTableClone.TempTableHandle:HAS-RECORDS THEN
            NEXT UpdateBlk.
         
         IF LOOKUP(ttTableClone.DbTableName,chrTablesToSkip) > 0 THEN 
            NEXT UpdateBlk.
         
         hdlTTBuffer = ttTableClone.BufferHandle NO-ERROR.
         
         IF logGblDebugging AND 
            NOT VALID-HANDLE(hdlTTBuffer) THEN
            fLog("Invalid Table Handle: hdlTTBuffer for Table:" + ttTableClone.DbTableName).
         
         chrDbTable = ttTableClone.DbTableName.
         
         /* Release any previous buffer */
         IF VALID-HANDLE(hdlDbTable) THEN
         DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
            
            chrLastTableName = hdlDbTable:TABLE.
            
            /* This release will fire the Write trigger for the previous record and can sometimes throw errors */
            logReleased = hdlDbTable:BUFFER-RELEASE NO-ERROR.
            
            IF logReleased <> TRUE OR ERROR-STATUS:ERROR THEN
            DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
               chrErrorOut = chrErrorOut + "Unable to create/update " + chrLastTableName + " record.".
               DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                  chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
               END.
               UNDO TransBlk, LEAVE TransBlk.
            END.
            
            IF hdlDbTable:TABLE <> chrDbTable THEN 
            DO:
               DELETE OBJECT hdlDbTable NO-ERROR.
               CREATE BUFFER hdlDbTable FOR TABLE ttTableClone.DbTableName NO-ERROR.
            END.
         END.
         ELSE 
         DO:
            CREATE BUFFER hdlDbTable FOR TABLE ttTableClone.DbTableName NO-ERROR.
         END.
         
         IF ERROR-STATUS:ERROR THEN
         DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
            chrErrorOut = chrErrorOut + "Unable to create/update " + chrLastTableName + " record.".
            DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
            END.
            UNDO TransBlk, LEAVE TransBlk.
         END.
         
         CREATE QUERY hdlQuery.
         hdlQuery:SET-BUFFERS(hdlTTBuffer).  /*idx=CommittedSessionID on all tables in the temp Db*/
         chrWhereClause = "FOR EACH ssn" + ttTableClone.DbTableName 
                             + " WHERE Committed = '' AND SessionID = '" 
                             + chrSessionID + "'" 
                             + " AND UserTransactionID = " + STRING(THIS-PROCEDURE:PRIVATE-DATA).
                             
         logValidQuery = hdlQuery:QUERY-PREPARE(chrWhereClause).
         IF NOT logValidQuery THEN
         DO:
            chrErrorOut = chrErrorOut + "Bad Query:" + chrWhereClause + ". Undoing all Updates.".
            UNDO TransBlk, LEAVE TransBlk.
         END.
         
         logValidQuery = hdlQuery:QUERY-OPEN().
         IF NOT logValidQuery THEN
         DO:
            chrErrorOut = chrErrorOut + "Nothing in Query:" + chrWhereClause + ". Undoing all Updates.".
            UNDO TransBlk, LEAVE TransBlk.
         END.
         
         hdlQuery:GET-FIRST() NO-ERROR.
         
         REPEAT ON ERROR UNDO TransBlk, LEAVE TransBlk:
            
            IF hdlQuery:QUERY-OFF-END THEN 
               LEAVE.
            
            chrProcessMode = hdlTTBuffer:BUFFER-FIELD("ProcessMode"):BUFFER-VALUE.
            
            CASE chrProcessMode:
               
               /* Creates */
               WHEN "Create" THEN
               DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                  
                  IF logGblDebugging THEN
                     fLog("Finding ttTableClone EXCLUSIVE for new: " + chrDbTable + " record.").
                  
                  logFound = hdlTTBuffer:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT) NO-ERROR.
                  
                  IF logFound <> TRUE THEN 
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     chrErrorOut = "Create record for Table " + chrDbTable + " is locked. Undoing all Updates.".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  IF logGblDebugging THEN
                     fLog("Create TT Record found EXCLUSIVE-LOCK.").
                  
                  logRecordCreated = hdlDbTable:BUFFER-CREATE() NO-ERROR.
                  
                  IF logRecordCreated <> TRUE OR ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
                     END.
                     chrErrorOut = chrErrorOut + "Unable to Create new " + hdlDbTable:NAME + " record.".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  IF logGblDebugging THEN
                     fLog("New " + chrDbTable + " record has been Created.").
                  
                  chrExceptList = "".
                  
                  DO intField = 1 TO hdlDbTable:NUM-FIELDS ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     
                     IF hdlDbTable:BUFFER-FIELD(intField):CAN-WRITE = FALSE THEN
                        chrExceptList = chrExceptList + hdlDbTable:BUFFER-FIELD(intField):NAME + ",".
                  END.
                  
                  chrExceptList = RIGHT-TRIM(chrExceptList,",").
                     
                  IF logGblDebugging THEN
                     fLog("Excepting fields:" + chrExceptList).
                  
                  logRecordCreated = hdlDbTable:BUFFER-COPY(hdlTTBuffer, chrExceptList) NO-ERROR.
                  
                  IF ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     chrErrorOut = "Unable to Create new " + hdlDbTable:NAME + " record. ".
                     DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
                     END.
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  ELSE
                  DO: 
                     /* This validates that the Create has gone successfully, we want to force it to validate here as it might not fire */
                     /* until the TRANS ends. If it fails here it will exit the entire Trans block, we can't trap the errors using      */
                     /* NO-ERROR (just doesn't work) but can read them from the log file later */
                     hdlDbTable:BUFFER-VALIDATE() /* Please do NOT add NO-ERROR here */.
                     
                  END. /* ELSE */
                  
                  /* Close Update records and set the Outcome */
                  hdlTTBuffer:BUFFER-FIELD("Committed"):BUFFER-VALUE = fTimestamp(NOW) NO-ERROR.
                  hdlTTBuffer:BUFFER-FIELD("Outcome"):BUFFER-VALUE   = "RecordCreated" NO-ERROR.
                  
                  IF ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     chrErrorOut = "Unable to Create new " + hdlDbTable:NAME + " record. ".
                     DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
                     END.
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  /* This will force the write trigger to fire and should cause the record to be written */
                  hdlDbTable:BUFFER-RELEASE().
                  
                  IF logGblDebugging THEN
                     fLog("New " + chrDbTable + " record has been Written.").
                  
               END. /*WHEN "Create" THEN*/
               
               
               /* Updates */
               WHEN "Update" THEN
               UpdateLoop:
               DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                  
                  intUniqueValue = hdlTTBuffer:BUFFER-FIELD(chrDbTable + "ID"):BUFFER-VALUE.
                  
                  IF logGblDebugging THEN
                     fLog("Finding ttTableClone EXCLUSIVE for: " + chrDbTable + " ID:" + STRING(intUniqueValue)).
                  
                  logFound = hdlTTBuffer:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT) NO-ERROR.
                  
                  IF logFound <> TRUE THEN 
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     chrErrorOut = "Update record for Table " + chrDbTable + " is locked. Undoing all Updates.".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  IF logGblDebugging THEN
                     fLog("Update TT Record found EXCLUSIVE-LOCK.").
                  
                  IF hdlTTBuffer:BUFFER-FIELD("Committed"):BUFFER-VALUE <> "" THEN
                     hdlQuery:GET-NEXT().
                  
                  IF logGblDebugging THEN
                     fLog("Update TT Record still uncommitted.").
                  
                  /* Release any previous buffer */
                  IF VALID-HANDLE(hdlDbTable) THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     
                     /* This release will fire the Write trigger for the previous record and can sometimes throw errors */
                     logReleased = hdlDbTable:BUFFER-RELEASE NO-ERROR.
                     
                     IF hdlDbTable:TABLE <> chrDbTable THEN 
                     DO:
                        DELETE OBJECT hdlDbTable NO-ERROR.
                        CREATE BUFFER hdlDbTable FOR TABLE chrDbTable NO-ERROR. 
                     END.
                  END.
                  ELSE 
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     CREATE BUFFER hdlDbTable FOR TABLE chrDbTable NO-ERROR.
                  END.
                  
                  IF logGblDebugging THEN
                     fLog("Handle released and buffer for Db Table created.").
                  
                  /* Could be a typo in the incoming Table Name - this will pick that up */
                  IF logReleased <> TRUE OR ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
                     END.
                     chrErrorOut = chrErrorOut + "Unable to Create new " + chrDbTable + " record.".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                 
                  IF logGblDebugging THEN
                     fLog("Finding real Db record now for Table:" + chrDbTable + " ID:" + STRING(intUniqueValue)).
                  
                  /* Now Find real Db Record EXCLUSIVE-LOCK */
                  logFound = hdlDbTable:FIND-FIRST("WHERE " + chrDbTable + "ID = " + STRING(intUniqueValue),
                                                   EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
                  IF logFound <> TRUE OR ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     
                     IF hdlDbTable:LOCKED THEN
                     DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                        
                        logFound = hdlDbTable:FIND-FIRST("WHERE " + chrDbTable + "ID = " + STRING(intUniqueValue),NO-LOCK,NO-WAIT) NO-ERROR.
                        IF logFound THEN
                           chrErrorOut = fGetLockingInfo(INPUT INTEGER(hdlDbTable:RECID),
                                                         INPUT chrDbTable,
                                                         INPUT STRING(intUniqueValue)) + " - please try later.".
                        ELSE
                           chrErrorOut = chrDbTable + " Record for Update:" + STRING(intUniqueValue) + " is Locked by another User.".
                        
                        UNDO TransBlk, LEAVE TransBlk.
                     END. /*IF hdlDbTable:LOCKED THEN*/
                     
                     /*Not found*/
                     chrErrorOut = chrDbTable + " Record for Update:" + STRING(intUniqueValue) + " cannot be Found.".
                     UNDO TransBlk, LEAVE TransBlk.
                     
                  END. /* IF NOT logFound THEN */
                  
                  IF logGblDebugging THEN
                     fLog("Found record EXCLUSIVE for Table:" + chrDbTable + " ID:" + STRING(intUniqueValue)).
                  
                  /* Only compare the versions if its not a customer specific table */
                  IF hdlDbTable:DBNAME = "cust" AND 
                     hdlDbTable:NAME BEGINS "Cust" THEN
                  DO:
                     IF logGblDebugging THEN
                        fLog("Skipping version check for Customer specific table:" + hdlDbTable:NAME).
                  END.
                  ELSE
                  DO:
                     /* Compare the Versions */
                     ASSIGN hdlDbField = hdlDbTable:BUFFER-FIELD["VersionID"] NO-ERROR.
                     IF hdlDbField:BUFFER-VALUE <> hdlTTBuffer:BUFFER-FIELD("VersionID"):BUFFER-VALUE THEN
                     DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                        chrErrorOut = chrDbTable + " Record of ID:" + STRING(intUniqueValue) + " has changed since Update began." + 
                                                   " Undoing All.".
                        UNDO TransBlk, LEAVE TransBlk.
                     END.
                     
                     IF logGblDebugging THEN
                        fLog("Versions match Ok @ " + hdlDbField:BUFFER-VALUE + " - Writing record now.").
                  END.
                  
                  ASSIGN chrExceptList = ""
                         chrWriteList  = "".
                  
                  IF NOT hdlDbTable:BUFFER-COMPARE(hdlTTBuffer) THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     
                     DO intField = 1 TO hdlDbTable:NUM-FIELDS ON ERROR UNDO TransBlk, LEAVE TransBlk:
                        
                        IF hdlDbTable:BUFFER-FIELD(intField):BUFFER-VALUE = hdlTTBuffer:BUFFER-FIELD(intField):BUFFER-VALUE THEN
                           chrExceptList = chrExceptList + hdlDbTable:BUFFER-FIELD(intField):NAME + ",".
                        ELSE
                           chrWriteList = chrWriteList + hdlDbTable:BUFFER-FIELD(intField):NAME + ",".
                     END.
                     
                     ASSIGN chrExceptList = RIGHT-TRIM(chrExceptList,",")
                            chrWriteList  = RIGHT-TRIM(chrWriteList,",").
                     
                     IF logGblDebugging THEN
                        fLog("Writing fields:" + chrWriteList + ". Not Writing fields:" + chrExceptList).
                     
                     hdlDbTable:BUFFER-COPY(hdlTTBuffer, chrExceptList) NO-ERROR.
                     
                     /* This validates that the Update has gone successfully, we want to force it to validate there as it might not fire */
                     /* until the TRANS ends. If it fails here it t will exit the entire Trans block, we can't trap the errors but can    */
                     /* read them from the log file later */
                     hdlDbTable:BUFFER-VALIDATE().
                     
                  END. /*IF NOT hdlDbTable:BUFFER-COMPARE(hdlTTBuffer) THEN*/
                  
                  /* Could be a typo in the incoming Table Name - this will pick that up */
                  IF ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
                     END.
                     chrErrorOut = chrErrorOut + "Unable to Update " + chrDbTable + " record.".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  /* Close Update records and set the Outcome */
                  hdlTTBuffer:BUFFER-FIELD("Committed"):BUFFER-VALUE = fTimestamp(NOW) NO-ERROR.
                  hdlTTBuffer:BUFFER-FIELD("Outcome"):BUFFER-VALUE   = "RecordUpdated" NO-ERROR.
                  
                  IF logGblDebugging THEN
                     fLog("Record has been Updated.").
                  
               END. /*WHEN "Update" THEN*/
               
               
               /* Deletes */
               WHEN "Delete" THEN
               DO:
                  ASSIGN chrDbTable     = ttTableClone.DbTableName
                         intUniqueValue = hdlTTBuffer:BUFFER-FIELD(chrDbTable + "ID"):BUFFER-VALUE.
                  
                  IF logGblDebugging THEN
                     fLog("Find Record:" + chrDbTable + " for ID:" + STRING(intUniqueValue)).
                  
                  logFound = hdlTTBuffer:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT) NO-ERROR.
                  
                  IF logFound <> TRUE THEN 
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     chrErrorOut = "Update record for Table " + chrDbTable + " is locked. Undoing all Updates.".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  IF logGblDebugging THEN
                     fLog("Delete TT Record found EXCLUSIVE-LOCK.").
                  
                  IF hdlTTBuffer:BUFFER-FIELD("Committed"):BUFFER-VALUE <> "" THEN
                     hdlQuery:GET-NEXT().
                  
                  IF logGblDebugging THEN
                     fLog("Delete TT Record still Uncommitted.").
                  
                  /* Release any previous buffer */
                  IF VALID-HANDLE(hdlDbTable) THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     
                     /* This release will fire the Write trigger for the previous record and can sometimes throw errors */
                     /* We can't trap them but they'll be written to the log file which we can read from later          */
                     hdlDbTable:BUFFER-RELEASE.
                     IF hdlDbTable:TABLE <> chrDbTable THEN 
                     DO:
                        DELETE OBJECT hdlDbTable NO-ERROR.
                        CREATE BUFFER hdlDbTable FOR TABLE chrDbTable NO-ERROR. 
                     END.
                  END.
                  ELSE 
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     CREATE BUFFER hdlDbTable FOR TABLE chrDbTable NO-ERROR.
                  END.
                  
                  IF logGblDebugging THEN
                     fLog("Handle released and Db Table buffer created.").
                  
                  /* Could be a typo in the incoming Table Name - this will pick that up */
                  IF ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
                     END.
                     chrErrorOut = chrErrorOut + "Unable to Create new " + chrDbTable + " record.".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  IF logGblDebugging THEN
                     fLog("Finding real Db record now for Table:" + chrDbTable + " ID:" + STRING(intUniqueValue)).
                  
                  /* Now Find real Db Record EXCLUSIVE-LOCK */
                  logFound = hdlDbTable:FIND-FIRST("WHERE " + chrDbTable + "ID = " + STRING(intUniqueValue),
                                                   EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
                  IF logFound <> TRUE OR ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     
                     IF hdlDbTable:LOCKED THEN
                     DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                        
                        logFound = hdlDbTable:FIND-FIRST("WHERE " + chrDbTable + "ID = " + STRING(intUniqueValue),NO-LOCK,NO-WAIT) NO-ERROR.
                        IF logFound THEN
                           chrErrorOut = fGetLockingInfo(INPUT INTEGER(hdlDbTable:RECID),
                                                         INPUT chrDbTable,
                                                         INPUT STRING(intUniqueValue)) + " - please try later.".
                        ELSE
                           chrErrorOut = chrDbTable + " Record for Update:" + STRING(intUniqueValue) + " is Locked by another User.".
                        
                        UNDO TransBlk, LEAVE TransBlk.
                     END. /*IF hdlDbTable:LOCKED THEN*/
                     
                     /*Not found*/
                     chrErrorOut = chrDbTable + " Record for Update:" + STRING(intUniqueValue) + " cannot be Found.".
                     UNDO TransBlk, LEAVE TransBlk.
                     
                  END. /* IF NOT logFound THEN */
                  
                  IF logGblDebugging THEN
                     fLog("Found record EXCLUSIVE for Table:" + chrDbTable + " ID:" + STRING(intUniqueValue)).
                  
                  /* Only compare the versions if its not a customer specific table */
                  IF hdlDbTable:DBNAME = "cust" AND 
                     hdlDbTable:NAME BEGINS "Cust" THEN
                  DO:
                     IF logGblDebugging THEN
                        fLog("Skipping version check for Customer specific table:" + hdlDbTable:NAME).
                  END.
                  ELSE
                  DO:
                     /* Compare the Versions */
                     ASSIGN hdlDbField = hdlDbTable:BUFFER-FIELD["VersionID"] NO-ERROR.
                     IF hdlDbField:BUFFER-VALUE <> hdlTTBuffer:BUFFER-FIELD("VersionID"):BUFFER-VALUE THEN
                     DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                        chrErrorOut = chrDbTable + " Record of ID:" + STRING(intUniqueValue) + " has changed since Delete began." + 
                                                   " Undoing All.".
                        UNDO TransBlk, LEAVE TransBlk.
                     END.
                     
                     IF logGblDebugging THEN
                        fLog("Versions match Ok @ " + hdlDbField:BUFFER-VALUE + " - Deleting record now.").
                  END.
                  
                  /* Actual Delete takes place here - unlike BUFFER-VALIDATE() errors are suppressed here */
                  logRecordDeleted = hdlDbTable:BUFFER-DELETE() NO-ERROR.
                  
                  IF logRecordDeleted <> TRUE OR 
                     ERROR-STATUS:ERROR THEN
                  DO ON ERROR UNDO TransBlk, LEAVE TransBlk:
                     DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        chrErrorOut = chrErrorOut + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
                     END.
                     ASSIGN chrErrorOut = chrErrorOut + " Unable to Delete " + hdlDbTable:NAME + " record. ".
                     UNDO TransBlk, LEAVE TransBlk.
                  END.
                  
                  /* Close Update records and set the Outcome */
                  hdlTTBuffer:BUFFER-FIELD("Committed"):BUFFER-VALUE = fTimestamp(NOW) NO-ERROR.
                  hdlTTBuffer:BUFFER-FIELD("Outcome"):BUFFER-VALUE   = "RecordDeleted" NO-ERROR.
                  
                  IF logGblDebugging THEN
                     fLog("Record has been Deleted.").
                  
               END. /*WHEN "Delete" THEN*/
              
            END CASE. /*CASE chrProcessMode:*/
            
            hdlQuery:GET-NEXT().
         
         END. /*REPEAT:*/
         
         /* Clean Up */
         DELETE OBJECT hdlQuery NO-ERROR.

         IF VALID-HANDLE(ttTableClone.TempTableHandle) AND ttTableClone.TempTableHandle:HAS-RECORDS THEN
         DO:
            IF logGblDebugging THEN
               fLog("Deleting TT records for Table:" + ttTableClone.TempTableHandle:NAME).
            
            ttTableClone.TempTableHandle:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE.
         END.
         
         /* Blank the Created timestamp here - we'll need to reset this if the tt record is to be used in subsequent creates/updates */
         ttTableClone.Created = "".
         
      END. /*FOR EACH ttTableClone BY ttTableClone.Created:*/
      
      /* Only set this flag when we know all is done without Error */
      logAllWentOK = TRUE.   
      
   END. /*TransBlk: DO TRANSACTION ON ERROR UNDO ON ERROR UNDO TransBlk, LEAVE TransBlk:*/
   
   IF logGblDebugging THEN
      fLog("Trans Successful?:" + STRING(logAllWentOk,"Yes/No") + " " + chrErrorOut).
   
   /* We can't trap the Progress errors for BUFFER-VALIDATE() method so they'll be written to this file by default */
   /* Extract them now so we can return to the user on the front end - usually unique index errors                 */
   IF logAllWentOk = FALSE AND chrErrorOut = "" THEN
   DO:
      /* Close output to Error log first */
      OUTPUT CLOSE.
      /* Import from Error log */
      INPUT FROM VALUE(chrLogFileDirectory + "Err_" + chrShortSessionID + "_" + STRING(intTransactionID) + ".log").
         
         REPEAT:
            IMPORT UNFORMATTED chrFileLine.
            /* Write errors to normal error variable */
            chrErrorOut = chrErrorOut + chrFileLine.
            IF logGblDebugging THEN
               fLog("Progress Error:" + chrFileLine).
         END.
      INPUT CLOSE.
   END. /*IF chrErrorOut = "" AND logAllWentOk = FALSE THEN*/
   
   IF logGblDebugging THEN
   DO:
      fLog("After reading Progress Errors:" + chrErrorOut).
      fLog("Transaction Active?:" + STRING(TRANSACTION,"Yes/No")).
   END. /*IF logGblDebugging THEN*/  
   
   /* Messages from Db Triggers will be contained in the RETURN-VALUE here */
   IF logAllWentOK = FALSE AND 
      RETURN-VALUE <> "" AND 
      RETURN-VALUE <> ? THEN
      chrErrorOut = chrErrorOut + RIGHT-TRIM(RETURN-VALUE,".") + ". ".
   
   IF logGblDebugging THEN
      fLog("After Triggers:" + chrErrorOut).
   
   /* Release locks & delete object*/
   hdlDbTable:BUFFER-RELEASE  NO-ERROR.
   hdlTTBuffer:BUFFER-RELEASE NO-ERROR.
   DELETE OBJECT hdlDbTable   NO-ERROR.
   
   IF logAllWentOK THEN 
      chrReason = "ChangesCommitted".
   ELSE
      chrReason = (IF chrErrorIn = "" THEN "ErrorIn:CommitAll" ELSE "ErrorIn:" + chrCallingProc).
   
   IF SESSION:CLIENT-TYPE = "WEBSPEED" THEN
   DO:
      IF logGblDebugging THEN
         fLog("Killing Locks:" + chrReason).
      
      /* External proc to Kill web record locks */
      RUN dbKillLocks.p (INPUT  chrSessionID,
                         INPUT  chrReason,
                         OUTPUT chrLockErrorOut).
      
      IF logGblDebugging THEN
         fLog("After Kill Locks:" + chrLockErrorOut).
      
   END. /*IF SESSION:CLIENT-TYPE = "WEBSPEED" THEN*/
   
   IF logAllWentOK <> TRUE THEN
   DO:
      /* Close off all Update Records now whether committed or not */
      RUN pKillUpdates (INPUT  chrReason,
                        INPUT  logGblDebugging,
                        OUTPUT chrUpdateErrorOut).
      
      IF logGblDebugging THEN
         fLog("After Kill Updates:" + chrUpdateErrorOut).
      
   END. /*IF logAllWentOK <> TRUE THEN*/
   
   IF chrErrorIn <> "" THEN
      chrErrorOut = chrErrorIn + chrErrorOut.
   
   IF logGblDebugging THEN
      fLog("Add ErrorIn:" + chrErrorOut).
   
   IF chrErrorOut = "" AND 
      chrLockErrorOut <> "" THEN
      chrErrorOut = chrLockErrorOut.
      
   IF chrErrorOut = "" AND 
      chrUpdateErrorOut <> "" THEN
      chrErrorOut = chrUpdateErrorOut.
   
   /* Strip spurious characters */
   chrErrorOut = REPLACE(chrErrorOut,"*","").
   chrErrorOut = REPLACE(chrErrorOut,"..",".").
   chrErrorOut = REPLACE(chrErrorOut,"'",'').   /* Javascript refresh cannot handle single quotes      */
   chrErrorOut = REPLACE(chrErrorOut,'"','').   /* systemAlert message box cannot handle double quotes */
   chrErrorOut = LEFT-TRIM(chrErrorOut,".").
   
   IF logGblDebugging THEN
   DO:
      fLog("After Trims:" + chrErrorOut).
      fLog("End pCommitAll.").
      OUTPUT STREAM sToLogFile CLOSE.
   END. /*IF logGblDebugging THEN*/
   
   /* Close the default stream */
   OUTPUT CLOSE.
   
   FILE-INFO:FILE-NAME = chrLogFileDirectory + "Err_" + chrShortSessionID + "_" + STRING(intTransactionID) + ".log".
   
   IF FILE-INFO:FILE-SIZE = 0 THEN
      fDeleteFile(chrLogFileDirectory + "Err_" + chrShortSessionID + "_" + STRING(intTransactionID) + ".log") NO-ERROR.
   
   /* Set the UserTransactionID for use in Db Triggers */
    intGblTransactionID = 0.
   
END PROCEDURE. /* pCommitAll */



PROCEDURE pKillUpdates:
   
   DEFINE INPUT  PARAMETER chrReason       AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER logGblDebugging AS LOGICAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER chrError        AS CHARACTER   NO-UNDO.
   
   /* Set the log file destination directory  */
   chrNewAgedDirectory = fGetAgedDirectory("../logs/", 2).
   IF chrNewAgedDirectory BEGINS "Error" THEN
      chrLogFileDirectory = "../logs/".
   ELSE
      chrLogFileDirectory = chrNewAgedDirectory.
   
   IF logGblDebugging THEN
      OUTPUT STREAM sLocks TO VALUE(chrLogFileDirectory + "Upd_" + chrShortSessionID + "_" + STRING(intTransactionID)+ ".log").
   
   UpdateBlk:
   DO TRANSACTION:
      
      TableLoop:
      FOR EACH ttTableClone /*BY ttTableClone.DbTableName DESC*/:
         
         hdlTTBuffer = ttTableClone.BufferHandle.
         
         IF logGblDebugging AND NOT VALID-HANDLE(hdlTTBuffer) THEN
            PUT STREAM sLocks UNFORMATTED "Buffer: " + hdlTTBuffer:TABLE SKIP.
         
         CREATE QUERY hdlQuery.
         hdlQuery:SET-BUFFERS(hdlTTBuffer).  /*idx=CommittedSessionID on all tables in the temp Db*/
         chrWhereClause = "FOR EACH ssn" + ttTableClone.DbTableName 
                             + " WHERE Committed = '' AND SessionID = '" 
                             + chrSessionID + "'".
         
         IF ttTableClone.DbTableName <> "SessionVariable" THEN
            chrWhereClause = chrWhereClause + " AND UserTransactionID = " + STRING(THIS-PROCEDURE:PRIVATE-DATA).
                             
         logValidQuery = hdlQuery:QUERY-PREPARE(chrWhereClause).
         IF NOT logValidQuery THEN
         DO:
            chrError = chrError + "Bad Query:" + chrWhereClause + ". Undoing all Update Kills.".
            LEAVE UpdateBlk.
         END.
         
         hdlQuery:QUERY-OPEN().
         hdlQuery:GET-FIRST(NO-LOCK) NO-ERROR.
         
         REPEAT:
            
            IF hdlQuery:QUERY-OFF-END THEN 
               LEAVE.
            
            /* Must be an active TRANS for this to work, otherwise returns false */
            logFound = hdlTTBuffer:FIND-CURRENT(EXCLUSIVE-LOCK, NO-WAIT).
            
            IF logFound = TRUE THEN 
            DO:
               /* Close Update records and set the Outcome */
               hdlTTBuffer:BUFFER-FIELD("Committed"):BUFFER-VALUE = fTimestamp(NOW) NO-ERROR.
               hdlTTBuffer:BUFFER-FIELD("Outcome"):BUFFER-VALUE   = chrReason NO-ERROR.
            END.
            ELSE
            DO:
               chrError = chrError + "Update record " + hdlTTBuffer:NAME + " is locked. Could not complete. "
                             + "Session has been corrupted, logging you out now.".
               UNDO UpdateBlk, LEAVE UpdateBlk.
            END.
            
            hdlTTBuffer:BUFFER-RELEASE NO-ERROR.
            hdlQuery:GET-NEXT().
         END. /*REPEAT:*/
          
      END. /*FOR EACH ttTableClone:*/

      /* Clean Up */
      DELETE OBJECT hdlQuery NO-ERROR.
      
   END. /*UpdateBlk: DO TRANSACTION:*/
   
   /* If Debug is enabled close the stream and if the log is empty delete the file */
   IF logGblDebugging THEN
   DO:
      OUTPUT STREAM sLocks CLOSE.
   
      FILE-INFO:FILE-NAME = chrLogFileDirectory + "Upd_" + chrShortSessionID + "_" + STRING(intTransactionID) + ".log".

      IF FILE-INFO:FILE-SIZE = 0 THEN
         fDeleteFile(chrLogFileDirectory + "Upd_" + chrShortSessionID + "_" + STRING(intTransactionID) + ".log") NO-ERROR.
   END. /*IF logGblDebugging THEN*/
   
END PROCEDURE. /*pKillUpdates*/
