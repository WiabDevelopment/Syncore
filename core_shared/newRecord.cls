/*------------------------------------------------------------------------------------------------------------------------------------------
Program : updRecord.cls
Purpose : New Class which is used in conjuntion with the Data Library to Create all records except for Batch jobs.
Author  : BG
Date    : 27th Jan 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
30/05/2014 BG  WiaB       Changed the temp Db Buffer finds to always use SHARE-LOCK as opposed to using the conLockTypeConst constant. This
                          should only be used for the real Db Record finds according to configuration. Also added Locking info.
27/08/2014 LT             Changed bufferCopy method that takes 2 parameter.  Previous code is commented out.
08/01/2016 CS  NextelBr   Added unique UserTransactionID to allow record tracking from each unique libDataManager call within the same 
                          session.                          
------------------------------------------------------------------------------------------------------------------------------------------*/

CLASS newRecord:
   
   /***** Vars *****/
   DEFINE PROTECTED VARIABLE hdlCreateRecord      AS HANDLE      NO-UNDO.
   DEFINE PROTECTED VARIABLE chrErrors            AS CHARACTER   NO-UNDO.
   DEFINE PROTECTED VARIABLE chrCurrentDb         AS CHARACTER   NO-UNDO INITIAL "Core".
   DEFINE PROTECTED VARIABLE intError             AS INTEGER     NO-UNDO.
   DEFINE PROTECTED VARIABLE conLockTypeConst     AS INTEGER     NO-UNDO.
   
   
   /***** Properties *****/
   DEFINE PUBLIC PROPERTY NewRecordTableName       AS CHARACTER   NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY NewRecordUniqueID        AS INTEGER     NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY CurrentSessionID         AS CHARACTER   NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY CurrentUserTransactionID AS INTEGER     NO-UNDO
       GET.
       PROTECTED SET.
   
   /***** Streams *****/
   DEFINE STREAM sToLockLog.
   DEFINE STREAM sFromTelnet.
   
   
   /***** Buffers *****/
   DEFINE BUFFER auditGateUser FOR gate.GateUser.
   
   
   /***** Contructor *****/
   CONSTRUCTOR newRecord(INPUT hdlCreateTableBuffer AS HANDLE,
                         INPUT chrUserSessionID     AS CHARACTER,
                         INPUT intCurrentUserTransactionID AS INTEGER):
      
      /* Session and Table get set here and can't be changed for this Class instance */
      ASSIGN hdlCreateRecord          = hdlCreateTableBuffer
             CurrentSessionID         = chrUserSessionID
             CurrentUserTransactionID = intCurrentUserTransactionID
             NewRecordTableName       = SUBSTRING(hdlCreateRecord:NAME,4). /* Temp Table name will have a ssn prefix - strip this off */
      
   END CONSTRUCTOR.
   

   /***** Private Internal Methods *****/
   METHOD PRIVATE CHARACTER displayDate&Time (INPUT chrTimeStamp  AS CHARACTER,
                                              INPUT chrDateFormat AS CHARACTER):
      
      DEFINE VARIABLE intCharacter    AS INTEGER     NO-UNDO.
      DEFINE VARIABLE chrInputFormat  AS CHARACTER   NO-UNDO CASE-SENSITIVE. /* Need case sensitivity for the 'm' month vs 'M' minutes */
      DEFINE VARIABLE chrOutputString AS CHARACTER   NO-UNDO.
      
      IF chrDateFormat = "" THEN
         chrDateFormat = "dmy".
      
      chrInputFormat = chrDateFormat.
      
      IF chrTimeStamp <> "" THEN
      DO intCharacter = 1 TO LENGTH(chrInputFormat):
         
         CASE(SUBSTRING(chrInputFormat,intCharacter,1)):
            
            WHEN ("d") THEN
               chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,7,2).
            WHEN ("m") THEN
               chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,5,2).
            WHEN ("y") THEN
               chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,1,4).
            WHEN ("H") THEN
               chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,9,2).
            WHEN ("M") THEN
               chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,11,2).
            WHEN ("S") THEN
               chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,13,2).
            OTHERWISE
               chrOutputString = chrOutputString + SUBSTRING(chrInputFormat,intCharacter,1).
            
         END CASE.
         
      END. /*DO intCharacter = 1 TO LENGTH(chrInputFormat):*/
      
      RETURN chrOutputString.
      
   END METHOD. /*displayDate&Time*/   
   
   
   METHOD PRIVATE CHARACTER getLockTimestamp (INPUT datDateToConvert AS DATETIME-TZ):
      
      DEFINE VARIABLE chrTimestamp     AS CHARACTER     NO-UNDO.
      DEFINE VARIABLE chrDateToConvert AS CHARACTER     NO-UNDO.
      
      ASSIGN chrDateToConvert = STRING(datDateToConvert).
      
      ASSIGN chrTimestamp = STRING(SUBSTRING(chrDateToConvert, 7,4),"9999") +
                            STRING(SUBSTRING(chrDateToConvert, 4,2),  "99") +
                            STRING(SUBSTRING(chrDateToConvert, 1,2),  "99") + 
                            STRING(SUBSTRING(chrDateToConvert,12,2),  "99") +
                            STRING(SUBSTRING(chrDateToConvert,15,2),  "99") +
                            STRING(SUBSTRING(chrDateToConvert,18,2),  "99") +
                            STRING(SUBSTRING(chrDateToConvert,21,3), "999").
      
      RETURN chrTimeStamp.
      
   END METHOD. /*getLockTimestamp*/
   
   
   /* This method reads the Lock table sin the Db and using a Telnet grep tries to return the User Info relating to the Lock */
   METHOD PRIVATE CHARACTER getLockingInfo (INPUT intRecID      AS INTEGER,    /*Recid of locked record -> got by refinding record no-lock*/
                                            INPUT chrTableName  AS CHARACTER,  /*Table Name*/
                                            INPUT chrFieldValue AS CHARACTER): /*chrFieldValue is Optional field to display*/
      
      DEFINE VARIABLE intTableId        AS INTEGER     NO-UNDO.
      DEFINE VARIABLE chrLockInfo       AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE chrFilePath       AS CHARACTER   NO-UNDO INITIAL "../logs/".
      DEFINE VARIABLE chrFileName       AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE intLockUser       AS INTEGER     NO-UNDO.
      DEFINE VARIABLE chrDevice         AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE intDbCount        AS INTEGER     NO-UNDO.
      DEFINE VARIABLE logFound          AS LOGICAL     NO-UNDO.
      DEFINE VARIABLE hdlTableHandle    AS HANDLE      NO-UNDO.
      DEFINE VARIABLE hdlLockQuery      AS HANDLE      NO-UNDO.
      DEFINE VARIABLE logLockFound      AS LOGICAL     NO-UNDO.
      DEFINE VARIABLE chrCurrentDb      AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE logFoundPath      AS LOGICAL     NO-UNDO.   
      DEFINE VARIABLE chrPts            AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE chrTelnetData     AS CHARACTER   NO-UNDO EXTENT 10.
      DEFINE VARIABLE chrLoginName      AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE chrLoginTime      AS CHARACTER   NO-UNDO.
      DEFINE VARIABLE chrDbName         AS CHARACTER   NO-UNDO.
      
      /* Go through connected Dbs and find hidden table (_file) record in correct Db                              */
      /* Doing Dbs in reverse order as Dell Db's have all the Transactional tables in the last Db so try it first */
      DbLoop:
      DO intDbCount = 1 TO NUM-DBS:
         
         ASSIGN chrCurrentDb = LDBNAME(intDbCount).
         
         /* Dynamic Query for the _file table so we can prefix table with DbName */
         CREATE BUFFER hdlTableHandle FOR TABLE chrCurrentDb + "._file".
         logFound = hdlTableHandle:FIND-UNIQUE(" WHERE _file-name = '" + chrTableName + "' AND _file._tbl-type = 'T'", NO-LOCK) NO-ERROR.
         
         IF logFound THEN
         FoundBlk:
         DO:
            ASSIGN intTableId = hdlTableHandle:BUFFER-FIELD("_file-number"):BUFFER-VALUE.
            
            ETIME(TRUE).
            
            /*Dynamic Query for the _lock table so we can prefix table with DbName*/
            CREATE QUERY hdlLockQuery.
            CREATE BUFFER hdlTableHandle FOR TABLE chrCurrentDb + "._lock".
            hdlLockQuery:ADD-BUFFER(hdlTableHandle).
            hdlLockQuery:QUERY-PREPARE("FOR EACH _lock NO-LOCK").
            hdlLockQuery:QUERY-OPEN.
            RepeatBlk:
            REPEAT:
               
               /*Try for 2 seconds, if its there it will find it in this time. Lock could be in another Db and there are 500k locks per Db*/
               IF ETIME > 2000 THEN 
                  NEXT DbLoop.
               
               hdlLockQuery:GET-NEXT.
               IF hdlLockQuery:QUERY-OFF-END THEN LEAVE RepeatBlk.
               
               IF hdlTableHandle:BUFFER-FIELD("_lock-recid"):BUFFER-VALUE = intRecID AND
                  hdlTableHandle:BUFFER-FIELD("_lock-table"):BUFFER-VALUE = intTableId AND
                  (hdlTableHandle:BUFFER-FIELD("_lock-flags"):BUFFER-VALUE  MATCHES "*X*" OR 
                   hdlTableHandle:BUFFER-FIELD("_lock-flags"):BUFFER-VALUE  MATCHES "*S*") THEN 
               DO:
                  ASSIGN logLockFound = TRUE.
                  LEAVE DbLoop.
               END.
               
               IF hdlTableHandle:BUFFER-FIELD("_lock-table"):BUFFER-VALUE = ? THEN
                  LEAVE DbLoop.
            END. /*Repeat*/
         END. /*IF logFound THEN*/
      END. /*DbLoop: DO intDbCount = 1 TO NUM-DBS:*/
      
      IF intTableId = 0 THEN
      DO:
         ASSIGN chrLockInfo = "Couldn't find _file record for " + chrTableName.
         RETURN chrLockInfo.
      END.
      
      IF NOT logLockFound THEN
      DO:
         chrLockInfo = chrTableName + " " + chrFieldValue + " is locked by another User.".
         /*chrLockInfo = "No LOCK found for Table:" + chrTableName + " Recid:" + STRING(intRecID) + " in DB:" + chrCurrentDb.*/
         RETURN chrLockInfo.
      END.
      
      IF logLockFound THEN 
      DO:
         ASSIGN intLockUser = hdlTableHandle:BUFFER-FIELD("_lock-usr"):BUFFER-VALUE.
         
         /* Dynamic Query for the _file table so we can prefix table with DbName */
         CREATE BUFFER hdlTableHandle FOR TABLE chrCurrentDb + "._connect".
         logFound = hdlTableHandle:FIND-UNIQUE(" WHERE _connect-usr = '" + STRING(intLockUser) + "'", NO-LOCK) NO-ERROR.
         IF logFound THEN
            ASSIGN chrDevice = hdlTableHandle:BUFFER-FIELD("_connect-device"):BUFFER-VALUE.
      END.
      ELSE
         ASSIGN chrLockInfo = "Couldn't find lock for " + chrTableName + " " + chrFieldValue.
      
      /*Read Telnet for Login info - this strips the leading prefix off Pts e.g. "/dev/pts/td" -> "pts/td" */
      IF INDEX(chrDevice,"pts") <> 0 THEN
      DO:
         ASSIGN chrPts = SUBSTRING(chrDevice,INDEX(chrDevice,"pts")).
         
         INPUT STREAM sFromTelnet THROUGH VALUE("ps -eafx | grep " + chrPts + " | grep -v grep | grep _progres").
         
            IMPORT STREAM sFromTelnet chrTelnetData[1] chrTelnetData[2] chrTelnetData[3] chrTelnetData[4] chrTelnetData[5] 
                                      chrTelnetData[6] chrTelnetData[7] chrTelnetData[8] chrTelnetData[9] chrTelnetData[10].
        
         INPUT STREAM sFromTelnet CLOSE.
         
         ASSIGN chrLoginName  = chrTelnetData[1]
                chrLoginTime  = chrTelnetData[5] NO-ERROR.
         
         IF NUM-ENTRIES(chrTelnetData[10],"/") > 2 THEN 
            ASSIGN chrDbName = " in Db: " + ENTRY(3,chrTelnetData[10],"/").  
      END.
      
      ASSIGN chrLockInfo = chrTableName + " " + chrFieldValue + " is locked by User: " + STRING(intLockUser).
      
      IF chrLoginName <> "" THEN
         ASSIGN chrLockInfo = chrLockInfo + "/" + chrLoginName + " who logged onto Device: " + chrPts + " @ " + chrLoginTime + chrDbName.
      ELSE
         ASSIGN chrLockInfo = chrLockInfo + " on Device:" + chrDevice.
      /*End Read Telnet*/
      
      /*Logging of locking conflicts*/
      ASSIGN chrFileName = chrFilePath + "LockingConflicts.log".
      
      IF logFoundPath THEN
      DO ON ERROR UNDO, LEAVE:
         OUTPUT STREAM sToLockLog TO VALUE(chrFileName) APPEND.
           PUT STREAM sToLockLog UNFORMATTED getLockTimestamp(NOW) + "," + chrLockInfo SKIP.
         OUTPUT STREAM sToLockLog CLOSE.
      END.
      /*End Logging*/
      
      RETURN chrLockInfo.
      
   END. /*METHOD getLockingInfo RETURNS CHARACTER*/
   
   
   /***** Private Internal Methods *****/
   /* Finds temp Create Record originally created within this Class instance. Could have been flushed out of scope by subsequent Creates */
   METHOD PRIVATE LOGICAL getCurrentRecord():
      
      DEFINE VARIABLE logRecordFound AS LOGICAL     NO-UNDO.
      
      /* What if they're looking for a create but we find an Update and vice-versa?? BG */
      
      /* Try to find existing uncomitted Create */
      logRecordFound = hdlCreateRecord:FIND-FIRST("WHERE " + NewRecordTableName + "ID = " + STRING(NewRecordUniqueID) + 
                                                   " AND Committed = ''" +
                                                   " AND SessionID = '" + CurrentSessionID + "'" +
                                                   " AND UserTransactionID = " + STRING(CurrentUserTransactionID), SHARE-LOCK) NO-ERROR.
      IF NOT logRecordFound THEN
      DO:
         chrErrors = chrErrors + "Cannot find Create record for:ssn" + NewRecordTableName + " ID:" + STRING(NewRecordUniqueID) + ". ".
         RETURN FALSE.
      END.
      
      RETURN TRUE.
      
   END METHOD. /*getCurrentRecord()*/

   
   /* This method uses the Unique ID to find a previosuly uncomitted Create */
   METHOD PUBLIC LOGICAL getCreatedRecord(INPUT intNewRecordUniqueID AS INTEGER):
      
      DEFINE VARIABLE logRecordFound AS LOGICAL     NO-UNDO.
      
      /* Try to find existing uncomitted Create */
      logRecordFound = hdlCreateRecord:FIND-FIRST("WHERE " + NewRecordTableName + "ID = " + STRING(intNewRecordUniqueID) + 
                                                   " AND Committed = ''" +
                                                   " AND SessionID = '" + CurrentSessionID + "'" +
                                                   " AND UserTransactionID = " + STRING(CurrentUserTransactionID), SHARE-LOCK) NO-ERROR.
      IF NOT logRecordFound THEN
      DO:
         chrErrors = chrErrors + "Cannot find Create record for:ssn" + NewRecordTableName + " ID:" + STRING(NewRecordUniqueID) + ". ".
         RETURN FALSE.
      END.
      
      RETURN TRUE.
      
   END METHOD. /*getCurrentRecord(INPUT intNewRecordUniqueID AS INTEGER)*/
   
   
   /* Need to get the Db that a table is in so we can do Dynamic incrementing of Sequence in createRecord method */
   METHOD PRIVATE LOGICAL getCurrentDb(INPUT chrTableName AS CHARACTER):
      
      DEFINE VARIABLE hdlTableHandle    AS HANDLE      NO-UNDO.
      
      /* This should work but isn't tested */
      CREATE BUFFER hdlTableHandle FOR TABLE chrTableName /*NO-ERROR*/.
      IF VALID-HANDLE(hdlTableHandle) THEN
      DO:
         chrCurrentDb = hdlTableHandle:DBNAME.
         DELETE OBJECT hdlTableHandle NO-ERROR.
         RETURN TRUE.
      END.           

      ASSIGN chrErrors = chrErrors + "No Table in any connected Db named:" + chrTableName + ". ".
      RETURN FALSE.
      
   END METHOD. /*getCurrentDb()*/
   
   
   METHOD PRIVATE INTEGER getLockType():
      
      /* This is a Constant (albeit defined AS INTEGER) that holds the Lock type we want to use either NO-LOCK, SHARE-LOCK or EXCLUSIVE-LOCK */
      /* Default to NO-LOCK to begin with */
      conLockTypeConst = NO-LOCK.
      
      FIND Config NO-LOCK.
      
      CASE Config.ClassLockType:
         
         WHEN "NO" THEN
            conLockTypeConst = NO-LOCK.
         
         WHEN "SHARE" THEN
            conLockTypeConst = SHARE-LOCK.
         
         WHEN "EXCLUSIVE" THEN
            conLockTypeConst = EXCLUSIVE-LOCK.
         
      END CASE.
      
      RETURN conLockTypeConst.
      
   END METHOD. /*getLockType():*/
   
   
   /***** Public External Methods *****/
   METHOD PUBLIC VOID createRecord():
      
      IF NOT getCurrentDb(NewRecordTableName) THEN
        RETURN.
      
      UpdateBlk:
      DO TRANSACTION:
         
         /* Creates a new Record on the dynamic Temp Table */
         hdlCreateRecord:BUFFER-CREATE() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error creating new:" + hdlCreateRecord:NAME + " record. " "UpdateBlk"}
         
         /* Get NEXT-VALUE of the sequence for real Table and assign to NewRecordUniqueID property which will remain for life of Class */
         NewRecordUniqueID = DYNAMIC-NEXT-VALUE(NewRecordTableName, chrCurrentDb) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error getting DYNAMIC-NEXT-VALUE OF:" + NewRecordTableName + " FOR Db:" + chrCurrentDb "UpdateBlk"}
         
         /* Assign Unique ID field */
         assignField(NewRecordTableName + "ID", NewRecordUniqueID) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting " + NewRecordTableName + "ID field on " + hdlCreateRecord:NAME + " record." "UpdateBlk"}
         
         assignField("ProcessMode", "Create") NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting ProcessMode field on " + hdlCreateRecord:NAME + " record." "UpdateBlk"}
         
         assignField("SessionID", CurrentSessionID) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting SessionID field on " + hdlCreateRecord:NAME + " record to:" + CurrentSessionID "UpdateBlk"}
         
         /* User Transaction ID binds the Update to the current Transaction */
         assignField("UserTransactionID", CurrentUserTransactionID) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting UserTransactionID field on " + hdlCreateRecord:NAME + " record to:" + STRING(CurrentUserTransactionID) "UpdateBlk"}
         
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*createRecord()*/
   
   
   METHOD PUBLIC VOID createCustRecord(INPUT intUniqueValue AS INTEGER):
      
      UpdateBlk:
      DO TRANSACTION:
         
         /* Creates a new Record on the dynamic Temp Table */
         hdlCreateRecord:BUFFER-CREATE() NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error creating new:" + hdlCreateRecord:NAME + " record. " "UpdateBlk"}
         
         /* Don't use sequences for Customer records, just set the UnmiqueID to the one sent in */
         NewRecordUniqueID = intUniqueValue NO-ERROR.
         
         /* Assign Unique ID field */
         assignField(NewRecordTableName + "ID", NewRecordUniqueID) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting " + NewRecordTableName + "ID field on " + hdlCreateRecord:NAME + " record." "UpdateBlk"}
         
         assignField("ProcessMode", "Create") NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting ProcessMode field on " + hdlCreateRecord:NAME + " record." "UpdateBlk"}
         
         assignField("SessionID", CurrentSessionID) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting SessionID field on " + hdlCreateRecord:NAME + " record to:" + CurrentSessionID "UpdateBlk"}
         
         /* User Transaction ID binds the Update to the current Transaction */
         assignField("UserTransactionID", CurrentUserTransactionID) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN 
            {clsErrors.i &Err = "Error setting UserTransactionID field on " + hdlCreateRecord:NAME + " record to:" + STRING(CurrentUserTransactionID) "UpdateBlk"}
         
      END. /*DO TRANSACTION:*/

   END METHOD. /*createCustRecord()*/
   
   
   /* Buffer Copy from a record whose UniqueID is passed in */
   METHOD PUBLIC VOID bufferCopy (INPUT intUniqueID AS INTEGER):  
   
      DEFINE VARIABLE hdlDbTable             AS HANDLE      NO-UNDO.    
      DEFINE VARIABLE logRecordFound         AS LOGICAL     NO-UNDO.
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN 
      DO:
         getCurrentRecord().
      END.
      
      /* Find real Db record so we can copy it */
      CREATE BUFFER hdlDbTable FOR TABLE NewRecordTableName NO-ERROR.
      /* Use a SHARE-LOCK here to give us Clean reads - may need to be downgraded is it proves too slow or causes locking */
      logRecordFound = hdlDbTable:FIND-FIRST("WHERE " + NewRecordTableName + "ID = " + STRING(intUniqueID), 
                                                conLockTypeConst, NO-WAIT) NO-ERROR.
      
      /* If SHARE-LOCK OR EXCLUSIVE-LOCK is used then the record could be locked so need to return who has it Locked  */
      IF hdlDbTable:LOCKED THEN
      DO:
         logRecordFound = hdlDbTable:FIND-FIRST("WHERE " + NewRecordTableName + "ID = " + STRING(intUniqueID), NO-LOCK) NO-ERROR.
         
         IF logRecordFound THEN
            chrErrors = getLockingInfo(INPUT INTEGER(hdlDbTable:RECID),
                                       INPUT hdlDbTable:TABLE,
                                       INPUT STRING(intUniqueID)) + " - please try later.".
         ELSE
            chrErrors = hdlDbTable:TABLE + " Record for Update:" + STRING(intUniqueID) + " is Locked by another User.".
         
         RETURN.
         
      END. /*IF hdlDbTable:LOCKED THEN*/

      IF NOT logRecordFound THEN
      DO:
         chrErrors = chrErrors + "Cannot find " + NewRecordTableName + " record for ID:" + STRING(intUniqueID) + ". ".
         RETURN.
      END.
      
      /* Immediately downgrade the lock */
      hdlDbTable:FIND-CURRENT(NO-LOCK) NO-ERROR.
      
      DO TRANSACTION:
         
         /* Buffer-copy from the source Db Record to the newly created tt Record */
         hdlCreateRecord:BUFFER-COPY(hdlDbTable).
         /* Don't think no-error works here */
         hdlCreateRecord:BUFFER-VALIDATE() NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error doing BUFFER-COPY.".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*bufferCopy()*/

   
   /* These assignField methods look to be duplicated but the DayaTypes of the parameters are different in each case. This allows us to */
   /* call the same function with different DataTypes and the class will use the ciorrect one                                           */
   
   /* Character */
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT chrFieldValue AS CHARACTER):      
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN 
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION:
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = chrFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + chrFieldValue + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /* Integer */
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT intFieldValue AS INTEGER):
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
/*       IF NOT hdlCreateRecord:NEW AND                                                                    */
/*          hdlCreateRecord:AVAILABLE AND                                                                  */
/*          NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN */
      
      IF NOT hdlCreateRecord:NEW AND 
         (NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE) THEN 
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION:
         
         /* If they're usdating the Unique field then we need to change the classes pointer to the nnew Unique ID */
         /* IF chrFieldName = NewRecordTableName + "ID" AND hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE <> intFieldValue THEN */
         /*   NewRecordUniqueID = intFieldValue.                                                                                          */
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = intFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + STRING(intFieldValue) + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /* Date */
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT datFieldValue AS DATE):

      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN 
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION:
        
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = datFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + STRING(datFieldValue) + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /*Decimal*/
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT decFieldValue AS DEC):

      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN 
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION:
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = decFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + STRING(decFieldValue) + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /* Logical */
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT logFieldValue AS LOGICAL):

      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN 
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION:
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = logFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + STRING(logFieldValue) + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /* Handle */
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT hdlFieldValue AS HANDLE):
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION: 
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = hdlFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + STRING(hdlFieldValue) + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /* Rowid */
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT rowFieldValue AS ROWID):
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION: 
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = rowFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + STRING(rowFieldValue) + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/

   /* Session Object */
   METHOD PUBLIC VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                  INPUT clsFieldValue AS sessionValue):
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN
      DO:
         getCurrentRecord().
      END.
      
      DO TRANSACTION: 
         
         CASE hdlCreateRecord:BUFFER-FIELD(chrFieldName):DATA-TYPE:
            WHEN "CHARACTER" THEN
            DO:
               hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = clsFieldValue:chrValue NO-ERROR.
            END.
            WHEN "INTEGER" THEN
            DO:
               hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = clsFieldValue:intValue NO-ERROR.
            END.
            WHEN "DECIMAL" THEN
            DO:
               hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = clsFieldValue:decValue NO-ERROR.
            END.
            WHEN "LOGICAL" THEN
            DO:
               hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = clsFieldValue:logValue NO-ERROR.
            END.
            WHEN "DATE" THEN
            DO:               
               hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = clsFieldValue:datValue NO-ERROR.
            END.
         END CASE.         
         
         IF ERROR-STATUS:ERROR THEN
         DO:
            chrErrors = chrErrors + "Error setting:" + hdlCreateRecord:NAME + "." + chrFieldName + " to:'" + STRING(clsFieldValue) + "'. ".
            DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
               chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
            END.
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   METHOD PUBLIC VOID incrementField(INPUT chrFieldName  AS CHARACTER,
                                     INPUT IntAddValue   AS INTEGER):
                                     /*,INPUT MaxTotalValue AS INTEGER):*/ 
      
      DEFINE VARIABLE intOldFieldValue AS INTEGER NO-UNDO.
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN 
      DO:
         getCurrentRecord().
      END.
      
      intOldFieldValue = hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE NO-ERROR.
      
      DO TRANSACTION: 
         
         /* Check if it exceeds max value, optional PARAMETER? BG */
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = intOldFieldValue + intAddValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
            ASSIGN chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*incrementField()*/
   
   
   METHOD PUBLIC VOID decrementField(INPUT chrFieldName  AS CHARACTER,
                                     INPUT IntAddValue   AS INTEGER):      
                                     /*,INPUT DontGoBelowZero AS LOGICAL):*/
      
      DEFINE VARIABLE intOldFieldValue AS INTEGER NO-UNDO.
      
      /*Check that we have a Record in the Buffer and that it matches the one we want, otherwise refind the one we want again*/
      IF NOT hdlCreateRecord:AVAILABLE OR NewRecordUniqueID <> hdlCreateRecord:BUFFER-FIELD(NewRecordTableName + "ID"):BUFFER-VALUE THEN 
      DO:
         getCurrentRecord().
      END.
      
      intOldFieldValue = hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE NO-ERROR.
      
      DO TRANSACTION: 
         
         /* Check for less than Zero, optional PARAMETER? BG */
         
         hdlCreateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = intOldFieldValue + intAddValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
            ASSIGN chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*decrementField()*/
   
   /* Buffer Copy matching fields from a Source Table */
   METHOD PUBLIC VOID bufferCopy(INPUT chrTableName AS CHARACTER,
                                 INPUT intUniqueID  AS INTEGER):

      DEFINE VARIABLE hdlReadBuffer  AS HANDLE    NO-UNDO.
      DEFINE VARIABLE hdlCheckBuffer AS HANDLE    NO-UNDO.      
      DEFINE VARIABLE intFieldCount  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE chrFieldName   AS CHARACTER NO-UNDO.
      
      CREATE BUFFER hdlReadBuffer FOR TABLE chrTableName NO-ERROR.
      
      /* Check its a valid Table */
      IF NOT VALID-HANDLE(hdlReadBuffer) THEN
      DO:      
         ASSIGN chrErrors = chrErrors + "Cannot find table : " + chrTableName.
      END.

      /* Find the record */      
      hdlReadBuffer:FIND-UNIQUE("WHERE " + chrTableName + "ID = " + STRING(intUniqueID)).

      /* Check its available */
      IF NOT hdlReadBuffer:AVAILABLE THEN
      DO:      
         ASSIGN chrErrors = chrErrors + "Cannot find record in : " + chrTableName + " with " + chrTableName + "ID :" + STRING(intUniqueID).
      END.
      
      /* Read the fields of the Copy Buffer and Assign whatever fields we can */
      Field_Check:
      DO intFieldCount = 1 TO hdlReadBuffer:NUM-FIELDS:
         
         hdlCheckBuffer = hdlCreateRecord:BUFFER-FIELD(hdlReadBuffer:BUFFER-FIELD(intFieldCount):NAME) NO-ERROR.
         
         /* Check the Copy Buffer field Exists in the Object Buffer */
         IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE (hdlCheckBuffer) THEN 
         DO:
            /**************************************************************************
            /* Ignore Unique ID and Version */
            IF hdlCreateRecord:BUFFER-FIELD(intFieldCount):NAME = chrTableName + "ID" THEN NEXT Field_Check.
            IF hdlCreateRecord:BUFFER-FIELD(intFieldCount):NAME = "VersionID" THEN NEXT Field_Check.

            /* Copy Fields using the Classes own Methods */
            assignField(hdlCreateRecord:BUFFER-FIELD(intFieldCount):NAME, hdlReadBuffer:BUFFER-FIELD(intFieldCount):BUFFER-VALUE).
            **************************************************************************/
        
            chrFieldName = hdlReadBuffer:BUFFER-FIELD(intFieldCount):NAME.

            /* Ignore Unique ID of target record if in source record and Version */
            IF hdlCreateRecord:BUFFER-FIELD(chrFieldName):NAME = NewRecordTableName + "ID" THEN 
               NEXT Field_Check.
            IF hdlCreateRecord:BUFFER-FIELD(chrFieldName):NAME = "VersionID" THEN 
               NEXT Field_Check.

            /* Copy Fields using the Classes own Methods */
            assignField(hdlCreateRecord:BUFFER-FIELD(chrFieldName):NAME, 
                        hdlReadBuffer:BUFFER-FIELD(intFieldCount):BUFFER-VALUE).
                        
         END.

      END.

      /* Clean Up */
      DELETE OBJECT hdlReadBuffer NO-ERROR.

   END METHOD. /* bufferCopy() */


   METHOD PUBLIC CHARACTER getErrors():
      
      IF chrErrors <> "" THEN
        RETURN TRIM(chrErrors).
      
      RETURN "".
      
   END METHOD. /*getErrors()*/
   
   
   /***** Destructor *****/
   DESTRUCTOR newRecord():
      
      /* Session and Table get set here and can't be changed */
      hdlCreateRecord:BUFFER-RELEASE NO-ERROR.

      
   END DESTRUCTOR.
   
   
END CLASS. /*newRecord*/
