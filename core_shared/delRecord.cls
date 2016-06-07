/*------------------------------------------------------------------------------------------------------------------------------------------
Program : delRecord.cls
Purpose : Delete Class which is used in conjuntion with the Data Library to Delete all records except for Batch jobs.
Author  : BG
Date    : 27th Jan 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
08/01/2016 CS  NextelBr   Added unique UserTransactionID to allow record tracking from each unique libDataManager call within the same 
                          session. 
11/01/2016 CS  NextelBr   Fixed issue with bad reference.                               
------------------------------------------------------------------------------------------------------------------------------------------*/

CLASS delRecord:
   
   /***** Vars *****/
   DEFINE PROTECTED VARIABLE hdlDeleteRecord    AS HANDLE      NO-UNDO.
   DEFINE PROTECTED VARIABLE hdlDbTable         AS HANDLE      NO-UNDO.
   DEFINE PROTECTED VARIABLE hdlDbRecord        AS HANDLE      NO-UNDO.
   DEFINE PROTECTED VARIABLE chrErrors          AS CHARACTER   NO-UNDO.
   DEFINE PROTECTED VARIABLE intError           AS INTEGER     NO-UNDO.
   DEFINE PROTECTED VARIABLE conLockTypeConst   AS INTEGER     NO-UNDO.
   
   
   /***** Properties *****/
   DEFINE PUBLIC PROPERTY CurrentUniqueID  AS INTEGER     NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY CurrentTableName AS CHARACTER   NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY CurrentSessionID AS CHARACTER   NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY CurrentUserTransactionID  AS INTEGER NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY RecordFound      AS LOGICAL     NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY RecordLocked     AS LOGICAL     NO-UNDO
       GET.
       PROTECTED SET.
   
   
   /***** Contructor *****/
   CONSTRUCTOR delRecord(INPUT hdlDeleteTableBuffer AS HANDLE,
                         INPUT chrUserSessionID     AS CHARACTER,
                         INPUT intUserTransactionID AS INTEGER):
      
      /* Session and Table get set here and can't be changed for this Class instance */
      ASSIGN hdlDeleteRecord          = hdlDeleteTableBuffer
             CurrentSessionID         = chrUserSessionID
             CurrentUserTransactionID = intUserTransactionID
             CurrentTableName         = SUBSTRING(hdlDeleteRecord:NAME,4). /* Temp Table name will have a ssn prefix - strip this off */
      
      /* This is a Constant (albeit defined AS INTEGER) that holds the Lock type we want to use either NO-LOCK, SHARE-LOCK or EXCLUSIVE-LOCK */
      conLockTypeConst = getLockType().
      
   END CONSTRUCTOR.
   
   
   /***** Public External Methods *****/
   METHOD PUBLIC VOID deleteRecord(INPUT intUniqueID  AS INTEGER):
      
      DEFINE VARIABLE logRecordFound    AS LOGICAL.
      
      FindBlk:
      DO:
         /* First try to find a previously existing uncomitted Update or Create for this Session - if we find one then use it */
         logRecordFound = hdlDeleteRecord:FIND-FIRST("WHERE " + CurrentTableName + "ID = " + STRING(intUniqueID) + 
                                                      " AND Committed = ''" +
                                                      " AND SessionID = '" + CurrentSessionID + "'" +
                                                      " AND UserTransactionID = " + STRING(CurrentUserTransactionID), conLockTypeConst) NO-ERROR.
         IF logRecordFound THEN
         DO:
            CurrentUniqueID = intUniqueID.
            LEAVE FindBlk.
         END.
         
         /* No existing Updates or Creates for the Session so go ahead and find real Db record and create a Update Record for it */
         CREATE BUFFER hdlDbTable FOR TABLE CurrentTableName NO-ERROR.
         /* Use a SHARE-LOCK here to give us clean reads - this may need to be downgraded is it proves too slow or causes locking */
         logRecordFound = hdlDbTable:FIND-FIRST("WHERE " + CurrentTableName + "ID = " + STRING(intUniqueID), conLockTypeConst) NO-ERROR.
         
         IF NOT logRecordFound THEN
         DO:
            chrErrors = chrErrors + "Cannot find " + CurrentTableName + " record for ID:" + STRING(intUniqueID).
            RETURN.
         END.
         
         /* Immediately downgrade the lock */
         hdlDbTable:FIND-CURRENT(NO-LOCK) NO-ERROR.
         
         UpdateBlk:
         DO TRANSACTION:
            
            /* Creates a new Record on the dynamic Temp Table which is a replica of the real Db table */
            hdlDeleteRecord:BUFFER-CREATE() NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
              {clsErrors.i &Err = "Error creating new:" + hdlDeleteRecord:NAME + " record. " "UpdateBlk"}
            
            /* Copies the current values of the real Db Table into the new Update record */
            hdlDeleteRecord:BUFFER-COPY(hdlDbTable) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
              {clsErrors.i &Err = "Error setting defaults for:" + hdlDeleteRecord:NAME + " record. " "UpdateBlk"}
            
            /* Assign CurrentUniqueID property so we can refind this recaord in future is its flushed out of scope */
            CurrentUniqueID = intUniqueID.
            RecordFound     = TRUE.
            
            /* ProcessMode field records whether we're Creating, Updating or Deleting */
            assignField("ProcessMode", "Delete") NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
              {clsErrors.i &Err = "Error setting ProcessMode field on " + hdlDeleteRecord:NAME + " record." "UpdateBlk"}
            
            /* Session ID binds the Update to the current Session */
            assignField("SessionID", CurrentSessionID) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
              {clsErrors.i &Err = "Error setting SessionID field on " + hdlDeleteRecord:NAME + " record to:" + CurrentSessionID "UpdateBlk"}
          
            /* User Transaction ID binds the Update to the current Transaction */
            assignField("UserTransactionID", CurrentUserTransactionID) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
              {clsErrors.i &Err = "Error setting UserTransactionID field on " + hdlDeleteRecord:NAME + " record to:" + STRING(CurrentUserTransactionID) "UpdateBlk"}
             
            /* Each record the core & file Dbs have a VersionID. Need to record this in Update to compare it with actual VersionID on */
            /* real Db record when it goes to Delete it. Only write Version ID if its not a Customer specific Table                     */
            IF hdlDbTable:DBNAME = "cust" AND hdlDbTable:NAME BEGINS "Cust" THEN
              LEAVE UpdateBlk.
            
            assignField("VersionID", hdlDbTable:BUFFER-FIELD("VersionID"):BUFFER-VALUE) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
              {clsErrors.i &Err = "Error setting VersionID field on " + hdlDeleteRecord:NAME + " record" "UpdateBlk"}
            
         END. /*DO TRANSACTION:*/
         
      END. /*FindBlk:*/
      
   END METHOD.
   
   
   METHOD PUBLIC CHARACTER getErrors():
      
      IF chrErrors <> "" THEN
        RETURN TRIM(chrErrors).
      
      RETURN "".
      
   END METHOD. /*getErrors():*/
   
   
   /***** Private Internal Methods *****/
   /* These assignField methods look to be duplicated but the DayaTypes of the parameters are different in each case. This allows us to  */
   /* call the same function with different DataTypes and the class will use the correct one automatically                               */
   /* In the newRecord & updRecord classes these methods are public but here are Private as we only ever call them from within the class */
   
   /*Character*/
   METHOD PRIVATE VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                   INPUT chrFieldValue AS CHARACTER):
      
      DO TRANSACTION: 
         
         hdlDeleteRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = chrFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
            ASSIGN chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /* Integer */
   METHOD PRIVATE VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                   INPUT intFieldValue AS INTEGER):
      
      DO TRANSACTION: 
         
         hdlDeleteRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = intFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
            ASSIGN chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /*Decimal*/
   METHOD PRIVATE VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                   INPUT decFieldValue AS DEC):
      
      DO TRANSACTION: 
         
         hdlDeleteRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = decFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
            ASSIGN chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /*Date*/
   METHOD PRIVATE VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                   INPUT datFieldValue AS DATE):
      
      DO TRANSACTION: 
         
         hdlDeleteRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = datFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
            ASSIGN chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
   /*Logical*/
   METHOD PRIVATE VOID assignField(INPUT chrFieldName  AS CHARACTER,
                                   INPUT logFieldValue AS LOGICAL):
      
      DO TRANSACTION: 
         
         hdlDeleteRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = logFieldValue NO-ERROR.
         
         IF ERROR-STATUS:ERROR THEN
         DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
            ASSIGN chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
         END.
      END. /*DO TRANSACTION:*/
      
   END METHOD. /*assignField()*/
   
   
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
   
   
   /***** Detructor *****/
   DESTRUCTOR delRecord():
      
      /* Session and Table get set here and can't be changed */
      hdlDeleteRecord:BUFFER-RELEASE NO-ERROR.
      
   END DESTRUCTOR.
   
   
END CLASS. /*delRecord*/
