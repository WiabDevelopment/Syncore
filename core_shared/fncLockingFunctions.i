/*******************************************************************************************************************************************
PROGRAM: fncLockingFunctions.i
AUTHOR:  BG
DATE:    18/03/2010
PURPOSE: Returns locking information about a locked record when given table name & record recid. The third parameter is any other info that 
         the user wants to display but was designed to contain a field value such as a record ID. Only works for IVT and brokers.
--------------------------------------------------------------------------------------------------------------------------------------------
MODIFICATIONS:
--------------------------------------------------------------------------------------------------------------------------------------------
Date        Who  Reference Description
----------  ---- --------- -----------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------
*******************************************************************************************************************************************/

/***** Suggested use *****
    
FIND StockPackage EXCLUSIVE-LOCK 
   WHERE StockPackage.intsyscontain = intStockPackageID NO-ERROR NO-WAIT.

IF NOT AVAILABLE StockPackage THEN 
DO:    
   IF LOCKED StockPackage THEN 
   DO:
      FIND StockPackage WHERE StockPackage.intsyscontain = jobline.intsyscontain NO-LOCK.
      MESSAGE fGetLockingInfo(INTEGER(RECID(StockPackage)), 
                              "StockPackage",
                              STRING(StockPackageID)) + " - please try again!" SKIP VIEW-AS ALERT-BOX.
      UNDO Main-Block, LEAVE Main-Block.
   END.
   ELSE...

***** End Suggested use *****/

DEFINE VARIABLE chrPts        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTelnetData AS CHARACTER   NO-UNDO EXTENT 10.
DEFINE VARIABLE chrLoginName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLoginTime  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDbName     AS CHARACTER   NO-UNDO.

DEFINE STREAM sToLockLog.
DEFINE STREAM sFromTelnet.

FUNCTION fLockTimestamp RETURNS CHARACTER (INPUT datDateToConvert AS DATETIME-TZ):
   
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
   
END FUNCTION.


FUNCTION fGetLockingInfo RETURNS CHARACTER (INPUT intRecId      AS INTEGER,    /*Recid of locked record -> got by refinding record no-lock*/
                                            INPUT chrTableName  AS CHARACTER,  /*Table Name*/
                                            INPUT chrFieldValue AS CHARACTER): /*chrFieldValue is Optional field to display*/
   
   DEFINE VARIABLE intTableId        AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrLockInfo       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrFilePath       AS CHARACTER   NO-UNDO INITIAL "../logs/".
   DEFINE VARIABLE chrFileName       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intLockUser       AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrDevice         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intDbCount        AS INTEGER     NO-UNDO.
   DEFINE VARIABLE logRecordFound    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE hdlTableHandle    AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlLockQuery      AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logLockFound      AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrCurrentDb      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE logFoundPath      AS LOGICAL     NO-UNDO.
   
   chrFilePath = "../logs/".
   
   /* Go through connected Dbs and find hidden table (_file) record in correct Db                              */
   /* Doing Dbs in reverse order as Dell Db's have all the Transactional tables in the last Db so try it first */
   DbLoop:
   DO intDbCount = 1 TO NUM-DBS:
      
      ASSIGN chrCurrentDb = LDBNAME(intDbCount).
      
      /* Dynamic Query for the _file table so we can prefix table with DbName */
      CREATE BUFFER hdlTableHandle FOR TABLE chrCurrentDb + "._file".
      logRecordFound = hdlTableHandle:FIND-UNIQUE(" WHERE _file-name = '" + chrTableName + "' AND _file._tbl-type = 'T'", NO-LOCK) NO-ERROR.
      IF logRecordFound THEN
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
            
            /* Try for 3 seconds, if its there it will find it in this time. Lock could be in another Db and there are ~500k locks per Db */
            IF ETIME > 3000 THEN 
               NEXT DbLoop.
            
            hdlLockQuery:GET-NEXT.
            IF hdlLockQuery:QUERY-OFF-END THEN LEAVE RepeatBlk.
            
            IF hdlTableHandle:BUFFER-FIELD("_lock-recid"):BUFFER-VALUE = intRecId AND
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
      END. /*IF logRecordFound THEN*/
   END. /*DbLoop: DO intDbCount = 1 TO NUM-DBS:*/
   
   IF intTableId = 0 THEN
   DO:
      ASSIGN chrLockInfo = "Couldn't find _file record for " + chrTableName.
      RETURN chrLockInfo.
   END.
   
   IF NOT logLockFound THEN
   DO:
      chrLockInfo = chrTableName + " " + chrFieldValue + " is locked by another User.".
      /*chrLockInfo = "No LOCK found for Table:" + chrTableName + " Recid:" + STRING(intRecId) + " in DB:" + chrCurrentDb.*/
      RETURN chrLockInfo.
   END.
   
   IF logLockFound THEN 
   DO:
      ASSIGN intLockUser = hdlTableHandle:BUFFER-FIELD("_lock-usr"):BUFFER-VALUE.
      
      /* Dynamic Query for the _file table so we can prefix table with DbName */
      CREATE BUFFER hdlTableHandle FOR TABLE chrCurrentDb + "._connect".
      logRecordFound = hdlTableHandle:FIND-UNIQUE(" WHERE _connect-usr = '" + STRING(intLockUser) + "'", NO-LOCK) NO-ERROR.
      IF logRecordFound THEN
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
        PUT STREAM sToLockLog UNFORMATTED fLockTimestamp(NOW) + "," + chrLockInfo SKIP.
      OUTPUT STREAM sToLockLog CLOSE.
   END.
   /*End Logging*/
   
   RETURN chrLockInfo.
   
END. /*FUNCTION fGetLockingInfo RETURNS CHARACTER*/
