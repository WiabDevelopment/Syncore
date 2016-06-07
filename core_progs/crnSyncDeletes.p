/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnSyncDelets.p
Purpose : Connects to Syngate LIVE and Current Application. 
          - Does a dynamic query to loop through all of the gate tables in the current application/env.
          - Does a dynamic find first off of Syngate Live for the current record if it can't find the current record in 
            Syngate Live it will delete the record in the current application.
          - This will prevent error messages that occur when records are deleted out of the LIVE Syngate Environment without
            being deleted in All Other Environments/Applications
         /*               hdlTargetDbTable:BUFFER-DELETE NO-ERROR.*/
         
Author  : SH
Date    : 1st July 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i}
{fncLockingFunctions.i}
{fncGlobalFunctions.i}   
{fncDateFunctions.i}   
{fncClassFunctions.i}
{fncServerFunctions.i}   

DEFINE TEMP-TABLE ttHeaders
   FIELD HeaderName AS CHARACTER.

/* UNDO Vars */
DEFINE VARIABLE intApplicationID           AS INTEGER      NO-UNDO.
DEFINE VARIABLE intExternalApplicationID   AS INTEGER      NO-UNDO.
DEFINE VARIABLE hdlTableName               AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlTableQuery              AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlDataQuery               AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlSourceDbTable           AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlTargetDbTable           AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlApplicationRecord       AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlGateConfigRecord        AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlFieldRecord             AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlUniqueField             AS HANDLE       NO-UNDO.
DEFINE VARIABLE chrDbTable                 AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrSourceDb                AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrTargetDb                AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrCoreDb                  AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrHostName                AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrTableWhere              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrWhereClause             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrUniqueValue             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrErrorOut                AS CHARACTER    NO-UNDO.
DEFINE VARIABLE logFound                   AS LOGICAL      NO-UNDO.
DEFINE VARIABLE chrSystemPostFix           AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrEnglishString           AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrTimestamp               AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFilePath             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile                 AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrConnectionString        AS CHARACTER    NO-UNDO FORMAT "x(40)".
DEFINE VARIABLE intCronEventRunID          AS INTEGER      NO-UNDO.
DEFINE VARIABLE chrSsnDeleteRecs           AS sessionValue NO-UNDO.
DEFINE VARIABLE chrEmailFilePath           AS CHARACTER    NO-UNDO.
DEFINE VARIABLE i                          AS INTEGER      NO-UNDO.
DEFINE VARIABLE chrNewTable                AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFullFilePath            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrEmailAddress            AS CHARACTER    NO-UNDO.


/* NO-UNDO Vars */
DEFINE VARIABLE intNumTranslations         AS INTEGER.
DEFINE VARIABLE logApplicationSyncRequired AS LOGICAL.

/* Streams */
DEFINE STREAM sToCronLog.
DEFINE STREAM chrDeleteRec.

FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToCronLog UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 

END FUNCTION. /*fLog*/

/* The batch job needs to connect to the source Db and target Dbs */
ASSIGN chrSourceDb       = "syngate"
       chrTargetDb       = "gate"
       chrCoreDb         = "core"
       chrTableWhere     = "FOR EACH _file WHERE _file._tbl-type = 'T'"
       chrSystemPostFix  = "_syngate_live".  /* syngate_live for Live     */
       chrHostName       = "syngate".  


/* First disconnect the 1 Db that we don't need - that will leave us connected to "core", "gate", "file", and "temp" from the */
/* local application                                                                                                          */
DISCONNECT "cust" NO-ERROR.

/* Connect to "gate" db within the syngate system using alias db name of "syngate" - change the service name according to environment */
chrConnectionString = "-db gate -ld syngate -H " + chrHostName + " -S 16544 -Mm 4096 -N TCP -n 8 -B 100".

/* Set the log file destination directory  */
chrLogFilePath = fGetAgedDirectory("../logs/", 90).
IF chrLogFilePath BEGINS "Error" THEN
   chrLogFilePath = "../logs/".

ASSIGN chrTimeStamp     = fTimestamp(NOW)
       chrLogFile       = "SyngateDeleteSync_" + fDisplayDate&Time(chrTimeStamp,"mdyHMS") + ".log"
       chrLogFile       = chrLogFilePath + chrLogFile.  

/* Do a straight output with a stream so this will catch all errors including unexpected Progress errors */
IF logGblDebugging THEN 
DO:
   OUTPUT STREAM sToCronLog TO VALUE(chrLogFile) APPEND.
   fLog(" Connecting to Syngate" + chrConnectionString).
   fLog("Delete Records: ").
END. /* IF logGblDebugging */


/* Connect to "gate" db within the syngate system using alias db name of "syngate" - these connection values should never change */
IF NOT CONNECTED(chrSourceDb) THEN
   CONNECT VALUE(chrConnectionString) NO-ERROR.

IF logGblDebugging THEN
   fLog(STRING(CONNECTED(chrSourceDb))).

/* Check if Dbs are Connected, won't be during maintenance windows or temporary disconnects, don't want alert emails on downtime */
IF NOT CONNECTED(chrSourceDb) THEN 
   LEAVE.

IF NOT CONNECTED(chrCoreDb) THEN
DO:
   DISCONNECT VALUE(chrSourceDb) NO-ERROR.
   RETURN ERROR "Core DB was not available.".
END. /* IF NOT CONNECTED(chrCoreDb */

IF NOT CONNECTED(chrTargetDb) THEN
DO:
   DISCONNECT VALUE(chrSourceDb) NO-ERROR.
   RETURN ERROR "Gate DB was not available.".
END. /* IF NOT CONNECTED(chrTargetDb) */
   
/* Find what application this is - only want data specifically for it */
FIND FIRST Config NO-LOCK NO-ERROR.
IF NOT AVAILABLE Config THEN DO:
   DISCONNECT VALUE(chrSourceDb) NO-ERROR.
   RETURN ERROR "Config was not available.".
END. /* IF NOT AVAILABLE Config */

ASSIGN intApplicationID         = Config.ApplicationID
       intExternalApplicationID = Config.ExternalApplicationID
       chrTimestamp             = fTimestamp(NOW)
       chrFullFilePath          = "../files/csv/SyncDeletedRecords" + fDisplayDate&Time(fTimestamp(NOW),"ymdHMS") + ".csv".
       
FIND FIRST Environment NO-LOCK /*idx=EnvironmentCode*/
   WHERE Environment.EnvironmentCode = chrGblEnvironment NO-ERROR.
IF NOT AVAILABLE Environment THEN
DO:
   RETURN ERROR "Environment was not found for EnvironmentCode: " + chrGblEnvironment.   
END. /*IF NOT AVAILABLE Environment THEN*/

FIND FIRST SystemDbAppEnvLink NO-LOCK /*idx=EnvironmentIDApplicationID*/
   WHERE SystemDbAppEnvLink.ApplicationID = intApplicationID 
   AND SystemDbAppEnvLink.EnvironmentID = Environment.EnvironmentID NO-ERROR.
IF NOT AVAILABLE SystemDbAppEnvLink THEN
DO:
   RETURN ERROR "SystemDbAppEnvLink was not found for ApplicationID: " + STRING(intApplicationID)
                   + " and EnvironmentID: " + STRING(Environment.EnvironmentID). 
END. /* IF NOT AVAILABLE SystemDbAppEnvLink THEN */

FIND FIRST Application NO-LOCK /*idx=ApplicationID*/
   WHERE Application.ApplicationID = intApplicationID NO-ERROR.
IF NOT AVAILABLE Application THEN
DO:
   RETURN ERROR "Application was not found for ApplicationID: " + STRING(intApplicationID).
END. /*IF NOT AVAILABLE Application THEN*/

FIND FIRST EmailGroup NO-LOCK /*idx=GroupCode*/
   WHERE EmailGroup.GroupCode = "CronGateDeletion" NO-ERROR.
IF NOT AVAILABLE EmailGroup THEN
DO:
   RETURN ERROR "EmailGroup was not found for GroupCode: CronGateDeletion".
END. /*IF NOT AVAILABLE EmailGroup THEN*/

FIND FIRST gate.GateUser NO-LOCK
   WHERE gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

IF logGblDebugging THEN
DO:
   fLog(" Before MainBlk chrGblEnvironment:" + chrGblEnvironment).
   fLog(" intGblApplicationID:" + STRING(intGblApplicationID)).
   fLog(" intApplicationID:" + STRING(intApplicationID)).
   fLog(" EnvironmentID:" + STRING(Environment.EnvironmentID)).
   fLog(" Environment.SyncGateDeletes:" + STRING(Environment.SyncGateDeletes)).
   fLog(" Syncing Application: " + Application.AppName + " for Environment: " + chrGblEnvironment).
   fLog(" HostName: " + SystemDbAppEnvLink.EnvHostName + " DbService" + SystemDbAppEnvLink.DbService).
END. /*IF logGblDebugging THEN*/

/*Open Stream for CSV file to OUTPUT Deleted Records for Data Integrity Purposes*/
OUTPUT STREAM chrDeleteRec to VALUE(chrFullFilePath).

/* Create Buffer for _field hidden table */
CREATE BUFFER hdlFieldRecord FOR TABLE chrSourceDb + "._field".

/* Dynamic Query for the _file table so we can prefix table with DbName - go through all the Tables one by one and download data */
CREATE QUERY hdlTableQuery.
CREATE BUFFER hdlTableName FOR TABLE chrSourceDb + "._file".
hdlTableQuery:ADD-BUFFER(hdlTableName) NO-ERROR.
hdlTableQuery:QUERY-PREPARE(chrTableWhere) NO-ERROR.
hdlTableQuery:QUERY-OPEN NO-ERROR.

IF logGblDebugging THEN
DO: 
   fLog(" Before MainBlk.").
   fLog(" Start Looping of Gate Db Tables.").
END. /*IF logGblDebugging THEN*/   
   
MainBlk:
DO TRANSACTION ON ERROR UNDO MainBlk, LEAVE MainBlk:

   TableLoop:
   REPEAT ON ERROR UNDO MainBlk, LEAVE MainBlk:

      hdlTableQuery:GET-NEXT.
      IF hdlTableQuery:QUERY-OFF-END THEN LEAVE.

      /* The _file-name here is the actual Db Table name */
      ASSIGN chrDbTable = hdlTableName:BUFFER-FIELD("_file-name"):BUFFER-VALUE.

      /* Don't want to delete the following because */
      IF chrDbTable = "UserSession" THEN
        NEXT TableLoop.
      IF chrDbTable = "UserSessionCrumb" THEN
        NEXT TableLoop.
      IF chrDbTable = "UserSessionBusUnitLink" THEN /*Might only want to delete these if they are older then 1/2 days for now leave off*/
        NEXT TableLoop.
      IF chrDbTable = "GateConfig" THEN
        NEXT TableLoop.
      IF chrDbTable = "GateSetting" THEN
        NEXT TableLoop.
      IF chrDbTable = "AdminTable" THEN
        NEXT TableLoop.
      IF chrDbTable = "WebBroker" THEN
        NEXT TableLoop.
      IF chrDbTable = "AccessAppLinkHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "AccessGroupBusUnitLinkHist" THEN
        NEXT TableLoop.
      IF chrDbTable = "AccessGroupEnvLinkHist" THEN
        NEXT TableLoop.
      IF chrDbTable = "AccessGroupHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "AccessGroupPermissionLinkHist" THEN
        NEXT TableLoop.
      IF chrDbTable = "AccessMenuItemLinkHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "AccessUserLinkHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "ApplicationHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "AppMenuItemLinkHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "GateUserHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "MenuItemHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "SystemDbAppEnvLinkHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "TranslationResult" THEN /*Not sure if I need to skip these but figure since it is not working completely
                                                 Might as well skip*/
        NEXT TableLoop.
      IF chrDbTable = "TranslationResultHistory" THEN
        NEXT TableLoop.
      IF chrDbTable = "WebBrokerHistory" THEN
        NEXT TableLoop.
        
      
      IF logGblDebugging THEN
         fLog(" Checking for Deletes for Table: " + chrDbTable).
      
      /* Skip Specific Tables */
      IF chrDbTable BEGINS "Translation" THEN
         NEXT TableLoop.
         
      /* If the Table has an ApplicationID field then add it to the where clause so we only download data for a system's Application */
      chrWhereClause = "FOR EACH " + chrDbTable + " NO-LOCK".
      logFound = hdlFieldRecord:FIND-FIRST("WHERE _file-recid = '" + STRING(hdlTableName:RECID) + "'"
                                           + "AND _field-name = 'ApplicationID'", NO-LOCK) NO-ERROR.

      IF logFound THEN
      DO:
         chrWhereClause = IF intApplicationID = 2 THEN /* SYNCORE Copy all the data */
                             chrWhereClause
                          ELSE chrWhereClause + " WHERE ApplicationID = '" + STRING(intApplicationID) + "'".

         IF intExternalApplicationID <> 0 THEN
            chrWhereClause = chrWhereClause + " OR ApplicationID = '" + STRING(intExternalApplicationID) + "'".
      END. /*IF logFound THEN*/
            
      CREATE BUFFER hdlSourceDbTable FOR TABLE chrSourceDb + "." + chrDbTable.

      /* Due to source Db possibly having been updated before the target Db, check if the table exists in the target Db */
      CREATE BUFFER hdlTargetDbTable FOR TABLE chrTargetDb + "." + chrDbTable NO-ERROR.

      IF NOT VALID-HANDLE(hdlTargetDbTable) THEN
         NEXT TableLoop.

      hdlUniqueField = hdlTargetDbTable:BUFFER-FIELD(chrDbTable + "ID").

      /* Query for the Target Db table*/
      CREATE QUERY hdlDataQuery.
      hdlDataQuery:ADD-BUFFER(hdlTargetDbTable) NO-ERROR.
      hdlDataQuery:QUERY-PREPARE(chrWhereClause) NO-ERROR.
      hdlDataQuery:QUERY-OPEN NO-ERROR.

      DeleteBlk:
      REPEAT ON ERROR UNDO MainBlk, LEAVE MainBlk:

         hdlDataQuery:GET-NEXT.
         IF hdlDataQuery:QUERY-OFF-END THEN LEAVE.

         /* Get unique value */
         chrUniqueValue = STRING(hdlUniqueField:BUFFER-VALUE).
         
         logFound = hdlSourceDbTable:FIND-UNIQUE("WHERE " + chrDbTable + "ID = '" + chrUniqueValue + "'", NO-LOCK, NO-WAIT) NO-ERROR.
         
         /*Record was either Locked or Not Found*/
         IF logFound THEN
         DO:
            NEXT DeleteBlk.
         END. /* IF logFound <> TRUE */

         hdlTargetDbTable:DISABLE-LOAD-TRIGGERS(YES). /* Adding this here to disable any firing of the triggers on application gates */
         
         ERROR-STATUS:ERROR = FALSE.

         hdlDataQuery:GET-CURRENT(EXCLUSIVE-LOCK).
         
         /*Delete the Record if logFound = TRUE*/
         DO ON ERROR UNDO DeleteBlk, NEXT DeleteBlk:
           
            IF NOT logFound THEN
            DO:     
               IF NOT Environment.SyncGateDeletes THEN
               DO:
                  IF logGblDebugging THEN
                     fLog(" Not Real Deletion RecordID: " + STRING(chrUniqueValue) + " for Table: " + chrDbTable).
               END. /*IF NOT Environment.SyncGateDeletes THEN*/
               
               /*This will output records that are being deleted along with what type of record it is*/
               IF NOT CAN-FIND(FIRST ttHeaders NO-LOCK 
                              WHERE ttHeaders.HeaderName = chrDbTable) THEN
               DO:
                  DO i = 1 TO hdlTargetDbTable:NUM-FIELDS:
                     PUT STREAM chrDeleteRec UNFORMATTED hdlTargetDbTable:BUFFER-FIELD(i):NAME + ",".
                  END. /*DO i = 1 TO hdlTargetDbTable:NUM-FIELDS:*/
                  
                  CREATE ttHeaders.
                  ASSIGN ttHeaders.HeaderName = chrDbTable.
                  
                  EXPORT STREAM chrDeleteRec "".
                  
               END. /*IF chrDbTable <> chrNewTable THEN*/
                              
               DO i = 1 TO hdlTargetDbTable:NUM-FIELDS:
                  PUT STREAM chrDeleteRec UNFORMATTED hdlTargetDbTable:BUFFER-FIELD(i):BUFFER-VALUE + ",".
               END. /*DO i = 1 TO hdlTargetDbTable:NUM-FIELDS:*/
               
               EXPORT STREAM chrDeleteRec "".
            END. /*IF NOT logFound THEN*/
            
            IF Environment.SyncGateDeletes AND NOT logFound THEN
            DO:
               IF logGblDebugging THEN
                  fLog(" Real Deletion RecordID: " + STRING(chrUniqueValue) + " for Table: " + chrDbTable).
                              
               hdlTargetDbTable:BUFFER-DELETE NO-ERROR.
               
            END. /*IF Environment.SyncGateDeletes AND NOT logFound THEN*/

         END. /* DO TRANS ON ERROR UNDO DeleteBlk, NEXT DeleteBlk: */
   
         logFound = FALSE.
         
         hdlTargetDbTable:BUFFER-VALIDATE() NO-ERROR.
         hdlTargetDbTable:BUFFER-RELEASE() NO-ERROR.
         
      END. /*Query Repeat*/
      
      IF logGblDebugging THEN
         fLog(" after query").

      hdlDataQuery:QUERY-CLOSE()     NO-ERROR.
      DELETE OBJECT hdlDataQuery     NO-ERROR.
      DELETE OBJECT hdlSourceDbTable NO-ERROR.
      DELETE OBJECT hdlTargetDbTable NO-ERROR.
      DELETE OBJECT hdlUniqueField   NO-ERROR.

   END. /*Query Repeat*/

   hdlTableQuery:QUERY-CLOSE()        NO-ERROR.
   DELETE OBJECT hdlTableQuery        NO-ERROR.
   DELETE OBJECT hdlTableName         NO-ERROR.
   DELETE OBJECT hdlApplicationRecord NO-ERROR.
   DELETE OBJECT hdlFieldRecord       NO-ERROR.

END. /*MainBlk: DO TRANSACTION ON ERROR UNDO, LEAVE:*/
               
IF logGblDebugging THEN
   fLog(" Deletes Complete").

DISCONNECT VALUE(chrSourceDb) NO-ERROR.

IF logGblDebugging THEN
DO:
   fLog(" Disconnected Syngate").
   OUTPUT STREAM sToCronLog CLOSE.
END. /* IF logGblDebugging */

OUTPUT STREAM chrDeleteRec CLOSE.

IF CAN-FIND(FIRST ttHeaders) THEN
DO:
   RUN osSendMail.p (INPUT chrEmailAddress,                              /* Optional list of Users */
                     INPUT "Sync Gate Deletes - Deleted Records File",   /* Email Subject */
                     INPUT "Please See Attached.",                       /* Plain text message Body */
                     INPUT "",                                           /* Html format message Body */
                     INPUT chrFullFilePath,                              /* File path ../files/file */
                     INPUT (IF chrEmailAddress = "" THEN
                              EmailGroup.EmailGroupID
                           ELSE 0),                                      /* EmailGroupID that you want to send this to */
                     INPUT 0).                                           /* File MasterID is it applies */
                     
   fDeleteFile(chrFullFilePath).
   
END. /*IF CAN-FIND(FIRST ttHeaders) THEN*/

RELEASE gate.GateUser      NO-ERROR.
RELEASE CronEvent          NO-ERROR.
RELEASE CronEventRun       NO-ERROR.
RELEASE Config             NO-ERROR.
RELEASE Environment        NO-ERROR.
RELEASE SystemDbAppEnvLink NO-ERROR.
RELEASE Application        NO-ERROR.
RELEASE EmailGroup         NO-ERROR.