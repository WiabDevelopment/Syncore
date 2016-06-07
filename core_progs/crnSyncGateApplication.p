/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnSyncGateApplication.p
Purpose : Is initially connected to the local system. Connects to the global syngate system and then syncs the data between the 2 systems.
          First it finds any Translations that have been created locally due to a code release or use of a new area of the system since the
          last Db sync. If it finds any it writes them to the syngate Db. After this is complete it goes through every table in the syngate 
          Db and updates the local Db to match the vales.
Author  : BG
Date    : 17th May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
06/01/2014 CS  CR1021     Added ability to run this from the web.
09/01/2014 CS  CR1021     Added some commented code back in.  Changed hdlApplicationRecord buffer to use syngate instead of variable.  This
                          is to attempt to fix the 7334 Error that is generated eventually from the manually ran job.
18/02/2014 CS  CR1021     Added stream and removed the File disconnects.   
20/02/2014 CS  CR1021     Changed connection string and removed no-error on buffer copy.             
30/03/2014 BG  Syngate    Changed to accommodate new Field naming and service name format on live.
04/03/2014 AB  Syncore    If run for syncore copy entire application table data into syncore from syngate
06/03/2014 CS  CR1021     Changed program name.
06/27/2014 AB  Syngate    Added condition to sync for ExternalApplicationID if specified for an Application.
10/02/2014 AB  Syngate    Disable the triggers on the gate db of the Application
15/12/2014 BG  Nextel     Added -H syngate to cater for WiaB instances that are on a different Server to syngate which is on dub1wms3
17/09/2015 SH  GoPro      Rewrite for GoPro to speed up Connections
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i}
{fncLockingFunctions.i}
{fncGlobalFunctions.i}   
{fncDateFunctions.i}   
{fncServerFunctions.i}   

/*Temp Tables*/
DEFINE TEMP-TABLE ttUniqueVersionsToUpdate
   FIELD UniqueID   AS INTEGER
   FIELD VersionID  AS INTEGER
   FIELD TableName  AS CHARACTER.

/* UNDO Vars */
DEFINE VARIABLE intApplicationID           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intExternalApplicationID   AS INTEGER     NO-UNDO.
DEFINE VARIABLE hdlTableName               AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlTableQuery              AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlDataQuery               AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlSourceDbTable           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlTargetDbTable           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlApplicationRecord       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlGateConfigRecord        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlFieldRecord             AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlUniqueField             AS HANDLE      NO-UNDO.
DEFINE VARIABLE chrDbTable                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSourceDb                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTargetDb                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCoreDb                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrHostName                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTableWhere              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrWhereClause             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrUniqueValue             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrErrorOut                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logFound                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLe logFoundSource             AS LOGICAL     NO-UNDO.
DEFINE VARIABLe logFoundTarget             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE chrSystemPostFix           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrEnglishString           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTimestamp               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLogFilePath             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLogFile                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrConnectionString        AS CHARACTER   NO-UNDO FORMAT "x(40)".
DEFINE VARIABLE intCronEventRunID          AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSourceVersionID         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTargetVersionID         AS CHARACTER   NO-UNDO.

/* NO-UNDO Vars */
DEFINE VARIABLE intNumTranslations         AS INTEGER.
DEFINE VARIABLE logApplicationSyncRequired AS LOGICAL.

/* Buffers */
DEFINE BUFFER deleteLocalTranslationSource FOR gate.TranslationSource.

/* Streams */
DEFINE STREAM sToCronLog.

/* The batch job needs to connect to the source Db and target Dbs */
ASSIGN chrSourceDb       = "syngate"
       chrTargetDb       = "gate"
       chrCoreDb         = "core"
       chrTableWhere     = "FOR EACH _file WHERE _file._tbl-type = 'T'".

/* This will be one of "dev,test,uat,sit,live" */

/* This will be one of "dev,test,uat,sit,live" */
CASE chrGblEnvironment:
   WHEN "DEV"  THEN 
      chrSystemPostFix = "_syngate_dev".   /* Just syngate for Dev    */
   WHEN "QA"   THEN 
      chrSystemPostFix = "_syngate_qa".    /* syngate_qa for Qa       */
   WHEN "UAT"  THEN 
      chrSystemPostFix = "_syngate_uat".   /* syngate_uat for Uat     */
   WHEN "SIT"  THEN
      chrSystemPostFix = "_syngate_sit".   /* syngate_sit for Sit     */
   WHEN "LIVE" THEN 
      chrSystemPostFix = "_syngate_live".  /* syngate_live for Live     */
END CASE.

/* Need this in the interim for host resolution between old server(dubdev) and new server(dubdev/qa/uat/sit) */
IF chrGblEnvironment = "LIVE" THEN 
   chrHostName = "syngate".
ELSE 
   chrHostName = "dubdev".

/* First disconnect the 1 Db that we don't need - that will leave us connected to "core", "gate", "file", and "temp" from the */
/* local syncore application                                                                                           */
DISCONNECT "cust" NO-ERROR.

/* Connect to "gate" db within the syngate system using alias db name of "syngate" - change the service name according to environment */
chrConnectionString = "-db gate -ld syngate -H " + chrHostName + " -S " + "rplsrc_syngate" + chrSystemPostFix + " -Mm 4096 -N TCP -n 8 -B 100".

/* Set the log file destination directory  */
chrLogFilePath = fGetAgedDirectory("../logs/", 90).
IF chrLogFilePath BEGINS "Error" THEN
   chrLogFilePath = "../logs/".

ASSIGN chrTimeStamp = fTimestamp(NOW)
       chrLogFile   = "SyngateSync_" + fDisplayDate&Time(chrTimeStamp,"m_y") + ".log"
       chrLogFile   = chrLogFilePath + chrLogFile.
    
/* Do a straight output with a stream so this will catch all errors including unexpected Progress errors */
IF logGblDebugging THEN DO:
   OUTPUT STREAM sToCronLog TO VALUE(chrLogFile) APPEND.
   PUT STREAM sToCronLog UNFORMATTED NOW " Connecting to Syngate:" chrConnectionString + "." SKIP.
END. /* IF logGblDebugging */

/* Connect to "gate" db within the syngate system using alias db name of "syngate" - these connection values should never change */
IF NOT CONNECTED(chrSourceDb) THEN
   CONNECT VALUE(chrConnectionString) NO-ERROR.

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
       chrTimestamp             = fTimestamp(NOW).

/* Check if there's any new Data for this Application to download from syngate */
CREATE BUFFER hdlApplicationRecord FOR TABLE "syngate.Application".
logFound = hdlApplicationRecord:FIND-UNIQUE("WHERE ApplicationID = '" + STRING(intApplicationID) + "'", NO-LOCK) NO-ERROR.
IF logFound THEN
DO:
   /* Only set it if it hasn't already been set by Uploading translations above */
   IF logApplicationSyncRequired <> TRUE THEN
      logApplicationSyncRequired = (hdlApplicationRecord:BUFFER-FIELD("ApplicationSyncRequired"):BUFFER-VALUE = YES).
END. /* IF logFound */

IF logApplicationSyncRequired <> TRUE THEN
DO:
   IF logGblDebugging THEN
      PUT STREAM sToCronLog UNFORMATTED NOW " Nothing to Download." SKIP.

   DISCONNECT VALUE(chrSourceDb) NO-ERROR.
   RETURN.
END. /* IF logApplicationSyncRequired <> TRUE */
/* END Check if there's any new Data to download from syngate */

/* Create Buffer for _field hidden table */
CREATE BUFFER hdlFieldRecord FOR TABLE chrSourceDb + "._field".

/* Dynamic Query for the _file table so we can prefix table with DbName - go through all the Tables one by one and download data */
CREATE QUERY hdlTableQuery.
CREATE BUFFER hdlTableName FOR TABLE chrSourceDb + "._file".
hdlTableQuery:ADD-BUFFER(hdlTableName) NO-ERROR.
hdlTableQuery:QUERY-PREPARE(chrTableWhere) NO-ERROR.
hdlTableQuery:QUERY-OPEN NO-ERROR.

IF logGblDebugging THEN
   PUT STREAM sToCronLog UNFORMATTED NOW " Before MainBlk." SKIP.

MainBlk:
DO TRANSACTION ON ERROR UNDO MainBlk, LEAVE MainBlk:

   TableLoop:
   REPEAT ON ERROR UNDO MainBlk, LEAVE MainBlk:

      hdlTableQuery:GET-NEXT.
      IF hdlTableQuery:QUERY-OFF-END THEN LEAVE.

      /* The _file-name here is the actual Db Table name */
      ASSIGN chrDbTable = hdlTableName:BUFFER-FIELD("_file-name"):BUFFER-VALUE.

      /* Don't want to copy down Session info as this will be handled locally */
      IF chrDbTable = "UserSession" THEN
        NEXT TableLoop.
      IF chrDbTable = "UserSessionBusUnitLink" THEN
        NEXT TableLoop.
      IF chrDbTable = "UserSessionCrumb" THEN
        NEXT TableLoop.
      IF chrDbTable = "GateConfig" THEN
        NEXT TableLoop.
      IF chrDbTable = "GateSetting" THEN
        NEXT TableLoop.
      IF chrDbTable = "AdminTable" THEN
        NEXT TableLoop.
      IF INDEX(chrDbTable, "History", 1) > 0 THEN
        NEXT TableLoop.
      IF INDEX(chrDbTable, "Hist", 1) > 0 THEN
        NEXT TableLoop.
        
      IF logGblDebugging THEN
         PUT STREAM sToCronLog UNFORMATTED chrDbTable SKIP.
      
      /* Only transfer Translations if on Live */
      IF chrDbTable BEGINS "Translation" AND chrGblEnvironment <> "LIVE" THEN
         NEXT TableLoop.

      /* If the Table has an ApplicationID field then add it to the where clause so we only download data for a system's Application */
      chrWhereClause = "FOR EACH " + chrDbTable + " FIELDS(" + chrDbTable + "ID VersionID) NO-LOCK".
      logFound = hdlFieldRecord:FIND-FIRST("WHERE _file-recid = '" + STRING(hdlTableName:RECID) + "'" 
                                           + "AND _field-name = 'ApplicationID'", NO-LOCK) NO-ERROR.      

      IF logFound THEN
      DO:
         chrWhereClause = IF intApplicationID = 2 THEN /* SYNCORE Copy all the data */
                             chrWhereClause
                          ELSE chrWhereClause + " WHERE ApplicationID = '" + STRING(intApplicationID) + "'".
      
         IF intExternalApplicationID <> 0 THEN 
            chrWhereClause = chrWhereClause + " OR ApplicationID = '" + STRING(intExternalApplicationID) + "'".
      END.
      
      CREATE BUFFER hdlSourceDbTable FOR TABLE chrSourceDb + "." + chrDbTable.

      IF NOT VALID-HANDLE(hdlSourceDbTable) THEN
         NEXT TableLoop.

      hdlUniqueField = hdlSourceDbTable:BUFFER-FIELD(chrDbTable + "ID").

      /* Query for the Source Db table */
      CREATE QUERY hdlDataQuery.
      hdlDataQuery:ADD-BUFFER(hdlSourceDbTable) NO-ERROR.
      hdlDataQuery:QUERY-PREPARE(chrWhereClause) NO-ERROR.
      hdlDataQuery:QUERY-OPEN NO-ERROR.

      TranslationBlk:
      REPEAT ON ERROR UNDO MainBlk, LEAVE MainBlk:

         hdlDataQuery:GET-NEXT.
         IF hdlDataQuery:QUERY-OFF-END THEN LEAVE.

         /* Get unique value */
         chrUniqueValue = STRING(hdlUniqueField:BUFFER-VALUE).

         CREATE ttUniqueVersionsToUpdate.
         ASSIGN ttUniqueVersionsToUpdate.VersionID = INTEGER(hdlSourceDbTable:BUFFER-FIELD("VersionID"):BUFFER-VALUE)
                ttUniqueVersionsToUpdate.UniqueID  = INTEGER(chrUniqueValue)
                ttUniqueVersionsToUpdate.TableName = chrDbTable.   
      END. /* REPEAT ON ERROR UNDO MainBlk, LEAVE MainBlk: */ 
      
      
      UpdateBlk:
      FOR EACH ttUniqueVersionsToUpdate NO-LOCK
         WHERE ttUniqueVersionsToUpdate.TableName = chrDbTable:
   
         CREATE BUFFER hdlTargetDbTable FOR TABLE chrTargetDb + "." + ttUniqueVersionsToUpdate.TableName. 
         
         IF logGblDebugging THEN
            PUT STREAM sToCronLog UNFORMATTED "Table: " + STRING(ttUniqueVersionsToUpdate.TableName) SKIP.

         IF logGblDebugging THEN
            PUT STREAM sToCronLog UNFORMATTED "UniqueID: " + STRING(ttUniqueVersionsToUpdate.UniqueID)
                                              " VersionID: " + STRING(ttUniqueVersionsToUpdate.VersionID) SKIP.                                                 
   
         logFoundTarget = hdlTargetDbTable:FIND-UNIQUE("WHERE " + ttUniqueVersionsToUpdate.TableName + "ID = " 
                                                   + STRING(ttUniqueVersionsToUpdate.UniqueID) + " AND " 
                                                   + "VersionID = " + STRING(ttUniqueVersionsToUpdate.VersionID), NO-LOCK) NO-ERROR.
         IF logFoundTarget THEN
            NEXT UpdateBlk.
   
   
         /*CATOR FOR 0 IF 0 THEN DO I COPY OR LEAVE ALONE OR SHOULD 0 NOT EVEN BE ON ANY VersionID's?*/                                     
    
         logFoundTarget = hdlTargetDbTable:FIND-UNIQUE("WHERE " + ttUniqueVersionsToUpdate.TableName + "ID = " 
                                                          + STRING(ttUniqueVersionsToUpdate.UniqueID), EXCLUSIVE-LOCK, NO-WAIT) NO-ERROR.
   
         IF logFoundTarget <> TRUE THEN
         DO ON ERROR UNDO UpdateBlk, NEXT UpdateBlk:
            
            IF hdlTargetDbTable:LOCKED THEN
            DO:
   
              logFound = hdlTargetDbTable:FIND-UNIQUE("WHERE " + ttUniqueVersionsToUpdate.TableName + "ID = " 
                                                      + STRING(ttUniqueVersionsToUpdate.UniqueID), NO-LOCK, NO-WAIT) NO-ERROR.
              IF logFound THEN
                 chrErrorOut = fGetLockingInfo(INPUT INTEGER(hdlTargetDbTable:RECID),
                                               INPUT STRING(ttUniqueVersionsToUpdate.TableName),
                                               INPUT STRING(ttUniqueVersionsToUpdate.UniqueID)) + " - please try later.".
                 chrErrorOut = chrErrorOut + ttUniqueVersionsToUpdate.TableName + " Record for Update:" + STRING(ttUniqueVersionsToUpdate.UniqueID) + " is Locked by another User.".
              
              IF logGblDebugging THEN
                 PUT STREAM sToCronLog UNFORMATTED NOW " Locked:" chrErrorOut SKIP.
      
              UNDO UpdateBlk, NEXT UpdateBlk.
            END.
         END. /*IF hdlTargetDbTable:LOCKED THEN*/
   
   
         hdlTargetDbTable:DISABLE-LOAD-TRIGGERS(YES).
         
         IF logFoundTarget THEN
         DO:
            CREATE BUFFER hdlSourceDbTable FOR TABLE chrSourceDb + "." + ttUniqueVersionsToUpdate.TableName. 
   
            logFoundSource = hdlSourceDbTable:FIND-UNIQUE("WHERE " + ttUniqueVersionsToUpdate.TableName + "ID = " 
                                                             + STRING(ttUniqueVersionsToUpdate.UniqueID), NO-LOCK) NO-ERROR.
                                                             
            IF logFoundSource THEN
            DO:  
               /* Overwrite old target Db record with new source Db record - will create & write a new target Db record if one wasn't found  */
               /* The BUFFER-COPY excludes any fields that might be in the source Db table but aren't yet in the target Db table             */
               CASE ttUniqueVersionsToUpdate.TableName:
                  WHEN "Application" THEN
                  DO:
                     hdlTargetDbTable:BUFFER-COPY(hdlSourceDbTable, "AppPath").
                  END.
                  OTHERWISE
                  DO:
                     hdlTargetDbTable:BUFFER-COPY(hdlSourceDbTable).
                  END.
               END CASE. 
   
               hdlTargetDbTable:BUFFER-VALIDATE().
               hdlTargetDbTable:BUFFER-RELEASE().
   
               /* Need to sync the sequences also, only really applies for TranslationSource table but best to have them all synced */
               DYNAMIC-CURRENT-VALUE(ttUniqueVersionsToUpdate.TableName, chrTargetDb) = DYNAMIC-CURRENT-VALUE(ttUniqueVersionsToUpdate.TableName, chrSourceDb).
            END. 
            ELSE
            DO:
               IF logGblDebugging THEN
                  PUT STREAM sToCronLog UNFORMATTED NOW "Unable to Find Source Record While Updating" SKIP.
            END.
      
         END. /*IF logFound THEN*/
         ELSE
         DO:
              CREATE BUFFER hdlSourceDbTable FOR TABLE chrSourceDb + "." + ttUniqueVersionsToUpdate.TableName. 
      
              logFoundSource = hdlSourceDbTable:FIND-UNIQUE("WHERE " + ttUniqueVersionsToUpdate.TableName + "ID = " 
                                                               + STRING(ttUniqueVersionsToUpdate.UniqueID), NO-LOCK) NO-ERROR.
              IF logFoundSource THEN
              DO:
                 /* Overwrite old target Db record with new source Db record - will create & write a new target Db record if one wasn't found  */
                 /* The BUFFER-COPY excludes any fields that might be in the source Db table but aren't yet in the target Db table             */
                 CASE ttUniqueVersionsToUpdate.TableName:
                    WHEN "Application" THEN
                    DO:
                       hdlTargetDbTable:BUFFER-CREATE.
                       hdlTargetDbTable:BUFFER-COPY(hdlSourceDbTable, "AppPath").
                    END.
                    OTHERWISE
                    DO:
                       hdlTargetDbTable:BUFFER-CREATE.
                       hdlTargetDbTable:BUFFER-COPY(hdlSourceDbTable).
                    END.
                 END CASE. 
      
      
               hdlTargetDbTable:BUFFER-VALIDATE().
               hdlTargetDbTable:BUFFER-RELEASE().
      
               /* Need to sync the sequences also, only really applies for TranslationSource table but best to have them all synced */
               DYNAMIC-CURRENT-VALUE(ttUniqueVersionsToUpdate.TableName, chrTargetDb) = DYNAMIC-CURRENT-VALUE(ttUniqueVersionsToUpdate.TableName, chrSourceDb). 
            END.
            ELSE
            DO:
               IF logGblDebugging THEN
                  PUT STREAM sToCronLog UNFORMATTED NOW "Unable to Find Source Record While Creating" SKIP.
            END.
  
         END.  
   
      END. /*FOR EACH ttUniqueVersionsToUpdate NO-LOCK:*/      
      
      EMPTY TEMP-TABLE ttUniqueVersionsToUpdate.
           
         
   END. /*Query Repeat*/      
          
   IF logGblDebugging THEN
      PUT STREAM sToCronLog UNFORMATTED "after query" SKIP.

    /* If no errors then set the download flag to FALSE */
    IF chrErrorOut = "" THEN
    DO ON ERROR UNDO MainBlk, LEAVE MainBlk:

       hdlApplicationRecord:FIND-CURRENT(EXCLUSIVE-LOCK).
       IF hdlApplicationRecord:AVAILABLE THEN
       DO:
          /* Disable this trigger as its not on the current propath and will give an error */
          hdlApplicationRecord:DISABLE-LOAD-TRIGGERS(YES).
          hdlApplicationRecord:BUFFER-FIELD("ApplicationSyncRequired"):BUFFER-VALUE = FALSE.
       END. /* IF hdlApplicationRecord:AVAILABLE */
    END. /* IF chrErrorOut = "" */

    hdlDataQuery:QUERY-CLOSE()     NO-ERROR.
    DELETE OBJECT hdlDataQuery     NO-ERROR.
    DELETE OBJECT hdlSourceDbTable NO-ERROR.
    DELETE OBJECT hdlTargetDbTable NO-ERROR.
    DELETE OBJECT hdlUniqueField   NO-ERROR.

    hdlTableQuery:QUERY-CLOSE()        NO-ERROR.
    DELETE OBJECT hdlTableQuery        NO-ERROR.
    DELETE OBJECT hdlTableName         NO-ERROR.
    DELETE OBJECT hdlApplicationRecord NO-ERROR.
    DELETE OBJECT hdlFieldRecord       NO-ERROR.

END. /*MainBlk: DO TRANSACTION ON ERROR UNDO, LEAVE:*/
               
IF logGblDebugging THEN
   PUT STREAM sToCronLog UNFORMATTED NOW " Download Complete." SKIP.

DISCONNECT VALUE(chrSourceDb) NO-ERROR.

IF logGblDebugging THEN
DO:
   PUT STREAM sToCronLog UNFORMATTED NOW " Disconnected Syngate" SKIP.
   OUTPUT STREAM sToCronLog CLOSE.
END. /* IF logGblDebugging */

RELEASE gate.GateUser NO-ERROR.
RELEASE CronEvent     NO-ERROR.
RELEASE CronEventRun  NO-ERROR.
RELEASE Config        NO-ERROR.
