/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnWebBrokerReset.p
Purpose : Ran from Cron.  Looks at each WebBroker to see if anyone has used it in the last 30 minutes and stop the broker if it is 
          running but not being used.  Connects to Syngate Gate DB to update the WebBroker.CurrentlyRunning and WebBroker.ChangeRequestNo.   
Author  : Christopher Shelley
Date    : 29th October 2014
---------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncClassFunctions.i}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}
{fncDateFunctions.i}
{fncGlobalFunctions.i}
{fncLockingFunctions.i}

/* Local Variables */
DEFINE VARIABLE chrEmailAddress       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrReturnValue        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID   AS INTEGER      NO-UNDO.
DEFINE VARIABLE logAllWentOK          AS LOGICAL      NO-UNDO.
DEFINE VARIABLE intLastInteraction    AS INTEGER      NO-UNDO.
DEFINE VARIABLE chrConnectionString   AS CHARACTER    NO-UNDO FORMAT "x(40)".
DEFINE VARIABLE chrSystemPostFix      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrDbTable            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrSyngateDb          AS CHARACTER    NO-UNDO.

DEFINE VARIABLE chrUnixCommand        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrUnixResult         AS CHARACTER    NO-UNDO.

DEFINE VARIABLE chrParentDirectory    AS CHARACTER    NO-UNDO FORMAT "X(20)".

DEFINE VARIABLE hdlGateWebBroker      AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlDataQuery          AS HANDLE       NO-UNDO.

DEFINE VARIABLE updWebBroker          AS updRecord.

/* Streams */
DEFINE STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
END FUNCTION.

/* Set the log file destination directory  */
chrNewAgedDirectory = fGetAgedDirectory("../logs/", 2).
IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

chrLogFile = chrLogFileDirectory + "crnWebBrokerReset_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN 
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.

FIND FIRST gate.GateUser NO-LOCK
   WHERE gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

FIND FIRST EmailGroup NO-LOCK /* index GroupCode */
   WHERE EmailGroup.GroupCode = "CronErrors" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.
   
/* The batch job needs to connect to the source Db and target Dbs */
chrSyngateDb = "syngate".

/* IF this is not DEV or QA return */
IF chrGblEnvironment <> "DEV" AND chrGblEnvironment <> "QA" THEN
   RETURN.

/* This will be one of "dev,qa" */
CASE chrGblEnvironment:
   WHEN "DEV"  THEN 
      chrSystemPostFix = "_syngate_dev".   /* Just syngate for Dev    */
   WHEN "QA"   THEN 
      chrSystemPostFix = "_syngate_qa".    /* syngate_qa for Qa       */
END CASE.

/* Connect to "gate" db within the syngate system using alias db name of "syngate" - change the service name according to environment */
chrConnectionString = "-db gate -ld syngate -S " + "rplsrc_syngate" + chrSystemPostFix + " -Mm 4096 -N TCP -n 8 -B 100".

/* Do a straight output with a stream so this will catch all errors including unexpected Progress errors */
IF logGblDebugging THEN
   PUT STREAM sToLogFile UNFORMATTED NOW " Connecting to Syngate:" chrConnectionString + "." SKIP.

/* Connect to "gate" db within the syngate system using alias db name of "syngate" - these connection values should never change */
IF NOT CONNECTED(chrSyngateDB) THEN
   CONNECT VALUE(chrConnectionString) NO-ERROR.

/* Check if Dbs are Connected, won't be during maintenance windows or temporary disconnects, don't want alert emails on downtime */
IF NOT CONNECTED(chrSyngateDb) THEN 
   LEAVE.

/* Create Buffer to access WebBroker on Syngate */
CREATE BUFFER hdlGateWebBroker FOR TABLE "syngate.WebBroker".

/* Query for the Syngate WebBroker Table */
CREATE QUERY hdlDataQuery.
hdlDataQuery:ADD-BUFFER(hdlGateWebBroker) NO-ERROR.
hdlDataQuery:QUERY-PREPARE("FOR EACH WebBroker WHERE WebBroker.ApplicationID = " + STRING(intGblApplicationID) + " NO-LOCK") NO-ERROR.
hdlDataQuery:QUERY-OPEN NO-ERROR.

MainBlock:
DO TRANSACTION ON STOP UNDO, RETURN ERROR:
   WebBrokerLoop:
   REPEAT ON ERROR UNDO WebBrokerLoop, LEAVE WebBrokerLoop:
      hdlDataQuery:GET-NEXT.
      IF hdlDataQuery:QUERY-OFF-END THEN
         LEAVE WebBrokerLoop.
      IF logGblDebugging THEN 
         fLog("Found WebBroker: " + hdlGateWebBroker::BrokerName).
      
      /* Check if any UserSession has been active in the last 30 minutes on the Broker */   
      FOR EACH UserSession NO-LOCK
         WHERE UserSession.ChangeRequestNo = hdlGateWebBroker::ChangeRequestNo
         AND   UserSession.LastInteraction > ""
         AND   UserSession.StatusCode      = "ACTIVE"
         AND   UserSession.ApplicationID   = intGblApplicationID
         AND   UserSession.WebBrokerID     = hdlGateWebBroker::WebBrokerID:
         
         intLastInteraction = fInterval(INPUT fTimeStamp(NOW), INPUT UserSession.LastInteraction, INPUT "minutes").
         IF intLastInteraction <= 30 THEN
         DO:
            IF logGblDebugging THEN 
               fLog(hdlGateWebBroker::BrokerName + " was used in the last 30 minutes by GateUserID: " + STRING(UserSession.GateUserID)).
            NEXT WebBrokerLoop.
         END. /* IF intLastInteraction <= 30 */         
      END. /* FOR EACH UserSession */
      
      IF logGblDebugging THEN 
         fLog("Checking status of Broker: " + hdlGateWebBroker::BrokerName).
         
      /* Check if the web broker is Active */
      IF fGetWebBrokerStatus(INPUT hdlGateWebBroker::BrokerName) = "ACTIVE" THEN
      DO:
         IF logGblDebugging THEN 
            fLog("Trying to stop the broker now.").
         
         /* Prevent logging to the error file while stopping the broker */
         OUTPUT TO VALUE("/dev/null").
         chrUnixResult = fControlWebBroker(INPUT hdlGateWebBroker::BrokerName,
                                           INPUT "-stop").
         OUTPUT CLOSE.
                           
         IF chrUnixResult <> "Ok" THEN
         DO:
            fLog(chrUnixResult).
            RETURN ERROR chrUnixResult.
         END. /* IF chrUnixResult <> "Ok" */  
      END. /* IF chrUnixResult = "ACTIVE" */
      
      hdlGateWebBroker:FIND-CURRENT(EXCLUSIVE-LOCK).
      IF hdlGateWebBroker:AVAILABLE THEN
      DO:
         IF logGblDebugging THEN 
            fLog("Setting WebBroker fields for WebBrokerID: " + hdlGateWebBroker::WebBrokerID). 
         
         /* Disable this trigger as its not on the current propath and will give an error */
         hdlGateWebBroker:DISABLE-LOAD-TRIGGERS(YES).
         hdlGateWebBroker::CurrentlyRunning = FALSE.
         hdlGateWebBroker::ChangeRequestNo  = "".
      END. /* IF hdlGateWebBroker:AVAILABLE */
      
      IF logGblDebugging THEN 
         fLog("Syngate WebBroker BrokerName: " + hdlGateWebBroker::BrokerName + " CurrentlyRunning: " + hdlGateWebBroker::CurrentlyRunning + " ChangeRequestNo: " + hdlGateWebBroker::ChangeRequestNo).
   END. /* REPEAT */
   
   hdlDataQuery:QUERY-CLOSE()     NO-ERROR.
   
   DELETE OBJECT hdlDataQuery     NO-ERROR.
   DELETE OBJECT hdlGateWebBroker   NO-ERROR.
END. /* MainBlock */

DISCONNECT VALUE(chrSyngateDb) NO-ERROR.

IF logGblDebugging THEN
DO:
   PUT STREAM sToLogFile UNFORMATTED NOW " Disconnected Syngate" SKIP.
   OUTPUT STREAM sToLogFile CLOSE.
END. /* IF logGblDebugging */     
  
RELEASE gate.GateUser    NO-ERROR.
RELEASE EmailGroup       NO-ERROR.
RELEASE gate.UserSession NO-ERROR.
RELEASE gate.WebBroker   NO-ERROR.