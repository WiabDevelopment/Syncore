/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnPurgeNetworkAutomation.p
Purpose : Delete the Network Automation records based on a input parameter of days, Network Automation records older than those
          many days will be purged
Author  : Nick Diessner
Date    : 11th November 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
/* Standard Mandatory Includes */
{defSessionVariables.i}
{fncClassFunctions.i}
{fncGlobalFunctions.i}
{fncServerFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{fncStatusTypeFunctions.i}

/* Session Variables */
DEFINE VARIABLE chrSsnPurgeDays       AS sessionValue NO-UNDO.
DEFINE VARIABLE chrSsnLoggingMode     AS sessionValue NO-UNDO.
DEFINE VARIABLE chrSsnDeleteMode      AS sessionValue NO-UNDO.
DEFINE VARIABLE intSsnCronEventRunID  AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrProgramName         AS CHARACTER    NO-UNDO INITIAL "crnPurgeNetworkAutomation.p".
DEFINE VARIABLE chrNewAgedDirectory    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intPurgeDays           AS INTEGER      NO-UNDO INITIAL 5.
DEFINE VARIABLE intReadPurgeDays       AS INTEGER      NO-UNDO INITIAL 5.
DEFINE VARIABLE intActionPurgeDays     AS INTEGER      NO-UNDO INITIAL 5.
DEFINE VARIABLE intDecisonPurgeDays    AS INTEGER      NO-UNDO INITIAL 5.
DEFINE VARIABLE intGateUserID          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID    AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNetworkReadCnt      AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNetworkActionCnt    AS INTEGER      NO-UNDO.
DEFINE VARIABLE intToteLineDecisionCnt AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNetworkAutoCnt      AS INTEGER      NO-UNDO.
DEFINE VARIABLE logLoggingMode         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE logDeleteMode          AS LOGICAL      NO-UNDO.
DEFINE VARIABLE chrEmailLogFile        AS CHARACTER    NO-UNDO.

/* buffers */
DEFINE BUFFER otherNetworkRead                   FOR NetworkRead.
DEFINE BUFFER otherNetworkAction                 FOR NetworkAction.
DEFINE BUFFER otherToteLineDecision              FOR ToteLineDecision.

/* Streams */
DEFINE STREAM sToLogFile.
DEFINE STREAM sToEmailFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

ASSIGN intSsnCronEventRunID = fGetSessionValue("CronEventRunID")
       chrSsnPurgeDays      = fGetSessionValue("PurgeDays")
/*       intPurgeDays         = INTEGER(chrSsnPurgeDays:chrValue)*/ /* Put back in if a number of days is decided*/
       chrSsnLoggingMode    = fGetSessionValue("LogMode")
       logLoggingMode       = LOGICAL(chrSsnLoggingMode:chrValue) 
       chrSsnDeleteMode     = fGetSessionValue("DeleteMode")
       logDeleteMode        = LOGICAL(chrSsnDeleteMode:chrValue)
       chrNewAgedDirectory  = fGetAgedDirectory("../logs/", 2) NO-ERROR.
       
       
IF ERROR-STATUS:ERROR THEN
   RETURN ERROR "Could not setup variables "  + RETURN-VALUE.

IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

chrLogFile = chrLogFileDirectory + "crnPurgeNetworkAutomation_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".
chrEmailLogFile  = chrLogFileDirectory + "crnPurgeNetworkAutomation_email.log".

IF logGblDebugging THEN 
DO:
   /* Setup log files */
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
   
   fLog("Debug enabled now running CronEventRun: " + STRING(intSsnCronEventRunID:intValue)).
END. /* IF logGblDebugging */

IF logLoggingMode = TRUE THEN
   OUTPUT STREAM sToEmailFile TO VALUE(chrEmailLogFile).

IF intPurgeDays = 0 THEN
   intPurgeDays = 100.
   
FIND FIRST CronEventRun NO-LOCK /*idx=CronEventRunID*/
   WHERE CronEventRun.CronEventRunID = intSsnCronEventRunID:intValue NO-ERROR.
IF NOT AVAILABLE CronEventRun THEN
   RETURN ERROR "CronEventRun missing for CronEventRunID: " + STRING(intSsnCronEventRunID:intValue).

intGateUserID  = CronEventRun.GateUserID.

FIND FIRST gate.GateUser NO-LOCK /*idx=GateUserID*/
   WHERE gate.GateUser.GateUserID = intGateUserID NO-ERROR.
IF NOT AVAILABLE gate.GateUser THEN
   RETURN ERROR "GateUser missing for GateUserID: " + STRING(intGateUserID).

chrEmailAddress = gate.GateUser.Email.

FIND FIRST EmailGroup NO-LOCK /*idx=GroupCode*/
   WHERE EmailGroup.GroupCode = "NetworkAutomationPurge" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

IF logGblDebugging THEN
   fLog(chrProgramName + " UNIX PID: " + STRING(CronEventRun.UnixPID) + " starting to purge Network Automation records older than : " 
           + STRING(intPurgeDays)).

/* Loop for all Network read records */
NetworkRead_Block:
FOR EACH NetworkRead NO-LOCK
   BY NetworkRead.NetworkReadID:
       
   IF fGetDate(NetworkRead.Created) >= (TODAY - intPurgeDays) THEN
      NEXT NetworkRead_Block.
  
  /* Finds the Audit record and deletes it  */
   DO TRANSACTION ON ERROR UNDO, LEAVE:
      FIND otherNetworkRead OF NetworkRead EXCLUSIVE-LOCK NO-WAIT NO-ERROR. /*idx=NetworkReadID*/
      IF AVAILABLE otherNetworkRead THEN
      DO:
          IF logLoggingMode = TRUE THEN
          DO:
             PUT STREAM sToEmailFile UNFORMATTED "Network Read Record ID: ". 
             PUT STREAM sToEmailFile UNFORMATTED NetworkRead.NetworkReadID SKIP.
          END.     
          IF logDeleteMode = TRUE THEN 
          DO:   
             DELETE otherNetworkRead NO-ERROR.
          END. /* IF logDeleteMode = TRUE */
         
          intNetworkReadCnt = intNetworkReadCnt + 1.
          intNetworkAutoCnt = intNetworkAutoCnt + 1.
      END. /*IF AVAILABLE otherNetworkRead THEN*/
   END.
END. /* FOR EACH NetworkRead NO-LOCK */

/* Loop through all the Network Action records*/
NetworkAction_Block:
FOR EACH NetworkAction NO-LOCK
   BY NetworkAction.NetworkActionID:
       
   IF fGetDate(NetworkAction.Created) >= (TODAY - intPurgeDays) THEN
      NEXT NetworkAction_Block.
  
  /* Finds the Network Action record and deletes it  */
   DO TRANSACTION ON ERROR UNDO, LEAVE:
      FIND otherNetworkAction OF NetworkAction EXCLUSIVE-LOCK NO-WAIT NO-ERROR. /*idx=NetworkAction*/
      IF AVAILABLE otherNetworkAction THEN
      DO:
          IF logLoggingMode = TRUE THEN
          DO:
             PUT STREAM sToEmailFile UNFORMATTED "Network Action Record ID: ". 
             PUT STREAM sToEmailFile UNFORMATTED NetworkAction.NetworkActionID SKIP.
          END.     
          IF logDeleteMode = TRUE THEN 
          DO:   
             DELETE otherNetworkAction NO-ERROR.
          END. /* IF logDeleteMode = TRUE */
         
          intNetworkActionCnt = intNetworkActionCnt + 1.
          intNetworkAutoCnt = intNetworkAutoCnt + 1.
      END. /*IF AVAILABLE otherNetworkAction THEN*/
   END.
END. /* FOR EACH NetworkRead NO-LOCK */

/* Loop through all the tote line decisions */
ToteLineDecision_Block:
FOR EACH ToteLineDecision NO-LOCK
   BY ToteLineDecision.ToteLineDecisionID:
       
   IF fGetDate(ToteLineDecision.Created) >= (TODAY - intPurgeDays) THEN
      NEXT ToteLineDecision_Block.
  
  /* Finds the Network Action record and deletes it  */
   DO TRANSACTION ON ERROR UNDO, LEAVE:
      FIND otherToteLineDecision OF ToteLineDecision EXCLUSIVE-LOCK NO-WAIT NO-ERROR. /*idx=ToteLineDecision*/
      IF AVAILABLE otherToteLineDecision THEN
      DO:
          IF logLoggingMode = TRUE THEN
          DO:
             PUT STREAM sToEmailFile UNFORMATTED "ToteLine Decision Record ID: ". 
             PUT STREAM sToEmailFile UNFORMATTED ToteLineDecision.ToteLineDecisionID SKIP.
          END.     
          IF logDeleteMode = TRUE THEN 
          DO:   
             DELETE otherToteLineDecision NO-ERROR.
          END. /* IF logDeleteMode = TRUE */
         
          intToteLineDecisionCnt = intToteLineDecisionCnt + 1.
          intNetworkAutoCnt = intNetworkAutoCnt + 1.
      END. /*IF AVAILABLE otherToteLineDecision THEN*/
   END.
END. /* FOR EACH NetworkRead NO-LOCK */

/*Send email with log file if Logging is set to Yes */
IF logLoggingMode = TRUE THEN
DO:
   OUTPUT STREAM sToEmailFile CLOSE.
      
   FIND FIRST EmailGroup NO-LOCK
      WHERE EmailGroup.GroupCode = "NetworkAutomationPurge" NO-ERROR.
   IF AVAILABLE EmailGroup THEN
      intCronEmailGroupID = EmailGroup.EmailGroupID.
   ELSE
      intCronEmailGroupID = 1.

   RELEASE EmailGroup NO-ERROR.

   RUN osSendMail.p (INPUT chrEmailAddress,                                                                  /* Optional list of Users */
                     INPUT "Cron Network Automation Purge",                                                  /* Email Subject */
                     INPUT " Number of Network Automation Records Logged: " + STRING(intNetworkAutoCnt) + 
                           ". Network Read Records Logged: " + STRING(intNetworkreadCnt) + 
                           ". Network Action Records Logged: " + STRING(intNetworkActionCnt) + 
                           ". ToteLine Decision Records Logged: " + STRING(intToteLineDecisionCnt),          /* Plain text message Body */
                     INPUT "",                                                                               /* Html format message Body */
                     INPUT chrEmailLogFile,                                                                  /* File path ../files/file */
                     INPUT (IF gate.GateUser.UserName = "cron" THEN
                               intCronEmailGroupID
                            ELSE 0),                                                                         /* EmailGroupID */
                     INPUT "").                                                                              /* File MasterID */
END. /* IF logLoggingMode = TRUE */

/* if a total of 200 or more records are deleted, then send out email to group */
IF logDeleteMode = TRUE THEN
DO:
IF intNetworkAutoCnt >= 200 THEN /* Not sure if we need this but its staying in right now*/
   RUN osSendMail.p (INPUT (IF gate.GateUser.UserName <> "cron" THEN 
                                  chrEmailAddress                                                       /* Optional list of */      
                               ELSE ""),                                                                /* Users */     
                     INPUT "Cron Job ID " + STRING(CronEventRun.CronEventID) + 
                           " Alert - Network Automation records deleted",                               /* Email Subject */
                     INPUT "Program: " + chrProgramName + " has deleted Network Automation "
                              + " records that were more than " + STRING(intPurgeDays) + " days old."
                              + " Network Automation Records deleted:" + STRING(intNetworkAutoCnt), 
                     INPUT "",                                                                          /* Html format message Body */
                     INPUT "",                                                                          /* File path ../files/file */                    
                     INPUT (IF gate.GateUser.UserName = "cron" THEN                                     
                               intCronEmailGroupID                                                      /* EmailGroupID that you */
                            ELSE 0),                                                                    /* want to send this to */
                     INPUT "").                                                                         /* File MasterID is it */
                     
END. /* IF logDeleteMode = TRUE */                 

IF logGblDebugging THEN
DO:
   fLog("Finished.").
   OUTPUT STREAM sToLogFile CLOSE.
END. /* IF logGblDebugging */

DELETE OBJECT chrSsnPurgeDays      NO-ERROR.
DELETE OBJECT intSsnCronEventRunID NO-ERROR.

RELEASE gate.GateUser               NO-ERROR.
RELEASE CronEvent                   NO-ERROR.
RELEASE CronEventRun                NO-ERROR.
RELEASE EmailGroup                  NO-ERROR.
RELEASE ProcessProgram              NO-ERROR.
RELEASE NetworkAction               NO-ERROR.
RELEASE otherNetworkAction          NO-ERROR.
RELEASE ToteLineDecision            NO-ERROR.
RELEASE otherToteLineDecision       NO-ERROR.
RELEASE NetworkRead                 NO-ERROR.
RELEASE otherNetworkRead            NO-ERROR.
