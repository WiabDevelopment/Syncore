/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnPurgeAuditRecords.p
Purpose : Delete the User Sessions based on a input parameter of days, Audit records older than those
          many days will be purged
Author  : Nick Diessner
Date    : 6th July 2015
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
DEFINE VARIABLE chrEmailAddress       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrProgramName        AS CHARACTER    NO-UNDO INITIAL "crnPurgeAuditRecords.p".
DEFINE VARIABLE chrNewAgedDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intPurgeDays          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intGateUserID         AS INTEGER      NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID   AS INTEGER      NO-UNDO.
DEFINE VARIABLE intAuditCnt           AS INTEGER      NO-UNDO.
DEFINE VARIABLE logLoggingMode        AS LOGICAL      NO-UNDO.
DEFINE VARIABLE logDeleteMode         AS LOGICAL      NO-UNDO.
DEFINE VARIABLE chrEmailLogFile       AS CHARACTER    NO-UNDO.

/* buffers */
DEFINE BUFFER otherAudit                   FOR Audit.

/* Streams */
DEFINE STREAM sToLogFile.
DEFINE STREAM sToEmailFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

ASSIGN intSsnCronEventRunID = fGetSessionValue("CronEventRunID")
       chrSsnPurgeDays      = fGetSessionValue("PurgeDays")
       intPurgeDays         = INTEGER(chrSsnPurgeDays:chrValue)
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

chrLogFile = chrLogFileDirectory + "crnPurgeAuditRecords_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".
chrEmailLogFile  = chrLogFileDirectory + "crnPurgeAuditRecords_email.log".

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
   
FIND FIRST CronEventRun NO-LOCK
   WHERE CronEventRun.CronEventRunID = intSsnCronEventRunID:intValue NO-ERROR.
IF NOT AVAILABLE CronEventRun THEN
   RETURN ERROR "CronEventRun missing for CronEventRunID: " + STRING(intSsnCronEventRunID:intValue).

intGateUserID  = CronEventRun.GateUserID.

FIND FIRST gate.GateUser NO-LOCK
   WHERE gate.GateUser.GateUserID = intGateUserID NO-ERROR.
IF NOT AVAILABLE gate.GateUser THEN
   RETURN ERROR "GateUser missing for GateUserID: " + STRING(intGateUserID).

chrEmailAddress = gate.GateUser.Email.

FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "AuditPurge" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

IF logGblDebugging THEN
   fLog(chrProgramName + " UNIX PID: " + STRING(CronEventRun.UnixPID) + " starting to purge Audit records older than : " 
           + STRING(intPurgeDays)).

Purge_Block:
FOR EACH Audit NO-LOCK
   BY Audit.AuditID:
       
   IF fGetDate(Audit.Created) >= (TODAY - intPurgeDays) THEN
      NEXT Purge_Block.
  
   /* if an aggregate of 100,000 records deleted, then leave */
   IF intAuditCnt > 100000 THEN LEAVE Purge_Block.

  /* Finds the Audit record and deletes it  */
   DO TRANSACTION ON ERROR UNDO, LEAVE:
      FIND otherAudit OF Audit EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAILABLE otherAudit THEN
      DO:
         
          IF logLoggingMode = TRUE THEN
          DO:
             PUT STREAM sToEmailFile UNFORMATTED "Audit Record ID: ". 
             PUT STREAM sToEmailFile UNFORMATTED Audit.AuditID SKIP.
          END.     
          IF logDeleteMode = TRUE THEN 
          DO:   
             DELETE otherAudit NO-ERROR.
          END. /* IF logDeleteMode = TRUE */
         
          intAuditCnt = intAuditCnt + 1.
      END.
   END.
END. /* FOR EACH Audit NO-LOCK */

IF logLoggingMode = TRUE THEN
DO:
   OUTPUT STREAM sToEmailFile CLOSE.
      
   FIND FIRST EmailGroup NO-LOCK
      WHERE EmailGroup.GroupCode = "AuditPurge" NO-ERROR.
   IF AVAILABLE EmailGroup THEN
      intCronEmailGroupID = EmailGroup.EmailGroupID.
   ELSE
      intCronEmailGroupID = 1.

   RELEASE EmailGroup NO-ERROR.

   RUN osSendMail.p (INPUT chrEmailAddress,                                /* Optional list of Users */
                     INPUT "Cron Audit Purge",                              /* Email Subject */
                     INPUT "Number of Audit Records Logged: " + STRING(intAuditCnt), /* Plain text message Body */
                     INPUT "",                                             /* Html format message Body */
                     INPUT chrEmailLogFile,                                /* File path ../files/file */
                     INPUT (IF gate.GateUser.UserName = "cron" THEN
                               intCronEmailGroupID
                            ELSE 0),                                       /* EmailGroupID */
                     INPUT "").                                            /* File MasterID */
END. /* IF logLoggingMode = TRUE */

/* if a total of 200 or more records are deleted, then send out email to group */
IF logDeleteMode = TRUE THEN
DO:
IF intAuditCnt >= 200 THEN
   RUN osSendMail.p (INPUT (IF gate.GateUser.UserName <> "cron" THEN 
                                  chrEmailAddress                                                       /* Optional list of */      
                               ELSE ""),                                                                /* Users */     
                     INPUT "Cron Job ID " + STRING(CronEventRun.CronEventID) + 
                           " Alert - Audit records deleted",                                           /* Email Subject */
                     INPUT "Program: " + chrProgramName + " has deleted Audit "
                              + " records that were more than " + STRING(intPurgeDays) + " days old."
                              + " Audit Records deleted:" + STRING(intAuditCnt), 
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
RELEASE Audit                       NO-ERROR.
RELEASE otherAudit                  NO-ERROR.
