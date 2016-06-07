/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnPurgeUserSessions.p
Purpose : Delete the User Sessions based on a input parameter of days, UserSession/UserSessionCrumb records older than those
          many days will be purged
Author  : Ashwin Baliga
Date    : 26th March 2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
10/04/2014 CS             V11 required changes.
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
DEFINE VARIABLE intSsnCronEventRunID  AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrProgramName        AS CHARACTER    NO-UNDO INITIAL "crnPurgeUserSessions.p".
DEFINE VARIABLE chrNewAgedDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intPurgeDays          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intGateUserID         AS INTEGER      NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID   AS INTEGER      NO-UNDO.
DEFINE VARIABLE intSessionCnt         AS INTEGER      NO-UNDO.
DEFINE VARIABLE intCrumbCnt           AS INTEGER      NO-UNDO.
DEFINE VARIABLE intBusUnitLinkCnt     AS INTEGER      NO-UNDO.

/* buffers */
DEFINE BUFFER otherUserSession            FOR UserSession.
DEFINE BUFFER otherUserSessionCrumb       FOR UserSessionCrumb.
DEFINE BUFFER otherUserSessionBusUnitLink FOR UserSessionBusUnitLink.

/* Streams */
DEFINE STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

ASSIGN intSsnCronEventRunID = fGetSessionValue("CronEventRunID")
       chrSsnPurgeDays      = fGetSessionValue("PurgeDays")
       intPurgeDays         = INTEGER(chrSsnPurgeDays:chrValue)
       chrNewAgedDirectory  = fGetAgedDirectory("../logs/", 2) NO-ERROR.
   
IF ERROR-STATUS:ERROR THEN
   RETURN ERROR "Could not setup variables "  + RETURN-VALUE.

IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

chrLogFile = chrLogFileDirectory + "crnPurgeUserSessions_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN 
DO:
   /* Setup log files */
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
   
   fLog("Debug enabled now running CronEventRun: " + STRING(intSsnCronEventRunID:intValue)).
END. /* IF logGblDebugging */

IF intPurgeDays = 0 THEN
   intPurgeDays = 200.
   
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
   WHERE EmailGroup.GroupCode = "FilePurge" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

IF logGblDebugging THEN
   fLog(chrProgramName + " UNIX PID: " + STRING(CronEventRun.UnixPID) + " starting to purge UserSession/UserSessionCrumb/UserSessionBusUnitLink records older than : " 
           + STRING(intPurgeDays)).

Purge_Block:
FOR EACH UserSession NO-LOCK
   BY UserSession.SessionID:
  
   /* if an aggregate of 100,000 records deleted, then leave */
   IF intCrumbCnt + intBusUnitLinkCnt + intSessionCnt > 100000 THEN LEAVE Purge_Block.

   /* Find records that are older than criteria below and delete them */
   IF ((DATE(UserSession.LoggedIn) <= (TODAY - intPurgeDays)) OR UserSession.LoggedIn = ?) THEN
   DO:
      /* Try and delete all the UserSessionCrumbs for the UserSession */
      FOR EACH UserSessionCrumb OF UserSession NO-LOCK:

        DO TRANSACTION ON ERROR UNDO, LEAVE:
           FIND otherUserSessionCrumb OF UserSessionCrumb EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
           IF AVAILABLE otherUserSessionCrumb THEN
           DO:
              DELETE otherUserSessionCrumb.
              intCrumbCnt = intCrumbCnt + 1.
           END.
        END.

      END. /* FOR EACH UserSessionCrumb */

      /* Try and delete all the UserSessionBusUnitLinks for the UserSession */
      FOR EACH UserSessionBusUnitLink OF UserSession NO-LOCK:

        DO TRANSACTION ON ERROR UNDO, LEAVE:
           FIND otherUserSessionBusUnitLink OF UserSessionBusUnitLink EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
           IF AVAILABLE otherUserSessionBusUnitLink THEN
           DO:
              DELETE otherUserSessionBusUnitLink.
              intBusUnitLinkCnt = intBusUnitLinkCnt + 1.
           END.
        END.

      END. /* FOR EACH UserSessionBusUnitLink */

      /* If no UserSessionCrumbs and no UserSessionBusUnitLinks found, then try and delete the UserSession */
      IF NOT CAN-FIND(FIRST UserSessionCrumb OF UserSession) AND 
         NOT CAN-FIND(FIRST UserSessionBusUnitLink OF UserSession)THEN
      DO TRANSACTION ON ERROR UNDO, LEAVE:
         FIND otherUserSession OF UserSession EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF AVAILABLE otherUserSession THEN
         DO:
            DELETE otherUserSession.
            intSessionCnt = intSessionCnt + 1.
         END.
      END. /* IF NOT CAN-FIND(FIRST */
   END.
END. /* FOR EACH UserSession NO-LOCK */

/* if a total of 200 or more records are deleted, then send out email to group */
IF intSessionCnt >= 200 THEN
   RUN osSendMail.p (INPUT (IF gate.GateUser.UserName <> "cron" THEN 
                                  chrEmailAddress                                                       /* Optional list of */      
                               ELSE ""),                                                                /* Users */     
                     INPUT "Cron Job ID " + STRING(CronEventRun.CronEventID) + 
                           " Alert - UserSession/UserSessionCrumb records deleted",                                                          /* Email Subject */
                     INPUT "Program: " + chrProgramName + " has deleted UserSession/UserSessionCrumb "
                              + " records that were more than " + STRING(intPurgeDays) + " days old."
                              + " UserSession Records deleted:" + STRING(intSessionCnt) 
                              + " UserSessionCrumb Records deleted:" + STRING(intCrumbCnt)
                              + " UserSessionBusUnitLink Records deleted:" + string(intBusUnitLinkCnt),
                     INPUT "",                                                                          /* Html format message Body */
                     INPUT "",                                                                          /* File path ../files/file */                    
                     INPUT (IF gate.GateUser.UserName = "cron" THEN                                     
                               intCronEmailGroupID                                                      /* EmailGroupID that you */
                            ELSE 0),                                                                    /* want to send this to */
                     INPUT "").                                                                         /* File MasterID is it */

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
RELEASE UserSession                 NO-ERROR.
RELEASE UserSessionCrumb            NO-ERROR.
RELEASE UserSessionBusUnitLink      NO-ERROR.
RELEASE otherUserSession            NO-ERROR.
RELEASE otherUserSessionCrumb       NO-ERROR.
RELEASE otherUserSessionBusUnitLink NO-ERROR.