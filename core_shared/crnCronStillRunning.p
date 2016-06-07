/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnCronStillRunning
Purpose : Finds CronEventRun records that CronEventRunResultTypeID is running and past the set time limit.  Email if the Unix PID is still
          active.  If it is not then set it to halted status and send an email.   
Author  : Christopher Shelley
Date    : 04/12/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/12/2013 CS  CR1021     Created.
09/01/2014 CS  CR1021     Added ability to halt jobs that get stuck without a RunResultTypeID assigned.
26/02/2014 CS  CR1021     Added CronEventRun duration to the email.
12/03/2014 CS  CR1021     Changed CronEventRunID to use Session Variable.
10/04/2014 CS  56693      V11 required changes.
05/27/2014 CS  56693      Added check for halted or still running email.  It will now only email in Live, manual runs, or logging enabled.
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
DEFINE VARIABLE chrSsnTimeLimit       AS sessionValue NO-UNDO.
DEFINE VARIABLE intSsnCronEventRunID  AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrReturnValue        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrUnixPID            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrEmailSubject       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrEmailBody          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrProgramName        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile            AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intTimeLimit          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intGateUserID         AS INTEGER      NO-UNDO.
DEFINE VARIABLE intRunning            AS INTEGER      NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID   AS INTEGER      NO-UNDO.
DEFINE VARIABLE intDuration           AS INTEGER      NO-UNDO.
DEFINE VARIABLE logAllWentOk          AS LOGICAL      NO-UNDO INITIAL TRUE.

/* Db Objects */
DEFINE VARIABLE updCronEventRun       AS updRecord.

/* Streams */
DEFINE STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

ASSIGN intSsnCronEventRunID = fGetSessionValue("CronEventRunID")
       chrSsnTimeLimit      = fGetSessionValue("TimeLimit")
       intTimeLimit         = INTEGER(chrSsnTimeLimit:chrValue)
       intRunning           = fGetTypeID("CronEventRunResult", "Running")
       chrNewAgedDirectory  = fGetAgedDirectory("../logs/", 2) NO-ERROR.
   
IF ERROR-STATUS:ERROR THEN
   RETURN ERROR "Could not setup variables "  + RETURN-VALUE.

IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

chrLogFile = chrLogFileDirectory + "crnCronStillRunning_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN 
DO:
   /* Setup log files */
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
   
   fLog("Debug enabled now running CronEventRun: " + STRING(intSsnCronEventRunID:intValue)).
END. /* IF logGblDebugging */

IF intTimeLimit = 0 THEN
   intTimeLimit = 30.
   
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
   WHERE EmailGroup.GroupCode = "CronErrors" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

Running_Block:
FOR EACH CronEventRun NO-LOCK 
   WHERE (CronEventRun.CronEventRunResultTypeID = intRunning
   OR     CronEventRun.CronEventRunResultTypeID = 0)
   AND    CronEventRun.CronEventRunID           <> intSsnCronEventRunID:intValue:
   
   intDuration = 0.

   IF CronEventRun.Started <> "" AND CronEventRun.Started <> "0" THEN
      intDuration =  fInterval(INPUT fTimeStamp(NOW), INPUT CronEventRun.Started, INPUT "minutes").
      
   IF intDuration > intTimeLimit THEN
   DO: 
      FOR FIRST CronEvent OF CronEventRun NO-LOCK,
         FIRST ProcessProgram OF CronEvent NO-LOCK:
         
         chrProgramName = ProcessProgram.ProgramName.

      END. /* FOR FIRST CronEvent */

      IF fValidPID(INPUT CronEventRun.UnixPID) = FALSE THEN
      DO:
         IF logGblDebugging THEN
            fLog(chrProgramName + " UNIX PID: " + STRING(CronEventRun.UnixPID) + " found still running past timelimit: " 
                    + STRING(intTimeLimit) + " Duration: " + STRING(intDuration)).

         /* Get CronEventRun Record  */
         updCronEventRun = fGetRecord("CronEventRun", CronEventRun.CronEventRunID).
         updCronEventRun:assignField("FinishedOk", FALSE).
         updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
         updCronEventRun:assignField("CronEventRunResultTypeID",  fGetTypeID("CronEventRunResult", "Halted")).
        
         /* Get Errors */
         chrError = chrError + updCronEventRun:getErrors().
             
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            IF logGblDebugging THEN 
               fLog("Error updating: " + chrError).   
           
            RUN osSendMail.p (INPUT (IF gate.GateUser.UserName <> "cron" THEN 
                                        chrEmailAddress
                                     ELSE ""),                                                            /* Optional list of Users */
                              INPUT "Cron Job ID " + STRING(CronEventRun.CronEventID)                 
                                       + " Found Still Running - orphaned record failed to Assign Field", /* Email Subject */
                              INPUT "Program: " + chrProgramName + " found as still running after "
                                       + STRING(intDuration) + " minutes and the assign updCronEventRun"
                                       + " for FinishedOk, Completed, and CronEventRunResultTypeID has"
                                       + " failed.  CronEventRunID: " + STRING(CronEventRun.CronEventRunID) 
                                       + " CronEventID: " + STRING(CronEventRun.CronEventID) 
                                       + " Errors: " + chrError,                                          /* Plain text message Body */
                              INPUT "",                                                                   /* Html format message Body */
                              INPUT "",                                                                   /* File path ../files/file */
                              INPUT (IF gate.GateUser.UserName = "cron" THEN
                                        intCronEmailGroupID                                               /* EmailGroupID that you want */
                                     ELSE 0),                                                             /* to send this to */
                              INPUT "").                                                                  /* File MasterID is it applies */ 
                              
            NEXT Running_Block.
         END. /* IF chrError <> "" */
        
         /* Commit */
         RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                         OUTPUT logAllWentOK,
                                         OUTPUT chrError).
        
         /* Error Check */                                   
         IF chrError <> "" THEN                              
         DO:
            IF logGblDebugging THEN 
               fLog("Error on commit: " + chrError).
            
            RUN osSendMail.p (INPUT (IF gate.GateUser.UserName <> "cron" THEN 
                                        chrEmailAddress
                                     ELSE ""),                                                      /* Optional list of Users */
                              INPUT "Cron Job ID " + STRING(CronEventRun.CronEventID) 
                                       + " Found Still Running - orphaned record Commit Failed",    /* Email Subject */
                              INPUT "Program: " + chrProgramName + " found as still running after "
                                       + STRING(intDuration) + " minutes and the commit for "
                                       + " updCronEventRun has failed.  CronEvent of CronEventID " 
                                       + STRING(CronEventRun.CronEventID) + " CronEventRunID: " 
                                       + STRING(CronEventRun.CronEventRunID) + " CronEventID: " 
                                       + STRING(CronEventRun.CronEventID) + " Errors: "
                                       + chrError,                                                 /* Plain text message Body */
                              INPUT "",                                                            /* Html format message Body */
                              INPUT "",                                                            /* File path ../files/file */
                              INPUT (IF gate.GateUser.UserName = "cron" THEN
                                        intCronEmailGroupID                                        /* EmailGroupID that you want to send */ 
                                     ELSE 0),                                                      /* this to */  
                              INPUT "").                                                           /* File MasterID is it applies */ 
             
             NEXT Running_Block.
         END. /* IF chrError <> "" */
        
         /* Only email if in LIVE, Manual Run, or Logging is enabled.  This was added to prevent emails from UNIX not having resources */
         IF fGetCurrentEnvironment() = "LIVE" OR gate.GateUser.UserName <> "cron" OR logGblDebugging = TRUE THEN
         DO:
            RUN osSendMail.p (INPUT (IF gate.GateUser.UserName <> "cron" THEN 
                                           chrEmailAddress                                                       /* Optional list of */      
                                        ELSE ""),                                                                /* Users */     
                              INPUT "Cron Job ID " + STRING(CronEventRun.CronEventID) + " Alert - orphaned"
                                       + " job halted",                                                          /* Email Subject */
                              INPUT "Program: " + chrProgramName + " found as still running after "
                                       + STRING(intDuration) + " minutes and has been set to halted.  CronEvent"
                                       + " of CronEventID: "+ STRING(CronEventRun.CronEventID) 
                                       + " CronEventRun of CronEventRunID: " 
                                       + STRING(CronEventRun.CronEventRunID),                                    /* Plain text message Body */   
                              INPUT "",                                                                          /* Html format message Body */
                              INPUT "",                                                                          /* File path ../files/file */                    
                              INPUT (IF gate.GateUser.UserName = "cron" THEN                                     
                                        intCronEmailGroupID                                                      /* EmailGroupID that you */
                                     ELSE 0),                                                                    /* want to send this to */
                              INPUT "").                                                                         /* File MasterID is it */
         END. /* IF fGetCurrentEnvironment() = "LIVE" OR gate.GateUser.UserName <> "cron" OR logGblDebugging = TRUE */
      END. /* IF chrUnixPID = "" */
      ELSE
      DO:
         /* Only email if in LIVE, Manual Run, or Logging is enabled.  This was added to prevent emails from UNIX not having resources */
         IF fGetCurrentEnvironment() = "LIVE" OR gate.GateUser.UserName <> "cron" OR logGblDebugging = TRUE THEN
         DO:
            RUN osSendMail.p (INPUT (IF gate.GateUser.UserName <> "cron" THEN 
                                        chrEmailAddress
                                     ELSE ""),                                                            /* Optional list of Users */
                              INPUT "Cron Job ID " + STRING(CronEventRun.CronEventID) + " Alert - Still"
                                       + " Running",                                                     /* Email Subject */
                              INPUT "Program: " + chrProgramName + " found as still running after "
                                       + STRING(intDuration) + " minutes.  CronEvent of CronEventID: " 
                                       + STRING(CronEventRun.CronEventID) + " CronEventRun of CronEventRunID: " 
                                       + STRING(CronEventRun.CronEventRunID) + " UNIX PID: "
                                       + STRING(CronEventRun.UnixPID),                                   /* Plain text message Body */
                              INPUT "",                                                                  /* Html format message Body */
                              INPUT "",                                                                  /* File path ../files/file */
                              INPUT (IF gate.GateUser.UserName = "cron" THEN
                                        intCronEmailGroupID                                              /* EmailGroupID that you want to */
                                    ELSE 0),                                                            /* send this to */
                              INPUT "").                                                                 /* File MasterID is it applies */                
         END. /* IF fGetCurrentEnvironment() = "LIVE" OR gate.GateUser.UserName <> "cron" OR logGblDebugging = TRUE */
      END. /* ELSE */
   END. /* IF intDuration > intTimeLimit */

END. /* FOR EACH CronEventRun */
      
IF logGblDebugging THEN
DO:
   fLog("Finished.").
   OUTPUT STREAM sToLogFile CLOSE.
END. /* IF logGblDebugging */

DELETE OBJECT chrSsnTimeLimit      NO-ERROR.
DELETE OBJECT intSsnCronEventRunID NO-ERROR.
DELETE OBJECT updCronEventRun      NO-ERROR.

RELEASE gate.GateUser          NO-ERROR.
RELEASE CronEvent              NO-ERROR.
RELEASE CronEventRun           NO-ERROR.
RELEASE EmailGroup             NO-ERROR.
RELEASE ProcessProgram         NO-ERROR.
