/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnReportWrapper.p
Purpose : Creates a session for the cron or user and runs a program in batch mode using ReportScheduleID or ReportRunID parameter sent in 
          the -param session:parameter.  Can be called from the scheduler or manually from web interface.
Author  : Christopher Shelley
Date    : 15/05/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
24/06/2014 CS             Added logic for parsing scheduled dates and moved Err_ Log Files into the days folder.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Parameter 1 = ReportSchedule.ReportScheduleID,gate.GateUser.GateUserID,ReportRun.ReportRunID */
/* Parameter 2 = Reports */

/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncClassFunctions.i}
{fncGlobalFunctions.i}
{fncServerFunctions.i}
{fncDateFunctions.i}
{fncStatusTypeFunctions.i}

/* Session Objects */
DEFINE VARIABLE chrSsnParameter                    AS sessionValue      NO-UNDO.
DEFINE VARIABLE datSsnParameter                    AS sessionValue      NO-UNDO.
DEFINE VARIABLE intSsnReportRunID                  AS sessionValue      NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrProgramName                      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrRunFromDirectory                 AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrLogFile                          AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrLastRunLogFile                   AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrEmailLogFile                     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrErrorFile                        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory                 AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory                 AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrEmailAddress                     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrEmailSubject                     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrEmailError                       AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrEmailBody                        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrProgramError                     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrProgressError                    AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrFileLine                         AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrReportDescription                AS CHARACTER         NO-UNDO.
DEFINE VARIABLE datParameter                        AS DATE              NO-UNDO.
DEFINE VARIABLE datToRun                            AS DATE              NO-UNDO.
DEFINE VARIABLE intEntry                            AS INTEGER           NO-UNDO.
DEFINE VARIABLE intReportScheduleID                 AS INTEGER           NO-UNDO.
DEFINE VARIABLE intReportRunID                      AS INTEGER           NO-UNDO.
DEFINE VARIABLE intReportID                         AS INTEGER           NO-UNDO.
DEFINE VARIABLE intUnixPID                          AS INTEGER           NO-UNDO.
DEFINE VARIABLE intGateUserID                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intErrorCount                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intResultTypeID                     AS INTEGER           NO-UNDO.
DEFINE VARIABLE intRunning                          AS INTEGER           NO-UNDO.
DEFINE VARIABLE intCompleted                        AS INTEGER           NO-UNDO.
DEFINE VARIABLE intCancelled                        AS INTEGER           NO-UNDO.
DEFINE VARIABLE intReportErrorEmailGroupID          AS INTEGER           NO-UNDO.
DEFINE VARIABLE intArrayCount                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intAverageRun                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intRunTime                          AS INTEGER EXTENT 10 NO-UNDO.
DEFINE VARIABLE logAllWentOk                        AS LOGICAL           NO-UNDO.
DEFINE VARIABLE logScheduledMode                    AS LOGICAL           NO-UNDO.

/* Db Objects */
DEFINE VARIABLE newReportRun                     AS newRecord.
DEFINE VARIABLE newReportRunParameter            AS newRecord.
DEFINE VARIABLE updReportRun                     AS updRecord.
DEFINE VARIABLE updReportSchedule                AS updRecord.

/* Streams */
DEFINE STREAM sToLogFile.
DEFINE STREAM sToLastFile.
DEFINE STREAM sToEmailFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   PUT STREAM sToLastFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   PUT STREAM sToEmailFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

/* Set the log file destination directory  */
chrNewAgedDirectory = fGetAgedDirectory("../logs/", 2).
IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

ASSIGN intRunning          = fGetStatusID("ReportRun", "Running")
       intCompleted        = fGetStatusID("ReportRun", "Complete")
       intCancelled        = fGetStatusID("ReportRun", "Cancelled")
       intReportScheduleID = INTEGER(ENTRY(1,SESSION:PARAMETER,","))
       intGateUserID       = INTEGER(ENTRY(2,SESSION:PARAMETER,","))
       intReportRunID      = INTEGER(ENTRY(3,SESSION:PARAMETER,","))
       datToRun            = TODAY.

IF intReportScheduleID > 0 THEN
   ASSIGN logScheduledMode = TRUE
          chrEmailError    = "Report Schedule " + STRING(intReportScheduleID) + " has finished in Error - ".
ELSE
   chrEmailError  = "Report Run " + STRING(intReportRunID) + " has finished in Error - ".

FIND FIRST EmailGroup NO-LOCK /* index GroupCode */
   WHERE EmailGroup.GroupCode = "ReportErrors" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intReportErrorEmailGroupID = EmailGroup.EmailGroupID.
ELSE
DO:
   intReportErrorEmailGroupID = 1.

   RUN osSendMail.p (INPUT "",                                   /* Optional list of Users */
                     INPUT "ReportErrors EmailGroup is missing", /* Email Subject */
                     INPUT "ReportErrors EmailGroup is missing"
                              + " sent from Report Wrapper.",    /* Plain text message Body */
                     INPUT "",                                   /* Html format message Body */
                     INPUT "",                                   /* File path../idev/hyperthermbr/files/file OR ../files/file */
                     INPUT intReportErrorEmailGroupID,           /* EmailGroupID that you want to send this to */
                     INPUT "").                                  /* File MasterID is it applies */
END. /* ELSE DO */

FIND FIRST gate.GateUser NO-LOCK /* index GateUserID */
   WHERE gate.GateUser.GateUserID = intGateUserID NO-ERROR.
IF NOT AVAILABLE gate.GateUser THEN 
DO:
   RUN osSendMail.p (INPUT "",                                                          /* Optional list of Users */
                     INPUT chrEmailError + "GateUser Record Not Found",                 /* Email Subject */
                     INPUT "GateUser of GateUserID: " + STRING(intGateUserID) 
                              + " does not Exist.  ReportScheduleID: " + STRING(intReportScheduleID)
                              + " ReportRunID: " + STRING(intReportRunID),              /* Plain text message Body */
                     INPUT "",                                                          /* Html format message Body */
                     INPUT "",                                                          /* File path../idev/hyperthermbr/files/file OR */
                                                                                        /* ../files/file */
                     INPUT intReportErrorEmailGroupID,                                  /* EmailGroupID that you want to send this to */
                     INPUT "").                                                         /* File MasterID is it applies */

   RELEASE EmailGroup    NO-ERROR.
   RELEASE gate.GateUser NO-ERROR.

   RETURN.   
END. /* IF NOT AVAILABLE gate.GateUser */

chrEmailAddress = gate.GateUser.Email.

IF logScheduledMode THEN 
DO:
   FIND FIRST ReportSchedule NO-LOCK /* index ReportScheduleID */
      WHERE ReportSchedule.ReportScheduleID = intReportScheduleID NO-ERROR.
   IF NOT AVAILABLE ReportSchedule THEN
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                    /* Optional list of Users */
                        INPUT chrEmailError + "Record Not Found",        /* Email Subject */
                        INPUT "ReportScheduleID: " + STRING(intReportScheduleID) 
                                 + " does not Exist.",                   /* Plain text message Body */
                        INPUT "",                                        /* Html format message Body */
                        INPUT "",                                        /* File path../idev/hyperthermbr/files/file OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intReportErrorEmailGroupID
                               ELSE 0),                                  /* EmailGroupID that you want to send this to */
                        INPUT "").                                       /* File MasterID is it applies */
       
      RELEASE EmailGroup     NO-ERROR.
      RELEASE gate.GateUser  NO-ERROR.
      RELEASE ReportSchedule NO-ERROR.
       
      RETURN.
   END. /* IF NOT AVAILABLE ReportSchedule */
   intReportID = ReportSchedule.ReportID.
   
   /* Find date these reports were suppose to run at incase the Cron was set inactive and these are running at a later date */
   FIND FIRST CronConfig NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CronConfig THEN
   DO:
       RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                    /* Optional list of Users */
                        INPUT chrEmailError + "Record Not Found",        /* Email Subject */
                        INPUT "CronConfig Record does not Exist.",       /* Plain text message Body */
                        INPUT "",                                        /* Html format message Body */
                        INPUT "",                                        /* File path../idev/hyperthermbr/files/file OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intReportErrorEmailGroupID
                               ELSE 0),                                  /* EmailGroupID that you want to send this to */
                        INPUT "").                                       /* File MasterID is it applies */
       
      RELEASE EmailGroup     NO-ERROR.
      RELEASE gate.GateUser  NO-ERROR.
      RELEASE ReportSchedule NO-ERROR.
       
      RETURN.
   END.
   
    datToRun = fGetDate(CronConfig.PreviousRun).
END. /* IF logScheduledMode */
ELSE
DO:
   FIND FIRST ReportRun NO-LOCK
      WHERE ReportRun.ReportRunID = intReportRunID NO-ERROR.
   IF NOT AVAILABLE ReportRun THEN
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                    /* Optional list of Users */
                        INPUT chrEmailError + "Record Not Found",        /* Email Subject */
                        INPUT "ReportRunID: " + STRING(intReportRunID) 
                                 + " does not Exist.",                   /* Plain text message Body */
                        INPUT "",                                        /* Html format message Body */
                        INPUT "",                                        /* File path../idev/hyperthermbr/files/file OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intReportErrorEmailGroupID
                               ELSE 0),                                  /* EmailGroupID that you want to send this to */
                        INPUT "").                                       /* File MasterID is it applies */
       
      RELEASE EmailGroup     NO-ERROR.
      RELEASE gate.GateUser  NO-ERROR.
      RELEASE ReportRun      NO-ERROR.
       
      RETURN.
   END. /* IF NOT AVAILABLE ReportRun */
   intReportID = ReportRun.ReportID.
END.

FIND FIRST Report NO-LOCK
   WHERE Report.ReportID = intReportID NO-ERROR.
IF NOT AVAILABLE Report THEN
DO:
   RUN osSendMail.p (INPUT "",                                                         /* Optional list of Users */
                     INPUT chrEmailError + "Report Record Not Found",                  /* Email Subject */
                     INPUT "Report of ReportID: " + STRING(intReportID) 
                              + " does not Exist.  ReportScheduleID: " + STRING(intReportScheduleID)
                              + " ReportRunID: " + STRING(intReportRunID),             /* Plain text message Body */
                     INPUT "",                                                         /* Html format message Body */
                     INPUT "",                                                         /* File path../idev/hyperthermbr/files/file OR */
                                                                                       /* ../files/file */
                     INPUT intReportErrorEmailGroupID,                                 /* EmailGroupID that you want to send this to */
                     INPUT "").                                                        /* File MasterID is it applies */

   RELEASE EmailGroup    NO-ERROR.
   RELEASE gate.GateUser NO-ERROR.
   RELEASE ReportRun     NO-ERROR.
   RELEASE Report        NO-ERROR.

   RETURN.   
END. /* IF NOT AVAILABLE Report */

ASSIGN chrReportDescription = Report.ReportDescr
       chrEmailError        = "Report " + chrReportDescription + " has finished in Error - ".

ReportRunBlk:
DO TRANSACTION ON ERROR UNDO, LEAVE:
   /* Now in an include file so we can share it with the Cron Login program */
   {usrCreateSession.i}
    
   /* Data Manager Access */
   RUN libDataManager.p PERSISTENT SET hdlGblLibrary(INPUT UserSession.SessionID).
   
   fClearSessionValue("ReportRunID").
   intSsnReportRunID = fNewSessionValue("ReportRunID").

   IF logScheduledMode THEN
   DO:
      newReportRun = fCreateRecord("ReportRun").
      newReportRun:assignField("ReportID", intReportID).
      newReportRun:assignField("GateUserID", intGateUserID).
      newReportRun:assignField("Created", fTimestamp(NOW)).
      newReportRun:assignField("ReportDescr", chrReportDescription).
      newReportRun:assignField("ReportScheduleID", intReportScheduleID).
      newReportRun:assignField("EmailGroupID", Report.EmailGroupID).
   
      chrError = newReportRun:getErrors().
   
      IF chrError <> '' THEN
         UNDO ReportRunBlk, LEAVE ReportRunBlk.
      
      intSsnReportRunID:setValue(newReportRun:NewRecordUniqueID).
      intReportRunID = intSsnReportRunID:intValue.
   END. /* IF logScheduledMode */
   ELSE
      intSsnReportRunID:setValue(intReportRunID).
END. /* ReportRunBlk */      
   
IF logScheduledMode THEN
DO:
   DELETE OBJECT newReportRun NO-ERROR.

   IF chrError <> "" THEN
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                                         /* Optional list of Users */
                        INPUT chrEmailError + "Assign Failed",                                /* Email Subject */
                        INPUT "ReportRunID of ReportScheduleID: " + STRING(intReportScheduleID)
                                 + " failed to assign. Description: " + chrReportDescription, /* Plain text message Body */
                        INPUT "",                                                             /* Html format message Body */
                        INPUT "",                                                             /* File path../idev/hyperthermbr/files/file */
                                                                                              /* OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intReportErrorEmailGroupID
                               ELSE 0),                                                       /* EmailGroupID to send this to */
                        INPUT "").                                                            /* File MasterID is it applies */
         
      RELEASE EmailGroup     NO-ERROR.
      RELEASE gate.GateUser  NO-ERROR.
      RELEASE ReportSchedule NO-ERROR.
    
      RETURN.
   END. /* IF chrError <> "" */
    
   RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                   OUTPUT logAllWentOK,
                                   OUTPUT chrError).
    
   /* Error Check */                                   
   IF chrError <> "" THEN                              
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                                         /* Optional list of Users */
                        INPUT chrEmailError + "Commit Failed",                                /* Email Subject */
                        INPUT "ReportRunID of ReportScheduleID: " + STRING(intReportScheduleID)
                                 + " failed to commit.  Description: " 
                                 + chrReportDescription,                                      /* Plain text message Body */
                        INPUT "",                                                             /* Html format message Body */
                        INPUT "",                                                             /* File path../idev/hyperthermbr/files/file */
                                                                                              /* OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intReportErrorEmailGroupID
                               ELSE 0),                                                       /* EmailGroupID to send this to */
                        INPUT "").                                                            /* File MasterID is it applies */
          
      RELEASE EmailGroup     NO-ERROR.
      RELEASE gate.GateUser  NO-ERROR.
      RELEASE ReportSchedule NO-ERROR.
          
      RETURN.
   END. /* IF chrError <> "" */
END. /* IF logScheduledMode */

ReportRunParameterBlk:
DO TRANSACTION ON ERROR UNDO, LEAVE:
   IF logScheduledMode THEN
   DO:
      FOR EACH ReportScheduleParameter NO-LOCK
         WHERE ReportScheduleParameter.ReportScheduleID = intReportScheduleID:

         newReportRunParameter = fCreateRecord("ReportRunParameter").
         newReportRunParameter:assignField("ReportParameterID", ReportScheduleParameter.ReportParameterID).
         newReportRunParameter:assignField("ReportRunID", intSsnReportRunID:intValue).
         newReportRunParameter:assignField("ParameterValue", ReportScheduleParameter.ParameterValue).
      
         chrError = newReportRunParameter:getErrors().
   
         IF chrError <> '' THEN
            UNDO ReportRunParameterBlk, LEAVE ReportRunParameterBlk.
         
         DELETE OBJECT newReportRunParameter NO-ERROR.
      END. /* FOR EACH ReportScheduleParameter */
   END. /* IF logScheduledMode */
END. /* ReportRunParameterBlk */

IF logScheduledMode THEN
DO:
   DELETE OBJECT newReportRunParameter NO-ERROR.

   IF chrError <> "" THEN
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                                       /* Optional list of Users */
                        INPUT chrEmailError + "Assign Failed",                              /* Email Subject */
                        INPUT "ReportRunParameterID of ReportRunID: " + STRING(intReportRunID)
                                 + " failed to assign. Description: " 
                                + chrReportDescription,                                     /* Plain text message Body */
                        INPUT "",                                                           /* Html format message Body */
                        INPUT "",                                                           /* File path../idev/hyperthermbr/files/file */
                                                                                            /* OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intReportErrorEmailGroupID
                               ELSE 0),                                                     /* EmailGroupID that you want to send this to */
                        INPUT "").                                                          /* File MasterID is it applies */
         
      RELEASE EmailGroup              NO-ERROR.
      RELEASE gate.GateUser           NO-ERROR.
      RELEASE ReportRun               NO-ERROR.
      RELEASE ReportRunParameter      NO-ERROR.
      RELEASE ReportScheduleParameter NO-ERROR.
    
      RETURN.
   END. /* IF chrError <> "" */
    
   RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                   OUTPUT logAllWentOK,
                                   OUTPUT chrError).
    
   /* Error Check */                                   
   IF chrError <> "" THEN                              
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                                      /* Optional list of Users */
                        INPUT chrEmailError + "Commit Failed",                             /* Email Subject */
                        INPUT "ReportRunParameterID of ReportRunID: " + STRING(intReportRunID)
                                 + " failed to commit.  Description: " 
                                 + chrReportDescription,                                   /* Plain text message Body */
                        INPUT "",                                                          /* Html format message Body */
                        INPUT "",                                                          /* File path../idev/hyperthermbr/files/file */
                                                                                           /* OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intReportErrorEmailGroupID                                 
                               ELSE 0),                                                    /* EmailGroupID that you want to send this to */
                        INPUT "").                                                         /* File MasterID is it applies */
          
      RELEASE EmailGroup              NO-ERROR.
      RELEASE gate.GateUser           NO-ERROR.
      RELEASE ReportRun               NO-ERROR.
      RELEASE ReportRunParameter      NO-ERROR.
      RELEASE ReportScheduleParameter NO-ERROR.
          
      RETURN.
   END. /* IF chrError <> "" */
END. /* IF logScheduledMode */

ASSIGN chrLogFile        = chrLogFileDirectory + REPLACE(chrReportDescription," ", "") + "_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") 
                              + ".log"
       chrLastRunLogFile = "../logs/" + REPLACE(chrReportDescription," ", "") + ".lastrun"
       chrEmailLogFile   = chrLogFileDirectory + REPLACE(chrReportDescription," ", "") + "_ID_" + STRING(intReportRunID) + ".log"
       chrErrorFile      = chrLogFileDirectory + "Err_" + REPLACE(chrReportDescription," ", "") + "_ID_" + STRING(intReportRunID) + ".log".                                                 

/* Setup log files */
OUTPUT STREAM sToLogFile   TO VALUE(chrLogFile) APPEND.
OUTPUT STREAM sToLastFile  TO VALUE(chrLastRunLogFile).
OUTPUT STREAM sToEmailFile TO VALUE(chrEmailLogFile).

IF AVAILABLE ReportSchedule AND ReportSchedule.LoggingIsOn = TRUE THEN 
DO:
   logGblDebugging = TRUE.
   fLog("Running for ReportScheduleID: " + STRING(intReportScheduleID)). 
END. /* IF AVAILABLE ReportSchedule AND ReportSchedule.LoggingIsOn = TRUE */

/* Output default Stream to File to catch Progress Errors that we're not expecting. */  
OUTPUT TO VALUE(chrErrorFile).

/* This is used to find the UNIX PID for the connection, which will be the UNIX PID from batch command */
FIND FIRST _MyConnection NO-LOCK NO-ERROR.
IF NOT AVAILABLE _MyConnection THEN
DO:
   IF logGblDebugging THEN 
      fLog("No _MyConnection was Available."). 
       
   RELEASE EmailGroup     NO-ERROR.
   RELEASE gate.GateUser  NO-ERROR.
   RELEASE ReportSchedule NO-ERROR.
   RELEASE ReportRun      NO-ERROR.
   RELEASE _MyConnection  NO-ERROR.

   RETURN.
END. /* IF NOT AVAILABLE _MyConnection */      

intUnixPID = _MyConnection._MyConn-Pid.
                                         
IF logGblDebugging THEN 
   fLog("Before Setup_Block."). 

/* Setup variables, User Session, and commit ReportRun.ReportRunStatusID of Running to allow viewability outside of the program */
Setup_Block:
DO TRANSACTION:
   FIND FIRST ReportRun NO-LOCK /* index ReportRunID */
      WHERE ReportRun.ReportRunID = intReportRunID NO-ERROR.
   IF NOT AVAILABLE ReportRun THEN
   DO:
      ASSIGN chrEmailAddress = ""
             chrEmailSubject = "ReportRun has finished in Error - Record Not Found"
             chrEmailBody    = "ReportRun of ReportRunID: " + STRING(intReportRunID) + " does not Exist.".
             
      LEAVE Setup_Block.
   END. /* IF NOT AVAILABLE ReportRun */

   FIND FIRST Environment NO-LOCK /* index EnvironmentCode */
      WHERE Environment.EnvironmentCode = chrGblEnvironment
      AND   Environment.ACTIVE          = TRUE NO-ERROR.
   IF NOT AVAILABLE Environment THEN
   DO:
      ASSIGN chrEmailAddress = ""
             chrEmailSubject = chrEmailError + "Active Environment Record Not Found"
             chrEmailBody    = "Active Environment Record does not Exist.  Environment: " + chrGblEnvironment + " ReportScheduleID: "
                                  + STRING(ReportRun.ReportScheduleID) + " ReportRunID: " + STRING(ReportRun.ReportRunID) + " UnixPID: "
                                  + STRING(intUnixPID) + " Description: " + ReportRun.ReportDescr.

      LEAVE Setup_Block.
   END. /* IF NOT AVAILABLE Environment */
   
   /* Get ReportRun Record */
   updReportRun = fGetRecord("ReportRun", intReportRunID).
   updReportRun:assignField("UnixPID", intUnixPID).
   updReportRun:assignField("StartedOnCron", fTimeStamp(NOW)).
   updReportRun:assignField("ReportRunStatusID",  intRunning).

   /* Get Errors */
   chrError = chrError + updReportRun:getErrors().
         
   /* Error Check */
   IF chrError <> "" THEN
   DO:
      ASSIGN chrEmailSubject = chrEmailError + "Failed to Assign Field"
             chrEmailBody    = "The assign updReportRun for UnixPID, Started, and ReportRunStatusID has failed."
                                  + " ReportRunID: " + STRING(intReportRunID) + " ReportScheduleID: " + STRING(intReportScheduleID) 
                                  + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrReportDescription + " Errors: " + chrError.
      
      LEAVE Setup_Block.
   END. /* IF chrError <> "" */
  
   DELETE OBJECT updReportRun NO-ERROR.
   
   /* Commit */
   RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                   OUTPUT logAllWentOK,
                                   OUTPUT chrError).
   
   /* Error Check */                                   
   IF chrError <> "" THEN                              
   DO:
      ASSIGN chrEmailSubject = chrEmailError + "Record Commit Failed"
             chrEmailBody    = "ReportRun of ReportRunID " + STRING(intSsnReportRunID:intValue) + " the commit for updReportRun"
                                  + " has failed.  ReportRunID: " + STRING(intReportRunID) + " ReportScheduleID: " 
                                  + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                  + chrReportDescription + " Errors: " + chrError.
      
      LEAVE Setup_Block.
   END. /* IF chrError <> "" */
   
   /* Only set this flag when we know all is done without Error */
   logAllWentOk = TRUE.
END. /* Setup_Block */

IF logAllWentOk = TRUE THEN 
DO:
   /* Reset this flag again for the next transaction block */
   logAllWentOk = FALSE.

   Main_Block:
   DO:
      IF logGblDebugging THEN 
      DO:
         fLog("GateUserID: " + STRING(intGateUserID) + " -> " + THIS-PROCEDURE:NAME + " ReportRunID: " + STRING(intReportRunID) 
                 + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID)).
         logGblDebugging = TRUE.
      END. /* IF ReportSchedule.LoggingIsOn = TRUE */
      
      /* Clear and set Session Variables */
      FOR EACH ReportRunParameter NO-LOCK 
         WHERE ReportRunParameter.ReportRunID = intReportRunID:

         FIND FIRST ReportParameter NO-LOCK 
            WHERE ReportParameter.ReportParameterID = ReportRunParameter.ReportParameterID NO-ERROR.
         IF NOT AVAILABLE ReportParameter THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Record Not Found"
                   chrEmailBody    = "ReportParameter of ReportParameterID: " + STRING(ReportRunParameter.ReportParameterID) 
                                        + " does not Exist.  ReportRunID: " + STRING(intReportRunID) + " ReportScheduleID: " 
                                        + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                        + chrReportDescription + " Errors: " + chrError.

            /* Get ReportRun Record  */
            updReportRun = fGetRecord("ReportRun", intReportRunID).
            updReportRun:assignField("ReportRunStatusID",  fGetStatusID("ReportRun", "Error")).
            updReportRun:assignField("FinishedOk", FALSE).
            updReportRun:assignField("Completed", fTimeStamp(NOW)).
              
            /* Get Errors */
            chrError = chrError + updReportRun:getErrors().
                
            /* Error Check */
            IF chrError <> "" THEN
            DO:
               ASSIGN chrEmailSubject = chrEmailError + "Parameter Not Found and Failed to Assign Field"
                      chrEmailBody    = "ProcessProgram of ProcessProgramID: " + STRING(Report.ProcessProgramID) + " does not Exist and"
                                           + " the assign updReportRun for FinishedOk, Completed, and ReportRunStatusID has failed."
                                           + " ReportRunID: " + STRING(intReportRunID) + " ReportScheduleID: " + STRING(intReportScheduleID) 
                                           + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrReportDescription + " Errors: " 
                                           + chrError.
                 
               LEAVE Main_Block.
            END. /* IF chrError <> "" */
            
            DELETE OBJECT updReportRun NO-ERROR.
            /* Commit */
            RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                            OUTPUT logAllWentOK,
                                            OUTPUT chrError).
             
            /* Error Check */
            IF chrError <> "" THEN
            DO:
               ASSIGN chrEmailSubject = chrEmailError + "Parameter Not Found and Failed to commit"
                      chrEmailBody    = "ProcessProgram of ProcessProgramID: " + STRING(Report.ProcessProgramID) + " does not Exist and"
                                           + " the commit for updReportRun has failed.  ReportRunID: " + STRING(intSsnReportRunID:intValue) 
                                           + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) 
                                           + " Description: " + chrReportDescription + " Errors: " + chrError.
            END. /* IF chrError <> "" */ 

            LEAVE Main_Block.
         END. /* IF NOT AVAILABLE ReportParameter */

         IF ReportParameter.Active = FALSE THEN
            NEXT.
         
         fClearSessionValue(ReportParameter.ParameterName).
         
         /* Parse Dates since they can be populated from text or drop downs */
         IF ReportParameter.DisplayType = "DateField" THEN 
         DO:
            IF NUM-ENTRIES(ReportRunParameter.ParameterValue) > 1 THEN
            DO:
               IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "TODAY" THEN
                 datParameter = TODAY NO-ERROR.

               CASE TRIM(ENTRY(2,ReportRunParameter.ParameterValue)):

                  WHEN "Minus" THEN
                     datParameter = datParameter - INTEGER(TRIM(ENTRY(3,ReportRunParameter.ParameterValue))) NO-ERROR.
                  WHEN "Plus" THEN
                     datParameter = datParameter + INTEGER(TRIM(ENTRY(3,ReportRunParameter.ParameterValue))) NO-ERROR.
                  WHEN "LastMonth" THEN
                  DO:
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "FirstDayOf" THEN
                        datParameter = fFirstDayOfPrevMonth(datToRun) NO-ERROR.
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "LastDayOf" THEN
                        datParameter = fLastDayOfPrevMonth(datToRun) NO-ERROR.  
                  END.
                  WHEN "LastWeek" THEN
                  DO:
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "FirstDayOf" THEN
                        datParameter = fFirstDayOfPrevWeek(datToRun) NO-ERROR.
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "LastDayOf" THEN
                        datParameter = fLastDayOfPrevWeek(datToRun) NO-ERROR.
                  END.
                  WHEN "ThisMonth" THEN
                  DO:
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "FirstDayOf" THEN
                        datParameter = fFirstDayOfMonth(datToRun) NO-ERROR.
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "LastDayOf" THEN
                        datParameter = fLastDayOfMonth(datToRun) NO-ERROR.
                  END.
                  WHEN "ThisWeek" THEN
                  DO:
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "FirstDayOf" THEN
                        datParameter = fFirstDayOfWeek(datToRun) NO-ERROR.
                     IF TRIM(ENTRY(1,ReportRunParameter.ParameterValue)) = "LastDayOf" THEN
                        datParameter = fLastDayOfWeek(datToRun) NO-ERROR.
                  END.
                  OTHERWISE 
                     datParameter = datParameter.     
               END CASE.          
            END. /* IF NUM-ENTRIES(ReportRunParameter.ParameterValue) > 1 */
            ELSE
               datParameter = DATE(TRIM(ReportRunParameter.ParameterValue)) NO-ERROR.
            
            IF ERROR-STATUS:ERROR THEN 
            DO:
               chrProgressError = "Invalid Date Parameter.  ".
                  
               /* Check error message */ 
               DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                  
                  chrProgressError = chrProgressError + STRING(ERROR-STATUS:GET-MESSAGE(intErrorCount)).                                        
               END. /* DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES */
               
               ASSIGN chrEmailSubject = chrEmailError + "Invalid Date Provided"
                         chrEmailBody    = "ReportParameter of ReportParameterID: " + STRING(ReportRunParameter.ReportParameterID) 
                                              + " has invalid date: " + STRING(ReportRunParameter.ParameterValue) + "  ReportRunID: " 
                                              + STRING(intSsnReportRunID:intValue) + " ReportScheduleID: " + STRING(intReportScheduleID) 
                                              + " UnixPID: " + STRING(intUnixPID) 
                                              + " Description: " + chrReportDescription + " Errors: " + chrProgressError.
            END. /* IF ERROR-STATUS:ERROR */ 
            
            IF datParameter = ? OR chrProgressError <> "" THEN 
            DO:
               /* Get ReportRun Record  */
               updReportRun = fGetRecord("ReportRun", intReportRunID).
               updReportRun:assignField("ReportRunStatusID",  fGetStatusID("ReportRun", "Error")).
               updReportRun:assignField("FinishedOk", FALSE).
               updReportRun:assignField("Completed", fTimeStamp(NOW)).
              
               /* Get Errors */
               chrError = chrError + updReportRun:getErrors().
                
               /* Error Check */
               IF chrError <> "" THEN
               DO:
                  ASSIGN chrEmailSubject = chrEmailError + "Invalid Date Provided and Failed to Assign Field"
                         chrEmailBody    = "ReportParameter of ReportParameterID: " + STRING(ReportRunParameter.ReportParameterID) 
                                              + " has invalid date: " + STRING(ReportRunParameter.ParameterValue) + " and the assign updReportRun"
                                              + " for FinishedOk, Completed, and ReportRunStatusID has failed."
                                              + " ReportRunID: " + STRING(intReportRunID) + " ReportScheduleID: " + STRING(intReportScheduleID) 
                                              + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrReportDescription + " Errors: " 
                                              + chrError + " " + chrProgressError.
                        
                  LEAVE Main_Block.
               END. /* IF chrError <> "" */
               
               /* Commit */
               RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                               OUTPUT logAllWentOK,
                                               OUTPUT chrError).
              
               /* Error Check */
               IF chrError <> "" THEN
               DO:
                  ASSIGN chrEmailSubject = chrEmailError + "Invalid Date Provided and Failed to commit"
                         chrEmailBody    = "ReportParameter of ReportParameterID: " + STRING(ReportRunParameter.ReportParameterID) 
                                              + " has invalid date: " + STRING(ReportRunParameter.ParameterValue) + " and"
                                              + " the commit for updReportRun has failed.  ReportRunID: " + STRING(intSsnReportRunID:intValue) 
                                              + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) 
                                              + " Description: " + chrReportDescription + " Errors: " + chrError.
               END. /* IF chrError <> "" */ 
            
               LEAVE Main_Block.
            END. /* IF datParameter = ? OR chrError <> "" */

            datSsnParameter = fNewSessionValue(ReportParameter.ParameterName).
            datSsnParameter:setValue(datParameter).
         END. /* IF ReportParameter.DisplayType = "DateField" */
         ELSE
         DO:
            chrSsnParameter = fNewSessionValue(ReportParameter.ParameterName).
            chrSsnParameter:setValue(ReportRunParameter.ParameterValue).
         END. /* ELSE DO */
         
         IF logGblDebugging THEN 
            fLog("Found Parameter: " + ReportParameter.ParameterName + " Value: "  + ReportRunParameter.ParameterValue). 
               
      END. /* FOR EACH ReportScheduleParameter */
      
      IF logGblDebugging THEN
         fLog("All Session Variables have been set.").
      
      FIND FIRST ProcessProgram NO-LOCK /* index ProcessProgramID */
         WHERE ProcessProgram.ProcessProgramID = Report.ProcessProgramID NO-ERROR.
      IF NOT AVAILABLE ProcessProgram THEN
      DO:
         IF logGblDebugging THEN
            fLog("ProcessProgram of ProcessProgramID: " + STRING(Report.ProcessProgramID)).
           
         /* Set ReportRun to show the program was not found */
         updReportRun = fGetRecord("ReportRun", intReportRunID).
         updReportRun:assignField("FinishedOk", FALSE).
         updReportRun:assignField("Completed", fTimeStamp(NOW)).
         updReportRun:assignField("ReportRunStatusID", fGetStatusID("ReportRun", "ProgramMissing")).
       
         /* Get Errors */
         chrError = chrError + updReportRun:getErrors().
            
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Record Not Found and Failed to Assign Field"
                   chrEmailBody    = "ProcessProgram of ProcessProgramID: " + STRING(Report.ProcessProgramID) + " does not Exist and"
                                        + " the assign updReportRun for FinishedOk, Completed, and ReportRunStatusID has failed."
                                        + " ReportRunID: " + STRING(intReportRunID) + " ReportScheduleID: " + STRING(intReportScheduleID) 
                                        + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrReportDescription + " Errors: " 
                                        + chrError.
             
            LEAVE Main_Block.
         END. /* IF chrError <> "" */
        
         /* Commit */
         RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                         OUTPUT logAllWentOK,
                                         OUTPUT chrError).
         
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Record Not Found and Failed to commit"
                   chrEmailBody    = "ProcessProgram of ProcessProgramID: " + STRING(Report.ProcessProgramID) + " does not Exist and"
                                        + " the commit for updReportRun has failed.  ReportRunID: " + STRING(intSsnReportRunID:intValue) 
                                        + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) 
                                        + " Description: "  
                                        + chrReportDescription + " Errors: " + chrError.
         END. /* IF chrError <> "" */ 
          
         LEAVE Main_Block.
      END. /* IF NOT AVAILABLE ProcessProgram */
    
      IF logGblDebugging THEN 
         fLog("Program Name: " + ProcessProgram.ProgramName + " has been found in ProcessProgram.").
        
      /* Add the specified directory to the propath of the session so program will pick up other dependant programs */
      IF logScheduledMode AND ReportSchedule.DirectoryToRunFrom <> "" THEN
      DO:
         PROPATH = ReportSchedule.DirectoryToRunFrom + "," + PROPATH.
         
         IF logGblDebugging THEN 
            fLog("DirectoryToRunFrom: " + STRING(ReportSchedule.DirectoryToRunFrom) + " parsed. PROPATH: " + PROPATH).
      END. /* IF logScheduledMode AND ReportSchedule.DirectoryToRunFrom <> "" */
        
      /* Get ReportRun Record  */
      updReportRun = fGetRecord("ReportRun", intReportRunID).
       
      /* Now run the program if we can find it on the propath */
      IF SEARCH(ProcessProgram.ProgramName) = ? THEN
      DO:
         IF logGblDebugging THEN
            fLog("Program was not found.").
   
         ASSIGN chrEmailSubject = chrEmailError + "Program not found"
                chrEmailBody    = "Program was not found".
   
         /* Set ReportRun to show the program was not found */
         updReportRun:assignField("FinishedOk", FALSE).
         updReportRun:assignField("Completed", fTimeStamp(NOW)).
         updReportRun:assignField("ReportRunStatusID",  fGetStatusID("ReportRun", "ProgramMissing")).
       
         /* Get Errors */
         chrError = chrError + updReportRun:getErrors().
           
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Program not found and failed to assign"
                   chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " was not found and the updReportRun FinishedOk,"
                                        + " Completed, and ReportRunStatusID has failed.  ReportRunID: " + STRING(intReportRunID) 
                                        + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrReportDescription + " Errors: " 
                                        + chrError.
      
            LEAVE Main_Block.   
         END. /* IF chrError <> "" */ 
        
         /* Error Check */  
         RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                         OUTPUT logAllWentOk,
                                         OUTPUT chrError).
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Program not found and Failed to commit"
                   chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " was not found and the commit for updReportRun has"
                                        + " failed. ReportRunID: " + STRING(intReportRunID) + " UnixPID: "  + STRING(intUnixPID) 
                                        + " Description: " + chrReportDescription + " Errors: " + chrError.
            LEAVE Main_Block.
         END. /* IF chrError <> "" */
         logAllWentOk = FALSE.
         LEAVE Main_Block.
      END. /*  IF SEARCH(ProcessProgram.ProgramName) = ? */
     
      IF logGblDebugging THEN
         fLog("Program has been located in PROPATH.").

      /* Find any unexpected Progress syntax errors.  This is left without NO-ERROR to trap the errors into the log file */
      COMPILE VALUE(ProcessProgram.ProgramName).
       
      /* IF Compiled without errors */
      IF NOT COMPILER:ERROR THEN 
      DO:
         RELEASE ReportRun NO-ERROR.
         FIND FIRST ReportRun NO-LOCK 
            WHERE ReportRun.ReportRunID = intReportRunID NO-ERROR.

         IF logGblDebugging THEN
            fLog("Program has syntaxed.  Running now. " + STRING(ReportRun.ReportRunStatusID)).
      
         RUN VALUE(ProcessProgram.ProgramName) NO-ERROR.
          
         IF logGblDebugging THEN
         DO:
            fLog("Program has finished. Errors found: " + STRING(ERROR-STATUS:ERROR)).
            fLog("Checking for internal Progress Errors from: " + chrErrorFile).
         END. /* IF ReportSchedule.LoggingIsOn = TRUE */
      END. /* IF NOT COMPILER:ERROR  */
      
      /* Close the output to read in any Compiler or Runtime Progress Errors */
      OUTPUT CLOSE.
      
      /* Import from Error log*/
      INPUT FROM VALUE(chrErrorFile).

      REPEAT:
         IMPORT UNFORMATTED chrFileLine.
         
         IF logGblDebugging THEN
            fLog(chrFileLine).              
         
         /* Add these to chrError for later use */
         chrError = chrError + chrFileLine.
      END. /* REPEAT */

      INPUT CLOSE.

      /* IF Program had errors, did not syntax, or had a Progress Runtime Error */
      IF ERROR-STATUS:ERROR OR 
         COMPILER:ERROR OR 
         chrError <> "" THEN
      DO:
         IF logGblDebugging THEN
            fLog("Parsing errors now.").
          
         IF COMPILER:ERROR THEN 
            chrError = " Compile Errors: " + chrError.
         ELSE
            chrError = " Program Errors: " + chrError.
          
         IF ERROR-STATUS:ERROR THEN 
         DO:
            /* Check for specific program error being returned back */
            IF RETURN-VALUE > "" THEN 
               ASSIGN chrProgramError = RETURN-VALUE
                      chrError        = chrError + " " + chrProgramError + " ".
            
            /* Check error message for program */ 
            DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                  
               chrError = chrError + STRING(ERROR-STATUS:GET-MESSAGE(intErrorCount)).                                        
            END. /* DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES */       
         END. /* IF ERROR-STATUS:ERROR */
            
         /* Get ReportRun Record  */
         updReportRun = fGetRecord("ReportRun", intReportRunID).
         intResultTypeID = fGetStatusID("ReportRun", chrProgramError).

         IF intResultTypeID = intCancelled THEN
         DO:
            IF gate.GateUser.Username <> "cron" THEN
            DO:
               IF logGblDebugging THEN 
                  fLog("Sending email to " + chrEmailAddress + " to show cancelled.").
         
                  OUTPUT STREAM sToLogFile   CLOSE.
                  OUTPUT STREAM sToLastFile  CLOSE.
                  OUTPUT STREAM sToEmailFile CLOSE.
                  OUTPUT CLOSE.

                  RUN osSendMail.p (INPUT chrEmailAddress,                                /* Optional list of Users */
                        INPUT "Report " + chrReportDescription + " has been cancelled",   /* Email Subject */
                        INPUT "The report has been cancelled.",                           /* Plain text message Body */
                        INPUT "",                                                         /* Html format message Body */
                        INPUT (IF logGblDebugging THEN
                                  chrEmailLogFile                                         
                               ELSE ""),                                                  /* File path ../files/file */
                        INPUT "",                                                         /* EmailGroupID */
                        INPUT "").                                                        /* File MasterID */

            END. /* IF gate.GateUser.UserName <> "cron" */
            logAllWentOk = FALSE.
            LEAVE Main_Block.
         END. /* IF intResultTypeID = intCancelled */
        
         IF intResultTypeID > 0 AND 
            CAN-FIND(FIRST ReportRunStatus NO-LOCK /* index ReportRunStatusID */
                        WHERE ReportRunStatus.ReportRunStatusID = intResultTypeID
                        AND   ReportRunStatus.Active            = TRUE) THEN
            updReportRun:assignField("ReportRunStatusID",  intResultTypeID).
         ELSE
            updReportRun:assignField("ReportRunStatusID",  fGetStatusID("ReportRun", "Error")).
        
         IF logGblDebugging THEN
            fLog("Program error returned: " + chrError).
         
         ASSIGN chrEmailSubject  = chrEmailError + "Program Error"
                chrEmailBody     = "The Program: " + ProcessProgram.ProgramName + " had an error.  ReportRunID: " 
                                      + STRING(intReportRunID) + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " 
                                      + STRING(intUnixPID) + " Description: " + chrReportDescription 
                chrProgressError = "None".

         /* If there were errors attach them to the email */
         IF chrError <> "" THEN
            ASSIGN chrEmailBody     = chrEmailBody + chrError
                   chrProgressError = chrError.
        
         /* Set ReportRun to show the program did not finish correctly and there was an error */
         updReportRun:assignField("FinishedOk", FALSE).
         updReportRun:assignField("Completed", fTimeStamp(NOW)).
         
         /* Get Errors */
         chrError = updReportRun:getErrors().
             
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Program Error and Failed to Assign Field"
                   chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " had an Error and the updReportRun FinishedOk,"
                                        + " Completed, and ReportRunStatusID assign has failed.   ReportRunID: " 
                                        + STRING(intReportRunID) + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " 
                                        + STRING(intUnixPID) + " Description: " + chrReportDescription + chrProgressError 
                                        + " Assign Errors: " + chrError.
            LEAVE Main_Block.
         END. /* IF chrError <> "" */
         ELSE
         DO: 
            /* Commit */
            RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                            OUTPUT logAllWentOK,
                                            OUTPUT chrError).
       
            /* Error Check */
            IF chrError <> "" THEN
            DO:
               ASSIGN chrEmailSubject = chrEmailError + "Program Error and Failed to commit"
                      chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " had an Error and the commit for updReportRun has"
                                           + " failed.  ReportRunID: " + STRING(intReportRunID) + " ReportScheduleID: " 
                                           + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                           + chrReportDescription + chrProgressError + " Errors: " + chrError.
               LEAVE Main_Block.
            END. /* IF chrError <> "" */ 
         END. /* ELSE DO */
         
         logAllWentOk = FALSE.
        
         IF logGblDebugging THEN
            fLog("Leaving Main_Block. STATUS: " + STRING(logAllWentOk) + chrError).
           
         LEAVE Main_Block.
      END. /* IF ERROR-STATUS:ERROR OR COMPILER:ERROR OR chrError <> "" */
   
      /* Get ReportRun Record  */
      updReportRun = fGetRecord("ReportRun", intReportRunID).    
      updReportRun:assignField("FinishedOk", TRUE).
      updReportRun:assignField("Completed", fTimeStamp(NOW)).
      updReportRun:assignField("ReportRunStatusID",  intCompleted).
     
      /* Get Errors */
      chrError = chrError + updReportRun:getErrors().
              
      IF logGblDebugging THEN
         fLog("Assigned Finished OK and completed " + chrError).
   
      /* Error Check */
      IF chrError <> "" THEN
      DO:
         IF logGblDebugging THEN
            fLog("Completed AND failed TO ASSIGN.").

         ASSIGN chrEmailSubject = chrEmailError + "Program Completed and Failed to assign"
                chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " has completed but the updReportRun FinishedOk,"
                                     + " Completed, and ReportRunStatusID assign has failed. ReportRunID: " + STRING(intReportRunID) 
                                     + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) 
                                     + " Description: " + chrReportDescription + " Errors: " + chrError.

         LEAVE Main_Block.
      END. /* IF chrError <> "" */ 
      
      /* Error Check */  
      RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                     OUTPUT logAllWentOK,
                                     OUTPUT chrError).                                                                                 

      /* Error Check */
      IF chrError <> "" THEN
      DO:
         ASSIGN chrEmailSubject = chrEmailError + "Failed to commit"
                chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " commit for updReportRun has failed. ReportRunID: " 
                                     + STRING(intReportRunID) + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " 
                                     + STRING(intUnixPID) + " Description: " + chrReportDescription + " Errors: " + chrError.
       
         LEAVE Main_Block.
      END. /* IF chrError <> "" */

      IF logGblDebugging THEN
        fLog("Program has finished without errors and ReportRun final commit is complete.").
   
      logAllWentOk = TRUE.
   END. /* Main_Block */
END. /* IF logAllWentOk = TRUE */

IF logAllWentOk = TRUE THEN
DO:
   logAllWentOk = FALSE.
   
   /* Only track metrics for Scheduled Reports */
   IF logScheduledMode THEN
   DO:
      Metrics_Block:
      DO TRANSACTION:
         FIND FIRST ReportRun NO-LOCK /* index ReportRunID */
            WHERE ReportRun.ReportRunID = intReportRunID NO-ERROR.
          IF AVAILABLE ReportRun THEN
          DO:
             ASSIGN intArrayCount = 1
                    intRunTime[intArrayCount] = 0.

             IF ReportRun.Completed > "" THEN
                ASSIGN intRunTime[intArrayCount] = fInterval(ReportRun.Completed, ReportRun.StartedOnCron, "Seconds")
                       intAverageRun = intAverageRun + intRunTime[intArrayCount].
         
             DO WHILE intArrayCount < 10:
                FIND PREV ReportRun NO-LOCK
                   WHERE ReportRun.ReportScheduleID = intReportScheduleID NO-ERROR.
                IF NOT AVAILABLE ReportRun THEN
                   LEAVE.
                 
                IF ReportRun.Completed > "" THEN
                   ASSIGN intArrayCount = intArrayCount + 1
                          intRunTime[intArrayCount] = fInterval(ReportRun.Completed, ReportRun.StartedOnCron, "Seconds")
                          intAverageRun = intAverageRun + intRunTime[intArrayCount].
                        
                IF logGblDebugging THEN
                   fLog("Metrics using previous ReportRunID: " + STRING(ReportRun.ReportRunID) + " " + STRING(intRunTime[intArrayCount]) 
                           + " " + STRING(ReportRun.Completed) + " " +  STRING(ReportRun.StartedOnCron) + " " 
                           + STRING(fInterval(ReportRun.Completed, ReportRun.StartedOnCron, "Seconds"))).
             END. /* DO WHILE intArrayCount < 10 */
              
             IF logGblDebugging THEN
                fLog("Average: " + STRING(intAverageRun) + " / " + STRING(intArrayCount)).
             intAverageRun = intAverageRun / intArrayCount.
              
             FIND FIRST ReportSchedule NO-LOCK
                WHERE ReportSchedule.ReportScheduleID = intReportScheduleID NO-ERROR.
             IF AVAILABLE ReportSchedule THEN
             DO:
                /* Get ReportRun Record  */
                updReportSchedule = fGetRecord("ReportSchedule", intReportScheduleID).
                 
                IF ReportSchedule.MinimumRunDuration > intRunTime[1] OR
                   ReportSchedule.MinimumRunDuration = 0 THEN
                DO:
                   updReportSchedule:assignField("MinimumRunDuration", intRunTime[1]).
                   IF logGblDebugging THEN
                      fLog("Updating MinimumRunDuration: " + STRING(intRunTime[1])).   
                END. /* IF ReportSchedule.MinimumRunDuration */
                    
                IF ReportSchedule.MaximumRunDuration < intRunTime[1] THEN
                DO:
                   updReportSchedule:assignField("MaximumRunDuration", intRunTime[1]).
                   IF logGblDebugging THEN
                      fLog("Updating MaximumRunDuration: " + STRING(intRunTime[1])).
                END. /* IF ReportSchedule.MaximumRunDuration */
                 
                IF ReportSchedule.AverageRunDuration <> intAverageRun THEN
                DO:
                   updReportSchedule:assignField("AverageRunDuration", intAverageRun).
                   IF logGblDebugging THEN
                      fLog("Updating AverageRunDuration: " + STRING(intAverageRun)).
                END. /* IF ReportSchedule.AverageRunDuration */

                /* Get Errors */
                chrError = chrError + updReportSchedule:getErrors().
                
                /* Error Check */
                IF chrError <> "" THEN
                DO:
                   ASSIGN chrEmailSubject = chrEmailError + "Failed to Assign Field"
                          chrEmailBody    = "The assign updReportSchedule for MinimumRunDuration, MaximumRunDuration, and"
                                               + " AverageRunDuration has failed. ReportRunID: " + STRING(intReportRunID) 
                                               + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) 
                                               + " Description: " + chrReportDescription + " Errors: " + chrError.
                      
                   LEAVE Metrics_Block.
                END. /* IF chrError <> "" */
                  
                DELETE OBJECT updReportSchedule NO-ERROR.
                   
                /* Commit */
                RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                                OUTPUT logAllWentOK,
                                                OUTPUT chrError).
                   
                /* Error Check */                                   
                IF chrError <> "" THEN                              
                DO:
                   ASSIGN chrEmailSubject = chrEmailError + " and Commit Failed"
                          chrEmailBody    = "ReportScheduleID " + STRING(intReportScheduleID) + " did not update metrics and the commit for"
                                               + " updReportSchedule has failed.  ReportRunID: " + STRING(intReportRunID) 
                                               + " ReportScheduleID: " + STRING(intReportScheduleID) + " UnixPID: " + STRING(intUnixPID) 
                                               + " Description: " + chrReportDescription + " Errors: " + chrError.
                      
                   LEAVE Metrics_Block.
                END. /* IF chrError <> "" */
             END. /* IF AVAILABLE ReportSchedule */
          END. /* IF AVAILABLE ReportRun */
       END. /* Metrics_Block */
       logAllWentOk = TRUE.   
   END. /* IF logScheduledMode */

   IF gate.GateUser.Username <> "cron" AND logGblDebugging THEN
   DO:
      fLog("Sending email to " + chrEmailAddress + " to show completed.").
         
      OUTPUT STREAM sToLogFile   CLOSE.
      OUTPUT STREAM sToLastFile  CLOSE.
      OUTPUT STREAM sToEmailFile CLOSE.
      OUTPUT CLOSE.

      RUN osSendMail.p (INPUT chrEmailAddress,                                     /* Optional list of Users */
                        INPUT "Report " + chrReportDescription + " has finished",  /* Email Subject */
                        INPUT "The report has finished.",                          /* Plain text message Body */
                        INPUT "",                                                  /* Html format message Body */
                        INPUT chrEmailLogFile,                                     /* File path ../files/file */
                        INPUT "",                                                  /* EmailGroupID */
                        INPUT "").                                                 /* File MasterID */

   END. /* IF gate.GateUser.UserName <> "cron" */

END. /* IF logAllWentOk */

IF chrEmailSubject > "" THEN 
DO:
   IF logGblDebugging THEN 
   DO:
      fLog("Sending error email").
      fLog("Subject: " + chrEmailSubject).
      fLog("Body: " + chrEmailBody).
   END. /* IF ReportSchedule.LoggingIsOn = TRUE */
   
   OUTPUT STREAM sToLogFile   CLOSE.
   OUTPUT STREAM sToLastFile  CLOSE.
   OUTPUT STREAM sToEmailFile CLOSE.
   OUTPUT CLOSE.
   
   /* Limit the email body if it is too long for the email to send correctly */
   IF LENGTH(chrEmailBody) > 900 THEN
      chrEmailBody = STRING(chrEmailBody, "X(900)") + CHR(13) + "There is too much to be displayed.".
   
   /* Email to notify IT of issue */
   RUN osSendMail.p (INPUT "",                                      /* Optional list of Users */
                     INPUT chrEmailSubject,                         /* Email Subject */
                     INPUT chrEmailBody,                            /* Plain text message Body */
                     INPUT "",                                      /* Html format message Body */
                     INPUT (IF logGblDebugging THEN
                               chrEmailLogFile
                            ELSE ""),                               /* File path ../files/file */
                     INPUT intReportErrorEmailGroupID,                                /* EmailGroupID */
                     INPUT "").                                     /* File MasterID */ 

   /* Email to User or Report Group */
   RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                               ""                                
                            ELSE chrEmailAddress),                  /* Optional list of Users */
                     INPUT chrEmailSubject,                         /* Email Subject */
                     INPUT chrEmailBody,                            /* Plain text message Body */
                     INPUT "",                                      /* Html format message Body */
                     INPUT (IF logGblDebugging THEN
                               chrEmailLogFile
                            ELSE ""),                               /* File path ../files/file */
                     INPUT ReportRun.EmailGroupID,                  /* EmailGroupID */
                     INPUT "").                                     /* File MasterID */ 
END. /* IF chrEmailSubject > "" */

/* Set permissions for everything in logs to be available to both manually ran and scheduled Cron */
IF gate.GateUser.Username = "cron" THEN 
   OS-COMMAND("chmod -R 777 ../logs").

/* Clean-up */
IF logAllWentOk = TRUE THEN  
   fDeleteFile(chrErrorFile)        NO-ERROR.

fDeleteFile(chrEmailLogFile)        NO-ERROR.

DELETE OBJECT chrSsnParameter       NO-ERROR.
DELETE OBJECT datSsnParameter       NO-ERROR.
DELETE OBJECT intSsnReportRunID     NO-ERROR.
DELETE OBJECT newReportRunParameter NO-ERROR.
DELETE OBJECT newReportRun          NO-ERROR.
DELETE OBJECT updReportRun          NO-ERROR.
DELETE OBJECT updReportSchedule     NO-ERROR.

/* Release statements need to have gate database fully qualified because of syngate data sync */
RELEASE gate.GateUser               NO-ERROR.
RELEASE _myConnection               NO-ERROR.
RELEASE EmailGroup                  NO-ERROR.
RELEASE Environment                 NO-ERROR.
RELEASE ReportSchedule              NO-ERROR.
RELEASE ReportRun                   NO-ERROR.
RELEASE ProcessProgram              NO-ERROR.
RELEASE ReportScheduleParameter     NO-ERROR.

DELETE PROCEDURE hdlGblLibrary      NO-ERROR.
