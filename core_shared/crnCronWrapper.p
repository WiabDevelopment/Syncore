/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnCronWrapper
Purpose : Creates a session for the cron or user and runs a program in batch mode using CronEventRunID parameter sent in the -param 
          session:parameter.  Can be called from the cron itself or manually from web interface.
Author  : BG
Date    : 31st July 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03/12/2013 CS  CR1021     Added more error handling and check for inprocess CronEvents.
08/01/2014 CS  CR1021     Added more logging and removed the Progress message parse.
10/01/2014 CS  CR1021     Fixed issue with streams and email not sending for duplicates.
15/01/2014 CS  CR1021     Moved the replace asterisk statement into osSendMail.p.  Added CronEventRunID to error email body.
21/02/2014 CS  CR1021     Added compiler error messaging.
04/03/2014 CS  CR1021     Added check to prevent email from sending when Cron Scheduler runs a job that causes a duplicate error.
12/03/2014 CS  CR1021     Changed Session Parameter to be CronEventID and GateUserID to allowcreation of CronEventRun.  This was to prevent 
                          the Scheduler from locking CronEventRun when this program is ran.
17/03/2014 CS  CR1021     Added Release Statements to the error emails.              
24/03/2014 CS  CR1021     Added chmod for logs directory and limit email body.
09/04/2014 CS  56693      Removed Transaction on Main_Block.
10/04/2014 CS  56693      V11 required changes.
29/04/2014 CS  56693      Added Metrics_Block to track current job's min and max along with average for last 10 jobs.
27/05/2014 CS  56693      Added logic to Metrics_Block to not allow runtime of ?.
24/06/2014 CS  WiaB       Moved Err_ Log Files into the days folder.
03/11/2014 CS  WiaB       Added Change Request Number logic to allow all folders in current ProPath to be used from the NIRS Number first.
19/05/2015 CS  CanonTlb   Added pause to the CronEvent to allow staggering jobs in the same minute, added more comments, and updated for
                          coding standards.
11/01/2016 CS  NextelBr   Merged Canon and NextelBr versions.  Moved NIRS ProPath set prior to the LibDataManager.p call while placing
                          cust before core.  Formatted some of the longer messages.  Commented out SecondsToPauseBeforeExecuting in this 
                          application since the Schema is out of date.
09/02/2016 CS  CanonTlb   Added remove error log file for duplicate CronEventRuns.                          
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Parameter 1 = CronEvent.CronEventID */
/* Parameter 2 = gate.GateUser.GateUserID */

/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncClassFunctions.i}
{fncGlobalFunctions.i}
{fncServerFunctions.i}
{fncDateFunctions.i}
{fncStatusTypeFunctions.i}

/* Session Objects */
DEFINE VARIABLE chrSsnParameter                     AS sessionValue      NO-UNDO.
DEFINE VARIABLE intSsnCronEventRunID                AS sessionValue      NO-UNDO.

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
DEFINE VARIABLE chrCurrentDirectory                 AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrCurrentEnv                       AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrRootDirectory                    AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrPropath                          AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrNirsPropath                      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrFileLine                         AS CHARACTER         NO-UNDO.
DEFINE VARIABLE chrCronDescription                  AS CHARACTER         NO-UNDO.
DEFINE VARIABLE intEntry                            AS INTEGER           NO-UNDO.
DEFINE VARIABLE intCronEventID                      AS INTEGER           NO-UNDO.
DEFINE VARIABLE intUnixPID                          AS INTEGER           NO-UNDO.
DEFINE VARIABLE intGateUserID                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intErrorCount                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intResultTypeID                     AS INTEGER           NO-UNDO.
DEFINE VARIABLE intRunning                          AS INTEGER           NO-UNDO.
DEFINE VARIABLE intCompleted                        AS INTEGER           NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID                 AS INTEGER           NO-UNDO.
DEFINE VARIABLE intArrayCount                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intAverageRun                       AS INTEGER           NO-UNDO.
DEFINE VARIABLE intRunTime                          AS INTEGER EXTENT 10 NO-UNDO.
DEFINE VARIABLE intApplicationID                    AS INTEGER           NO-UNDO.
DEFINE VARIABLE logAllWentOk                        AS LOGICAL           NO-UNDO.
DEFINE VARIABLE logDuplicateFound                   AS LOGICAL           NO-UNDO.

/* Db Objects */
DEFINE VARIABLE newCronEventRun                     AS newRecord.
DEFINE VARIABLE updCronEventRun                     AS updRecord.
DEFINE VARIABLE updCronEvent                        AS updRecord.

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

/* Set variables to be used later */
ASSIGN intRunning     = fGetTypeID("CronEventRunResult", "Running")
       intCompleted   = fGetTypeID("CronEventRunResult", "Completed")
       intCronEventID = INTEGER(ENTRY(1,SESSION:PARAMETER,","))
       intGateUserID  = INTEGER(ENTRY(2,SESSION:PARAMETER,","))
       chrEmailError  = "Cron Event " + STRING(intCronEventID) + " has finished in Error - ".

/* Find the CronErrors EmailGroup for reporting issues or send an email to the SuperUsers EmailGroup */  
FIND FIRST EmailGroup NO-LOCK /* idx=GroupCode */
   WHERE EmailGroup.GroupCode = "CronErrors" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
DO:
   intCronEmailGroupID = 1.

   RUN osSendMail.p (INPUT "",                                   /* Optional list of Users */
                     INPUT "CronErrors EmailGroup is missing",   /* Email Subject */
                     INPUT "CronErrors EmailGroup is missing "
                              + "sent from Cron Wrapper.",       /* Plain text message Body */
                     INPUT "",                                   /* Html format message Body */
                     INPUT "",                                   /* File path../idev/hyperthermbr/files/file OR ../files/file */
                     INPUT intCronEmailGroupID,                  /* EmailGroupID that you want to send this to */
                     INPUT 0).                                   /* File MasterID is it applies */
END. /*IF NOT AVAILABLE EmailGroup THEN*/

/* Check if the provided GateUserID is valid */
FIND FIRST gate.GateUser NO-LOCK /* idx=GateUserID */
   WHERE gate.GateUser.GateUserID = intGateUserID NO-ERROR.
IF NOT AVAILABLE gate.GateUser THEN 
DO:
   RUN osSendMail.p (INPUT "",                                                              /* Optional list of Users */
                     INPUT chrEmailError + "GateUser Record Not Found",                     /* Email Subject */
                     INPUT "GateUser of GateUserID: " + STRING(intGateUserID) 
                              + " does not Exist.  CronEventID: " + STRING(intCronEventID), /* Plain text message Body */
                     INPUT "",                                                              /* Html format message Body */
                     INPUT "",                                                              /* File path../idev/hyperthermbr/files/file OR*/
                                                                                            /* ../files/file */
                     INPUT intCronEmailGroupID,                                             /* EmailGroupID that you want to send this to */
                     INPUT 0).                                                              /* File MasterID is it applies */

   RELEASE EmailGroup    NO-ERROR.
   RELEASE gate.GateUser NO-ERROR.

   RETURN.   
END. /*IF NOT AVAILABLE gate.GateUser THEN*/

chrEmailAddress = gate.GateUser.Email.

/* Check if the provided CronEventID is valid */
FIND FIRST CronEvent NO-LOCK /* idx=CronEventID */
   WHERE CronEvent.CronEventID = intCronEventID NO-ERROR.
IF NOT AVAILABLE CronEvent THEN
DO:
   RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                               ""                                
                            ELSE chrEmailAddress),                   /* Optional list of Users */
                     INPUT chrEmailError + "Record Not Found",       /* Email Subject */
                     INPUT "CronEventID: " + STRING(intCronEventID) 
                             + " does not Exist.",                    /* Plain text message Body */
                     INPUT "",                                        /* Html format message Body */
                     INPUT "",                                        /* File path../idev/hyperthermbr/files/file OR ../files/file */
                     INPUT (IF gate.GateUser.UserName = "cron" THEN 
                               intCronEmailGroupID
                            ELSE 0),                                  /* EmailGroupID that you want to send this to */
                     INPUT 0).                                        /* File MasterID is it applies */
   
   RELEASE EmailGroup    NO-ERROR.
   RELEASE gate.GateUser NO-ERROR.
   RELEASE CronEvent     NO-ERROR.
   
   RETURN.
END. /*IF NOT AVAILABLE CronEvent THEN*/

ASSIGN chrCronDescription  = CronEvent.EventDescr
       chrEmailError       = "Cron " + chrCronDescription + " has finished in Error - "
       chrCurrentEnv       = fGetCurrentEnvironment()
       chrCurrentDirectory = fGetCurrentDirectory()
       intApplicationID    = fGetCurrentApplicationID().
       
/* Check for the Application */
FIND FIRST Application NO-LOCK /* idx=ApplicationID */
   WHERE Application.ApplicationID = intApplicationID NO-ERROR.
IF NOT AVAILABLE Application THEN
DO:
   RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                               ""                                
                            ELSE chrEmailAddress),                         /* Optional list of Users */
                     INPUT chrEmailError + "Application Record Not Found", /* Email Subject */
                     INPUT "Application of ApplicationID: " + STRING(intApplicationID) + " does not Exist." 
                               + " CronEventID: " + STRING(CronEvent.CronEventID) + " Description: " 
                               + chrCronDescription,                      /* Plain text message Body */
                     INPUT "",                                            /* Html format message Body */
                     INPUT "",                                            /* File path../idev/hyperthermbr/files/file OR ../files/file */
                     INPUT (IF gate.GateUser.UserName = "cron" THEN 
                               intCronEmailGroupID
                            ELSE 0),                                      /* EmailGroupID that you want to send this to */
                     INPUT 0).                                            /* File MasterID is it applies */
   
   RELEASE EmailGroup    NO-ERROR.
   RELEASE gate.GateUser NO-ERROR.
   RELEASE CronEvent     NO-ERROR.
   
   RETURN.
END. /*IF NOT AVAILABLE Application THEN*/

/* If the environment is Dev or QA then check if the CronEvent has a NIRS specified and set the ProPath now */
IF chrCurrentEnv = "DEV" OR 
   chrCurrentEnv = "QA" THEN
DO:   
   IF NUM-ENTRIES(chrCurrentDirectory,"/") > 2 THEN
      chrRootDirectory = "/" + ENTRY(2,chrCurrentDirectory,"/") + "/" + ENTRY(3,chrCurrentDirectory,"/").
    
   /* Add the specified directory to the propath of the session so program will pick up other dependant programs */
   IF chrRootDirectory > "" AND 
      CronEvent.DirectoryToRunFrom <> "" THEN
   DO:
      IF fValidDirectory(chrRootDirectory + "/nirs/" + CronEvent.DirectoryToRunFrom) = "Ok"  THEN
      DO:
        /* Set Propath to be NIRS Project */
         chrPropath = PROPATH.
         IF INDEX(chrPropath,"nirs") = 0 THEN
         DO:
            REPEAT intEntry = 1 TO NUM-ENTRIES(chrPropath):
               IF NUM-ENTRIES(ENTRY(intEntry,chrPropath),"/") > 3 AND 
                  INDEX(ENTRY(intEntry,chrPropath), Application.PackageName) > 0 THEN 
               DO:
                  IF chrNirsPropath > "" THEN
                     chrNirsPropath = chrNirsPropath + ",".
               
                  chrNirsPropath = chrNirsPropath + chrRootDirectory + "/nirs/" + CronEvent.DirectoryToRunFrom + "/" 
                                      + ENTRY(4,ENTRY(intEntry,chrPropath),"/").
                                    
               END. /*IF NUM-ENTRIES(ENTRY(intEntry,chrPropath),"/") > 3*/
               PROPATH = chrNirsPropath + "," + chrPropath.
            END. /*REPEAT intEntry = 1 TO NUM-ENTRIES(chrPropath):*/
         END. /*IF INDEX(chrPropath,"nirs") = 0 THEN*/
      END. /*IF fValidDirectory(chrRootDirectory + "/nirs/" + CronEvent.DirectoryToRunFrom) = "Ok"  THEN*/
   END. /*IF NUM-ENTRIES(chrCurrentDirectory,"/") > 2 THEN*/
END. /*IF chrCurrentEnv = "DEV" OR chrCurrentEnv = "QA" THEN*/

/* Create the CronEventRun in a seperate transaction so it can be viewed as started on the WebPage */
CronEventRunBlk:
DO TRANSACTION ON ERROR UNDO, LEAVE:
   /* Now in an include file so we can share it with the Cron Login program */
   {usrCreateSession.i}
    
   /* Data Manager Access */
   RUN libDataManager.p PERSISTENT SET hdlGblLibrary(INPUT UserSession.SessionID).
   
   /* Create a new CronEventRun for this is instance of the CronEvent to track status */
   newCronEventRun = fCreateRecord("CronEventRun").
   newCronEventRun:assignField("CronEventID", CronEvent.CronEventID).
   newCronEventRun:assignField("GateUserID", intGateUserID).
   newCronEventRun:assignField("Started",fTimestamp(NOW)).
   
   chrError = newCronEventRun:getErrors().
   
   fClearSessionValue("CronEventRunID").
   intSsnCronEventRunID = fNewSessionValue("CronEventRunID").
   intSsnCronEventRunID:setValue(newCronEventRun:NewRecordUniqueID).
  
   DELETE OBJECT newCronEventRun NO-ERROR.
   
   IF chrError <> "" THEN
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                                       /* Optional list of Users */
                        INPUT chrEmailError + "Assign Failed",                              /* Email Subject */
                        INPUT "CronEventRunID of CronEventID: " + STRING(intCronEventID)
                                 + " failed to assign. Description: " + chrCronDescription, /* Plain text message Body */
                        INPUT "",                                                           /* Html format message Body */
                        INPUT "",                                                           /* File path../idev/hyperthermbr/files/file */
                                                                                            /* OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intCronEmailGroupID
                               ELSE 0),                                                     /* EmailGroupID that you want to send this to */
                        INPUT 0).                                                           /* File MasterID is it applies */
         
      RELEASE EmailGroup    NO-ERROR.
      RELEASE gate.GateUser NO-ERROR.
      RELEASE CronEvent     NO-ERROR.
   
      RETURN.
   END. /*IF chrError <> "" THEN*/
   
   /* Commit */
   RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                   OUTPUT logAllWentOK,
                                   OUTPUT chrError).
   
   /* Error Check */                                   
   IF chrError <> "" THEN                              
   DO:
      RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  ""                                
                               ELSE chrEmailAddress),                                        /* Optional list of Users */
                        INPUT chrEmailError + "Commit Failed",                               /* Email Subject */
                        INPUT "CronEventRunID of CronEventID: " + STRING(intCronEventID)
                                 + " failed to commit.  Description: " + chrCronDescription, /* Plain text message Body */
                        INPUT "",                                                            /* Html format message Body */
                        INPUT "",                                                            /* File path../idev/hyperthermbr/files/file */
                                                                                             /* OR ../files/file */
                        INPUT (IF gate.GateUser.UserName = "cron" THEN 
                                  intCronEmailGroupID
                               ELSE 0),                                                      /* EmailGroupID that you want to send this to*/
                        INPUT 0).                                                            /* File MasterID is it applies */
          
      RELEASE EmailGroup    NO-ERROR.
      RELEASE gate.GateUser NO-ERROR.
      RELEASE CronEvent     NO-ERROR.
         
      RETURN.
   END. /*IF chrError <> "" THEN*/
END. /*CronEventRunBlk:*/

ASSIGN chrLogFile        = chrLogFileDirectory + REPLACE(chrCronDescription," ", "") + "_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") 
                              + ".log"
       chrLastRunLogFile = "../logs/" + REPLACE(chrCronDescription," ", "") + ".lastrun"
       chrEmailLogFile   = chrLogFileDirectory + REPLACE(chrCronDescription," ", "") + "_ID_" + STRING(intSsnCronEventRunID:intValue) 
                              + ".log"
       chrErrorFile      = chrLogFileDirectory + "Err_" + REPLACE(chrCronDescription," ", "") + "_ID_" 
                              + STRING(intSsnCronEventRunID:intValue) + ".log".                                                 

/* Setup log files */
OUTPUT STREAM sToLogFile   TO VALUE(chrLogFile) APPEND.
OUTPUT STREAM sToLastFile  TO VALUE(chrLastRunLogFile).
OUTPUT STREAM sToEmailFile TO VALUE(chrEmailLogFile).

IF CronEvent.LoggingIsOn = TRUE THEN 
   fLog("Running for CronEventRunID: " + STRING(intSsnCronEventRunID:intValue)). 

/* Output default Stream to File to catch Progress Errors that we're not expecting. All Cron Programs should not use the unnamed stream
   for anything else.  If they do this program will not be able to track the Progress Errors after that point. */  
OUTPUT TO VALUE(chrErrorFile).

/* Check if this CronEvent should pause before executing.  This allows events to be offset to run in the same minute. */
IF CronEvent.SecondsToPauseBeforeExecuting > 0 THEN
DO:
   IF CronEvent.LoggingIsOn = TRUE THEN 
      fLog("Starting to pause " + STRING(CronEvent.SecondsToPauseBeforeExecuting) + " seconds").
   
   PAUSE CronEvent.SecondsToPauseBeforeExecuting NO-MESSAGE.
   
   IF CronEvent.LoggingIsOn = TRUE THEN 
      fLog("Pause has ended resuming now").
END. /*IF CronEvent.SecondsToPauseBeforeExecuting > 0 THEN*/

/* This is used to find the UNIX PID for the connection, which will be the UNIX PID from batch command */
FIND FIRST _MyConnection NO-LOCK NO-ERROR. /* idx=_MyConnection-Id WHOLE-INDEX */
IF NOT AVAILABLE _MyConnection THEN
DO:
   IF CronEvent.LoggingIsOn = TRUE THEN 
      fLog("No _MyConnection was Available."). 
       
   RELEASE EmailGroup    NO-ERROR.
   RELEASE gate.GateUser NO-ERROR.
   RELEASE CronEvent     NO-ERROR.
   RELEASE _MyConnection NO-ERROR.

   RETURN.
END. /*IF NOT AVAILABLE _MyConnection THEN*/      

intUnixPID = _MyConnection._MyConn-Pid.
                                         
IF CronEvent.LoggingIsOn = TRUE THEN 
   fLog("Before Setup_Block."). 

/* Setup variables, User Session, and commit CronEventRun.CronEventRunResultTypeID of Running to allow viewability outside of the program */
Setup_Block:
DO TRANSACTION:
   /* Check for the CronEventRun that should have just commited to the database */
   FIND FIRST CronEventRun NO-LOCK /* idx=CronEventRunID */
      WHERE CronEventRun.CronEventRunID = intSsnCronEventRunID:intValue NO-ERROR.
   IF NOT AVAILABLE CronEventRun THEN
   DO:
      ASSIGN chrEmailAddress = ""
             chrEmailSubject = "Cron Event has finished in Error - Record Not Found"
             chrEmailBody    = "CronEventRun of CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " does not Exist.".
             
      LEAVE Setup_Block.
   END. /*IF NOT AVAILABLE CronEventRun THEN*/
   
   /* Find the CronEvent.EventDescr since this will be used in log files */
   FIND FIRST CronEvent NO-LOCK /* idx=CronEventID */
      WHERE CronEvent.CronEventID = intCronEventID NO-ERROR.
   IF AVAILABLE CronEvent THEN
      chrCronDescription = CronEvent.EventDescr.
   
   /* Check the Environment is available and set to active*/
   FIND FIRST Environment NO-LOCK /* idx=EnvironmentCode */
      WHERE Environment.EnvironmentCode = chrGblEnvironment
      AND   Environment.ACTIVE          = TRUE NO-ERROR.
   IF NOT AVAILABLE Environment THEN
   DO:
      ASSIGN chrEmailAddress = ""
             chrEmailSubject = chrEmailError + "Active Environment Record Not Found"
             chrEmailBody    = "Active Environment Record does not Exist.  Environment: " + chrGblEnvironment + " CronEventID: "
                                  + STRING(intCronEventID) + " CronEventRunID: " + STRING(CronEventRun.CronEventRunID) + " UnixPID: "
                                  + STRING(intUnixPID) + " Description: " + chrCronDescription.

      LEAVE Setup_Block.
   END. /*IF NOT AVAILABLE Environment THEN*/
   
   /* Get CronEventRun Record  */
   updCronEventRun = fGetRecord("CronEventRun", intSsnCronEventRunID:intValue).
   updCronEventRun:assignField("UnixPID", intUnixPID).
   updCronEventRun:assignField("Started", fTimeStamp(NOW)).
   updCronEventRun:assignField("CronEventRunResultTypeID",  intRunning).

   /* Get Errors */
   chrError = chrError + updCronEventRun:getErrors().
         
   /* Error Check */
   IF chrError <> "" THEN
   DO:
      ASSIGN chrEmailSubject = chrEmailError + "Failed to Assign Field"
             chrEmailBody    = "The assign updCronEventRun for UnixPID, Started, and CronEventRunResultTypeID has failed."
                                  + " CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) 
                                  + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrCronDescription + " Errors: " + chrError.
      
      LEAVE Setup_Block.
   END. /*IF chrError <> "" THEN*/
  
   DELETE OBJECT updCronEventRun NO-ERROR.
   
   /* Commit */
   RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                   OUTPUT logAllWentOK,
                                   OUTPUT chrError).
   
   /* Error Check */                                   
   IF chrError <> "" THEN                              
   DO:
      ASSIGN chrEmailSubject = chrEmailError + "Record Not Found and Commit Failed"
             chrEmailBody    = "CronEvent of CronEventID " + STRING(intCronEventID) + " does not Exist and the commit for updCronEventRun"
                                  + " has failed.  CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " 
                                  + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrCronDescription 
                                  + " Errors: " + chrError.
      
      LEAVE Setup_Block.
   END. /*IF chrError <> "" THEN*/
   
   /* Check if any previous CronEventRuns are still running for this same CronEventID */
   Running_Block:
   FOR EACH CronEventRun NO-LOCK /* idx=CronEventResultTypeCronEvent */
      WHERE CronEventRun.CronEventID              = intCronEventID
      AND   CronEventRun.CronEventRunID           <> intSsnCronEventRunID:intValue
      AND   CronEventRun.CronEventRunResultTypeID = intRunning:
      
      /* Check if the CronEventRun PID is still valid.  It might have finished and never was updated. */
      IF fValidPID(INPUT CronEventRun.UnixPID) = TRUE THEN
      DO:
         logDuplicateFound = TRUE.
         
         /* If Cron is running this we do not need an email everytime it tries, which could be every minute */
         IF gate.GateUser.Username <> "cron" THEN
            ASSIGN chrEmailSubject   = "Cron Event " + STRING(intCronEventID) + " has finished - Already Running"
                   chrEmailBody      = "CronEvent of CronEventID: " + STRING(intCronEventID) + " already is running as CronEventRunID: " 
                                       + STRING(CronEventRun.CronEventRunID) + ".  Can not start a duplicate CronEventRunID: " 
                                       + STRING(intSsnCronEventRunID:intValue) + " Description: " + chrCronDescription.
                
         LEAVE Running_Block. 
      END. /*IF fValidPID(INPUT CronEventRun.UnixPID) = TRUE THEN*/ 
   END. /*FOR EACH CronEventRun NO-LOCK*/
                                     
   /* Mark this CronEventRun as Duplicate since a previous one is still running and has a valid PID */
   IF logDuplicateFound = TRUE THEN 
   DO:
      /* Get CronEventRun Record  */
      updCronEventRun = fGetRecord("CronEventRun", intSsnCronEventRunID:intValue).
      updCronEventRun:assignField("FinishedOk", FALSE).
      updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
      updCronEventRun:assignField("CronEventRunResultTypeID", fGetTypeID("CronEventRunResult", "Duplicate")).
      
      /* Get Errors */
      chrError = chrError + updCronEventRun:getErrors().
             
      /* Error Check */
      IF chrError <> "" THEN
      DO:
         ASSIGN logAllWentOK    = FALSE
                chrEmailSubject = chrEmailError + "Failed to Assign Field"
                chrEmailBody    = "The assign updCronEventRun for UnixPID, Completed, and CronEventRunResultTypeID has failed." 
                                     + " CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " 
                                     + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrCronDescription 
                                     + " Errors: " + chrError.
         
         LEAVE Setup_Block.
      END. /*IF chrError <> "" THEN*/
    
      /* Commit */
      RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                      OUTPUT logAllWentOK,
                                      OUTPUT chrError).
    
      /* Error Check */                                   
      IF chrError <> "" THEN                              
      DO:
         ASSIGN logAllWentOK    = FALSE
                chrEmailSubject = chrEmailError + "Duplicate Found and Commit Failed"
                chrEmailBody    = "CronEvent of CronEventID " + STRING(intCronEventID) + " duplicate found and the commit for"
                                     + " updCronEventRun has failed.  CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) 
                                     + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                     + chrCronDescription + " Errors: " + chrError.
                
         LEAVE Setup_Block.
      END. /*IF chrError <> "" THEN*/
      
      /* Remove the Error File if this is a duplicate event.  The File should be blank if there are no errors */
      IF chrError = "" THEN
         fDeleteFile(chrErrorFile) NO-ERROR.
      
      logAllWentOK = FALSE.
      LEAVE Setup_Block.
   END. /*IF logDuplicateFound = TRUE THEN*/
   
   /* Only set this flag when we know all is done without Error */
   logAllWentOk = TRUE.
END. /*Setup_Block:*/

IF logAllWentOk = TRUE THEN 
DO:
   /* Reset this flag again for the next transaction block */
   logAllWentOk = FALSE.

   Main_Block:
   DO:
      /* Check for a valid CronEvent again */
      FIND FIRST CronEvent NO-LOCK /* idx=CronEventID */
         WHERE CronEvent.CronEventID = intCronEventID NO-ERROR.
      IF NOT AVAILABLE CronEvent THEN
      DO:
         updCronEventRun:assignField("FinishedOk", FALSE).
         updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
         updCronEventRun:assignField("CronEventRunResultTypeID",  fGetTypeID("CronEventRunResult", "CronEventMissing")).                       
        
         /* Get Errors */
         chrError = chrError + updCronEventRun:getErrors().
        
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Record Not Found and Failed to Assign Field"
                   chrEmailBody    = "CronEvent of CronEventID: " + STRING(intCronEventID) + " does not Exist.  The Assign updCronEventRun"
                                        + " for FinishedOk, Completed, and CronEventRunResultTypeID has failed. CronEventRunID: " 
                                        + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " 
                                        + STRING(intUnixPID) + " Description: " + chrCronDescription + " Errors: " + chrError.
    
            LEAVE Main_Block.
         END. /*IF chrError <> "" THEN*/

         /* Commit */
         RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                         OUTPUT logAllWentOK,
                                         OUTPUT chrError).
           
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Record Not Found and Commit Failed"
                   chrEmailBody    = "CronEvent of CronEventID " + STRING(intCronEventID) + " does not Exist and the commit for"
                                        + " updCronEventRun has failed.  CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) 
                                        + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                        + chrCronDescription + " Errors: " + chrError.
         END. /*IF chrError <> "" THEN*/ 
        
         LEAVE Main_Block.
      END. /*IF NOT AVAILABLE CronEvent THEN*/
       
      IF CronEvent.LoggingIsOn = TRUE THEN 
      DO:
         fLog("GateUserID: " + STRING(intGateUserID) + " -> " + THIS-PROCEDURE:NAME + " CronEventRunID: " 
                 + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID)).
         logGblDebugging = TRUE.
      END. /*IF CronEvent.LoggingIsOn = TRUE THEN*/
      
      /* Clear and set Session Variables */
      FOR EACH CronEventParameter NO-LOCK /* idx=CronEventIDParameterName */
         WHERE CronEventParameter.CronEventID = intCronEventID:
           
         fClearSessionValue(CronEventParameter.ParameterName).
         chrSsnParameter = fNewSessionValue(CronEventParameter.ParameterName).
         chrSsnParameter:setValue(CronEventParameter.ParameterValue).
           
         IF CronEvent.LoggingIsOn = TRUE THEN 
            fLog("Found Parameter: " + CronEventParameter.ParameterName + " Value: "  + CronEventParameter.ParameterValue). 
                            
      END. /*FOR EACH CronEventParameter NO-LOCK*/
        
      IF CronEvent.LoggingIsOn THEN
         fLog("All Session Variables have been set.").
    
      /* Check for a ProcessProgram to makes sure it is valid */
      FIND FIRST ProcessProgram NO-LOCK /* idx=ProcessProgramID */
         WHERE ProcessProgram.ProcessProgramID = CronEvent.ProcessProgramID NO-ERROR.
      IF NOT AVAILABLE ProcessProgram THEN
      DO:
         IF CronEvent.LoggingIsOn THEN
            fLog("ProcessProgram of ProcessProgramID: " + STRING(CronEvent.ProcessProgramID)).
           
         /* Set CronEventRun to show the program was not found */
         updCronEventRun = fGetRecord("CronEventRun", intSsnCronEventRunID:intValue).
         updCronEventRun:assignField("FinishedOk", FALSE).
         updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
         updCronEventRun:assignField("CronEventRunResultTypeID",  fGetTypeID("CronEventRunResult", "ProgramMissing")).
       
         /* Get Errors */
         chrError = chrError + updCronEventRun:getErrors().
            
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Record Not Found and Failed to Assign Field"
                   chrEmailBody    = "ProcessProgram of ProcessProgramID: " + STRING(CronEvent.ProcessProgramID) + " does not Exist and"
                                        + " the assign updCronEventRun for FinishedOk, Completed, and CronEventRunResultTypeID has failed."
                                        + " CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " 
                                        + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrCronDescription 
                                        + " Errors: " + chrError.
             
            LEAVE Main_Block.
         END. /*IF chrError <> "" THEN*/
        
         /* Commit */
         RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                         OUTPUT logAllWentOK,
                                         OUTPUT chrError).
         
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Record Not Found and Failed to commit"
                   chrEmailBody    = "ProcessProgram of ProcessProgramID: " + STRING(CronEvent.ProcessProgramID) + " does not Exist and"
                                        + " the commit for updCronEventRun has failed.  CronEventRunID: " 
                                        + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " 
                                        + STRING(intUnixPID) + " Description: " + chrCronDescription + " Errors: " + chrError.
         END. /*IF chrError <> "" THEN*/ 
          
         LEAVE Main_Block.
      END. /*IF NOT AVAILABLE ProcessProgram THEN*/
    
      IF CronEvent.LoggingIsOn = TRUE THEN 
         fLog("Program Name: " + ProcessProgram.ProgramName + " has been found in ProcessProgram.").
      
      RELEASE Application NO-ERROR.
      
      /* Should still have this record created since it was checked earlier, but just in case */
      FIND FIRST Application NO-LOCK /* idx=ApplicationID */
         WHERE Application.ApplicationID = intGblApplicationID NO-ERROR.
      IF NOT AVAILABLE Application THEN
      DO:
         ASSIGN chrEmailSubject = chrEmailError + "Application Record Not Found"
                chrEmailBody    = "Application of ApplicationID: " + STRING(intGblApplicationID) + " does not Exist." 
                                      + " CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) 
                                      + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                      + chrCronDescription + " Errors: " + chrError.
            
         /* Set CronEventRun to show the program was not found */
         updCronEventRun = fGetRecord("CronEventRun", intSsnCronEventRunID:intValue).
         updCronEventRun:assignField("FinishedOk", FALSE).
         updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
         updCronEventRun:assignField("CronEventRunResultTypeID",  fGetTypeID("CronEventRunResult", "Error")).
       
         /* Get Errors */
         chrError = chrError + updCronEventRun:getErrors().
            
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Application Record Not Found and Failed to Assign Field"
                   chrEmailBody    = "Application of ApplicationID: " + STRING(intGblApplicationID) + " does not Exist and"
                                        + " the assign updCronEventRun for FinishedOk, Completed, and CronEventRunResultTypeID has failed."
                                        + " CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " 
                                        + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrCronDescription 
                                        + " Errors: " + chrError.
             
            LEAVE Main_Block.
         END. /*IF chrError <> "" THEN*/
        
         /* Commit */
         RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                         OUTPUT logAllWentOK,
                                         OUTPUT chrError).
         
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Application Record Not Found and Failed to commit"
                   chrEmailBody    = "Application of ApplicationID: " + STRING(intGblApplicationID) + " does not Exist and"
                                        + " the commit for updCronEventRun has failed.  CronEventRunID: " 
                                        + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " 
                                        + STRING(intUnixPID) + " Description: " + chrCronDescription + " Errors: " + chrError.
         END. /*IF chrError <> "" THEN*/ 
          
         LEAVE Main_Block.                         
      END. /*IF NOT AVAILABLE Application THEN*/ 
      
      RELEASE Application NO-ERROR.  
      
      IF CronEvent.LoggingIsOn = TRUE THEN 
         fLog("DirectoryToRunFrom: " + STRING(CronEvent.DirectoryToRunFrom) + " parsed. PROPATH: " + PROPATH).
        
      /* Get CronEventRun Record  */
      updCronEventRun = fGetRecord("CronEventRun", intSsnCronEventRunID:intValue).
       
      /* Now run the program if we can find it on the propath */
      IF SEARCH(ProcessProgram.ProgramName) = ? THEN
      DO:
         IF CronEvent.LoggingIsOn = TRUE THEN
            fLog("Program was not found.").
   
         ASSIGN chrEmailSubject = chrEmailError + "Program not found"
                chrEmailBody    = "Program was not found".
   
         /* Set CronEventRun to show the program was not found */
         updCronEventRun:assignField("FinishedOk", FALSE).
         updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
         updCronEventRun:assignField("CronEventRunResultTypeID", fGetTypeID("CronEventRunResult", "ProgramMissing")).
       
         /* Get Errors */
         chrError = chrError + updCronEventRun:getErrors().
           
          /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Program not found and failed to assign"
                   chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " was not found and the updCronEventRun FinishedOk,"
                                        + " Completed, and CronEventRunResultTypeID has failed.  CronEventID: " + STRING(intCronEventID) 
                                        + " UnixPID: " + STRING(intUnixPID) + " Description: " + chrCronDescription + " Errors: " 
                                        + chrError.
      
            LEAVE Main_Block.   
         END. /*IF chrError <> "" THEN*/ 
        
         /* Error Check */  
         RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                         OUTPUT logAllWentOK,
                                         OUTPUT chrError).
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Program not found and Failed to commit"
                   chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " was not found and the commit for updCronEventRun has"
                                        + " failed. CronEventID: " + STRING(intCronEventID) + " UnixPID: "  + STRING(intUnixPID) 
                                        + " Description: " + chrCronDescription + " Errors: " + chrError.
            LEAVE Main_Block.
         END. /* IF chrError <> "" */
         
         logAllWentOk = FALSE.
         LEAVE Main_Block.
      END. /*IF SEARCH(ProcessProgram.ProgramName) = ? THEN*/
     
      IF CronEvent.LoggingIsOn = TRUE THEN
         fLog("Program has been located in PROPATH.").
      
      /* Find any unexpected Progress syntax errors.  This is left without NO-ERROR to trap the warnings and errors into the log file. */
      COMPILE VALUE(ProcessProgram.ProgramName).
       
      /* If Compiled without errors */
      IF NOT COMPILER:ERROR THEN 
      DO:
         IF CronEvent.LoggingIsOn = TRUE THEN
            fLog("Program has syntaxed.  Running now.").
         
         /* Run the program */
         RUN VALUE(ProcessProgram.ProgramName) NO-ERROR.
     
         IF CronEvent.LoggingIsOn = TRUE THEN
         DO:
            fLog("Program has finished. Errors found: " + STRING(ERROR-STATUS:ERROR)).
            fLog("Checking for internal Progress Errors from: " + chrErrorFile).
         END. /*IF CronEvent.LoggingIsOn = TRUE THEN*/
      END. /*IF NOT COMPILER:ERROR THEN*/
      
      /* Close the output to read in any Compiler or Runtime Progress Errors */
      OUTPUT CLOSE.

      /* Import from Error log*/
      INPUT FROM VALUE(chrErrorFile).

      REPEAT:
         IMPORT UNFORMATTED chrFileLine.
              
         /* Add these to chrError for later use */
         chrError = chrError + chrFileLine.
      END. /*REPEAT:*/

      INPUT CLOSE.

      /* Check for errors in the Program, syntax issues, or if it had a Progress Runtime Error */
      IF ERROR-STATUS:ERROR OR 
         COMPILER:ERROR OR 
         chrError <> "" THEN
      DO:
        
         IF CronEvent.LoggingIsOn = TRUE THEN
            fLog("Parsing errors now.").
         
         /* Change the error message depending on what kind of error it was */ 
         IF COMPILER:ERROR THEN 
            chrError = " Compile Errors: " + chrError.
         ELSE
            chrError = " Program Errors: " + chrError.
         
         /* If it was was a error in the program */ 
         IF ERROR-STATUS:ERROR THEN 
         DO:
            /* Check for specific program error being returned back */
            IF RETURN-VALUE > "" THEN 
               ASSIGN chrProgramError = RETURN-VALUE
                      chrError        = chrError + " " + chrProgramError + " ".
            
            /* Check error message for program */ 
            DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:                                                                  
               chrError = chrError + STRING(ERROR-STATUS:GET-MESSAGE(intErrorCount)).                                        
            END. /*DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:*/       
         END. /*IF ERROR-STATUS:ERROR THEN*/
            
         /* Get CronEventRun Record  */
         updCronEventRun = fGetRecord("CronEventRun", intSsnCronEventRunID:intValue).
         intResultTypeID = fGetTypeID("CronEventRunResult", chrProgramError).
         
         /* If this is trying to set a Result based on the Error make sure it is valid */
         IF intResultTypeID > 0 AND 
            CAN-FIND(FIRST CronEventRunResultType NO-LOCK /* idx=CronEventRunResultTypeID */
                        WHERE CronEventRunResultType.CronEventRunResultTypeID = intResultTypeID
                        AND   CronEventRunResultType.IsError                  = TRUE 
                        AND   CronEventRunResultTYpe.ACTIVE                   = TRUE) THEN
            updCronEventRun:assignField("CronEventRunResultTypeID", intResultTypeID).
         ELSE
            updCronEventRun:assignField("CronEventRunResultTypeID", fGetTypeID("CronEventRunResult", "Error")).
        
         /* Debugging mesage to show there as error */
         IF CronEvent.LoggingIsOn AND 
            chrError <> "" THEN
            fLog("Program error returned: " + chrError).
         
         ASSIGN chrEmailSubject  = chrEmailError + "Program Error"
                chrEmailBody     = "The Program: " + ProcessProgram.ProgramName + " had an error.  CronEventRunID: " 
                                      + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " 
                                      + STRING(intUnixPID) + " Description: " + chrCronDescription 
                chrProgressError = "None".

         /* If there were errors attach them to the email */
         IF chrError <> "" THEN
            ASSIGN chrEmailBody     = chrEmailBody + chrError
                   chrProgressError = chrError.
        
         /* Set CronEventRun to show the program did not finish correctly and there was an error */
         updCronEventRun:assignField("FinishedOk", FALSE).
         updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
         
         /* Get Errors */
         chrError = updCronEventRun:getErrors().
             
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            ASSIGN chrEmailSubject = chrEmailError + "Program Error and Failed to Assign Field"
                   chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " had an Error and the updCronEventRun FinishedOk,"
                                        + " Completed, and CronEventRunResultTypeID assign has failed.   CronEventRunID: " 
                                        + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " 
                                        + STRING(intUnixPID) + " Description: " + chrCronDescription + chrProgressError 
                                        + " Assign Errors: " + chrError.
            LEAVE Main_Block.
         END. /*IF chrError <> "" THEN*/
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
                      chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " had an Error and the commit for updCronEventRun has"
                                           + " failed.  CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " 
                                           + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                           + chrCronDescription + chrProgressError + " Errors: " + chrError.
               LEAVE Main_Block.
            END. /*IF chrError <> "" THEN*/ 
         END. /*IF chrError = "" THEN*/
         
         logAllWentOk = FALSE.
        
         IF CronEvent.LoggingIsOn = TRUE THEN
            fLog("Leaving Main_Block. STATUS: " + STRING(logAllWentOk) + chrError).
           
         LEAVE Main_Block.
      END. /*IF ERROR-STATUS:ERROR OR COMPILER:ERROR OR chrError <> "" THEN*/
      
      /* Get CronEventRun Record  */
      updCronEventRun = fGetRecord("CronEventRun", intSsnCronEventRunID:intValue).    
      updCronEventRun:assignField("FinishedOk", TRUE).
      updCronEventRun:assignField("Completed", fTimeStamp(NOW)).
      updCronEventRun:assignField("CronEventRunResultTypeID", intCompleted).
     
      /* Get Errors */
      chrError = chrError + updCronEventRun:getErrors().
              
      IF CronEvent.LoggingIsOn = TRUE THEN
         fLog("Assigned Finished OK and completed " + chrError).
   
      /* Error Check */
      IF chrError <> "" THEN
      DO:
         IF CronEvent.LoggingIsOn = TRUE THEN
            fLog("Completed AND failed TO ASSIGN.").

         ASSIGN chrEmailSubject = chrEmailError + "Program Completed and Failed to assign"
                chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " has completed but the updCronEventRun FinishedOk,"
                                     + " Completed, and CronEventRunResultTypeID assign has failed. CronEventRunID: " 
                                     + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " 
                                     + STRING(intUnixPID) + " Description: " + chrCronDescription + " Errors: " + chrError.

         LEAVE Main_Block.
      END. /*IF chrError <> "" THEN*/ 
      
      /* Error Check */  
      RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                      OUTPUT logAllWentOK,
                                      OUTPUT chrError).                                                                                 

      /* Error Check */
      IF chrError <> "" THEN
      DO:
         ASSIGN chrEmailSubject = chrEmailError + "Failed to commit"
                chrEmailBody    = "Program: " + ProcessProgram.ProgramName + " commit for updCronEventRun has failed. CronEventRunID: " 
                                     + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " + STRING(intCronEventID) + " UnixPID: " 
                                     + STRING(intUnixPID) + " Description: " + chrCronDescription + " Errors: " + chrError.
       
         LEAVE Main_Block.
      END. /*IF chrError <> "" THEN*/

      IF CronEvent.LoggingIsOn = TRUE THEN
        fLog("Program has finished without errors and CronEventRun final commit is complete.").
   
      logAllWentOk = TRUE.
   END. /*Main_Block:*/
END. /*IF logAllWentOk = TRUE THEN*/

RELEASE CronEventRun NO-ERROR.

/* If previous blocks finished Ok then calculate metrics */
IF logAllWentOk = TRUE THEN
DO:
   logAllWentOk = FALSE.
   Metrics_Block:
   DO TRANSACTION:
       FIND FIRST CronEventRun NO-LOCK /* idx=CronEventRunID */
         WHERE CronEventRun.CronEventRunID = intSsnCronEventRunID:intValue NO-ERROR.
       IF AVAILABLE CronEventRun THEN
       DO:
          ASSIGN intArrayCount = 1
                 intRunTime[intArrayCount] = 0.
          
          IF CronEventRun.Completed > "" THEN
             ASSIGN intRunTime[intArrayCount] = fInterval(CronEventRun.Completed, CronEventRun.Started, "Seconds")
                    intAverageRun             = intAverageRun + intRunTime[intArrayCount].
          
          /* Check previous 9 CronEvnetRuns for this CronEvent to get an average for metrics */
          Previous_Block:
          DO WHILE intArrayCount < 10:
             FIND PREV CronEventRun NO-LOCK /* idx=CronEventIDStarted */
                WHERE CronEventRun.CronEventID = intCronEventID NO-ERROR.
             IF NOT AVAILABLE CronEventRun THEN
                LEAVE Previous_Block.
             
             /* If this CronEventRun has Completed stamp then use it else try to find the next one to use good data for metrics */
             IF CronEventRun.Completed > "" THEN
             DO:
                ASSIGN intArrayCount = intArrayCount + 1
                       intRunTime[intArrayCount] = fInterval(CronEventRun.Completed, CronEventRun.Started, "Seconds")
                       intAverageRun = intAverageRun + intRunTime[intArrayCount].
                    
                IF CronEvent.LoggingIsOn = TRUE THEN
                   fLog("Metrics using previous CronEventRunID: " + STRING(CronEventRun.CronEventRunID) + " " 
                            + STRING(intRunTime[intArrayCount])).
             END. /*IF CronEventRun.Completed > "" THEN*/
          END. /*Previous_Block:*/
          
          IF CronEvent.LoggingIsOn = TRUE THEN
             fLog("Average: " + STRING(intAverageRun) + " / " + STRING(intArrayCount)).
          
          /* Calculate the average run */
          intAverageRun = intAverageRun / intArrayCount.
                    
          /* Find the current CronEvent to update the metrics */          
          FIND FIRST CronEvent NO-LOCK /* idx=CronEventId */
             WHERE CronEvent.CronEventID = intCronEventID NO-ERROR.
          IF AVAILABLE CronEvent THEN
          DO:
             /* Get CronEventRun Record  */
             updCronEvent = fGetRecord("CronEvent", intCronEventID).
             
             /* Check if MinimumRunDuration needs to be updated for this current CronEventRun */
             IF CronEvent.MinimumRunDuration > intRunTime[1] OR
                CronEvent.MinimumRunDuration = 0 THEN
             DO:
                updCronEvent:assignField("MinimumRunDuration", intRunTime[1]).
                IF CronEvent.LoggingIsOn = TRUE THEN
                   fLog("Updating MinimumRunDuration: " + STRING(intRunTime[1])).   
             END. /*IF CronEvent.MinimumRunDuration > intRunTime[1] OR CronEvent.MinimumRunDuration = 0 THEN*/
             
             /* Check if MaximumRunDuation needs to be updated for this current CronEventRun */   
             IF CronEvent.MaximumRunDuration < intRunTime[1] THEN
             DO:
                updCronEvent:assignField("MaximumRunDuration", intRunTime[1]).
                IF CronEvent.LoggingIsOn = TRUE THEN
                   fLog("Updating MaximumRunDuration: " + STRING(intRunTime[1])).
             END. /*IF CronEvent.MaximumRunDuration < intRunTime[1] THEN*/
             
             /* Check if the AverageRunDuration needs to be updated for the new calculation */
             IF CronEvent.AverageRunDuration <> intAverageRun THEN
             DO:
                updCronEvent:assignField("AverageRunDuration", intAverageRun).
                IF CronEvent.LoggingIsOn = TRUE THEN
                   fLog("Updating AverageRunDuration: " + STRING(intAverageRun)).
             END. /*IF CronEvent.AverageRunDuration <> intAverageRun THEN*/
                
             /* Get Errors */
             chrError = chrError + updCronEvent:getErrors().
            
             /* Error Check */
             IF chrError <> "" THEN
             DO:
                ASSIGN chrEmailSubject = chrEmailError + "Failed to Assign Field"
                       chrEmailBody    = "The assign updCronEvent for MinimumRunDuration, MaximumRunDuration, and AverageRunDuration"
                                            + " has failed. CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " 
                                            + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                            + chrCronDescription + " Errors: " + chrError.
                  
                LEAVE Metrics_Block.
             END. /*IF chrError <> "" THEN*/
              
             DELETE OBJECT updCronEvent NO-ERROR.
               
             /* Commit */
             RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                             OUTPUT logAllWentOK,
                                             OUTPUT chrError).
               
             /* Error Check */                                   
             IF chrError <> "" THEN                              
             DO:
                ASSIGN chrEmailSubject = chrEmailError + " and Commit Failed"
                       chrEmailBody    = "CronEventID " + STRING(intCronEventID) + " did not update metrics and the commit for updCronEvent"
                                            + " has failed.  CronEventRunID: " + STRING(intSsnCronEventRunID:intValue) + " CronEventID: " 
                                            + STRING(intCronEventID) + " UnixPID: " + STRING(intUnixPID) + " Description: " 
                                            + chrCronDescription + " Errors: " + chrError.
                  
                LEAVE Metrics_Block.
             END. /*IF chrError <> "" THEN*/
          END. /*IF AVAILABLE CronEvent THEN*/
       END. /*IF AVAILABLE CronEventRun THEN*/
       
       IF gate.GateUser.Username <> "cron" THEN
       DO:
         IF CronEvent.LoggingIsOn = TRUE THEN 
            fLog("Sending email to " + chrEmailAddress + " to show completed.").
         
         OUTPUT STREAM sToLogFile   CLOSE.
         OUTPUT STREAM sToLastFile  CLOSE.
         OUTPUT STREAM sToEmailFile CLOSE.
         OUTPUT CLOSE.

         RUN osSendMail.p (INPUT chrEmailAddress,                                                        /* Optional list of Users */
                           INPUT "Cron Manually Ran Job ID " + STRING(intCronEventID) + " has finished", /* Email Subject */
                           INPUT "Program: " + ProcessProgram.ProgramName + " has finished.",            /* Plain text message Body */
                           INPUT "",                                                                     /* Html format message Body */
                           INPUT (IF CronEvent.LoggingIsOn = TRUE THEN
                                     chrEmailLogFile
                                  ELSE ""),                                                              /* File path ../files/file */
                           INPUT 0,                                                                      /* EmailGroupID */
                           INPUT 0).                                                                     /* File MasterID */
       END. /* IF gate.GateUser.UserName <> "cron" */
       
       logAllWentOk = TRUE.   
   END. /* Metrics_Block */
END. /* IF logAllWentOk */

/* Check if there is an email to send for errors */
IF chrEmailSubject > "" THEN 
DO:
   IF CronEvent.LoggingIsOn = TRUE THEN 
   DO:
      fLog("Sending error email").
      fLog("Subject: " + chrEmailSubject).
      fLog("Body: " + chrEmailBody).
   END. /*IF CronEvent.LoggingIsOn = TRUE THEN*/
   
   OUTPUT STREAM sToLogFile   CLOSE.
   OUTPUT STREAM sToLastFile  CLOSE.
   OUTPUT STREAM sToEmailFile CLOSE.
   OUTPUT CLOSE.
   
   /* Limit the email body if it is too long for the email to send correctly */
   IF LENGTH(chrEmailBody) > 900 THEN
      chrEmailBody = STRING(chrEmailBody, "X(900)") + CHR(13) + "There is too much to be displayed.".

   RUN osSendMail.p (INPUT (IF gate.GateUser.UserName = "cron" THEN 
                               ""                                
                            ELSE chrEmailAddress),                  /* Optional list of Users */
                     INPUT chrEmailSubject,                         /* Email Subject */
                     INPUT chrEmailBody,                            /* Plain text message Body */
                     INPUT "",                                      /* Html format message Body */
                     INPUT (IF AVAILABLE CronEvent AND
                               CronEvent.LoggingIsOn = TRUE THEN
                               chrEmailLogFile
                            ELSE ""),                               /* File path ../files/file */
                     INPUT (IF gate.GateUser.UserName = "cron" THEN 
                               intCronEmailGroupID
                            ELSE 0),                                /* EmailGroupID */
                     INPUT 0).                                      /* File MasterID */ 
END. /*IF chrEmailSubject > "" THEN*/

/* Set permissions for everything in logs to be available to both manually ran and scheduled Cron */
IF gate.GateUser.Username = "cron" THEN 
   OS-COMMAND("chmod -R 777 ../logs").

/* Clean-up */
/* Delete the error file if no errors happened */
IF logAllWentOk = TRUE THEN
   fDeleteFile(chrErrorFile)   NO-ERROR.

/* Delete the email file */
fDeleteFile(chrEmailLogFile)   NO-ERROR.

DELETE OBJECT chrSsnParameter      NO-ERROR.
DELETE OBJECT intSsnCronEventRunID NO-ERROR.
DELETE OBJECT updCronEventRun      NO-ERROR.
DELETE OBJECT updCronEvent         NO-ERROR.

/* Release statements need to have gate database fully qualified because of syngate data sync */
RELEASE gate.GateUser          NO-ERROR.
RELEASE _myConnection          NO-ERROR.
RELEASE EmailGroup             NO-ERROR.
RELEASE Environment            NO-ERROR.
RELEASE CronEvent              NO-ERROR.
RELEASE CronEventRun           NO-ERROR.
RELEASE ProcessProgram         NO-ERROR.
RELEASE CronEventParameter     NO-ERROR.

DELETE PROCEDURE hdlGblLibrary NO-ERROR.
