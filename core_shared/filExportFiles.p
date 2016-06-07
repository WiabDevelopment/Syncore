/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filExportFiles.p
Purpose : Finds a FileMaster record either from the Cron via the SESSION:PARAMETER or from the web using session variables. Then checks the 
          path associated with the FileMaster and calls the Export program associated with that FileMaster.
Author  : BG
Date    : 14th June 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03/12/2013 CS  CR1021     Made compatible with the Cron Admin from the Web.  Added return error on stop condition to prevent infinite loops.
16/01/2014 CS  CR1021     Made debugging compatible with the web.
10/04/2014 CS  56693      V11 required changes.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncServerFunctions.i}
{fncClassFunctions.i}
{fncStatusTypeFunctions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}

/* Shared Variables */
DEFINE NEW GLOBAL SHARED VARIABLE intGblFileMasterID    AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE intGblFileTypeID      AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE chrGblFilePath        AS CHARACTER NO-UNDO.

/* Create a Temp Table for FileUploadErrors */
DEFINE NEW GLOBAL SHARED TEMP-TABLE ttFileExport        NO-UNDO 
   LIKE File.

DEFINE NEW GLOBAL SHARED TEMP-TABLE ttFileExportError   NO-UNDO 
   LIKE FileUploadError.

/* Session Variables */
DEFINE VARIABLE chrSsnFileMasterName                    AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileExportError                      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrWriterProgram                        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrUnixCommand                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileMasterName                       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrTempFileName                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFinalFileName                        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFeedback                             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrReturnValue                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile                              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intVersion                              AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumSuccessful                        AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumFailures                          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID                     AS INTEGER      NO-UNDO.
DEFINE VARIABLE logEnableDebugging                      AS LOGICAL      NO-UNDO.

/* Streams */
DEFINE STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

/* This is name of FileMaster that we're wanting to run the import for - is set as the Session parameter by Cron Run that calls this prog */
ASSIGN chrSsnFileMasterName = fGetSessionValue("FileMasterName")
       chrFileMasterName    = chrSsnFileMasterName:chrValue NO-ERROR.

/* Verify a FileMasterName was provided */
IF chrFileMasterName = "" THEN 
DO:
   chrReturnValue = "Error Running: '" + chrFileMasterName + "' File Name is blank.".
   
   RETURN ERROR chrReturnValue.
END. /* IF chrFileMasterName = "" */

/* Set the log file destination directory  */
chrNewAgedDirectory = fGetAgedDirectory("../logs/", 2).
IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

chrLogFile = chrLogFileDirectory + chrFileMasterName + "_filExportFiles_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN 
DO:
   /* This is used to reset logGblDebugging before exiting because FileMaster.DebuggingOn can override CronEvent and enable debugging */
   logEnableDebugging = TRUE.
   /* Setup log files */
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
END. /* IF logGblDebugging */

FIND FIRST gate.GateUser NO-LOCK
   WHERE gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "CronErrors" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

ExportBlock:
DO ON STOP UNDO, RETURN ERROR:
   FIND FIRST FileMaster NO-LOCK
      WHERE FileMaster.MasterName = chrFileMasterName NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrFileExportError = "No FileMaster for '" + chrFileMasterName + "'.".
      LEAVE ExportBlock.
   END. /* IF NOT AVAILABLE FileMaster */
   
   /* IF debugging is disabled but FileMaster debugging is on allow debugging to write */
   IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE THEN 
   DO:
      logGblDebugging = TRUE.
      OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
      fLog("Debugging enabled by FileMaster.DebuggingOn").
   END. /* IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE */

   FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FileType THEN
   DO:
      chrFileExportError = "No FileType for FileMaster '" + chrFileMasterName + "'.".
      LEAVE ExportBlock.
   END. /* IF NOT AVAILABLE FileType */ 
   
   /* Set the Global Vars so we can retrieve them in the next Prog */
   ASSIGN intGblFileMasterID = FileMaster.FileMasterID
          intGblFileTypeID   = FileType.FileTypeID
          chrGblFilePath     = RIGHT-TRIM(FileMaster.FilePath, "/").
   
   EMPTY TEMP-TABLE ttFileExport.
   
   IF logGblDebugging THEN 
      fLog("Running:" + FileMaster.ProgramName).
   
   RUN VALUE(FileMaster.ProgramName) (OUTPUT chrFileExportError) NO-ERROR.
   
   IF logGblDebugging THEN 
      fLog("Back From " + FileMaster.ProgramName + ": " + chrFileExportError).
   
   IF chrFileExportError = "" AND ERROR-STATUS:ERROR THEN
      chrFileExportError = chrFileExportError + ERROR-STATUS:GET-MESSAGE(1).
   
   IF chrFileExportError <> "" THEN
   DO:
      IF logGblDebugging THEN
      DO:
          fLog(chrFileExportError).
          logGblDebugging = logEnableDebugging.
          OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */
       
      RETURN ERROR chrFileExportError.
   END. /* IF chrFileExportError <> "" */
   
   FileLoop:
   FOR EACH ttFileExport ON STOP UNDO, RETURN ERROR:
      
      /* Send internal Error e-mail, move to pnd folder and move on to the next File */
      IF CAN-FIND(FIRST ttFileExportError OF ttFileExport) THEN
      DO:
         /* Accumulate the ttFileExportErrors into one string for Emailing */
         chrFileExportError = "".
         FOR EACH ttFileExportError OF ttFileExport:
            
            chrFileExportError = chrFileExportError + ttFileExportError.ErrorString + CHR(13).

         END. /* FOR EACH ttFileExportError */
         
         IF logGblDebugging THEN
            fLog(chrFileExportError).
         
         RUN osSendMail.p (INPUT chrEmailAddress,                          /* Optional list of Users */
                           INPUT chrFileMasterName + " File Upload Error", /* Email Subject */
                           INPUT chrFileExportError + " from Cron.",       /* Plain text message Body */
                           INPUT "",                                       /* Html format message Body */
                           INPUT ttFileExport.FilePath,                    /* File path ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     FileMaster.InternalEmailGroupID
                                  ELSE 0),                                 /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                 /* File MasterID is it applies */
         
         chrFileExportError = "".
         
         /* Delete the temp file - no use to us now */
         fDeleteFile(ttFileExport.FilePath).
         
         intNumFailures = intNumFailures + 1.
         
         NEXT FileLoop.
      END. /* IF CAN-FIND(FIRST ttFileExportError OF ttFileExport) */
      
      /* If successfully completed then move from "tmp" folder back to outray for sending */
      chrReturnValue = fMoveFile(ttFileExport.FilePath, /* Source full file path */
                                 FileMaster.FilePath,   /* Target directory */
                                 ttFileExport.FileName, /* File Name without path */
                                 0) NO-ERROR.           /* Days to archive if applicable */
      
      IF logGblDebugging THEN
         fLog("Move from: " + ttFileExport.FilePath  + " to dir: " + FileMaster.FilePath + " FILE: " + ttFileExport.FileName).      
      IF chrReturnValue BEGINS "Error" THEN
      DO:
         RUN osSendMail.p (INPUT chrEmailAddress,                            /* Optional list of Users */
                           INPUT ttFileExport.FilePath + " File Move Error", /* Email Subject */
                           INPUT chrReturnValue + " Skipping file Export.",  /* Plain text message Body */
                           INPUT "",                                         /* Html format message Body */
                           INPUT ttFileExport.FilePath,                      /* File path ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     FileMaster.InternalEmailGroupID
                                  ELSE 0),                                   /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                   /* File MasterID is it applies */
         
         IF logGblDebugging THEN
            fLog(chrReturnValue).
         NEXT FileLoop.
      END. /* IF chrReturnValue BEGINS "Error" */
      
      /* If move was successful then this should find the file */
      IF SEARCH(FileMaster.FilePath + ttFileExport.FileName) <> ? THEN
      DO:
         FIND FIRST File EXCLUSIVE-LOCK 
            WHERE File.FileID = ttFileExport.FileID NO-WAIT NO-ERROR.
         IF AVAILABLE File THEN
            File.Completed = fTimestamp(NOW).
         
         RELEASE File NO-ERROR.
         intNumSuccessful = intNumSuccessful + 1.
      END. /* IF SEARCH(chrFinalFileName) <> ? */
      
   END. /* FOR EACH ttFileExport */
   
   chrFeedback = chrFeedback + STRING(intNumSuccessful) + " File" + (IF intNumSuccessful = 1 THEN " has" ELSE "s have") 
                    + " been Exported successfully." + CHR(13).
   IF intNumFailures > 0 THEN
      chrFeedback = chrFeedback + STRING(intNumFailures) + " File" + (IF intNumFailures = 1 THEN " has" ELSE "s have") 
                       + " FAILED and a MESSAGE sent TO the Internal Group." + CHR(13).
      
   IF logGblDebugging THEN
      fLog(chrFeedback).
   
   chrFileExportError = "".
   
END. /* ExportBlock */

/* If this is populated here then we've exited the block early so need to warn users */
IF chrFileExportError <> "" THEN
DO:
   IF logGblDebugging THEN
   DO:
      fLog(chrFileExportError).
      logGblDebugging = logEnableDebugging.
      OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logGblDebugging */
   RETURN ERROR chrFileExportError.
END. /* IF chrFileExportError */

IF logGblDebugging THEN
DO:
   fLog("Number of failures: " + STRING(intNumFailures)).
   logGblDebugging = logEnableDebugging.
   OUTPUT STREAM sToLogFile CLOSE.
END. /* IF logGblDebugging */

IF intNumFailures > 0 THEN
   RETURN "Number of failures: " + STRING(intNumFailures).

DELETE OBJECT chrSsnFileMasterName NO-ERROR.

RELEASE gate.GateUser              NO-ERROR.
RELEASE FileMaster                 NO-ERROR.
RELEASE FileType                   NO-ERROR.
RELEASE File                       NO-ERROR.
RELEASE EmailGroup                 NO-ERROR.
