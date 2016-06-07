/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filEmailFiles.p
Purpose : Finds a FileMaster record from the Cron web using session variables. Then checks the path associated with the FileMaster, moves 
          the file to the bck folder, and sends the file in an email to the user or to the email group.
Author  : Christopher Shelley
Date    : 28/10/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03/12/2013 CS  CR1021     Made compatible with the Cron Admin from the Web.  Added return error on stop condition to prevent infinite loops.
14/01/2014 CS  CR1021     Added debugging.
10/04/2014 CS  56693      V11 required changes.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncClassFunctions.i}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}
{fncDateFunctions.i}
{fncGlobalFunctions.i}

/* Session Variables */
DEFINE VARIABLE chrSsnFileMasterName                    AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrReturnValue                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileMasterName                       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileMask                             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrTempFilePath                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFeedback                             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile                              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intNumSuccessful                        AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumFailures                          AS INTEGER      NO-UNDO.

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

chrLogFile = chrLogFileDirectory + "fileEmailFiles_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN 
DO:
   /* Setup log files */
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
END. /* IF logGblDebugging */

ASSIGN chrSsnFileMasterName = fGetSessionValue("FileMasterName")
       chrFileMasterName    = chrSsnFileMasterName:chrValue NO-ERROR.
   
FIND FIRST gate.GateUser NO-LOCK
   WHERE   gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

IF chrGblEnvironment = "Live" THEN
DO:
   IF logGblDebugging THEN 
   DO:
      fLog("Running: " + chrFileMasterName + " Environment " + chrGblEnvironment).
      OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logGblDebugging */

   RETURN ERROR "Can not run in Live.  Environment: " + chrGblEnvironment.
END. /* IF LOOKUP(chrGblEnvironment,"DEV,QA,UAT") = 0 */

IF chrFileMasterName = "" THEN 
DO:
   chrReturnValue = "Error Running: " + chrFileMasterName + " File Name is blank.".
   
   IF logGblDebugging THEN
   DO:
       fLog(chrReturnValue).
       OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logGblDebugging */
    
   RETURN ERROR chrReturnValue.
END. /* IF chrFileMasterName = "" */

Read_Block:
DO ON STOP UNDO, RETURN ERROR:

   FIND FIRST FileMaster NO-LOCK
      WHERE   FileMaster.MasterName = chrFileMasterName NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrReturnValue = "Error no FileMaster for " + chrFileMasterName + ".".
      
      IF logGblDebugging THEN
      DO:
         fLog(chrReturnValue).
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */
      
      RETURN ERROR chrReturnValue.
   END. /* IF NOT AVAILABLE FileMaster */
   
   FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FileType THEN
   DO:
      chrReturnValue = "Error no FileType for FileMaster " + chrFileMasterName + ".".
      
      IF logGblDebugging THEN
      DO:
         fLog(chrReturnValue).
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */
          
      RETURN ERROR chrReturnValue.
   END. /* IF NOT AVAILABLE FileType  */ 

   chrFileMask = TRIM(FileMaster.FileMask).
   
   IF chrFileMask = "" THEN
   DO:
      chrFileMask = TRIM(FileMaster.FilePrefix) + "*." + TRIM(FileType.Extension).
      
      IF chrFileMask = "" THEN 
      DO:
         chrReturnValue = "Error no FileMask, FilePrefix, and Extension were found for FileMaster " + chrFileMasterName + ".".

         IF logGblDebugging THEN
         DO:   
            fLog(chrReturnValue).
            OUTPUT STREAM sToLogFile CLOSE.
         END. /* IF logGblDebugging */
         
         RETURN ERROR chrReturnValue.
      END. /* IF chrFileMask = "" */
   END. /* IF chrFileMask = "" */
   
   IF logGblDebugging THEN 
      fLog("Running: " + FileMaster.ProgramName).
   
   chrReturnValue = fGetFilesFromDir(FileMaster.FilePath,
                                     chrFileMask,
                                     FileType.Extension).

   IF chrReturnValue BEGINS "Error" THEN 
   DO:
      IF logGblDebugging THEN
      DO:   
         fLog(chrReturnValue).
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */
      
      RETURN ERROR chrReturnValue.
   END. /* IF chrReturnValue BEGINS "Error" */

END. /* Read_Block */

IF chrReturnValue BEGINS "Error" THEN
DO:
   IF logGblDebugging THEN 
   DO:
      fLog("Error reading files: " + chrReturnValue).   
      OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logDblDebugging */
   
   RUN osSendMail.p (INPUT "",                                       /* Optional list of Users */
                     INPUT "Error reading Files: " + chrReturnValue, /* Email Subject */
                     INPUT "Error reading Files: " + chrReturnValue, /* Plain text message Body */
                     INPUT "",                                       /* Html format message Body */
                     INPUT "",                                       /* File path../idev/hyperthermbr/files/file OR ../files/file */
                     INPUT FileMaster.InternalEmailGroupID,          /* EmailGroupID that you want to send this to */
                     INPUT FileMaster.FileMasterID).                 /* File MasterID is it applies */
   

   RETURN ERROR chrReturnValue.
END. /* IF chrReturnValue BEGINS "Error" */

IF logGblDebugging THEN
   fLog("Files have been read.").

File_Loop:
FOR EACH ttSystemFile ON STOP UNDO, RETURN:
   
   chrTempFilePath = FileMaster.FilePath + "bck/" + ttSystemFile.FileName.
   
   chrReturnValue = fMoveFile(ttSystemFile.FilePath, 
                              FileMaster.FilePath + "bck/", 
                              ttSystemFile.FileName,
                              FileMaster.DaysToArchive).
   
   IF logGblDebugging THEN
      fLog("Back From Moving File For " + FileMaster.ProgramName + ":" + chrReturnValue).
   
   IF chrReturnValue BEGINS "Error" THEN
   DO:
      RUN osSendMail.p (INPUT chrEmailAddress,                              /* Optional list of Users */
                        INPUT chrFileMasterName + " File Email Move Error", /* Email Subject */
                        INPUT chrReturnValue + " from Cron.",               /* Plain text message Body */
                        INPUT "",                                           /* Html format message Body */
                        INPUT ttSystemFile.FilePath,                        /* File path ../files/file */
                        INPUT (IF chrEmailAddress = "" THEN 
                                  FileMaster.InternalEmailGroupID 
                               ELSE 0),                                     /* EmailGroupID that you want to send this to */
                        INPUT FileMaster.FileMasterID).                     /* File MasterID is it applies */
      
      intNumFailures = intNumFailures + 1.
      NEXT File_Loop.
   END. /* IF chrReturnValue BEGINS "Error" */
   
   ASSIGN ttSystemFile.FilePath = chrTempFilePath
          chrFileMasterName     = ttSystemFile.FileName.
   
   IF logGblDebugging THEN
      fLog("Emailing Files " + FileMaster.ProgramName + ":" + chrReturnValue).
   
   RUN osSendMail.p (INPUT chrEmailAddress,                    /* Optional list of Users */
                     INPUT chrFileMasterName + " File Email",  /* Email Subject */
                     INPUT "The file was attached from Cron.", /* Plain text message Body */
                     INPUT "",                                 /* Html format message Body */
                     INPUT ttSystemFile.FilePath,              /* File path ../files/file */
                     INPUT (IF chrEmailAddress = "" THEN 
                               FileMaster.InternalEmailGroupID 
                            ELSE 0),                           /* EmailGroupID that you want to send this to */
                     INPUT FileMaster.FileMasterID).           /* File MasterID is it applies */
   
   intNumSuccessful = intNumSuccessful + 1.
  
END. /* FOR EACH ttSystemFile */

IF logGblDebugging THEN
DO:
   IF intNumSuccessful > 0 THEN
      chrFeedback = chrFeedback + STRING(intNumSuccessful) + " File" + (IF intNumSuccessful = 1 THEN " has" ELSE "s have") + " been Emailed"
                       + " successfully." + CHR(13).

   IF intNumFailures > 0 THEN
      chrFeedback = chrFeedback + STRING(intNumFailures) + " File" + (IF intNumFailures = 1 THEN " has" ELSE "s have") + " generated an"
                       + " error email." + CHR(13).

   IF chrFeedBack > "" THEN
      fLog(chrFeedback).
   OUTPUT STREAM sToLogFile CLOSE.
END. /* IF logGblDebugging */

DELETE OBJECT chrSsnFileMasterName NO-ERROR.

RELEASE gate.GateUser              NO-ERROR.
RELEASE FileMaster                 NO-ERROR.
RELEASE FileType                   NO-ERROR.
