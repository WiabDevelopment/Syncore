/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filImportFiles.p
Purpose : Finds a FileMaster record from the session variables. Then checks the path associated with the FileMaster and imports any Files 
          from that path that match the FileMaster mask & extension.
Author  : BG
Date    : 30th May 2012
---------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03/12/2013 CS  CR1021     Made compatible with the Cron Admin from the Web.  Added return error on stop condition to prevent infinite loops.
16/01/2014 CS  CR1021     Made debugging compatible with the web.  Added return-value to allow fatal error emails.
13/03/2014 CS  CR1021     Removed EmailGroup and return because it was not used by Cron.
10/04/2014 CS  56693      V11 required changes.
07/07/2014 CS             Added logic to update rejected files status, name, and path.
19/11/2014 MC  Nextel     After calling the reader program, blank the variable that reads the return value 
                          in case there is no return value  
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncClassFunctions.i}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}
{fncDateFunctions.i}
{fncGlobalFunctions.i}

/* Shared Variables */
DEFINE NEW GLOBAL SHARED VARIABLE intGblFileMasterID    AS INTEGER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE intGblFileTypeID      AS INTEGER     NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE chrGblFileName        AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE chrGblFilePath        AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE intGblNewFileID       AS INTEGER     NO-UNDO.

/* Create a Temp Table for FileUploadErrors */
DEFINE NEW GLOBAL SHARED TEMP-TABLE ttFileUploadError NO-UNDO
   LIKE FileUploadError.

/* Session Variables */
DEFINE VARIABLE chrSsnFileMasterName                    AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileUploadError                      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrReturnValue                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrReaderProgram                        AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrUnixCommand                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileMasterName                       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFeedback                             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrRejectFileName                       AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrTempFilePath                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile                              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intVersion                              AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumSuccessful                        AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumRejected                          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumPending                           AS INTEGER      NO-UNDO.
DEFINE VARIABLE logEnableDebugging                      AS LOGICAL      NO-UNDO.

/* Streams */
DEFINE STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

ASSIGN chrSsnFileMasterName = fGetSessionValue("FileMasterName")
       chrFileMasterName    = chrSsnFileMasterName:chrValue NO-ERROR.

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

chrLogFile = chrLogFileDirectory + chrFileMasterName + "_filImportFiles_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

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

ImportBlock:
DO ON STOP UNDO, RETURN ERROR:
   FIND FIRST FileMaster NO-LOCK 
      WHERE FileMaster.MasterName = chrFileMasterName NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrFileUploadError = "No FileMaster for '" + chrFileMasterName + "'.".
      
      IF logGblDebugging THEN
      DO:
         fLog(chrFileUploadError).
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */

      RETURN ERROR chrFileUploadError.
   END. /* IF NOT AVAILABLE FileMaster */
   
   /* Enable debugging if FileMaster debugging is enabled */
   IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE THEN 
   DO:
      OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
      logGblDebugging = FileMaster.DebuggingOn.
   END. /* IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE */

   FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FileType THEN
   DO:
      chrFileUploadError = "No FileType for FileMaster '" + chrFileMasterName + "'.".
      
      IF logGblDebugging THEN 
      DO:
         fLog(chrFileUploadError).
         logGblDebugging = logEnableDebugging.
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */
          
      RETURN ERROR chrFileUploadError.
   END. /* IF NOT AVAILABLE FileType */ 

   /* If a File uploads but there is some data missing for it to upload correctly, it is set to "pnd" status and a FileUploadError */
   /* record is created for the data gap. This procedure checks whether that data has been created since the File was forst run and if */
   /* so it moves the File back from the "pnd" folder into the upload directory to be reuploaded. */
   RUN pCheckForPendingFiles.
   
   /* This searches the directory of the FileMaster and creates ttSystemFile records for each File that matches the Mask we input */
   chrReturnValue = fGetFilesFromDir(FileMaster.FilePath,
                                     FileMaster.FileMask,
                                     FileType.Extension) NO-ERROR.

   IF chrReturnValue BEGINS "Error" THEN
   DO:
      IF logGblDebugging THEN 
      DO:
         fLog(chrReturnValue).
         logGblDebugging = logEnableDebugging.
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */

      RETURN ERROR chrReturnValue.
   END. /* IF chrReturnValue BEGINS "Error" */
   
   IF logGblDebugging AND NOT CAN-FIND(FIRST ttSystemFile) THEN
   DO:
      chrFileUploadError = "No files in Directory:'" + FileMaster.FilePath + "' for Upload".
      
      fLog(chrFileUploadError).
      logGblDebugging = logEnableDebugging.
      OUTPUT STREAM sToLogFile CLOSE.
      
      RETURN ERROR chrFileUploadError.
   END. /* IF logGblDebugging AND NOT CAN-FIND(FIRST ttSystemFile) */
      
   FileLoop:
   FOR EACH ttSystemFile ON STOP UNDO, RETURN ERROR:
      
      chrFileUploadError = "".
      EMPTY TEMP-TABLE ttFileUploadError NO-ERROR.
      
      chrReaderProgram = "fil" + fCapitalise(FileType.Extension) + "Reader.p". /* filXmlReader.p */
      
      /* Move the file to tmp directory so that no other file reader picks it up while we're in middle of processing it */
      chrTempFilePath = RIGHT-TRIM(FileMaster.FilePath,"/") + "/tmp/" + ttSystemFile.FileName.
      chrReturnValue = fMoveFile(ttSystemFile.FilePath, 
                                 RIGHT-TRIM(FileMaster.FilePath,"/") + "/tmp/", 
                                 ttSystemFile.FileName,
                                 FileMaster.DaysToArchive) NO-ERROR.
      
      IF logGblDebugging THEN
         fLog("Moving:" + ttSystemFile.FilePath + " to:"  + RIGHT-TRIM(FileMaster.FilePath,"/") + "/tmp/").
      
      IF chrReturnValue BEGINS "Error" THEN
      DO:
         RUN osSendMail.p (INPUT chrEmailAddress,                           /* Optional list of Users */
                           INPUT chrFileMasterName + " File Move Error",    /* Email Subject */
                           INPUT chrReturnValue + " Skipping file Upload.", /* Plain text message Body */
                           INPUT "",                                        /* Html format message Body */
                           INPUT ttSystemFile.FilePath,                     /* File path../idev/hyperthermbr/files/file OR ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     FileMaster.InternalEmailGroupID
                                  ELSE 0),                                  /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                  /* File MasterID is it applies */
         
        IF logGblDebugging THEN
           fLog(chrReturnValue).
            
         NEXT FileLoop.
      END. /* IF chrReturnValue BEGINS "Error" */
      
      /* Set the Global Vars so we can retrieve them in the next Prog */
      ASSIGN intGblFileMasterID    = FileMaster.FileMasterID
             intGblFileTypeID      = FileType.FileTypeID
             ttSystemFile.FilePath = chrTempFilePath
             chrGblFileName        = ttSystemFile.FileName
             chrGblFilePath        = ttSystemFile.FilePath.
       
      /* Reset this before running the file reader */
      ERROR-STATUS:ERROR = FALSE.

      RUN VALUE(chrReaderProgram) (INPUT FileMaster.ProgramName) NO-ERROR.
      
      /* Get and process Errors if any returned */
      chrFileUploadError = RETURN-VALUE.
      
      /* To avoid errors and confusions, if there is no return value, then blank the variable */
      IF chrFileUploadError = ? THEN chrFileUploadError = "".

      IF logGblDebugging THEN
         fLog("Raw Return Value from:" + chrReaderProgram + ". Err:" + chrFileUploadError) + " File Path: " + chrGblFilePath.

      IF chrFileUploadError = "" AND ERROR-STATUS:ERROR THEN
         chrFileUploadError = chrFileUploadError + ERROR-STATUS:GET-MESSAGE(1).
      
      IF logGblDebugging THEN
         fLog("After Progress Errs:" + chrFileUploadError).
      
      /* Send internal Error e-mail, move to pnd folder and move on to the next File */
      IF chrFileUploadError = "Pending" THEN
      DO:
         /* Accumulate the ttFileUploadErrors into one string for Emailing */
         chrFileUploadError = "".
         FOR EACH ttFileUploadError:
            
            chrFileUploadError = chrFileUploadError + ttFileUploadError.ErrorString + CHR(13).
         
         END. /* FOR Each ttFileUploadError */
         
         /* Could be at "Pending" without an error due to Locking etc - no need to send mail in such cases */
         IF chrFileUploadError <> "" THEN
         DO:
            RUN osSendMail.p (INPUT chrEmailAddress,                                        /* Optional list of Users */
                              INPUT chrFileMasterName + " File Upload Error",               /* Email Subject */
                              INPUT chrFileUploadError + CHR(13) + "Skipping file Upload.", /* Plain text message Body */
                              INPUT "",                                                     /* Html format message Body */
                              INPUT ttSystemFile.FilePath,                                  /* File path ../files/file */
                              INPUT (IF chrEmailAddress = "" THEN
                                        FileMaster.InternalEmailGroupID
                                     ELSE 0),                                               /* EmailGroupID that you want to send this to */
                              INPUT FileMaster.FileMasterID).                               /* File MasterID is it applies */
            
            IF logGblDebugging THEN
               fLog(chrFileUploadError).
         END. /* IF chrFileUploadError <> "" */
         
         chrReturnValue = fMoveFile(ttSystemFile.FilePath,                         /* Source File full path including FileName */
                                    RIGHT-TRIM(FileMaster.FilePath,"/") + "/pnd/", /* Target path exclusing FileName */
                                    ttSystemFile.FileName,                         /* FileName */
                                    0).                                            /* No of days to retain the backup file */
         
         IF chrReturnValue BEGINS "Error" THEN
         DO:
            RUN osSendMail.p (INPUT chrEmailAddress,                           /* Optional list of Users */
                              INPUT chrFileMasterName + " File Move Error",    /* Email Subject */
                              INPUT chrReturnValue + " Skipping file Upload.", /* Plain text message Body */
                              INPUT "",                                        /* Html format message Body */
                              INPUT ttSystemFile.FilePath,                     /* File path ../files/file */
                              INPUT (IF chrEmailAddress = "" THEN
                                        FileMaster.InternalEmailGroupID
                                     ELSE 0),                                  /* EmailGroupID that you want to send this to */
                              INPUT FileMaster.FileMasterID).                  /* File MasterID is it applies */
            
            IF logGblDebugging THEN
               fLog(chrFileUploadError).
         END. /* IF chrReturnValue BEGINS "Error" */
         
         FIND FIRST File EXCLUSIVE-LOCK
            WHERE File.FileID = intGblNewFileID NO-WAIT NO-ERROR.
         IF AVAILABLE File THEN
         DO:
            File.FilePath = RIGHT-TRIM(FileMaster.FilePath,"/") + "/pnd/" + ttSystemFile.FileName.
            RELEASE File NO-ERROR.
         END. /* IF AVAILABLE File */
         
         intNumPending = intNumPending + 1.
         
         NEXT FileLoop.
      END. /* IF chrFileUploadError = "Pending" */
      
      /* Send External Error e-mail, move to rej folder and move on to the next File */
      IF chrFileUploadError <> "" THEN
      DO:
         RUN osSendMail.p (INPUT chrEmailAddress,                                                  /* Optional list of Users */
                           INPUT chrFileMasterName + " File Upload Error",                         /* Email Subject */
                           INPUT chrFileUploadError + CHR(13) + CHR(13) + "Aborting file Upload.", /* Plain text message Body */
                           INPUT "",                                                               /* Html format message Body */
                           INPUT ttSystemFile.FilePath,                                            /* File path ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     FileMaster.ExternalEmailGroupID
                                  ELSE 0),                                                         /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                                         /* File MasterID is it applies */
         
         IF logGblDebugging THEN
            fLog(chrFileUploadError).
         
         chrRejectFileName = fNextFileVersion(ttSystemFile.FileName, 
                                              RIGHT-TRIM(FileMaster.FilePath,"/") + "/rej/",
                                              FileMaster.DaysToArchive).
        
         /* Update File status and path to Rejected to prevent reprocessing on datagap */
         FIND FIRST FILE EXCLUSIVE-LOCK 
            WHERE FILE.FileName = ttSystemFile.FileName NO-WAIT NO-ERROR.
         IF AVAILABLE FILE THEN
         DO:
            ASSIGN File.FileStatusID = fGetStatusID("File","Rejected")
                   File.FileName     = chrRejectFileName
                   File.FilePath     = RIGHT-TRIM(FileMaster.FilePath,"/") + "/rej/" + chrRejectFileName.
            RELEASE File NO-ERROR.
         END.   
         
         IF NOT chrRejectFileName BEGINS "Error" THEN
            chrReturnValue = fMoveFile(ttSystemFile.FilePath,                         /* Source File full path including FileName */
                                       RIGHT-TRIM(FileMaster.FilePath,"/") + "/rej/", /* Target path exclusing FileName */
                                       chrRejectFileName,                             /* FileName */
                                       0).                                            /* No of days to retain the backup file */
         
         intNumRejected = intNumRejected + 1.
         
         NEXT FileLoop.
      END. /* IF chrFileUploadError <> "" */
      
      IF FileMaster.DeleteWhenProcessed THEN
         chrReturnValue = fDeleteFile(ttSystemFile.FilePath).     /* File full path including FileName */
      ELSE
         chrReturnValue = fMoveFile(ttSystemFile.FilePath,                         /* Source File full path including FileName */
                                    RIGHT-TRIM(FileMaster.FilePath,"/") + "/bck/", /* Target path exclusing FileName */
                                    ttSystemFile.FileName,                         /* FileName */
                                    FileMaster.DaysToArchive).                     /* No of days to retain the backup file */
      
      IF chrReturnValue BEGINS "Error" THEN
      DO:
         RUN osSendMail.p (INPUT chrEmailAddress,                            /* Optional list of Users */
                           INPUT chrFileMasterName + " File Move Error",     /* Email Subject */
                           INPUT chrReturnValue + " File already Uploaded.", /* Plain text message Body */
                           INPUT "",                                         /* Html format message Body */
                           INPUT ttSystemFile.FilePath,                      /* File path ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     FileMaster.InternalEmailGroupID
                                  ELSE 0),                                   /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                   /* File MasterID is it applies */
         
         IF logGblDebugging THEN
           fLog(chrFileUploadError).
      END. /* IF chrReturnValue BEGINS "Error" */
      
      /* Try to work out what dir the File was moved to above */
      IF FileMaster.DaysToArchive > 0 THEN
      FileUpdateBlk:
      DO:
         chrNewAgedDirectory = fGetAgedDirectory(RIGHT-TRIM(FileMaster.FilePath,"/") + "/bck/", FileMaster.DaysToArchive).
         IF NOT chrNewAgedDirectory BEGINS "Error" THEN
            LEAVE FileUpdateBlk.
         
         FIND FIRST File EXCLUSIVE-LOCK 
            WHERE File.FileID = intGblNewFileID NO-WAIT NO-ERROR.
         IF AVAILABLE File THEN
         DO:
            File.FilePath = chrNewAgedDirectory + ttSystemFile.FileName.
            RELEASE File NO-ERROR.
         END. /* IF AVAILABLE File */
      END. /* IF FileMaster.DaysToArchive > 0 */
      
      intNumSuccessful = intNumSuccessful + 1.
      
   END. /* FOR EACH ttSystemFile: */
   
   IF logGblDebugging THEN
   DO:
      IF intNumSuccessful > 0 THEN
         chrFeedback = chrFeedback + STRING(intNumSuccessful) + " File" + (IF intNumSuccessful = 1 THEN " has" ELSE "s have") + " been"
                          + " Uploaded successfully.\n\n".
      IF intNumRejected > 0 THEN
         chrFeedback = chrFeedback + STRING(intNumRejected) + " File" + (IF intNumRejected = 1 THEN " has" ELSE "s have") + " been rejected"        
                          + " and a message sent to the External Group.\n\n".
      IF intNumPending > 0 THEN
         chrFeedback = chrFeedback + STRING(intNumPending) + " File" + (IF intNumPending = 1 THEN " is" ELSE "s are") + " Pending and a"
                          + " message sent to the Internal Group.\n\n".
      
      fLog(chrFeedback).
      logGblDebugging = logEnableDebugging.
      OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logGblDebugging */
   
END. /* ImportBlock */

DELETE OBJECT chrSsnFileMasterName NO-ERROR.

RELEASE gate.GateUser NO-ERROR.
RELEASE FileMaster    NO-ERROR.
RELEASE FileType      NO-ERROR.
RELEASE File          NO-ERROR.

PROCEDURE pCheckForPendingFiles:
   
   /* If a File uploads but there is some data missing for it to upload correctly, it is set to "pnd" status and a FileUploadError */
   /* record is created for the data gap. This procedure checks whether that data has been created since the File was forst run and if */
   /* so it moves the File back from the "pnd" folder into the upload directory to be reuploaded. */
   
   DEFINE VARIABLE intPendingStatusID  AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrTableName        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hdlRecordBuffer     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound      AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrFieldName        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrFieldValue       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrWhereClause      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrReturnValue      AS CHARACTER   NO-UNDO.
   
   DEFINE BUFFER updFileUploadError FOR FileUploadError.
   DEFINE BUFFER updFile            FOR File.
   
   intPendingStatusID = fGetStatusID("File","Pending").
   
   FOR EACH File OF FileMaster NO-LOCK
      WHERE File.FileStatusID = intPendingStatusID:
      
      FOR EACH FileUploadError OF File NO-LOCK
         WHERE FileUploadError.Resolved = "":
         
         CREATE BUFFER hdlRecordBuffer FOR TABLE FileUploadError.TableName.
         chrWhereClause = "WHERE " + FileUploadError.FieldName + " = '" + FileUploadError.FieldValue + "'".
         logRecordFound = hdlRecordBuffer:FIND-FIRST(chrWhereClause, NO-LOCK) NO-ERROR.
         
         IF logRecordFound THEN
         DO:
            FIND FIRST updFileUploadError EXCLUSIVE-LOCK
               WHERE ROWID(updFileUploadError) = ROWID(FileUploadError) NO-WAIT NO-ERROR.
            IF AVAILABLE updFileUploadError THEN
            DO:
               updFileUploadError.Resolved = fTimestamp(NOW).
               RELEASE updFileUploadError NO-ERROR.
            END. /* IF AVAILABLE updFileUploadError */
         END. /* IF logRecordFound */
         
         hdlRecordBuffer:BUFFER-RELEASE() NO-ERROR.
         DELETE OBJECT hdlRecordBuffer NO-ERROR.
         
      END. /* FOR EACH FileUploadError OF File NO-LOCK */
      
      IF NOT CAN-FIND(FIRST FileUploadError OF File WHERE 
                         FileUploadError.Resolved = "") THEN
      DO:
         FIND FIRST updFile EXCLUSIVE-LOCK 
            WHERE ROWID(updFile) = ROWID(File) NO-WAIT NO-ERROR.
         IF AVAILABLE updFile THEN
         DO:
            chrReturnValue = fMoveFile(RIGHT-TRIM(FileMaster.FilePath,"/") + "/pnd/" + File.FileName,
                                       FileMaster.FilePath,
                                       File.FileName,
                                       0).
            IF chrReturnValue BEGINS "Error" THEN
            DO:
               RUN osSendMail.p (INPUT chrEmailaddress,                                               /* Optional list of Users */
                                 INPUT chrFileMasterName + " File Move Error",                        /* Email Subject */
                                 INPUT chrReturnValue + " File should be in '/pnd' Directory.",       /* Plain text message Body */
                                 INPUT "",                                                            /* Html format message Body */
                                 INPUT RIGHT-TRIM(FileMaster.FilePath,"/") + "/pnd/" + File.FileName, /* File path ../files/file */
                                 INPUT (IF chrEmailAddress = "" THEN
                                           FileMaster.InternalEmailGroupID
                                        ELSE 0),                                                      /* EmailGroupID to send this to */
                                 INPUT FileMaster.FileMasterID).                                      /* File MasterID is it applies */
               
               IF logGblDebugging THEN
                  fLog(chrFileUploadError).
            END. /* IF chrReturnValue BEGINS "Error" */
            
            RELEASE updFile NO-ERROR.
            
         END. /* IF AVAILABLE updFile */
      END. /* IF NOT CAN-FIND(FIRST FileUploadError OF File WHERE FileUploadError.Resolved = "") */
      
   END. /* FOR EACH File OF FileMaster NO-LOCK */

   RELEASE updFileUploadError NO-ERROR.
   RELEASE updFile            NO-ERROR.
   
END PROCEDURE. /* pCheckForPendingFiles */
