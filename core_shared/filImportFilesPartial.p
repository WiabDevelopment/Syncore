/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filImportFilesPartial.p
Purpose : Finds a FileMaster record from the session variable. Then checks the path associated with the FileMaster and imports any Files 
          from that path that match the FileMaster mask & extension.
Author  : BG
Date    : 30th May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
30/01/2014 CS  CR1043     Allow partial file imports.
05/02/2014 CS  CR1043     Added logic for prevents and better email errors.
10/02/2014 CS  CR1043     Added limit for email body and allow pending files to be reprocessed without all datagaps resolved.
11/02/2014 CS  CR1043     Added text to display after limit for email and logic to prevent reprocess if datagaps still exist for the order.
12/03/2014 CS  CR1021     Made compatiable with Cron.
10/04/2014 CS  56693      V11 required changes.
06/03/2015 CS  Canon      Moved FileMaster case logic to a customer include file to allow for use of different FileMasters.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncClassFunctions.i}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}
{fncDateFunctions.i}
{fncGlobalFunctions.i}

/* These vars will hold the File information through the program chain */
DEFINE NEW GLOBAL SHARED VARIABLE intGblFileMasterID    AS INTEGER      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE intGblFileTypeID      AS INTEGER      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE chrGblFileName        AS CHARACTER    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE chrGblFilePath        AS CHARACTER    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE intGblNewFileID       AS INTEGER      NO-UNDO.

/* Create a Temp Table for FileImportErrors */
DEFINE NEW GLOBAL SHARED TEMP-TABLE ttFileImportError NO-UNDO
   LIKE FileImportError.

/* Create a Temp Table for ShipOrderImports */
DEFINE NEW GLOBAL SHARED TEMP-TABLE ttShipOrderImport NO-UNDO
   LIKE ShipOrderImport.

/* Session Variables */
DEFINE VARIABLE chrSsnFileMasterName                    AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileImportError                      AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrFileImportStatus                     AS CHARACTER    NO-UNDO.
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
DEFINE VARIABLE intShipOrderUploadStatus                AS INTEGER      NO-UNDO.
DEFINE VARIABLE intEmailGroupID                         AS INTEGER      NO-UNDO.
DEFINE VARIABLE logEnableDebugging                      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE logErrorLimit                           AS LOGICAL      NO-UNDO.

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

chrLogFile = chrLogFileDirectory + chrFileMasterName + "_filImportFilesPartial_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN 
DO:
   /* This is used to reset logGblDebugging before exiting because FileMaster.DebuggingOn can override CronEvent and enable debugging */
   logEnableDebugging = TRUE.
   /* Setup log files */
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
END. /* IF logGblDebugging */

FIND FIRST gate.GateUser NO-LOCK /* idx=GateUserID */
   WHERE gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

ImportBlock:
DO ON STOP UNDO, RETURN ERROR:
   FIND FIRST FileMaster NO-LOCK /* idx=MasterName */
      WHERE FileMaster.MasterName = chrFileMasterName NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrFileImportError = "No FileMaster for: '" + chrFileMasterName + "'.".
      
      IF logGblDebugging THEN
      DO:
         fLog(chrFileImportError).
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */

      RETURN ERROR chrFileImportError.
   END. /* IF NOT AVAILABLE FileMaster */
   
   /* Enable debugging if FileMaster debugging is enabled */
   IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE THEN 
   DO:
      OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
      logGblDebugging = FileMaster.DebuggingOn.
   END. /* IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE */

   FIND FIRST FileType OF FileMaster NO-LOCK /* idx=FileTypeID */ NO-ERROR.
   IF NOT AVAILABLE FileType THEN
   DO:
      chrFileImportError = "No FileType for FileMaster '" + chrFileMasterName + "'.".
	  
	  IF logGblDebugging THEN 
      DO:
         fLog(chrFileImportError).
         logGblDebugging = logEnableDebugging.
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */
          
      RETURN ERROR chrFileImportError.
   END. /* IF NOT AVAILABLE FileType */
   
   /* If a File uploads but there is some data missing for it to upload correctly, it is set to "pnd" status and a FileImportError */
   /* record is created for the data gap. This procedure checks whether that data has been created since the File was first run and if */
   /* so it moves the File back from the "pnd" folder into the upload directory to be reuploaded.                                  */
   RUN pCheckForPendingFiles.
   
   /* This searches the directory of the FileMaster and creates ttSystemFile records for each File that matches the Mask we input */
   chrReturnValue = fGetFilesFromDir(FileMaster.FilePath,
                                     FileMaster.FileMask,
                                     FileType.Extension) NO-ERROR.
   
   IF chrReturnValue BEGINS "Error" THEN
   DO:
      chrFileImportError = "Error reading Directory: '" + FileMaster.FilePath + "' Error: " + chrReturnValue + ".".

      IF logGblDebugging THEN 
      DO:
         fLog(chrFileImportError).
         logGblDebugging = logEnableDebugging.
         OUTPUT STREAM sToLogFile CLOSE.
      END. /* IF logGblDebugging */
      
      RETURN ERROR chrFileImportError.
   END. /* IF chrReturnValue BEGINS "Error" */
   
   IF logGblDebugging AND NOT CAN-FIND(FIRST ttSystemFile) THEN
   DO:
      chrFileImportError = "No files in Directory: '" + FileMaster.FilePath + "' for Upload".
      
      fLog(chrFileImportError).
      logGblDebugging = logEnableDebugging.
      OUTPUT STREAM sToLogFile CLOSE.
      
      RETURN ERROR chrFileImportError.
   END. /* IF logGblDebugging AND NOT CAN-FIND(FIRST ttSystemFile) */
   
   FileLoop:
   FOR EACH ttSystemFile ON STOP UNDO, RETURN ERROR:
      
      ASSIGN chrFileImportError = ""
             logErrorLimit      = FALSE.
      EMPTY TEMP-TABLE ttFileImportError NO-ERROR.
      EMPTY TEMP-TABLE ttShipOrderImport NO-ERROR.
      
      chrReaderProgram = "fil" + fCapitalise(FileType.Extension) + "Reader.p". /* filXmlReader.p, filTxtReader.p */
      
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
      
      RUN VALUE(chrReaderProgram) (INPUT FileMaster.ProgramName) NO-ERROR. /* "xmlAsnUpload.p" */
      
      /* Get and process Errors if any returned */
      chrFileImportError = RETURN-VALUE.

      IF logGblDebugging THEN
         fLog("RAW Return Value from: " + chrReaderProgram + ". Err: " + chrFileImportError).
      
      IF chrFileImportError = "" AND ERROR-STATUS:ERROR THEN
         chrFileImportError = chrFileImportError + ERROR-STATUS:GET-MESSAGE(1).
      
      IF logGblDebugging THEN
         fLog("After Progress Errs: " + chrFileImportError).
      
      chrFileImportStatus = "".
      
      CASE chrFileMasterName:
         /* Customer Specific Case Statements */
         {filImportFilesPartial.i}
         
         OTHERWISE
         DO:
            /* Check if there is an error */
            FOR EACH ttFileImportError:
                       
               IF (LENGTH(chrFileImportStatus) + LENGTH(ttFileImportError.ErrorString)) < 900 THEN
                  chrFileImportStatus = chrFileImportStatus + "Error: " + ttFileImportError.ErrorString + CHR(13).
               ELSE
                  logErrorLimit = TRUE.
             
            END. /* FOR EACH ttFileImportError */
         END. /*OTHERWISE DO*/     
      END. /* CASE chrFileMasterName */
      
      IF logErrorLimit THEN
         chrFileImportStatus = chrFileImportStatus + "There are too many errors to be displayed.".

      FIND FIRST EmailGroup NO-LOCK /* idx=GroupCode */
         WHERE EmailGroup.GroupCode = chrFileMasterName NO-ERROR.
      IF AVAILABLE EmailGroup THEN
         intEmailGroupID = EmailGroup.EmailGroupID.
      ELSE
         intEmailGroupID = FileMaster.ExternalEmailGroupID.
      
      /* If errors were found send an email */
      IF chrFileImportStatus <> "" THEN
      DO:
         RUN osSendMail.p (INPUT chrEmailAddress,                                        /* Optional list of Users */
                           INPUT chrFileMasterName + " File Error",                      /* Email Subject */
                           INPUT STRING(chrFileImportStatus, "X(1000)"),                 /* Plain text message Body */
                           INPUT "",                                                     /* Html format message Body */
                           INPUT ttSystemFile.FilePath,                                  /* File path ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     intEmailGroupID
                                  ELSE 0),                                               /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                               /* File MasterID is it applies */
         IF logGblDebugging THEN
            fLog(chrFileImportStatus).			   
      END. /* IF chrFileImportStatus <> "" */

      /* Move to pnd folder and move on to the next File */
      IF chrFileImportError = "Pending" THEN
      DO:
         chrFileImportError = "".
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
               fLog(chrFileImportError + " " + chrReturnValue + " Skipping file Upload.").
         END. /* IF chrReturnValue BEGINS "Error" */
         
         FIND FIRST File EXCLUSIVE-LOCK /* idx=FileID */
            WHERE File.FileID = intGblNewFileID NO-WAIT NO-ERROR.
         IF AVAILABLE File THEN
         DO:
            File.FilePath = RIGHT-TRIM(FileMaster.FilePath,"/") + "/pnd/" + ttSystemFile.FileName.
            RELEASE File NO-ERROR.
         END. /* IF AVAILABLE File */
         
         intNumPending = intNumPending + 1.
         
         NEXT FileLoop.
      END. /* IF chrFileImportError = "Pending" */
      
      /* Send External Error e-mail, move to rej folder if this was an error and not PreventRejected and move on to the next File */
      IF chrFileImportError <> "" AND chrFileImportError <> "PreventRejected" THEN
      DO:
         IF logGblDebugging THEN
            fLog(chrFileImportError).
         
         /* Email the fatal error */
         IF chrFileImportStatus = "" THEN
            RUN osSendMail.p (INPUT chrEmailAddress,                      /* Optional list of Users */
                              INPUT chrFileMasterName + " File Error",    /* Email Subject */
                              INPUT chrFileImportError,                   /* Plain text message Body */
                              INPUT "",                                   /* Html format message Body */
                              INPUT ttSystemFile.FilePath,                /* File path ../files/file */
                              INPUT (IF chrEmailAddress = "" THEN
                                        intEmailGroupID
                                     ELSE 0),                             /* EmailGroupID that you want to send this to */
                              INPUT FileMaster.FileMasterID).             /* File MasterID is it applies */

         chrRejectFileName = fNextFileVersion(ttSystemFile.FileName, 
                                              RIGHT-TRIM(FileMaster.FilePath,"/") + "/rej/",
                                              FileMaster.DaysToArchive).
         
         IF NOT chrRejectFileName BEGINS "Error" THEN
            chrReturnValue = fMoveFile(ttSystemFile.FilePath,                         /* Source File full path including FileName */
                                       RIGHT-TRIM(FileMaster.FilePath,"/") + "/rej/", /* Target path exclusing FileName */
                                       chrRejectFileName,                             /* FileName */
                                       0).                                            /* No of days to retain the backup file */
         
         intNumRejected = intNumRejected + 1.
         
         NEXT FileLoop.
      END. /*IF chrFileImportError <> "" THEN*/
      
      IF FileMaster.DeleteWhenProcessed THEN
         chrReturnValue = fDeleteFile(ttSystemFile.FilePath).     /* File full path including FileName */
      ELSE
         chrReturnValue = fMoveFile(ttSystemFile.FilePath,                         /* Source File full path including FileName k*/
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
            fLog(chrFileImportError + " " + chrReturnValue + " File already Uploaded.").
      END. /* IF chrReturnValue BEGINS "Error" */
      
      /* Try to work out what dir the File was moved to above */
      IF FileMaster.DaysToArchive > 0 THEN
      FileUpdateBlk:
      DO:
         chrNewAgedDirectory = fGetAgedDirectory(RIGHT-TRIM(FileMaster.FilePath,"/") + "/bck/", FileMaster.DaysToArchive).
         IF NOT chrNewAgedDirectory BEGINS "Error" THEN
            LEAVE FileUpdateBlk.
         
         FIND FIRST File EXCLUSIVE-LOCK /* idx=FileID */
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
         chrFeedback = chrFeedback + STRING(intNumSuccessful) + " File" + (IF intNumSuccessful = 1 THEN " has" ELSE "s have") 
                                + " been Uploaded successfully.\n\n".
      IF intNumRejected > 0 THEN
         chrFeedback = chrFeedback + STRING(intNumRejected) + " File" + (IF intNumRejected = 1 THEN " has" ELSE "s have") 
                                + " been Rejected and a message sent to the External Group.\n\n".
      IF intNumPending > 0 THEN
         chrFeedback = chrFeedback + STRING(intNumPending) + " File" + (IF intNumPending = 1 THEN " is" ELSE "s are") 
                                + " Pending and a message sent to the Internal Group.\n\n".
      
      fLog(chrFeedback).
      logGblDebugging = logEnableDebugging.
      OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logGblDebugging */
   
END. /*ImportBlock*/

DELETE OBJECT chrSsnFileMasterName NO-ERROR.

RELEASE gate.GateUser NO-ERROR.
RELEASE FileMaster    NO-ERROR.
RELEASE FileType      NO-ERROR.
RELEASE File          NO-ERROR.
RELEASE EmailGroup    NO-ERROR.

PROCEDURE pCheckForPendingFiles:
   
   /* If a File uploads but there is some data missing for it to upload correctly, it is set to "pnd" status and a FileImportError */
   /* record is created for the data gap. This procedure checks whether that data has been created since the File was first run and if */
   /* so it moves the File back from the "pnd" folder into the upload directory to be reuploaded.                                  */
   
   DEFINE VARIABLE intPendingStatusID  AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrTableName        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE hdlRecordBuffer     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound      AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrFieldName        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrFieldValue       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrWhereClause      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrReturnValue      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE logRecordResolved   AS LOGICAL     NO-UNDO.
   
   DEFINE BUFFER updFileImportError  FOR FileImportError.
   DEFINE BUFFER readFileImportError FOR FileImportError.
   DEFINE BUFFER updFile             FOR File.
   
   intPendingStatusID = fGetStatusID("File","Pending").
   
   IF logGblDebugging THEN
      fLog("Checking for Pending Files in '/pnd' Directory. Pending Status: " + STRING(intPendingStatusID) + " FileMasterID: " + STRING(FileMaster.FileMasterID)).
   
   FOR EACH File OF FileMaster NO-LOCK /* idx=FileMasterIDFileStatusID */
      WHERE File.FileStatusID = intPendingStatusID:
      
      IF logGblDebugging THEN
         fLog("Checking File: " + File.FileName).
      logRecordResolved = FALSE.
      FOR EACH FileImportError OF File NO-LOCK /* idx=FileID */
         WHERE FileImportError.Resolved  = ""
         AND   FileImportError.TableName > "":
         
         CREATE BUFFER hdlRecordBuffer FOR TABLE FileImportError.TableName.
         chrWhereClause = "WHERE " + FileImportError.FieldName + " = '" + FileImportError.FieldValue + "'".
         logRecordFound = hdlRecordBuffer:FIND-FIRST(chrWhereClause, NO-LOCK) NO-ERROR.
         
         IF logGblDebugging THEN
            fLog(chrWhereClause + " result: " + STRING(logRecordFound)).
         
         IF logRecordFound THEN
         DO:
            FIND FIRST updFileImportError EXCLUSIVE-LOCK /* idx=RecID */
               WHERE ROWID(updFileImportError) = ROWID(FileImportError) NO-WAIT NO-ERROR.
            IF AVAILABLE updFileImportError THEN
            DO:
               updFileImportError.Resolved = fTimestamp(NOW).
               RELEASE updFileImportError NO-ERROR.

               /* If all errors for this RefID have had a resolution then set the file to be reprocess */
               IF NOT CAN-FIND(FIRST readFileImportError 
                                  WHERE readFileImportError.FileID                   = File.FileID
                                  AND   readFileImportError.RefID                    = FileImportError.RefID
                                  AND   readFileImportError.Resolved                 = ""
                                  AND   readFileImportError.TableName                > "") THEN
                  logRecordResolved = TRUE.
            END. /* IF AVAILABLE updFileImportError */
         END. /* IF logRecordFound */
         
         hdlRecordBuffer:BUFFER-RELEASE() NO-ERROR.
         DELETE OBJECT hdlRecordBuffer NO-ERROR.
         
      END. /* FOR EACH FileImportError OF File NO-LOCK */
      
      /* If datagap was resolved try to process the file again */
      IF logRecordResolved = TRUE THEN
      DO:
         FIND FIRST updFile EXCLUSIVE-LOCK /* idx=FileID */
            WHERE ROWID(updFile) = ROWID(File) NO-WAIT NO-ERROR.
         IF AVAILABLE updFile THEN
         DO:
            chrReturnValue = fMoveFile(RIGHT-TRIM(FileMaster.FilePath,"/") + "/pnd/" + File.FileName,
                                       FileMaster.FilePath,
                                       File.FileName,
                                       0).
            IF chrReturnValue BEGINS "Error" THEN
            DO:
               RUN osSendMail.p (INPUT chrEmailAddress,                                               /* Optional list of Users */
                                 INPUT chrFileMasterName + " File Move Error",                        /* Email Subject */
                                 INPUT chrReturnValue + " File should be in '/pnd' Directory.",       /* Plain text message Body */
                                 INPUT "",                                                            /* Html format message Body */
                                 INPUT RIGHT-TRIM(FileMaster.FilePath,"/") + "/pnd/" + File.FileName, /* File path ../files/file */
                                 INPUT (IF chrEmailAddress = "" THEN
                                           FileMaster.InternalEmailGroupID
                                        ELSE 0),                                                      /* EmailGroupID to send this to */
                                 INPUT FileMaster.FileMasterID).                                      /* File MasterID is it applies */
               
               IF logGblDebugging THEN
                  fLog(chrFileImportError + " " + chrReturnValue + " File should be in '/pnd' Directory.").
            END. /* IF chrReturnValue BEGINS "Error" */
            
            RELEASE updFile NO-ERROR.
         END. /* IF AVAILABLE updFile */
      END. /* IF logRecordResolved = TRUE */
      
   END. /* FOR EACH File OF FileMaster NO-LOCK */

   RELEASE updFileImportError NO-ERROR.
   RELEASE updFile            NO-ERROR.

END PROCEDURE. /* pCheckForPendingFiles */
