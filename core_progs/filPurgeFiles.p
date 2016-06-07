/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filPurgeFiles.p
Purpose : This gets a system's root folder and then recursively runs through all of its sub folders looking for the keyword "days" in the 
          folder names. If it finds this keyword then it strips out the number of days preceding the word and then locates any file 
          within the subfolder that is older than that the number of days. This allows two modes: delete mode and logging mode.  Logging
          mode only provides a log file of the file names found past archive date in an email.  The delete mode is intended for purging
          the files found from system.
Author  : Bryan Gilsenan
Date    : 15th June 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
20/11/2013 CS  CR1021     Added ability to parse session variables.
27/03/2014 CS  56716      Added logging and delete modes.
09/04/2014 CS  56716      Added more extensions.
10/04/2014 CS  56716      V11 required changes.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}
{fncDateFunctions.i}
{fncClassFunctions.i}
{fncGlobalFunctions.i}

/* Session Variables */
DEFINE VARIABLE chrSsnLoggingMode           AS sessionValue NO-UNDO.
DEFINE VARIABLE chrSsnDeleteMode            AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrApplicationRootFolder    AS CHARACTER    NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE chrCurrentDirectory         AS CHARACTER    NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE chrServerHostName           AS CHARACTER    NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE chrServerRootFolder         AS CHARACTER    NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE chrReturnValue              AS CHARACTER    NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE chrReturnError              AS CHARACTER    NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE chrEmailAddress             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrUnixCommand              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile                  AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrEmailLogFile             AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intNumFilesDeleted          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intProcIteration            AS INTEGER      NO-UNDO.
DEFINE VARIABLE intFolderNumDays            AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumDaysOld               AS INTEGER      NO-UNDO.
DEFINE VARIABLE intFileType                 AS INTEGER      NO-UNDO.
DEFINE VARIABLE intEmailGroupID             AS INTEGER      NO-UNDO.
DEFINE VARIABLE logLoggingMode              AS LOGICAL      NO-UNDO.
DEFINE VARIABLE logDeleteMode               AS LOGICAL      NO-UNDO.

/* This is important - the file types that will be deleted */
DEFINE VARIABLE chrFileTypeList             AS CHARACTER    NO-UNDO INIT "log,txt,xml,csv,prn,ps,lg,xls,xlsx".

/* Streams */
DEFINE STREAM sToLogFile.
DEFINE STREAM sToEmailFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

ASSIGN chrSsnLoggingMode    = fGetSessionValue("LogMode")
       logLoggingMode       = LOGICAL(chrSsnLoggingMode:chrValue) 
       chrSsnDeleteMode     = fGetSessionValue("DeleteMode")
       logDeleteMode        = LOGICAL(chrSsnDeleteMode:chrValue)
       chrNewAgedDirectory  = fGetAgedDirectory("../logs/", 2) NO-ERROR. 

IF ERROR-STATUS:ERROR THEN
DO:
   /* Clean up streams and objects */
   RUN pCleanUp.
   RETURN ERROR "Could not setup variables " + RETURN-VALUE.
END.

IF logLoggingMode = ? THEN
   logLoggingMode = FALSE.

IF logDeleteMode = ? THEN
   logDeleteMode = FALSE.

IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

chrLogFile = chrLogFileDirectory + "filPurgeFiles_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".
chrEmailLogFile  = chrLogFileDirectory + "filPurgeFiles_email.log".

IF logGblDebugging = TRUE THEN 
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.

IF logLoggingMode = TRUE THEN
   OUTPUT STREAM sToEmailFile TO VALUE(chrEmailLogFile).

FIND FIRST gate.GateUser NO-LOCK
   WHERE gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

MainBlk:
DO:
   ASSIGN chrServerHostName   = fGetCurrentEnvironment()
          chrCurrentDirectory = fGetCurrentDirectory().
   
   /* Parse application file path */
   IF NUM-ENTRIES(chrCurrentDirectory,"/") > 2 THEN
      chrApplicationRootFolder = "/" + ENTRY(2,chrCurrentDirectory,"/") + "/" + ENTRY(3,chrCurrentDirectory,"/").

   chrReturnValue = fValidDirectory(chrApplicationRootFolder).

   IF chrReturnValue <> "OK" THEN
   DO:
      chrReturnError = "Invalid directory:'" + chrApplicationRootFolder + "'" + chrReturnValue.
      
      IF logGblDebugging = TRUE THEN
         fLog(chrReturnError).
      
      /* Clean up streams and objects */
      RUN pCleanUp.
      RETURN ERROR chrReturnError.
   END. /* IF chrReturnValue <> "OK" */

   IF logGblDebugging = TRUE THEN
      fLog("Application Root Folder: " + chrApplicationRootFolder + " Logging: " + STRING(logLoggingMode) + " Delete: " 
              + STRING(logDeleteMode)).
       
   /* This is the main proc which recursively runs all the others. */
   RUN pGetChildDirectories (INPUT chrApplicationRootFolder).
   
   IF chrReturnError <> "" THEN
   DO:
      IF logGblDebugging = TRUE THEN
         fLog(chrReturnError).
       
      /* Clean up streams and objects */
      RUN pCleanUp.
      RETURN ERROR chrReturnError.
   END. /* IF chrReturnError <> " */
   
   IF logLoggingMode = TRUE THEN
   DO:
      OUTPUT STREAM sToEmailFile CLOSE.
      
      FIND FIRST EmailGroup NO-LOCK
         WHERE EmailGroup.GroupCode = "FilePurge" NO-ERROR.
      IF AVAILABLE EmailGroup THEN
         intEmailGroupID = EmailGroup.EmailGroupID.
      ELSE
         intEmailGroupID = 1.

      RELEASE EmailGroup NO-ERROR.

      RUN osSendMail.p (INPUT chrEmailAddress,                                /* Optional list of Users */
                        INPUT "Cron File Purge",                              /* Email Subject */
                        INPUT "Files deleted: " + STRING(intNumFilesDeleted), /* Plain text message Body */
                        INPUT "",                                             /* Html format message Body */
                        INPUT chrEmailLogFile,                                /* File path ../files/file */
                        INPUT (IF chrEmailAddress = "" THEN
                                  intEmailGroupID
                               ELSE 0),                                       /* EmailGroupID */
                        INPUT "").                                            /* File MasterID */
   END. /* IF logLoggingMode = TRUE */

   IF logGblDebugging = TRUE THEN
      fLog("Files deleted: " + STRING(intNumFilesDeleted)).
END. /* MainBlk: */

/* Clean up streams and objects */
RUN pCleanUp.

PROCEDURE pCleanUp:
   IF LogGblDebugging = TRUE THEN
      OUTPUT STREAM sToLogFile CLOSE.

   IF logLoggingMode = TRUE THEN
   DO:
      OUTPUT STREAM sToEmailFile CLOSE.
      fDeleteFile(chrEmailLogFile) NO-ERROR.
   END. /* IF logLoggingMode = TRUE */

   DELETE OBJECT chrSsnLoggingMode NO-ERROR.
   DELETE OBJECT chrSsnDeleteMode  NO-ERROR.
END PROCEDURE.

PROCEDURE pGetChildDirectories:
   
   DEFINE INPUT PARAMETER chrParentDirectory AS CHARACTER   NO-UNDO FORMAT "X(20)".
   
   /* This searches the Parent directory and creates ttSystemFile records for each Child Directory */
   chrReturnValue = fGetDirectoryList(chrParentDirectory).
   IF chrReturnValue = "OK" THEN
   DO:
      /* Due to this being run recursively we need to keep track of the ones created in this Iteration */
      intProcIteration = intProcIteration + 1.
      FOR EACH ttSystemFile 
         WHERE ProcNo = 0:
         ttSystemFile.ProcNo = intProcIteration.
      
      END. /* FOR EACH ttSystemFile */
      
      FileLoop:
      FOR EACH ttSystemFile 
         WHERE ttSystemFile.ProcNo = intProcIteration:
         
         /* Skip these or it creates an infinte loop */
         IF ttSystemFile.FileName = "." OR ttSystemFile.FileName = ".." THEN
            NEXT FileLoop.
         
         /* If it has "days" string in the title then strip out the number before that */
         IF INDEX(ttSystemFile.FileName,"days") <> 0 THEN
         DO:
            intFolderNumDays = INTEGER(fTrim(ttSystemFile.FileName,"RIGHT",4)) NO-ERROR.
            /* If there isn't a valid integer then it won't come in here */
            IF NOT ERROR-STATUS:ERROR AND (intFolderNumDays > 0 AND intFolderNumDays < 365) THEN
            DO:
               /* Run the purger for all of the files in this folder that are older than the number of days in the folder name */
               RUN pPurgeFiles(INPUT ttSystemFile.FilePath,
                               INPUT intFolderNumDays).
            END. /*IF NOT ERROR-STATUS:ERROR THEN*/
         END. /*IF INDEX(ttSystemFile.FileName,"days") <> 0 THEN*/
         /* Once this directory is finished then run its children */
         RUN pGetChildDirectories (INPUT ttSystemFile.FilePath).

      END. /* FOR EACH ttSystemFile WHERE ttSystemFile.ProcNo = intProcIteration: */
   END. /* IF chrReturnValue = "OK" THEN */
END PROCEDURE. /* pGetChildDirectories */

PROCEDURE pPurgeFiles:
   
   DEFINE INPUT PARAMETER chrParentDirectory AS CHARACTER   NO-UNDO.
   DEFINE INPUT PARAMETER intDaysToKeepFiles AS INTEGER     NO-UNDO.
   
   /* Need this for recursivity */
   DEFINE BUFFER ttSystemFile FOR ttSystemFile.
   
   DO intFileType = 1 TO NUM-ENTRIES(chrFileTypeList):
      
      /* This searches the Parent directory and creates ttSystemFile records for each Child Directory */
      chrReturnValue = fGetFilesFromDir(chrParentDirectory,                  /* File Path */ 
                                        "",                                  /* File Mask */
                                        ENTRY(intFileType,chrFileTypeList)). /* File Extension */

      IF logGblDebugging = TRUE THEN 
         fLog("Directory: " + chrParentDirectory + " Extension: " + ENTRY(intFileType,chrFileTypeList)).
      
      IF chrReturnValue = "OK" THEN
      DO:
         intProcIteration = intProcIteration + 1.
         FOR EACH ttSystemFile 
            WHERE ProcNo = 0:
            ttSystemFile.ProcNo = intProcIteration.

         END. /* FOR EACH ttSystemFile */
         
         FileLoop:
         FOR EACH ttSystemFile 
            WHERE ttSystemFile.ProcNo = intProcIteration:
            
            intNumDaysOld = TODAY - ttSystemFile.DateModified.
            
            IF intNumDaysOld > intDaysToKeepFiles AND intNumDaysOld <> ? THEN
            DO:

               IF logGblDebugging = TRUE THEN
                  fLog(ttSystemFile.FilePath + " " + STRING(ttSystemFile.DateModified)).

               IF logLoggingMode = TRUE THEN
                  PUT STREAM sToEmailFile UNFORMATTED ttSystemFile.FilePath SKIP.
               
               IF logDeleteMode = TRUE THEN 
               DO:   
                  fDeleteFile(ttSystemFile.FilePath) NO-ERROR.
                  IF NOT ERROR-STATUS:ERROR THEN
                     intNumFilesDeleted = intNumFilesDeleted + 1.
               END. /* IF logDeleteMode = TRUE */
            END. /* IF (TODAY - ttSystemFile.DateModified) > intDaysToKeepFiles THEN */
         
         END. /* FOR EACH ttSystemFile */
      END. /* IF chrReturnValue = "OK" THEN */
   END. /* DO intFileType = 1 TO NUM-ENTRIES(chrFileTypeList): */
END PROCEDURE. /* pGetChildDirectories */
