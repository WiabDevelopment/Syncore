/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncServerFunctions.i
Purpose : Server & file functions globally used
Author  : BG
Date    : 17th May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
17/12/2013 CS  CR1021     Added fGetErrorMessageText.
20/12/2013 BG  WiaB       Added fGetCurrentEnvironmentID to return integer ID instead of character.
18/02/2014 CS  CR1021     Updated fValidPID to also check for _progres.
10/02/2014 AB  WiaB       Added fGetCurrentPackage and fGetCurrentApplicationID functions.
03/11/2014 CS  WiaB       Added fGetWebBrokerStatus, fGetWebBrokerAvailableAgents, and fControlWebBroker Functions.
02/01/2015 CS  WiaB       Added -f parameter to fDeleteFile rm command.
05/01/2015 CS  WiaB       Added output redirect to fDeletefile rm command.
10/06/2015 ND  GoPro      Added in the fGrepFileMasterString() function.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Used by the fGetFilesFromDir Function below - must be defined outside of the function to be in scope for the calling program */
DEFINE STREAM sReadFiles.
DEFINE STREAM sReadSubDirs.

/* Used by the fGetFilesFromDir Function below - must be defined outside of the function to be in scope for the calling program */
DEFINE TEMP-TABLE ttSystemFile NO-UNDO
    FIELD FileNo               AS INTEGER
    FIELD ProcNo               AS INTEGER
    FIELD FileName             AS CHARACTER
    FIELD FileType             AS CHARACTER
    FIELD FilePath             AS CHARACTER
    FIELD FileSize             AS INTEGER
    FIELD FileExtension        AS CHARACTER
    FIELD DateCreated          AS DATE
    FIELD DateModified         AS DATE.

DEFINE TEMP-TABLE ttSubDir     NO-UNDO
    FIELD NumDaysToArchive     AS INTEGER
    FIELD FileName             AS CHARACTER
    FIELD FileType             AS CHARACTER
    FIELD FilePath             AS CHARACTER
    FIELD FileSize             AS INTEGER
    FIELD FileExtension        AS CHARACTER
    FIELD DateCreated          AS DATE
    FIELD DateModified         AS DATE
    INDEX idxNumDaysToArchive NumDaysToArchive DESC.


FUNCTION fFileExtension RETURNS CHARACTER (INPUT chrFileName AS CHARACTER):
   
   IF NUM-ENTRIES(chrFileName,".") = 1 THEN
      RETURN "".
   
   RETURN ENTRY(NUM-ENTRIES(chrFileName,"."),chrFileName,".").
END.


FUNCTION fFileName RETURNS CHARACTER (INPUT chrFileName AS CHARACTER):
   
   RETURN ENTRY(NUM-ENTRIES(chrFileName,"/"),chrFileName,"/").
END.


FUNCTION fValidDirectory RETURNS CHARACTER (INPUT chrDirectoryPath AS CHARACTER): 
   
   /* Validate that the directory path exists */
   FILE-INFO:FILE-NAME = chrDirectoryPath.
   IF (FILE-INFO:FILE-TYPE = ?) THEN
   DO:
      RETURN "Directory '" + chrDirectoryPath + "' does NOT Exist.".
   END.
   
   /* Now check that its a Directory - If this = L then it is a symbolic link in Unix, decided to exclude these for now */
   IF SUBSTRING(FILE-INFO:FILE-TYPE,1,1) <> "D" AND SUBSTRING(FILE-INFO:FILE-TYPE,1,1) <> "L" THEN
   DO:
      RETURN "'" + chrDirectoryPath + "' is a FILE NOT a Directory.".
   END.
   
   RETURN "Ok".
   
END FUNCTION. /* fValidDirectory */


/* Check for valid directory and that we have write access */
FUNCTION fValidWriteDirectory RETURNS CHARACTER (INPUT chrDirectoryPath AS CHARACTER): 
   
   IF fValidDirectory(chrDirectoryPath) <> "Ok" THEN
      RETURN fValidDirectory(chrDirectoryPath).
   
   /* Validate that the log file dir exists*/
   FILE-INFO:FILE-NAME = chrDirectoryPath.
   
   /*Now check that its a Directory and that we have write permissions to it */
   IF INDEX(FILE-INFO:FILE-TYPE,"W") = 0 THEN
   DO:
      RETURN "Cannot write to Directory '" + chrDirectoryPath + "'.".
   END.
   
   RETURN "Ok".
   
END FUNCTION. /* fValidDirectory */


/* Check if PID is running using the "ps" Unix command.  Will only work on Unix. */
FUNCTION fValidPID RETURNS LOGICAL (INPUT intUnixPID AS INTEGER): 
   
   DEFINE VARIABLE chrUnixCommand AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrUnixResult  AS CHARACTER NO-UNDO.
   
   IF intUnixPID = 0 THEN
      RETURN FALSE.
   
   /* Look for the Unix PID, get the last line printed, and capture last column */
   chrUnixCommand = "ps -p "  + STRING(intUnixPID) + " | tail -1 | tr -s ' ' | cut -d ' ' -f5".
   
   INPUT THROUGH VALUE(chrUnixCommand).
      IMPORT chrUnixResult NO-ERROR.
   INPUT CLOSE. 
   
   /* Check if this is progress process */
   IF chrUnixResult = "_progres" THEN
      RETURN TRUE.
   ELSE
      RETURN FALSE.
   
END FUNCTION. /* fValidPID */

/* Grep for the following string in all files for a Linux or Unix system */
FUNCTION fGrepFileMasterString RETURNS LOGICAL (INPUT chrGrepString AS CHARACTER,
                                                INPUT chrFilePath   AS CHARACTER):
   
   DEFINE VARIABLE chrGrepCommand     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrGrepResult      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrFilePathCommand AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrFilePathExists  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrCurrentOS       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrOSCommand       AS CHARACTER NO-UNDO.
   
   chrOSCommand = "uname".
   
   INPUT THROUGH VALUE(chrOSCommand).
      IMPORT chrCurrentOS.
   INPUT CLOSE.
   
   IF chrGrepString = "" THEN
      RETURN FALSE.
   
   IF chrCurrentOS = "Linux" THEN
   DO:
   
      chrGrepCommand = 'grep "' + chrGrepString + '" ' + chrFilePath.
      
      INPUT THROUGH VALUE(chrGrepCommand).
         IMPORT chrGrepResult.
      INPUT CLOSE.
      
      OUTPUT CLOSE.
   
      IF chrGrepResult <> "grep:" THEN
         RETURN TRUE.
      ELSE 
         RETURN FALSE. 
   END. /* IF chrCurrentOS = "Linux" THEN */
   ELSE
   DO:
      
/*      chrFilePathCommand = "cd " + chrFilePath.*/
/*                                               */
/*      INPUT THROUGH VALUE(chrFilePathCommand). */
/*      INPUT CLOSE.                             */
      
      chrGrepCommand = 'find ./ -type f | xargs grep -i -s ' + chrGrepString + " " + chrFilePath.
      
      INPUT THROUGH VALUE(chrGrepCommand).
         IMPORT chrGrepResult.
      INPUT CLOSE.
      
      OUTPUT CLOSE.
   
      IF chrGrepResult <> "grep:" THEN
         RETURN TRUE.
      ELSE 
         RETURN FALSE. 
      
   END. /*ELSE OF IF chrCurrentOS = "Linux" THEN */
      
END FUNCTION. /* fGrepFileMasterString */


/* Returns the current working directory using the "pwd" Unix command. Will only work on Unix. */
FUNCTION fGetCurrentDirectory RETURNS CHARACTER:
   
   DEFINE VARIABLE chrCurrenrtDir AS CHARACTER.
   
   /* This uses the "pwd" Unix command to return the current directory into the Progress stream. Will only work on Unix. */
   INPUT THROUGH "pwd".
      IMPORT chrCurrenrtDir.
   INPUT CLOSE.
   
   RETURN chrCurrenrtDir.
   
END FUNCTION. /* fGetCurrentDirectory */


FUNCTION fGetCurrentPackageName RETURNS CHARACTER:

   DEFINE VARIABLE intEntry              AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrRootDirectory      AS CHARACTER   NO-UNDO FORMAT "x(20)".

   /* Returns the current working directory using Unix pwd command */
   chrRootDirectory = fGetCurrentDirectory().
   
   /* Root directory should be in the format "/dubdev/hyperthermbr/.." so 3nd entry (delimited by "/") should be the packagename */
   IF NUM-ENTRIES(chrRootDirectory,"/") > 1 THEN
      RETURN ENTRY(3,chrRootDirectory,"/").
   ELSE 
      RETURN "".

END FUNCTION. /* fGetCurrentPackageName */


FUNCTION fGetCurrentApplicationID RETURNS INTEGER:
   
   DEFINE VARIABLE chrPackageName AS CHARACTER.
   
   DEFINE BUFFER readApplication FOR gate.Application.
   
   chrPackageName = fGetCurrentPackageName().
   
   FIND FIRST readApplication NO-LOCK
      WHERE readApplication.PackageName = chrPackageName NO-ERROR.
   
   IF AVAILABLE readApplication THEN
      RETURN readApplication.ApplicationID.
   ELSE
      RETURN 0.
   
END FUNCTION. /*fGetCurrentApplicationID*/


/* Gets the working directory of the session and extracts the environment from the directory root */
FUNCTION fGetCurrentEnvironment RETURNS CHARACTER:
   
   DEFINE VARIABLE intEntry              AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrServerList         AS CHARACTER   NO-UNDO INITIAL "dev,qa,uat,sit".
   DEFINE VARIABLE chrRootDirectory      AS CHARACTER   NO-UNDO FORMAT "x(20)".
   
   /* Returns the current working directory using Unix pwd command */
   chrRootDirectory = fGetCurrentDirectory().
   
   DO intEntry = 1 TO NUM-ENTRIES(chrServerList):
      
      /* Root directory should be in the format "/dubdev/hyperthermbr/.." so 2nd entry (delimited by "/") should be the environment */
      IF NUM-ENTRIES(chrRootDirectory,"/") > 1 AND (INDEX(ENTRY(1,chrRootDirectory,"/"), ENTRY(intEntry,chrServerList)) > 0 
         OR INDEX(ENTRY(2,chrRootDirectory,"/"), ENTRY(intEntry,chrServerList)) > 0) THEN
      DO:
         RETURN CAPS(ENTRY(intEntry,chrServerList)).
      END.
   END. /*DO intEntry = 1 TO NUM-ENTRIES(chrServerList):*/
   
   /* If we didn't find one of the environments in the list then we must be in LIVE */
   RETURN "LIVE".
   
END FUNCTION. /*fGetCurrentEnvironment*/


FUNCTION fGetCurrentEnvironmentID RETURNS INTEGER:
   
   DEFINE VARIABLE chrEnvironmentCode AS CHARACTER.
   
   DEFINE BUFFER readEnvironment FOR gate.Environment.
   
   chrEnvironmentCode = fGetCurrentEnvironment().
   
   FIND FIRST readEnvironment NO-LOCK
      WHERE readEnvironment.EnvironmentCode = chrEnvironmentCode NO-ERROR.
   
   IF AVAILABLE readEnvironment THEN
      RETURN readEnvironment.EnvironmentID.
   ELSE
      RETURN 0.
   
END FUNCTION. /*fGetCurrentEnvironmentID*/


/* Gets the Progress error message description */
FUNCTION fGetErrorMessageText RETURNS CHARACTER(intMsg AS INTEGER):
   
   DEFINE VARIABLE intMsgNo AS INTEGER NO-UNDO. 
   DEFINE VARIABLE chrMsgText AS CHARACTER NO-UNDO. 

   ASSIGN chrMsgText = SEARCH("prohelp/msgdata/msg" + STRING(TRUNCATE((INTEGER(_MSG(1)) - 1) / 50, 0) + 1)) 
          chrMsgText = SUBSTRING(chrMsgText, 1, LENGTH(chrMsgText) - 1)
          intMsgNo = TRUNCATE((intMsg - 1) / 50, 0) + 1 
          chrMsgText = chrMsgText + STRING(intMsgNo) 
          FILE-INFO:FILE-NAME = chrMsgText.
   
   /* If MsgNum is too high for current Progress version: */ 
   IF FILE-INFO:FULL-PATHNAME EQ ? THEN 
      RETURN "(" + STRING(intMsg) + ")". 
   
   INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME). 
   
   REPEAT: 
      ASSIGN intMsgNo  = ? 
             chrMsgText = ?.
 
      IMPORT intMsgNo chrMsgText ^. 
  
      IF intMsgNo EQ intMsg THEN 
         LEAVE. 
   END. /* REPEAT: */
   
   INPUT CLOSE.
   
   RETURN chrMsgText. 
   
END FUNCTION. /* fGetErrorMessageText */ 


FUNCTION fGetFilesFromDir RETURNS CHARACTER (INPUT chrFilesDirectory    AS CHARACTER,  /* Must be populated */
                                             INPUT chrFileMask          AS CHARACTER,  /* If not blank format will be like "ABC*.xml" */
                                             INPUT chrFileExtensionList AS CHARACTER): /* If not blank then only files with NO extensions */
   
   DEFINE VARIABLE chrCurrentFileName     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrFilePath            AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intNumFiles            AS INTEGER     NO-UNDO.
   DEFINE VARIABLE logCheckForWriteAccess AS LOGICAL     NO-UNDO.
   
   IF fValidDirectory(chrFilesDirectory) <> "Ok" THEN
      RETURN fValidDirectory(chrFilesDirectory).
   
   INPUT STREAM sReadFiles FROM OS-DIR(chrFilesDirectory).
   
   /* This reads the files and sub-directories within the given directory */
   FileLoop:
   REPEAT:
      
      /* Just gets the name of the file/directory */
      IMPORT STREAM sReadFiles chrCurrentFileName.
      
      /* If we add the path to the file name and then we can get the handle to the OS file and all of the OS file info */
      ASSIGN chrFilePath = chrFilesDirectory + "/" + chrCurrentFileName
             FILE-INFO:FILE-NAME = chrFilePath.
      
      /* Don't want sub-directories */
      IF(SUBSTRING(FILE-INFO:FILE-TYPE,1,1) = "D") THEN
         NEXT FileLoop.
      
      /*MESSAGE SKIP chrCurrentFileName ":" chrFileMask "MATCHES?" chrCurrentFileName MATCHES chrFileMask VIEW-AS ALERT-BOX.*/
      
      /* If Mask is set them use it to filter */
      IF chrFileMask <> "" AND NOT chrCurrentFileName MATCHES chrFileMask THEN
         NEXT FileLoop.
      
      IF chrFileExtensionList <> "ALL" AND LOOkUP(fFileExtension(chrCurrentFileName), chrFileExtensionList) = 0 THEN
         /* If not using a Mask then exclude extensions that we don't want */
         NEXT FileLoop.
      
      CREATE ttSystemFile.
      ASSIGN intNumFiles                = intNumFiles + 1
             ttSystemFile.FileNo        = intNumFiles
             ttSystemFile.FileName      = chrCurrentFileName
             ttSystemFile.FileType      = FILE-INFO:FILE-TYPE
             ttSystemFile.FilePath      = FILE-INFO:FULL-PATHNAME
             ttSystemFile.FileSize      = FILE-INFO:FILE-SIZE
             ttSystemFile.DateModified  = FILE-INFO:FILE-MOD-DATE
             ttSystemFile.DateCreated   = FILE-INFO:FILE-CREATE-DATE
             ttSystemFile.FileExtension = fFileExtension(chrCurrentFileName).
      
   END. /*REPEAT:*/
   
   INPUT STREAM sReadFiles CLOSE.
   
   RETURN "Ok".
   
END FUNCTION. /*fGetFilesFromDir*/


FUNCTION fGetDirectoryList RETURNS CHARACTER (INPUT chrParentDirectory AS CHARACTER):
   
   DEFINE VARIABLE    chrCurrentFileName     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE    chrFilePath            AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE    intNumFiles            AS INTEGER     NO-UNDO.
   DEFINE VARIABLE    logCheckForWriteAccess AS LOGICAL     NO-UNDO.
   
   IF fValidDirectory(chrParentDirectory) <> "Ok" THEN
      RETURN fValidDirectory(chrParentDirectory).
   
   INPUT STREAM sReadSubDirs FROM OS-DIR(chrParentDirectory).
   
   /* This reads the files and sub-directories within the given directory */
   FileLoop:
   REPEAT:
      
      /* Just gets the name of the file/directory */
      IMPORT STREAM sReadSubDirs chrCurrentFileName.
      
      /* If we add the path to the file name and then we can get the handle to the OS file and all of the OS file info */
      ASSIGN chrFilePath = chrParentDirectory + "/" + chrCurrentFileName
             FILE-INFO:FILE-NAME = chrFilePath.
      
      /* Don't want files */
      IF(SUBSTRING(FILE-INFO:FILE-TYPE,1,1) <> "D") THEN
         NEXT FileLoop.
      
      CREATE ttSystemFile.
      ASSIGN intNumFiles                = intNumFiles + 1
             ttSystemFile.FileNo        = intNumFiles
             ttSystemFile.FileName      = chrCurrentFileName
             ttSystemFile.FileType      = FILE-INFO:FILE-TYPE
             ttSystemFile.FilePath      = FILE-INFO:FULL-PATHNAME
             ttSystemFile.FileSize      = FILE-INFO:FILE-SIZE
             ttSystemFile.DateModified  = FILE-INFO:FILE-MOD-DATE
             ttSystemFile.DateCreated   = FILE-INFO:FILE-CREATE-DATE.
      
   END. /*REPEAT:*/
   
   INPUT STREAM sReadSubDirs CLOSE.
   
   RETURN "Ok".
   
END FUNCTION. /*fGetDirectoryList*/


FUNCTION fExtractNumDays RETURNS INTEGER (INPUT chrDirectoryName AS CHARACTER):
   
   DEFINE VARIABLE intFolderNumDays AS INTEGER     NO-UNDO.
   
   IF INDEX(chrDirectoryName,"days") <> 0 THEN
   DO:
      intFolderNumDays = INTEGER(SUBSTRING(chrDirectoryName,1,(LENGTH(chrDirectoryName) - 4))) NO-ERROR.
      /* If there isn't a valid integer then it won't come in here */
      IF NOT ERROR-STATUS:ERROR AND (intFolderNumDays > 0 AND intFolderNumDays < 365) THEN
      DO:
         RETURN intFolderNumDays.
      END. /*IF NOT ERROR-STATUS:ERROR THEN*/
   END. /*IF INDEX(chrDirectoryName,"days") <> 0 THEN*/
   
   RETURN 0.
   
END FUNCTION. /* fExtractNumDays */


FUNCTION fGetAgedDirectory RETURNS CHARACTER (INPUT chrParentDirectory  AS CHARACTER,
                                              INPUT intNumDaysToArchive AS INTEGER):
   
   DEFINE VARIABLE chrDirectoryError       AS CHARACTER.
   DEFINE VARIABLE chrCurrentFileName      AS CHARACTER.
   DEFINE VARIABLE logMatchingFolder       AS LOGICAL.
   DEFINE VARIABLE intPrevDaysDiscrepancy  AS INTEGER.
   DEFINE VARIABLE chrFilePath             AS CHARACTER.
   
   chrDirectoryError = fValidDirectory(chrParentDirectory).
   IF chrDirectoryError <> "Ok" THEN
      RETURN "Error: Invalid Directory:'" + chrParentDirectory + "' Error:" + chrDirectoryError.
   
   EMPTY TEMP-TABLE ttSubDir.
   
   INPUT STREAM sReadSubDirs FROM OS-DIR(chrParentDirectory).
   
   /* This reads the files and sub-directories within the given directory */
   SubDirLoop:
   REPEAT:
      /* Just gets the name of the file/directory */
      IMPORT STREAM sReadSubDirs chrCurrentFileName.
      
      /* Skip these */
      IF chrCurrentFileName = "." OR chrCurrentFileName = ".." THEN
         NEXT SubDirLoop.
      
      /* If we add the path to the file name and then we can get the handle to the OS file and all of the OS file info */
      ASSIGN chrFilePath = chrParentDirectory + chrCurrentFileName
             FILE-INFO:FILE-NAME = chrFilePath.
      
      /* Don't want files */
      IF(SUBSTRING(FILE-INFO:FILE-TYPE,1,1) <> "D") THEN
         NEXT SubDirLoop.
      
      /* Write a record for each Subdirectory */
      CREATE ttSubDir.
      ASSIGN ttSubDir.NumDaysToArchive = fExtractNumDays(chrCurrentFileName)
             ttSubDir.FileName         = chrCurrentFileName
             ttSubDir.FileType         = FILE-INFO:FILE-TYPE
             ttSubDir.FilePath         = FILE-INFO:FULL-PATHNAME
             ttSubDir.FileSize         = FILE-INFO:FILE-SIZE
             ttSubDir.DateModified     = FILE-INFO:FILE-MOD-DATE
             ttSubDir.DateCreated      = FILE-INFO:FILE-CREATE-DATE.
      
      IF ttSubDir.NumDaysToArchive = intNumDaysToArchive THEN
      DO:
         ASSIGN chrParentDirectory = ttSubDir.FilePath + "/"
                logMatchingFolder  = TRUE.
         
         chrDirectoryError = fValidWriteDirectory(chrParentDirectory).
         IF chrDirectoryError <> "Ok" THEN
         DO:
            INPUT STREAM sReadSubDirs CLOSE.
            RETURN "Error: No write access to Directory:'" + chrParentDirectory + "' Error:" + chrDirectoryError.
         END.
         
         LEAVE SubDirLoop.
      END. /*IF ttSubDir.NumDaysToArchive = intNumDaysToArchive THEN*/
   END. /*REPEAT: SubDirLoop*/
   
   INPUT STREAM sReadSubDirs CLOSE.
   
   IF NOT logMatchingFolder THEN
   DO:
      intPrevDaysDiscrepancy = 365.
      
      SubDirLoop:
      FOR EACH ttSubDir
         BY    ttSubDir.NumDaysToArchive DESC:
         
         IF ttSubDir.NumDaysToArchive = 0 THEN
            NEXT SubDirLoop.
         
         /* Find the closest match to the number of days required while always trying to use one that has MORE days than required */
         IF (ttSubDir.NumDaysToArchive - intNumDaysToArchive > 0) AND 
            (ttSubDir.NumDaysToArchive - intNumDaysToArchive < intPrevDaysDiscrepancy) THEN
         DO:
            ASSIGN chrParentDirectory = ttSubDir.FilePath + "/"
                   intPrevDaysDiscrepancy = ttSubDir.NumDaysToArchive - intNumDaysToArchive.
         END.
      END. /*FOR EACH ttSubDir*/
      
      chrDirectoryError = fValidWriteDirectory(chrParentDirectory).
      IF chrDirectoryError <> "Ok" THEN
         RETURN "Error: No write access to Directory:'" + chrParentDirectory + "' Error:" + chrDirectoryError.
      
   END. /*IF NOT logMatchingFolder THEN*/
   
   EMPTY TEMP-TABLE ttSubDir.
   
   RETURN chrParentDirectory.
   
END FUNCTION. /* fGetAgedDirectory */


FUNCTION fMoveFile RETURNS CHARACTER (INPUT chrSourceFile       AS CHARACTER,
                                      INPUT chrTargetDirectory  AS CHARACTER,
                                      INPUT chrFileName         AS CHARACTER,
                                      INPUT intNumDaysToArchive AS INTEGER):
   
   DEFINE VARIABLE chrDirectoryError       AS CHARACTER.
   DEFINE VARIABLE chrUnixCommand          AS CHARACTER.
   DEFINE VARIABLE logMatchingFolder       AS LOGICAL.
   DEFINE VARIABLE intPrevDaysDiscrepancy  AS INTEGER.
   DEFINE VARIABLE chrFilePath             AS CHARACTER.
   DEFINE VARIABLE chrNewAgedDirectory     AS CHARACTER.
   
   /* Make sure there's a forward slash on the end of the directory name */
   ASSIGN chrTargetDirectory = RIGHT-TRIM(chrTargetDirectory,"/").
          chrTargetDirectory = chrTargetDirectory + "/".
   
   chrDirectoryError = fValidWriteDirectory(chrTargetDirectory).
   IF chrDirectoryError <> "Ok" THEN
      RETURN "Error: Invalid Directory:'" + chrTargetDirectory + "' Error:" + chrDirectoryError.
   
   /* Is this isn't Zero then we need to find the closest matching subdirectory with the number of days in the folder name e.g. "30days" */
   IF intNumDaysToArchive > 0 THEN
   DO:
      chrNewAgedDirectory = fGetAgedDirectory(chrTargetDirectory, intNumDaysToArchive).
      IF chrNewAgedDirectory BEGINS "Error" THEN
         RETURN "Error: Invalid Aged Directory:" + chrNewAgedDirectory.
      
      chrTargetDirectory = chrNewAgedDirectory.
   END.
   
   /* If successfully completed then move from source folder to target folder */
   chrUnixCommand = "mv " + chrSourceFile + " " + chrTargetDirectory + chrFileName.
   OS-COMMAND SILENT VALUE(chrUnixCommand).
   
   IF SEARCH(chrTargetDirectory + chrFileName) <> ? THEN
      RETURN chrTargetDirectory + chrFileName.
   ELSE
      RETURN "Error moving File:'" + chrSourceFile + "' to:'" + chrTargetDirectory + chrFileName + "'. File NOT Moved.".
   
END FUNCTION. /*fMoveFile*/


FUNCTION fDeleteFile RETURNS CHARACTER (INPUT chrFilePath AS CHARACTER):
   
   DEFINE VARIABLE chrUnixCommand    AS CHARACTER.
   
   IF SEARCH(chrFilePath) = ? THEN
      RETURN "Error Deleting File:'" + chrFilePath + "'. File could not be found.".
   
   /* Delete file using rm command and hide the output if any */
   chrUnixCommand = "rm -f " + chrFilePath + " 2>/dev/null".
   OS-COMMAND SILENT VALUE(chrUnixCommand).
   
   IF SEARCH(chrFilePath) <> ? THEN
      RETURN "Error Deleting File:'" + chrFilePath + "'. File could not be found.".
   ELSE
      RETURN "Ok".
   
END FUNCTION. /*fDeleteFile*/


FUNCTION fNextFileVersion RETURNS CHARACTER (INPUT chrFileName          AS CHARACTER,
                                             INPUT chrFilePath          AS CHARACTER,
                                             INPUT intNumDaysToArchive  AS INTEGER):
   
   DEFINE VARIABLE intVersion         AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrRejectFileName  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrDirectoryError  AS CHARACTER   NO-UNDO.
   
   /* Make sure there's a forward slash on the end of the directory name */
   ASSIGN chrFilePath = RIGHT-TRIM(chrFilePath,"/").
          chrFilePath = chrFilePath + "/".
   
   chrDirectoryError = fValidWriteDirectory(chrFilePath).
   IF chrDirectoryError <> "Ok" THEN
      RETURN "Error: Invalid Directory:'" + chrFilePath + "'. " + chrDirectoryError.
   
   /* Rename the File with a _v1, _v2, _v3 etc on the end so that we don't overwrite an existing File */
   VersionLoop:
   DO intVersion = 1 TO 100:
      
      chrRejectFileName = ENTRY(1,chrFileName,".") + "_v" + STRING(intVersion) + "." + ENTRY(2,chrFileName,".").
      IF SEARCH(chrFilePath + "rej/" + chrRejectFileName) = ? THEN
         LEAVE VersionLoop.
   END. /*DO intVersion = 1 TO 100:*/
   
   RETURN chrRejectFileName.
   
END FUNCTION. /* fNextFileVersion */


FUNCTION fControlWebBroker RETURNS CHARACTER (INPUT  chrBrokerName  AS CHARACTER,
                                              INPUT  chrOperation   AS CHARACTER):
   
   DEFINE VARIABLE chrEnvironment          AS CHARACTER.
   DEFINE VARIABLE chrScriptDirectory      AS CHARACTER.
   DEFINE VARIABLE chrDirectoryError       AS CHARACTER.
   DEFINE VARIABLE chrFileName             AS CHARACTER.
   DEFINE VARIABLE chrUnixCommand          AS CHARACTER.
   DEFINE VARIABLE chrBrokerResponse       AS CHARACTER.
   DEFINE VARIABLE chrPackageName          AS CHARACTER.
   
   ASSIGN chrEnvironment = fGetCurrentEnvironment()
          chrPackageName = fGetCurrentPackageName().
   
   IF chrOperation <> "-start" AND chrOperation <> "-stop" AND chrOperation <> "-query" THEN
      RETURN "Error: Invalid Operation:'" + chrOperation.
   
   /* Make sure there's a forward slash on the end of the directory name */
   chrScriptDirectory = LOWER("/i" + TRIM(REPLACE(chrEnvironment,"QA","TEST")) + "/" + TRIM(chrPackageName) + "/stopstart/").
   
   chrDirectoryError = fValidWriteDirectory(chrScriptDirectory).
   IF chrDirectoryError <> "Ok" THEN
      RETURN "Error: Invalid Script Directory:'" + chrScriptDirectory + "' Error:" + chrDirectoryError.
   
   chrFileName = "control_broker.sh".

   IF SEARCH(chrScriptDirectory + chrFileName) = ? THEN
      RETURN "Error finding script File:'" + chrScriptDirectory + chrFileName + "'".
   
   /* If all ok then try to manipulate the broker */
   chrUnixCommand = chrScriptDirectory + chrFileName + " " + chrBrokerName + " " + chrOperation.
   
   /* If not querying the Broker then just shell out to the Start Stop command */
   IF chrOperation <> "-query" THEN
   DO:
      OS-COMMAND SILENT VALUE(chrUnixCommand).
   END.   
   ELSE
   DO:
      /* This returns the Unix output of this Command into the Progress stream. Will only work on Unix. */
      INPUT THROUGH VALUE(chrUnixCommand).
         
         IMPORT chrBrokerResponse.
      INPUT CLOSE.
      
      RETURN chrBrokerResponse.
   END.
   
   RETURN "Ok".
   
END FUNCTION. /*fControlWebBroker*/


/* Check Broker status using the "wtbman" Unix command.  Will only work on Unix. */
FUNCTION fGetWebBrokerStatus RETURNS CHARACTER (INPUT chrBrokerName AS CHARACTER): 
   
   DEFINE VARIABLE chrUnixCommand AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrUnixResult  AS CHARACTER NO-UNDO.
   
   IF chrBrokerName = "" THEN
      RETURN "".
   
   /* Look for the Unix PID, get the last line printed, and capture last column */
   chrUnixCommand = "/$DLC/bin/wtbman -port 20934 -name " + chrBrokerName + " -query | grep 'Broker Status' | tail -1 | tr -s ' ' | cut -d ' ' -f4".
   
   INPUT THROUGH VALUE(chrUnixCommand).
      IMPORT chrUnixResult NO-ERROR.
   INPUT CLOSE. 
   
   RETURN chrUnixResult.
   
END FUNCTION. /* fGetWebBrokerStatus */


/* Check Available Agents using the "wtbman" Unix command.  Will only work on Unix. */
FUNCTION fGetWebBrokerAvailableAgents RETURNS CHARACTER (INPUT chrBrokerName AS CHARACTER): 
    
    DEFINE VARIABLE chrUnixCommand AS CHARACTER NO-UNDO.
    DEFINE VARIABLE chrUnixResult  AS CHARACTER NO-UNDO.
    
    IF chrBrokerName = "" THEN
       RETURN "".
    
    /* Check for the Web Available Agents */
    chrUnixCommand = "./$DLC/bin/wtbman -port 20934 -name " + chrBrokerName + " -query | grep 'Available Agents' | tail -1 | tr -s ' ' | cut -d ' ' -f4".
   
    INPUT THROUGH VALUE(chrUnixCommand).
    IMPORT chrUnixResult NO-ERROR.
    INPUT CLOSE.
    
    RETURN chrUnixResult. 

END FUNCTION. /* fGetWebBrokerAvailableAgents */



         
