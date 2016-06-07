/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filMoveOutputFiles.p
Purpose : Finds output files then checks the days associated with the FileMaster and moves the files to the bck days directory. 
Author  : Christopher Shelley
Date    : 27th March 2014
---------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
19/01/2015 CS  WiaB       Enhanced to run for all outbound file types on the FileMaster.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncClassFunctions.i}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}
{fncDateFunctions.i}
{fncGlobalFunctions.i}

/* Local Variables */
DEFINE VARIABLE chrEmailAddress                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrReturnValue                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory                     AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrLogFile                              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE chrParentDirectory                      AS CHARACTER    NO-UNDO FORMAT "X(20)".
DEFINE VARIABLE intFileType                             AS INTEGER      NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID                     AS INTEGER      NO-UNDO.

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

chrLogFile = chrLogFileDirectory + "filMoveOutputFiles_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN 
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.

FIND FIRST gate.GateUser NO-LOCK
   WHERE gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

FIND FIRST EmailGroup NO-LOCK /* index GroupCode */
   WHERE EmailGroup.GroupCode = "CronErrors" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

MainBlock:
DO ON STOP UNDO, RETURN ERROR:
   /* Only do for outbound files since inbound get moved when processed */
   FileMaster_Loop:
   FOR EACH FileMaster NO-LOCK 
      WHERE FileMaster.Direction = "OUT":
            
      EMPTY TEMP-TABLE ttSystemFile NO-ERROR.

      FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FileType THEN
      DO:
         RUN osSendMail.p (INPUT chrEmailAddress,                             /* Optional list of Users */
                           INPUT FileMaster.MasterName + " missing FileType", /* Email Subject */
                           INPUT "Skipping FileMaster.  Could not find file "
                                    + "extension to search for files.",       /* Plain text message Body */
                           INPUT "",                                          /* Html format message Body */
                           INPUT ttSystemFile.FilePath,                       /* File path../idev/hyperthermbr/files/file OR */
                                                                              /* ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     intCronEmailGroupID
                                  ELSE 0),                                    /* EmailGroupID that you want to send this to */
                           INPUT "").                                         /* File MasterID is it applies */ 
          
         NEXT FileMaster_Loop.
      END.   
      
      IF logGblDebugging THEN 
         fLog("FileMaster: " + FileMaster.MasterName + " Path: " + RIGHT-TRIM(FileMaster.FilePath,"/") + "/bck/" + " Mask: " 
                 + FileMaster.FileMask + " Extension: " + FileType.Extension).
      
      /* This searches the files/outray/bck directory and creates ttSystemFile records for each file that matches the Mask and Extension */
      chrReturnValue = fGetFilesFromDir(RIGHT-TRIM(FileMaster.FilePath,"/") + "/bck/",
                                        FileMaster.FileMask,
                                        FileType.Extension) NO-ERROR.

      IF chrReturnValue BEGINS "Error" THEN
      DO:
         IF logGblDebugging THEN 
         DO:
            fLog(chrReturnValue).
            OUTPUT STREAM sToLogFile CLOSE.
         END. /* IF logGblDebugging */
         
         RELEASE gate.GateUser NO-ERROR.
         RELEASE EmailGroup    NO-ERROR.
         RELEASE FileMaster    NO-ERROR.
         RELEASE FileType      NO-ERROR.
         
         RETURN ERROR chrReturnValue.
      END. /* IF chrReturnValue BEGINS "Error" */
      
      File_Loop:
      FOR EACH ttSystemFile ON STOP UNDO, RETURN ERROR:

         /* Move File to the No of days directory or leave it in bck directory */
         chrReturnValue = fMoveFile(ttSystemFile.FilePath,                         /* Source File full path including FileName */
                                    RIGHT-TRIM(FileMaster.FilePath,"/") + "/bck/", /* Target path exclusing FileName */
                                    ttSystemFile.FileName,                         /* FileName */
                                    FileMaster.DaysToArchive).                     /* No of days to retain the backup file */
      
         IF logGblDebugging THEN
            fLog("Moving:" + ttSystemFile.FilePath + " Days: "  + STRING(FileMaster.DaysToArchive)).
      
         IF chrReturnValue BEGINS "Error" THEN
         DO:
            RUN osSendMail.p (INPUT chrEmailAddress,                            /* Optional list of Users */
                              INPUT FileMaster.MasterName + " File Move Error", /* Email Subject */
                              INPUT chrReturnValue + " Skipping file move to " 
                                       + STRING(FileMaster.DaysToArchive) 
                                       + " Folder",                             /* Plain text message Body */
                              INPUT "",                                         /* Html format message Body */
                              INPUT ttSystemFile.FilePath,                      /* File path../idev/hyperthermbr/files/file OR */
                                                                                /* ../files/file */
                              INPUT (IF chrEmailAddress = "" THEN
                                        intCronEmailGroupID
                                     ELSE 0),                                   /* EmailGroupID that you want to send this to */
                              INPUT "").                                        /* File MasterID is it applies */
                
            IF logGblDebugging THEN
               fLog(chrReturnValue).
            
            NEXT File_Loop.
         END. /* IF chrReturnValue BEGINS "Error" */

      END. /* FOR EACH ttSystemFile */

   END. /* FOR EACH FileMaster */
END. /* MainBlock */
      
IF logGblDebugging THEN
   OUTPUT STREAM sToLogFile CLOSE.

RELEASE gate.GateUser NO-ERROR.
RELEASE EmailGroup    NO-ERROR.
RELEASE FileMaster    NO-ERROR.
RELEASE FileType      NO-ERROR.