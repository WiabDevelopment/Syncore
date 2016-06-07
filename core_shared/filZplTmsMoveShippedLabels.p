/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filZplTmsMoveShippedLabels.p
Purpose : Moves 'Shipped' status ShipOrders to the bck folder. The zpl mask for the files is
          ShipOrder.Shipped.zpl
Author  : SH
Date    : 30th March 2015
---------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
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
DEFINE VARIABLE chrShipOrderRef                         AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intTotalShipOrdersUpdated               AS INTEGER      NO-UNDO INITIAL 0.
DEFINE VARIABLE logErrorFound                           AS LOGICAL      NO-UNDO.

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

chrLogFile = chrLogFileDirectory + chrFileMasterName + "_filZplShipOrderShipped_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

IF logGblDebugging THEN
DO:
   /* This is used to reset logGblDebugging before exiting because FileMaster.DebuggingOn can override CronEvent and enable debugging */
   logEnableDebugging = TRUE.
   /* Setup log files */
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
END. /* IF logGblDebugging */

FIND FIRST gate.GateUser NO-LOCK /*idx=GateUserID*/
   WHERE gate.GateUser.GateUserID = intGblUserID NO-ERROR.
IF AVAILABLE gate.GateUser AND gate.GateUser.Username <> "cron" THEN
   chrEmailAddress = gate.GateUser.Email.

ImportBlock:
DO ON STOP UNDO, RETURN ERROR:
   FIND FIRST FileMaster NO-LOCK /*idx=MasterName*/
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

   FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR. /*idx=FileTypeID*/
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
   
   /* This searches the directory of the FileMaster and creates ttSystemFile records for each File that matches the Mask we input */
   chrReturnValue = fGetFilesFromDir(FileMaster.FilePath,
                                     FileMaster.FileMask,
                                     FileType.Extension) NO-ERROR.

   IF chrReturnValue BEGINS "Error" THEN
   DO:
      IF logGblDebugging THEN 
      DO:
         
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
   FOR EACH ttSystemFile ON ERROR UNDO:
      
      chrFileUploadError = "".
      EMPTY TEMP-TABLE ttFileUploadError NO-ERROR.
        
      /* Get the Package Tracking Ref from Label File */
      IF NUM-ENTRIES(ttSystemFile.FileName, ".") > 1 THEN 
         chrShipOrderRef = ENTRY(1, ttSystemFile.FileName, ".").

      FIND FIRST ShipOrder NO-LOCK /*idx=ShipOrderID*/
         WHERE ShipOrder.OrderRef = chrShipOrderRef NO-ERROR.
      IF NOT AVAILABLE ShipOrder THEN
      DO:
         NEXT FileLoop.
      END. /* IF NOT AVAILABLE ShipOrder THEN */
        
      IF ShipOrder.Shipped <> "" THEN
      DO:
         fMoveFile(ttSystemFile.FilePath,
                   FileMaster.FileBackupPath,
                   ttSystemFile.FileName,
                   FileMaster.DaysToArchive).
      END.
                                      
   END. /* FOR EACH ttSystemFile: */
      
   IF logGblDebugging THEN
   DO:
     OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logGblDebugging */
END. /* ImportBlock */

DELETE OBJECT chrSsnFileMasterName NO-ERROR.

RELEASE gate.GateUser NO-ERROR.
RELEASE FileMaster    NO-ERROR.
RELEASE FileType      NO-ERROR.
RELEASE File          NO-ERROR.


PROCEDURE pSendErrorEmail:

   DEFINE INPUT PARAMETER chrSubjectString AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER chrErrorString   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER chrAttachment    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER intEmailGroup    AS INTEGER   NO-UNDO.

/*   RUN osSendMail.p(INPUT "",               /* Optional list of Users */                    */
/*                    INPUT chrSubjectString, /* Email Subject */                             */
/*                    INPUT chrErrorString,   /* Plain text message Body */                   */
/*                    INPUT "",               /* Html format message Body */                  */
/*                    INPUT chrAttachment,    /* File path ../files/file */                   */
/*                    INPUT intEmailGroup,    /* EmailGroupID that you want to send this to */*/
/*                    INPUT 0).               /* File MasterID is it applies */               */

END PROCEDURE. /*End of pSendErrorMail */ 

/* Generates new GUIDs */
PROCEDURE pGetGUIDs:
   
   DEFINE OUTPUT PARAMETER chrTransactionGUID AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER chrMessageGUID     AS CHARACTER NO-UNDO.
   
   chrTransactionGUID = GUID(GENERATE-UUID).
   chrMessageGUID     = GUID(GENERATE-RANDOM-KEY).

END PROCEDURE. /* pGetGUIDs */