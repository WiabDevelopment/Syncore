/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filZplTmsAwaitingLabelsUpdate.p
Purpose : Updates ShipOrders from "AwaitingTMSLabels" status to "ReadyToProcess" status when all ShipPackages
          for a given Order have an associated zpl label file in files/zpl/intray. The zpl mask for the files is
          ShipPackageCalculation.CarrierTrackingRef.zpl
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

DEFINE TEMP-TABLE ttShipOrdersToUpdate NO-UNDO
   FIELD TotalTrackingRefsRequired AS INTEGER
   FIELD TotalTrackingRefsFound AS INTEGER
   FIELD ShipOrderID AS INTEGER. 

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
DEFINE VARIABLE intEmailSendCount                       AS INTEGER      NO-UNDO INITIAL 0.
DEFINE VARIABLE intNumSuccessful                        AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumRejected                          AS INTEGER      NO-UNDO.
DEFINE VARIABLE intNumPending                           AS INTEGER      NO-UNDO.
DEFINE VARIABLE logEnableDebugging                      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE chrTrackingRef                          AS CHARACTER    NO-UNDO FORMAT "x(30)".
DEFINE VARIABLE intTotalShipOrdersUpdated               AS INTEGER      NO-UNDO INITIAL 0.
DEFINE VARIABLE logErrorFound                           AS LOGICAL      NO-UNDO.
DEFINE VARIABLE intShipPackageCalculationCount          AS INTEGER      NO-UNDO.
DEFINE VARIABLE chrErrorString                          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE intShipPackageCalcNums                  AS INTEGER      NO-UNDO.
DEFINE VARIABLE chrErrorDetails                  AS CHARACTER NO-UNDO.

DEFINE BUFFER otherShipOrderLine FOR ShipOrderLine.

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

chrLogFile = chrLogFileDirectory + chrFileMasterName + "_filZplImport_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

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

FIND FIRST OperationType NO-LOCK /*idx=TypeCode*/
   WHERE OperationType.TypeCode = "TmsLabelVerification" NO-ERROR.
IF NOT AVAILABLE OperationType THEN
DO:
   chrErrorDetails = chrErrorDetails + "No OperationType for TmsLabelVerification".
   logErrorFound = TRUE.
END.
intGblOperationTypeID = OperationType.OperationTypeID. 
   
FIND FIRST EmailGroup NO-LOCK /*idx=GroupCode*/
   WHERE   EmailGroup.GroupCode = "ZplAwaitingLabelUpdate" NO-ERROR.
IF NOT AVAILABLE EmailGroup THEN
DO:
   RETURN ERROR "NO EmailGroup for ZplAwaitingLabelUpdate". 
END.   

ImportBlock:
DO ON STOP UNDO, RETURN ERROR:
         
   IF logErrorFound THEN
      LEAVE ImportBlock.
      
   FIND FIRST FileMaster NO-LOCK /*idx=MasterName*/
      WHERE FileMaster.MasterName = chrFileMasterName NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrFileUploadError = "No FileMaster for '" + chrFileMasterName + "'.".
      
      IF logGblDebugging THEN
      DO:
         fLog(chrFileUploadError).
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
         chrTrackingRef = REPLACE(ENTRY(1, ttSystemFile.FileName, "."), "label","").
      
      FOR EACH ShipPackageCalculation NO-LOCK /*idx=CarrierTrackingRef*/
         WHERE ShipPackageCalculation.CarrierTrackingRef = chrTrackingRef:
        
         intShipPackageCalculationCount = intShipPackageCalculationCount + 1.
         intShipPackageCalcNums         = ShipPackageCalculation.ShipPackageCalculationID.
      END.
      
      IF intShipPackageCalculationCount > 1 THEN
      DO:
         
         IF intEmailSendCount = 0 THEN
         DO:
            RUN pSendErrorEmail("ZplAwaitingLabel Cron Error",
                                "Duplicate CarrierTracking Refs found in ShipPackageCalculation Table for " +
                                " ShipPackageCalculationID " + STRING(intShipPackageCalcNums),
                                "",
                                INTEGER(EmailGroup.EmailGroupID)).
                                
            intEmailSendCount = intEmailSendCount + 1.
         END.
         
         /* Reset Package Count */
         intShipPackageCalculationCount = 0.
         
         NEXT FileLoop.
      END. /* IF intShipPackageCalculationCount > 1 THEN */
      
      intShipPackageCalculationCount = 0.

      FIND FIRST ShipPackageCalculation NO-LOCK /*idx=CarrierTrackingRef*/
         WHERE ShipPackageCalculation.CarrierTrackingRef = chrTrackingRef NO-ERROR.
      IF NOT AVAILABLE ShipPackageCalculation THEN
      DO:
         NEXT FileLoop.
      END. /* IF NOT AVAILABLE ShipPackage THEN */

      FIND FIRST ShipOrder NO-LOCK /*idx=ShipOrderID*/
         WHERE ShipOrder.ShipOrderID = ShipPackageCalculation.ShipOrderID NO-ERROR.
      IF NOT AVAILABLE ShipOrder THEN
      DO:
         NEXT FileLoop.
      END. /* IF NOT AVAILABLE ShipOrder THEN */
        
      IF ShipOrder.ShipOrderStatusID <> fGetStatusID("ShipOrder", "AwaitingTmsLabels") THEN
      DO:
         NEXT FileLoop.
      END. /* IF ShipOrder.ShipOrderStatusID <> fGetStatusID("ShipOrder", "AwaitingTmsLabels") THEN */

      FIND FIRST ttShipOrdersToUpdate NO-LOCK
         WHERE ttShipOrdersToUpdate.ShipOrderID = ShipOrder.ShipOrderID NO-ERROR.
      IF AVAILABLE ttShipOrdersToUpdate THEN
      DO:
         ttShipOrdersToUpdate.TotalTrackingRefsFound = ttShipOrdersToUpdate.TotalTrackingRefsFound + 1.
      END. /* IF AVAILABLE ttShipOrdersToUpdate THEN */
      ELSE
      DO:
         /* Create a Temp-Table to Keep Track of PackagesFound */
         CREATE ttShipOrdersToUpdate.
         ASSIGN ttShipOrdersToUpdate.ShipOrderID               = ShipOrder.ShipOrderID
                ttShipOrdersToUpdate.TotalTrackingRefsRequired = ShipOrder.NumPackages
                ttShipOrdersToUpdate.TotalTrackingRefsFound    = 1.
      END. /* ELSE */
   END. /* FOR EACH ttSystemFile: */
   
   FOR EACH ttShipOrdersToUpdate ON ERROR UNDO:
      IF ttShipOrdersToUpdate.TotalTrackingRefsFound = ttShipOrdersToUpdate.TotalTrackingRefsRequired THEN
      DO TRANS:
         FIND FIRST ShipOrder EXCLUSIVE-LOCK /*idx=ShipOrderID*/
            WHERE ShipOrder.ShipOrderID = ttShipOrdersToUpdate.ShipOrderID NO-WAIT NO-ERROR.
         IF AVAILABLE ShipOrder THEN
         DO:
            /*Set Invalid/ShipOrder Error and Send Email/Continue? */
            ASSIGN ShipOrder.ShipOrderStatusID = fGetStatusID("ShipOrder", "ReadyToProcess").
            intTotalShipOrdersUpdated = intTotalShipOrdersUpdated + 1.
            
            ShipOrderLineLoop:
            FOR EACH ShipOrderLine OF ShipOrder NO-LOCK: /*idx=ShipOrderID*/
            
               FIND FIRST otherShipOrderLine EXCLUSIVE-LOCK /*idx=ShipOrderLineID*/
                  WHERE   otherShipOrderLine.ShipOrderLineID = ShipOrderLine.ShipOrderLineID NO-WAIT NO-ERROR.
               IF AVAILABLE otherShipOrderLine THEN
               DO:
                  
                  IF otherShipOrderLine.ShipOrderStatusID = fGetStatusID("ShipOrder", "Cancelled") THEN
                     NEXT ShipOrderLineLoop.
             
                  ASSIGN otherShipOrderLine.ShipOrderStatusID = fGetStatusID("ShipOrder", "ReadyToProcess").
                  
               END. /*IF AVAILABLE otherShipOrderLine THEN*/
               FIND CURRENT otherShipOrderLine NO-LOCK NO-ERROR.
            END. /* FOR EACH ShipOrderLine OF ShipOrder NO-LOCK */
            
            /* Remove EXCLUSIVE-LOCK */
            FIND CURRENT ShipOrder          NO-LOCK NO-ERROR.
         
         END. /*IF AVAILABLE ShipOrder THEN */
      END. /* IF ttShipOrdersToUpdate.TotalTrackingRefsFound */
      ELSE
      DO:
         DELETE ttShipOrdersToUpdate.
      END. /* ELSE */
   END. /* FOR EACH ttShipORdersToUpdate */
   
   IF logGblDebugging THEN
   DO:
     OUTPUT STREAM sToLogFile CLOSE.
   END. /* IF logGblDebugging */
END. /* ImportBlock */

/* Send Email of Errors */
IF logErrorFound THEN
DO:    
   chrErrorString = chrErrorString + chrErrorDetails. 
                
   RUN pSendErrorEmail(INPUT "Ship Order Cancel Error",
                       INPUT chrErrorString,
                       INPUT "",
                       INPUT EmailGroup.EmailGroupID).    
END. /* IF logErrorFound */  

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

   RUN osSendMail.p(INPUT "",               /* Optional list of Users */
                    INPUT chrSubjectString, /* Email Subject */
                    INPUT chrErrorString,   /* Plain text message Body */
                    INPUT "",               /* Html format message Body */
                    INPUT chrAttachment,    /* File path ../files/file */
                    INPUT intEmailGroup,    /* EmailGroupID that you want to send this to */
                    INPUT 0).               /* File MasterID is it applies */

END PROCEDURE. /*End of pSendErrorMail */ 

/* Generates new GUIDs */
PROCEDURE pGetGUIDs:
   
   DEFINE OUTPUT PARAMETER chrTransactionGUID AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER chrMessageGUID     AS CHARACTER NO-UNDO.
   
   chrTransactionGUID = GUID(GENERATE-UUID).
   chrMessageGUID     = GUID(GENERATE-RANDOM-KEY).

END PROCEDURE. /* pGetGUIDs */
