/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filXmlTmsShippedStatusExport.p
Purpose : 

Author  : Shawn Hilts
Date    : 11/03/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
8/17/2015  AGL            Added update to Outbound timestamp
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncClassFunctions.i}
{fncServerFunctions.i}
{fncLockingFunctions.i}
{fncDataFunctions.i}
{fncStockFunctions.i}
{fncStatusTypeFunctions.i}
{fncFileExportFunctions.i}

/* LOCAL VARIABLES */ 
DEFINE VARIABLE OrderStatusesIDCounter           AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE chrTransactionID                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE intPackedOutStatusID             AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrTimeStampTrimmed              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCurrentDate                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEmailAddress                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCurrentTime                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intB2CShipOrderTypeID            AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE chrTempDirectory                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOutputDirectory               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDirectoryError                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReturnValue                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReturnError                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileName                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileExportError               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLogFile                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileMasterName                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory              AS CHARACTER NO-UNDO.
DEFINE VARIABLE logEnableDebugging               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID              AS INTEGER   NO-UNDO.

/*Undo Variables*/
DEFINE VARIABLE logTransSuccessful               AS LOGICAL. /*Want this to Undo*/

/* Local Temp-Tables */
DEFINE TEMP-TABLE ttFileExport      NO-UNDO 
   LIKE File.

DEFINE TEMP-TABLE ttFileExportError NO-UNDO 
   LIKE FileUploadError.
   
/*Session Variables*/
DEFINE VARIABLE chrSsnFileMasterName             AS sessionValue NO-UNDO.

/* DataSet Temp-Table Structures */
DEFINE TEMP-TABLE ttMecuryGate XML-NODE-NAME "MecuryGate"
   FIELD specVersion         AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "1.0"
   FIELD OrderStatusUpdateID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX OrderStatusUpdateID IS UNIQUE OrderStatusUpdateID.

DEFINE TEMP-TABLE ttMessageHeader XML-NODE-NAME "Header"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD Action              AS CHARACTER XML-NODE-NAME "Action" INITIAL "Add"
   FIELD DocTypeID           AS CHARACTER XML-NODE-NAME "DocTypeID" INITIAL "Status".
  
DEFINE TEMP-TABLE ttDateTimeCrap XML-NODE-NAME "Date"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD type                AS CHARACTER XML-NODE-TYPE "Attribute"  INITIAL "created"
   FIELD ElementValue        AS CHARACTER XML-NODE-TYPE "Text".
   
DEFINE TEMP-TABLE ttStatus XML-NODE-NAME "Status"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD carrierSCAC         AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD proNumber           AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD shipmentId          AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "".

DEFINE TEMP-TABLE ttStatusDetails XML-NODE-NAME "StatusDetails"
   FIELD OrderStatusUpdateID    AS INTEGER   XML-NODE-TYPE "Hidden".
   
DEFINE TEMP-TABLE ttStatusDetail XML-NODE-NAME "StatusDetail"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD countryCode         AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "NL"
   FIELD stopNum             AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "1"
   FIELD equipNumCheckDigit  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD equipDescCode       AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD equipNum            AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD scacCode            AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" /* Returned from Extract Message - Chris */
   FIELD stateCode           AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" 
   FIELD cityName            AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Tilburg"
   FIELD address             AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Athenastraat 6-10"
   FIELD timeDetails         AS CHARACTER XML-NODE-TYPE "Attribute" XML-NODE-NAME "time" INITIAL ""
   FIELD date                AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" /* Current Date */
   FIELD apptReasonCode      AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD apptCode            AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD statusReasonCode    AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD statusCode          AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" 
   FIELD indexDetails        AS CHARACTER XML-NODE-TYPE "Attribute" XML-NODE-NAME "index" INITIAL "1"
   FIELD ElementValue        AS CHARACTER XML-NODE-TYPE "Text" INITIAL "".

DEFINE TEMP-TABLE ttReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden".
   
DEFINE TEMP-TABLE ttReferenceNum XML-NODE-NAME "ReferenceNumbers"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD type                AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "SyncreonOrderNo"
   FIELD ElementValue        AS CHARACTER XML-NODE-TYPE "Text".

DEFINE TEMP-TABLE ttLocations XML-NODE-NAME "Locations"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden".
   
DEFINE TEMP-TABLE ttLocation XML-NODE-NAME "Location"
   FIELD OrderStatusUpdateID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD type                AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "DistributionCentre"
   FIELD countryCode         AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "NL"
   FIELD postalCode          AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "5047 RK"
   FIELD state               AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD city                AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Tilburg"
   FIELD addr2               AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL ""
   FIELD addr1               AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Athenastraat 6-10"
   FIELD name                AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "c/o Syncreon"
   FIELD ElementValue        AS CHARACTER XML-NODE-TYPE "Text".
   
/* Define Dataset with child-parent relationship */
DEFINE DATASET OrderStatusUpdate 
   XML-NODE-TYPE "Hidden" FOR ttMecuryGate, ttMessageHeader, ttDateTimeCrap, ttStatus, ttReferenceNum, ttReferenceNumbers, 
   ttLocation, ttLocations, ttStatusDetails, ttStatusDetail
   DATA-RELATION ttMessageHeader FOR ttMecuryGate, ttMessageHeader NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttDateTimeCrap FOR ttMessageHeader, ttDateTimeCrap NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttStatus FOR ttMecuryGate, ttStatus NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttReferenceNumbers FOR ttStatus, ttReferenceNumbers NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttReferenceNum FOR ttReferenceNumbers, ttReferenceNum NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttLocations FOR ttStatus, ttLocations NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttLocation FOR ttLocations, ttLocation NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttStatusDetails FOR ttStatus, ttStatusDetails NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID)
   DATA-RELATION ttStatusDetail FOR ttStatusDetails, ttStatusDetail NESTED
      RELATION-FIELDS(OrderStatusUpdateID, OrderStatusUpdateID).   

chrCurrentTime = REPLACE(STRING(TIME, "HH:MM"),":", "").
chrCurrentDate = STRING(fDisplayDateWithFormat(TODAY, "yyyymmdd","")).
            
/* BUFFERS */ 
DEFINE BUFFER updShipOrder           FOR ShipOrder.
DEFINE BUFFER shippedShipOrderStatus FOR ShipOrderStatus.
DEFINE BUFFER readShipOrder          FOR ShipOrder.
DEFINE BUFFER updOutbound            FOR Outbound.
DEFINE BUFFER readOutboundStatus     FOR OutboundStatus.

/* Streams */
DEFINE STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

/*Paths*/
FIND FIRST FileMaster NO-LOCK /*idx=FileMasterID*/
   WHERE FileMaster.MasterName = "TmsShippedStatusUpdate" NO-ERROR.
IF NOT AVAILABLE FileMaster THEN 
DO:
   chrReturnError = "FileMaster: TmsShippedStatusUpdate does not exist!".
   RETURN ERROR chrReturnError.  
END. /* IF AVAILABLE FileMaster THEN */

FIND FIRST FileType NO-LOCK /*idx=FileMasterID*/
   WHERE FileType.FileTypeID = FileMaster.FileTypeID NO-ERROR.
IF NOT AVAILABLE FileType THEN 
DO:
   chrReturnError = "FileTypeID: " + STRING(FileMaster.FileTypeID) + " does not exist!".
   RETURN ERROR chrReturnError.  
END. /* IF AVAILABLE FileMaster THEN */

/* This is name of FileMaster that we're wanting to run the import for - is set as the Session parameter by Cron Run that calls this prog */
ASSIGN chrFileMasterName = FileMaster.MasterName NO-ERROR.

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

chrLogFile = chrLogFileDirectory + chrFileMasterName + "_filXmlTmsShippedStatusExport_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

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

FIND FIRST OperationType NO-LOCK /*idx=TypeCode*/
   WHERE OperationType.TypeCode = "TmsShippedStatusUpdate".
IF NOT AVAILABLE OperationType THEN
DO:
   RETURN ERROR "NO OperationTypeCode for TmsShippedStatusUpdate".
END. /*IF NOT AVAILABLE OperationType THEN*/

intGblOperationTypeID = OperationType.OperationTypeID. 

FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "TmsShippedtatusUpdate" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

UpdateBlk:
DO:
   
   IF logGblDebugging THEN 
      fLog("Running: filXmlTmsShippedStatusExport.p").

   /* IF debugging is disabled but FileMaster debugging is on allow debugging to write */
   IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE THEN 
   DO:
      logGblDebugging = TRUE.
      OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
      fLog("Debugging enabled by FileMaster.DebuggingOn").
   END. /* IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE */

   ASSIGN chrTempDirectory   = FileMaster.FilePath + "tmp/"
          chrOutputDirectory = FileMaster.FilePath.
   
   chrDirectoryError = fValidWriteDirectory(chrTempDirectory).
   IF chrDirectoryError <> "OK" THEN
   DO:
      chrReturnError = "Invalid Directory:'" + chrTempDirectory + "' " + chrDirectoryError.
      RETURN ERROR chrReturnError.
   END. /* IF chrDirectoryError <> "OK" THEN */
   
   chrDirectoryError = fValidWriteDirectory(chrOutputDirectory).
   IF chrDirectoryError <> "OK" THEN
   DO:
      chrReturnError = "Invalid Directory:'" + chrTempDirectory + "' " + chrDirectoryError.
      RETURN ERROR chrReturnError.
   END. /* IF chrDirectoryError <> "OK" THEN */
   
   FIND FIRST shippedShipOrderStatus NO-LOCK /*idx=StatusCode*/
      WHERE shippedShipOrderStatus.StatusCode = "ShipConfirmed" NO-ERROR.
   IF NOT AVAILABLE shippedShipOrderStatus THEN
   DO:
      chrReturnError = "ShipOrderStatus: ShipConfirmed, does not exist!".
      RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE ShipOrderStatus */
   
   FIND FIRST readOutboundStatus NO-LOCK /*idx=StatusCode*/
      WHERE readOutboundStatus.StatusCode = "ShipConfirmed" NO-ERROR.
   IF NOT AVAILABLE readOutboundStatus THEN
   DO:
      chrReturnError = "OutboundStatus: ShipConfirmed, does not exist!".
      RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE readOutboundStatus */
   
   /* Set int Values */
   ASSIGN intB2CShipOrderTypeID = fGetTypeID("ShipOrder", "B2C"). 
          
   IF intB2CShipOrderTypeID = 0 THEN
   DO:
      chrReturnError = "Unable to Find ShipOrderType for B2C".
      RETURN ERROR chrReturnError.
   END. /* IF intB2CShipOrderTypeID = 0 THEN */
    
   /* Storing timestamp in the format for MessageHeader */
   chrTimeStampTrimmed = fXmlTimestamp(NOW).
   chrTimeStampTrimmed = SUBSTRING(chrTimeStampTrimmed,1,19).
   chrTimeStampTrimmed = chrTimeStampTrimmed + "Z".
    
   /* Must Be Specific ShipOrderType */
   OrderLoop:
   FOR EACH ShipOrder NO-LOCK /*idx=ShipOrderTypeId*/
      WHERE ShipOrder.ShipOrderTypeID      = intB2CShipOrderTypeID
      AND   ShipOrder.CarrierShipConfirmed = ""
      AND   ShipOrder.ShipConfirmed       <> "":
             
      logTransSuccessful = FALSE.
      
      OrderBlk:
      DO TRANS ON ERROR UNDO, LEAVE:

         CREATE ttFileExport.
         ASSIGN ttFileExport.Completed     = ""
                ttFileExport.Created       = fTimestamp(NOW)
                ttFileExport.FileID        = NEXT-VALUE(File)
                ttFileExport.FileMasterID  = FileMaster.FileMasterID
                ttFileExport.FileName      = FileMaster.FilePrefix + STRING(ShipOrder.OrderRef) + "_" 
                                             + fDisplayDate&Time(fTimestamp(NOW),"ymdHMS") + ".xml"
                ttFileExport.FilePath      = chrTempDirectory + ttFileExport.FileName
                ttFileExport.FileTypeID    = FileType.FileTypeID
                ttFileExport.GateUserID    = GateUser.GateUserID
                ttFileExport.Sent          = "".                   
         
         FIND FIRST CustShipOrder NO-LOCK /*idx=CustShipOrderID*/
            WHERE CustShipOrder.CustShipOrderID = ShipOrder.ShipOrderID NO-ERROR.
         IF NOT AVAILABLE CustShipOrder THEN
         DO:
            RUN pCreateFileError("CustShipOrder does not exist with OrderRef: " + STRING(ShipOrder.OrderRef)).
            UNDO OrderBlk, LEAVE OrderBlk.
         END. /* End of  IF NOT AVAILABLE CustShipOrder */
          
         FIND FIRST ShipOrderStatus NO-LOCK /*idx=ShipOrderStatusID*/
            WHERE ShipOrderStatus.ShipOrderStatusID = ShipOrder.ShipOrderStatusID NO-ERROR.
         IF NOT AVAILABLE ShipOrderStatus THEN
         DO:
            RUN pCreateFileError("ShipOrderStatus does not exist with StatusID: " + STRING(ShipOrder.ShipOrderStatusID)).
            UNDO OrderBlk, LEAVE OrderBlk.         
         END. /* End of  IF NOT AVAILABLE CustShipOrder */
         
         FIND FIRST Carrier NO-LOCK /*idx=CarrierID*/
            WHERE Carrier.CarrierID = ShipOrder.CarrierID NO-ERROR.
         IF NOT AVAILABLE Carrier THEN
         DO:
            RUN pCreateFileError("Carrier does not exist for OrderRef: " + STRING(ShipOrder.OrderRef)).
            UNDO OrderBlk, LEAVE OrderBlk.
         END. /* IF NOT AVAILABLE Carrier THEN */
         
         FIND FIRST ShipPackage NO-LOCK /*idx=ShipOrderID*/
            WHERE ShipPackage.ShipOrderID = ShipOrder.ShipOrderID NO-ERROR.
         IF NOT AVAILABLE ShipPackage THEN
         DO:
            RUN pCreateFileError("ShipPackage does not exist for OrderRef: " + STRING(ShipOrder.OrderRef)).
            UNDO OrderBlk, LEAVE OrderBlk.
         END. /* IF NOT AVAILABLE ShipPackage */
         
         FIND FIRST Outbound NO-LOCK /*idx=OutboundID*/
            WHERE Outbound.OutboundID = ShipPackage.OutboundID NO-ERROR.
         IF NOT AVAILABLE Outbound THEN
         DO:
            RUN pCreateFileError("Outbound does not exist for Outbound ID: " + STRING(ShipPackage.OutboundID)).
            UNDO OrderBlk, LEAVE OrderBlk.
         END. /* IF NOT AVAILABLE Outbound */
         
         /* Genereate-Fill Temp-Tables */
         CREATE ttMecuryGate.
         ASSIGN ttMecuryGate.OrderStatusUpdateID    = OrderStatusesIDCounter.   
         
         CREATE ttMessageHeader.
         ASSIGN ttMessageHeader.OrderStatusUpdateID = OrderStatusesIDCounter.
   
         CREATE ttDateTimeCrap.
         ASSIGN ttDateTimeCrap.OrderStatusUpdateID  = OrderStatusesIDCounter
                ttDateTimeCrap.ElementValue         = chrTimeStampTrimmed.
                
         CREATE ttStatusDetails.
         ASSIGN ttStatusDetails.OrderStatusUpdateID = OrderStatusesIDCounter.
             
         CREATE ttStatusDetail.
         ASSIGN ttStatusDetail.OrderStatusUpdateID  = OrderStatusesIDCounter
                ttStatusDetail.scacCode             = STRING(Carrier.SCACCode)
                ttStatusDetail.timeDetail           = chrCurrentTime
                ttStatusDetail.date                 = chrCurrentDate. 
            
         ASSIGN ttStatusDetail.statusReasonCode = "ORDER DISPATCHED" 
                ttStatusDetail.statusCode       = "ORDP".
                   
         /* FIND Find/Update ShipOrder */               
         FIND updShipOrder EXCLUSIVE-LOCK /*idx=ShipOrderID*/
            WHERE ROWID(updShipOrder) = ROWID(ShipOrder) NO-ERROR NO-WAIT.
         IF AVAILABLE updShipOrder THEN
         DO ON ERROR UNDO OrderBlk, LEAVE OrderBlk:
            
            ASSIGN updShipOrder.CarrierShipConfirmed = fTimestamp(NOW).
         END. /* IF AVAILABLE updShipORder THEN */
         ELSE    
         DO:
            RUN pCreateFileError(fGetLockingInfo(INTEGER(RECID(ShipOrder)),
                                 "ShipOrder",
                                 STRING(ShipOrder.OrderRef)) + " - please try again!").
            UNDO OrderBlk, LEAVE OrderBlk.
         END. /* ELSE */
             
         CREATE ttStatus.
         ASSIGN ttStatus.OrderStatusUpdateID = OrderStatusesIDCounter
                ttStatus.carrierSCAC         = STRING(Carrier.SCACCode)
                ttStatus.ProNumber           = STRING(ShipOrder.ProNumber)
                ttStatus.shipmentId          = STRING(ShipPackage.ShipPackageID).
                
         CREATE ttReferenceNumbers.
                ttReferenceNumbers.OrderStatusUpdateID = OrderStatusesIDCounter.
   
         CREATE ttReferenceNum.
         ASSIGN ttReferenceNum.OrderStatusUpdateID = OrderStatusesIDCounter
                ttReferenceNum.ElementValue        = STRING(ShipOrder.OrderRef).
         
         CREATE ttLocations.
                ttLocations.OrderStatusUpdateID = OrderStatusesIDCounter.
                
         CREATE ttLocation.
         ASSIGN ttLocation.OrderStatusUpdateID  = OrderStatusesIDCounter.
         
         DATASET OrderStatusUpdate:WRITE-XML("FILE", ttFileExport.FilePath).
         DATASET OrderStatusUpdate:EMPTY-DATASET().
         OrderStatusesIDCounter = OrderStatusesIDCounter + 1.
         
         logTransSuccessful = TRUE.
         
         RUN pEmptyMessageTT.
         
      END. /*DO TRANS OrderBlk*/
         
      IF logTransSuccessful <> TRUE THEN
      DO:
         CREATE ttFileExportError.
         ASSIGN ttFileExportError.FileUploadErrorID = NEXT-VALUE(FileUploadError)
                ttFileExportError.FileID            = ttFileExport.FileID
                ttFileExportError.ErrorString       = "TRANSACTION NOT completed. Undoing All.".
      END.
      
      IF CAN-FIND(FIRST ttFileExportError) THEN
      DO:
         /* Accumulate the ttFileExportErrors into one string for Emailing */
         chrFileExportError = "".
         FOR EACH ttFileExportError OF ttFileExport:
            
            chrFileExportError = chrFileExportError + ttFileExportError.ErrorString + CHR(13).

         END. /* FOR EACH ttFileExportError */
         
         RUN osSendMail.p (INPUT chrEmailAddress,                              /* Optional list of Users */
                           INPUT ttFileExport.FileName + " File Export Error", /* Email Subject */
                           INPUT chrFileExportError + " from Cron.",           /* Plain text message Body */
                           INPUT "",                                           /* Html format message Body */
                           INPUT "",                                           /* File path ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     FileMaster.InternalEmailGroupID
                                  ELSE 0),                                     /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                     /* File MasterID is it applies */
         
         chrFileExportError = "".
         
         RUN pEmptyErrorTempTables.
         
         NEXT OrderLoop.
               
      END. /*IF CAN-FIND(FIRST ttFileExportError) THEN*/
      
      IF SEARCH(ttFileExport.FilePath) <> ? THEN
      DO:
         /* If successfully completed then move from "tmp" folder back to outray for sending */
         chrReturnValue = fMoveFile(ttFileExport.FilePath, /* Source full file path */
                                    FileMaster.FilePath,   /* Target directory */
                                    ttFileExport.FileName, /* File Name without path */
                                    0) NO-ERROR.           /* Days to archive if applicable */
      END.
      
      IF logGblDebugging THEN
         fLog("Move from: " + ttFileExport.FilePath  + " to dir: " + FileMaster.FilePath + 
              " FILE: " + ttFileExport.FileName + " chrReturnValue: " + chrReturnValue).
               
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
                           INPUT  FileMaster.FileMasterID).                  /* File MasterID is it applies */
         
         IF logGblDebugging THEN
            fLog(chrReturnValue).

      END. /* IF chrReturnValue BEGINS "Error" */
      
      RUN pEmptyErrorTempTables.
     
   END. /* FOR EACH ShipOrder NO-LOCK */
   
   UpdateOutboundLoop:
   FOR EACH Outbound 
      WHERE Outbound.ShipConfirmed = "" NO-LOCK 
      ON ERROR UNDO UpdateOutboundLoop, NEXT UpdateOutboundLoop: /*idx=OutboundID*/
      
      FOR EACH ShipPackage OF Outbound NO-LOCK /*idx=OutBoundID*/
         BREAK BY ShipPackage.OutboundID: 
           
         FIND FIRST ShipOrder OF ShipPackage NO-LOCK NO-ERROR. /*idx=ShipOrderID*/
         IF ShipOrder.ShipConfirmed = "" THEN
            NEXT UpdateOutboundLoop.
          
         IF LAST-OF(ShipPackage.OutboundID) THEN
         DO:    
            FIND FIRST updOutbound EXCLUSIVE-LOCK 
               WHERE updOutbound.OutboundID = Outbound.OutboundID NO-ERROR NO-WAIT.
                       
            IF AVAILABLE updOutBound THEN
               ASSIGN updOutbound.CarrierShipConfirmed = fTimestamp(NOW)
                      updOutbound.ShipConfirmed        = fTimestamp(NOW)
                      updOutbound.OutboundStatusID     = readOutboundStatus.OutboundStatusID.
            ELSE
               NEXT UpdateOutboundLoop.
               
            RELEASE updOutbound.   
         END. /*IF LAST-OF(ShipPackage) THEN*/           
      END. /* FOR EACH ShipPackage WHERE ShipPackage.OutboundId NO-LOCK */
   END. /* FOR Each updOutbound WHERE updOutbound.ShipConfirmed = "" NO-LOCK*/

END. /* End of UpdateBlk */  

IF logGblDebugging THEN 
   fLog(FileMaster.ProgramName + " finished").

IF logGblDebugging THEN
DO:
   OUTPUT STREAM sToLogFile CLOSE.
END. /* IF logGblDebugging */

DELETE OBJECT chrSsnFileMasterName NO-ERROR.

RELEASE FileMaster                 NO-ERROR.
RELEASE FileType                   NO-ERROR.
RELEASE File                       NO-ERROR.
RELEASE EmailGroup                 NO-ERROR.
RELEASE ShipOrder                  NO-ERROR.

/**********************************************************************************************/      
/**************************************  - PROCEDURES -  **************************************/
/**********************************************************************************************/   
PROCEDURE pEmptyErrorTempTables:
   
   EMPTY TEMP-TABLE ttFileExportError NO-ERROR.
   EMPTY TEMP-TABLE ttFileExport NO-ERROR.
END. /*PROCEDURE pEmptyErrorTempTables*/      

PROCEDURE pEmptyMessageTT:
   
   EMPTY TEMP-TABLE ttMecuryGate NO-ERROR.
   EMPTY TEMP-TABLE ttMessageHeader NO-ERROR.
   EMPTY TEMP-TABLE ttReferenceNum NO-ERROR.
   EMPTY TEMP-TABLE ttReferenceNumbers NO-ERROR.
   EMPTY TEMP-TABLE ttStatus NO-ERROR.
   EMPTY TEMP-TABLE ttStatusDetail NO-ERROR.
   EMPTY TEMP-TABLE ttStatusDetail NO-ERROR.
   EMPTY TEMP-TABLE ttLocations NO-ERROR.
   EMPTY TEMP-TABLE ttLocation NO-ERROR.
END. /*PROCEDURE pEmptyMessageTT*/
         
PROCEDURE pCreateFileError:
   
   DEFINE INPUT PARAMETER chrErrorString AS CHARACTER NO-UNDO.
   
   CREATE ttFileExportError.
   ASSIGN ttFileExportError.FileUploadErrorID = NEXT-VALUE(FileUploadError)
          ttFileExportError.FileID            = ttFileExport.FileID
          ttFileExportError.ErrorString       = chrErrorString.
   
END PROCEDURE. /*PROCEDURE pCreateFileError*/
   