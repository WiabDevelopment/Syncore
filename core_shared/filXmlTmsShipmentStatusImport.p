/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filXmlTmsShipmentStatusImport.p
Purpose :  

Author  : Nick Diessner
Date    : 20th July 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/
/* Cron Output Parameter */
/*DEFINE OUTPUT PARAMETER chrReturnError AS CHARACTER NO-UNDO.*/

/* Includes */
{defSessionVariables.i GLOBAL NEW}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncStatusTypeFunctions.i} 
{fncStockFunctions.i}
{fncServerFunctions.i}  
{fncLockingFunctions.i}

/* Shared Temp Tables */
/*DEFINE SHARED TEMP-TABLE ttFileExport      NO-UNDO LIKE File.           */
/*DEFINE SHARED TEMP-TABLE ttFileExportError NO-UNDO LIKE FileUploadError.*/

/* Create a Shared Temp Table for FileUploadErrors to return any File Upload errors to the calling program */
DEFINE SHARED TEMP-TABLE ttFileUploadError NO-UNDO
   LIKE FileUploadError.
   
/* Local Temp Tables */
DEFINE TEMP-TABLE ttFile NO-UNDO 
   LIKE File.

/* Shared Vars */
DEFINE SHARED VARIABLE chrGblFilePath       AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE chrGblFileName       AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE intGblFileTypeID     AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE intGblFileMasterID   AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE intGblNewFileID      AS INTEGER   NO-UNDO.


/* DataSet Temp-Table Structures */
DEFINE TEMP-TABLE ttMercuryGate XML-NODE-NAME "MercuryGate"
   FIELD MercuryGateID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX MercuryGateID IS UNIQUE MercuryGateID.


DEFINE TEMP-TABLE ttShipmentStatus XML-NODE-NAME "ShipmentStatus"
   FIELD ShipmentStatusID     AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD MercuryGateID        AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX MercuryGateID MercuryGateID.
   
DEFINE TEMP-TABLE ttReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD ReferenceNumbersID   AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ShipmentsID          AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentsID ShipmentsID.
   
   
DEFINE TEMP-TABLE ttReferenceNumber XML-NODE-NAME "ReferenceNumber"
   FIELD ReferenceNumberID    AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ReferenceNumbersID   AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ReferenceNumberType  AS CHARACTER XML-NODE-TYPE "Attribute" XML-NODE-NAME "type"
   FIELD isPrimary            AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD ReferenceNumber      AS CHARACTER XML-NODE-TYPE "text"
   INDEX ReferenceNumbersID ReferenceNumbersID.
   
   
DEFINE TEMP-TABLE ttStatusDetails XML-NODE-NAME "StatusDetails"
   FIELD StatusDetailsID     AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ShipmentStatusID    AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentStatusID ShipmentStatusID.
   
DEFINE TEMP-TABLE ttStatusDetail XML-NODE-NAME "StatusDetail"
   FIELD StatusDetailID                AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD StatusDetailsID               AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD StatusCode                    AS CHARACTER
   FIELD ShipmentStatusReason          AS CHARACTER 
   FIELD ShipmentAppointmentStatusCode AS CHARACTER
   FIELD ShipmentApointmentReason      AS CHARACTER 
   INDEX StatusDetailsID StatusDetailsID.
   
   
DEFINE TEMP-TABLE ttEvent XML-NODE-NAME "Event"
   FIELD EventID            AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD StatusDetailID     AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD EventType          AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD internalId         AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD CarrierActionCode  AS CHARACTER
   FIELD LateReason         AS CHARACTER 
   FIELD AppointmentType    AS CHARACTER
   FIELD Activities         AS CHARACTER
   INDEX StatusDetailID StatusDetailID.
   
DEFINE TEMP-TABLE ttStatusDate XML-NODE-NAME "Date"
   FIELD DateID           AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD StatusDetailID   AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD DateType         AS CHARACTER XML-NODE-TYPE "Attribute" XML-NODE-NAME "type"
   FIELD DateInfo         AS CHARACTER XML-NODE-TYPE "text" 
   INDEX StatusDetailID StatusDetailID.

DEFINE TEMP-TABLE ttShipments XML-NODE-NAME "Shipments"
   FIELD ShipmentsID   AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD EventID       AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX EventID EventID.
   
/* Define Dataset with child-parent relationship */
DEFINE DATASET MercuryReportMessage XML-NODE-TYPE "Hidden" FOR ttMercuryGate, ttShipmentStatus, ttStatusDetails, ttStatusDetail, 
                                                               ttEvent, ttShipments, ttReferenceNumbers, ttReferenceNumber, ttStatusDate

/************************* Beginning headers **********************************/
   DATA-RELATION MercuryGateShipmentStatusDR FOR ttMercuryGate, ttShipmentStatus NESTED
      RELATION-FIELDS(MercuryGateID,MercuryGateID)
      
/********************* Shipment Status Starts Here****************************/   
   DATA-RELATION ShipmentStatusStatusDetailsDR FOR ttShipmentStatus, ttStatusDetails NESTED
      RELATION-FIELDS(ShipmentStatusID, ShipmentStatusID)   
      
/******************* Status Details Starts Here ****************************/
   DATA-RELATION StatusDetailsStatusDetailDR FOR ttStatusDetails, ttStatusDetail NESTED
      RELATION-FIELDS(StatusDetailsID, StatusDetailsID)
   DATA-RELATION StatusDetailDateDR FOR ttStatusDetail, ttStatusDate NESTED
      RELATION-FIELDS(StatusDetailID, StatusDetailID)
   DATA-RELATION StatusDetailEventDR FOR ttStatusDetail, ttEvent NESTED
      RELATION-FIELDS(StatusDetailID, StatusDetailID)
      
/******************* Event Starts Here ************************************/
   DATA-RELATION EventShipmentsDR FOR ttEvent, ttShipments NESTED
      RELATION-FIELDS(EventID, EventID)
      DATA-RELATION ReferenceNumbersDR FOR ttShipments, ttReferenceNumbers NESTED
         RELATION-FIELDS(ShipmentsID, ShipmentsID)
         DATA-RELATION ReferenceNumbersNumberDR FOR ttReferenceNumbers, ttReferenceNumber NESTED
            RELATION-FIELDS(ReferenceNumbersID, ReferenceNumbersID).
      
/* LOCAL VARIABLES */
DEFINE VARIABLE logErrorFound                  AS LOGICAL   NO-UNDO INITIAL FALSE. 
DEFINE VARIABLE intUploadErrorGroup            AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrDebugString                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReturnError                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrErrorList                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrErrorDetails                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTempDirectory               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOutputDirectory             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrMessageDateStamp            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrMessageTimeStamp            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTodayTimeStamp              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDeliveryTimestamp           AS CHARACTER NO-UNDO.
DEFINE VARIABLE intCarrierShipOrderStatusID    AS INTEGER   NO-UNDO.
DEFINE VARIABLE intFailedShipOrderStatusID     AS INTEGER   NO-UNDO.
DEFINE VARIABLE intDeliveredShipOrderStatusID  AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrDeliveryDay                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDeliveryMonth               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDeliveryYear                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDeliveryHour                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDeliveryMinutes             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDeliverySeconds             AS CHARACTER NO-UNDO INITIAL "000".  
DEFINE VARIABLE intFileCompletedStatus         AS INTEGER   NO-UNDO.   
DEFINE VARIABLE logDataGapError                AS LOGICAL   NO-UNDO.       


/* BUFFERS */ 
DEFINE BUFFER updShipOrder FOR ShipOrder.
DEFINE BUFFER failedShipOrderStatus FOR ShipOrderStatus.
DEFINE BUFFER deliveredShipOrderStatus FOR ShipOrderStatus.



/* Get the Web Service User and if NOT available assign to DC */
FIND FIRST GateUser NO-LOCK 
   WHERE GateUser.Username = "WebServ" NO-ERROR.
IF NOT AVAILABLE GateUser THEN 
DO:
   chrErrorDetails = "GateUser with Username: WebServ does not exist!".
   logErrorFound = TRUE.
END.  
   
{usrCreateSession.i}   

Main_Block:
DO ON ERROR UNDO, RETURN ERROR:
    
   intGblOperationTypeID = fGetTypeID("Operation", "Web Service"). /* What Operation Type Should be Used? */
   
   chrMessageDateStamp = fDisplayDateWithFormat(INPUT TODAY, 
                                                INPUT "YYYYMMDD",
                                                INPUT "-" ).
                                                
   chrTodayTimeStamp = fTimeStamp(NOW).
   
   chrMessageTimeStamp = fTime(INPUT chrTodayTimeStamp).
   
   /* Retrieving FileMaster Record for this program */
   FIND FIRST FileMaster NO-LOCK 
      WHERE FileMaster.FileMasterID = intGblFileMasterID NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN 
   DO:
      chrErrorDetails = "FileMaster with GblFileMasterID: " + STRING(intGblFileMasterID) + " is not available!".
      logErrorFound = TRUE.
      LEAVE Main_Block.
   END.
   
   ASSIGN intFileCompletedStatus  = fGetStatusID("File", "Complete").

   IF intFileCompletedStatus = 0 THEN
      RETURN "No FileStatus exists for Code: Complete".
   
   /* Check file has not been completed */
   FIND FIRST File NO-LOCK /* idx=FileName */
      WHERE File.FileName     = chrGblFileName
      AND   File.FileStatusID = intFileCompletedStatus NO-ERROR.
   IF AVAILABLE File THEN
   DO:
      chrReturnError = "A File of this Name: " + chrGblFileName + " has already been Completed. Aborting File Upload.".
   
      RELEASE File NO-ERROR. 
   
      RETURN chrReturnError.
   END. /*IF AVAILABLE File THEN*/
   
   FIND FileType OF FileMaster NO-LOCK.
   IF NOT AVAILABLE FileType THEN 
   DO:
      chrErrorDetails = "FileType with GblFileMasterID: " + STRING(intGblFileMasterID) + " is not available!".
      logErrorFound = TRUE.
      LEAVE Main_Block.
   END.
   IF AVAILABLE FileMaster THEN 
   DO:
      FIND FileType OF FileMaster NO-LOCK.
      
      ASSIGN chrTempDirectory   = FileMaster.FilePath
             chrOutputDirectory = FileMaster.FilePath.
   END.
  
   /* Could be running the file import for a second time having closed a data gap */
   FIND FIRST File NO-LOCK /* idx=FileName */
        WHERE File.FileName = chrGblFileName NO-ERROR.
   IF AVAILABLE File THEN
   DO:
      /* If file was reprocessed then get the existing data */
      CREATE ttFile.
      BUFFER-COPY File TO ttFile.
   END. /*IF AVAILABLE File THEN*/
   ELSE 
   DO:
      /* New file so create the new record */
      CREATE ttFile.
      ASSIGN ttFile.Completed       = ""
             ttFile.Created         = fTimestamp(NOW)
             ttFile.FileID          = NEXT-VALUE(File)
             ttFile.BusinessUnitID  = intGblBusinessUnitID
             ttFile.FileMasterID    = intGblFileMasterID
             ttFile.FileName        = chrGblFileName
             ttFile.FilePath        = chrGblFilePath
             ttFile.FileTypeID      = intGblFileTypeID
             ttFile.GateUserID      = intGblUserID
             ttFile.Sent            = "".
   END. /*IF NOT AVAILABLE File THEN*/
  
   /* This below code will be added in if there are ShipOrderStatuses added for Deliveries */
   FIND FIRST failedShipOrderStatus NO-LOCK /*idx=StatusCode*/
      WHERE failedShipOrderStatus.StatusCode = "DeliveryFailed" NO-ERROR.

   IF NOT AVAILABLE failedShipOrderStatus THEN
   DO:
      chrErrorList = chrErrorList + " No ShipOrderStatus for StatusCode: DeliveryFailed.".
      logErrorFound = TRUE.
   END.

   FIND FIRST deliveredShipOrderStatus NO-LOCK /*idx=StatusCode*/
      WHERE deliveredShipOrderStatus.StatusCode = "DeliverySuccess" NO-ERROR.

   IF NOT AVAILABLE deliveredShipOrderStatus THEN
   DO:
      chrErrorList = chrErrorList + " No ShipOrderStatus for StatusCode: Delivery.".
      logErrorFound = TRUE.
   END.

   intFailedShipOrderStatusID = failedShipOrderStatus.ShipOrderStatusID.
   intDeliveredShipOrderStatusID = deliveredShipOrderStatus.ShipOrderStatusID.
  
   
   /* Set the Email Group */
   /* intUploadErrorGroup = FileMaster.InternalEmailGroupID. */
   
   /********************************************************/
   /* Check all fields that we going to use, are available */
   /********************************************************/

   DATASET MercuryReportMessage:READ-XML("file", chrGblFilePath, "empty",  ?, FALSE, ?, ?).

   FIND FIRST ttShipmentStatus NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE ttShipmentStatus THEN
   DO:
      chrErrorList = chrErrorList + " No records in Shipment Status file.".
      logErrorFound = TRUE.
   END.
   
   FIND FIRST ttStatusDetail NO-LOCK NO-ERROR.
   
   /* Check to see whether the StatusCode is for Delivered or Failed */
   FIND FIRST CarrierDeliveryStatus NO-LOCK
      WHERE CarrierDeliveryStatus.DeliveryCode = ttStatusDetail.StatusCode NO-ERROR.      
   IF NOT AVAILABLE CarrierDeliveryStatus THEN
   DO:
      /* This could be an internal Error due to a data gap so we create create an internal Alert for it */
      chrReturnError = "Carrier Delivery Code: " + ttStatusDetail.StatusCode + " was not found in the system for an active Delivery Code. ".
      
      RUN pCreateFileUploadError(INPUT  "StockStatus",                 /* Table Name */
                                 INPUT  "StockStatus.StatusName",      /* Field Name */
                                 INPUT  ttStatusDetail.StatusCode,     /* Field Value */
                                 INPUT  chrReturnError).               /* Error */
                                 
      /* Not sure if this can be approved but works to place the file in pending folder if the datagap is hit. */
      RETURN "Pending".
   END.
   
   /* This delivery was failed */
   IF CarrierDeliveryStatus.FailedDelivery THEN
      intCarrierShipOrderStatusID = intFailedShipOrderStatusID.
   
   /* This delivery was a success */
   IF CarrierDeliveryStatus.FailedDelivery <> TRUE THEN
      intCarrierShipOrderStatusID = intDeliveredShipOrderStatusID.

   /* Update the ShipOrder records with the DeliveryConfirmed date, CarrierDeliveryStatus, and possibly update the ShipOrderStatus */
   FIND FIRST ttReferenceNumber NO-LOCK
      WHERE ttReferenceNumber.ReferenceNumberType = "SyncreonOrderNo" NO-ERROR.
   IF NOT AVAILABLE ttReferenceNumber THEN
   DO:
      chrErrorList = chrErrorList + "Reference Number Data Type: SyncreonOrderNo is NOT in the Delivered Milestone XML from TMS.".
      logErrorFound = TRUE.
   END.
   ELSE
   DO:
      /* Get the ShipOrder */
      FIND FIRST ShipOrder NO-LOCK
         WHERE ShipOrder.OrderRef = ttReferenceNumber.ReferenceNumber NO-ERROR.
      IF NOT AVAILABLE ShipOrder THEN
      DO:
         chrErrorList = chrErrorList + "Ship Order Ref: " + STRING(ttReferenceNumber.ReferenceNumber) + " in the Delivered Milestone from TMS does NOT Exist.".
         logErrorFound = TRUE.
      END.
   END.   
   
   FIND FIRST ttStatusDate NO-LOCK 
      WHERE ttStatusDate.DateType = "actual" NO-ERROR.
   IF AVAILABLE ttStatusDate THEN
   DO: 
      chrDeliveryDay = SUBSTRING(ttStatusDate.DateInfo, 4, 2).
      chrDeliveryMonth = SUBSTRING(ttStatusDate.DateInfo, 1, 2).
      chrDeliveryYear =  SUBSTRING(ttStatusDate.DateInfo, 7, 4).
      chrDeliveryHour = SUBSTRING(ttStatusDate.DateInfo, 12, 2).
      chrDeliveryMinutes = SUBSTRING(ttStatusDate.DateInfo, 15, 2).
   END.   

   chrDeliveryTimestamp = chrDeliveryYear + chrDeliveryMonth + chrDeliveryDay + chrDeliveryHour + chrDeliveryMinutes + chrDeliverySeconds.
      
   /* Update Database Tables if NO Errors were Found */
   IF logErrorFound = FALSE THEN
   DO:
      Update_Block:
      DO ON ERROR UNDO, RETURN ERROR:
          
         FIND FIRST updShipOrder EXCLUSIVE-LOCK /*idx=ShipOrderID*/
             WHERE updShipOrder.ShipOrderID = ShipOrder.ShipOrderID NO-WAIT NO-ERROR.   
         IF NOT AVAILABLE updShipOrder THEN
         DO:
            IF LOCKED updShipOrder THEN
            DO:
               RUN pSendErrorEmail(INPUT fGetLockingInfo(INTEGER(RECID(ShipOrder)),
                                         "ShipOrder",
                                         STRING(ShipOrder.OrderRef)) + " - please try again!",
                                         GateUser.Email).
               UNDO Update_Block, LEAVE Update_Block.
            END.
    
            chrReturnError = "Unable to Update ShipOrder: " + STRING(ShipOrder.ShipOrderID) + ". Undoing all updates.".
            UNDO Update_Block, LEAVE Update_Block.
          
         END.
    
         /* Change status to ShipConfirmed for ShipOrder */
         ASSIGN updShipOrder.ShipOrderStatusID = intCarrierShipOrderStatusID /* In here just in case we decide to add new Statuses */
                updShipOrder.Delivered = chrDeliveryTimestamp NO-ERROR.
     
         RELEASE updShipOrder.          
         
      END. /* Update_Block */             
   END. /* Error Found FALSE */
   ELSE
   DO:
      RETURN chrErrorList.
   END.
   
   EMPTY TEMP-TABLE ttStatusDate.
   EMPTY TEMP-TABLE ttEvent.
   EMPTY TEMP-TABLE ttReferenceNumber.
   EMPTY TEMP-TABLE ttReferenceNumbers.
   EMPTY TEMP-TABLE ttShipments.
   EMPTY TEMP-TABLE ttShipmentStatus.
   EMPTY TEMP-TABLE ttStatusDetail.
   EMPTY TEMP-TABLE ttStatusDetails. 
   
END. /* End of Main_Block */     

/**********************************************************************************************/      
/**************************************  - PROCEDURES -  **************************************/
/**********************************************************************************************/   

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

/* This will track file and ID that error during validation */ 
PROCEDURE pCreateFileUploadError:
   
   DEFINE INPUT  PARAMETER chrTableName             AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER chrFieldName             AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER chrFieldValue            AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER chrErrorString           AS CHARACTER   NO-UNDO.

    /* Create the temp-table FileUploadError to hold error history for FileImport Process */
   CREATE ttFileUploadError.
   ASSIGN ttFileUploadError.FileUploadErrorID = NEXT-VALUE(FileUploadError)
          ttFileUploadError.FileID            = ttFile.FileID
          ttFileUploadError.TableName         = chrTableName
          ttFileUploadError.FieldName         = chrFieldName
          ttFileUploadError.FieldValue        = chrFieldValue
          ttFileUploadError.ErrorString       = chrErrorString.
          
          
END PROCEDURE. /* pCreateFileUploadError */     
