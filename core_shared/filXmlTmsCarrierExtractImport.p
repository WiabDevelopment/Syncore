/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filXmlTmsCarrierExtractImport.p
Purpose : This is the program for TMS Carrier Extract Import.  This will import the Carrier information for this 
          ShipOrder and ShipPackages.  This includes the SCAC selected, service, mode, load reference, pro number, and tracking numbers.  

Author  : Christopher Shelley
Date    : 10/03/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
31/03/2015 CS  Canon      Added update for ShipOrderLine Status and ShipOrder CarrierSortationID.
06/04/2015 CS  Canon      Added datagap errors to allow for automatic reprocessing.
07/04/2015 CS  Canon      Fully qualify some database fields in the FileUploadError query.
20/04/2015 CS  Canon      Added cutOff time logic.
23/04/2015 CS  Canon      Check if Order is for today and if the current day is a working day.
27/04/2015 CS  Canon      Renamed and moved the program to core_shared.
05/05/2015 CS  Canon      Added logic to bypass all errors and update the file to complete if the ShipOrder is at cancelled and included
                          order refs to error message.
11/05/2015 CS  Canon      Removed ShipOrder.CarrierModeID references.     
25/06/2015 CS  56780      Removed DateToShip logic to calculate off the cutoff time.                          
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i}

/* Functions */
{fncStatusTypeFunctions.i}
/* fGetStatusID()*/
{fncGlobalFunctions.i}
/* fTimestamp() */
{fncDateFunctions.i}
/* fIntHour() */

/* Create a Shared Temp Table for FileUploadErrors to return any File Upload errors to the calling program */
DEFINE SHARED TEMP-TABLE ttFileUploadError NO-UNDO
   LIKE FileUploadError.

/* These variables will hold the File information through the program chain */
/* Shared Variables */
DEFINE SHARED VARIABLE chrGblFilePath       AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE chrGblFileName       AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE intGblFileTypeID     AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE intGblFileMasterID   AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE intGblNewFileID      AS INTEGER   NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrFileMasterName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLoadNumber               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrProNumber                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReturnError              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReturnValue              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOrderRef                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE intFileCompletedStatus      AS INTEGER   NO-UNDO.
DEFINE VARIABLE intShipOrderLabelStatus     AS INTEGER   NO-UNDO.
DEFINE VARIABLE intShipOrderResponseStatus  AS INTEGER   NO-UNDO.
DEFINE VARIABLE intShipOrderCancelledStatus AS INTEGER   NO-UNDO.
DEFINE VARIABLE logDataGapError             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE intShipOrderID              AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrCutOffTime               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTimestamp                AS CHARACTER NO-UNDO.
DEFINE VARIABLE intDayNumber                AS INTEGER   NO-UNDO.

/* Buffer */
DEFINE BUFFER updateShipOrderLine FOR ShipOrderLine.

/* Local Temp Tables */
DEFINE TEMP-TABLE ttFile          NO-UNDO 
   LIKE File.

/* DataSet Temp-Table Structures */
DEFINE TEMP-TABLE ttMercuryGate          XML-NODE-NAME "MercuryGate"
   FIELD MercuryGateID                   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD specVersion                     AS CHARACTER XML-NODE-TYPE "Attribute"
   INDEX MercuryGateID IS UNIQUE MercuryGateID.

DEFINE TEMP-TABLE ttHeader               XML-NODE-NAME "Header"
   FIELD MercuryGateID                   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD SenderID                        AS CHARACTER
   FIELD ReceiverID                      AS CHARACTER
   FIELD DocTypeID                       AS CHARACTER
   FIELD DocCount                        AS CHARACTER.

DEFINE TEMP-TABLE ttDate                 XML-NODE-NAME "Date"
   FIELD MercuryGateID                   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD type                            AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD CreationDate                    AS CHARACTER XML-NODE-TYPE "Text" XML-NODE-NAME "ElementValue".
   
DEFINE TEMP-TABLE ttExtractRequest       XML-NODE-NAME "ExtractRequest"
   FIELD MercuryGateID                   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD Date                            AS CHARACTER
   FIELD Reason                          AS CHARACTER.     
   
DEFINE TEMP-TABLE ttMasterBillOfLading   XML-NODE-NAME "MasterBillOfLading"
   FIELD MercuryGateID                   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD internalID                      AS CHARACTER
   FIELD type                            AS CHARACTER
   INDEX internalID IS UNIQUE internalID.

DEFINE TEMP-TABLE ttReferenceNumbers     XML-NODE-NAME "ReferenceNumbers"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden"
   INDEX internalID IS UNIQUE internalID.
   
DEFINE TEMP-TABLE ttReferenceNumber      XML-NODE-NAME "ReferenceNumber"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden"
   FIELD type                            AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD isPrimary                       AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD ElementValue                    AS CHARACTER XML-NODE-TYPE "Text"
   INDEX type IS PRIMARY type.

DEFINE TEMP-TABLE ttShipments            XML-NODE-NAME "Shipments"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden"
   INDEX internalID IS UNIQUE internalID.
   
DEFINE TEMP-TABLE ttShipment             XML-NODE-NAME "Shipment"
   FIELD internalid                      AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD type                            AS CHARACTER XML-NODE-TYPE "Attribute".

DEFINE TEMP-TABLE ttShipReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden".
   
DEFINE TEMP-TABLE ttShipReferenceNumber  XML-NODE-NAME "ReferenceNumber"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden"
   FIELD type                            AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD isPrimary                       AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD ElementValue                    AS CHARACTER XML-NODE-TYPE "Text".

DEFINE TEMP-TABLE ttPriceSheets          XML-NODE-NAME "PriceSheets"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden".
   
DEFINE TEMP-TABLE ttPriceSheet           XML-NODE-NAME "PriceSheet"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden"
   FIELD chargeModel                     AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD isSelected                      AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD isAllocated                     AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD currencyCode                    AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD createDate                      AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD PriceInternalID                 AS CHARACTER XML-NODE-TYPE "Attribute" XML-NODE-NAME "internalid"
   FIELD SCAC                            AS CHARACTER
   FIELD Mode                            AS CHARACTER
   FIELD Service                         AS CHARACTER
   INDEX isSelected isSelected.
   
DEFINE TEMP-TABLE ttItemGroups           XML-NODE-NAME "ItemGroups"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden".
   
DEFINE TEMP-TABLE ttItemGroup            XML-NODE-NAME "ItemGroup"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden"
   FIELD type                            AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD isShipUnit                      AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD isHandlingUnit                  AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD Description                     AS CHARACTER
   FIELD TrackingNumber                  AS CHARACTER.
   
DEFINE TEMP-TABLE ttItemReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD internalID                      AS CHARACTER XML-NODE-TYPE "Hidden".
   
DEFINE TEMP-TABLE ttItemReferenceNumber  XML-NODE-NAME "ReferenceNumber"
   FIELD internalID                      AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ReferenceNumberID               AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD type                            AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD isPrimary                       AS CHARACTER XML-NODE-TYPE "Attribute"
   FIELD ElementValue                    AS CHARACTER XML-NODE-TYPE "Text".

/* DataSet Data Releations */   
DEFINE DATASET MasterBillOfLading FOR ttMercuryGate, ttHeader, ttDate, ttExtractRequest, ttMasterBillofLading, ttReferenceNumbers, 
                                      ttReferenceNumber, ttShipReferenceNumbers, ttShipReferenceNumber, ttShipment, ttShipments, 
                                      ttPriceSheet, ttPriceSheets, ttItemGroups, ttItemGroup, ttItemReferenceNumbers, ttItemReferenceNumber 
   DATA-RELATION ttHeader FOR ttMercuryGate, ttHeader NESTED
      RELATION-FIELDS(MercuryGateID, MercuryGateID)
   DATA-RELATION ttDate FOR ttHeader, ttDate NESTED
      RELATION-FIELDS(MercuryGateID, MercuryGateID)
   DATA-RELATION ttExtractRequest FOR ttHeader, ttExtractRequest NESTED
      RELATION-FIELDS(MercuryGateID, MercuryGateID)
   DATA-RELATION ttMasterBillOfLading FOR ttHeader, ttMasterBillOfLading NESTED
      RELATION-FIELDS(MercuryGateID, MercuryGateID)
   DATA-RELATION ttReferenceNumbers FOR ttMasterBillOfLading, ttReferenceNumbers NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttReferenceNumber FOR ttReferenceNumbers, ttReferenceNumber NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttShipments FOR ttMasterBillOfLading, ttShipments NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttShipment FOR ttShipments, ttShipment NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttShipReferenceNumbers FOR ttShipment, ttShipReferenceNumbers NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttShipReferenceNumber FOR ttShipReferenceNumbers, ttShipReferenceNumber NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttPriceSheets FOR ttMasterBillOfLading, ttPriceSheets NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttPriceSheet FOR ttPriceSheets, ttPriceSheet NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttItemGroups FOR ttMasterBillOfLading, ttItemGroups NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttItemGroup FOR ttItemGroups, ttItemGroup NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttItemReferenceNumbers FOR ttItemGroup, ttItemReferenceNumbers NESTED  
      RELATION-FIELDS(internalID, internalID)
   DATA-RELATION ttItemReferenceNumber FOR ttItemReferenceNumbers, ttItemReferenceNumber NESTED  
      RELATION-FIELDS(internalID, internalID).
      
Main_Block:
DO:
   /* Set the Global OperationType */
   FIND FIRST OperationType NO-LOCK /* idx=TypeCode */
      WHERE OperationType.TypeCode = "CarrierExtract" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN 
   DO:
      chrReturnError = "No OperationType exists for Code: CarrierExtract".
   
      RETURN chrReturnError.
   END. /*IF NOT AVAILABLE OperationType THEN*/
   
   intGblOperationTypeID = OperationType.OperationTypeID.
   
   RELEASE OperationType NO-ERROR.
   
   FIND FIRST FileMaster NO-LOCK /* idx=FileMasterID */
      WHERE FileMaster.FileMasterID = intGblFileMasterID NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN 
   DO:
      chrReturnError = "No FileMaster exists for FileMasterID:" + STRING(intGblFileMasterID).
   
      RETURN chrReturnError.
   END. /*IF NOT AVAILABLE FileMaster THEN*/
   
   FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR. /* idx=FileTypeID */
   IF NOT AVAILABLE FileType THEN
   DO:
      chrReturnError = "No FileType for FileMaster '" + chrFileMasterName + "'.".
      
      RETURN chrReturnError.
   END. /*IF NOT AVAILABLE FileType THEN*/ 
    
   ASSIGN intFileCompletedStatus      = fGetStatusID("File", "Complete")
          intShipOrderLabelStatus     = fGetStatusID("ShipOrder", "AwaitingTmsLabels")
          intShipOrderResponseStatus  = fGetStatusID("ShipOrder", "AwaitingTmsResponse")
          intShipOrderCancelledStatus = fGetStatusID("ShipOrder", "Cancelled").

   IF intFileCompletedStatus = 0 THEN
      RETURN "No FileStatus exists for Code: Complete".
      
   IF intShipOrderLabelStatus = 0 THEN
      RETURN "No ShipOrderStatus exists for Code: AwaitingTmsLabels".
      
   IF intShipOrderResponseStatus = 0 THEN
      RETURN "No ShipOrderStatus exists for Code: AwaitingTmsResponse".   
   
   IF intShipOrderCancelledStatus = 0 THEN
      RETURN "No ShipOrderStatus exists for Code: Cancelled".
    
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
  
   /* Load the dataset for the file */
   DATASET MasterBillOfLading:READ-XML("file", chrGblFilePath, "empty",  ?, FALSE, ?, "IGNORE") NO-ERROR.

   /* Header Validation */
   FIND FIRST ttHeader NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttHeader THEN
      RETURN "Header was not received".
   
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
   
   FIND FIRST ttMasterBillOfLading NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttMasterBillOfLading THEN
      RETURN "MasterBillOfLading cannot be empty".
    
   /* Validate Structure */
   FIND FIRST ttDate NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttDate THEN
      RETURN "Date cannot be empty".

   FIND FIRST ttExtractRequest NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttExtractRequest THEN
      RETURN "ExtractRequest cannot be empty".

   FIND FIRST ttReferenceNumber NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttReferenceNumber THEN
      RETURN "MasterBillOfLading ReferenceNumber cannot be empty".
 
   FIND FIRST ttShipReferenceNumber NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttShipReferenceNumber THEN
      RETURN "Shipment ReferenceNumber cannot be empty".
 
      FIND FIRST ttPriceSheet NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttPriceSheet THEN
         RETURN "PriceSheet cannot be empty".

   FIND FIRST ttItemGroup NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttItemGroup THEN
      RETURN "ItemGroup cannot be empty".

   FIND FIRST ttItemReferenceNumber NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ttItemReferenceNumber THEN
      RETURN "Item ReferenceNumber cannot be empty".

   /* RefereneceNumber Validation */
   FIND FIRST ttReferenceNumber NO-LOCK /* idx=type */ 
      WHERE ttReferenceNumber.type = "Load ID" NO-ERROR.
   IF NOT AVAILABLE ttReferenceNumber THEN
      RETURN "MasterBillOfLading ReferenceNumber for type 'Load ID' was not found".

   chrLoadNumber = ttReferenceNumber.ElementValue. 

   IF chrLoadNumber = "" THEN
      RETURN "MasterBillOfLading ReferenceNumber for type 'Load ID' Text cannot be blank".
 
   FIND FIRST ttReferenceNumber NO-LOCK /* idx=type */ 
      WHERE ttReferenceNumber.type = "SyncreonOrderNo" NO-ERROR.
   IF NOT AVAILABLE ttReferenceNumber THEN
      RETURN "MasterBillOfLading ReferenceNumber for type 'SyncreonOrderNo' was not found".
 
   chrOrderRef = ttReferenceNumber.ElementValue.
 
   IF chrOrderRef = "" THEN
      RETURN "MasterBillOfLading ReferenceNumber for type 'SyncreonOrderNo' Text cannot be blank".
   
   FIND FIRST ShipOrder NO-LOCK /* idx=OrderRef */
      WHERE ShipOrder.OrderRef = chrOrderRef NO-ERROR.
   IF NOT AVAILABLE ShipOrder THEN
      RETURN "No ShipOrder exists for OrderRef: " + chrOrderRef.
   
   intShipOrderID = ShipOrder.ShipOrderID.
   
   /* If this is a cancelled order just complete the file and leave the Main Block so we do not get error emails */
   IF ShipOrder.ShipOrderStatusID = intShipOrderCancelledStatus THEN
   DO TRANSACTION:
      /* Create File Record if this is the first attempt at processing this file */
      IF NOT AVAILABLE File THEN
      DO:
         CREATE File.
         BUFFER-COPY ttFile TO File.
      END. /*IF NOT AVAILABLE File THEN*/
      
      ASSIGN File.Completed    = fTimestamp(NOW)
             File.FileStatusID = fGetStatusID("File","Complete").
      LEAVE Main_Block.
   END. /*IF ShipOrder.ShipOrderStatusID = intShipOrderCancelledStatus THEN*/    
      
   IF ShipOrder.ShipOrderStatusID <> intShipOrderResponseStatus THEN
      RETURN "ShipOrder: " + chrOrderRef + " was not at 'AwaitingTmsResponse' Status". 
   
   FIND FIRST ShipOrderLine NO-LOCK /* idx=ShipOrderID */
      WHERE ShipOrderLine.ShipOrderID = ShipOrder.ShipOrderID
      AND   ShipOrderLine.ShipOrderStatusID = intShipOrderResponseStatus NO-ERROR.
   IF NOT AVAILABLE ShipOrderLine THEN
      RETURN "No ShipOrderLine at 'AwaitingTmsResponse' Status for ShipOrder: " + chrOrderRef.
 
   FIND FIRST ttReferenceNumber NO-LOCK /* idx=type */ 
      WHERE ttReferenceNumber.type = "PRO" NO-ERROR.
   IF NOT AVAILABLE ttReferenceNumber THEN
      RETURN "MasterBillOfLading ReferenceNumber for type 'PRO' was not found for OrderRef: " + chrOrderRef.

   chrProNumber = ttReferenceNumber.ElementValue.
 
   IF chrProNumber = "" THEN
      RETURN "MasterBillOfLading ReferenceNumber for type 'PRO' Text cannot be blank for OrderRef: " + chrOrderRef.
 
   /* PriceSheet Validation */
   FIND FIRST ttPriceSheet NO-LOCK /* idx=isSelected */ 
      WHERE ttPriceSheet.isSelected = "TRUE" NO-ERROR.
   IF NOT AVAILABLE ttPriceSheet THEN
      RETURN "PriceSheet isSelected cannot all be false for OrderRef: " + chrOrderRef.
   
   IF ttPriceSheet.SCAC = "" THEN
      RETURN "PriceSheet for SCAC Text cannot be blank for OrderRef: " + chrOrderRef.

   FIND FIRST Carrier NO-LOCK /* idx=SCACCode */
      WHERE Carrier.SCACCode = ttPriceSheet.SCAC 
      AND   Carrier.Active NO-ERROR.
   IF NOT AVAILABLE Carrier THEN
   DO:
      chrReturnError = "PriceSheet SCAC: " + ttPriceSheet.SCAC + " was not found in the system for an active Carrier for OrderRef: " 
                          + chrOrderRef.
      
      /* This could be an internal Error due to a data gap so we create create an internal Alert for it */
      RUN pCreateFileUploadError(INPUT  "Carrier",                     /* Table Name */
                                 INPUT  "Active AND Carrier.SCACCode", /* Field Name */
                                 INPUT  ttPriceSheet.SCAC,             /* Field Value */
                                 INPUT  chrReturnError).               /* Error */
   END. /*IF NOT AVAILABLE Carrier THEN*/   

   IF ttPriceSheet.Mode = "" THEN
      RETURN "PriceSheet for Mode Text cannot be blank for OrderRef: " + chrOrderRef.
      
   IF AVAILABLE Carrier THEN
   DO:
      FIND FIRST CarrierMode NO-LOCK /* idx=CarrierIDModeCode */
         WHERE CarrierMode.CarrierID = Carrier.CarrierID
         AND   CarrierMode.ModeCode = ttPriceSheet.Mode 
         AND   CarrierMode.Active NO-ERROR.
      IF NOT AVAILABLE CarrierMode THEN
      DO:
         chrReturnError = "PriceSheet Mode: " + ttPriceSheet.Mode + " was not found in the system for Carrier: " + Carrier.CarrierCode 
                             + " for OrderRef: " + chrOrderRef.
         
         /* This could be an internal Error due to a data gap so we create create an internal Alert for it */
         RUN pCreateFileUploadError(INPUT  "CarrierMode",                                           /* Table Name */
                                    INPUT  "CarrierID = " + STRING(Carrier.CarrierID) 
                                              + " AND CarrierMode.Active AND CarrierMode.ModeCode", /* Field Name */
                                    INPUT  ttPriceSheet.Mode,                                       /* Field Value */
                                    INPUT  chrReturnError).                                         /* Error */
      END. /*IF NOT AVAILABLE CarrierMode THEN*/
      
      IF ttPriceSheet.Service = "" THEN
          RETURN "PriceSheet for Service Text cannot be blank for OrderRef: " + chrOrderRef.
   
      FIND FIRST CarrierService NO-LOCK /* idx=CarrierIDServiceCode */
         WHERE CarrierService.CarrierID = Carrier.CarrierID
         AND   CarrierService.ServiceCode = ttPriceSheet.Service 
         AND   CarrierService.Active NO-ERROR.
      IF NOT AVAILABLE CarrierService THEN
      DO:
         chrReturnError = "PriceSheet Service: " + ttPriceSheet.Service + " was not found in the system for Carrier: " 
                             + Carrier.CarrierCode + " for OrderRef: " + chrOrderRef.
        
         /* This could be an internal Error due to a data gap so we create create an internal Alert for it */
         RUN pCreateFileUploadError(INPUT  "CarrierService",                                                 /* Table Name */
                                    INPUT  "CarrierID = " + STRING(Carrier.CarrierID) 
                                              + " AND CarrierService.Active AND CarrierService.ServiceCode", /* Field Name */
                                    INPUT  ttPriceSheet.Service,                                             /* Field Value */
                                    INPUT  chrReturnError).                                                  /* Error */
      END. /*IF NOT AVAILABLE CarrierService THEN*/
      
      FIND FIRST CarrierSortation OF Carrier NO-LOCK  /* idx=CarrierIDSortationCode */
         WHERE CarrierSortation.SortationCode = "B2C" 
         AND   CarrierSortation.Active NO-ERROR.
      IF NOT AVAILABLE CarrierSortation THEN
      DO:
         chrReturnError = "CarrierSortation was not available for Carrier: " + Carrier.CarrierCode + " Sortation: B2C for OrderRef: " 
                             + chrOrderRef.
            
         /* This could be an internal Error due to a data gap so we create create an internal Alert for it */
         RUN pCreateFileUploadError(INPUT  "CarrierSortation",                                                     /* Table Name */
                                    INPUT  "CarrierID = " + STRING(Carrier.CarrierID) 
                                              + " AND CarrierSortation.Active AND CarrierSortation.SortationCode", /* Field Name */
                                    INPUT  "B2C",                                                                  /* Field Value */
                                    INPUT  chrReturnError).                                                        /* Error */
      END. /*IF NOT AVAILABLE CarrierSortation THEN*/
      ELSE
      DO:
         chrCutOffTime = CarrierSortation.CutOffTime.
         /* Error Handling for CarrierSortation CutOffTime */
         IF chrCutOffTime = "" THEN
         DO:
            chrReturnError = "No CarrierSortation CutOffTime exists for Carrier Code: " + Carrier.CarrierCode + " for OrderRef: " 
                                + chrOrderRef.
            
            /* This could be an internal Error due to a data gap so we create create an internal Alert for it */
            RUN pCreateFileUploadError(INPUT "CarrierSortation",                                                            /* Table Name */
                                       INPUT "CarrierID = " + STRING(Carrier.CarrierID) 
                                                + " AND CarrierSortation.SortationCode = 'B2C' AND CarrierSortation.Active"
                                                + " AND NOT CarrierSortation.CutOffTime",                                   /* Field Name */
                                       INPUT "",                                                                            /* Field Value */
                                       INPUT chrReturnError).                                                               /* Error */
         END. /*IF chrCutOffTime = "" THEN*/
      END. /*IF AVAILABLE CarrierSortation THEN*/
   END. /*IF AVAILABLE Carrier THEN*/
   
   /* ItemGroup Validation */
   FIND FIRST ShipOrder NO-LOCK /* idx=OrderRef */
      WHERE ShipOrder.OrderRef = chrOrderRef NO-ERROR.
   IF AVAILABLE ShipOrder THEN
   DO:
      FOR EACH ttItemGroup NO-LOCK: 
   
         FIND FIRST ShipPackageCalculation NO-LOCK /* idx=ShipOrderID */
            WHERE ShipPackageCalculation.ShipOrderID    = ShipOrder.ShipOrderID
            AND   ShipPackageCalculation.CalculationRef = ttItemGroup.Description NO-ERROR.
         IF NOT AVAILABLE ShipPackageCalculation THEN
         DO:
            chrReturnError = "ShipPackageCalculation CalculationRef was not available for ShipOrderID: " + STRING(ShipOrder.ShipOrderID) 
                                + " CalculationRef: " + ttItemGroup.Description + " for OrderRef: " + chrOrderRef.
            
            /* This could be an internal Error due to a data gap so we create create an internal Alert for it */
            RUN pCreateFileUploadError(INPUT  "ShipPackageCalculation",                          /* Table Name */
                                       INPUT  "ShipOrderID = " + STRING(ShipOrder.ShipOrderID) 
                                                 + " AND ShipPackageCalculation.CalculationRef", /* Field Name */
                                       INPUT  ttItemGroup.Description,                           /* Field Value */
                                       INPUT  chrReturnError).                                   /* Error */
         END. /*IF NOT AVAILABLE ShipPackageCalculation THEN*/                    
      END. /*FOR EACH ttItemGroup NO-LOCK*/                       
   END. /*IF AVAILABLE ShipOrder THEN*/
   
   /* File Record Setup */
   File_Block:
   DO TRANSACTION ON ERROR UNDO, RETURN ERROR:
      /* Create File Record if this is the first attempt at processing this file */
      IF NOT AVAILABLE File THEN
      DO:
         CREATE File.
         BUFFER-COPY ttFile TO File.
      END. /*IF NOT AVAILABLE File THEN*/
      
      intGblNewFileID = File.FileID.
       
      FIND CURRENT File EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAILABLE File THEN
      DO:
         /* If File is LOCKED then set file to Pending and it will be uploaded again on the next Run. */
         chrReturnError = "Pending".
         RETURN chrReturnError.
      END. /*IF NOT AVAILABLE File THEN*/
      
      FileUploadError_Block:      
      FOR EACH ttFileUploadError ON ERROR UNDO File_Block, RETURN ERROR:
         
         /* If the error doesn't already exist create it */
         FIND FIRST FileUploadError NO-LOCK /* idx=FileID */
            WHERE FileUploadError.FileID      = ttFileUploadError.FileID
            AND   FileUploadError.TableName   = ttFileUploadError.TableName
            AND   FileUploadError.FieldName   = ttFileUploadError.FieldName
            AND   FileUploadError.FieldValue  = ttFileUploadError.FieldValue
            AND   FileUploadError.ErrorString = ttFileUploadError.ErrorString
            AND   FileUploadError.Resolved    = "" NO-ERROR.
         IF NOT AVAILABLE FileUploadError THEN
         DO:
            CREATE FileUploadError.
            BUFFER-COPY ttFileUploadError TO FileUploadError.
            RELEASE FileUploadError NO-ERROR.
         END. /*IF NOT AVAILABLE FileUploadError THEN*/
         ELSE 
            /* If this error already exists delete it does not get used in the email */
            DELETE ttFileUploadError.
          
         logDataGapError = TRUE.
         
      END. /*FOR EACH ttFileUploadError*/
      
      /* If this is set to data gap error set to pending and RETURN */
      IF logDataGapError = TRUE THEN
      DO:
         File.FileStatusID = fGetStatusID("File","Pending").
         RETURN "Pending".
      END. /*IF logDataGapError THEN*/
   END. /*File_Block*/   
   
   /* Update Database Tables */
   Update_Block:
   DO TRANSACTION ON ERROR UNDO, RETURN ERROR:
      FIND FIRST ShipOrder EXCLUSIVE-LOCK /* idx=OrderRef */
         WHERE ShipOrder.OrderRef = chrOrderRef NO-WAIT NO-ERROR.
      IF NOT AVAILABLE ShipOrder THEN
      DO:
         File.FileStatusID = fGetStatusID("File","Pending").
         RETURN "Pending".
      END. /*IF NOT AVAILABLE ShipOrder THEN*/   
      
      FOR EACH ShipOrderLine NO-LOCK /* idx=ShipOrderID */
         WHERE ShipOrderLine.ShipOrderID       = ShipOrder.ShipOrderID
         AND   ShipOrderLine.ShipOrderStatusID = intShipOrderResponseStatus:
             
         FIND FIRST updateShipOrderLine EXCLUSIVE-LOCK /* idx=ShipOrderLineID */
            WHERE updateShipOrderLine.ShipOrderLineID = ShipOrderLine.ShipOrderLineID NO-WAIT NO-ERROR.
         IF NOT AVAILABLE updateShipOrderLine THEN
         DO:
            chrReturnError = "Pending".
            UNDO Update_Block, LEAVE Update_Block.
         END. /*IF NOT AVAILABLE updateShipOrderLine THEN*/
            
         updateShipOrderLine.ShipOrderStatusID = intShipOrderLabelStatus.
         
         RELEASE updateShipOrderLine NO-ERROR.
         
      END. /*FOR EACH ShipOrderLine NO-LOCK*/
      
      ASSIGN ShipOrder.ShipOrderStatusID = intShipOrderLabelStatus
             ShipOrder.LoadRef           = chrLoadNumber
             ShipOrder.ProNumber         = chrProNumber.       
         
      IF AVAILABLE CarrierSortation THEN
      DO:
         ASSIGN ShipOrder.CarrierID          = CarrierSortation.CarrierID
                ShipOrder.CarrierSortationID = CarrierSortation.CarrierSortationID
                chrTimestamp                 = fTimestamp(NOW).
      END. /*IF AVAILABLE CarrierSortation THEN*/
             
      RELEASE CarrierSortation NO-ERROR.
      RELEASE Carrier          NO-ERROR.
         
      IF AVAILABLE CarrierService THEN
         ShipOrder.CarrierServiceID = CarrierService.CarrierServiceID.
      RELEASE CarrierService NO-ERROR.
          
      RELEASE CarrierMode NO-ERROR.
      RELEASE ShipOrderLine NO-ERROR.
         
      FOR EACH ttItemGroup NO-LOCK: 
      
         FIND FIRST ShipPackageCalculation EXCLUSIVE-LOCK /* idx=ShipOrderID */
            WHERE ShipPackageCalculation.ShipOrderID    = ShipOrder.ShipOrderID
            AND   ShipPackageCalculation.CalculationRef = ttItemGroup.Description NO-WAIT NO-ERROR.
         IF NOT AVAILABLE ShipPackageCalculation THEN
         DO:
            chrReturnError = "Pending".
            UNDO Update_Block, LEAVE Update_Block.
         END. /*IF NOT AVAILABLE ShipPackageCalculation THEN*/
         
         ShipPackageCalculation.CarrierTrackingRef = ttItemGroup.TrackingNumber.
         
         RELEASE ShipPackageCalculation NO-ERROR.
            
      END. /*FOR EACH ttItemGroup NO-LOCK*/   
         
      RELEASE ShipOrder NO-ERROR.
      
      IF chrReturnError <> "Pending" AND chrReturnError <> "Rejected" THEN
      ASSIGN File.Completed    = fTimestamp(NOW)
             File.FileStatusID = fGetStatusID("File","Complete").
   END. /*Update_Block:*/   
   
   /* Update the File FileStatusID for Pending Status if there was an error */
   Status_Block:   
   DO TRANSACTION ON ERROR UNDO, LEAVE:   
      IF chrReturnError = "Pending" THEN
      DO:
         File.FileStatusID = fGetStatusID("File","Pending").
         RETURN "Pending".
      END. /*IF chrReturnError = "Pending" THEN*/
   END. /*Status_Block:*/
END. /*Main_Block:*/

RELEASE OperationType          NO-ERROR.
RELEASE FileMaster             NO-ERROR.
RELEASE FileType               NO-ERROR.
RELEASE File                   NO-ERROR.
RELEASE GateUser               NO-ERROR.  
RELEASE ShipOrder              NO-ERROR.
RELEASE ShipOrderLine          NO-ERROR.
RELEASE updateShipOrderLine    NO-ERROR.
RELEASE ShipPackageCalculation NO-ERROR.
RELEASE CarrierMode            NO-ERROR.
RELEASE CarrierService         NO-ERROR.
RELEASE Carrier                NO-ERROR.

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