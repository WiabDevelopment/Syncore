/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filXmlTmsEstimatesExport.p
Purpose : Creates TMS XML files based on the BoxLogic calculated estimates

Author  : Alex Dolotov
Date    : 03/03/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncLockingFunctions.i}
{fncStatusTypeFunctions.i}
{fncStockFunctions.i}
{fncServerFunctions.i}

FIND FIRST TmsConfig NO-LOCK NO-ERROR.
IF TmsConfig.ManuallyBypassTmsMessaging = TRUE THEN
   LEAVE. 

/* Local Temp-Tables */
DEFINE TEMP-TABLE ttFileExport      NO-UNDO 
   LIKE File.

DEFINE TEMP-TABLE ttFileExportError NO-UNDO 
   LIKE FileUploadError.

/* DataSet Temp-Table Structures */
DEFINE TEMP-TABLE ttMercuryGate XML-NODE-NAME "MercuryGate"
   FIELD MercuryGateID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX MercuryGateID IS UNIQUE MercuryGateID.

DEFINE TEMP-TABLE ttHeader XML-NODE-NAME "Header"
   FIELD MercuryGateID AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD SenderID      AS CHARACTER 
   FIELD ReceiverID    AS CHARACTER
   FIELD DocTypeID     AS CHARACTER
   FIELD DocCount      AS CHARACTER
   INDEX MercuryGateID IS UNIQUE MercuryGateID.

DEFINE TEMP-TABLE ttLoad XML-NODE-NAME "Load"
   FIELD action        AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Add" XML-NODE-NAME "action"
   FIELD MercuryGateID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD LoadID        AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD AssignedTo    AS CHARACTER
   INDEX MercuryGateID IS UNIQUE MercuryGateID. 

DEFINE TEMP-TABLE ttEnterprise XML-NODE-NAME "Enterprise"
   FIELD CustomerAcctNum AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "customerAcctNum"
   FIELD EnterpriseName  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "name"
   FIELD LoadID          AS INTEGER   XML-NODE-TYPE "Hidden"
   INDEX LoadID LoadID.

DEFINE TEMP-TABLE ttLoadReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD LoadID                 AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD LoadReferenceNumbersID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX LoadReferenceNumbersID LoadReferenceNumbersID.

DEFINE TEMP-TABLE ttLoadReferenceNumber XML-NODE-NAME "ReferenceNumber"
   FIELD LoadReferenceNumbersID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD RefType                AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD IsPrimary              AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "isPrimary"
   FIELD ReferenceNumber        AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ReferenceNumber ReferenceNumber.
   
DEFINE TEMP-TABLE ttLoadPayment XML-NODE-NAME "Payment"
   FIELD LoadID        AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD PaymentID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD PaymentMethod AS CHARACTER XML-NODE-NAME "Method"
   INDEX PaymentID IS UNIQUE PaymentID.       

DEFINE TEMP-TABLE ttLoadBillTo XML-NODE-NAME "BillTo"
   FIELD PaymentID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX PaymentID PaymentID.         

DEFINE TEMP-TABLE ttLoadAddress XML-NODE-NAME "Address"
   FIELD IsResidential AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isResidential"
   FIELD IsPrimary     AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isPrimary"
   FIELD PaymentID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD AddressID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ShipmentID    AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD LocationCode  AS CHARACTER
   FIELD BillToName    AS CHARACTER XML-NODE-NAME "Name"
   FIELD AddrLine1     AS CHARACTER
   FIELD AddrLine2     AS CHARACTER
   FIELD City          AS CHARACTER
   FIELD StateProvince AS CHARACTER
   FIELD PostalCode    AS CHARACTER
   FIELD CountryCode   AS CHARACTER
   INDEX PaymentID PaymentID.   

DEFINE TEMP-TABLE ttLoadAddrReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD AddressID                  AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD LoadAddrReferenceNumbersID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX LoadAddrReferenceNumbersID LoadAddrReferenceNumbersID.
   
 DEFINE TEMP-TABLE ttLoadContacts XML-NODE-NAME "Contacts"
   FIELD AddressID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX AddressID AddressID.  

/***********************************************************************/
/******************************* Plan **********************************/
/***********************************************************************/

DEFINE TEMP-TABLE ttPlan XML-NODE-NAME "Plan"
   FIELD MercuryGateID AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD PlanID        AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX MercuryGateID IS UNIQUE MercuryGateID.      

DEFINE TEMP-TABLE ttEvents XML-NODE-NAME "Events"
   FIELD EventsCount AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "2" XML-NODE-NAME "count"
   FIELD PlanID      AS INTEGER   XML-NODE-TYPE "Hidden"
   INDEX PlanID PlanID.

DEFINE TEMP-TABLE ttEvent XML-NODE-NAME "Event"
   FIELD EventType   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD sequenceNum AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "sequenceNum"
   FIELD PlanID      AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD EventID     AS INTEGER   XML-NODE-TYPE "Hidden"
   INDEX EventID IS UNIQUE EventID.

DEFINE TEMP-TABLE ttEventDates XML-NODE-NAME "Dates"
   FIELD EventID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX EventID EventID.

DEFINE TEMP-TABLE ttEventDate XML-NODE-NAME "Date"
   FIELD EventID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD DateType  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD EventDate AS CHARACTER XML-NODE-TYPE "Text"
   INDEX EventID EventID.

DEFINE TEMP-TABLE ttEventAddress XML-NODE-NAME "Address"
   FIELD IsPrimary     AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isPrimary"
   FIELD EventAddrType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD isResidential AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "true" XML-NODE-NAME "isResidential"
   FIELD EventID       AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD AddressID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD LocationCode  AS CHARACTER
   FIELD LocationName  AS CHARACTER XML-NODE-NAME "Name"
   FIELD AddrLine1     AS CHARACTER
   FIELD AddrLine2     AS CHARACTER
   FIELD City          AS CHARACTER
   FIELD StateProvince AS CHARACTER
   FIELD PostalCode    AS CHARACTER
   FIELD CountryCode   AS CHARACTER
   INDEX EventID EventID.

DEFINE TEMP-TABLE ttEventContacts XML-NODE-NAME "Contacts"
   FIELD AddressID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX AddressID AddressID.
   
DEFINE TEMP-TABLE ttEventContact XML-NODE-NAME "Contact"
   FIELD ContactType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "Type"
   FIELD AddressID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactName AS CHARACTER XML-NODE-NAME "Name"
   INDEX ContactID ContactID.

DEFINE TEMP-TABLE ttEventContactMethods XML-NODE-NAME "ContactMethods"
   FIELD ContactID        AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ContactMethodsID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ContactID ContactID.    

DEFINE TEMP-TABLE ttEventContactMethod XML-NODE-NAME "ContactMethod"
   FIELD ContactType   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "phone" XML-NODE-NAME "type"
   FIELD SequenceNum   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "1" XML-NODE-NAME "sequenceNum"
   FIELD ContactID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactMethod AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ContactID ContactID. 

DEFINE TEMP-TABLE ttEventShipments XML-NODE-NAME "Shipments"
   FIELD EventID     AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ShipmentsID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX EventID EventID.  
   
DEFINE TEMP-TABLE ttEventReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD ShipmentsID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentsID ShipmentsID.

DEFINE TEMP-TABLE ttEventReferenceNumber XML-NODE-NAME "ReferenceNumber"
   FIELD RefType         AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "SyncreonOrderNo" XML-NODE-NAME "type"
   FIELD IsPrimary       AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "true" XML-NODE-NAME "isPrimary"
   FIELD ShipmentsID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ReferenceNumber AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ShipmentsID ShipmentsID.
     
/***********************************************************************/
/**************************** PriceSheets ******************************/
/***********************************************************************/   

DEFINE TEMP-TABLE ttLoadPriceSheets XML-NODE-NAME "PriceSheets"
   FIELD LoadID        AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD PriceSheetsID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX LoadID LoadID.

DEFINE TEMP-TABLE ttLoadPriceSheet XML-NODE-NAME "PriceSheet"
   FIELD PriceSheetType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Carrier" XML-NODE-NAME "type"
   FIELD isSelected     AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isSelected"
   FIELD PriceSheetsID  AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContractId     AS CHARACTER 
   FIELD SCAC           AS CHARACTER
   FIELD Mode           AS CHARACTER
   FIELD Service        AS CHARACTER
   INDEX PriceSheetsID PriceSheetsID.      

/***********************************************************************/
/****************************** Shipments ******************************/
/***********************************************************************/

DEFINE TEMP-TABLE ttLoadShipments XML-NODE-NAME "Shipments"
   FIELD LoadID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX LoadID LoadID. 

DEFINE TEMP-TABLE ttLoadShipment XML-NODE-NAME "Shipment"
   FIELD ShipmentType   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Regular" XML-NODE-NAME "type"
   FIELD ShipmentAction AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Add" XML-NODE-NAME "action"
   FIELD LoadID         AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ShipmentID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ShipmentStatus AS CHARACTER XML-NODE-NAME "Status"
   FIELD Comments       AS CHARACTER
   FIELD EquipmentList  AS CHARACTER
   FIELD ServiceList    AS CHARACTER
   INDEX ShipmentID ShipmentID. 

DEFINE TEMP-TABLE ttShipmentEnterprise XML-NODE-NAME "Enterprise"
   FIELD CustomerAcctNum AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "customerAcctNum"
   FIELD EnterpriseName  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "name"
   FIELD ShipmentID      AS INTEGER   XML-NODE-TYPE "Hidden"
   INDEX ShipmentID ShipmentID.

DEFINE TEMP-TABLE ttShipmentReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD ShipmentID                 AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ShipmentReferenceNumbersID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentReferenceNumbersID ShipmentReferenceNumbersID.

DEFINE TEMP-TABLE ttShipmentReferenceNumber XML-NODE-NAME "ReferenceNumber"
   FIELD RefType                    AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "SyncreonOrderNo" XML-NODE-NAME "type"
   FIELD IsPrimary                  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "true" XML-NODE-NAME "isPrimary"
   FIELD ShipmentReferenceNumbersID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ReferenceNumber            AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ShipmentReferenceNumbersID ShipmentReferenceNumbersID.

DEFINE TEMP-TABLE ttShipmentDimensions XML-NODE-NAME "Dimensions"
   FIELD ShipmentID   AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD DimensionsID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentID ShipmentID.

DEFINE TEMP-TABLE ttShipmentDates XML-NODE-NAME "Dates"
   FIELD ShipmentID      AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ShipmentDatesID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentID ShipmentID.

DEFINE TEMP-TABLE ttPickup XML-NODE-NAME "Pickup"
   FIELD ShipmentDatesID AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD PickUpID        AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentDatesID ShipmentDatesID.

DEFINE TEMP-TABLE ttPickupDate XML-NODE-NAME "Date"
   FIELD DateType   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD PickUpID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD PickUpDate AS CHARACTER XML-NODE-TYPE "Text"
   INDEX PickUpID PickUpID.
 
DEFINE TEMP-TABLE ttDrop XML-NODE-NAME "Drop"
   FIELD ShipmentDatesID AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD DropID          AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentDatesID ShipmentDatesID. 

DEFINE TEMP-TABLE ttDropDate XML-NODE-NAME "Date"
   FIELD DateType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD DropID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD DropDate AS CHARACTER XML-NODE-TYPE "Text"
   INDEX DropID DropID.
   
DEFINE TEMP-TABLE ttShipmentPriceSheets XML-NODE-NAME "PriceSheets"
   FIELD ShipmentID    AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD PriceSheetsID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentID ShipmentID.

DEFINE TEMP-TABLE ttShipmentPriceSheet XML-NODE-NAME "PriceSheet"
   FIELD PriceSheetType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Carrier" XML-NODE-NAME "type"
   FIELD isSelected     AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isSelected"
   FIELD PriceSheetsID  AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContractId     AS CHARACTER 
   FIELD SCAC           AS CHARACTER
   FIELD Mode           AS CHARACTER
   FIELD Service        AS CHARACTER
   INDEX PriceSheetsID PriceSheetsID.   
   
DEFINE TEMP-TABLE ttShipper XML-NODE-NAME "Shipper"
   FIELD ShipmentID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentID ShipmentID.   

DEFINE TEMP-TABLE ttShipperAddress XML-NODE-NAME "Address"
   FIELD AddressType   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "DistrbutionCentre" XML-NODE-NAME "type" 
   FIELD IsResidential AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isResidential" 
   FIELD ShipmentID    AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD AddressID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD LocationCode  AS CHARACTER
   FIELD LocationName  AS CHARACTER
   FIELD BillToName    AS CHARACTER XML-NODE-NAME "Name"
   FIELD AddrLine1     AS CHARACTER
   FIELD AddrLine2     AS CHARACTER
   FIELD City          AS CHARACTER
   FIELD StateProvince AS CHARACTER
   FIELD PostalCode    AS CHARACTER
   FIELD CountryCode   AS CHARACTER
   INDEX ShipmentID ShipmentID. 

DEFINE TEMP-TABLE ttShipperContacts XML-NODE-NAME "Contacts"
   FIELD AddressID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX AddressID AddressID.
   
DEFINE TEMP-TABLE ttShipperContact XML-NODE-NAME "Contact"
   FIELD ContactType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "Type" 
   FIELD AddressID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactName AS CHARACTER XML-NODE-NAME "Name"
   INDEX ContactID ContactID.

DEFINE TEMP-TABLE ttShipperContactMethods XML-NODE-NAME "ContactMethods"
   FIELD ContactID        AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ContactMethodsID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ContactID ContactID.

DEFINE TEMP-TABLE ttShipperContactMethod XML-NODE-NAME "ContactMethod"
   FIELD ContactPhone     AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "phone" XML-NODE-NAME "type" 
   FIELD SequenceNum      AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "1" XML-NODE-NAME "sequenceNum" 
   FIELD ContactMethodsID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactMethod    AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ContactMethodsID ContactMethodsID.

DEFINE TEMP-TABLE ttConsignee XML-NODE-NAME "Consignee"
   FIELD ShipmentID  AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ConsigneeID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentID ShipmentID.   

DEFINE TEMP-TABLE ttConsigneeAddress XML-NODE-NAME "Address"
   FIELD AddressType   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "Consignee" XML-NODE-NAME "type"
   FIELD IsResidential AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isResidential"
   FIELD ConsigneeID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD AddressID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD LocationCode  AS CHARACTER
   FIELD LocationName  AS CHARACTER
   FIELD BillToName    AS CHARACTER XML-NODE-NAME "Name"
   FIELD AddrLine1     AS CHARACTER
   FIELD AddrLine2     AS CHARACTER
   FIELD City          AS CHARACTER
   FIELD StateProvince AS CHARACTER
   FIELD PostalCode    AS CHARACTER
   FIELD CountryCode   AS CHARACTER
   INDEX ConsigneeID ConsigneeID. 

DEFINE TEMP-TABLE ttConsigneeContacts XML-NODE-NAME "Contacts"
   FIELD AddressID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX AddressID AddressID.
   
DEFINE TEMP-TABLE ttConsigneeContact XML-NODE-NAME "Contact"
   FIELD ContactType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "Type"
   FIELD AddressID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactName AS CHARACTER XML-NODE-NAME "Name"
   INDEX ContactID ContactID.

DEFINE TEMP-TABLE ttConsigneeContactMethods XML-NODE-NAME "ContactMethods"
   FIELD ContactID        AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ContactMethodsID AS INTEGER XML-NODE-TYPE "Hidden"
   FIELD ContactMethod    AS CHARACTER
   INDEX ContactID ContactID.

DEFINE TEMP-TABLE ttConsigneeContactMethod XML-NODE-NAME "ContactMethod"
   FIELD MethodType       AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "phone" XML-NODE-NAME "type"
   FIELD SequenceNum      AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "1" XML-NODE-NAME "sequenceNum"
   FIELD ContactMethodsID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContactMethod    AS CHARACTER
   INDEX ContactMethodsID ContactMethodsID.
   
/***********************************************************************/
/************************** itemGroups *********************************/
/***********************************************************************/

DEFINE TEMP-TABLE ttItemGroups XML-NODE-NAME "ItemGroups"
   FIELD ShipmentID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ShipmentID ShipmentID.  

DEFINE TEMP-TABLE ttItemGroup XML-NODE-NAME "ItemGroup"
   FIELD GroupID           AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "id"
   FIELD IsShipUnit        AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "isShipUnit"
   FIELD isHandlingUnit    AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "isHandlingUnit"
   FIELD Sequence          AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "sequence"
   FIELD ShipmentID        AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ItemGroupID       AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD GroupDescription  AS CHARACTER XML-NODE-NAME "Description"
   FIELD Commodity         AS CHARACTER
   FIELD NmfcCode          AS CHARACTER
   FIELD StccCode          AS CHARACTER
   FIELD Stackability      AS CHARACTER
   FIELD TrackingNumber    AS CHARACTER
   FIELD DeliveryStatus    AS CHARACTER
   FIELD MinTemperature    AS CHARACTER
   FIELD MaxTemperature    AS CHARACTER
   FIELD TemperatureUnits  AS CHARACTER
   FIELD HazardousMaterial AS CHARACTER
   INDEX ItemGroupID ItemGroupID.  

DEFINE TEMP-TABLE ttContainedBy XML-NODE-NAME "ContainedBy"
   FIELD ContainedByID AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "id"
   FIELD ItemGroupID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ContainedBy   AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttLineItem XML-NODE-NAME "LineItem"
   FIELD LineNumber           AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "lineNumber"
   FIELD ItemGroupID          AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD Cube                 AS CHARACTER
   FIELD HarmonizedTariffCode AS CHARACTER
   FIELD PortOfLading         AS CHARACTER
   FIELD CountryOfOrigin      AS CHARACTER
   FIELD CountryOfManufacture AS CHARACTER
   FIELD InsuredValue         AS CHARACTER
   FIELD InsuredValueCurrency AS CHARACTER
   FIELD CustomsValue         AS CHARACTER
   FIELD CustomsValueCurrency AS CHARACTER
   FIELD AESExportCode        AS CHARACTER
   FIELD AESLicenseNumber     AS CHARACTER
   FIELD AESLicenseType       AS CHARACTER
   FIELD CustomerPartNum      AS CHARACTER
   FIELD ManufacturerPartNum  AS CHARACTER
   FIELD DistributorPartNum   AS CHARACTER
   INDEX ItemGroupID ItemGroupID. 

DEFINE TEMP-TABLE ttDimensions XML-NODE-NAME "Dimensions"
   FIELD ItemGroupID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttDimension XML-NODE-NAME "Dimension"
   FIELD DimensionType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD DimensionUOM  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "uom"
   FIELD ItemGroupID   AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD Dimension     AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttItemReferenceNumbers XML-NODE-NAME "ReferenceNumbers"
   FIELD ItemGroupID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttItemReferenceNumber XML-NODE-NAME "ReferenceNumber"
   FIELD RefType         AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "CRFShipUnitID" XML-NODE-NAME "type"
   FIELD IsPrimary       AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isPrimary"
   FIELD ItemGroupID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ReferenceNumber AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttFreightClasses XML-NODE-NAME "FreightClasses"
   FIELD ItemGroupID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttFreightClass XML-NODE-NAME "FreightClass"
   FIELD FreightClassType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD ItemGroupID      AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD FreightClass     AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ItemGroupID ItemGroupID.  

DEFINE TEMP-TABLE ttWeights XML-NODE-NAME "Weights"
   FIELD ItemGroupID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttWeight XML-NODE-NAME "Weight"
   FIELD WeightType  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD WeightUOM   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "uom"
   FIELD ItemGroupID AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD Weight      AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttQuantities XML-NODE-NAME "Quantities"
   FIELD ItemGroupID  AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX ItemGroupID ItemGroupID.

DEFINE TEMP-TABLE ttQuantity XML-NODE-NAME "Quantity"
   FIELD QuantityType AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD QuantityUOM  AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "uom"
   FIELD ItemGroupID  AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD Quantity     AS CHARACTER XML-NODE-TYPE "Text"
   INDEX ItemGroupID ItemGroupID.
      
/***********************************************************************/
/************************* Payment Details *****************************/
/***********************************************************************/

DEFINE TEMP-TABLE ttShipmentPayment XML-NODE-NAME "Payment"
   FIELD ShipmentID    AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD PaymentID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD PaymentMethod AS CHARACTER XML-NODE-NAME "Method"
   INDEX PaymentID IS UNIQUE PaymentID.       

DEFINE TEMP-TABLE ttShipmentBillTo XML-NODE-NAME "BillTo"
   FIELD ThirdParty AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "thirdParty"
   FIELD PaymentID  AS INTEGER   XML-NODE-TYPE "Hidden"
   INDEX PaymentID PaymentID.         

DEFINE TEMP-TABLE ttShipmentAddress XML-NODE-NAME "Address"
   FIELD AddressType   AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "" XML-NODE-NAME "type"
   FIELD isResidential AS CHARACTER XML-NODE-TYPE "Attribute" INITIAL "false" XML-NODE-NAME "isResidential"
   FIELD PaymentID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD AddressID     AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD ShipmentID    AS INTEGER   XML-NODE-TYPE "Hidden"
   FIELD LocationCode  AS CHARACTER
   FIELD BillToName    AS CHARACTER XML-NODE-NAME "Name"
   FIELD AddrLine1     AS CHARACTER
   FIELD AddrLine2     AS CHARACTER
   FIELD City          AS CHARACTER
   FIELD StateProvince AS CHARACTER
   FIELD PostalCode    AS CHARACTER
   FIELD CountryCode   AS CHARACTER
   INDEX PaymentID PaymentID.   

 DEFINE TEMP-TABLE ttShipmentContacts XML-NODE-NAME "Contacts"
   FIELD AddressID AS INTEGER XML-NODE-TYPE "Hidden"
   INDEX AddressID AddressID.
   
/* Define Dataset with child-parent relationship */
DEFINE DATASET MercuryGateNotificationMessage XML-NODE-TYPE "Hidden" FOR ttMercuryGate, ttHeader, ttLoad, ttEnterprise, ttLoadReferenceNumbers,
ttLoadReferenceNumber, ttLoadPayment, ttLoadBillTo, ttLoadAddress, ttLoadAddrReferenceNumbers, ttLoadContacts, ttPlan, ttEvents, ttEvent, 
ttEventDates, ttEventDate, ttEventContacts, ttEventContact, ttEventContactMethods, ttEventContactMethod, ttEventAddress, ttEventShipments, 
ttEventReferenceNumbers, ttEventReferenceNumber, ttLoadPriceSheets, ttLoadPriceSheet, ttLoadShipments, ttLoadShipment, ttShipmentEnterprise, 
ttShipmentReferenceNumbers, ttShipmentReferenceNumber, ttShipmentDimensions, ttShipmentDates, ttPickUp, ttPickUpDate, ttDrop, ttDropDate, 
ttShipmentPriceSheets, ttShipmentPriceSheet, ttShipper, ttShipperAddress, ttShipperContacts, ttShipperContact, ttShipperContactMethods, 
ttShipperContactMethod, ttConsignee, ttConsigneeAddress, ttConsigneeContacts, ttConsigneeContact, ttConsigneeContactMethods, 
ttConsigneeContactMethod, ttItemGroups, ttItemGroup, ttContainedBy, ttLineItem, ttDimensions, ttDimension, ttItemReferenceNumbers, 
ttItemReferenceNumber, ttFreightClasses, ttFreightClass, ttWeights,  ttWeight, ttQuantities, ttQuantity, ttShipmentPayment, ttShipmentBillTo, 
ttShipmentAddress, ttShipmentContacts
   
DATA-RELATION MercuryGateHeaderDR FOR ttMercuryGate, ttHeader NESTED
   RELATION-FIELDS(MercuryGateID, MercuryGateID)
DATA-RELATION MercuryGateLoadDR FOR ttMercuryGate, ttLoad NESTED
   RELATION-FIELDS(MercuryGateID, MercuryGateID)    
DATA-RELATION LoadEnterpriseDR FOR ttLoad, ttEnterprise NESTED
   RELATION-FIELDS(LoadID, LoadID)   
DATA-RELATION LoadReferenceNumbersDR FOR ttLoad, ttLoadReferenceNumbers NESTED
   RELATION-FIELDS(LoadID, LoadID)
DATA-RELATION ReferenceNumbersReferenceNumberDR FOR ttLoadReferenceNumbers, ttLoadReferenceNumber NESTED
   RELATION-FIELDS(LoadReferenceNumbersID, LoadReferenceNumbersID)
/********* Payment DR starts here *************/  
DATA-RELATION LoadPaymentDR FOR ttLoad, ttLoadPayment NESTED
   RELATION-FIELDS(LoadID, LoadID)  
DATA-RELATION PaymentBillToDR FOR ttLoadPayment, ttLoadBillTo NESTED
   RELATION-FIELDS(PaymentID, PaymentID)   
DATA-RELATION BillToAddressDR FOR ttLoadBillTo, ttLoadAddress NESTED
   RELATION-FIELDS(PaymentID, PaymentID)  
DATA-RELATION AddressReferenceNumbersDR FOR ttLoadAddress, ttLoadAddrReferenceNumbers NESTED
   RELATION-FIELDS(AddressID, AddressID) 
DATA-RELATION AddressContactsDR FOR ttLoadAddress, ttLoadContacts NESTED
   RELATION-FIELDS(AddressID, AddressID)                 
/********* Plan DR starts here *************/   
DATA-RELATION LoadPlanDR FOR ttLoad, ttPlan NESTED
   RELATION-FIELDS(MercuryGateID, MercuryGateID)    
DATA-RELATION PlanEventsDR FOR ttPlan, ttEvents NESTED
   RELATION-FIELDS(PlanID, PlanID)      
DATA-RELATION EventsEventDR FOR ttEvents, ttEvent NESTED
   RELATION-FIELDS(PlanID, PlanID)  
DATA-RELATION EventDatesDR FOR ttEvent, ttEventDates NESTED
   RELATION-FIELDS(EventID, EventID) 
DATA-RELATION EventDatesDR FOR ttEventDates, ttEventDate NESTED
   RELATION-FIELDS(EventID, EventID) 
DATA-RELATION EventAddressDR FOR ttEvent, ttEventAddress NESTED
   RELATION-FIELDS(EventID, EventID)
DATA-RELATION AddressContactsDR FOR ttEventAddress, ttEventContacts NESTED
   RELATION-FIELDS(AddressID, AddressID)
DATA-RELATION ContactsContactDR FOR ttEventContacts, ttEventContact NESTED
   RELATION-FIELDS(AddressID, AddressID)
DATA-RELATION ContactContactMethodsDR FOR ttEventContact, ttEventContactMethods NESTED
   RELATION-FIELDS(ContactID, ContactID)
DATA-RELATION ContactMethodsContactMethodDR FOR ttEventContactMethods, ttEventContactMethod NESTED
   RELATION-FIELDS(ContactID, ContactID)
DATA-RELATION EventShipments FOR ttEvent, ttEventShipments NESTED
   RELATION-FIELDS(EventID, EventID)
DATA-RELATION ShipmentsReferenceNumbersRD FOR ttEventShipments, ttEventReferenceNumbers NESTED
   RELATION-FIELDS(ShipmentsID, ShipmentsID)   
DATA-RELATION ShipmentsReferenceNumbersRD FOR ttEventReferenceNumbers, ttEventReferenceNumber NESTED
   RELATION-FIELDS(ShipmentsID, ShipmentsID)   
/********* PriceSheets DR starts here *************/         
DATA-RELATION LoadPriceSheetsDR FOR ttLoad, ttLoadPriceSheets NESTED
   RELATION-FIELDS(LoadID, LoadID)      
DATA-RELATION PriceSheetsPriceSheetDR FOR ttLoadPriceSheets, ttLoadPriceSheet NESTED
   RELATION-FIELDS(PriceSheetsID, PriceSheetsID)  
/********* Shipments DR starts here *************/                
DATA-RELATION LoadShipmentsDR FOR ttLoad, ttLoadShipments NESTED
   RELATION-FIELDS(LoadID, LoadID)      
DATA-RELATION ShipmentsShipmentDR FOR ttLoadShipments, ttLoadShipment NESTED
   RELATION-FIELDS(LoadID, LoadID)   
DATA-RELATION ShipmentsShipmentDR FOR ttLoadShipment, ttShipmentEnterprise NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID) 
DATA-RELATION ShipmentReferenceNumbersDR FOR ttLoadShipment, ttShipmentReferenceNumbers NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)    
DATA-RELATION ShipmentReferenceNumbersDR FOR ttShipmentReferenceNumbers, ttShipmentReferenceNumber NESTED
   RELATION-FIELDS(ShipmentReferenceNumbersID, ShipmentReferenceNumbersID) 
DATA-RELATION ShipmentDimensionsDR FOR ttLoadShipment, ttShipmentDimensions NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)     
DATA-RELATION ShipmentDatesDR FOR ttLoadShipment, ttShipmentDates NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)    
/* PickUp Dates starts here */   
DATA-RELATION DatesPickUpDR FOR ttShipmentDates, ttPickUp NESTED
   RELATION-FIELDS(ShipmentDatesID, ShipmentDatesID)   
DATA-RELATION DatesPickUpDR FOR ttPickUp, ttPickUpDate NESTED
   RELATION-FIELDS(PickUpID, PickUpID)  
/* Drop Dates starts here */   
DATA-RELATION DatesDropDR FOR ttShipmentDates, ttDrop NESTED
   RELATION-FIELDS(ShipmentDatesID, ShipmentDatesID)    
DATA-RELATION DatesPickUpDR FOR ttDrop, ttDropDate NESTED
   RELATION-FIELDS(DropID, DropID)          
DATA-RELATION ShipmentPriceSheetsDR FOR ttLoadShipment, ttShipmentPriceSheets NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)
DATA-RELATION PriceSheetsPriceSheetID FOR ttShipmentPriceSheets, ttShipmentPriceSheet NESTED
   RELATION-FIELDS(PriceSheetsID, PriceSheetsID)
/* Shipper starts here */   
DATA-RELATION ShipmentShipperID FOR ttLoadShipment, ttShipper NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)  
DATA-RELATION ShipperAddressDR FOR ttShipper, ttShipperAddress NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)   
DATA-RELATION AddressContactsDR FOR ttShipperAddress, ttShipperContacts NESTED
   RELATION-FIELDS(AddressID, AddressID)     
DATA-RELATION ContactsContactDR FOR ttShipperContacts, ttShipperContact NESTED
   RELATION-FIELDS(AddressID, AddressID)
DATA-RELATION ContactContactMethodsDR FOR ttShipperContact, ttShipperContactMethods NESTED
   RELATION-FIELDS(ContactID, ContactID)
DATA-RELATION ContactContactMethodsDR FOR ttShipperContactMethods, ttShipperContactMethod NESTED
   RELATION-FIELDS(ContactMethodsID, ContactMethodsID)
/* Consignee starts here */       
DATA-RELATION ShipmentConsigneeID FOR ttLoadShipment, ttConsignee NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)  
DATA-RELATION ConsigneeAddressDR FOR ttConsignee, ttConsigneeAddress NESTED
   RELATION-FIELDS(ConsigneeID, ConsigneeID)   
DATA-RELATION AddressContactsDR FOR ttConsigneeAddress, ttConsigneeContacts NESTED
   RELATION-FIELDS(AddressID, AddressID)     
DATA-RELATION ContactsContactDR FOR ttConsigneeContacts, ttConsigneeContact NESTED
   RELATION-FIELDS(AddressID, AddressID)
DATA-RELATION ContactContactMethodsDR FOR ttConsigneeContact, ttConsigneeContactMethods NESTED
   RELATION-FIELDS(ContactID, ContactID)
DATA-RELATION ContactContactMethodsDR FOR ttConsigneeContactMethods, ttConsigneeContactMethod NESTED
   RELATION-FIELDS(ContactMethodsID, ContactMethodsID)                       
/* ItemGroups starts here */       
DATA-RELATION ShipmentItemGroupsDR FOR ttLoadShipment, ttItemGroups NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID) 
DATA-RELATION ItemGroupItemsGroupDR FOR ttItemGroups, ttItemGroup NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID) 
DATA-RELATION ItemGroupItemsGroupDR FOR ttItemGroup, ttContainedBy NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID) 
DATA-RELATION ItemGroupLineItemDR FOR ttItemGroup, ttLineItem NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)
DATA-RELATION ItemGroupDimensionDR FOR ttItemGroup, ttDimensions NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)
DATA-RELATION DimensionsDimensionDR FOR ttDimensions, ttDimension NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)
DATA-RELATION ItemGroupReferenceNumbersDR FOR ttItemGroup, ttItemReferenceNumbers NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)   
DATA-RELATION ReferenceNumbersReferenceNumberDR FOR ttItemReferenceNumbers, ttItemReferenceNumber NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID) 
DATA-RELATION ItemGroupFreightClassesDR FOR ttItemGroup, ttFreightClasses NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)
DATA-RELATION FreightClassesFreightClassDR FOR ttFreightClasses, ttFreightClass NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)
DATA-RELATION ItemGroupWeightsDR FOR ttItemGroup, ttWeights NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)
DATA-RELATION ItemGroupWeightsDR FOR ttWeights, ttWeight NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)  
DATA-RELATION ItemGroupQuantitiesDR FOR ttItemGroup, ttQuantities NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID)
DATA-RELATION ItemGroupQuantitiesDR FOR ttQuantities, ttQuantity NESTED
   RELATION-FIELDS(ItemGroupID, ItemGroupID) 
/* Payment starts here */  
DATA-RELATION ShipmentPaymentDR FOR ttLoadShipment, ttShipmentPayment NESTED
   RELATION-FIELDS(ShipmentID, ShipmentID)   
DATA-RELATION PaymentBillToDR FOR ttShipmentPayment, ttShipmentBillTo NESTED
   RELATION-FIELDS(PaymentID, PaymentID)  
DATA-RELATION BillToAddressDR FOR ttShipmentBillTo, ttShipmentAddress NESTED
   RELATION-FIELDS(PaymentID, PaymentID) 
DATA-RELATION AddressContactsDR FOR ttShipmentAddress, ttShipmentContacts NESTED
   RELATION-FIELDS(AddressID, AddressID).

/* LOCAL VARIABLES */
DEFINE VARIABLE chrReturnError                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE EventIDCounter                 AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE intItemGroupIDCounter          AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE intNumberOfSkus                AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE intReadyForTmsExchangeStatusID AS INTEGER   NO-UNDO.
DEFINE VARIABLE intAwaitingTmsResponseStatusID AS INTEGER   NO-UNDO.
DEFINE VARIABLE intPackageSequenceCounter      AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE chrDocTypeID                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEnterpriseCustomerAcctNum   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEnterpriseName              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPaymentMethod               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpLocationCode          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpName                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpAddrLine1             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpAddrLine2             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpCity                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpStateProvince         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpPostalCode            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpCountryCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpContactName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickUpContactPhone          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperLocationCode         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperName                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperAddrLine1            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperAddrLine2            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperCity                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperStateProvince        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperPostalCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperCountryCode          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperContactName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipperContactPhone         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrItemGroupCommodity          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrItemGroupSTCCCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrMessageType                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE decPackageCubicVolume          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE decTotalPackageWeight          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE intCancelledStatusID           AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrTodayDateStamp              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTomorrowDateStamp           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTodayEarliest               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTodayLatest                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTomorrowEarliest            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTomorrowLatest              AS CHARACTER NO-UNDO.
DEFINE VARIABLE logEnableDebugging             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrFileExportError             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLogFile                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileMasterName              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReturnValue                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTempDirectory               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOutputDirectory             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDirectoryError              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEmailAddress                AS CHARACTER NO-UNDO.

/*Undo Variables*/
DEFINE VARIABLE logTransSuccessful             AS LOGICAL. /* Want this to Undo */

/* BUFFERS */ 
DEFINE BUFFER readShipOrder    FOR ShipOrder.
DEFINE BUFFER updShipOrder     FOR ShipOrder.
DEFINE BUFFER updShipOrderLine FOR ShipOrderLine.

/* Streams */
DEFINE STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION.

MainBlk:
DO:
   
   intGblOperationTypeID = fGetTypeID("Operation", "TmsDataExchange").
   
   chrTodayDateStamp = fDisplayDateWithFormat(INPUT TODAY, 
                                              INPUT "MMDDYYYY",
                                              INPUT "/" ).
                                              
   chrTomorrowDateStamp = fDisplayDateWithFormat(INPUT fNextWorkingDay(TODAY), 
                                                 INPUT "MMDDYYYY",
                                                 INPUT "/" ).
   
   FIND FIRST GateUser NO-LOCK
      WHERE GateUser.GateUserID = intGblUserID NO-ERROR.
   IF AVAILABLE GateUser AND GateUser.Username <> "cron" THEN
      chrEmailAddress = GateUser.Email.
   
   /* Check Cancelled status */
   FIND FIRST ShipOrderStatus NO-LOCK
      WHERE ShipOrderStatus.StatusCode = "Cancelled" NO-ERROR.
   IF NOT AVAILABLE ShipOrderStatus THEN
   DO:
      chrReturnError = "ShipOrderStatus: Cancelled does not exist!".
      RETURN ERROR chrReturnError. 
   END.
   intCancelledStatusID = ShipOrderStatus.ShipOrderStatusID.
   
   /* Retrieving FileMaster Record for this program */
   FIND FIRST FileMaster NO-LOCK 
      WHERE FileMaster.MasterName = "tmsShipPackageEstimates" NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN 
   DO:
      chrReturnError = "FileMaster: tmsShipPackageEstimates does not exist!".
      RETURN ERROR chrReturnError.  
   END. /* IF AVAILABLE FileMaster THEN */
   
   /* Retrieve FileType for the FileMaster */
   FIND FileType OF FileMaster NO-LOCK.
   IF NOT AVAILABLE FileType THEN 
   DO:
      chrReturnError = "FileType with GblFileMasterID: " + STRING(FileMaster.FileMasterID) + " is not available!".
      RETURN ERROR chrReturnError. 
   END.
  
   /* This is name of FileMaster that we're wanting to run the import for - 
      is set as the Session parameter by Cron Run that calls this prog */
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
   
   chrLogFile = chrLogFileDirectory + chrFileMasterName + "_filXmlTmsEstimatesExport_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".
   
   IF logGblDebugging THEN 
   DO:
      /* This is used to reset logGblDebugging before exiting FileMaster.
         DebuggingOn can override CronEvent and enable debugging */
      logEnableDebugging = TRUE.
      /* Setup log files */
      OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
   END. /* IF logGblDebugging */
  
   /* Validate ReadyForTmsExchange Status */
   FIND FIRST ShipOrderStatus NO-LOCK 
       WHERE ShipOrderStatus.StatusCode = "ReadyForTmsExchange" NO-ERROR.
   IF NOT AVAILABLE ShipOrderStatus THEN
   DO:
       chrReturnError = "ShipOrderStatus: ReadyForTmsExchange, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE ShipOrderStatus */
   intReadyForTmsExchangeStatusID = ShipOrderStatus.ShipOrderStatusID.
   
   /* Validate AwaitingTmsResponse Status */
   FIND FIRST ShipOrderStatus NO-LOCK 
       WHERE ShipOrderStatus.StatusCode = "AwaitingTmsResponse" NO-ERROR.
   IF NOT AVAILABLE ShipOrderStatus THEN
   DO:
       chrReturnError = "ShipOrderStatus: AwaitingTmsResponse, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE ShipOrderStatus */
   intAwaitingTmsResponseStatusID = ShipOrderStatus.ShipOrderStatusID.
   
   /***********************/
   /* Log Related Section */
   /***********************/   
   
   IF logGblDebugging THEN 
   DO:
      fLog("Running: filXmlTmsEstimatesExport.p").
   END.
   
   /* IF debugging is disabled but FileMaster debugging is on allow debugging to write */
   IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE THEN 
   DO:
      logGblDebugging = TRUE.
      OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.
      fLog("Debugging enabled by FileMaster.DebuggingOn").
   END. /* IF logGblDebugging = FALSE AND FileMaster.DebuggingOn = TRUE */

   ASSIGN chrTempDirectory   = FileMaster.FilePath + "tmp/"
          chrOutputDirectory = FileMaster.FilePath NO-ERROR.
   
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
   
   /***************************************************/
   /* Check all TMS MessageNode records are available */
   /***************************************************/
   
   FIND FIRST TMSMessageType NO-LOCK
      WHERE TMSMessageType.TypeName = "MasterBillOfLading" NO-ERROR.
   IF NOT AVAILABLE TMSMessageType THEN
   DO:
       chrReturnError = "TMSMessageType: MasterBillOfLading, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageType */ 
   
   FIND FIRST TMSMessage NO-LOCK 
      WHERE TMSMessage.TMSMessageTypeID = TMSMessageType.TMSMessageTypeID NO-ERROR.
   IF NOT AVAILABLE TMSMessage THEN
   DO:
       chrReturnError = "TMSMessage OF: MasterBillOfLading Type, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessage */ 
   
   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "DocTypeID" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: DocTypeID, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode DocTypeID */   
   chrDocTypeID = TMSMessageNode.NodeValue.
      
   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "EnterpriseCustomerAcctNum" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: EnterpriseCustomerAcctNum, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode EnterpriseCustomerAcctNum */         
   chrEnterpriseCustomerAcctNum = TMSMessageNode.NodeValue.
      
   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "EnterpriseName" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: EnterpriseName, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode EnterpriseName */        
   chrEnterpriseName = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PaymentMethod" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PaymentMethod, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PaymentMethod */      
   chrPaymentMethod = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupLocationCode" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupLocationCode, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupLocationCode */        
   chrPickupLocationCode = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupName" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupName, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupName */       
   chrPickupName = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupAddrLine1" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupAddrLine1, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupAddrLine1 */      
   chrPickupAddrLine1 = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupAddrLine2" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupAddrLine2, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupAddrLine2 */         
   chrPickupAddrLine2 = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupCity" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupCity, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupCity */      
   chrPickupCity = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupStateProvince" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupStateProvince, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupStateProvince */      
   chrPickupStateProvince = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupPostalCode" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupPostalCode, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupPostalCode */         
   chrPickupPostalCode = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupCountryCode" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupCountryCode, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupCountryCode */           
   chrPickupCountryCode = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickUpContactName" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickUpContactName, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickUpContactName */      
   chrPickUpContactName = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "PickupContactPhone" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: PickupContactPhone, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode PickupContactPhone */      
   chrPickupContactPhone = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperLocationCode" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperLocationCode, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperLocationCode */       
   chrShipperLocationCode = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperName" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperName, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperName */      
   chrShipperName = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperAddrLine1" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperAddrLine1, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperAddrLine1 */      
   chrShipperAddrLine1 = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperAddrLine2" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperAddrLine2, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperAddrLine2 */       
   chrShipperAddrLine2 = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperCity" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperCity, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperCity */      
   chrShipperCity = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperStateProvince" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperStateProvince, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperStateProvince */      
   chrShipperStateProvince = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperPostalCode" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperPostalCode, does not exist!".
       RETURN ERROR chrReturnError. 
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperPostalCode */        
   chrShipperPostalCode = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperCountryCode" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperCountryCode, does not exist!".
       RETURN ERROR chrReturnError.
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperCountryCode */      
   chrShipperCountryCode = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperContactName" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperContactName, does not exist!".
       RETURN ERROR chrReturnError.
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperContactName */      
   chrShipperContactName = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ShipperContactPhone" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ShipperContactPhone, does not exist!".
       RETURN ERROR chrReturnError.
   END. /* End of IF NOT AVAILABLE TMSMessageNode ShipperContactPhone */      
   chrShipperContactPhone = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ItemGroupCommodity" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ItemGroupCommodity, does not exist!".
       RETURN ERROR chrReturnError.
   END. /* End of IF NOT AVAILABLE TMSMessageNode ItemGroupCommodity */        
   chrItemGroupCommodity = TMSMessageNode.NodeValue.

   FIND FIRST TMSMessageNode OF TMSMessage 
      WHERE TMSMessageNode.NodeName = "ItemGroupSTCCCode" NO-ERROR.
   IF NOT AVAILABLE TMSMessageNode THEN
   DO:
       chrReturnError = "TMSMessageNode: ItemGroupSTCCCode, does not exist!".
       RETURN ERROR chrReturnError.
   END. /* End of IF NOT AVAILABLE TMSMessageNode ItemGroupSTCCCode */      
   chrItemGroupSTCCCode = TMSMessageNode.NodeValue.
   
   /* Create MercuryGate and Header elements */
   CREATE ttMercuryGate.
   CREATE ttHeader. 
   
   ASSIGN ttHeader.DocTypeID = chrDocTypeID
          ttHeader.DocCount  = "1" NO-ERROR. 
   
   CREATE ttLoad. 
   CREATE ttEnterprise.
   
   ASSIGN ttEnterprise.CustomerAcctNum = chrEnterpriseCustomerAcctNum
          ttEnterprise.EnterpriseName  = chrEnterpriseName NO-ERROR.
           
   CREATE ttLoadReferenceNumbers.   
   
   CREATE ttLoadReferenceNumber. 
   ASSIGN ttLoadReferenceNumber.RefType          = "SyncreonOrderNo"
          ttLoadReferenceNumber.IsPrimary        = "true"
          ttLoadReferenceNumber.ReferenceNumber  = "" NO-ERROR.
   
   /* Create Load  parent and its Payment child */
   CREATE ttLoadPayment. 
          ttLoadPayment.PaymentMethod = chrPaymentMethod.
          
   CREATE ttLoadBillTo. 
   CREATE ttLoadAddress.            
   CREATE ttLoadAddrReferenceNumbers. 
   CREATE ttLoadContacts. 
   
   /*************************************************/
   /************ Create Plan and Events *************/
   /*************************************************/
   
   /* Get WorkDay using Today's day number */
   FIND FIRST WorkDay NO-LOCK WHERE WorkDay.DayNumber = WEEKDAY(TODAY) NO-ERROR.
   IF NOT AVAILABLE WorkDay THEN
   DO:
       chrReturnError = "WorkDay: " + STRING(WEEKDAY(TODAY)) + ", does not exist!".
       RETURN ERROR chrReturnError.
   END.
   
   CREATE ttPlan. 
   CREATE ttEvents. 
   
   /* Create PickUp Event Record */
   CREATE ttEvent. 
   ASSIGN ttEvent.EventID     = EventIDCounter
          ttEvent.EventType   = "PickUp"
          ttEvent.SequenceNum = "1" NO-ERROR.

   CREATE ttEventDates. 
          ttEventDates.EventID = EventIDCounter.

   FIND FIRST WorkShift OF WorkDay NO-LOCK 
      WHERE WorkShift.ShiftName = "Morning" NO-ERROR.
   IF NOT AVAILABLE WorkShift THEN
   DO:
       chrReturnError = "WorkShift with WorkDayID: " + STRING(WorkDay.WorkDayID) + 
                        " and ShiftName: Morning, does not exist!".
       RETURN ERROR chrReturnError.
   END.
   chrTodayEarliest = " " + WorkShift.StartTime.
   
   CREATE ttEventDate.
   ASSIGN ttEventDate.EventID   = EventIDCounter
          ttEventDate.EventDate = chrTodayDateStamp + chrTodayEarliest
          ttEventDate.DateType  = "earliest".
   
   FIND FIRST WorkShift OF WorkDay NO-LOCK 
      WHERE WorkShift.ShiftName = "Evening" NO-ERROR.
   IF NOT AVAILABLE WorkShift THEN
   DO:
       chrReturnError = "WorkShift with WorkDayID: " + STRING(WorkDay.WorkDayID) + 
                        " and ShiftName: Evening, does not exist!".
       RETURN ERROR chrReturnError.
   END.
   chrTodayLatest = " " + WorkShift.FinishTime.
    
   /* Create Latest Event */    
   CREATE ttEventDate.
   ASSIGN ttEventDate.EventID   = EventIDCounter
          ttEventDate.EventDate = chrTodayDateStamp + chrTodayLatest
          ttEventDate.DateType  = "latest".

   CREATE ttEventContacts. 
          ttEventContacts.AddressID = EventIDCounter.
   CREATE ttEventContact. 
   ASSIGN ttEventContact.AddressID   = EventIDCounter
          ttEventContact.ContactID   = EventIDCounter
          ttEventContact.ContactName = chrPickUpContactName NO-ERROR.
        
   CREATE ttEventContactMethods. 
          ttEventContactMethods.ContactID = EventIDCounter.
   CREATE ttEventContactMethod.
   ASSIGN ttEventContactMethod.ContactID     = EventIDCounter
          ttEventContactMethod.ContactMethod = chrPickupContactPhone NO-ERROR.

   CREATE ttEventShipments.
   ASSIGN ttEventShipments.EventID     = EventIDCounter
          ttEventShipments.ShipmentsID = EventIDCounter NO-ERROR.
          
   CREATE ttEventReferenceNumbers. 
          ttEventReferenceNumbers.ShipmentsID = EventIDCounter.
   
   CREATE ttEventReferenceNumber. 
   ASSIGN ttEventReferenceNumber.ShipmentsID     = EventIDCounter
          ttEventReferenceNumber.ReferenceNumber = "" NO-ERROR.

   EventIDCounter = EventIDCounter + 1.
   
   /* Getting nextday time range */
   FIND FIRST WorkDay NO-LOCK WHERE WorkDay.DayNumber = WEEKDAY(TODAY + 1) NO-ERROR.
   IF NOT AVAILABLE WorkDay THEN
   DO:
       chrReturnError = "WorkDay: " + STRING(WEEKDAY(TODAY + 1)) + ", does not exist!".
       RETURN ERROR chrReturnError.
   END.
   
   /* Create Drop Event Record */
   CREATE ttEvent. 
   ASSIGN ttEvent.EventID     = EventIDCounter
          ttEvent.EventType   = "Drop"
          ttEvent.SequenceNum = "2" NO-ERROR.

   CREATE ttEventDates. 
          ttEventDates.EventID = EventIDCounter.
   
   FIND FIRST WorkShift OF WorkDay NO-LOCK 
      WHERE WorkShift.ShiftName = "Morning" NO-ERROR.
   IF NOT AVAILABLE WorkShift THEN
   DO:
       chrReturnError = "WorkShift with WorkDayID: " + STRING(WorkDay.WorkDayID) + 
                        " and ShiftName: Morning, does not exist!".
       RETURN ERROR chrReturnError.
   END.
   chrTomorrowEarliest = " " + WorkShift.StartTime.
   
   CREATE ttEventDate.
   ASSIGN ttEventDate.EventID   = EventIDCounter
          ttEventDate.EventDate = chrTomorrowDateStamp + chrTomorrowEarliest
          ttEventDate.DateType  = "earliest".
   
   FIND FIRST WorkShift OF WorkDay NO-LOCK 
      WHERE WorkShift.ShiftName = "Evening" NO-ERROR.
   IF NOT AVAILABLE WorkShift THEN
   DO:
       chrReturnError = "WorkShift with WorkDayID: " + STRING(WorkDay.WorkDayID) + 
                        " and ShiftName: Evening, does not exist!".
       RETURN ERROR chrReturnError.
   END.
   chrTomorrowLatest = " " + WorkShift.FinishTime.
       
   CREATE ttEventDate.
   ASSIGN ttEventDate.EventID   = EventIDCounter
          ttEventDate.EventDate = chrTomorrowDateStamp + chrTomorrowLatest
          ttEventDate.DateType  = "latest".

   CREATE ttEventShipments. 
   ASSIGN ttEventShipments.EventID     = EventIDCounter
          ttEventShipments.ShipmentsID = EventIDCounter NO-ERROR.
          
   CREATE ttEventReferenceNumbers. 
          ttEventReferenceNumbers.ShipmentsID = EventIDCounter.
          
   CREATE ttEventReferenceNumber. 
   ASSIGN ttEventReferenceNumber.ShipmentsID     = EventIDCounter
          ttEventReferenceNumber.ReferenceNumber = "" NO-ERROR.

   /********************************************************/
   /****************** CREATE PriceSheets ******************/
   /********************************************************/

   CREATE ttLoadPriceSheets. 
   CREATE ttLoadPriceSheet. 
   
   /********************************************************/
   /********* CREATE Shipment, Shipper, Consignee **********/
   /********************************************************/

   CREATE ttLoadShipments. 
   CREATE ttLoadShipment.
    
   CREATE ttShipmentEnterprise.
   ASSIGN ttShipmentEnterprise.CustomerAcctNum = chrEnterpriseCustomerAcctNum
          ttShipmentEnterprise.EnterpriseName  = chrEnterpriseName NO-ERROR.
   
   CREATE ttShipmentReferenceNumbers. 
   CREATE ttShipmentReferenceNumber.
          ttShipmentReferenceNumber.ReferenceNumber = "". 

   CREATE ttShipmentDimensions. 
   
   CREATE ttShipmentDates. 

   CREATE ttPickUp. 
   CREATE ttPickUpDate.
   ASSIGN ttPickUpDate.DateType   = "earliest"
          ttPickUpDate.PickUpDate = chrTodayDateStamp + chrTodayEarliest.
          
   CREATE ttPickUpDate.
   ASSIGN ttPickUpDate.DateType   = "latest"
          ttPickUpDate.PickUpDate = chrTodayDateStamp + chrTodayLatest.
              
   CREATE ttDrop. 
   CREATE ttDropDate.
   ASSIGN ttDropDate.DateType = "earliest"
          ttDropDate.DropDate = chrTomorrowDateStamp + chrTomorrowEarliest NO-ERROR.
          
   CREATE ttDropDate.
   ASSIGN ttDropDate.DateType = "latest"
          ttDropDate.DropDate = chrTomorrowDateStamp + chrTomorrowLatest NO-ERROR.

   CREATE ttShipmentPriceSheets. 
   CREATE ttShipmentPriceSheet. 
   
   /* CREATE Shipper Record */
   CREATE ttShipper. 
   CREATE ttShipperAddress.
   ASSIGN ttShipperAddress.LocationName = chrShipperName
          ttShipperAddress.AddrLine1    = chrShipperAddrLine1
          ttShipperAddress.City         = chrShipperCity
          ttShipperAddress.PostalCode   = chrShipperPostalCode
          ttShipperAddress.CountryCode  = chrShipperCountryCode NO-ERROR.
          
   CREATE ttShipperContacts. 
   CREATE ttShipperContact.
          ttShipperContact.ContactName = chrShipperContactName.
           
   CREATE ttShipperContactMethods.
   CREATE ttShipperContactMethod.
          ttShipperContactMethod.ContactMethod = chrShipperContactPhone.
   
   /********************************************************/
   /*************** CREATE ShipmentPayment *****************/
   /********************************************************/
   
   /* This part is at the very end of the XML file */
   CREATE ttShipmentPayment.
          ttShipmentPayment.PaymentMethod = chrPaymentMethod.
           
   CREATE ttShipmentBillTo. 
   CREATE ttShipmentAddress. 
   CREATE ttShipmentContacts.
   
   /********************************************************/
   /***************** CREATE ItemGroups ********************/
   /********************************************************/
   
   CREATE ttItemGroups. 
   
   ShipOrderLoop:
   FOR EACH readShipOrder NO-LOCK 
      WHERE readShipOrder.ShipOrderStatusID = intReadyForTmsExchangeStatusID:
      
      /* The Above NO-LOCK read is not reliable had to add a SHARE-LOCK FIND see progress article 000021786 */            
      FIND FIRST ShipOrder SHARE-LOCK
         WHERE ROWID(ShipOrder) = ROWID(readShipOrder) NO-ERROR NO-WAIT.
      IF NOT AVAILABLE ShipOrder THEN NEXT ShipOrderLoop.   
      
      /* by default the message is unspecified */
      chrMessageType = "UnspecifiedCarrierOut".
      
      /* Clean dynamically populated data from tt */
      RUN pEmptyDynamicTTData.
      
      /* Reset PackageSequence */
      ASSIGN intPackageSequenceCounter = 1
             decTotalPackageWeight     = 0 /* Resets weight accumulator */
             intNumberOfSkus           = 0 NO-ERROR.
      
      /* by default set it to false, if no errors found it 
         will be overridden with TRUE */
      logTransSuccessful = FALSE.
      
      /* Main Transaction Block */
      ShipOrderBlk:
      DO TRANS ON ERROR UNDO, LEAVE:

         /* Check if Order has a Carrier Information */
         IF ShipOrder.CarrierID <> 0 THEN
         DO:
            FIND FIRST Carrier NO-LOCK OF ShipOrder NO-ERROR.
            IF NOT AVAILABLE Carrier THEN
            DO:
               RUN pCreateFileError(INPUT "No Carrier found with CarrierID: " + 
                                          STRING(ShipOrder.CarrierID)         +
                                          " for OrderRef: "                   + 
                                          STRING(ShipOrder.OrderRef)). 
               UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
            END.
            
            FIND FIRST CarrierService NO-LOCK OF ShipOrder NO-ERROR.
            IF NOT AVAILABLE CarrierService THEN
            DO:
               RUN pCreateFileError(INPUT "No CarrierService found with ServiceID: " + 
                                          STRING(ShipOrder.CarrierServiceID)         + 
                                          " for OrderRef: "                          + 
                                          STRING(ShipOrder.OrderRef)). 
               UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
            END.
            
            FIND FIRST ttLoadPriceSheet NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttLoadPriceSheet THEN
            DO:
               RUN pCreateFileError(INPUT "ttLoadPriceSheet is not available for OrderRef: " + 
                                          STRING(ShipOrder.OrderRef)).             
            END.
            
            /* Update Carrier Info for this ShipOrder */
            ASSIGN ttLoadPriceSheet.SCAC    = Carrier.SCAC  
                   ttLoadPriceSheet.Mode    = "Small Package"
                   ttLoadPriceSheet.Service = CarrierService.TmsServiceCode NO-ERROR.
            
            /* set MSG as specified */       
            chrMessageType = "SpecifiedCarrierOut".
            
         END. /*  IF ShipOrder.CarrierID <> 0 THEN */
      
         CREATE ttFileExport.
         ASSIGN ttFileExport.Completed     = ""
                ttFileExport.Created       = fTimestamp(NOW)
                ttFileExport.FileID        = NEXT-VALUE(File)
                ttFileExport.FileMasterID  = FileMaster.FileMasterID
                ttFileExport.FileName      = FileMaster.FilePrefix + 
                                             chrMessageType + "_"  +
                                             ShipOrder.OrderRef + ".xml"
                ttFileExport.FilePath      = chrTempDirectory + ttFileExport.FileName
                ttFileExport.FileTypeID    = FileType.FileTypeID
                ttFileExport.GateUserID    = GateUser.GateUserID
                ttFileExport.Sent          = "".      
      
         /* Dynamic data for PickUp Event */
         CREATE ttEventAddress. 
         ASSIGN ttEventAddress.EventID       = 1
                ttEventAddress.EventAddrType = "DistributionCentre"
                ttEventAddress.AddressID     = 1
                ttEventAddress.LocationCode  = chrPickupLocationCode
                ttEventAddress.LocationName  = chrPickupName
                ttEventAddress.AddrLine1     = chrPickupAddrLine1
                ttEventAddress.City          = chrPickupCity
                ttEventAddress.PostalCode    = chrPickupPostalCode
                ttEventAddress.CountryCode   = chrPickupCountryCode NO-ERROR.

         CREATE ttEventContacts. 
                ttEventContacts.AddressID = 1.
             
         CREATE ttEventContact. 
         ASSIGN ttEventContact.AddressID   = 1
                ttEventContact.ContactID   = 1
                ttEventContact.ContactName = chrPickUpContactName NO-ERROR.
             
         CREATE ttEventContactMethods. 
                ttEventContactMethods.ContactID = 1.
                
         CREATE ttEventContactMethod.
         ASSIGN ttEventContactMethod.ContactID     = 1
                ttEventContactMethod.ContactMethod = (IF chrPickUpContactPhone <> "" THEN chrPickUpContactPhone ELSE "NA")  NO-ERROR.          
         
         /* CREATE Consignee record */
         FIND FIRST Address NO-LOCK OF ShipOrder NO-ERROR.
         IF NOT AVAILABLE Address THEN
         DO:
            RUN pCreateFileError(INPUT "Address does not exist for OrderRef: " + 
                                       STRING(ShipOrder.OrderRef)). 
            UNDO ShipOrderBlk, LEAVE ShipOrderBlk.    
         END.
         
         FIND FIRST Country NO-LOCK OF Address NO-ERROR.
         IF NOT AVAILABLE Country THEN
         DO:
            RUN pCreateFileError(INPUT "No Country found with CountryID: " + 
                                       STRING(Address.CountryID)           + 
                                       " of Address for OrderRef: "        + 
                                       STRING(ShipOrder.OrderRef)). 
            UNDO ShipOrderBlk, LEAVE ShipOrderBlk.    
         END.
         
         /* Retrieve ShipOrder Customer */
         FIND FIRST Customer NO-LOCK OF ShipOrder NO-ERROR.
         IF NOT AVAILABLE Customer THEN
         DO:
            RUN pCreateFileError(INPUT "No Customer found with CustomerID: " + 
                                       STRING(ShipOrder.CustomerID)          +
                                       " for OrderRef: "                     + 
                                       STRING(ShipOrder.OrderRef)). 
            UNDO ShipOrderBlk, LEAVE ShipOrderBlk.    
         END.
         
         /* Dynamic EventAddress for Drop Event, EventIDCounter = 2 */
         CREATE ttEventAddress. 
         ASSIGN ttEventAddress.EventID       = EventIDCounter
                ttEventAddress.EventAddrType = "Consignee"
                ttEventAddress.AddressID     = EventIDCounter
                ttEventAddress.LocationName  = SUBSTR(Address.FullName, 1, 35)
                ttEventAddress.AddrLine1     = SUBSTR(Address.AddressLine1, 1, 35)
                ttEventAddress.AddrLine2     = SUBSTR(Address.AddressLine2, 1, 35)
                ttEventAddress.City          = SUBSTR(Address.City,1, 30)
                ttEventAddress.PostalCode    = Address.PostalCode
                ttEventAddress.StateProvince = Address.State
                ttEventAddress.CountryCode   = Country.Alpha2Code NO-ERROR.                                       
                
         CREATE ttEventContacts. 
                ttEventContacts.AddressID = EventIDCounter.
             
         CREATE ttEventContact. 
         ASSIGN ttEventContact.AddressID   = EventIDCounter
                ttEventContact.ContactID   = EventIDCounter
                ttEventContact.ContactName = SUBSTR(Address.FullName, 1, 35) NO-ERROR.
             
         CREATE ttEventContactMethods. 
                ttEventContactMethods.ContactID = EventIDCounter.
                
         CREATE ttEventContactMethod.
         ASSIGN ttEventContactMethod.ContactID     = EventIDCounter
                ttEventContactMethod.ContactMethod = (IF Customer.Phone <> "" THEN Customer.Phone ELSE "NA")  NO-ERROR.          
         
         CREATE ttConsignee. 
         CREATE ttConsigneeAddress. 
         ASSIGN ttConsigneeAddress.LocationName  = SUBSTR(Address.FullName, 1, 35)
                ttConsigneeAddress.AddrLine1     = SUBSTR(Address.AddressLine1, 1, 35)
                ttConsigneeAddress.AddrLine2     = SUBSTR(Address.AddressLine2, 1, 35)
                ttConsigneeAddress.City          = SUBSTR(Address.City, 1, 30)
                ttConsigneeAddress.PostalCode    = Address.PostalCode
                ttConsigneeAddress.StateProvince = Address.State
                ttConsigneeAddress.CountryCode   = Country.Alpha2Code NO-ERROR.
                
         CREATE ttConsigneeContacts. 
         CREATE ttConsigneeContact. 
                ttConsigneeContact.ContactName = SUBSTR(Address.FullName, 1, 35).
                
         CREATE ttConsigneeContactMethods. 
         CREATE ttConsigneeContactMethod.
                ttConsigneeContactMethod.ContactMethod = Customer.Phone.
            
         FOR EACH BoxLogicCalculation OF ShipOrder NO-LOCK 
            WHERE BoxLogicCalculation.Superceded = "":
               
            FOR EACH ShipPackageCalculation OF BoxLogicCalculation 
               WHERE ShipPackageCalculation.Superceded = "" NO-LOCK:
            
               FOR EACH ShipPackageCalcLine OF ShipPackageCalculation NO-LOCK:
                                     
                  /* ################## */
                  /* CREATE Part Record */  
                  /* ################## */                                     
                                     
                  FIND FIRST Part OF ShipPackageCalcLine NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE Part THEN
                  DO:
                     RUN pCreateFileError(INPUT "No Part found with PartID: "      + 
                                                STRING(ShipPackageCalcLine.PartID) + 
                                                " for OrderRef: "                  +
                                                ShipOrder.OrderRef). 
                     UNDO ShipOrderBlk, LEAVE ShipOrderBlk.  
                  END.
                  
                  /* Check that Part Weight not equals to zero */
                  IF Part.UnitWeight = 0 THEN
                  DO:
                     RUN pCreateFileError(INPUT "Weight cannot be 0. PartRef: " + 
                                                Part.PartRef                    + 
                                                " for OrderRef: "               + 
                                                ShipOrder.OrderRef). 
                     UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
                  END. /* IF Part.UnitWeight = 0 */
                  
                  FIND FIRST ShipPackageSize NO-LOCK OF ShipPackageCalculation NO-ERROR.
                  IF NOT AVAILABLE ShipPackageSize THEN
                  DO:
                     RUN pCreateFileError(INPUT "No ShipPackageSizeID found: "                    + 
                                                STRING(ShipPackageCalculation.ShipPackageSizeID)  + 
                                                " for OrderRef: "                                 + 
                                                ShipOrder.OrderRef                                +
                                                " CalculationRef: " + ShipPackageCalculation.CalculationRef).
                     UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
                  END. /* IF NOT AVAILABLE ShipPackageSize */
                  
                  FIND FIRST ShipPackageType NO-LOCK OF ShipPackageCalculation NO-ERROR.
                  IF NOT AVAILABLE ShipPackageType THEN
                  DO:
                     RUN pCreateFileError(INPUT "No ShipPackageTypeID found: + "                  + 
                                                STRING(ShipPackageCalculation.ShipPackageTypeID)  + 
                                                " for OrderRef: "                                 + 
                                                ShipOrder.OrderRef                                +
                                                " CalculationRef: " + ShipPackageCalculation.CalculationRef).
                     UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
                  END. /* IF NOT AVAILABLE ShipPackageType */
                   
                  CREATE ttItemGroup. 
                  ASSIGN ttItemGroup.ItemGroupID       = intItemGroupIDCounter
                         ttItemGroup.GroupID           = STRING(ShipPackageCalcLine.ShipPackageCalcLineID)
                         ttItemGroup.Sequence          = "1"
                         ttItemGroup.isShipUnit        = "false"
                         ttItemGroup.isHandlingUnit    = "false"
                         ttItemGroup.Commodity         = chrItemGroupCommodity
                         ttItemGroup.NmfcCode          = ""
                         ttItemGroup.StccCode          = chrItemGroupSTCCCode
                         ttItemGroup.HazardousMaterial = "false" 
                         ttItemGroup.GroupDescription  = Part.PartDescr NO-ERROR.
                  
                  CREATE ttContainedBy.
                  ASSIGN ttContainedBy.ItemGroupID   = intItemGroupIDCounter
                         ttContainedBy.ContainedByID = ShipPackageCalculation.CalculationRef NO-ERROR.
                  
                  CREATE ttLineItem. 
                  ASSIGN ttLineItem.ItemGroupID          = intItemGroupIDCounter
                         ttLineItem.LineNumber           = "1"
                         ttLineItem.Cube                 = STRING("")
                         ttLineItem.InsuredValue         = "1"
                         ttLineItem.InsuredValueCurrency = "EUR"
                         ttLineItem.CustomsValue         = "1"
                         ttLineItem.CustomsValueCurrency = "EUR"
                         ttLineItem.CustomerPartnum      = Part.PartRef
                         ttLineItem.ManufacturerPartNum  = Part.PartRef
                         ttLineItem.DistributorPartNum   = Part.PartRef NO-ERROR.  
                  
                  CREATE ttDimensions.
                         ttDimensions.ItemGroupID = intItemGroupIDCounter.
                          
                  CREATE ttDimension.
                  ASSIGN ttDimension.ItemGroupID   = intItemGroupIDCounter
                         ttDimension.DimensionType = "Length"
                         ttDimension.DimensionUOM  = "cm"
                         ttDimension.Dimension     = STRING(Part.Depth) NO-ERROR.  
               
                  CREATE ttDimension.
                         ttDimensions.ItemGroupID = intItemGroupIDCounter.
                         
                  ASSIGN ttDimension.ItemGroupID   = intItemGroupIDCounter
                         ttDimension.DimensionType = "Width"
                         ttDimension.DimensionUOM  = "cm"
                         ttDimension.Dimension     = STRING(Part.Width) NO-ERROR.
               
                  CREATE ttDimension.
                         ttDimensions.ItemGroupID = intItemGroupIDCounter.
                         
                  ASSIGN ttDimension.ItemGroupID   = intItemGroupIDCounter
                         ttDimension.DimensionType = "Height"
                         ttDimension.DimensionUOM  = "cm"
                         ttDimension.Dimension     =  STRING(Part.Height) NO-ERROR.       
               
                  CREATE ttItemReferenceNumbers. 
                         ttItemReferenceNumbers.ItemGroupID = intItemGroupIDCounter.
                         
                  CREATE ttFreightClasses. 
                         ttFreightClasses.ItemGroupID = intItemGroupIDCounter.
                         
                  CREATE ttFreightClass.
                  ASSIGN ttFreightClass.ItemGroupID  = intItemGroupIDCounter
                         ttFreightClass.FreightClass = "-1" NO-ERROR.
                         
                  CREATE ttWeights. 
                         ttWeights.ItemGroupID = intItemGroupIDCounter.
                         
                  CREATE ttWeight.
                  ASSIGN ttWeight.ItemGroupID = intItemGroupIDCounter
                         ttWeight.WeightType  = "actual"
                         ttWeight.WeightUOM   = "kg"
                         ttWeight.Weight      = STRING(Part.UnitWeight) NO-ERROR.
                         
                  CREATE ttWeight.    
                  ASSIGN ttWeight.ItemGroupID = intItemGroupIDCounter
                         ttWeight.WeightType  = "planned"
                         ttWeight.WeightUOM   = "kg"
                         ttWeight.Weight      = STRING(Part.UnitWeight) NO-ERROR.
                              
                  CREATE ttWeight.  
                  ASSIGN ttWeight.ItemGroupID = intItemGroupIDCounter
                         ttWeight.WeightType  = "ordered"
                         ttWeight.WeightUOM   = "kg"
                         ttWeight.Weight      = STRING(Part.UnitWeight) NO-ERROR.
                  
                  /* Accumulating weight */
                  decTotalPackageWeight = decTotalPackageWeight + 
                                          ( Part.UnitWeight * ShipPackageCalcLine.QtyAssigned ).
                             
                  CREATE ttQuantities. 
                         ttQuantities.ItemGroupID = intItemGroupIDCounter.
                             
                  CREATE ttQuantity. 
                  ASSIGN ttQuantity.ItemGroupiD  = intItemGroupIDCounter
                         ttQuantity.QuantityType = "actual"
                         ttQuantity.QuantityUOM  = ""
                         ttQuantity.Quantity     = STRING(ShipPackageCalcLine.QtyAssigned) NO-ERROR.
                  
                  CREATE ttQuantity.    
                  ASSIGN ttQuantity.ItemGroupiD  = intItemGroupIDCounter
                         ttQuantity.QuantityType = "planned"
                         ttQuantity.QuantityUOM  = ""
                         ttQuantity.Quantity     = STRING(ShipPackageCalcLine.QtyAssigned) NO-ERROR.
                         
                  CREATE ttQuantity. 
                  ASSIGN ttQuantity.ItemGroupiD  = intItemGroupIDCounter
                         ttQuantity.QuantityType = "ordered"
                         ttQuantity.QuantityUOM  = ""
                         ttQuantity.Quantity     = STRING(ShipPackageCalcLine.QtyAssigned) NO-ERROR.       
      
                  ASSIGN intItemGroupIDCounter = intItemGroupIDCounter + 1
                         intNumberOfSkus       = intNumberOfSkus + 1 NO-ERROR.
      
               END. /* End of for each ShipPackageLineCalc */
               
               /* ########################## */
               /* CREATE Package Unit Record */  
               /* ########################## */
               
               CREATE ttItemGroup. 
               ASSIGN ttItemGroup.ItemGroupID       = intItemGroupIDCounter
                      ttItemGroup.Sequence          = STRING(intPackageSequenceCounter)
                      ttItemGroup.GroupID           = ShipPackageCalculation.CalculationRef
                      ttItemGroup.isShipUnit        = "true"
                      ttItemGroup.isHandlingUnit    = "true"
                      ttItemGroup.Commodity         = chrItemGroupCommodity
                      ttItemGroup.NmfcCode          = ""
                      ttItemGroup.StccCode          = chrItemGroupSTCCCode
                      ttItemGroup.HazardousMaterial = "false" 
                      ttItemGroup.GroupDescription  = ShipPackageCalculation.CalculationRef NO-ERROR.
               
               CREATE ttContainedBy.
               ASSIGN ttContainedBy.ItemGroupID   = intItemGroupIDCounter
                      ttContainedBy.ContainedByID = "" NO-ERROR.
               
               decPackageCubicVolume = (ShipPackageSize.PackageHeight * 
                                        ShipPackageSize.PackageWidth  *
                                        ShipPackageSize.PackageDepth ) / 1000000000. /* in cubic metres */       
               
               /* Check Minimum Weight for Specified MSG,
                * if overall package weight is less than minimum 
                * then assign it to minimum */
               IF chrMessageType = "SpecifiedCarrierOut" THEN
               DO:
                  IF decTotalPackageWeight < CarrierService.MinPackageWeight THEN
                  DO:
                     decTotalPackageWeight = CarrierService.MinPackageWeight.
                  END.
               END. /* IF chrMessageType = "SpecifiedCarrierOut" */
               
               CREATE ttLineItem. 
               ASSIGN ttLineItem.ItemGroupID          = intItemGroupIDCounter
                      ttLineItem.Cube                 = STRING(decPackageCubicVolume)
                      ttLineItem.LineNumber           = "1"
                      ttLineItem.InsuredValue         = "1"
                      ttLineItem.InsuredValueCurrency = "EUR"
                      ttLineItem.CustomsValue         = "1"
                      ttLineItem.CustomsValueCurrency = "EUR"
                      ttLineItem.CustomerPartnum      = ""
                      ttLineItem.ManufacturerPartNum  = ""
                      ttLineItem.DistributorPartNum   = "" NO-ERROR.
            
               CREATE ttDimensions.
                      ttDimensions.ItemGroupID = intItemGroupIDCounter.
                       
               CREATE ttDimension.
               ASSIGN ttDimension.ItemGroupID   = intItemGroupIDCounter
                      ttDimension.DimensionType = "Length"
                      ttDimension.DimensionUOM  = "cm"
                      ttDimension.Dimension     = STRING(ShipPackageSize.PackageDepth) NO-ERROR.  
            
               CREATE ttDimension.
                      ttDimensions.ItemGroupID = intItemGroupIDCounter.
                      
               ASSIGN ttDimension.ItemGroupID   = intItemGroupIDCounter
                      ttDimension.DimensionType = "Width"
                      ttDimension.DimensionUOM  = "cm"
                      ttDimension.Dimension     = STRING(ShipPackageSize.PackageWidth) NO-ERROR.
            
               CREATE ttDimension.
                      ttDimensions.ItemGroupID = intItemGroupIDCounter.
                      
               ASSIGN ttDimension.ItemGroupID   = intItemGroupIDCounter
                      ttDimension.DimensionType = "Height"
                      ttDimension.DimensionUOM  = "cm"
                      ttDimension.Dimension     = STRING(ShipPackageSize.PackageHeight) NO-ERROR.       
            
               CREATE ttItemReferenceNumbers. 
                      ttItemReferenceNumbers.ItemGroupID = intItemGroupIDCounter.
                           
               CREATE ttItemReferenceNumber. 
               ASSIGN ttItemReferenceNumber.ItemGroupID     = intItemGroupIDCounter
                      ttItemReferenceNumber.ReferenceNumber = ShipPackageCalculation.CalculationRef NO-ERROR.
         
               CREATE ttFreightClasses. 
                      ttFreightClasses.ItemGroupID = intItemGroupIDCounter.
                      
               CREATE ttFreightClass.
               ASSIGN ttFreightClass.ItemGroupID  = intItemGroupIDCounter
                      ttFreightClass.FreightClass = "-1" NO-ERROR.
                      
               CREATE ttWeights. 
                      ttWeights.ItemGroupID = intItemGroupIDCounter.
                      
               CREATE ttWeight.
               ASSIGN ttWeight.ItemGroupID = intItemGroupIDCounter
                      ttWeight.WeightType  = "actual"
                      ttWeight.WeightUOM   = "kg"
                      ttWeight.Weight      = STRING(decTotalPackageWeight) NO-ERROR.
                      
               CREATE ttWeight.    
               ASSIGN ttWeight.ItemGroupID = intItemGroupIDCounter
                      ttWeight.WeightType  = "planned"
                      ttWeight.WeightUOM   = "kg"
                      ttWeight.Weight      = STRING(decTotalPackageWeight) NO-ERROR.
                           
               CREATE ttWeight.  
               ASSIGN ttWeight.ItemGroupID = intItemGroupIDCounter
                      ttWeight.WeightType  = "ordered"
                      ttWeight.WeightUOM   = "kg"
                      ttWeight.Weight      = STRING(decTotalPackageWeight) NO-ERROR.
                          
               CREATE ttQuantities. 
                      ttQuantities.ItemGroupID = intItemGroupIDCounter.
                          
               CREATE ttQuantity. 
               ASSIGN ttQuantity.ItemGroupiD  = intItemGroupIDCounter
                      ttQuantity.QuantityType = "actual"
                      ttQuantity.QuantityUOM  = ShipPackageType.TypeName
                      ttQuantity.Quantity     = "1" NO-ERROR.
               
               CREATE ttQuantity.    
               ASSIGN ttQuantity.ItemGroupiD  = intItemGroupIDCounter
                      ttQuantity.QuantityType = "planned"
                      ttQuantity.QuantityUOM  = ShipPackageType.TypeName
                      ttQuantity.Quantity     = "1" NO-ERROR.
                      
               CREATE ttQuantity. 
               ASSIGN ttQuantity.ItemGroupiD  = intItemGroupIDCounter
                      ttQuantity.QuantityType = "ordered"
                      ttQuantity.QuantityUOM  = ShipPackageType.TypeName
                      ttQuantity.Quantity     = "1" NO-ERROR.
               
               /* Setting OrderRef in static part of the XML */
               FIND FIRST ttShipmentReferenceNumber NO-ERROR.
               ttShipmentReferenceNumber.ReferenceNumber = ShipOrder.OrderRef .
               FIND FIRST ttLoadReferenceNumber NO-ERROR.
               ttLoadReferenceNumber.ReferenceNumber = ShipOrder.OrderRef.
               
               /* Retrieve PickUp EventReferenceNumber */
               FIND FIRST ttEventReferenceNumber EXCLUSIVE-LOCK 
                  WHERE ttEventReferenceNumber.ShipmentsID = 1 NO-ERROR.
               IF NOT AVAILABLE ttEventReferenceNumber THEN
               DO:
                  RUN pCreateFileError(INPUT "No PickUp Event found: " + 
                                             " for OrderRef: "         + 
                                             STRING(ShipOrder.OrderRef)).              
                  UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
               END.               
               ttEventReferenceNumber.ReferenceNumber = ShipOrder.OrderRef. 
               
               /* Retrieve Drop EventReferenceNumber */
               FIND FIRST ttEventReferenceNumber EXCLUSIVE-LOCK 
                  WHERE ttEventReferenceNumber.ShipmentsID = 2 NO-ERROR.
               
               IF NOT AVAILABLE ttEventReferenceNumber THEN
               DO:
                  RUN pCreateFileError(INPUT "No Drop Event found: " + 
                                             " for OrderRef: "       + 
                                             STRING(ShipOrder.OrderRef)).          
                  UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
               END.
               ttEventReferenceNumber.ReferenceNumber = ShipOrder.OrderRef.
               
               /* increment counter again, if there are more than one package unit */
               intItemGroupIDCounter = intItemGroupIDCounter + 1.
               
               /* increment sequence number for package unit */
               intPackageSequenceCounter = intPackageSequenceCounter + 1.
               
               /* clear totalPackageWeight */
               decTotalPackageWeight = 0.
               
            END. /* End of FOR EACH ShipPackageCalculation */
         END. /* END of FOR EACH BoxLogicCalcution */
         
         /* Checking Number of SKUs */
         IF intNumberOfSkus = 0 THEN
         DO:
            RUN pCreateFileError(INPUT "No parts were found in the ShipPackageCalcLine for OrderRef: " + 
                                       STRING(ShipOrder.OrderRef)).         
            UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
         END. /* IF intNumberOfSkus = 0 */
         
         /* Getting ready to update Order Status */
         FIND FIRST updShipOrder EXCLUSIVE-LOCK 
            WHERE updShipOrder.ShipOrderID = ShipOrder.ShipOrderID NO-WAIT NO-ERROR.
         IF NOT AVAILABLE updShipOrder THEN
         DO:
            IF LOCKED updShipOrder THEN
            DO:
               RUN pCreateFileError(INPUT fGetLockingInfo(INTEGER(RECID(ShipOrder)),
                                          "ShipOrder",
                                          STRING(ShipOrder.OrderRef)) + " - please try again!").
               UNDO ShipOrderBlk, LEAVE ShipOrderBlk.   
            END.
            
            RUN pCreateFileError(INPUT "No ShipOrder found with OrderRef: " + 
                                       STRING(ShipOrder.OrderRef)). 
            UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
         END. /* End of IF NOT AVAILABLE updShipOrder */
         
         updShipOrder.ShipOrderStatusID = intAwaitingTmsResponseStatusID.
         
         /* Update Statuses of ShipOrderLines */
         FOR EACH ShipOrderLine OF updShipOrder NO-LOCK 
            WHERE ShipOrderLine.ShipOrderStatusID <> intCancelledStatusID:
            
            FIND FIRST updShipOrderLine EXCLUSIVE-LOCK 
               WHERE updShipOrderLine.ShipOrderLineID = ShipOrderLine.ShipOrderLineID NO-WAIT NO-ERROR.
            IF NOT AVAILABLE updShipOrderLine THEN
            DO:
               IF LOCKED updShipOrderLine THEN
               DO:
                  RUN pCreateFileError(INPUT fGetLockingInfo(INTEGER(RECID(ShipOrderLine)),
                                             "ShipOrderLine",
                                             STRING(ShipOrderLine.ShipOrderLineID)) + " - please try again!").   
                                                            
                  UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
               END.
   
               RUN pCreateFileError(INPUT "No ShipOrderLine found with ShipOrderLineIDL: " + 
                                          STRING(ShipOrderLine.ShipOrderLineID)            + 
                                          " for OrderRef: "                                + 
                                          STRING(updShipOrder.OrderRef)).
               UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
               
            END. /* End of IF NOT AVAILABLE updShipOrder */
            
            updShipOrderLine.ShipOrderStatusID = intAwaitingTmsResponseStatusID.
            
         END. /* FOR EACH ShipOrderLine OF updShipOrder NO-LOCK */
   
         /* check if any errors occured while updating statuses */
         IF ERROR-STATUS:ERROR THEN
         DO:
            RUN pCreateFileError(INPUT "System Error:" + ERROR-STATUS:GET-MESSAGE(1)).   
            UNDO ShipOrderBlk, LEAVE ShipOrderBlk.
         END.  
         
         /* CREATE TMS XML MSG */
         DATASET MercuryGateNotificationMessage:WRITE-XML("FILE", ttFileExport.FilePath).
         /* No errors were found, transaction successfully committed */
         logTransSuccessful = TRUE.
      
      END. /* DO TRANS ShipOrderBlk */
      
      /* Check if Transaction failed */
      IF NOT logTransSuccessful THEN
      DO:
         CREATE ttFileExportError.
         ASSIGN ttFileExportError.FileUploadErrorID = NEXT-VALUE(FileUploadError)
                ttFileExportError.FileID            = ttFileExport.FileID
                ttFileExportError.ErrorString       = "TRANSACTION NOT completed. Undoing All." NO-ERROR.
      END.
      
      /* Check if any errors occured during Main Transaction execution */
      IF CAN-FIND(FIRST ttFileExportError) THEN
      DO:
         /* Accumulate the ttFileExportErrors into one string for Emailing */
         chrFileExportError = "".
         FOR EACH ttFileExportError OF ttFileExport NO-LOCK:
            
            chrFileExportError = chrFileExportError + ttFileExportError.ErrorString + CHR(13) + CHR(10).

         END. /* FOR EACH ttFileExportError */
         
         RUN osSendMail.p (INPUT chrEmailAddress,                              /* Optional list of Users */
                           INPUT ttFileExport.FileName + " File Export Error", /* Email Subject */
                           INPUT chrFileExportError    + " from Cron.",        /* Plain text message Body */
                           INPUT "",                                           /* Html format message Body */
                           INPUT "",                                           /* File path ../files/file */
                           INPUT (IF chrEmailAddress = "" THEN
                                     FileMaster.InternalEmailGroupID
                                  ELSE 0),                                     /* EmailGroupID that you want to send this to */
                           INPUT FileMaster.FileMasterID).                     /* File MasterID is it applies */
         
         chrFileExportError = "".
         
         RUN pEmptyErrorTempTables.
         NEXT ShipOrderLoop.
               
      END. /*IF CAN-FIND(FIRST ttFileExportError) THEN*/
      
      /* Check if there is anything to move */
      IF SEARCH(ttFileExport.FilePath) <> ? THEN
      DO:
         /* If successfully completed then move from "tmp" folder back to outray for sending */
         chrReturnValue = fMoveFile(ttFileExport.FilePath, /* Source full file path */
                                    FileMaster.FilePath,   /* Target directory */
                                    ttFileExport.FileName, /* File Name without path */
                                    0) NO-ERROR.           /* Days to archive if applicable */
      END. /* IF SEARCH(ttFileExport.FilePath) <> ? */
      
      IF logGblDebugging THEN
      DO:
         fLog("Move from: " + ttFileExport.FilePath  + " to dir: " + FileMaster.FilePath +
              " FILE: "     + ttFileExport.FileName  + " chrReturnValue: " + chrReturnValue).
      END. /* IF logGblDebugging THEN */
      
      /* Check if moving process was successful */        
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
                           INPUT FileMaster.FileMasterID).                   /* File MasterID is it applies */
         
         IF logGblDebugging THEN
         DO:
            fLog(chrReturnValue).
         END.
         
      END. /* IF chrReturnValue BEGINS "Error" */
      
      /* Empty all errors from the tt */
      RUN pEmptyErrorTempTables.
      
   END. /* End of for each ShipOrder WHERE.. */ 
END. /* End of MainBlk */     

RELEASE FileMaster NO-ERROR.
RELEASE FileType   NO-ERROR.
RELEASE File       NO-ERROR.
RELEASE EmailGroup NO-ERROR.
RELEASE ShipOrder  NO-ERROR.

/**********************************************************************************************/      
/**************************************  - PROCEDURES -  **************************************/
/**********************************************************************************************/   

PROCEDURE pEmptyErrorTempTables:
   
   EMPTY TEMP-TABLE ttFileExportError NO-ERROR.
   EMPTY TEMP-TABLE ttFileExport      NO-ERROR.
   
END PROCEDURE. /* pEmptyErrorTempTables */   

PROCEDURE pCreateFileError:
   
   DEFINE INPUT PARAMETER chrErrorString AS CHARACTER NO-UNDO.
   
   CREATE ttFileExportError.
   ASSIGN ttFileExportError.FileUploadErrorID = NEXT-VALUE(FileUploadError)
          ttFileExportError.FileID            = ttFileExport.FileID
          ttFileExportError.ErrorString       = chrErrorString.
   
END PROCEDURE. /* pCreateFileError */

PROCEDURE pEmptyDynamicTTData:
   
      EMPTY TEMP-TABLE ttConsignee                NO-ERROR.
      EMPTY TEMP-TABLE ttConsigneeAddress         NO-ERROR.
      EMPTY TEMP-TABLE ttConsigneeContacts        NO-ERROR.
      EMPTY TEMP-TABLE ttConsigneeContact         NO-ERROR.
      EMPTY TEMP-TABLE ttConsigneeContactMethods  NO-ERROR.
      EMPTY TEMP-TABLE ttConsigneeContactMethod   NO-ERROR.
      EMPTY TEMP-TABLE ttItemGroup                NO-ERROR.
      EMPTY TEMP-TABLE ttQuantities               NO-ERROR.
      EMPTY TEMP-TABLE ttQuantity                 NO-ERROR.
      EMPTY TEMP-TABLE ttWeights                  NO-ERROR.
      EMPTY TEMP-TABLE ttWeight                   NO-ERROR.
      EMPTY TEMP-TABLE ttFreightClasses           NO-ERROR.
      EMPTY TEMP-TABLE ttFreightClass             NO-ERROR.
      EMPTY TEMP-TABLE ttItemReferenceNumber      NO-ERROR.
      EMPTY TEMP-TABLE ttItemReferenceNumbers     NO-ERROR.
      EMPTY TEMP-TABLE ttDimension                NO-ERROR.
      EMPTY TEMP-TABLE ttDimensions               NO-ERROR.
      EMPTY TEMP-TABLE ttLineItem                 NO-ERROR.
      EMPTY TEMP-TABLE ttContainedBy              NO-ERROR.
      EMPTY TEMP-TABLE ttEventAddress             NO-ERROR.
      EMPTY TEMP-TABLE ttEventContacts            NO-ERROR.
      EMPTY TEMP-TABLE ttEventContact             NO-ERROR.
      EMPTY TEMP-TABLE ttEventContactMethods      NO-ERROR.
      EMPTY TEMP-TABLE ttEventContactMethod       NO-ERROR.
   
END PROCEDURE. /* pEmptyDynamicTTData */
