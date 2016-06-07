/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getReceivingOptions.i
Purpose : This sets a group of variables that will be used in receiving both in Character and Web enviroments. Included in all programs 
          in receiving.
Author  : BG
Date    : 4th May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/06/2015 ND  Canon      Changed database call to include config table not the porcessoption table. Removed unsed variables/code.
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on defSessionVariables.i */

/* Are we using Receiving POs between the Cutomer and the Vendor? */
DEFINE VARIABLE logUsingReceivingPOs                  AS LOGICAL     NO-UNDO. 

/* Can we assign more than 1 Asn to an Inbound Load or is it always one-to-one? */
DEFINE VARIABLE logUsingMultiAsns                     AS LOGICAL     NO-UNDO. 

/* Are we using Customs features for a Bonded warehouse? */
DEFINE VARIABLE logUsingCustoms                       AS LOGICAL     NO-UNDO.

/* Are we allowed to release reconciled Stock from an Inbound before the full Inbound has been Completed? */
DEFINE VARIABLE logAllowPartialRelease                AS LOGICAL     NO-UNDO.

/* Are we allowed to receive more Stock that the Qty detailed on the Asn? */
DEFINE VARIABLE logAllowOverReceiving                 AS LOGICAL     NO-UNDO.

/* Are we allowed to complete an Inbound having received less Stock that the Qty detailed on the Asn? */
DEFINE VARIABLE logAllowUnderReceiving                AS LOGICAL     NO-UNDO.

/* If Overs are allowed then - Does an operator need prior approval from a Supervisor to create Overs? */
DEFINE VARIABLE logNeedApprovalForOvers               AS LOGICAL     NO-UNDO.

/* Will we Consolidate all AsnLines for a Part into a single TaskWork at Receiving? */
DEFINE VARIABLE logConsolidateAsnLines                AS LOGICAL     NO-UNDO.

/* Should the system print floor sheets when an Inbound trailer is attached to a Bay? */
DEFINE VARIABLE logPrintFloorSheetsAtInbound          AS LOGICAL     NO-UNDO.

/* When Parent Building in Receiving, should the system display a message to direct the User to the Parent after every child scan */
/* e.g. "Open Pallet:Pal0123 exists for Part A please build this Box onto it."*/
DEFINE VARIABLE logDirectUserToOpenParent             AS LOGICAL     NO-UNDO.

/* Using FAI Functionality to inspect Parts before Picking begins? */
DEFINE VARIABLE logUsingFaiFunctionality              AS LOGICAL NO-UNDO.

/* If Part is AwaitingFai move its Pickable Stock to AwaitingFai? */
DEFINE VARIABLE logPutStockOnHoldDuringFai            AS LOGICAL NO-UNDO.

/* Allow release of Stock of Reconciled Part B4 Inbound Completed? */
DEFINE VARIABLE logAllowPartialReconcile              AS LOGICAL NO-UNDO.

/* Put Stock on hold after Inbound @ AwaitingRelease status? */
DEFINE VARIABLE logPutStockOnHoldAfterInbound         AS LOGICAL NO-UNDO.

/* Allow Manual release of Stock @ AwaitingRelease after Inbound? */
DEFINE VARIABLE logManuallyReleaseStockAfterInbound   AS LOGICAL NO-UNDO.

/* Should Serials be set to Active = YES when Created? */
DEFINE VARIABLE logActivateSerialsOnCreation          AS LOGICAL NO-UNDO.

/* Use the Customer's Package Label at Inbound instead of our own? */
DEFINE VARIABLE logUseCustomerPackageLabelAtInbound  AS LOGICAL NO-UNDO.

FIND FIRST InboundConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE InboundConfig THEN
DO:
   chrError = chrError + "No InboundConfig exist. Please create InboundConfig record.".
END.
ELSE
DO:
   ASSIGN logUsingReceivingPOs                  = InboundConfig.UsingReceivingPOs
          logUsingMultiAsns                     = InboundConfig.UsingMultiAsns
          logUsingCustoms                       = InboundConfig.UsingCustoms
          logAllowPartialRelease                = InboundConfig.AllowPartialRelease
          logAllowOverReceiving                 = InboundConfig.AllowOverReceiving
          logAllowUnderReceiving                = InboundConfig.AllowUnderReceiving
          logConsolidateAsnLines                = InboundConfig.ConsolidateAsnLinesAtReceiving
          logPrintFloorSheetsAtInbound          = InboundConfig.PrintFloorSheetsAtInbound
          logUsingFaiFunctionality              = InboundConfig.UsingFaiFunctionality
          logPutStockOnHoldDuringFai            = InboundConfig.PutStockOnHoldDuringFai
          logAllowPartialReconcile              = InboundConfig.AllowPartialReconcile
          logPutStockOnHoldAfterInbound         = InboundConfig.PutStockOnHoldAfterInbound
          logManuallyReleaseStockAfterInbound   = InboundConfig.ManuallyReleaseStockAfterInbound
          logActivateSerialsOnCreation          = InboundConfig.ActivateSerialsOnCreation
          logUseCustomerPackageLabelAtInbound   = InboundConfig.UseCustomerPackageLabelAtInbound.
END. 


