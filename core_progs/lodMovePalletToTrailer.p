/*------------------------------------------------------------------------------------------------------------------------------------------
Program : lodMovePalletToTrailer.p   
Purpose : Load the Pallet and Packages to the Trailer

          Possible results : Continue

Author  : DCummins   
Date    : 27/03/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
23/04/2014 BR  CR 1052    UI Standardization
15/12/2014 CS  Nextel     Added releases.
15/04/2015 CS  Canontlb   Added cancelled status check for ShipOrderLine to skip updating statuses.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character SessionValue Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}

   /* Optional Includes */
   {fncStatusTypeFunctions.i}
   {fncDateFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i} 
   
   /* Buffers */
   DEFINE BUFFER ShipOrderPackage FOR ShipPackage.

   /* Local Variables */
   DEFINE VARIABLE intPalletLoaded             AS INTEGER NO-UNDO.
   DEFINE VARIABLE intCancelledShipOrderStatus AS INTEGER NO-UNDO.

   /* Session Objects */                      
   DEFINE VARIABLE intSsnShipPalletID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnOutboundID   AS sessionValue NO-UNDO.   
   DEFINE VARIABLE intSsnShipBayID    AS sessionValue NO-UNDO.   

   /* DB Objects */      
   DEFINE VARIABLE updShipPallet      AS updRecord.      
   DEFINE VARIABLE updShipPackage     AS updRecord.      
   DEFINE VARIABLE updShipOrder       AS updRecord.      
   DEFINE VARIABLE updShipOrderLine   AS updRecord.      
   DEFINE VARIABLE updOutbound        AS updRecord. 

   /* Get Current Data */
   intSsnShipPalletID          = fGetSessionValue("ShipPalletID").
   intSsnOutboundID            = fGetSessionValue("OutboundID").
   intSsnShipBayID             = fGetSessionValue("ShipBayID").
   intPalletLoaded             = fGetStatusID("Ship", "Loaded").
   intCancelledShipOrderStatus = fGetStatusID("ShipOrder", "Cancelled").

   Main_Block:
   DO ON ERROR UNDO:

      /* Get the Ship Pallet */
      FIND FIRST ShipPallet NO-LOCK /* idx=ShipPalletID */
         WHERE ShipPallet.ShipPalletID = intSsnShipPalletID:intValue NO-ERROR.
      IF NOT AVAILABLE ShipPallet THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "ShipPallet scanned does not exist on the system").
         LEAVE Main_Block.
      END. /*IF NOT AVAILABLE ShipPallet THEN*/
      
      FIND FIRST Outbound NO-LOCK /* idx=OutboundID */
         WHERE Outbound.OutboundID = intSsnOutboundID:intValue NO-ERROR.
      IF NOT AVAILABLE Outbound THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Outbound scanned does not exist on the system").
         LEAVE Main_Block.
      END. /*IF NOT AVAILABLE Outbound THEN*/
      
      FIND FIRST Carrier OF ShipPallet NO-LOCK NO-ERROR. /* idx=CarrierID */
      IF NOT AVAILABLE Carrier THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "Carrier of ShipPallet does not exist on the system").
         LEAVE Main_Block.
      END. /*IF NOT AVAILABLE Carrier THEN*/
      
      /* Check if DangerousGoodsLimit is reached */
      IF Outbound.DangerousGoodsTotal + ShipPallet.DangerousGoodsTotal > Carrier.DangerousGoodsLimit THEN 
      DO:
         RUN DisplayError("Dangerous Goods Limit Reached",
                          "Moving this ShipPallet will exceed the Dangerous Goods Limit for this trailer").
         LEAVE Main_Block.
      END. /*IF Outbound.DangerousGoodsTotal + ShipPallet.DangerousGoodsTotal > Carrier.DangerousGoodsLimit THEN*/
      
      /* Set the Outbound to Being Loaded */
      IF Outbound.OutboundStatusID = fGetStatusID("Outbound", "Attached") THEN
      DO:               
         updOutbound = fGetRecord("Outbound", intSsnOutboundID:intValue).
         
         /* Check for WebLocks */
         IF updOutbound:RecordLocked THEN
         DO:
            RUN DisplayError("Record Locked",
                          updOutbound:getErrors()).
            UNDO Main_Block, LEAVE Main_Block.
         END. /*IF updOutbound:RecordLocked THEN*/
         
         updOutbound:assignField("OutboundStatusID", fGetStatusID("Outbound", "BeingLoaded")).
         updOutbound:incrementField("DangerousGoodsTotal", ShipPallet.DangerousGoodsTotal).
         
         chrError = chrError + updOutbound:getErrors().

         DELETE OBJECT updOutbound NO-ERROR.
      END. /*IF Outbound.OutboundStatusID = fGetStatusID("Outbound", "Attached") THEN*/

      updShipPallet = fGetRecord("ShipPallet", ShipPallet.ShipPalletID).
      
      /* Check for WebLocks */
      IF updShipPallet:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                          updShipPallet:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END. /*IF updShipPallet:RecordLocked THEN*/
      
      updShipPallet:assignField("ShipStatusID", fGetStatusID("Ship", "Loaded")).      
      updShipPallet:assignField("OutboundID", intSsnOutboundID:intValue).         
      updShipPallet:assignField("LocationID", intSsnShipBayID:intValue).      

      chrError = chrError + updShipPallet:getErrors().

      /* Get the Ship Package */
      FOR EACH ShipPackage OF ShipPallet NO-LOCK: /* idx=ShipPalletID */

         updShipPackage = fGetRecord("ShipPackage", ShipPackage.ShipPackageID).
         
         /* Check for WebLocks */
         IF updShipPackage:RecordLocked THEN
         DO:
            RUN DisplayError("Record Locked",
                             updShipPackage:getErrors()).
            UNDO Main_Block, LEAVE Main_Block.
         END. /*IF updShipPackage:RecordLocked THEN*/
         
         updShipPackage:assignField("ShipStatusID", fGetStatusID("Ship", "Loaded")).         
         updShipPackage:assignField("OutboundID", intSsnOutboundID:intValue).         
         updShipPackage:assignField("LocationID", intSsnShipBayID:intValue).         

         chrError = chrError + updShipPackage:getErrors().

         DELETE OBJECT updShipPackage NO-ERROR.

      END. /*FOR EACH ShipPackage OF ShipPallet NO-LOCK:*/

      /* Check the Ship Order is fully Loaded before Changing Status */
      FOR EACH ShipPackage OF ShipPallet NO-LOCK: /* idx=ShipPalletID */
      
         IF NOT CAN-FIND(FIRST ShipOrderPackage NO-LOCK /* idx=ShipOrderID */
                            WHERE ShipOrderPackage.ShipOrderID = ShipPackage.ShipOrderID
                            AND   ShipOrderPackage.ShipPalletID <> ShipPallet.ShipPalletID
                            AND   ShipOrderPackage.ShipStatusID <> intPalletLoaded) THEN
         DO:     

            /* Update the Children */
            ShipOrderLine_Loop:
            FOR EACH ShipOrderLine NO-LOCK /* idx=ShipOrderPartPriority */
               WHERE ShipOrderLine.ShipOrderID = ShipPackage.ShipOrderID:
               
               /* Skip cancelled ShipOrderLines to prevent updating status */
               IF ShipOrderLine.ShipOrderStatusID = intCancelledShipOrderStatus THEN
                  NEXT ShipOrderLine_Loop.
               
               updShipOrderLine = fGetRecord("ShipOrderLine", ShipOrderLine.ShipOrderLineID).
               
               /* Check for WebLocks */
               IF updShipOrderLine:RecordLocked THEN
               DO:
                  RUN DisplayError("Record Locked",
                                   updShipOrderLine:getErrors()).
                  UNDO Main_Block, LEAVE Main_Block.
               END. /*IF updShipOrderLine:RecordLocked THEN*/
               
               updShipOrderLine:assignField("ShipOrderStatusID", fGetStatusID("ShipOrder", "Loaded")).

               chrError = chrError + updShipOrderLine:getErrors().

               DELETE OBJECT updShipOrderLine NO-ERROR. 
                          
            END. /*FOR EACH ShipOrderLine NO-LOCK*/

            updShipOrder = fGetRecord("ShipOrder", ShipPackage.ShipOrderID).
            
            /* Check for WebLocks */
            IF updShipOrder:RecordLocked THEN
            DO:
               RUN DisplayError("Record Locked",
                                updShipOrder:getErrors()).
               UNDO Main_Block, LEAVE Main_Block.
            END. /*IF updShipOrder:RecordLocked THEN*/
            
            updShipOrder:assignField("ShipOrderStatusID", fGetStatusID("ShipOrder", "Loaded")).
   
            chrError = chrError + updShipOrder:getErrors().
   
            DELETE OBJECT updShipOrder NO-ERROR.            
         END. /*IF NOT CAN-FIND*/
         ELSE DO:

            updShipOrder = fGetRecord("ShipOrder", ShipPackage.ShipOrderID).
            
            /* Check for WebLocks */
            IF updShipOrder:RecordLocked THEN
            DO:
               RUN DisplayError("Record Locked",
                                updShipOrder:getErrors()).
               UNDO Main_Block, LEAVE Main_Block.
            END. /*IF updShipOrder:RecordLocked THEN*/
            
            updShipOrder:assignField("ShipOrderStatusID", fGetStatusID("ShipOrder", "BeingLoaded")).
   
            chrError = chrError + updShipOrder:getErrors().
   
            DELETE OBJECT updShipOrder NO-ERROR.            
         END. /*ELSE DO:*/
         
      END. /*FOR EACH ShipPackage OF ShipPallet NO-LOCK:*/
      
      /* Check Errors */
      IF chrError <> "" THEN
      DO:
         RUN DisplayError("Update Error",
                          chrError).       
         UNDO Main_Block, LEAVE Main_Block.
      END. /*IF chrError <> "" THEN*/    
   
      RUN DisplayMessage("Ship Pallet Loaded",
                         "Ship Pallet " + ShipPallet.PalletRef + " has been Loaded.").

      chrResult = "Continue".

   END. /*Main_Block:*/
   
   /* Releases */
   RELEASE Outbound      NO-ERROR.
   RELEASE Carrier       NO-ERROR.
   RELEASE ShipPallet    NO-ERROR.
   RELEASE ShipPackage   NO-ERROR.
   RELEASE ShipOrderLine NO-ERROR.
   
   /* Clean Up */
   DELETE OBJECT intSsnShipPalletID NO-ERROR.   
   DELETE OBJECT intSsnShipBayID    NO-ERROR.   
   DELETE OBJECT intSsnOutboundID   NO-ERROR.   
   DELETE OBJECT updShipPallet      NO-ERROR.
   DELETE OBJECT updShipPackage     NO-ERROR.
   DELETE OBJECT updShipOrderLine   NO-ERROR.
   DELETE OBJECT updShipOrder       NO-ERROR.
   DELETE OBJECT updOutbound        NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /*DO ON STOP UNDO, RETRY*/
