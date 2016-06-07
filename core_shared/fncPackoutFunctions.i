/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncPackoutFunctions.i
Purpose : All functions to do with Packout
Author  : BG
Date    : 4th Sept 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
27/08/2014 BG  Ghd        Moved the piece that was exclusing the non Loose packages when a specific Tote was passed in. Originally these 
                          were excluded in the build up of the ttStockInTotes table but this caused anomolies when there were multiple
                          packages both loose and otherwise for multiple orders in the same tote.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Temp Tables */
DEFINE TEMP-TABLE ttStockForOrder NO-UNDO
   FIELD PartID             AS INTEGER
   FIELD QtyPacked          AS INTEGER
   FIELD QtyOrdered         AS INTEGER
   FIELD QtyToAssignToTotes AS INTEGER
   FIELD QtyAssignedToTotes AS INTEGER
   INDEX PartID IS UNIQUE PRIMARY PartID.

DEFINE TEMP-TABLE ttStockInTotes NO-UNDO
   FIELD ToteID             AS INTEGER
   FIELD ToteRef            AS CHARACTER
   FIELD LocationRef        AS CHARACTER
   FIELD PartID             AS INTEGER
   FIELD PartRef            AS CHARACTER
   FIELD EanCode            AS CHARACTER
   FIELD PartDescr          AS CHARACTER
   FIELD SerialisedPart     AS LOGICAL
   FIELD StockPackageID     AS INTEGER
   FIELD StockStatusID      AS INTEGER
   FIELD StockEntityID      AS INTEGER
   FIELD VersionID          AS INTEGER
   FIELD PackageRef         AS CHARACTER
   FIELD EntityName         AS CHARACTER
   FIELD EntityListingSeq   AS INTEGER
   FIELD QtyAssignedToOrder AS INTEGER
   FIELD QtyRemaining       AS INTEGER
   INDEX StockPackageID IS UNIQUE PRIMARY StockPackageID
   INDEX PartID PartID
   INDEX ToteLocationEntityListingSeq ToteRef LocationRef EntityListingSeq
   INDEX LocationToteEntityListingSeq LocationRef ToteRef EntityListingSeq.

DEFINE TEMP-TABLE ttOrderInTote NO-UNDO
   FIELD ShipOrderID       AS INTEGER 
   FIELD OrderRef          AS CHARACTER
   FIELD Priority          AS INTEGER
   FIELD Created           AS CHARACTER
   FIELD ShipOrderStatusID AS INTEGER 
   INDEX ShipOrderID IS UNIQUE PRIMARY ShipOrderID
   INDEX ShipOrderStatusID ShipOrderStatusID Priority DESC Created.

DEFINE TEMP-TABLE ttPriorityOrderFromTote NO-UNDO /* one record temp-table so no indexes */
   FIELD ShipOrderID   AS INTEGER 
   FIELD OrderRef      AS CHARACTER
   FIELD ShipPackageID AS INTEGER
   FIELD PackageRef    AS CHARACTER
   FIELD QtyToScan     AS INTEGER.

DEFINE TEMP-TABLE ttPartInTote NO-UNDO
   FIELD PartID  AS INTEGER
   FIELD PartRef AS CHARACTER
   INDEX PartID IS UNIQUE PRIMARY PartID.


FUNCTION fNextBoxNo RETURNS INTEGER (INPUT intShipOrderID AS INTEGER):
   
   DEFINE BUFFER readShipPackage FOR ShipPackage.
   
   BoxLoop:
   FOR EACH readShipPackage NO-LOCK 
      WHERE readShipPackage.ShipOrderID = intShipOrderID
      BY    readShipPackage.BoxNo DESC:
      
      LEAVE BoxLoop.
   END.
   
   IF AVAILABLE readShipPackage THEN
      RETURN readShipPackage.BoxNo + 1.
   ELSE 
      RETURN 1.
   
END FUNCTION. /* fNextBoxNo */


FUNCTION fGetStockRequiredForOrder RETURNS CHARACTER (INPUT intShipOrderID    AS INTEGER,
                                                      INPUT intSelectedToteID AS INTEGER):
   
   DEFINE VARIABLE intQtyOrdered              AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intQtyToAllocate           AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intToteUnAllocatedQty      AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intOrderUnAllocatedQty     AS INTEGER     NO-UNDO.
   
   DEFINE BUFFER   readShipOrder              FOR ShipOrder.
   DEFINE BUFFER   readShipOrderLine          FOR ShipOrderLine.
   DEFINE BUFFER   readShipOrderLineComponent FOR ShipOrderLineComponent.
   DEFINE BUFFER   readTote                   FOR Tote.
   DEFINE BUFFER   readTask                   FOR Task.
   DEFINE BUFFER   readTaskLine               FOR TaskLine.
   DEFINE BUFFER   readTaskLineWork           FOR TaskLineWork.
   DEFINE BUFFER   readTaskLineWorkToteLink   FOR TaskLineWorkToteLink.
   DEFINE BUFFER   readStockEntity            FOR StockEntity.
   DEFINE BUFFER   readLocation               FOR Location.
   DEFINE BUFFER   readPart                   FOR Part.
   DEFINE BUFFER   readStockPackage           FOR StockPackage.
   DEFINE BUFFER   totePackageStockEntity     FOR StockEntity.
   
   FIND FIRST readShipOrder NO-LOCK 
      WHERE readShipOrder.ShipOrderID = intShipOrderID NO-ERROR.
   
   IF NOT AVAILABLE readShipOrder THEN
      RETURN "Error: No ShipOrder exists for ID:[" + STRING(intShipOrderID) + "].".
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = readShipOrder.TaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(ShipOrder.TaskID) + "].".
   
   FIND FIRST totePackageStockEntity NO-LOCK 
      WHERE totePackageStockEntity.EntityCode = "TotePackage" NO-ERROR.
   IF NOT AVAILABLE totePackageStockEntity THEN
      RETURN "Error: No StockEntity exists for EntityCode:[TotePackage].".
   
   OrderLineLoop:
   FOR EACH readShipOrderLine OF readShipOrder NO-LOCK,
      EACH readPart OF readShipOrderLine NO-LOCK
         WHERE readPart.InventoryPart = TRUE: /* Be sure to exclude non-inventory Parts*/
         
         /*** Removed BG
         IF CAN-FIND(FIRST readShipOrderLineComponent OF readShipOrderLine) THEN
         DO:
            FOR EACH readShipOrderLineComponent OF readShipOrderLine NO-LOCK:
               
               FIND FIRST ttStockForOrder 
                  WHERE ttStockForOrder.PartID = readShipOrderLineComponent.PartID NO-ERROR.
               IF NOT AVAIL ttStockForOrder THEN
               DO:
                  CREATE ttStockForOrder.
                  ASSIGN ttStockForOrder.PartID = readShipOrderLineComponent.PartID.
               END.
               
               ASSIGN intQtyOrdered                      = readShipOrderLineComponent.QtyPerEach * readShipOrderLine.QtyOrdered
                      ttStockForOrder.QtyPacked          = ttStockForOrder.QtyPacked  + readShipOrderLineComponent.QtyPacked
                      ttStockForOrder.QtyOrdered         = ttStockForOrder.QtyOrdered + intQtyOrdered
                      ttStockForOrder.QtyToAssignToTotes = ttStockForOrder.QtyOrdered - ttStockForOrder.QtyPacked.
               
            END. /*FOR EACH readShipOrderLineComponent OF readShipOrderLine NO-LOCK:*/
             
            NEXT OrderLineLoop.
            
         END. /*IF CAN-FIND(FIRST readShipOrderLineComponent OF readShipOrderLine) THEN*/
         ***/
         
         FIND FIRST ttStockForOrder 
            WHERE ttStockForOrder.PartID = readShipOrderLine.PartID NO-ERROR.
         IF NOT AVAIL ttStockForOrder THEN
         DO:
            CREATE ttStockForOrder.
            ASSIGN ttStockForOrder.PartID = readShipOrderLine.PartID.
         END.
         
         ASSIGN ttStockForOrder.QtyPacked          = ttStockForOrder.QtyPacked  + readShipOrderLine.QtyPacked
                ttStockForOrder.QtyOrdered         = ttStockForOrder.QtyOrdered + readShipOrderLine.QtyOrdered
                ttStockForOrder.QtyToAssignToTotes = ttStockForOrder.QtyOrdered - ttStockForOrder.QtyPacked.
         
   END. /*FOR EACH readShipOrderLine OF ShipOrder NO-LOCK*/
   
   /* Get all of the Stock in all of the Totes that were used to Pick this Order */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkToteLink OF readTaskLineWork NO-LOCK,
            EACH readTote OF readTaskLineWorkToteLink NO-LOCK
            BREAK BY readTote.ToteID:
            
            /* Only want to look at each Tote once */
            IF FIRST-OF(readTote.ToteID) THEN
            DO:
               /* Might only want to see a single Tote */
               IF intSelectedToteID <> 0 AND readTote.ToteID <> intSelectedToteID THEN
                  NEXT ToteLoop.
               
               /* If the TaskLineWork has a specific ShipOrderID attached then we need to filter the wrong ones out here */
               IF readTaskLineWork.ShipOrderID <> 0 AND readTaskLineWork.ShipOrderID <> intShipOrderID THEN
                  NEXT ToteLoop.
               
               FIND FIRST readLocation OF readTote NO-LOCK NO-ERROR.
               
               PackageLoop:
               FOR EACH readStockPackage OF readTote NO-LOCK /*idx=readToteIDParentStockPackageID*/
                  WHERE readStockPackage.ParentStockPackageID = 0:
                  
                  /* Took this out as it was causing mismatches in Packout with Box stock and Loose stock. Packout now strips */
                  /* the non-loose stock out in JS instead of here - want consistent results for a specific Tote or all Totes */
                  /*IF intSelectedToteID <> 0 AND readStockPackage.StockEntityID <> totePackageStockEntity.StockEntityID THEN
                     NEXT PackageLoop.*/
                  
                  FIND FIRST readPart        OF readStockPackage NO-LOCK.
                  FIND FIRST readStockEntity OF readStockPackage NO-LOCK.
                  
                  CREATE ttStockInTotes.
                  ASSIGN ttStockInTotes.ToteID             = readTote.ToteID
                         ttStockInTotes.ToteRef            = readTote.ToteRef
                         ttStockInTotes.LocationRef        = IF AVAIL readLocation THEN readLocation.LocationRef ELSE "No Loc"
                         ttStockInTotes.PartID             = readPart.PartID
                         ttStockInTotes.PartRef            = readPart.PartRef
                         ttStockInTotes.EanCode            = readPart.EanCode
                         ttStockInTotes.SerialisedPart     = readPart.SerialisedPart
                         ttStockInTotes.PartDescr          = readPart.PartDescr
                         ttStockInTotes.StockPackageID     = readStockPackage.StockPackageID
                         ttStockInTotes.StockStatusID      = readStockPackage.StockStatusID
                         ttStockInTotes.VersionID          = readStockPackage.VersionID
                         ttStockInTotes.PackageRef         = readStockPackage.PackageRef
                         ttStockInTotes.StockEntityID      = readStockEntity.StockEntityID
                         ttStockInTotes.EntityName         = readStockEntity.EntityName
                         ttStockInTotes.EntityListingSeq   = readStockEntity.ListingSeq
                         ttStockInTotes.QtyAssignedToOrder = 0
                         ttStockInTotes.QtyRemaining       = readStockPackage.PackageQty.
                  
               END. /*FOR EACH StockPackage OF readTote NO-LOCK*/
               
            END. /*IF FIRST-OF(readTote.ToteID) THEN*/
            
   END. /*FOR EACH readTaskLineWork OF readTask NO-LOCK*/
   
   OrderUpdateLoop:
   FOR EACH ttStockForOrder:
      
      ToteUpdateLoop:
      FOR EACH ttStockInTotes /*idx=PartID*/
         WHERE ttStockInTotes.PartID = ttStockForOrder.PartID:
         
         IF ttStockForOrder.QtyToAssignToTotes >= ttStockInTotes.QtyRemaining THEN
            intQtyToAllocate = ttStockInTotes.QtyRemaining.
         ELSE
            intQtyToAllocate = ttStockForOrder.QtyToAssignToTotes.
         
         ASSIGN ttStockInTotes.QtyAssignedToOrder  = ttStockInTotes.QtyAssignedToOrder  + intQtyToAllocate
                ttStockInTotes.QtyRemaining        = ttStockInTotes.QtyRemaining        - intQtyToAllocate
                ttStockForOrder.QtyAssignedToTotes = ttStockForOrder.QtyAssignedToTotes + intQtyToAllocate
                ttStockForOrder.QtyToAssignToTotes = ttStockForOrder.QtyToAssignToTotes - intQtyToAllocate.
         
         /* Need to include these for the purposes of the Stock Qty calculations but here in the event that we're passed in */
         /* a specific Tote that means we only want loose stock to show in the Browser - so we deletet those entries here   */
         IF intSelectedToteID <> 0 AND ttStockInTotes.StockEntityID <> totePackageStockEntity.StockEntityID THEN
           DELETE ttStockInTotes.
         
         IF ttStockForOrder.QtyToAssignToTotes = 0 THEN
            LEAVE ToteUpdateLoop.
         
      END. /*FOR EACH ttStockInTotes*/
      
   END. /*readToteUpdateLoop: FOR EACH ttStockForOrder:*/
   
   RETURN "Ok".
   
END FUNCTION. /* fGetStockRequiredForOrder */
 

FUNCTION fWasToteUsedToPickOrder RETURNS CHARACTER (INPUT intSelectedToteID AS INTEGER,
                                                    INPUT intShipOrderID    AS INTEGER):
   
   DEFINE BUFFER readShipOrder            FOR ShipOrder.
   DEFINE BUFFER readTask                 FOR Task.
   DEFINE BUFFER readTaskLine             FOR TaskLine.
   DEFINE BUFFER readTaskLineWork         FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkToteLink FOR TaskLineWorkToteLink.
   DEFINE BUFFER readTote                 FOR Tote.
   DEFINE BUFFER checkTote                FOR Tote.
   
   FIND FIRST readShipOrder NO-LOCK 
      WHERE readShipOrder.ShipOrderID = intShipOrderID NO-ERROR.
   IF NOT AVAILABLE readShipOrder THEN
      RETURN "Error: No ShipOrder exists for ID:[" + STRING(intShipOrderID) + "].".
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = readShipOrder.TaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(ShipOrder.TaskID) + "].".
   
   FIND FIRST readTote NO-LOCK 
      WHERE readTote.ToteID = intSelectedToteID NO-ERROR.
   IF NOT AVAILABLE readTote THEN
      RETURN "Error: No Tote exists for ID:[" + STRING(intSelectedToteID) + "].".
   
   /* Get all of the Stock in all of the readTotes that were used to pick this Order */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkToteLink OF readTaskLineWork NO-LOCK,
            EACH checkTote OF readTaskLineWorkToteLink NO-LOCK
            BREAK BY checkTote.ToteID:
            
            /* Only want to look at each readTote once */
            IF FIRST-OF(checkTote.ToteID) THEN
            DO:
               IF checkTote.ToteID = intSelectedToteID THEN
                  RETURN "Yes".
               
            END. /*IF FIRST-OF(checkTote.ToteID) THEN*/
            
   END. /*FOR EACH readTaskLineWork OF readTask NO-LOCK*/
   
   RETURN "This Tote:[" + readTote.ToteRef + "] was not used to pick this Order, please try another.".
   
END FUNCTION. /* fWasToteUsedToPickOrder */


FUNCTION fOrdersPickedInTote RETURNS CHARACTER(INPUT intSelectedToteID           AS INTEGER,
                                               INPUT intSelectedPackoutStationID AS INTEGER):

   DEFINE BUFFER readTaskLineWorkToteLink       FOR TaskLineWorkToteLink.
   DEFINE BUFFER readTote                       FOR Tote.
   DEFINE BUFFER readTaskLineWork               FOR TaskLineWork.
   DEFINE BUFFER readTaskLine                   FOR TaskLine.
   DEFINE BUFFER readTask                       FOR Task.
   DEFINE BUFFER readShipOrder                  FOR ShipOrder.
   DEFINE BUFFER readShipOrderStream            FOR ShipOrderStream.
   DEFINE BUFFER readShipOrderStreamPackoutLink FOR ShipOrderStreamPackoutLink.
   DEFINE BUFFER readPickedStatus               FOR ShipOrderStatus.
   DEFINE BUFFER readBeingPackedOutStatus       FOR ShipOrderStatus.
   DEFINE BUFFER readPackoutOnHoldStatus        FOR ShipOrderStatus.
   
   FIND FIRST readTote NO-LOCK 
      WHERE readTote.ToteID = intSelectedToteID NO-ERROR.
   IF NOT AVAILABLE readTote THEN
      RETURN "Error: No Tote exists for ID:[" + STRING(intSelectedToteID) + "].".

   FIND FIRST readPickedStatus NO-LOCK 
      WHERE readPickedStatus.StatusCode = "Picked" NO-ERROR.
   IF NOT AVAILABLE readPickedStatus THEN 
      RETURN "Error: No ShipOrderStatus exists for 'Picked'".

   FIND FIRST readBeingPackedOutStatus NO-LOCK 
      WHERE readBeingPackedOutStatus.StatusCode = "BeingPackedOut" NO-ERROR.
   IF NOT AVAILABLE readBeingPackedOutStatus THEN 
      RETURN "Error: No ShipOrderStatus exists for 'BeingPackedOut'".

   FIND FIRST readPackoutOnHoldStatus NO-LOCK 
      WHERE readPackoutOnHoldStatus.StatusCode = "PackoutOnHold" NO-ERROR.
   IF NOT AVAILABLE readPackoutOnHoldStatus THEN 
      RETURN "Error: No ShipOrderStatus exists for 'PackoutOnHold'".

   EMPTY TEMP-TABLE ttOrderInTote.

   readTaskLineWorkToteLinkLoop:
   FOR EACH readTaskLineWorkToteLink OF readTote NO-LOCK,
      EACH readTaskLineWork OF readTaskLineWorkToteLink NO-LOCK,
         EACH readTaskLine OF readTaskLineWork NO-LOCK,
            EACH readTask OF readTaskLine NO-LOCK,
               EACH readShipOrder OF readTask NO-LOCK:

      IF readShipOrder.ShipOrderStatusID <> readPickedStatus.ShipOrderStatusID AND
         readShipOrder.ShipOrderStatusID <> readBeingPackedOutStatus.ShipOrderStatusID AND
         readShipOrder.ShipOrderStatusID <> readPackoutOnHoldStatus.ShipOrderStatusID THEN
         NEXT readTaskLineWorkToteLinkLoop.

      IF intSelectedPackoutStationID > 0 THEN
      DO:
         readShipOrderStreamLoop:
         FOR EACH readShipOrderStream OF readShipOrder NO-LOCK:
            FIND FIRST readShipOrderStreamPackoutLink OF readShipOrderStream NO-LOCK
               WHERE readShipOrderStreamPackoutLink.PackoutStationID = intSelectedPackoutStationID NO-ERROR.
            IF AVAILABLE readShipOrderStreamPackoutLink THEN
               LEAVE readShipOrderStreamLoop.
         END.
         IF NOT AVAILABLE readShipOrderStream THEN
            NEXT readTaskLineWorkToteLinkLoop.
      END.

      FIND FIRST ttOrderInTote NO-LOCK
         WHERE ttOrderInTote.ShipOrderID = readShipOrder.ShipOrderID NO-ERROR.

      IF NOT AVAILABLE ttOrderInTote THEN
      DO:
         CREATE ttOrderInTote.
         ASSIGN ttOrderInTote.ShipOrderID       = readShipOrder.ShipOrderID
                ttOrderInTote.OrderRef          = readShipOrder.OrderRef
                ttOrderInTote.Priority          = readShipOrder.Priority
                ttOrderInTote.Created           = readShipOrder.Created
                ttOrderInTote.ShipOrderStatusID = readShipOrder.ShipOrderStatusID.
      END.    
   END.

   RETURN "Ok".

END FUNCTION. /* fOrdersPickedInTote */


FUNCTION fGetPriorityOrderFromTote RETURNS CHARACTER (INPUT intSelectedToteID           AS INTEGER,
                                                      INPUT intSelectedPackoutStationID AS INTEGER,
                                                      INPUT intSelectedPartID           AS INTEGER):

   DEFINE BUFFER readTote                 FOR Tote.
   DEFINE BUFFER readPackoutStation       FOR PackoutStation.
   DEFINE BUFFER readPart                 FOR Part.
   DEFINE BUFFER readPickedStatus         FOR ShipOrderStatus.
   DEFINE BUFFER readBeingPackedOutStatus FOR ShipOrderStatus.
   DEFINE BUFFER readShipOrder            FOR ShipOrder.
   DEFINE BUFFER readShipOrderLine        FOR ShipOrderLine.
   
   DEFINE VARIABLE chrReturnValue AS CHARACTER NO-UNDO.

   FIND FIRST readPickedStatus NO-LOCK 
      WHERE readPickedStatus.StatusCode = "Picked" NO-ERROR.
   IF NOT AVAILABLE readPickedStatus THEN 
      RETURN "Error: No ShipOrderStatus exists for 'Picked'".

   FIND FIRST readBeingPackedOutStatus NO-LOCK 
      WHERE readBeingPackedOutStatus.StatusCode = "BeingPackedOut" NO-ERROR.
   IF NOT AVAILABLE readBeingPackedOutStatus THEN 
      RETURN "Error: No ShipOrderStatus exists for 'BeingPackedOut'".
      
   FIND FIRST readTote NO-LOCK 
      WHERE readTote.ToteID = intSelectedToteID NO-ERROR.
   IF NOT AVAILABLE readTote THEN
      RETURN "Error: No Tote exists for ID:[" + STRING(intSelectedToteID) + "].".

   FIND FIRST readPackoutStation NO-LOCK 
      WHERE readPackoutStation.PackoutStationID = intSelectedPackoutStationID NO-ERROR.
   IF NOT AVAILABLE readPackoutStation THEN
      RETURN "Error: No PackoutStation exists for ID:[" + STRING(intSelectedPackoutStationID) + "].".

   FIND FIRST readPart NO-LOCK 
      WHERE readPart.PartID = intSelectedPartID NO-ERROR.
   IF NOT AVAILABLE readPart THEN
      RETURN "Error: No Part exists for ID:[" + STRING(intSelectedPartID) + "].".

   /* Function call to above to get the temp-table created */
   chrReturnValue = fOrdersPickedInTote(readTote.ToteID,readPackoutStation.PackoutStationID).
   IF chrReturnValue <> "Ok" THEN
      RETURN "Error: fOrdersPickedInTote resulted in an error!".
   
   ttOrderInToteLoop:   
   FOR EACH ttOrderInTote NO-LOCK
         BY ttOrderInTote.Priority DESC
         BY ttOrderInTote.Created:
      IF ttOrderInTote.ShipOrderStatusID <> readPickedStatus.ShipOrderStatusID AND
         ttOrderInTote.ShipOrderStatusID <> readBeingPackedOutStatus.ShipOrderStatusID THEN
         NEXT ttOrderInToteLoop.
      FIND FIRST readShipOrderLine NO-LOCK 
         WHERE readShipOrderLine.ShipOrderID = ttOrderInTote.ShipOrderID
         AND readShipOrderLine.PartID = intSelectedPartID NO-ERROR.
      IF NOT AVAILABLE readShipOrderLine OR 
         readShipOrderLine.QtyOrdered <= readShipOrderLine.QtyPacked THEN
         NEXT ttOrderInToteLoop.
      LEAVE ttOrderInToteLoop.
   END.
   
   IF AVAILABLE ttOrderInTote THEN
   DO:
      FIND FIRST readShipOrder NO-LOCK
         WHERE readShipOrder.ShipOrderID = ttOrderInTote.ShipOrderID NO-ERROR. 
         
      IF AVAILABLE readShipOrder THEN 
      DO:
         CREATE ttPriorityOrderFromTote.
         ASSIGN ttPriorityOrderFromTote.ShipOrderID = readShipOrder.ShipOrderID
                ttPriorityOrderFromTote.OrderRef    = readShipOrder.OrderRef
                ttPriorityOrderFromTote.QtyToScan   = readShipOrderLine.QtyOrdered.
      END.
   END.
   
   RETURN "Ok".
   
END FUNCTION. /* fGetPriorityOrderFromTote */

FUNCTION fPartsInTote RETURNS CHARACTER(INPUT intSelectedToteID AS INTEGER):
   
   DEFINE BUFFER readTote                 FOR Tote.
   DEFINE BUFFER readStockTotePackageLink FOR StockTotePackageLink.
   DEFINE BUFFER readStockPackage         FOR StockPackage.
   DEFINE BUFFER readPart                 FOR Part.
   
   FIND FIRST readTote NO-LOCK 
      WHERE readTote.ToteID = intSelectedToteID NO-ERROR.
   IF NOT AVAILABLE readTote THEN
      RETURN "Error: No Tote exists for ID:[" + STRING(intSelectedToteID) + "].".

   EMPTY TEMP-TABLE ttOrderInTote.
   /* Add Loose stock to the list of parts in Tote */
   FOR EACH readStockTotePackageLink OF readTote NO-LOCK,
      EACH readStockPackage OF readStockTotePackageLink NO-LOCK,
        EACH readPart OF readStockPackage NO-LOCK:

      FIND FIRST ttPartInTote NO-LOCK
         WHERE ttPartInTote.PartID = readPart.PartID NO-ERROR.
      IF NOT AVAILABLE ttPartInTote THEN
      DO:
         CREATE ttPartInTote.
         ASSIGN ttPartInTote.PartID  = readPart.PartID
                ttPartInTote.PartRef = readPart.PartRef.
      END.
   END.
   /* Add parts in stock entities to list of parts in tote */
   FOR EACH readStockPackage OF readTote NO-LOCK,
        EACH readPart OF readStockPackage NO-LOCK:

      FIND FIRST ttPartInTote NO-LOCK
         WHERE ttPartInTote.PartID = readPart.PartID NO-ERROR.
      IF NOT AVAILABLE ttPartInTote THEN
      DO:
         CREATE ttPartInTote.
         ASSIGN ttPartInTote.PartID  = readPart.PartID
                ttPartInTote.PartRef = readPart.PartRef.
      END.
   END.
   
   RETURN "Ok".
   
END FUNCTION. /* fPartsInTote */

