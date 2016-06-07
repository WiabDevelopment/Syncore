/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncPickingFunctions.i
Purpose : All functions to do with Picking
Author  : BG
Date    : 29th May 2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttStockAllocatedToOrder NO-UNDO
   FIELD ShipOrderID        AS INTEGER
   FIELD PartID             AS INTEGER
   FIELD TaskID             AS INTEGER
   FIELD QtyAllocated       AS INTEGER
   INDEX ShipOrderIDPartID  IS PRIMARY UNIQUE ShipOrderID PartID
   INDEX PartID PartID.


FUNCTION fGetOrdersAllocatedToTaskLineWork RETURNS CHARACTER (INPUT intTaskLineWorkID AS INTEGER):
   
   DEFINE VARIABLE chrShipOrdersList          AS CHARACTER   NO-UNDO.
   
   DEFINE BUFFER   readShipOrder              FOR ShipOrder.
   DEFINE BUFFER   readShipOrderLine          FOR ShipOrderLine.
   DEFINE BUFFER   readTask                   FOR Task.
   DEFINE BUFFER   readTaskLine               FOR TaskLine.
   DEFINE BUFFER   readTaskLineWork           FOR TaskLineWork.
   
   FIND FIRST readTaskLineWork NO-LOCK 
      WHERE readTaskLineWork.TaskLineWorkID = intTaskLineWorkID NO-ERROR.
   IF NOT AVAILABLE readTaskLineWork THEN
      RETURN "Error: No TaskLineWorkID exists for ID:[" + STRING(intTaskLineWorkID) + "].".
   
   FIND FIRST readTaskLine OF readTaskLineWork NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTaskLine THEN
      RETURN "Error: No TaskLine exists for ID:[" + STRING(readTaskLineWork.TaskLineID) + "].".
   
   FIND FIRST readTask OF readTaskLine NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(readTaskLine.TaskID) + "].".
   
   FOR EACH readShipOrder NO-LOCK
      WHERE readShipOrder.TaskID = readTask.TaskID:
      
      FOR EACH readShipOrderLine OF readShipOrder NO-LOCK 
         WHERE readShipOrderLine.PartID = readTaskLine.PartID:
         
         FIND FIRST ttStockAllocatedToOrder  /*idx=ShipOrderIDPartID*/
            WHERE ttStockAllocatedToOrder.ShipOrderID = readShipOrder.ShipOrderid
            AND   ttStockAllocatedToOrder.PartID      = readTaskLine.PartID NO-ERROR.
         IF NOT AVAILABLE ttStockAllocatedToOrder THEN
         DO:
            CREATE ttStockAllocatedToOrder.
            ASSIGN ttStockAllocatedToOrder.ShipOrderID  = readShipOrder.ShipOrderid
                   ttStockAllocatedToOrder.PartID       = readTaskLine.PartID.
         END.
         ttStockAllocatedToOrder.QtyAllocated = ttStockAllocatedToOrder.QtyAllocated + readShipOrderLine.QtyOrdered.  
         
      END. /*FOR EACH readShipOrderLine OF readShipOrder NO-LOCK */
      
      IF CAN-FIND(FIRST readShipOrderLine OF readShipOrder NO-LOCK 
                     WHERE readShipOrderLine.PartID = readTaskLine.PartID) THEN
      DO:
         chrShipOrdersList = chrShipOrdersList + ",".
      END.
   END.
   chrShipOrdersList = RIGHT-TRIM(chrShipOrdersList,",").
   
   RETURN chrShipOrdersList.
   
END FUNCTION.



/*
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
   FIELD VersionID          AS INTEGER
   FIELD PackageRef         AS CHARACTER
   FIELD EntityName         AS CHARACTER
   FIELD EntityListingSeq   AS INTEGER
   FIELD QtyAssignedToOrder AS INTEGER
   FIELD QtyRemaining       AS INTEGER
   INDEX StockPackageID is UNIQUE PRIMARY StockPackageID
   INDEX PartID PartID
   INDEX ToteLocationEntityListingSeq ToteRef LocationRef EntityListingSeq
   INDEX LocationToteEntityListingSeq LocationRef ToteRef EntityListingSeq.


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
   FOR EACH readShipOrderLine OF readShipOrder NO-LOCK:
      
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
               
               FIND readLocation OF readTote NO-LOCK NO-ERROR.
               
               PackageLoop:
               FOR EACH readStockPackage OF readTote NO-LOCK /*idx=readToteIDParentStockPackageID*/
                  WHERE readStockPackage.ParentStockPackageID = 0:
                  
                  IF intSelectedToteID <> 0 AND readStockPackage.StockEntityID <> totePackageStockEntity.StockEntityID  THEN
                     NEXT PackageLoop.
                  
                  FIND readPart        OF readStockPackage NO-LOCK.
                  FIND readStockEntity OF readStockPackage NO-LOCK.
                  
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
         
         IF ttStockForOrder.QtyToAssignToTotes = 0 THEN
            LEAVE ToteUpdateLoop.
         
      END. /*FOR EACH ttStockInTotes*/
      
   END. /*readToteUpdateLoop: FOR EACH ttStockForOrder:*/
   
   RETURN "Ok".
   
END FUNCTION. /* fGetStockInTotes */
 
*/
