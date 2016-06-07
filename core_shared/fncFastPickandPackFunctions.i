/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncFastPickandPackFunctions.i
Purpose : All functions to do with Fast Pick and Pack
Author  : 
Date    : 3rd Apr 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
02/07/2015 TAW CanonTLB	  Include Part.EanCode in the ttPickInfo, and return when finding picks
06/07/2015 CS  Canon      Added new function fGetShipOrderPriority().
7/27/2015  LT  Canon      Remove the Part.Serialised serial scan required check
------------------------------------------------------------------------------------------------------------------------------------------*/
/* Dependencies */
/*{defSessionVariables.i}   */
/*{fncGlobalFunctions.i}    */
/*{fncStatusTypeFunctions.i}*/
/*{fncLoggingFunctions.i}   */

/* Temp Tables */
DEFINE TEMP-TABLE ttPartCheck NO-UNDO
   FIELD PartID LIKE Part.PartID
   INDEX idxPart AS PRIMARY UNIQUE PartID.

/* No need to create an index, there should alway be one record */
DEFINE TEMP-TABLE ttPickInfo NO-UNDO 
   FIELD PartID                LIKE Part.PartID
   FIELD PartRef               LIKE Part.PartRef
   FIELD PartEAN               LIKE Part.EanCode
   FIELD LocationID            LIKE Location.LocationID
   FIELD LocationRef           LIKE Location.LocationRef
   FIELD AvailQtyToPick          AS INTEGER
   FIELD ShipOrderStreamID     LIKE ShipOrder.ShipOrderStreamID
   FIELD BusinessUnitID        LIKE ShipOrder.BusinessUnitID
   FIELD CarrierSortationID    LIKE ShipOrder.CarrierSortationID
   FIELD Priority              LIKE ShipOrder.Priority
   FIELD StockStatusID         LIKE StockStatus.StockStatusID
   FIELD MinutesAwayFromPickUp   AS INTEGER
   FIELD MaxPriorityVariance     AS INTEGER
   FIELD MaxNumOrdersToPick      AS INTEGER.
   
DEFINE TEMP-TABLE ttPartEntityStatus NO-UNDO
   FIELD PartID        LIKE Part.PartID 
   FIELD StockEntityID LIKE StockEntity.StockEntityID
   FIELD StockStatusID LIKE StockStatus.StockStatusID
   FIELD AvailQty        AS INTEGER
   INDEX idxPartEntityStatus PartID StockEntityID StockStatusID.    

DEFINE TEMP-TABLE ttShipOrderToGenerate NO-UNDO
   FIELD ShipOrderID       LIKE ShipOrder.ShipOrderID
   FIELD PartID            LIKE Part.PartID
   FIELD LocationID        LIKE Location.LocationID
   FIELD QtyOrdered          AS INTEGER    
   INDEX idxShipOrder AS PRIMARY UNIQUE ShipOrderID.
   
      
/* This function Convert Carrier PickUpTime from character to integer for calculations but store
   the integer value as a string.  In the calling program, check if it begins with Error */
FUNCTION fConvertCarrierPickUpTime RETURNS CHARACTER (INPUT intCarrierSortationID AS INTEGER):
   
   /* Local Variables */
   DEFINE VARIABLE chrErrorString AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intCounter     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intHour        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intMinute      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intPickUpTime  AS INTEGER   NO-UNDO. /* Value representing number of seconds since midnight */
   
   /* Buffers */
   DEFINE BUFFER readCarrierSortation FOR CarrierSortation.
      
   FIND FIRST readCarrierSortation NO-LOCK /* idx=CarrierSortationID */
      WHERE readCarrierSortation.CarrierSortationID = intCarrierSortationID NO-ERROR.
   IF NOT AVAILABLE readCarrierSortation THEN
      RETURN "Fatal Error: No CarrierSortation exists for ID: [" + STRING(intCarrierSortationID) + "].".      
      
   ASSIGN intHour       = INTEGER(ENTRY(1, readCarrierSortation.PickUpTime, ":"))
          intMinute     = INTEGER(ENTRY(2, readCarrierSortation.PickUpTime, ":"))
          intPickUpTime = (intHour * 60 * 60) + (intMinute * 60) NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN
   DO: 
      DO intCounter = 1 TO ERROR-STATUS:NUM-MESSAGES:
         chrErrorString = chrErrorString + STRING(ERROR-STATUS:GET-MESSAGE(intCounter)).
      END.
      RETURN "Fatal Error: " + chrErrorString.
      
   END. /* IF ERROR-STATUS:ERROR */          
       
   RETURN STRING(intPickUpTime).            
   
END FUNCTION. /* fConvertCarrierPickUpTime */


/* Use fConvertCarrierPickUpTime to obtain the integer value of the PickUpTime before using this function */
FUNCTION fMinutesAwayFromPickUpTime RETURNS INTEGER (INPUT intPickUpTime AS INTEGER):

   /* Local Variables */
   DEFINE VARIABLE datCurrentTime         AS DATETIME NO-UNDO.
   DEFINE VARIABLE datPickUpTime          AS DATETIME NO-UNDO.   
   DEFINE VARIABLE intDifferenceInMinutes AS INTEGER  NO-UNDO.   
   
   ASSIGN datCurrentTime         = NOW
          datPickUpTime          = DATETIME(TODAY, (intPickUpTime * 1000))             
          intDifferenceInMinutes = INTERVAL(datPickUpTime, datCurrentTime, "Minutes").
          
   RETURN intDifferenceInMinutes.       
          
END FUNCTION. /* fMinutesAwayFromPickUpTime */


/* This function create a temp-table record to store current Generate related data. */
FUNCTION fCreatePickInfo RETURNS CHARACTER (INPUT intFastPickPackStationID AS INTEGER,
                                            INPUT intShipOrderStatusID     AS INTEGER,
                                            INPUT chrPickTaskTypeList      AS CHARACTER,
                                            INPUT chrPickableStatusList    AS CHARACTER,
                                            INPUT intMaxOrdersPerTask      AS INTEGER):
   
   /* Local Variables */
   DEFINE VARIABLE intAllocatedQty                   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intCounter                        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intMaxPriorityVariation           AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intMinutesFromPickUpTime          AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intTotalAvailableQty              AS INTEGER   NO-UNDO.
   DEFINE VARIABLE chrPickUpTime                     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE logAvailPartInProcessableLocation AS LOGICAL   NO-UNDO.
   
   /* Buffers */
   DEFINE BUFFER otherShipOrder                  FOR ShipOrder.
   DEFINE BUFFER otherShipOrderLine              FOR ShipOrderLine.  
   DEFINE BUFFER readCarrierSortation            FOR CarrierSortation. 
   DEFINE BUFFER readLocation                    FOR Location.
   DEFINE BUFFER readPart                        FOR Part.
   DEFINE BUFFER readPartType                    FOR PartType.   
   DEFINE BUFFER readPickPackWorkZoneLink        FOR PickPackWorkZoneLink.   
   DEFINE BUFFER readShipOrder                   FOR ShipOrder.
   DEFINE BUFFER readShipOrderLine               FOR ShipOrderLine.   
   DEFINE BUFFER readShipOrderStreamPickPackLink FOR ShipOrderStreamPickPackLink.
   DEFINE BUFFER readStockPackage                FOR StockPackage.
   DEFINE BUFFER readTask                        FOR Task.
   DEFINE BUFFER readTaskLine                    FOR TaskLine.
   DEFINE BUFFER readTaskLineWork                FOR TaskLineWork.
   
   /* ----- Begins Logic ----- */
   
   EMPTY TEMP-TABLE ttPartCheck        NO-ERROR.      
   EMPTY TEMP-TABLE ttPickInfo         NO-ERROR.
   
   IF logGblDebugging THEN
   DO:
      fLog("--- FIND FIRST ORDER TO PROCESS FROM ORDER QUEUE ---").
      fLog("In fCreatePickInfo --> StationID: " + STRING(intFastPickPackStationID)
         + " ShipOrderStatus: "                 + STRING(intShipOrderStatusID)
         + " TaskTypeList: "                    + chrPickTaskTypeList
         + " PickableStatus: "                  + chrPickableStatusList
         + " Default Max Orders Per Task: "     + STRING(intMaxOrdersPerTask)).
   END.      
                        
   /* Obtain the ShipOrder that is processable for selected PickPackStation, sorted by DateToShip, Priority
      and ShipOrderID.  Need to do an inner join here so that we won't loop through each stream at a time */
   OrderStationLoop:
   FOR EACH readShipOrderStreamPickPackLink NO-LOCK /* idx=PickPackStationID */
      WHERE readShipOrderStreamPickPackLink.PickPackStationID = intFastPickPackStationID
      AND   readShipOrderStreamPickPackLink.Active,      
         EACH readShipOrder NO-LOCK /* idx=ShipOrderStreamID, TaskID */
            WHERE readShipOrder.ShipOrderStreamID = readShipOrderStreamPickPackLink.ShipOrderStreamID
            AND   readShipOrder.ShipOrderStatusID = intShipOrderStatusID
            AND   readShipOrder.TaskID            = 0
            BY readShipOrder.DateToShip 
            BY readShipOrder.Priority DESCENDING 
            BY readShipOrder.ShipOrderID:  

      /* Obtain CarrierSortation to Check PickUpTime */      
      FIND FIRST readCarrierSortation OF readShipOrder NO-LOCK NO-ERROR.
      IF NOT AVAILABLE readCarrierSortation THEN
      DO:
         IF logGblDebugging THEN
            fLog("CarrierSortationID: " + STRING(readShipOrder.CarrierSortationID) 
               + " does not exists for OrderRef: " + readShipOrder.OrderRef
               + " skipping this Order.").
         NEXT OrderStationLoop.
      END.   
      
      IF logGblDebugging THEN
         fLog("ShipOrderStreamID: "   + STRING(readShipOrder.ShipOrderStreamID)
            + " CarrierSortationID: " + STRING(readShipOrder.CarrierSortationID)
            + " ShipOrder Ref: "      + readShipOrder.OrderRef
            + " DateToShip: "         + STRING(DateToShip)
            + " PickUp Time: "        + readCarrierSortation.PickUpTime             
            + " Order Priority: "     + STRING(readShipOrder.Priority)
            + " Current Time: "       + STRING(TIME, "HH:MM:SS")).
                  
      /* Convert Carrier PickUp */   
      chrPickUpTime = fConvertCarrierPickUpTime(INPUT readCarrierSortation.CarrierSortationID). 
      IF chrPickUpTime BEGINS "Fatal" THEN
      DO:
         IF logGblDebugging THEN
            fLog("Error converting PickUpTime for CarrierSortationID: " + STRING(readShipOrder.CarrierSortationID)
               + " " + chrPickUpTime + " skipping this order.").
         NEXT OrderStationLoop.
      END.
      
      ASSIGN intMinutesFromPickUpTime = fMinutesAwayFromPickUpTime(INPUT INTEGER(chrPickUpTime))
             intMaxPriorityVariation  = MAXIMUM(readShipOrder.Priority - intMinutesFromPickUpTime, 0).
            
      /* a. If readShipOrder.DateToShip < Today then we should process the order regardless of 
            Carrier Pickup Time 
         b. If readShipOrder.DateToShip > Today then all orders for Today were processed so
            we should process order according to the Priority */           
      IF (TIME >= INTEGER(chrPickUpTime) AND readShipOrder.DateToShip = TODAY) THEN
      DO:
         IF logGblDebugging THEN
            fLog("Order: " + readShipOrder.OrderRef + " should be shipped today but is past carrier pickup time.").
            
         /* Only skip if have no more order with a later PickUp time for Today */
         IF CAN-FIND(FIRST otherShipOrder NO-LOCK
            WHERE otherShipOrder.ShipOrderStreamID = readShipOrder.ShipOrderStreamID
            AND   otherShipOrder.DateToShip        = readShipOrder.DateToShip
            AND   otherShipOrder.ShipOrderStatusID = readShipOrder.ShipOrderStatusID
            AND   otherShipOrder.BusinessUnitID    = readShipOrder.BusinessUnitID 
            AND   otherShipOrder.Priority          < readShipOrder.Priority) THEN
         DO:   
            IF logGblDebugging THEN
            DO:
               fLog("CANNOT process Order: " + readShipOrder.OrderRef 
                  + ", there are other orders that should be shipped today with a later carrier pickup.").

               FIND FIRST otherShipOrder NO-LOCK
                  WHERE otherShipOrder.ShipOrderStreamID = readShipOrder.ShipOrderStreamID
                  AND   otherShipOrder.DateToShip        = readShipOrder.DateToShip
                  AND   otherShipOrder.ShipOrderStatusID = readShipOrder.ShipOrderStatusID
                  AND   otherShipOrder.BusinessUnitID    = readShipOrder.BusinessUnitID 
                  AND   otherShipOrder.Priority          < readShipOrder.Priority NO-ERROR.
                   
               IF AVAILABLE otherShipOrder THEN
                  fLog("Caused by OrderRef: "   + otherShipOrder.OrderRef  
                     + " Priority: "            + STRING(otherShipOrder.Priority)
                     + " CarrierSortationID: "  + STRING(otherShipOrder.CarrierSortationID)).
                                                                           
            END. /* IF logGblDebugging */
         
            NEXT OrderStationLoop.
         END.
         
         IF logGblDebugging THEN
            fLog("CAN process Order: " + readShipOrder.OrderRef + ", all orders with a later carrier pickup were processed.").                     
      END.    
                          
      /* Check that there is only one ShipOrderLine at ReadyToProcess status */
      intCounter = 0.
      FOR EACH readShipOrderLine OF readShipOrder NO-LOCK
         WHERE readShipOrderLine.ShipOrderStatusID = readShipOrder.ShipOrderStatusID:
         
         intCounter = intCounter + 1.
         
      END. /* FOR EACH readShipOrderLine */      
      
      IF intCounter <> 1 THEN
      DO:
         IF logGblDebugging THEN
            fLog("ShipOrderLine check failed for OrderRef " + readShipOrder.OrderRef).
            
         NEXT OrderStationLoop.
      END.     
      
      FIND FIRST readShipOrderLine OF readShipOrder 
         WHERE readShipOrderLine.ShipOrderStatusID = readShipOrder.ShipOrderStatusID NO-LOCK NO-ERROR. /* idx=ShipOrderPartPriority */
      IF NOT AVAILABLE readShipOrderLine THEN
         NEXT OrderStationLoop.
         
      /* Check that the StockStatus of ShipOrderLine is pickable Stock Status */
      IF NOT CAN-DO(chrPickableStatusList, STRING(readShipOrderLine.StockStatusID)) THEN
      DO:
         IF logGblDebugging THEN
            fLog("ShipOrderLine StockStatus check failed for OrderRef " + readShipOrder.OrderRef).
            
         NEXT OrderStationLoop.         
      END.   
                                    
      FIND FIRST readPart OF readShipOrderLine NO-LOCK NO-ERROR. /* idx=PartID */
      IF NOT AVAILABLE readPart THEN
      DO:
         IF logGblDebugging THEN
            fLog("ShipOrderLineID: " + STRING(readShipOrderLine.ShipOrderLineID)
               + " PartID: " + STRING(readShipOrderLine.PartID) + " Part NOT Found.").
                                  
         NEXT OrderStationLoop.
      END.   
                
      /* Skip ShipOrder if we have already checked that part already */   
      FIND FIRST ttPartCheck NO-LOCK 
         WHERE ttPartCheck.PartID = readPart.PartID NO-ERROR.
      IF NOT AVAILABLE ttPartCheck THEN
      DO:
         CREATE ttPartCheck.
         ASSIGN ttPartCheck.PartID = readPart.PartID.
      END. 
      ELSE
      DO:
         IF logGblDebugging THEN
            fLog("PartID: " + STRING(readPart.PartID) + " Ref: " + readPart.PartRef 
               + " did not past stock allocation on another order during this pick generation.").
                                          
         NEXT OrderStationLoop.
      END.                
      
      /* Check that Part is stored in WorkZone for selected PickPackStation */
      logAvailPartInProcessableLocation = NO.
      WorkZoneLoop:
      FOR EACH readPickPackWorkZoneLink NO-LOCK /* idx=PickPackID */
         WHERE readPickPackWorkZoneLink.PickPackStation = intFastPickPackStationID
         AND   readPickPackWorkZoneLink.Active:

         IF logGblDebugging THEN
            fLog("PickPackWorkZoneLinkID: " + STRING(readPickPackWorkZoneLink.PickPackWorkZoneLinkID)
               + " WorkZoneID: "            + STRING(readPickPackWorkZoneLink.WorkZoneID)).         
                           
         LocationLoop:      
         FOR EACH readLocation NO-LOCK /* idx=WorkZoneIDActive */
            WHERE readLocation.WorkZoneID = readPickPackWorkZoneLink.WorkZoneID
            AND   readLocation.Active:
                                 
            EMPTY TEMP-TABLE ttPartEntityStatus NO-ERROR.   
                                       
            intTotalAvailableQty = 0.
            StockCheckLoop:                      
            FOR EACH readStockPackage NO-LOCK /* idx=LocationPartStockStatusCreated, PartIDDetrashed */
               WHERE readStockPackage.LocationID           = readLocation.LocationID
               AND   readStockPackage.PartID               = readPart.PartID  
               AND   readStockPackage.StockStatusID        = readShipOrderLine.StockStatusID
               AND   readStockPackage.Detrashed            = ""               
               AND   readStockPackage.ParentStockPackageID = 0:
                                    
               intTotalAvailableQty = intTotalAvailableQty + readStockPackage.PackageQty.  
               
               FIND FIRST ttPartEntityStatus
                  WHERE ttPartEntityStatus.PartID        = readStockPackage.PartID
                  AND   ttPartEntityStatus.StockEntityID = readStockPackage.StockEntityID
                  AND   ttPartEntityStatus.StockStatusID = readStockPackage.StockStatusID NO-ERROR.
               IF NOT AVAILABLE ttPartEntityStatus THEN
               DO:
                  CREATE ttPartEntityStatus.
                  ASSIGN ttPartEntityStatus.PartID        = readStockPackage.PartID
                         ttPartEntityStatus.StockEntityID = readStockPackage.StockEntityID
                         ttPartEntityStatus.StockStatusID = readStockPackage.StockStatusID
                         ttPartEntityStatus.AvailQty      = readStockPackage.PackageQty.    
               END.
               ELSE
               DO:         
                  ttPartEntityStatus.AvailQty = ttPartEntityStatus.AvailQty + readStockPackage.PackageQty.
               END.   
                      
            END. /* StockCheckLoop */
            
            IF intTotalAvailableQty = 0 THEN                     
               NEXT LocationLoop.
         
            /* Obtain Allocated Qty */
            intAllocatedQty = 0.
            AllocatedLoop:
            FOR EACH readTaskLine NO-LOCK /* idx=PartIDCompleted */
               WHERE readTaskLine.PartID    = readPart.PartID
               AND   readTaskLine.Completed = "",
                  EACH readTaskLineWork NO-LOCK /* idx=CompletedSourceLocationID, TaskLineIDCompleted */
                     WHERE readTaskLineWork.TaskLineID = readTaskLine.TaskLineID
                     AND readTaskLineWork.LocationID   = readLocation.LocationID
                     AND readTaskLineWork.Completed    = "":
                           
               FIND FIRST readTask OF readTaskLine NO-LOCK NO-ERROR. /* idx=TaskID */
               IF NOT AVAILABLE readTask THEN 
                  NEXT AllocatedLoop.         
               
               /* Skip Task that does not affect inventory allocation */
               IF NOT CAN-DO(chrPickTaskTypeList, STRING(readTask.TaskTypeID)) THEN
                  NEXT AllocatedLoop.            
                                                                         
               intAllocatedQty = intAllocatedQty + readTaskLineWork.QtyAssigned.
               
               FIND FIRST ttPartEntityStatus
                  WHERE ttPartEntityStatus.PartID        = readTaskLine.PartID
                  AND   ttPartEntityStatus.StockEntityID = readTaskLineWork.StockEntityID
                  AND   ttPartEntityStatus.StockStatusID = readTaskLineWork.StockStatusID NO-ERROR.
               IF AVAILABLE ttPartEntityStatus THEN
               DO:
                  ttPartEntityStatus.AvailQty = ttPartEntityStatus.AvailQty - readTaskLineWork.QtyAssigned.   
               END.       
                                    
            END. /* AllocatedLoop */   
               
            /* Adjust Available Pick Quantity */
            intTotalAvailableQty = intTotalAvailableQty - intAllocatedQty.   
               
            IF intTotalAvailableQty <= 0 THEN
            DO:
               IF logGblDebugging THEN
                  fLog("No stock available after adjustment of pending Tasks for PartID: " + STRING(readPart.PartID)
                     + " PartRef: " + readPart.PartRef + " in LocationRef: " + readLocation.LocationRef).
               
               NEXT LocationLoop.
            END.   
               
            logAvailPartInProcessableLocation = YES.   
            
            /* There should always be one record only */       
            CREATE ttPickInfo.
            ASSIGN ttPickInfo.PartID                = readPart.PartID
                   ttPickInfo.PartRef               = readPart.PartRef
                   ttPickInfo.PartEAN               = readPart.EanCode
                   ttPickInfo.LocationID            = readLocation.LocationID
                   ttPickInfo.LocationRef           = readLocation.LocationRef           
                   ttPickInfo.AvailQtyToPick        = intTotalAvailableQty           
                   ttPickInfo.ShipOrderStreamID     = readShipOrder.ShipOrderStreamID
                   ttPickInfo.BusinessUnitID        = readShipOrder.BusinessUnitID
                   ttPickInfo.CarrierSortationID    = readShipOrder.CarrierSortationID
                   ttPickInfo.Priority              = readShipOrder.Priority
                   ttPickInfo.MinutesAwayFromPickUp = intMinutesFromPickUpTime
                   ttPickInfo.StockStatusID         = readShipOrderLine.StockStatusID.
                   
            IF intMaxPriorityVariation > readShipOrder.Priority THEN
               ttPickInfo.MaxPriorityVariance = 0.
            ELSE
               ttPickInfo.MaxPriorityVariance = intMaxPriorityVariation.       
                   
            FIND FIRST readPartType OF readPart NO-LOCK NO-ERROR.
            IF AVAILABLE PartType AND PartType.MaxNumberOrdersPerTask > 0 THEN                                                
               ttPickInfo.MaxNumOrdersToPick = readPartType.MaxNumberOrdersPerTask.
            ELSE    
               ttPickInfo.MaxNumOrdersToPick = intMaxOrdersPerTask.
                   
            IF logGblDebugging THEN
               fLog("ttPickInfo created successfully, PartID: " + STRING(ttPickInfo.PartID)
                       + " PartRef: "               + ttPickInfo.PartRef 
                       + " LocationID: "            + STRING(ttPickInfo.LocationID)
                       + " LocationRef: "           + ttPickInfo.LocationRef 
                       + " AvailQtyToPick: "        + STRING(ttPickInfo.AvailQtyToPick)
                       + " ShipOrderStreamID: "     + STRING(ttPickInfo.ShipOrderStreamID)
                       + " BusinessUnitID: "        + STRING(ttPickInfo.BusinessUnitID)
                       + " CarrierSortationID: "    + STRING(ttPickInfo.CarrierSortationID)
                       + " Priority: "              + STRING(ttPickInfo.Priority)
                       + " MinutesAwayFromPickUp: " + STRING(ttPickInfo.MinutesAwayFromPickUp)
                       + " MaxPriorityVariance: "   + STRING(ttPickInfo.MaxPriorityVariance)
                       + " MaxNumOrdersToPick: "    + STRING(ttPickInfo.MaxNumOrdersToPick)
                       + " StockStatusID: "         + STRING(ttPickInfo.StockStatusID)).
                                  
               LEAVE WorkZoneLoop.
                  
         END. /* FOR EACH readLocation */                              
      END. /* FOR EACH readPickPackWorkZone */

      IF logAvailPartInProcessableLocation = YES THEN
         RETURN "".

   END. /* OrderStationLoop */    
     
   IF logGblDebugging THEN
      fLog("No more ShipOrder to process for this PickPackStation.").
      
   RETURN "No more ShipOrder to process for this PickPackStation.". 
   
END FUNCTION. /* fCreatePickInfo */

 
FUNCTION fGetShipOrderToGenerate RETURNS CHARACTER (INPUT intPartID              AS INTEGER,
                                                    INPUT intLocationID          AS INTEGER,
                                                    INPUT intBusinessUnitID      AS INTEGER,
                                                    INPUT intShipOrderStreamID   AS INTEGER,
                                                    INPUT intAvailQtyInLocation  AS INTEGER,
                                                    INPUT intShipOrderStatusID   AS INTEGER,  
                                                    INPUT intMaxOrdersToGenerate AS INTEGER,
                                                    INPUT intMaxPriorityVariance AS INTEGER,
                                                    INPUT intStockStatusID       AS INTEGER):

   /* Buffers */
   DEFINE BUFFER otherShipOrderLine FOR ShipOrderLine.
   DEFINE BUFFER readShipOrder      FOR ShipOrder.
   DEFINE BUFFER readShipOrderLine  FOR ShipOrderLine.
   
   /* Local Variables */
   DEFINE VARIABLE intCounter              AS INTEGER NO-UNDO.
   DEFINE VARIABLE intOrderCounter         AS INTEGER NO-UNDO.
   DEFINE VARIABLE intOrderQtyCounter      AS INTEGER NO-UNDO.
 
   /* ----- Begins Logic ----- */
     
   EMPTY TEMP-TABLE ttShipOrderToGenerate NO-ERROR.
   
   IF logGblDebugging THEN
   DO:      
      fLog("--- FINDING ORDERS WITH EXACT PART MATCH ---").
      fLog(" PartID: "            + STRING(intPartID)
         + " LocationID: "        + STRING(intLocationID)
         + " BusinessUnitID: "    + STRING(intBusinessUnitID)
         + " ShipOrderStreamID: " + STRING(intShipOrderStreamID)
         + " AvailQty: "          + STRING(intAvailQtyInLocation)
         + " ShiporderStatusID: " + STRING(intShipOrderStatusID)
         + " MaxOrders: "         + STRING(intMaxOrdersToGenerate)
         + " MaxPriorityVar: "    + STRING(intMaxPriorityVariance)).
   END.   
   
   /* Initialize Counters  */                                                   
   ASSIGN intOrderCounter    = 0
          intOrderQtyCounter = 0.
             
   GenerateBlk:
   FOR EACH readShipOrder NO-LOCK /* idx=ShipOrderStreamID, TaskID */
      WHERE readShipOrder.ShipOrderStreamID = intShipOrderStreamID
      AND   readShipOrder.BusinessUnitID    = intBusinessUnitID
      AND   readShipOrder.ShipOrderStatusID = intShipOrderStatusID
      AND   readShipOrder.TaskID            = 0,
         EACH readShipOrderLine NO-LOCK
            WHERE readShipOrderLine.ShipOrderID       = readShipOrder.ShipOrderID
            AND   readShipOrderLine.PartID            = intPartID
            AND   readShipOrderLine.ShipOrderStatusID = readShipOrder.ShipOrderStatusID
            AND   readShipOrderLine.StockStatusID     = intStockStatusID     
      BY readShipOrder.DateToShip
      BY readShipOrder.Priority DESCENDING 
      BY readShipOrder.ShipOrderID:
         
      IF logGblDebugging THEN
         fLog("OrderRef: "    + readShipOrder.OrderRef
            + " DateToShip: " + STRING(readShipOrder.DateToShip)
            + " Priority: "   + STRING(readShipOrder.Priority)).   
         
      /* Check that there is only one ShipOrderLine at ReadyToProcess status */
      IF CAN-FIND(FIRST otherShipOrderLine NO-LOCK
                  WHERE otherShipOrderLine.ShipOrderID        = readShipOrderLine.ShipOrderID
                  AND   otherShipOrderLine.ShipOrderStatusID  = readShipOrderLine.ShipOrderStatusID
                  AND   otherShipOrderLine.ShipOrderLineID   <> readShipOrderLine.ShipOrderLineID) THEN
      DO:
         IF logGblDebugging THEN
            fLog("CANNOT PROCESS ORDERREF: " + readShipOrder.OrderRef + " more than 1 Line.").            
         NEXT GenerateBlk.
      END.   
                                             
      FIND FIRST ttShipOrderToGenerate NO-LOCK /* idx=idxShipOrder TEMPTABLE */
         WHERE ttShipOrderToGenerate.ShipOrderID = readShipOrderLine.ShipOrderID NO-ERROR.
      IF NOT AVAILABLE ttShipOrderToGenerate THEN
      DO:
         /* Leave GenerateBlk if 
            a.  Maximum number of order to generate per Task is reached
            b.  Available Qty for Part in identified location is reached
            c.  ShipOrder.Priority > intMaxPriorityVariance
           Note:  If a, b, or c is met, only leave if we generated one order */   
         IF (intOrderCounter = intMaxOrdersToGenerate 
               OR intOrderQtyCounter = intAvailQtyInLocation
               OR readShipOrder.Priority < intMaxPriorityVariance)
            AND intOrderCounter >= 1    THEN
         DO:
            IF logGblDebugging THEN
               fLog("CANNOT PROCESS ORDEREF: " + readShipOrder.OrderRef 
                  + " MaxOrdersToGenerate reached: " + STRING((intOrderCounter = intMaxOrdersToGenerate AND intOrderCounter >= 1), "Yes/No")
                  + " AvailQtyInLocation reached: "  + STRING((intOrderQtyCounter = intAvailQtyInLocation AND intOrderCounter >= 1), "Yes/No")
                  + " MaxPriorityVar reached: "      + STRING((readShipOrder.Priority < intMaxPriorityVariance AND intOrderCounter >= 1), "Yes/No")).
                     
            LEAVE GenerateBlk.
         END.   
                  
         /* Increase counter */
         ASSIGN intOrderCounter    = intOrderCounter + 1
                intOrderQtyCounter = intOrderQtyCounter + readShipOrderLine.QtyOrdered. 
                
         CREATE ttShipOrderToGenerate.
         ASSIGN ttShipOrderToGenerate.ShipOrderID = readShipOrderLine.ShipOrderID
                ttShipOrderToGenerate.PartID      = readShipOrderLine.PartID  
                ttShipOrderToGenerate.QtyOrdered  = readShipOrderLine.QtyOrdered
                ttShipOrderToGenerate.LocationID  = intLocationID.
                                 
      END. /* IF NOT AVAILABLE ttShipOrderToGenerate */                          
   END. /* GenerateBlk */            

   IF CAN-FIND(FIRST ttShipOrderToGenerate) THEN
      RETURN "Yes".
   ELSE   
      RETURN "No".
      
END FUNCTION. /* fGetShipOrderToGenerate */ 

/* This function will calculate the ShipOrderPriority for the provided Carrier Sortation from the CutOff */
FUNCTION fGetShipOrderPriority RETURNS INTEGER (INPUT intCarrierSortationID AS INTEGER):
   /* variables */
   DEFINE VARIABLE intOrderPriority AS INTEGER   NO-UNDO.
   DEFINE VARIABLE chrCutOff        AS CHARACTER NO-UNDO.
  
   /* Buffers */
   DEFINE BUFFER readCarrierSortation FOR CarrierSortation.
         
   FIND FIRST readCarrierSortation NO-LOCK /* idx=CarrierSortationID */
      WHERE readCarrierSortation.CarrierSortationID = intCarrierSortationID NO-ERROR.
   IF NOT AVAILABLE readCarrierSortation THEN
      RETURN 0.
   
   ASSIGN chrCutOff        = REPLACE(readCarrierSortation.CutOffTime, ":", "") 
          intOrderPriority = 2400 - INTEGER(chrCutOff).
      
   RETURN intOrderPriority.
   
END FUNCTION. /*fGetShipOrderPriority*/
