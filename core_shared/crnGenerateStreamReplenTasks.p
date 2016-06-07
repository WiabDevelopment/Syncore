/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnGenerateStreamReplenTasks.p
Purpose : Creates Ship Order Stream Replen Tasks off the cron.   
Author  : Christopher Shelley
Date    : 28/05/2015

This will only generate a Replen if:
   - Ground Location has Stock, WorkZone, and PartLocationSize assigned
   - Rack Location has Stock
   - More demand for the part than what is currently in the Ground Location
   - PartLocationSize is setup for the Part
   - ShipOrderStream is Active
   - PickPack Station is Active and linked to the Stream
   - WorkZone is Active and linked to the PickPack Station
   - ShipOrder and ShipOrderLine are prior to packed out status with a Stream assigned 
   
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
28/05/2015 CS  Canontlb   Created.
05/06/2015 CS  Canontlb   Added more debug messages, added delete of debug log file if empty, and code cleanup.
07/07/2015 CS  Canontlb   Added default Stream Replen priority. 
31/07/2015 CS  Canontlb   Changed to do only full boxes, create tasks, and not send email reports.
03/08/2015 CS  Canontlb   Added new threshold field and fixed issue with buffer not being available.
15/12/2015 CS  CanonTlb   Changed debugging to make it more clear and logic changes for Looping and package qty.
16/12/2015 CS  CanonTlb   Added Partial Qty Replens.
29/12/2015 CS  CanonTlb   Added check if WorkZone is Active and replens for ShipOrders that have not been generated yet.
30/12/2015 CS  CanonTlb   Added ability to do multiple replens if first created replen does not meet the replen qty.
13/01/2016 CS  CanonTlb   Added creation of ttStockPackageAlreadyUsed to track which packages have been used.
14/01/2016 CS  CanonTlb   Fixed updates for mulitiple replens and for counting PickShortage ShipOrders twice for MaxReplenQty.
15/01/2016 CS  CanonTlb   More logic changes about consolidating line. 
22/01/2016 CS  CanonTlb   Fixed issue where TaskLine was doubling QtyAssigned in specific case and added more comments.
29/01/2016 CS  CanonTlb   Added tracking qty assigned StockPackages.
09/02/2016 CS  CanonTlb   Added tracking to the PartID level and set to allow mulitple replens to generate.
10/02/2016 CS  CanonTlb   Fixed issue with updating existing ttStockPackageAlreadyUsed and fixed some logging.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Custom Includes DO NOT REMOVE. This is required to handle all generic web functions */
{defSessionVariables.i}     

/* Functions */
{fncClassFunctions.i}
{fncGlobalFunctions.i}
{fncServerFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{fncStatusTypeFunctions.i}
{fncTaskFunctions.i}
{fncReplenFunctions.i}  /* ttLocationAlreadyUsed, ttStockPackageAlreadyUsed and ttStockLocation are defined in this include */
{prcReplenProcedures.i}

/* Variable for Field Names */
DEFINE VARIABLE intShipPackageID                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPackageRef                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intPartCounter                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intTotalStockQty                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrReturnValue                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrResult                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrReturnMessage                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNewPackageStatus              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSuccessfulReplenCounter       AS INTEGER     NO-UNDO.
DEFINE VARIABLE intReplenTaskID                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE intNumDaysToReplen               AS INTEGER     NO-UNDO.
DEFINE VARIABLE intTaskPriority                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE decMinReplenThreshold            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE chrReturnError                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSuccessMessage                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intAvailableStockStatusID        AS INTEGER     NO-UNDO.
DEFINE VARIABLE intAvailableTaskStatusID         AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrNewAgedDirectory              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLogFile                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logAllWentOk                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intMaxReplenQty                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE intMinReplenQty                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE intReplenQty                     AS INTEGER     NO-UNDO. 
DEFINE VARIABLE intStockPackageID                AS INTEGER     NO-UNDO.
DEFINE VARIABLE intRMAShipOrderTypeID            AS INTEGER     NO-UNDO.
DEFINE VARIABLE intFullTaskExecutionType         AS INTEGER     NO-UNDO.
DEFINE VARIABLE intPartialTaskExecutionType      AS INTEGER     NO-UNDO.
DEFINE VARIABLE intShipOrderOrdersToGenerate     AS INTEGER     NO-UNDO.
DEFINE VARIABLE intShipOrderPriorToPackout       AS INTEGER     NO-UNDO.
DEFINE VARIABLE intTaskLineID                    AS INTEGER     NO-UNDO.
DEFINE VARIABLE intTaskLineWorkID                AS INTEGER     NO-UNDO.
DEFINE VARIABLE intLastSourceLocationID          AS INTEGER     NO-UNDO.
DEFINE VARIABLE intTotalQtyAssigned              AS INTEGER     NO-UNDO.
DEFINE VARIABLE intShipOrderReplenTaskType       AS INTEGER     NO-UNDO.
DEFINE VARIABLE intShipOrderPickTaskType         AS INTEGER     NO-UNDO.
DEFINE VARIABLE intPartQtyInUse                  AS INTEGER     NO-UNDO.

/* Data Object Definitions */
DEFINE VARIABLE newTask                          AS newRecord.
DEFINE VARIABLE newTaskLine                      AS newRecord.
DEFINE VARIABLE newTaskLineWork                  AS newRecord.
DEFINE VARIABLE updTaskLine                      AS updRecord.
DEFINE VARIABLE updTaskLineWork                  AS updRecord.

/* Streams */
DEFINE STREAM sToLogFile.

/* All ShipOrderStreams and there WorkZones */
DEFINE TEMP-TABLE ttAllStreamWorkZone       NO-UNDO
  FIELD StreamID                            AS INTEGER
  FIELD WorkZoneID                          AS INTEGER.
  
/* All Locations in a WorkZone that are attached to a Stream that can be Replenished. Will have current stock and max qty if there is */
/* a part in the location */
DEFINE TEMP-TABLE ttWorkZoneLocation        NO-UNDO 
   FIELD LocationID                         AS INTEGER
   FIELD WorkZoneID                         AS INTEGER
   FIELD PartID                             AS INTEGER
   FIELD OnHandQty                          AS INTEGER   
   FIELD PhysicalMaxQty                     AS INTEGER
   FIELD OutStandingReplenQty               AS INTEGER
   FIELD ShippingMaxQty                     AS INTEGER
   FIELD AvgPickedQty                       AS INTEGER
   INDEX LocationID IS UNIQUE PRIMARY LocationID
   INDEX WorkZoneID WorkZoneID
   INDEX PartID PartID.

DEFINE TEMP-TABLE ttPickedFromWorkZone      NO-UNDO
   FIELD WorkZoneID                         AS INTEGER
   FIELD StreamID                           AS INTEGER   
   FIELD PartID                             AS INTEGER
   FIELD PickedQty                          AS INTEGER
   INDEX WorkZoneID WorkZoneID
   INDEX StreamID   StreamID
   INDEX PartID     PartID.

DEFINE TEMP-TABLE ttShiporder               NO-UNDO
   FIELD ShipOrderID                        AS INTEGER.


/* Buffers */
DEFINE BUFFER readttWorkZoneLocation    FOR ttWorkZoneLocation.
DEFINE BUFFER otherttStockPackageAlreadyUsed FOR ttStockPackageAlreadyUsed.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP. 
   
END FUNCTION. /*fLog*/

/* Set the log file destination directory  */
chrNewAgedDirectory = fGetAgedDirectory("../logs/", 2).
IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

chrLogFile = chrLogFileDirectory +  "crnGenerateStreamReplenTasks" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

/* Setup log file if Debug is enabled */
IF logGblDebugging = TRUE THEN 
   OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.

MainBlk:
DO:
   
   IF logGblDebugging = TRUE THEN 
      fLog("Starting MainBlk Logic").
   
   FIND FIRST LocationType NO-LOCK /* idx=LocationTypeID WHOLE-INDEX */
      WHERE LocationType.AutoReplenishLocType NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      RETURN fTL("No [LocationType] available with [AutoReplenishLocType = TRUE]. Cannot Generate Replens.").
   
   FIND FIRST TaskType NO-LOCK /* idx=TypeCode */
      WHERE TaskType.TypeCode = "ShipOrderReplen" NO-ERROR. 
   IF NOT AVAILABLE TaskType THEN
      RETURN fTL("No [TaskType] available with [TypeCode = ShipOrderReplen]. Cannot Generate Replens.").
   intShipOrderReplenTaskType = TaskType.TaskTypeID.
   
   RELEASE TaskType NO-ERROR.
   
   FIND FIRST TaskType NO-LOCK /* idx=TypeCode */
      WHERE TaskType.TypeCode = "ShipOrderPick" NO-ERROR. 
   IF NOT AVAILABLE TaskType THEN
      RETURN fTL("No [TaskType] available with [TypeCode = ShipOrderPick]. Cannot Generate Replens.").
   intShipOrderPickTaskType = TaskType.TaskTypeID.
   
   /* Get the Statuses for Orders that can be Packed Out */
   FIND FIRST ShipOrderStatusGroup NO-LOCK /* idx=GroupCode */
      WHERE ShipOrderStatusGroup.GroupCode = "PriorToPackedOut" NO-ERROR.   
   IF NOT AVAILABLE ShipOrderStatusGroup THEN
      RETURN fTL("[ShipOrderStatusGroup] [PriorToPackedOut] does not exist. Cannot Generate Replens."). 
   
   intShipOrderPriorToPackout = ShipOrderStatusGroup.ShipOrderStatusGroupID.
   
   /* Release for looking up OrdersToGenerate GroupCode */
   RELEASE ShipOrderStatusGroup NO-ERROR.
   
   /* Get the Statuses for Orders that can be Generated */
   FIND FIRST ShipOrderStatusGroup NO-LOCK /* idx=GroupCode */
      WHERE ShipOrderStatusGroup.GroupCode = "OrdersToGenerate" NO-ERROR.   
   IF NOT AVAILABLE ShipOrderStatusGroup THEN
      RETURN fTL("[ShipOrderStatusGroup] [OrdersToGenerate] does not exist. Cannot Generate Replens."). 
   
   intShipOrderOrdersToGenerate = ShipOrderStatusGroup.ShipOrderStatusGroupID.
   
   /* Get the TaskExecutionType for Full as these are Complete StockPackages that we want to Replen */
   FIND FIRST TaskExecutionType NO-LOCK /* idx=TypeCode */
      WHERE TaskExecutionType.TypeCode = "Full"
      AND   TaskExecutionType.Active NO-ERROR.
   IF NOT AVAILABLE TaskExecutionType THEN 
      RETURN fTL("No [TaskExecutionType] is available for TypeCode: Full.").
   
   intFullTaskExecutionType = TaskExecutionType.TaskExecutionTypeID.
   
   /* Release for looking up Partial TypeCode */
   RELEASE TaskExecutionType NO-ERROR.
   
   /* Get the TaskExecutionType for Partial as there are partial Replens that will need to use loose StockPackages */
   FIND FIRST TaskExecutionType NO-LOCK /* idx=TypeCode */
      WHERE TaskExecutionType.TypeCode = "Partial"
      AND   TaskExecutionType.Active NO-ERROR.
   IF NOT AVAILABLE TaskExecutionType THEN 
      RETURN fTL("No [TaskExecutionType] is available for TypeCode: Partial.").
      
   intPartialTaskExecutionType = TaskExecutionType.TaskExecutionTypeID.
   
   FIND FIRST ShipOrderType NO-LOCK
      WHERE ShipOrderType.TypeCode = "RMA" 
      AND   ShipOrderType.Active NO-ERROR.
   IF AVAILABLE ShipOrderType THEN
      intRMAShipOrderTypeID = ShipOrderType.ShipOrderTypeID.   
   
   /* Get the Default Replen Priority */
   FIND FIRST ReplenishConfig NO-LOCK NO-ERROR. /* idx=ReplenishConfigID WHOLE-INDEX */
   IF AVAILABLE ReplenishConfig THEN
      ASSIGN intTaskPriority       = ReplenishConfig.DefaultReplenPriority
             decMinReplenThreshold = ReplenishConfig.MinReplenThreshold.
   
   IF logGblDebugging = TRUE THEN 
      fLog("Default Task Priority: " + STRING(intTaskPriority) + " Default Min Replen Threshold: " + STRING(decMinReplenThreshold)).
      
   ASSIGN intAvailableStockStatusID = fGetStatusID("Stock", "Available")
          intAvailableTaskStatusID  = fGetStatusID("Task", "Available").
   
   IF intAvailableStockStatusID = 0 THEN
      RETURN fTL("No [StockStatus] available with [Available]. Cannot Generate Replens.").
   
   IF logGblDebugging = TRUE THEN 
      fLog("Finding Stream information about WorkZones, Locations, and PartLocationSizeFits"). 
   
   /* Get information about each Stream that is Active and set to generate PartProfile Calculations*/    
   ShipOrderStreamLoop:
   FOR EACH ShipOrderStream NO-LOCK /* idx=ActiveListingSequence */
      WHERE ShipOrderStream.Active = TRUE
      AND   ShipOrderStream.GeneratePartProfileCalculations = TRUE:
      
      IF logGblDebugging = TRUE THEN 
         fLog("Inside ShipOrderStream Loop - Stream: " + ShipOrderStream.StreamName).
      
      /* Find all WorkZones for each PickPackStation on Stream */
      ShipOrderStreamStationLoop:
      FOR EACH ShipOrderStreamPickPackLink NO-LOCK /* idx=ShipOrderStreamID */
         WHERE ShipOrderStreamPickPackLink.ShipOrderStreamID = ShipOrderStream.ShipOrderStreamID
         AND   ShipOrderStreamPickPackLink.Active = TRUE:
         
         /* Verify PickPackStation exists and is active so we do not create replens for an missing or inactive station */
         FIND FIRST PickPackStation NO-LOCK /* idx=PickPackStationID */
            WHERE PickPackStation.PickPackStationID = ShipOrderStreamPickPackLink.PickPackStationID 
            AND   PickPackStation.Active = TRUE NO-ERROR.
         IF NOT AVAILABLE PickPackStation THEN
            NEXT ShipOrderStreamStationLoop.
         
         /* Find all the WorkZones attached to this PickPackStation */
         PickPackWorkZoneLoop:
         FOR EACH PickPackWorkZoneLink NO-LOCK /* idx=PickPackID */
            WHERE PickPackWorkZoneLink.PickPackStationID = ShipOrderStreamPickPackLink.PickPackStationID
            AND   PickPackWorkZoneLink.Active:
            
            /* Find if this WorkZone has been added before for the Stream */
            FIND FIRST ttAllStreamWorkZone
               WHERE ttAllStreamWorkZone.StreamID   = ShipOrderStream.ShipOrderStreamID
               AND   ttAllStreamWorkZone.WorkZoneID = PickPackWorkZoneLink.WorkZoneID NO-ERROR.
            IF AVAILABLE ttAllStreamWorkZone THEN
               NEXT PickPackWorkZoneLoop.
            
            /* Find if this WorkZone is Active */
            FIND FIRST WorkZone NO-LOCK
               WHERE WorkZone.WorkZoneID = PickPackWorkZoneLink.WorkZoneID
               AND   WorkZone.Active     = TRUE NO-ERROR.
            IF NOT AVAILABLE WorkZone THEN
               NEXT PickPackWorkZoneLoop.
            
            /* Track the Stream and WorkZones */
            CREATE ttAllStreamWorkZone.
            ASSIGN ttAllStreamWorkZone.StreamID   = ShipOrderStream.ShipOrderStreamID
                   ttAllStreamWorkZone.WorkZoneID = PickPackWorkZoneLink.WorkZoneID.
            
            IF logGblDebugging = TRUE THEN 
               fLog("Found WorkZone for the Stream: " + STRING(ShipOrderStream.StreamName) + " " 
                       + "WorkZone: " + STRING(ttAllStreamWorkZone.WorkZoneID)).
            
            /* Build TempTable of all Locations that can be replenished by WorkZone */   
            LocationLoop:
            FOR EACH Location NO-LOCK /* idx=WorkZoneIDActive */
               WHERE Location.WorkZoneID = PickPackWorkZoneLink.WorkZoneID:
               
               /* Find if this Location has been added before */
               FIND FIRST ttWorkZoneLocation
                  WHERE ttWorkZoneLocation.LocationID = Location.LocationID NO-ERROR.
               IF AVAILABLE ttWorkZoneLocation THEN
                  NEXT LocationLoop.
                  
               CREATE ttWorkZoneLocation.
               ASSIGN ttWorkZoneLocation.LocationID = Location.LocationID
                      ttWorkZoneLocation.WorkZoneID = Location.WorkZoneID.
               
               IF logGblDebugging = TRUE THEN 
                  fLog("Found Location for WorkZoneID: " + STRING(ttWorkZoneLocation.WorkZoneID) + " LocationRef: " 
                          + STRING(Location.LocationRef)).
               
               /* Check if the location has any available stock */
               FIND FIRST StockPackage NO-LOCK /* idx=DetrashedParentIDStockStatusID */
                  WHERE StockPackage.Detrashed  = ""
                  AND   StockPackage.LocationID = Location.LocationID 
                  AND   StockPackage.StockStatusID = intAvailableStockStatusID NO-ERROR.
               IF AVAILABLE StockPackage THEN
               DO:
                  /* Store the PartID */
                  ttWorkZoneLocation.PartID = StockPackage.PartID.
                  
                  /* Find the maximum part qty this location can hold */
                  FIND FIRST PartLocationSizeFit NO-LOCK /* idx=PartIDLocationSizeFitID */
                     WHERE PartLocationSizeFit.PartID = ttWorkZoneLocation.PartID
                     AND   PartLocationSizeFit.LocationSizeID = Location.LocationSizeID NO-ERROR.
                  IF AVAILABLE PartLocationSizeFit THEN
                  DO:
                     ttWorkZoneLocation.PhysicalMaxQty = PartLocationSizeFit.MaxQty.
                     
                     IF logGblDebugging = TRUE THEN 
                        fLog("Found PartLocationSizeFit for PartID: " + STRING(ttWorkZoneLocation.PartID) + " LocationRef: " 
                                + STRING(Location.LocationRef) + " PhysicalMaxQty: " + STRING(ttWorkZoneLocation.PhysicalMaxQty)).
                  END. /*IF AVAILABLE PartLocationSizeFit THEN*/
                  ELSE
                  DO:
                     /* Set a default in here so we can generate something at least for this location until it is setup? */
                     IF logGblDebugging = TRUE THEN 
                        fLog("Missing PartLocationSizefit for PartID: " + STRING(ttWorkZoneLocation.PartID) + " LocationRef: " 
                                + STRING(Location.LocationRef) + ". Will not replen until setup.").
                  END. /*IF NOT AVAILABLE PartLocationSizeFit THEN*/   
               END. /*IF AVAILABLE StockPackage THEN*/
               
            END. /*FOR EACH Location NO-LOCK*/
              
         END. /*FOR EACH PickPackWorkZoneLink NO-LOCK*/
         
      END. /*FOR EACH ShipOrderStreamPickPackLink NO-LOCK*/
      
   END. /*FOR EACH ShipOrderStream NO-LOCK*/   
   
   IF logGblDebugging = TRUE THEN 
      fLog("Finding outstanding replen qty now.").
                        
   /* Find all outstanding replenishment qty for the locations.  Start at Tasks and lookup temp table to try to reduce record reads */   
   TaskLoop: 
   FOR EACH Task NO-LOCK /* idx=TaskTypeCompletedPriorityDCreate */
      WHERE Task.TaskTypeID = intShipOrderReplenTaskType
      AND   Task.Completed  = "": 
          
      TaskLineLoop:
      FOR EACH TaskLine OF Task NO-LOCK /* idx=TaskIDCompleted */
         WHERE TaskLine.Completed = "":
         
         TaskLineWorkLoop:
         FOR EACH TaskLineWork OF TaskLine NO-LOCK /* idx=TaskLineIDCompleted */
            WHERE TaskLineWork.Completed = "":
            
            /* Try to find this location in the TempTable */   
            FIND FIRST ttWorkZoneLocation 
               WHERE ttWorkZoneLocation.LocationID = TaskLineWork.TargetLocationID NO-ERROR.
            IF NOT AVAILABLE ttWorkZoneLocation THEN
               NEXT TaskLineWorkLoop.
            
            /* Calculate the outstanding replen qty */
            ttWorkZoneLocation.OutStandingReplenQty = ttWorkZoneLocation.OutStandingReplenQty + TaskLineWork.QtyAssigned 
                                                         - TaskLineWork.QtyCompleted.
            
            IF logGblDebugging = TRUE AND
               ttWorkZoneLocation.OutStandingReplenQty > 0 THEN 
               fLog("Found outstanding replen for PartID: " + STRING(TaskLine.PartID) + " TaskID: " + STRING(Task.TaskID) 
                       + " OutStandingReplenQty: " + STRING(ttWorkZoneLocation.OutStandingReplenQty)).
            
         END. /*FOR EACH TaskLineWork OF TaskLine NO-LOCK*/
             
      END. /*FOR EACH TaskLine OF Task NO-LOCK*/            
   
   END. /*FOR EACH Task NO-LOCK*/ 
   
   IF logGblDebugging = TRUE THEN 
      fLog("Finding outstanding parts to be shipped by WorkZone.").
   
   /* Calculate the Parts that are going to be picked for Orders from the WorkZones */
   /* Find all ShipOrder Statuses that can Generate or be Packed out  */
   FOR EACH ShipOrderStatusGroupLink NO-LOCK /* idx=ShipOrderStatusGroupID */
      WHERE (ShipOrderStatusGroupLink.ShipOrderStatusGroupID = intShipOrderOrdersToGenerate
      OR    ShipOrderStatusGroupLink.ShipOrderStatusGroupID = intShipOrderPriorToPackout)
      AND   ShipOrderStatusGroupLink.Active = TRUE:
      
      ShipOrderBlk:
      /* Find all the ShipOrders that could be Picked for this stream */
      FOR EACH ShipOrder NO-LOCK /* idx=ShipOrderStatusOrderRef */
         WHERE ShipOrder.ShipOrderStatusID = ShipOrderStatusGroupLink.ShipOrderStatusID:
         
         /* Do not look at future ShipOrders.  Only look at current day or missed Orders. */
         IF ShipOrder.DateToShip > TODAY THEN
            NEXT ShipOrderBlk.
         
         /* Do not look at RMA ShipOrders.  There should be no replens for these. */
         IF ShipOrder.ShipOrderTypeID = intRMAShipOrderTypeID THEN
            NEXT ShipOrderBlk.
         
         /* Do not look at Streams we are not tracking.  This means the Stream is inactive or not set to replen */
         FIND FIRST ttAllStreamWorkZone
            WHERE ttAllStreamWorkZone.StreamID = ShipOrder.ShipOrderStreamID NO-ERROR.
         IF NOT AVAILABLE ttAllStreamWorkZone THEN
            NEXT ShipOrderBlk.
         
         IF CAN-FIND(ttShipOrder WHERE ttShipOrder.ShipOrderID = ShipOrder.ShipOrderID) THEN
            NEXT ShipOrderBlk.
         
         CREATE ttShipOrder.
         ttShiporder.ShipOrderID = ShipOrder.ShipOrderID.
         
         /* Find all the ShipOrderLines that could be Packed on the ShipOrder */
         FOR EACH ShipOrderLine NO-LOCK /* idx=ShipOrderPartPriority */
            WHERE ShipOrderLine.ShipOrderID       = ShipOrder.ShipOrderID
            AND   ShipOrderLine.ShipOrderStatusID = ShipOrder.ShipOrderStatusID:

            /* Find all WorkZones that belong to the Stream */
            FOR EACH ttAllStreamWorkZone 
               WHERE ttAllStreamWorkZone.StreamID = ShipOrder.ShipOrderStreamID:
               
               /* Check for ShipOrderLine part in stock in the location attached to the WorkZone on the Stream */   
               FOR EACH ttWorkZoneLocation
                  WHERE ttWorkZoneLocation.PartID = ShipOrderLine.PartID
                  AND   ttWorkZoneLocation.WorkZoneID = ttAllStreamWorkZone.WorkZoneID:

                  /* Calculate the outstanding part qty to pack */   
                  ttWorkZoneLocation.ShippingMaxQty = ttWorkZoneLocation.ShippingMaxQty + ShipOrderLine.QtyOrdered 
                                                         - ShipOrderLine.QtyPacked.

                  IF logGblDebugging = TRUE AND
                     ttWorkZoneLocation.ShippingMaxQty > 0 THEN 
                     fLog("Found outstanding part to ship.  PartID: " + STRING(ShipOrderLine.PartID) + " WorkZoneID: " 
                             + STRING(ttWorkZoneLocation.WorkZoneID) + " LocationID: " + STRING(ttWorkZoneLocation.LocationID) 
                             + " PartID: " + STRING(ShipOrderLine.PartID) + " ShipOrderID: " + STRING(ShipOrder.ShipOrderID) 
                             + " ShipOrderLineID: " + STRING(ShipOrderLine.ShipOrderLineID) +  " Qty: " 
                             + STRING(ttWorkZoneLocation.ShippingMaxQty)).
                
               END. /*FOR EACH ttWorkZoneLocation*/
 
            END. /*FOR EACH ttAllStreamWorkZone NO-LOCK*/
         
         END. /*FOR EACH ShipOrderLine NO-LOCK*/        
         
      END. /*FOR EACH ShipOrderLine NO-LOCK*/    
   
   END. /*FOR EACH ShipOrderStatusGroupLink OF ShipOrderStatusGroup NO-LOCK*/
   
   IF logGblDebugging = TRUE THEN 
      fLog("Finding on Hand Part Qty for each Location.").
   
   /* Calculate the On Hand Qty for each Location in the Temp Table */
   FOR EACH ttWorkZoneLocation:
      
      /* Get Current Stock in the locations */
      StockPackageLoop:
      FOR EACH StockPackage NO-LOCK /* idx=ParentStockPackageID */
         WHERE StockPackage.PartID = ttWorkZoneLocation.PartID 
         AND   StockPackage.Detrashed  = ""
         AND   StockPackage.LocationID = ttWorkZoneLocation.LocationID
         AND   StockPackage.StockStatusID = intAvailableStockStatusID
         AND   StockPackage.ParentStockPackageID = 0:
         
         /* Update the Temp Table On Hand Qty for this Location */
         ttWorkZoneLocation.OnHandQty = ttWorkZoneLocation.OnHandQty + StockPackage.PackageQty.
         
      END. /*FOR EACH StockPackage NO-LOCK*/

      IF logGblDebugging = TRUE AND
         ttWorkZoneLocation.OnHandQty > 0 THEN 
         fLog("LocationID: " + STRING(ttWorkZoneLocation.LocationID) + " PartID: " + STRING(ttWorkZoneLocation.PartID)+ " OnHandQty: " 
                 + STRING(ttWorkZoneLocation.OnHandQty)).
      
   END. /*FOR EACH ttWorkZoneLocation:*/  
   
   IF logGblDebugging = TRUE THEN 
      fLog("Finding the Qty Picked Per Day for each Part by WorkZone and Stream.").
   
   /* Find the Qty Picked Per Day for the SKU in the WorkZone and Stream */
   AvgPartLoop:
   FOR EACH readttWorkZoneLocation NO-LOCK:
      
      /* If there is no stock or the highest average has already been assigned */
      IF readttWorkZoneLocation.PartID = 0 OR
         readttWorkZoneLocation.AvgPickedQty > 0 THEN
         NEXT AvgPartLoop.
      
      IF logGblDebugging = TRUE THEN 
         fLog("LocationID: " + STRING(readttWorkZoneLocation.LocationID) + " PartID: " + STRING(readttWorkZoneLocation.PartID)).
      
      /* Find the last PartProfile */
      FIND LAST PartProfile NO-LOCK /* idx=PartIDCompletedHighRunnerRanking */ 
         WHERE PartProfile.PartID    = readttWorkZoneLocation.PartID 
         AND   PartProfile.Completed = "" NO-ERROR.
      IF NOT AVAILABLE PartProfile THEN
      DO:
         IF logGblDebugging = TRUE THEN 
            fLog("No PartProfile Available for Part").
         NEXT AvgPartLoop.
      END. /*IF NOT AVAILABLE PartProfile THEN*/
      
      
      /* If this Part has not already been populated into the Temp Table for all existing TaskLineWorks */
      IF NOT CAN-FIND(FIRST ttStockPackageAlreadyUsed 
                         WHERE ttStockPackageAlreadyUsed.PartID = readttWorkZoneLocation.PartID) THEN
      DO:
         IF logGblDebugging = TRUE THEN 
            fLog("Trying to Populate Outstanding Tasks for PartID: " + STRING(readttWorkZoneLocation.PartID)).
         
         /* Populate Already Used Packages by other Tasks */
         intPartQtyInUse = fNumOutstandingTaskQtyForPartSourceLocation(INPUT readttWorkZoneLocation.PartID,
                                                                       INPUT "PickRelatedTaskTypes").   
         IF logGblDebugging = TRUE THEN 
            fLog("Finished Populate Outstanding Tasks for PartID: " + STRING(readttWorkZoneLocation.PartID) + " Qty: " 
                    + STRING(intPartQtyInUse)).
      END. /*IF NOT CAN-FIND(FIRST ttStockPackageAlreadyUsed*/
         
      /* Find the Part info for each Stream */   
      FOR EACH PartProfileOrderStreamLink NO-LOCK /* idx=PartProfileIDCreated */ 
         WHERE PartProfileOrderStreamLink.PartProfileID = PartProfile.PartProfileID 
         AND   PartProfileOrderStreamLink.Completed = "":
            
         /* Find the WorkZones for the current Stream*/   
         FOR EACH ttAllStreamWorkZone
            WHERE ttAllStreamWorkZone.StreamID = PartProfileOrderStreamLink.ShipOrderStreamID:
            
            FIND FIRST ttPickedFromWorkZone
               WHERE ttPickedFromWorkZone.PartID     = readttWorkZoneLocation.PartID
               AND   ttPickedFromWorkZone.StreamID   = ttAllStreamWorkZone.StreamID
               AND   ttPickedFromWorkZone.WorkZoneID = ttAllStreamWorkZone.WorkZoneID NO-ERROR.
            IF NOT AVAILABLE ttPickedFromWorkZone THEN
            DO:
               CREATE ttPickedFromWorkZone.
               ASSIGN ttPickedFromWorkZone.PartID     = readttWorkZoneLocation.PartID
                      ttPickedFromWorkZone.StreamID   = ttAllStreamWorkZone.StreamID
                      ttPickedFromWorkZone.WorkZoneID = ttAllStreamWorkZone.WorkZoneID
                      ttPickedFromWorkZone.PickedQty  = PartProfileOrderStreamLink.AvgQtyPickedPerDay.
            
               IF logGblDebugging = TRUE THEN 
                  fLog("Set average picked for PartID: " + STRING(ttPickedFromWorkZone.PartID) 
                          + " StreamID: " + STRING(ttPickedFromWorkZone.StreamID) + " WorkZoneID: "
                          + STRING(ttPickedFromWorkZone.WorkZoneID) + " PickedQty: " + STRING(ttPickedFromWorkZone.PickedQty)).
            END. /*IF NOT AVAILABLE ttPickedFromWorkZone*/
               
         END. /*FOR EACH ttAllStreamWorkZone*/
         
      END. /* FOR EACH PartProfileOrderStreamLink*/
      
   END. /*FOR EACH readttWorkZoneLocation*/    
   
   
   
   IF logGblDebugging = TRUE THEN 
      fLog("Checking for locations that do not have a current replen and see if a Replenish Task needs created").
   
   /* Find which Locations need Replens made */   
   ReplenTaskLoop:
   FOR EACH ttWorkZoneLocation
      WHERE ttWorkZoneLocation.OutStandingReplenQty = 0:
      
      IF ttWorkZoneLocation.PartID = 0 THEN
         NEXT ReplenTaskLoop.
      
      IF logGblDebugging = TRUE THEN 
         fLog("WorkZone: " + STRING(ttWorkZoneLocation.WorkZoneID) + " LocationID: " + STRING(ttWorkZoneLocation.LocationID) + " PartID: " + STRING(ttWorkZoneLocation.PartID) + " OnHand: " 
                 + STRING(ttWorkZoneLocation.OnHandQty) + " MaxQty: " + STRING(ttWorkZoneLocation.PhysicalMaxQty)).
      
      IF ttWorkZoneLocation.OnHandQty = 0 THEN
      DO:
         IF logGblDebugging = TRUE THEN 
            fLog("Skipping - OnHandQty = 0").
         NEXT ReplenTaskLoop.
      END. /*IF ttWorkZoneLocation.OnHandQty = 0 THEN*/
      
      /* Check if Location is at already at or past max qty */
      IF ttWorkZoneLocation.OnHandQty >= ttWorkZoneLocation.PhysicalMaxQty THEN
      DO:
         IF logGblDebugging = TRUE THEN 
            fLog("Skipping - OnHandQty: " + STRING(ttWorkZoneLocation.OnHandQty) + " >= PhysicalMaxQty: " 
                    + STRING(ttWorkZoneLocation.PhysicalMaxQty)).
                    
         NEXT ReplenTaskLoop.
      END. /*IF ttWorkZoneLocation.OnHandQty >= ttWorkZoneLocation.PhysicalMaxQty THEN*/
         
      intMaxReplenQty = ttWorkZoneLocation.ShippingMaxQty.
      
      MaxReplenQtyLoop:
      FOR EACH ttPickedFromWorkZone 
         WHERE ttPickedFromWorkZone.PartID = ttWorkZoneLocation.PartID
         AND   ttPickedFromWorkZone.WorkZoneID = ttWorkZoneLocation.WorkZoneID
         BY    ttPickedFromWorkZone.PickedQty DESCENDING:
         
         IF intMaxReplenQty < ttPickedFromWorkZone.PickedQty THEN
         DO:
            IF logGblDebugging = TRUE THEN 
               fLog("Setting intMaxReplenQty from Avg PickedQty Per Day").
               
            intMaxReplenQty = ttPickedFromWorkZone.PickedQty.
         END. /*IF intMaxReplenQty < ttPickedFromWorkZone.PickedQty THEN*/
         
         LEAVE MaxReplenQtyLoop.
               
      END. /*FOR EACH ttPickedFromWorkZone*/
      
      IF logGblDebugging = TRUE THEN 
         fLog("Found MaxReplenQty: " + STRING(intMaxReplenQty)).
      
      /* Min Replen Qty is percent of MaxQty + 1.  This percent needs to go on a database field on ReplenConfig */
      ASSIGN intMinReplenQty = (intMaxReplenQty + 1) * decMinReplenThreshold
             intReplenQty    = 0.
      
      IF logGblDebugging = TRUE THEN 
         fLog("Calculated MinReplenQty: " + STRING(intMinReplenQty) + " from: (" + STRING(intMaxReplenQty) + " + 1) * " 
                 + STRING(decMinReplenThreshold) + " Current OnHandQty: " + STRING(ttWorkZoneLocation.OnHandQty)).
      
      IF ttWorkZoneLocation.OnHandQty < intMinReplenQty THEN
      DO:
         /* Create Replen for Minimum Qty to top off but not more than what the location can hold */
         IF intMaxReplenQty > ttWorkZoneLocation.PhysicalMaxQty THEN 
            intReplenQty = ttWorkZoneLocation.PhysicalMaxQty - ttWorkZoneLocation.OnHandQty. 
         ELSE
            intReplenQty = intMaxReplenQty - ttWorkZoneLocation.OnHandQty.
      END. /*IF ttWorkZoneLocation.OnHandQty < intMinReplenQty THEN*/  
      
      IF logGblDebugging = TRUE AND 
         intReplenQty > 0 THEN 
         fLog("Need to ReplenQty: " + STRING(intReplenQty)).
      
      /* If no replens for this location then go to the next */
      IF intReplenQty < 1 THEN
         NEXT ReplenTaskLoop.
      
      /* Reset the variables used to track the Replen Task creation and updates each time through the repeat loop */
      ASSIGN intReplenTaskID = 0
             intTaskLineID = 0
             intTaskLineWorkID = 0
             intLastSourceLocationID = 0.
      
      /* Repeat trying to find a StockPackage to use for Replen until all qty is fullfilled or no available inventory */
      ReplenPackageLoop:
      REPEAT WHILE intReplenQty > 0:
         IF intReplenTaskID = 0 AND 
            logGblDebugging = TRUE THEN
         DO: 
            fLog("Running pGetStockPackageReplenStream for first time for this Replen Qty.").
         END.
         ELSE
         DO:
            IF logGblDebugging = TRUE THEN 
               fLog("Running pGetStockPackageReplenStream again for this Replen Qty to try to fullfill it all.").
         END.
               
         /* Find a StockPackage to use for the Replen */
         RUN pGetStockPackageToReplenStream(INPUT  ttWorkZoneLocation.PartID,
                                            INPUT  intAvailableStockStatusID,
                                            INPUT  TRUE,
                                            OUTPUT intStockPackageID,
                                            OUTPUT chrReturnError).
          
         IF logGblDebugging = TRUE THEN 
            fLog("After GetStockPackagePackageToReplenStream - StockPackageID: " + STRING(intStockPackageID) + " Error: " + chrReturnError).
         
         /* If there was an error or there was not a StockPackage go to the next replen */
         IF chrReturnError <> "" OR
            intStockpackageID = 0 THEN
         DO:
            IF logGblDebugging = TRUE THEN 
            DO:
               
               FOR EACH ttStockPackageAlreadyUsed NO-LOCK
                  WHERE ttStockPackageAlreadyUsed.PartID = ttWorkZoneLocation.PartID:
                  
                  fLog("After ttStockPackageAlreadyUsed - StockPackageID: " + STRING(ttStockPackageAlreadyUsed.StockPackageID) 
                          + " Assigned Qty: " + STRING(ttStockPackageAlreadyUsed.QtyAssigned) + " TaskID: " 
                          + STRING(ttStockPackageAlreadyUsed.TaskID) + " TLID: " + STRING(ttStockPackageAlreadyUsed.TaskLineID) + " TLWID: " 
                          + STRING(ttStockPackageAlreadyUsed.TaskLineWorkID)).
                  
               END. /*FOR EACH ttStockPackageAlreadyUsed NO-LOCK*/
               
            END. /*IF logGblDebugging = TRUE THEN*/     
            
            NEXT ReplenTaskLoop.
         END. /*IF chrReturnError <> "" OR intStockpackageID = 0 THEN*/
         
         FIND FIRST StockPackage NO-LOCK /* idx=StockPackageID */
            WHERE StockPackage.StockPackageID = intStockPackageID NO-ERROR.
         IF NOT AVAILABLE StockPackage THEN
         DO:
            chrReturnError = fTL("No [StockPackage] available for ID:" + STRING(intStockPackageID)).
            LEAVE ReplenTaskLoop.
         END. /*IF NOT AVAILABLE StockPackage THEN*/
         
         intTotalQtyAssigned = 0.
         
         /* Calculate existing assigned qty for the StockPackage */
         FOR EACH ttStockPackageAlreadyUsed NO-LOCK
            WHERE ttStockPackageAlreadyUsed.StockPackageID = StockPackage.StockPackageID:
               
            intTotalQtyAssigned = intTotalQtyAssigned + ttStockPackageAlreadyUsed.QtyAssigned.
             
         END. /*FOR EACH ttStockPackageAlreadyUsed*/
         
         IF logGblDebugging = TRUE THEN 
            fLog("Calculated Qty Already Assigned for StockPackageID: " + STRING(intStockPackageID) + " Qty: " + STRING(intTotalQtyAssigned)).
         
         /* Start the transaction to create the Task, TaskLine, and TaskLineWorks */
         CreateBlk:
         DO TRANSACTION ON ERROR UNDO, LEAVE: 
            IF logGblDebugging = TRUE THEN 
               fLog("Create Block").
            
            /* Create tt Record to keep track of the uncomitted Replens that are using StockPackages as they go */
            CREATE ttStockPackageAlreadyUsed.
            ASSIGN ttStockPackageAlreadyUsed.StockPackageID = StockPackage.StockPackageID
                   ttStockPackageAlreadyUsed.PartID         = StockPackage.PartID.
                   ttStockPackageAlreadyUsed.TaskID         = intReplenTaskID.
            
            /* If there is no Replen Task created yet then create one as this is the first time in the loop */ 
            IF intReplenTaskID = 0 THEN
            DO:
               /* Create a single Task and a single TaskLine for the Combined Replen TaskLineWorks for this Part */
               newTask = fCreateRecord("Task").
               newTask:assignField("Created",        fTimestamp(NOW)).
               newTask:assignField("BusinessUnitID", intGblBusinessUnitID).
               newTask:assignField("Priority",       intTaskPriority).
               newTask:assignField("TaskStatusID",   intAvailableTaskStatusID).
               newTask:assignField("TaskTypeID",     intShipOrderReplenTaskType).
               
               chrReturnError = newTask:getErrors().
               
               IF chrReturnError <> "" THEN
                  UNDO CreateBlk, LEAVE CreateBlk.
               
               /* Set this output parameter to return the TaskID to the calling program */
               intReplenTaskID = newTask:NewRecordUniqueID.
               ttStockPackageAlreadyUsed.TaskID = intReplenTaskID.
               
               IF logGblDebugging = TRUE THEN 
                  fLog("Create TaskID: " + STRING(intReplenTaskID)).
            END. /*IF intReplenTaskID = 0 THEN*/
            
            /* If there isn't a TaskLine Record created for this Task create one */
            IF intTaskLineID = 0 THEN
            DO:
               /* Create a TaskLine for this Part */
               newTaskLine = fCreateRecord("TaskLine").
               newTaskLine:assignField("TaskID",       intReplenTaskID).
               newTaskLine:assignField("Priority",     intTaskPriority).
               newTaskLine:assignField("TaskStatusID", intAvailableTaskStatusID).
               newTaskLine:assignField("PartID",       ttWorkZoneLocation.PartID).
               
               chrReturnError = newTaskLine:getErrors().
               IF chrReturnError <> "" THEN
                  UNDO CreateBlk, LEAVE CreateBlk.
                  
               intTaskLineID = newTaskLine:NewRecordUniqueID.
               ttStockPackageAlreadyUsed.TaskLineID = intTaskLineID.
               
               IF logGblDebugging = TRUE THEN 
                  fLog("Create TaskLineID: " + STRING(intTaskLineID)).
            END. /*IF intTaskLineID = 0 THEN*/
            
            IF logGblDebugging = TRUE THEN 
               fLog("Trying to find previous TaskLineWork for TaskLineID: " + STRING(intTaskLineID) + " Location: "
                       + STRING(StockPackage.LocationID)).
            
            /* If there is not a ttStockPackageAlreadyUsed Record created for this TaskLine and LocationID we need a new TaskLineWork 
               record since we consolide the TLWs for the same location otherwise */
            FIND FIRST otherttStockPackageAlreadyUsed 
               WHERE otherttStockPackageAlreadyUsed.TaskLineID = intTaskLineID 
               AND otherttStockPackageAlreadyUsed.LocationID = StockPackage.LocationID NO-ERROR.
            IF NOT AVAILABLE otherttStockPackageAlreadyUsed THEN
            DO:
               intLastSourceLocationID = StockPackage.LocationID.
                
               /* Create a TaskLineWork for this TaskLineID and LocationID */
               newTaskLineWork = fCreateRecord("TaskLineWork").
               newTaskLineWork:assignField("TaskLineID",          intTaskLineID).
               newTaskLineWork:assignField("TaskStatusID",        intAvailableTaskStatusID).
               newTaskLineWork:assignField("LocationID",          StockPackage.LocationID).
               newTaskLineWork:assignField("StockEntityID",       StockPackage.StockEntityID).
               newTaskLineWork:assignField("StockStatusID",       StockPackage.StockStatusID).
               newTaskLineWork:assignField("TargetStockStatusID", StockPackage.StockStatusID).
               newTaskLineWork:assignField("TaskID",              intReplenTaskID).  
               
               intTaskLineWorkID = newTaskLineWork:NewRecordUniqueID.
               
               IF logGblDebugging = TRUE THEN 
                  fLog("Create TaskLineWorkID: " + STRING(intTaskLineWorkID) + " LocationID: " + STRING(StockPackage.LocationID) 
                          + " StockPackageID: " + STRING(StockPackage.StockPackageID) + " StockPackage Qty: " 
                          + STRING(StockPackage.PackageQty) + " Qty Assigned: " + STRING(intTotalQtyAssigned)).
               
               ASSIGN ttStockPackageAlreadyUsed.TaskLineWorkID = intTaskLineWorkID
                      ttStockPackageAlreadyUsed.LocationID     = StockPackage.LocationID
                      ttStockPackageAlreadyUsed.TaskLineID     = intTaskLineID.                  
               
               /* Full Task Execution as Stock Package Full Qty is still LESS than Total Replen Qty */
               IF intReplenQty - (StockPackage.PackageQty - intTotalQtyAssigned) >= 0 THEN
               DO:         
                  /* Assign remaining fields to the new TaskLineWork */
                  newTaskLineWork:assignField("QtyAssigned",         (StockPackage.PackageQty - intTotalQtyAssigned)).
                  newTaskLineWork:assignField("TaskExecutionTypeID", intFullTaskExecutionType).                                    
                  newTaskLineWork:assignField("Priority",            intTaskPriority).
                  newTaskLineWork:assignField("TargetLocationID",    ttWorkZoneLocation.LocationID).
                  
                  /* Update existing TaskLine Qty */
                  updTaskLine = fGetRecord("TaskLine", intTaskLineID).
                  updTaskLine:incrementField("QtyAssigned", (StockPackage.PackageQty - intTotalQtyAssigned)).
                  
                  ttStockPackageAlreadyUsed.QtyAssigned = StockPackage.PackageQty - intTotalQtyAssigned.
                  
                  IF logGblDebugging = TRUE THEN                
                     fLog("Full Replen - " + STRING((StockPackage.PackageQty - intTotalQtyAssigned))).
                  
                  /* Reduce Replen Qty by the remaining Package Qty for the next loop */   
                  intReplenQty = intReplenQty - (StockPackage.PackageQty - intTotalQtyAssigned).   
               END. /*IF intReplenQty - StockPackage.PackageQty > 0 THEN*/
               ELSE 
               DO:
                  /* Partial Task Execution as Stock Package Partial Qty is equal to the Total Replen Qty */
                  /* Assign remaining fields to the new TaskLineWork */
                  newTaskLineWork:assignField("QtyAssigned", intReplenQty).
                  newTaskLineWork:assignField("TaskExecutionTypeID", intPartialTaskExecutionType).                                    
                  newTaskLineWork:assignField("Priority",            intTaskPriority).
                  newTaskLineWork:assignField("TargetLocationID",    ttWorkZoneLocation.LocationID).
                  
                  /* Update existing TaskLine Qty */
                  updTaskLine = fGetRecord("TaskLine", intTaskLineID).
                  updTaskLine:incrementField("QtyAssigned", intReplenQty).
                  
                  ttStockPackageAlreadyUsed.QtyAssigned = intReplenQty.
                  
                  IF logGblDebugging = TRUE THEN 
                     fLog("Partial Replen - " + STRING(intReplenQty)).
                  
                  /* Set the Replen to 0 since this should be the last TaskLineWork */   
                  intReplenQty = 0.   
               END. /*IF intReplenQty - StockPackage.PackageQty <= 0 THEN*/
               
               chrReturnError = newTaskLineWork:getErrors().
               IF chrReturnError <> "" THEN
               DO:
                  IF logGblDebugging = TRUE THEN
                     fLog("newTaskLineWork errors: " + chrReturnError).
                  UNDO CreateBlk, LEAVE CreateBlk.
               END. /*IF chrReturnError <> "" THEN*/
               
               chrReturnError = updTaskLine:getErrors().
               IF chrReturnError <> "" THEN
               DO:
                  IF logGblDebugging = TRUE THEN
                     fLog("updTaskLine errors: " + chrReturnError).
                  UNDO CreateBlk, LEAVE CreateBlk.
               END. /*IF chrReturnError <> "" THEN*/
               
            END. /*IF intReplenTaskID = 0 THEN*/
            ELSE
            DO:
               /* There is an otherttStockPackageAlreadyUsed Record for the TaskLine and LocationID so do not create a new TaskLineWork 
                  Record since the existing TLW Record should be used for same Part and Location on the TaskLine */
               
               IF logGblDebugging = TRUE THEN 
                  fLog("Update TaskLineWorkID: " + STRING(otherttStockPackageAlreadyUsed.TaskLineWorkID) + " Location: " + STRING(StockPackage.LocationID)).
               
               /* Update existing TaskLine and TaskLineWork records */
               /* Increment the Qty on the TaskLine for every Package added */
               updTaskLine = fGetRecord("TaskLine", intTaskLineID).
               updTaskLineWork = fGetRecord("TaskLineWork", otherttStockPackageAlreadyUsed.TaskLineWorkID). 
               intTaskLineWorkID = otherttStockPackageAlreadyUsed.TaskLineWorkID.
               
               /* Update the new ttStockPackageAlreadyUsed */
               ASSIGN ttStockPackageAlreadyUsed.TaskLineWorkID      = intTaskLineWorkID
                      ttStockPackageAlreadyUsed.LocationID          = StockPackage.LocationID
                      ttStockPackageAlreadyUsed.TaskLineID          = intTaskLineID.
               
               /* Full Task Execution as Stock Package Full Qty is still LESS than Total Replen Qty */
               /* Should this be >= for Full Replen meeting exact qty? */
               IF intReplenQty - (StockPackage.PackageQty - intTotalQtyAssigned) >= 0 THEN
               DO:
                  /* Update existing TaskLine and TaskLineWork Qty */
                  updTaskLine:incrementField("QtyAssigned", (StockPackage.PackageQty - intTotalQtyAssigned)).
                  updTaskLineWork:incrementField("QtyAssigned", (StockPackage.PackageQty - intTotalQtyAssigned)).
                  
                  ttStockPackageAlreadyUsed.QtyAssigned = StockPackage.PackageQty - intTotalQtyAssigned.
                  
                  IF logGblDebugging = TRUE THEN                
                     fLog("Full Replen - " + STRING(StockPackage.PackageQty)).
                  
                  /* Reduce Replen Qty by the remaining Package Qty for the next loop */
                  intReplenQty = intReplenQty - (StockPackage.PackageQty - intTotalQtyAssigned). 
               END. /*IF intReplenQty - StockPackage.PackageQty > 0 THEN*/
               ELSE
               DO:
                  /* Partial Task Execution as Stock Package Partial Qty is equal to the Total Replen Qty */
                  /* Update existing TaskLine and TaskLineWork Qty */                  
                  updTaskLine:incrementField("QtyAssigned", intReplenQty).
                  updTaskLineWork:incrementField("QtyAssigned", intReplenQty).
                  
                  ttStockPackageAlreadyUsed.QtyAssigned = intReplenQty.
                  
                  IF logGblDebugging = TRUE THEN                
                     fLog("Partial Replen - " + STRING(intReplenQty)).
                     
                  /* Set the Replen to 0 since this should be the last TaskLineWork */   
                  intReplenQty = 0.
               END. /*IF intReplenQty - StockPackage.PackageQty <= 0 THEN*/
               
               /* Check TaskLine for Update Errors */   
               chrReturnError = updTaskLine:getErrors().
               IF chrReturnError <> "" THEN
               DO:
                  IF logGblDebugging = TRUE THEN
                     fLog("updTaskLine errors: " + chrReturnError).
                  UNDO CreateBlk, LEAVE CreateBlk.
               END. /*IF chrReturnError <> "" THEN*/    
               
               /* Check TaskLineWork for Update Errors */
               chrReturnError = updTaskLineWork:getErrors().
               IF chrReturnError <> "" THEN
               DO:
                  IF logGblDebugging = TRUE THEN
                     fLog("updTaskLineWork errors: " + chrReturnError).
                  UNDO CreateBlk, LEAVE CreateBlk.
               END. /*IF chrReturnError <> "" THEN*/ 
            END. /*IF intReplenTaskID > 0 THEN*/
                      
            IF logGblDebugging = TRUE AND
               chrError <> "" THEN
               fLog("Before pCommitAll - chrError: " + chrError).
            
            RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                            OUTPUT logAllWentOk,
                                            OUTPUT chrError).
            
            IF logGblDebugging = TRUE AND
               (chrError <> "" OR 
                chrReturnError <> "") THEN
               fLog("After pCommitAll - chrError: " + chrError + " - chrReturnError: " + chrReturnError).      
            
            /* Check pCommitAll for Errors */
            IF chrError <> "" THEN
            DO:
               UNDO CreateBlk, LEAVE CreateBlk.
            END. /*IF chrError <> "" THEN*/
            
            IF logGblDebugging = TRUE THEN 
               fLog("Replen created successfully for PartID: " + STRING(ttWorkZoneLocation.PartID) + " LocationID: "
                       + STRING(StockPackage.LocationID)).
         END. /*CreateBlk:*/
         
         /* Task, TaskLine, and TaskLineWork should have been created at this point.  Loop again to see if we have additional Replens to 
            create for this Replen Task.  There might not have been enough in the Source Location or the Package */
         
      END. /*ReplenPackageLoop:*/
      
   END. /*FOR EACH ttWorkZoneLocation NO-LOCK*/
   
   IF logGblDebugging = TRUE THEN 
      fLog("End of MainBlk").
   
END. /*MainBlk:*/