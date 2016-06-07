/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncKittingFunctions.i
Purpose : All functions to do with Kitting
Author  : 
Date    : 23rd Feb 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
10/06/2015 TAW GoPro      Update fCheckComponentStockForWorkOrder to look to FG Backflush field for case size
01/07/2015 TAW GoPro      Update LineSide location part counts to only require integer per case, then calc for number
   				             of cases per order to find total required
13/07/2015 BR  GoPro      New Function for PalletBuild	
09/09/2015 AL  GoPro      New Function for StockIntTotes
17/09/2015 TAW GoPro      Add KittingLine LocationID parameter to check for enough component stock
                          Only count stock that is still on the Line, if excluded from Pick
05/10/2015 TAW GoPro      Update checks in fCanStartWorkOrder to look if the KittingLine has any WO in process, instead of current station 
------------------------------------------------------------------------------------------------------------------------------------------*/

/* DEPENDENCIES */
/*{prcKittingProcedures.i}*/

/* Temp Tables */
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

/* ************************  Function Implementations ***************** */
FUNCTION fCheckComponentStockForWorkOrder RETURNS CHARACTER (INPUT intWorkOrderID        AS INTEGER,
                                                             INPUT intBomLineID          AS INTEGER,
                                                             INPUT intLineSideLocationID AS INTEGER,
                                                             INPUT intUserSessionID      AS INTEGER):
   
   DEFINE VARIABLE intQuantity    AS INTEGER NO-UNDO.
   DEFINE VARIABLE intRequiredQty AS INTEGER NO-UNDO.
   DEFINE VARIABLE intBulkQty     AS INTEGER NO-UNDO.
   DEFINE VARIABLE chrReadError   AS CHARACTER NO-UNDO.

   DEFINE BUFFER readWorkOrder           FOR WorkOrder.
   DEFINE BUFFER readBomLine             FOR BomLine.
   DEFINE BUFFER readUserSession         FOR UserSession.
   DEFINE BUFFER readKittingProcess      FOR KittingProcess.
   DEFINE BUFFER readStockPackage        FOR StockPackage.
   DEFINE BUFFER readFGPart              FOR Part.
   DEFINE BUFFER readComponentPart       FOR Part.
   DEFINE BUFFER readPartStockEntityLink FOR PartStockEntityLink.
   
   FIND FIRST readWorkOrder NO-LOCK /* idx=WorkOrderID */
      WHERE readWorkOrder.WorkOrderID = intWorkOrderID NO-ERROR.
   IF NOT AVAILABLE readWorkOrder THEN 
      RETURN "Error: Work Order does not exist!!".  
      
   FIND FIRST readBomLine NO-LOCK /* idx=BomLineID */
      WHERE readBomLine.BomLineID = intBomLineID NO-ERROR.
   IF NOT AVAILABLE readBomLine THEN 
      RETURN "Error: Bom Line does not exist!!".
      
   FIND FIRST readUserSession NO-LOCK /* idx=UserSessionID */
      WHERE readUserSession.UserSessionID = intUserSessionID NO-ERROR.
   IF NOT AVAILABLE readUserSession THEN 
      RETURN "Error: User Session does not exist!!".
   
   FIND FIRST readFGPart NO-LOCK
      WHERE readFGPart.PartID = readWorkOrder.PartID NO-ERROR.
   IF NOT AVAILABLE readFGPart THEN
      RETURN "Error: Finished Good part from WorkOrder does not exist.".
   
   IF readFGPart.BackflushPerFullFGStockEntity THEN
   DO:
      FIND FIRST readPartStockEntityLink NO-LOCK
         WHERE readPartStockEntityLink.PartID = readFGPart.PartID
         AND   readPartStockEntityLink.StockEntityID = readFGPart.DefaultStockEntityID NO-ERROR.
      IF NOT AVAILABLE readPartStockEntityLink THEN 
         RETURN "Error: No Default PartStockEntity Link exists for Finished Good Part.".
      
      intBulkQty = readPartStockEntityLink.MaxQty.
   END.
   ELSE
      intBulkQty = 1.
   
   KittingProcessLoop:
   FOR EACH readKittingProcess NO-LOCK /* idx=StockUserBomWo */
      WHERE readKittingProcess.WorkOrderID   = readWorkOrder.WorkOrderID
      AND   readKittingProcess.BomLineID     = readBomLine.BomLineID
      AND   readKittingProcess.UserSessionID = readUserSession.UserSessionID,
      EACH readStockPackage OF readKittingProcess NO-LOCK /* idx=StockPackageID */
         WHERE readStockPackage.PackageQty > 0:
      
      FIND FIRST readComponentPart OF readStockPackage NO-LOCK NO-ERROR.
      IF NOT AVAILABLE readComponentPart THEN
         RETURN "Error: Component Part from StockPackage [" + readStockPackage.PackageRef + "] does not exist.".
      
      IF readComponentPart.ExcludeFromWorkOrderPicks AND readStockPackage.LocationID <> intLineSideLocationID THEN
         NEXT KittingProcessLoop.   
         
      intQuantity = intQuantity + readStockPackage.PackageQty.
   
   END. /* FOR EACH readKittingProcess */
   
   /* Also count the ttKittingProcess records that have not been committed yet */
   FOR EACH ttKittingProcess NO-LOCK
      WHERE ttKittingProcess.WorkOrderID   = readWorkOrder.WorkOrderID
      AND   ttKittingProcess.BomLineID     = readBomLine.BomLineID
      AND   ttKittingProcess.UserSessionID = readUserSession.UserSessionID,
      EACH readStockPackage OF ttKittingProcess NO-LOCK /* idx=StockPackageID */
         WHERE readStockPackage.PackageQty > 0:
            
      intQuantity = intQuantity + readStockPackage.PackageQty.
   
   END.
   
   RUN pCloseEnough(INPUT  (readBomLine.QtyRequired * intBulkQty),
                    OUTPUT intRequiredQty,
                    OUTPUT chrReadError).

   IF chrReadError <> "" THEN
      RETURN chrReadError.
   
   IF intQuantity >= intRequiredQty THEN 
      RETURN "Yes".
   ELSE
      RETURN "No".
      
END FUNCTION. /* fCheckComponentStockForWorkOrder */


FUNCTION fCanCreateNewStockPackageForWorkOrder RETURNS CHARACTER (INPUT intWorkOrderID AS INTEGER):
                                                    
   DEFINE BUFFER readWorkOrder           FOR WorkOrder.
   DEFINE BUFFER readPart                FOR Part.
   DEFINE BUFFER readPartStockEntityLink FOR PartStockEntityLink.
   DEFINE BUFFER readKittedUnit          FOR KittedUnit.
   
   DEFINE VARIABLE intMaxAllowableStockPackages AS INTEGER NO-UNDO.
   DEFINE VARIABLE intStockPackagesCreated      AS INTEGER NO-UNDO.
   
   FIND FIRST readWorkOrder NO-LOCK /* idx=WorkOrderID */
      WHERE readWorkOrder.WorkOrderID = intWorkOrderID NO-ERROR.
   IF NOT AVAILABLE readWorkOrder THEN 
      RETURN "Error: Work Order does not exist!!".

   /* Check maximum allowable StockPackages for the WorkOrder */
   FIND FIRST readPart OF readWorkOrder NO-LOCK NO-ERROR. /* idx=PartID */
   IF NOT AVAILABLE readPart THEN 
      RETURN "Error: Part does not exist!!".
   
   FIND FIRST readPartStockEntityLink OF readPart NO-LOCK /* idx=StockEntityID */
      WHERE readPartStockEntityLink.StockEntityID = readPart.DefaultStockEntityID NO-ERROR. 
   IF NOT AVAILABLE readPartStockEntityLink THEN 
      RETURN "Error: PartStockEntityLink does not exist!!".
   
   intMaxAllowableStockPackages = (readWorkOrder.QtyOrdered / readPartStockEntityLink.MaxQty) +
                                  (readWorkOrder.QtyOrdered MOD readPartStockEntityLink.MaxQty).
       
   /* How many StockPackages the Work Order has Created so far */
   FOR EACH readKittedUnit OF readWorkOrder NO-LOCK /* idx=WorkOrderStockPackage */
      BREAK BY readKittedUnit.StockPackageID:
      IF FIRST-OF(readKittedUnit.StockPackageID) THEN 
         intStockPackagesCreated = intStockPackagesCreated + 1.
   END.
   
   IF intMaxAllowableStockPackages > intStockPackagesCreated THEN 
      RETURN "Yes".
   ELSE 
      RETURN "No".
      
END FUNCTION. /* fCanCreateNewStockPackageForWorkOrder */


FUNCTION fCanStartWorkOrder RETURN CHARACTER (INPUT intKittingStationID AS INTEGER,
                                              INPUT intUserSessionID    AS INTEGER,
                                              INPUT intWorkOrderID      AS INTEGER):
   
   DEFINE BUFFER readKittingStation             FOR KittingStation.
   DEFINE BUFFER readKittingLine                FOR KittingLine.
   DEFINE BUFFER readUserSession                FOR UserSession.
   DEFINE BUFFER readWorkOrder                  FOR WorkOrder.
   DEFINE BUFFER readStockPackage               FOR StockPackage.
   DEFINE BUFFER readKittedUnit                 FOR KittedUnit.
   DEFINE BUFFER readUnlabelledStockStatus      FOR StockStatus.
   DEFINE BUFFER readBeingKittedWorkOrderStatus FOR WorkOrderStatus.
   DEFINE BUFFER readWorkOrderKittingLineLink   FOR WorkOrderKittingLineLink.
   DEFINE BUFFER readKittingProcess             FOR KittingProcess.
   DEFINE BUFFER readHoldWorkOrderStatus        FOR WorkOrderStatus.
   DEFINE BUFFER readBom                        FOR Bom.
   DEFINE BUFFER readBomLine                    FOR BomLine.
   DEFINE BUFFER readPart                       FOR Part.
   DEFINE BUFFER readFGPart                     FOR Part.
   DEFINE BUFFER readFGPartStockEntityLink      FOR PartStockEntityLink.
   
   DEFINE VARIABLE intRequiredQty AS INTEGER NO-UNDO.
   DEFINE VARIABLE intLineSideQty AS INTEGER NO-UNDO.
   DEFINE VARIABLE intNumOfCases  AS INTEGER NO-UNDO.
   DEFINE VARIABLE chrReadError   AS CHARACTER NO-UNDO.
   
   /* Initial Checks for passed in parameters */
   FIND FIRST readKittingStation NO-LOCK /* idx=KittingStationID */
      WHERE readKittingStation.KittingStationID = intKittingStationID 
      AND readKittingStation.Active NO-ERROR.
   IF NOT AVAILABLE readKittingStation THEN 
      RETURN "Error: KittingStation does not exist or is inactive!!".
   
   FIND FIRST readKittingLine NO-LOCK /* idx=KittingLineID*/
      WHERE readKittingLine.KittingLineID = readKittingStation.KittingLineID
      AND readKittingLine.Active NO-ERROR.
   IF NOT AVAILABLE readKittingLine THEN 
      RETURN "Error: KittingLine does not exist or is inactive for KittingStation!!".  
      
   FIND FIRST readUserSession NO-LOCK /* idx=UserSessionID */
      WHERE readUserSession.UserSessionID = intUserSessionID NO-ERROR.
   IF NOT AVAILABLE readUserSession THEN 
      RETURN "Error: UserSession does not exist!!".
      
   FIND FIRST readWorkOrder NO-LOCK /* idx=WorkOrderID */
      WHERE readWorkOrder.WorkOrderID = intWorkOrderID NO-ERROR.
   IF NOT AVAILABLE readWorkOrder THEN 
      RETURN "Error: WorkOrder does not exist!!".
      
   FIND FIRST readFGPart NO-LOCK /* idx=PartID */
      WHERE readFGPart.PartID = readWorkOrder.PartID NO-ERROR.
   IF NOT AVAILABLE readFGPart THEN 
      RETURN "Error: No FinishedGood Part record for PartID[" + STRING(readWorkOrder.PartID) + "]!!".
   
      
   FIND FIRST readBom OF readWorkOrder NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readBom THEN
      RETURN "Error: Bom ID [" + STRING(readWorkOrder.BomID) + "]does not exists!!".
   
   /* Check if Work Order is on Hold */
   FIND FIRST readHoldWorkOrderStatus NO-LOCK /* idx=StatusCode */
      WHERE readHoldWorkOrderStatus.StatusCode = "OnHold" NO-ERROR.
   IF NOT AVAILABLE readHoldWorkOrderStatus THEN 
      RETURN "Error: WorkOrderStatus does not exist for 'OnHold'!!".
   
   IF readWorkOrder.WorkOrderStatusID = readHoldWorkOrderStatus.WorkOrderStatusID THEN 
      RETURN "Error: WorkOrder " + STRING(intWorkOrderID) + " is on Hold!!". 
   
   /* Check if KittingStation has Unlabelled StockPackage */
   FIND FIRST readUnlabelledStockStatus NO-LOCK /* idx=StatusCode */
      WHERE readUnlabelledStockStatus.StatusCode = "Unlabelled" NO-ERROR.
   IF NOT AVAILABLE readUnlabelledStockStatus THEN 
      RETURN "Error: StockStatus not found for 'Unlabelled'!!".
   
   FIND FIRST readStockPackage NO-LOCK /* idx=LocationIDStockStatusID */
      WHERE readStockPackage.LocationID    = readKittingStation.LocationID 
      AND   readStockPackage.StockStatusID = readUnlabelledStockStatus.StockStatusID
      NO-ERROR.
   IF AVAILABLE readStockPackage THEN
   DO:
      /* Check if for another WorkOrder than the one being started */
      FIND FIRST readKittedUnit OF readStockPackage NO-LOCK NO-ERROR. /* idx=StockPackageID */
      IF NOT AVAILABLE readKittedUnit THEN
         RETURN "Error: KittedUnit does not exist for StockPackage " + readStockPackage.PackageRef + "!!".         
       
      IF readKittedUnit.WorkOrderID <> intWorkOrderID THEN 
        RETURN "Error: WorkOrder " + STRING(readKittedUnit.WorkOrderID) + 
               " is currently being kitted at this station, cannot start another!!".   
   END.   
   
   /* Check for sufficient non-picked components */
   LineSideComponentCheckLoop:
   FOR EACH readBomLine OF readBom NO-LOCK, /* idx=BomID */
      EACH readPart OF readBomLine NO-LOCK: /* idx=PartID */

      IF NOT readPart.ExcludeFromWorkOrderPicks THEN
         NEXT LineSideComponentCheckLoop.
         
      FIND FIRST readFGPartStockEntityLink NO-LOCK
         WHERE readFGPartStockEntityLink.PartID = readFGPart.PartID
         AND   readFGPartStockEntityLink.StockEntityID = readFGPart.DefaultStockEntityID NO-ERROR.
         
      IF NOT AVAILABLE readFGPartStockEntityLink THEN
         RETURN "Error: No PartStockEntityLink record exists for Part:" + readFGPart.PartRef + " and StockEntityID: "
                 + STRING(readFGPart.DefaultStockEntityID) + ". Cannot continue!!".

      RUN pCloseEnough(INPUT (readBomLine.QtyRequired * readFGPartStockEntityLink.MaxQty),
                       OUTPUT intRequiredQty,
                       OUTPUT chrReadError).
      
      IF chrReadError <> "" THEN
         RETURN chrReadError + " BomLine ID [" + STRING(readBomLine.BomLineID) + "] used on WorkOrder ID ["
                             + STRING(readWorkOrder.WorkOrderID) + "].".

      intLineSideQty = 0.                                                          /* reset for each bomline */
                                                                                   /* number of cases for workorder */
      intNumOfCases = (readWorkOrder.QtyOrdered - readWorkOrder.QtyCompleted) / readFGPartStockEntityLink.MaxQty.
      intRequiredQty = intRequiredQty * intNumOfCases.                             /* calculate the number of total required */
      
      /* Count Qty of BomLine.PartID at KittingLine */
      FOR EACH readStockPackage NO-LOCK
         WHERE readStockPackage.LocationID = readKittingLine.LocationID
         AND   readStockPackage.PartID     = readBomLine.PartID:

         intLineSideQty = intLineSideQty + readStockPackage.PackageQty.
      END. /* FOR EACH readStockPacakge */

      IF intRequiredQty > intLineSideQty THEN
         RETURN "Error: Insufficient Component part [" + readPart.PartRef + "] on the Line. Please restock and retry.".
        
   END. /* LineSideComponentCheckLoop */
   
   /* Check if UserSession is already linked to another WorkOrder that is being Kitted */
   FIND FIRST readBeingKittedWorkOrderStatus NO-LOCK /* idx=StatusCode */
      WHERE readBeingKittedWorkOrderStatus.StatusCode = "BeingKitted" NO-ERROR.
   IF NOT AVAILABLE readBeingKittedWorkOrderStatus THEN 
      RETURN "Error: WorkOrderStatus not found for 'BeingKitted'".
   
   /* REMOVING CHECK PER STATION AS THERE IS NOW A CHECK FOR ONE WORKORDER IN BeingKitted STATUS PER LINe */
/*   FOR EACH readWorkOrderKittingLineLink OF readKittingLine NO-LOCK /* idx=KittingLineID */       */
/*      WHERE readWorkOrderKittingLineLink.Active,                                                  */
/*      EACH readWorkOrder OF readWorkOrderKittingLineLink NO-LOCK /* idx=WorkOrderID */            */
/*         WHERE readWorkOrder.WorkOrderStatusID = readBeingKittedWorkOrderStatus.WorkOrderStatusID,*/
/*      EACH readKittingProcess OF readWorkOrder NO-LOCK: /* idx=WorkOrderID */                     */
/*                                                                                                  */
/*      IF readKittingProcess.UserSessionID = intUserSessionID AND                                  */
/*         readKittingProcess.WorkOrderID <> intWorkOrderID THEN                                    */
/*        RETURN "Error: WorkOrder " + STRING(readKittingProcess.WorkOrderID) +                     */
/*               " is currently being kitted at this station, cannot start another!!".              */
/*   END.                                                                                           */
   
   FOR EACH readWorkOrderKittingLineLink OF readKittingLine NO-LOCK /* idx=KittingLineID */
      WHERE readWorkOrderKittingLineLink.Active,
      EACH readWorkOrder OF readWorkOrderKittingLineLink NO-LOCK /* idx=WorkOrderID */
         WHERE readWorkOrder.WorkOrderStatusID = readBeingKittedWorkOrderStatus.WorkOrderStatusID:
      
      /* If a Workorder is in BeingKitted status, that is not the one trying to be Started, return error */
      IF readWorkOrder.WorkOrderID <> intWorkOrderID THEN
         RETURN "Error: WorkOrder " + STRING(readWorkOrder.WorkOrderID) + 
                " is currently being kitted on this Kitting Line, cannot start a different WorkOrder!!".   
   END.

   RETURN "Yes".
   
END FUNCTION. /* fCanStartWorkOrder */


/****
Check all lines of a WorkOrder's Bom for sufficient stock, and if the WorkOrder has been put on hold.

Returns: Yes  - Sufficient stock is available in the KittingProcess table for all lines of the Bom
         No   - Missing one or more components from stock in the KittingProcess table
         Hold - WorkOrder's OnHold flag has been set to yes, processing should stop. 
****/
FUNCTION fCheckAllStockForWorkOrder RETURNS CHARACTER (INPUT intWorkOrderID        AS INTEGER,
                                                       INPUT intLineSideLocationID AS INTEGER,
                                                       INPUT intUserSessionID      AS INTEGER):
   
   DEFINE VARIABLE chrLineCheck  AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER readAllWorkOrder           FOR WorkOrder.
   DEFINE BUFFER readAllBomLine             FOR BomLine.
   DEFINE BUFFER readAllHoldWorkOrderStatus FOR WorkOrderStatus.

   FIND FIRST readAllWorkOrder NO-LOCK  /*idx=WorkOrderID*/
      WHERE readAllWorkOrder.WorkOrderID = intWorkOrderID NO-ERROR.
   IF NOT AVAILABLE readAllWorkOrder THEN 
      RETURN "Error: WorkOrder does not exist!!".

   /* Check if Work Order is on Hold */
   FIND FIRST readAllHoldWorkOrderStatus NO-LOCK /* idx=StatusCode */
      WHERE readAllHoldWorkOrderStatus.StatusCode = "OnHold" NO-ERROR.
   IF NOT AVAILABLE readAllHoldWorkOrderStatus THEN 
      RETURN "Error: WorkOrderStatus does not exist for 'OnHold'!!".
   
   IF readAllWorkOrder.WorkOrderStatusID = readAllHoldWorkOrderStatus.WorkOrderStatusID THEN 
      RETURN "Hold". 
   
   BomLineCheck:
   FOR EACH readAllBomLine NO-LOCK /*idx=BomActive*/
      WHERE readAllBomLine.BomID = readAllWorkOrder.BomID:
      
      chrLineCheck = fCheckComponentStockForWorkOrder (INPUT readAllWorkOrder.WorkOrderID,
                                                       INPUT readAllBomLine.BomLineID,
                                                       INPUT intLineSideLocationID,
                                                       INPUT intUserSessionID).
      IF chrLineCheck BEGINS "Error" THEN
         LEAVE BomLineCheck.
      IF chrLineCheck = "No" THEN
         LEAVE BomLineCheck.
   END.
   RETURN chrLineCheck.   
   
END FUNCTION. /* fCheckAllStockForWorkOrder */


FUNCTION fGetLoosePackageForWorkOrder RETURN CHARACTER(INPUT intSelectedToteID AS INTEGER,
                                                       INPUT intPartID         AS INTEGER,
                                                       INPUT intWorkOrderID    AS INTEGER):
   
   /* Variable */
   DEFINE VARIABLE logToteWorkOrderCheckPassed AS LOGICAL NO-UNDO. 
                                                          
   /* Buffer */
   DEFINE BUFFER readPart                  FOR Part.                                                          
   DEFINE BUFFER readStockPackage          FOR StockPackage.
   DEFINE BUFFER readTask                  FOR Task.
   DEFINE BUFFER readTaskLine              FOR TaskLine.
   DEFINE BUFFER readTaskLineWork          FOR TaskLineWork.
   DEFINE BUFFER stockTaskLineWork         FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkStockLink FOR TaskLineWorkStockLink.
   DEFINE BUFFER readTaskLineWorkToteLink  FOR TaskLineWorkToteLink.
   DEFINE BUFFER checkTote                 FOR Tote.
   DEFINE BUFFER readTote                  FOR Tote.
   DEFINE BUFFER readWorkOrder             FOR WorkOrder.   


   FIND FIRST readWorkOrder NO-LOCK 
      WHERE readWorkOrder.WorkOrderID = intWorkOrderID NO-ERROR.
   IF NOT AVAILABLE readWorkOrder THEN
      RETURN "Error: No WorkOrder exists for ID:[" + STRING(intWorkOrderID) + "].".
      
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = readWorkOrder.TaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(WorkOrder.TaskID) + "].".
      
   FIND FIRST readTote NO-LOCK 
      WHERE readTote.ToteID = intSelectedToteID NO-ERROR.
   IF NOT AVAILABLE readTote THEN
      RETURN "Error: No Tote exists for ID:[" + STRING(intSelectedToteID) + "].".
      
   FIND FIRST readPart NO-LOCK
      WHERE readPart.PartID = intPartID NO-ERROR.
   IF NOT AVAILABLE readPart THEN
      RETURN "Error: No Part exists for ID:[" + STRING(intPartID) + "].".
   
   logToteWorkOrderCheckPassed = NO.               
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK
      WHERE readTaskLine.PartID = readPart.PartID,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkToteLink OF readTaskLineWork NO-LOCK
            WHERE readTaskLineWorkToteLink.ToteID = readTote.ToteID:
               
      logToteWorkOrderCheckPassed = YES.
      
      StockLoop:
      FOR EACH stockTaskLineWork NO-LOCK
         WHERE stockTaskLineWork.TaskLineWorkID = readTaskLineWork.TaskLineWorkID,
         EACH readTaskLineWorkStockLink OF stockTaskLineWork
         WHERE readTaskLineWorkStockLink.TotePackageID > 0: 
            
         FIND FIRST readStockPackage NO-LOCK
            WHERE readStockPackage.StockPackageID = readTaskLineWorkStockLink.TotePackageID NO-ERROR.            
         IF NOT AVAILABLE readStockPackage THEN
            NEXT StockLoop.    
            
         IF readStockPackage.Detrashed <> "" THEN
            NEXT StockLoop.   
               
         RETURN readStockPackage.PackageRef.
      END. /* StockLoop */
                                                                      
   END. /* ToteLoop */               
   
   IF NOT logToteWorkOrderCheckPassed THEN   
      RETURN "Error: This Tote:[" + readTote.ToteRef + "] was not used to pick this WorkOrder, please try another.".
   ELSE       
      RETURN "Error: There are no loose stock in this Tote.  Please scan StockPackage.".                                                    
                                                          
                                                          
END FUNCTION. /* fGetLoosePackageForWorkOrder */       


/* Function to get the Stock Required for a WorkOrder and Tote  */
FUNCTION fGetStockRequiredForOrder RETURNS CHARACTER (INPUT intWorkOrderID    AS INTEGER,
                                                      INPUT intSelectedToteID AS INTEGER):
   
   DEFINE VARIABLE intQtyOrdered              AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intQtyToAllocate           AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intToteUnAllocatedQty      AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intOrderUnAllocatedQty     AS INTEGER     NO-UNDO.
   
   DEFINE BUFFER   readWorkOrder              FOR WorkOrder.
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
      
   FIND FIRST readWorkOrder NO-LOCK /*idx=WorkOrderID*/
      WHERE readWorkOrder.WorkOrderID = intWorkOrderID NO-ERROR.
   
   IF NOT AVAILABLE readWorkOrder THEN
      RETURN "Error: No WorkOrder exists for ID:[" + STRING(intWorkOrderID) + "].".
   
   FIND FIRST readTask NO-LOCK /*idx=TaskID*/
      WHERE readTask.TaskID = readWorkOrder.TaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(WorkOrder.TaskID) + "].".
   
   FIND FIRST totePackageStockEntity NO-LOCK  
      WHERE totePackageStockEntity.EntityCode = "TotePackage" NO-ERROR.
   IF NOT AVAILABLE totePackageStockEntity THEN
      RETURN "Error: No StockEntity exists for EntityCode:[TotePackage].".

   /* Get all of the Stock in all of the Totes that were used to Pick this Order */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK, /*idx=TaskLineID*/
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
               
               FIND FIRST readLocation OF readTote NO-LOCK NO-ERROR. /*idx=LocationID*/
               
               PackageLoop:
               FOR EACH readStockPackage OF readTote NO-LOCK /*idx=ToteIDParentStockPackageID*/
                  WHERE readStockPackage.ParentStockPackageID = 0:
                  FIND FIRST readPart        OF readStockPackage NO-LOCK. /*idx=PartID*/
                  FIND FIRST readStockEntity OF readStockPackage NO-LOCK. /*idx=StockEntityID*/
                  
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
                         ttStockInTotes.QtyAssignedToOrder = readStockPackage.PackageQty
                         ttStockInTotes.QtyRemaining       = readStockPackage.PackageQty.
                
               END. /*FOR EACH StockPackage OF readTote NO-LOCK*/
               
            END. /*IF FIRST-OF(readTote.ToteID) THEN*/
            
   END. /*FOR EACH readTaskLineWork OF readTask NO-LOCK*/
   
   RETURN "Ok".
   
END FUNCTION. /* fGetStockRequiredForOrder */

/* Function to get the WorkOrderID of the Parent Stockpackage passed during Palletbuild  */
FUNCTION fGetWorkOrderIDForPalletBuild RETURNS INTEGER (INPUT intParentPackageID AS INTEGER):
   
   DEFINE VARIABLE intWorkOrderID   AS INTEGER NO-UNDO.
   
   DEFINE BUFFER readparentStockPackage FOR StockPackage.
   DEFINE BUFFER readchildStockPackage  FOR StockPackage.
   DEFINE BUFFER readKittedUnit         FOR KittedUnit.
   
   FIND FIRST readparentStockPackage NO-LOCK 
      WHERE readparentStockPackage.StockPackageID = intParentPackageID
      AND   readparentStockPackage.Detrashed      = "" NO-ERROR.
   IF AVAILABLE readparentStockPackage THEN   
   DO:
      FIND FIRST readchildStockPackage NO-LOCK
         WHERE readchildStockPackage.ParentStockPackageID = readparentStockPackage.StockPackageID 
         AND   readchildStockPackage.Detrashed            = "" NO-ERROR.
      IF AVAILABLE readchildStockPackage THEN 
      DO:
         FIND FIRST readKittedUnit NO-LOCK 
            WHERE readKittedUnit.StockPackageID = readchildStockPackage.StockPackageID NO-ERROR.
         IF AVAILABLE readKittedUnit THEN 
         DO:
            RETURN readKittedUnit.WorkOrderID.
         END.          
      END. /* AVAILABLE readchildStockPackage */             
   END. /* AVAILABLE readparentStockPackage*/
     
END FUNCTION. /* fGetWorkOrderIDForPalletBuild */
                                                    

