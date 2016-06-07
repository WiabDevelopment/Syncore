/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncReplenFunctions.i
Purpose : All functions to do with Replenishment
Author  : BG
Date    : 25th October 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
02/06/2015 CS  CanonTlb   Added fNumReplensOutstandingByLocation and coding standards.
29/12/2015 CS  CanonTlb   Added releases of buffer to fValidStockPackageToReplen.
30/12/2015 CS  CanonTlb   Added ability to create multiple replens in fValidStockPackageToReplen.
08/02/2016 CS  CanonTlb   Added fNumOutstandingTaskQtyForPartSourceLocation.
09/02/2016 CS  CanonTlb   Logic cleanup.
10/02/2016 CS  CanonTlb   Changed fNumOutstandingTaskQtyForPartSourceLocation to ignore child StockPackages.
11/02/2016 CS  CanonTlb   Fixed issue where not calculating units left to allocate correctly.
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttLocationAlreadyUsed     NO-UNDO 
   FIELD LocationID                         AS INTEGER
   INDEX LocationID IS UNIQUE PRIMARY LocationID.

DEFINE TEMP-TABLE ttStockPackageAlreadyUsed NO-UNDO 
   FIELD StockPackageID                     AS INTEGER
   FIELD LocationID                         AS INTEGER
   FIELD TaskID                             AS INTEGER
   FIELD TaskLineID                         AS INTEGER
   FIELD TaskLineWorkID                     AS INTEGER
   FIELD QtyAssigned                        AS INTEGER
   FIELD PartID                             AS INTEGER
   INDEX StockPackageID IS PRIMARY StockPackageID
   INDEX TLW TaskLineWorkID.

DEFINE STREAM strToLogFile.

FUNCTION fValidTargetReplenLocation RETURNS CHARACTER (INPUT intLocationID AS INTEGER):
   
   DEFINE BUFFER readTaskLineWork   FOR TaskLineWork.
   DEFINE BUFFER readLocation       FOR Location.
   DEFINE BUFFER readStockPackage   FOR StockPackage.
   
   FIND FIRST readLocation NO-LOCK /* idx=LocationID */
      WHERE readLocation.LocationID = intLocationID NO-ERROR.
   IF NOT AVAILABLE readLocation THEN
      RETURN "No Location exists for ID:[" + STRING(intLocationID) + "]".
   
   IF readLocation.Active = NO THEN
      RETURN "Location " + readLocation.LocationRef + " is not Active.".
   
   IF CAN-FIND(FIRST readStockPackage OF readLocation NO-LOCK) THEN
      RETURN "Already Stock in this Location:" + readLocation.LocationRef + " - cannot use.".
   
   IF CAN-FIND(FIRST readTaskLineWork NO-LOCK /* idx=CompletedSourceLocationID */
                  WHERE readTaskLineWork.Completed = ""
                  AND   readTaskLineWork.LocationID = readLocation.LocationID) THEN
      RETURN "Open TaskWork exists with this Location as it's Source - cannot use.".
   
   IF CAN-FIND(FIRST readTaskLineWork NO-LOCK /* idx=CompletedTargetLocationID */
                  WHERE readTaskLineWork.Completed = ""
                  AND   readTaskLineWork.TargetLocationID = readLocation.LocationID) THEN
      RETURN "Open TaskWork exists with this Location as it's Target - cannot use.".
   
   IF CAN-FIND(FIRST ttLocationAlreadyUsed OF readLocation NO-LOCK) THEN /* idx=LocationID */
      RETURN "Location has already been assigned a Replen within this Operation - cannot use.".
   
   RETURN "Yes".
   
END FUNCTION. /*fIsValidTargetReplenLocation*/

FUNCTION fValidStockPackageToReplen RETURNS CHARACTER (INPUT intStockPackageID AS INTEGER,
                                                       INPUT logAllowMultipleReplen AS LOGICAL):
   
   DEFINE BUFFER readTaskLineWork   FOR TaskLineWork.
   DEFINE BUFFER readLocation       FOR Location.
   DEFINE BUFFER readLocationType   FOR LocationType.
   DEFINE BUFFER readStockPackage   FOR StockPackage.
   
   FIND FIRST readStockPackage NO-LOCK /* idx=StockpackageID */
      WHERE readStockPackage.StockPackageID = intStockPackageID NO-ERROR.
   IF NOT AVAILABLE readStockPackage THEN
      RETURN "No StockPackage exists for ID:[" + STRING(intStockPackageID) + "] from within [fValidTargetReplenStockPackage].".
   
   FIND FIRST readLocation OF readStockPackage NO-LOCK NO-ERROR. /* idx=LocationID */
   IF NOT AVAILABLE readLocation THEN
      RETURN "No Location exists for ID:[" + STRING(readStockPackage.LocationID) + "] from within [fValidTargetReplenLocation].".
   
   FIND FIRST readLocationType OF readLocation NO-LOCK NO-ERROR. /* idx=LocationTypeID */
   IF NOT AVAILABLE readLocationType THEN
      RETURN "No LocationType exists for ID:[" + STRING(readLocation.LocationTypeID) + "] from within [fValidTargetReplenLocation].".
   
   IF readLocationType.AutoPutawayLocType <> TRUE THEN
      RETURN "Location Type:[" + readLocationType.TypeCode + "] is NOT Valid FOR Replenish - from within [fValidTargetReplenLocation].".
   
   RELEASE readTaskLineWork NO-ERROR.
   
   IF logAllowMultipleReplen = FALSE THEN
   DO:
      IF CAN-FIND(FIRST readTaskLineWork NO-LOCK  /* idx=CompletedSourceLocationID */
                     WHERE readTaskLineWork.Completed = ""
                     AND   readTaskLineWork.LocationID = readStockPackage.LocationID) THEN
         RETURN "Open TaskWork exists with this Location as it's Source, cannot use. From within [fValidTargetReplenLocation].".
   
      RELEASE readTaskLineWork NO-ERROR.
      
      IF CAN-FIND(FIRST readTaskLineWork NO-LOCK  /* idx=CompletedTargetLocationID */
                     WHERE readTaskLineWork.Completed = ""
                     AND   readTaskLineWork.TargetLocationID = readStockPackage.LocationID) THEN
         RETURN "Open TaskWork exists with this Location as it's Target, cannot use. From within [fValidTargetReplenLocation].".
      
      IF CAN-FIND(FIRST ttLocationAlreadyUsed OF readLocation NO-LOCK) THEN /* idx=LocationID */
         RETURN "Location has already been assigned a Replen within this Operation, cannot use. From within [fValidTargetReplenLocation].".
   
      IF CAN-FIND(FIRST ttStockPackageAlreadyUsed OF readStockPackage NO-LOCK) THEN /* idx=StockPackageID */
         RETURN "StockPackage has already been assigned a Replen within this Operation, cannot use." 
                   + " From within [fValidTargetReplenStockPackage].".
                   
   END. /*IF logAllowMultipleReplen = FALSE THEN*/
   
   
   RETURN "Yes".
   
END FUNCTION. /*fValidStockPackageToReplen*/

FUNCTION fNumReplensOutstanding RETURNS INTEGER (INPUT intPartID               AS INTEGER,
                                                 INPUT intSourceLocationTypeID AS INTEGER,
                                                 INPUT intTargetLocationTypeID AS INTEGER):
   
   DEFINE VARIABLE intNumReplensOutstanding AS INTEGER NO-UNDO.
   
   DEFINE BUFFER sourceLocation                  FOR Location.
   DEFINE BUFFER targetLocation                  FOR Location.
   DEFINE BUFFER targetLocationType              FOR LocationType.
   DEFINE BUFFER sourceLocationType              FOR LocationType.
   DEFINE BUFFER readPart                        FOR Part.
   DEFINE BUFFER readTaskType                    FOR TaskType.
   DEFINE BUFFER readTask                        FOR Task.
   DEFINE BUFFER readTaskLine                    FOR TaskLine.
   DEFINE BUFFER readTaskLineWork                FOR TaskLineWork.
   DEFINE BUFFER readPickPackLocEaseOfAccessLink FOR PickPackLocEaseOfAccessLink.
   
   FIND FIRST readPart NO-LOCK /* idx=PartID */
      WHERE readPart.PartID = intPartID NO-ERROR.
   IF NOT AVAILABLE readPart THEN
      RETURN 0.
   
   FIND FIRST sourceLocationType NO-LOCK /* idx=LocationTypeID */
      WHERE sourceLocationType.LocationTypeID = intSourceLocationTypeID NO-ERROR.
   IF NOT AVAILABLE sourceLocationType THEN
      RETURN 0.
   
   FIND FIRST targetLocationType NO-LOCK /* idx=LocationTypeID */
      WHERE targetLocationType.LocationTypeID = intTargetLocationTypeID NO-ERROR.
   IF NOT AVAILABLE targetLocationType THEN
      RETURN 0.
   
   FIND FIRST readTaskType NO-LOCK /* idx=TypeCode */
      WHERE readTaskType.TypeCode = "ShipOrderReplen" NO-ERROR.
   IF NOT AVAILABLE readTaskType THEN
      RETURN 0.
   
   TaskLoop: 
   FOR EACH readTaskLine OF readPart NO-LOCK /* idx=PartIDCompleted */
      WHERE readTaskLine.Completed = "":
      
      FIND FIRST readTask OF readTaskLine NO-LOCK NO-ERROR. /* idx=TaskID */
      IF NOT AVAILABLE readTask OR 
         readTask.TaskTypeID <> readTaskType.TaskTypeID THEN
         NEXT TaskLoop.
      
      TaskWorkLoop:
      FOR EACH readTaskLineWork OF readTaskLine NO-LOCK /* idx=TaskLineIDCompleted */
         WHERE readTaskLineWork.Completed = "":
         
         FIND FIRST sourceLocation NO-LOCK /* idx=LocationID */
            WHERE sourceLocation.LocationID = readTaskLineWork.LocationID NO-ERROR.
         IF AVAILABLE sourceLocation AND 
            sourceLocation.LocationTypeID <> intSourceLocationTypeID THEN
            NEXT TaskWorkLoop.
         
         FIND FIRST targetLocation NO-LOCK /* idx=LocationID */
            WHERE targetLocation.LocationID = readTaskLineWork.TargetLocationID NO-ERROR.
         IF AVAILABLE targetLocation AND 
            targetLocation.LocationTypeID <> intTargetLocationTypeID THEN
            NEXT TaskWorkLoop.
         
         /* Check if target location is setup on a ShipOrderStream */
         FIND FIRST readPickPackLocEaseOfAccessLink NO-LOCK /* idx=LocationIDPickPackStationID */
            WHERE readPickPackLocEaseOfAccessLink.LocationID = readTaskLineWork.TargetLocationID NO-ERROR.
         IF AVAILABLE readPickPackLocEaseOfAccessLink THEN
            NEXT TaskWorkLoop.
             
         intNumReplensOutstanding = intNumReplensOutstanding + 1.
         
      END. /*FOR EACH readTaskLineWork OF readTaskLine NO-LOCK*/
      
   END. /*FOR EACH readTaskLine OF readPart NO-LOCK*/
   
   RETURN intNumReplensOutstanding.
   
END FUNCTION. /*fNumReplensOutstanding*/

FUNCTION fNumReplensOutstandingByLocation RETURNS INTEGER (INPUT intPartID           AS INTEGER,
                                                           INPUT intTargetLocationID AS INTEGER):
   
   DEFINE VARIABLE intNumReplensOutstanding AS INTEGER     NO-UNDO.
   
   DEFINE BUFFER sourceLocation             FOR Location.
   DEFINE BUFFER targetLocation             FOR Location.
   DEFINE BUFFER targetLocationType         FOR LocationType.
   DEFINE BUFFER sourceLocationType         FOR LocationType.
   DEFINE BUFFER readPart                   FOR Part.
   DEFINE BUFFER readTaskType               FOR TaskType.
   DEFINE BUFFER readTask                   FOR Task.
   DEFINE BUFFER readTaskLine               FOR TaskLine.
   DEFINE BUFFER readTaskLineWork           FOR TaskLineWork.
   
   FIND FIRST readPart NO-LOCK /* idx=PartID */
      WHERE readPart.PartID = intPartID NO-ERROR.
   IF NOT AVAILABLE readPart THEN
      RETURN 0.
   
   FIND FIRST targetLocation NO-LOCK /* idx=LocationID */
      WHERE targetLocation.LocationID = intTargetLocationID NO-ERROR.
   IF NOT AVAILABLE targetLocation THEN
      RETURN 0.
   
   FIND FIRST readTaskType NO-LOCK /* idx=TypeCode */
      WHERE readTaskType.TypeCode = "ShipOrderReplen" NO-ERROR.
   IF NOT AVAILABLE readTaskType THEN
      RETURN 0.
   
   TaskLoop: 
   FOR EACH readTaskLine OF readPart NO-LOCK /* idx=PartIDCompleted */
      WHERE readTaskLine.Completed = "":
      
      FIND readTask OF readTaskLine NO-LOCK NO-ERROR. /* idx=TaskID */
      IF NOT AVAILABLE readTask OR 
         (readTask.TaskTypeID <> readTaskType.TaskTypeID) THEN
         NEXT TaskLoop.
      
      TaskWorkLoop:
      FOR EACH readTaskLineWork OF readTaskLine NO-LOCK /* idx=CompletedTargetLocationID */
         WHERE readTaskLineWork.Completed = ""
         AND   readTaskLineWork.TargetLocationID = intTargetLocationID:
         
         intNumReplensOutstanding = intNumReplensOutstanding + 1.
         
      END. /*FOR EACH readTaskLineWork OF readTaskLine NO-LOCK*/
      
   END. /*FOR EACH readTaskLine OF readPart NO-LOCK*/
   
   RETURN intNumReplensOutstanding.
   
END FUNCTION. /*fNumReplensOutstandingByLocation*/

FUNCTION fNumReplensOutstandingForTargetLocation RETURNS INTEGER (INPUT intTargetLocationID AS INTEGER):
   
   DEFINE VARIABLE intNumReplensOutstanding AS INTEGER     NO-UNDO.
   
   DEFINE BUFFER targetLocation             FOR Location.
   DEFINE BUFFER targetLocationType         FOR LocationType.
   DEFINE BUFFER readTaskType               FOR TaskType.
   DEFINE BUFFER readTask                   FOR Task.
   DEFINE BUFFER readTaskLine               FOR TaskLine.
   DEFINE BUFFER readTaskLineWork           FOR TaskLineWork.
   
   FIND FIRST targetLocation NO-LOCK /* idx=LocationID */
      WHERE targetLocation.LocationID = intTargetLocationID NO-ERROR.
   IF NOT AVAILABLE targetLocation THEN
      RETURN 0.
   
   FIND FIRST readTaskType NO-LOCK /* idx=TypeCode */
      WHERE readTaskType.TypeCode = "ShipOrderReplen" NO-ERROR.
   IF NOT AVAILABLE readTaskType THEN
      RETURN 0.
   
   TaskLoop: 
   FOR EACH readTask NO-LOCK /* idx=TaskID */
      WHERE readTask.TaskTypeID = readTaskType.TaskTypeID 
      AND   readTask.Completed = "": 
      
      FOR EACH readTaskLine OF readTask NO-LOCK /* idx=TaskID */
         WHERE readTaskLine.Completed = "": 
     
         FOR EACH readTaskLineWork OF readTaskLine NO-LOCK /* idx=CompletedTargetLocationID */
            WHERE readTaskLineWork.Completed = ""
            AND   readTaskLineWork.TargetLocationID = intTargetLocationID:
            
            intNumReplensOutstanding = intNumReplensOutstanding + 1.
            
         END. /*FOR EACH readTaskLineWork OF readTaskLine NO-LOCK*/
      
      END. /*FOR EACH readTaskLine OF readTask NO-LOCK*/
   
   END. /*FOR EACH readTask NO-LOCK*/   
   
   RETURN intNumReplensOutstanding.
   
END FUNCTION. /*fNumReplensOutstandingForTargetLocation*/

FUNCTION fPartReplensOutstandingForTargetLocation RETURNS INTEGER (INPUT intTargetLocationID AS INTEGER):
   
   DEFINE VARIABLE intPartID AS INTEGER     NO-UNDO.
   
   DEFINE BUFFER targetLocation             FOR Location.
   DEFINE BUFFER targetLocationType         FOR LocationType.
   DEFINE BUFFER readTaskType               FOR TaskType.
   DEFINE BUFFER readTask                   FOR Task.
   DEFINE BUFFER readTaskLine               FOR TaskLine.
   DEFINE BUFFER readTaskLineWork           FOR TaskLineWork.
   
   FIND FIRST targetLocation NO-LOCK /* idx=LocationID */
      WHERE targetLocation.LocationID = intTargetLocationID NO-ERROR.
   IF NOT AVAILABLE targetLocation THEN
      RETURN 0.
   
   FIND FIRST readTaskType NO-LOCK /* idx=TypeCode */
      WHERE readTaskType.TypeCode = "ShipOrderReplen" NO-ERROR.
   IF NOT AVAILABLE readTaskType THEN
      RETURN 0.
   
   TaskLoop: 
   FOR EACH readTask NO-LOCK /* idx=TaskID */
      WHERE readTask.TaskTypeID = readTaskType.TaskTypeID 
      AND   readTask.Completed = "": 
      
      FOR EACH readTaskLine OF readTask NO-LOCK /* idx=TaskID */
         WHERE readTaskLine.Completed = "": 
     
         FOR EACH readTaskLineWork OF readTaskLine NO-LOCK /* idx=CompletedTargetLocationID */
            WHERE readTaskLineWork.Completed = ""
            AND   readTaskLineWork.TargetLocationID = intTargetLocationID:
            
            intPartID = readTaskLine.PartID.
            
            LEAVE TaskLoop.
         END. /*FOR EACH readTaskLineWork OF readTaskLine NO-LOCK*/
      
      END. /*FOR EACH readTaskLine OF readTask NO-LOCK*/
   
   END. /*FOR EACH readTask NO-LOCK*/   
   
   RETURN intPartID.
   
END FUNCTION. /*fPartReplensOutstandingForTargetLocation*/

FUNCTION fNumOutstandingTaskQtyForPartSourceLocation RETURNS INTEGER (INPUT intPartID           AS INTEGER,
                                                                      INPUT chrTaskTypeGroup    AS CHARACTER):
                                        
   DEFINE VARIABLE chrTaskTypeList             AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intTaskStatusCancelled      AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE intTaskStatusAwaitingReplen AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intAllocatedQty             AS INTEGER   NO-UNDO.  
   DEFINE VARIABLE intPalletStockEntityID      AS INTEGER NO-UNDO.
   DEFINE VARIABLE intBoxStockEntityID         AS INTEGER NO-UNDO.
   DEFINE VARIABLE intTotalQtyAssigned         AS INTEGER NO-UNDO.
               
   DEFINE BUFFER readTaskStatus FOR TaskStatus.
   DEFINE BUFFER readStockEntity FOR StockEntity. 
   DEFINE BUFFER readTaskTypeGroup FOR TaskTypeGroup.   
   DEFINE BUFFER readTaskTypeGroupLink FOR TaskTypeGroupLink. 
   DEFINE BUFFER readTask FOR Task.
   DEFINE BUFFER readTaskLine FOR TaskLine.
   DEFINE BUFFER readTaskLineWork FOR TaskLineWork.  
   DEFINE BUFFER readStockPackage FOR StockPackage.  
   DEFINE BUFFER readttStockPackageAlreadyUsed FOR ttStockPackageAlreadyUsed.     
   DEFINE BUFFER totalttStockPackageAlreadyUsed FOR ttStockPackageAlreadyUsed.            
   
   FIND FIRST readTaskStatus NO-LOCK
      WHERE readTaskStatus.StatusCode = "Cancelled" 
      AND   readTaskStatus.Active = TRUE NO-ERROR.
   IF AVAILABLE readTaskStatus THEN
      intTaskStatusCancelled = readTaskStatus.TaskStatusID.
      
   FIND FIRST readStockEntity NO-LOCK /* idx=StockEntityID WHOLE-INDEX */
      WHERE readStockEntity.IsPallet = TRUE NO-ERROR.
   IF AVAILABLE readStockEntity THEN
      intPalletStockEntityID = readStockEntity.StockEntityID.   
   
   RELEASE readStockEntity NO-ERROR.
   
   FIND FIRST readStockEntity NO-LOCK
      WHERE readStockEntity.EntityCode = "Box" NO-ERROR.
   IF AVAILABLE readStockEntity THEN
      intBoxStockEntityID = readStockEntity.StockEntityID.
           
   FIND FIRST readTaskTypeGroup NO-LOCK /* idx=GroupCode */
      WHERE readTaskTypeGroup.GroupCode = chrTaskTypeGroup NO-ERROR.
   IF AVAILABLE readTaskTypeGroup THEN
   DO:
      FOR EACH readTaskTypeGroupLink OF readTaskTypeGroup NO-LOCK /* idx=TaskTypeGroupID */
         WHERE readTaskTypeGroupLink.Active = TRUE:
            
         chrTaskTypeList = chrTaskTypeList + STRING(readTaskTypeGroupLink.TaskTypeID) + ",".
                
      END. /* FOR EACH readTaskTypeGroupLink */
      
      chrTaskTypeList = TRIM(chrTaskTypeList, ",").
                  
   END. /* IF AVAILABLE readTaskTypeGroup */                                        
   
   TaskLineLoop:
   FOR EACH readTaskLine NO-LOCK /* idx=PartIDCompleted */
      WHERE readTaskLine.PartID    = intPartID
      AND   readTaskLine.Completed = "":
      
      IF readTaskLine.TaskStatus = intTaskStatusCancelled OR
         readTaskLine.TaskStatus = intTaskStatusAwaitingReplen THEN 
         NEXT TaskLineLoop.
            
      /* Get Task Record to check TaskType */
      FIND FIRST readTask OF readTaskLine NO-LOCK NO-ERROR. /* idx=TaskID */
      IF NOT AVAILABLE readTask THEN 
         NEXT TaskLineLoop.
      
      /* Skip Task that does not affect Stock Allocation i.e., Unload, ShipPalletBuild, etc. */
      IF NOT CAN-DO(chrTaskTypeList, STRING(readTask.TaskTypeID)) THEN
         NEXT TaskLineLoop.            
      
      WorkLoop:
      FOR EACH readTaskLineWork OF readTaskLine NO-LOCK /* idx=TaskLineIDCompleted */
         WHERE readTaskLineWork.Completed = "":
         
         /* Don't need to account for these since Stock is picked already this can
            happen for replen Task where everything was picked to Tote but Target Location
            have not been scanned (TLW.Completed are populated when all Totes linked to that TLW are
            scanned to the Target Location. */
         IF readTaskLineWork.QtyAssigned = readTaskLineWork.QtyCompleted THEN
            NEXT WorkLoop.     
         
         /* If the TaskLineWork is Cancelled then skip it */
         IF readTaskLineWork.TaskStatusID = intTaskStatusCancelled OR
            readTaskLineWork.TaskStatusID = intTaskStatusAwaitingReplen THEN
            NEXT WorkLoop.
         
         intAllocatedQty = readTaskLineWork.QtyAssigned - readTaskLineWork.QtyCompleted.
         
         StockPackageLoop:
         FOR EACH readStockPackage NO-LOCK /* idx=PartIDDetrashed */
            WHERE readStockPackage.PartID = readTaskLine.PartID 
            AND   readStockPackage.LocationID = readTaskLineWork.LocationID
            AND   readStockPackage.StockStatusID = readTaskLineWork.StockStatusID
            AND   readStockPackage.Detrashed = ""
            AND   readStockPackage.ParentStockPackageID = 0
            BY    readStockPackage.Created:
            
            IF intAllocatedQty <= 0 THEN
               LEAVE StockPackageLoop.
            
            /* Only replen boxes or full pallets */
            IF intBoxStockEntityID <> readStockPackage.StockEntityID AND 
               intPalletStockEntityID <> readStockPackage.StockEntityID THEN
               NEXT StockPackageLoop.
            
            RELEASE readttStockPackageAlreadyUsed NO-ERROR.
            
            /* Check this StockPackage has not already been used for this TaskLineWork, If so skip */
            FIND FIRST readttStockPackageAlreadyUsed NO-LOCK
               WHERE readttStockPackageAlreadyUsed.TaskLineID = readTaskLine.TaskLineID 
               AND   readttStockPackageAlreadyUsed.TaskLineWorkID = readTaskLineWork.TaskLineWorkID 
               AND   readttStockPackageAlreadyUsed.StockPackageID = readStockPackage.StockPackageID NO-ERROR.
            IF AVAILABLE readttStockPackageAlreadyUsed THEN
               LEAVE StockPackageLoop.
            
            /* Clear the StockPackage Qty Assigned count for this StockPackage Record */
            intTotalQtyAssigned = 0.
            
            /* Calculate the total qty assigned so far for this StockPackage */
            FOR EACH totalttStockPackageAlreadyUsed NO-LOCK
               WHERE totalttStockPackageAlreadyUsed.StockPackageID = readStockPackage.StockPackageID:
               
               intTotalQtyAssigned = intTotalQtyAssigned + totalttStockPackageAlreadyUsed.QtyAssigned.
            
            END. /*FOR EACH totalttStockPackageAlreadyUsed NO-LOCK*/ 
            
            /* If all assigned go to the next StockPackage */      
            IF intTotalQtyAssigned >= readStockPackage.PackageQty THEN
               NEXT StockPackageLoop.
            
            /* Qty left to assign for this StockPackage so apply it towards the TaskLineWork */
            CREATE totalttStockPackageAlreadyUsed.   
            ASSIGN totalttStockPackageAlreadyUsed.StockPackageID = readStockPackage.StockPackageID
                   totalttStockPackageAlreadyUsed.LocationID     = readStockPackage.LocationID
                   totalttStockPackageAlreadyUsed.TaskID         = readTaskLine.TaskID
                   totalttStockPackageAlreadyUsed.TaskLineID     = readTaskLine.TaskLineID
                   totalttStockPackageAlreadyUsed.TaskLineWorkID = readTaskLineWork.TaskLineWorkID
                   totalttStockPackageAlreadyUsed.PartID         = readTaskLine.PartID.
            
            /* If StockPackage is the same or more than what we need to assign then set the value and leave the Loop */                   
            IF (readStockPackage.PackageQty - intTotalQtyAssigned) >= intAllocatedQty THEN
            DO:
               totalttStockPackageAlreadyUsed.QtyAssigned = intAllocatedQty.
               LEAVE StockPackageLoop.
            END. /*IF readStockPackage.PackageQty - intTotalQtyAssigned >= intAllocatedQty THEN*/
            
            /* If StockPackage is less than what we need to assign so only assign the available units and go to the next StockPackage */
            ASSIGN totalttStockPackageAlreadyUsed.QtyAssigned = readStockPackage.PackageQty - intTotalQtyAssigned     
                   intAllocatedQty = intAllocatedQty - totalttStockPackageAlreadyUsed.QtyAssigned.
            NEXT StockPackageLoop.
            
         END. /*StockPackageLoop:*/          
               
      END. /*FOR EACH TaskLineWork OF TaskLine NO-LOCK*/
      
   END. /*FOR EACH TaskLine NO-LOCK*/
   
   intAllocatedQty = 0.
   
   FOR EACH readttStockPackageAlreadyUsed NO-LOCK
     WHERE readttStockPackageAlreadyUsed.PartID = intPartID:
        
     intAllocatedQty = intAllocatedQty + readttStockPackageAlreadyUsed.QtyAssigned.
     
   END. /*FOR EACH readttStockPackageAlreadyUsed NO-LOCK*/
   
   RETURN intAllocatedQty.
      
END FUNCTION. /*fNumOutstandingTaskQtyForPartSourceLocation*/
