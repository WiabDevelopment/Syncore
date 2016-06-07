/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncRelocateFunctions.i
Purpose : All functions to do with Relocate    
Author  : Shane Conaty  
Date    : 23/01/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
05/12/2014 BG  AmazonCA   Took out session BusinessUnit validation.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* This include has a dependency on */
/* {fncStatusTypeFunctions.i} */

FUNCTION fValidateCanRelocate RETURNS CHARACTER (INPUT intPackageID  AS INTEGER,
                                                 INPUT intLocationID AS INTEGER).

   /* Buffers */
   DEFINE BUFFER otherPart                    FOR Part.    
   DEFINE BUFFER otherLocation                FOR Location. 
   DEFINE BUFFER otherPackage                 FOR StockPackage. 
   DEFINE BUFFER packageBusinessUnit          FOR BusinessUnit.
   DEFINE BUFFER locationBusinessUnit         FOR BusinessUnit.
   DEFINE BUFFER otherRelocateStockEntityRule FOR RelocateStockEntityRule.

   /* Local Variables */
   DEFINE VARIABLE intNumPartsStored  AS INTEGER NO-UNDO.
   DEFINE VARIABLE logStockEntityRule AS LOGICAL NO-UNDO.

   FIND FIRST StockPackage NO-LOCK
      WHERE StockPackage.StockPackageID = intPackageID NO-ERROR.
   IF NOT AVAILABLE StockPackage THEN
      RETURN "No [StockPackage] exists for [PackageID]:[" + STRING(intPackageID) + "]".
   
   FIND FIRST Location NO-LOCK
      WHERE Location.LocationID = intLocationID NO-ERROR.
   IF NOT AVAILABLE Location THEN
      RETURN "No [Location] exists for [LocationID]:[" + STRING(intLocationID) + "]".

   FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      RETURN "No [LocationType] exists for [LocationRef]:[" + Location.LocationRef + "]".

   FIND FIRST Part OF StockPackage NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Part THEN
      RETURN "No [Part] exists For [PartID] [" + STRING(StockPackage.PartID) + "].".
   
   FIND FIRST StockEntity OF StockPackage NO-LOCK NO-ERROR.
   IF NOT AVAILABLE StockEntity THEN
      RETURN "No [StockEntity] exists For [StockEntityID] [" + STRING(StockPackage.StockEntityID) + "].".
   
   FIND FIRST PartStockEntityLink OF StockPackage NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PartStockEntityLink THEN
      RETURN "No [PartStockEntityLink] exists For [Part] [" + part.PartRef + "] and [StockEntity] [" 
               + StockEntity.EntityCode + "].".

   /* There are certain locationTypes which we do not need to check that there are Rules for.   */
   /* eg A user should always be able to relocate to a build Location.                          */
   FIND FIRST LocationTypeGroup NO-LOCK 
      WHERE LocationTypeGroup.GroupCode = "TargetLocationsWithoutRelocateRules" NO-ERROR.
   IF NOT AVAILABLE LocationTypeGroup THEN
      RETURN "No [LocationTypeGroup] exists with [GroupCode] [TargetLocationsWithoutRelocateRules].".
      
   FIND FIRST LocationTypeGroupLink OF LocationTypeGroup NO-LOCK 
      WHERE LocationTypeGroupLink.LocationTypeID = LocationType.LocationTypeID 
      AND   LocationTypeGroupLink.Active         = YES NO-ERROR.
   
   /* If the LocationType is not Linked to the Group then we want to go through the normal Rule check. */   
   IF NOT AVAILABLE LocationTypeGroupLink THEN 
   DO:
     
      /* Check all Active RelocatePartStockEntityRules for this Part/Entity combo */
      FOR EACH RelocatePartStockEntityRule NO-LOCK /* idx=PartStockEntityLinkIDActive */
         WHERE RelocatePartStockEntityRule.PartStockEntityLinkID = PartStockEntityLink.PartStockEntityLinkID
         AND   RelocatePartStockEntityRule.Active                = YES.
   
         /* If we Find a Direct Rule for the scanned Location OR LocationType, we want to notify the user. */
         IF  RelocatePartStockEntityRule.RuleAction = "DIRECT" THEN 
         DO:
            IF  RelocatePartStockEntityRule.LocationID <> Location.LocationID
            AND RelocatePartStockEntityRule.LocationTypeID <> Location.LocationTypeID THEN
               RETURN "There is an Active [RelocatePartStockEntityRule] which is not for [Location] [" + Location.LocationRef + "].".
         END.
         
         /* If we Find a Prevent Rule for the scanned Location OR LocationType, we want to notify the user. */
         IF  RelocatePartStockEntityRule.RuleAction = "PREVENT" 
         AND RelocatePartStockEntityRule.LocationID = Location.LocationID THEN
            RETURN "A Part/StockEntity Relocate Rule is set up to PREVENT relocate of a [" + StockEntity.EntityCode + "] of [Part:] [" 
                     + Part.PartRef + "]  to [Location:] [" + Location.LocationRef + "].".
            
         IF  RelocatePartStockEntityRule.RuleAction = "PREVENT" 
         AND RelocatePartStockEntityRule.LocationTypeID = Location.LocationTypeID THEN
            RETURN "A Part/StockEntity Relocate Rule is set up to PREVENT relocate of a [" + StockEntity.EntityCode + "] of [Part:] [" 
                     + Part.PartRef + "]  to [LocationType:] [" + LocationType.TypeCode + "].".
      
      END. /* FOR EACH RelocatePartStockEntityRule */
      
      /* If there is NO RelocatePartStockEntityRule then we want to go through this validation. */
      IF NOT CAN-FIND(FIRST RelocatePartStockEntityRule NO-LOCK
                      WHERE RelocatePartStockEntityRule.PartStockEntityLinkID = PartStockEntityLink.PartStockEntityLinkID
                      AND   RelocatePartStockEntityRule.RuleAction = "DIRECT"
                      AND   RelocatePartStockEntityRule.Active = YES)    THEN
      DO:
         
         FIND FIRST RelocateStockEntityRule NO-LOCK
            WHERE RelocateStockEntityRule.LocationTypeID = Location.LocationTypeID
            AND   RelocateStockEntityRule.StockEntityID = StockEntity.StockEntityID
            AND   RelocateStockEntityRule.Active = YES NO-ERROR.
         /* If there is no RelocateStockEntityRule for the LocationType scanned then we want to go through this validation. */
         IF NOT AVAILABLE RelocateStockEntityRule THEN
         DO:
            FIND FIRST otherRelocateStockEntityRule NO-LOCK
               WHERE otherRelocateStockEntityRule.LocationTypeID <> Location.LocationTypeID
               AND   otherRelocateStockEntityRule.StockEntityID = StockEntity.StockEntityID
               AND   otherRelocateStockEntityRule.Active = YES NO-ERROR.
            /* If there is a RelocateStockEntityRule for another LocationType but none for the LocationType scanned, */ 
            /* then we want to Return an Error.                                                                      */
            IF AVAILABLE otherRelocateStockEntityRule THEN
               RETURN "There is no Active [RelocateStockEntityRule] for [Entity] [" + StockEntity.EntityCode + "], [Part] ["
                  + Part.PartRef + "] and [LocationType] [" + LocationType.TypeCode + "].".
         END.
      END. /* NOT CAN-FIND RelocatePartStockEntityRule */
      
   END. /* IF NOT AVAILABLE LocationTypeGroupLink */
   
   /* Check if the LocationType can Store Multiple Part IDs */
   IF LocationType.StoreMultiPart = FALSE THEN
   DO:
      /* Check there is not a Different Part Already Stored here */
      FIND FIRST otherPackage NO-LOCK 
         WHERE otherPackage.LocationID    =  Location.LocationID
         AND   otherPackage.PartID        <> StockPackage.PartID NO-ERROR.

      IF AVAILABLE otherPackage THEN
      DO:
         /* Get its Part */
         FIND FIRST otherPart OF otherPackage NO-LOCK NO-ERROR.
         RETURN "Stock for Part:[" + otherPart.PartRef + "] is already stored in Location:[" + Location.LocationRef 
                               + "]. Location Type:[" + LocationType.TypeName + "] cannot store multiple Parts.".
      END.
   END. /*IF LocationType.StoreMultiPart = FALSE*/
   IF LocationType.StoreMultiPart = TRUE THEN
   DO: /* Check on MaxNumDifferentParts limit*/
      
      FOR EACH otherPackage NO-LOCK 
         WHERE otherPackage.LocationID =  Location.LocationID
         AND   otherPackage.PartID     <> StockPackage.PartID
         BREAK BY otherPackage.PartID:
         
         IF FIRST-OF(otherPackage.PartID) THEN
            intNumPartsStored = intNumPartsStored + 1.
      END.
      
      IF intNumPartsStored >= Location.MaxNumDifferentParts THEN
         RETURN "There are " + STRING(intNumPartsStored) + " Parts already stored in Location:[" + Location.LocationRef 
                  + "]. Only " + STRING(Location.MaxNumDifferentParts) + " Parts can be stored in this Location.".
            
   END. /* LocationType.StoreMultiPart = TRUE */

   /* If Location Type cannot store stock with different statuses and there is already stock at the Target Location */
   /* which is at a different status, then we want to inform the user.                                              */
   IF LocationType.StoreMultiStockStatus = FALSE THEN
   DO:
      FIND FIRST otherPackage NO-LOCK /* idx=LocationIDStockStatusID */
         WHERE otherPackage.LocationID = Location.LocationID
         AND   otherPackage.StockStatusID <> StockPackage.StockStatusID NO-ERROR.
      IF AVAILABLE otherPackage THEN
      DO:
         RETURN "There is already Stock in this [Location] which is at a Different [Status]:[" 
                    + fGetStatusName('Stock',otherPackage.StockStatusID) + "]".
      END.
   END.

   /* Validation if the BusinessUnit on the Location is set */
   IF Location.BusinessUnitID <> 0 THEN
   DO: 
      FIND FIRST locationBusinessUnit OF Location NO-LOCK NO-ERROR.
      IF NOT AVAILABLE locationBusinessUnit THEN
         RETURN "No [BusinessUnit] exists for [BusinessUnitID] [" + STRING(Location.BusinessUnitID) + "]".
      
      /* Validate If BusinessUnitID's Are The Same */
      IF Location.BusinessUnitID <> StockPackage.BusinessUnitID THEN
      DO:
         FIND FIRST packageBusinessUnit OF StockPackage NO-LOCK NO-ERROR.
         IF NOT AVAILABLE packageBusinessUnit THEN
            RETURN "No [BusinessUnit] exists for [BusinessUnitID] [" + STRING(StockPackage.BusinessUnitID) + "]".
         
         RETURN "[Location] [" + Location.LocationRef + "] is For [BusinessUint] [" + locationBusinessUnit.UnitName 
                  + "]. Package [" + StockPackage.PackageRef + "] is For [BusinessUint] [" + packageBusinessUnit.UnitName 
                  + "]. Scan Another ".
      END.
   END.   /* END Location.BusinessUnitID <> 0 */
   
   /* Check that there is not another StockPackage in that location */
   FIND FIRST otherPackage NO-LOCK
      WHERE otherPackage.LocationID = Location.LocationID
      AND   otherPackage.BusinessUnitID <> StockPackage.BusinessUnitID NO-ERROR.
   IF AVAILABLE otherPackage THEN
   DO:
      RETURN "[Location] [" + Location.LocationRef + "] already has Stock in it of a Different BusinessUnit.".
   END.

   /* Check there are no Open TaskLineWorks for the source location  */
   FIND FIRST TaskLineWork NO-LOCK
      WHERE TaskLineWork.LocationID = StockPackage.LocationID
      AND   TaskLineWork.Completed  = "" NO-ERROR.
   IF AVAILABLE TaskLineWork THEN
   DO:
      FIND FIRST TaskLine OF TaskLineWork NO-LOCK NO-ERROR.
      FIND FIRST Task OF TaskLine NO-LOCK NO-ERROR.
      
      FIND FIRST TaskType NO-LOCK 
         WHERE TaskType.TypeCode = "Unload" NO-ERROR.
      
      /* We only want to continue if the incomplete TLW is NOT an Unloading Task. */
      IF TaskType.TaskTypeID <> Task.TaskTypeID THEN 
      DO:         
         /* Find the Location of the TLW */
         FIND FIRST otherLocation OF TaskLineWork NO-LOCK NO-ERROR.
         
         RETURN "There is an Open [TaskWork] [" + STRING(TaskLineWork.TaskLineWorkID) + "] for Source [Location] [" 
            + otherLocation.LocationRef + "].".
      END.      
   END.
   
   /* Check there are no Open TaskLineWorks for the target location  */
   FIND FIRST TaskLineWork NO-LOCK
      WHERE TaskLineWork.TargetLocationID = Location.LocationID
      AND   TaskLineWork.Completed = "" NO-ERROR.

   IF AVAILABLE TaskLineWork THEN
   DO:
      RETURN "There is an Open [TaskWork] [" + STRING(TaskLineWork.TaskLineWorkID) + "] for Target [Location] [" 
         + Location.LocationRef + "].".
   END.

   RETURN "OK".
END. /* End Function */


FUNCTION fRelocateChildrenOfPackage RETURNS CHARACTER (INPUT intPackageID  AS INTEGER,
                                                       INPUT intLocationID AS INTEGER).
   
   ChildPackageUpdateLoop:
   DO ON ERROR UNDO, LEAVE:

      /* Buffers */
      DEFINE BUFFER childPackage   FOR StockPackage.
      DEFINE BUFFER targetLocation FOR Location.
      
      /* DB Objects */
      DEFINE VARIABLE updChildPackage AS updRecord.

      /* Local Variables */
      DEFINE VARIABLE chrChildUpdateResult AS CHARACTER.  
   
      IF NOT CAN-FIND(FIRST childPackage NO-LOCK 
                      WHERE childPackage.ParentStockPackageID = intPackageID) THEN
         RETURN "OK".

      FIND FIRST targetLocation NO-LOCK
         WHERE targetLocation.LocationID = intLocationID NO-ERROR.
      IF NOT AVAILABLE targetLocation THEN
      DO:     
         UNDO ChildPackageUpdateLoop, RETURN "No [Location] exists for [LocationID]:[" + STRING(intLocationID) + "]".
      END.
   
      FOR EACH childPackage NO-LOCK
         WHERE childPackage.ParentStockPackageID = intPackageID.
   
         chrChildUpdateResult = fRelocateChildrenOfPackage(INPUT childPackage.StockPackageID,
                                                           INPUT intLocationID).
    
         /* Check the Result from the function */
         IF chrChildUpdateResult <> "OK" THEN 
         DO:        
            UNDO ChildPackageUpdateLoop, RETURN chrChildUpdateResult.
         END.
   
         updChildPackage = fGetRecord("StockPackage", childPackage.StockPackageID).
         
         /* Check for WebLocks */
         IF updChildPackage:RecordLocked THEN 
         DO:  
            RUN DisplayMessage("Record Locked",
                               updChildPackage:getErrors()).   
            UNDO ChildPackageUpdateLoop, RETURN "Error Updating ChildPackage.".
         END.
         
         /* Update the Location on the Stock Package */ 
         updChildPackage:assignField("LocationID", targetLocation.LocationID).
         
         /* Conditionally update putaway timestamp */
         IF (childPackage.Putaway = "" AND targetLocation.LocationTypeID <> fGetTypeID("Location", "ReceivingBay")) THEN
            updChildPackage:assignField("Putaway", fTimeStamp(NOW)).
                   
         chrError = chrError + updChildPackage:getErrors().

         IF chrError <> "" THEN
            RETURN chrError.
      
         /* Delete Object here to avoid building up Objects in the stack */
         DELETE OBJECT updChildPackage NO-ERROR.

      END.
      RETURN "OK".
   END.
END. /* End Function */

FUNCTION fGetChildPackageVersions RETURNS CHARACTER(INPUT intParentPackageID AS INTEGER).

   /* Buffers */
   DEFINE BUFFER childrenStockPackage FOR StockPackage.

   /* Local Variables */   
   DEFINE VARIABLE chrGetVersionError AS CHARACTER.
   
   /* DB Objects */
   DEFINE VARIABLE updChildPackage AS updRecord.

   IF NOT CAN-FIND(FIRST childrenStockPackage NO-LOCK
                   WHERE childrenStockPackage.ParentStockPackageID = intParentPackageID) THEN
      RETURN "OK".

   FOR EACH childrenStockPackage NO-LOCK
      WHERE childrenStockPackage.ParentStockPackageID = intParentPackageID:

      chrGetVersionError = fGetChildPackageVersions(INPUT childrenStockPackage.StockPackageID).
      
      IF chrGetVersionError <> "OK" THEN
         RETURN chrGetVersionError.
      
      updChildPackage = fGetVersion("StockPackage", childrenStockPackage.StockPackageID).

      /* Check any Errors and Display */
      chrError = chrError + updChildPackage:getErrors().
      
      IF chrError <> "" THEN 
         RETURN chrError.

      DELETE OBJECT updChildPackage NO-ERROR.

   END.

   RETURN "OK".

END. /* End Function */
