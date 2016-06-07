/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsFindSuggestedLocationForPart.p
Purpose : Finds all locations where the Part is stored which meet these criteria
          -Must be in a Valid Location for relocate;
          -Must be in a Valid LocationType for the Part and Entity
          Then finds the first location by PutawaySequence with the lowest qty

          Possible Results : Continue

Author  : Maria Chereches
Date    : 11th June 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
31/10/2014 BG  GHD        Added a piece to exclude Target Replen LocationType if Auto Replen is turned on, we don't want relocates going
                          to the LocationType that Replen is also filling up.
17/04/2015 BG  CanonTlb   Added a block to find PartProfileOrderStreamLink records for Stock that we haven't received before.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Relocate Functions */
   {fncStatusTypeFunctions.i}
   {fncRelocateFunctions.i}
   
   /* pGetNextEmptyLocationByWalkSequence */
   {fncReplenFunctions.i}     
   {prcReplenProcedures.i}
   {getReplenOptions.i}
   
   
   /* Local Variables */
   DEFINE VARIABLE chrValidLocation               AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrReturnError                 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intNextLocationID              AS INTEGER   NO-UNDO.
   
   /* Buffers */
   DEFINE BUFFER otherStockPackage                FOR StockPackage.
   DEFINE BUFFER oldestPackage                    FOR StockPackage.
   DEFINE BUFFER otherLocation                    FOR Location.
   DEFINE BUFFER otherLocationType                FOR LocationType.
   DEFINE BUFFER replenLocationType               FOR LocationType.
   DEFINE BUFFER pickLocationTypeGroup            FOR LocationTypeGroup.
   
   /* Temp Tables */
   DEFINE TEMP-TABLE ttPartLocation
      FIELD LocationID                            AS INTEGER
      FIELD LocationTypeID                        AS INTEGER
      FIELD PutawaySequence                       AS INTEGER
      FIELD LocationQty                           AS INTEGER
      FIELD WalkSequence                          AS INTEGER
      FIELD EaseOfAccess                          AS INTEGER
      INDEX LocationID                            
         LocationID
      INDEX PutawaySequenceLocationQty       
         PutawaySequence 
         LocationQty
      INDEX WalkSequenceEaseOfAccess 
         WalkSequence 
         EaseOfAccess.
   
   /* Session Objects */
   DEFINE VARIABLE intSsnPackageID                AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnSuggestedLocationID      AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnSuggestedLocationTypeRef AS sessionValue NO-UNDO.

   /* Clear Session Data */
   fClearSessionValue("SuggestedLocationID").
   
   /* Get Current Session Data */
   intSsnPackageID = fGetSessionValue("PackageID").
   chrSsnSuggestedLocationTypeRef = fGetSessionValue("SuggestedLocationTypeRef").
   
   /* Create New Session Data */
   intSsnSuggestedLocationID = fNewSessionValue("SuggestedLocationID").
   
   Main_Block:
   DO ON ERROR UNDO:
      
      /* See if there is an AutoPutaway LocationType for Full Pallet picks */
      FIND replenLocationType NO-LOCK
         WHERE replenLocationType.AutoReplen = TRUE NO-ERROR.
      
      /* Get the Stock Package record */
      FIND FIRST StockPackage NO-LOCK
         WHERE StockPackage.StockPackageID = intSsnPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN
      DO:
         RUN DisplayError("[Record Not Found]",
                          "[Stock Package ID] [" + STRING(intSsnPackageID:intValue)
                           +  "] does not exist.").
         LEAVE Main_Block.
      END.
      
      FIND FIRST Part OF StockPackage NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Part THEN
      DO:
         RUN DisplayError("[Record Not Found]",
                          "[Part ID] [" + STRING(StockPackage.PartID) +
                          "] does not exist.").
         LEAVE Main_Block.
      END.
      
      FIND FIRST PartStockEntityLink OF Part NO-LOCK
          WHERE PartStockEntityLink.StockEntityID = StockPackage.StockEntityID NO-ERROR.
      IF NOT AVAILABLE PartStockEntityLink THEN
      DO:
         RUN DisplayError("[Record Not Found]",
                          "[StockEntity ID] [" +
                          STRING(StockPackage.StockEntityID)  + "] for Part [" +
                          Part.PartRef + "] does not exist.").
         LEAVE Main_Block.
      END.
      
      /* Get the LocationTypeGroup  record */
      FIND FIRST pickLocationTypeGroup NO-LOCK
         WHERE pickLocationTypeGroup.GroupCode = "PickLocations" NO-ERROR.
      IF NOT AVAILABLE pickLocationTypeGroup THEN
      DO:
         RUN DisplayError("[Record Not Found]",
                          "[LocationTypeGroup] :PickLocations does not exist.").
         LEAVE Main_Block.
      END.
      
      FIND FIRST StockEntity OF StockPackage NO-LOCK NO-ERROR.
      IF NOT AVAILABLE StockEntity THEN
      DO:
         RUN DisplayError("[Record Not Found]",
                          "[Stock Package ID] [" + STRING(StockPackage.StockPackageID)
                           +  "] does not exist.").
         LEAVE Main_Block.
      END.
      
      OtherPackageLoop:
      FOR EACH otherStockPackage OF Part NO-LOCK
         WHERE otherStockPackage.Detrashed = "",
            EACH otherLocation OF otherStockPackage NO-LOCK,
               EACH otherLocationType OF otherLocation NO-LOCK:
               
               /* Don't want Stock that's not a matching Stock Status */
               IF otherStockPackage.StockStatusID <> StockPackage.StockStatusID THEN
                  NEXT OtherPackageLoop.
               
               /* If we're using Auto Replen then we don't want to suggest target Replen LocationType so skip anything in there */
               IF logUsingAutoReplenishFunctionality AND AVAILABLE replenLocationType THEN
               DO:
                  /* Don't want Stock that's not a matching Stock Status */
                  IF otherLocation.LocationTypeID = replenLocationType.LocationTypeID THEN
                     NEXT OtherPackageLoop.
               END.
               
               /* If the scanned package is NOT a Pallet & is not the same Entity as this OtherPackage then Next */
               IF StockEntity.IsPallet = NO AND StockPackage.StockEntityID <> otherStockPackage.StockEntityID THEN
                  NEXT OtherPackageLoop.
               
               FIND FIRST ttPartLocation
                  WHERE ttPartLocation.LocationID = otherStockPackage.LocationID NO-ERROR.
               IF NOT AVAILABLE ttPartLocation THEN
               DO:
                  CREATE ttPartLocation.
                  ASSIGN ttPartLocation.LocationID = otherStockPackage.LocationID.                  
               END.
               
               ttPartLocation.LocationQty = ttPartLocation.LocationQty + otherStockPackage.PackageQty.
               
      END. /*FOR EACH otherStockPackage OF Part NO-LOCK */
      
      /* Now we have a list of Locations where this Part resides inthe Warehouse */
      PartLocationLoop:
      FOR EACH ttPartLocation,
         EACH Location NO-LOCK
            WHERE Location.LocationID = ttPartLocation.LocationID
            AND   Location.Active  = YES,
               EACH LocationType OF Location NO-LOCK:
               
               ASSIGN ttPartLocation.LocationTypeID = Location.LocationTypeID
                      ttPartLocation.WalkSequence   = Location.WalkSequence
                      ttPartLocation.EaseOfAccess   = Location.EaseOfAccess.
               
               /* If we have a LocationType(s) set in the session and this LocationType is not in it then   */
               /* we want to move on the next one.                                                          */
               IF chrSsnSuggestedLocationTypeRef:chrValue <> "" THEN
               DO:
                  FIND FIRST RelocatePartStockEntityRule NO-LOCK
                     WHERE RelocatePartStockEntityRule.LocationTypeID        = ttPartLocation.LocationTypeID
                     AND   RelocatePartStockEntityRule.PartStockEntityLinkID = PartStockEntityLink.PartStockEntityLinkID /*StockPackage.StockEntityID*/
                     AND   RelocatePartStockEntityRule.RuleAction            = "Direct"
                     AND   RelocatePartStockEntityRule.ACTIVE                = YES NO-ERROR.
                  IF NOT AVAILABLE RelocatePartStockEntityRule THEN
                  DO:
                     FIND FIRST RelocateStockEntityRule NO-LOCK
                        WHERE RelocateStockEntityRule.LocationTypeID = ttPartLocation.LocationTypeID
                        AND   RelocateStockEntityRule.StockEntityID  = StockPackage.StockEntityID /*PartStockEntityLink.PartStockEntityLinkID*/
                        AND   RelocateStockEntityRule.ACTIVE         = YES NO-ERROR.
                     IF NOT AVAILABLE RelocateStockEntityRule THEN
                     DO:
                        DELETE ttPartLocation NO-ERROR.
                        NEXT PartLocationLoop.
                     END.

                  END. /*IF NOT AVAILABLE RelocatePartStockEntityRule THEN*/
               END. /*IF chrSsnSuggestedLocationTypeRef:chrValue <> "" THEN*/

               /* Validate Location is OK for StockPackage */
               chrValidLocation = fValidateCanRelocate(INPUT StockPackage.StockPackageID,
                                                       INPUT Location.LocationID).

               /* If the Location is NOT OK then notify the user and Leave */
               IF chrValidLocation <> "OK" THEN
               DO:
                  DELETE ttPartLocation NO-ERROR.
                  NEXT PartLocationLoop.
               END.

               /* At this point we know the locationtype is either not set in the session or it is set and */
               /* the current locationtype is valid.                                                       */
               ttPartLocation.PutawaySequence = LocationType.PutawaySequence.

      END.  /*FOR EACH ttPartLocation:*/
      
      /* Just setting the default value here in case we leave any of these subsequent loops */
      intSsnSuggestedLocationID:setValue(0).
      
      /* If it is NOT a Pallet then we want to search by PutawaySequence by LocationQty */
      IF StockEntity.IsPallet = NO THEN
      DO:
         BestPartLocationLoop:
         FOR EACH ttPartLocation /*idx=PutawaySequenceLocationQty*/
            BY ttPartLocation.PutawaySequence
            BY ttPartLocation.LocationQty:
            
            IF ttPartLocation.PutawaySequence > 0 THEN
            DO:
               /* For Non Pallets its ok to suggest a current Location to co-house the new Stock with the existing Stock */
               intSsnSuggestedLocationID:setValue(ttPartLocation.LocationID).
               
               LEAVE BestPartLocationLoop.
            END.
            
         END. /*FOR EACH ttPartLocation*/
      END. /* IF StockEntity.IsPallet = NO THEN*/
      
      IF intSsnSuggestedLocationID:intValue = 0 THEN
      DO:
         /* This loop appears to go through the Pallets in the warehouse by FIFO */
         IF StockEntity.IsPallet = YES THEN
         DO:
            OldestPackageLoop:
            FOR EACH oldestPackage NO-LOCK /* idx=PartIDDetrashed */
               WHERE oldestPackage.PartID    = StockPackage.PartID
               AND   oldestPackage.Detrashed = "",
                  EACH otherLocation OF oldestPackage NO-LOCK,
                     EACH otherLocationType OF otherLocation NO-LOCK
                     BY    oldestPackage.Created:
                     
                     /* Don't want Stock that's not a matching Stock Status */
                     IF oldestPackage.StockStatusID <> StockPackage.StockStatusID THEN
                        NEXT OldestPackageLoop.
                     
                     /* If we're using Auto Replen then we don't want to suggest target Replen LocationType so skip anything in there */
                     IF logUsingAutoReplenishFunctionality AND AVAILABLE replenLocationType THEN
                     DO:
                        /* Don't want Stock that's not a matching Stock Status */
                        IF otherLocation.LocationTypeID = replenLocationType.LocationTypeID THEN
                           NEXT OldestPackageLoop.
                     END.
                     
                     /* Get the next Location By Walk Sequence */
                     RUN pGetNextEmptyLocationByWalkSequence(INPUT oldestPackage.LocationID, 
                                                             OUTPUT intNextLocationID,
                                                             OUTPUT chrReturnError).  

                     /* If an Error is Returned by pGetNextEmptyLocationByWalkSequence, then move onto the next Location */
                     IF chrReturnError <> "" THEN
                     DO:
                        /* Reset chrReturnError */
                        chrReturnError = "".                  
                        NEXT OldestPackageLoop.
                     END.

                     /* Validate Location is OK for StockPackage */
                     chrValidLocation = fValidateCanRelocate(INPUT StockPackage.StockPackageID,
                                                             INPUT intNextLocationID).

                     /* If we have a Valid Location then set the Location & Result and Leave */
                     IF chrValidLocation = "OK" THEN
                     DO:
                        intSsnSuggestedLocationID:setValue(intNextLocationID).
                        chrResult = "Continue".        
                        LEAVE Main_Block.
                     END.
                     
            END.  /* OldestPackageLoop */
            
            /* intSsnSuggestedLocationID is still 0 */
            /* If we dont have a suggested location ID by here, we just want to go through the existing Locations in  */
            /* LocationTypes for Picking and get first empty one.                                                     */
            LocationTypeLoop:             
            FOR EACH LocationTypeGroupLink OF pickLocationTypeGroup NO-LOCK,
               EACH LocationType OF LocationTypeGroupLink NO-LOCK
                  BY LocationType.PutawaySequence:
               
                  IF LocationType.PutawaySequence <= 0 THEN
                     NEXT LocationTypeLoop.
   
                  /* If we're using Auto Replen then we don't want to suggest target Replen LocationType so skip anything in there */
                  IF logUsingAutoReplenishFunctionality AND AVAILABLE replenLocationType THEN
                  DO:
                     IF LocationType.LocationTypeID = replenLocationType.LocationTypeID THEN
                        NEXT LocationTypeLoop.
                  END.
                  
                  LocationLoop:
                  FOR EACH Location OF LocationType NO-LOCK
                     WHERE Location.Active = TRUE:
                     
                     IF CAN-FIND(FIRST otherStockPackage OF Location NO-LOCK) THEN
                        NEXT LocationLoop.
                     
                     /* Validate Location is OK for StockPackage */
                     chrValidLocation = fValidateCanRelocate(INPUT StockPackage.StockPackageID,
                                                             INPUT Location.LocationID).
                     
                     /* If we have a Valid Location then set the Location & Result and Leave */
                     IF chrValidLocation = "OK" THEN
                     DO:
                        intSsnSuggestedLocationID:setValue(Location.LocationID).
                        chrResult = "Continue".        
                        LEAVE Main_Block.
                     END.
                     
                  END. /*FOR EACH Location OF LocationType NO-LOCK.*/
                  
            END. /*FOR EACH LocationTypeGroupLink OF LocationTypeGroup NO-LOCK*/           
            
         END. /*IF StockEntity.IsPallet = YES*/
         
         IF StockEntity.IsPallet = NO THEN
         DO:
            /* New Section to deal with Location matches that are set up before a Part arrives for the first time */
            FIND FIRST PartProfile OF Part NO-LOCK
               WHERE PartProfile.Completed = "" NO-ERROR.
            IF AVAILABLE PartProfile THEN
            DO:
               FOR EACH PartProfileOrderStreamLink OF PartProfile NO-LOCK:
                  
                  FOR EACH PartProfileOrderStreamRplLoc OF PartProfileOrderStreamLink NO-LOCK.
                     
                     /* Validate Location is OK for StockPackage */
                     chrValidLocation = fValidateCanRelocate(INPUT StockPackage.StockPackageID,
                                                             INPUT PartProfileOrderStreamRplLoc.LocationID).
                     
                     /* If we have a Valid Location then set the Location & Result and Leave */
                     IF chrValidLocation = "OK" THEN
                     DO:
                        intSsnSuggestedLocationID:setValue(PartProfileOrderStreamRplLoc.LocationID).
                        chrResult = "Continue".        
                        LEAVE Main_Block.
                     END.
                     
                  END. /*FOR EACH PartProfileOrderStreamRplLoc OF PartProfileOrderStreamLink NO-LOCK.*/
               END. /*FOR EACH PartProfileOrderStreamLink OF PartProfile NO-LOCK:*/
            END. /*IF AVAILABLE PartProfile THEN*/
            
            LocationTypeLoop:
            FOR EACH LocationTypeGroupLink OF pickLocationTypeGroup NO-LOCK,
               EACH LocationType OF LocationTypeGroupLink NO-LOCK
               BY LocationType.PutawaySequence:
               
               IF LocationType.PutawaySequence <= 0 THEN
                  NEXT LocationTypeLoop.
               
               LocationLoop:
               FOR EACH Location OF LocationType NO-LOCK
                  WHERE Location.Active = TRUE:
                  
                  /* Validate Location is OK for StockPackage */
                  chrValidLocation = fValidateCanRelocate(INPUT StockPackage.StockPackageID,
                                                          INPUT Location.LocationID).
                  
                  /* If we have a Valid Location then set the Location & Result and Leave */
                  IF chrValidLocation = "OK" THEN
                  DO:
                     intSsnSuggestedLocationID:setValue(Location.LocationID).
                     chrResult = "Continue".        
                     LEAVE Main_Block.
                  END.
                  
               END. /*FOR EACH Location OF LocationType NO-LOCK.*/
            END. /*FOR EACH LocationTypeGroupLink OF LocationTypeGroup NO-LOCK*/
         END. /*StockEntity.IsPallet = NO*/
         
      END. /*IF intSsnSuggestedLocationID:intValue = 0 THEN*/
      
      /* No errors */
      chrResult = "Continue".
      
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnPackageID                  NO-ERROR.
   DELETE OBJECT intSsnSuggestedLocationID        NO-ERROR.
   DELETE OBJECT chrSsnSuggestedLocationTypeRef   NO-ERROR.
   
   /* Releases */
   RELEASE Part                                   NO-ERROR.
   RELEASE Location                               NO-ERROR.
   RELEASE LocationType                           NO-ERROR.
   RELEASE StockPackage                           NO-ERROR.
   RELEASE oldestPackage                          NO-ERROR.
   RELEASE otherStockPackage                      NO-ERROR.
   RELEASE otherLocationType                      NO-ERROR.
   RELEASE otherLocation                          NO-ERROR.
   RELEASE LocationTypeGroup                      NO-ERROR.
   RELEASE PartStockEntityLink                    NO-ERROR.
   RELEASE LocationTypeGroupLink                  NO-ERROR.
   RELEASE RelocateStockEntityRule                NO-ERROR.
   RELEASE RelocatePartStockEntityRule            NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */


