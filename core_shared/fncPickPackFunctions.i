/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncPickPackFunctions.i
Purpose : Functions used during (fast) Pick and Pack process.
Author  : Todd Wierzchowski
Date    : 6th May 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
06/05/2015 TAW Canon      Created
------------------------------------------------------------------------------------------------------------------------------------------*/

/* DEPENDENCIES */
/*{defSessionVariables.i}   */
/*{fncClassFunctions.i}     */
/*{fncLoggingFunctions.i}   */
/*{fncStatusTypeFunctions.i}*/
/*{fncGlobalFunctions.i}    */


/* Check that the links between Picked stock and the ShipOrderLines that it was packed out to matches exactly */
FUNCTION fCheckStockShipOrderLineLinks RETURNS CHARACTER(INPUT intShipOrderID AS INTEGER):

   DEFINE BUFFER readShipOrder FOR ShipOrder.
   DEFINE BUFFER readShipOrderLine FOR ShipOrderLine.
   DEFINE BUFFER readPart FOR Part.
   DEFINE BUFFER readStockShipOrderLineLink FOR StockShipOrderLineLink.
   
   DEFINE VARIABLE intTotalLinked AS INTEGER NO-UNDO.
   
   FIND FIRST readShipOrder WHERE readShipOrder.ShipOrderID = intShipOrderID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readShipOrder THEN
   DO:
      RETURN "Error: No ShipOrder exists for ShipOrderID: " + STRING(intShipOrderID).
   END.
   
   PickedStockLoop:
   FOR EACH readShipOrderLine OF readShipOrder NO-LOCK:
   
      /* Skip the non inventory parts from the order like delivery charge */
      IF CAN-FIND (FIRST readPart OF readShipOrderLine WHERE readPart.InventoryPart = NO ) THEN NEXT PickedStockLoop.
      
      intTotalLinked = 0.
      FOR EACH readStockShipOrderLineLink OF readShipOrderLine NO-LOCK:
         
         intTotalLinked = intTotalLinked + readStockShipOrderLineLink.LinkQty.
      END.
      
      IF intTotalLinked <> readShipOrderLine.QtyPacked THEN
      DO:
         RETURN "Error: There is at least one ShipOrderLine for this ShipOrder with a Stock Link mismatch. Cannot Complete.".
      END.
   END. /*FOR EACH ShipOrderLine OF ShipOrder NO-LOCK:*/
   
   RETURN "Ok".
      
END FUNCTION. /* fCheckStockShipOrderLineLinks */

/* Check that the number of Serials attached to the ShipOrderLine matches the QtyPacked */
FUNCTION fCheckSerialsAttached RETURNS CHARACTER(INPUT intShipOrderID AS INTEGER):
   
   DEFINE BUFFER readShipOrder FOR ShipOrder.
   DEFINE BUFFER readShipOrderLine FOR ShipOrderLine.
   DEFINE BUFFER readSerialScan FOR SerialScan.
   DEFINE BUFFER readPart FOR Part.
   
   DEFINE VARIABLE intSerialCounter AS INTEGER NO-UNDO.
   
   FIND FIRST readShipOrder WHERE readShipOrder.ShipOrderID = intShipOrderID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readShipOrder THEN
   DO:
      RETURN "Error: No ShipOrder exists for ShipOrderID: " + STRING(intShipOrderID).
   END.
   
   SerialsAttachedLoop:
   FOR EACH readShipOrderLine OF readShipOrder NO-LOCK:
         
         /* Skip the non inventory parts from the order like delivery charge */
         IF CAN-FIND (FIRST readPart OF readShipOrderLine WHERE readPart.InventoryPart = NO ) THEN NEXT SerialsAttachedLoop.
         
         FOR EACH readSerialScan OF readShipOrderLine NO-LOCK /* idx=ShipOrderLineIDSerialisedUnitID */
            BREAK BY readSerialScan.SerialisedUnitID ON ERROR UNDO SerialsAttachedLoop, RETURN "Error: Error checking for attached Serials, cannot continue!":
            
            IF LAST-OF(readSerialScan.SerialisedUnitID) THEN
               intSerialCounter = intSerialCounter + 1.
         END.
         
         IF intSerialCounter <> 0 AND intSerialCounter <> readShipOrderLine.QtyPacked THEN
         DO:
            RETURN "Error: Wrong number OF Serials FOR ShipOrderLine:" + STRING(readShipOrderLine.ShipOrderLineID) + "." 
                   + " There should be:" + STRING(readShipOrderLine.QtyPacked) + " but there are actually:" 
                   + STRING(intSerialCounter) + ". Cannot Complete.".
         END.
      END. /*FOR EACH ShipOrderLine OF ShipOrder NO-LOCK:*/
   
   RETURN "Ok".
   
END FUNCTION. /* fCheckSerialsAttached */

/* Check that the appropriate number of ShipPackages are attached to the ShipOrder */
FUNCTION fCheckNumOfShipPackages RETURNS CHARACTER(INPUT intShipOrderID AS INTEGER):
   
   DEFINE BUFFER readShipOrder FOR ShipOrder.
   DEFINE BUFFER readShipPackage FOR ShipPackage.
   
   DEFINE VARIABLE intNumPackages AS INTEGER NO-UNDO.
   
   FIND FIRST readShipOrder WHERE readShipOrder.ShipOrderID = intShipOrderID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readShipOrder THEN
   DO:
      RETURN "Error: No ShipOrder exists for ShipOrderID: " + STRING(intShipOrderID).
   END.
   
   FOR EACH readShipPackage OF readShipOrder NO-LOCK:
      intNumPackages = intNumPackages + 1.
   END.
   
   IF ShipOrder.NumPackages <> intNumPackages THEN
   DO:
      /* Need to tidy this up for wrong number of Packages */
      RETURN "Error: Wrong number of Packages for ShipOrder:" + STRING(readShipOrder.NumPackages) + "." 
             + " There are:" + STRING(intNumPackages) + " Packages actually PackedOut." 
             + "Please UPDATE the number of Packages on the Order and then REPRINT ALL the Labels. Cannot Complete.".
   END. /*IF ShipOrder.NumPackages <> intNumPackages THEN*/
   
   RETURN "Ok".
   
END FUNCTION. /* fCheckNumOfShipPackages */

/* All StockPackages from all Totes should have been cleared during Packout */
FUNCTION fClearStockPackagesFromTotes RETURN CHARACTER (INPUT intTaskID AS INTEGER):

   DEFINE BUFFER readTask                       FOR Task.
   DEFINE BUFFER readTaskLine                   FOR TaskLine.
   DEFINE BUFFER readTaskLineWork               FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkToteLink       FOR TaskLineWorkToteLink.
   DEFINE BUFFER readOtherTaskLineWorkToteLink  FOR TaskLineWorkToteLink.
   DEFINE BUFFER readTote                       FOR Tote.
   DEFINE BUFFER readStockPackage               FOR StockPackage.
   DEFINE BUFFER readToteType                   FOR ToteType.
   DEFINE BUFFER readToteParent                 FOR ToteParent.
   DEFINE BUFFER readParentToteType             FOR ToteType.
   DEFINE BUFFER readChildTote                  FOR Tote.
   DEFINE BUFFER readToteStatus                 FOR ToteStatus.
   
   DEFINE VARIABLE updReadTote                  AS updRecord.
   DEFINE VARIABLE updReadTaskLineWorkToteLink  AS updRecord.
   DEFINE VARIABLE updReadToteParent            AS updRecord.
   DEFINE VARIABLE updReadChildTote             AS updRecord.
   
   DEFINE VARIABLE chrReadError                 AS CHARACTER.

   FIND FIRST readTask WHERE readTask.TaskID = intTaskID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTask THEN DO:
      RETURN "Error: No Task exists for TaskID: " + STRING(intTaskID).
   END.

   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkToteLink OF readTaskLineWork NO-LOCK,
            EACH readTote OF readTaskLineWorkToteLink NO-LOCK
            BREAK BY readTote.ToteID:
            
            /* Only want to look at each Tote once */
            IF FIRST-OF(readTote.ToteID) THEN
            DO ON ERROR UNDO ToteLoop, LEAVE ToteLoop:
               
               /* No other TaskLineWorkToteLink records to this tote other than the ones we're completeing so release the tote */
               IF NOT CAN-FIND(FIRST readStockPackage OF readTote NO-LOCK) THEN
               ReleaseToteBlk:
               DO ON ERROR UNDO ToteLoop, LEAVE ToteLoop:
                  
                  updReadTote = fGetRecord("Tote",readTote.ToteID).
                  
                  fLog("Updating ToteID:" + STRING(readTote.ToteID)).
                  
                  FIND FIRST readToteType OF readTote NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE readToteType THEN
                  DO:
                     chrReadError = chrReadError + "Tote:" + readTote.ToteRef + " does NOT have a valid ToteType. Cannot Complete.".
                     UNDO ToteLoop, LEAVE ToteLoop.
                  END.
                  
                  /* ReUsable Totes such as Crates or Trollys should be set back to Available and have the User wiped */
                  IF AVAILABLE readToteType AND readToteType.ReUsable THEN
                  DO ON ERROR UNDO ToteLoop, LEAVE ToteLoop:
                     
                     updReadTote:assignField("ToteStatusID", fGetStatusID("Tote","Available")).
                     updReadTote:assignField("GateUserID",   0).
                  END.
                  /* Disposable Totes like PickerPallet or FullPallet (virtual Tote created to hold full Pallet) should be Completed */
                  ELSE
                  DO ON ERROR UNDO ToteLoop, LEAVE ToteLoop:
                     
                     updReadTote:assignField("ToteStatusID", fGetStatusID("Tote","Complete")).
                  END.
                  
                  chrReadError = chrReadError + updReadTote:getErrors().
                  DELETE OBJECT updReadTote NO-ERROR.
                  IF chrReadError <> "" THEN
                     UNDO ToteLoop, LEAVE ToteLoop.
                  
                  fLog("Updating TaskLineWorkToteLinks for ToteID:" + STRING(readTote.ToteID)).
                  
                  /* Clean up the Links to the Tote */
                  FOR EACH readOtherTaskLineWorkToteLink OF Tote NO-LOCK
                     WHERE readOtherTaskLineWorkToteLink.Completed = "" ON ERROR UNDO ToteLoop, LEAVE ToteLoop:
                     
                     /* Complete all Links to this Tote as its empty */
                     updReadTaskLineWorkToteLink = fGetRecord("TaskLineWorkToteLink", readOtherTaskLineWorkToteLink.TaskLineWorkToteLinkID).
                     updReadTaskLineWorkToteLink:assignField("Completed", fTimestamp(NOW)).
                     
                     chrReadError = chrReadError + updReadTaskLineWorkToteLink:getErrors().
                     DELETE OBJECT updReadTaskLineWorkToteLink NO-ERROR.
                     IF chrReadError <> "" THEN
                        UNDO ToteLoop, LEAVE ToteLoop.
                  END.
                  
                  fLog("Updating Parent Tote with ToteID:" + STRING(readTote.ToteParentID)).
                  
                  /* Check for Parent Tote and if found release it also */
                  IF readTote.ToteParentID <> 0 THEN
                  ToteParentBlk:
                  DO ON ERROR UNDO ToteLoop, LEAVE ToteLoop:
                     
                     /* Validate Tote Parent Exists */
                     FIND FIRST readToteParent OF readTote NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE readToteParent THEN
                     DO:
                        chrReadError = chrReadError + "Tote:" + readTote.ToteRef + " does NOT have a valid Parent Tote. Cannot Complete.".
                        UNDO ToteLoop, LEAVE ToteLoop.
                     END.
                     
                     FIND FIRST readParentToteType OF readToteParent NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE readParentToteType THEN
                     DO:
                        chrReadError = chrReadError + "Tote Parent:" + readToteParent.ToteParentRef 
                                            + " does NOT have a valid ToteType. Cannot Complete.".
                        UNDO ToteLoop, LEAVE ToteLoop.
                     END.
                     
                     FIND FIRST readToteStatus NO-LOCK
                        WHERE readToteStatus.StatusCode = "PostPick" NO-ERROR.
                     IF NOT AVAILABLE readToteStatus THEN
                     DO:
                        chrReadError = chrReadError + "ToteStatus:PostPick does NOT exist. Cannot Complete.".
                        UNDO ToteLoop, LEAVE ToteLoop.
                     END.
                     
                     /* If there are no other child Totes for the Parent that haven't been released then we can release the Parent */
                     OtherToteChildrenLoop:
                     FOR EACH readChildTote OF readToteParent NO-LOCK
                        WHERE readChildTote.ToteID <> readTote.ToteID,
                           EACH  readOtherTaskLineWorkToteLink OF readChildTote NO-LOCK
                           WHERE readOtherTaskLineWorkToteLink.Completed = "":
                           
                           LEAVE ToteParentBlk.
                     END. /*FOR EACH childTote OF ToteParent NO-LOCK*/
                     
                     /* If we get to here we need to close the Parent and all unused Child Totes */
                     updReadToteParent = fGetRecord("ToteParent", readToteParent.ToteParentID).
                     updReadToteParent:assignField("GateUserID",   0).
                     updReadToteParent:assignField("ToteStatusID", fGetStatusID("Tote", "Available")).
                     
                     chrReadError = chrReadError + updReadToteParent:getErrors().
                     DELETE OBJECT updReadToteParent NO-ERROR.
                     IF chrReadError <> "" THEN
                        UNDO ToteLoop, LEAVE ToteLoop.
                     
                     /* Clean up status of any other child Totes of the Parent that weren't used during Picking */
                     OtherToteChildrenLoop:
                     FOR EACH readChildTote OF readToteParent NO-LOCK
                        WHERE readChildTote.ToteID       <> readTote.ToteID
                        AND   readChildTote.ToteStatusID =  readToteStatus.ToteStatusID ON ERROR UNDO ToteLoop, LEAVE ToteLoop:
                        
                        fLog("Updating Child ToteID:" + STRING(readChildTote.ToteID)).
                        
                        updReadChildTote = fGetRecord("Tote", readChildTote.ToteID).
                        updReadChildTote:assignField("ToteStatusID", fGetStatusID("Tote", "Available")).
                        updReadChildTote:assignField("GateUserID",   0).
                        
                        chrReadError = chrReadError + updReadChildTote:getErrors().
                        DELETE OBJECT updReadChildTote NO-ERROR.
                        
                        /* Message Error */
                        IF chrReadError <> "" THEN
                           UNDO ToteLoop, LEAVE ToteLoop.
                        
                     END. /*FOR EACH childTote OF ToteParent NO-LOCK*/
                     
                  END. /*IF Tote.ToteParentID <> 0 THEN ToteParentBlk:*/
                  
               END. /*IF NOT CAN-FIND(FIRST StockPackage OF Tote) THEN ReleaseToteBlk:*/
               
            END. /*IF FIRST-OF(Tote.ToteID) THEN DO:*/
            
   END. /*FOR EACH TaskLineWorkToteLink OF Task NO-LOCK,*/
   
   IF chrReadError <> "" THEN 
      RETURN "Error:" + chrReadError.
   
   RETURN "Ok".
   
END FUNCTION. /* fClearStockPackagesFromTotes */