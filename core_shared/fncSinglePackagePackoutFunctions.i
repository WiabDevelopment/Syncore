/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncSinglePackagePackout.i
Purpose : All functions to do with SinglePackagePackout
Author  : SH
Date    : 
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttOrderTote NO-UNDO
   FIELD ShipOrderID       AS INTEGER 
   FIELD OrderRef          AS CHARACTER
   FIELD Priority          AS INTEGER
   FIELD Created           AS CHARACTER
   FIELD ShipOrderStatusID AS INTEGER 
   INDEX ShipOrderID IS UNIQUE PRIMARY ShipOrderID
   INDEX ShipOrderStatusID ShipOrderStatusID Priority DESC Created.

FUNCTION fGetStreamID RETURNS INTEGER (INPUT chrStreamCode AS CHARACTER):
   
   DEFINE BUFFER readShipOrderStream FOR ShipOrderStream.
   
   FIND FIRST readShipOrderStream NO-LOCK
      WHERE readShipOrderStream.StreamCode = chrStreamCode NO-ERROR.
   IF AVAILABLE readShipOrderStream THEN
      RETURN readShipOrderStream.ShipOrderStreamID.
   ELSE
      RETURN 0.
      
END FUNCTION. /*fGetStreamID*/

FUNCTION fGetStreamCode RETURNS CHARACTER (INPUT intStreamID AS INTEGER):
   
   DEFINE BUFFER readShipOrderStream FOR ShipOrderStream.
   
   FIND FIRST readShipOrderStream NO-LOCK
      WHERE readShipOrderStream.ShipOrderStreamID = intStreamID NO-ERROR.
   IF AVAILABLE readShipOrderStream THEN
      RETURN readShipOrderStream.StreamCode.
   ELSE
      RETURN "".
      
END FUNCTION. /*fGetStreamID*/

/* Check that the number of Serials attached to the ShipOrderLine matches the QtyPacked */
FUNCTION fCheckSerialsAttachedSPP RETURNS CHARACTER(INPUT intShipOrderID AS INTEGER):
   
   DEFINE BUFFER readShipOrder     FOR ShipOrder.
   DEFINE BUFFER readShipOrderLine FOR ShipOrderLine.
   DEFINE BUFFER readSerialScan    FOR SerialScan.
   DEFINE BUFFER readPart          FOR Part.
   
   DEFINE VARIABLE intSerialCounter AS INTEGER NO-UNDO.
   
   FIND FIRST readShipOrder WHERE readShipOrder.ShipOrderID = intShipOrderID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readShipOrder THEN
   DO:
      RETURN "Error: No ShipOrder exists for ShipOrderID: " + STRING(intShipOrderID).
   END.
   
   SerialsAttachedLoop:
   FOR EACH readShipOrderLine OF readShipOrder NO-LOCK:
         
         /* Skip the non inventory parts from the order like delivery charge */
         IF CAN-FIND (FIRST readPart OF readShipOrderLine WHERE readPart.InventoryPart = NO OR readPart.Serialised = NO) THEN 
            NEXT SerialsAttachedLoop.
         
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
         
         /*Reset Counter for Next Line*/
         intSerialCounter = 0.
         
      END. /*FOR EACH ShipOrderLine OF ShipOrder NO-LOCK:*/
   
   RETURN "Ok".
   
END FUNCTION. /* fCheckSerialsAttached */

FUNCTION fCheckOrderConfiguration RETURNS CHARACTER (INPUT intShipOrderID AS INTEGER,
                                                     INPUT intToteID AS INTEGER):
      /*BUFFERS*/
      DEFINE BUFFER readShipOrder              FOR ShipOrder.
      DEFINE BUFFER readShipPackageCalculation FOR ShipPackageCalculation.
      DEFINE BUFFER readLocation               FOR Location.
      DEFINE BUFFER readTote                   FOR Tote.
      DEFINE BUFFER readToteLine               FOR ToteLine.
      
      /*VARIABLES*/
      DEFINE VARIABLE intShipPackageCount      AS INTEGER NO-UNDO.
      
      FIND FIRST readShipOrder NO-LOCK /*idx=ShipOrderID*/
         WHERE readShipOrder.ShipOrderID = intShipOrderID NO-ERROR.
      IF NOT AVAILABLE readShipOrder THEN
         RETURN "Error: ShipOrderID: " + STRING(intShipOrderID) + " was not found. Cannot Continue!". 
      
      IF readShipOrder.NumPackages <> 1 THEN
         RETURN "Error: NumPackages on the ShipOrder not equal to 1. Cannot Continue!".
       
      FOR EACH readShipPackageCalculation NO-LOCK /*idx=ShipOrderIDSupercededCreated*/
         WHERE readShipPackageCalculation.ShipOrderID = readShipOrder.ShipOrderID
         AND readShipPackageCalculation.Superceded = "":
         
         intShipPackageCount = intShipPackageCount + 1.
      END. /*FOR EACH readShipPackageCalculation NO-LOCK*/
      
      IF intShipPackageCount <> 1 THEN
         RETURN "Error: Number of ShipPackages for the order is not equal to 1. Cannot Continue!".
      
      FIND FIRST readTote NO-LOCK /*idx=ToteID*/
         WHERE readTote.ToteID = intToteID NO-ERROR.
      IF NOT AVAILABLE readTote THEN
      DO:
         RETURN "Error: Could not find Tote for ID: [" + STRING(intToteID) + "]. Cannot Continue!".
      END. /*IF NOT AVAILABLE readTote THEN*/
      
      FIND FIRST readLocation NO-LOCK
         WHERE readLocation.LocationID = readTote.LocationID NO-ERROR.
      IF NOT AVAILABLE readLocation THEN
      DO:
         RETURN "Error: Could not find Location for ID: [" + STRING(readTote.LocationID) + "]. Cannot Continue!".
      END. /*IF NOT AVAILABLE readLocation THEN*/
      
      IF readLocation.LocationTypeID <> fGetTypeID("Location", "PostPick") THEN
      DO:
         RETURN "Error: Tote was not found in a PostPick Location. Cannot Continue!".
      END. /*IF readLocation.LocationTypeID <> fGetTypeID("Location", "PostPick") THEN*/
      
      FIND FIRST readToteLine NO-LOCK
         WHERE readToteLine.PostPickLocationID = readLocation.LocationID NO-ERROR.
      IF NOT AVAILABLE readToteLine THEN
      DO:
         RETURN "Error: Tote was not found in a PostPick Location attached to the ToteLine. Cannot Continue!".         
      END. /*IF NOT AVAILABLE readToteLine THEN*/
         
      RETURN "Ok".

END.

FUNCTION fCheckOrderStatus RETURNS CHARACTER (INPUT intShipOrderID AS INTEGER,
                                              INPUT chrStatusCode AS CHARACTER):

   /*BUFFERS*/
   DEFINE BUFFER readShipOrder       FOR ShipOrder.
   DEFINE BUFFER readShipOrderStatus FOR ShipOrderStatus.
   
   FIND FIRST readShipOrder NO-LOCK /*idx=ShipOrderID*/
      WHERE readShipOrder.ShipOrderID = intShipOrderID NO-ERROR.
   IF NOT AVAILABLE readShipOrder THEN
      RETURN "Error: ShipOrderID: " + STRING(intShipOrderID) + " was not found. Cannot Continue!". 
      
   FIND FIRST readShipOrderStatus NO-LOCK
      WHERE readShipOrderStatus.StatusCode = chrStatusCode NO-ERROR.
   IF NOT AVAILABLE readShipOrderStatus THEN
      RETURN "Error: Could not find status for StatusCode: " + chrStatusCode + ". Cannot Continue!".
      
   IF readShipOrder.ShipOrderStatusID <> readShipOrderStatus.ShipOrderStatusID THEN
      RETURN "Error: OrderRef: " + readShipOrder.OrderRef + " was not in " + chrStatusCode + " Status. Cannot Continue!". 
   
   RETURN "Ok".
   
END.                           
               
FUNCTION fGetOrderInTote RETURNS CHARACTER(INPUT intSelectedToteID           AS INTEGER,
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

   EMPTY TEMP-TABLE ttOrderTote.

   readTaskLineWorkToteLinkLoop:
   FOR EACH readTaskLineWorkToteLink OF readTote NO-LOCK,
      EACH readTaskLineWork OF readTaskLineWorkToteLink NO-LOCK,
               EACH readShipOrder OF readTaskLineWork NO-LOCK:

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

      FIND FIRST ttOrderTote NO-LOCK
         WHERE ttOrderTote.ShipOrderID = readShipOrder.ShipOrderID NO-ERROR.

      IF NOT AVAILABLE ttOrderTote THEN
      DO:
         CREATE ttOrderTote.
         ASSIGN ttOrderTote.ShipOrderID       = readShipOrder.ShipOrderID
                ttOrderTote.OrderRef          = readShipOrder.OrderRef
                ttOrderTote.Priority          = readShipOrder.Priority
                ttOrderTote.Created           = readShipOrder.Created
                ttOrderTote.ShipOrderStatusID = readShipOrder.ShipOrderStatusID.
      END.    
   END.

   RETURN "Ok".

END FUNCTION. /* fGetOrderInTote */       

/* All StockPackages from all Totes should have been cleared during Packout */
FUNCTION fClearStockPackagesFromToteSPP RETURN CHARACTER (INPUT intShipOrderID AS INTEGER):

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
   DEFINE BUFFER readShipOrder                  FOR ShipOrder.
   DEFINE BUFFER readChildToteType              FOR ToteType.
   
   DEFINE VARIABLE updReadTote                  AS updRecord.
   DEFINE VARIABLE updReadTaskLineWorkToteLink  AS updRecord.
   DEFINE VARIABLE updReadToteParent            AS updRecord.
   DEFINE VARIABLE updReadChildTote             AS updRecord.
   
   DEFINE VARIABLE chrReadError                 AS CHARACTER.

   FIND FIRST readShipOrder NO-LOCK
      WHERE readShipOrder.ShipOrderID = intShipOrderID NO-ERROR.
   IF NOT AVAILABLE readShipOrder THEN 
   DO:
      RETURN "Error: No ShipOrder exists for ShipOrderID: [" + STRING(intShipOrderID) + "]".
   END.

   /*Only want to Compelte the TaskLineWorkToteLinks for the Order 
    Being PackedOut so added ShipOrderID into this Query*/
   ToteLoop:
   FOR EACH readTaskLineWork NO-LOCK
      WHERE readTaskLineWork.ShipOrderID = intShipOrderID,
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
                  updReadTote:assignField("GateUserID", 0).
                  updReadTote:assignField("LocationID", 0).
                  
                  /*If it is a Removeable Tote then
                    We want to Reset the Location,Parent and TaskType
                    as Removeable Tote's and Detachable Trolley Tote's
                    are Used in Trolley/ Trolley&ToteLine Streamed Orders*/
                  IF readToteType.TypeCode = "RemoveableTote" THEN
                  DO:
                     updReadTote:assignField("ToteParentID", 0).
                     updReadTote:assignField("TaskTypeID", 0).
                  END. /*IF readToteType.TypeCode = "RemoveableTote" THEN*/
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
               FOR EACH readOtherTaskLineWorkToteLink OF readTote NO-LOCK
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
                  updReadToteParent:assignField("GateUserID",  0).
                  updReadToteParent:assignField("LocationID", 0).
                  
                  /*If the Parent Tote is a Detacheable Trolley
                    Set Status to BeingBuilt OtherWise do the Default */
                  IF readParentToteType.TypeCode = "DetachableTrolley" THEN
                  DO:
                     updReadToteParent:assignField("ToteStatusID", fGetStatusID("Tote", "BeingBuilt")).
                  END. /*IF parentToteType.TypeCode = "DetachableTrolley" THEN*/
                  ELSE
                  DO:
                     updReadToteParent:assignField("ToteStatusID", fGetStatusID("Tote", "Available")).
                  END. /*ELSE*/
                        
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
                     updReadChildTote:assignField("LocationID", 0).
                           
                     FIND FIRST readChildToteType NO-LOCK /*idx=ToteTypeID*/
                        WHERE readChildToteType.ToteTypeID = readChildTote.ToteTypeID NO-ERROR.
                     IF AVAILABLE readChildToteType AND readChildToteType.TypeCode = "RemoveableTote" THEN
                     DO:
                        updReadChildTote:assignField("ToteParentID", 0).
                        updReadChildTote:assignField("TaskTypeID", 0).
                     END. /*IF ToteType.TypeCode = "RemoveableTote" THEN*/
                     
                     chrReadError = chrReadError + updReadChildTote:getErrors().
                     DELETE OBJECT updReadChildTote NO-ERROR.
                     
                     /* Message Error */
                     IF chrReadError <> "" THEN
                        UNDO ToteLoop, LEAVE ToteLoop.
                     
                  END. /*FOR EACH childTote OF ToteParent NO-LOCK*/
                  
               END. /*IF Tote.ToteParentID <> 0 THEN ToteParentBlk:*/
               
            END. /*IF NOT CAN-FIND(FIRST StockPackage OF Tote) THEN ReleaseToteBlk:*/
            
         END. /*IF FIRST-OF(Tote.ToteID) THEN DO:*/
            
   END. /* FOR EACH readTaskLineWork OF readTaskLine NO-LOCK*/
   
   IF chrReadError <> "" THEN 
      RETURN "Error:" + chrReadError.
   
   RETURN "Ok".
   
END FUNCTION. /* fClearStockPackagesFromTotes */