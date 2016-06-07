/*------------------------------------------------------------------------------------------------------------------------------------------
Program : lodScanShipPallet.p
Purpose : Scan a Ship Pallet to Load
 
          Possible results : Continue/CloseTrailer

Author  : DCummins
Date    : 27/03/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
01/10/2013 MN  CR1012     Adding Extra Validation and Message if Not All Packaged of the Order in Shiplane
23/04/2014 BR  CR 1052    UI Standardization
16/07/2015 CS  GoPro      Added Pallet lookup for scanned child ShipPackage.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character SessionValue Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}

   /* Optional Includes */
   {fncStatusTypeFunctions.i}
   {fncDateFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i} 
   
   /* Local Variables */     
   DEFINE VARIABLE chrScanShipPalletID           AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   DEFINE VARIABLE intLoadValue                  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE intDangerousGoodsLoadValue    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE intReadyToLoadShipOrderStatus AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intReadyToLoadShipStatus      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intBeingLoadedShipOrderStatus AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intLoadedShipStatus           AS INTEGER   NO-UNDO.
   DEFINE VARIABLE chrOtherPalletsOfOrder        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrCheckMessage               AS CHARACTER NO-UNDO.

   /* Local Objects */   
   DEFINE VARIABLE intSsnShipPalletID AS sessionValue NO-UNDO.   
   DEFINE VARIABLE intSsnShipLaneID   AS sessionValue NO-UNDO.   
   DEFINE VARIABLE intSsnOutboundID   AS sessionValue NO-UNDO.   
   DEFINE VARIABLE chrSsnLastScan     AS sessionValue NO-UNDO.   
      
   /* Temp Tables */
   DEFINE TEMP-TABLE ttOrderValue
      FIELD OrderNo    AS INTEGER
      FIELD OrderValue AS DECIMAL.

   DEFINE TEMP-TABLE ttPackageList NO-UNDO
      FIELD Package AS CHARACTER
      INDEX idx Package.

   /* Buffers */
   DEFINE BUFFER orderPackage      FOR ShipPackage.
   DEFINE BUFFER loadPallet        FOR ShipPallet.
   DEFINE BUFFER otherShipPackage  FOR ShipPackage.
   DEFINE BUFFER otherShipPallet   FOR ShipPallet.
   DEFINE BUFFER loadedShipPackage FOR ShipPackage.
   DEFINE BUFFER loadedShipOrder   FOR ShipOrder.


   /* Reset Data */   
   fClearSessionValue("ShipPalletID").
   fClearSessionValue("LastScan").
   
   /* Set New Data */   
   chrSsnLastScan     = fNewSessionValue("LastScan").
   intSsnShipPalletID = fNewSessionValue("ShipPalletID").   
      
   /* Get Current Data */
   intSsnOutboundID = fGetSessionValue("OutboundID").
   intSsnShipLaneID = fGetSessionValue("ShipLaneID").

   intReadyToLoadShipOrderStatus = fGetStatusID("ShipOrder", "ReadyToLoad").
   intBeingLoadedShipOrderStatus = fGetStatusID("ShipOrder", "BeingLoaded").
   intReadyToLoadShipStatus      = fGetStatusID("Ship", "ReadyToLoad").
   intLoadedShipStatus           = fGetStatusID("Ship", "Loaded").

   /* Frames and UI */
   DEFINE FRAME ScanShipPalletFrame                 
       SKIP(4)    
       "    Scan Ship Pallet ID"       
       chrScanShipPalletID NO-LABEL COLON 1
       SKIP(6)
   WITH SIDE-LABELS TITLE " Scan Ship Pallet " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   /* Events */
   ON 'PF4':U ANYWHERE
   DO:
      chrScanShipPalletID:SCREEN-VALUE IN FRAME ScanShipPalletFrame = "COMPLETE".
      RETURN NO-APPLY.
   END.      

   Main_Block:
   DO ON ERROR UNDO:

      /* Get the Ship Status Group */
      FIND FIRST ShipOrderStatusGroup NO-LOCK
         WHERE ShipOrderStatusGroup.GroupName = "Loading" NO-ERROR.
   
      IF NOT AVAILABLE ShipOrderStatusGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[ShipOrderStatusGroup] [Loading] does not exist").
         LEAVE Main_Block.
      END. /* Not Available Ship Status Group */

      Scan_Block:
      REPEAT ON STOP UNDO, RETRY:       
            
         ASSIGN chrScanShipPalletID        = ""
                intLoadValue               = 0
                intDangerousGoodsLoadValue = 0.
                
         EMPTY TEMP-TABLE ttOrderValue.

         UPDATE chrScanShipPalletID WITH FRAME ScanShipPalletFrame.         
         chrSsnLastScan:setValue(chrScanShipPalletID).

         IF chrScanShipPalletID = "CloseTrailer" THEN
         DO:

            /* Check Orders are Fully Loaded */
            FOR EACH loadPallet NO-LOCK
               WHERE loadPallet.OutboundID = intSsnOutboundID:intValue:
               FOR EACH ShipPackage OF loadPallet NO-LOCK:
                  FOR EACH ShipOrder OF ShipPackage NO-LOCK:
                     FOR EACH orderPackage OF ShipOrder NO-LOCK:

                        IF orderPackage.OutboundID = 0 THEN
                        DO:
                           RUN DisplayMessage("Ship Load Error",
                                              "[Ship Order] [" + ShipOrder.OrderRef + "] has NOT been fully Loaded to this Trailer.  Please Complete Load.").
                           NEXT Scan_Block.
                        END.

                        IF orderPackage.OutboundID <> intSsnOutboundID:intValue THEN DO:
                           RUN DisplayMessage("Ship Load Error",
                                              "[Ship Order] [" + ShipOrder.OrderRef + "] has been Loaded on another Trailer.  Cannot Close.").
                           NEXT Scan_Block.
                        END.

                     END.
                  END.
               END.
            END.

            chrResult = "CloseTrailer".
            LEAVE Main_Block.
         END.

         IF chrScanShipPalletID = "COMPLETE" THEN
         DO:
            chrResult = "Continue".
            LEAVE Main_Block.
         END.
         
         /* Find the ShipPallet Ref if a child ShipPackage was scanned */
         FIND FIRST ShipPackage NO-LOCK
            WHERE ShipPackage.PackageRef = chrScanShipPalletID NO-ERROR.
         IF AVAILABLE ShipPackage AND 
            ShipPackage.ShipPalletID > 0 THEN
         DO:
            FIND FIRST ShipPallet NO-LOCK
               WHERE ShipPallet.ShipPalletID = ShipPackage.ShipPalletID NO-ERROR.
            IF AVAILABLE ShipPallet THEN
               chrScanShipPalletID = ShipPallet.PalletRef.
         END. /*IF AVAILABLE ShipPackage*/
         
         /* Verify the Pallet Exists */
         FIND FIRST ShipPallet NO-LOCK
            WHERE ShipPallet.PalletRef = chrScanShipPalletID NO-ERROR.
         IF NOT AVAILABLE ShipPallet THEN
         DO:
            RUN DisplayMessage("Ship Pallet Error",
                               "[Ship Pallet] [" + chrScanShipPalletID + "] does not exist.  Scan another.").
            NEXT Scan_Block.
         END.

         /* Verify Pallet is not yet loaded */
         IF ShipPallet.ShipStatusID = intLoadedShipStatus THEN
         DO:
            RUN DisplayMessage("Ship Pallet Error",
                               "[Ship Pallet] [" + chrScanShipPalletID + "] is already Loaded.  Scan another.").
            NEXT Scan_Block.
         END. 
  
         /* Verify Pallet is at Correct Status */
         IF ShipPallet.ShipStatusID <> fGetStatusID("Ship", "ReadyToLoad") THEN
         DO:
            RUN DisplayMessage("Ship Pallet Error",
                               "[Ship Pallet] [" + chrScanShipPalletID + "] is NOT at Ready to Load Status.  Scan another.").
            NEXT Scan_Block.
         END.        

         /* Verify the Ship Pallet, Packages and Orders is at ReadyToLoad Status */
         FOR EACH ShipPackage OF ShipPallet NO-LOCK:

            IF ShipPackage.ShipStatusID <> intReadyToLoadShipStatus THEN
            DO:
               RUN DisplayMessage("Invalid Ship Pallet",
                                  "[Ship Package] [" + ShipOrder.OrderRef + "] is NOT at Ready to Load Status.  Scan another.").
               chrResult = "NotValid".
               NEXT Scan_Block.
            END.

            FOR EACH ShipOrder OF ShipPackage NO-LOCK:

               IF ShipOrder.ShipOrderStatusID <> intReadyToLoadShipOrderStatus 
                  AND ShipOrder.ShipOrderStatusID <> intBeingLoadedShipOrderStatus THEN
               DO:
                  RUN DisplayMessage("Invalid Ship Pallet",
                                     "[Ship Order] [" + ShipOrder.OrderRef + "] is NOT at ReadyToLoad or BeingLoaded Status.  Scan another.").
                  chrResult = "NotValid".
                  NEXT Scan_Block.
               END.

            END. /* EACH ShipOrder */

         END. /* EACH ShipPackage */

         /* Verify Pallet is in Correct Location */
         FIND FIRST ShipLane NO-LOCK
            WHERE ShipLane.ShipLaneID = intSsnShipLaneID:intValue NO-ERROR.

         IF ShipPallet.LocationID <> ShipLane.LocationID THEN
         DO:
            FIND FIRST Location NO-LOCK
               WHERE Location.LocationID = ShipLane.LocationID NO-ERROR.

            RUN DisplayMessage("Ship Pallet Error",
                               "[Ship Pallet] [" + chrScanShipPalletID + "] is NOT in Location " + Location.LocationRef + ".  Scan another.").
            NEXT Scan_Block.
         END.        

         /* Check All Orders are Ready to Load */
         FOR EACH ShipPackage OF ShipPallet NO-LOCK:
            FIND FIRST ShipOrder OF ShipPackage NO-LOCK NO-ERROR.

            IF NOT CAN-FIND(FIRST ShipOrderStatusGroupLink
                            WHERE ShipOrderStatusGroupLink.ShipOrderStatusGroupID = ShipOrderStatusGroup.ShipOrderStatusGroupID
                            AND   ShipOrderStatusGroupLink.ShipOrderStatusID = ShipOrder.ShipOrderStatusID) THEN
            DO:
               RUN DisplayMessage("Ship Order Error",
                                  "[Ship Order] [" + ShipOrder.OrderRef + "] is NOT at Ready to Load Status.  Scan another.").
               NEXT Scan_Block.               
            END.

         END.

         /* Find the Carrier to get the Trailer Load Max */
         FIND FIRST Carrier OF ShipPallet NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Carrier THEN
         DO:
            RUN DisplayMessage("Ship Load Error",
                              "[Carrier ID] [" + STRING(ShipPallet.CarrierID) + "] does not exist.  Scan another.").
            NEXT Scan_Block.
         END.

         /* Check the Value of the Pallets for the Load */
         FOR EACH loadPallet NO-LOCK
            WHERE loadPallet.OutboundID = intSsnOutboundID:intValue 
            OR    loadPallet.ShipPalletID = ShipPallet.ShipPalletID:
            
            ASSIGN intLoadValue               = intLoadValue + loadPallet.PalletValue
                   intDangerousGoodsLoadValue = intDangerousGoodsLoadValue + loadPallet.DangerousGoodsTotal.

         END.
         IF intLoadValue > Carrier.MaxValuePerTrailer THEN
         DO:
            RUN DisplayMessage("Ship Load Error",
                               "[Loading Pallet] [" + ShipPallet.PalletRef + "] will Exceed the Carrier Max Value of " + STRING(Carrier.MaxValuePerTrailer) + ".  Scan another.").
            NEXT Scan_Block.               
         END.
         IF intDangerousGoodsLoadValue > Carrier.DangerousGoodsLimit THEN 
         DO:
            RUN DisplayMessage("Ship Load Error",
                               "[Loading Pallet] [" + ShipPallet.PalletRef + "] will Exceed the DangerousGoods Limit of " + STRING(Carrier.DangerousGoodsLimit) + ".  Scan another.").
            NEXT Scan_Block.               
         END.
         
         /* Reset and Check for Order Value */
         intLoadValue = 0.
         
         /* Check the Value of the Orders for the Load */
         Pallet_Block:
         FOR EACH loadPallet NO-LOCK
            WHERE loadPallet.OutboundID = intSsnOutboundID:intValue 
            OR    loadPallet.ShipPalletID = ShipPallet.ShipPalletID:

            /* Get all Packages of Pallet */
            FOR EACH ShipPackage OF loadPallet NO-LOCK:

               /* Get Order Of Packages */
               Order_Block:
               FOR EACH ShipOrder OF ShipPackage NO-LOCK:                 

                  FIND FIRST ttOrderValue NO-LOCK
                    WHERE ttOrderValue.OrderNo = ShipOrder.ShipOrderID NO-ERROR.

                  /* Order Value Already Processed */
                  IF AVAILABLE ttOrderValue THEN 
                     NEXT Order_Block.

                  CREATE ttOrderValue.
                  ASSIGN ttOrderValue.OrderNo = ShipOrder.ShipOrderID.

                  /* Accum all Packages of Order */
                  FOR EACH orderPackage OF ShipOrder NO-LOCK:
                     /* Check Order is Not on another Load */
                     IF orderPackage.OutboundID <> intSsnOutboundID:intValue AND
                        orderPackage.OutboundID <> 0 THEN
                     DO:
                        RUN DisplayMessage("Ship Load Error",
                                           "[Ship Order] [" + ShipOrder.OrderRef + "] has been Loaded on another Trailer.  Scan another.").
                        NEXT Scan_Block.
                     END.

                     ttOrderValue.OrderValue = orderPackage.PackageValue.
                     
                  END.

               END. /* Order_Block */

            END. /* FOR EACH ShipPackage of loadPallet */

         END. /* Pallet_Block */

         /* Total up and Check */
         FOR EACH ttOrderValue:
            intLoadValue = intLoadValue + ttOrderValue.OrderValue.
         END.

         IF intLoadValue > Carrier.MaxValuePerTrailer THEN
         DO:
            RUN DisplayMessage("Ship Load Error",
                               "[Loading Pallet] [" + ShipPallet.PalletRef 
                                 + "] will Exceed the Carrier Max Value of " 
                                 + STRING(Carrier.MaxValuePerTrailer) + ".  Scan another.").
            NEXT Scan_Block.               
         END.

         /* See if the other Packges of any of the ShipOrders on this Pallet either not yet built or on other Pallets not in ShipLane */
         chrOtherPalletsOfOrder  = "".
         FOR EACH loadedShipPackage OF ShipPallet NO-LOCK,
            EACH loadedShipOrder OF loadedShipPackage NO-LOCK:

            FOR EACH otherShipPackage OF loadedShipOrder NO-LOCK,
               EACH  otherShipPallet OF otherShipPackage NO-LOCK
                  WHERE otherShipPallet.ShipStatusID <> intReadyToLoadShipStatus
                  AND   otherShipPallet.ShipStatusID <> intLoadedShipStatus
                  AND   otherShipPallet.ShipPalletID <> ShipPallet.ShipPalletID:
                  
                     RUN DisplayMessage("Ship Load Error",
                                        "Ship Pallet [" + otherShipPallet.PalletRef 
                                          + "] of same Order is not loaded and not at status "
                                          + "[ReadyToLoad].  Scan another.").
                     NEXT Scan_Block.   
                     
            END.   

            FOR EACH otherShipPackage OF loadedShipOrder NO-LOCK,
               EACH  otherShipPallet OF otherShipPackage NO-LOCK
                  WHERE otherShipPallet.LocationID <> ShipLane.LocationID
                  AND   otherShipPallet.ShipPalletID <> ShipPallet.ShipPalletID
                  AND   otherShipPallet.ShipStatusID <> intLoadedShipStatus:
                  
                     FIND FIRST ttPackageList 
                        WHERE ttPackageList.Package = otherShipPallet.PalletRef NO-ERROR.
                     IF NOT AVAILABLE ttPackageList THEN
                     DO:
                        CREATE ttPackageList.
                        ASSIGN ttPackageList.Package = otherShipPallet.PalletRef.
                     END.
                     
            END.  

            FOR EACH otherShipPackage OF loadedShipOrder NO-LOCK
               WHERE otherShipPackage.LocationID <> ShipLane.LocationID
               AND   otherShipPackage.ShipPalletID = 0:

                  FIND FIRST ttPackageList 
                        WHERE ttPackageList.Package = otherShipPackage.PackageRef NO-ERROR.
                  IF NOT AVAILABLE ttPackageList THEN
                  DO:
                     CREATE ttPackageList.
                     ASSIGN ttPackageList.Package = otherShipPackage.PackageRef .
                  END.   

            END.

         END. /* EACH ShipPackage */
         
         /* Building the Packages List */
         FOR EACH ttPackageList
            BY ttPackageList.Package:
   
               chrOtherPalletsOfOrder = chrOtherPalletsOfOrder + ttPackageList.Package + ",".      

         END.

         IF chrOtherPalletsOfOrder > "" THEN
         DO:
            chrOtherPalletsOfOrder = RIGHT-TRIM(chrOtherPalletsOfOrder,",").
            chrCheckMessage = "There are other Ship Pallet/Ship Packages of same Order not in [" 
                                 + ShipLane.LaneName + "] location. Item Ref: [" + chrOtherPalletsOfOrder + "]".
         END.

         IF chrCheckMessage > "" THEN
         DO:
            RUN DisplayMessage("Warning",
                               chrCheckMessage).
         END.


         /* All Ok */
         intSsnShipPalletID:setValue(ShipPallet.ShipPalletID).

         chrResult = "Continue".
         LEAVE Main_Block.
   
      END. /* Scan_Block */
   END. /* Main_Block */
      

   /* Clean Up */
   DELETE OBJECT chrSsnLastScan     NO-ERROR.
   DELETE OBJECT intSsnShipPalletID NO-ERROR.   
   DELETE OBJECT intSsnOutboundID   NO-ERROR.   

   /* Releases */
   RELEASE ShipOrderStatusGroup NO-ERROR.
   RELEASE loadPallet           NO-ERROR.
   RELEASE ShipPackage          NO-ERROR.
   RELEASE ShipOrder            NO-ERROR.
   RELEASE orderPackage         NO-ERROR.
   RELEASE ShipPallet           NO-ERROR.
   RELEASE ShipLane             NO-ERROR.
   RELEASE Carrier              NO-ERROR.
   RELEASE otherShipPackage     NO-ERROR.
   RELEASE otherShipPackage     NO-ERROR.
   RELEASE loadedShipPackage    NO-ERROR.
   RELEASE loadedShipOrder      NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
