/*------------------------------------------------------------------------------------------------------------------------------------------
Program : lodScanBay.p
Purpose : Scan the Outbound Ship Bay.

          Possible results : Continue           

Author  : DCummins
Date    : 20/03/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
19/09/2013 AB  CR1018     Added ScanBayUserPrompt session variable to define UserPrompt for frame
                          Fixed "?" DisplayMessage issue 
                          Added validation check for OutboundStatusGroup
23/04/2014 BR  CR 1052    UI Standardization                          
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
   DEFINE VARIABLE chrUserPrompt AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrShipBay    AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   
   /* Session Objects */   
   DEFINE VARIABLE intSsnShipBayID  AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnOutboundID AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnLastScan   AS sessionValue NO-UNDO.
      
   /* Reset Data */
   fClearSessionValue("ShipBay").
   fClearSessionValue("LastScan").
   
   /* Set New Data */
   intSsnShipBayID  = fNewSessionValue("ShipBayID").
   intSsnOutboundID = fNewSessionValue("OutboundID").
   chrSsnLastScan   = fNewSessionValue("LastScan").
   
   /* Get Current Data */
   chrUserPrompt = fGetSessionValue("ScanBayUserPrompt"):chrValue.
   
   /* Falling Back To Default Value */
   IF TRIM(chrUserPrompt) = "" THEN
   DO:
      chrUserPrompt = " Scan Ship Bay Label".
   END. /* IF TRIM(chrUserPrompt) = "" THEN */                

   /* Frames and UI */
   DEFINE FRAME ShipBayFrame                 
      SKIP(4)           
      chrUserPrompt NO-LABEL COLON 1
      chrShipBay    NO-LABEL COLON 1
      SKIP(6)
   WITH SIDE-LABELS TITLE " Scan Ship Bay " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.

   fTranslateWidget(FRAME ShipBayFrame:HANDLE).

   /* Events */
   ON 'PF4':U ANYWHERE
   DO:
      chrShipBay:SCREEN-VALUE IN FRAME ShipBayFrame = "COMPLETE".
      RETURN NO-APPLY.
   END. /* ON 'PF4':U ANYWHERE */   


   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      /* Get the Outbound Status Group */
      FIND FIRST OutboundStatusGroup NO-LOCK
         WHERE OutboundStatusGroup.GroupName = "Loading" NO-ERROR.   
      IF NOT AVAILABLE OutboundStatusGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[OutboundStatusGroup] [" + "Loading" + "] does not Exist").       
         UNDO Main_Block, LEAVE Main_Block.
      END. /* IF NOT AVAILABLE OutboundStatusGroup THEN */

      Scan_Block:
      REPEAT ON STOP UNDO Scan_Block, RETRY Scan_Block:    

         /* Prompt to Scan */         
         DISPLAY chrUserPrompt WITH FRAME ShipBayFrame.
         UPDATE chrShipBay WITH FRAME ShipBayFrame.

         chrShipBay = TRIM(chrShipBay).
         chrSsnLastScan:setValue(chrShipBay). 

         IF chrShipBay = "COMPLETE" THEN
         DO:
            chrResult = "Continue".
            LEAVE Main_Block.
         END. /* IF chrShipBay = "COMPLETE" THEN */

         FIND FIRST Location NO-LOCK
            WHERE Location.LocationRef = chrShipBay NO-ERROR.
         IF NOT AVAILABLE Location THEN
         DO:
            RUN DisplayMessage("Location Error",
                               "[Location] [" + (IF chrShipBay = ? THEN "?" ELSE chrShipBay) + "] record not found.  Scan another.").
            NEXT Scan_Block.
         END. /* IF NOT AVAILABLE Location THEN */         

         IF Location.LocationType <> fGetTypeID("Location", "ShippingBay") THEN
         DO:
            RUN DisplayMessage("Location Error",
                               "[Location] [" + (IF chrShipBay = ? THEN "?" ELSE chrShipBay) + "] is NOT a [Shipping Bay].  Scan another.").
            NEXT Scan_Block.
         END. /* IF Location.LocationType <> fGetTypeID("Location", "ShippingBay") THEN */         
          
         /* Check Outbound is Attached */
         FIND FIRST Outbound NO-LOCK
            WHERE Outbound.LocationID = Location.LocationID NO-ERROR.
         IF NOT AVAILABLE Outbound THEN
         DO:
            RUN DisplayMessage("Location Error",
                               "[Location] [" + chrShipBay + "] does NOT have anything Attached.  Scan another.").
            NEXT Scan_Block.
         END. /* IF NOT AVAILABLE Outbound THEN */

         /* Validate the Outbound is at a Valid Status */
         IF NOT CAN-FIND(FIRST OutboundStatusGroupLink 
                         WHERE OutboundStatusGroupLink.OutboundStatusGroupID = OutboundStatusGroup.OutboundStatusGroupID
                         AND   OutboundStatusGroupLink.OutboundStatusID = Outbound.OutboundStatusID) THEN
         DO:
            RUN DisplayMessage("Outbound Error",
                               "[Outbound] [" + STRING(Outbound.OutboundID) + "] is NOT at a Valid Status.").
            NEXT Scan_Block.
         END. /* IF NOT CAN-FIND(FIRST OutboundStatusGroupLink */

         /* Store the Bay Location ID and Outbound ID*/
         intSsnShipBayID:setValue(Location.LocationID).
         intSsnOutboundID:setValue(Outbound.OutboundID).
         
         chrResult = "Continue".             
         
         LEAVE Main_Block.

      END. /* Scan_Block */
      
   END. /* Main_Block */       

   /* Clean Up */   
   DELETE OBJECT intSsnShipBayID  NO-ERROR.
   DELETE OBJECT intSsnOutboundID NO-ERROR.
   DELETE OBJECT chrSsnLastScan   NO-ERROR.
   
   /* Releases */
   RELEASE Location            NO-ERROR.
   RELEASE Outbound            NO-ERROR.
   RELEASE OutboundStatusGroup NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
