/*------------------------------------------------------------------------------------------------------------------------------------------
Program : lodScanSort.p
Purpose : Scan the Outbound Sort.
Author  : DCummins

          Possible results : Continue

Date    : 27/03/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
15/12/2014 CS  Nextel     Added releases
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
   DEFINE VARIABLE chrShipLane      AS CHARACTER FORMAT "x(24)" NO-UNDO.
   
   /* Session Objects */   
   DEFINE VARIABLE intSsnShipLaneID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnShipBayID  AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnOutboundID AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnLastScan   AS sessionValue NO-UNDO.
      
   /* Reset Data */
   fClearSessionValue("ShipLaneID").
   fClearSessionValue("LastScan").
   
   /* Set New Data */
   intSsnShipLaneID = fNewSessionValue("ShipLaneID").
   chrSsnLastScan   = fNewSessionValue("LastScan").

   /* Get Current Data */
   intSsnShipBayID  = fGetSessionValue("ShipBayID").
   intSsnOutboundID = fGetSessionValue("OutboundID").
   
   /* Frames and UI */
   DEFINE FRAME ShipLaneFrame                 
       SKIP(4)           
       "    Scan Ship Lane Code "     
       SKIP
       chrShipLane NO-LABEL COLON 1
       SKIP(6)
   WITH SIDE-LABELS TITLE " Scan Ship Lane " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   /* Events */
   ON 'PF4':U ANYWHERE
   DO:
      chrShipLane:SCREEN-VALUE IN FRAME ShipLaneFrame = "COMPLETE".
      RETURN NO-APPLY.
   END.   
   

   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      Scan_Block:
      REPEAT ON STOP UNDO, RETRY:    

         /* Prompt to Scan */         
         UPDATE chrShipLane WITH FRAME ShipLaneFrame.

         chrShipLane = TRIM(chrShipLane).
         chrSsnLastScan:setValue(chrShipLane).
                
         IF chrShipLane = "COMPLETE" THEN
         DO:
            chrResult = "Continue".
            LEAVE Main_Block.
         END.

         FIND FIRST ShipLane NO-LOCK
            WHERE ShipLane.LaneCode = chrShipLane NO-ERROR.
         IF NOT AVAILABLE ShipLane THEN
         DO:
            RUN DisplayMessage("Ship Lane Error",
                               "Ship Lane " + STRING(chrShipLane) + " record not found.  Scan another.").
            NEXT Scan_Block.
         END.         

         /* Check there is Something in the Ship Lane for the Outbound */
         FIND FIRST ShipLaneOutboundLink NO-LOCK
            WHERE ShipLaneOutboundLink.OutboundID = intSsnOutboundID:intValue
            AND   ShipLaneOutboundLink.ShipLaneID = ShipLane.ShipLaneID NO-ERROR.
         IF NOT AVAILABLE ShipLaneOutboundLink THEN
         DO:
            RUN DisplayMessage("Ship Lane Error",
                               "Ship Lane " + ShipLane.LaneName + " is NOT linked to Outbound #" + STRING(intSsnOutboundID:intValue) + ".  Scan another.").
            NEXT Scan_Block.
         END.         
          
         /* Store the Ship Lane ID */
         intSsnShipLaneID:setValue(ShipLane.ShipLaneID).         
         
         chrResult = "Continue".             

         LEAVE Scan_Block.
      END.
      
   END.
     
   /* Clean Up */   
   DELETE OBJECT intSsnShipLaneID NO-ERROR.
   DELETE OBJECT intSsnShipBayID  NO-ERROR.
   DELETE OBJECT intSsnOutboundID NO-ERROR.
   DELETE OBJECT chrSsnLastScan   NO-ERROR.

   /* Releases */
   RELEASE ShipLane             NO-ERROR.
   RELEASE ShipLaneOutboundLink NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
