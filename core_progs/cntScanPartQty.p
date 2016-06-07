/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntScanPartQty.p
Purpose : Scan Part and Quantity

          Possible Results : Continue

Author  : Mateusz Nogaj
Date    : 07/05/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
17/10/2014 BR  DaymenNL   Add EAN code search during part scan
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:

   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}

   /*Optional Includes*/
   {fncStringFunctions.i}

   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */ 
   {prcProcessDebugging.i}
   
   /* Local Variables */     
   DEFINE VARIABLE chrScanPart       AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrPartQty        AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrScanQty        AS CHARACTER FORMAT "x(8)"  VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrCountTask      AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrCountLocation  AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrExpectedQty    AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrExpectedPrompt AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE intScanQty        AS INTEGER                                         NO-UNDO.

   /* Local Objects */   
   DEFINE VARIABLE intSsnPartID         AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartQty        AS sessionValue NO-UNDO. 
   DEFINE VARIABLE chrSsnLastScan       AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.  
   DEFINE VARIABLE intSsnPartRef        AS sessionValue NO-UNDO. 
            
   /* Clear Session Data */
   fClearSessionValue("LastScan").
   fClearSessionValue("PartID").
   fClearSessionValue("PartQty").  
   fClearSessionValue("PartRef").  
   
   /* Create New Session Data */
   chrSsnLastScan = fNewSessionValue("LastScan").
   intSsnPartID   = fNewSessionValue("PartID").
   intSsnPartQty  = fNewSessionValue("PartQty").
   intSsnPartRef  = fNewSessionValue("PartRef").
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").   
      
   /* Frames and UI */
   DEFINE FRAME ScanPartFrame                 
       chrCountTask      NO-LABEL COLON 1 SKIP
       chrCountLocation  NO-LABEL COLON 1 
       SKIP(1)
       "   Scan PartRef/EanCode  "    
       chrScanPart       NO-LABEL COLON 1 
       SKIP(1)       
       chrExpectedPrompt NO-LABEL COLON 1 SKIP
       chrExpectedQty    NO-LABEL COLON 1 
       SKIP(1)
       chrPartQty        NO-LABEL COLON 1 SKIP
       chrScanQty        NO-LABEL COLON 9 
       SKIP(1)
   WITH SIDE-LABELS TITLE " Scan Part " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   fTranslateWidget(FRAME ScanPartFrame:HANDLE).
   
   /* Events */
   ON 'PF4':U ANYWHERE
   DO:
      chrScanPart:SCREEN-VALUE IN FRAME ScanPartFrame = "COMPLETE".
      chrScanQty:SCREEN-VALUE IN FRAME ScanPartFrame = "COMPLETE".
      RETURN NO-APPLY.
   END.
   
   ON 'F6':U ANYWHERE
   DO:
      chrScanPart:SCREEN-VALUE IN FRAME ScanPartFrame = "COMPLETELOCATION".
      RETURN NO-APPLY.
   END.      

   Main_Block:
   DO ON ERROR UNDO:
      
      /* Validate Session Data */
      FIND FIRST CountTaskLocation NO-LOCK
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Task Location ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.
            
      FIND FIRST CountTask OF CountTaskLocation NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountTask THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTask ID] [" + STRING(CountTaskLocation.CountTaskID) + "] does not Exist.").
         LEAVE Main_Block.
      END. 
                  
      FIND FIRST Location NO-LOCK
         WHERE Location.LocationID = CountTaskLocation.LocationID NO-ERROR.
      IF NOT AVAILABLE Location THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Location ID] [" + STRING(CountTaskLocation.LocationID) + "] does not Exist.").
         LEAVE Main_Block.
      END.
      
      chrCountTask = " Count TaskID: " + STRING(CountTask.CountTaskID,"999999").
      chrCountTask = fCenterOnScreen(chrCountTask).
      chrCountLocation = " Location: " + Location.LocationRef.
      chrCountLocation = fCenterOnScreen(chrCountLocation).     

      Scan_Block:
      REPEAT ON STOP UNDO, RETRY:      

         HIDE chrScanQty IN FRAME ScanPartFrame.
                    
         chrScanPart = "".
         DISPLAY chrCountTask     WITH FRAME ScanPartFrame.
         DISPLAY chrCountLocation WITH FRAME ScanPartFrame.
         
         UPDATE chrScanPart WITH FRAME ScanPartFrame.         

         IF chrScanPart = "COMPLETE" OR chrScanPart = "COMPLETELOCATION" THEN
         DO:
            chrSsnLastScan:setValue(chrScanPart).
            chrResult = "Continue".
            LEAVE Main_Block.
         END.
         
         /* Cannot be Blank */
         IF TRIM(chrScanPart) = "" THEN
         DO:
            NEXT Scan_Block.
         END.
          
         /* Check Part has a Match */
         FIND FIRST Part NO-LOCK
            WHERE Part.PartRef = chrScanPart NO-ERROR.                                                    
         IF NOT AVAILABLE Part THEN
         DO:             
            FIND FIRST Part NO-LOCK
               WHERE Part.EanCode = chrScanPart NO-ERROR. 
            IF NOT AVAILABLE Part THEN
            DO:                      
               FIND FIRST PartStockEntityLink NO-LOCK
                  WHERE PartStockEntityLink.PartStockEntityEanCode = chrScanPart NO-ERROR.
               IF NOT AVAILABLE PartStockEntityLink THEN 
               DO:
                  RUN DisplayMessage("Part Error",
                  "[PartRef / EanCode] [" + chrScanPart + "] does not Exist.").
                  NEXT Scan_Block.   
               END.
            END.  /*not available EanCode = chrScanPart*/                                            
         END. /*not available PartRef = chrScanPart*/
                              
         IF NOT AVAILABLE Part THEN /*validate against PartStockEntityLink*/
         DO:
            FIND FIRST Part NO-LOCK
               WHERE Part.PartID =  PartStockEntityLink.PartID NO-ERROR.
         END.                           
         IF AVAILABLE Part THEN  /*PartID available either from PartStockEntityLink or Part*/  
         DO:          
            /* Assign Part Data */
            chrSsnLastScan:setValue(chrScanPart).
            intSsnPartID:setValue(Part.PartID).

            /* Qty Capture Required */
            Qty_Block:
            REPEAT:
            
               /* Reset Variables */
               chrScanQty  = "".
               chrPartQty = "Scan Qty".

               /*Center the label*/
               chrPartQty = fCenterOnScreen(TRIM(chrPartQty)).
               
               FIND FIRST CountTaskLocationPart NO-LOCK 
                  WHERE CountTaskLocationPart.CountTaskLocationID = CountTaskLocation.CountTaskLocationID
                  AND   CountTaskLocationPart.PartID = Part.PartID NO-ERROR.
                  
               IF AVAILABLE CountTaskLocationPart then
               DO:
                  chrExpectedPrompt = "Expected from Location".
                  chrExpectedPrompt = fCenterOnScreen(chrExpectedPrompt).
                  chrExpectedQty = STRING(CountTaskLocationPart.QtyExpected,"999999").
                  chrExpectedQty = fCenterOnScreen(chrExpectedQty). 
               
                  IF CountTaskLocation.BlindCount = YES THEN 
                  DO:
                     HIDE chrExpectedPrompt IN FRAME ScanPartFrame.
                     HIDE chrExpectedQty    IN FRAME ScanPartFrame.
                  END.
                  ELSE 
                  DO:
                     DISPLAY chrExpectedPrompt WITH FRAME ScanPartFrame.
                     DISPLAY chrExpectedQty    WITH FRAME ScanPartFrame.
                  END.
               END. /* AVAILABLE CountTaskLocationPart */
               
               DISPLAY chrPartQty WITH FRAME ScanPartFrame.
               UPDATE  chrScanQty WITH FRAME ScanPartFrame.    

               IF chrScanQty = "COMPLETE" OR chrScanPart = "COMPLETELOCATION" THEN
               DO:
                  chrSsnLastScan:setValue(chrScanQty).
                  chrResult = "Continue".
                  LEAVE Main_Block.
               END.
               
               ASSIGN intScanQty = INTEGER(chrScanQty) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
               DO:
                  RUN DisplayMessage("Qty Error",
                                     "Scanned Qty [" + chrScanQty + "] is incorrect. Enter Again.").
                  NEXT Qty_Block.
               END.
               ELSE IF intScanQty = 0 THEN 
               DO:
                  RUN DisplayMessage("Qty Error",
                                     "Scanned Qty is 0. Enter Again.").
                  NEXT Qty_Block.
               END.
               ELSE DO:
                  /* Assign Part Data */                  
                  chrSsnLastScan:setValue(chrScanPart).
                  intSsnPartID:setValue(Part.PartID).
                  intSsnPartRef:setValue(Part.PartRef).
                  intSsnPartQty:setValue(intScanQty).

                  chrResult = "Continue".
                  LEAVE Main_Block.
               END.
            END. /* Qty_Block */
         END. /*Available part*/
      END. /* Scan_Block */
   END. /* Main_Block */
         
   /* Clean Up */
   DELETE OBJECT chrSsnLastScan       NO-ERROR.   
   DELETE OBJECT intSsnPartID         NO-ERROR.
   DELETE OBJECT intSsnPartQty        NO-ERROR.
   DELETE OBJECT intSsnPartRef        NO-ERROR.
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.

   /* Release */
   RELEASE Part                NO-ERROR.
   RELEASE CountTaskLocation   NO-ERROR.
   RELEASE CountTask           NO-ERROR.
   RELEASE Location            NO-ERROR.
   RELEASE PartStockEntityLink NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */