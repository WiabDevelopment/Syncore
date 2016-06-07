/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntScanPackageQty.p
Purpose : Scan Package and Qty

          Possible Results : Continue

Author  : Mateusz Nogaj
Date    : 07/05/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
19/09/2014  MC HyperBR    If scanned package has a parent, ask user to scan parent
22/10/2014  BR DaymenNL   Add F6 CompleteLocation on scanpackage
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
   
   /* Buffers */
   DEFINE BUFFER workStockPackage FOR StockPackage.
   
   /* Local Variables */     
   DEFINE VARIABLE chrScanPackage    AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrPackageQty     AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrScanQty        AS CHARACTER FORMAT "x(8)"  VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrCountTask      AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrCountLocation  AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrExpectedQty    AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrExpectedPrompt AS CHARACTER FORMAT "x(24)" VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE intScanQty        AS INTEGER                                         NO-UNDO.

   /* Local Objects */   
   DEFINE VARIABLE intSsnPackageID      AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageRef     AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageQty     AS sessionValue NO-UNDO. 
   DEFINE VARIABLE chrSsnLastScan       AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.   
          
   /* Clear Session Data */
   fClearSessionValue("LastScan").
   fClearSessionValue("PackageID"). 
   fClearSessionValue("PackageRef").  
   fClearSessionValue("PackageQty").   
   
   /* Create New Session Data */
   chrSsnLastScan   = fNewSessionValue("LastScan").
   intSsnPackageID  = fNewSessionValue("PackageID").
   intSsnPackageRef = fNewSessionValue("PackageRef").
   intSsnPackageQty = fNewSessionValue("PackageQty").   
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").   
      
   /* Frames and UI */
   DEFINE FRAME ScanPackageFrame                 
       chrCountTask      NO-LABEL COLON 1 SKIP
       chrCountLocation  NO-LABEL COLON 1 
       SKIP(1)         
       "      Scan Package Ref     "    
       chrScanPackage    NO-LABEL COLON 1 
       SKIP(1)
       chrExpectedPrompt NO-LABEL COLON 1 SKIP
       chrExpectedQty    NO-LABEL COLON 1 
       SKIP(1) 
       chrPackageQty     NO-LABEL COLON 1 SKIP
       chrScanQty        NO-LABEL COLON 9 
       SKIP(1)  
   WITH SIDE-LABELS TITLE " Scan Package " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   fTranslateWidget(FRAME ScanPackageFrame:HANDLE).
   
   /* Events */
   ON 'PF4':U ANYWHERE
   DO:
      chrScanPackage:SCREEN-VALUE IN FRAME ScanPackageFrame = "COMPLETE".
      chrScanQty:SCREEN-VALUE IN FRAME ScanPackageFrame = "COMPLETE".
      RETURN NO-APPLY.
   END.    
     
   ON 'F6':U ANYWHERE
   DO:
      chrScanPackage:SCREEN-VALUE IN FRAME ScanPackageFrame = "COMPLETELOCATION".
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

         HIDE chrScanQty IN FRAME ScanPackageFrame.
            
         chrScanPackage = "".
         DISPLAY chrCountTask     WITH FRAME ScanPackageFrame.
         DISPLAY chrCountLocation WITH FRAME ScanPackageFrame.
         
         UPDATE chrScanPackage WITH FRAME ScanPackageFrame.         

         IF chrScanPackage = "COMPLETE" OR chrScanPackage = "COMPLETELOCATION" THEN
         DO:
            chrSsnLastScan:setValue(chrScanPackage).
            chrResult = "Continue".
            LEAVE Main_Block.
         END.

         /* Cannot be Blank */
         IF TRIM(chrScanPackage) = "" THEN
         DO:
            NEXT Scan_Block.
         END.

         /* Check Package has a Match */
         FIND FIRST StockPackage NO-LOCK
            WHERE StockPackage.PackageRef = chrScanPackage NO-ERROR.
         IF NOT AVAILABLE StockPackage THEN
         DO:
            RUN DisplayMessage("Package Error",
                               "[Package Ref] [" + chrScanPackage + "] does not Exist.").
            NEXT Scan_Block.
         END.
         ELSE /* AVAILABLE Package */
         DO:
            /* Check if package has parent */
            IF StockPackage.ParentStockPackageID > 0 AND StockPackage.LocationID = Location.LocationID THEN 
            DO:
               FIND FIRST workStockPackage NO-LOCK 
                  WHERE workStockPackage.StockPackageID = StockPackage.ParentStockPackageID NO-ERROR.
               IF NOT AVAILABLE workStockPackage THEN 
               DO:
                  RUN DisplayMessage("Package Error",
                                     "This Package has a parent; parent package ID [" 
                                       + STRING(StockPackage.ParentStockPackageID) + "] does not exist.").
                  NEXT Scan_Block.
               END.
               
               RUN DisplayMessage("Package Error",
                                  "This Package has a parent [" + workStockPackage.PackageRef 
                                    + "], please scan this Parent label to continue with the Count.").
               NEXT Scan_Block.
            END.
            
            /* Assign Package Data */
            chrSsnLastScan:setValue(chrScanPackage).
            intSsnPackageRef:setValue(StockPackage.PackageRef).
            intSsnPackageID:setValue(StockPackage.StockPackageID).

            /* Qty Capture Required */
            Qty_Block:
            REPEAT:
            
               /* Reset Variables */
               chrScanQty  = "".
               chrPackageQty = "Scan Qty".

               /*Center the label*/
               chrPackageQty = fCenterOnScreen(TRIM(chrPackageQty)).
               
               FIND FIRST CountTaskLocationPackage NO-LOCK 
                  WHERE CountTaskLocationPackage.CountTaskLocationID = CountTaskLocation.CountTaskLocationID
                  AND   CountTaskLocationPackage.StockPackageID = StockPackage.StockPackageID NO-ERROR.
                  
               IF AVAILABLE CountTaskLocationPackage then
               DO:
                  chrExpectedPrompt = "Expected from Location".
                  chrExpectedPrompt = fCenterOnScreen(chrExpectedPrompt).
                  chrExpectedQty = STRING(CountTaskLocationPackage.QtyExpected,"999999").
                  chrExpectedQty = fCenterOnScreen(chrExpectedQty). 

                  IF CountTaskLocation.BlindCount = YES THEN 
                  DO:
                     HIDE chrExpectedPrompt IN FRAME ScanPackageFrame.
                     HIDE chrExpectedQty    IN FRAME ScanPackageFrame.
                  END.
                  ELSE 
                  DO:
                     DISPLAY chrExpectedPrompt WITH FRAME ScanPackageFrame.
                     DISPLAY chrExpectedQty    WITH FRAME ScanPackageFrame.
                  END.
               END. /* AVAILABLE CountTaskLocationPart */

               DISPLAY chrPackageQty WITH FRAME ScanPackageFrame.
               UPDATE  chrScanQty WITH FRAME ScanPackageFrame.    

               IF chrScanQty = "COMPLETE" OR chrScanQty = "COMPLETELOCATION" THEN
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
                  /* Assign Package Data */                  
                  chrSsnLastScan:setValue(chrScanPackage).
                  intSsnPackageRef:setValue(StockPackage.PackageRef).
                  intSsnPackageID:setValue(StockPackage.StockPackageID).
                  intSsnPackageQty:setValue(intScanQty).

                  chrResult = "Continue".
                  LEAVE Main_Block.
               END.

            END. /* Qty_Block */
            
         END. /* IF AVAILABLE Package */

      END. /* Scan_Block */

   END. /* Main_Block */
         
   /* Clean Up */
   DELETE OBJECT chrSsnLastScan       NO-ERROR.   
   DELETE OBJECT intSsnPackageID      NO-ERROR.
   DELETE OBJECT intSsnPackageQty     NO-ERROR.
   DELETE OBJECT intSsnPackageRef     NO-ERROR.
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.

   /* Release */
   RELEASE StockPackage NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */