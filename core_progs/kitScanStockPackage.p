/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitScanStockPackage.p
Purpose : Scan a Kitting Stock Package and checks whether it Exists or not.
                    
          Possible outcomes : Continue.
          
Author  : BR
Date    : 17th Aug 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}   
   {fncStringFunctions.i}   
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Status Functions */
   {fncStatusTypeFunctions.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
  
   /* Clear Session */
   fClearSessionValues("").

   /* Local Variables */
   DEFINE VARIABLE chrScannedPackage   AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   DEFINE VARIABLE chrUserPrompt       AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   
   /* Session Objects */
   DEFINE VARIABLE chrSsnLastScan        AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnChildPackageID  AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartID          AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnUserPrompt      AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnBuildLocationID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnWorkOrderID     AS sessionValue NO-UNDO.
   
   /* Frames and UI */
   DEFINE FRAME ScanPackageFrame
      SKIP(4)
      chrUserPrompt     NO-LABEL COLON 1
      chrScannedPackage NO-LABEL COLON 1
      SKIP(6)
   WITH SIDE-LABELS TITLE " Scan Stock Package " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   fTranslateWidget(FRAME ScanPackageFrame:HANDLE).
   
   /* Events */
   ON 'PF4':U ANYWHERE 
   DO:
      chrScannedPackage:SCREEN-VALUE IN FRAME ScanPackageFrame = "COMPLETE".
      RETURN NO-APPLY.
   END. /* ON 'PF4':U ANYWHERE */
   
   
   /* Create New Session Data */
   chrSsnLastScan        = fNewSessionValue("LastScan").
   intSsnChildPackageID  = fNewSessionValue("ChildPackageID").
   intSsnPartID          = fNewSessionValue("PartID").
   intSsnBuildLocationID = fNewSessionValue("BuildLocationID").
   intSsnWorkOrderID     = fNewSessionValue("WorkOrderID").
   
   /* Get Current Session Data */
   chrUserPrompt = fGetSessionValue("StockPackageUserPrompt"):chrValue.

   /*Center the label value derived from session*/
   chrUserPrompt = fCenterOnScreen(TRIM(chrUserPrompt)).
                         
   IF TRIM(chrUserPrompt) = "" THEN
   DO:
      chrUserPrompt = fCenterOnScreen("Scan Stock Package").
   END.

   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:

      Scan_Block:
      REPEAT ON STOP UNDO Scan_Block, RETRY Scan_Block:
         
         /* Clear UI Variable */
         chrScannedPackage = "".
         
         /* Get scanned value from GUN */
         DISPLAY chrUserPrompt     WITH FRAME ScanPackageFrame.
         UPDATE  chrScannedPackage WITH FRAME ScanPackageFrame.
         
         chrScannedPackage = TRIM(chrScannedPackage).
         
         /* Cannot be blank */
         IF chrScannedPackage = "" OR
            chrScannedPackage = ? THEN 
         DO:
            NEXT Scan_Block.
         END. /*  IF chrScannedPackage = "" OR chrScannedPackage = ? */
         
         /* Save last scanned value into the Session Last Scan variable for the next programs */
         chrSsnLastScan:setValue(chrScannedPackage).
         
         /* if user scanned COMPLETE */
         IF chrScannedPackage = "COMPLETE" THEN 
         DO:            
            chrResult = "Continue".
            LEAVE Main_Block.
         END. /* IF chrScannedPackage = "COMPLETE" */
         
         /* Check KittingConfig*/
         FIND FIRST KittingConfig NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KittingConfig THEN 
         DO:
            RUN DisplayError("System Error",
                             "No KittingConfig record found.").
            LEAVE Main_Block.                                   
         END. /* NOT AVAILABLE KittingConfig */
         
         FIND FIRST StockPackage NO-LOCK
            WHERE StockPackage.PackageRef = chrScannedPackage NO-ERROR.
         IF NOT AVAILABLE StockPackage THEN
         DO:
            RUN DisplayMessage("Scan Error",
                               "Stock Package " + chrScannedPackage + " does not exist. Scan another Package.").
            NEXT Scan_Block.
         END. /* IF NOT AVAILABLE StockPackage */

         /* Cant be Detrashed */
         IF StockPackage.Detrashed <> "" THEN
         DO:
            RUN DisplayMessage("Invalid Package",
                               "Stock Package " + chrScannedPackage + " was Detrashed.  Scan another Package.").
            NEXT Scan_Block.
         END.
         
         /* Check if its a Child to Parent */
         IF StockPackage.ParentStockPackageID <> 0 THEN 
         DO:
            RUN DisplayMessage("Invalid Package",
                               "Stock Package " + chrScannedPackage + " is already built to Parent.").
            NEXT Scan_Block.
         END.
         
         FIND FIRST KittedUnit NO-LOCK 
            WHERE KittedUnit.StockPackageID = StockPackage.StockPackageID NO-ERROR.
         IF NOT AVAILABLE KittedUnit THEN 
         DO:            
            RUN DisplayMessage("Scan Error",
                               "Stock Package " + chrScannedPackage + " is not from kitting. Scan another Package.").
            NEXT Scan_Block.
         END.
         
         IF StockPackage.StockStatusID <> KittingConfig.PostKittingStockStatusID THEN 
         DO:  
            RUN DisplayMessage("Scan Error",
                               "Stock Status: " + fGetStatusName("Stock", StockPackage.StockStatusID) 
                                  + " does not match KittingConfig PostKittingStockStatus: " 
                                  + fGetStatusName("Stock", KittingConfig.PostKittingStockStatusID)).
            NEXT Scan_Block. 
         END.
         
         intSsnChildPackageID:SetValue(StockPackage.StockPackageID).
         intSsnBuildLocationID:SetValue(StockPackage.LocationID).
         intSsnPartID:SetValue(StockPackage.PartID).
         intSsnWorkOrderID:SetValue(KittedUnit.WorkOrderID).
         
         chrResult = "Continue".
         
         LEAVE Main_Block.
      END. /* Scan Block */
   END. /* Main_Block: */     

   /* Clean Up */
   DELETE OBJECT chrSsnLastScan        NO-ERROR.
   DELETE OBJECT intSsnChildPackageID  NO-ERROR.
   DELETE OBJECT chrSsnUserPrompt      NO-ERROR.
   DELETE OBJECT intSsnPartID          NO-ERROR.
   DELETE OBJECT intSsnBuildLocationID NO-ERROR.
   DELETE OBJECT intSsnWorkOrderID     NO-ERROR.

   /* Release */
   RELEASE StockPackage NO-ERROR.
   RELEASE KittedUnit   NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
