/*------------------------------------------------------------------------------------------------------------------------------------------
Program : bldScanParentForWorkOrder.p
Purpose : Scan a Stock Package and checks whether it Exists or not.
                    
          Possible outcomes : Continue.
          
Author  : BR
Date    : 13th July 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
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
   
   /* Kitting Dependencies */  
   {fncStatusTypeFunctions.i}
   {fncDateFunctions.i}      
   {fncLoggingFunctions.i}     
   {prcKittingProcedures.i}
   {fncKittingFunctions.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Clear Session */
   fClearSessionValue("LastScan").
   fClearSessionValue("VerifyLabelID").
   fClearSessionValue("PackageID").
   
   /* Local Variables */
   DEFINE VARIABLE chrScannedPackage      AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   DEFINE VARIABLE chrUserPrompt          AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   DEFINE VARIABLE logAllowCloseParent    AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE intReturnedWorkOrderID AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE intBuildStatusID       AS INTEGER   NO-UNDO. 
      
   /* Session Objects */
   DEFINE VARIABLE chrSsnLastScan      AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnVerifyLabelID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageID     AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnUserPrompt    AS sessionValue NO-UNDO.
   
   
   /* Frames and UI */
   DEFINE FRAME ScanPackageFrame
      SKIP(4)
      chrUserPrompt     NO-LABEL COLON 1
      chrScannedPackage NO-LABEL COLON 1
      SKIP(6)
   WITH SIDE-LABELS TITLE " Scan Parent Package " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   fTranslateWidget(FRAME ScanPackageFrame:HANDLE).
   
   /* Events */
   ON 'PF4':U ANYWHERE 
   DO:
      chrScannedPackage:SCREEN-VALUE IN FRAME ScanPackageFrame = "COMPLETE".
      RETURN NO-APPLY.
   END. /* ON 'PF4':U ANYWHERE */
   
   /* Create New Session Data */
   chrSsnLastScan      = fNewSessionValue("LastScan").
   intSsnVerifyLabelID = fNewSessionValue("VerifyLabelID").
   intSsnPackageID     = fNewSessionValue("PackageID").
   
   /* Get Status */
   intBuildStatusID  = fGetStatusID("Stock", "Build").

   chrUserPrompt = fCenterOnScreen("Scan Parent Package").
  
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
         
         FIND FIRST StockPackage NO-LOCK
            WHERE StockPackage.PackageRef = chrScannedPackage NO-ERROR.
         IF NOT AVAILABLE StockPackage THEN
         DO:
            RUN DisplayMessage("Scan Error",
                               "Stock Package " + chrScannedPackage + " does not exist. Scan another Package.").
            
            /* repeat scanning until valid Package or COMPLETE */
            NEXT Scan_Block.
         END. /* IF NOT AVAILABLE StockPackage */

         /* Cant be Detrashed */
         IF StockPackage.Detrashed <> "" THEN
         DO:
            RUN DisplayMessage("Invalid Package",
                               "Stock Package " + chrScannedPackage + " was Detrashed.  Scan another Package.").
            
            /* repeat scanning until valid Package or COMPLETE */
            NEXT Scan_Block.
         END.
         
         IF StockPackage.StockStatusID <> intBuildStatusID THEN 
         DO:
            RUN DisplayMessage("Invalid Package",
                               "Stock Package " + chrScannedPackage + " was is not of status Build.  Scan another Package.").
            
            /* repeat scanning until valid Package or COMPLETE */
            NEXT Scan_Block.
         END.
         
         IF StockPackage.ParentStockPackageID <> 0 THEN 
         DO:
            RUN DisplayMessage("Invalid Package",
                               "Stock Package " + chrScannedPackage + " is not a Parent.  Scan another Package.").
            
            /* repeat scanning until valid Package or COMPLETE */
            NEXT Scan_Block.
         END.
         
         /* Run function to get workorderid of this parent */                                
         intReturnedWorkOrderID = fGetWorkOrderIDForPalletBuild(StockPackage.StockPackageID).
         
         IF intReturnedWorkOrderID = 0 OR intReturnedWorkOrderID = ? THEN 
         DO:
            RUN DisplayMessage("Invalid Package",
                               "WorkorderID returned is invalid. Parent scanned is not from Kitting").
             
            NEXT Scan_Block.
         END.
         
         /* Store Parent Stock Package in session so it can be scanned and verified later */ 
         intSsnVerifyLabelID:SetValue(StockPackage.StockPackageID).
         intSsnPackageID:SetValue(StockPackage.StockPackageID). 
         
         chrResult = "Continue".
         
         LEAVE Main_Block.
      END. /* Scan Block */
   END. /* Main_Block: */     

   /* Clean Up */
   DELETE OBJECT chrSsnLastScan      NO-ERROR.
   DELETE OBJECT intSsnVerifyLabelID NO-ERROR.
   DELETE OBJECT intSsnPackageID     NO-ERROR.
   DELETE OBJECT chrSsnUserPrompt    NO-ERROR.

   /* Release */
   RELEASE StockPackage NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
