/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitScanParentStockPackage.p
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
   
   /* Local Variables */
   DEFINE VARIABLE chrScannedPackage      AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   DEFINE VARIABLE chrUserPrompt          AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   DEFINE VARIABLE chrPackageToScan       AS CHARACTER NO-UNDO FORMAT "x(24)" VIEW-AS FILL-IN NATIVE.
   DEFINE VARIABLE intReturnedWorkOrderID AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE chrOpenPalletRefs      AS CHARACTER NO-UNDO. 
   
   /* Temp-tables */ 
   DEFINE TEMP-TABLE ttOpenPallets
      FIELD OpenPalletID     AS INTEGER
      FIELD OpenPalletRef    AS CHARACTER.
     
   /* Session Objects */
   DEFINE VARIABLE chrSsnLastScan        AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnParentPackageID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartID          AS sessionValue NO-UNDO.
         
   /* Buffer */
   DEFINE BUFFER otherStockPackage FOR Stockpackage.
   DEFINE BUFFER parentStockPackage FOR Stockpackage.
   
  
   /* Frames and UI */
   DEFINE FRAME ScanPackageFrame
      SKIP(4)
      chrUserPrompt     NO-LABEL COLON 1
      chrPackageToScan  NO-LABEL COLON 1
      chrScannedPackage NO-LABEL COLON 1
      SKIP(5)
   WITH SIDE-LABELS TITLE " Scan Parent Package " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   fTranslateWidget(FRAME ScanPackageFrame:HANDLE).
   
   /* Events */
   ON 'PF4':U ANYWHERE 
   DO:
      chrScannedPackage:SCREEN-VALUE IN FRAME ScanPackageFrame = "COMPLETE".
      RETURN NO-APPLY.
   END. /* ON 'PF4':U ANYWHERE */
   
   /* Get Session Values */
   intSsnParentPackageID = fGetSessionValue("ParentPackageID").
   intSsnPartID          = fGetSessionValue("PartID").
   
   /* Create New Session Data */
   chrSsnLastScan  = fNewSessionValue("LastScan").
      
   chrUserPrompt = fCenterOnScreen("Scan Suggested Parent ").
   

   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      /* Check KittingConfig*/
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN 
      DO:
         RUN DisplayError("System Error",
                          "No KittingConfig record found.").
         LEAVE Main_Block.                                   
      END. /* NOT AVAILABLE KittingConfig */
      
      /* Get parent package in session*/
      FIND FIRST StockPackage NO-LOCK 
         WHERE Stockpackage.StockPackageID = intSsnParentPackageID:intValue.
      IF NOT AVAILABLE StockPackage THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "Package ID " + STRING(intSsnParentPackageID:intValue) + " does not Exist.").
         LEAVE Main_Block.        
      END.
      
      chrPackageToScan = fCenterOnScreen(Stockpackage.PackageRef).
                  
      FOR EACH ParentBuild NO-LOCK 
         WHERE ParentBuild.Complete = "":
            
         FIND FIRST parentStockPackage OF ParentBuild NO-LOCK 
            WHERE parentStockPackage.PartID    = intSsnPartID:intValue 
            AND   parentStockPackage.Detrashed = "" NO-ERROR.
         IF AVAILABLE parentStockPackage THEN
         DO:                          
            CREATE ttOpenPallets.
            ASSIGN 
               ttOpenPallets.OpenPalletID     = parentStockPackage.StockPackageID
               ttOpenPallets.OpenPalletRef    = parentStockPackage.PackageRef.
            
         END. /* IF AVAILABLE StockPackage */           
      END. /* EACH ParentBuild */
      
      /* Append the open pallet refs to string to be used for validation error */
      FOR EACH ttOpenPallets NO-LOCK:
         chrOpenPalletRefs = chrOpenPalletRefs + " " + ttOpenPallets.OpenPalletRef.
      END.
      
      Scan_Block:
      REPEAT ON STOP UNDO Scan_Block, RETRY Scan_Block:
         
         /* Clear UI Variable */
         chrScannedPackage = "".
         
         /* Get scanned value from GUN */
         DISPLAY chrUserPrompt  chrPackageToScan  WITH FRAME ScanPackageFrame.
         UPDATE  chrScannedPackage WITH FRAME ScanPackageFrame.
                           
         /* Cannot be blank */
         IF chrScannedPackage = "" OR
            chrScannedPackage = ? THEN 
         DO:
            NEXT Scan_Block.
         END. /*  IF chrScannedPackage = "" OR chrScannedPackage = ? */
         
         /* Save last scanned value into the Session Last Scan variable for the next programs */
         chrSsnLastScan:setValue(chrScannedPackage).
         
         /* If user scanned COMPLETE */
         IF chrScannedPackage = "COMPLETE" THEN 
         DO:            
            chrResult = "Continue".
            LEAVE Main_Block.
         END. /* IF chrScannedPackage = "COMPLETE" */
         
         FIND FIRST otherStockPackage NO-LOCK
            WHERE otherStockPackage.PackageRef = chrScannedPackage NO-ERROR.
         IF NOT AVAILABLE otherStockPackage THEN
         DO:
            RUN DisplayMessage("Scan Error",
                               "Stock Package " + chrScannedPackage + " does not exist. Scan another Package.").
            NEXT Scan_Block.
         END. /* IF NOT AVAILABLE StockPackage */
         
         /* Larger Scan criteria only if AllowMixedWorkOrdersOnOnePallet = YES */
         IF KittingConfig.AllowMixedWorkOrdersOnOnePallet = YES THEN
         DO:
            /* Check if scanned parent exist in the list of open pallets */
            FIND FIRST ttOpenPallets NO-LOCK 
               WHERE ttOpenPallets.OpenPalletID = otherStockPackage.StockPackageID NO-ERROR.  
            IF NOT AVAILABLE ttOpenPallets THEN 
            DO:
               RUN DisplayMessage("Invalid Package",
                                  "Please scan parent package: " + chrOpenPalletRefs).
               NEXT Scan_Block.
            END. /* NOT AVAILABLE ttOpenPallets */
         END. /* AllowMixedWorkOrdersOnOnePallet = YES */
         ELSE IF KittingConfig.AllowMixedWorkOrdersOnOnePallet = NO THEN
         DO:
            IF otherStockPackage.StockPackageID <> StockPackage.StockPackageID THEN 
            DO:
               RUN DisplayMessage("Invalid Package",
                                  "Please scan parent package: " + StockPackage.PackageRef).
               NEXT Scan_Block.
            END.
         END. /* AllowMixedWorkOrdersOnOnePallet = NO */
          
         /* Cant be Detrashed */
         IF otherStockPackage.Detrashed <> "" THEN
         DO:
            RUN DisplayMessage("Invalid Package",
                               "Stock Package " + chrScannedPackage + " was Detrashed.  Scan another Package.").
            NEXT Scan_Block.
         END.
                           
         IF otherStockPackage.ParentStockPackageID <> 0 THEN 
         DO:
            RUN DisplayMessage("Invalid Package",
                               "Stock Package " + chrScannedPackage + " is not a Parent.  Scan another Package.").
            NEXT Scan_Block.
         END.
         

         /* Re-Set Parent PackageID */
         intSsnParentPackageID:setValue(otherStockPackage.StockPackageID).
                                            
         chrResult = "Continue".
         
         LEAVE Main_Block.
      END. /* Scan Block */
   END. /* Main_Block: */     

   /* Clean Up */
   DELETE OBJECT chrSsnLastScan        NO-ERROR.
   DELETE OBJECT intSsnParentPackageID NO-ERROR.
   DELETE OBJECT intSsnPartID          NO-ERROR.

   /* Release */
   RELEASE StockPackage  NO-ERROR.
   RELEASE ParentBuild   NO-ERROR.
   RELEASE KittingConfig NO-ERROR.
  
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
