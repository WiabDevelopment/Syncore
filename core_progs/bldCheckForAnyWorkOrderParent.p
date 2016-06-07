/*------------------------------------------------------------------------------------------------------------------------------------------
Program : bldCheckForAnyWorkOrderParent.p 
Purpose : Checks whether StockPackage is from Kitting

Author  : BR
Date    : 8th July 2015
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
   
   /* Local Variables */   
   DEFINE VARIABLE intReturnedWorkOrderID AS INTEGER NO-UNDO.
   DEFINE VARIABLE intBuildStatusID       AS INTEGER NO-UNDO. 
   
   /* Buffer */ 
   DEFINE BUFFER parentStockPackage FOR StockPackage.
   DEFINE BUFFER childStockPackage  FOR StockPackage.
   DEFINE BUFFER otherkittedUnit    FOR KittedUnit.

   /* Session Objects */   
   DEFINE VARIABLE intSsnStockPackageID AS sessionValue NO-UNDO.

   /* Get Current Session Data */
   intSsnStockPackageID = fGetSessionValue("PackageID").
   
   /* Get Status */
   intBuildStatusID  = fGetStatusID("Stock", "Build").

   Main_Block:
   DO ON ERROR UNDO:
    
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "No Kitting config record exist.").
         LEAVE Main_Block.
      END.
      
      FIND FIRST StockPackage NO-LOCK
         WHERE StockPackage.StockPackageID = intSsnStockPackageID:intValue
         AND   StockPackage.Detrashed = "" NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Package ID " + STRING(intSsnStockPackageID:intValue) + " does not Exist.").
         LEAVE Main_Block.
      END.

      /* KittedUnit record should be available for Kitted stockpackages*/
      FIND FIRST KittedUnit NO-LOCK
         WHERE KittedUnit.StockPackageID = StockPackage.StockPackageID NO-ERROR.
      IF NOT AVAILABLE KittedUnit THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Kitted unit doesnot exist for Package ID " + STRING(intSsnStockPackageID:intValue)).
         LEAVE Main_Block.
      END.            
      
      /* Check if parent exist in status build with any workorderid */
      KittedUnit_block:
      FOR EACH otherkittedUnit NO-LOCK 
         WHERE otherkittedUnit.StockPackageID <> StockPackage.StockPackageID :
         
            
         /* Find the child StockPackage record */
         FIND FIRST childStockPackage NO-LOCK 
            WHERE childStockPackage.StockPackageID       = otherkittedUnit.StockPackageID 
            AND   childStockPackage.ParentStockPackageID <> 0 
            AND   childStockPackage.Detrashed            = "" NO-ERROR.
         IF AVAILABLE childStockPackage THEN 
         DO:   
            /* Find the Parent of this other Kitted unit Package */
            FIND FIRST parentStockPackage NO-LOCK 
               WHERE parentStockPackage.ParentStockPackageID = 0
               AND   parentStockPackage.StockPackageID = childStockPackage.ParentStockPackageID
               AND   parentStockPackage.StockStatusID  = intBuildStatusID
               AND   parentStockPackage.Detrashed = "" NO-ERROR. 
            IF AVAILABLE  parentStockPackage THEN   
            DO:              
               LEAVE KittedUnit_block.
            END.
          END.  /* AVAILABLE childStockPackage */ 
      END. /* KittedUnit_block */
      
      IF AVAILABLE parentStockPackage THEN 
      DO:        
         /* Run function to get workorderid of this parent to validate against AllowMultiple flag */                                
         intReturnedWorkOrderID = fGetWorkOrderIDForPalletBuild(parentStockPackage.StockPackageID).
               
         IF intReturnedWorkOrderID = 0 OR intReturnedWorkOrderID = ? THEN 
         DO:
            RUN DisplayError("System Error",
                             "WorkorderID returned is invalid").
            LEAVE Main_Block.
         END.
               
         /* Validation to ensure only same Workorderid packages are loaded to pallet */
         IF intReturnedWorkOrderID <> KittedUnit.WorkOrderID AND KittingConfig.AllowMixedWorkOrdersOnOnePallet = NO THEN 
         DO:
            RUN DisplayError("System Error",
                             "WorkorderID: " + STRING(intReturnedWorkOrderID) + " returned is different from Scanned StockPackage").
            LEAVE Main_Block.                  
         END.
         
         chrResult = "Yes". /* Parent found. Proceed to scan parent */    
               
         LEAVE Main_Block.
         
      END. /* AVAILABLE parentStockPackage */
   
      /* No Parent found Found, proceed creating a new parent*/
      chrResult = "No".

   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnStockPackageID NO-ERROR.
   
   /* Releases */
   RELEASE StockPackage NO-ERROR.
   RELEASE KittedUnit   NO-ERROR.
    

   /* Map Result Debugging */
   {prcProcessDebugging.i}


END. /* CTRL-C Catch */
