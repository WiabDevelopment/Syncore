/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckAnyOpenParentForWorkOrder.p 
Purpose : Checks whether open pallet exits for workorder.
Possible Results : Yes/No

Author  : BR
Date    : 18th Aug 2015
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
   
   /* Buffer */ 
   DEFINE BUFFER parentStockPackage FOR StockPackage.
   DEFINE BUFFER childStockPackage  FOR StockPackage.
   DEFINE BUFFER otherkittedUnit    FOR KittedUnit.

   /* Session Objects */   
   DEFINE VARIABLE intSsnChildPackageID        AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnParentPackageID       AS sessionValue NO-UNDO.
    
   /* Clear Session Values */
   fClearSessionValue("ParentPackageID").
      
   /* Create New Session Data */
   intSsnParentPackageID       = fNewSessionValue("ParentPackageID").

   /* Get Current Session Data */
   intSsnChildPackageID = fGetSessionValue("ChildPackageID").
   

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
         WHERE StockPackage.StockPackageID = intSsnChildPackageID:intValue
         AND   StockPackage.Detrashed = "" NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Package ID " + STRING(intSsnChildPackageID:intValue) + " does not Exist.").
         LEAVE Main_Block.
      END.

      /* KittedUnit record should be available for Kitted stockpackages*/
      FIND FIRST KittedUnit NO-LOCK
         WHERE KittedUnit.StockPackageID = StockPackage.StockPackageID NO-ERROR.
      IF NOT AVAILABLE KittedUnit THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Kitted unit doesnot exist for Package ID " + STRING(intSsnChildPackageID:intValue)).
         LEAVE Main_Block.
      END.            
            
      KittedUnit_Block:
      FOR EACH  otherkittedUnit NO-LOCK 
         WHERE otherkittedUnit.WorkOrderID    =  KittedUnit.WorkOrderID :
           
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
               AND   parentStockPackage.StockPackageID       = childStockPackage.ParentStockPackageID
               AND   parentStockPackage.Detrashed            = "" NO-ERROR.
            IF AVAILABLE  parentStockPackage THEN   
            DO:
               /* Check if the parent is open */
               FIND FIRST ParentBuild NO-LOCK 
                  WHERE ParentBuild.StockPackageID = parentStockPackage.StockPackageID
                  AND   ParentBuild.Complete       = "" NO-ERROR.
               IF AVAILABLE ParentBuild THEN 
               DO:
                  LEAVE KittedUnit_Block.             
               END. /* AVAILABLE ParentBuild */
                                         
            END. /* AVAILABLE  parentStockPackage */            
         END.  /* AVAILABLE childStockPackage */             
      END. /* KittedUnit_Block */     
           
      IF AVAILABLE ParentBuild THEN 
      DO:     
         
         /* Run function to get workorderid of this parent to validate against AllowMultiple flag */                                
         intReturnedWorkOrderID = fGetWorkOrderIDForPalletBuild(ParentBuild.StockPackageID).
                 
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
                              
         /* Store Parent Stock Package and in session so it can be used later */   
         intSsnParentPackageID:setValue(ParentBuild.StockPackageID). 
                          
         chrResult = "Yes".
         
         LEAVE Main_Block.
         
      END. /* AVAILABLE parentStockPackage */
           
      /* No Parent found Found, proceed creating a new parent*/
      chrResult = "No".

   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnChildPackageID        NO-ERROR.
   DELETE OBJECT intSsnParentPackageID       NO-ERROR.
   
   /* Releases */
   RELEASE StockPackage        NO-ERROR.
   RELEASE KittedUnit          NO-ERROR.
   RELEASE ParentBuild         NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
