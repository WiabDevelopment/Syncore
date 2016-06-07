/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunKittingParentBuild.p
Purpose : Init program for the Kitting Pallet Build Process

          Possible outcomes : Continue.
          
Author  : BR
Date    : 17/08/2015
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
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Session Objects */      
  
   /* Clear Session */
   fClearSessionValues("").
   
   /* This record the data history Operation Type */
   FIND FIRST OperationType NO-LOCK
      WHERE OperationType.TypeCode = "KittingParentBuild" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN
   DO:
      RUN DisplayError("Record Not Found",
                       "[Operation Type] [KittingParentBuild] does not Exist.").
      
      RETURN.            
   END. /* IF NOT AVAILABLE OperationType THEN */
   
   intGblOperationTypeID = OperationType.OperationTypeID.

   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "No Kitting Config Record found").
         LEAVE Main_Block. 
      END.
      
      /* Check if KittingConfig Location is valid */
      FIND FIRST Location NO-LOCK 
         WHERE Location.LocationID = KittingConfig.PostPalletBuildLocationID NO-ERROR.
      IF NOT AVAILABLE Location THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "KittingConfig PostPalletBuildLocationID: " + STRING(KittingConfig.PostPalletBuildLocationID) + " is invalid.").
         LEAVE Main_Block. 
      END.  
      
      /* Check if KittingConfig StockStatus is valid */
      FIND FIRST StockStatus NO-LOCK 
         WHERE StockStatus.StockStatusID = KittingConfig.PostPalletBuildStockStatusID NO-ERROR.
      IF NOT AVAILABLE Location THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "KittingConfig PostPalletBuildStockStatusID: " + STRING(KittingConfig.PostPalletBuildStockStatusID) + " is invalid.").
         LEAVE Main_Block. 
      END.  
          
      chrResult = "Continue".
      
   END. /* Main_Block */

   /* Clean Up */      
   
   /* Releases */
   RELEASE OperationType NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
