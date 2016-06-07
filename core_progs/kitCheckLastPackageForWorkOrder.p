/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckLastPackageForWorkOrder.p
Purpose : Check the if the last package for WorkOrder has been reached.
          
          Possible results: Yes, No
          
Author  : BR
Date    : 21st August 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
14/09/2015 ML  GoProTOA   Added logic to update WorkOrderStatus and StockPackageStatus at end of Pallet Build.
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
   
   /* Optional Includes */
   {fncStatusTypeFunctions.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i} 
   
   /* Session Objects */      
   DEFINE VARIABLE intSsnChildPackageID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnWorkOrderID    AS sessionValue NO-UNDO.
   
   /* Db ojects */
   DEFINE VARIABLE updWorkOrder         AS updRecord    NO-UNDO.
   
   /* Get Current Session Data */
   intSsnChildPackageID = fGetSessionValue("ChildPackageID").
   intSsnWorkOrderID    = fGetSessionValue("WorkOrderID").
   
   Main_Block:
   DO ON ERROR UNDO:
      
      /* Check if the flag for build by workorder is true */
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "No Kitting config record exist.").
         LEAVE Main_Block.
      END.
      
      IF KittingConfig.AllowMixedWorkOrdersOnOnePallet = NO THEN 
      DO:
         
         /* Check Status of WorkOrder. If still open then do not close pallet*/
         FIND FIRST WorkOrder NO-LOCK 
            WHERE WorkOrder.WorkOrderID = intSsnWorkOrderID:intValue NO-ERROR.
         IF NOT AVAILABLE WorkOrder THEN 
         DO:
            RUN DisplayError("Record Not Found",
                             "WorkOrder record not found for WorkOrderID: " + STRING(intSsnWorkOrderID:intValue)).
            LEAVE Main_Block.
         END.
         
         IF WorkOrder.Completed = "" THEN 
         DO:
            chrResult = "No".
               
            LEAVE Main_Block.
         END.   
         
         /* Get the list of all child stockpackages thats linked to the session workorder */    
         FOR EACH KittedUnit NO-LOCK 
            WHERE KittedUnit.WorkOrderID = intSsnWorkOrderID:intValue :
            
            FIND FIRST StockPackage NO-LOCK 
               WHERE StockPackage.StockPackageID = KittedUnit.StockPackageID NO-ERROR.
            IF NOT AVAILABLE StockPackage THEN 
            DO:
               RUN DisplayError("Record Not Found",
                                "Stockpackage record not found for PackageID ." + STRING(KittedUnit.StockPackageID)).
               LEAVE Main_Block.
            END.
            
            /* If any child stock package is not built to parent then prompt to next scan else close parent */
            IF StockPackage.ParentStockPackageID = 0 THEN 
            DO:
               chrResult = "No".
               
               LEAVE Main_Block.
               
            END. /* ParentStockPackageID = 0 */   
         END. /* FOR EACH KittedUnit */
      END. /* AllowMixedWorkOrdersOnOnePallet = NO */
      
      /* The last StockPackage was pallet built. Update WorkOrderStatus and StockPackageStatus */
      updWorkOrder = fGetRecord("WorkOrder", intSsnWorkOrderID:intValue).
      updWorkOrder:assignField("WorkOrderStatusID", fGetStatusID("WorkOrder", "BeingPutaway")).
      
      /* Error Check for updWorkOrder */
      chrError = chrError + updWorkOrder:getErrors().
      
      /* No unbuilt child found Proceed to close parent*/
      chrResult = "Yes".
            
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnChildPackageID NO-ERROR.
   DELETE OBJECT intSsnWorkOrderID    NO-ERROR.
   
   /* Releases */
   RELEASE KittedUnit    NO-ERROR.
   RELEASE StockPackage  NO-ERROR.
   RELEASE KittingConfig NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END.  /* CTRL-C Catch */
