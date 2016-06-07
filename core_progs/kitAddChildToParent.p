/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitAddChildToParent.p 
Purpose : Adds a stock package to a parent stock package as part of the Kitting Pallet Building Process
          Updates Child Parent Package ID 
          Increments Parent Package Qty with the Child Package Qty
          Updats Parent Package LocationID with the Child Package Location ID 
          Updates all Children of the Parent Package with the Current Child LocationID
          
          Possible results: Continue
          
Author  : BR
Date    : 21st August 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
14/09/2015 ML  GoProTOA   Added logic to update WorkOrderStatus at initial Being Pallet Built.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character Parameter Include */
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
   
   /* Session Objects */
   DEFINE VARIABLE intSsnParentPackageID   AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnChildPackageID    AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnLabelType         AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnUserPrompt        AS sessionValue NO-UNDO.
   DEFINE VARIABLE logSsnTempLabel         AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnLabelName         AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnVerifyLabelID     AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageID         AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnWorkOrderID       AS sessionValue NO-UNDO. 
   
   /* DB Objects */
   DEFINE VARIABLE updchildStockPackage  AS updRecord NO-UNDO.
   DEFINE VARIABLE updparentStockPackage AS updRecord NO-UNDO.
   DEFINE VARIABLE updStockPackage       AS updRecord NO-UNDO.
   DEFINE VARIABLE updParentBuild        AS updRecord NO-UNDO.
   DEFINE VARIABLE updWorkOrder          AS updRecord NO-UNDO.
   
   /* Local Buffers */
   DEFINE BUFFER childStockPackage       FOR StockPackage.
   DEFINE BUFFER parentStockPackage      FOR StockPackage.
   
   /* Clear Session Data */
   fClearSessionValue("PrintLabelType").   
   fClearSessionValue("VerifyStockPackageUserPrompt").
   fClearSessionValue("VerifyLabelID").
   fClearSessionValue("PackageID").
   
   /* Create New Session Data */
   chrSsnLabelType         = fNewSessionValue("PrintLabelType").   
   chrSsnUserPrompt        = fNewSessionValue("VerifyStockPackageUserPrompt").
   logSsnTempLabel         = fNewSessionValue("TempLabel").
   chrSsnLabelName         = fNewSessionValue("LabelName").
   intSsnVerifyLabelID     = fNewSessionValue("VerifyLabelID").
   intSsnPackageID         = fNewSessionValue("PackageID").
   
   /* Get Current Session Data */ 
   intSsnParentPackageID  = fGetSessionValue("ParentPackageID").      
   intSsnChildPackageID   = fGetSessionValue("ChildPackageID").
   intSsnWorkOrderID      = fGetSessionValue("WorkOrderID").    
   
   logSsnTempLabel:setValue(FALSE).
   chrSsnLabelName:setValue("StockPackage").
   
   Main_Block:
   DO ON ERROR UNDO:
      
      /* Get the Child */
      FIND FIRST childStockPackage NO-LOCK
         WHERE childStockPackage.StockPackageID = intSsnChildPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE childStockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "StockPackage ID [" + STRING(intSsnChildPackageID:intValue) + "] does not Exist.").       
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      /* Check if another user built this child to a parent */
      IF childStockPackage.ParentStockPackageID <> 0 THEN 
      DO:
          RUN DisplayError("System Error",
                           "StockPackage [" + childStockPackage.PackageRef + "] is already built. Cannot continue.").       
          UNDO Main_Block, LEAVE Main_Block.
      END.
      
      /* Find WorkOrder for WorkOrderStatus Update */
      FIND FIRST WorkOrder NO-LOCK 
         WHERE WorkOrder.WorkOrderID = intSsnWorkOrderID:intValue NO-ERROR.
      IF NOT AVAILABLE WorkOrder THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "WorkOrder for StockPackage ID [" + STRING(childStockPackage.StockPackageID) + "] does not Exist.").       
         UNDO Main_Block, LEAVE Main_Block.
      END. /* FIND FIRST WorkOrder NO-LOCK */  
             
      /* Get the Parent */
      FIND FIRST parentStockPackage NO-LOCK
         WHERE parentStockPackage.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE parentStockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "StockPackage ID [" + STRING(intSsnParentPackageID:intValue) + "] does not Exist.").       
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      FIND FIRST ParentBuild NO-LOCK 
         WHERE ParentBuild.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE ParentBuild THEN 
      DO:
         RUN DisplayError("System error",
                          "ParentBuild record doesnot exist for PackageID: " + STRING(intSsnParentPackageID:intValue)).                
         LEAVE Main_Block.                 
      END.
      
      updParentBuild = fGetRecord("ParentBuild", ParentBuild.ParentBuildID).
      
      /* Update ParentBuild record */
      updParentBuild:assignField("StockStatusID", fGetStatusID("Stock", "Build")).
      
      /* Error Check for updparentStockPackage */
      chrError = chrError + updParentBuild:getErrors().  
        
      updparentStockPackage = fGetRecord("StockPackage", parentStockPackage.StockPackageID).
      
      /* Increase the Parent Qty's and Set Status to Build */
      updparentStockPackage:assignField("PackageQty", parentStockPackage.PackageQty + childStockPackage.PackageQty).
      updparentStockPackage:assignField("OriginalQty", parentStockPackage.OriginalQty + childStockPackage.PackageQty).
      updparentStockPackage:assignField("LocationID", childStockPackage.LocationID).
      updparentStockPackage:assignField("LocationID", childStockPackage.LocationID).
      updparentStockPackage:assignField("StockStatusID", fGetStatusID("Stock", "Build")). 

      /* Error Check for updparentStockPackage */
      chrError = chrError + updparentStockPackage:getErrors().  
      
      updchildStockPackage = fGetRecord("StockPackage", childStockPackage.StockPackageID).
      updchildStockPackage:assignField("ParentStockPackageID", parentStockPackage.StockPackageID).
      updchildStockPackage:assignField("StockStatusID", fGetStatusID("Stock", "Build")). 
            
      /* Error Check for updchildStockPackage */
      chrError = chrError + updchildStockPackage:getErrors().
      
      /* if W/O is complete then update W/O Status to “BeingPalletBuilt” Else leave it the same*/
      IF WorkOrder.Completed <> "" THEN 
      DO:
         /* Update WorkOrderStatus */
         updWorkOrder = fGetRecord("WorkOrder", WorkOrder.WorkOrderID).
         updWorkOrder:assignField("WorkOrderStatusID", fGetStatusID("WorkOrder", "BeingPalletBuilt")).
         
         /* Error Check for updWorkOrder */
         chrError = chrError + updWorkOrder:getErrors().
         
      END. /* IF WorkOrder.Completed */
      
      /* Updates all Children of the Parent Package with the Current Child LocationID */
      FOR EACH StockPackage NO-LOCK
         WHERE StockPackage.parentStockPackageID = intSsnParentPackageID:intValue:
         
         updStockPackage = fGetRecord("StockPackage", StockPackage.StockPackageID).
         updStockPackage:assignField("LocationID", childStockPackage.LocationID).
         
         /* Error Check for updStockPackage */
         chrError = chrError + updStockPackage:getErrors().  
         
         DELETE OBJECT updStockPackage NO-ERROR.
      END.
      
      /* Check Errors */
      IF chrError <> "" THEN DO:
         RUN DisplayError("Update Error",
                           chrError).       
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      /* Set Label to Print Final */
      chrSsnLabelType:setValue("RECEIPT").
      chrSsnUserPrompt:setValue("Scan Parent Final Label").
      
      /* Set the parent in session so it can be verified later */
      intSsnVerifyLabelID:setValue(parentStockPackage.StockPackageID).
      intSsnPackageID:setValue(parentStockPackage.StockPackageID).
      
      chrResult = "Continue".
      
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnChildPackageID    NO-ERROR.
   DELETE OBJECT intSsnParentPackageID   NO-ERROR.
   DELETE OBJECT intSsnPackageID         NO-ERROR.
   DELETE OBJECT chrSsnLabelType         NO-ERROR.
   DELETE OBJECT chrSsnUserPrompt        NO-ERROR.
   DELETE OBJECT updchildStockPackage    NO-ERROR.
   DELETE OBJECT updparentStockPackage   NO-ERROR.
   DELETE OBJECT updParentBuild          NO-ERROR.
   DELETE OBJECT logSsnTempLabel         NO-ERROR.
   DELETE OBJECT chrSsnLabelName         NO-ERROR.

   /* Releases */
   RELEASE childStockPackage  NO-ERROR.
   RELEASE parentStockPackage NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */

