/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitReachedParentMaxQty.p
Purpose : Check the if the Max Qty of the prarent has been reached.
          
          Possible results: Yes, No
          
Author  : BR
Date    : 21st August 2015
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
   DEFINE VARIABLE intSsnParentPackageID AS sessionValue NO-UNDO.
   
   /* Get Current Session Data */
   intSsnParentPackageID = fGetSessionValue("ParentPackageID").
   
   Main_Block:
   DO ON ERROR UNDO:
      
      /* Validate the Session Data */
      FIND FIRST StockPackage NO-LOCK
         WHERE StockPackage.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "StockPackage ID [" + STRING(intSsnParentPackageID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.
      
      FIND Part OF StockPackage NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Part THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Part ID [" + STRING(StockPackage.PartID) + "] does not Exist.").
         LEAVE Main_Block.
      END.
      
      /* Get the Stock Entity of the Stock Package*/
      FIND FIRST StockEntity NO-LOCK
         WHERE StockEntity.StockEntityID = StockPackage.StockEntityID NO-ERROR.

      /* Get the PartStockEntity Link for the MaxQty */
      FIND FIRST PartStockEntityLink NO-LOCK
         WHERE PartStockEntityLink.PartID        = StockPackage.PartID
         AND   PartStockEntityLink.StockEntityID = StockPackage.StockEntityID NO-ERROR.
      IF NOT AVAILABLE PartStockEntityLink THEN
      DO:        
         IF AVAILABLE StockEntity THEN
         DO:
            RUN DisplayError("Record Not Found",
                             "StockEntity [" + StockEntity.EntityCode + "] for Part [" + Part.PartRef + "] does not exist.").
         END.
         ELSE 
         DO:
            RUN DisplayError("Record Not Found",
                             "StockEntity ID [" + STRING(StockPackage.StockEntityID) + "] does not exist.").
         END.
         
         LEAVE Main_Block.
      END.
      
      /* Validate the Qty is doesn NOT Breach the Max Qty */
      IF PartStockEntityLink.MaxQty <= StockPackage.PackageQty AND NOT PartStockEntityLink.AllowOverBuild THEN
      DO:
         RUN DisplayMessage("Max Qty Reached",
                            "Max Qty for Part [" + Part.PartRef + "] and Package Type ["
                               + StockEntity.EntityName + "] is [" + STRING(PartStockEntityLink.MaxQty) + "]. Closing [" + StockEntity.EntityName + "].").
         chrResult = "Yes".
         LEAVE Main_Block.
      END.
      
      IF PartStockEntityLink.MaxQty <= StockPackage.PackageQty AND PartStockEntityLink.AllowOverBuild THEN
      DO:

         RUN DisplayConfirm("Max Qty",
                            "Max Qty for Part [" + Part.PartRef + "] and Package Type ["
                              + StockEntity.EntityName + "] is [" + STRING(PartStockEntityLink.MaxQty) + "]. Do you want Close [" 
                              + StockEntity.EntityName + "] [" + StockPackage.PackageRef + "]?").
         IF logMessageResult THEN
         DO:         
            /* Close */
            chrResult = "Yes".
         END.
         ELSE DO:
            /* Leave Open */
            chrResult = "No".
         END.

         LEAVE Main_Block.
      END.

      /* Max Qty was not reached */
      chrResult = "No".
      
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnParentPackageID NO-ERROR.
   
   /* Releases */
   RELEASE StockPackage        NO-ERROR.
   RELEASE PartStockEntityLink NO-ERROR.
   RELEASE StockEntity         NO-ERROR.
   RELEASE Part                NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END.  /* CTRL-C Catch */
