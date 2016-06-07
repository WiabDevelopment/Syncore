/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCreateParentStockPackage.p 
Purpose : creates a StockPackage record and populates it with Session New Stock Package Values;
          sets Package ID Session value to the New Package ID
          Note: This program is a copy of stkCreatePackage.p in GoPro
          
          Possible Results : Continue
          
Author  : BR
Date    : 26th August 2015
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
   
   /* Optional Includes */
   {fncStatusTypeFunctions.i}
   /* fGetStatusID */
   {fncDateFunctions.i}
   /* fTimeStamp */
   {fncDataFunctions.i}
   /* fGetFieldValue */
   {fncStockFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Local Variables */
   DEFINE VARIABLE chrEntityPrefix        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrPackageRef          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrNowAsTimeStamp      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrNeedsImeiValidation AS CHARACTER NO-UNDO.
   
   /* Session Objects */   
   DEFINE VARIABLE intSsnParentPackageID     AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageID           AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnNewParentPackageRef AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnVerifyLabelID       AS sessionValue NO-UNDO.   
   DEFINE VARIABLE intSsnStockEntityID       AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnTitle               AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnQuestion            AS sessionValue NO-UNDO.
   DEFINE VARIABLE logSsnTempLabel           AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnLabelName           AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnUserPrompt AS sessionValue NO-UNDO.
   
   /* Session Objects to keep values fo the new Stock Package record */
   DEFINE VARIABLE genSsnFieldValue     AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnCreated        AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartID         AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnVendorID       AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnBusinessUnitID AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnReconciled     AS sessionValue NO-UNDO.
   
   /* DB Objects */
   DEFINE VARIABLE newStockPackage     AS newRecord.
   DEFINE VARIABLE newCustStockPackage AS newRecord.
   
   /* Buffers */
   DEFINE BUFFER otherCustDbTableLink FOR CustDbTableLink.
   
   /* Clear Session Values */
   fClearSessionValue("ParentPackageID").
   fClearSessionValue("PackageID").
   fClearSessionValue("NewParentPackageRef").
   fClearSessionValue("VerifyLabelID").
   fClearSessionValue("TempLabel").   
   fClearSessionValue("LabelName").  
   fClearSessionValue("VerifyStockPackageUserPrompt").

   /* Create New Session Data */
   intSsnParentPackageID     = fNewSessionValue("ParentPackageID").
   intSsnPackageID           = fNewSessionValue("PackageID").
   chrSsnNewParentPackageRef = fNewSessionValue("NewParentPackageRef").
   intSsnVerifyLabelID       = fNewSessionValue("VerifyLabelID").
   logSsnTempLabel           = fNewSessionValue("TempLabel").
   chrSsnLabelName           = fNewSessionValue("LabelName").
   chrSsnUserPrompt          = fNewSessionValue("VerifyStockPackageUserPrompt").
   
   /* Get Current Session Data */
   intSsnStockEntityID  = fGetSessionValue("StockPackage.StockEntityID").
   chrSsnCreated        = fGetSessionValue("StockPackage.Created").
   intSsnPartID         = fGetSessionValue("StockPackage.PartID").
   intSsnVendorID       = fGetSessionValue("StockPackage.VendorID").
   intSsnBusinessUnitID = fGetSessionValue("StockPackage.BusinessUnit").
   chrSsnReconciled     = fGetSessionValue("StockPackage.Reconciled").
   
   DEFINE VARIABLE ssnLogTest AS sessionValue NO-UNDO.
   DEFINE VARIABLE logTest AS LOGICAL NO-UNDO.
      
   Main_Block:
   DO ON ERROR UNDO:
      
      /* Create Stock Package */
      newStockPackage = fCreateRecord("StockPackage").
      
      /* Assign Field Values */
      FIND FIRST core._File  NO-LOCK  
         WHERE core._File._File-Name = "StockPackage" NO-ERROR.
      IF AVAILABLE core._File THEN
      DO:      
         FOR EACH core._Field OF core._File NO-LOCK:
            
            genSsnFieldValue = fGetSessionValue(core._File._File-Name + "." + core._Field._Field-Name).
            
            IF genSsnFieldValue:checkExists() THEN 
            DO:
               newStockPackage:assignField(core._Field._Field-Name, genSsnFieldValue).               
               chrError = chrError + newStockPackage:getErrors().

               /* Clean up Data Created */
               fClearSessionValue(core._File._File-Name + "." + core._Field._Field-Name).

            END. /*IF genSsnFieldValue:checkExists() THEN*/
            
         END. /*FOR EACH core._Field OF core._File NO-LOCK:*/
         
         IF chrError <> "" THEN
         DO:
            RUN DisplayError("Stock Package Update Failed",
                             chrError).
            UNDO Main_Block, LEAVE Main_Block.
         END. /*IF chrError <> "" THEN*/
      END. /*IF AVAILABLE core._File THEN*/
      
      /* Get new PackageRef */ 
      chrPackageRef = fNewPackageRef(newStockPackage:NewRecordUniqueID, intSsnStockEntityID:intValue).

      /* Assign more Fields */
      newStockPackage:assignField("StockStatusID", fGetStatusID("Stock", "UnLabelled")).
      newStockPackage:assignField("PackageRef", chrPackageRef).
      newStockPackage:assignField("Created", fTimeStamp(NOW)).
      newStockPackage:assignField("VendorID", intSsnVendorID).
      newStockPackage:assignField("PartID", intSsnPartID).
      
      /* If Created field is loaded in session then overide it (i.e. comes from a split) */
      IF chrSsnCreated:chrValue <> "" THEN
         newStockPackage:assignField("Created", chrSsnCreated:chrValue).
      
      /* Error Check */
      chrError = chrError + newStockPackage:getErrors().
      
      IF chrError <> "" THEN
      DO:
         RUN DisplayError("Update Failed",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END. /*IF chrError <> "" THEN*/
      
      /* These records indicate whether we shoud create a Cust* version of each record */
      FIND FIRST CustDbTableLink NO-LOCK 
         WHERE CustDbTableLink.CoreTableName     = "StockPackage" 
         AND   CustDbTableLink.CreateCustRecords = YES NO-ERROR.
         
      IF AVAILABLE CustDbTableLink THEN 
      DO:
         /* Create Cust Stock Package */
         newCustStockPackage = fCreateCustRecord("CustStockPackage", newStockPackage:NewRecordUniqueID).
         
         newCustStockPackage:assignField("CustStockPackageID", newStockPackage:NewRecordUniqueID).
         
         /* Dynamically get the NeedsImeiValidation value from the CustPart table, if the table AND field exist */
         chrNeedsImeiValidation = fGetCustFieldValue("NeedsImeiValidation", "Part", intSsnPartID:intValue).
         
         /* This means the CustPart table AND NeedsImeiValidation field EXIST */
         IF chrNeedsImeiValidation = "NO" THEN 
         DO:
            chrNowAsTimeStamp = fTimeStamp(NOW).
            newCustStockPackage:assignField("ImeiValidationStarted",   chrNowAsTimeStamp).
            newCustStockPackage:assignField("ImeiValidationCompleted", chrNowAsTimeStamp).
         END. /*IF chrNeedsImeiValidation = "NO" THEN*/
         
         /* Error Check */
         chrError = chrError + newCustStockPackage:getErrors().
         
         IF chrError <> "" THEN
         DO:
            RUN DisplayError("Update Failed",
                              chrError).
            UNDO Main_Block, LEAVE Main_Block.
         END. /*IF chrError <> "" THEN*/
         
      END. /*IF AVAILABLE CustDbTableLink THEN*/
      
      /* Set session values */
      intSsnVerifyLabelID:setValue(newStockPackage:NewRecordUniqueID).
      intSsnParentPackageID:setValue(newStockPackage:NewRecordUniqueID).
      intSsnPackageID:setValue(newStockPackage:NewRecordUniqueID). /* Set Session for Print program */
      chrSsnNewParentPackageRef:setValue(chrPackageRef).
      logSsnTempLabel:setValue(TRUE).
      chrSsnLabelName:setValue("StockPackage").
      chrSsnUserPrompt:setValue("Scan Parent Temp Label").
      
      /* Update Sucessfully Completed */
      chrResult = "Continue".
      
   END. /*Main_Block:*/
   
   /* Clean Up */  
   DELETE OBJECT intSsnParentPackageID     NO-ERROR.
   DELETE OBJECT intSsnPackageID           NO-ERROR.
   DELETE OBJECT chrSsnNewParentPackageRef NO-ERROR.
   DELETE OBJECT intSsnVerifyLabelID       NO-ERROR.
   DELETE OBJECT intSsnStockEntityID       NO-ERROR.
   DELETE OBJECT genSsnFieldValue          NO-ERROR.
   DELETE OBJECT chrSsnCreated             NO-ERROR.   
   DELETE OBJECT newStockPackage           NO-ERROR.
   DELETE OBJECT newCustStockPackage       NO-ERROR. 
   DELETE OBJECT chrSsnQuestion            NO-ERROR.
   DELETE OBJECT chrSsnTitle               NO-ERROR.
   DELETE OBJECT intSsnPartID              NO-ERROR.
   DELETE OBJECT intSsnVendorID            NO-ERROR.
   DELETE OBJECT intSsnBusinessUnitID      NO-ERROR.
   DELETE OBJECT chrSsnReconciled          NO-ERROR.
   DELETE OBJECT logSsnTempLabel           NO-ERROR.
   DELETE OBJECT chrSsnLabelName           NO-ERROR.
   DELETE OBJECT chrSsnUserPrompt          NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END.  /* CTRL-C Catch */