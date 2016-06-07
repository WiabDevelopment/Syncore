/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckOpenParentExistsForPart.p
Purpose : Checks if an open Parent for part exist

          Possible Results : Yes, No.

Author  : BR 
Date    : 17th Aug 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/11/2015 CS  CanonTlb   Commented out a logic check because was having an issues with multiple Pallet Builds.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:

   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}
   
   /* Optional includes*/   
   {fncDataFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Session Objects */
   DEFINE VARIABLE intSsnParentPackageID       AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartID                AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnStockPartID           AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnStockReconciled       AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnChildPackageID        AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageStockStatusID  AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnParentEntityID        AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnStockTaskUserLinkID   AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnStockLocationID       AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnStockStatusID         AS sessionValue NO-UNDO.      
   DEFINE VARIABLE intSsnStockInboundID        AS sessionValue NO-UNDO.   
   DEFINE VARIABLE intSsnStockOwnerID          AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnStockVendorID         AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnStockBusinessUnitID   AS sessionValue NO-UNDO.   
   
   /* Temp-tables */ 
   DEFINE TEMP-TABLE ttOpenPallets
      FIELD OpenPalletID     AS INTEGER
      FIELD OpenPalletUserID AS INTEGER .
   
   /* Clear Session */
   fClearSessionValue("PackageStockStatusID").
   fClearSessionValue("StockPackage.PartID").
   fClearSessionValue("StockPackage.StockEntityID").
   fClearSessionValue("StockPackage.TaskLineWorkUserLinkID").
   fClearSessionValue("StockPackage.LocationID").
   fClearSessionValue("StockPackage.StockStatusID").
   fClearSessionValue("StockPackage.BusinessUnitID").
   fClearSessionValue("StockPackage.OwnerID").
   fClearSessionValue("StockPackage.VendorID").
   fClearSessionValue("StockPackage.InboundID").
   fClearSessionValue("ParentPackageID").
         
   /* Create New Session Data */
   intSsnPackageStockStatusID  = fNewSessionValue("PackageStockStatusID").
   intSsnStockPartID           = fNewSessionValue("StockPackage.PartID").
   intSsnParentEntityID        = fNewSessionValue("StockPackage.StockEntityID").
   intSsnStockTaskUserLinkID   = fNewSessionValue("StockPackage.TaskLineWorkUserLinkID").
   intSsnStockLocationID       = fNewSessionValue("StockPackage.LocationID").
   intSsnStockStatusID         = fNewSessionValue("StockPackage.StockStatusID").
   intSsnStockOwnerID          = fNewSessionValue("StockPackage.OwnerID").
   intSsnStockVendorID         = fNewSessionValue("StockPackage.VendorID").      
   intSsnStockBusinessUnitID   = fNewSessionValue("StockPackage.BusinessUnitID"). 
   intSsnStockInboundID        = fNewSessionValue("StockPackage.InboundID").
   intSsnParentPackageID       = fNewSessionValue("ParentPackageID").
   chrSsnStockReconciled       = fNewSessionValue("StockPackage.Reconciled").
   
   /* Get Current Data */
   intSsnPartID  = fGetSessionValue("PartID").
   intSsnChildPackageID = fGetSessionValue("ChildPackageID").
 
   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      /* Get scanned StockPackage */
      FIND FIRST StockPackage NO-LOCK /* idx=StockPackageID */
         WHERE StockPackage.StockPackageID = intSsnChildPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "[StockPackage] ID [" + STRING(intSsnChildPackageID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END. /*IF NOT AVAILABLE StockPackage THEN*/

      /* Set the Child Package information */
      intSsnStockPartID:setValue(StockPackage.PartID).
      intSsnStockTaskUserLinkID:setValue(StockPackage.TaskLineWorkUserLinkID).
      intSsnStockStatusID:setValue(StockPackage.StockStatusID).
      intSsnPackageStockStatusID:setValue(StockPackage.StockStatusID).
      intSsnStockLocationID:setValue(StockPackage.LocationID).
      intSsnStockOwnerID:setValue(StockPackage.OwnerID).
      intSsnStockVendorID:setValue(StockPackage.VendorID).
      intSsnStockBusinessUnitID:setValue(StockPackage.BusinessUnitID).
      intSsnStockInboundID:setValue(StockPackage.InboundID).
      chrSsnStockReconciled:setValue(StockPackage.Reconciled).
      
      /* Get StockEntity information */
      FIND FIRST PartStockEntityLink NO-LOCK 
         WHERE PartStockEntityLink.PartID = StockPackage.PartID
         AND PartStockEntityLink.StockEntityID = StockPackage.StockEntityID NO-ERROR.
      IF NOT AVAILABLE PartStockEntityLink THEN
      DO:
         RUN DisplayError("Record not Found",   
                          "[PartStockEntityLink] does not Exist for Part [" 
                             + TRIM(fGetFieldValue("PartRef", "Part", StockPackage.PartID)) 
                             + "] and [StockEntity] ["
                             + TRIM(fGetFieldValue("EntityName", "StockEntity", StockPackage.StockEntityID)) 
                             + "].").
         LEAVE Main_Block.                  
      END. /*IF NOT AVAILABLE PartStockEntityLink THEN*/
      
      IF PartStockEntityLink.ParentStockEntityID = 0 THEN
      DO:
         RUN DisplayError("Record not Found",   
                          "[PartStockEntityLink ParentStockEntityID] does not Exist for Part [" 
                             + TRIM(fGetFieldValue("PartRef", "Part", StockPackage.PartID)) 
                             + "] and [StockEntity] ["
                             + TRIM(fGetFieldValue("EntityName", "StockEntity", StockPackage.StockEntityID)) 
                             + "].").
         LEAVE Main_Block.             
      END.
      
      /* Set the Parent Stock Entity ID */
      intSsnParentEntityID:setValue(PartStockEntityLink.ParentStockEntityID).
  
      /* Start: Multiple Open Pallets for Same FG then pass parent of same userid */  
      FOR EACH ParentBuild NO-LOCK 
         WHERE ParentBuild.Complete = "":
            
         FIND FIRST StockPackage OF ParentBuild NO-LOCK 
            WHERE StockPackage.PartID    = intSsnPartID:intValue 
            AND   StockPackage.Detrashed = "" NO-ERROR.
         IF AVAILABLE StockPackage THEN
         DO:                
            /* Get the id of the original parent creator */
            FIND FIRST ParentBuildUserLink NO-LOCK 
               WHERE ParentBuildUserLink.ParentBuildID = ParentBuild.ParentBuildID NO-ERROR.
            
            /******* Commented out in Live to prevent error between Pallet Builds 
            IF NOT AVAILABLE ParentBuildUserLink THEN 
            DO:
               RUN DisplayError("System Error",
                                "ParentBuildID: " + STRING(ParentBuild.ParentBuildID) + " does not have a ParentBuildUserLink record").
               LEAVE Main_Block.  
            END.  
            ***************/
            
            IF AVAILABLE ParentBuildUserLink THEN 
            DO:
                CREATE ttOpenPallets.
                ASSIGN ttOpenPallets.OpenPalletID     = StockPackage.StockPackageID
                       ttOpenPallets.OpenPalletUserID = ParentBuildUserLink.GateUserID.
            END.
            
         END. /* IF AVAILABLE StockPackage */           
      END. /* EACH ParentBuild */
      
      FIND FIRST ttOpenPallets NO-LOCK 
         WHERE ttOpenPallets.OpenPalletUserID = intGblUserID NO-ERROR.
      IF AVAILABLE ttOpenPallets THEN 
      DO:
         /* Set Parent PackageID */
         intSsnParentPackageID:setValue(ttOpenPallets.OpenPalletID).
           
         chrResult = "Yes".
         
         LEAVE Main_Block.
         
      END. /* Available, = intGblUserID */
      ELSE 
      DO:
         FIND FIRST ttOpenPallets NO-LOCK 
            WHERE ttOpenPallets.OpenPalletUserID <> intGblUserID NO-ERROR.
         IF AVAILABLE ttOpenPallets THEN 
         DO:
            /* Set Parent PackageID */
            intSsnParentPackageID:setValue(ttOpenPallets.OpenPalletID).
         
            chrResult = "Yes".
            
            LEAVE Main_Block.
            
         END.  /*Available, <> intGblUserID */
         ELSE 
         DO:
            /* No records in ttOpenPallets, No open ParentBuild found for the same part */
            chrResult = "No".
         END.
      END. /* ELSE  <> intGblUserID */
      
      /* End: Multiple Open Pallets for Same FG then pass parent of same user */ 
         
   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnPartID                NO-ERROR.
   DELETE OBJECT intSsnParentPackageID       NO-ERROR.
   DELETE OBJECT intSsnChildPackageID        NO-ERROR.
   DELETE OBJECT intSsnPackageStockStatusID  NO-ERROR.
   DELETE OBJECT intSsnPartID                NO-ERROR.
   DELETE OBJECT intSsnStockPartID           NO-ERROR.
   DELETE OBJECT intSsnStockTaskUserLinkID   NO-ERROR.
   DELETE OBJECT intSsnStockLocationID       NO-ERROR.
   DELETE OBJECT intSsnStockStatusID         NO-ERROR.
   DELETE OBJECT intSsnStockInboundID        NO-ERROR.
   DELETE OBJECT intSsnStockOwnerID          NO-ERROR.
   DELETE OBJECT intSsnStockVendorID         NO-ERROR.
   DELETE OBJECT intSsnStockBusinessUnitID   NO-ERROR.
   DELETE OBJECT chrSsnStockReconciled       NO-ERROR.
   DELETE OBJECT intSsnParentEntityID        NO-ERROR.

   /* Releases */
   RELEASE ParentBuild         NO-ERROR.
   RELEASE StockPackage        NO-ERROR.
   RELEASE PartStockEntityLink NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
