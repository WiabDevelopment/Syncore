/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckParentLinkedToOperator.p 
Purpose : Check if Parent pallets original creator is the current user.
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
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Session Objects */ 
   DEFINE VARIABLE intSsnParentPackageID AS sessionValue NO-UNDO.
   
   /* Get Current Session Data */
   intSsnParentPackageID = fGetSessionValue("ParentPackageID").
    
   Main_Block:
   DO ON ERROR UNDO:
      
      FIND FIRST StockPackage NO-LOCK 
         WHERE StockPackage.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN 
      DO:
         RUN DisplayError("System Error",
                          "StockPackageID: " + STRING(intSsnParentPackageID:intValue) + " does not exist").
         LEAVE Main_Block.     
      END.   
      
      FIND FIRST ParentBuild NO-LOCK 
         WHERE ParentBuild.StockPackageID = StockPackage.StockPackageID NO-ERROR.
      IF AVAILABLE ParentBuild THEN 
      DO:
         /* Look thru the link records if the current user is linked to the pallet */
         FOR EACH ParentBuildUserLink OF ParentBuild NO-LOCK:
            
            IF ParentBuildUserLink.GateUserID = intGblUserID THEN 
            DO:
               chrResult = "Yes".
               
               LEAVE Main_Block.
                  
            END. /* IF ParentBuildUserLink.GateUserID = intGblUserID */     
         END. /* FOR EACH ParentBuildUserLink */
      END. /* IF AVAILABLE ParentBuil */
      
      /* Did not find any link records that links current user to pallet */   
      chrResult = "No".
     
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnParentPackageID       NO-ERROR.
   
   /* Releases */
   RELEASE StockPackage        NO-ERROR.
   RELEASE ParentBuild         NO-ERROR.
   RELEASE ParentBuildUserLink NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
