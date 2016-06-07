/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckParentStillOpen.p
Purpose : Checks if the parent is still open.

          Possible Results : Yes, No.

Author  : BR
Date    : 17th Aug 2015
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

   /* Get Current Data */
   intSsnParentPackageID = fGetSessionValue("ParentPackageID").
 
   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      FIND FIRST StockPackage NO-LOCK 
         WHERE StockPackage.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN 
      DO:
         RUN DisplayMessage("Record not found",
                            "Parent Stock Package " + STRING(intSsnParentPackageID:intValue) + " does not exist.").
         LEAVE Main_Block.                   
      END.
   
      /* Atleast one record should be incomplete meaning pallets still open else its closed */
      FIND FIRST ParentBuild NO-LOCK 
         WHERE ParentBuild.Complete = "" 
         AND ParentBuild.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE ParentBuild THEN 
      DO:      
         RUN DisplayMessage("Parent closed",
                            "Parent Package  " + StockPackage.PackageRef + " is already closed").
         chrResult = "No".
         
         LEAVE Main_Block.
      END.   
         
      /* Open ParentBuild found */
      chrResult = "Yes".
   
   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnParentPackageID NO-ERROR.
   
   /* Releases */
   RELEASE ParentBuild         NO-ERROR.
   RELEASE StockPackage        NO-ERROR.
   RELEASE ParentBuildUserLink NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */