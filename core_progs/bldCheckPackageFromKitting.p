/*------------------------------------------------------------------------------------------------------------------------------------------
Program : bldCheckPackageFromKitting.p 
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
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Local Variables */   

   /* Session Objects */   
   DEFINE VARIABLE intSsnStockPackageID AS sessionValue NO-UNDO.

   /* Get Current Session Data */
   intSsnStockPackageID = fGetSessionValue("PackageID").

   Main_Block:
   DO ON ERROR UNDO:

      /* Validate Session Data */
      FIND FIRST StockPackage NO-LOCK
         WHERE StockPackage.StockPackageID = intSsnStockPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Package ID " + STRING(intSsnStockPackageID:intValue) + " does not Exist.").
         LEAVE Main_Block.
      END.

      /* Find any KittedUnit Records for StockPackage */
      IF CAN-FIND(FIRST KittedUnit NO-LOCK
                  WHERE KittedUnit.StockPackageID = StockPackage.StockPackageID) THEN
      DO:
         chrResult = "Yes".
         LEAVE Main_Block.
      END.
      
      /* Nothing Found */
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
