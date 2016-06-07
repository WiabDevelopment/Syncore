/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCheckPackageScanned.p
Purpose : Checks if Package was already scanned for CountTaskLocationPackage

          Possible Results : Yes, No
bg
Author  : Christopher Shelley
Date    : 1/04/2014
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
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Local Variables */     

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageID      AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   intSsnPackageID      = fGetSessionValue("PackageID").

   Main_Block:
   DO ON ERROR UNDO:

      /* Validate Session Data */
      FIND FIRST CountTaskLocation NO-LOCK
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskLocation ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST StockPackage NO-LOCK
         WHERE StockPackage.StockPackageID = intSsnPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[StockPackage ID] [" + STRING(intSsnPackageID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST CountTaskLocationPackage NO-LOCK
         WHERE CountTaskLocationPackage.StockPackageID      = intSsnPackageID:intValue
         AND   CountTaskLocationPackage.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF AVAILABLE CountTaskLocationPackage 
         AND CountTaskLocationPackage.Completed <> "" THEN
      DO:
         chrResult = "Yes".
         LEAVE Main_Block.
      END.

      chrResult = "No".

   END. /* Main_Block */   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   DELETE OBJECT intSsnPackageID      NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation        NO-ERROR.
   RELEASE CountTaskLocationPackage NO-ERROR.
   RELEASE StockPackage             NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.