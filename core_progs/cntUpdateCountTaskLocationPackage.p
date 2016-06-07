/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateCountTaskLocationPackage.p
Purpose : Updates the CountTaskLocationPackage for quantity

          Possible Results : Continue

Author  : Christopher Shelley
Date    : 28/04/2014
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
     
   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageID      AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageQty     AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   DEFINE VARIABLE updCountTaskLocationPackage AS updRecord NO-UNDO.

   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   intSsnPackageID      = fGetSessionValue("PackageID").
   intSsnPackageQty     = fGetSessionValue("PackageQty").

   Main_Block:
   DO ON ERROR UNDO:

      /* Validate Session Data */
      FIND FIRST CountTaskLocation NO-LOCK
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Task Location ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST StockPackage NO-LOCK
         WHERE StockPackage.StockPackageID = intSsnPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Package ID] [" + STRING(intSsnPackageID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST CountTaskLocationPackage NO-LOCK /* idx = CountTaskLocationIDCompleted */
         WHERE CountTaskLocationPackage.StockPackageID = intSsnPackageID:intValue
         AND   CountTaskLocationPackage.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocationPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskLocationPackage ID] for [CountTaskLocationID] [" 
                            + STRING(intSsnTaskLocationID:intValue) + "] and Package [" 
                            + StockPackage.PackageRef + "] does not Exist.").
         LEAVE Main_Block.
      END.
      
      IF intSsnPackageQty:intValue <= 0 THEN 
      DO:
          RUN DisplayError("Record Error",
                          "Incorrect Count Quantity [" + STRING(intSsnPackageQty:intValue) + "].").
         LEAVE Main_Block.
      END.
      
      /* Get the CountTaskLocationPackage Record and Update countedQty field */
      updCountTaskLocationPackage = fGetRecord("CountTaskLocationPackage",CountTaskLocationPackage.CountTaskLocationPackageID).

      /* Check for WebLocks */
      IF updCountTaskLocationPackage:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updCountTaskLocationPackage:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      updCountTaskLocationPackage:assignField("Completed", fTimeStamp(NOW)).
      updCountTaskLocationPackage:assignField("QtyCounted", intSsnPackageQty:intValue).
      updCountTaskLocationPackage:assignField("CountTaskStatusID", fGetStatusID("CountTask", "Complete")).
      
      /* Error Check Update */
      chrError = updCountTaskLocationPackage:getErrors().

      IF chrError <> "" THEN DO:      
         RUN DisplayError("Record Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID        NO-ERROR.
   DELETE OBJECT intSsnPackageID             NO-ERROR.
   DELETE OBJECT intSsnPackageQty            NO-ERROR.
   DELETE OBJECT updCountTaskLocationPackage NO-ERROR.

   /* Releases */
   RELEASE CountTaskLocation        NO-ERROR.
   RELEASE StockPackage             NO-ERROR.
   RELEASE CountTaskLocationPackage NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
