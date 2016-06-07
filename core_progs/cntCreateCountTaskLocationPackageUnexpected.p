/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCreateCountTaskLocationPackageUnexpected.p
Purpose : Create the CountTaskLocationPackage for unexpected scan

          Possible Results : Continue

Author  : Mateusz Nogaj
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
   
      /* Local Varaibles */
   DEFINE VARIABLE intStatusInProcess AS INTEGER NO-UNDO.
   
   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPackageID      AS sessionValue NO-UNDO.
   
   /* DB Objects */
   DEFINE VARIABLE newCountTaskLocationPackage AS newRecord NO-UNDO. 
   
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
                          "[Task Location ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST StockPackage NO-LOCK
         WHERE StockPackage.StockPackageID = intSsnPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE StockPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[StockPackage ID] [" + STRING(intSsnPackageID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST CountTaskLocationPackage NO-LOCK
         WHERE CountTaskLocationPackage.StockPackageID = StockPackage.StockPackageID
         AND   CountTaskLocationPackage.CountTaskLocationID = CountTaskLocation.CountTaskLocationID NO-ERROR.
      IF AVAILABLE CountTaskLocationPackage THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskLocationPackage ID] for [CountTaskLocationID] [" 
                            + STRING(intSsnTaskLocationID:intValue) + "] and StockPackage [" 
                            + StockPackage.PackageRef + "] does not Exist.").
         LEAVE Main_Block.
      END. 
      ELSE 
      DO:
         intStatusInProcess = fGetStatusID("CountTask", "InProcess").
         
         newCountTaskLocationPackage = fCreateRecord("CountTaskLocationPackage").
            
         newCountTaskLocationPackage:assignField("CountTaskLocationID" ,CountTaskLocation.CountTaskLocationID).
         newCountTaskLocationPackage:assignField("CountTaskStatusID"   ,intStatusInProcess).
         newCountTaskLocationPackage:assignField("StockPackageID"      ,StockPackage.StockPackageID).
         newCountTaskLocationPackage:assignField("QtyExpected"         ,0).
         newCountTaskLocationPackage:assignField("QtyCounted"          ,0).
         newCountTaskLocationPackage:assignField("Completed"           ,"").
         newCountTaskLocationPackage:assignField("Priority"            ,0). /* Possibly need to be changed to right value */
         newCountTaskLocationPackage:assignField("AddedAfterCountBegan",FALSE).
      
         /* Save Errors */
         chrError = chrError + newCountTaskLocationPackage:getErrors().
   
         /* Error Check */
         IF chrError <> "" THEN
         DO:
            RUN DisplayMessage("Create Failed",
                               chrError).
            UNDO Main_Block, LEAVE Main_Block.
         END.       
      END.

      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID        NO-ERROR.
   DELETE OBJECT intSsnPackageID             NO-ERROR.
   DELETE OBJECT newCountTaskLocationPackage NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation        NO-ERROR.
   RELEASE CountTaskLocationPackage NO-ERROR.
   RELEASE StockPackage             NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
