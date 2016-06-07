/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCreateCountTaskLocationPackage.p
Purpose : Create the CountTaskLocationPackage

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
   
   /* Local Variables */
   DEFINE VARIABLE intStatusInProcess AS INTEGER NO-UNDO.    

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */
   DEFINE VARIABLE newCountTaskLocationPackage AS newRecord NO-UNDO.
   DEFINE VARIABLE updCountTaskLocationPackage AS updRecord NO-UNDO.

   /* Temp Tables */
   DEFINE TEMP-TABLE ttPackage
      FIELD PackageID AS INTEGER
      FIELD QtyExpected AS INTEGER
      INDEX idxPartID PackageID.

   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").

   Main_Block:
   DO ON ERROR UNDO:
      intStatusInProcess = fGetStatusID("CountTask", "InProcess").
      
      /* Validate Session Data */
      FIND FIRST CountTaskLocation NO-LOCK /* idx=CountTaskLocationID */
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue
         AND   CountTaskLocation.Completed = "" NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Location ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      /* Find all Packages for this Location */
      Package_Loop:
      FOR EACH StockPackage NO-LOCK /*idx = LocationIDStockStatus */
         WHERE StockPackage.LocationID = CountTaskLocation.LocationID:
           
         IF StockPackage.Detrashed <> "" THEN NEXT Package_Loop.
         IF StockPackage.ParentStockPackageID <> 0 THEN NEXT Package_Loop. /* Removing all Children - placed here to speed up */
         
         CREATE ttPackage.
         ASSIGN ttPackage.PackageID   = StockPackage.StockPackageID                
                ttPackage.QtyExpected = StockPackage.PackageQty.

      END.

      FOR EACH ttPackage:
         
         /* Try to find CountTaskLocationPackage for the PartID if not available create one with the expectedQty */
         FIND FIRST CountTaskLocationPackage NO-LOCK /*idx = StockPackageID */
            WHERE CountTaskLocationPackage.StockPackageID = ttPackage.PackageID
            AND   CountTaskLocationPackage.CountTaskLocationID = CountTaskLocation.CountTaskLocationID NO-ERROR.
         IF NOT AVAILABLE CountTaskLocationPackage THEN
         DO:
            newCountTaskLocationPackage = fCreateRecord("CountTaskLocationPackage").
            
            newCountTaskLocationPackage:assignField("CountTaskLocationID" ,CountTaskLocation.CountTaskLocationID).
            newCountTaskLocationPackage:assignField("CountTaskStatusID"   ,intStatusInProcess).
            newCountTaskLocationPackage:assignField("StockPackageID"      ,ttPackage.PackageID).
            newCountTaskLocationPackage:assignField("QtyExpected"         ,ttPackage.QtyExpected).
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
         ELSE IF  CountTaskLocationPackage.Completed = "" 
              AND CountTaskLocationPackage.QtyExpected <> ttPackage.QtyExpected THEN 
         DO:
            /* IF available CountTaskLocationPackage and Completed = "" update the expectedQty*/
            updCountTaskLocationPackage = fGetRecord("CountTaskLocationPackage", CountTaskLocationPackage.CountTaskLocationPackageID).
         
            /* Check for WebLocks */
            IF updCountTaskLocationPackage:RecordLocked THEN 
            DO:  
               RUN DisplayMessage("Record Locked",
                                  updCountTaskLocationPackage:getErrors()).       
               UNDO Main_Block, LEAVE Main_Block.
            END.
            
            /* Finally Updating */
            updCountTaskLocationPackage:assignField("QtyExpected", ttPackage.QtyExpected).
            
            /* Error Check */
            chrError = updCountTaskLocationPackage:getErrors().
            
            IF chrError <> "" THEN 
            DO:
               RUN DisplayMessage("Update Failed",
                                  chrError).
               UNDO Main_Block, LEAVE Main_Block.
            END.
            
         END.  /*  ELSE IF CountTaskLocationPackage.Completed = ""  */     
               
      END. /* FOR EACH ttPackage: */
       
      /* Created Sucessfully */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   EMPTY TEMP-TABLE ttPackage NO-ERROR.
   
   DELETE OBJECT intSsnTaskLocationID        NO-ERROR.
   DELETE OBJECT newCountTaskLocationPackage NO-ERROR.
   DELETE OBJECT updCountTaskLocationPackage NO-ERROR.
   
   /* Releases */
   RELEASE StockPackage             NO-ERROR.
   RELEASE CountTaskLocation        NO-ERROR.
   RELEASE CountTaskLocationPackage NO-ERROR.
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
