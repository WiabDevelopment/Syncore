/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCreateCountTaskLocationPart.p
Purpose : Creates the CountTaskLocationPart Record for expected parts and quantities for the Count Task

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
   DEFINE VARIABLE newCountTaskLocationPart AS newRecord NO-UNDO.
   DEFINE VARIABLE updCountTaskLocationPart AS updRecord NO-UNDO.

   /* Temp Tables */
   DEFINE TEMP-TABLE ttPart
      FIELD PartID AS INTEGER
      FIELD QtyExpected AS INTEGER
      INDEX idxPartID PartID.

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

      /* Find all parts for this Location */
      FOR EACH StockPackage NO-LOCK /*idx = LocationIDStockStatus */
         WHERE StockPackage.LocationID = CountTaskLocation.LocationID:

         IF StockPackage.ParentStockPackageID <> 0 THEN NEXT. /* Removing all Children - placed here to speed up */
         
         FIND FIRST ttPart /* idx = idxPartID */
            WHERE ttPart.PartID = StockPackage.PartID NO-ERROR.
         IF NOT AVAILABLE ttPart THEN 
         DO:
            CREATE ttPart.
            ASSIGN ttPart.PartID = StockPackage.PartID.
         END.
                
         ASSIGN ttPart.QtyExpected = ttPart.QtyExpected + StockPackage.PackageQty.

      END.

      FOR EACH ttPart:
         
         /* Try to find CountTaskLocationPart for the PartID if not available create one with the expectedQty */
         FIND FIRST CountTaskLocationPart NO-LOCK /*idx = PartID */
            WHERE CountTaskLocationPart.PartID = ttPart.PartID
            AND   CountTaskLocationPart.CountTaskLocationID = CountTaskLocation.CountTaskLocationID NO-ERROR.
         IF NOT AVAILABLE CountTaskLocationPart THEN
         DO:
            newCountTaskLocationPart = fCreateRecord("CountTaskLocationPart").
            
            newCountTaskLocationPart:assignField("CountTaskLocationID" ,CountTaskLocation.CountTaskLocationID).
            newCountTaskLocationPart:assignField("CountTaskStatusID"   ,intStatusInProcess).
            newCountTaskLocationPart:assignField("PartID"              ,ttPart.PartID).
            newCountTaskLocationPart:assignField("QtyExpected"         ,ttPart.QtyExpected).
            newCountTaskLocationPart:assignField("QtyCounted"          ,0).
            newCountTaskLocationPart:assignField("Completed"           ,"").
            newCountTaskLocationPart:assignField("Priority"            ,0). /* Possibly need to be changed to right value */
            newCountTaskLocationPart:assignField("AddedAfterCountBegan",FALSE).
         
            /* Save Errors */
            chrError = chrError + newCountTaskLocationPart:getErrors().
      
            /* Error Check */
            IF chrError <> "" THEN
            DO:
               RUN DisplayMessage("Create Failed",
                                  chrError).
               UNDO Main_Block, LEAVE Main_Block.
            END.            
         END.   
         ELSE IF  CountTaskLocationPart.Completed = "" 
              AND CountTaskLocationPart.QtyExpected <> ttPart.QtyExpected THEN 
         DO:
            /* IF available CountTaskLocationPart and Completed = "" update the expectedQty*/
            updCountTaskLocationPart = fGetRecord("CountTaskLocationPart", CountTaskLocationPart.CountTaskLocationPartID).
         
            /* Check for WebLocks */
            IF updCountTaskLocationPart:RecordLocked THEN 
            DO:  
               RUN DisplayMessage("Record Locked",
                                  updCountTaskLocationPart:getErrors()).       
               UNDO Main_Block, LEAVE Main_Block.
            END.
            
            /* Finally Updating */
            updCountTaskLocationPart:assignField("QtyExpected", ttPart.QtyExpected).
            
            /* Error Check */
            chrError = updCountTaskLocationPart:getErrors().
            
            IF chrError <> "" THEN 
            DO:
               RUN DisplayMessage("Update Failed",
                                  chrError).
               UNDO Main_Block, LEAVE Main_Block.
            END.
            
         END.  /*  ELSE IF CountTaskLocationPart.Completed = ""  */     
               
      END. /* FOR EACH ttPart: */
       
      /* Created Sucessfully */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   EMPTY TEMP-TABLE ttPart NO-ERROR.
   
   DELETE OBJECT intSsnTaskLocationID     NO-ERROR.
   DELETE OBJECT newCountTaskLocationPart NO-ERROR.
   DELETE OBJECT updCountTaskLocationPart NO-ERROR.
   
   /* Releases */
   RELEASE StockPackage          NO-ERROR.
   RELEASE CountTaskLocation     NO-ERROR.
   RELEASE CountTaskLocationPart NO-ERROR.
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
