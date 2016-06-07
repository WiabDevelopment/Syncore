/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCreateCountTaskLocationPartUnexpected.p
Purpose : Create the CountTaskLocationPart for unexpected scan

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
   
   /* Local Varaibles */
   DEFINE VARIABLE intStatusInProcess AS INTEGER NO-UNDO.
   
   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartID         AS sessionValue NO-UNDO.
   
   /* DB Objects */
   DEFINE VARIABLE newCountTaskLocationPart AS newRecord NO-UNDO. 
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   intSsnPartID         = fGetSessionValue("PartID").

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

      FIND FIRST Part NO-LOCK
         WHERE Part.PartID = intSsnPartID:intValue NO-ERROR.
      IF NOT AVAILABLE Part THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Part ID] [" + STRING(intSsnPartID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST CountTaskLocationPart NO-LOCK
         WHERE CountTaskLocationPart.PartID = Part.PartID
         AND   CountTaskLocationPart.CountTaskLocationID = CountTaskLocation.CountTaskLocationID NO-ERROR.
      IF AVAILABLE CountTaskLocationPart THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskLocationPart ID] for [CountTaskLocationID] [" 
                            + STRING(intSsnTaskLocationID:intValue) + "] and Part [" 
                            + Part.PartRef + "] does not Exist.").
         LEAVE Main_Block.
      END. 
      ELSE 
      DO:
         intStatusInProcess = fGetStatusID("CountTask", "InProcess").
         
         newCountTaskLocationPart = fCreateRecord("CountTaskLocationPart").
            
         newCountTaskLocationPart:assignField("CountTaskLocationID" ,CountTaskLocation.CountTaskLocationID).
         newCountTaskLocationPart:assignField("CountTaskStatusID"   ,intStatusInProcess).
         newCountTaskLocationPart:assignField("PartID"              ,Part.PartID).
         newCountTaskLocationPart:assignField("QtyExpected"         ,0).
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

      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID     NO-ERROR.
   DELETE OBJECT intSsnPartID             NO-ERROR.
   DELETE OBJECT newCountTaskLocationPart NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation     NO-ERROR.
   RELEASE CountTaskLocationPart NO-ERROR.
   RELEASE Part                  NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
