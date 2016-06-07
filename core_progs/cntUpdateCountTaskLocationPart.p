/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateCountTaskLocationPart.p
Purpose : Updates the CountTaskLocationPart for counted quantity

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
   DEFINE VARIABLE intSsnPartID         AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartQty        AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   DEFINE VARIABLE updCountTaskLocationPart AS updRecord NO-UNDO.

   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   intSsnPartID         = fGetSessionValue("PartID").
   intSsnPartQty        = fGetSessionValue("PartQty").

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

      FIND FIRST CountTaskLocationPart NO-LOCK /* idx = CountTaskLocationIDCompleted */
         WHERE CountTaskLocationPart.PartID = intSsnPartID:intValue
         AND   CountTaskLocationPart.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocationPart THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskLocationPart ID] for [CountTaskLocationID] [" 
                            + STRING(intSsnTaskLocationID:intValue) + "] and Part [" 
                            + Part.PartRef + "] does not Exist.").
         LEAVE Main_Block.
      END.
      
      IF intSsnPartQty:intValue <= 0 THEN 
      DO:
          RUN DisplayError("Record Error",
                          "Incorrect Count Quantity [" + STRING(intSsnPartQty:intValue) + "].").
         LEAVE Main_Block.
      END.
      
      /* Get the CountTaskLocationPart Record and Update countedQty field */
      updCountTaskLocationPart = fGetRecord("CountTaskLocationPart",CountTaskLocationPart.CountTaskLocationPartID).

      /* Check for WebLocks */
      IF updCountTaskLocationPart:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updCountTaskLocationPart:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      updCountTaskLocationPart:assignField("Completed", fTimeStamp(NOW)).
      updCountTaskLocationPart:assignField("QtyCounted", intSsnPartQty:intValue).
      updCountTaskLocationPart:assignField("CountTaskStatusID", fGetStatusID("CountTask", "Complete")).
      
      /* Error Check Update */
      chrError = updCountTaskLocationPart:getErrors().

      IF chrError <> "" THEN DO:      
         RUN DisplayError("Record Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID     NO-ERROR.
   DELETE OBJECT intSsnPartID             NO-ERROR.
   DELETE OBJECT intSsnPartQty            NO-ERROR.
   DELETE OBJECT updCountTaskLocationPart NO-ERROR.

   /* Releases */
   RELEASE CountTaskLocation     NO-ERROR.
   RELEASE Part                  NO-ERROR.
   RELEASE CountTaskLocationPart NO-ERROR.
   
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
