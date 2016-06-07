/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateCountTaskInProcess.p
Purpose : Updates the CountTask Status to InProcess

          Possible Results : Continue

Author  : Mateusz Nogaj
Date    : 16/05/2014
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
   
   /* DB Objects */   
   DEFINE VARIABLE updCountTask AS updRecord NO-UNDO.

   /* Get Current Session Data */
   intSsnTaskLocationID  = fGetSessionValue("TaskLocationID").

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

      FIND FIRST CountTask OF CountTaskLocation NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountTask THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTask ID] [" + STRING(CountTaskLocation.CountTaskID) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      /* Get the CountTask Record and Update AssignedTo field */
      updCountTask = fGetRecord("CountTask", CountTaskLocation.CountTaskID).

      /* Check for WebLocks */
      IF updCountTask:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updCountTask:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      updCountTask:assignField("CountTaskStatusID", fGetStatusID("CountTask", "InProcess")).
      
      /* Error Check Update */
      chrError = updCountTask:getErrors().

      IF chrError <> "" THEN DO:      
         RUN DisplayError("Record Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.   
   DELETE OBJECT updCountTask         NO-ERROR.

   /* Releases */
   RELEASE CountTask         NO-ERROR.
   RELEASE CountTaskLocation NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.