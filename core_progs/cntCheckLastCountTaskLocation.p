/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCheckLastCountTaskLocation.p
Purpose : Checks if this is the last CountTaskLocation on a Count Task

          Possible Results : Yes, No

Author  : Christopher Shelley
Date    : 29/04/2014
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
   DEFINE VARIABLE intSkippedCountTaskStatus AS INTEGER NO-UNDO.
   
   /* Buffers */
   DEFINE BUFFER otherCountTaskLocation FOR CountTaskLocation.     

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
 
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   
   intSkippedCountTaskStatus = fGetStatusID("CountTask", "Skipped").

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

      FIND FIRST CountTask OF CountTaskLocation NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountTask THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTask ID] [" + STRING(CountTaskLocation.CountTaskID) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST otherCountTaskLocation OF CountTask no-lock
         WHERE otherCountTaskLocation.Completed = "" NO-ERROR.
      IF AVAILABLE otherCountTaskLocation THEN
      DO: 
         /* If Current Location Was Just Skipped we need to get next one */
         IF CountTaskLocation.Completed <> "" THEN 
         DO:
            chrResult = "Skipped".
            LEAVE Main_Block.
         END.
         ELSE 
         DO:
            chrResult = "No".
            LEAVE Main_Block.
         END.
      END.

      chrResult = "Yes".
   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   
   /* Releases */
   RELEASE CountTask              NO-ERROR.
   RELEASE CountTaskLocation      NO-ERROR.
   RELEASE otherCountTaskLocation NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
