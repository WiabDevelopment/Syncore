/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateLocationCountCompleted.p
Purpose : Updates the Location to no longer be count in process for a Count Task

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
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Local Variables */     

   /* Session Objects */   
   DEFINE VARIABLE intSsnLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   DEFINE VARIABLE updLocation AS updRecord NO-UNDO.

   /* Get Current Session Data */
   intSsnLocationID = fGetSessionValue("LocationID").

   Main_Block:
   DO ON ERROR UNDO:

      /* Validate Session Data */
      FIND FIRST Location NO-LOCK
         WHERE Location.LocationID = intSsnLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE Location THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Location ID] [" + STRING(intSsnLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      /* Get the Location Record and Update CountInProcess field */
      updLocation = fGetRecord("Location", intSsnLocationID:intValue).

      /* Check for WebLocks */
      IF updLocation:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updLocation:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      updLocation:assignField("CountInProcess", FALSE).

      /* Error Check Update */
      chrError = updLocation:getErrors().

      IF chrError <> "" THEN DO:      
         RUN DisplayError("Record Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      /* Update Sucessfully Completed */
      chrResult = "Continue".
   END. /* Main_Block */  

   /* Clean Up */
   DELETE OBJECT intSsnLocationID NO-ERROR.   
   DELETE OBJECT updLocation      NO-ERROR.

   /* Releases */
   RELEASE Location NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.