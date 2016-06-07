/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCheckCountTaskLocationPart.p
Purpose : Check CountTaskLocationPart for any remaining to count

          Possible Results : Yes, No

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

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   
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

      FIND FIRST CountTaskLocationPart NO-LOCK /* idx = CountTaskLocationIDCompleted */
         WHERE CountTaskLocationPart.CountTaskLocationID = intSsnTaskLocationID:intValue
         AND   CountTaskLocationPart.Completed = "" NO-ERROR.
      IF AVAILABLE CountTaskLocationPart THEN
      DO:
         chrResult = "Yes".
         LEAVE Main_Block.
      END.

      chrResult = "No".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation     NO-ERROR.
   RELEASE CountTaskLocationPart NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
