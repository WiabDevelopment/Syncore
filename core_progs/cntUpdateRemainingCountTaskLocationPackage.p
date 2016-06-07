/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateRemainingCountTaskLocationPackage.p
Purpose : Updates the unscanned CountTaskLocationPackage to be Completed

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

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   DEFINE VARIABLE updCountTaskLocationPackage AS updRecord NO-UNDO.

   /* Get Current Session Data */
   intSsnTaskLocationID  = fGetSessionValue("TaskLocationID").

   Main_Block:
   DO ON ERROR UNDO:

      FOR EACH CountTaskLocationPackage NO-LOCK
         WHERE CountTaskLocationPackage.CountTaskLocationID = intSsnTaskLocationID:intValue
         AND   CountTaskLocationPackage.Completed = "":

          /* Get the CountTaskLocationPackage Record */
          updCountTaskLocationPackage = fGetRecord("CountTaskLocationPackage", CountTaskLocationPackage.CountTaskLocationPackageID).
          updCountTaskLocationPackage:assignField("Completed", fTimeStamp(NOW)).
          updCountTaskLocationPackage:assignField("CountTaskStatusID", fGetStatusID("CountTask", "Aborted")).

          /* Error Check Update */
          chrError = updCountTaskLocationPackage:getErrors().
    
          IF chrError <> "" THEN DO:      
             RUN DisplayError("Record Update Error",
                               chrError).
             UNDO Main_Block, LEAVE Main_Block.
          END.
          DELETE OBJECT updCountTaskLocationPackage NO-ERROR.

      END. /* FOR EACH CountTaskLocationPackage */

      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID        NO-ERROR.
   DELETE OBJECT updCountTaskLocationPackage NO-ERROR.

   /* Releases */
   RELEASE CountTaskLocationPackage NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
