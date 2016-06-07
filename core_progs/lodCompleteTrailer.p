/*------------------------------------------------------------------------------------------------------------------------------------------
Program : lodCompleteTrailer.p   
Purpose : Complete the Trailer and Update the Outbound to Loaded

          Possible results : Continue

Author  : DCummins   
Date    : 27/03/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
23/04/2014 BR  CR 1052    UI Standardization
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character SessionValue Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}

   /* Optional Includes */
   {fncStatusTypeFunctions.i}
   {fncDateFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i} 
   
   /* Session Objects */                      
   DEFINE VARIABLE intSsnOutboundID AS sessionValue NO-UNDO.   

   /* DB Objects */      
   DEFINE VARIABLE updOutbound      AS updRecord    NO-UNDO. 

   /* Get Current Data */   
   intSsnOutboundID = fGetSessionValue("OutboundID").
               
   Main_Block:
   DO ON ERROR UNDO:

      FIND FIRST Outbound NO-LOCK
         WHERE Outbound.OutboundID = intSsnOutboundID:intValue NO-ERROR.
      IF NOT AVAILABLE Outbound THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Outbound #" + STRING(intSsnOutboundID:intValue) + " does not exist on the system").
         LEAVE Main_Block. 
      END.
      
      /* Set the Outbound to Loaded */
      IF Outbound.OutboundStatusID = fGetStatusID("Outbound", "BeingLoaded") THEN
      DO:               
         updOutbound = fGetRecord("Outbound", intSsnOutboundID:intValue).
         
         /* Check for WebLocks */
         IF updOutbound:RecordLocked THEN
         DO:
            RUN DisplayError("Record Locked",
                          updOutbound:getErrors()).
            UNDO Main_Block, LEAVE Main_Block.
         END.
         
         updOutbound:assignField("OutboundStatusID", fGetStatusID("Outbound", "Loaded")).

         chrError = chrError + updOutbound:getErrors().

         DELETE OBJECT updOutbound NO-ERROR.
      END.
      
      /* Check Errors */
      IF chrError <> "" THEN
      DO:
         RUN DisplayError("Update Error",
                          chrError).       
         UNDO Main_Block, LEAVE Main_Block.
      END.     
   
      RUN DisplayMessage("Trailer Loaded",
                         "Trailer for Outbound #" + STRING(intSsnOutboundID:intValue) + " has been Fully Loaded.").

      chrResult = "Continue".

   END.

   /* Releases */
   RELEASE Outbound NO-ERROR.
   
   /* Clean Up */
   DELETE OBJECT intSsnOutboundID NO-ERROR.   
   DELETE OBJECT updOutbound      NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */

