/*------------------------------------------------------------------------------------------------------------------------------------------
Program : bldCheckMultipleWorkOrdersPerPallet.p 
Purpose : Checks whether to allow mixed WorkOrders on one pallet

Author  : BR
Date    : 8th July 2015
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
   
   
   Main_Block:
   DO ON ERROR UNDO:

      /* Validate Session Data */
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "No Kitting config record exist.").
         LEAVE Main_Block.
      END.

      /* Check for MixedWorkOrders */
      IF KittingConfig.AllowMixedWorkOrdersOnOnePallet THEN 
      DO:
         chrResult = "Yes".
         LEAVE Main_Block.
      END.
      
      /* Single WorkOrder on 1 Pallet */
      chrResult = "No".

   END. /* Main_Block */

   /* Releases */
   RELEASE KittingConfig NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}


END. /* CTRL-C Catch */
