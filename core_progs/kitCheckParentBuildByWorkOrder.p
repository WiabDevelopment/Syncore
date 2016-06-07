/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckParentBuildByWorkOrder.p 
Purpose : Checks whether PalletBuild can be done By Work Order

Author  : BR
Date    : 18th Aug 2015
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
   
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "No Kitting config record exist.").
         LEAVE Main_Block.
      END.

      /* Check KittingConfig PalletBuildByWorkOrder */
      IF KittingConfig.AllowMixedWorkOrdersOnOnePallet THEN
      DO:
         chrResult = "Yes".
         LEAVE Main_Block.
      END.
      ELSE      
         chrResult = "No".

   END. /* Main_Block */

   /* Releases */
   RELEASE KittingConfig   NO-ERROR.
    
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
