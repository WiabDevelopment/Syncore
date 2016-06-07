/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunRemovePalletFromLane.p
Purpose : Initiates Remove Shipping Pallet From Ship Lane Proces

Possible Results: Continue
         
Author  : Mateusz Nogaj  
Date    : 26/08/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
23/04/2014 BR  CR 1052    UI Standardization
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

   /* Session Objects */   
   DEFINE VARIABLE chrSsnShipPalletUserPrompt AS sessionValue NO-UNDO.

   /* Clear Session Data */
   fClearSessionValues("").

   /* Create New Session Data */
   chrSsnShipPalletUserPrompt = fNewSessionValue("ShipPalletUserPrompt").

   Main_Block:
   DO ON ERROR UNDO:

      FIND FIRST OperationType NO-LOCK
         WHERE OperationType.TypeCode = "RemovePalletFromLane" NO-ERROR.

      IF NOT AVAILABLE OperationType THEN
      DO:
         RUN DisplayError("OperationType Not Found",
                          "[OperationType] [RemovePalletFromLane] does not exist").
         LEAVE Main_Block.
      END. /* Not Available Operation */

      IF NOT OperationType.Active THEN
      DO:
         RUN DisplayError("OperationType Incorrect",
                          "[OperationType] [RemovePalletFromLane] is not active").
         LEAVE Main_Block.
      END. /* Not Available Operation */

      /* Set Global Operation for History */
      intGblOperationTypeID = OperationType.OperationTypeID.      

      chrSsnShipPalletUserPrompt:setValue("Pallet To Move From Lane").
      chrResult = "Continue".

   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT chrSsnShipPalletUserPrompt NO-ERROR.

   /* Releases */
   RELEASE OperationType NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */

