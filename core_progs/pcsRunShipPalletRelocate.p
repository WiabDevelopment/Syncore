/*------------------------------------------------------------------------------------------------------------------------------------------
Program : psRunShipPalletRelocate.p
Purpose : Sets the initial data required for the Shipping Pallet Relocate

          Possible results : Continue

Author  : DCummins
Date    : 26/03/2013
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
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}

   /* Session Objects */

   /* Clear out ALL Sesion Values */
   fClearSessionValues("").
      
   /* This record the data history Operation Type */
   FIND OperationType NO-LOCK
      WHERE OperationType.TypeCode = "ShipPalletRelocate" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN
   DO:
      RUN DisplayError("Record Not Found",
                       "[Operation Type] [ShipPalletRelocate] does not exist.").
      LEAVE.            
   END.
      
   /* Set Global Operation for History */
   intGblOperationTypeID = OperationType.OperationTypeID.   

   Main_Block:
   DO ON ERROR UNDO, LEAVE:
   
      /* No Result Required */
      chrResult = "Continue".
   
   END.  /* Main_Block */ 

   /* Releases */
   RELEASE OperationType NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */





