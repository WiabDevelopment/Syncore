/*------------------------------------------------------------------------------------------------------------------------------------------
Program : psRunCycleCounting.p
Purpose : Starts the Cycle Counting Process

          Possible outcomes : Continue.
          
Author  : Christopher Shelley
Date    : 16/04/2014
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
   
   /* Clear Session Values */
   fClearSessionValues("").
   
   
   /* Get the OperationType for the Process */
   FIND FIRST OperationType NO-LOCK
      WHERE OperationType.TypeCode = "CycleCount" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN
   DO:
      RUN DisplayMessage("Missing Operation",
                         "[OperationType] [CycleCount] is NOT Setup.").          
      LEAVE.
   END. /* IF NOT AVAILABLE OperationType */
   
   intGblOperationTypeID = OperationType.OperationTypeID.
   
   Main_Block:
   DO ON ERROR UNDO:

      chrResult = "Continue".
      
   END. /* Main Block */

   /* Clean Up */   
   
   /* Release */
   RELEASE OperationType NO-ERROR.
 
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
