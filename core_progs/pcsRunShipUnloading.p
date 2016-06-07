/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunShipUnloading.p
Purpose : Sets the initial data required for the Ship Unloading

          Possible results : Continue

Author  : Ashwin Baliga CR1018
Date    : 19/09/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
06/01/2015 CS  Nextelbr   Added Unloading OperationType.
12/01/2015 CS  Nextelbr   Renamed OperationType Code to UndoLoading.
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
   DEFINE VARIABLE chrSsnScanBayUserPrompt AS sessionValue NO-UNDO.

   /* Clear out ALL Session Values */
   fClearSessionValues("").

   /* Create New Session Data */
   chrSsnScanBayUserPrompt = fNewSessionValue("ScanBayUserPrompt").
      
   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      /* This record the data history Operation Type */
      FIND FIRST OperationType NO-LOCK
         WHERE OperationType.TypeCode = "UndoLoading" NO-ERROR.
      IF NOT AVAILABLE OperationType THEN
      DO:
         RUN DisplayMessage("Record Not Found",
                            "[Operation Type] [UndoLoading] does not exist.").
         LEAVE Main_Block.            
      END. /* IF NOT AVAILABLE OperationType THEN */
   
      IF NOT OperationType.Active THEN
      DO:
         RUN DisplayError("OperationType Incorrect",
                          "[OperationType] [UndoLoading] is not active").
         LEAVE Main_Block.
      END. /* IF NOT AVAILABLE Operation */

      /* Set Global Operation for History */
      intGblOperationTypeID = OperationType.OperationTypeID.   
      
      chrSsnScanBayUserPrompt:setValue(" Scan Ship Bay to Unload").
      chrResult = "Continue".
   
   END. /* Main_Block */  

   /* Clean Up */
   DELETE OBJECT chrSsnScanBayUserPrompt NO-ERROR.

   /* Releases */
   RELEASE OperationType NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */
