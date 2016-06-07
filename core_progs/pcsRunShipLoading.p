/*------------------------------------------------------------------------------------------------------------------------------------------
Program : psRunShipLoading.p
Purpose : Sets the initial data required for the Ship Loading

          Possible results : Continue

Author  : DCummins
Date    : 27/03/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
19/09/2013 AB  CR1018     Added ScanBayUserPrompt session variable and release statement for OperationType
23/04/2014 BR  CR 1052    UI Standardization
07/01/2015 CS  NExtel     Added Loading OperationType and set it.
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

   /* Session Objects */   
   DEFINE VARIABLE chrSsnScanBayUserPrompt AS sessionValue NO-UNDO.

   /* Clear out ALL Session Values */
   fClearSessionValues("").

   /* Create New Session Data */
   chrSsnScanBayUserPrompt = fNewSessionValue("ScanBayUserPrompt").
      
   Main_Block:
   DO ON ERROR UNDO, LEAVE:
      
      /* This record the data history Operation Type */
      FIND FIRST OperationType NO-LOCK
         WHERE OperationType.TypeCode = "Loading" NO-ERROR.
      IF NOT AVAILABLE OperationType THEN
      DO:
         RUN DisplayMessage("Record Not Found",
                            "[Operation Type] [Loading] does not exist.").
         LEAVE Main_Block.            
      END. /* IF NOT AVAILABLE OperationType THEN */
   
      IF NOT OperationType.Active THEN
      DO:
         RUN DisplayError("OperationType Incorrect",
                          "[OperationType] [Loading] is not active").
         LEAVE Main_Block.
      END. /* IF NOT AVAILABLE Operation */

      /* Set Global Operation for History */
      intGblOperationTypeID = OperationType.OperationTypeID.  
      
      chrSsnScanBayUserPrompt:setValue(" Scan Ship Bay to Load").
      chrResult = "Continue".
   
   END. /* Main_Block */  

   /* Clean Up */
   DELETE OBJECT chrSsnScanBayUserPrompt NO-ERROR.

   /* Releases */
   RELEASE OperationType NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */





