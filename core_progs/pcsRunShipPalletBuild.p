/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunShipPalletBuild.p
Purpose : Sets the initial data required for the Shipping Pallet Build

          Possible results : Continue

Author  : DCummins
Date    : 26/03/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
23/04/2014 BR  CR 1052    UI Standardization
07/07/2015 CS  GoPro      Added check prior to LocationTypeGroup Error for missing AwaitingNFLocations.
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

   /* Clear out ALL Session Values */
   fClearSessionValues("").

   /* Local Objects */
   DEFINE VARIABLE chrSsnGetOptions AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskTypeID AS sessionValue NO-UNDO.

   /* Set New Data */
   chrSsnGetOptions = fNewSessionValue("GetOptions").
   intSsnTaskTypeID = fNewSessionValue("TaskTypeID").
      
   /* This record the data history Transaction Type */
   FIND FIRST OperationType NO-LOCK /* idx=TypeCode */
      WHERE OperationType.TypeCode = "ShipPalletBuild" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN
   DO:
      RUN DisplayError("Record Not Found",
                       "[Operation Type] [ShipPalletBuild] does not exist.").
      LEAVE.            
   END. /*IF NOT AVAILABLE OperationType THEN*/
   
   /* Get the Statuses for Ship Pallet Build */
   FIND FIRST ShipOrderStatusGroup NO-LOCK /* idx=GroupCode */
        WHERE ShipOrderStatusGroup.GroupCode = "ShipPalletBuild" NO-ERROR.   
   IF NOT AVAILABLE ShipOrderStatusGroup THEN
   DO:
      RUN DisplayError("Record Not Found",
                       "[ShipOrderStatusGroup] [ShipPalletBuild] does not exist.").
      LEAVE.            
   END. /*IF NOT AVAILABLE ShipOrderStatusGroup THEN*/

   /* Get the Task Type of ShipPalletBuild */
   FIND FIRST TaskType NO-LOCK /* idx=TypeCode */
        WHERE TaskType.TypeCode = "ShipPalletBuild" NO-ERROR.   
   IF NOT AVAILABLE TaskType THEN
   DO:
      RUN DisplayError("Record Not Found",
                       "[TaskType] [ShipPalletBuild] does not exist.").
      LEAVE.            
   END. /*IF NOT AVAILABLE TaskType THEN*/
   
   /* Check if Using Nota Fiscal After Packout */
   FIND FIRST PackoutConfig NO-LOCK NO-ERROR.
   
   IF AVAILABLE PackoutConfig AND PackoutConfig.UsingNotaFiscalAfterPackout THEN
   DO:
      /* Get the NF Location Storage Group */
      FIND FIRST LocationTypeGroup NO-LOCK /* idx=GroupCode */
           WHERE LocationTypeGroup.GroupCode = "AwaitingNFLocations" NO-ERROR.
      IF NOT AVAILABLE LocationTypeGroup THEN
      DO:
         RUN DisplayError("Record not found",
                          "Location Type Group [AwaitingNFLocations] does not exist.").
         LEAVE.
      END. /*IF NOT AVAILABLE LocationTypeGroup THEN*/
   END. /*IF AVAILABLE PackoutConfig AND PackoutConfig.UsingNotaFiscalAfterPackout THEN*/
   
   /* Set Global Operation for History */
   intGblOperationTypeID = OperationType.OperationTypeID.   

   chrSsnGetOptions:setValue("UsingNotaFiscalAfterPackout").
   
   intSsnTaskTypeID:setValue(TaskType.TaskTypeID).

   Main_Block:
   DO ON ERROR UNDO, LEAVE:
   
      /* No Result Required */
      chrResult = "Continue".
   
   END. /*Main_Block:*/   

   /* Clean Up */
   DELETE OBJECT chrSsnGetOptions NO-ERROR.
   DELETE OBJECT intSsnTaskTypeID NO-ERROR.

   /* Releases */
   RELEASE OperationType        NO-ERROR.
   RELEASE ShipOrderStatusGroup NO-ERROR.
   RELEASE TaskType             NO-ERROR.
   RELEASE LocationTypeGroup    NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /*DO ON STOP UNDO, RETRY:*/
