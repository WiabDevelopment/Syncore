/*------------------------------------------------------------------------------------------------------------------------------------------
Program : psRunShipRelocate.p
Purpose : Sets the initial data required for the Shipping Move to NF process to begin

          Possible Results: Continue

Author  : DCummins
Date    : 1st October 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character Mode Fixed Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i} 
   {fncGlobalFunctions.i}
   {fncStatusTypeFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}

   /* Clear out ALL Sesion Values */
   fClearSessionValues("").
   
   /* This record the data history Operation Type */
   FIND OperationType NO-LOCK
      WHERE OperationType.TypeCode = "Shipping" NO-ERROR.
   
   IF NOT AVAIL OperationType THEN
   DO:
      RUN DisplayMessage("Record not found",
                         "Operation Type [Shipping] does not exist.").
      RETURN.
   END.
   
   intGblOperationTypeID = OperationType.OperationTypeID.   
   
   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
   
      /* Get the Valid ShipStatusGroup */
      FIND FIRST ShipStatusGroup NO-LOCK
           WHERE ShipStatusGroup.GroupCode = "MoveNF" NO-ERROR.
      IF NOT AVAILABLE ShipStatusGroup THEN
      DO:
         RUN DisplayMessage("Record Not Found",
                            "Ship Status Group [MoveNF] does not exist.").
         LEAVE.
      END.
   
      /* Get the Valid ShipOrderStatusGroup */
      FIND FIRST ShipOrderStatusGroup NO-LOCK
           WHERE ShipOrderStatusGroup.GroupCode = "MoveNF" NO-ERROR.
      IF NOT AVAILABLE ShipOrderStatusGroup THEN 
      DO:
         RUN DisplayMessage("Record Not Found",
                            "Ship Order Status Group [MoveNF] does not exist.").
         LEAVE.
      END.

      /* Get the NF Location Storage Group */
      FIND FIRST LocationTypeGroup NO-LOCK
           WHERE LocationTypeGroup.GroupCode = "AwaitingNFLocations" NO-ERROR.
      IF NOT AVAILABLE LocationTypeGroup THEN 
      DO:
         RUN DisplayMessage("Record Not Found",
                            "Location Type Group [AwaitingNFLocations] does not exist.").
         LEAVE.
      END.

      /* No Result Required */
      chrResult = "CONTINUE".
   
   END.   
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.





