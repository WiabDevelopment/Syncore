/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunDetrash.p
Purpose : Starts the Detrash process

          Possible Results : Continue

Author  : MC
Date    : 22nd Nov 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
06/06/2014 BR  All        CR 1052 Standardization Project
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
   
   /* Clear the Session */
   fClearSessionValues("").
   
   /* Session Objects */
   DEFINE VARIABLE chrSsnUserPrompt AS sessionValue NO-UNDO.
   
   /* Create New Session Data */
   chrSsnUserPrompt = fNewSessionValue("StockPackageUserPrompt").
   
   /* Set UserPrompt Session value */
   chrSsnUserPrompt:setValue(fTL("Scan Package to Detrash")).
   
   /* Get the Operation Type for the Process */
   FIND OperationType NO-LOCK
      WHERE OperationType.TypeCode = "Detrash" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN
   DO:
      RUN DisplayError("Record Not Found",
                       "Operation Type [Detrash] does not exist.").
      RETURN.
   END.

   intGblOperationTypeID = OperationType.OperationTypeID.
   
   Main_Block:
   DO ON ERROR UNDO:
      
      /* Find Groups */
      FIND FIRST StockStatusGroup NO-LOCK
         WHERE StockStatusGroup.GroupCode = "DetrashableStatuses" NO-ERROR.
      IF NOT AVAILABLE StockStatusGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "Stock Status Group [DetrashableStatuses] does not exist.").
         LEAVE.
      END.
      
      FIND FIRST LocationTypeGroup NO-LOCK
         WHERE LocationTypeGroup.GroupCode = "DetrashLocations" NO-ERROR.
      IF NOT AVAILABLE LocationTypeGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "LocationType Group [DetrashLocations] does not exist.").
         LEAVE.
      END.
      
      chrResult = "CONTINUE".
      
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT chrSsnUserPrompt NO-ERROR.
   
   /* Releases */
   RELEASE OperationType     NO-ERROR.
   RELEASE StockStatusGroup  NO-ERROR.
   RELEASE LocationTypeGroup NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */
