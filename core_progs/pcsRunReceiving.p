/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunReceiving.p
Purpose : Starts the Receiving Process and set the TaskTypeID Session value to Unload

          Possible outcomes : Continue.
          
Author  : DCummins
Date    : 29/01/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
27/01/2014 BR  All        Replace added spaces with centring function
12/03/2014 BR  All        Standardization Project
26/03/2014 BR  1052       Modify label passed to chrUserPrompt
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
   
   /* Local Variables */
   
   /* Clear the Session */
   fClearSessionValues("").
   
   /* Buffers */
   DEFINE BUFFER receivingOperationType FOR OperationType.
   
   /* Temp-Tables */
   DEFINE NEW GLOBAL SHARED TEMP-TABLE ttSerialScan LIKE SerialScan. 
   
   /* Always Empty a Global Temp-Table after defining it. */
   EMPTY TEMP-TABLE ttSerialScan.
   
   /* Session Objects */      
   DEFINE VARIABLE intSsnTaskTypeID                 AS sessionValue.
   DEFINE VARIABLE chrSsnUserPrompt                 AS sessionValue.
   DEFINE VARIABLE intSsnSerialisedUnitID           AS sessionValue.
   DEFINE VARIABLE intSsnSerialCreatedCount         AS sessionValue.
   DEFINE VARIABLE intSsnOperationTypeForSerialScan AS sessionValue NO-UNDO.
   
   
   /* Clear Session Values */
   fClearSessionValue("VerifyStockPackageUserPrompt").
   fClearSessionValue("TaskTypeID").
   fClearSessionValue("SerialisedUnitID").
   fClearSessionValue("SerialCreatedCount").
   
   /* Create New Session Data */   
   intSsnTaskTypeID                 = fNewSessionValue("TaskTypeID").
   chrSsnUserPrompt                 = fNewSessionValue("VerifyStockPackageUserPrompt").
   intSsnSerialisedUnitID           = fNewSessionValue("SerialisedUnitID").
   intSsnSerialCreatedCount         = fNewSessionValue("SerialCreatedCount").
   intSsnOperationTypeForSerialScan = fNewSessionValue("OperationTypeForSerialScan").  
   
   
   /* Set Session Data */
   chrSsnUserPrompt:setValue("Scan Label Applied").
   
   /* Get the Transaction Type for the Process */
   FIND OperationType NO-LOCK
      WHERE OperationType.TypeCode = "Receiving" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN
   DO:   
      RUN DisplayError("Record Not Found",
                       "[Operation Type] [Receiving] does not exist.").
      LEAVE.            
   END.
      
   intSsnOperationTypeForSerialScan:setValue(OperationType.OperationTypeID).

   /* Set Global Operation for History */
   intGblOperationTypeID = OperationType.OperationTypeID.      

   Main_Block:
   DO ON ERROR UNDO:
      
      /* Set Session Data */      
      intSsnTaskTypeID:setValue(fGetTypeID("Task", "Unload")).      

      /* No Result Required */
      chrResult = "Continue".
 
   END.

   /* Clean Up */      
   DELETE OBJECT intSsnTaskTypeID NO-ERROR.
   DELETE OBJECT chrSsnUserPrompt NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
