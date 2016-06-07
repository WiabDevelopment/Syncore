/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunPickingAndReplenish.p
Purpose : Initialise Program for Both Picking and Replenishment process. Operationtype is set after the Task is retrieved.

          Possible Results : Continue

Author  : Shane Conaty
Date    : 30/10/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
27/01/2014 BR  All        Replace added spaces with centring function
15/04/2014 BR  CR 1052    UI Standardization
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
   DEFINE VARIABLE chrSsnStockPackageUserPrompt AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskTypeGroupID        AS sessionValue NO-UNDO.

   /* Clear the Session */
   fClearSessionValues("").

   /* Create New Session Data */   
   intSsnTaskTypeGroupID        = fNewSessionValue("TaskTypeGroupID").
   chrSsnStockPackageUserPrompt = fNewSessionValue("StockPackageUserPrompt").

   Main_Block:
   DO ON ERROR UNDO:
   
      /* Get the Picking TaskType Group */
      FIND FIRST TaskTypeGroup NO-LOCK
         WHERE TaskTypeGroup.GroupCode = "ShipOrderPickandReplen" NO-ERROR.
      IF NOT AVAILABLE TaskTypeGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[TaskType Group] [ShipOrderPickandReplen] does not exist.").
         LEAVE.      
      END.

      /* Set Session Data */   
      intSsnTaskTypeGroupID:setValue(TaskTypeGroup.TaskTypeGroupID).   
      chrSsnStockPackageUserPrompt:setValue("Scan Package").
            
      /* No Result Required */
      chrResult = "Continue".

   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnTaskTypeGroupID        NO-ERROR.
   DELETE OBJECT chrSsnStockPackageUserPrompt NO-ERROR.

   /* Releases */
   RELEASE TaskTypeGroup NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */

