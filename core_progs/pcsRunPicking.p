/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunPicking.p
Purpose : Starts the Picking Process. Operation Type is set after the Task is retrieved.

          Possible results: Continue
          
Author  : Dcummins
Date    : 05/03/2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
01/04/2014 BR  CR 1052    UI Standardization
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
   DEFINE VARIABLE intSsnTaskTypeGroupID AS sessionValue NO-UNDO.

   /* Clear the Session */
   fClearSessionValues("").

   /* Create New Session Data */   
   intSsnTaskTypeGroupID = fNewSessionValue("TaskTypeGroupID").
         
   Main_Block:
   DO ON ERROR UNDO:

      /* Get the Picking TaskType Group */
      FIND FIRST TaskTypeGroup NO-LOCK
         WHERE TaskTypeGroup.GroupCode = "ShipOrderPick" NO-ERROR.
      IF NOT AVAILABLE TaskTypeGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[TaskType Group] [ShipOrderPick] does not exist.").
         LEAVE.      
      END.

      /* Set Session Data */      
      intSsnTaskTypeGroupID:setValue(TaskTypeGroup.TaskTypeGroupID).      
            
      /* No Result Required */
      chrResult = "Continue".
      
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnTaskTypeGroupID NO-ERROR.

   /* Releases */
   RELEASE TaskTypeGroup     NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */
