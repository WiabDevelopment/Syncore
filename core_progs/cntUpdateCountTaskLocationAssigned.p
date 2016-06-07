/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateCountTaskLocationAssigned.p
Purpose : Updates the CountTaskLocation User Assigned and Status to InProcess for a Count Task

          Possible Results : Continue

Author  : Christopher Shelley
Date    : 28/04/2014
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
   
   /* Optional Includes */  
   {fncStatusTypeFunctions.i} 
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Local Variables */     

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   DEFINE VARIABLE updCountTaskLocation AS updRecord NO-UNDO.

   /* Get Current Session Data */
   intSsnTaskLocationID  = fGetSessionValue("TaskLocationID").

   Main_Block:
   DO ON ERROR UNDO:

      /* Validate Session Data */
      FIND FIRST CountTaskLocation NO-LOCK
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Task Location ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST CountTask OF CountTaskLocation NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountTask THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTask ID] [" + STRING(CountTaskLocation.CountTaskID) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST CountTaskType OF CountTask NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountTaskType THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskType ID] [" + STRING(CountTask.CountTaskTypeID) + "] does not Exist.").
         LEAVE Main_Block.
      END.
      
      FIND FIRST CountGroupType OF CountTask NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountGroupType THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountGroupType ID] [" + STRING(CountTask.CountGroupTypeID) + "] does not Exist.").
         LEAVE Main_Block.
      END.
            
      /* Check Count Status ID also?  */
      
       FIND FIRST Location NO-LOCK 
          WHERE Location.LocationID = CountTaskLocation.LocationID NO-ERROR.
       IF NOT AVAILABLE Location THEN
       DO: 
          RUN DisplayError("Record Not Found",
                           "[Location ID] [" + STRING(CountTaskLocation.LocationID) + "] does not Exist.").
          LEAVE Main_Block.
      END.

      FIND FIRST GateUser NO-LOCK
         WHERE GateUser.GateUserID = intGblUserID NO-ERROR.
      IF NOT AVAILABLE GateUser THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[GateUser ID] [" + STRING(intGblUserID) + "] does not Exist.").
         LEAVE Main_Block.
      END.   

      /* Check CountTaskLocation is NOT Complete */
      IF CountTaskLocation.Completed <> "" THEN
      DO:
         RUN DisplayMessage("Record Completed",
                            "Selected Task for Location " + Location.LocationRef + " has been Completed.  Please try again.").
         LEAVE Main_Block.
      END.
      
      /* Checking if the Task is assigned to someone else */
      IF CountTaskLocation.AssignedTo > 0 AND CountTaskLocation.AssignedTo <> intGblUserID THEN
      DO:
         RUN DisplayMessage("Count In Process",
                            "Selected Task for Location " + Location.LocationRef + " is already in process.  Please try again.").
         LEAVE Main_Block.
      END.

      /* Get the CountTaskLocation Record and Update AssignedTo field */
      updCountTaskLocation = fGetRecord("CountTaskLocation", intSsnTaskLocationID:intValue).

      /* Check for WebLocks */
      IF updCountTaskLocation:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updCountTaskLocation:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      updCountTaskLocation:assignField("AssignedTo", intGblUserID).
      updCountTaskLocation:assignField("CountTaskStatusID", fGetStatusID("CountTask", "InProcess")).
      
      /* Error Check Update */
      chrError = updCountTaskLocation:getErrors().

      IF chrError <> "" THEN DO:      
         RUN DisplayError("Record Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   
   DELETE OBJECT updCountTaskLocation NO-ERROR.

   /* Releases */
   RELEASE CountGroupType    NO-ERROR.
   RELEASE CountTask         NO-ERROR.
   RELEASE CountTaskLocation NO-ERROR.
   RELEASE CountTaskType     NO-ERROR.
   RELEASE GateUser          NO-ERROR.
   RELEASE Location          NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.