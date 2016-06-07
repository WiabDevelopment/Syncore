/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateCountTaskLocationSkipped.p
Purpose : Updates the CountTaskLocation Completed Field and status of Skipped

          Possible Results : Continue

Author  : Mateusz Nogaj
Date    : 20/05/2014
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
   {fncDateFunctions.i} 
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}  

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   DEFINE VARIABLE updCountTaskLocation AS updRecord NO-UNDO.

   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   
   /* Buffers */
   DEFINE BUFFER readPrevCountTaskLocation FOR CountTaskLocation.
   
   /* Local Variables */
   DEFINE VARIABLE intSkippedCountTaskStatus AS INTEGER   NO-UNDO.
   
   intSkippedCountTaskStatus = fGetStatusID("CountTask", "Skipped").

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
      
      IF CountTaskLocation.AssignedTo <> intGblUserID  
         AND CountTaskLocation.AssignedTo <> 0 THEN
      DO:
         RUN DisplayMessage("Count In Process",
                            "Selected Task for Location " + Location.LocationRef + " is in process for another user.  Please try again.").
         LEAVE Main_Block.
      END.
      
      FIND FIRST CountGroup NO-LOCK 
         WHERE CountGroup.CountGroupID = CountTask.CountGroupID NO-ERROR.
      IF NOT AVAILABLE CountGroup THEN
      DO:
         RUN DisplayMessage("Record Not Found",
                            "[CountGroup] ID [" + STRING(CountTask.CountGroupID) + "] does not exist.").
         LEAVE Main_Block.
      END.
      
      /* If This Task is for Recount - No skipping */
      IF CountTask.CountGroupRecountID > 0 THEN 
      DO:
         chrResult = "Continue".
         LEAVE Main_Block.
      END.
      
      /* Find previous completed for this Location, that was not skipped */   
      PrevCountLocationLoop:
      FOR EACH readPrevCountTaskLocation NO-LOCK
         WHERE readPrevCountTaskLocation.LocationID = CountTaskLocation.LocationID
         AND   readPrevCountTaskLocation.Completed > "" /* Cannot be blank as fInterval will cause runtime error */
         AND   readPrevCountTaskLocation.CountTaskStatusID <> intSkippedCountTaskStatus
         BY    readPrevCountTaskLocation.Completed DESCENDING: 
            
         /* Skip counting this location again if last completed is less than frequency */
         IF fInterval(INPUT fTimeStamp(NOW), INPUT readPrevCountTaskLocation.Completed, INPUT "days") < CountGroup.Frequency THEN
         DO:

            /* Set this to be "Skipped" Status and update Completed fields */
            updCountTaskLocation = fGetRecord("CountTaskLocation", CountTaskLocation.CountTaskLocationID).
      
            /* Check for WebLocks */
            IF updCountTaskLocation:RecordLocked THEN
            DO:
               RUN DisplayError("Record Locked",
                                 updCountTaskLocation:getErrors()).
               UNDO Main_Block, LEAVE Main_Block.
            END.
      
            updCountTaskLocation:assignField("Completed", fTimeStamp(NOW)).
            updCountTaskLocation:assignField("CountTaskStatusID", intSkippedCountTaskStatus).
            
            /* Error Check Update */
            chrError = updCountTaskLocation:getErrors().
      
            IF chrError <> "" THEN DO:      
               RUN DisplayError("Record Update Error",
                                 chrError).
               UNDO Main_Block, LEAVE Main_Block.
            END. 
            DELETE OBJECT updCountTaskLocation NO-ERROR.       
            
         END.    
         LEAVE PrevCountLOcationLoop.
         
      END. /*  FOR EACH readPrevCountTaskLocation */

      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.   
   DELETE OBJECT updCountTaskLocation NO-ERROR.

   /* Releases */
   RELEASE CountGroupType            NO-ERROR.
   RELEASE CountTask                 NO-ERROR.
   RELEASE CountTaskLocation         NO-ERROR.
   RELEASE CountTaskType             NO-ERROR.
   RELEASE GateUser                  NO-ERROR.
   RELEASE Location                  NO-ERROR.
   RELEASE readPrevCountTaskLocation NO-ERROR.
   RELEASE CountGroup                NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.