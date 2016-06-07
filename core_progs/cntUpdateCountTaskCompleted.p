/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntUpdateCountTaskCompleted.p
Purpose : Updates the CountTask Completed Field

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
   DEFINE VARIABLE intTaskLocationsTotalQty   AS INTEGER NO-UNDO.
   DEFINE VARIABLE intTaskLocationsSkippedQty AS INTEGER NO-UNDO.
   DEFINE VARIABLE intSkippedCountTaskStatus  AS INTEGER NO-UNDO.
      
   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID          AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskID                  AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnGroupName               AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnGroupFrequency          AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskLocationsSkippedQty AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskLocationsTotalQty   AS sessionValue NO-UNDO.
       
   /* DB Objects */   
   DEFINE VARIABLE updCountTask         AS updRecord NO-UNDO.
   DEFINE VARIABLE updCountTaskUserLink AS updRecord NO-UNDO.
   
   /* Buffers */
   DEFINE BUFFER skippedCountTaskLocation FOR CountTaskLocation.
   
   fClearSessionValue("TaskID").
   fClearSessionValue("GroupName").
   fClearSessionValue("GroupFrequency").
   fClearSessionValue("TaskLocationsSkippedQty").
   fClearSessionValue("TaskLocationsTotalQty").

   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   
   /* Create New Session Data */
   intSsnTaskID                  = fNewSessionValue("TaskID").
   intSsnGroupName               = fNewSessionValue("GroupName").
   intSsnGroupFrequency          = fNewSessionValue("GroupFrequency").
   intSsnTaskLocationsSkippedQty = fNewSessionValue("TaskLocationsSkippedQty").
   intSsnTaskLocationsTotalQty   = fNewSessionValue("TaskLocationsTotalQty").
      
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
      
      FIND FIRST CountGroup OF CountTask NO-ERROR.   
      IF NOT AVAILABLE CountGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountGroup ID] [" + STRING(CountTask.CountGroupID) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      /* Get the CountTask Record and Update AssignedTo field */
      updCountTask = fGetRecord("CountTask", CountTaskLocation.CountTaskID).

      /* Check for WebLocks */
      IF updCountTask:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updCountTask:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.

      updCountTask:assignField("Completed", fTimeStamp(NOW)).
      updCountTask:assignField("CountTaskStatusID", fGetStatusID("CountTask", "Complete")).
      
      /* Error Check Update */
      chrError = updCountTask:getErrors().

      IF chrError <> "" THEN DO:      
         RUN DisplayError("Record Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      UserLinkLoop:
      FOR EACH CountTaskUserLink OF CountTask NO-LOCK  /* idx=CoutTaskIdCompleted */
         WHERE CountTaskUserLink.Completed = "":

         /* Get the CountTask Record and Update AssignedTo field */
         updCountTaskUserLink = fGetRecord("CountTaskUserLink", CountTaskUserLink.CountTaskUserLinkID).
   
         /* Check for WebLocks */
         IF updCountTaskUserLink:RecordLocked THEN
         DO:
            RUN DisplayError("Record Locked",
                              updCountTaskUserLink:getErrors()).
            UNDO Main_Block, LEAVE Main_Block.
         END.
   
         updCountTaskUserLink:assignField("Completed", fTimeStamp(NOW)).
         
         /* Error Check Update */
         chrError = updCountTaskUserLink:getErrors().
   
         IF chrError <> "" THEN DO:      
            RUN DisplayError("Record Update Error",
                              chrError).
            UNDO Main_Block, LEAVE Main_Block.
         END.
         DELETE OBJECT updCountTaskUserLink NO-ERROR.
         
      END. /*UserLinkLoop */  
      
      /* Counting Number of skipped and total location in a task */ 
      FOR EACH skippedCountTaskLocation OF CountTask NO-LOCK:
         IF skippedCountTaskLocation.CountTaskStatus = intSkippedCountTaskStatus THEN 
         DO:
            intTaskLocationsSkippedQty = intTaskLocationsSkippedQty + 1.
         END.
         intTaskLocationsTotalQty   = intTaskLocationsTotalQty + 1. 
      END.
      
      /* Set Session Value */
      intSsnTaskID:setValue(CountTask.CountTaskID).
      intSsnGroupName:setValue(CountGroup.CountGroupName).
      intSsnGroupFrequency:setValue(CountGroup.Frequency).
      intSsnTaskLocationsSkippedQty:setValue(intTaskLocationsSkippedQty).
      intSsnTaskLocationsTotalQty:setValue(intTaskLocationsTotalQty).

      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID          NO-ERROR.   
   DELETE OBJECT updCountTask                  NO-ERROR.
   DELETE OBJECT intSsnTaskID                  NO-ERROR.
   DELETE OBJECT intSsnGroupName               NO-ERROR.
   DELETE OBJECT intSsnGroupFrequency          NO-ERROR.
   DELETE OBJECT intSsnTaskLocationsSkippedQty NO-ERROR.
   DELETE OBJECT intSsnTaskLocationsTotalQty   NO-ERROR.

   /* Releases */
   RELEASE CountTask                NO-ERROR.
   RELEASE CountTaskLocation        NO-ERROR.
   RELEASE CountGroup               NO-ERROR.
   RELEASE skippedCountTaskLocation NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.