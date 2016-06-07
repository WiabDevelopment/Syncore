/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCreateRecountAuditTasks.p
Purpose : Create Recount Audit Tasks

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
   
   /* Counting Procedures */
   {prcCountProcedures.i}
   
   /* Local Variables */  
   DEFINE VARIABLE intExistingRecountSequence AS INTEGER NO-UNDO.   
   DEFINE VARIABLE logWrongLocationFound      AS LOGICAL NO-UNDO.
   DEFINE VARIABLE intCountTaskID             AS INTEGER NO-UNDO.
   DEFINE VARIABLE intCompleteCountTask       AS INTEGER NO-UNDO.

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   DEFINE VARIABLE newCountTaskType     AS newRecord NO-UNDO.
   DEFINE VARIABLE newCountTask         AS newRecord NO-UNDO.
   DEFINE VARIABLE newCountTaskLocation AS newRecord NO-UNDO.
   
   /* Buffer */
   DEFINE BUFFER newCountGroupRecount FOR CountGroupRecount.
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   
   intCompleteCountTask = fGetStatusID("CountTask", "Complete").

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
      
      FIND FIRST CountGroup OF CountTask NO-ERROR.   
      IF NOT AVAILABLE CountGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountGroup ID] [" + STRING(CountTask.CountGroupID) + "] does not Exist.").
         LEAVE Main_Block.
      END.
       
      FIND FIRST CountGroupRecount OF CountTask NO-ERROR.   
      IF NOT AVAILABLE CountGroupRecount THEN
      DO:
         intExistingRecountSequence = 0.
      END.
      ELSE 
      DO:
         intExistingRecountSequence = CountGroupRecount.CountSequence.
      END.
         
      
      /* Finding Recounts with CountSequence/ID higher than current */
      FOR EACH newCountGroupRecount OF CountGroup NO-LOCK 
         WHERE newCountGroupRecount.CountSequence >= intExistingRecountSequence
         AND   newCountGroupRecount.Active = TRUE
         AND   newCountGroupRecount.CountGroupRecountID <> CountGroupRecount.CountGroupRecountID
         BY    newCountGroupRecount.CountSequence:
            
            LEAVE.
            
      END. 
      
      /* If No Recount found - No regenerating new */
      IF NOT AVAILABLE newCountGroupRecount THEN
      DO:
         chrResult = "Continue".
         LEAVE Main_Block.
      END.

      WrongCountPartLoop:
      FOR EACH CountTaskLocation OF CountTask NO-LOCK
         WHERE CountTaskLocation.Completed  <> ""
         AND   CountTaskLocation.CountTaskStatusID = intCompleteCountTask,
         EACH CountTaskLocationPart OF CountTaskLocation NO-LOCK
            WHERE CountTaskLocationPart.QtyExpected <> CountTaskLocationPart.QtyCounted:
            
            logWrongLocationFound = YES.
            LEAVE WrongCountPartLoop.
         
      END. /* FOR EACH CountTaskLocation */ 

      IF logWrongLocationFound = NO THEN 
      DO:          
         WrongCountPackageLoop:
         FOR EACH CountTaskLocation OF CountTask NO-LOCK
            WHERE CountTaskLocation.Completed  <> ""
            AND   CountTaskLocation.CountTaskStatusID = intCompleteCountTask,
            EACH CountTaskLocationPackage OF CountTaskLocation NO-LOCK
               WHERE CountTaskLocationPackage.QtyExpected <> CountTaskLocationPackage.QtyCounted:
               
               logWrongLocationFound = YES.
               LEAVE WrongCountPackageLoop.
            
         END. /* FOR EACH CountTaskLocation */ 
      END.

      /* If Not found not matching location then leave without generating Recount */
      IF logWrongLocationFound = NO THEN
      DO: 
         chrResult = "Continue".
         LEAVE Main_Block.
      END.

      /* Create a single CountTask and multiple CountTaskLocations 
         for each of the Locations that were counted not as expected */
      newCountTask = fCreateRecord("CountTask").

      newCountTask:assignField("CountTaskTypeID"    ,newCountGroupRecount.CountTaskTypeID).
      newCountTask:assignField("CountGroupID"       ,CountGroup.CountGroupID).
      newCountTask:assignField("BusinessUnitID"     ,CountGroup.BusinessUnitID).
      newCountTask:assignField("CountGroupTypeID"   ,CountGroup.CountGroupTypeID).
      newCountTask:assignField("BlindCount"         ,CountTask.BlindCount).
      newCountTask:assignField("Priority"           ,newCountGroupRecount.Priority).
      newCountTask:assignField("CountGroupRecountID",newCountGroupRecount.CountGroupRecountID).
      newCountTask:assignField("CountTaskStatusID"  ,fGetStatusID("CountTask", "Available")). 

      chrError = chrError + newCountTask:getErrors().

      /* Error Check */
      IF chrError <> "" THEN
      DO:
         RUN DisplayMessage("Create Failed",
                            chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      /* Set this output parameter to return the CountTaskID to the calling program */
      intCountTaskID = newCountTask:NewRecordUniqueID.
      
      DELETE OBJECT newCountTask NO-ERROR. 
         
      FOR EACH CountTaskLocation OF CountTask NO-LOCK 
         WHERE CountTaskLocation.Completed  <> ""
         AND   CountTaskLocation.CountTaskStatusID = intCompleteCountTask,
         FIRST CountTaskLocationPart OF CountTaskLocation NO-LOCK
            WHERE CountTaskLocationPart.QtyExpected <> CountTaskLocationPart.QtyCounted:
               
         newCountTaskLocation = fCreateRecord("CountTaskLocation").

         newCountTaskLocation:assignField("LocationID",CountTaskLocation.LocationID).
         newCountTaskLocation:assignField("CountTaskID",intCountTaskID).
         newCountTaskLocation:assignField("Priority",CountTaskLocation.Priority).
         newCountTaskLocation:assignField("AssignedTo",0).
         newCountTaskLocation:assignField("BlindCount",CountTaskLocation.BlindCount).
         newCountTaskLocation:assignField("CountTaskStatusID",fGetStatusID("CountTask", "Available")). 

         chrError = chrError + newCountTaskLocation:getErrors().

        /* Error Check */
         IF chrError <> "" THEN
         DO:
            RUN DisplayMessage("Create Failed",
                               chrError).
            UNDO Main_Block, LEAVE Main_Block.
         END.
            
         DELETE OBJECT newCountTaskLocation NO-ERROR.     
         
      END. /* FOR EACH CountTaskLocation */
      
      FOR EACH CountTaskLocation OF CountTask NO-LOCK
         WHERE CountTaskLocation.Completed  <> ""
         AND   CountTaskLocation.CountTaskStatusID = intCompleteCountTask,
          FIRST CountTaskLocationPackage OF CountTaskLocation NO-LOCK
            WHERE CountTaskLocationPackage.QtyExpected <> CountTaskLocationPackage.QtyCounted:
            
         newCountTaskLocation = fCreateRecord("CountTaskLocation").

         newCountTaskLocation:assignField("LocationID",CountTaskLocation.LocationID).
         newCountTaskLocation:assignField("CountTaskID",intCountTaskID).
         newCountTaskLocation:assignField("Priority",CountTaskLocation.Priority).
         newCountTaskLocation:assignField("AssignedTo",0).
         newCountTaskLocation:assignField("BlindCount",CountTaskLocation.BlindCount).
         newCountTaskLocation:assignField("CountTaskStatusID",fGetStatusID("CountTask", "Available")).

         chrError = chrError + newCountTaskLocation:getErrors().

         /* Error Check */
         IF chrError <> "" THEN
         DO:
            RUN DisplayMessage("Create Failed",
                               chrError).
            UNDO Main_Block, LEAVE Main_Block.
         END.
         
         DELETE OBJECT newCountTaskLocation NO-ERROR.     
         
      END. /* FOR EACH CountTaskLocation */
      
      /* Update Sucessfully Completed */
      chrResult = "Continue".

   END. /* Main_Block: */   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   DELETE OBJECT newCountTask         NO-ERROR.
   DELETE OBJECT newCountTaskType     NO-ERROR.
   DELETE OBJECT newCountTaskLocation NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation NO-ERROR.
   RELEASE CountTask         NO-ERROR.
   RELEASE CountTaskType     NO-ERROR.
   RELEASE CountGroup        NO-ERROR.
   RELEASE CountGroupRecount NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
