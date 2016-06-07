/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncTaskFunctions.i
Purpose : All functions to do with Tasks
Author  : BG
Date    : 16th Oct 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

FUNCTION fCanAssignCompleteTaskToUser RETURNS CHARACTER (INPUT intTaskID     AS INTEGER,
                                                         INPUT intGateUserID AS INTEGER):
   
   DEFINE VARIABLE logTaskRuleForUserFound       AS LOGICAL     NO-UNDO.
   
   DEFINE BUFFER readTask                        FOR Task.
   DEFINE BUFFER readTaskStatus                  FOR TaskStatus.
   DEFINE BUFFER readTaskLine                    FOR TaskLine.
   DEFINE BUFFER readTaskLineWork                FOR TaskLineWork.
   DEFINE BUFFER readTaskType                    FOR TaskType.
   DEFINE BUFFER readGateUser                    FOR GateUser.
   DEFINE BUFFER readLocation                    FOR Location.
   DEFINE BUFFER readTaskExecutionRule           FOR TaskExecutionRule.
   DEFINE BUFFER readTaskEquipmentUserLink       FOR TaskEquipmentUserLink.
   DEFINE BUFFER readTaskTypeEquipToteTypeLink   FOR TaskTypeEquipToteTypeLink.
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = intTaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(intTaskID) + "].".
   
   FIND FIRST readGateUser NO-LOCK 
      WHERE readGateUser.GateUserID = intGateUserID NO-ERROR.
   IF NOT AVAILABLE readGateUser THEN
      RETURN "Error: No GateUser exists for ID:[" + STRING(intGateUserID) + "].".
   
   FIND FIRST readTaskStatus OF readTask NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTaskStatus THEN
      RETURN "Error: No TaskStatus exists for TaskID:[" + STRING(readTask.TaskID) + "].".
   
   FIND FIRST readTaskType OF readTask NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTaskType THEN
      RETURN "Error: No TaskType exists for TaskID:[" + STRING(readTask.TaskID) + "].".
   
   IF readTaskStatus.StatusCode <> "Available" THEN
      RETURN "Error: Task must be at Available Status to assign a complete Task. ".
   
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readLocation OF readTaskLineWork NO-LOCK:
         
         FIND FIRST readTaskStatus OF readTaskLineWork NO-LOCK NO-ERROR.
         IF NOT AVAILABLE readTaskStatus THEN
            RETURN "Error: No valid TaskStatus assigned to TaskLineWorkID:[" + STRING(readTaskLineWork.TaskLineWorkID) + "].".
         
         IF readTaskStatus.StatusCode <> "Available" THEN
            RETURN "All TaskWork must be at Available Status to assign a complete Task.".
         
         IF readTaskLineWork.QtyCompleted > 0 THEN
            RETURN "All TaskWork must have QtyCompleted = 0 to assign a complete Task.".
         
         logTaskRuleForUserFound = FALSE.
         
         TaskRulesBlk:
         FOR EACH readTaskExecutionRule NO-LOCK
            WHERE readTaskExecutionRule.LocationTypeID      = readLocation.LocationTypeID
            AND   readTaskExecutionRule.StockEntityID       = readTaskLineWork.StockEntityID
            AND   readTaskExecutionRule.TaskExecutionTypeID = readTaskLineWork.TaskExecutionTypeID
            AND   readTaskExecutionRule.Active:
            
            FOR EACH readTaskTypeEquipToteTypeLink OF readTaskExecutionRule NO-LOCK
               WHERE readTaskTypeEquipToteTypeLink.TaskTypeID = readTaskType.TaskTypeID
               AND   readTaskTypeEquipToteTypeLink.Active
               BREAK BY readTaskTypeEquipToteTypeLink.TaskEquipmentID:
               
               IF FIRST-OF(readTaskTypeEquipToteTypeLink.TaskEquipmentID) THEN
               DO:
                  FOR EACH readTaskEquipmentUserLink NO-LOCK
                     WHERE readTaskEquipmentUserLink.TaskEquipmentID = readTaskTypeEquipToteTypeLink.TaskEquipmentID
                     AND   readTaskEquipmentUserLink.GateUserID      = intGateUserID
                     AND   readTaskEquipmentUserLink.Active: 
                     
                     logTaskRuleForUserFound = TRUE.
                     LEAVE TaskRulesBlk.
                     
                  END. /*FOR EACH readTaskEquipmentUserLink NO-LOCK*/
               END. /*IF FIRST-OF(readTaskTypeEquipToteTypeLink.TaskEquipmentID) THEN*/
            END. /*FOR EACH readTaskTypeEquipToteTypeLink OF readTaskType NO-LOCK*/
         END. /*TaskRulesBlk: FOR EACH readTaskExecutionRule OF TaskTypeEquipToteTypeLink NO-LOCK*/
         
         IF logTaskRuleForUserFound = FALSE THEN
            RETURN "There is [TaskWork] on this [Task] that this User cannot Process, cannot assign Complete Task.".
         
   END. /*FOR EACH readTaskLineWork OF readTask NO-LOCK*/
   
   RETURN "Yes".
   
END FUNCTION. /* fCanAssignCompleteTaskToUser */


FUNCTION fCanAssignTaskLineWorkToUser RETURNS CHARACTER (INPUT intTaskLineWorkID AS INTEGER,
                                                         INPUT intGateUserID     AS INTEGER):
   
   DEFINE VARIABLE logTaskRuleForUserFound       AS LOGICAL     NO-UNDO.
   
   DEFINE BUFFER readTask                        FOR Task.
   DEFINE BUFFER readTaskLine                    FOR TaskLine.
   DEFINE BUFFER readTaskLineWork                FOR TaskLineWork.
   DEFINE BUFFER readTaskType                    FOR TaskType.
   DEFINE BUFFER readTaskStatus                  FOR TaskStatus.
   DEFINE BUFFER readGateUser                    FOR GateUser.
   DEFINE BUFFER readLocation                    FOR Location.
   DEFINE BUFFER readTaskExecutionRule           FOR TaskExecutionRule.
   DEFINE BUFFER readTaskEquipmentUserLink       FOR TaskEquipmentUserLink.
   DEFINE BUFFER readTaskTypeEquipToteTypeLink   FOR TaskTypeEquipToteTypeLink.
   
   FIND FIRST readTaskLineWork NO-LOCK 
      WHERE readTaskLineWork.TaskLineWorkID = intTaskLineWorkID NO-ERROR.
   IF NOT AVAILABLE readTaskLineWork THEN
      RETURN "Error: No TaskLineWork exists for TaskLineWorkID:[" + STRING(intTaskLineWorkID) + "].".
   
   FIND FIRST readTaskLine OF readTaskLineWork NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTaskLine THEN
      RETURN "Error: No TaskLine exists for TaskLineID:[" + STRING(readTaskLineWork.TaskLineID) + "].".
   
   FIND FIRST readTask OF readTaskLine NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(readTaskLine.TaskID) + "].".
   
   FIND FIRST readGateUser NO-LOCK 
      WHERE readGateUser.GateUserID = intGateUserID NO-ERROR.
   IF NOT AVAILABLE readGateUser THEN
      RETURN "Error: No GateUser exists for ID:[" + STRING(intGateUserID) + "].".
   
   FIND FIRST readTaskStatus OF readTask NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTaskStatus THEN
      RETURN "Error: No TaskStatus exists for TaskID:[" + STRING(readTask.TaskID) + "].".
   
   FIND FIRST readTaskType OF readTask NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTaskType THEN
      RETURN "Error: No TaskType exists for TaskID:[" + STRING(readTask.TaskID) + "].".
   
   IF readTaskStatus.StatusCode <> "Available" THEN
      RETURN "Error: Task must be at Available Status to assign a complete Task. ".
   
   FOR EACH readLocation OF readTaskLineWork NO-LOCK:
      
      FIND FIRST readTaskStatus OF readTaskLineWork NO-LOCK NO-ERROR.
      IF NOT AVAILABLE readTaskStatus THEN
         RETURN "Error: No valid TaskStatus assigned to TaskLineWorkID:[" + STRING(readTaskLineWork.TaskLineWorkID) + "].".
      
      IF readTaskStatus.StatusCode <> "Available" THEN
         RETURN "All TaskWork must be at Available Status to assign a complete Task.".
      
      IF readTaskLineWork.QtyCompleted > 0 THEN
         RETURN "All TaskWork must have QtyCompleted = 0 to assign a complete Task.".
      
      logTaskRuleForUserFound = FALSE.
      
      TaskRulesBlk:
      FOR EACH readTaskExecutionRule NO-LOCK
         WHERE readTaskExecutionRule.LocationTypeID      = readLocation.LocationTypeID
         AND   readTaskExecutionRule.StockEntityID       = readTaskLineWork.StockEntityID
         AND   readTaskExecutionRule.TaskExecutionTypeID = readTaskLineWork.TaskExecutionTypeID
         AND   readTaskExecutionRule.Active:
         
         FOR EACH readTaskTypeEquipToteTypeLink OF readTaskExecutionRule NO-LOCK
            WHERE readTaskTypeEquipToteTypeLink.TaskTypeID = readTaskType.TaskTypeID
            AND   readTaskTypeEquipToteTypeLink.Active
            BREAK BY readTaskTypeEquipToteTypeLink.TaskEquipmentID:
            
            IF FIRST-OF(readTaskTypeEquipToteTypeLink.TaskEquipmentID) THEN
            DO:
               FOR EACH readTaskEquipmentUserLink NO-LOCK
                  WHERE readTaskEquipmentUserLink.TaskEquipmentID = readTaskTypeEquipToteTypeLink.TaskEquipmentID
                  AND   readTaskEquipmentUserLink.GateUserID      = intGateUserID
                  AND   readTaskEquipmentUserLink.Active: 
                  
                  logTaskRuleForUserFound = TRUE.
                  LEAVE TaskRulesBlk.
                  
               END. /*FOR EACH readTaskEquipmentUserLink NO-LOCK*/
            END. /*IF FIRST-OF(readTaskTypeEquipToteTypeLink.TaskEquipmentID) THEN*/
         END. /*FOR EACH readTaskTypeEquipToteTypeLink OF readTaskType NO-LOCK*/
      END. /*TaskRulesBlk: FOR EACH readTaskExecutionRule OF TaskTypeEquipToteTypeLink NO-LOCK*/
      
      IF logTaskRuleForUserFound = FALSE THEN
         RETURN "This [TaskWork] cannot be processed by the selected User, cannot assign.".
         
   END. /*FOR EACH readTaskLineWork OF readTask NO-LOCK*/
   
   RETURN "Yes".
   
END FUNCTION. /* fCanAssignTaskLineWorkToUser */


FUNCTION fHasTaskBegun RETURNS CHARACTER (INPUT intTaskID AS INTEGER):
   
   DEFINE BUFFER readTask                  FOR Task.
   DEFINE BUFFER readTaskStatus            FOR TaskStatus.
   DEFINE BUFFER readTaskLine              FOR TaskLine.
   DEFINE BUFFER readTaskLineWork          FOR TaskLineWork.
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = intTaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(intTaskID) + "].".
   
   FIND FIRST readTaskStatus OF readTask NO-LOCK NO-ERROR.
   IF NOT AVAILABLE readTaskStatus THEN
      RETURN "Error: No TaskStatus exists for TaskID:[" + STRING(readTask.TaskID) + "].".
   
   IF readTaskStatus.StatusCode = "InProcess" OR readTaskStatus.StatusCode = "Complete" THEN
      RETURN "Yes".
   
   /* Read through lines and see if any of them have begun */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK:
      
      FIND FIRST readTaskStatus OF readTaskLineWork NO-LOCK NO-ERROR.
      IF NOT AVAILABLE readTaskStatus THEN
         RETURN "Error: No TaskStatus exists for TaskLineWorkID:[" + STRING(readTaskLineWork.TaskLineWorkID) + "].".
      
      IF readTaskStatus.StatusCode = "InProcess" OR readTaskStatus.StatusCode = "Complete" THEN
         RETURN "Yes".
      
      IF readTaskLineWork.QtyCompleted > 0 THEN
         RETURN "Yes".
      
   END. /*FOR EACH TaskLineWork OF readTask NO-LOCK*/
   
   RETURN "No".
   
END FUNCTION. /* fHasTaskBegun */


FUNCTION fHasTaskBeenAssigned RETURNS CHARACTER (INPUT intTaskID AS INTEGER):
   
   DEFINE BUFFER readTask                  FOR Task.
   DEFINE BUFFER readTaskLine              FOR TaskLine.
   DEFINE BUFFER readTaskLineWork          FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkUserLink  FOR TaskLineWorkUserLink.
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = intTaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(intTaskID) + "].".
   
   /* Read through lines and see if any of them have begun */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkUserLink OF readTaskLineWork NO-LOCK:
         
         RETURN "Yes".
   END. /*FOR EACH TaskLineWork OF readTask NO-LOCK*/
   
   RETURN "No".
   
END FUNCTION. /* fHasTaskBeenAssigned */


FUNCTION fHasTaskBeenPreAssigned RETURNS CHARACTER (INPUT intTaskID AS INTEGER):
   
   DEFINE BUFFER readTask                  FOR Task.
   DEFINE BUFFER readTaskLine              FOR TaskLine.
   DEFINE BUFFER readTaskLineWork          FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkUserLink  FOR TaskLineWorkUserLink.
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = intTaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(intTaskID) + "].".
   
   /* Read through lines and see if any of them have begun */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkUserLink OF readTaskLineWork NO-LOCK
         WHERE readTaskLineWorkUserLink.PreAssignedBy <> 0:
         
         RETURN "Yes".
   END. /*FOR EACH TaskLineWork OF readTask NO-LOCK*/
   
   RETURN "No".
   
END FUNCTION. /* fHasTaskBeenPreAssigned */


FUNCTION fGetPreAssignedUserNameList RETURNS CHARACTER (INPUT intTaskID AS INTEGER):
   
   DEFINE BUFFER readTask                  FOR Task.
   DEFINE BUFFER readTaskLine              FOR TaskLine.
   DEFINE BUFFER readTaskLineWork          FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkUserLink  FOR TaskLineWorkUserLink.
   DEFINE BUFFER readGateUser              FOR GateUser.
   
   DEFINE VARIABLE chrUserList             AS CHARACTER   NO-UNDO.
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = intTaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(intTaskID) + "].".
   
   /* Read through lines and see if any of them have begun */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkUserLink OF readTaskLineWork NO-LOCK
         WHERE readTaskLineWorkUserLink.PreAssignedBy <> 0,
            EACH readGateUser OF readTaskLineWorkUserLink NO-LOCK:
            
            IF LOOKUP(readGateUser.FullName, chrUserList) = 0 THEN 
               chrUserList = chrUserList + readGateUser.FullName + ",".
            
   END. /*FOR EACH TaskLineWork OF readTask NO-LOCK*/
   
   chrUserList = TRIM(chrUserList, ",").
   
   RETURN chrUserList.
   
END FUNCTION. /* fGetPreAssignedUserList */


FUNCTION fGetPreAssignedUserIDList RETURNS CHARACTER (INPUT intTaskID AS INTEGER):
   
   DEFINE BUFFER readTask                  FOR Task.
   DEFINE BUFFER readTaskLine              FOR TaskLine.
   DEFINE BUFFER readTaskLineWork          FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkUserLink  FOR TaskLineWorkUserLink.
   DEFINE BUFFER readGateUser              FOR GateUser.
   
   DEFINE VARIABLE chrUserList             AS CHARACTER   NO-UNDO.
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = intTaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(intTaskID) + "].".
   
   /* Read through lines and see if any of them have begun */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readTaskLineWorkUserLink OF readTaskLineWork NO-LOCK
         WHERE readTaskLineWorkUserLink.PreAssignedBy <> 0,
            EACH readGateUser OF readTaskLineWorkUserLink NO-LOCK:
            
            IF LOOKUP(STRING(readGateUser.GateUserID), chrUserList) = 0 THEN 
               chrUserList = chrUserList + STRING(readGateUser.GateUserID) + ",".
            
   END. /*FOR EACH TaskLineWork OF readTask NO-LOCK*/
   
   chrUserList = TRIM(chrUserList, ",").
   
   RETURN chrUserList.
   
END FUNCTION. /* fGetPreAssignedUserList */


FUNCTION fGetTargetLocationList RETURNS CHARACTER (INPUT intTaskID AS INTEGER):
   
   DEFINE BUFFER readTask                  FOR Task.
   DEFINE BUFFER readTaskLine              FOR TaskLine.
   DEFINE BUFFER readTaskLineWork          FOR TaskLineWork.
   DEFINE BUFFER readTaskLineWorkUserLink  FOR TaskLineWorkUserLink.
   DEFINE BUFFER readLocation              FOR Location.
   
   DEFINE VARIABLE chrLocationList         AS CHARACTER   NO-UNDO.
   
   FIND FIRST readTask NO-LOCK 
      WHERE readTask.TaskID = intTaskID NO-ERROR.
   IF NOT AVAILABLE readTask THEN
      RETURN "Error: No Task exists for ID:[" + STRING(intTaskID) + "].".
   
   /* Read through lines and see if any of them have begun */
   ToteLoop:
   FOR EACH readTaskLine OF readTask NO-LOCK,
      EACH readTaskLineWork OF readTaskLine NO-LOCK,
         EACH readLocation WHERE readLocation.LocationID = readTaskLineWork.TargetLocationID NO-LOCK:
         
         IF LOOKUP(readLocation.LocationRef, chrLocationList) = 0 THEN 
            chrLocationList = chrLocationList + readLocation.LocationRef + ",".
         
   END. /*FOR EACH TaskLineWork OF readTask NO-LOCK*/
   
   chrLocationList = TRIM(chrLocationList, ",").
   
   RETURN chrLocationList.
   
END FUNCTION. /* fGetPreAssignedUserList */
