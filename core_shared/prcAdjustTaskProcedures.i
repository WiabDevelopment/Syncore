/*------------------------------------------------------------------------------------------------------------------------------------------
Program : prcAdjustTaskProcedures.i
Purpose : All procedures to do with FPP Task Adjustment on Logout
Author  : Michael Landess
Date    : 17/06/2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
24/08/2015 TAW CanonTLB   When the UserSEssionStationLogin records are completed, mark as NOT active as well.
16/12/2015 TAW CanonTLB   Do not complete the UserSessionStaionLogin records here, logic moved to gatLogout 
                          to break strong association to FPP
------------------------------------------------------------------------------------------------------------------------------------------*/
/* Dependencies */
/* Run of libDataManager.p */


PROCEDURE pAdjustFppStationWork:
   
   /* Parameter                                               */
   DEFINE INPUT  PARAMETER intUserSessionID AS INTEGER   NO-UNDO.   
   DEFINE OUTPUT PARAMETER chrError         AS CHARACTER NO-UNDO.
   
   /* Local Variable Definitions ---                          */
   DEFINE VARIABLE intNewShipOrderStatus       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intShipOrderStatusCancelled AS INTEGER   NO-UNDO.
   
   /* Data Object Definitions ---                             */
   DEFINE VARIABLE updShipOrder               AS updRecord.
   DEFINE VARIABLE updShipOrderLine           AS updRecord.
   DEFINE VARIABLE updUserSessionStationLogin AS updRecord.
   DEFINE VARIABLE updTaskLineWorkUserLink    AS updRecord.
   DEFINE VARIABLE updTaskLineWork            AS updRecord.
   DEFINE VARIABLE updTaskLine                AS updRecord.
   DEFINE VARIABLE updTask                    AS updRecord.
   
   FIND FIRST LocationType NO-LOCK 
      WHERE LocationType.TypeCode = "PickPackStation" NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
   DO:
      chrError = "No LocationType of [PickPackStation] was found.".
      LEAVE.
   END.
   
   FIND FIRST ShipOrderStatus NO-LOCK
      WHERE ShipOrderStatus.StatusCode = "Cancelled" NO-ERROR.
   IF NOT AVAILABLE ShipOrderStatus THEN
   DO:
      chrError = chrError + "No ShipOrderStatus exists for: Cancelled.".
      LEAVE.
   END.
   intShipOrderStatusCancelled = ShipOrderStatus.ShipOrderStatusID.
   
   FIND FIRST ShipOrderStatus NO-LOCK
      WHERE ShipOrderStatus.StatusCode = "Generated" NO-ERROR.
   IF NOT AVAILABLE ShipOrderStatus THEN
   DO:
      chrError = chrError + "No ShipOrderStatus exists for: Generated.".
      LEAVE.
   END.
   
   FIND FIRST TaskStatus NO-LOCK
      WHERE TaskStatus.StatusCode = "Cancelled" NO-ERROR.
   IF NOT AVAILABLE TaskStatus THEN
   DO:
      chrError = chrError + "No TaskStatus exists for: Cancelled.".
      LEAVE.
   END.

   UpdateBlk:
   DO TRANS:

      /* FPPStation Logic: If user logs out before a pick is confirmed, delete the TaskLineWork,
      TaskLine, and Task that was created by the generate and complete UserSessionStationLogin.  */
      FOR EACH UserSessionStationLogin NO-LOCK /* idx=UserSessionIDCompleted */
         WHERE UserSessionStationLogin.UserSessionID = intUserSessionID
         AND   UserSessionStationLogin.Completed     = "":
        
         /* If a UserStation location can be located with a type of FPP and task,taskline, and tasklinework
           are not completed, then delete those records.                                                   */
         FIND FIRST Location NO-LOCK /* idx=LocationID */
            WHERE Location.LocationID     = UserSessionStationLogin.LocationID
            AND   Location.LocationTypeID = LocationType.LocationTypeID NO-ERROR.
         IF AVAILABLE Location THEN 
         DO:           
            /* Completion of the UserSessionStationLogin records has been moved out to the actual gatLogout program. */
/*            updUserSessionStationLogin = fGetRecord("UserSessionStationLogin", UserSessionStationLogin.UserSessionStationLoginID).*/
/*            updUserSessionStationLogin:assignField("Completed", fTimeStamp(NOW)).                                                 */
/*            updUserSessionStationLogin:assignField("Active", NO).                                                                 */
/*                                                                                                                                  */
/*            chrError = chrError + updUserSessionStationLogin:getErrors().                                                         */
/*            DELETE OBJECT updUserSessionStationLogin NO-ERROR.                                                                    */
/*            IF chrError <> "" THEN                                                                                                */
/*               UNDO UpdateBlk, LEAVE UpdateBlk.                                                                                   */
/*                                                                                                                                  */
            FIND FIRST TasklineWork NO-LOCK /* idx=CompletedTargetLocationID */
               WHERE TaskLineWork.Completed        = ""
               AND   TaskLineWork.TargetLocationID = Location.LocationID NO-ERROR.
            IF AVAILABLE TaskLineWork THEN 
            DO:
               /* If User used Logout button-set status to "Canceled" and complete */
               updTaskLineWork = fGetRecord("TaskLineWork", TaskLineWork.TaskLineWorkID).
               updTaskLineWork:assignField("TaskStatusID",  TaskStatus.TaskStatusID).
               updTaskLineWork:assignField("Completed",     fTimestamp(NOW)).
                                    
               chrError = chrError + updTaskLineWork:getErrors().
               DELETE OBJECT updTaskLineWork NO-ERROR.
               IF chrError <> "" THEN
                  UNDO UpdateBlk, LEAVE UpdateBlk.
               
               FIND FIRST TaskLineWorkUserLink NO-LOCK /* idx=TaskLineWorkIDCompleted */
                  WHERE TaskLineWorkUserLink.TaskLineWorkID = TaskLineWork.TaskLineWorkID NO-ERROR.
               IF AVAILABLE TaskLineWorkUserLink THEN 
               DO:
                  /* If User used Logout button-set complete */
                  updTaskLineWorkUserLink = fGetRecord("TaskLineWorkUserLink", TaskLineWorkUserLink.TaskLineWorkUserLinkID).
                  updTaskLineWorkUserLink:assignField("Completed",             fTimestamp(NOW)).
                                       
                  chrError = chrError + updTaskLineWorkUserLink:getErrors().
                  DELETE OBJECT updTaskLineWorkUserLink NO-ERROR.
                  IF chrError <> "" THEN
                     UNDO UpdateBlk, LEAVE UpdateBlk.
               END.
               
               FIND FIRST TaskLine NO-LOCK /* idx=TaskLineID */
                  WHERE TaskLine.TaskLineID = TaskLineWork.TaskLineID NO-ERROR.
               IF AVAILABLE TaskLine THEN 
               DO:
                  /* If User used Logout button-set status to "Canceled" and complete */   
                  updTaskLine = fGetRecord("TaskLine",    TaskLine.TaskLineID).
                  updTaskLine:assignField("TaskStatusID", TaskStatus.TaskStatusID).
                  updTaskLine:assignField("Completed",    fTimestamp(NOW)).
                  
                  chrError = chrError + updTaskLine:getErrors().
                  DELETE OBJECT updTaskLine NO-ERROR.
                  IF chrError <> "" THEN
                     UNDO UpdateBlk, LEAVE UpdateBlk.
                  
                  FIND FIRST Task NO-LOCK /* idx=TaskID */
                     WHERE Task.TaskID = TaskLine.TaskID NO-ERROR.
                  IF AVAILABLE Task THEN 
                  DO:
                     /* If User used Logout button-set status to "Canceled" and complete */
                     updTask = fGetRecord("Task",        Task.TaskID).
                     updTask:assignField("TaskStatusID", TaskStatus.TaskStatusID).
                     updTask:assignField("Completed",    fTimestamp(NOW)).
                     
                     chrError = chrError + updTask:getErrors().
                     DELETE OBJECT updTask NO-ERROR.
                     IF chrError <> "" THEN
                        UNDO UpdateBlk, LEAVE UpdateBlk.
                     
                     FOR EACH ShipOrder OF Task NO-LOCK:
                        
                        FIND LAST ShipOrderHistory NO-LOCK
                           WHERE ShipOrderHistory.ShipOrderID       = ShipOrder.ShipOrderID
                           AND   ShipOrderHistory.ShipOrderStatusID = ShipOrderStatus.ShipOrderStatusID NO-ERROR.
                        IF NOT AVAILABLE ShipOrderHistory THEN
                        DO:
                           chrError = "No ShipOrderHistory exists where the ShipOrder is in Generated Status.". 
                           LEAVE UpdateBlk.                  
                        END.
                        FIND PREV ShipOrderHistory NO-LOCK NO-ERROR.
                        IF AVAILABLE ShipOrderHistory THEN
                           intNewShipOrderStatus = ShipOrderHistory.ShipOrderStatusID.
                           
                        updShipOrder = fGetRecord("ShipOrder",        ShipOrder.ShipOrderID).
                        updShipOrder:assignField("ShipOrderStatusID", intNewShipOrderStatus).
                        updShipOrder:assignField("TaskID",            0).
                        
                        chrError = chrError + updShipOrder:getErrors().
                        DELETE OBJECT updShipOrder NO-ERROR.
                        IF chrError <> "" THEN
                           UNDO UpdateBlk, LEAVE UpdateBlk.
                        
                        OrderLine:   
                        FOR EACH ShipOrderLine OF ShipOrder NO-LOCK 
                           ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
                           
                           IF ShipOrderLine.ShipOrderStatusID = intShipOrderStatusCancelled THEN
                              NEXT OrderLine.
                              
                           IF ShipOrderLine.ShipOrderStatusID <> ShipOrderStatus.ShipOrderStatusID THEN
                           DO:
                              chrError = chrError + "ShipOrder is at an InValid Status FOR Cancelling Task:" .
                              UNDO UpdateBlk, LEAVE UpdateBlk.
                           END.
                           
                           updShipOrderLine = fGetRecord("ShipOrderLine", ShipOrderLine.ShipOrderLineID).
                           updShipOrderLine:assignField("ShipOrderStatusID", intNewShipOrderStatus).
                           
                           chrError = chrError + updShipOrderLine:getErrors().
                           DELETE OBJECT updShipOrderLine NO-ERROR.
                           IF chrError <> "" THEN
                              UNDO UpdateBlk, LEAVE UpdateBlk.
                           
                        END. /*FOR EACH ShipOrderLine OF ShipOrder NO-LOCK */
                        
                     END. /* FOR EACH ShipOrder OF Task */
                        
                  END. /* IF AVAILABLE Task */
                  
               END. /* IF AVAILABLE TaskLine */
               
            END. /* IF AVAILABLE TaskLineWork */
            
         END. /* IF AVAILABLE Location  */
         
      END. /* FOR EACH UserSessionStationLogin  */  
   
   END. /*UpdateBlk:*/ 
   
   RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                   OUTPUT logAllWentOK,
                                   OUTPUT chrError).
   
         
END PROCEDURE. /* pAdjustFppStationWork */   
   
