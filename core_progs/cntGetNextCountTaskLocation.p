/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntGetNextCountTaskLocation.p
Purpose : Gets the next available CountTaskLocation for the Selected Equipment and by count sequence from User's previous location

          Possible Results : Continue                                                                                  
                                                                                      
Author  : Christopher Shelley
Date    : 22/04/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
24/09/2014  MC  GHDDE     change the order sequence the cycle count tasks are found in from highest to lowest (Priority DESCENDING)
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
   
   /* Local Variables */      
   DEFINE VARIABLE intHighTaskID             AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intLowTaskID              AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intHighValue              AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intLowValue               AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intPalletBufferCheck      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE chrExtendMessage          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intSkippedCountTaskStatus AS INTEGER   NO-UNDO.
   DEFINE VARIABLE logUserPreassigned        AS LOGICAL   NO-UNDO.

   /* Temp Tables */
   DEFINE TEMP-TABLE ttCountTaskLocation
      FIELD CountTaskLocationID LIKE CountTaskLocation.CountTaskLocationID
      FIELD CountTaskID         LIKE CountTaskLocation.CountTaskID
      FIELD LocationID          LIKE CountTaskLocation.LocationID
      FIELD CountSequence       LIKE Location.CountSequence.

   /* Buffer */
   DEFINE BUFFER ttCheckTaskLocation       FOR ttCountTaskLocation.
   DEFINE BUFFER readPrevCountTaskLocation FOR CountTaskLocation.

   /* Session Objects */
   DEFINE VARIABLE intSsnLocationID        AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnTaskLocationID    AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnLocationTypeGroup AS sessionValue NO-UNDO.   
   
   fClearSessionValue("TaskLocationID").
   
   /* Get Current Session Data */
   intSsnLocationID        = fGetSessionValue("LocationID").
   intSsnLocationTypeGroup = fGetSessionValue("LocationTypeGroup").

   /* Create New Session Data */
   intSsnTaskLocationID  = fNewSessionValue("TaskLocationID").   
   
   /* DB Objects */   
   DEFINE VARIABLE updCountTaskLocation AS updRecord NO-UNDO.
   
   Main_Block:
   DO ON ERROR UNDO:
      
      FIND FIRST LocationTypeGroup NO-LOCK 
         WHERE LocationTypeGroup.LocationTypeGroupID = intSsnLocationTypeGroup:intValue NO-ERROR.
      IF NOT AVAILABLE LocationTypeGroup THEN 
      DO:
         RUN DisplayMessage("Record Not Found",
                            "[LocationTypeGroupID] [" + STRING(intSsnLocationTypeGroup:intValue) + "] does not exist.").
         LEAVE Main_Block.
      END.
      
      /* Finding Task Already Assigned */
      CountTaskBlockAssigned:
      FOR EACH CountTask NO-LOCK
         WHERE CountTask.Completed = ""
         AND   CountTask.BusinessUnitID = intGblBusinessUnitID
         BY CountTask.Priority DESCENDING 
         BY CountTask.CountTaskID:
         
         /* Checking if CountTask is assgned to the User */
         logUserPreassigned = NO.
         UserLinkLoop:
         FOR EACH CountTaskUserLink OF CountTask NO-LOCK  /* idx=CompletedGateuserID + CoutTaskIdCompleted */
            WHERE CountTaskUserLink.Completed = "" 
            AND   CountTaskUserLink.GateUserID = intGblUserID:
                
            logUserPreassigned = YES.    
            LEAVE UserLinkLoop. 
            
         END.          
         IF logUserPreassigned = NO THEN 
            NEXT CountTaskBlockAssigned.
         
         FIND FIRST CountGroup NO-LOCK 
            WHERE CountGroup.CountGroupID = CountTask.CountGroupID NO-ERROR.
         IF NOT AVAILABLE CountGroup THEN
         DO:
            RUN DisplayMessage("Record Not Found",
                               "[CountGroup] ID [" + STRING(CountTask.CountGroupID) + "] does not exist.").
            NEXT CountTaskBlockAssigned.
         END.
                  
         CountTaskLocationAssignedBlock:
         FOR EACH CountTaskLocation NO-LOCK 
            WHERE CountTaskLocation.CountTaskID = CountTask.CountTaskID
            AND   CountTaskLocation.Completed   = ""
            AND  (CountTaskLocation.AssignedTo  = 0 
                  OR 
                  CountTaskLocation.AssignedTo  = intGblUserID):
               
            /* Checking if Location belongs to Chosen Location Group */      
            FIND FIRST Location NO-LOCK
               WHERE Location.LocationID = CountTaskLocation.LocationID NO-ERROR.
            IF AVAILABLE Location THEN
            DO:               
               LocationTypeGroupAssigned_Block:
               FOR EACH LocationType OF Location NO-LOCK,
                  EACH LocationTypeGroupLink OF LocationType NO-LOCK,
                     EACH LocationTypeGroup OF LocationTypeGroupLink 
                        WHERE LocationTypeGroup.LocationTypeGroupID = intSsnLocationTypeGroup:intValue NO-LOCK:
                 
                           /* Compile the Valid Tasks for Calculating the nearest later */
                           CREATE ttCountTaskLocation.
                           ASSIGN ttCountTaskLocation.CountTaskLocationID = CountTaskLocation.CountTaskLocationID
                                  ttCountTaskLocation.CountTaskID         = CountTaskLocation.CountTaskID
                                  ttCountTaskLocation.LocationID          = CountTaskLocation.LocationID
                                  ttCountTaskLocation.CountSequence       = Location.CountSequence.
                           LEAVE  LocationTypeGroupAssigned_Block.                     
               
               END.  /* LocationTypeGroup_Block */ 
            END. /* IF AVAILABLE LOCATION */
            
         END. /* FOR EACH CountTaskLocation */
         
         /* If Reached Here then it is valid CountTask Location */
         IF CAN-FIND(FIRST ttCountTaskLocation) THEN 
            LEAVE CountTaskBlockAssigned.                 
      
      END. /* FOR EACH CountTask */   
      
      /* Finding Unassigned Task if No Assigned Found */
      IF NOT CAN-FIND(FIRST ttCountTaskLocation) THEN DO:
         CountTaskBlockUnassigned:
         FOR EACH CountTask NO-LOCK
            WHERE CountTask.Completed = ""
            AND   CountTask.BusinessUnitID = intGblBusinessUnitID
            BY CountTask.Priority DESCENDING 
            BY CountTask.CountTaskID:
                           
            /* Checking if CountTask is not assigned to Anybody */
            UserLinkLoop:
            FOR EACH CountTaskUserLink OF CountTask NO-LOCK  /* idx=CompletedGateuserID + CoutTaskIdCompleted */
               WHERE CountTaskUserLink.Completed = "" 
               AND   CountTaskUserLink.GateUserID <> intGblUserID:

               NEXT CountTaskBlockUnassigned. 
               
            END.                           
            
            FIND FIRST CountGroup NO-LOCK 
               WHERE CountGroup.CountGroupID = CountTask.CountGroupID NO-ERROR.
            IF NOT AVAILABLE CountGroup THEN
            DO:
               RUN DisplayMessage("Record Not Found",
                                  "[CountGroup] ID [" + STRING(CountTask.CountGroupID) + "] does not exist.").
               NEXT.
            END.
            CountTaskLocationUnassignedBlock:
            FOR EACH CountTaskLocation NO-LOCK 
               WHERE CountTaskLocation.CountTaskID = CountTask.CountTaskID
               AND   CountTaskLocation.Completed   = ""
               AND  (CountTaskLocation.AssignedTo  = 0 
                     OR 
                     CountTaskLocation.AssignedTo  = intGblUserID):
                  
               /* Checking if Location belongs to Chosen Location Group */      
               FIND FIRST Location NO-LOCK
                  WHERE Location.LocationID = CountTaskLocation.LocationID NO-ERROR.
               IF AVAILABLE Location THEN
               DO:               
                  LocationTypeGroupUnassigned_Block:
                  FOR EACH LocationType OF Location NO-LOCK,
                     EACH LocationTypeGroupLink OF LocationType NO-LOCK,
                        EACH LocationTypeGroup OF LocationTypeGroupLink 
                           WHERE LocationTypeGroup.LocationTypeGroupID = intSsnLocationTypeGroup:intValue NO-LOCK:
                    
                              /* Compile the Valid Tasks for Calculating the nearest later */
                              CREATE ttCountTaskLocation.
                              ASSIGN ttCountTaskLocation.CountTaskLocationID = CountTaskLocation.CountTaskLocationID
                                     ttCountTaskLocation.CountTaskID         = CountTaskLocation.CountTaskID
                                     ttCountTaskLocation.LocationID          = CountTaskLocation.LocationID
                                     ttCountTaskLocation.CountSequence       = Location.CountSequence.
                              LEAVE  LocationTypeGroupUnassigned_Block.                     
                  
                  END.  /* LocationTypeGroup_Block */ 
               END. /* IF AVAILABLE LOCATION */
            
            END. /* FOR EACH CountTaskLocation */
            
            /* If Reached Here then it is valid CountTask Location */
            IF CAN-FIND(FIRST ttCountTaskLocation) THEN 
               LEAVE CountTaskBlockUnassigned.                 
         
         END. /* FOR EACH CountTask */ 
      END.  /* IF NOT CAN-FIND(FIRST ttCountTaskLocation) */
       
      Task_Check_Block:
      DO:      
         /* Get the Users Current Location */
         FIND FIRST Location NO-LOCK
            WHERE Location.LocationID = intSsnLocationID:intValue NO-ERROR.
         IF NOT AVAILABLE Location THEN
         DO:         
            RUN DisplayMessage("Record Not Found",
                               "User Location could not be determined. First Available Task will be Selected.").

            /* Get Any Location */
            FIND FIRST Location NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Location THEN
            DO:
               RUN DisplayMessage("Record Not Found",
                                  "[Location] does not exist.").
               RETURN.
            END.
         END. /* IF NOT AVAILABLE Location */

         /* Get the nearest Task Equal to or beyond the Current Location */
         High_Task_Block:
         FOR EACH ttCountTaskLocation NO-LOCK
            WHERE ttCountTaskLocation.CountSequence >= Location.CountSequence
            BY ttCountTaskLocation.CountSequence:                
            
            intHighTaskID = ttCountTaskLocation.CountTaskLocationID.
            intHighValue  = ttCountTaskLocation.CountSequence.
   
            LEAVE High_Task_Block.
   
         END. /* FOR EACH ttCountTaskLocation */
   
         /* Get the nearest Task before the Current Location */
         Low_Task_Block:
         FOR EACH ttCountTaskLocation NO-LOCK
            WHERE ttCountTaskLocation.CountSequence < Location.CountSequence
            BY ttCountTaskLocation.CountSequence:                
   
            intLowTaskID = ttCountTaskLocation.CountTaskLocationID.
            intLowValue  = ttCountTaskLocation.CountSequence.
   
            LEAVE Low_Task_Block.
   
         END. /* FOR EACH ttCountTaskLocation */                           

         /* Now see which is closer */
         IF (intHighTaskID = 0 AND intLowTaskID <> 0) OR 
            ((Location.CountSequence - intLowValue) < (intHighValue - Location.CountSequence) AND intLowValue <> 0) THEN
         DO:            
            FIND FIRST CountTaskLocation NO-LOCK
               WHERE CountTaskLocation.CountTaskLocationID = intLowTaskID NO-ERROR.               
         END.        
         
         IF (intLowTaskID = 0 AND intHighTaskID <> 0) OR 
            ((Location.CountSequence - intLowValue) > (intHighValue - Location.CountSequence) AND intHighValue <> 0) THEN
         DO:
            FIND FIRST CountTaskLocation NO-LOCK
               WHERE CountTaskLocation.CountTaskLocationID = intHighTaskID NO-ERROR.               
         END.  

         /* If there is no CountSequence get the first */
         IF (intLowValue = 0 AND intHighValue = 0) THEN
         DO:
            FOR FIRST ttCountTaskLocation NO-LOCK
               BY ttCountTaskLocation.CountSequence:

               FIND FIRST CountTaskLocation NO-LOCK
                  WHERE CountTaskLocation.CountTaskLocationID = ttCountTaskLocation.CountTaskLocationID NO-ERROR.               

            END. /* FOR FIRST ttCountTaskLocation */
         END. /* IF (intLowValue = 0 AND intHighValue = 0) */
      END. /* Task_Check_Block */

      /* Message Error */
      IF NOT AVAILABLE CountTaskLocation THEN
      DO: 
/*         RUN DisplayMessage("Record Not Found",                                                                                     */
/*                            "No more Tasks could be found near Location [" + Location.LocationRef + "] for your equipment choice.").*/
         /* No Tasks found for the Equipment and User */
         chrResult = "NotFound".
         LEAVE Main_Block.
      END. /* IF NOT AVAILABLE CountTaskLocation */
      
      /* Set Session Value */
      intSsnTaskLocationID:setValue(CountTaskLocation.CountTaskLocationID).
      chrResult = "Found".
   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID    NO-ERROR.
   DELETE OBJECT intSsnLocationTypeGroup NO-ERROR.
   DELETE OBJECT intSsnLocationID        NO-ERROR.
   
   /* Releases */
   RELEASE CountGroup                NO-ERROR.
   RELEASE CountTask                 NO-ERROR.
   RELEASE CountTaskLocation         NO-ERROR.
   RELEASE Location                  NO-ERROR.
   RELEASE readPrevCountTaskLocation NO-ERROR.
   
   EMPTY TEMP-TABLE ttCountTaskLocation NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
END.  /* CTRL-C */
