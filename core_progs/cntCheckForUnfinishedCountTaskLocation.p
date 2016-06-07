/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCheckForUnfinishedCountTask.p
Purpose : Check to see if there are unfinished CountTaskLocation records created by this user for the equipment

         Possible Results : Yes, No  

Author  : Christopher Shelley
Date    : 16/04/2014
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
   {fncDateFunctions.i}
   {fncStatusTypeFunctions.i}

   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}

   /* Local Variables */
   DEFINE VARIABLE chrTimeStamp       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intStatusInProcess AS INTEGER NO-UNDO.
                  
   /* Session Objects */  
   DEFINE VARIABLE intSsnTaskLocationID    AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnLocationTypeGroup AS sessionValue NO-UNDO.

   /* Clear Session Data */
   fClearSessionValue("TaskLocationID").

   /* Create New Session Data */
   intSsnTaskLocationID = fNewSessionValue("TaskLocationID").
   
   /* Get Current Session Data */
   intSsnLocationTypeGroup = fGetSessionValue("LocationTypeGroup").

   Main_Block:
   DO ON ERROR UNDO:
      intStatusInProcess = fGetStatusID("CountTask", "InProcess").
      
      /* If there is an unfinished CountTaskLocation which this user started, then this user will finish it. */
      FOR EACH CountTask NO-LOCK
         WHERE CountTask.Completed = ""
         BY    CountTask.CountTaskID:

         FOR EACH CountTaskLocation OF CountTask NO-LOCK 
            WHERE CountTaskLocation.Completed   = "" 
            AND   CountTaskLocation.AssignedTo  = intGblUserID 
            AND   CountTaskLocation.CountTaskStatusID = intStatusInProcess:

            FIND FIRST Location OF CountTaskLocation NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Location THEN
            DO:
               RUN DisplayMessage("Record not found",
                                  "[Location] ID [" + STRING(CountTaskLocation.LocationID) + " ] does not exist.").         
               LEAVE Main_Block.
            END. /*  IF NOT AVAILABLE Location */   

            /* If user can access location */
            LocationTypeGroup_Block:
            FOR EACH LocationType OF Location NO-LOCK,
               EACH LocationTypeGroupLink OF LocationType NO-LOCK,
                  EACH LocationTypeGroup OF LocationTypeGroupLink 
                     WHERE LocationTypeGroup.LocationTypeGroupID = intSsnLocationTypeGroup:intValue NO-LOCK:
                           
                        intSsnTaskLocationID:setValue(CountTaskLocation.CountTaskLocationID).
            
                        RUN DisplayMessage("CycleCount Status", 
            		                         "Unfinished task exist for current user for [Location]: [" + Location.LocationRef + "].").
            
                        chrResult = "Yes".
                        LEAVE Main_Block.
            END. /* LocationTypeGroup_Block: */
               
         END. /* FOR EACH CoutTaskLocation */
         
      END. /* FOR EACH CountTask */
      
      /* If there are no unfinfished CountTaskLocations that the user can access then the user can start a new one */
      chrResult = "No".
      
   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID    NO-ERROR.
   DELETE OBJECT intSsnLocationTypeGroup NO-ERROR.
   
   /* Releases */
   RELEASE CountTask         NO-ERROR.
   RELEASE CountTaskLocation NO-ERROR.
   RELEASE Location          NO-ERROR.
  
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */