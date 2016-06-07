/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsProcessEngine.p
Purpose : Program to drive the process map events and routes in character mode
  
          Possible Results : None

Author  : DCummins
Date    : 19th June 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
05/11/2013 BR  CR1030     Log system errors within database table by calling procedure DisplayError, Removed Case logic for SubMaps, 
                          stopped intNextEventID from being assigned with same process eventid if there is a systems error, CR 1030.
10/03/2014 BR  CR1029     Pass SessionStatus for killed session to usrLogin.                           
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER logSessionStatus AS LOGICAL NO-UNDO.

/* Standard Mandatory Variable*/
DEFINE VARIABLE intEventID AS INTEGER NO-UNDO INITIAL 0. 
                      
/* Standard Mandatory Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE}
{fncClassFunctions.i}   
{fncGlobalFunctions.i}

/* Scan Gun Message/Confirm */
{prcScannerMessaging.i}

/* Local Buffers */
DEFINE BUFFER ErrEvent FOR ProcessEvent.

/* Local Variables */
DEFINE VARIABLE intNextEventID AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrResult      AS CHARACTER NO-UNDO.

/* Local Objects */
DEFINE VARIABLE intSsnStartProcessID AS sessionValue NO-UNDO.

/* Get Current Data */
intSsnStartProcessID = fGetSessionValue("StartProcessID").

FIND FIRST ProcessEvent NO-LOCK
   WHERE ProcessEvent.ProcessEventID = intSsnStartProcessID:intValue NO-ERROR.
IF NOT AVAILABLE ProcessEvent THEN
DO:
   /* Dump the Temp Data */
   RUN pFlushCloneTables IN hdlGblLibrary.

   RUN DisplayError("Process Error",
                   "No Event for [ProcessID] [" + STRING(intSsnStartProcessID:intValue) + "].").
   LEAVE.
END.

Process_Block:
REPEAT:
  
   /*This check is to validate the if avalilable ProcessEvent first at the start of Process_Block and then for every successive 
   iterations within the repeat loop*/
   IF NOT AVAILABLE ProcessEvent THEN
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.    

      RUN DisplayError("Record Not Found",
                       "ProcessEvent record does not exist.").
      LEAVE Process_Block.
   END.

   /*If system maintenance flag is set yes then processengine should stop moving further */
   FIND FIRST Config NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Config THEN
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.

      RUN DisplayError("System Error",
                       "No System Configuration Found").
      LEAVE Process_Block.
   END.

   IF Config.Maintenance THEN
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.
      
      LEAVE Process_Block.
   END.

   /*If user has killed an exiting session on another terminal then processengine should stop moving further*/
   FIND FIRST UserSession NO-LOCK 
      WHERE UserSession.GateUserID    = intGblUserID
      AND   UserSession.StatusCode    = "SESSION_KILLED" 
      AND   UserSession.UserSessionID = intGblSessionID NO-ERROR.
   IF AVAILABLE UserSession THEN
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.

      RUN DisplayError("User Session",
                       "This session has been terminated for current user.").

      /*Pass session terminated yes to usrLogin.*/
      logSessionStatus = YES.

      LEAVE Process_Block.
   END.
      
   CASE ProcessEvent.EventType:         
   
      WHEN "Program" THEN
      DO:      
         /* Check its a Valid Process Program */
         FIND FIRST ProcessProgram NO-LOCK
            WHERE ProcessProgram.ProcessProgramID = ProcessEvent.ProcessProgramID NO-ERROR.       
         IF AVAILABLE ProcessProgram THEN
         DO:
            /* Check the Program Physically Exists */
            IF SEARCH(ProcessProgram.ProgramName) <> ? THEN
            DO ON STOP UNDO, RETRY:          
              
               /* Set the EventID for the Process Error Capture within Process Engine */
               intEventID = ProcessEvent.ProcessEventID.

               RUN VALUE(ProcessProgram.ProgramName) (INPUT  ProcessEvent.ProcessEventID,
                                                      OUTPUT chrResult).
            END.
            ELSE
            DO:
               chrResult = "NO_PROGRAM".
            END.
         END. /*Available ProcessProgram*/
         ELSE 
         DO:
             chrResult = "NO_PROCESS".
         END.
      END.
      OTHERWISE /*ProcessEvent.EventType is not Program*/
      DO:
         chrResult = "NO_EVENT".
      END. 
       
   END CASE.

   /* Exit if no Program Found */
   IF chrResult = "NO_PROGRAM" THEN 
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.   

      RUN DisplayError("Process Error",
                       "Next Process Program : [" + ProcessProgram.ProgramName + "] is not on the Propath.").  
      LEAVE Process_Block.
   END.
  
   /* Exit if no Process Found */
   IF chrResult = "NO_PROCESS" THEN 
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.  

      RUN DisplayError("Process Error",
                       "[ProcessProgram ID] [" + STRING(ProcessEvent.ProcessProgramID) + "] does not Exist."). 

      LEAVE Process_Block.
   END.

   /* Exit if no Event Found */
   IF chrResult = "NO_EVENT" THEN 
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary. 

      RUN DisplayError("Process Error",
                       "[ProcessEventID ] [" + STRING(ProcessEvent.ProcessEventID) + "] is not a Program EventType.").
      LEAVE Process_Block.
   END.

   /* Exit process was run */
   IF chrResult = "EXIT" THEN LEAVE Process_Block.
  
   /* Interim Escape for Old Maps */
   IF chrResult = "ALLOW_EXIT" THEN
   DO:      
      LEAVE Process_Block.
   END.

   /* If there has been an Error loop back to the predefined if error process on the Map*/
   IF chrResult = "ERROR" THEN 
   DO:
      /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.     

      IF ProcessEvent.NextErrorEventID <> 0 THEN 
      DO: 
         /* Check the Error Event exists. If yes then process it*/
         IF CAN-FIND(FIRST ErrEvent WHERE ErrEvent.ProcessEventID = ProcessEvent.NextErrorEventID) THEN 
         DO:   
            intNextEventID = ProcessEvent.NextErrorEventID.

            RUN DisplayError("Process Error",
                             "An Error has occurred for EventID [" + STRING(ProcessEvent.ProcessEventID) + "], Program: [" + ProcessProgram.ProgramName + "] ." 
                                + " Error event [" + STRING(ProcessEvent.NextErrorEventID) + "] will now be processed.").           
         END.
         ELSE
         DO:
            /* Donot rerun the original process*/
            RUN DisplayError("Process Error",
                             "An Un-Trapped Error has occurred in Program: [" + ProcessProgram.ProgramName + "].").
            LEAVE Process_Block.
         END.
      END.
      ELSE /*NextErrorEventID = 0*/
      DO:
         /* Donot rerun the original process*/
         RUN DisplayError("Process Error",
                          "An Un-Trapped Error has occurred in Program: [" + ProcessProgram.ProgramName + "]. [NextErrorEventID] is 0").
         LEAVE Process_Block.       
      END. 
   END. /*chrResult is = "ERROR"*/
   ELSE    
   DO:             
      /* Check for Event Routing */
      IF ProcessEvent.NextAutoEventID = 0 THEN
      DO:
         FIND FIRST ProcessRoute NO-LOCK
            WHERE ProcessRoute.ProcessEventID = ProcessEvent.ProcessEventID
            AND   ProcessRoute.EventResult = chrResult NO-ERROR.       
         IF NOT AVAILABLE ProcessRoute THEN
         DO:
            /* Dump the Temp Data */
            RUN pFlushCloneTables IN hdlGblLibrary.    
            
            RUN DisplayError("Process Error",
                             "[ProcessEventID] [" + STRING(ProcessEvent.ProcessEventID) + "] has no [ProcessRoute].").

            LEAVE Process_Block.
         END.
        
         FIND FIRST ProcessEvent NO-LOCK 
            WHERE ProcessEvent.ProcessEventID = ProcessRoute.NextProcessEventID NO-ERROR.
         IF NOT AVAILABLE ProcessEvent THEN
         DO:
             /* Dump the Temp Data */
            RUN pFlushCloneTables IN hdlGblLibrary.    
          
            RUN DisplayError("Process Error",
                             "[NextProcessEventID] [" + STRING(ProcessRoute.NextProcessEventID) + "] has no ProcessEvent.").
            LEAVE Process_Block.
         END.
       
         ASSIGN intNextEventID = ProcessEvent.ProcessEventID.
      END. /*ProcessEvent.NextAutoEventID = 0 */
      ELSE 
      DO:     
        ASSIGN intNextEventID = ProcessEvent.NextAutoEventID.
      END.     
   END. /*chrResultis not "ERROR"*/

   /* Get the Next Event */
   FIND FIRST ProcessEvent NO-LOCK 
      WHERE ProcessEvent.ProcessEventID = intNextEventID NO-ERROR.
   IF NOT AVAILABLE ProcessEvent THEN
   DO:
       /* Dump the Temp Data */
      RUN pFlushCloneTables IN hdlGblLibrary.    
      
      RUN DisplayError("Process Error",
                       "[NextEventID] [" + STRING(intNextEventID) + "] has no ProcessEvent.").
      LEAVE Process_Block.
   END.  

END. /*Process_Block:*/

/* Clean Up */
DELETE OBJECT intSsnStartProcessID NO-ERROR.

/* Releases */   
RELEASE ErrEvent        NO-ERROR.
RELEASE ProcessProgram  NO-ERROR.
RELEASE ProcessEvent    NO-ERROR.
