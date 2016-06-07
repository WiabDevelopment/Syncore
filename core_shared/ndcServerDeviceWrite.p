/*------------------------------------------------------------------------------------------------------------------------------------------
Program : ndcServerDeviceWrite.p
Purpose : Persistent Procedure to Listen for Network Traffic from a Device on a Server Port and Perform a Write Action

Author  : DCummins
Date    : 07/10/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncStatusTypeFunctions.i}
{fncServerFunctions.i}
{fncLoggingFunctions.i}

/* Buffers */
DEFINE BUFFER updNetworkActionPoint FOR NetworkActionPoint.

/* Local Variables */
DEFINE VARIABLE intNetworkActionPointID  AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdlServerSocket          AS HANDLE    NO-UNDO.
DEFINE VARIABLE mptTransmittedData       AS MEMPTR    NO-UNDO.
DEFINE VARIABLE chrDataReceived          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intBytesToRead           AS INTEGER   NO-UNDO.
DEFINE VARIABLE logStopProcess           AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE intEmailGroupID          AS INTEGER   NO-UNDO INITIAL 1 /* SuperUsers */.
DEFINE VARIABLE chrCurrentDirectory      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrRootDirectory         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPropath               AS CHARACTER NO-UNDO.
DEFINE VARIABLE intEntry                 AS INTEGER   NO-UNDO.

/* Get Defaults */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "NetworkActionPointAlerts" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intEmailGroupID = 1.

/* Get the Network User */
FIND FIRST GateUser NO-LOCK 
   WHERE GateUser.Username = "Network" NO-ERROR.
IF NOT AVAILABLE GateUser THEN 
DO:
   /* Error Email */
   RUN pSendEmail(INPUT "User Setup",
                  INPUT "No 'Network' User Setup. User account must be Setup.",
                  INPUT "",
                  INPUT intEmailGroupID).   
                  
END. /* End of IF NOT AVAILABLE GateUser */    

/* Create a Session */
DO TRANS:
   {usrCreateSession.i}
END.

/* Data Manager Access */
RUN libDataManager.p PERSISTENT SET hdlGblLibrary(INPUT UserSession.SessionID).

Main_Block:
DO ON ERROR UNDO, LEAVE:

   /* Set to Session Parameter */
   intNetworkActionPointID = INT(SESSION:PARAMETER).

   /* Get Device */
   FIND FIRST NetworkActionPoint NO-LOCK
      WHERE NetworkActionPoint.NetworkActionPointID = intNetworkActionPointID NO-ERROR.
   IF NOT AVAILABLE NetworkActionPoint THEN
   DO:
      /* Error Email */
      RUN pSendEmail(INPUT "Network Action Point Error",
                     INPUT "No Network Action Point Record Exists for Network Action Point ID: " + STRING(intNetworkActionPointID),
                     INPUT "",
                     INPUT intEmailGroupID).
                     
      LEAVE Main_Block.                
   END.   

   /* Set Propath */
   FIND FIRST Application NO-LOCK /* idx=ApplicationID */
      WHERE Application.ApplicationID = intGblApplicationID NO-ERROR.
   IF AVAILABLE Application THEN
   DO:
      chrCurrentDirectory = fGetCurrentDirectory().
         
      IF NUM-ENTRIES(chrCurrentDirectory,"/") > 2 THEN
         chrRootDirectory = "/" + ENTRY(2,chrCurrentDirectory,"/") + "/" + ENTRY(3,chrCurrentDirectory,"/").
         
      /* Add the specified directory to the propath of the session so program will pick up other dependant programs */
      IF chrRootDirectory > "" AND 
         NetworkActionPoint.DirectoryToRunFrom <> "" THEN
      DO:
         IF fValidDirectory(chrRootDirectory + "/nirs/" + NetworkActionPoint.DirectoryToRunFrom) = "Ok"  THEN
         DO:
            /* Set Propath to be NIRS Project */
            chrPropath = PROPATH.
            REPEAT intEntry = 1 TO NUM-ENTRIES(chrPropath):
               IF NUM-ENTRIES(ENTRY(intEntry,chrPropath),"/") > 3 AND 
                  INDEX(ENTRY(intEntry,chrPropath), Application.PackageName) > 0 THEN
                  PROPATH = chrRootDirectory + "/nirs/" + NetworkActionPoint.DirectoryToRunFrom + "/" 
                               + ENTRY(4,ENTRY(intEntry,chrPropath),"/") +  "," + PROPATH.
            END.
         END.
      END.
   END.  
   
   /* Start Log */
   IF logGblDebugging THEN 
   DO: 
      chrNewAgedDirectory = fGetAgedDirectory("../logs/", 10).
       
      IF chrNewAgedDirectory BEGINS "Error" THEN
         chrTargetDirectory = "../logs/".              
      ELSE
         chrTargetDirectory = chrNewAgedDirectory.
                
      OUTPUT STREAM sToLogFile TO VALUE(chrTargetDirectory + "writeronport_" + NetworkActionPoint.ServerPortNumber + "_" + STRING(TODAY, "999999") + ".log") APPEND.
      
   END.

   /* Start Connection */
   CREATE SERVER-SOCKET hdlServerSocket.
   
   /* Test Devices */
   /* -S 6055 - DUBLIN  */
  
   /* Set the Client Connection Procedure */
   hdlServerSocket:SET-CONNECT-PROCEDURE("pProcessClientConnect") NO-ERROR.
  
   /* Connect to Device */
   hdlServerSocket:ENABLE-CONNECTIONS(" -S " + NetworkActionPoint.ServerPortNumber + " -clientConnectTimeout 1000 ") NO-ERROR.
   
   IF logGblDebugging THEN
      fLog("Connecting to -S " + NetworkActionPoint.ServerPortNumber + " -clientConnectTimeout 1000 ").      
   
   /* Validate Connected */   
   IF NOT ERROR-STATUS:ERROR THEN
   DO:   
      /* Start Listening */
      Processing_Loop:
      REPEAT ON STOP UNDO, LEAVE:

         PROCESS EVENTS.
         
         /* QUIT if Data Library Handle Fails */
         IF NOT VALID-HANDLE(hdlGblLibrary) THEN LEAVE Processing_Loop.
         
         /* Run Process Program Configured for Device */
         FIND FIRST ProcessProgram NO-LOCK
            WHERE ProcessProgram.ProcessProgramID = NetworkActionPoint.ProcessProgramID NO-ERROR.
         IF AVAILABLE ProcessProgram THEN
         DO:
            /* Check Program Compiles */       
            COMPILE VALUE(ProcessProgram.ProgramName) NO-ERROR.
            
            /* IF Compiled with Error */
            IF COMPILER:ERROR THEN               
            DO:
               /* Send Email */
               RUN pSendEmail(INPUT "Process Program Error for Network Action Point",
                              INPUT ("The Program Run from Network Action Point: " + NetworkActionPoint.NetworkActionPointName 
                                      + " -H " + NetworkActionPoint.ServerIPAddress 
                                      + " -S " + NetworkActionPoint.ServerPortNumber + " did NOT Compile. Progress Error: " + ERROR-STATUS:GET-MESSAGE(1)),
                              INPUT "",
                              INPUT intEmailGroupID).          
                              
               IF logGblDebugging THEN
                  fLog("Program" + ProcessProgram.ProgramName + " Compile Error -> " + ERROR-STATUS:GET-MESSAGE(1)).
                              
            END.
            ELSE 
            DO:                          
               RUN VALUE(ProcessProgram.ProgramName) (INPUT NetworkActionPoint.NetworkActionPointID) NO-ERROR.       
                                         
               IF ERROR-STATUS:ERROR THEN
               DO:
                  /* Send Email */
                  RUN pSendEmail(INPUT "Process Program Error for Network Action Point",
                                 INPUT ("The Program Run from Network Action Point: " + NetworkActionPoint.NetworkActionPointName 
                                         + " -H " + NetworkActionPoint.ServerIPAddress 
                                         + " -S " + NetworkActionPoint.ServerPortNumber + " had an Internal Error. Progress Error: " + ERROR-STATUS:GET-MESSAGE(1)),
                                 INPUT "",
                                 INPUT intEmailGroupID).    
                              
                  IF logGblDebugging THEN
                     fLog("Program " + ProcessProgram.ProgramName + " Run Error -> " + ERROR-STATUS:GET-MESSAGE(1)).
               END. 
               ELSE
               DO:
                  IF logGblDebugging THEN
                     fLog("Program " + ProcessProgram.ProgramName + " Ran OK.").               
               END.                       
            END.
         END.       
                                    
         IF logStopProcess THEN LEAVE Processing_Loop.         
                
         PAUSE NetworkActionPoint.TriggerInterval NO-MESSAGE.
                    
      END.      
   END.   
   ELSE      
   DO:
      /* Error Email */
      RUN pSendEmail(INPUT "Network Action Point Error",
                     INPUT ("Could not Connect to Device: " + NetworkActionPoint.NetworkActionPointName 
                             + " -H " + NetworkActionPoint.ServerIPAddress 
                             + " -S " + NetworkActionPoint.ServerPortNumber),
                     INPUT "",
                     INPUT intEmailGroupID).
                     
      IF logGblDebugging THEN
         fLog("Could not Connect to Device: " + NetworkActionPoint.NetworkActionPointName 
                + " -H " + NetworkActionPoint.ServerIPAddress 
                + " -S " + NetworkActionPoint.ServerPortNumber).                     
   END.
  
   /* Clean Up */
   hdlServerSocket:DISABLE-CONNECTIONS() NO-ERROR.
  
   SET-SIZE(mptTransmittedData) = 0.
  
   DELETE OBJECT hdlServerSocket NO-ERROR.
  
   IF logGblDebugging THEN
   DO:
      fLog("Shutting down").   
  
      OUTPUT STREAM sToLogFile CLOSE.  
   END.   
  
   DELETE PROCEDURE hdlGblLibrary NO-ERROR.    
  
   QUIT.
           
END. /* Main Block */

PROCEDURE pProcessClientConnect:

   DEFINE INPUT PARAMETER hdlSocket AS HANDLE NO-UNDO.
    
   hdlSocket:SET-READ-RESPONSE-PROCEDURE('pProcessDataPacket') NO-ERROR.
       
END PROCEDURE.

/* Procedure Run on READ-RESPONSE of Device */
PROCEDURE pProcessDataPacket:

    IF logGblDebugging THEN
       fLog("Bytes received -> " + STRING(SELF:GET-BYTES-AVAILABLE())).
       
    /* If Nothing Read then Leave and Wait for Another Read */ 
    IF SELF:GET-BYTES-AVAILABLE() = 0 THEN
    DO:
       LEAVE.
    END.
 
    /* Set Memptr to Size of Data Sent */
    SET-SIZE(mptTransmittedData) = SELF:GET-BYTES-AVAILABLE().

    intBytesToRead = (SELF:GET-BYTES-AVAILABLE()).        
    
    /* Read Data into Memptr */
    SELF:READ(mptTransmittedData, 1, intBytesToRead).
                        
    /* Put Memptr Data into Character String. Start at Position two as first byte is text begin placeholder and last 3 are end of Message */
    chrDataReceived = GET-STRING(mptTransmittedData, 2, (intBytesToRead - 2)).
    
    IF logGblDebugging THEN
       fLog("Data Captured -> " + chrDataReceived).               
    
    /* Reset Data Memptr */          
    SET-SIZE(mptTransmittedData) = 0.
                    
    /* Check for Test Connection */
    IF chrDataReceived BEGINS "TestWrite" THEN    
    DO:                       
       /* Temp Run for Testing */
       CREATE NetworkAction.
       ASSIGN NetworkAction.NetworkActionID      = NEXT-VALUE(NetworkAction)
              NetworkAction.NetworkActionPointID = NetworkActionPoint.NetworkActionPointID
              NetworkAction.Created              = fTimeStamp(NOW)
              NetworkAction.ActionValue          = ENTRY(2, chrDataReceived, ":") NO-ERROR.
              
       IF ERROR-STATUS:ERROR THEN
       DO:
          /* Send Email */
          RUN pSendEmail(INPUT "Network Action Error",
                         INPUT "Could NOT Create Network Action. " + ERROR-STATUS:GET-MESSAGE(1),
                         INPUT "",
                         INPUT intEmailGroupID).        
       
          IF logGblDebugging THEN
             fLog("Network Action Create Error -> " + ERROR-STATUS:GET-MESSAGE(1)).       
       END.       
                                  
       RUN ndcSendPLCData.p(INPUT NetworkAction.NetworkActionID) NO-ERROR.   
       
       IF ERROR-STATUS:ERROR THEN
       DO:
          /* Send Email */
          RUN pSendEmail(INPUT "Send PLC Data Error",
                         INPUT "SendPLCData did NOT run successfully. " + ERROR-STATUS:GET-MESSAGE(1),
                         INPUT "",
                         INPUT intEmailGroupID).        
       
          IF logGblDebugging THEN
             fLog("SendPLCData Error -> " + ERROR-STATUS:GET-MESSAGE(1)).       
       END.       
                         
      
       /* Unlock the NetworkAction */
       RELEASE NetworkAction NO-ERROR.

    END.          
                                        
    /* Check for Test Connection */
    IF chrDataReceived = "TestConnection" THEN    
    DO:
       /* Send Email */
       RUN pSendEmail(INPUT "Network Action Point Test",
                      INPUT ("Network Action Point: " + NetworkActionPoint.NetworkActionPointName 
                              + " -H " + NetworkActionPoint.ServerIPAddress 
                              + " -S " + NetworkActionPoint.ServerPortNumber + " is Online."),
                      INPUT "",
                      INPUT intEmailGroupID). 
       LEAVE.
    END.    
    
    /* Check for StopProcess and QUIT if Issued */
    IF chrDataReceived = "StopProcess" THEN
    Update_Action:
    DO:
       /* Sub Transaction */    
       DO ON ERROR UNDO, LEAVE:
           
          /* Update the Network Action Record */
          FIND FIRST updNetworkActionPoint EXCLUSIVE-LOCK
             WHERE ROWID(updNetworkActionPoint) = ROWID(NetworkActionPoint) NO-ERROR NO-WAIT.
          IF NOT AVAILABLE updNetworkActionPoint THEN
          DO:   
             /* Send Email */
             RUN pSendEmail(INPUT "Network Action Point Locked",
                            INPUT ("Network Action Point: " + NetworkActionPoint.NetworkActionPointName 
                                    + " -H " + NetworkActionPoint.ServerIPAddress 
                                    + " -S " + NetworkActionPoint.ServerPortNumber + " is Locked. Cannot Update. Shutdown Aborted."),
                            INPUT "",
                            INPUT intEmailGroupID). 
       
             IF logGblDebugging THEN
                fLog("Network Action Point Update Error -> " + ERROR-STATUS:GET-MESSAGE(1)).             
                              
             LEAVE Update_Action.                
          END.   
     
          /* Change to NOT Operational */
          ASSIGN updNetworkActionPoint.IsOperating = FALSE.         
          
          IF logGblDebugging THEN
             fLog("Network Action Point Updated").              
                   
          /* Send Email */
          RUN pSendEmail(INPUT "Network Action Point Shutdown",
                         INPUT ("Network Action Point: " + NetworkActionPoint.NetworkActionPointName 
                                 + " -H " + NetworkActionPoint.ServerIPAddress 
                                 + " -S " + NetworkActionPoint.ServerPortNumber + " has been Shutdown."),
                         INPUT "",
                         INPUT intEmailGroupID).             
     
          RELEASE updNetworkActionPoint NO-ERROR.
              
          /* Then Shutdown the Server Process */      
          logStopProcess = TRUE.         
          SET-SIZE(mptTransmittedData) = 0.
                      
          hdlServerSocket:DISABLE-CONNECTIONS().    
          DELETE OBJECT hdlServerSocket NO-ERROR.      
          
          IF logGblDebugging THEN
             fLog("Network Action Point Disconnected").              
          
       END.
       
       IF logGblDebugging THEN
       DO:
          fLog("Shutting down").   
       
          OUTPUT STREAM sToLogFile CLOSE.  
       END.    
                     
       /* Kill Program */
       DELETE PROCEDURE hdlGblLibrary NO-ERROR.         
       
       QUIT.
                    
    END.        
    
END.

PROCEDURE pSendEmail:

   DEFINE INPUT PARAMETER chrSubjectString AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER chrBodyString    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER chrAttachment    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER intEmailGroup    AS INTEGER   NO-UNDO.
   
   RUN osSendMail.p(INPUT "",               /* Optional list of Users */
                    INPUT chrSubjectString, /* Email Subject */
                    INPUT chrBodyString,    /* Plain text message Body */
                    INPUT "",               /* Html format message Body */
                    INPUT chrAttachment,    /* File path ../files/file */
                    INPUT intEmailGroup,    /* EmailGroupID that you want to send this to */
                    INPUT 0).               /* File MasterID is it applies */   

END PROCEDURE. /*End of pSendErrorMail */
