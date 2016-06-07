/*------------------------------------------------------------------------------------------------------------------------------------------
Program : ndcServerDeviceRead.p
Purpose : Persistent Procedure to Listen for Network Traffic from a Device on a Server Port and Perform a Read Action

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
DEFINE BUFFER updNetworkReader FOR NetworkReader.

/* Local Variables */
DEFINE VARIABLE intNetworkReaderID  AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdlServerSocket     AS HANDLE    NO-UNDO.
DEFINE VARIABLE mptTransmittedData  AS MEMPTR    NO-UNDO.
DEFINE VARIABLE chrDataReceived     AS CHARACTER NO-UNDO.
DEFINE VARIABLE intBytesToRead      AS INTEGER   NO-UNDO.
DEFINE VARIABLE logStopProcess      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE intEmailGroupID     AS INTEGER   NO-UNDO INITIAL 1 /* SuperUsers */.
DEFINE VARIABLE chrCurrentDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrRootDirectory    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPropath          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intEntry            AS INTEGER   NO-UNDO.



/* Get Defaults */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "NetworkReaderAlerts" NO-ERROR.
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
                  INPUT "No 'Network' User Setup. User Account must be Setup.",
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
   intNetworkReaderID = INT(SESSION:PARAMETER).

   /* Get Device */
   FIND FIRST NetworkReader NO-LOCK
      WHERE NetworkReader.NetworkReaderID = intNetworkReaderID NO-ERROR.
   IF NOT AVAILABLE NetworkReader THEN
   DO:
      /* Error Email */
      RUN pSendEmail(INPUT "Network Reader Error",
                     INPUT "No Network Reader Record Exists for Network Reader ID: " + STRING(intNetworkReaderID),
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
         NetworkReader.DirectoryToRunFrom <> "" THEN
      DO:
         IF fValidDirectory(chrRootDirectory + "/nirs/" + NetworkReader.DirectoryToRunFrom) = "Ok"  THEN
         DO:
            /* Set Propath to be NIRS Project */
            chrPropath = PROPATH.
            REPEAT intEntry = 1 TO NUM-ENTRIES(chrPropath):
               IF NUM-ENTRIES(ENTRY(intEntry,chrPropath),"/") > 3 AND 
                  INDEX(ENTRY(intEntry,chrPropath), Application.PackageName) > 0 THEN
                  PROPATH = chrRootDirectory + "/nirs/" + NetworkReader.DirectoryToRunFrom + "/" 
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
                
      OUTPUT STREAM sToLogFile TO VALUE(chrTargetDirectory + "readeronport_" + NetworkReader.ServerPortNumber + "_" + STRING(TODAY, "999999") + ".log") APPEND.
             
   END.
    
   /* Start Connection */
   CREATE SERVER-SOCKET hdlServerSocket.
   
   /* Test Devices */
   /* -S 6051 - DUBLIN  */
  
   /* Set the Client Connection Procedure */
   hdlServerSocket:SET-CONNECT-PROCEDURE("pProcessClientConnect") NO-ERROR.
  
   /* Connect to Device */
   hdlServerSocket:ENABLE-CONNECTIONS(" -S " + NetworkReader.ServerPortNumber + " -clientConnectTimeout 1000 ") NO-ERROR.
   
   IF logGblDebugging THEN
      fLog("Connecting to -S " + NetworkReader.ServerPortNumber + " -clientConnectTimeout 1000 ").   
   
   /* Validate Connected */   
   IF NOT ERROR-STATUS:ERROR THEN
   DO:   
      /* Start Listening */
      Listening_Loop:
      REPEAT ON STOP UNDO, LEAVE:

         WAIT-FOR CONNECT OF hdlServerSocket.
           
      END.      
   END.   
   ELSE      
   DO:
      /* Error Email */
      RUN pSendEmail(INPUT "Network Reader Error",
                     INPUT ("Could not Connect to Device: " + NetworkReader.NetworkReaderName 
                             + " -H " + NetworkReader.ServerIPAddress 
                             + " -S " + NetworkReader.ServerPortNumber),
                     INPUT "",
                     INPUT intEmailGroupID).                     
                     
      IF logGblDebugging THEN
         fLog("Could not Connect to Device: " + NetworkReader.NetworkReaderName 
                + " -H " + NetworkReader.ServerIPAddress 
                + " -S " + NetworkReader.ServerPortNumber).   
                     
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
           
END.

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
    
    /* Check for HeartBeat. HeartBeat is the Keep Alive signal from the Device so the Network maintains connectivity. Dont want to record it though.*/
    IF chrDataReceived BEGINS "HeartBeat" THEN
    DO:
       LEAVE.
    END.      
    
    /* Create Network Read for Tracking */
    IF chrDataReceived <> "StopProcess" THEN
    DO TRANS ON ERROR UNDO, LEAVE:
    
       CREATE NetworkRead.
       ASSIGN NetworkRead.NetworkReadID   = NEXT-VALUE(NetworkRead)
              NetworkRead.NetworkReaderID = NetworkReader.NetworkReaderID
              NetworkRead.Created         = fTimeStamp(NOW)
              NetworkRead.MessageReceived = REPLACE(chrDataReceived, CHR(13), "") NO-ERROR.     
              
       IF ERROR-STATUS:ERROR THEN
       DO:
          /* Send Email */
          RUN pSendEmail(INPUT "Network Read Error",
                         INPUT "Could NOT Create Network Read. " + ERROR-STATUS:GET-MESSAGE(1),
                         INPUT "",
                         INPUT intEmailGroupID).        
       
          IF logGblDebugging THEN
             fLog("Network Read Create Error -> " + ERROR-STATUS:GET-MESSAGE(1)).       
       END.       
              
    END.          
            
    /* Release NetworkRead Record */
    FIND CURRENT NetworkRead NO-LOCK NO-ERROR.
                 
    /* Check for NoRead */
    IF chrDataReceived = "NoRead" THEN
    DO:
       LEAVE.
    END.
    
    /* Check for Test Connection */
    IF chrDataReceived = "TestConnection" THEN    
    DO:
       /* Send Email */
       RUN pSendEmail(INPUT "Network Reader Test",
                      INPUT ("Network Reader: " + NetworkReader.NetworkReaderName 
                              + " -H " + NetworkReader.ServerIPAddress 
                              + " -S " + NetworkReader.ServerPortNumber + " is Online."),
                      INPUT "",
                      INPUT intEmailGroupID). 
       LEAVE.
    END.    
    
    
    /* Check for StopProcess and QUIT if Issued or Data Library has Failed */
    IF chrDataReceived = "StopProcess" OR NOT VALID-HANDLE(hdlGblLibrary) THEN
    Update_Reader:
    DO:

       IF logGblDebugging AND NOT VALID-HANDLE(hdlGblLibrary) THEN
          fLog("Data Library Failed and Stop Process triggered").

       IF logGblDebugging THEN
          fLog("Stop Process triggered").
           
       DO TRANS ON ERROR UNDO:
          /* Update the Network Reader Record */
          FIND FIRST updNetworkReader EXCLUSIVE-LOCK
             WHERE ROWID(updNetworkReader) = ROWID(NetworkReader) NO-ERROR NO-WAIT.
          IF NOT AVAILABLE updNetworkReader THEN
          DO:   
             /* Send Email */
             RUN pSendEmail(INPUT "Network Reader Locked",
                            INPUT ("Network Reader: " + NetworkReader.NetworkReaderName 
                                    + " -H " + NetworkReader.ServerIPAddress 
                                    + " -S " + NetworkReader.ServerPortNumber + " is Locked. Cannot Update. Shutdown Aborted."),
                            INPUT "",
                            INPUT intEmailGroupID). 
                         
             IF logGblDebugging THEN
                fLog("Network Read Update Error -> " + ERROR-STATUS:GET-MESSAGE(1)).                         
                         
             LEAVE Update_Reader.                
          END.   
   
          /* Change to NOT Listening */
          ASSIGN updNetworkReader.IsListening = FALSE.
          
          IF logGblDebugging THEN
             fLog("Network Reader Updated").          
         
          /* Send Email */
          RUN pSendEmail(INPUT "Network Reader Shutdown",
                         INPUT ("Network Reader: " + NetworkReader.NetworkReaderName 
                                 + " -H " + NetworkReader.ServerIPAddress 
                                 + " -S " + NetworkReader.ServerPortNumber + " has been Shutdown."),
                         INPUT "",
                         INPUT intEmailGroupID).             
         
           /* Then Shutdown the Server Process */                    
          RELEASE updNetworkReader NO-ERROR.
          SET-SIZE(mptTransmittedData) = 0.
  
          hdlServerSocket:DISABLE-CONNECTIONS().                      
          DELETE OBJECT hdlServerSocket NO-ERROR. 
          
          IF logGblDebugging THEN
             fLog("Network Reader Disconnected").                    
       
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
        
    /* Run Process Program Configured for Device */
    FIND FIRST ProcessProgram NO-LOCK
       WHERE ProcessProgram.ProcessProgramID = NetworkReader.ProcessProgramID NO-ERROR.
    IF AVAILABLE ProcessProgram THEN
    Run_Block:
    DO:           
          
       /* Check Program Compiles */       
       COMPILE VALUE(ProcessProgram.ProgramName) NO-ERROR.
       
       /* IF Compiled with Error */
       IF COMPILER:ERROR THEN               
       DO:
          /* Send Email */
          RUN pSendEmail(INPUT "Process Program Error for Network Reader",
                         INPUT ("The Program Run from Network Reader: " + NetworkReader.NetworkReaderName 
                                 + " -H " + NetworkReader.ServerIPAddress 
                                 + " -S " + NetworkReader.ServerPortNumber + " did NOT Compile. Progress Error: " + ERROR-STATUS:GET-MESSAGE(1)),
                         INPUT "",
                         INPUT intEmailGroupID).          
                         
          IF logGblDebugging THEN
             fLog("Program" + ProcessProgram.ProgramName + " Compile Error -> " + ERROR-STATUS:GET-MESSAGE(1)).    
                                      
          LEAVE Run_Block.              
       END.
       ELSE
       DO:
          RUN VALUE(ProcessProgram.ProgramName) (INPUT NetworkRead.NetworkReadID) NO-ERROR.       
           
          IF ERROR-STATUS:ERROR THEN
          DO:
             /* Send Email */
             RUN pSendEmail(INPUT "Process Program Error for Network Reader",
                            INPUT ("The Program Run from Network Reader: " + NetworkReader.NetworkReaderName 
                                    + " -H " + NetworkReader.ServerIPAddress 
                                    + " -S " + NetworkReader.ServerPortNumber + " had an Internal Error. Progress Error: " + ERROR-STATUS:GET-MESSAGE(1)),
                            INPUT "",
                            INPUT intEmailGroupID).                 
                         
             IF logGblDebugging THEN
                fLog("Program " + ProcessProgram.ProgramName + " Run Error -> " + ERROR-STATUS:GET-MESSAGE(1)).           
             
             LEAVE Run_Block.                  
          END.
          ELSE
          DO:
             IF logGblDebugging THEN
                fLog("Program " + ProcessProgram.ProgramName + " Ran OK.").               
                
             IF logGblDebugging AND RETURN-VALUE <> "" THEN
                fLog("Program Returned -> " + RETURN-VALUE).             
                
          END.          
       END.       
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
