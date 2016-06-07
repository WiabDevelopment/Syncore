/*------------------------------------------------------------------------------------------------------------------------------------------
Program : ndcServerListener.p
Purpose : Persistent Procedure to Listen for Network Traffic from a Device on a Server Port

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

/* Get Defaults */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "NetworkReaderAlerts" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intEmailGroupID = 1.

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
   END.   

   /* Start Connection */
   CREATE SERVER-SOCKET hdlServerSocket.
   
   /* Test Devices */
   /* -S 6051 - DUBLIN  */
  
   /* Set the Client Connection Procedure */
   hdlServerSocket:SET-CONNECT-PROCEDURE("pProcessClientConnect") NO-ERROR.
  
   /* Connect to Device */
   hdlServerSocket:ENABLE-CONNECTIONS(" -S " + NetworkReader.PortNumber) NO-ERROR.
   
   /* Validate Connected */   
   IF NOT ERROR-STATUS:ERROR THEN
   DO:   
      /* Start Listening */
      Listening_Loop:
      REPEAT ON STOP UNDO, LEAVE ON QUIT UNDO, LEAVE:

         WAIT-FOR CONNECT OF hdlServerSocket.
           
      END.      
   END.   
   ELSE      
   DO:
      /* Error Email */
      RUN pSendEmail(INPUT "Network Reader Error",
                     INPUT ("Could not Connect to Device: " + NetworkReader.NetworkReaderName 
                             + " -H " + NetworkReader.IPAddress 
                             + " -S " + NetworkReader.PortNumber),
                     INPUT "",
                     INPUT intEmailGroupID).
   END.
  
   /* Clean Up */
   hdlServerSocket:DISABLE-CONNECTIONS().
  
   SET-SIZE(mptTransmittedData) = 0.
  
   DELETE OBJECT hdlServerSocket NO-ERROR.
  
   QUIT.
           
END.

PROCEDURE pProcessClientConnect:

   DEFINE INPUT PARAMETER hdlSocket AS HANDLE NO-UNDO.
    
   hdlSocket:SET-READ-RESPONSE-PROCEDURE('pProcessDataPacket') NO-ERROR.
       
END PROCEDURE.

/* Procedure Run on READ-RESPONSE of Device */
PROCEDURE pProcessDataPacket:

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
    
    /* Reset Data Memptr */          
    SET-SIZE(mptTransmittedData) = 0.
    
    /* Create Network Read for Tracking */
    DO TRANS ON ERROR UNDO, LEAVE:
    
       CREATE NetworkRead.
       ASSIGN NetworkRead.NetworkReadID   = NEXT-VALUE(NetworkRead)
              NetworkRead.NetworkReaderID = NetworkReader.NetworkReaderID
              NetworkRead.Created         = fTimeStamp(NOW)
              NetworkRead.MessageReceived = chrDataReceived.     
              
    END.          
             
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
                              + " -H " + NetworkReader.IPAddress 
                              + " -S " + NetworkReader.PortNumber + " is Online."),
                      INPUT "",
                      INPUT intEmailGroupID). 
       LEAVE.
    END.    
    
    /* Check for StopProcess and QUIT if Issued */
    IF chrDataReceived = "StopProcess" THEN
    Update_Reader:
    DO TRANS ON ERROR UNDO, LEAVE:
    
       /* Update the Network Reader Record */
       FIND FIRST updNetworkReader EXCLUSIVE-LOCK
          WHERE ROWID(updNetworkReader) = ROWID(NetworkReader) NO-ERROR.
       IF NOT AVAILABLE updNetworkReader THEN
       DO:   
          /* Send Email */
          RUN pSendEmail(INPUT "Network Reader Locked",
                         INPUT ("Network Reader: " + NetworkReader.NetworkReaderName 
                                 + " -H " + NetworkReader.IPAddress 
                                 + " -S " + NetworkReader.PortNumber + " is Locked. Cannot Update. Shutdown Aborted."),
                         INPUT "",
                         INPUT intEmailGroupID). 
                         
          LEAVE Update_Reader.                
       END.   

       /* Change to NOT Listening */
       ASSIGN NetworkReader.IsListening = FALSE
              NetworkReader.StartListening = FALSE.
         
       /* Send Email */
       RUN pSendEmail(INPUT "Network Reader Shutdown",
                      INPUT ("Network Reader: " + NetworkReader.NetworkReaderName 
                              + " -H " + NetworkReader.IPAddress 
                              + " -S " + NetworkReader.PortNumber + " has been Shutdown."),
                      INPUT "",
                      INPUT intEmailGroupID).             
         
       /* Then Shutdown the Server Process */             
       QUIT.
       
    END.        
    
    /* Run Process Program Configured for Device */
    FIND FIRST ProcessProgram NO-LOCK
       WHERE ProcessProgram.ProcessProgramID = NetworkReader.ProcessProgramID NO-ERROR.
    IF AVAILABLE ProcessProgram THEN
    DO:
       RUN VALUE(ProcessProgram.ProgramName) (INPUT chrDataReceived) NO-ERROR.
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
