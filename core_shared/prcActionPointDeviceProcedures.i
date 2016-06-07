/*------------------------------------------------------------------------------------------------------------------------------------------
Program : prcActionPointDeviceProcedures.i
Purpose : Procedure library for ActionPoint Device Control
Author  : DCummins
Date    : 07/10/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* Buffers */
DEFINE BUFFER readNetworkActionPoint FOR NetworkActionPoint.
DEFINE BUFFER updNetworkActionPoint  FOR NetworkActionPoint.

/* Local Variables */
DEFINE VARIABLE hdlListenerServer   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdlListenerDevice   AS HANDLE    NO-UNDO.
DEFINE VARIABLE chrDataToTransmit   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intEmailGroupID     AS INTEGER   NO-UNDO INITIAL 1 /* SuperUsers */.
DEFINE VARIABLE chrCurrentDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrRootDirectory    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrProgramToRun     AS CHARACTER NO-UNDO.

/* Memory Pointers */
DEFINE VARIABLE mptTransmittedData AS MEMPTR NO-UNDO.


/* Get Defaults */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "NetworkActionPointAlerts" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intEmailGroupID = 1.

/* Default Program to Run */   
chrProgramToRun = "ndcServerDeviceWrite.p".   
   
PROCEDURE pCheckConnection:

   DEFINE INPUT PARAMETER inpNetworkActionPointID AS INTEGER NO-UNDO.
      
   DEFINE VARIABLE chrResponse AS CHARACTER NO-UNDO.   
      
   FIND FIRST readNetworkActionPoint NO-LOCK
      WHERE readNetworkActionPoint.NetworkActionPointID = inpNetworkActionPointID NO-ERROR.
   IF NOT AVAILABLE readNetworkActionPoint THEN
   DO:
      /* Send Email */
      RUN pSendEmail(INPUT "Network Action Point Error",
                     INPUT ("Network Action Point with ID: " + STRING(inpNetworkActionPointID) + " does NOT Exist."),
                     INPUT "",
                     INPUT intEmailGroupID).         
   END.
   
   /* Socket to Connect to Device */
   CREATE SOCKET hdlListenerDevice.
               
   /* Test the Phyiscal Device is Online */
   hdlListenerDevice:CONNECT("-H " + readNetworkActionPoint.IPAddress + " -S " + readNetworkActionPoint.PortNumber + " -clientConnectTimeout 1000") NO-ERROR.
   
   IF NOT hdlListenerDevice:CONNECTED() THEN   
   DO:
      RUN pStopActionPointDevice(INPUT  readNetworkActionPoint.NetworkActionPointID,
                                 OUTPUT chrResponse).
   END.

   /* Clean Up for Re-Use */
   RUN pCleanUp.

   /* Re-Fetch */
   FIND CURRENT readNetworkActionPoint NO-LOCK NO-ERROR.

   /* Socket to Connect to Server */
   CREATE SOCKET hdlListenerServer.

   /* Connect to Device Server */
   hdlListenerServer:CONNECT("-H " + readNetworkActionPoint.ServerIPAddress + " -S " + readNetworkActionPoint.ServerPortNumber) NO-ERROR.
                   
   IF NOT hdlListenerServer:CONNECTED() AND readNetworkActionPoint.IsOperating THEN
   DO TRANS ON ERROR UNDO:
   
      /* Update NetworkActionPoint to IsOperating FALSE */
      FIND FIRST updNetworkActionPoint EXCLUSIVE-LOCK
         WHERE ROWID(updNetworkActionPoint) = ROWID(readNetworkActionPoint) NO-ERROR NO-WAIT.
      IF AVAILABLE updNetworkActionPoint THEN
      DO:
      
         updNetworkActionPoint.IsOperating = FALSE.
      
      END.    
              
   END.

   RELEASE updNetworkActionPoint NO-ERROR.

   /* Final Clean Up */
   RUN pCleanUp.            

END PROCEDURE.   
   
PROCEDURE pStartActionPointDevice:   

   DEFINE INPUT  PARAMETER inpNetworkActionPointID AS INTEGER   NO-UNDO. 
   DEFINE OUTPUT PARAMETER oupResponse 	          AS CHARACTER NO-UNDO.   

   FIND FIRST readNetworkActionPoint NO-LOCK
      WHERE readNetworkActionPoint.NetworkActionPointID = inpNetworkActionPointID NO-ERROR.
   IF NOT AVAILABLE readNetworkActionPoint THEN
   DO:
      /* Send Email */
      RUN pSendEmail(INPUT "Network Action Point Error",
                     INPUT ("Network Action Point with ID: " + STRING(inpNetworkActionPointID) + " does NOT Exist."),
                     INPUT "",
                     INPUT intEmailGroupID).   
   
      oupResponse =  "Network Action Point with ID: " + STRING(inpNetworkActionPointID) + " does NOT Exist.".   
   
   END.  

   /* Set Startup Program Location */
   FIND FIRST Application NO-LOCK /* idx=ApplicationID */
      WHERE Application.ApplicationID = intGblApplicationID NO-ERROR.
   IF AVAILABLE Application THEN
   DO:
      chrCurrentDirectory = fGetCurrentDirectory().
          
      IF NUM-ENTRIES(chrCurrentDirectory,"/") > 2 THEN
         chrRootDirectory = "/" + ENTRY(2,chrCurrentDirectory,"/") + "/" + ENTRY(3,chrCurrentDirectory,"/").
          
      /* Add the specified directory to the propath of the session so program will pick up other dependant programs */
      IF chrRootDirectory > "" AND 
         readNetworkActionPoint.DirectoryToRunFrom <> "" THEN
      DO:
         IF fValidDirectory(chrRootDirectory + "/nirs/" + readNetworkActionPoint.DirectoryToRunFrom) = "Ok"  THEN
         DO:
            chrProgramToRun = chrRootDirectory + "/nirs/" + readNetworkActionPoint.DirectoryToRunFrom + "/core_shared/ndcServerDeviceWrite.p".
         END.
      END.
   END.     

   /* Socket to Connect to Device */
   CREATE SOCKET hdlListenerDevice.
               
   /* Test the Phyiscal Device is Online */
   hdlListenerDevice:CONNECT("-H " + readNetworkActionPoint.IPAddress + " -S " + readNetworkActionPoint.PortNumber + " -clientConnectTimeout 1000") NO-ERROR.
    
   /* Check Device is Connected and Update CanConnect */                  
   DO TRANS ON ERROR UNDO:
    
      /* Update NetworkActionPoint CanConnect to False if can NOT Physically Connect */
      FIND FIRST updNetworkActionPoint EXCLUSIVE-LOCK
         WHERE ROWID(updNetworkActionPoint) = ROWID(readNetworkActionPoint) NO-ERROR NO-WAIT.
      IF NOT AVAILABLE updNetworkActionPoint THEN
      DO:
         /* Send Email */
         RUN pSendEmail(INPUT "Network Action Point Locked",
                        INPUT ("Network ActionPoint: " + readNetworkActionPoint.NetworkActionPointName 
                                + " -H " + readNetworkActionPoint.IPAddress 
                                + " -S " + readNetworkActionPoint.PortNumber + " is Locked. Cannot Update. Start Up Aborted."),
                        INPUT "",
                        INPUT intEmailGroupID).        
		 
         oupResponse = "Network ActionPoint: " + readNetworkActionPoint.NetworkActionPointName 
                         + " -H " + readNetworkActionPoint.IPAddress 
                         + " -S " + readNetworkActionPoint.PortNumber + " is Locked. Cannot Update. Start Up Aborted.".	 
                         
         /* Delete Socket Objects */
         RUN pCleanUp.
                      
      END.   
       
      IF hdlListenerDevice:CONNECTED() THEN
         updNetworkActionPoint.CanConnect = TRUE.
      ELSE
      DO:   
         updNetworkActionPoint.CanConnect = FALSE.
          
         /* Send Email */
         RUN pSendEmail(INPUT "Network Action Point Offline",
                        INPUT ("Network Reader: " + readNetworkActionPoint.NetworkActionPointName 
                                 + " -H " + readNetworkActionPoint.IPAddress 
                                 + " -S " + readNetworkActionPoint.PortNumber + " is Offline. Server will still Start."),
                        INPUT "",
                        INPUT intEmailGroupID).
          
         oupResponse =  "Network Action Point: " + readNetworkActionPoint.NetworkActionPointName 
                          + " -H " + readNetworkActionPoint.IPAddress 
                          + " -S " + readNetworkActionPoint.PortNumber + " is Offline. Server will still Start.".                   
  
         /* Delete Socket Objects */
         RUN pCleanUp.
         		 
      END.   
       
      RELEASE updNetworkActionPoint NO-ERROR.

      /* Delete Socket Objects */
      RUN pCleanUp.
      
   END.            
    

   /* Socket to Connect to Device Server */
   CREATE SOCKET hdlListenerServer.

   /* Connect to Device Server */
   hdlListenerServer:CONNECT("-H " + readNetworkActionPoint.ServerIPAddress + " -S " + readNetworkActionPoint.ServerPortNumber) NO-ERROR.
                  
   /* If its NOT Connected we want to Restart and Alert */      
   IF NOT hdlListenerServer:CONNECTED() THEN      
   DO:

      /* Want to Re-Use to try Connect again after the Batch kicks off */
      RUN pCleanUp.         
      
      OUTPUT TO VALUE("../wrk/startwriter.log").
      
      /* Start the Action Point */   
      OS-COMMAND SILENT VALUE("../cronscripts/./runbatch_socket_utf8.sh '" + chrProgramToRun + "' '" 
                                + STRING(readNetworkActionPoint.NetworkActionPointID) + "'").      
      
      OUTPUT CLOSE.
                    
      /* Give the Batch Process a couple of Seconds to Start */
      PAUSE 2 NO-MESSAGE.
       
      /* Try again now after Batch Start */
      CREATE SOCKET hdlListenerServer.         
       
      /* Connect to Device */
      hdlListenerServer:CONNECT("-H " + readNetworkActionPoint.ServerIPAddress + " -S " + readNetworkActionPoint.ServerPortNumber) NO-ERROR.
           
      IF hdlListenerServer:CONNECTED() THEN      
      DO TRANS ON ERROR UNDO:
	  
         /* Update the Network ActionPoint Record */
         FIND FIRST updNetworkActionPoint EXCLUSIVE-LOCK
            WHERE ROWID(updNetworkActionPoint) = ROWID(readNetworkActionPoint) NO-ERROR NO-WAIT.
         IF NOT AVAILABLE updNetworkActionPoint THEN
         DO:   
            /* Send Email */
            RUN pSendEmail(INPUT "Network ActionPoint Locked",
                           INPUT ("Network ActionPoint: " + readNetworkActionPoint.NetworkActionPointName 
                                    + " -H " + readNetworkActionPoint.ServerIPAddress 
                                    + " -S " + readNetworkActionPoint.ServerPortNumber + " is Locked. Cannot Update. Start Up Aborted."),
                           INPUT "",
                           INPUT intEmailGroupID).   
                                 
            oupResponse = "Network ActionPoint: " + readNetworkActionPoint.NetworkActionPointName 
                            + " -H " + readNetworkActionPoint.ServerIPAddress 
                            + " -S " + readNetworkActionPoint.ServerPortNumber + " is Locked. Cannot Update. Start Up Aborted.".
            /* Delete Socket Objects */
            RUN pCleanUp.                           
                                        
         END.   

         /* Change to Listening */
         ASSIGN updNetworkActionPoint.IsOperating = TRUE.
         
         /* Create an Activity */
         CREATE NetworkActionPointActivity.
         ASSIGN NetworkActionPointActivity.NetworkActionPointActivityID = NEXT-VALUE(NetworkActionPointActivity)
                NetworkActionPointActivity.NetworkActionPointID = readNetworkActionPoint.NetworkActionPointID
                NetworkActionPointActivity.Started = fTimeStamp(NOW)
                NetworkActionPointActivity.Reason = "Manual Start"
                NetworkActionPointActivity.GateUserID = intGblUserID. 
                                   
         /* Send Email */
         RUN pSendEmail(INPUT "Network ActionPoint Startup",
                        INPUT ("Network ActionPoint: " + readNetworkActionPoint.NetworkActionPointName 
                                 + " -H " + readNetworkActionPoint.ServerIPAddress 
                                 + " -S " + readNetworkActionPoint.ServerPortNumber + " has been Started."),
                        INPUT "",
                        INPUT intEmailGroupID).       					
                         
      END.   
      ELSE
      DO:
	  /* Send Email */
         RUN pSendEmail(INPUT "Network ActionPoint Startup Error",
                        INPUT ("Cannot connect to Network Action Point: " + readNetworkActionPoint.NetworkActionPointName 
                                 + " -H " + readNetworkActionPoint.ServerIPAddress 
                                 + " -S " + readNetworkActionPoint.ServerPortNumber + ". Progress Error: " + ERROR-STATUS:GET-MESSAGE(1)),
                        INPUT "",
                        INPUT intEmailGroupID). 
  
	  oupResponse = "Cannot connect to Network ActionPoint: " + readNetworkActionPoint.NetworkActionPointName 
                         + " -H " + readNetworkActionPoint.ServerIPAddress 
                         + " -S " + readNetworkActionPoint.ServerPortNumber + ". Progress Error: " + ERROR-STATUS:GET-MESSAGE(1).
      END.	   	  
       
      RELEASE updNetworkActionPoint NO-ERROR.
                        
   END.   
   
   /* Delete Socket Objects */
   RUN pCleanUp.
    
END PROCEDURE.  

PROCEDURE pStopActionPointDevice:

   DEFINE INPUT  PARAMETER inpNetworkActionPointID AS INTEGER   NO-UNDO.
   DEFINE OUTPUT PARAMETER oupResponse 	          AS CHARACTER NO-UNDO.   
   
   FIND FIRST readNetworkActionPoint NO-LOCK
      WHERE readNetworkActionPoint.NetworkActionPointID = inpNetworkActionPointID NO-ERROR.

   /* Socket to Connect to Device Server */
   CREATE SOCKET hdlListenerServer.
    
   /* Connect to Device Server */
   hdlListenerServer:CONNECT("-H " + readNetworkActionPoint.ServerIPAddress + " -S " + readNetworkActionPoint.ServerPortNumber) NO-ERROR.
                   
   IF hdlListenerServer:CONNECTED() THEN
   DO TRANS ON ERROR UNDO:
          
      /* Create an Activity */
      CREATE NetworkActionPointActivity.
      ASSIGN NetworkActionPointActivity.NetworkActionPointActivityID = NEXT-VALUE(NetworkActionPointActivity)
             NetworkActionPointActivity.NetworkActionPointID = readNetworkActionPoint.NetworkActionPointID
             NetworkActionPointActivity.Stopped = fTimeStamp(NOW)
             NetworkActionPointActivity.Reason = "Manual Stop"             
             NetworkActionPointActivity.GateUserID = intGblUserID. 
                                          
      
      /* Issue StopProcess */
      chrDataToTransmit = "StopProcess".

      SET-SIZE(mptTransmittedData) = LENGTH(chrDataToTransmit) + 2.
      PUT-STRING(mptTransmittedData, 2) = chrDataToTransmit.
 
      hdlListenerServer:WRITE(mptTransmittedData, 1, GET-SIZE(mptTransmittedData)).

      RUN pCleanUp.      
      
   END.
   ELSE
   DO:
      /* Send Email */
      RUN pSendEmail(INPUT "Network Action Point Shutdown Error",
                     INPUT ("Cannot connect to Network ActionPoint: " + readNetworkActionPoint.NetworkActionPointName 
                              + " -H " + readNetworkActionPoint.ServerIPAddress 
                              + " -S " + readNetworkActionPoint.ServerPortNumber + ". Progress Error: " + ERROR-STATUS:GET-MESSAGE(1)),
                     INPUT "",
                     INPUT intEmailGroupID). 
	     
      oupResponse = "Cannot connect to Network Action Point: " + readNetworkActionPoint.NetworkActionPointName 
                      + " -H " + readNetworkActionPoint.ServerIPAddress 
                      + " -S " + readNetworkActionPoint.ServerPortNumber + ". Progress Error: " + ERROR-STATUS:GET-MESSAGE(1).
   END.   

END PROCEDURE.

PROCEDURE pCleanUp:

   SET-SIZE(mptTransmittedData) = 0.
  
   IF VALID-HANDLE(hdlListenerServer) AND hdlListenerServer:CONNECTED() THEN
      hdlListenerServer:DISCONNECT() NO-ERROR.
      
   IF VALID-HANDLE(hdlListenerDevice) AND hdlListenerDevice:CONNECTED() THEN      
      hdlListenerDevice:DISCONNECT() NO-ERROR.
      
   /* Clean Up */
   IF VALID-HANDLE(hdlListenerServer) THEN
      DELETE OBJECT hdlListenerServer NO-ERROR.
      
   IF VALID-HANDLE(hdlListenerDevice) THEN       
      DELETE OBJECT hdlListenerDevice NO-ERROR.

END PROCEDURE.

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
