/*------------------------------------------------------------------------------------------------------------------------------------------
Program : ndcSendPLCData.p
Purpose : Sends data to PLC controller from values extracted from NetworkAction records based on index  NetworkActionID 

Author  : Kitty Jose
Date    : 20/10/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Custom Includes */
{defSessionVariables.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Input Parameters */
DEFINE INPUT PARAMETER intNetworkActionID  AS INTEGER NO-UNDO.

/* Buffers */
DEFINE BUFFER updNetworkAction FOR NetworkAction.

/* Local Variables */
DEFINE VARIABLE intEmailGroupID AS INTEGER NO-UNDO.
DEFINE VARIABLE hdlSocket AS HANDLE  NO-UNDO.
DEFINE VARIABLE logResponse     AS LOGICAL NO-UNDO.

/* Get Defaults */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "NetworkPLCAlerts" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intEmailGroupID = 1.

Main_Block:
DO ON ERROR UNDO, LEAVE:      

   /* Get Network Action */
   FIND FIRST NetworkAction NO-LOCK 
      WHERE NetworkAction.NetworkActionID = intNetworkActionID NO-ERROR.
   IF AVAILABLE NetworkAction THEN 
   DO:    
      /* Get Network Action Point */   
      FIND FIRST NetworkActionPoint OF NetworkAction NO-LOCK NO-ERROR.
      IF AVAILABLE NetworkActionPoint THEN 
      DO TRANS ON ERROR UNDO:    
          
         /* Cheking Connection */
         CREATE SOCKET hdlSocket.

         /* Connect to Device */
         logResponse = hdlSocket:CONNECT(SUBSTITUTE('-H &1 -S &2',NetworkActionPoint.IPAddress,NetworkActionPoint.PortNumber)) NO-ERROR.                           
         
         /*If Unsuccessfull connection*/
         IF logResponse = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN
         DO:   
            /* IF Server IP address and Server port are blank */
            IF NetworkActionPoint.IPAddress = '' OR NetworkActionPoint.PortNumber = '' THEN 
            DO:
               /* Send Email if NetworkActionPoint IP Address and Port Number is blank */
               RUN pSendEmail("Network Action Connection Error",
                              "No IPAddress and Port Number for Network Action ID: " + STRING(NetworkAction.NetworkActionPointID) + ". Cannot Connect.",
                              "",
                              intEmailGroupID).     
               LEAVE Main_Block.       
            END. /* IF IP Server address and Server port are blank */
            ELSE  /* IF IP Server address and Server port are invalid*/
            DO:
               /* Send Email if cannot connect to NetworkActionPoint IP Address and Port Number */
               RUN pSendEmail("Network Action Connection Error",
                              "Invalid IPAddress and Port Number for Network Action ID: " + STRING(NetworkAction.NetworkActionPointID) + ". Cannot Connect.",
                              "",
                              intEmailGroupID).   
               LEAVE Main_Block.      
            END. /* IF Server IP address and Server port are invalid*/
         END.   /*If Unsuccessfull connection*/
         
         hdlSocket:DISCONNECT() NO-ERROR.
         
         DELETE OBJECT hdlSocket NO-ERROR.
         
         /* Add to capture output */
         OUTPUT TO VALUE("../wrk/plc.log") APPEND.
         
         /* Trigger the Send Data to the PLC */                      
         OS-COMMAND SILENT VALUE (SUBSTITUTE("/usr/local/plc/bin/./setval &1 &2 &3", 
                                             NetworkActionPoint.IPAddress,
                                             NetworkActionPoint.Register,
                                             NetworkAction.ActionValue + " &")).
                                             
         OUTPUT CLOSE.                                    

         /* Update Network Action to Completed */                                      
         FIND updNetworkAction EXCLUSIVE-LOCK
            WHERE ROWID(updNetworkAction) = ROWID(NetworkAction) NO-ERROR NO-WAIT.                                                   
         IF LOCKED NetworkAction THEN 
         DO:
            /* Send Email if NetworkActionPoint does NOT exist */
            RUN pSendEmail("Network Action Update Error",
                           "Network Action for Network Action ID: " + STRING(NetworkAction.NetworkActionPointID) + " is Locked. Cannot Update.",
                           "",
                           intEmailGroupID).    
            LEAVE Main_Block.         
         END. /* IF LOCKED */
         ELSE
         DO:
            ASSIGN NetworkAction.Completed  = fTimeStamp(NOW).         
         END.
         
         /* Release Update */
         RELEASE updNetworkAction NO-ERROR.
         
      END.  /* FIND NetworkActionPoint */
      ELSE
      DO:
         /* Send Email if NetworkActionPoint does NOT exist */
         RUN pSendEmail("Network Action Point Record Not Found",
                        "No Network Action Point Exist for Network Action Point ID: " + STRING(NetworkAction.NetworkActionPointID),
                        "",
                        intEmailGroupID).    
         LEAVE Main_Block.         
      END.
      
   END.   /* FIND NetworkAction */
   ELSE
   DO:
      /* Send Email if NetworkAction does NOT exist */
      RUN pSendEmail("Network Action Record Not Found",
                     "No Network Action Exist for Network Action ID: " + STRING(intNetworkActionID),
                     "",
                     intEmailGroupID).
      LEAVE Main_Block. 
   END.   
 
END.   /* Main_Block */
   
  
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










