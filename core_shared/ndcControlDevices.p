/*------------------------------------------------------------------------------------------------------------------------------------------
Program : ndcControlReaderDevices.p
Purpose : Start/Stop Network Reader Devices and check they are Running

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
DEFINE VARIABLE hdlListenerSocket   AS HANDLE    NO-UNDO.
DEFINE VARIABLE intEmailGroupID      AS INTEGER   NO-UNDO INITIAL 1 /* SuperUsers */.

/* Get Defaults */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "NetworkReaderAlerts" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intEmailGroupID = 1.
   
Main_Block:
DO ON ERROR UNDO, LEAVE:

   /* Check All Reader Devices that Should be Running */
   Check_Reader:
   FOR EACH NetworkReader NO-LOCK
      WHERE NetworkReader.StartListening
      AND   NetworkReader.Active
      AND   NetworkReader.NetworkReaderID = 1:
      
      /* Try to Connect to each Device */
      CREATE SOCKET hdlListenerSocket.
      
      /* Connect to Device */
      hdlListenerSocket:CONNECT("-H " + NetworkReader.IPAddress + " -S " + NetworkReader.PortNumber) NO-ERROR.
                     
      /* If its NOT Connected we want to Alert and Log and Restart */      
      IF NOT hdlListenerSocket:CONNECTED() THEN      
      DO:
               
         OS-COMMAND SILENT VALUE("mbpro -p ../core_shared/ndcServerListener.p -param " + STRING(NetworkReader.NetworkReaderID) + " -pf ../stopstart/pf_batch.pf").
         
         /* Connect to Device */
         hdlListenerSocket:CONNECT("-H " + NetworkReader.IPAddress + " -S " + NetworkReader.PortNumber) NO-ERROR.
             
         IF hdlListenerSocket:CONNECTED() THEN      
         DO TRANS ON ERROR UNDO:
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
                           
               NEXT Check_Reader.                
            END.   
            
            /* Change to Listening */
            ASSIGN NetworkReader.IsListening = TRUE.
                        
         END.   
         
         RELEASE updNetworkReader NO-ERROR.
                          
      END.
      
      /* Clean Up */
      DELETE OBJECT hdlListenerSocket NO-ERROR.
      
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

