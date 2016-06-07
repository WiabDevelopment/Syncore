/*------------------------------------------------------------------------------------------------------------------------------------------
Program : ndcReadDeviceControl.p
Purpose : Start/Stop Network Read Devices and check they are Running and Online

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
{fncServerFunctions.i}
{fncLoggingFunctions.i}
{fncStatusTypeFunctions.i}
{prcReaderDeviceProcedures.i}

/* Buffers */
DEFINE BUFFER checkNetworkReader FOR NetworkReader.
   
/* Local Variables */
DEFINE VARIABLE chrResponse AS CHARACTER NO-UNDO.   
   
Main_Block:
DO ON ERROR UNDO, LEAVE:

   /* Check All Reader Devices that Should be Running */
   Check_Reader:
   FOR EACH NetworkReader NO-LOCK
      WHERE NetworkReader.StartListening
      AND   NetworkReader.Active:
      
      /* Check Online */
      RUN pCheckConnection(INPUT NetworkReader.NetworkReaderID) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN
      DO:
         RETURN ERROR-STATUS:GET-MESSAGE(1).
      END.                            
      
      FIND checkNetworkReader NO-LOCK
         WHERE ROWID(checkNetworkReader) = ROWID(NetworkReader) NO-ERROR.
         
      IF checkNetworkReader.IsListening THEN NEXT.   
      
      /* Run the Startup Procedure */
      RUN pStartReaderDevice(INPUT NetworkReader.NetworkReaderID,
                             OUTPUT chrResponse) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN
      DO:
         RETURN ERROR-STATUS:GET-MESSAGE(1).         
      END.             
                       
   END.   
           
END.


