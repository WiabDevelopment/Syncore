/*------------------------------------------------------------------------------------------------------------------------------------------
Program : ndcWriteDeviceControl.p
Purpose : Start/Stop Network Write Devices and check they are Running and Online

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
{prcActionPointDeviceProcedures.i}
      
/* Buffers */
DEFINE BUFFER checkNetworkActionPoint FOR NetworkActionPoint.      
      
/* Local Variables */
DEFINE VARIABLE chrResponse AS CHARACTER NO-UNDO.   
   
Main_Block:
DO ON ERROR UNDO, LEAVE:

   /* Check All Action Point Devices that Should be Running */
   Check_Reader:
   FOR EACH NetworkActionPoint NO-LOCK
      WHERE NetworkActionPoint.StartOperating
      AND   NetworkActionPoint.Active:
  
      /* Check Online */
      RUN pCheckConnection(INPUT NetworkActionPoint.NetworkActionPointID) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN
      DO:
         RETURN ERROR-STATUS:GET-MESSAGE(1).
      END.                
      
      FIND checkNetworkActionPoint NO-LOCK
         WHERE ROWID(checkNetworkActionPoint) = ROWID(NetworkActionPoint) NO-ERROR.
         
      IF checkNetworkActionPoint.IsOperating THEN NEXT.        
      
      /* Run the Startup Procedure */
      RUN pStartActionPointDevice(INPUT NetworkActionPoint.NetworkActionPointID,
                                  OUTPUT chrResponse) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN
      DO:
         RETURN ERROR-STATUS:GET-MESSAGE(1).         
      END.             
      
   END.   
           
END.
