/*------------------------------------------------------------------------------------------------------------------------------------------
Program : psExit.p
Purpose : Generic Exit program which notifies psProcessEngine.p to stop.
         
          Possible results: EXIT
         
Author  : DC
Date    : 19th June 2012
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
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Dump the Temp Data */
   RUN pFlushCloneTables IN hdlGblLibrary.
   
   chrResult = "EXIT".
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */

