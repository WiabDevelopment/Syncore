/*------------------------------------------------------------------------------------------------------------------------------------------
Program : lodScanCloseTrailer.p
Purpose : Generic program to check if CloseTrailer has been scanned
         
          Possible results: Yes, No
         
Author  : DCummins
Date    : 12/03/2013
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
   
   /* Local Objects */
   DEFINE VARIABLE chrSsnLastScan AS sessionValue NO-UNDO.
   
   /* Get Current Data */
   chrSsnLastScan = fGetSessionValue("LastScan").
   
   Main_Block:
   DO ON ERROR UNDO, LEAVE:
      
      CASE chrSsnLastScan:chrValue:
         
         WHEN "CloseTrailer" THEN
         DO:
            chrResult = "Yes".
            LEAVE Main_Block.
         END.
         
         OTHERWISE
         DO:
            chrResult = "No".
            LEAVE Main_Block.
         END.
         
      END CASE. /* CASE chrSsnLastScan:chrValue */
      
   END. /* Main_Block */
   
   DELETE OBJECT chrSsnLastScan NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */

