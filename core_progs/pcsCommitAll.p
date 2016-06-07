/*------------------------------------------------------------------------------------------------------------------------------------------
Program : psCommitAll.p
Purpose : This Commits the current Transaction anywhere in the map and returns Error
         
          Possible results: Committed
         
Author  : DCummins
Date    : 3rd December 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
13/03/2014 BR  All        Standardization Project
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
   
   /* Local Variables */
   DEFINE VARIABLE logAllWentOk AS LOGICAL NO-UNDO.
   
   Main_Block:
   DO ON ERROR UNDO:
      
      RUN pCommitAll IN hdlGblLibrary(INPUT  chrError,
                                      OUTPUT logAllWentOK,
                                      OUTPUT chrError).
      /* Check Errors */
      IF chrError <> "" THEN
      DO:
         RUN DisplayError("Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      chrResult = "Committed".
      
   END. /* Main_Block */
   
   /* Clean Up */
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */

