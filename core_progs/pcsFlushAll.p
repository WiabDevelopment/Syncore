/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsFlushAll.p
Purpose : This Flushes the current Transaction anywhere in the map
         
          Possible results: Flushed
         
Author  : MChereches
Date    : 15th January 2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
06/06/2014 BR  All        CR 1052 Standardization Project
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
   
   Main_Block:
   DO ON ERROR UNDO:
      
      RUN pFlushCloneTablesExceptSession IN hdlGblLibrary.

      /* Check Errors */
      IF chrError <> "" THEN
      DO:
         RUN DisplayError("Update Error",
                           chrError).
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      chrResult = "Flushed".
      
   END. /* Main_Block */
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */

