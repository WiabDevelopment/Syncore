/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsCheckScannedPrinter.p
Purpose : Checks whether a user has scanned the printer before.

          Possible Results : Yes/No        

Author  : Mateusz Nogaj
Date    : 17/01/2014
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
   DEFINE VARIABLE chrSsnPrinterID AS sessionValue NO-UNDO.

   /* Get Current Data */
   chrSsnPrinterID = fGetSessionValue("PrinterID").

   Main_Block:
   DO ON ERROR UNDO:
      
      IF chrSsnPrinterID:intValue > 0 THEN
      DO:
         chrResult = "Yes".
         LEAVE Main_Block.
      END.
      ELSE 
      DO:
         chrResult = "No".
         LEAVE Main_Block.
      END.

   END. /*Main_Block: DO ON ERROR UNDO:*/
   
   /* Clean Up */
   DELETE OBJECT chrSsnPrinterID NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
