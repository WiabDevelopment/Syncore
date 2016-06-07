/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsCheckPrinterForLocation.p
Purpose : Check the if printer is setup for build location.
          
          Possible results: Yes, No
          
Author  : BR
Date    : 21st August 2015
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
   
   /* Session Objects */      
   DEFINE VARIABLE intSsnBuildLocationID AS sessionValue NO-UNDO.
   
   /* Get Current Session Data */
   intSsnBuildLocationID  = fGetSessionValue("BuildLocationID").
   
   Main_Block:
   DO ON ERROR UNDO:
      
      FIND FIRST PrinterType NO-LOCK
         WHERE PrinterType.TypeCode  = "Label" NO-ERROR.
      IF NOT AVAILABLE PrinterType THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "PrinterType [Label] does not exist.").
         LEAVE Main_Block.
      END.
      
      FIND FIRST Location NO-LOCK 
         WHERE Location.LocationID = intSsnBuildLocationID:intValue NO-ERROR. 
      IF NOT AVAILABLE Location THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "LocationID: " + STRING (intSsnBuildLocationID:intValue) + " does not exist.").
         LEAVE Main_Block. 
      END.       
 
      PrinterLoop:
      FOR EACH PrinterLocationLink NO-LOCK 
         WHERE PrinterLocationLink.LocationID = Location.LocationID
         AND   PrinterLocationLink.Active,
         FIRST Printer OF PrinterLocationLink NO-LOCK
         WHERE Printer.PrinterTypeID = PrinterType.PrinterTypeID
         AND Printer.Active:
            
         LEAVE PrinterLoop.
      END. /* FOR EACH PrinterLocationLink */

      IF NOT AVAILABLE Printer THEN
      DO:
         RUN DisplayMessage("Message",
                            "No Printer linked to Location: " + Location.LocationRef  + ". Proceed to Scan Printer.").                        
         chrResult = "No".
                            
         LEAVE Main_Block.
      END.
      
      /* Printer linked */
      chrResult = "Yes".
            
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnBuildLocationID NO-ERROR.
   
   /* Releases */
   RELEASE Location             NO-ERROR.
   RELEASE PrinterLocationLink  NO-ERROR.
   RELEASE Printer              NO-ERROR.
   RELEASE PrinterType          NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END.  /* CTRL-C Catch */
