/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncLabelPrintFunctions.i
Purpose : Character & web functions for Zebra Label Printing
Author  : DC
Date    : 5th June 2012  
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03Apr2013  MC  DayMen     If LabelFile.DeleteBlankBarCodes is Yes, if a barcode value is blank, delete the file lines that define 
                          the barcode so that the barcode won't show
24/06/2014 BG  DayMen     Added another entry to the Unix printing errors list for printers set up for Remote printing
31/06/2014 MC  All        UnixName changes according to Printer.PrintingMode
08/10/2014 MC  GHDDE      Cater for Nirs path   
23/10/2014 MC  GHDDE      global change request number is now a char
30/03/2015 BG  Canon      Added error checking and FIND for LabelType OF LabelFile.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* NOTE:  This Include has Dependencies on... */
/*{defSessionVariables.i}*/
/*{fncGlobalFunctions.i} */
/*{fncDateFunctions.i}   */

/* Streams */        
DEFINE STREAM sLabel.                            

/* Temp Tables */
DEFINE TEMP-TABLE ttFileLine
   FIELD LineNumber      AS INTEGER
   FIELD LineString      AS CHARACTER
   INDEX IndexLineNumber AS PRIMARY LineNumber 
   INDEX IndexLineString LineString. 

DEFINE TEMP-TABLE ttBarCode
   FIELD FirstLine       AS CHARACTER
   FIELD SecondLine      AS CHARACTER.

DEFINE TEMP-TABLE ttFileField
   FIELD FieldName       AS CHARACTER
   FIELD FieldData       AS CHARACTER
   FIELD Is2DBarcode     AS LOGICAL.

DEFINE VARIABLE logDeleteBlankBarCodes AS LOGICAL   NO-UNDO.
DEFINE VARIABLE intFileLineNumber      AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrActualPrinterName   AS CHARACTER NO-UNDO.


/*/* Pull the Definition for Replacement */                                                                         */
/*FUNCTION fGetDefinition RETURNS CHARACTER(INPUT chrProcess AS CHARACTER,                                          */
/*                                          INPUT chrLabel   AS CHARACTER):                                         */
/*                                                                                                                  */
/*   DEFINE VARIABLE intBarcodeLine    AS INTEGER   NO-UNDO.                                                        */
/*   DEFINE VARIABLE chrDefinitionFile AS CHARACTER NO-UNDO.                                                        */
/*                                                                                                                  */
/*                                                                                                                  */
/*   /* Validate File */                                                                                            */
/*   FIND FIRST LabelFile NO-LOCK                                                                                   */
/*        WHERE LabelFile.LabelType = chrProcess                                                                    */
/*        AND   LabelFile.LabelName = chrLabel NO-ERROR.                                                            */
/*   IF NOT AVAILABLE LabelFile THEN                                                                                */
/*   DO:                                                                                                            */
/*      RETURN "No Label record Available for Process:" + chrProcess + " and Label:" + chrLabel + ".".              */
/*   END.                                                                                                           */
/*                                                                                                                  */
/*   /* Set the file name acording to LabelFile setting */                                                          */
/*   chrDefinitionFile = LabelFile.DefinitionFile.                                                                  */
/*                                                                                                                  */
/*   /* If program is run under a Nirs number, try to find the file under the Nirs path */                          */
/*   IF chrGblChangeRequestNo <> "0" AND chrGblChangeRequestNo <> "" THEN /* /label_templates/recStockPackage.prn */*/
/*   DO:                                                                                                            */
/*      chrDefinitionFile = REPLACE(LabelFile.DefinitionFile, "label_templates",                                    */
/*                          "nirs/" + chrGblChangeRequestNo + "/label_templates").                                  */
/*      /* If the file is not found under the Nirs path, set the file name back to LabelFile setting */             */
/*      IF SEARCH(chrDefinitionFile) = ? THEN                                                                       */
/*         chrDefinitionFile = LabelFile.DefinitionFile.                                                            */
/*   END.                                                                                                           */
/*                                                                                                                  */
/*   IF SEARCH(chrDefinitionFile) = ? THEN                                                                          */
/*   DO:                                                                                                            */
/*      RETURN "Cannot find Label File: " + chrDefinitionFile + ".".                                                */
/*   END.                                                                                                           */
/*                                                                                                                  */
/*   INPUT STREAM sLabel FROM VALUE(chrDefinitionFile).                                                             */
/*                                                                                                                  */
/*   intFileLineNumber = 1.                                                                                         */
/*   REPEAT:                                                                                                        */
/*      CREATE ttFileLine.                                                                                          */
/*      IMPORT STREAM sLabel UNFORMATTED ttFileLine.LineString.                                                     */
/*      ttFileLine.LineNumber = intFileLineNumber.                                                                  */
/*      intFileLineNumber = intFileLineNumber + 1.                                                                  */
/*   END.                                                                                                           */
/*                                                                                                                  */
/*   INPUT STREAM sLabel CLOSE.                                                                                     */
/*                                                                                                                  */
/*   /* Find all barcodes; store the barcode lines in ttBarCode temp-table */                                       */
/*   logDeleteBlankBarCodes = LabelFile.DeleteBlankBarCodes.                                                        */
/*   IF logDeleteBlankBarCodes THEN                                                                                 */
/*   DO:                                                                                                            */
/*      intBarcodeLine = 1.                                                                                         */
/*      FOR EACH ttFileLine USE-INDEX IndexLineNumber NO-LOCK:                                                      */
/*                                                                                                                  */
/*         /* If this is the second line of the bar code, add it to the ttBarCode record */                         */
/*         IF intBarcodeLine = 2 THEN                                                                               */
/*         DO:                                                                                                      */
/*            ttBarcode.SecondLine = ttFileLine.LineString.                                                         */
/*            intBarcodeLine = 1.                                                                                   */
/*         END.                                                                                                     */
/*                                                                                                                  */
/*         /* If this is the first line of a bar code, create a ttBarCode record */                                 */
/*         IF ttFileLine.LineString BEGINS "^BY" THEN                                                               */
/*         DO:                                                                                                      */
/*            CREATE ttBarCode.                                                                                     */
/*            ttBarCode.FirstLine = ttFileLine.LineString.                                                          */
/*            intBarcodeLine = intBarcodeLine + 1.                                                                  */
/*         END.                                                                                                     */
/*      END.                                                                                                        */
/*   END.                                                                                                           */
/*                                                                                                                  */
/*   RETURN "Ok".                                                                                                   */
/*                                                                                                                  */
/*END FUNCTION. /*FUNCTION fGetDefinition*/                                                                         */


FUNCTION fGetLabelFile RETURNS CHARACTER(INPUT intOperationTypeID AS INTEGER,
                                         INPUT chrLabelName       AS CHARACTER,
                                         INPUT logIsTempLabel     AS LOGICAL):
   
   DEFINE VARIABLE intBarcodeLine    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE chrFilePath       AS CHARACTER NO-UNDO.
   
   DEFINE BUFFER readOperationType FOR OperationType.
   
   FIND FIRST LabelFile NO-LOCK 
      WHERE LabelFile.OperationTypeID = intOperationTypeID 
      AND   LabelFile.LabelName       = chrLabelName       
      AND   LabelFile.IsTempLabel     = logIsTempLabel NO-ERROR.
   
   IF NOT AVAILABLE LabelFile THEN 
   DO:
      FIND FIRST readOperationType NO-LOCK
         WHERE readOperationType.OperationTypeID = intOperationTypeID NO-ERROR.
      IF NOT AVAILABLE readOperationType THEN
      DO:
         RETURN "No OperationType Available for ID:" + STRING(intOperationTypeID).
      END.
      
      RETURN "No Label record Available for OperationType:" +  readOperationType.TypeCode 
                + " LabelName:" + chrLabelName + " and IsTempLabel:" + STRING(logIsTempLabel) + ".".
   END.
   /* Set the file name acording to LabelFile setting */
   chrFilePath = TRIM(LabelFile.FilePath, "/") + "/" + LabelFile.FileName.
   
   /* If program is run under a Nirs number, try to find the file under the Nirs path */
   IF chrGblChangeRequestNo <> "" AND chrGblChangeRequestNo <> "0" AND chrGblChangeRequestNo <> "Base" THEN /* /label_templates/recStockPackage.prn */
   DO:
      chrFilePath = REPLACE(chrFilePath, "label_templates", 
                          "nirs/" + chrGblChangeRequestNo + "/label_templates").
      /* If the file is not found under the Nirs path, set the file name back to LabelFile setting */
      IF SEARCH(chrFilePath) = ? THEN
         RETURN "Cannot find Label File: " + chrFilePath + ".".
   END.
   
   IF SEARCH(chrFilePath) = ? THEN
   DO:
      RETURN "Cannot find Label File: " + chrFilePath + ".".
   END.
   
   INPUT STREAM sLabel FROM VALUE(chrFilePath).
   
   intFileLineNumber = 1.
   REPEAT:
      CREATE ttFileLine.
      IMPORT STREAM sLabel UNFORMATTED ttFileLine.LineString.
      ttFileLine.LineNumber = intFileLineNumber.
      intFileLineNumber = intFileLineNumber + 1.
   END.
   
   INPUT STREAM sLabel CLOSE.
   
   /* Find all barcodes; store the barcode lines in ttBarCode temp-table */
   logDeleteBlankBarCodes = LabelFile.DeleteBlankBarCodes.
   IF logDeleteBlankBarCodes THEN 
   DO:
      intBarcodeLine = 1.
      FOR EACH ttFileLine USE-INDEX IndexLineNumber NO-LOCK:
         
         /* If this is the second line of the bar code, add it to the ttBarCode record */
         IF intBarcodeLine = 2 THEN
         DO:
            ttBarcode.SecondLine = ttFileLine.LineString.
            intBarcodeLine = 1.
         END.
         
         /* If this is the first line of a bar code, create a ttBarCode record */
         IF ttFileLine.LineString BEGINS "^BY" THEN
         DO:
            CREATE ttBarCode.
            ttBarCode.FirstLine = ttFileLine.LineString.
            intBarcodeLine = intBarcodeLine + 1.
         END.
      END.
   END.
   
   RETURN "Ok".
   
END FUNCTION. /*FUNCTION fGetLabelFile*/


FUNCTION fSubstituteField RETURNS CHARACTER(INPUT chrName AS CHARACTER,
                                            INPUT chrData AS CHARACTER):

   CREATE ttFileField.
   ASSIGN ttFileField.FieldName = chrName
          ttFileField.FieldData = TRIM(chrData).

END FUNCTION. /*FUNCTION fSubstituteField*/


FUNCTION fSubstitute2DBarcode RETURNS CHARACTER(INPUT chrName          AS CHARACTER,
                                                INPUT chrData          AS CHARACTER,
                                                INPUT intX             AS INTEGER,
                                                INPUT intY             AS INTEGER,
                                                INPUT intMagnification AS INTEGER,
                                                INPUT chrNumBytes      AS CHARACTER):

   CREATE ttFileField.
   ASSIGN ttFileField.FieldName = chrName
          ttFileField.FieldData = "^FT" + STRING(intX) + "," + STRING(intY) + "^BQN,2," + STRING(intMagnification) + "^FDMM,B" 
                                     + STRING(chrNumBytes) + chrData + "^FS"
          ttFileField.Is2DBarcode = TRUE.
  
END FUNCTION. /*FUNCTION fSubstitute2DBarcode*/


FUNCTION fCheckPrinter RETURNS CHARACTER(INPUT chrPrinterName AS CHARACTER):
   
   DEFINE VARIABLE chrReturnValue     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrBadEntriesList  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intCounter         AS INTEGER   NO-UNDO.
   
   /* First entry here is definitely correct and returns when a printer is set up to print in Network mode via Unix spooler         */
   /* Second entry is taken from a "Warning: tlb3zn046252 is down" message returned from a printer which was prining in Remote mode */
   chrBadEntriesList = "not a request id or a destination,is down".
   
   /* This uses the "lpstat" Unix command to check on the Status of the Printer name. Will only work on Unix. */
   INPUT THROUGH VALUE("lpstat " + chrPrinterName).
      
      REPEAT:
         IMPORT UNFORMATTED chrReturnValue.
         
         DO intCounter = 1 TO NUM-ENTRIES(chrBadEntriesList):
            
            /* Look for the bad entry string in the full return value */
            IF INDEX(chrReturnValue, ENTRY(intCounter,chrBadEntriesList)) <> 0 THEN
            DO:
               chrReturnValue = "The Printer " + chrPrinterName + " is not set up in UNIX correctly. " + chrReturnValue + " ." +
                                   "Please log a ticket for the IT Unix Admin team to solve this " +
                                   "issue @ <a href='http://itsupport' target='_blank'>http://itsupport</a>".
               RETURN chrReturnValue.
            END.
            
         END. /*DO intCounter = 1 to NUM-ENTRIES(chrBadEntriesList):*/
      END. /*REPEAT:*/
      
   INPUT CLOSE.
   
   RETURN "Ok".
   
END FUNCTION. /* fCheckPrinter */


FUNCTION fCancelPrintQueue RETURNS CHARACTER(INPUT chrPrinterName AS CHARACTER):
   
   UNIX SILENT VALUE("cancel -a " + chrPrinterName).
   
   RETURN "Ok".
   
END FUNCTION. /* fCancelPrintQueue */


FUNCTION fPrintTMSFile RETURNS CHARACTER (INPUT intShipPackageID AS INTEGER,
                                          INPUT intPrinterID     AS INTEGER):
  
   
   DEFINE BUFFER otherShipPackage FOR ShipPackage.
   
   DEFINE VARIABLE chrPrintFile   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrReturnValue AS CHARACTER NO-UNDO.
   
   FIND FIRST otherShipPackage NO-LOCK 
      WHERE otherShipPackage.ShipPackageID = intShipPackageID NO-ERROR.
      
   IF NOT AVAILABLE otherShipPackage THEN 
       RETURN "No ShipPackage Exists for ID:" + STRING(intShipPackageID).

   FIND FIRST Printer NO-LOCK 
      WHERE Printer.PrinterID = intPrinterID NO-ERROR.
   IF NOT AVAILABLE Printer THEN 
      RETURN "No Printer Exists for ID:" + STRING(intPrinterID).
   
   FIND FIRST FileMaster NO-LOCK 
      WHERE FileMaster.MasterName = "TmsZplLabelImport" NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN 
      RETURN "No FileMaster Exists for TmsZplLabelImport".
   
   FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FileType THEN 
      RETURN "No FileType Exists for FileMaster TmsZplLabelImport".
   
   chrPrintFile = FileMaster.FilePath + "label" + STRING(otherShipPackage.CarrierTrackingRef) + "." + LC(FileType.TypeCode).
   /* Print the File */
   IF Printer.PrintingMode = "Network" THEN 
      chrActualPrinterName = Printer.OSNetworkPrinterName.
   ELSE 
      chrActualPrinterName = Printer.OSRemotePrinterName.
   
   /* Checks that the Printer is set up correctly on Unix */
   chrReturnValue = fCheckPrinter(chrActualPrinterName).
   
   IF chrReturnValue <> "Ok" THEN
      RETURN chrReturnValue.
   
   /* Print */
   UNIX SILENT VALUE("lp -d " + chrActualPrinterName + " -s " + chrPrintFile).
   
   RETURN "Ok".
                                             
END FUNCTION. /* fPrintTMSFile */



FUNCTION fPrintFile RETURNS CHARACTER (INPUT intPrinterID AS INTEGER):
   
   DEFINE VARIABLE chrPrintFile   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrReturnValue AS CHARACTER NO-UNDO.
   
   IF NOT AVAILABLE LabelType THEN
   DO:
      FIND FIRST LabelType OF LabelFile NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LabelType THEN
      DO:
         RETURN "No LabelType Exists for LabelFile:" + LabelFile.FileName.
      END.
   END.
   
   IF logDeleteBlankBarCodes THEN
   DO:
      /* Go through all file fields with blank value */
      FOR EACH ttFileField NO-LOCK
         WHERE ttFileField.FieldData = "":
         
         FOR EACH ttBarCode NO-LOCK:
            
            /* Find the file field that represents a barcode */
            IF INDEX(ttBarCode.SecondLine, ttFileField.FieldName) > 0 THEN 
            DO:
               /* Delete the file lines that define the barcode, so the barcode won't show */
               FIND FIRST ttFileLine /*idx=IndexLineString*/
                  WHERE ttFileLine.LineString = ttBarCode.FirstLine NO-ERROR.
               
               IF AVAILABLE ttFileLine THEN
                  DELETE ttFileLine.
               
               FIND FIRST ttFileLine /*idx=IndexLineString*/
                  WHERE ttFileLine.LineString = ttBarCode.SecondLine NO-ERROR.
               
               IF AVAILABLE ttFileLine THEN
                  DELETE ttFileLine.
            END.
         END. /* FOR EACH ttBarCode */
      END. /* FOR EACH ttFileField */
   END. /* IF logDeleteBlankBarCodes */
   
   
   /* Read through the file and substitute the fields for data */
   FOR EACH ttFileLine NO-LOCK /*idx=IndexLineNumber*/
      BY LineNumber:
      
      FOR EACH ttFileField NO-LOCK:
         
         /* Replace the field name with the value if found */
         IF INDEX(ttFileLine.LineString, ttFileField.FieldName) > 0 THEN
         DO:
            /* When 2D we need to replace the Whole Line */
            IF ttFileField.Is2DBarcode THEN
            DO:
               ttFileLine.LineString = ttFileField.FieldData.
            END.
            ELSE 
            DO:
               ttFileLine.LineString = REPLACE(ttFileLine.LineString, ttFileField.FieldName, ttFileField.FieldData).
            END.

         END. /*IF INDEX(ttFileLine.LineString, ttFileField.FieldName) > 0 THEN*/
      END. /*FOR EACH ttFileField NO-LOCK:*/
   END. /*FOR EACH ttFileLine NO-LOCK:*/
   
   /* This function is in fncDateFunctions.i so dependency on that having been included in the Parent program */
   chrPrintFile = "../tmp/" + LabelFile.FilePrefix + fTimeStamp(NOW) + "." + LabelType.Extension.
   
   /* Print the File */
   OUTPUT STREAM sLabel TO VALUE(chrPrintFile) CONVERT TARGET "UTF-8".
      
      FOR EACH ttFileLine USE-INDEX IndexLineNumber NO-LOCK:
         PUT STREAM sLabel UNFORMATTED ttFileLine.LineString.
      END.
   OUTPUT STREAM sLabel CLOSE.
   
   FIND FIRST Printer NO-LOCK 
      WHERE Printer.PrinterID = intPrinterID NO-ERROR.
   IF NOT AVAILABLE Printer THEN 
      RETURN "No Printer Exists for ID:" + STRING(intPrinterID).
   
   IF Printer.PrintingMode = "Network" THEN 
      chrActualPrinterName = Printer.OSNetworkPrinterName.
   ELSE 
      chrActualPrinterName = Printer.OSRemotePrinterName.
   
   /* Checks that the Printer is set up correctly on Unix */
   chrReturnValue = fCheckPrinter(chrActualPrinterName).
   
   IF chrReturnValue <> "Ok" THEN
      RETURN chrReturnValue.
   
   /* Print */
   UNIX SILENT VALUE("lp -d " + chrActualPrinterName + " -s " + chrPrintFile).
   
   IF LabelFile.DeleteWhenPrinted THEN
      UNIX SILENT VALUE("rm " + chrPrintFile).
   
   RETURN "Ok".
   
END FUNCTION. /*FUNCTION fPrintFile*/

