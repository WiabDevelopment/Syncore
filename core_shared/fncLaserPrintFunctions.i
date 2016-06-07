/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncLaserPrintFunctions.i
Purpose : Character & web functions for Laser Printing using PostScript
Author  : BG
Date    : 8th Oct 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
19/04/2013 MC  DayMen     add fReplaceTextAligned, fPostScriptReplaceRightAligned to accomodate for right alignment 
02/05/2013 BG  DayMen     Took out UTF-8 convert pieces as we'll now use pure UTF-8. 
                          Now uses the Postscript field on PsFontCharacter to populate new strings so we can cater for non ASCII characters
04/12/2013 BG  DayMen     Added extra logging for bad characters
31/06/2014 MC  All        UnixName changes according to Printer.PrintingMode   
07/10/2014 MC  GHDDE      Cater for Nirs path 
23/10/2014 MC  GHDDE      global change request number is now a char
16/10/2015 ND  GoPro      Added in Centering function for centering text in excel cells.
------------------------------------------------------------------------------------------------------------------------------------------*/

/*************************************** 
 NOTE:  This Include has Dependencies on 
 {defSessionVariables.i}
 {fncDateFunctions.i}
 {fncGlobalFunctions.i}
***************************************/

/* Streams */        
DEFINE STREAM sLaser.
DEFINE STREAM sBadChars.

/* Temp Tables */
DEFINE TEMP-TABLE ttLaserFileLine
   FIELD FileLineNo        AS INTEGER
   FIELD LineString        AS CHARACTER
   FIELD HasTextToReplace  AS LOGICAL
   INDEX FileLineNo        IS UNIQUE PRIMARY FileLineNo
   INDEX HasTextToReplace  HasTextToReplace
   INDEX LineStringWord    AS WORD-INDEX LineString.

DEFINE TEMP-TABLE ttTextToReplace
   FIELD OldText           AS CHARACTER
   FIELD NewText           AS CHARACTER
   FIELD FontName          AS CHARACTER
   FIELD FontSize          AS INTEGER
   FIELD Alignment         AS CHARACTER.

DEFINE VARIABLE chrActualPrinterName AS CHARACTER NO-UNDO.

FUNCTION fEmptyLaserFileTempTables RETURNS LOGICAL:
   
   EMPTY TEMP-TABLE ttLaserFileLine.
   EMPTY TEMP-TABLE ttTextToReplace.
   
   RETURN TRUE.
   
END FUNCTION.


/* Pull the Template for Replacement */
FUNCTION fGetTemplate RETURNS CHARACTER(INPUT chrLaser AS CHARACTER):
   
   DEFINE VARIABLE intFileLineSequence AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrTemplateFileName AS CHARACTER   NO-UNDO.
   
   /* Validate File */
   FIND FIRST LaserFile NO-LOCK 
        WHERE LaserFile.FileName = chrLaser NO-ERROR.
   IF NOT AVAILABLE LaserFile THEN 
   DO:
      RETURN fTL("No LaserFile record AVAILABLE for:[" + chrLaser + "].").
   END.
   
   /* Set the file name acording to LaserFile setting */
   chrTemplateFileName = LaserFile.TemplateFileName.
   
   /* If program is run under a Nirs number, try to find the file under the Nirs path */
   IF chrGblChangeRequestNo <> "0" AND chrGblChangeRequestNo <> "" THEN 
   DO:
      chrTemplateFileName = REPLACE(LaserFile.TemplateFileName, "laser_templates", 
                            "nirs/" + chrGblChangeRequestNo + "/laser_templates").
      /* If the file is not found under the Nirs path, set the file name back to LaserFile setting */
      IF SEARCH(chrTemplateFileName) = ? THEN
         chrTemplateFileName = LaserFile.TemplateFileName.
   END.
   
   IF SEARCH(chrTemplateFileName) = ? THEN
   DO:
      RETURN fTL("Cannot find Laser File:[" + chrTemplateFileName + "].").
   END.
   
   INPUT STREAM sLaser FROM VALUE(chrTemplateFileName).
   
   REPEAT:
      
      CREATE ttLaserFileLine.
      ASSIGN intFileLineSequence        = intFileLineSequence + 1
             ttLaserFileLine.FileLineNo = intFileLineSequence.
      
      IMPORT STREAM sLaser UNFORMATTED ttLaserFileLine.LineString.
   END.
   
   INPUT STREAM sLaser CLOSE.
   
   RETURN "Ok".
   
END FUNCTION. /* FUNCTION fGetTemplate */


FUNCTION fReplaceText RETURNS CHARACTER(INPUT chrOldText  AS CHARACTER,
                                        INPUT chrNewText  AS CHARACTER,
                                        INPUT chrFontName AS CHARACTER,
                                        INPUT intFontSize AS INTEGER):

   chrNewText = REPLACE(chrNewText,"(","<").
   chrNewText = REPLACE(chrNewText,")",">").
   
   CREATE ttTextToReplace.
   ASSIGN ttTextToReplace.OldText    = chrOldText
          ttTextToReplace.NewText    = chrNewText
          ttTextToReplace.FontName   = chrFontName
          ttTextToReplace.FontSize   = intFontSize
          ttTextToReplace.Alignment  = "Left".
   
   /* Tag the lines that require updates here, will save us time later on when substituting */
   FOR EACH ttLaserFileLine 
      WHERE ttLaserFileLine.LineString CONTAINS chrOldText:
      
      ttLaserFileLine.HasTextToReplace = TRUE.
   END. /* FOR EACH ttLaserFileLine */
   
END FUNCTION. /* FUNCTION fReplaceText */


FUNCTION fReplaceTextAligned RETURNS CHARACTER(INPUT chrOldText   AS CHARACTER,
                                               INPUT chrNewText   AS CHARACTER,
                                               INPUT chrFontName  AS CHARACTER,
                                               INPUT intFontSize  AS INTEGER,
                                               INPUT chrAlignment AS CHARACTER):
   CREATE ttTextToReplace.
   ASSIGN ttTextToReplace.OldText    = chrOldText
          ttTextToReplace.NewText    = chrNewText
          ttTextToReplace.FontName   = chrFontName
          ttTextToReplace.FontSize   = intFontSize
          ttTextToReplace.Alignment  = chrAlignment.
   
   /* Tag the lines that require updates here, will save us time later on when substituting */
   FOR EACH ttLaserFileLine 
      WHERE ttLaserFileLine.LineString CONTAINS chrOldText:
      
      ttLaserFileLine.HasTextToReplace = TRUE.
   END. /* FOR EACH ttLaserFileLine */
   
END FUNCTION. /* FUNCTION fReplaceTextAligned */


FUNCTION fCheckPrinter RETURNS CHARACTER(INPUT chrPrinterName AS CHARACTER):
   
   DEFINE VARIABLE chrReturnValue     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrBadEntriesList  AS CHARACTER NO-UNDO INITIAL "not a request id or a destination".
   DEFINE VARIABLE intCounter         AS INTEGER   NO-UNDO.
   
   /* This uses the "lpstat" Unix command to check on teh Status of the printer name. Will only work on Unix. */
   INPUT THROUGH VALUE("lpstat " + chrPrinterName).
      
      REPEAT:
         IMPORT UNFORMATTED chrReturnValue.
         
         DO intCounter = 1 to NUM-ENTRIES(chrBadEntriesList):
            
            /* Look for the bad entry string in the full return value */
            IF INDEX(chrReturnValue, ENTRY(intCounter,chrBadEntriesList)) <> 0 THEN
            DO:
               chrReturnValue = fTL("The Printer:[" + chrPrinterName + "] is not set up in UNIX
                                    . [" + chrReturnValue + "]"
                                    + ". Please log a ticket for the IT Unix Admin team to solve this issue @ [" 
                                    + "<a href='http://itsupport' target='_blank'>http://itsupport</a>]").
               RETURN chrReturnValue.
            END.
            
         END. /* DO intCounter = 1 to NUM-ENTRIES(chrBadEntriesList): */
      END. /* REPEAT: */
      
   INPUT CLOSE.
   
   RETURN "Ok".
   
END FUNCTION. /* fGetCurrentDirectory */


FUNCTION fPostScriptReplace RETURNS CHARACTER (INPUT chrFontName         AS CHARACTER,
                                               INPUT intFontSize         AS INTEGER,
                                               INPUT chrOriginalFileLine AS CHARACTER,
                                               INPUT chrStringToReplace  AS CHARACTER,
                                               INPUT chrStringNewValue   AS CHARACTER,
                                               INPUT chrAlignment        AS CHARACTER):
   
   DEFINE VARIABLE chrNewString            AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrNewPostscriptString  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrOldSpacingString     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrNewSpacingString     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intChar                 AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intOriginalLength       AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intNewStringLength      AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intCenterLength         AS INTEGER     NO-UNDO.
   
   FIND FIRST PsFont NO-LOCK /*idx=FontNameFontSize*/
      WHERE PsFont.FontName = chrFontName
      AND   PsFont.FontSize = intFontSize NO-ERROR.
   IF NOT AVAILABLE PsFont THEN
      RETURN fTL("Error: No PsFont record for FontName:[" + chrFontName + "] and FontSize:" + STRING(intFontSize) + ".").
   
   chrOldSpacingString = ENTRY(1,chrOriginalFileLine,"]").
   chrOldSpacingString = ENTRY(2,chrOldSpacingString,"[").
   chrOldSpacingString = "[" + chrOldSpacingString + "]".
   
   IF chrAlignment = "Right" THEN
   DO:
      /* Calculate the spacing for the chrStringToReplace */
      DO intChar = 1 TO LENGTH(chrStringToReplace):
         
         FIND FIRST PsFontCharacter OF PsFont NO-LOCK /*idx=PsFontIDCharacter*/
            WHERE PsFontCharacter.Character = SUBSTRING(chrStringToReplace, intChar, 1) NO-ERROR.
         
         IF AVAILABLE PsFontCharacter THEN
         DO:
            intOriginalLength = intOriginalLength + PsFontCharacter.Spacing.
         END.
         ELSE 
         DO:
            /* for each missing character (from DB) the length will be skipped, so the replacement text will be mis-aligned - this is */
            /* to prevent errors; it will also give the developer a hint on why the text is mis-aligned or not correct */
            OUTPUT STREAM sBadChars TO VALUE("../logs/BadCharacters.txt") APPEND.
               
               IF NOT AVAILABLE PsFont THEN
                  PUT STREAM sBadChars UNFORMATTED "ERROR: No PsFont record in Scope for FontName: " + chrFontName 
                                                    + " FontSize: " + STRING(intFontSize)  + " " + STRING(TODAY)  + "-" + STRING(TIME) + ".    ".
               ELSE
                  PUT STREAM sBadChars UNFORMATTED "ERROR: No PsFontCharacter record for FontName: " + PsFont.FontName 
                                                    + " FontSize: " + STRING(PsFont.FontSize) 
                                                    + " AND Character: '" + SUBSTRING(chrStringNewValue, intChar, 1)  + "' " + STRING(TODAY)  + "-" + STRING(TIME) + ".    ".
            OUTPUT STREAM sBadChars CLOSE.
         END.
      END. /*DO intChar = 1 TO LENGTH(chrStringToReplace):*/
      
      /* Calculate the spacing for the chrStringNewValue */
      DO intChar = 1 TO LENGTH(chrStringNewValue):
         
         FIND FIRST PsFontCharacter OF PsFont NO-LOCK /*idx=PsFontIDCharacter*/
            WHERE PsFontCharacter.Character = SUBSTRING(chrStringNewValue, intChar, 1) NO-ERROR.
         IF AVAILABLE PsFontCharacter THEN
         DO:
            intNewStringLength = intNewStringLength + PsFontCharacter.Spacing.
         END.
         ELSE
         DO:
            /* replace any character missing from DB with blank */
            chrStringNewValue = REPLACE(chrStringNewValue, SUBSTRING(chrStringNewValue, intChar, 1), "").
            /* Decrease counter as the chrStringNewValue is one character shorter */
            intChar = intChar - 1.
         END.
      END.
   END.
   
   /* Center Align */
   IF chrAlignment = "Center" THEN
   DO:
      
      /* Calculate the spacing for the chrStringToReplace */
      DO intChar = 1 TO LENGTH(chrStringToReplace):
         
         FIND FIRST PsFontCharacter OF PsFont NO-LOCK /*idx=PsFontIDCharacter*/
            WHERE PsFontCharacter.Character = SUBSTRING(chrStringToReplace, intChar, 1) NO-ERROR.
         
         IF AVAILABLE PsFontCharacter THEN
         DO:
            intOriginalLength = intOriginalLength + PsFontCharacter.Spacing.
         END.
         ELSE 
         DO:
            /* for each missing character (from DB) the length will be skipped, so the replacement text will be mis-aligned - this is */
            /* to prevent errors; it will also give the developer a hint on why the text is mis-aligned or not correct */
            OUTPUT STREAM sBadChars TO VALUE("../logs/BadCharacters.txt") APPEND.
               
               IF NOT AVAILABLE PsFont THEN
                  PUT STREAM sBadChars UNFORMATTED "ERROR: No PsFont record in Scope for FontName: " + chrFontName 
                                                    + " FontSize: " + STRING(intFontSize)  + " " + STRING(TODAY)  + "-" + STRING(TIME) + ".    ".
               ELSE
                  PUT STREAM sBadChars UNFORMATTED "ERROR: No PsFontCharacter record for FontName: " + PsFont.FontName 
                                                    + " FontSize: " + STRING(PsFont.FontSize) 
                                                    + " AND Character: '" + SUBSTRING(chrStringNewValue, intChar, 1)  + "' " + STRING(TODAY)  + "-" + STRING(TIME) + ".    ".
            OUTPUT STREAM sBadChars CLOSE.
         END.
      END. /*DO intChar = 1 TO LENGTH(chrStringToReplace):*/
      
      /* Calculate the spacing for the chrStringNewValue */
      DO intChar = 1 TO LENGTH(chrStringNewValue):
         
         FIND FIRST PsFontCharacter OF PsFont NO-LOCK /*idx=PsFontIDCharacter*/
            WHERE PsFontCharacter.Character = SUBSTRING(chrStringNewValue, intChar, 1) NO-ERROR.
         IF AVAILABLE PsFontCharacter THEN
         DO:
            intNewStringLength = intNewStringLength + PsFontCharacter.Spacing.
         END.
         ELSE
         DO:
            /* replace any character missing from DB with blank */
            chrStringNewValue = REPLACE(chrStringNewValue, SUBSTRING(chrStringNewValue, intChar, 1), "").
            /* Decrease counter as the chrStringNewValue is one character shorter */
            intChar = intChar - 1.
         END.
      END.
   END.
   
   chrNewSpacingString = "[".

   DO intChar = 1 TO LENGTH(chrStringNewValue):

      FIND FIRST PsFontCharacter OF PsFont NO-LOCK /*idx=PsFontIDCharacter*/
         WHERE PsFontCharacter.Character = SUBSTRING(chrStringNewValue, intChar, 1) NO-ERROR.
      IF NOT AVAILABLE PsFontCharacter THEN
      DO:
         OUTPUT STREAM sBadChars TO VALUE("../logs/BadCharacters.txt") APPEND.

            IF NOT AVAILABLE PsFont THEN
               PUT STREAM sBadChars UNFORMATTED "Error: No PsFont record in Scope for FontName:" + chrFontName
                                                 + " FontSize:" + STRING(intFontSize)  + " " + STRING(TODAY)  + "-" + STRING(TIME) + ".    ".
            ELSE
               PUT STREAM sBadChars UNFORMATTED "Error: No PsFontCharacter record for FontName:" + PsFont.FontName
                                                 + " FontSize:" + STRING(PsFont.FontSize)
                                                 + " AND Character:'" + SUBSTRING(chrStringNewValue, intChar, 1)  + "' " + STRING(TODAY)  + "-" + STRING(TIME) + ".    ".
         OUTPUT STREAM sBadChars CLOSE.

         /* replace any character missing from DB with blank */
         chrStringNewValue = REPLACE(chrStringNewValue,  SUBSTRING(chrStringNewValue, intChar, 1), "").
         /* Decrease counter as the length of the chrNewSpacingString decreased */
         intChar = intChar - 1.

      END.
      ELSE
      DO:
         IF SUBSTRING(chrStringNewValue, intChar, 1) = " " THEN
            chrNewPostscriptString = chrNewPostscriptString + " ".
         ELSE
         DO:
            IF chrAlignment = "Center" THEN
               chrNewPostscriptString = chrNewPostscriptString + STRING(PsFontCharacter.Postscript).
            ELSE
               chrNewPostscriptString = chrNewPostscriptString + STRING(PsFontCharacter.Postscript).
         END.
         chrNewSpacingString    = chrNewSpacingString    + STRING(PsFontCharacter.Spacing) + " ".
      END.
   END. /*DO intChar = 1 TO LENGTH(chrStringNewValue):*/

   IF chrAlignment = "Right" THEN
   DO:
      /* If new string is shorter than the original string then we need to add a single space character at the beginning and space it out */
      /* so that the test will end up being right-aligned to where the original text ended left-to-right                                  */
      /* e.g. (abc)[12 12 12] could become ( abc)[544 12 12 12} with the extra space at the start of string and extra postscript spacing  */
      IF intOriginalLength - intNewStringLength > 0 THEN 
      DO:
         chrNewPostscriptString = " " + chrNewPostscriptString.
         chrNewSpacingString = REPLACE(chrNewSpacingString, "[", "[" + STRING(intOriginalLength - intNewStringLength) + " ").
      END.
   END.
   
   IF chrAlignment = "Center" THEN
   DO:
      /* If new string is shorter than the original string then we need to add a single space character at the beginning and space it out */
      /* so that the test will end up being right-aligned to where the original text ended left-to-right                                  */
      /* e.g. (abc)[12 12 12] could become ( abc)[544 12 12 12} with the extra space at the start of string and extra postscript spacing  */
      IF intOriginalLength - intNewStringLength > 0 THEN 
      DO:
         chrNewPostscriptString = " " + chrNewPostscriptString.
         
         intCenterLength = (intOriginalLength - intNewStringLength) / 2.
         chrNewSpacingString = REPLACE(chrNewSpacingString, "[", "[" + STRING(intCenterLength) + " ").
      END.
   END.
   
   IF chrAlignment = "Center" THEN
      chrNewSpacingString = chrNewSpacingString + STRING(intCenterLength) + "]".
   ELSE 
       chrNewSpacingString = chrNewSpacingString + " 0]".
   
   chrNewString = REPLACE(chrOriginalFileLine, chrStringToReplace,  chrNewPostscriptString).
   chrNewString = REPLACE(chrNewString,        chrOldSpacingString, chrNewSpacingString).
   
   RETURN chrNewString.
   
END FUNCTION. /* fPostScriptReplace */


FUNCTION fPrintFile RETURNS CHARACTER (INPUT intPrinterID AS INTEGER):
   
   DEFINE VARIABLE chrPrintFile   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrReturnValue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intCopies      AS INTEGER   NO-UNDO.
   
   /* Read through the file and substitute the fields for data */
   FileLineLoop:
   FOR EACH ttLaserFileLine
      WHERE ttLaserFileLine.HasTextToReplace = TRUE:
      
      FieldLoop:
      FOR EACH ttTextToReplace NO-LOCK:
         
         /* Replace the field name with the value if found - should we use CONTAINS here - quicker? */
         IF INDEX(ttLaserFileLine.LineString, "(" + ttTextToReplace.OldText + ")") > 0 THEN
         DO:
            chrReturnValue = fPostScriptReplace(ttTextToReplace.FontName,
                                                ttTextToReplace.FontSize,
                                                ttLaserFileLine.LineString,
                                                ttTextToReplace.OldText,
                                                ttTextToReplace.NewText,
                                                ttTextToReplace.Alignment).
            
            IF chrReturnValue BEGINS "Error" THEN
            DO:
               RETURN chrReturnValue.
            END.
            
            ttLaserFileLine.LineString = chrReturnValue.
            
         END. /*IF INDEX(ttLaserFileLine.LineString, "(" + ttTextToReplace.OldText + ")") > 0 THEN*/
      END. /*FOR EACH ttTextToReplace NO-LOCK:*/
   END. /*FOR EACH ttLaserFileLine*/
   
   /* This function is in fncDateFunctions.i so dependency on that having been included in the Parent program */
   chrPrintFile = "../tmp/" + LaserFile.FilePrefix + fTimeStamp(NOW) + "." + LaserFile.Extension.
   
   /* Print the File */
   OUTPUT STREAM sLaser TO VALUE(chrPrintFile).
      
      FOR EACH ttLaserFileLine NO-LOCK:
         PUT STREAM sLaser UNFORMATTED ttLaserFileLine.LineString SKIP.
      END.
   OUTPUT STREAM sLaser CLOSE.
   
   FIND FIRST Printer NO-LOCK 
      WHERE Printer.PrinterID = intPrinterID NO-ERROR.
   IF NOT AVAILABLE Printer THEN 
      RETURN fTL("No Printer Exists for ID:[" + STRING(intPrinterID) + "].").
   
   IF Printer.PrintingMode = "Network" THEN 
      chrActualPrinterName = Printer.OSNetworkPrinterName.
   ELSE 
      chrActualPrinterName = Printer.OSRemotePrinterName.
   
   /* Checks that the Printer is set up correctly on Unix */
   chrReturnValue = fCheckPrinter(chrActualPrinterName).
   
   IF chrReturnValue <> "Ok" THEN
      RETURN chrReturnValue.
   
   /* Print */
   DO intCopies = 1 to LaserFile.NumCopiesToPrint:
      
      UNIX SILENT VALUE("lp -d " + chrActualPrinterName + " -s " + chrPrintFile).
   END. /*DO intCopies = 1 to LaserFile.NumCopiesToPrint:*/
   
   IF LaserFile.DeleteWhenPrinted THEN
      UNIX SILENT VALUE("rm " + chrPrintFile).
   
   RETURN "Ok".
   
END FUNCTION. /*FUNCTION fPrintFile*/



