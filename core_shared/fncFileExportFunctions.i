/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncFileExportFunctions.i
Purpose : Functions for structured flat File Exports using delimiters
Author  : BG
Date    : 19th Feb 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03/03/2015 BG  Canon      Remove carraige Return as per Canon Request - Needs to be tidied up.
13/04/2015 BG  GoPro      Reversed the sequence of adding Carriage Return and Line Feed entries. 
------------------------------------------------------------------------------------------------------------------------------------------*/

/*************************************
 NOTE:  This Include has Dependencies on 

 {defSessionVariables.i}
 {fncDateFunctions.i}
 {fncGlobalFunctions.i}
**************************************/

DEFINE VARIABLE intFileLineSequence      AS INTEGER.
DEFINE VARIABLE intLineNoSequence        AS INTEGER.
DEFINE VARIABLE intFileLineEntrySequence AS INTEGER.
DEFINE VARIABLE intEntryNoSequence       AS INTEGER.

/* Streams */        
DEFINE STREAM sToFile.

/* Temp Tables */
DEFINE TEMP-TABLE ttExportFileLine
   FIELD FileLineID        AS INTEGER
   FIELD LineNo            AS INTEGER
   FIELD LineString        AS CHARACTER
   FIELD NumLineEntries    AS INTEGER
   INDEX FileLineID        IS UNIQUE PRIMARY 
      FileLineID           ASCENDING
   INDEX LineNoAsc         IS UNIQUE
      LineNo               ASCENDING
   INDEX LineNoDesc        IS UNIQUE
      LineNo               DESCENDING
   INDEX LineStringWord    AS WORD-INDEX 
      LineString           ASCENDING.

DEFINE TEMP-TABLE ttExportFileLineEntry
   FIELD FileLineID        AS INTEGER
   FIELD FileLineEntryID   AS INTEGER
   FIELD EntryNo           AS INTEGER
   FIELD EntryString       AS CHARACTER
   FIELD DelimiterChar     AS CHARACTER
   INDEX FileLineEntryID   IS UNIQUE PRIMARY 
      FileLineEntryID      ASCENDING
   INDEX FileLineIDEntryNo        
      FileLineID           ASCENDING
      EntryNo              ASCENDING
   INDEX EntryStringWord   AS WORD-INDEX 
      EntryString          ASCENDING.


FUNCTION fEmptyExportFileTempTables RETURNS LOGICAL:
   
   EMPTY TEMP-TABLE ttExportFileLine.
   EMPTY TEMP-TABLE ttExportFileLineEntry.
   
   RETURN TRUE.
   
END FUNCTION.


FUNCTION fCreateFileLine RETURNS INTEGER():
   
   DEFINE VARIABLE intHighestLineNo AS INTEGER.
   
   FileLineLoop:
   FOR EACH ttExportFileLine
      BY ttExportFileLine.LineNo DESC:
      
      LEAVE FileLineLoop.
   END.
   IF AVAILABLE ttExportFileLine THEN
      intHighestLineNo = ttExportFileLine.LineNo.
   
   CREATE ttExportFileLine.
   ASSIGN intFileLineSequence         = intFileLineSequence + 1
          ttExportFileLine.FileLineID = intFileLineSequence
          ttExportFileLine.LineNo     = intHighestLineNo + 1.
    
   RETURN ttExportFileLine.FileLineID.
   
END FUNCTION. /* FUNCTION fCreateFileLineEntry */


FUNCTION fCreateFileLineEntry RETURNS CHARACTER(INPUT intFileLineID  AS INTEGER,
                                                INPUT intEntryNo     AS INTEGER,
                                                INPUT chrTextString  AS CHARACTER):
   
   DEFINE VARIABLE intHighestEntryNo AS INTEGER.
   
   FIND FIRST ttExportFileLine EXCLUSIVE-LOCK
      WHERE ttExportFileLine.FileLineID = intFileLineID NO-ERROR.
   IF NOT AVAILABLE ttExportFileLine THEN
   DO:
      RETURN "No ttExportFileLine exists with FileLineID " + STRING(intFileLineID).
   END.
   
   IF intEntryNo = 0 THEN
   DO:
      FileLineLoop:
      FOR EACH ttExportFileLineEntry 
         WHERE ttExportFileLineEntry.FileLineID = intFileLineID
         BY    ttExportFileLineEntry.EntryNo DESC:
         
         LEAVE FileLineLoop.
      END.
      IF AVAILABLE ttExportFileLineEntry THEN
         intHighestEntryNo = ttExportFileLineEntry.EntryNo.
   END.
   
   CREATE ttExportFileLineEntry.
   ASSIGN ttExportFileLineEntry.FileLineID      = intFileLineID
          intFileLineEntrySequence              = intFileLineEntrySequence + 1
          ttExportFileLineEntry.FileLineEntryID = intFileLineEntrySequence
          ttExportFileLineEntry.EntryNo         = (IF intEntryNo = 0 THEN (intHighestEntryNo + 1) ELSE intEntryNo)
          ttExportFileLineEntry.EntryString     = chrTextString.
   
   /* Get Total Entries on every addition for Triming the Delimiter later */                 
   ASSIGN ttExportFileLine.NumLineEntries = ttExportFileLineEntry.EntryNo.       
   
   RETURN "Ok".
   
END FUNCTION. /* FUNCTION fCreateFileLineEntry */



FUNCTION fOutputFile RETURNS CHARACTER (INPUT intFileMasterID     AS INTEGER,
                                        INPUT chrFileName         AS CHARACTER):
   
   DEFINE VARIABLE chrOutputFile   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrDelimiter    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrReturnValue  AS CHARACTER NO-UNDO.   
   
   DEFINE BUFFER readFileDelimiter FOR FileDelimiter.
   DEFINE BUFFER readFileMaster    FOR FileMaster.
   DEFINE BUFFER readFileType      FOR FileType.
   
   FIND FIRST readFileMaster NO-LOCK
      WHERE readFileMaster.FileMasterID = intFileMasterID NO-ERROR.
   IF NOT AVAILABLE readFileMaster THEN
   DO:
      RETURN "No readFileMaster exists with FileMasterID " + STRING(FileMasterID).
   END.
   
   FIND FIRST readFileType      OF    readFileMaster NO-LOCK.
   IF NOT AVAILABLE readFileType THEN
   DO:
      RETURN "No readFileType exists with FileMasterID " + STRING(FileMasterID).
   END.
   
   FIND FIRST readFileDelimiter OF    readFileMaster NO-LOCK. 
   IF NOT AVAILABLE readFileDelimiter THEN
   DO:
      RETURN "No readFileDelimiter exists with FileMasterID " + STRING(FileMasterID).
   END.
   
   /* This function is in fncDateFunctions.i so dependency on that having been included in the Parent program */
   chrOutputFile = readFileMaster.FilePath + "tmp/" + chrFileName.
   
   /* Set the Delimiter use ASCII if set */
   IF readFileDelimiter.DelimiterASCIIValue <> 0 THEN
      chrDelimiter = CHR(readFileDelimiter.DelimiterASCIIValue).
   ELSE   
      chrDelimiter = readFileDelimiter.DelimiterValue.
  
   /* Print the File */
   OUTPUT STREAM sToFile TO VALUE(chrOutputFile).
      
      /* Read through the file and substitute the fields for data */
      FileLineLoop:
      FOR EACH ttExportFileLine
         BY ttExportFileLine.LineNo:
         
         FieldLoop:
         FOR EACH ttExportFileLineEntry OF ttExportFileLine
            BY ttExportFileLineEntry.EntryNo:
            
            /* TRIM doesnt work here as it trims more than the last delimiter if blank values are recorded at the end of the line */            
            ttExportFileLine.LineString = ttExportFileLine.LineString + ttExportFileLineEntry.EntryString 
                                             + (IF ttExportFileLineEntry.EntryNo = ttExportFileLine.NumLineEntries THEN
                                                   ""
                                                ELSE
                                                   chrDelimiter).
            
         END. /*FOR EACH ttExportFileLineEntry NO-LOCK:*/
         
         /* Add Carriage Return and/or Line Feed here */
         IF readFileMaster.AddCarriageReturnAfterEachLine THEN
            ttExportFileLine.LineString = ttExportFileLine.LineString + chr(13).
         
         IF readFileMaster.AddLineFeedAfterEachLine THEN
            ttExportFileLine.LineString = ttExportFileLine.LineString + chr(10).
         
         PUT STREAM sToFile UNFORMATTED ttExportFileLine.LineString.
         
      END. /*FOR EACH ttExportFileLine*/
      
   OUTPUT STREAM sToFile CLOSE.

   RETURN "Ok".
   
END FUNCTION. /*FUNCTION fOutputFile*/



