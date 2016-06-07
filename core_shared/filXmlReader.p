/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filXmlFileReader.p
Purpose : Called from filImportFiles.p in the event that a file has arrived in xml format. Handles the xml parsing and then calls a 
          child program to handle the actual data manipulation. e.g. xmlAsnUpload.p.
Author  : BG
Date    : 29th May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
07/02/2014 CS  CR1043     Added new return condition for OrderUpload.
12/03/2014 CS  CR1021     Removed debugging to make compatiable with Cron.
19/03/2015 CS  Canon      Added ability to bypass SAX Parser.
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER chrUploadProgram   AS CHARACTER     NO-UNDO.

/* Standard Mandatory Includes */
{defSessionVariables.i}

{fncClassFunctions.i}
/*fGetSessionValue()*/

/* These vars will hold the File information through the program chain */
DEFINE SHARED VARIABLE intGblFileMasterID   AS INTEGER      NO-UNDO.
DEFINE SHARED VARIABLE intGblFileTypeID     AS INTEGER      NO-UNDO.
DEFINE SHARED VARIABLE chrGblFileName       AS CHARACTER    NO-UNDO FORMAT "x(20)".
DEFINE SHARED VARIABLE chrGblFilePath       AS CHARACTER    NO-UNDO FORMAT "x(20)".
DEFINE SHARED VARIABLE intGblNewFileID      AS INTEGER      NO-UNDO.

/* Local Variables */
DEFINE VARIABLE hdlSaxParser                AS HANDLE       NO-UNDO.
DEFINE VARIABLE hdlHandler                  AS HANDLE       NO-UNDO.
DEFINE VARIABLE chrReturnError              AS CHARACTER    NO-UNDO.
DEFINE VARIABLE logBypassSAXParser          AS LOGICAL      NO-UNDO.

/* Session Variables */
DEFINE VARIABLE chrSsnBypassSAXParser       AS sessionValue NO-UNDO.

/* Don't want to lose the record in the calling procedure so keep this scoped locally */
DEFINE BUFFER FileMaster FOR FileMaster.
DEFINE BUFFER readFileMaster FOR FileMaster.

ASSIGN chrSsnBypassSAXParser = fGetSessionValue("BypassSAXParser")
       logBypassSAXParser    = LOGICAL(chrSsnBypassSAXParser:chrValue) NO-ERROR.

ReadFileBlk:
DO TRANSACTION ON ERROR UNDO, RETURN ERROR:  
   
   IF SEARCH(chrGblFilePath) = ? THEN 
   DO:
      chrReturnError = "Cannot find File:" + chrGblFilePath.
      RETURN ERROR chrReturnError.
   END. /*IF SEARCH(chrGblFilePath) = ? THEN*/
   
   IF logBypassSAXParser = TRUE THEN
   DO:
      FIND FIRST readFileMaster NO-LOCK /* idx=FileMasterID */
         WHERE readFileMaster.FileMasterID = intGblFileMasterID NO-ERROR.
      IF NOT AVAILABLE readFileMaster THEN
      DO:
         chrReturnError = "Error running Prog:'" + chrUploadProgram + "'. FileMaster not found for ID: " + STRING(intGblFileMasterID).
         RETURN ERROR chrReturnError.  
      END. /*IF NOT AVAILABLE FileMaster THEN*/
      
      RUN VALUE(readFileMaster.ProgramName) NO-ERROR.
      
      /* Must check the error immediately after NO-ERROR condition */
      chrReturnError = RETURN-VALUE.
   END. /*IF logBypassSAXParser = TRUE THEN*/
   ELSE 
   DO:
      /* Create the SAX-READER object */
      CREATE SAX-READER hdlSaxParser.               
       
      /* Run the persistent procedure that contains the callbacks */
      RUN VALUE(chrUploadProgram) PERSISTENT SET hdlHandler NO-ERROR.
       
      /* Must check the error immediately after NO-ERROR condition */
      IF ERROR-STATUS:ERROR THEN
      DO:
         chrReturnError = "Error running Prog:'" + chrUploadProgram + "'. " + ERROR-STATUS:GET-MESSAGE(1).
         RETURN ERROR chrReturnError.
      END. /*IF ERROR-STATUS:ERROR THEN*/
   
      /* Give the SAX-READER the handle to the persistent procedure */
      hdlSaxParser:HANDLER = hdlHandler.
   
      /* Give the SAX-READER the info on the file to parse.*/
      hdlSaxParser:SET-INPUT-SOURCE("FILE", chrGblFilePath).
      /* This Parses the file, runs the internal procs and creates and validates records */
      hdlSaxParser:SAX-PARSE() NO-ERROR.               
   
      /* Must check the error immediately after NO-ERROR condition */
      chrReturnError = RETURN-VALUE.
   
      /* Clean Up */
      DELETE OBJECT hdlSaxParser NO-ERROR.
      DELETE PROCEDURE hdlHandler NO-ERROR. 
   END. /*IF logBypassSAXParser = FALSE THEN*/
   
   IF chrReturnError = "Pending" 
      OR chrReturnError = "PreventRejected" THEN 
   DO:
      /* Don't want an Error returned in these cases as they're valid returns */
      RETURN chrReturnError.
   END. /*IF chrReturnError = "Pending" OR chrReturnError = "PreventRejected" THEN*/
   
   IF chrReturnError <> "" OR ERROR-STATUS:ERROR THEN 
   DO:
      chrReturnError = "Error:" + chrReturnError + " " + ERROR-STATUS:GET-MESSAGE(1). 
      
      RETURN chrReturnError.
   END. /*IF chrReturnError <> "" OR ERROR-STATUS:ERROR THEN*/
END. /*ReadFileBlk:*/

DELETE OBJECT chrSsnBypassSAXParser NO-ERROR.
