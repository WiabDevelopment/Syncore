/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filCsvReader.p
Purpose : Called from filImportFiles.p in the event that a file has arrived in csv format. Calls a child program to handle the actual 
          data manipulation. 
Author  : Christopher Shelley
Date    : 2nd April 2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER chrUploadProgram   AS CHARACTER     NO-UNDO.

/* These vars will hold the File information through the program chain */
DEFINE SHARED VARIABLE intGblFileMasterID   AS INTEGER      NO-UNDO.
DEFINE SHARED VARIABLE intGblFileTypeID     AS INTEGER      NO-UNDO.
DEFINE SHARED VARIABLE chrGblFileName       AS CHARACTER    NO-UNDO FORMAT "x(20)".
DEFINE SHARED VARIABLE chrGblFilePath       AS CHARACTER    NO-UNDO FORMAT "x(20)".
DEFINE SHARED VARIABLE intGblNewFileID      AS INTEGER      NO-UNDO.

DEFINE VARIABLE hdlHandler                  AS HANDLE       NO-UNDO.
DEFINE VARIABLE chrReturnError              AS CHARACTER    NO-UNDO.

/* Don't want to lose the record in the calling procedure so keep this scoped locally */
DEFINE BUFFER FileMaster FOR FileMaster.

{defSessionVariables.i}

ReadFileBlk:
DO TRANSACTION ON ERROR UNDO, RETURN ERROR:  
   
   IF SEARCH(chrGblFilePath) = ? THEN 
   DO:
      chrReturnError = "Cannot find File:" + chrGblFilePath.
      RETURN ERROR chrReturnError.
   END.
   
   /* Run the persistent procedure that contains the callbacks */
   RUN VALUE(chrUploadProgram) PERSISTENT SET hdlHandler NO-ERROR.
   
   /* Must check the error immediately after NO-ERROR condition */
   IF ERROR-STATUS:ERROR THEN
   DO:
      chrReturnError = "Error running Prog:'" + chrUploadProgram + "'. " + ERROR-STATUS:GET-MESSAGE(1).
      RETURN ERROR chrReturnError.
   END.

   /* Must check the error immediately after NO-ERROR condition */
   chrReturnError = RETURN-VALUE.
   
   /* Clean up */
   DELETE PROCEDURE hdlHandler NO-ERROR. 

   IF chrReturnError = "Pending" THEN 
   DO:
      /* Don't want an Error returned in these cases as they're valid returns */
      RETURN chrReturnError.
   END. /* IF chrReturnError = "Pending" OR chrReturnError = "PreventRejected" */
   
   IF chrReturnError <> "" OR ERROR-STATUS:ERROR THEN 
   DO:
      chrReturnError = "Error:" + chrReturnError + " " + ERROR-STATUS:GET-MESSAGE(1). 
      RETURN chrReturnError.
   END. /* IF chrReturnError <> "" OR ERROR-STATUS:ERROR */
   
END. /*ReadFileBlk:*/



