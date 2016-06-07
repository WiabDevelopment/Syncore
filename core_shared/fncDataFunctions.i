/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncDataFunctions.i
Purpose : Functions to do with finding Data
Author  : BG
Date    : 23rd Feb 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Returns the String Value of a specific field from the Cust Db */
FUNCTION fGetCustFieldValue RETURNS CHARACTER (INPUT chrFieldName AS CHARACTER,
                                               INPUT chrTableName AS CHARACTER,
                                               INPUT intUniqueID  AS INTEGER):
   
   DEFINE VARIABLE logRecordFound    AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE hdlCustomerRecord AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlCustomerField  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE chrReturnValue    AS CHARACTER   NO-UNDO.
   
   /* See if there is a Customer Specific Table & Fields - if so find and return record */
   FIND cust._file NO-LOCK WHERE cust._file._file-Name = "Cust" + chrTableName NO-ERROR.
   
   IF NOT AVAILABLE cust._file THEN
     RETURN "".
   
   CREATE BUFFER hdlCustomerRecord FOR TABLE "Cust" + chrTableName.
   
   logRecordFound = hdlCustomerRecord:FIND-FIRST("WHERE Cust" + chrTableName + "ID = " + STRING(intUniqueID),
                                                 NO-LOCK) NO-ERROR.
   IF logRecordFound THEN
   DO:
      hdlCustomerField = hdlCustomerRecord:BUFFER-FIELD(chrFieldName) NO-ERROR.
      IF VALID-HANDLE(hdlCustomerField) THEN
         chrReturnValue = hdlCustomerField:STRING-VALUE NO-ERROR.
   END.
   
   hdlCustomerRecord:BUFFER-RELEASE() NO-ERROR.
   DELETE OBJECT hdlCustomerRecord NO-ERROR.
   
   RETURN chrReturnValue.
   
END FUNCTION. /*fGetCustFieldValue*/


/* Returns the String Value of a specific field from the Db */
FUNCTION fGetFieldValue RETURNS CHARACTER (INPUT chrFieldName AS CHARACTER,
                                           INPUT chrTableName AS CHARACTER,
                                           INPUT intUniqueID  AS INTEGER):

   DEFINE VARIABLE hdlRecordBuffer AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlRecordField  AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound  AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrReturnValue  AS CHARACTER   NO-UNDO.

   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName NO-ERROR.
   logRecordFound = hdlRecordBuffer:FIND-FIRST(" WHERE " + chrTableName + "." + chrTableName + "ID  = " + STRING(intUniqueID), 
                                               NO-LOCK) NO-ERROR.

   IF logRecordFound THEN 
   DO:      
      hdlRecordField = hdlRecordBuffer:BUFFER-FIELD(chrFieldName) NO-ERROR.
      IF VALID-HANDLE(hdlRecordField) THEN
         chrReturnValue = hdlRecordField:STRING-VALUE NO-ERROR.
   END.

   hdlRecordBuffer:BUFFER-RELEASE() NO-ERROR.
   DELETE OBJECT hdlRecordBuffer    NO-ERROR.

   RETURN chrReturnValue.

END FUNCTION.  /* fGetFieldValue */


/* Function to check a Record Exists by ID */
FUNCTION fCheckAvailable RETURNS LOGICAL (INPUT chrTableName AS CHAR,
                                          INPUT intUniqueID  AS INT):

   DEFINE VARIABLE hdlRecordBuffer AS HANDLE      NO-UNDO.   
   DEFINE VARIABLE logRecordFound  AS LOGICAL     NO-UNDO.   

   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName NO-ERROR.
   logRecordFound = hdlRecordBuffer:FIND-FIRST(" WHERE " + chrTableName + "." + chrTableName + "ID  = " + STRING(intUniqueID), 
                                               NO-LOCK) NO-ERROR.

   RUN DisplayMessage("Record Not Found",
                      chrTableName + " ID " + STRING(intUniqueID) +  " does not Exist.").

   hdlRecordBuffer:BUFFER-RELEASE() NO-ERROR.
   DELETE OBJECT hdlRecordBuffer    NO-ERROR.

   IF logRecordFound THEN 
   DO:      
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.
