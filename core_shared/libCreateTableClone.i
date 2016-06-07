/*------------------------------------------------------------------------------------------------------------------------------------------
Program : libCreateTableClone.i
Purpose : Creates either a dynamic TempTable for the Table (gun & batch) or a real buffer for Table in temp Db (web) and it then creates 
          a static ttTeableClone record to hold a link between the buffer handle for each one and the actual table name.

Author  : BG
Date    : 28th Feb 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
08/01/2016 CS  NextelBr   Added unique UserTransactionID to allow record tracking from each unique libDataManager call within the same 
                          session.
------------------------------------------------------------------------------------------------------------------------------------------*/
   
   /* Get a temp Db record and create either a dynamic TT for the table (gun & batch) or create real buffer for each one (web) */
   /* Then create a static TT record to hold a link between the buffer handle for each one and the actual table name           */
   
   FIND FIRST temp._file NO-LOCK 
      WHERE temp._file._file-name = "ssn" + chrTableName NO-ERROR.
   IF AVAIL temp._file THEN
   DO:
      /* Create an empty static Temp Table Record */
      CREATE ttTableClone.
      
      /* Only define these as Temp Tables if its not a Web session  */
      IF SESSION:CLIENT-TYPE = "WEBSPEED" THEN
      DO:
         /* Create a buffer for the real tt Table in the temp Db and assign the handle to it into the ttTableClone */
         CREATE BUFFER hdlCloneTableBuffer FOR TABLE temp._file._file-name.
         ttTableClone.DbTableName  = SUBSTRING(temp._file._file-name,4). /* Strip the ssn prefix off*/
         ttTableClone.UserTransactionID = INTEGER(THIS-PROCEDURE:Private-Data).
         ttTableClone.BufferHandle = hdlCloneTableBuffer.
         /* ttTableClone.TempTableHandle doesn't get set for Web so VALID-HANDLE will always return FALSE */
      END.
      ELSE
      DO: /* Gun or Batch Session */
         /* Define a dynamic tt duplicating real table schema and assign the handle of the dynamic TT into the static TT record */
         CREATE TEMP-TABLE hdlCloneTableHandle.
         hdlCloneTableHandle:UNDO     = TRUE.
         ttTableClone.TempTableHandle = hdlCloneTableHandle.
         ttTableClone.DbTableName     = SUBSTRING(temp._file._file-name,4). /* Strip the ssn prefix off*/
         ttTableClone.TempTableHandle:CREATE-LIKE(temp._file._file-name).
         ttTableClone.TempTableHandle:TEMP-TABLE-PREPARE(temp._file._file-name).
         ttTableClone.UserTransactionID = INTEGER(THIS-PROCEDURE:Private-Data).
         ttTableClone.BufferHandle    = ttTableClone.TempTableHandle:DEFAULT-BUFFER-HANDLE.      
      END. /* Gun or Batch Session */
   END. /*IF AVAIL temp._file THEN*/
   ELSE
   DO:
      MESSAGE "No Table:" + "ssn" + chrTableName + " exists in the Temp Db".
   END.
   
   
   
