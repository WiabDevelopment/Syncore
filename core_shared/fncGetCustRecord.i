  DEFINE VARIABLE logRecordFound    AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE hdlCustomerRecord AS HANDLE      NO-UNDO.
  
  DEFINE INPUT PARAMETER intUniqueID  AS INTEGER.
  DEFINE INPUT PARAMETER chrTableName AS CHARACTER.
  
  /* New section to deal with Virtual Fields - Updated from below to use real Db Tables */
  FIND cust._file NO-LOCK WHERE cust._file._file-Name = "Cust" + chrTableName NO-ERROR.
  IF AVAILABLE _file THEN
  DO:
    CREATE BUFFER hdlCustomerRecord FOR TABLE "Cust" + chrTableName.
    
    logRecordFound = hdlCustomerRecord:FIND-FIRST("WHERE Cust" + chrTableName + "ID = " + STRING(intUniqueID), SHARE-LOCK) NO-ERROR.
    hdlCustomerRecord:BUFFER-RELEASE() NO-ERROR.
    DELETE OBJECT hdlCustomerRecord NO-ERROR.
    
  END. /*IF AVAILABLE _File THEN*/
  
  
