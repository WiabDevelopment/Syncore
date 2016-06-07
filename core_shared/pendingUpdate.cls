/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pendingUpdates.cls
Purpose : Pending Update Class which is used in conjuntion with the Data Library to Extract all pending updates for a Table.
Author  : DCummins
Date    : 07/01/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
08/01/2016 CS  NextelBr   Added unique UserTransactionID to allow record tracking from each unique libDataManager call within the same 
                          session.
11/01/2016 CS  NextelBr   Fixed issue with bad reference.                           
------------------------------------------------------------------------------------------------------------------------------------------*/

CLASS pendingUpdate:
   
   /***** Vars *****/
   DEFINE PROTECTED VARIABLE hdlUpdateRecord      AS HANDLE    NO-UNDO.
   DEFINE PROTECTED VARIABLE hdlDbTable           AS HANDLE    NO-UNDO.
   DEFINE PROTECTED VARIABLE hdlDbRecord          AS HANDLE    NO-UNDO.
   DEFINE PROTECTED VARIABLE chrErrors            AS CHARACTER NO-UNDO.
   DEFINE PROTECTED VARIABLE intError             AS INTEGER   NO-UNDO.
   DEFINE PROTECTED VARIABLE chrErrorString       AS CHARACTER NO-UNDO.
   DEFINE PROTECTED VARIABLE conLockTypeConst     AS INTEGER   NO-UNDO.
   DEFINE PROTECTED VARIABLE hdlQuery             AS HANDLE    NO-UNDO.
   DEFINE PROTECTED VARIABLE intRecordCount       AS INTEGER   NO-UNDO.
   
   
   /***** Properties *****/
   DEFINE PUBLIC PROPERTY CurrentUniqueID  AS INTEGER   NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY CurrentTableName AS CHARACTER NO-UNDO
       GET.
       PROTECTED SET.
   
   DEFINE PUBLIC PROPERTY CurrentSessionID AS CHARACTER NO-UNDO
       GET.
       PROTECTED SET.

   DEFINE PUBLIC PROPERTY CurrentUserTransactionID AS INTEGER NO-UNDO
       GET.
       PROTECTED SET.

   DEFINE PUBLIC PROPERTY NumRecords       AS INTEGER   NO-UNDO
       GET.
       PROTECTED SET.

   DEFINE PRIVATE PROPERTY UniqueFieldName AS CHARACTER NO-UNDO
       GET.
       SET.
      
   /***** Contructor *****/
   CONSTRUCTOR pendingUpdate(INPUT hdlUpdateTableBuffer AS HANDLE,
                             INPUT chrUserSessionID     AS CHARACTER,
                             INPUT intUserTransactionID AS INTEGER):
      
      /* Session and Table get set here and can't be changed for this Class instance */
      ASSIGN hdlUpdateRecord  = hdlUpdateTableBuffer
             CurrentSessionID = chrUserSessionID
             CurrentTableName = hdlUpdateRecord:NAME
             CurrentUserTransactionID = intUserTransactionID
             UniqueFieldName  = SUBSTRING(CurrentTableName,4) + "ID".
            
      NumRecords = getRecordCount().

   END CONSTRUCTOR.
        

   /***** Public External Methods *****/
   METHOD PRIVATE INTEGER getRecordCount():
 
      /* Reset Counter */
      NumRecords = 0.

      CREATE QUERY hdlQuery.

      hdlQuery:SET-BUFFERS(hdlUpdateRecord).
      hdlQuery:QUERY-PREPARE("FOR EACH " + CurrentTableName + " NO-LOCK "
                                + "WHERE " + CurrentTableName + ".SessionID = '" + CurrentSessionID + "' "
                                + "AND Committed = '' "
                                + "AND UserTransactionID = " + STRING(CurrentUserTransactionID)).

      hdlQuery:QUERY-OPEN().
      hdlQuery:GET-FIRST().

      Loop_Block:
      REPEAT:
         IF hdlQuery:QUERY-OFF-END THEN LEAVE Loop_Block.

         /* Increment Record Counter */
         intRecordCount = intRecordCount + 1.

         hdlQuery:GET-NEXT().
      END.

      RETURN intRecordCount.

   END METHOD.


   METHOD PUBLIC INTEGER getFirstID():
                  
      /* Reset the Query and Return 0 id nothing exists */
      hdlQuery:GET-FIRST().

      IF hdlQuery:QUERY-OFF-END THEN
         RETURN 0.
      ELSE
         RETURN hdlUpdateRecord:BUFFER-FIELD(UniqueFieldName):BUFFER-VALUE.  
      
   END METHOD. /*getFirstID()*/

   
   METHOD PUBLIC INTEGER getNextID():
                  
      /* Get the Next record in the Query and Return 0 when at end */
      hdlQuery:GET-NEXT().

      IF hdlQuery:QUERY-OFF-END THEN
         RETURN 0.
      ELSE
         RETURN hdlUpdateRecord:BUFFER-FIELD(UniqueFieldName):BUFFER-VALUE.  
      
   END METHOD. /*getNextID()*/

   METHOD PUBLIC VOID setField(INPUT chrFieldName AS CHARACTER,
                               INPUT intNewValue  AS INTEGER):
      
      /* If we get the Update record ok then the Db Record should be in scope also */
      hdlUpdateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE = intNewValue.
      
   END METHOD. /*getField():*/

   METHOD PUBLIC CHARACTER getField(INPUT chrFieldName AS CHARACTER):
      
      /* If we get the Update record ok then the Db Record should be in scope also */
      RETURN hdlUpdateRecord:BUFFER-FIELD(chrFieldName):BUFFER-VALUE.
      
   END METHOD. /*getField():*/
   
   
   METHOD PUBLIC CHARACTER getErrors():
      
      IF chrErrors <> "" THEN
        RETURN TRIM(chrErrors).
      
      RETURN "".
      
   END METHOD. /*getErrors():*/
   
   
END CLASS. /*pendingUpdates*/
