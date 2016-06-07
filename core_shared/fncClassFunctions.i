/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncClassFunctions.i
Purpose : These functions are called by the web and character programs to create and update records. Each fFunctionName runs a corresponding
          pFunctionName internal procedure in libDataManager.p. The procedure uses the ttTableClone table to find the correct ssnTableName 
          table that we need to create records for and passes back a handle to the new table. This handle allows the classes to add records 
          for any updates etc that they want to perform.
Author  : DC
Date    : 17th Jan 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
12/03/2013 BG  DayMen     Added fGetCreatedRecord function so we can find a previously uncommitted Create in a different program to that in 
                          which it was created.
------------------------------------------------------------------------------------------------------------------------------------------*/
/* Has a dependency on {defSessionVariables.i}*/

FUNCTION fCreateRecord RETURNS CLASS newRecord(INPUT chrTableName AS CHARACTER):
   
   DEFINE VARIABLE newRecord AS newRecord.
   
   RUN pCreateRecord IN hdlGblLibrary (INPUT  chrTableName,
                                       OUTPUT newRecord).
   
   RETURN newRecord.
   
END FUNCTION. /*fCreateRecord*/


FUNCTION fGetCreatedRecord RETURNS CLASS newRecord(INPUT chrTableName AS CHARACTER,
                                                   INPUT intUniqueID  AS INTEGER):
   
   DEFINE VARIABLE newRecord AS newRecord.
   
   RUN pGetCreatedRecord IN hdlGblLibrary (INPUT  chrTableName,
                                           INPUT  intUniqueID,
                                           OUTPUT newRecord).
   
   RETURN newRecord.
   
END FUNCTION. /*fGetCreatedRecord*/


FUNCTION fCreateCustRecord RETURNS CLASS newRecord(INPUT chrTableName AS CHARACTER,
                                                   INPUT intUniqueID  AS INTEGER):
   
   DEFINE VARIABLE newRecord AS newRecord.
   
   RUN pCreateCustRecord IN hdlGblLibrary (INPUT  chrTableName,
                                           INPUT  intUniqueID,
                                           OUTPUT newRecord).
   
   RETURN newRecord.
   
END FUNCTION. /*fCreateRecord*/


FUNCTION fGetRecord RETURNS CLASS updRecord (INPUT chrTableName AS CHARACTER,
                                             INPUT intUniqueID  AS INTEGER):
   
   DEFINE VARIABLE updRecord AS updRecord.
   
   RUN pUpdateRecord IN hdlGblLibrary (INPUT  chrTableName,
                                       INPUT  intUniqueID,
                                       OUTPUT updRecord).
   
   RETURN updRecord.
   
END FUNCTION. /*fGetRecord*/


FUNCTION fGetVersion RETURNS CLASS updRecord(INPUT chrTableName AS CHARACTER,
                                             INPUT intUniqueID  AS INTEGER):
   
   DEFINE VARIABLE updRecord AS updRecord.

   updRecord = fGetRecord(INPUT chrTableName,
                          INPUT intUniqueID).   
      
   RETURN updRecord.
   
END FUNCTION. /*fGetVersion*/


FUNCTION fGetUpdates RETURNS CLASS pendingUpdate(INPUT chrTableName AS CHARACTER):
   
   DEFINE VARIABLE pendingUpdate AS pendingUpdate.
   
   RUN ppendingUpdate IN hdlGblLibrary (INPUT  chrTableName,                                         
                                         OUTPUT pendingUpdate).
   
   RETURN pendingUpdate.
   
END FUNCTION. /*fGetRecord*/


FUNCTION fDeleteRecord RETURNS CLASS delRecord(INPUT chrTableName AS CHARACTER,
                                               INPUT intUniqueID  AS INTEGER):  
  
   DEFINE VARIABLE delRecord AS delRecord.
   
   RUN pDeleteRecord IN hdlGblLibrary (INPUT  chrTableName,
                                       INPUT  intUniqueID,
                                       OUTPUT delRecord). 
   
   RETURN delRecord.
   
END FUNCTION. /*fDeleteRecord*/


FUNCTION fGetSessionRecord RETURNS CLASS sessionRecord(INPUT chrTableName AS CHARACTER,
                                                       INPUT recRecordID  AS RECID):                                                
   
   DEFINE VARIABLE sessionRecord AS sessionRecord.
     
   RUN pGetSessionRecord IN hdlGblLibrary(INPUT  chrTableName,
                                          INPUT  recRecordID,
                                          OUTPUT sessionRecord).
   
   RETURN sessionRecord.
   
END FUNCTION. /*fGetSessionValue*/


FUNCTION fNewSessionRecord RETURNS CLASS sessionRecord(INPUT chrTableName AS CHARACTER,
                                                       INPUT recRecordID  AS RECID):                                                
   
   DEFINE VARIABLE sessionRecord AS sessionRecord.
     
   RUN pSetSessionRecord IN hdlGblLibrary(INPUT  chrTableName,
                                          INPUT  recRecordID,
                                          OUTPUT sessionRecord).
   
   RETURN sessionRecord.
   
END FUNCTION. /*fGetSessionValue*/


FUNCTION fGetSessionValue RETURNS CLASS sessionValue(INPUT chrsessionValueName AS CHARACTER):                                                
   
   DEFINE VARIABLE sessionValue AS sessionValue.
     
   RUN pGetSessionValue IN hdlGblLibrary (INPUT  chrsessionValueName,                                      
                                          OUTPUT sessionValue).
   
   RETURN sessionValue.
   
END FUNCTION. /*fGetSessionValue*/


FUNCTION fNewSessionValue RETURNS CLASS sessionValue(INPUT chrsessionValueName AS CHARACTER):                                                
   
   DEFINE VARIABLE sessionValue  AS sessionValue.
   DEFINE VARIABLE pmProcessName    AS sessionValue.
   
   /* Get the parent process */
   pmProcessName = fGetSessionValue("ProcessName").
   
   RUN pSetsessionValue IN hdlGblLibrary (INPUT  chrsessionValueName,                                      
                                          INPUT  pmProcessName:intValue,
                                          OUTPUT sessionValue).
      
   RETURN sessionValue.
   
END FUNCTION. /*fNewSessionValue*/


FUNCTION fClearSessionValues RETURNS LOGICAL(INPUT chrProcessName AS CHARACTER):                                                
   
   DEFINE VARIABLE pmProcessID  AS sessionValue.
   DEFINE VARIABLE logSuccess   AS LOGICAL NO-UNDO.
   
   /* Get the Parent Process */
   pmProcessID = fGetSessionValue("ProcessName").  
   
   /* If Process Input Blank do all*/
   IF chrProcessName = "" THEN 
   DO:  
      pmProcessID:setValue(0).  
   END.
   
   RUN pClearSessionValues IN hdlGblLibrary (INPUT  pmProcessID:intValue,
                                             OUTPUT logSuccess).
   
   RETURN logSuccess.
   
END FUNCTION. /*fClearSessionValues*/


FUNCTION fClearSessionValue RETURNS LOGICAL(INPUT chrsessionValueName AS CHARACTER):                                                
   
   DEFINE VARIABLE logSuccess AS LOGICAL NO-UNDO.
   
   RUN pClearSessionValue IN hdlGblLibrary (INPUT  chrsessionValueName,
                                            OUTPUT logSuccess).
   
   RETURN logSuccess.
   
END FUNCTION. /*fClearSessionValues*/


/* Get the ttClone Handle */
FUNCTION fGetCloneHandle RETURNS HANDLE(INPUT chrTableName  AS CHARACTER):
   
   DEFINE VARIABLE hdlBufferHandle AS HANDLE      NO-UNDO.

   RUN pGetCloneHandle IN hdlGblLibrary (INPUT  chrTableName,
                                         OUTPUT hdlBufferHandle).

   RETURN hdlBufferHandle.
   
END FUNCTION. /*fGetCloneHandle*/
