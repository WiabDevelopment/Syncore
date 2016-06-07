/*------------------------------------------------------------------------------------------------------------------------------------------
Program : dbKillLocks.p
Purpose : This program is called by dbGetRecord.p and dbReleaseLocks.p and dbWebLockUpdate.p used to mark the ssnWebLock record as completed.
Author  : BG
Date    : 23rd May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER chrSessionID AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER chrReason    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER chrErrorMsg  AS CHARACTER   NO-UNDO.

{defSessionVariables.i}
{fncGlobalFunctions.i}

DEFINE VARIABLE intErrorCount        AS INTEGER     NO-UNDO.

UpdateBlk:
DO TRANSACTION:
   
   FIND ssnWebLock NO-LOCK 
      WHERE ssnWebLock.Completed = "" 
      AND   ssnWebLock.SessionID = chrSessionID NO-ERROR.
   
   IF AVAILABLE ssnWebLock THEN
   DO:
      FIND CURRENT ssnWebLock EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      
      IF NOT AVAILABLE ssnWebLock THEN
      DO:
         chrErrorMsg = "Unable to Cancel Record Locks. Please Log Out and In again before proceeding. ".
         LEAVE UpdateBlk.
      END.
      
      /* Db Trigger will automatically complete all of this lock's children also */
      ASSIGN ssnWebLock.Committed = fTimestamp(NOW)
             ssnWebLock.Completed = ssnWebLock.Committed
             ssnWebLock.Outcome   = chrReason NO-ERROR.
      
      IF ssnWebLock.Completed = "" OR ssnWebLock.Completed = ? OR ERROR-STATUS:ERROR THEN 
      DO:
         DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
           chrErrorMsg = chrErrorMsg + ERROR-STATUS:GET-MESSAGE(intErrorCount).
         END.
         chrErrorMsg = "Unable to Cancel Record Locks. " + chrErrorMsg.
      END.
   END. /*IF AVAILABLE ssnWebLock THEN*/
   
END. /*UpdateBlk*/

FIND CURRENT ssnWebLock NO-LOCK NO-ERROR.
ERROR-STATUS:ERROR = FALSE.