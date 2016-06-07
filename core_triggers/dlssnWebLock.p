TRIGGER PROCEDURE FOR DELETE OF ssnWebLock.

DEFINE BUFFER updWebRecordLock FOR ssnWebRecordLock.

/* Just clean up the children */
FOR EACH ssnWebRecordLock NO-LOCK 
   WHERE ssnWebRecordLock.WebLockID = ssnWebLock.WebLockID:
   
   FIND updWebRecordLock WHERE ROWID(updWebRecordLock) = ROWID(ssnWebRecordLock) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF AVAILABLE updWebRecordLock THEN
      DELETE updWebRecordLock.
   
END. /*FOR EACH ssnWebRecordLock NO-LOCK */

