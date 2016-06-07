TRIGGER PROCEDURE FOR DELETE OF RmaLine.

{trgValidateSession.i}
{fncStatusTypeFunctions.i}
{trgCreateAudit.i "RmaLine" "DELETE"}

DEFINE BUFFER delRmaLineHistory FOR RmaLineHistory.

IF fGetStatusCode("RmaLine", RmaLine.RmaStatusID) <> "Created" THEN
DO:
   RETURN ERROR "RmaLine must be at Created status for Deletion. Cannot Delete.".
END.

/* Tidy up RmaLineHistory */
FOR EACH RmaLineHistory OF RmaLine NO-LOCK:
   
   FIND FIRST delRmaLineHistory EXCLUSIVE-LOCK
      WHERE ROWID(delRmaLineHistory) = ROWID(RmaLineHistory) NO-ERROR NO-WAIT.
   IF AVAILABLE delRmaLineHistory THEN
      DELETE delRmaLineHistory.  
   
END. /* FOR EACH RmaLineHistory */   




