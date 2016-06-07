TRIGGER PROCEDURE FOR DELETE OF TaskLine.

IF NOT NEW TaskLine THEN
   ASSIGN TaskLine.VersionID = NEXT-VALUE(Version).

{trgValidateSession.i}

{trgCreateAudit.i "TaskLine" "DELETE"}

/* Bespoke Trigger Code goes here */
DEFINE BUFFER deleteTaskLineWork FOR TaskLineWork.

/* Clear the children also */
FOR EACH TaskLineWork OF TaskLine NO-LOCK:
  
   FIND deleteTaskLineWork WHERE ROWID(deleteTaskLineWork) = ROWID(TaskLineWork) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF LOCKED(deleteTaskLineWork) THEN
   DO:
      RETURN ERROR "TaskLine Work child record locked cannot Delete. Please try again later.".
   END.
   
   /* Added BG */
   DELETE deleteTaskLineWork.
END.
