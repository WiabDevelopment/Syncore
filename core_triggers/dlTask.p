TRIGGER PROCEDURE FOR DELETE OF Task.

IF NOT NEW Task THEN
   ASSIGN Task.VersionID = NEXT-VALUE(Version).

{trgValidateSession.i}

{trgCreateAudit.i "Task" "DELETE"}

/* Bespoke Trigger Code goes here */
DEFINE BUFFER deleteTaskLine FOR TaskLine.

/* Clear the children also */
FOR EACH TaskLine OF Task NO-LOCK:
  
   FIND deleteTaskLine WHERE ROWID(deleteTaskLine) = ROWID(TaskLine) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
   IF LOCKED(deleteTaskLine) THEN 
   DO:
      RETURN ERROR "TaskLine child record locked cannot Delete. Please try again later.".
   END.
   
   DELETE deleteTaskLine.
END.

