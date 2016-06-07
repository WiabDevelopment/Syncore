TRIGGER PROCEDURE FOR DELETE OF TaskExecutionRule.

{trgValidateSession.i}

{trgCreateAudit.i "TaskExecutionRule" "DELETE"}

/* Bespoke Trigger Code goes here */
IF intGblUserID <> 1 AND intGblUserID <> 2 THEN
DO:
   RETURN ERROR "You must have SuperUser Status to Delete these Records.".
END.

