TRIGGER PROCEDURE FOR DELETE OF TaskTypeGroup.

IF NOT NEW TaskTypeGroup THEN
   ASSIGN TaskTypeGroup.VersionID = NEXT-VALUE(Version).

{trgValidateSession.i}

{trgCreateAudit.i "TaskTypeGroup" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST TaskTypeGroupLink of TaskTypeGroup) THEN
DO:
   RETURN ERROR "TaskTypeGroup has at least one TaskTypeGroupLink Record. Cannot Delete.".
END.
