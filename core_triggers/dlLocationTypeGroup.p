TRIGGER PROCEDURE FOR DELETE OF LocationTypeGroup.

IF NOT NEW LocationTypeGroup THEN
   ASSIGN LocationTypeGroup.VersionID = NEXT-VALUE(Version).

{trgValidateSession.i}

{trgCreateAudit.i "LocationTypeGroup" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST LocationTypeGroupLink of LocationTypeGroup) THEN
DO:
   RETURN ERROR "LocationTypeGroup has at least one LocationTypeGroupLink Record. Cannot Delete.".
END.
