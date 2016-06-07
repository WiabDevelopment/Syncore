TRIGGER PROCEDURE FOR DELETE OF CountGroup.

{trgValidateSession.i}

{trgCreateAudit.i "CountGroup" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST CountGroupLocationLink OF CountGroup) THEN
DO:
   RETURN ERROR "CountGroup has at least one linked Location. Cannot Delete.".
END.