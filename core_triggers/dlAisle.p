TRIGGER PROCEDURE FOR DELETE OF Aisle.

{trgValidateSession.i}

{trgCreateAudit.i "Aisle" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Location OF Aisle) THEN
DO:
   RETURN ERROR "Aisle has at least one Location. Cannot Delete.".
END.