TRIGGER PROCEDURE FOR DELETE OF PartType.

{trgValidateSession.i}

{trgCreateAudit.i "PartType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Part OF PartType) THEN
DO:
   RETURN ERROR "PartType has at least one Part. Cannot Delete.".
END.

