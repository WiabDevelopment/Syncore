TRIGGER PROCEDURE FOR DELETE OF CountTaskType.

{trgValidateSession.i}

{trgCreateAudit.i "CountTaskType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST CountGroup OF CountTaskType) THEN
DO:
   RETURN ERROR "CountTaskType has at least one CountGroup. Cannot Delete.".
END.
