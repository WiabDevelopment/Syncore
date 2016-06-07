TRIGGER PROCEDURE FOR DELETE OF CountGroupType.

{trgValidateSession.i}

{trgCreateAudit.i "LocationType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST CountGroup OF CountGroupType) THEN
DO:
   RETURN ERROR "CountGroupType has at least one CountGroup. Cannot Delete.".
END.