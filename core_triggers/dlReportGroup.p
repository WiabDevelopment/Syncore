TRIGGER PROCEDURE FOR DELETE OF ReportGroup.

{trgValidateSession.i}

{trgCreateAudit.i "ReportGroup" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Report of ReportGroup) THEN
DO:
   RETURN ERROR "ReportGroup has at least one Report Record. Cannot Delete.".
END.


