TRIGGER PROCEDURE FOR DELETE OF CronEventRunResultType.

{trgValidateSession.i}

{trgCreateAudit.i "CronEventRunResultType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST CronEventRun OF CronEventRunResultType) THEN
DO:
   RETURN ERROR "CronEventRunResultType has at least one CronEventRun Record. Cannot Delete.".
END.
