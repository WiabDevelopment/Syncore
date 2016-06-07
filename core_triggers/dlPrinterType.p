TRIGGER PROCEDURE FOR DELETE OF PrinterType.

{trgValidateSession.i}

{trgCreateAudit.i "PrinterType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Printer of PrinterType) THEN
DO:
   RETURN ERROR "PrinterType has at least one Printer Record. Cannot Delete.".
END.