TRIGGER PROCEDURE FOR DELETE OF Printer.

{trgValidateSession.i}

{trgCreateAudit.i "Printer" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST PrinterLocationLink of Printer) THEN
DO:
   RETURN ERROR "Printer has at least one PrinterLocationLink Record. Cannot Delete.".
END.


