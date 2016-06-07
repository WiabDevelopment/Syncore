TRIGGER PROCEDURE FOR DELETE OF ShipOrderNotaFiscal.

{trgValidateSession.i}

{trgCreateAudit.i ShipOrderNotaFiscal "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST File OF ShipOrderNotaFiscal) THEN
DO:
   RETURN ERROR "ShipOrderNotaFiscal has at least one File assigned to it. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipOrderNotaFiscalHist OF ShipOrderNotaFiscal) THEN
DO:
   RETURN ERROR "ShipOrderNotaFiscal has at least one ShipOrderNotaFiscalHist assigned to it. Cannot Delete.".
END.

