TRIGGER PROCEDURE FOR DELETE OF Outbound.

{trgValidateSession.i}

{trgCreateAudit.i "Outbound" "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}

IF CAN-FIND(FIRST ShipPackage OF Outbound) THEN
DO:
   RETURN ERROR "Outbound has at least one ShipPackage attached. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipPallet OF Outbound) THEN
DO:
   RETURN ERROR "Outbound has at least one ShipPallet attached. Cannot Delete.".
END.

IF fGetStatusName("Outbound", Outbound.OutboundStatusID) <> "Created" THEN
DO:
   RETURN ERROR "Outbound must be at Created status. Cannot Delete.".
END.
