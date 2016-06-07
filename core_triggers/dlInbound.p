TRIGGER PROCEDURE FOR DELETE OF Inbound.

{trgValidateSession.i}

{trgCreateAudit.i "Inbound" "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}

IF CAN-FIND(FIRST Asn OF Inbound) THEN
DO:
   RETURN ERROR "Inbound has at least one Asn attached. Cannot Delete.".
END.

IF fGetStatusCode("Inbound",Inbound.InboundStatusID) <> "Created" THEN
DO:
   RETURN ERROR "Inbound must be at Created status for Deletion. Cannot Delete.".
END.
 
