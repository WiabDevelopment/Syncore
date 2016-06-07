TRIGGER PROCEDURE FOR DELETE OF InboundStatus.

{trgValidateSession.i}

{trgCreateAudit.i "InboundStatus" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Inbound OF InboundStatus) THEN
DO:
   RETURN ERROR "InboundStatus has at least one Inbound Record. Cannot Delete.".
END.
