TRIGGER PROCEDURE FOR DELETE OF Carrier.

{trgValidateSession.i}

{trgCreateAudit.i "Carrier" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Inbound of Carrier) THEN
DO:
   RETURN ERROR "Carrier has at least one Inbound Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST Outbound of Carrier) THEN
DO:
   RETURN ERROR "Carrier has at least one Outbound Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST CarrierSortation of Carrier) THEN
DO:
   RETURN ERROR "Carrier has at least one Sortation Record. Cannot Delete.".
END.

