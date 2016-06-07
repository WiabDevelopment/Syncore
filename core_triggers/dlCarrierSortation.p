TRIGGER PROCEDURE FOR DELETE OF CarrierSortation.

{trgValidateSession.i}

{trgCreateAudit.i "CarrierSortation" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST ShipOrder of CarrierSortation) THEN
DO:
   RETURN ERROR "CarrierSortation has at least one ShipOrder Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST SortationShipLaneLink of CarrierSortation) THEN
DO:
   RETURN ERROR "CarrierSortation has at least one SortationShipLaneLink Record. Cannot Delete.".
END.


