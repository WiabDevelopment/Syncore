TRIGGER PROCEDURE FOR DELETE OF ShipLane.

{trgValidateSession.i}

{trgCreateAudit.i "ShipLane" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST SortationShipLaneLink of ShipLane) THEN
DO:
   RETURN ERROR "ShipLane has at least one SortationShipLaneLink Record. Cannot Delete.".
END.


