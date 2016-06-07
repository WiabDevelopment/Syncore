TRIGGER PROCEDURE FOR DELETE OF ShipOrderSplit.

{trgValidateSession.i}

{trgCreateAudit.i "ShipOrderSplit" "DELETE"}

/* Check that ShipOrder is not generated from the record to be deleted */
IF ShipOrderSplit.TargetShipOrderID > 0 THEN 
DO:
   RETURN ERROR "A target ShipOrder was created for this ShipOrderSplit record. Cannot Delete.".
END.


