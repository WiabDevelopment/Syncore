TRIGGER PROCEDURE FOR DELETE OF ShipOrder.

{trgValidateSession.i}
{fncStatusTypeFunctions.i}
{trgCreateAudit.i "ShipOrder" "DELETE"} 

IF fGetStatusCode("ShipOrder",ShipOrder.ShipOrderStatusID) <> "BeingCreated" THEN
DO:
   RETURN ERROR "ShipOrder must be at BeingCreated status for Deletion. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipOrderLine OF ShipOrder) THEN
DO:
   RETURN ERROR "ShipOrder has at least one ShipOrderLine. Cannot Delete.".
END.

