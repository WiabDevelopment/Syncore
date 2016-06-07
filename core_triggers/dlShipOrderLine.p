TRIGGER PROCEDURE FOR DELETE OF ShipOrderLine.

{trgValidateSession.i}
{fncStatusTypeFunctions.i}
{trgCreateAudit.i "ShipOrderLine" "DELETE"}

DEFINE BUFFER delShipOrderLineHistory FOR ShipOrderLineHistory.

IF fGetStatusCode("ShipOrder",ShipOrderLine.ShipOrderStatusID) <> "BeingCreated" THEN
DO:
   RETURN ERROR "ShipOrderLine must be at BeingCreated status for Deletion. Cannot Delete.".
END.

/* Tidy up ShipOrderLineHistory */
FOR EACH ShipOrderLineHistory OF ShipOrderLine NO-LOCK:
   
   FIND FIRST delShipOrderLineHistory EXCLUSIVE-LOCK
      WHERE ROWID(delShipOrderLineHistory) = ROWID(ShipOrderLineHistory) NO-ERROR NO-WAIT.
   IF AVAILABLE delShipOrderLineHistory THEN
      DELETE delShipOrderLineHistory.  
   
END. /* FOR EACH ShipOrderLineHistory */   

