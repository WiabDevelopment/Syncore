TRIGGER PROCEDURE FOR DELETE OF ShipOrderSplitLine.

{trgValidateSession.i}

{trgCreateAudit.i "ShipOrderSplitLine" "DELETE"}

FIND FIRST ShipOrderSplit NO-LOCK
   WHERE ShipOrderSplit.ShipOrderSplitID = ShipOrderSplitLine.ShipOrderSplitID NO-ERROR.
   
IF AVAILABLE ShipOrderSplit AND    
   ShipOrderSplit.TargetShipOrderID > 0 THEN 
DO:
   RETURN ERROR "A target ShipOrder was created for this ShipOrderSplitLine record. Cannot Delete.".
END.
