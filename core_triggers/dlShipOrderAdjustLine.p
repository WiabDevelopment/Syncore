TRIGGER PROCEDURE FOR DELETE OF ShipOrderAdjustLine.

{trgValidateSession.i}
{fncStatusTypeFunctions.i}
{trgCreateAudit.i "ShipOrderAdjustLine" "DELETE"}

IF fGetStatusCode("ShipOrderAdjust", ShipOrderAdjustLine.ShipOrderAdjustStatusID) = "Completed" THEN        
DO:
   RETURN ERROR "ShipOrderAdjustLine was completed for this ShipOrderAdjustLine record. Cannot Delete.".
END.



