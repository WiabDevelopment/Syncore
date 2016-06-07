TRIGGER PROCEDURE FOR DELETE OF ShipOrderAdjust.

{trgValidateSession.i}
{fncStatusTypeFunctions.i}
{trgCreateAudit.i "ShipOrderAdjust" "DELETE"}

IF fGetStatusCode("ShipOrderAdjust", ShipOrderAdjust.ShipOrderAdjustStatusID) = "Completed" THEN        
DO:
   RETURN ERROR "ShipOrderAdjust was completed for this ShipOrderAdjust record. Cannot Delete.".
END.


