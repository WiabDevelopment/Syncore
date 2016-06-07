TRIGGER PROCEDURE FOR DELETE OF PackoutStation.

{trgValidateSession.i}

{trgCreateAudit.i PackoutStation "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}

IF CAN-FIND(FIRST PostpickPackoutLink NO-LOCK 
               WHERE PostpickPackoutLink.PackoutStationID = PackoutStation.PackoutStationID) THEN
DO:
   RETURN ERROR "PackoutStation has linked PostpickPackoutLinks. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipOrderStreamPackoutLink NO-LOCK 
               WHERE ShipOrderStreamPackoutLink.PackoutStationID = PackoutStation.PackoutStationID) THEN
DO:
   RETURN ERROR "PackoutStation has linked ShipOrderStreamPackoutLinks. Cannot Delete.".
END.

