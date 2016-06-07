TRIGGER PROCEDURE FOR DELETE OF KittingStation.

{trgValidateSession.i}

{trgCreateAudit.i KittingStation "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}

IF CAN-FIND(FIRST KittingStationUserLink OF KittingStation WHERE Completed = "") THEN
DO:
   RETURN ERROR "KittingStation has a KittingStationUserLink attached. Cannot Delete.".
END.
