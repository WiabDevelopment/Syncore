TRIGGER PROCEDURE FOR DELETE OF SerialisedUnit.

{trgValidateSession.i}

{trgCreateAudit.i "SerialisedUnit" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST SerialScan
              WHERE SerialScan.SerialisedUnitID = SerialisedUnit.SerialisedUnitID) 
THEN
DO:
   RETURN ERROR "SerialScan exist for this SerialisedUnit. Cannot Delete.".
END.
