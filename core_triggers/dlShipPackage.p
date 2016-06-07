TRIGGER PROCEDURE FOR DELETE OF ShipPackage.

{trgValidateSession.i}

{trgCreateAudit.i "ShipPackage" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST ShipPackageLine OF ShipPackage) THEN
DO:
   RETURN ERROR "ShipPackage has at least one ShipPackageLine Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipPallet OF ShipPackage) THEN
DO:
   RETURN ERROR "ShipPackage is attached to a ShipPallet Record. Cannot Delete.".
END.

