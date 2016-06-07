TRIGGER PROCEDURE FOR DELETE OF ShipPackageType.

IF NOT NEW ShipPackageType THEN
   ASSIGN ShipPackageType.VersionID = NEXT-VALUE(Version).

{trgValidateSession.i}

{trgCreateAudit.i "ShipPackageType" "DELETE"}

/* Bespoke Trigger Code goes here*/
IF CAN-FIND(FIRST ShipPackage OF ShipPackageType) THEN
DO:
   RETURN ERROR "ShipPackageType has at least one Ship Package Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipPackageSize OF ShipPackageType) THEN
DO:
   RETURN ERROR "ShipPackageType has at least one Ship Package Size Record. Cannot Delete.".
END.

