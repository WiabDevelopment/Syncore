TRIGGER PROCEDURE FOR DELETE OF ShipPackageSize.
   
IF NOT NEW core.ShipPackageSize THEN
   ASSIGN core.ShipPackageSize.VersionID = NEXT-VALUE(Version, core).

{trgValidateSession.i}

{trgCreateAudit.i "ShipPackageSize" "DELETE"}

/* Bespoke Trigger Code goes here */


