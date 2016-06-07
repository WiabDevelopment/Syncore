TRIGGER PROCEDURE FOR DELETE OF ShipPalletType.

{trgValidateSession.i}

{trgCreateAudit.i "ShipPalletType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST ShipPallet OF ShipPalletType) THEN
DO:
   RETURN ERROR "ShipPalletType has at least one ShipPallet. Cannot Delete.".
END.
