TRIGGER PROCEDURE FOR DELETE OF Location.

{trgValidateSession.i}

{trgCreateAudit.i "Location" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST StockPackage OF Location) THEN
DO:
   RETURN ERROR "Location has at least one StockPackage Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST StockHistory OF Location) THEN
DO:
   RETURN ERROR "Location has at least one StockHistory Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST Replenish OF Location) THEN
DO:
   RETURN ERROR "Location has at least one Replenish Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST RelocatePartStockEntityRule OF Location) THEN
DO:
   RETURN ERROR "Location has at least one RelocatePartStockEntityRule Record. Cannot Delete.".
END.
