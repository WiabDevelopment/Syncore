TRIGGER PROCEDURE FOR DELETE OF Part.

{trgValidateSession.i}

{trgCreateAudit.i "Part" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST StockPackage OF Part) THEN
DO:
   RETURN ERROR "Part has at least one StockPackage Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST Replenish OF Part) THEN
DO:
   RETURN ERROR "Part has at least one Replenish Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST AsnLine OF Part) THEN
DO:
   RETURN ERROR "Part has at least one AsnLine Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST InventoryBalance OF Part WHERE InventoryBalance.Balance <> 0) THEN
DO:
   RETURN ERROR "Part has at least one InventoryBalance Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST StockUpdate OF Part WHERE StockUpdate.Updated = "") THEN
DO:
   RETURN ERROR "Part has at least one StockUpdate Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipPackageLine OF Part) THEN
DO:
   RETURN ERROR "Part has at least one ShipPackageLine Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST PartStockEntityLink OF Part) THEN
DO:
   RETURN ERROR "Part has at least one PartStockEntityLink Record. Cannot Delete.".
END. 

IF CAN-FIND(FIRST SerialScanRequired OF Part) THEN
DO:
   RETURN ERROR "Part has at least one SerialScanRequired Record. Cannot Delete.".
END.  
