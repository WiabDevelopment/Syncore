TRIGGER PROCEDURE FOR DELETE OF StockEntity.

{trgValidateSession.i}

{trgCreateAudit.i "StockEntity" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST StockPackage OF StockEntity) THEN
DO:
   RETURN ERROR "StockEntity has at least one StockPackage Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST PartStockEntityLink OF StockEntity) THEN
DO:
   RETURN ERROR "StockEntity has at least one PartStockEntityLink Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST RelocateStockEntityRule OF StockEntity) THEN
DO:
   RETURN ERROR "StockEntity has at least one RelocateStockEntityRule Record. Cannot Delete.".
END.

