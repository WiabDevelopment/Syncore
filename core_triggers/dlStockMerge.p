TRIGGER PROCEDURE FOR DELETE OF StockMerge.

{trgValidateSession.i}

{trgCreateAudit.i "StockMerge" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST StockPackage WHERE StockPackage.StockPackageID = StockMerge.MergeToStockPackageID) THEN
DO:
   RETURN ERROR "StockMerge has at least one StockPackage Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST StockPackage WHERE StockPackage.StockPackageID = StockMerge.MergeFromStockPackageID) THEN
DO:
   RETURN ERROR "StockMerge has at least one StockPackage Record. Cannot Delete.".
END.

