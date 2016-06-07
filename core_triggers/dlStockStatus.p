TRIGGER PROCEDURE FOR DELETE OF StockStatus.

{trgValidateSession.i}

{trgCreateAudit.i "StockStatus" "DELETE"}

/* Bespoke Trigger Code goes here */
IF intGblUserID <> 1 AND intGblUserID <> 2 THEN
DO:
   RETURN ERROR "StockStatuses Cannot be Deleted.".
END.

IF CAN-FIND(FIRST StockPackage OF StockStatus) THEN
DO:
   RETURN ERROR "StockStatus has at least one StockPackage Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST AsnLine OF StockStatus) THEN
DO:
   RETURN ERROR "StockStatus is attached to a AsnLine Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST StockStatusGroupLink OF StockStatus) THEN
DO:
   RETURN ERROR "StockStatus is attached to a StockStatusGroupLink Record. Cannot Delete.".
END.
