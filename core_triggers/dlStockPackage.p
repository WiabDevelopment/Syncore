TRIGGER PROCEDURE FOR DELETE OF StockPackage.

{trgValidateSession.i}

{trgCreateAudit.i "StockPackage" "DELETE"}

IF intGblUserID <> 1 AND intGblUserID <> 2 THEN
DO:
   RETURN ERROR "StockPackages Cannot be Deleted.".
END.
