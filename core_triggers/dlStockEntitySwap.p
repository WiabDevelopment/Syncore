TRIGGER PROCEDURE FOR DELETE OF StockEntitySwap.

{trgValidateSession.i}

{trgCreateAudit.i "StockEntitySwap" "DELETE"}

/* Bespoke Trigger Code goes here */
IF StockEntitySwap.Completed = "" THEN
DO:
   RETURN ERROR "StockEntitySwap is not Completed. Cannot Delete.".
END.


