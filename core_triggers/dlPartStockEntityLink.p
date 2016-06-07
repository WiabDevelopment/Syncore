TRIGGER PROCEDURE FOR DELETE OF PartStockEntityLink.

{trgValidateSession.i}

{trgCreateAudit.i "PartStockEntityLink" "DELETE"}

DEFINE BUFFER childPartStockEntityLink       FOR PartStockEntityLink.
DEFINE BUFFER delRelocatePartStockEntityRule FOR RelocatePartStockEntityRule.

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST StockPackage 
              WHERE StockPackage.PartID        = PartStockEntityLink.PartID
              AND   StockPackage.StockEntityID = PartStockEntityLink.StockEntityID) THEN
DO:
   RETURN ERROR "PartStockEntityLink has at least one StockPackage Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST childPartStockEntityLink 
               WHERE childPartStockEntityLink.PartID              = PartStockEntityLink.PartID
               AND   childPartStockEntityLink.ParentStockEntityID = PartStockEntityLink.StockEntityID) THEN
DO:
   RETURN ERROR "PartStockEntityLink is the Parent OF another PartStockEntityLink for this Part. Cannot Delete.".
END.

/* Tidy up RelocatePartStockEntityRule */
FOR EACH RelocatePartStockEntityRule OF PartStockEntityLink NO-LOCK:
   
   FIND FIRST delRelocatePartStockEntityRule EXCLUSIVE-LOCK
      WHERE ROWID(delRelocatePartStockEntityRule) = ROWID(RelocatePartStockEntityRule) NO-ERROR NO-WAIT.
   IF AVAILABLE delRelocatePartStockEntityRule THEN
      DELETE delRelocatePartStockEntityRule.  
   
END. /* FOR EACH RelocatePartStockEntityRule */   
