TRIGGER PROCEDURE FOR DELETE OF OperationType.

{trgValidateSession.i}

{trgCreateAudit.i "OperationType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST ToteHistory WHERE ToteHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ToteHistory.  Cannot Delete.".
END.
    
IF CAN-FIND(FIRST TaskHistory WHERE TaskHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one TaskHistory. Cannot Delete.".
END.
            
IF CAN-FIND(FIRST ShipOrderHistory WHERE ShipOrderHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ShipOrderHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST StockHistory WHERE StockHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one StockHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST StockStatusHistory WHERE StockStatusHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one StockStatusHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipPalletHistory WHERE ShipPalletHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ShipPalletHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST StockItemHistory WHERE StockItemHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one StockItemHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipPackageHistory WHERE ShipPackageHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ShipPackageHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST PartStockEntityLinkHistory WHERE PartStockEntityLinkHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one PartStockEntityLinkHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST PartHistory WHERE PartHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one PartHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipOrderLineHistory WHERE ShipOrderLineHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ShipOrderLineHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST LocationHistory WHERE LocationHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one LocationHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST InboundHistory WHERE InboundHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one InboundHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST OutboundHistory WHERE OutboundHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one OutboundHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST TaskExecutionRuleHistory WHERE TaskExecutionRuleHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one TaskExecutionRuleHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST TaskTypeEquipToteTypeLinkHist WHERE TaskTypeEquipToteTypeLinkHist.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one TaskTypeEquipToteTypeLinkHist. Cannot Delete.".
END.

IF CAN-FIND(FIRST CronEventHistory WHERE CronEventHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one CronEventHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST ConfigHistory WHERE ConfigHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ConfigHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST CronConfigHistory WHERE CronConfigHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one CronConfigHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST ReplenishConfigHistory WHERE ReplenishConfigHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ReplenishConfigHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipOrderPickConfigHistory WHERE ShipOrderPickConfigHistory.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one ShipOrderPickConfigHistory. Cannot Delete.".
END.

IF CAN-FIND(FIRST TaskType WHERE TaskType.OperationTypeID = OperationType.OperationTypeID) THEN
DO:
   RETURN ERROR "OperationType has at least one TaskType. Cannot Delete.".
END.
