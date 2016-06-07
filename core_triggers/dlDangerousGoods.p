TRIGGER PROCEDURE FOR DELETE OF DangerousGoods.

{trgValidateSession.i}
{trgCreateAudit.i "DangerousGoods" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST PartDangerousGoodsLink OF DangerousGoods) THEN
DO:
   RETURN ERROR "DangerousGoods has at least one PartDangerousGoodsLink Record.".
END.
