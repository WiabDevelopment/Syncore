TRIGGER PROCEDURE FOR DELETE OF DangerousGoodsSubDivision.

{trgValidateSession.i}
{trgCreateAudit.i "DangerousGoodsSubDivision" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST DangerousGoods OF DangerousGoodsSubDivision) THEN
DO:
   RETURN ERROR "DangerousGoodsSubDivision is used by at least one DangerousGoods Record.".
END.
