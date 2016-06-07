TRIGGER PROCEDURE FOR DELETE OF DangerousGoodsClass.

{trgValidateSession.i}
{trgCreateAudit.i "DangerousGoodsClass" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST DangerousGoodsSubDivision OF DangerousGoodsClass) THEN
DO:
   RETURN ERROR "DangerousGoodsClass has at least one DangerousGoodsSubDivision Record.".
END.
