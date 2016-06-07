TRIGGER PROCEDURE FOR DELETE OF RmaDisposition.

{trgValidateSession.i}
{trgCreateAudit.i "RmaDisposition" "DELETE"}

/* Bespoke Trigger Code goes here */
IF intGblUserID <> 1 AND intGblUserID <> 2 THEN
DO:
   RETURN ERROR "RmaDisposition Cannot be Deleted.".
END.

IF CAN-FIND(FIRST Rma OF RmaDisposition) THEN
DO:
   RETURN ERROR "RmaDisposition has at least one Rma Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST RmaLine OF RmaDisposition) THEN
DO:
   RETURN ERROR "RmaDisposition has at least one RmaLine Record. Cannot Delete.".
END.

