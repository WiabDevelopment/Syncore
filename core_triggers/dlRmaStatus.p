TRIGGER PROCEDURE FOR DELETE OF RmaStatus.

{trgValidateSession.i}
{trgCreateAudit.i "RmaStatus" "DELETE"}

/* Bespoke Trigger Code goes here */
IF intGblUserID <> 1 AND intGblUserID <> 2 THEN
DO:
   RETURN ERROR "RmaStatuses Cannot be Deleted.".
END.

IF CAN-FIND(FIRST Rma OF RmaStatus) THEN
DO:
   RETURN ERROR "RmaStatus has at least one Rma Record. Cannot Delete.".
END.

IF CAN-FIND(FIRST RmaLine OF RmaStatus) THEN
DO:
   RETURN ERROR "RmaStatus has at least one RmaLine Record. Cannot Delete.".
END.

