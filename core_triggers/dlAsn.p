TRIGGER PROCEDURE FOR DELETE OF Asn.

{trgValidateSession.i}

{trgCreateAudit.i "Asn" "DELETE"}

/* Bespoke Trigger Code goes here  */
IF CAN-FIND(FIRST AsnLine OF Asn) THEN
DO:
   RETURN ERROR "Asn has at least one AsnLine. Cannot Delete.".
END.

FIND FIRST TaskStatus WHERE TaskStatus.StatusCode = "Cancelled" NO-LOCK NO-ERROR.
FIND FIRST Inbound NO-LOCK WHERE Inbound.InboundID = Asn.InboundID NO-ERROR.
FIND FIRST Task OF Inbound NO-LOCK NO-ERROR.
IF AVAILABLE Task THEN
DO:
   IF AVAILABLE TaskStatus THEN
   DO:
      IF Task.TaskStatusID <> TaskStatus.TaskStatusID THEN
         RETURN ERROR "Inbound already has tasks associated with it.".
   END. /*IF AVAILABLE TaskStatus THEN*/
END. /* FIND FIRST Task OF TaskLine NO-LOCK NO-ERROR.*/

