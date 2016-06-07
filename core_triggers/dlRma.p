TRIGGER PROCEDURE FOR DELETE OF Rma.

{trgValidateSession.i}
{fncStatusTypeFunctions.i}
{trgCreateAudit.i "Rma" "DELETE"}

IF fGetStatusCode("Rma", Rma.RmaStatusID) <> "Created" THEN
DO:
   RETURN ERROR "Rma must be at Created status for Deletion. Cannot Delete.".
END.

IF CAN-FIND(FIRST RmaLine OF Rma) THEN
DO:
   RETURN ERROR "Rma has at least one RmaLine. Cannot Delete.".
END.

