TRIGGER PROCEDURE FOR DELETE OF BusinessUnit.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for BusinessUnit Table, please make record inactive!".
