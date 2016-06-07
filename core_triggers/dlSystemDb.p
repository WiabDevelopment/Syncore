TRIGGER PROCEDURE FOR DELETE OF SystemDb.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for SystemDb, please make record inactive!".
