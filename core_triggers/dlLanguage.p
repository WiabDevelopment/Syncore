TRIGGER PROCEDURE FOR DELETE OF Language.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for Language Table, please make record inactive!".
