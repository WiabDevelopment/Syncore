TRIGGER PROCEDURE FOR DELETE OF MenuItem.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for MenuItem Table, please make record inactive!".
