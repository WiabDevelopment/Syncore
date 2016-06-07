TRIGGER PROCEDURE FOR DELETE OF Environment.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for Environment Table, please make record inactive!".
