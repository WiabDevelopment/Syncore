TRIGGER PROCEDURE FOR DELETE OF GateUser.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for GateUser Table, please make record inactive!".
