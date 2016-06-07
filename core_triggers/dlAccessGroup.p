TRIGGER PROCEDURE FOR DELETE OF AccessGroup.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for AccessGroup Table, please make record inactive!".
