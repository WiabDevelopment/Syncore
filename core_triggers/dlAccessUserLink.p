TRIGGER PROCEDURE FOR DELETE OF AccessUserLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for AccessUserLink, please make record inactive!".