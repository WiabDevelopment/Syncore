TRIGGER PROCEDURE FOR DELETE OF AccessAppLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for AccessAppLink, please make record inactive!".