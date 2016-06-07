TRIGGER PROCEDURE FOR DELETE OF AccessMenuItemLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for AccessMenuItemLink, please make record inactive!".
