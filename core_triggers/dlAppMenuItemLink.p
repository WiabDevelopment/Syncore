TRIGGER PROCEDURE FOR DELETE OF AppMenuItemLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for AppMenuItemLink, please make record inactive!".