TRIGGER PROCEDURE FOR DELETE OF AccessGroupPermissionLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for AccessGroupPermissionLink, please make record inactive!". 