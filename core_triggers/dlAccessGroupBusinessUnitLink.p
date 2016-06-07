TRIGGER PROCEDURE FOR DELETE OF AccessGroupBusinessUnitLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for AccessGroupBusinessUnitLink, please make record inactive!". 