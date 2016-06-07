TRIGGER PROCEDURE FOR DELETE OF ShipOrderStreamPackoutLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for ShipOrderStreamPackoutLink, please make record inactive!".