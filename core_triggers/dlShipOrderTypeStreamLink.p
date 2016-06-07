TRIGGER PROCEDURE FOR DELETE OF ShipOrderTypeStreamLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for ShipOrderTypeStreamLink, please make record inactive!".