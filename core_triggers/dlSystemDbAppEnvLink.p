TRIGGER PROCEDURE FOR DELETE OF SystemDbAppEnvLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for SystemDbAppEnvLink, please make record inactive!".
