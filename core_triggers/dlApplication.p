TRIGGER PROCEDURE FOR DELETE OF Application.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for Application Table, please make record inactive!".
