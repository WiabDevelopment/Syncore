TRIGGER PROCEDURE FOR DELETE OF SerialScanRequiredOpTypeLink.

{trgValidateSession.i}

/* Bespoke Trigger Code goes here */
RETURN ERROR "Deletes are not allowed for SerialScanRequiredOpTypeLink, please make record inactive!".