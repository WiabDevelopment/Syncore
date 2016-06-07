TRIGGER PROCEDURE FOR DELETE OF FileMaster.

{trgValidateSession.i}

{trgCreateAudit.i "FileMaster" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST File of FileMaster) THEN                                 
DO:                                                                                    
   RETURN ERROR "FileMaster has at least one File Record. Cannot Delete.".
END.



