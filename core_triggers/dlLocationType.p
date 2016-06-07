TRIGGER PROCEDURE FOR DELETE OF LocationType.

{trgValidateSession.i}

{trgCreateAudit.i "LocationType" "DELETE"}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST Location OF LocationType) THEN
DO:
   RETURN ERROR "LocationType has at least one Location. Cannot Delete.".
END.

IF CAN-FIND(FIRST LocationTypeGroupLink OF LocationType) THEN
DO:
   RETURN ERROR "LocationType has at least one LocationTypeGroupLink. Cannot Delete.".
END.

IF CAN-FIND(FIRST RelocatePartStockEntityRule OF LocationType) THEN
DO:
   RETURN ERROR "LocationType has at least one RelocatePartStockEntityRule. Cannot Delete.".
END.

IF CAN-FIND(FIRST RelocateStockEntityRule OF LocationType) THEN
DO:
   RETURN ERROR "LocationType has at least one RelocateStockEntityRule. Cannot Delete.". 
END.   