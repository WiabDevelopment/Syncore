TRIGGER PROCEDURE FOR DELETE OF ShipOrderType.
/*------------------------------------------------------------------------
  File: dlShipOrderType.p 
  Description: delete trigger for the ShipOrderType table
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: Todd Wierzchowski
  Created: 21/11/2014
  Revisions:
      
--------------------------------------------------------------------------*/

{trgValidateSession.i}

{trgCreateAudit.i ShipOrderType "DELETE"}

{fncStatusTypeFunctions.i}

/* Bespoke Trigger Code goes here */
IF CAN-FIND(FIRST ShipOrderTypeStreamLink NO-LOCK 
               WHERE ShipOrderTypeStreamLink.ShipOrderTypeID = ShipOrderType.ShipOrderTypeID) THEN
DO:
   RETURN ERROR "ShipOrderType has linked ShipOrderTypeStreamLinks. Cannot Delete.".
END.

