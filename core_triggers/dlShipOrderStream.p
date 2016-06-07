TRIGGER PROCEDURE FOR DELETE OF ShipOrderStream.
/*------------------------------------------------------------------------
  File: dlShipOrderStream.p 
  Description: delete trigger for the ShipOrderStream table
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: Todd Wierzchowski
  Created: 21/11/2014
  Revisions:
      
--------------------------------------------------------------------------*/

{trgValidateSession.i}

{trgCreateAudit.i ShipOrderStream "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}

IF CAN-FIND(FIRST ShipOrderStreamPackoutLink NO-LOCK 
               WHERE ShipOrderStreamPackoutLink.ShipOrderStreamID = ShipOrderStream.ShipOrderStreamID) THEN
DO:
   RETURN ERROR "ShipOrderStream has linked ShipOrderStreamPackoutLinks. Cannot Delete.".
END.

IF CAN-FIND(FIRST ShipOrderTypeStreamLink NO-LOCK 
               WHERE ShipOrderTypeStreamLink.ShipOrderStreamID = ShipOrderStream.ShipOrderStreamID) THEN
DO:
   RETURN ERROR "ShipOrderStream has linked ShipOrderTypeStreamLinks. Cannot Delete.".
END.