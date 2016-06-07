TRIGGER PROCEDURE FOR DELETE OF KittingLine.
/*------------------------------------------------------------------------
  File: dlKittingLine.p 
  Description: delete trigger for the ShipOrderStream table
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: Nick Diessner
  Created: 20/02/2015
  Revisions:
      
--------------------------------------------------------------------------*/

{trgValidateSession.i}

{trgCreateAudit.i KittingLine "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}
/* Need a KittingLineStationLink Table */
IF CAN-FIND(FIRST KittingStation NO-LOCK 
               WHERE KittingStation.KittingLineID = KittingLine.KittingLineID) THEN
DO:
   RETURN ERROR "Kitting Line has Linked KittingLineStationLink. Cannot Delete.".
END.