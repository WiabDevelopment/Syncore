TRIGGER PROCEDURE FOR DELETE OF SerialMaskType.
/*------------------------------------------------------------------------
  File: dlSerialMaskType.p 
  Description: delete trigger for the SerialMaskType table
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: Michael Landess
  Created: 24/11/2014
  Revisions:
      
--------------------------------------------------------------------------*/

{trgValidateSession.i}

{trgCreateAudit.i SerialMaskType "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}