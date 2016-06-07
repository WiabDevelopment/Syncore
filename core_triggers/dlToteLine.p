TRIGGER PROCEDURE FOR DELETE OF ToteLine.
/*------------------------------------------------------------------------
  File: dlToteLine.p 
  Description: delete trigger for the ToteLine table
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: Anthony Ferrari
  Created: 22/09/15
  Revisions:
      
--------------------------------------------------------------------------*/

{trgValidateSession.i}

{trgCreateAudit.i ToteLine "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}