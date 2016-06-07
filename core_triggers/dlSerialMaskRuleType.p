TRIGGER PROCEDURE FOR DELETE OF SerialMaskRuleType.
/*------------------------------------------------------------------------
  File: dlSerialMaskRuleType.p 
  Description: delete trigger for the SerialMaskRuleType table
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: Michael Landess
  Created: 24/11/2014
  Revisions:
      
--------------------------------------------------------------------------*/

{trgValidateSession.i}

{trgCreateAudit.i SerialMaskRuleType "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}