TRIGGER PROCEDURE FOR DELETE OF BomLine.
/*------------------------------------------------------------------------
  File: dlBomLine.p 
  Description: delete trigger for the BomLine table
  Input Parameters:
      <none>
  Output Parameters:
      <none>
  Author: BomLine
  Created: 11/06/2015
  Revisions:
      
--------------------------------------------------------------------------*/

{trgValidateSession.i}

{trgCreateAudit.i BomLine "DELETE"}

/* Bespoke Trigger Code goes here */
{fncStatusTypeFunctions.i}

DEFINE BUFFER readBom FOR Bom.

FIND FIRST readBom OF BomLine NO-LOCK.

IF readBom.BomStatusID = fGetStatusID("Bom", "Completed") THEN
   RETURN ERROR "Bom is in Status of Complete. Cannot Delete.". 

