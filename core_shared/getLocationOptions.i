/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getLocationOptions.i
Purpose : This sets a group of variables that will be used in Location both in Character and Web enviroments. Included in all programs 
          using Location manipulation.
Author  : BG
Date    : 6th Nov 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
20/05/2013 MC  DayMen     Add DirectUserToSuggestedLocation
03/04/2015 ND  CanonTlb   Changed to incorporate the new LocationConfig file not the process option.
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on defSessionVariables.i */

/* When relocating Stock, will the system suggest a new Location to relocate to. */
DEFINE VARIABLE logDirectUserToSuggestedLocation AS LOGICAL NO-UNDO.

FIND FIRST LocationConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE LocationConfig THEN
DO:
   chrError = chrError + "No LocationConfig exist. Please create LocationConfig record.".
END.
ELSE
DO:
   ASSIGN logDirectUserToSuggestedLocation   = LocationConfig.DirectUserToSuggestedLocation.
END.

