/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getKittingOptions.i
Purpose : This sets a group of variables that will be used in Kitting both in Character and Web environments. Included in all programs 
          in Kitting.
Author  : BG
Date    : 17th February 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependency on defSessionVariables.i */
/*
{defSessionVariables.i}
*/

DEFINE VARIABLE logForceScanningOfComponentParts     AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE logForceScanningOfFgPart             AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE logBackflushPerFullFGStockEntity     AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE logAllowMixedWorkOrdersOnOnePallet   AS LOGICAL NO-UNDO INITIAL NO.

FIND FIRST KittingConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE KittingConfig THEN
DO:
   chrError = chrError + "No KittingConfig exist. Please create KittingConfig record.".
END.
ELSE
DO:
   ASSIGN logForceScanningOfComponentParts    = KittingConfig.ForceScanningOfComponentParts
          logForceScanningOfFgPart            = KittingConfig.ForceScanningOfFgPart
          logBackflushPerFullFGStockEntity    = KittingConfig.BackflushPerFullFGStockEntity
          logAllowMixedWorkOrdersOnOnePallet  = KittingConfig.AllowMixedWorkOrdersOnOnePallet.
END. 

