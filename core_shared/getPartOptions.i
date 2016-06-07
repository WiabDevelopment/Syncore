/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getPartOptions.i
Purpose : This sets a group of variables that will be used in both Character and Web enviroments. Included in all programs to so with Parts.
Author  : BG
Date    : 1st Feb 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/06/2015 ND  Canon      Removed all variables that where not used anywhere else. 
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on usrSession.i */

/* When a Part is set to AwaitingFai move the Pickable Stock to AwaitingFai    */
DEFINE VARIABLE logPutStockOnHoldDuringFAI AS LOGICAL NO-UNDO.

FIND FIRST InboundConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE InboundConfig THEN
DO:
   chrError = chrError + "No InboundConfig exist. Please create InboundConfig record.".
END.
ELSE
DO:
   ASSIGN
    logPutStockOnHoldDuringFAI = InboundConfig.PutStockOnHoldDuringFAI.
END.
