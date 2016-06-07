/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getReplenOptions.i
Purpose : This sets a group of variables that will be used in Replen both in Character and Web enviroments. Included in all programs 
          in Replen.
Author  : BG
Date    : 30th Oct 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/06/2015 ND  Canon       Changed Database call to include Config table not the process option table. 
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on defSessionVariables.i */

/* Are we allowed to generate a Pick for Multiple Orders into to One Task */
DEFINE VARIABLE logUsingAutoReplenishFunctionality  AS LOGICAL NO-UNDO.

FIND FIRST ReplenishConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE ReplenishConfig THEN
DO:
   chrError = chrError + "No ReplenishConfig exist. Please create ReplenishConfig record.".
END.
ELSE
DO:
   ASSIGN logUsingAutoReplenishFunctionality = ReplenishConfig.UsingAutoReplenishFunctionality.
END.
