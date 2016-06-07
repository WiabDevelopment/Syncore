/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getShipOrderOptions.i
Purpose : This sets a group of variables that will be used in both Character and Web enviroments. Included in all programs to so with Parts.
Author  : BG
Date    : 17/04/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on usrSession.i */

/* Allow ShipOrders to be Cancelled? */
DEFINE VARIABLE logAllowShipOrderLineCancel AS LOGICAL NO-UNDO.

/* Allow ShipOrders to be Split by Line prior to Generate? */
DEFINE VARIABLE logAllowShipOrderLineSplit  AS LOGICAL NO-UNDO.

/* Allow ShipOrders to be Split prior to Generate? */
DEFINE VARIABLE logAllowShipOrderSplit      AS LOGICAL NO-UNDO.

/* Allow Adjustment up and down of ShipOrderLine Qty? */
DEFINE VARIABLE logAllowShipOrderLineAdjust AS LOGICAL NO-UNDO.

FIND FIRST ShipOrderConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE ShipOrderConfig THEN
DO:
   chrError = chrError + "No ShipOrderConfig exist. Please create ShipOrderConfig record.".
END.
ELSE
DO:
   ASSIGN
    logAllowShipOrderLineCancel = ShipOrderConfig.AllowShipOrderLineCancel
    logAllowShipOrderLineSplit  = ShipOrderConfig.AllowShipOrderLineSplit
    logAllowShipOrderSplit      = ShipOrderConfig.AllowShipOrderSplit
    logAllowShipOrderLineAdjust = ShipOrderConfig.AllowShipOrderLineAdjust.
END.
