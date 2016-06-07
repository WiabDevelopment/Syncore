/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getPickingOptions.i
Purpose : This sets a group of variables that will be used in Picking both in Character and Web enviroments. Included in all programs 
          in Picking.
Author  : BG
Date    : 5th Oct 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/06/2015 ND  Canon      Removed Unused variables, changed Database check to include config table not process options table.
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on defSessionVariables.i */

/* Are we allowed to generate a Pick for Multiple Orders into to One Task */
DEFINE VARIABLE logAllowConsolidatedOrderPicking     AS LOGICAL NO-UNDO.

/* Will we allow User to Select a specific PostPick Location for a Pick at the Pick Generate stage? */
/* Otherewise Picker chooses after Pick subject to OrderType & full/empty PostPick Locations etc    */
DEFINE VARIABLE logAllowSelectionOfPostPickAtGen     AS LOGICAL NO-UNDO.

/* Will we allow User to Select a specific Parent ToteType for a Pick at the Pick Generate stage? */
/* Otherewise Pick goes to any ToteType whose rules allow it to Pick                              */
DEFINE VARIABLE logAllowSelectionOfParentToteType    AS LOGICAL NO-UNDO.

/* Will we allow User to Select a specific Operator for a Pick at the Pick Generate stage? */
/* Otherewise Pick goes to any User whose Eqipment rules allow them to Pick it             */
DEFINE VARIABLE logAllowPickTaskAssignmentAtGen      AS LOGICAL NO-UNDO.

/* When Picker has finished Picking a Pick will we force them to Postpick?  */ 
/* Otherwise we will allow them to continue Picking the next available Pick */
DEFINE VARIABLE logAllowMultiplePicksInOnePostPick   AS LOGICAL NO-UNDO.

DEFINE VARIABLE logAllowSelectionOfStagingAtGen      AS LOGICAL NO-UNDO.

FIND FIRST ShipOrderPickConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE ShipOrderPickConfig THEN
DO:
   chrError = chrError + "No ShipOrderPickConfig exist. Please create ShipOrderPickConfig record.".
END.
ELSE
DO:
   ASSIGN logAllowConsolidatedOrderPicking        = ShipOrderPickConfig.AllowConsolidatedOrderPicking
          logAllowSelectionOfPostPickAtGen        = ShipOrderPickConfig.AllowSelectionOfPostPickAtGen
          logAllowSelectionOfStagingAtGen         = ShipOrderPickConfig.AllowSelectionOfStagingAtGen
          logAllowSelectionOfParentToteType       = ShipOrderPickConfig.AllowSelectionOfParentToteType
          logAllowPickTaskAssignmentAtGen         = ShipOrderPickConfig.AllowPickTaskAssignmentAtGen
          logAllowMultiplePicksInOnePostPick      = ShipOrderPickConfig.AllowMultiplePicksInOnePostPick.
END.

