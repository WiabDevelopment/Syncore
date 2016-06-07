/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncToteFunctions.i
Purpose : All functions to do with Totes
Author  : BG
Date    : 1st November 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

FUNCTION fNewToteRef RETURNS CHAR (INPUT intToteID     AS INT,
                                   INPUT intToteTypeID AS INT):
   
   DEF BUFFER newToteType FOR ToteType.
   
   FIND newToteType NO-LOCK
      WHERE newToteType.ToteTypeID = intToteTypeID NO-ERROR.
   IF NOT AVAIL newToteType THEN
      RETURN "".
   
   /* For ChildTotes we need to use a lower Sequence for the Ref field */
   IF newToteType.IsChildType THEN
      RETURN newToteType.LabelPrefix + STRING(NEXT-VALUE(ToteChild), FILL("9", newToteType.NumNumeralsAfterPrefix)).
   ELSE
      RETURN newToteType.LabelPrefix + STRING(intToteID, FILL("9", newToteType.NumNumeralsAfterPrefix)).
   
END FUNCTION. /* fNewPackageRef */
