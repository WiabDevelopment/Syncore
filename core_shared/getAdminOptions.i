/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getAdminOptions.i
Purpose : This sets a group of variables that will be used for Admin both in Character and Web enviroments. Included in all programs 
          using Admin manipulation.
Author  : BG
Date    : 6th Nov 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
20.03.2014 MN  CR1049     Adding core. database prefix for Data Migrtion Tool 
04.03.2015 ND  Canontlb   Commented out code. File is not used in Canon at the moment. 
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on usrSession.i */

/* Will we be using to determine whether Relocate to Build locations will be required before performing Pallet Build */
DEFINE VARIABLE logUseBuildLocations AS LOGICAL NO-UNDO.

/* If the option is StockWriteOn */
FIND FIRST StockAdminOption NO-LOCK 
   WHERE StockAdminOption.OptionCode = "StockWriteOn" NO-ERROR.
IF NOT AVAILABLE StockAdminOption THEN
DO:
   chrError = chrError + "No StockAdminOption exist. Please create StockAdminOption record.".
END.
ELSE
DO:
   ASSIGN logUseBuildLocations = StockAdminOption.UseBuildLocations.
END.

/* If the option is StockSplit */
FIND FIRST StockAdminOption NO-LOCK 
   WHERE StockAdminOption.OptionCode = "StockSplit" NO-ERROR.
IF NOT AVAILABLE StockAdminOption THEN
DO:
   chrError = chrError + "No StockAdminOption exist. Please create StockAdminOption record.".
END.
ELSE
DO:
   ASSIGN logUseBuildLocations = StockAdminOption.UseBuildLocations.
END.

/* If the option is StockStatusChange */
FIND FIRST StockAdminOption NO-LOCK 
   WHERE StockAdminOption.OptionCode = "StockStatusChange" NO-ERROR.
IF NOT AVAILABLE StockAdminOption THEN
DO:
   chrError = chrError + "No StockAdminOption exist. Please create StockAdminOption record.".
END.
ELSE
DO:
   ASSIGN logUseBuildLocations = StockAdminOption.UseBuildLocations.
END.

/* If the option is StockMerge */
FIND FIRST StockAdminOption NO-LOCK 
   WHERE StockAdminOption.OptionCode = "StockMerge" NO-ERROR.
IF NOT AVAILABLE StockAdminOption THEN
DO:
   chrError = chrError + "No StockAdminOption exist. Please create StockAdminOption record.".
END.
ELSE
DO:
   ASSIGN logUseBuildLocations = StockAdminOption.UseBuildLocations.
END.

/* If the option is StockEntitySwap */
FIND FIRST StockAdminOption NO-LOCK 
   WHERE StockAdminOption.OptionCode = "StockEntitySwap" NO-ERROR.
IF NOT AVAILABLE StockAdminOption THEN
DO:
   chrError = chrError + "No StockAdminOption exist. Please create StockAdminOption record.".
END.
ELSE
DO:
   ASSIGN logUseBuildLocations = StockAdminOption.UseBuildLocations.
END.

/* If the option is StockAdjust */
FIND FIRST StockAdminOption NO-LOCK 
   WHERE StockAdminOption.OptionCode = "StockAdjust" NO-ERROR.
IF NOT AVAILABLE StockAdminOption THEN
DO:
   chrError = chrError + "No StockAdminOption exist. Please create StockAdminOption record.".
END.
ELSE
DO:
   ASSIGN logUseBuildLocations = StockAdminOption.UseBuildLocations.
END.

/* If the option is StockWriteOff */
FIND FIRST StockAdminOption NO-LOCK 
   WHERE StockAdminOption.OptionCode = "StockWriteOff" NO-ERROR.
IF NOT AVAILABLE StockAdminOption THEN
DO:
   chrError = chrError + "No StockAdminOption exist. Please create StockAdminOption record.".
END.
ELSE
DO:
   ASSIGN logUseBuildLocations = StockAdminOption.UseBuildLocations.
END.
