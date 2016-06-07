/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncHistoryFunctions.i
Purpose : Functions to make searching through history records easier and more readble
Author  : BG
Date    : 15th July 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
02.09.13   MN              adding fGetPreviousShipPalletLocationID to get a last location from pallet's history
------------------------------------------------------------------------------------------------------------------------------------------*/

FUNCTION fGetPreviousShipOrderStatusID RETURNS INTEGER (INPUT intShipOrderID     AS INTEGER,
                                                        INPUT intCurrentStatusID AS INTEGER):
   
   HistoryLoop:
   FOR EACH ShipOrderHistory NO-LOCK
      WHERE ShipOrderHistory.ShipOrderID = intShipOrderID
      BY    ShipOrderHistory.ShipOrderHistoryID DESC:
      
      IF ShipOrderHistory.ShipOrderStatusID <> intCurrentStatusID AND ShipOrderHistory.ShipOrderStatusID <> 0 THEN
      DO:
         RETURN ShipOrderHistory.ShipOrderHistoryID.
      END.
   END.
   
   RETURN 0.
   
END FUNCTION. /*fGetStatusID*/


FUNCTION fGetPreviousToteID RETURNS INTEGER (INPUT intStockPackageID AS INTEGER,
                                             INPUT intCurrentToteID  AS INTEGER):
   
   HistoryLoop:
   FOR EACH StockHistory NO-LOCK
      WHERE StockHistory.StockPackageID = intStockPackageID
      BY    StockHistory.StockHistoryID DESC:
      
      IF StockHistory.ToteID <> intCurrentToteID AND StockHistory.ToteID <> 0 THEN
      DO:
         RETURN StockHistory.ToteID.
      END.
   END.
   
   RETURN 0.
   
END FUNCTION. /*fGetToteID*/


FUNCTION fGetPreviousShipPackageLocationID RETURNS INTEGER (INPUT intShipPackageID     AS INTEGER,
                                                            INPUT intCurrentLocationID AS INTEGER,
                                                            INPUT intLocationTypeID    AS INTEGER):
   
   DEFINE VARIABLE logSearchHasBegun  AS LOGICAL     NO-UNDO.
   
   DEFINE BUFFER   readLocation       FOR Location.
   
   HistoryLoop:
   FOR EACH ShipPackageHistory NO-LOCK
      WHERE ShipPackageHistory.ShipPackageID = intShipPackageID
      BY    ShipPackageHistory.ShipPackageHistoryID DESC:
      
      IF ShipPackageHistory.LocationID = 0 THEN
         NEXT HistoryLoop.
      
      /* We want to look backwards from the sent in Location so ignore all history thereafter */
      IF ShipPackageHistory.LocationID = intCurrentLocationID THEN
      DO:
         logSearchHasBegun = TRUE.
         NEXT HistoryLoop.
      END.
      
      /* We want to look backwards from the sent in Location so ignore all history thereafter */
      IF logSearchHasBegun = FALSE AND ShipPackageHistory.LocationID <> intCurrentLocationID THEN
         NEXT HistoryLoop.
      
      /* If they've sent in a Location Type then make sure it matches */
      IF intLocationTypeID <> 0 THEN
      DO:
         FIND readLocation OF ShipPackageHistory NO-LOCK NO-ERROR.
         IF AVAILABLE readLocation AND readLocation.LocationTypeID <> intLocationTypeID THEN
            NEXT HistoryLoop.
      END.
      
      /* If it gets to here then it's non zero, matches LocationType if any, isn't = current Location and search has begun */
      RETURN ShipPackageHistory.LocationID.
      
   END. /*FOR EACH ShipPackageHistory NO-LOCK*/
   
   RETURN 0.
   
END FUNCTION. /*fGetPreviousShipPackageLocationID*/


FUNCTION fGetPreviousShipPalletLocationID RETURNS INTEGER (INPUT intShipPalletID      AS INTEGER,
                                                           INPUT intCurrentLocationID AS INTEGER,
                                                           INPUT intLocationTypeID    AS INTEGER):
   
   DEFINE VARIABLE logSearchHasBegun  AS LOGICAL NO-UNDO.
   
   DEFINE BUFFER   readLocation       FOR Location.
   
   HistoryLoop:
   FOR EACH ShipPalletHistory NO-LOCK
      WHERE ShipPalletHistory.ShipPalletID = intShipPalletID
      BY    ShipPalletHistory.ShipPalletHistoryID DESC:
      
      IF ShipPalletHistory.LocationID = 0 THEN
         NEXT HistoryLoop.

      /* We want to look backwards from the sent in Location so ignore all history thereafter */
      IF ShipPalletHistory.LocationID = intCurrentLocationID THEN
      DO:
         logSearchHasBegun = TRUE.
         NEXT HistoryLoop.
      END.

      /* We want to look backwards from the sent in Location so ignore all history thereafter */
      IF logSearchHasBegun = FALSE AND ShipPalletHistory.LocationID <> intCurrentLocationID THEN
         NEXT HistoryLoop.
      
      /* If they've sent in a Location Type then make sure it matches */
      IF intLocationTypeID <> 0 THEN
      DO:
         FIND readLocation OF ShipPalletHistory NO-LOCK NO-ERROR.
         IF AVAILABLE readLocation AND readLocation.LocationTypeID <> intLocationTypeID THEN
            NEXT HistoryLoop.
      END.
      
      /* If it gets to here then it's non zero, matches LocationType if any, isn't = current Location and search has begun */
      RETURN ShipPalletHistory.LocationID.
      
   END. /*FOR EACH ShipPalletHistory NO-LOCK*/
   
   RETURN 0.
   
END FUNCTION. /*fGetPreviousShipPalletLocationID */
