/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncStockFunctions.i
Purpose : All functions to do with Stock Packages and Stock Items and their recursive nature
Author  : BG
Date    : 24th July 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
01/11/2013 BG  Daymen     Moved fNewToteRef from here into fToteFunctions.i
------------------------------------------------------------------------------------------------------------------------------------------*/
/**
/* ttDef */
DEFINE TEMP-TABLE ttStockItem LIKE StockItem
   FIELD PartStatusQtyID      AS INTEGER
   FIELD PartID               LIKE Part.PartID
   FIELD AsnLineID            LIKE AsnLine.AsnLineID
   INDEX PartStatusQtyID      PartStatusQtyID
   INDEX PartID               PartID
   INDEX AsnLineID            AsnLineID.
**/
DEFINE TEMP-TABLE ttChildStockPackage 
   LIKE StockPackage.


FUNCTION fNewPackageRef RETURNS CHAR (INPUT intNewPackageID  AS INT,
                                      INPUT intStockEntityID AS INT):
   
   DEF BUFFER newStockEntity FOR StockEntity.
   
   FIND newStockEntity NO-LOCK
      WHERE newStockEntity.StockEntityID = intStockEntityID NO-ERROR.
   IF NOT AVAIL newStockEntity THEN
      RETURN "".
   
   RETURN newStockEntity.EntityPrefix + STRING(intNewPackageID, FILL("9", newStockEntity.NumNumeralsAfterPrefix)).
   
END FUNCTION. /* fNewPackageRef */

/**
PROCEDURE pGetAllChildrenStockItems:
   
   DEFINE INPUT PARAMETER intStockPackageID AS INTEGER NO-UNDO.
   
   DEFINE BUFFER childStockPackage   FOR StockPackage.
   DEFINE BUFFER childStockItem      FOR StockItem.
   
   /* Write a tt record for each Stock Item of this Package */
   FOR EACH childStockItem WHERE childStockItem.StockPackageID = intStockPackageID NO-LOCK:
      
      CREATE ttStockItem.
      BUFFER-COPY childStockItem TO ttStockItem.
   END.
   
   /* If its got children then run recursively again for each child in case they might also have Stock Items attached */
   FOR EACH childStockPackage NO-LOCK
      WHERE childStockPackage.ParentStockPackageID = intStockPackageID:
      
      IF CAN-FIND(FIRST StockItem OF childStockPackage NO-LOCK) OR 
         CAN-FIND(FIRST StockPackage WHERE StockPackage.ParentStockPackageID = childStockPackage.StockPackageID NO-LOCK) THEN
      DO:
         /* Run recursively for the children */
         RUN pGetAllChildrenStockItems (INPUT childStockPackage.StockPackageID).
      END.
   END. /*FOR EACH childStockPackage NO-LOCK*/
   
END PROCEDURE. /* pGetAllChildrenStockItems */
**/

PROCEDURE pGetAllChildrenStockPackages:
   
   DEFINE INPUT PARAMETER intStockPackageID AS INTEGER NO-UNDO.
   
   DEFINE BUFFER childStockPackage   FOR StockPackage.
   
   /* If its got children then run recursively again for each child in case they might also have children */
   FOR EACH childStockPackage NO-LOCK
      WHERE childStockPackage.ParentStockPackageID = intStockPackageID:
      
      /* Write a tt record for each StockPackage child of this StockPackage */
      CREATE ttChildStockPackage.
      BUFFER-COPY childStockPackage TO ttChildStockPackage.
      
      IF CAN-FIND(FIRST StockPackage WHERE StockPackage.ParentStockPackageID = childStockPackage.StockPackageID NO-LOCK) THEN
      DO:
         /* Run recursively for the children */
         RUN pGetAllChildrenStockPackages (INPUT childStockPackage.StockPackageID).
      END.
   END. /*FOR EACH childStockPackage NO-LOCK*/
   
END PROCEDURE. /* pGetAllChildrenStockPackages */

