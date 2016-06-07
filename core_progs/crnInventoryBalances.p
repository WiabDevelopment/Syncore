/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnInventoryBalances.p
Purpose : This program will run off the cron prob every minute. It takes StockUpdate records which are created in Db triggers when writing
          StockPackage records and uses them to create Inventory Balance records. Each InventoryBalance record replaces the previous one 
          for certain criteria BusinessUnit, Owner, Part & StockStatus.
Author  : BG
Date    : 22nd May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
08/07/2014 CS             Removed usrCreateSession.i to try to stop session and user mismatches since the include is in the CronWrapper.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncGlobalFunctions.i}   
{fncDateFunctions.i}
{fncLoggingFunctions.i}
{fncLockingFunctions.i}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}

/* Vars */
DEFINE VARIABLE chrTimeStamp           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLogFile             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLogFilePath         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDate&Time           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intPreviousBalance     AS INTEGER     NO-UNDO.

/* NO-UNDO Vars */                     
DEFINE VARIABLE intNewBalances         AS INTEGER.
DEFINE VARIABLE intPairsDeleted        AS INTEGER.
DEFINE VARIABLE logCompletedOk         AS LOGICAL.

/* Buffers */                          
DEFINE BUFFER newInventoryBalance      FOR InventoryBalance.
DEFINE BUFFER openInventoryBalance     FOR InventoryBalance.
DEFINE BUFFER updateStockUpdate        FOR StockUpdate.
DEFINE BUFFER updatePairedStockUpdate  FOR StockUpdate.

/* Streams */
DEFINE STREAM sToLogFile.

/* Set the log file destination directory  */
chrLogFilePath = fGetAgedDirectory("../logs/", 90).
IF chrLogFilePath BEGINS "Error" THEN
   chrLogFilePath = "../logs/".

ASSIGN chrTimeStamp = fTimestamp(NOW)
       chrLogFile   = "InventoryBalanceUpdates_" + fDisplayDate&Time(fTimestamp(NOW),"m_y") + ".log"
       chrLogFile   = chrLogFilePath + chrLogFile.

/* Do a straight output with a stream so this will catch all errors including unexpected Progress errors */
OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.

/* If we have any errors or locking issues we want to UNDO All */
TransBlk:
DO TRANSACTION ON ERROR UNDO, LEAVE:
   
   /* Cron login */
   FIND FIRST gate.GateUser NO-LOCK
      WHERE gate.GateUser.Username = "cron" NO-ERROR.
   
   /* Go through the unUpdated Updates in order of creation - need them to updateate the InventoryBalances in the cprrect sequence */
   UpdateBlk:
   FOR EACH StockUpdate NO-LOCK   /* Idx = UpdatedCreated*/
      WHERE StockUpdate.Updated = "" 
      BY    StockUpdate.Created ON ERROR UNDO UpdateBlk, NEXT UpdateBlk:
      
      FIND FIRST updateStockUpdate EXCLUSIVE-LOCK 
         WHERE ROWID(updateStockUpdate) = ROWID(StockUpdate) NO-ERROR NO-WAIT.
      IF NOT AVAILABLE updateStockUpdate THEN
      DO ON ERROR UNDO UpdateBlk, NEXT UpdateBlk:
         /* Write to log file here with Locking issue */
         PUT STREAM sToLogFile UNFORMATTED NOW " " + fGetLockingInfo(INPUT INTEGER(RECID(StockUpdate)), 
                                                   INPUT "StockUpdate",
                                                   INPUT STRING(StockUpdate.StockUpdateID)) SKIP.
         UNDO TransBlk, LEAVE TransBlk.
      END. /*IF NOT AVAILABLE updateStockUpdate THEN*/
      
      /* See if there's a paired entry for exactly the reverse amount and if so delete them both */
      FIND FIRST updatePairedStockUpdate EXCLUSIVE-LOCK /*UpdatedPartStatusOwnerBusUnit*/
         WHERE updatePairedStockUpdate.Updated        =  ""
         AND   updatePairedStockUpdate.PartID         =  updateStockUpdate.PartID        
         AND   updatePairedStockUpdate.StockStatusID  =  updateStockUpdate.StockStatusID 
         AND   updatePairedStockUpdate.OwnerID        =  updateStockUpdate.OwnerID       
         AND   updatePairedStockUpdate.BusinessUnitID =  updateStockUpdate.BusinessUnitID 
         AND   updatePairedStockUpdate.QtyChanged     =  updateStockUpdate.QtyChanged * -1
         AND   ROWID(updatePairedStockUpdate)         <> ROWID(updateStockUpdate)  NO-ERROR NO-WAIT.
      IF AVAILABLE updatePairedStockUpdate THEN
      DO ON ERROR UNDO UpdateBlk, NEXT UpdateBlk:
         
         DELETE updatePairedStockUpdate.
         DELETE updateStockUpdate.
         intPairsDeleted = intPairsDeleted + 1.
         NEXT UpdateBlk.
      END.
      
      RELEASE InventoryBalance      NO-ERROR.
      RELEASE openInventoryBalance  NO-ERROR.
      
      /* Try to find previous Open Stock Balance */
      FIND FIRST InventoryBalance NO-LOCK        /* Idx = PartOwnerStatusBusUnitSuperceded */
         WHERE InventoryBalance.PartID         = updateStockUpdate.PartID
         AND   InventoryBalance.OwnerID        = updateStockUpdate.OwnerID
         AND   InventoryBalance.StockStatusID  = updateStockUpdate.StockStatusID
         AND   InventoryBalance.BusinessUnitID = updateStockUpdate.BusinessUnitID
         AND   InventoryBalance.Superceded     = "" NO-ERROR.
      
      IF AVAILABLE InventoryBalance THEN
      DO ON ERROR UNDO UpdateBlk, NEXT UpdateBlk:
         
         FIND FIRST openInventoryBalance EXCLUSIVE-LOCK
            WHERE ROWID(openInventoryBalance) = ROWID(InventoryBalance) NO-ERROR NO-WAIT.
         /* Only do updateate if we can get all receords exclusively - if not it will be picked up next time around */
         IF NOT AVAILABLE openInventoryBalance THEN
            UNDO UpdateBlk, NEXT UpdateBlk.
      END.
      
      IF AVAILABLE openInventoryBalance THEN
         intPreviousBalance = openInventoryBalance.Balance.
      ELSE
         intPreviousBalance = 0.
      
      /* Create a new Stock Balance record for the Upate whether a prev one exists or not */
      CREATE newInventoryBalance.
      ASSIGN newInventoryBalance.InventoryBalanceID = NEXT-VALUE(InventoryBalance) 
             newInventoryBalance.StockUpdateID      = updateStockUpdate.StockUpdateID /*Store what Update created each Balance for reports*/
             newInventoryBalance.Balance            = intPreviousBalance + updateStockUpdate.QtyChanged
             newInventoryBalance.BusinessUnitID     = updateStockUpdate.BusinessUnitID
             newInventoryBalance.OwnerID            = updateStockUpdate.OwnerID
             newInventoryBalance.PartID             = updateStockUpdate.PartID
             newInventoryBalance.StockStatusID      = updateStockUpdate.StockStatusID
             newInventoryBalance.Created            = updateStockUpdate.Created /*Take timestamp frm StockUpdate when change really happen*/
             updateStockUpdate.Updated              = chrTimeStamp              /*Store actual current timestamp on this for sequencing   */
             intNewBalances                         = intNewBalances + 1.
      
      /* One should replace the next simultaneously */
      IF AVAILABLE openInventoryBalance THEN 
         openInventoryBalance.Superceded = newInventoryBalance.Created.
      
      RELEASE newInventoryBalance   NO-ERROR.
      RELEASE openInventoryBalance  NO-ERROR.
      RELEASE updateStockUpdate     NO-ERROR.
      
   END. /*UpdateBlk: FOR EACH StockUpdate NO-LOCK...ON ERROR UNDO, RETURN ERROR:*/
   
   logCompletedOk = TRUE.
   
END. /*TransBlk: DO TRANS ON ERROR UNDO, LEAVE:*/

IF logCompletedOk = TRUE THEN
DO:
   IF intPairsDeleted > 0 THEN
      PUT STREAM sToLogFile UNFORMATTED NOW " " + TRIM(STRING(intPairsDeleted)) + " identical pairs deleted." SKIP.
   PUT STREAM sToLogFile UNFORMATTED NOW " " + TRIM(STRING(intNewBalances)) + " successfully updateated." SKIP.
END.
ELSE
   PUT STREAM sToLogFile UNFORMATTED NOW " " + "Error in Transaction, Undoing All." SKIP.

/* Close default stream */
OUTPUT STREAM sToLogFile CLOSE.
