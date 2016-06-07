/*------------------------------------------------------------------------------------------------------------------------------------------
Program : filCsvWriteOffNotificationExport.p
Purpose : Core program to write the WriteOff Notification file for DayMen NL.
Author  : MC
Date    : 15th July 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Parameters */
DEFINE OUTPUT PARAMETER chrReturnError     AS CHARACTER   NO-UNDO.

/* Includes */
{defSessionVariables.i}
{fncGlobalFunctions.i}
{fncDataFunctions.i}
{fncDateFunctions.i}
{fncServerFunctions.i}
{fncStatusTypeFunctions.i}
{fncLockingFunctions.i}
{fncStockFunctions.i}

/* Shared Vars */
DEFINE SHARED VARIABLE intGblFileMasterID  AS INTEGER     NO-UNDO.
DEFINE SHARED VARIABLE intGblFileTypeID    AS INTEGER     NO-UNDO.
DEFINE SHARED VARIABLE chrGblFilePath      AS CHARACTER   NO-UNDO FORMAT "x(20)".

/* Shared Temp Tables */
DEFINE SHARED TEMP-TABLE ttFileExport      NO-UNDO 
   LIKE File.

DEFINE SHARED TEMP-TABLE ttFileExportError NO-UNDO 
   LIKE FileUploadError.

/* NO-UNDO Vars */
DEFINE VARIABLE chrOutputDirectory         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrRejectFile              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTempDirectory           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDirectoryError          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intCount                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrTextBody                AS CHARACTER   NO-UNDO.

/* UNDO vars */
DEFINE VARIABLE logCompletedOk             AS LOGICAL.

/* Temp Tables */
DEFINE TEMP-TABLE ttFileExportLine         NO-UNDO 
   LIKE FileLine.

/* Buffers */
/* 
DEFINE BUFFER updOutbound       FOR Outbound.
DEFINE BUFFER updShipPackage    FOR ShipPackage.
DEFINE BUFFER updShipOrder      FOR ShipOrder.
DEFINE BUFFER updShipOrderLine  FOR ShipOrderLine. */

DEFINE BUFFER updStockWriteOff FOR StockWriteOff.

/* Streams */
DEFINE STREAM sToCsvFile.


/* Set this for use in a Query */

/* Paths */
FIND FIRST FileMaster WHERE FileMaster.FileMasterID = intGblFileMasterID NO-LOCK NO-ERROR.
IF NOT AVAILABLE FileMaster THEN 
DO:
   chrReturnError = "No FileMaster exists for FileMasterID:" + STRING(intGblFileMasterID).
   LEAVE.
END.

FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR.
IF NOT AVAILABLE FileType THEN 
DO:
   chrReturnError = "No FileType exists for FileTypeID:" + STRING(FileMaster.FileTypeID).
   LEAVE.
END.

ASSIGN chrTempDirectory   = RIGHT-TRIM(FileMaster.FilePath, "/") + "/tmp/"
       chrOutputDirectory = RIGHT-TRIM(FileMaster.FilePath, "/") + "/".

chrDirectoryError = fValidWriteDirectory(chrTempDirectory) NO-ERROR.
IF chrDirectoryError <> "Ok" THEN
DO:
   chrReturnError = "Invalid Directory:'" + chrTempDirectory + "' " + (IF chrDirectoryError <> ? THEN chrDirectoryError ELSE "?").
   LEAVE.
END.

chrDirectoryError = fValidWriteDirectory(chrOutputDirectory) NO-ERROR.
IF chrDirectoryError <> "OK" THEN
DO:
   chrReturnError = "Invalid Directory:'" + chrTempDirectory + "' " + (IF chrDirectoryError <> ? THEN chrDirectoryError ELSE "?").
   LEAVE.
END.

IF NOT CAN-FIND(FIRST StockWriteOff NO-LOCK 
                   WHERE StockWriteOff.NotificationSent = "") THEN
DO:
   LEAVE.
END.

MainBlk:
DO:
   UpdateBlk:
   DO TRANSACTION ON ERROR UNDO, RETURN ERROR:
      
      OutboundLoop:
      FOR EACH StockWriteOff NO-LOCK /* idx=NotificationSentCreated */
         WHERE StockWriteOff.NotificationSent = "" ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
         
         CREATE ttFileExport.
         ASSIGN ttFileExport.Completed    = ""
                ttFileExport.Created      = fTimestamp(NOW)
                ttFileExport.FileID       = NEXT-VALUE(File)
                ttFileExport.FileMasterID = FileMaster.FileMasterID
                ttFileExport.FileName     = FileMaster.FilePrefix + STRING(StockWriteOff.StockWriteOffID) + "_" 
                                               + fTimestamp(NOW) + "." + FileType.Extension
                ttFileExport.FilePath     = chrTempDirectory + ttFileExport.FileName
                ttFileExport.FileTypeID   = FileType.FileTypeID
                ttFileExport.GateUserID   = intGblUserID
                ttFileExport.Sent         = "".
         
         OUTPUT STREAM sToCsvFile TO VALUE(ttFileExport.FilePath).
         
         PUT STREAM sToCsvFile UNFORMATTED "Package Type, Package Ref, Part, Quantity, Status, Operator,"
                                              + CHR(13) + CHR(10). /* SKIP isn't enough, need these */
         
         FOR EACH StockPackage OF StockWriteOff NO-LOCK:
            
            FIND FIRST StockEntity NO-LOCK
               WHERE StockEntity.StockEntityID = StockPackage.StockEntityID NO-ERROR.
            
            FIND Part OF StockPackage NO-LOCK NO-ERROR.
               
               chrTextBody = StockEntity.EntityName + "," 
                                     + StockPackage.PackageRef + "," 
                                     + Part.PartRef + "," 
                                     + STRING(StockPackage.PackageQty) + "," 
                                     + fGetStatusCode("Stock", StockPackage.StockStatusID) + "," 
                                     + fGetFieldValue("UserName", "GateUser", StockWriteOff.GateUserID) + ",".
               
               PUT STREAM sToCsvFile UNFORMATTED chrTextBody.
               PUT STREAM sToCsvFile UNFORMATTED CHR(13) + CHR(10). /* SKIP isn't enough need these */
               /* END. */
         END.
      
         OUTPUT STREAM sToCsvFile CLOSE.
         
         /* Update the Outbound now - if it doesn't work the error created here will prevent the file being moved to the outray */
         FIND updStockWriteOff EXCLUSIVE-LOCK WHERE ROWID(updStockWriteOff) = ROWID(StockWriteOff) NO-ERROR NO-WAIT.
         IF AVAILABLE updStockWriteOff THEN
         DO ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:

            updStockWriteOff.NotificationSent = fTimeStamp(NOW).
            
         END. /*IF AVAILABLE updOutbound THEN*/
         ELSE
         DO:
            CREATE ttFileExportError.
            ASSIGN ttFileExportError.FileUploadErrorID = NEXT-VALUE(FileUploadError)
                   ttFileExportError.FileID            = ttFileExport.FileID
                   ttFileExportError.ErrorString       = "WriteOff Record:" + STRING(updStockWriteOff.StockWriteOffID) + " is LOCKED. Undoing All.".
            UNDO UpdateBlk, LEAVE UpdateBlk.
         END.
         
      END. /* FOR EACH StockWriteOff NO-LOCK */
      
      logCompletedOk = TRUE.
      
   END. /*UpdateBlk: DO TRANSACTION ON ERROR UNDO, RETURN ERROR:*/
   
   IF logCompletedOk <> TRUE THEN
   DO:
      CREATE ttFileExportError.
      ASSIGN ttFileExportError.FileUploadErrorID = NEXT-VALUE(FileUploadError)
             ttFileExportError.FileID            = ttFileExport.FileID
             ttFileExportError.ErrorString       = "TRANSACTION NOT completed. Undoing All.".
   END.
   
END. /* MainBlk*/




