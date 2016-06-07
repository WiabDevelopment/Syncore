/* Parameters */
DEFINE OUTPUT PARAMETER chrReturnError     AS CHARACTER   NO-UNDO.
/* Includes */
{defSessionVariables.i}
{fncGlobalFunctions.i}
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
DEFINE TEMP-TABLE ttInvBal LIKE InventoryBalance 
   INDEX idxStockPackage PartID StockStatusID OwnerID BusinessUnitID.   


DEFINE SHARED TEMP-TABLE ttFileExportError NO-UNDO 
   LIKE FileUploadError.
   
/* NO-UNDO Vars */
DEFINE VARIABLE chrOutputDirectory         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrRejectFile              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTempDirectory           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDirectoryError          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrErrorDetails            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logIsData                  AS LOGICAL     NO-UNDO INITIAL FALSE. 

/* UNDO vars */
DEFINE VARIABLE logCompletedOk             AS LOGICAL. 

/* Temp Tables */
DEFINE TEMP-TABLE ttFileExportLine         NO-UNDO 
   LIKE FileLine.
   
/* Streams */
DEFINE STREAM sToCsvFile.

  

FIND FIRST GateUser NO-LOCK 
   WHERE GateUser.Username = "WebServ" NO-ERROR.
IF NOT AVAILABLE GateUser THEN 
   chrErrorDetails = "GateUser with Username: WebServ does not exist!".
   
{usrCreateSession.i} 

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

CREATE ttFileExport.
ASSIGN ttFileExport.Completed    = ""
       ttFileExport.Created      = fTimestamp(NOW)
       ttFileExport.FileID       = NEXT-VALUE(File)
       ttFileExport.FileMasterID = FileMaster.FileMasterID
       ttFileExport.FileName     = FileMaster.FilePrefix + fTimestamp(NOW) + "." + FileType.Extension
       ttFileExport.FilePath     = chrTempDirectory + ttFileExport.FileName
       ttFileExport.FileTypeID   = FileType.FileTypeID
       ttFileExport.GateUserID   = intGblUserID
       ttFileExport.Sent         = "".
       
OUTPUT STREAM sToCsvFile TO VALUE(ttFileExport.FilePath).

    
FOR EACH StockPackage WHERE Detrashed = ""  
   AND StockPackage.ParentStockPackageID = 0 NO-LOCK:
   
   FIND FIRST ttInvBal 
       WHERE ttInvBal.PartID         = StockPackage.PartID 
         AND ttInvBal.StockStatusID  = StockPackage.StockStatusID 
         AND ttInvBal.OwnerID        = StockPackage.OwnerID        
         AND ttInvBal.BusinessUnitID = StockPackage.BusinessUnitID NO-ERROR.
   IF NOT AVAILABLE ttInvBal THEN 
   DO:
      CREATE ttInvBal.
      ASSIGN ttInvBal.PartID         = StockPackage.PartID 
             ttInvBal.StockStatusID  = StockPackage.StockStatusID 
             ttInvBal.OwnerID        = StockPackage.OwnerID        
             ttInvBal.BusinessUnitID = StockPackage.BusinessUnitID.
   END.
    
   ttInvBal.Balance = ttInvBal.Balance + StockPackage.PackageQty.
    
END.

PUT STREAM sToCsvFile UNFORMATTED  "InventoryBalanceID, PartRef, Balance, Business Unit, Owner, Stock Status, " +
                                      "StockUpdateID, Created, Superceded" + CHR(13). /* SKIP isn't enough need these */

FOR EACH ttInvBal /*where ttInvBal.Balance > 0*/:
   FIND LAST InventoryBalance NO-LOCK 
      WHERE InventoryBalance.PartID         = ttInvBal.PartID 
      AND   InventoryBalance.StockStatusID  = ttInvBal.StockStatusID 
      AND   InventoryBalance.OwnerID        = ttInvBal.OwnerID        
      AND   InventoryBalance.BusinessUnitID = ttInvBal.BusinessUnitID 
      AND   InventoryBalance.Balance        = ttInvBal.Balance NO-ERROR.
   IF NOT AVAILABLE InventoryBalance THEN
   DO:
      logIsData = TRUE.
      FIND FIRST Part OF ttInvBal NO-LOCK NO-ERROR.
      FIND FIRST BusinessUnit OF ttInvBal NO-LOCK NO-ERROR.
      FIND FIRST Owner OF ttInvBal NO-LOCK NO-ERROR.
      FIND FIRST StockStatus OF ttInvBal NO-LOCK NO-ERROR.

      PUT STREAM sToCsvFile UNFORMATTED STRING(ttInvBal.InventoryBalanceID) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE Part THEN Part.PartRef ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(ttInvBal.Balance) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE BusinessUnit THEN BusinessUnit.UnitName ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE Owner THEN Owner.OwnerName ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE StockStatus THEN StockStatus.StatusName ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(ttInvBal.StockUpdateID) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(ttInvBal.Created) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(ttInvBal.Superceded) +  CHR(13).
   END.
END.

PUT STREAM sToCsvFile UNFORMATTED CHR(13).

FOR EACH InventoryBalance NO-LOCK
   WHERE InventoryBalance.Superceded = "",
      EACH Part OF InventoryBalance NO-LOCK,
         EACH  StockStatus OF InventoryBalance NO-LOCK
         BY    Part.PartRef 
         BY    StockStatus.ListingSequence:
         
         IF InventoryBalance.Balance = 0 THEN NEXT.
         
   FIND FIRST ttInvBal 
       WHERE ttInvBal.PartID         = InventoryBalance.PartID 
       AND   ttInvBal.StockStatusID  = InventoryBalance.StockStatusID 
       AND   ttInvBal.OwnerID        = InventoryBalance.OwnerID        
       AND   ttInvBal.BusinessUnitID = InventoryBalance.BusinessUnitID NO-ERROR.
   IF NOT AVAILABLE ttInvBal THEN
   DO:
      logIsData = TRUE.
      FIND FIRST BusinessUnit OF InventoryBalance NO-LOCK NO-ERROR.
      FIND FIRST Owner OF InventoryBalance NO-LOCK NO-ERROR.
      PUT STREAM sToCsvFile UNFORMATTED STRING(InventoryBalance.InventoryBalanceID) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE Part THEN Part.PartRef ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(InventoryBalance.Balance) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE BusinessUnit THEN BusinessUnit.UnitName ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE Owner THEN Owner.OwnerName ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(IF AVAILABLE StockStatus THEN StockStatus.StatusName ELSE "") + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(InventoryBalance.StockUpdateID) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(InventoryBalance.Created) + ",".
      PUT STREAM sToCsvFile UNFORMATTED STRING(InventoryBalance.Superceded) +  CHR(13).
   END.
END.         

OUTPUT STREAM sToCsvFile CLOSE.

IF logIsData = TRUE THEN
DO:      
   RUN osSendMail.p (INPUT "",                 /* Optional list of Users */
               INPUT "Check Inventory Balances",    /* Email Subject */
               INPUT "",                                /* Plain text message Body */
               INPUT "",                                /* Html format message Body */
               INPUT ttFileExport.FilePath,             /* File path ../files/file */
               INPUT FileMaster.InternalEmailGroupID,   /* EmailGroupID that you want to send this to */
               INPUT FileMaster.FileMasterID).          /* File MasterID is it applies */
             /*End Send Email with E2File per NF*/   
END.

   
