/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsRunSplitStockPackage.p 
Purpose : Starts the Splitting process

          Possible results: Continue
          
Author  : MC
Date    : 29nd Nov 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
06/06/2014 BR  All        CR 1052 Standardization Project
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}
   
   /* Optional Includes */
   {fncStringFunctions.i}
   /* fCenterOnScreen */

   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Clear the Session */
   fClearSessionValues("").
   
   /* Temp Tables */
   DEFINE NEW GLOBAL SHARED TEMP-TABLE ttSerialScan LIKE SerialScan.
   
   EMPTY TEMP-TABLE ttSerialScan.
   
   /* Session Objects */
   DEFINE VARIABLE chrSsnScanUserPrompt             AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnVerifyUserPrompt           AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnOperationTypeForSerialScan AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnSerialCreatedCount         AS sessionValue.
   DEFINE VARIABLE logSsnTempLabel                  AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnLabelName                  AS sessionValue NO-UNDO.
   
   /* Buffers */
   DEFINE BUFFER receivingOperationType FOR OperationType.   
   
   /* Set New Data */
   chrSsnScanUserPrompt             = fNewSessionValue("StockPackageUserPrompt").
   chrSsnVerifyUserPrompt           = fNewSessionValue("VerifyStockPackageUserPrompt").
   intSsnOperationTypeForSerialScan = fNewSessionValue("OperationTypeForSerialScan").  
   intSsnSerialCreatedCount         = fNewSessionValue("SerialCreatedCount").   
   logSsnTempLabel                  = fNewSessionValue("TempLabel").
   chrSsnLabelName                  = fNewSessionValue("LabelName").
   
   /* Set Session Data */
   chrSsnScanUserPrompt:setValue(fCenterOnScreen("Scan Package to Split")).
   chrSsnVerifyUserPrompt:setValue(fCenterOnScreen("Scan New Package Label")).
   logSsnTempLabel:setValue(FALSE).
   chrSsnLabelName:setValue("StockPackage").
   
   /* Get the Operation Type for the Process */
   FIND OperationType NO-LOCK
      WHERE OperationType.TypeCode = "Split" NO-ERROR.
   IF NOT AVAILABLE OperationType THEN
   DO:
      RUN DisplayError("Record Not Found",
                       "[Operation Type] [Split] does not Exist.").
      LEAVE.
   END.
   
   intGblOperationTypeID = OperationType.OperationTypeID.
   
   Main_Block:
   DO ON ERROR UNDO:
      
      /* Find Groups */
      FIND FIRST LocationTypeGroup NO-LOCK
         WHERE LocationTypeGroup.GroupCode = "SplitLocations" NO-ERROR.
      IF NOT AVAILABLE LocationTypeGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[LocationType Group] [SplitLocations] does not exist.").
         LEAVE.
      END.
      
      FIND FIRST StockStatusGroup NO-LOCK
         WHERE StockStatusGroup.GroupCode = "SplitableStatuses" NO-ERROR.
      IF NOT AVAILABLE StockStatusGroup THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Stock Status Group] [SplitableStatuses] does not exist.").
         LEAVE.
      END.
      
      FIND FIRST receivingOperationType NO-LOCK 
         WHERE receivingOperationType.TypeCode = "Receiving" NO-ERROR.
      IF NOT AVAILABLE receivingOperationType THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[OperationType] [Receiving] does not exist.").
         LEAVE.
      END.
      
      intSsnOperationTypeForSerialScan:setValue(receivingOperationType.OperationTypeID).
      
      chrResult = "CONTINUE".
      
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT chrSsnScanUserPrompt   NO-ERROR.
   DELETE OBJECT chrSsnVerifyUserPrompt NO-ERROR.
   DELETE OBJECT logSsnTempLabel        NO-ERROR.
   DELETE OBJECT chrSsnLabelName        NO-ERROR.
   
   /* Releases */
   RELEASE OperationType     NO-ERROR.
   RELEASE LocationTypeGroup NO-ERROR.
   RELEASE StockStatusGroup  NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */
