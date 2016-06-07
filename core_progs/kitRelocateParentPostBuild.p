/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitRelocateParentPostBuild.p
Purpose : Relocate the parent to PostBuild location.
          
          Possible results: Continue
          
Author  : BR
Date    : 21st August 2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
    /* Optional Includes */
   {fncStatusTypeFunctions.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i} 
   
   /* Session Objects */      
   DEFINE VARIABLE intSsnParentPackageID AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnMessageTitle    AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnMessage         AS sessionValue NO-UNDO.
   
   /* Db ojects */
   DEFINE VARIABLE updParentStockPackage AS updRecord    NO-UNDO.
   DEFINE VARIABLE updChildStockPackage  AS updRecord    NO-UNDO.
   DEFINE VARIABLE updParentBuild        AS updRecord    NO-UNDO.
    
   /* Buffers */
   DEFINE BUFFER parentStockPackage FOR StockPackage.
   DEFINE BUFFER childStockPackage  FOR StockPackage.
   
   /* Clear Session Values */
   fClearSessionValue("MessageTitle").
   fClearSessionValue("Message").
   
   /* Set New Data */
   chrSsnMessageTitle = fNewSessionValue("MessageTitle").
   chrSsnMessage      = fNewSessionValue("Message").

   /* Get Current Session Data */
   intSsnParentPackageID = fGetSessionValue("ParentPackageID").
  
   Main_Block:
   DO ON ERROR UNDO:
      
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "No Kitting Config Record found").
         LEAVE Main_Block. 
      END.
      
      FIND FIRST ParentBuild NO-LOCK 
         WHERE ParentBuild.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE ParentBuild THEN 
      DO:
         RUN DisplayError("System error",
                          "ParentBuild record doesnot exist for PackageID: " + STRING(intSsnParentPackageID:intValue)).                
         LEAVE Main_Block.                 
      END.
      
      updParentBuild = fGetRecord("ParentBuild", ParentBuild.ParentBuildID).
      
      /* Check for WebLocks */
      IF updParentBuild:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updParentBuild:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      /* Update ParentBuild record */
      updParentBuild:assignField("StockStatusID", KittingConfig.PostPalletBuildStockStatusID).
      
       /* Error Check for updParentStockPackage */
      chrError = chrError + updParentBuild:getErrors(). 
      
      FIND FIRST parentStockPackage NO-LOCK 
         WHERE parentStockPackage.StockPackageID     = intSsnParentPackageID:intValue 
         AND parentStockPackage.ParentStockPackageID = 0 NO-ERROR. 
      IF NOT AVAILABLE parentStockPackage THEN 
      DO:
         RUN DisplayError("Record Not Found",
                          "Parent PackageID: " + STRING(intSsnParentPackageID:intValue) +  " does not exist.").
         LEAVE Main_Block.
      END.
      
      /* Set the location of the parent to PostBuildLocation & Status as PostBuildStatus from Kitting config */  
      updParentStockPackage = fGetRecord("StockPackage", ParentStockPackage.StockPackageID).
      
      /* Check for WebLocks */
      IF updParentStockPackage:RecordLocked THEN
      DO:
         RUN DisplayError("Record Locked",
                           updParentStockPackage:getErrors()).
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      updParentStockPackage:assignField("LocationID", KittingConfig.PostPalletBuildLocationID). 
      updParentStockPackage:assignField("StockStatusID", KittingConfig.PostPalletBuildStockStatusID). 
   
      /* Error Check for updParentStockPackage */
      chrError = chrError + updParentStockPackage:getErrors(). 

      FOR EACH  childStockPackage NO-LOCK 
         WHERE  childStockPackage.ParentStockpackageID =  parentStockPackage.StockPackageID
         AND    childStockPackage.ParentStockpackageID <> 0 :
            
         /* Set the location of the child to PostBuildLocation & Status as PostBuildStatus from Kitting config*/  
         updChildStockPackage = fGetRecord("StockPackage", childStockPackage.StockPackageID).
         
         /* Check for WebLocks */
         IF updChildStockPackage:RecordLocked THEN
         DO:
            RUN DisplayError("Record Locked",
                              updChildStockPackage:getErrors()).
            UNDO Main_Block, LEAVE Main_Block.
         END.
         
         updChildStockPackage:assignField("LocationID", KittingConfig.PostPalletBuildLocationID).
         updChildStockPackage:assignField("StockStatusID", KittingConfig.PostPalletBuildStockStatusID). 
         
         /* Error Check for updChildStockPackage */
         chrError = chrError + updChildStockPackage:getErrors(). 
  
      END. /* FOR EACH childStockPackage */
      
      /* Check Errors */
      IF chrError <> "" THEN 
      DO:
         RUN DisplayError("Update Error",
                           chrError).       
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      chrSsnMessageTitle:setValue("Parent Closed").
      chrSsnMessage:setValue("Parent Package: " + parentStockPackage.PackageRef + " has been closed").  
      
      chrResult = "Continue".
      
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnParentPackageID NO-ERROR.
   DELETE OBJECT updParentStockPackage NO-ERROR.
   DELETE OBJECT updChildStockPackage  NO-ERROR.
   DELETE OBJECT updParentBuild        NO-ERROR. 
   DELETE OBJECT chrSsnMessageTitle    NO-ERROR.
   DELETE OBJECT chrSsnMessage         NO-ERROR.
   
   /* Releases */
   RELEASE ChildStockPackage  NO-ERROR.
   RELEASE ParentStockPackage NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END.  /* CTRL-C Catch */
