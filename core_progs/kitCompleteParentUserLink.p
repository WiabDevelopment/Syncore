/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCompleteParentBuildUserLink.p
Purpose : Mark the ParentBuild and ParentBuildUserLink records as complete for the parent.
          
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
   
   /* Optional Includes */
   {fncDateFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i} 
   
   /* Session Objects */      
   DEFINE VARIABLE intSsnParentPackageID  AS sessionValue NO-UNDO.
   DEFINE VARIABLE updParentBuild         AS updRecord   NO-UNDO.
   DEFINE VARIABLE updParentBuildUserLink AS updRecord   NO-UNDO.
   
   /* Get Current Session Data */
   intSsnParentPackageID = fGetSessionValue("ParentPackageID").
  
   Main_Block:
   DO ON ERROR UNDO:
      
      FIND FIRST ParentBuild NO-LOCK 
         WHERE ParentBuild.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
      IF NOT AVAILABLE ParentBuild THEN 
      DO:
         RUN DisplayError("System error",
                          "ParentBuild record doesnot exist for PackageID: " + STRING(intSsnParentPackageID:intValue)).                
         LEAVE Main_Block.                 
      END.
      
      /* Mark ParentBuild Record as complete */
      updParentBuild = fGetRecord("ParentBuild", ParentBuild.ParentBuildID).
      updParentBuild:assignField("Completed", fTimeStamp(NOW)).
      
      /* Error Check for updParentBuild */
      chrError = chrError + updParentBuild:getErrors(). 
      
      FOR EACH ParentBuildUserLink NO-LOCK 
         WHERE ParentBuildUserLink.ParentBuildID = ParentBuild.ParentBuildID:
         
         /* Mark ParentBuildUserLink Record as complete */   
         updParentBuildUserLink = fGetRecord("ParentBuildUserLink", ParentBuildUserLink.ParentBuildUserLinkID).
         updParentBuildUserLink:assignField("Completed", fTimeStamp(NOW)).   
         
         /* Error Check for updParentBuild */
         chrError = chrError + updParentBuildUserLink:getErrors(). 
    
      END. /* FOR EACH ParentBuildUserLink */
      
      /* Check Errors */
      IF chrError <> "" THEN 
      DO:
         RUN DisplayError("Update Error",
                          chrError).       
         UNDO Main_Block, LEAVE Main_Block.
      END.
      
      chrResult = "Continue".

   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnParentPackageID   NO-ERROR.
   DELETE OBJECT updParentBuild          NO-ERROR.
   DELETE OBJECT updParentBuildUserLink  NO-ERROR.
   
   /* Releases */
   RELEASE ParentBuild         NO-ERROR.
   RELEASE ParentBuildUserLink NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END.  /* CTRL-C Catch */
