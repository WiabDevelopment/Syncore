/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckAnyOpenParentForUser.p
Purpose : Checks if a Open ParentBuild exists for user

          Possible results : Yes, No.

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
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Session Objects */                      
   
   DEFINE VARIABLE chrSsnTitle             AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnQuestion          AS sessionValue NO-UNDO.
   
   /* Buffers */
   DEFINE BUFFER childStockPackage  FOR StockPackage. 
   DEFINE BUFFER parentStockPackage FOR StockPackage. 

   /* Clear Session */
   fClearSessionValue("QuestionTitle").
   fClearSessionValue("Question").
   fClearSessionValue("BuildLocationID").
   fClearSessionValue("PackageID").

   /* Set New Data */
   chrSsnTitle             = fNewSessionValue("QuestionTitle").
   chrSsnQuestion          = fNewSessionValue("Question").
        
   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:

      /* Get the open ParentBuild for user */   
      FOR EACH ParentBuild NO-LOCK 
         WHERE ParentBuild.Completed = "":

         FIND FIRST parentStockPackage NO-LOCK 
            WHERE parentStockPackage.StockPackageID =  ParentBuild.StockPackageID NO-ERROR.
         IF NOT AVAILABLE parentStockPackage THEN 
         DO:
            RUN DisplayError("Record Not Found",
                             "ParentPackage ID " + STRING(ParentBuild.StockPackageID) + " does not Exist.").      
            LEAVE Main_Block.
         END. /* NOT AVAILABLE parentStockPackage */
                        
         FIND FIRST ParentBuildUserLink NO-LOCK 
            WHERE ParentBuildUserLink.ParentBuildID = ParentBuild.ParentBuildID
            AND   ParentBuildUserLink.Completed = "" NO-ERROR.
         IF AVAILABLE ParentBuildUserLink THEN 
         DO:
            /* Check Parent has children */
            FIND FIRST childStockPackage NO-LOCK
               WHERE childStockPackage.ParentStockPackageID = ParentBuild.StockPackageID NO-ERROR.
            IF AVAILABLE childStockPackage THEN
            DO:                          
               /* Set session values for pcsAskQuestion.p*/
               chrSsnTitle:setValue("Close parent").
               chrSsnQuestion:setValue("Open parents exist. Do you wish to close one of them ?").
                    
               chrResult = "Yes".
               
               LEAVE Main_Block.
            END. /* IF NOT AVAILABLE childStockPackage */  
         END. /* IF AVAILABLE ParentBuildUserLink  */  
      END. /* FOR EACH ParentBuild */
      
      /* No Open Parent found */
      chrResult = "No". 

   END. /* Main_Block */

   /* Clean Up */
   DELETE OBJECT chrSsnTitle             NO-ERROR.
   DELETE OBJECT chrSsnQuestion          NO-ERROR.

   /* Releases */
   RELEASE ParentBuild         NO-ERROR.
   RELEASE childStockPackage   NO-ERROR.
   RELEASE parentStockPackage  NO-ERROR.
   RELEASE ParentBuildUserLink NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
