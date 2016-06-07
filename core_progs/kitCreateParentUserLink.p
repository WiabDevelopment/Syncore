/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCreateParentBuildUserLink.p
Purpose : Creates a ParentBuildUserLink record for a new user
          
          Possible Results : Continue
          
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
   {fncStatusTypeFunctions.i}

   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}

   /* Session Objects */   
   DEFINE VARIABLE intSsnParentPackageID     AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnNewParentPackageRef AS sessionValue NO-UNDO.
   
   /* DB Objects */
   DEFINE VARIABLE newParentBuildUserLink AS newRecord.
   DEFINE VARIABLE newParentBuild         AS newRecord.
   
   /* Get Current Session Data */
   intSsnParentPackageID     = fGetSessionValue("ParentPackageID").
   chrSsnNewParentPackageRef = fGetSessionValue("NewParentPackageRef").
   
   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
   
      /* Check if its a new parent package created or an existing one being worked on */
      IF chrSsnNewParentPackageRef:chrValue = "" THEN 
      DO:
         
         FIND FIRST ParentBuild NO-LOCK 
            WHERE ParentBuild.StockPackageID = intSsnParentPackageID:intValue NO-ERROR.
         IF NOT AVAILABLE ParentBuild THEN 
         DO:
            RUN DisplayError("System error",
                             "ParentBuild record doesnot exist for PackageID: " + STRING(intSsnParentPackageID:intValue)).                
            LEAVE Main_Block.                 
         END.
        
         /* Create a new ParenUserLink record only if its a new user for same parent */   
         FIND FIRST ParentBuildUserLink NO-LOCK 
            WHERE ParentBuildUserLink.ParentBuildID = ParentBuild.ParentBuildID
            AND   ParentBuildUserLink.GateUserID    = intGblUserID NO-ERROR.
         IF NOT AVAILABLE ParentBuildUserLink THEN 
         DO:    
            /* Create ParentBuildUserLink */
            newParentBuildUserLink = fCreateRecord("ParentBuildUserLink").
            
            /* Assign Fields */
            newParentBuildUserLink:assignField("ParentBuildID", ParentBuild.ParentBuildID).
            newParentBuildUserLink:assignField("GateUserID", intGblUserID).
            newParentBuildUserLink:assignField("Created", fTimeStamp(NOW)).
            
            /* Save Errors */
            chrError = chrError + newParentBuildUserLink:getErrors().
         
            /* Error Check */
            IF chrError <> "" THEN
            DO:
               RUN DisplayMessage("Create Failed",
                  chrError).
               UNDO Main_Block, LEAVE Main_Block.
            END.            
            
         END. /* NOT AVAILABLE ParentBuildUserLink */  

      END. /* IF chrSsnNewParentPackageRef:chrValue = "" */
      ELSE 
      DO:
                    
         /* Create ParentBuild */
         newParentBuild = fCreateRecord("ParentBuild").
         
         /* Assign Fields */
         newParentBuild:assignField("StockStatusID", fGetStatusID("Stock", "UnLabelled")).
         newParentBuild:assignField("StockPackageID", intSsnParentPackageID:intValue).
         newParentBuild:assignField("GateUserID", intGblUserID).
         newParentBuild:assignField("Created", fTimeStamp(NOW)).
   
         /* Error Check */
         chrError = chrError + newParentBuild:getErrors().

         /* Create ParentBuildUserLink */
         newParentBuildUserLink = fCreateRecord("ParentBuildUserLink").
            
         /* Assign Fields */
         newParentBuildUserLink:assignField("ParentBuildID", newParentBuild:NewRecordUniqueID).
         newParentBuildUserLink:assignField("GateUserID", intGblUserID).
         newParentBuildUserLink:assignField("Created", fTimeStamp(NOW)).
            
         /* Save Errors */
         chrError = chrError + newParentBuildUserLink:getErrors().
         
         IF chrError <> "" THEN
         DO:
            RUN DisplayMessage("Update Failed",
                               chrError).
            
            UNDO Main_Block, LEAVE Main_Block.
         END. /* IF chrError <> "" */
         
      END. /*Working on newly created parent */
      
      chrResult = "Continue".
         
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT newParentBuildUserLink     NO-ERROR.  
   DELETE OBJECT intSsnParentPackageID      NO-ERROR.  
   DELETE OBJECT newParentBuild             NO-ERROR. 
   DELETE OBJECT chrSsnNewParentPackageRef  NO-ERROR. 
   
   /* Releases */
   RELEASE ParentBuildUserLink NO-ERROR.
   RELEASE ParentBuild         NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */
