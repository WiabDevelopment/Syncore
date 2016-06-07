/*------------------------------------------------------------------------------------------------------------------------------------------
Program : kitCheckParentMultiOperator.p 
Purpose : Checks whether PalletBuild can be multi operator
Possible results: Yes/No
Author  : BR
Date    : 18th Aug 2015
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
   DEFINE VARIABLE chrSsnTitle    AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnQuestion AS sessionValue NO-UNDO.
   
   /* Clear Session Values */
   fClearSessionValue("QuestionTitle").
   fClearSessionValue("Question").
   
   /* Create New Session Data */
   chrSsnTitle    = fNewSessionValue("QuestionTitle").
   chrSsnQuestion = fNewSessionValue("Question").
    
   Main_Block:
   DO ON ERROR UNDO:
   
      FIND FIRST KittingConfig NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KittingConfig THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "No Kitting config record exist.").
         LEAVE Main_Block.
      END.

      /* Check KittingConfig for multiuser */
      IF KittingConfig.AllowMultiUserPalletBuild THEN
      DO:
         chrResult = "Yes".
         
         /* Set session values for pcsAskQuestion.p*/
         chrSsnTitle:setValue("New Pallet").
         chrSsnQuestion:setValue("Do you wish to create a new parent pallet?").
         
         LEAVE Main_Block.
      END.
      ELSE      
         chrResult = "No".

   END. /* Main_Block */

   /* Releases */
   RELEASE KittingConfig   NO-ERROR.
    
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
