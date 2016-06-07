/*------------------------------------------------------------------------------------------------------------------------------------------
Program : psAskQuestion.p
Purpose : Generic User Confirm option

          Possible Results : Yes/No

Author  : DCummins
Date    : 19th June 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character SessionValue Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}
   {fncStatusTypeFunctions.i}
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Local Variables */
   DEFINE VARIABLE intCount         AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrTextToReplace AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrNewString     AS CHARACTER   NO-UNDO.

   /* Session Objects */
   DEFINE VARIABLE chrSsnTitle    AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnQuestion AS sessionValue NO-UNDO.
   DEFINE VARIABLE logSsnOverride AS sessionValue NO-UNDO.
   
   /* Get Current Session Data */
   chrSsnTitle    = fGetSessionValue("QuestionTitle").
   chrSsnQuestion = fGetSessionValue("Question").
   logSsnOverride = fGetSessionValue("OverrideQuestionWithRoute").
        
   IF logSsnOverride:checkExists() THEN
   DO:
      chrResult = STRING(logSsnOverride:logValue).
   END.
   ELSE
   Main_Block:
   DO ON ERROR UNDO:
      
      /* Check for Event Messaging */
      IF CAN-FIND(FIRST ProcessMessage NO-LOCK
                  WHERE ProcessMessage.ProcessEventID = ProcessEvent.ProcessEventID) THEN
      DO:      
         FOR EACH ProcessMessage NO-LOCK
            WHERE ProcessMessage.ProcessEventID = ProcessEvent.ProcessEventID:
   
            chrNewString = ProcessMessage.MessageText.

            /* Replace the Dynamic Values in the String */
            DO intCount = 1 TO NUM-ENTRIES(ProcessMessage.MessageText, "[") - 1:

               chrTextToReplace = ENTRY(2, chrNewString, "[").
               chrTextToReplace = ENTRY(1, chrTextToReplace,"]").               
               
               IF fGetSessionValue(chrTextToReplace):chrValue <> "" THEN 
               DO:               
                  chrNewString = REPLACE(chrNewString, "[" + chrTextToReplace + "]", fGetSessionValue(chrTextToReplace):chrValue).
               END.
               ELSE
               DO:
                  chrNewString = REPLACE(chrNewString, "[" + chrTextToReplace + "]", STRING(fGetSessionValue(chrTextToReplace):intValue)).
               END.               

            END.

            /* Ask Event Question */
            RUN DisplayConfirm("Process Question",
                               chrNewString).
   
         END.
      END.
      ELSE
      DO:      
         /* Ask the Question */
         RUN DisplayConfirm(chrSsnTitle:chrValue, chrSsnQuestion:chrValue).
      END.
      
      CASE logMessageResult:
         WHEN ? THEN
         DO:
            UNDO Main_Block, LEAVE Main_Block.
         END.
         WHEN TRUE THEN
         DO:
            chrResult = "Yes".
         END.
         WHEN FALSE THEN
         DO:
            chrResult = "No".
         END.
      END CASE. /* CASE logMessageResult */
   
   END. /* Main_Block */      
   
   /* Clear the Session */
   fClearSessionValue("OverrideQuestionWithRoute").      
   
   /* Clean Up */
   DELETE OBJECT chrSsnTitle    NO-ERROR.
   DELETE OBJECT chrSsnQuestion NO-ERROR.
   DELETE OBJECT logSsnOverride NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END.

