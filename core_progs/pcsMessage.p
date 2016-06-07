/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsMessage.p
Purpose : Generic Message option

          Possible Results : Continue

Author  : MC
Date    : 10th December 2012
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

   /* Local Objects */
   DEFINE VARIABLE chrSsnTitle   AS sessionValue.
   DEFINE VARIABLE chrSsnMessage AS sessionValue.

   /* Get Current Data */
   chrSsnTitle   = fGetSessionValue("MessageTitle").
   chrSsnMessage = fGetSessionValue("Message").

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
               
               IF fGetSessionValue(chrTextToReplace):chrValue <> "" THEN DO:               
                  chrNewString = REPLACE(chrNewString, "[" + chrTextToReplace + "]", fGetSessionValue(chrTextToReplace):chrValue).
               END.
               ELSE
               DO:
                  chrNewString = REPLACE(chrNewString, "[" + chrTextToReplace + "]", STRING(fGetSessionValue(chrTextToReplace):intValue)).
               END.               

            END.

            /* Ask Event Question */
            RUN DisplayMessage("Process Message",
                               chrNewString).
   
         END.
      END.
      ELSE
      DO: 
         /* Display the Message */
         RUN DisplayMessage(chrSsnTitle:chrValue, chrSsnMessage:chrValue).
      END.

      chrResult = "Continue".
   END.

   /* Clean Up */
   DELETE OBJECT chrSsnTitle   NO-ERROR.
   DELETE OBJECT chrSsnMessage NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.

