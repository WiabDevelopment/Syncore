/*------------------------------------------------------------------------------------------------------------------------------------------
Program : prcCronProcedures.i
Purpose : Group of generic procedures which can be called from anywhere
Author  : Lily Tran
Date    : 15th November 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
12/03/2014 CS  CR1021     Removed CronEventRun creation to prevent locking issues.
------------------------------------------------------------------------------------------------------------------------------------------*/
PROCEDURE pGetUnixCommand:
   
   /* --- Parameter Definitions --- */
   DEFINE INPUT  PARAMETER intCronEventID     AS INTEGER    NO-UNDO.
   DEFINE OUTPUT PARAMETER chrErrorMessage    AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER chrUnixCommand     AS CHARACTER  NO-UNDO.
   
   FIND FIRST CronEvent NO-LOCK
      WHERE CronEvent.CronEventID = intCronEventID NO-ERROR.
   IF NOT AVAILABLE CronEvent THEN
   DO:
      chrErrorMessage = "CronEventID: " + STRING(intCronEventID) + " does not exist.".  
      RETURN.      
   END.
   
   FIND FIRST Environment NO-LOCK
      WHERE Environment.EnvironmentCode = chrGblEnvironment NO-ERROR.      
   IF NOT AVAILABLE Environment THEN
   DO:
      chrErrorMessage = "EnvironmentCode: " + chrGblEnvironment + " does not exist.".
      RETURN.
   END.   
   
   FIND FIRST CronEventEnvironmentLink NO-LOCK
      WHERE CronEventEnvironmentLink.CronEventID = CronEvent.CronEventID
      AND CronEventEnvironmentLink.EnvironmentID = Environment.EnvironmentID NO-ERROR.
   IF NOT AVAILABLE CronEventEnvironmentLink THEN
   DO:
      chrErrorMessage = "CronEventEnvironmentLink for CronEventID: " + string(intCronEventID) + " and EnvironmentCode: "
                           + chrGblEnvironment + " does not exist.".
      RETURN.                     
   END.   
   
   FIND FIRST CodePage NO-LOCK
      WHERE CodePage.CodePageID = CronEvent.CodePageID NO-ERROR.     
   IF NOT AVAILABLE CodePage THEN
   DO:
      chrErrorMessage = "CodePageID: " + STRING(CronEvent.CodePageID) + " for CronEventID: " 
                           + STRING(CronEvent.CronEventID) + " does not exist.".
      RETURN.                                                                    
   END.
   
   chrUnixCommand = "../cronscripts/runbatch_" + TRIM(LOWER(CodePage.CodePageCode)) + ".sh". 

   IF SEARCH(chrUnixCommand) = ? THEN
   DO:
      chrErrorMessage = "Unix script for CodePageCode: " + CodePage.CodePageCode + " does not exist. The script should be in: " 
                           + REPLACE(chrUnixCommand, "..", TRIM(LOWER(Config.PackageName))).
      RETURN.
   END.

   FIND FIRST ProcessProgram NO-LOCK
      WHERE ProcessProgram.ProcessProgramID = CronEvent.ProcessProgramID NO-ERROR.     
   IF NOT AVAILABLE ProcessProgram THEN
   DO:
      chrErrorMessage = "ProcessProgramID: " + STRING(CronEvent.ProcessProgramID) + " for CronEventID: "
                           + STRING(CronEvent.CronEventID) + " does not exist.".
      RETURN.                                                 
   END.
   
   IF NOT AVAILABLE Config THEN
   DO:
      chrErrorMessage = "No Config record found.".
      RETURN.
   END.
      
END PROCEDURE. /* pGetUnixCommand */   


PROCEDURE pValidateCronEventFields:
   
   /* --- Parameter Definitions --- */
   DEFINE INPUT  PARAMETER chrEntryList       AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER chrListType        AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER chrErrorMessage    AS CHARACTER NO-UNDO.

   /* --- Local Variables ---*/
   DEFINE VARIABLE intEntry     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE intIntValue  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE chrPrevValue AS CHARACTER NO-UNDO.

   /* Make sure the leading zeros aren't left out and that they're entering numbers*/
   DO intEntry = 1 TO NUM-ENTRIES(chrEntryList):
    
      ASSIGN intIntValue = INTEGER(ENTRY(intEntry,chrEntryList)) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN
      DO:
         chrErrorMessage = "You can only enter 'ALL' or else numerical values seperated by commas.".
         RETURN.
      END.
      ELSE
      DO:
         ENTRY(intEntry,chrEntryList) = STRING(INTEGER(ENTRY(intEntry,chrEntryList)),"99") NO-ERROR.
      
         IF ENTRY(intEntry,chrEntryList) < chrPrevValue THEN
         DO:
            chrErrorMessage = "Entries must be in sequential Order. Cannot have:" + chrPrevValue + " -> " + ENTRY(intEntry,chrEntryList). 
            RETURN.
         END.
      
         IF LOOKUP(ENTRY(intEntry,chrEntryList),chrEntryList) <> intEntry THEN
         DO:
            chrErrorMessage = "You cannot have duplicate entries: " + ENTRY(intEntry,chrEntryList).
            RETURN.
         END.
      
         CASE chrListType:
        
            WHEN "DaysOfWeek" THEN
            DO:
               IF INTEGER(ENTRY(intEntry,chrEntryList)) < 1 OR INTEGER(ENTRY(intEntry,chrEntryList)) > 7 THEN
               DO:
                  chrErrorMessage = "Days of the Week can only be 01-07".
                  RETURN.
               END.
            END. /* DaysOfWeek */
           
            WHEN "DaysOfMonth" THEN
            DO:
               IF INTEGER(ENTRY(intEntry,chrEntryList)) < 1 OR INTEGER(ENTRY(intEntry,chrEntryList)) > 31 THEN
               DO:
                  chrErrorMessage = "Days of the Month can only be 01->31".
                  RETURN.
               END.
            END. /* DaysOfMonth */
           
            WHEN "Month" THEN
            DO:
               IF INTEGER(ENTRY(intEntry,chrEntryList)) < 1 OR INTEGER(ENTRY(intEntry,chrEntryList)) > 12 THEN
               DO:
                  chrErrorMessage = "Months can only be 01->12".
                  RETURN.
               END.
            END. /* Month */
           
            WHEN "Hour" THEN
            DO:
               IF INTEGER(ENTRY(intEntry,chrEntryList)) < 0 OR INTEGER(ENTRY(intEntry,chrEntryList)) > 23 THEN
               DO:
                  chrErrorMessage = "Hours can only be 00->23".
                  RETURN.
               END.
            END. /* Hour */
           
            WHEN "Minute" THEN
            DO:
               IF INTEGER(ENTRY(intEntry,chrEntryList)) < 0 OR INTEGER(ENTRY(intEntry,chrEntryList)) > 59 THEN
               DO:
                  chrErrorMessage =  "Minutes can only be 01->59".
                  RETURN.
               END.
            END. /* Minute */
      
         END CASE. /*CASE chrListType:*/
   
         chrPrevValue = ENTRY(intEntry,chrEntryList).
         
      END. /* if not error:status-error */
   END. /* intEntry to num-entries */

END PROCEDURE. /* pValidateCronEventFields */
