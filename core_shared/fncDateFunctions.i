/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncDateFunctions.i
Purpose : Date & Time functions used by character and web
Author  : BG
Date    : 27th April 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
26/06/13   BG  DayMen     Added fGetDate() function - poorly named I know.
02/03/14   AB  Syngate    Added fDisplayDate&Timestamp() function.
20/03/14   MN  SynCore    Added core database prefix
16/06/14   AB  DayMen     Added returns for functions with date inputs that return a character string when input is ?
23/06/14   CS  DayMen     Added fFirstDayOfMonth, fLastDayOfMonth, fFirstDayOfWeek, fLastDayOfWeek, fFirstDayOfPrevMonth, 
                          fLastDayOfPrevMonth, fFirstDayOfPrevWeek, and fLastDayOfPrevWeek functions.
21/04/2015 CS  Canontlb   Added WorkDay Table logic to fNextWorkingDay.                         
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttPosition
   FIELD Position AS INTEGER
   FIELD Type     AS CHARACTER
   INDEX Position Position.
   

/* ************************  Function Implementations ***************** */
FUNCTION fDateToTimestamp RETURNS CHARACTER (INPUT datDate AS DATE):
   
   IF datDate = ? THEN
      RETURN "".   

   RETURN STRING(YEAR(datDate)) + 
          STRING(MONTH(datDate),"99") + 
          STRING(DAY(datDate),"99").
   
END FUNCTION. /*fDateToTimestamp*/


FUNCTION fDisplayDate RETURNS CHARACTER (INPUT datDate    AS DATE,
                                         INPUT chrFormat  AS CHARACTER,
                                         INPUT chrDivider AS CHARACTER):
   
   DEFINE VARIABLE intCharacter  AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrOutput     AS CHARACTER   NO-UNDO.
   
   IF datDate = ? THEN
      RETURN "".

   IF chrFormat = "" OR LENGTH(chrFormat) <> 3 THEN
      ASSIGN chrFormat = "dmy".
   
   DO intCharacter = 1 TO LENGTH(chrFormat):
     
      CASE(SUBSTRING(chrFormat,intCharacter,1)):
         
         WHEN ("d") THEN
            chrOutput = chrOutput + STRING(DAY(datDate),"99") + chrDivider.
         WHEN ("m") THEN
            chrOutput = chrOutput + STRING(MONTH(datDate),"99") + chrDivider.
         WHEN ("y") THEN
            chrOutput = chrOutput + STRING(YEAR(datDate)) + chrDivider.
         
      END CASE.
      
   END. /*DO intCharacter = 1 TO LENGTH(chrFormat):*/
   
   chrOutput = TRIM(chrOutput,chrDivider).
   
   RETURN chrOutput.
   
END FUNCTION. /*fDisplayDate*/


FUNCTION fDisplayDateWithFormat RETURNS CHARACTER (INPUT datDate    AS DATE,
                                                   INPUT chrFormat  AS CHARACTER,
                                                   INPUT chrDivider AS CHARACTER):
   
   DEFINE VARIABLE intCharacter  AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrOutput     AS CHARACTER   NO-UNDO.
   
   /* Clear Previosu Data */
   EMPTY TEMP-TABLE ttPosition.   
         
   IF datDate = ? THEN
      RETURN "".

   IF INDEX(chrFormat, "DD") <> 0 THEN
   DO:
      CREATE ttPosition.
      ASSIGN ttPosition.Position = INDEX(chrFormat, "DD")
             ttPosition.Type     = "DD".
   END.
   
   IF INDEX(chrFormat, "MM") <> 0 THEN
   DO:
      CREATE ttPosition.
      ASSIGN ttPosition.Position = INDEX(chrFormat, "MM")
             ttPosition.Type     = "MM".
   END.
   
   IF INDEX(chrFormat, "YY") <> 0 THEN
   DO:
      CREATE ttPosition.
      ASSIGN ttPosition.Position = INDEX(chrFormat, "YY")
             ttPosition.Type     = "YY".
             
      IF INDEX(chrFormat, "YYYY") <> 0 THEN
         ttPosition.Type     = "YYYY".

   END.
            
   
   FOR EACH ttPosition NO-LOCK BY ttPosition.Position:
   
      CASE ttPosition.Type:      
         
         WHEN "DD" THEN 
            chrOutput = chrOutput + STRING(DAY(datDate),"99") + chrDivider.            
         WHEN "MM" THEN 
            chrOutput = chrOutput + STRING(MONTH(datDate),"99") + chrDivider.
         WHEN "YY" THEN 
            chrOutput = chrOutput + SUBSTRING(STRING(YEAR(datDate),"9999"),3,2) + chrDivider.
         WHEN "YYYY" THEN 
            chrOutput = chrOutput + STRING(YEAR(datDate),"9999") + chrDivider.
         
      END CASE.
   
   END.               
                 
   chrOutput = TRIM(chrOutput,chrDivider).
   
   RETURN chrOutput.
   
END FUNCTION. /* fDisplayDateWithFormat */



FUNCTION fDisplayDate&Time RETURNS CHARACTER (INPUT chrTimeStamp  AS CHARACTER,
                                              INPUT chrDateFormat AS CHARACTER):
   
   DEFINE VARIABLE intCharacter    AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrInputFormat  AS CHARACTER   NO-UNDO CASE-SENSITIVE. /* Need case sensitivity for the 'm' month vs 'M' minutes */
   DEFINE VARIABLE chrOutputString AS CHARACTER   NO-UNDO.
   
   IF chrDateFormat = "" THEN
      chrDateFormat = "dmy".
   
   chrInputFormat = chrDateFormat.
   
   IF chrTimeStamp <> "" THEN
   DO intCharacter = 1 TO LENGTH(chrInputFormat):
      
      CASE(SUBSTRING(chrInputFormat,intCharacter,1)):
         
         WHEN ("d") THEN
            chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,7,2).
         WHEN ("m") THEN
            chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,5,2).
         WHEN ("y") THEN
            chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,1,4).
         WHEN ("H") THEN
            chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,9,2).
         WHEN ("M") THEN
            chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,11,2).
         WHEN ("S") THEN
            chrOutputString = chrOutputString + SUBSTRING(chrTimeStamp,13,2).
         OTHERWISE
            chrOutputString = chrOutputString + SUBSTRING(chrInputFormat,intCharacter,1).
         
      END CASE.
      
   END. /*DO intCharacter = 1 TO LENGTH(chrInputFormat):*/
   
   RETURN chrOutputString.
   
END FUNCTION. /*fDisplayDate&Time*/


/* Use the chrLongMonths and chrShortMonths from fncGlobalFunctions as an input.*/
FUNCTION fGetMonthName RETURNS CHARACTER (INPUT chrMonths AS CHARACTER,
                                          INPUT intMonth  AS INTEGER):

   DEFINE VARIABLE chrOutput AS CHARACTER NO-UNDO.
   
   IF intMonth = 0 OR chrMonths = "" THEN
      RETURN "".
   
   chrOutput = ENTRY(intMonth,chrMonths).
    
   RETURN chrOutput.
    
END FUNCTION. /*fGetMonthName*/


FUNCTION fShortDate RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN SUBSTRING(chrTimestamp,7,2) + "/" +
          SUBSTRING(chrTimestamp,5,2) + "/" +
          SUBSTRING(chrTimestamp,3,2).
   
END FUNCTION. /*fShortDate*/


FUNCTION fShortDateUnFormatted RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN SUBSTRING(chrTimestamp,3,2) + 
          SUBSTRING(chrTimestamp,5,2) + 
          SUBSTRING(chrTimestamp,7,2).
   
END FUNCTION. /*fShortDate*/


FUNCTION fDate RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN SUBSTRING(chrTimestamp,7,2) + "/" +
          SUBSTRING(chrTimestamp,5,2) + "/" +
          SUBSTRING(chrTimestamp,1,4).
   
END FUNCTION. /*fDate*/


FUNCTION fGetDate RETURNS DATE (INPUT chrTimestamp AS CHARACTER):
   
   RETURN DATE(SUBSTRING(chrTimestamp,7,2) + SUBSTRING(chrTimestamp,5,2) + SUBSTRING(chrTimestamp,1,4)).
   
END FUNCTION. /*fGetDate*/


FUNCTION fGetDateTime RETURNS DATETIME (INPUT chrTimestamp AS CHARACTER):
   /*DATETIME ( month , day , year , hours , minutes        [ , seconds [ , milliseconds ] ] )*/
   RETURN DATETIME(INTEGER(SUBSTRING(chrTimestamp,6,2)),
                   INTEGER(SUBSTRING(chrTimestamp,9,2)),
                   INTEGER(SUBSTRING(chrTimestamp,1,4)),
                   INTEGER(SUBSTRING(chrTimestamp,12,2)),
                   INTEGER(SUBSTRING(chrTimestamp,15,2)),
                   INTEGER(SUBSTRING(chrTimestamp,18,2))).
   
END FUNCTION. /*fGetDateTime*/


FUNCTION fTime RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN SUBSTRING(chrTimestamp,9,2)  + ":" +
          SUBSTRING(chrTimestamp,11,2) + ":" +
          SUBSTRING(chrTimestamp,13,2).
   
END FUNCTION. /*fTime*/


FUNCTION fDay RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN SUBSTRING(chrTimestamp,7,2).
   
END FUNCTION.


FUNCTION fMonth RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN SUBSTRING(chrTimestamp,5,2).
   
END FUNCTION.


FUNCTION fYear RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN SUBSTRING(chrTimestamp,1,4).
   
END FUNCTION.


FUNCTION fIntHour RETURNS INTEGER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN INTEGER(SUBSTRING(chrTimestamp,9,2)).
   
END FUNCTION.


FUNCTION fIntMins RETURNS INTEGER (INPUT chrTimestamp AS CHARACTER):
   
   RETURN INTEGER(SUBSTRING(chrTimestamp,11,2)).
   
END FUNCTION.


FUNCTION fOpeningTimestamp RETURNS CHARACTER (INPUT datDate AS DATE):
   
   IF datDate = ? THEN
      RETURN "".   
   
   RETURN STRING(YEAR(datDate)) +
          STRING(MONTH(datDate),"99") +
          STRING(DAY(datDate),"99") + "000000000".
   
END FUNCTION.


FUNCTION fClosingTimestamp RETURNS CHARACTER (INPUT datDate AS DATE):

   IF datDate = ? THEN
      RETURN "".

   RETURN STRING(YEAR(datDate)) +
          STRING(MONTH(datDate),"99") +
          STRING(DAY(datDate),"99") + "235959999".
   
END FUNCTION.


/* Interval between two Timestamps in various denominations */
FUNCTION fInterval RETURNS INTEGER (INPUT chrFromTimestamp AS CHARACTER,
                                    INPUT chrToTimestamp   AS CHARACTER,
                                    INPUT chrDenomination  AS CHARACTER):
   
   DEFINE VARIABLE datFromRunTime   AS DATETIME   NO-UNDO.
   DEFINE VARIABLE datToRunTime     AS DATETIME   NO-UNDO.
   DEFINE VARIABLE intInterval      AS INTEGER    NO-UNDO.
   
   ASSIGN datFromRunTime = DATETIME(INTEGER(SUBSTRING(chrFromTimestamp,5,2)),
                                    INTEGER(SUBSTRING(chrFromTimestamp,7,2)),
                                    INTEGER(SUBSTRING(chrFromTimestamp,1,4)),
                                    INTEGER(SUBSTRING(chrFromTimestamp,9,2)),
                                    INTEGER(SUBSTRING(chrFromTimestamp,11,2))).

   IF LENGTH(chrFromTimestamp) >= 15 THEN
      ASSIGN datFromRunTime = DATETIME(INTEGER(SUBSTRING(chrFromTimestamp,5,2)),
                                       INTEGER(SUBSTRING(chrFromTimestamp,7,2)),
                                       INTEGER(SUBSTRING(chrFromTimestamp,1,4)),
                                       INTEGER(SUBSTRING(chrFromTimestamp,9,2)),
                                       INTEGER(SUBSTRING(chrFromTimestamp,11,2)),
                                       INTEGER(SUBSTRING(chrFromTimestamp,13,2))).
   
   ASSIGN datToRunTime = DATETIME(INTEGER(SUBSTRING(chrToTimestamp,5,2)),
                                  INTEGER(SUBSTRING(chrToTimestamp,7,2)),
                                  INTEGER(SUBSTRING(chrToTimestamp,1,4)),
                                  INTEGER(SUBSTRING(chrToTimestamp,9,2)),
                                  INTEGER(SUBSTRING(chrToTimestamp,11,2))).

   IF LENGTH(chrToTimestamp) >= 15 THEN
      ASSIGN datToRunTime = DATETIME(INTEGER(SUBSTRING(chrToTimestamp,5,2)),
                                     INTEGER(SUBSTRING(chrToTimestamp,7,2)),
                                     INTEGER(SUBSTRING(chrToTimestamp,1,4)),
                                     INTEGER(SUBSTRING(chrToTimestamp,9,2)),
                                     INTEGER(SUBSTRING(chrToTimestamp,11,2)),
                                     INTEGER(SUBSTRING(chrToTimestamp,13,2))).
                                    
   
   ASSIGN intInterval = INTERVAL(datFromRunTime, datToRunTime, chrDenomination). 
   
   RETURN intInterval.
   
END FUNCTION. /*fInterval*/


/* Accepts a Timestamp and returns one with a single minute added - seconds ignored */
FUNCTION fNextMinute RETURNS CHARACTER (INPUT chrTimestamp AS CHARACTER):
   
   DEFINE VARIABLE datFromRunTime   AS DATETIME   NO-UNDO.
   DEFINE VARIABLE chrDateTime      AS CHARACTER  NO-UNDO.
   
   ASSIGN datFromRunTime = DATETIME(INTEGER(SUBSTRING(chrTimestamp,5,2)),
                                    INTEGER(SUBSTRING(chrTimestamp,7,2)),
                                    INTEGER(SUBSTRING(chrTimestamp,1,4)),
                                    INTEGER(SUBSTRING(chrTimestamp,9,2)),
                                    INTEGER(SUBSTRING(chrTimestamp,11,2))). 
   
   ASSIGN datFromRunTime = datFromRunTime + (60000)
          chrDateTime    = STRING(datFromRunTime).
   
   ASSIGN chrTimestamp = SUBSTRING(chrDateTime,7,4) + 
                         SUBSTRING(chrDateTime,4,2) + 
                         SUBSTRING(chrDateTime,1,2) + 
                         SUBSTRING(chrDateTime,12,2) + 
                         SUBSTRING(chrDateTime,15,2).
   
   RETURN chrTimestamp.
   
END FUNCTION. /*fNextMinute*/


/* Returns a string with the next year and month combined in YYYYMM format */
FUNCTION fNextYearMonth RETURNS CHARACTER (INPUT chrYearMonth AS CHARACTER):
   
   DEFINE VARIABLE chrMonth AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrYear  AS CHARACTER   NO-UNDO.
   
   ASSIGN chrYear  = SUBSTRING(chrYearMonth,1,4)
          chrMonth = SUBSTRING(chrYearMonth,5,2).
   
   IF chrMonth = "12" THEN
      ASSIGN chrYear  = STRING((INTEGER(chrYear) + 1),"9999")
             chrMonth = "01".
   ELSE
      ASSIGN chrMonth = STRING((INTEGER(chrMonth) + 1),"99").
   
   RETURN STRING(chrYear,"9999") + STRING(chrMonth,"99").
   
END FUNCTION. /*fNextYearMonth*/


/* Returns the Timestamp that the Cron is currently running */
FUNCTION fCurrentCronTime RETURNS CHARACTER():
   
   DEFINE VARIABLE chrCurrentCronTimestamp AS CHARACTER.
   
   FIND core.CronConfig NO-LOCK.
   
   /* If CronIsRunning = TRUE then the Cron is in the process of running the next minute after chrPreviousRun     */
   /* If it isn't TRUE then it has already finished and will have updated the chrPreviousRun field itself already */
   IF core.CronConfig.CronIsRunning = TRUE THEN
      chrCurrentCronTimestamp = fNextMinute(core.CronConfig.PreviousRun).
   ELSE
      chrCurrentCronTimestamp = core.CronConfig.PreviousRun.
   
   RETURN chrCurrentCronTimestamp.
   
END FUNCTION. /*fCurrentCronTime*/


FUNCTION fNextWorkingDay RETURNS DATE (INPUT datStartDate AS DATE):
   
   DEFINE VARIABLE datReturnDate AS DATE.
   DEFINE VARIABLE intDayNumber  AS INTEGER.
   DEFINE VARIABLE logSetDay     AS LOGICAL.
   
   /* Current Day of Week */
   intDayNumber = WEEKDAY(datStartDate).
   
   IF intDayNumber = 7 THEN
      intDayNumber = 1.
   ELSE
      intDayNumber = intDayNumber + 1.
   
   /* Try to find WorkDay Record for next working day*/
   NextWorkDay_Loop:
   REPEAT:   
      
      FIND FIRST WorkDay NO-LOCK /* idx=DayNumberActive */
         WHERE WorkDay.DayNumber = intDayNumber 
         AND   WorkDay.Active NO-ERROR.
      IF NOT AVAILABLE WorkDay THEN
         LEAVE NextWorkDay_Loop.

      /* This is a working day calculate the date and return it */      
      IF WorkDay.IsWorkingDay THEN
      DO:
         /*If the end of the week just add the Day to it to get the new date */
         IF WEEKDAY(datStartDate) = 7 THEN
         DO:
            datReturnDate = datStartDate + intDayNumber.
            RETURN datReturnDate.
         END. /*IF WEEKDAY(datStartDate) = 7 THEN*/
          
         /* If we have not looped around the end of the week just add Next Working Day Number - Current Day */
         IF logSetDay = FALSE THEN
            datReturnDate = datStartDate + intDayNumber - WEEKDAY(datStartDate).
         ELSE
         DO:
            /* Had to reset the day number so need to find how many days until end of current week and the new Working Day Number */
            datReturnDate = datStartDate + intDayNumber + 7 - WEEKDAY(datStartDate).
         END. /*IF logSetDay = TRUE THEN*/  
             
         RETURN datReturnDate.
      END. /*IF WorkDay.IsWorkingDay THEN*/
      
      /* If this is Saturday set to Sunday Day 1 and flag so it doesn't reset it again */
      IF intDayNumber = 7 THEN
      DO:
         IF logSetDay = TRUE THEN
            LEAVE NextWorkDay_Loop.

         ASSIGN intDayNumber = 1
                logSetDay    = TRUE.
      END. /*IF intDayNumber = 7 THEN*/
      ELSE 
         intDayNumber = intDayNumber + 1.
      
      NEXT NextWorkDay_Loop.
      
   END. /*REPEAT:*/
   
   /* Did not find a WorkDay Record so use the old logic */
   IF WEEKDAY(datStartDate) = 7 /*Sat*/ THEN
      datReturnDate = datStartDate + 2.
   ELSE IF WEEKDAY(datStartDate) = 6 /*Fri*/ THEN
      datReturnDate = datStartDate + 3.
   ELSE
      datReturnDate = datStartDate + 1.
      
   RETURN datReturnDate.
       
   
END FUNCTION. /*fNextWorkingDay*/


FUNCTION fPrevWorkingDay RETURNS DATE (INPUT datStartDate AS DATE):
   
   DEFINE VARIABLE datReturnDate AS DATE.

   IF WEEKDAY(datStartDate) = 1 /*Sun*/ THEN
      datReturnDate = datStartDate - 2.
   ELSE IF WEEKDAY(datStartDate) = 2 /*Mon*/ THEN 
      datReturnDate = datStartDate - 3.
   ELSE
      datReturnDate = datStartDate - 1.
   
   RETURN datReturnDate.
   
END FUNCTION. /*fPrevWorkingDay*/


FUNCTION fLastWorkingDayOfMonth RETURNS DATE (INPUT datStartDate AS DATE):
   
   DEFINE VARIABLE datReturnDate    AS DATE.
   DEFINE VARIABLE chrNextYearMonth AS CHARACTER.
   
   ASSIGN chrNextYearMonth = fNextYearMonth(STRING(YEAR(datStartDate)) + STRING(MONTH(datStartDate)))
          datReturnDate    = DATE("01" + SUBSTRING(chrNextYearMonth,5,2) + SUBSTRING(chrNextYearMonth,1,4))
          datReturnDate    = fPrevWorkingDay(datReturnDate).
   
   RETURN datReturnDate.
   
END FUNCTION. /*fLastWorkingDayOfMonth*/


FUNCTION fFirstDayOfMonth RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE((MONTH(datStartDate)),1,YEAR(datStartDate)).
   
END FUNCTION. /*fFirstDayOfMonth*/


FUNCTION fLastDayOfMonth RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE((MONTH(datStartDate) + 1),1, YEAR(datStartDate)) - 1.
   
END FUNCTION. /*fLastDayOfMonth*/


FUNCTION fFirstDayOfPrevMonth RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE((MONTH(datStartDate) - 1),1,YEAR(datStartDate)).
   
END FUNCTION. /*fFirstDayOfPrevMonth*/


FUNCTION fLastDayOfPrevMonth RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE(MONTH(datStartDate),1,YEAR(datStartDate)) - 1.
      
END FUNCTION. /*fLastDayOfPrevMonth*/


FUNCTION fFirstDayOfWeek RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE((MONTH(datStartDate)),(DAY(datStartDate) + 1 - WEEKDAY(datStartDate)),YEAR(datStartDate)).
   
END FUNCTION. /*fFirstDayOfWeek*/


FUNCTION fLastDayOfWeek RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE((MONTH(datStartDate)),(DAY(datStartDate) + 7 - WEEKDAY(datStartDate)),YEAR(datStartDate)).
   
END FUNCTION. /*fLastDayOfWeek*/


FUNCTION fFirstDayOfPrevWeek RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE((MONTH(datStartDate)),(DAY(datStartDate) - (WEEKDAY(datStartDate) + 6)),YEAR(datStartDate)).
   
END FUNCTION. /*fFirstDayOfPrevWeek*/


FUNCTION fLastDayOfPrevWeek RETURNS DATE (INPUT datStartDate AS DATE):
   
   RETURN DATE((MONTH(datStartDate)),(DAY(datStartDate) - WEEKDAY(datStartDate)),YEAR(datStartDate)).
   
END FUNCTION. /*fLastDayOfPrevWeek*/


FUNCTION fCapitalise RETURNS CHARACTER (INPUT chrInputString AS CHARACTER):
   
   RETURN UPPER(SUBSTRING(chrInputString,1,1)) + LOWER(SUBSTRING(chrInputString,2)).
   
END FUNCTION. /*fCapitalise*/


FUNCTION fTrim RETURNS CHARACTER (INPUT chrInputString AS CHARACTER,
                                  INPUT chrSide        AS CHARACTER,
                                  INPUT intNumChars    AS INTEGER):
   
   CASE chrSide:
      
      WHEN "RIGHT" THEN
      DO:
         chrInputString = SUBSTRING(chrInputString,1,(LENGTH(chrInputString) - intNumChars)).
      END.
      
      WHEN "LEFT" THEN
      DO:
         chrInputString = SUBSTRING(chrInputString, intNumChars + 1).
      END.
      
   END CASE.
   
   RETURN chrInputString.
   
END FUNCTION. /* fTrim */


/* Returns an XML formatted timstamp when given a Progress Timestamp in DATETIME-TZ format e.g. NOW()*/
FUNCTION fXmlTimestamp RETURNS CHARACTER(INPUT datDateToConvert AS DATETIME-TZ):

   DEFINE VARIABLE chrTimeStamp AS CHARACTER.

   IF datDateToConvert = ? THEN
      RETURN "".   
   
   chrTimeStamp = STRING(datDateToConvert).
   
   RETURN STRING(SUBSTRING(chrTimeStamp, 7,4),"9999") + "-" +
          STRING(SUBSTRING(chrTimeStamp, 4,2),  "99") + "-" +
          STRING(SUBSTRING(chrTimeStamp, 1,2),  "99") + "T" +
          STRING(SUBSTRING(chrTimeStamp, 12)).
   
END. /* fXmlTimestamp */


/* Returns a character string of the datetime datatype */
FUNCTION fDisplayDate&Timestamp RETURNS CHARACTER(INPUT datDateToConvert AS DATETIME):
   
   DEFINE VARIABLE chrTimeStamp AS CHARACTER.
   
   IF datDateToConvert = ? THEN
      RETURN "".
   
   chrTimeStamp = STRING(datDateToConvert).
   
   RETURN chrTimeStamp.

END. /* fDisplayDate&Timestamp */
