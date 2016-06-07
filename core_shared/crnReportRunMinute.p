/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnReportRunMinute.p
Purpose : This file is called by the crnReportScheduler.p with a timestamp input param. It reads ReportSchedule records and 
          if the month, day, hour, minute etc match the incoming timestamp time then it outputs a unix command 
          for each program to be run. May not be run in real time as the Db could have been down for maintenance
          or whatever. 
         
          Writes a log file entry for each time a program is run in the logs directory and to a separate log if a 
          ReportSchedule is paused.
         
          Cloned from crnCronRunMinute.p
         
Author  : Christopher Shelley
Date    : 14/05/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER chrTimestampToRun        AS CHARACTER.
DEFINE INPUT PARAMETER logSkipRecurringCrons    AS LOGICAL.
DEFINE INPUT PARAMETER logSkipNonRecurringCrons AS LOGICAL.

/* Includes */
{defSessionVariables.i}
{fncDateFunctions.i}
{fncGlobalFunctions.i}
{fncServerFunctions.i}
{fncDataFunctions.i}
{fncClassFunctions.i}
{prcReportProcedures.i}

/* Local Variables */
DEFINE VARIABLE chrMinute                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrHour                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDay                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrMonth                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrWeekday                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrYear                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrUnixScript                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrParameterString              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrBatchScript                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrProgramName                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCodePage                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intErrorCount                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intReportEmailGroupID             AS INTEGER     NO-UNDO.
DEFINE VARIABLE logAllWentOK                    AS LOGICAL     NO-UNDO.
                    
/* Streams */
DEFINE STREAM str2LogFile.

/* Functions */
FUNCTION fWriteToLog RETURNS CHARACTER (INPUT chrMessageString AS CHARACTER,
                                        INPUT intUserID        AS INTEGER,
                                        INPUT intDaysToStore   AS INTEGER,
                                        INPUT chrFilePrefix    AS CHARACTER,
                                        INPUT chrFileInterval  AS CHARACTER):
   
   DEFINE VARIABLE chrLogFile      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrDate&Time    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrLogFilePath  AS CHARACTER   NO-UNDO.
   
   IF chrFilePrefix = "" THEN
      chrFilePrefix = "ReportLog".
   
   IF chrFileInterval = "" THEN
      chrFileInterval = "Day".
   
   IF chrFileInterval = "Year" THEN
      ASSIGN chrLogFile   = chrFilePrefix + "_" + fDisplayDate&Time(fTimestamp(NOW),"y") + ".log"
             chrDate&Time = fDisplayDate&Time(fTimestamp(NOW),"d_m_y H:M:S").
   
   IF chrFileInterval = "Month" THEN
      ASSIGN chrLogFile   = chrFilePrefix + "_" + fDisplayDate&Time(fTimestamp(NOW),"m_y") + ".log"
             chrDate&Time = fDisplayDate&Time(fTimestamp(NOW),"d_m_y H:M:S").
   
   IF chrFileInterval = "Day" THEN
      ASSIGN chrLogFile   = chrFilePrefix + "_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log"
             chrDate&Time = fDisplayDate&Time(fTimestamp(NOW),"H:M:S").
   
   IF chrFileInterval = "Hour" THEN
      ASSIGN chrLogFile = chrFilePrefix + "_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y_H") + ".log"
             chrDate&Time = fDisplayDate&Time(fTimestamp(NOW),"H:M:S").
   
   IF chrLogFilePath = "" THEN
      chrLogFilePath = "logs".
   
   /* Set the log file destination directory  */
   chrLogFilePath = fGetAgedDirectory("../logs/", intDaysToStore).
   IF chrLogFilePath BEGINS "Error" THEN
      chrLogFilePath = "../logs/".
   
   chrLogFile = chrLogFilePath + chrLogFile.
   
   OUTPUT STREAM str2LogFile TO VALUE(chrLogFile) APPEND.

   PUT STREAM str2LogFile UNFORMATTED chrDate&Time + " " + chrMessageString SKIP.

   OUTPUT STREAM str2LogFile CLOSE.
   
END FUNCTION. /*fWriteToLog*/


FUNCTION fLogPauses RETURNS CHARACTER (INPUT chrMessageString AS CHARACTER):
   
   /* This function is in logging_functions.i */
   fWriteToLog (INPUT chrMessageString,    /* Message */
                INPUT intGblUserID,
                INPUT 90,                  /* Days to Store */
                INPUT "CronPauses",        /* Log File Prefix */
                INPUT "Month").            /* Log File Interval */
   
END FUNCTION.


FUNCTION fLogRuns RETURNS CHARACTER (INPUT chrMessageString AS CHARACTER):
   
   /* This function is in logging_functions.i */
   fWriteToLog (INPUT chrMessageString,    /* Message */
                INPUT intGblUserID,
                INPUT 10,                  /* Days to Store */
                INPUT "CronRuns",          /* Log File Prefix */
                INPUT "Day").              /* Log File Interval */
   
END FUNCTION.

/* This holds the PackageName e.g. hyperthermbr */
FIND FIRST Config NO-LOCK NO-ERROR.
IF NOT AVAILABLE Config THEN
DO:
   FIND FIRST EmailGroup NO-LOCK
      WHERE EmailGroup.GroupCode = "ReportErrors" NO-ERROR.
   IF AVAILABLE EmailGroup THEN
      intReportEmailGroupID = EmailGroup.EmailGroupID.
   ELSE
      intReportEmailGroupID = 1.
END. /* IF NOT AVAILABLE Config */

FIND FIRST Environment NO-LOCK
   WHERE Environment.EnvironmentCode = chrGblEnvironment
   AND   Environment.ACTIVE          = TRUE NO-ERROR.
IF NOT AVAILABLE Environment THEN
DO:
   FIND FIRST EmailGroup NO-LOCK
      WHERE EmailGroup.GroupCode = "CronErrors" NO-ERROR.
   IF AVAILABLE EmailGroup THEN
      intReportEmailGroupID = EmailGroup.EmailGroupID.
   ELSE
      intReportEmailGroupID = 1.

   RUN osSendMail.p (INPUT "",                                       /* Optional list of Users */
                     INPUT "Active Environment Record Not Found",     /* Email Subject */
                     INPUT "Active Environment Record does not Exist sent from Report Run. "
                              + "Environment: " + chrGblEnvironment, /* Plain text message Body */
                     INPUT "",                                       /* Html format message Body */
                     INPUT "",                                       /* File path../idev/hyperthermbr/files/file OR ../files/file */
                     INPUT intReportEmailGroupID,                      /* EmailGroupID that you want to send this to */
                     INPUT "").                                      /* File MasterID is it applies */

END. /* IF NOT AVAILABLE Environment */

ASSIGN chrMinute      = STRING(SUBSTRING(chrTimestampToRun,11,2),"99")
       chrHour        = STRING(SUBSTRING(chrTimestampToRun,9,2),"99")
       chrDay         = STRING(SUBSTRING(chrTimestampToRun,7,2),"99")
       chrMonth       = STRING(SUBSTRING(chrTimestampToRun,5,2),"99")
       chrYear        = STRING(SUBSTRING(chrTimestampToRun,1,4),"9999")
       chrWeekday     = STRING(WEEKDAY(DATE(STRING(chrDay,"99") + STRING(chrMonth,"99") + chrYear)),"99").

fLogRuns("Running Report for Timestamp: " + chrTimestampToRun).

ReportScheduleLoop:
FOR EACH ReportSchedule NO-LOCK /*idx Active_Priority*/
   BY ReportSchedule.Priority DESC:
   
   FIND FIRST ReportScheduleEnvironmentLink NO-LOCK /* idx ReportScheduleIDEnvironmentID */
      WHERE ReportScheduleEnvironmentLink.ReportScheduleID = ReportSchedule.ReportScheduleID
      AND   ReportScheduleEnvironmentLink.EnvironmentID    = Environment.EnvironmentID
      AND   ReportScheduleEnvironmentLink.ACTIVE           = TRUE NO-ERROR.
   IF NOT AVAILABLE ReportScheduleEnvironmentLink THEN
      NEXT ReportScheduleLoop.

   fLogRuns("Running ReportScheduleID: " + STRING(ReportSchedule.ReportScheduleID)).

   /* If we're currently back dating crons that were missed due to maintenance downtime only process those which */
   /* are set to run during a specific hour - other hourly ones will kick off again when backdating is over      */
   IF logSkipRecurringCrons AND ReportSchedule.HourList = "ALL" THEN
      NEXT ReportScheduleLoop.
   
   /* If we're currently running a daylight saving hour for the second time (1am - 2am runs twice in autumn) then*/
   /* only process those crons which aren't set to run during a specific hour - don't want a 2am cron run twice  */
   IF logSkipNonRecurringCrons AND ReportSchedule.HourList <> "ALL" THEN
      NEXT ReportScheduleLoop.
   
   IF ReportSchedule.MonthList <> "ALL" AND LOOKUP(chrMonth,ReportSchedule.MonthList) = 0 THEN
      NEXT ReportScheduleLoop.
   
   IF ReportSchedule.DaysOfWeekList <> "ALL" AND LOOKUP(chrWeekday,ReportSchedule.DaysOfWeekList) = 0 THEN
      NEXT ReportScheduleLoop.
   
   IF ReportSchedule.DaysOfMonthList <> "ALL" AND LOOKUP(chrDay,ReportSchedule.DaysOfMonthList) = 0 THEN
      NEXT ReportScheduleLoop.
   
   IF ReportSchedule.HourList <> "ALL" AND LOOKUP(chrHour,ReportSchedule.HourList) = 0 THEN
      NEXT ReportScheduleLoop.
   
   IF ReportSchedule.MinuteList <> "ALL" AND LOOKUP(chrMinute,ReportSchedule.MinuteList) = 0 THEN
      NEXT ReportScheduleLoop.
   
   IF ReportSchedule.Beginning > chrTimestampToRun THEN
      NEXT ReportScheduleLoop.
   
   IF ReportSchedule.Ending < chrTimestampToRun THEN
      NEXT ReportScheduleLoop.
   
   FIND FIRST ProcessProgram NO-LOCK
      WHERE   ProcessProgram.ProcessProgramID = ReportSchedule.ProcessProgramID NO-ERROR.
   IF NOT AVAILABLE ProcessProgram THEN
      NEXT ReportScheduleLoop.

   /* Document pauses as they will be unusual and could cause problems if set up incorrectly */
   IF chrTimestampToRun >= ReportSchedule.BeginPause 
      AND chrTimestampToRun <= ReportSchedule.EndPause THEN
   DO:
      fLogPauses("Cron Program " + ProcessProgram.ProgramName + " was skipped @ " + chrTimestampToRun + " due to a Pause in the"
                    + " schedule from " + ReportSchedule.BeginPause + " -> " + ReportSchedule.EndPause + ".").
      NEXT ReportScheduleLoop.
   END. /* IF chrTimestampToRun >= ReportSchedule.BeginPause AND chrTimestampToRun <= ReportSchedule.EndPause */
   
   FIND FIRST UserSession NO-LOCK
      WHERE UserSession.SessionID  = chrGblSessionID
      AND   UserSession.StatusCode = "ACTIVE" NO-ERROR.
   IF NOT AVAILABLE UserSession THEN
      RETURN.
   
   RUN pGetUnixCommand(INPUT  ReportSchedule.ReportScheduleID,
                       INPUT  0,
                       OUTPUT chrError,
                       OUTPUT chrUnixScript).

   fLogRuns("Generated UNIX Script: " + chrUnixScript).

   IF chrUnixScript > "" THEN 
   DO:
      chrUnixScript = chrUnixScript + " " + QUOTER(STRING(ReportSchedule.ReportScheduleID) + "," + STRING(intGblUserID) + ",0") 
                         + " " + QUOTER("Reports") + " &".  

      fLogRuns("UNIX COMMAND: " + chrUnixScript).

      OS-COMMAND SILENT VALUE(chrUnixScript).
   END. /* IF chrUnixScript > "" */
          
END. /* FOR EACH ReportSchedule NO-LOCK */

/* Clean-Up */
RELEASE ReportScheduleParameter NO-ERROR.
RELEASE ReportSchedule          NO-ERROR.
RELEASE Config                  NO-ERROR.
RELEASE UserSession             NO-ERROR.
