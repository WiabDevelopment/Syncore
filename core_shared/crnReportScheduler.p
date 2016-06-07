/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnReportScheduler.p
Purpose : This program will be used to call the crnReportRunMinute.p.  This file is called by the Cron Scheduler.  CronConfig locking 
          happens in the crnCronScheduler.p.           
Author  : Christopher Shelley
Date    : 15/05/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
DEFINE VARIABLE chrCurrentTimestamp        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrTimestampToRun          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE intMinutesFromLastRun      AS INTEGER    NO-UNDO.
DEFINE VARIABLE intReportEmailGroupID      AS INTEGER    NO-UNDO.
DEFINE VARIABLE logSkipRecurringCrons      AS LOGICAL    NO-UNDO.
DEFINE VARIABLE logSkipNonRecurringCrons   AS LOGICAL    NO-UNDO.

/* Set these as early as possible - we only have a minute */
chrCurrentTimestamp = STRING(YEAR(TODAY)) + 
                      STRING(MONTH(TODAY),"99") + 
                      STRING(DAY(TODAY),"99") + 
                      REPLACE(STRING(TIME,"hh:mm"),":","").
 
/******************************************* Functions **********************************************************/
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncDateFunctions.i}
{fncServerFunctions.i}

/******************************************* Main Block *********************************************************/

IF SESSION:BATCH-MODE THEN
DO:
   FIND FIRST gate.GateUser NO-LOCK
      WHERE gate.GateUser.Username = "cron" NO-ERROR.
   {usrCreateSession.i}
END. /* IF SESSION:BATCH-MODE */

/* This holds the Previous minute that the cron was run. */
FIND FIRST CronConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE CronConfig THEN
DO:
   FIND FIRST EmailGroup NO-LOCK
      WHERE EmailGroup.GroupCode = "ReportErrors" NO-ERROR.
   IF AVAILABLE EmailGroup THEN
      intReportEmailGroupID = EmailGroup.EmailGroupID.
   ELSE
      intReportEmailGroupID = 1.

   RETURN.
END. /* IF NOT AVAILABLE CronConfig */

intMinutesFromLastRun = fInterval(INPUT chrCurrentTimestamp,
                                  INPUT CronConfig.PreviousRun,
                                  INPUT "minutes").

IF intMinutesFromLastRun < 1 THEN
DO:
   /* Check for the interval since the last Cron was run. If its <= normal interval then just loop through the    */
   /* minutes from the last run to the current minute. If interval is < 1 then the clock has gone back so we      */
   /* don't want to rerun crons that have a particular hour assigned and have already run but we should still run */
   /* recurring crons (no hour assigned) in case Ops are working night shift and need them.                       */
   
   /* Skip non recurring Crons but run recurring ones */
   ASSIGN logSkipRecurringCrons    = FALSE
          logSkipNonRecurringCrons = TRUE.
   
   
   RUN crnReportRunMinute.p(INPUT chrCurrentTimestamp,
                            INPUT logSkipRecurringCrons,
                            INPUT logSkipNonRecurringCrons) NO-ERROR.
   
END. /*IF intMinutesFromLastRun < 1 THEN*/
ELSE
DO:
   /* Check for the interval since the last Cron was run. If its <= normal interval then just loop through the      */
   /* minutes from the last run to the current minute. If the interval is larger than normal then this is because   */
   /* of Db maintenance downtime, daylight saving or a Db or Server crash. In these cases do the same loop through  */
   /* the minutes but skip any crons that are recurring - i.e. only run those that have a particular hour assigned. */
   
   /* Only skip recurring Crons if the interval is larger than expected */
   logSkipRecurringCrons = (intMinutesFromLastRun > CronConfig.CronInterval).
   
   /* Always FALSE in this loop */
   logSkipNonRecurringCrons = FALSE.
   
   /* Begin the backtim with one minute further on than the last Cron run */
   chrTimestampToRun = fNextMinute(CronConfig.PreviousRun).
   
   /* Loop through minutes from last run until current minute */
   DO WHILE chrTimestampToRun <= chrCurrentTimestamp:
     
      /* NO-ERROR here will isolate errors from this calling program so Cron won't get stuck */
      RUN crnReportRunMinute.p(INPUT chrTimestampToRun,
                               INPUT logSkipRecurringCrons,
                               INPUT logSkipNonRecurringCrons) NO-ERROR.
      
      chrTimestampToRun = fNextMinute(chrTimestampToRun).
      
   END. /* DO WHILE chrTimestampToRun < chrCurrentTimestamp: */
   
END. /* ELSE intMinutesFromLastRun > 1 THEN */

/* Release statements need to have gate database fully qualified because of syngate data sync */
RELEASE CronConfig       NO-ERROR.
RELEASE EmailGroup       NO-ERROR.
RELEASE gate.GateUser    NO-ERROR.
