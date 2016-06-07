/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnCronScheduler.p
Purpose : This file is called by the actual cron every minute. It reads CronConfig records and checks when the
          crnCronRun.p was last run. If there has been a delay (> 1 minute) since it last ran then it sets a flag
          logCronIsRunning = TRUE and then runs crnCronRun.p for all of the minutes between when it was last run 
          and the current time. Also caters for daylight saing and the clocks going backwards, if it sees the time
          going backwards it only runs recurring crons but skips those with an hour attached so avoids running 
          everything twice.
          
          Daylight saving....A one-hour shift occurs at 02:00 local time, in spring the clock jumps forward from 
          01:59 to 03:00 and that day has 23 hours, whereas in autumn the clock jumps backward 
          from 01:59 DST to 01:00 standard time, repeating that hour, and that day has 25 hours.
         
          Cloned from crnCronScheduler.p

Author  : BG
Date    : 13/10/2011
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03/12/2013 CS  CR1021     Added more error handling and check for inprocess CronEvents.
30/05/2014 CS  56727      Added call for crnReportRunMinute.p
18/07/2014 CS             Added logic to prevent looping intervals more than one day.
------------------------------------------------------------------------------------------------------------------------------------------*/
DEFINE VARIABLE chrCurrentTimestamp        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chrTimestampToRun          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE intMinutesFromLastRun      AS INTEGER    NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID        AS INTEGER    NO-UNDO.
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

FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "CronErrors" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

/* This holds the Previous minute that the cron was run. */
FIND FIRST CronConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE CronConfig THEN
DO:
   RUN osSendMail.p (INPUT "",                                       /* Optional list of Users */
                     INPUT "CronConfig is missing",                  /* Email Subject */
                     INPUT "CronConfig is missing sent from Cron Scheduler.", /* Plain text message Body */
                     INPUT "",                                       /* Html format message Body */
                     INPUT "",                                       /* File path../idev/hyperthermbr/files/file OR ../files/file */
                     INPUT intCronEmailGroupID,                      /* EmailGroupID that you want to send this to */
                     INPUT "").                                      /* File MasterID is it applies */

END. /* IF NOT AVAILABLE CronConfig */
                                   
/* If we're in the middle of BackDating Cron events after a maintenance window this will be set to TRUE 
   or if Cron is set as inactive                                                                        */
IF CronConfig.CronIsRunning OR
   CronConfig.Active = FALSE THEN
   RETURN.

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
   
   FIND FIRST CronConfig EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF NOT AVAILABLE CronConfig THEN
      RETURN.
   
   /* Set this to TRUE so that if the cron runs again while this is still in progress it will skip until we're done */
   CronConfig.CronIsRunning = TRUE.
   
   RUN crnCronRunMinute.p(INPUT chrCurrentTimestamp,
                          INPUT logSkipRecurringCrons,
                          INPUT logSkipNonRecurringCrons) NO-ERROR.

   RUN crnReportRunMinute.p(INPUT chrCurrentTimestamp,
                            INPUT logSkipRecurringCrons,
                            INPUT logSkipNonRecurringCrons) NO-ERROR.
   
   /* End of Cron Run */
   CronConfig.CronIsRunning = FALSE.
   
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
   
   FIND FIRST CronConfig EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF NOT AVAILABLE CronConfig THEN
      RETURN.
   
   /* Set this to TRUE so that if the cron runs again while this is still in progress it will skip until we're done */
   CronConfig.CronIsRunning = TRUE.
   
   /* If Cron more than a day old set Previous Run to Previous day to prevent long running processes after long inactive peroids */
   IF intMinutesFromLastRun > 1440 THEN
   DO: 
      CronConfig.PreviousRun = STRING(YEAR(TODAY - 1)) + 
                               STRING(MONTH(TODAY - 1),"99") + 
                               STRING(DAY(TODAY - 1),"99") + 
                               REPLACE(STRING(TIME,"hh:mm"),":","").
      
      /* Send an email to notify PreviousRun has changed */
      RUN osSendMail.p (INPUT "",                                       /* Optional list of Users */
                        INPUT "CronConfig PreviousRun has been set",    /* Email Subject */
                        INPUT "CronConfig PreviousRun was older than"
                                 + " a day and has been set to previous"
                                 + " day from Cron Scheduler.",         /* Plain text message Body */
                        INPUT "",                                       /* Html format message Body */
                        INPUT "",                                       /* File path../idev/hyperthermbr/files/file OR ../files/file */
                        INPUT intCronEmailGroupID,                      /* EmailGroupID that you want to send this to */
                        INPUT "").                                      /* File MasterID is it applies */                         
   END.
   
   /* Begin the backtime with one minute further on than the last Cron run */
   chrTimestampToRun = fNextMinute(CronConfig.PreviousRun).
   
   /* Loop through minutes from last run until current minute */
   DO WHILE chrTimestampToRun <= chrCurrentTimestamp:
     
      /* NO-ERROR here will isolate errors from this calling program so Cron won't get stuck */
      RUN crnCronRunMinute.p(INPUT chrTimestampToRun,
                             INPUT logSkipRecurringCrons,
                             INPUT logSkipNonRecurringCrons) NO-ERROR.

      RUN crnReportRunMinute.p(INPUT chrTimestampToRun,
                               INPUT logSkipRecurringCrons,
                               INPUT logSkipNonRecurringCrons) NO-ERROR.
   
      ASSIGN CronConfig.PreviousRun = chrTimestampToRun
             chrTimestampToRun      = fNextMinute(chrTimestampToRun).
      
   END. /* DO WHILE chrTimestampToRun < chrCurrentTimestamp: */
   
   /* End of Run */
   CronConfig.CronIsRunning = FALSE.
   
END. /* ELSE intMinutesFromLastRun > 1 THEN */

/* Release statements need to have gate database fully qualified because of syngate data sync */
RELEASE CronConfig       NO-ERROR.
RELEASE EmailGroup       NO-ERROR.
RELEASE gate.GateUser    NO-ERROR.       
