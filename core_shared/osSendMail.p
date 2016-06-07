/*------------------------------------------------------------------------------------------------------------------------------------------
Program : osSendMail.p
Purpose : Sends emails from the OS to email groups or lists of users or a single user with or without attachment
Author  : BG
Date    : 6th July 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
03/12/2013 MC  WiaB       Excluded non syncreon Email addresses from email lists if we're not in the LIVE environment
06/01/2014 CS  CR1021     Fully qualified gate database for use with syngate and gate databases.
15/01/2014 CS  CR1021     Added replace asterisks statements and removed debugging.
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER chrUserList        AS CHARACTER. /* Comma seperated list of e-mail addresses */
DEFINE INPUT PARAMETER chrEmailSubject    AS CHARACTER. 
DEFINE INPUT PARAMETER chrPlainTextBody   AS CHARACTER. /* Plain text body "Hello World" */
DEFINE INPUT PARAMETER chrHtmlBody        AS CHARACTER. /* Html body "<p style=font-size;12px;>Hello World</p>" */
DEFINE INPUT PARAMETER chrFileToAttach    AS CHARACTER. /* Either full OR relative path of file to attach e.g. "/etc/hosts" OR "../hosts" */
DEFINE INPUT PARAMETER intEmailGroupID    AS INTEGER.   /* Optional - if you want to send e-mail to a system EmailGroup */
DEFINE INPUT PARAMETER intFileMasterID    AS INTEGER.   /* Optional FileMasterID - to help trace bad Email group links */

/* Includes */
{defSessionVariables.i}
{fncServerFunctions.i}

/*---------------------------------------------------------------------------------------------------------------------------------------
   The sendmail.sh (in the scripts folder) accepts the following Parameters.
   The full string (apart from the initial ../scripts/sendmail.sh) must be enclosed in double quotes and seperated by hashes #.
   
   PARAMETER 1: Comma seperated list of e-mail addresses e.g. jen.ryan@syncreon.com,bryan.gilsenan@syncreon.com
   PARAMETER 2: E-mail Subject
   PARAMETER 3: Plain text e-mail body e.g. testing 123
   PARAMETER 4: Full or relative path for a file to attach e.g. /idev/idev/hyperthermbr/files/filename OR ../files/filename
   PARAMETER 5: Sender e.g. HyperthermBR_DEV@syncreon.com
   
   'sendmail.sh "jenn.ryan@syncreon.com,bryan.gilsenan@syncreon.com#Test Subject#Test Body#../files/filename#hyperthermWMS@syncreon.com"'
---------------------------------------------------------------------------------------------------------------------------------------*/

/* Vars */
DEFINE VARIABLE chrUnixCommand              AS CHARACTER   NO-UNDO INITIAL '../scripts/sendmail.sh "'.
DEFINE VARIABLE logAtLeastOneUser           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE chrSenderAddress            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDelimiter                AS CHARACTER   NO-UNDO INITIAL "#".
DEFINE VARIABLE hdlCallingProg              AS HANDLE      NO-UNDO.
DEFINE VARIABLE chrCallingProg              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrEmailEnvironment         AS CHARACTER   NO-UNDO.


/* Buffers */
/* gate database had to fully qualified because this can run with syngate and gate databases connected */
DEFINE BUFFER GateUser   FOR gate.GateUser.
DEFINE BUFFER EmailGroup FOR EmailGroup.

FIND Config NO-LOCK.

/* Need to strip out semi-hashes as they will corrupt the unix email script - replace with commas */
/* Need to strip out the asterisks and replace with spaces */
ASSIGN chrUserList      = REPLACE(chrUserList,"#",":")
       chrUserList      = REPLACE(chrUserList,"*"," ")
       chrEmailSubject  = REPLACE(chrEmailSubject,"#",":")
       chrEmailSubject  = REPLACE(chrEmailSubject,"*"," ")
       chrPlainTextBody = REPLACE(chrPlainTextBody,"#",":")
       chrPlainTextBody = REPLACE(chrPlainTextBody,"*"," ")
       chrHtmlBody      = REPLACE(chrHtmlBody,"#",":")
       chrHtmlBody      = REPLACE(chrHtmlBody,"*"," ").

/* usrSession.i and fncServerFunctions.i must be inclused in calling program - this will likely already be set but just in case */
chrEmailEnvironment = REPLACE(fGetCurrentEnvironment(),"LIVE","WMS").

/* Sender should be a combination of the System Name + the Environment e.g. HyperthermBR_DEV@syncreon.com */
chrSenderAddress = REPLACE(Config.SystemName + "_" + chrEmailEnvironment," ","") + "@syncreon.com".

/* If there is a Group coming in then we try to send it to the group first */
IF intEmailGroupID <> 0 THEN
DO:
   FIND FIRST EmailGroup NO-LOCK 
      WHERE EmailGroup.EmailGroupID = intEmailGroupID NO-ERROR.
   /* If valid Group then check that there is at least one valid User linked to the Group */
   IF AVAILABLE EmailGroup THEN
   DO:
      CheckUserBlk:
      FOR EACH EmailGroupUserLink OF EmailGroup NO-LOCK 
         WHERE EmailGroupUserLink.Active,
         EACH GateUser OF EmailGroupUserLink NO-LOCK 
         WHERE GateUser.Active:
         
         logAtLeastOneUser = TRUE.
         LEAVE CheckUserBlk.
      END. /*FOR EACH EmailGroupUserLink OF EmailGroup NO-LOCK WHERE EmailGroupUserLink.Active,*/
   END. /*IF AVAILABLE EmailGroup THEN*/
   
   IF NOT logAtLeastOneUser THEN
   DO:
      /* Get the calling Proc */
      hdlCallingProg = THIS-PROCEDURE:INSTANTIATING-PROCEDURE NO-ERROR.
      chrCallingProg = hdlCallingProg:NAME NO-ERROR.
      
      FIND FIRST FileMaster NO-LOCK WHERE FileMaster.FileMasterID = intFileMasterID NO-ERROR.
      
      IF NOT AVAILABLE EmailGroup THEN
      DO:
         chrEmailSubject  = "/*** Email directed to Invalid Group ***/   " + chrEmailSubject.
         chrPlainTextBody = "Email directed to Invalid Group:" + STRING(intEmailGroupID) + "   Prog:" + chrCallingProg + "   " 
                               + (IF AVAILABLE FileMaster THEN "FileMaster:" + FileMaster.MasterName ELSE "")
                               + CHR(13) + CHR(13) + chrPlainTextBody.
      END.
      ELSE
      DO:
         chrEmailSubject  = "/*** No Active User linked to Group ***/   " + chrEmailSubject.
         chrPlainTextBody = "No Active User linked to Group:" + STRING(intEmailGroupID) + "   Prog:" + chrCallingProg + "   " 
                               + (IF AVAILABLE FileMaster THEN "FileMaster:" + FileMaster.MasterName ELSE "")
                               + CHR(13) + CHR(13) + chrPlainTextBody.
      END.
      
      FIND GateUser NO-LOCK WHERE GateUser.GateUSerID = intGblUserID NO-ERROR.
      
      /* If running off the cron  */
      IF SESSION:BATCH-MODE OR NOT AVAILABLE GateUser THEN
      DO:
         FIND EmailGroup WHERE GroupName = "SuperUsers" NO-LOCK.
         
         RUN osSendMail.p (INPUT "",
                           INPUT chrEmailSubject,
                           INPUT chrPlainTextBody,
                           INPUT chrHtmlBody,
                           INPUT chrFileToAttach,
                           INPUT EmailGroup.EmailGroupID,
                           INPUT intFileMasterID).
         LEAVE.
      END. /*IF SESSION:BATCH-MODE OR NOT AVAILABLE GateUser THEN*/
      
      /* No User sent in and not running in Batch mode so just send error the current User who is logged in */
      chrUnixCommand = chrUnixCommand + 
                       GateUser.Email   + chrDelimiter + 
                       chrEmailSubject  + chrDelimiter + 
                       chrPlainTextBody + chrDelimiter +
                       /*chrHtmlBody      + chrDelimiter +*/
                       chrFileToAttach  + chrDelimiter +
                       chrSenderAddress + '"'.
      
      UNIX SILENT VALUE(chrUnixCommand).
      
      LEAVE.
   END. /*IF NOT logAtLeastOneUser THEN*/
   
   /* We have a valid group so go through all the Users linked to the group and send the mail */
   chrUserList = "".
   UserLoop:
   FOR EACH EmailGroupUserLink OF EmailGroup NO-LOCK 
      WHERE EmailGroupUserLink.Active,
      EACH  GateUser OF EmailGroupUserLink NO-LOCK 
      BY    GateUser.Surname:

      IF chrEmailEnvironment <> "WMS" AND NUM-ENTRIES(GateUser.Email, "@") > 1 AND ENTRY(2, GateUser.Email, "@") <> "syncreon.com" THEN
         NEXT UserLoop.

      chrUserList = chrUserList + GateUser.Email + ",".
   END.
   chrUserList = TRIM(chrUserList,",").
   
   chrUnixCommand = chrUnixCommand   + 
                    chrUserList      + chrDelimiter +
                    chrEmailSubject  + chrDelimiter +
                    "Email Group:" + TRIM(EmailGroup.GroupName) + CHR(13) + CHR(13) + chrPlainTextBody + chrDelimiter +
                    /*chrHtmlBody      + chrDelimiter +*/
                    chrFileToAttach  + chrDelimiter +
                    chrSenderAddress + '"'.
   
   UNIX SILENT VALUE(chrUnixCommand).
   
   LEAVE.
END. /*IF intEmailGroupID <> 0 THEN*/

IF chrUserList = "" THEN
DO:
   
   chrEmailSubject  = "/*** Email directed to Invalid Group and Missing User Email ***/   " + chrEmailSubject.
    
   FIND FIRST EmailGroup NO-LOCK
      WHERE GroupName = "SuperUsers" NO-ERROR.
   IF AVAILABLE EmailGroup THEN
   DO:
      RUN osSendMail.p (INPUT "",
                        INPUT chrEmailSubject,
                        INPUT chrPlainTextBody,
                        INPUT chrHtmlBody,
                        INPUT chrFileToAttach,
                        INPUT EmailGroup.EmailGroupID,
                        INPUT intFileMasterID).
      LEAVE.
   END. /*IF AVAILABLE EmailGroup THEN*/
END. /*IF chrUserList = "" THEN*/

/* No group was sent in so we just send to the user list that was passed in */
chrUnixCommand = chrUnixCommand + 
                 chrUserList      + chrDelimiter + 
                 chrEmailSubject  + chrDelimiter + 
                 chrPlainTextBody + chrDelimiter +
                 /*chrHtmlBody      + chrDelimiter +*/
                 chrFileToAttach  + chrDelimiter +
                 chrSenderAddress + '"'.

UNIX SILENT VALUE(chrUnixCommand).
      

