/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnBulkPartUploadNextel.p
Purpose : 
Author  : Maria Chereches
Date    : 4th Oct 2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
/* Standard Mandatory Includes */
{defSessionVariables.i}
{fncClassFunctions.i}
{fncGlobalFunctions.i}
{fncServerFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{fncStatusTypeFunctions.i}

/* Session Variables */
DEFINE VARIABLE intSsnCronEventRunID  AS sessionValue NO-UNDO.

/* Local Variables */
DEFINE VARIABLE chrEmailAddress     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrProgramName      AS CHARACTER NO-UNDO INITIAL "crnBulkPartUploadNextel.p".
DEFINE VARIABLE chrNewAgedDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLogFileDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLogFile          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intGateUserID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCronEmailGroupID AS INTEGER   NO-UNDO.

DEFINE VARIABLE chrUploadFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrImportedLine     AS CHARACTER NO-UNDO.
DEFINE VARIABLE intFileLineSequence AS INTEGER   NO-UNDO.

DEFINE VARIABLE intEmailGroupID     AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrEmailSubject     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEmailBody        AS CHARACTER NO-UNDO.

/* buffers */
DEFINE BUFFER otherUserSession      FOR UserSession.
DEFINE BUFFER otherUserSessionCrumb FOR UserSessionCrumb.

/* Streams */
DEFINE STREAM sToLogFile.
DEFINE STREAM sUploadFile.

/* Temp-tables */
DEFINE TEMP-TABLE ttFileLine
   FIELD FileLineNo      AS INTEGER
   FIELD LineString      AS CHARACTER
   FIELD PartRef         AS CHARACTER
   FIELD ExistingPartRef AS LOGICAL 
   FIELD ExistingLink    AS LOGICAL
   INDEX idxFileLineNo FileLineNo PartRef 
   INDEX idxPartRef    PartRef FileLineNo.

DEFINE TEMP-TABLE ttFileLineInfo LIKE FileMasterField
   FIELD DataType AS CHARACTER.

DEFINE TEMP-TABLE ttError
   FIELD LineNo AS INTEGER
   FIELD ColumnNo AS INTEGER
   FIELD Error AS CHARACTER
   INDEX LineColumn LineNo ColumnNo.

/* Functions */
FUNCTION fLog RETURNS CHARACTER(INPUT chrString AS CHARACTER):
   
   /* Output to LogFile the important steps of the process only if the global debugging is on */
   IF logGblDebugging THEN 
   PUT STREAM sToLogFile UNFORMATTED SUBSTRING(STRING(NOW),1,23) + " " + chrString SKIP.
   
END FUNCTION.


/* ******************** Main Block ******************** */

ASSIGN intSsnCronEventRunID = fGetSessionValue("CronEventRunID")
       chrNewAgedDirectory  = fGetAgedDirectory("../logs/", 2) NO-ERROR.
   
IF ERROR-STATUS:ERROR THEN
   RETURN ERROR "Could not setup variables "  + RETURN-VALUE.

IF chrNewAgedDirectory BEGINS "Error" THEN
   chrLogFileDirectory = "../logs/".
ELSE
   chrLogFileDirectory = chrNewAgedDirectory.

/* Set LogFile path and name */
chrLogFile = chrLogFileDirectory + "crnBulkPartUploadNextel_" + fDisplayDate&Time(fTimestamp(NOW),"d_m_y") + ".log".

/* Start logging */
OUTPUT STREAM sToLogFile TO VALUE(chrLogFile) APPEND.

fLog("Debug enabled now running CronEventRun: " + STRING(intSsnCronEventRunID:intValue)).

/* Validate CronEventRun found in session */
FIND FIRST CronEventRun NO-LOCK
   WHERE CronEventRun.CronEventRunID = intSsnCronEventRunID:intValue NO-ERROR.
IF NOT AVAILABLE CronEventRun THEN
   RETURN ERROR "CronEventRun missing for CronEventRunID: " + STRING(intSsnCronEventRunID:intValue).

/* set the user id */
intGateUserID  = CronEventRun.GateUserID.

/* Validate CronEventRun.GateUserID in the gate.GateUser table */
FIND FIRST gate.GateUser NO-LOCK
   WHERE gate.GateUser.GateUserID = intGateUserID NO-ERROR.
IF NOT AVAILABLE gate.GateUser THEN
   RETURN ERROR "GateUser missing for GateUserID: " + STRING(intGateUserID).

intGateUserID   = gate.GateUser.GateUserID.
chrEmailAddress = gate.GateUser.Email.

/* Find the Email Group for this process */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "BulkPartUpload" NO-ERROR.
IF AVAILABLE EmailGroup THEN
   intCronEmailGroupID = EmailGroup.EmailGroupID.
ELSE
   intCronEmailGroupID = 1.

fLog(chrProgramName + " UNIX PID: " + STRING(CronEventRun.UnixPID) + " starting to upload Part records from file: " + chrUploadFile).


/* ************ Start Uploading ************ */

INPUT STREAM sUploadFile FROM VALUE(chrUploadFile).

REPEAT:
   
   chrImportedLine = "".
   
   IMPORT STREAM sUploadFile UNFORMATTED chrImportedLine NO-ERROR.
   
   chrImportedLine = REPLACE(chrImportedLine, CHR(10), " ").
   
   intFileLineSequence = intFileLineSequence + 1.
   
   /* the first row in the file only represents column names */
   IF intFileLineSequence = 1 THEN NEXT.
   
   CREATE ttFileLine.
   ASSIGN ttFileLine.FileLineNo = intFileLineSequence
          ttFileLine.LineString = chrImportedLine.
END.

INPUT STREAM sUploadFile CLOSE.

/* *********** Finish Uploading ************ */

/* start validation */

/* "1. Processed by syncreon"  */
CustPart.ProcessedBySyncreon

/* "2. Oracle Part Number"  */
Part.PartRef
CustPart.OraclePartNumber

/* "3. Oracle Description"  */
CustPart.OracleDesc

/* "4. EAN"   */
Part.EanCode

/* "5. Manufacturer PN"   */
Part.VendorCode

/* "6. Maestro PN"    */
CustPart.MaestroCode

/* "7. Maestro Description"   */
CustPart.MaestroDescr

/* "8. Manufacturer"  */
Part.VendorName?

/* "9. Marketing Name"    */
Part.PartDescr

/* "10. Status (Life Cycle)"   */
CustPart.LifeCycleStatus

/* "11. Technology"  */
CustPart.Technology

/* "12. Type"    */
CustPart.TechnologyType

/* "13. Unitized by"  */
CustPart.UnitizedBy

/* "14. SIM Card Type"    */
CustPart.SimCardType

/* "15. IMEI Mask"    */
CustPart.IMEIMask

/* Product  */
/* "16. H (cm)"    */
Part.Height

/* "17. L (cm)"  */
Part.Depth

/* "18. W(cm)"  */
Part.Width

/* "19. Weight" */
Part.UnitWeight

/* finish validation */

chrEmailAddress = IF gate.GateUser.UserName <> "cron" THEN chrEmailAddress ELSE "". /* Optional Email List ELSE Users */
intEmailGroupID = IF gate.GateUser.UserName = "cron" THEN intCronEmailGroupID ELSE 0.
chrEmailSubject = "Cron Job ID " + STRING(CronEventRun.CronEventID) + " Upload successful ".
chrEmailBody    = "Program: " + chrProgramName + " has uploaded " + STRING("") + " Part records".

/* if upload finished succesfuly, send an email */
RUN osSendMail.p (INPUT chrEmailAddress,     
                  INPUT chrEmailSubject, /* Email Subject */
                  INPUT chrEmailBody,    
                  INPUT "",              /* Html format message Body */
                  INPUT "",              /* File path ../files/file */                    
                  INPUT intEmailGroupID, /* EmailGroupID want to send this to */
                  INPUT "").             /* File MasterID is it */

fLog("Finish").

/* Close logging */
OUTPUT STREAM sToLogFile CLOSE.

/* Clean-up */
DELETE OBJECT intSsnCronEventRunID NO-ERROR.

/* Releases */
RELEASE Part                   NO-ERROR.
RELEASE CustPart               NO-ERROR.
RELEASE CronEvent              NO-ERROR.
RELEASE CronEventRun           NO-ERROR.
RELEASE EmailGroup             NO-ERROR.

