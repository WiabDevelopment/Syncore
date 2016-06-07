/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncGlobalFunctions.i
Purpose : Character & web functions globally used
Author  : BG
Date    : 27th April 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
20.03.2014 MN  CR1049     Adding core datanase prefix for Data Migration Tool 
------------------------------------------------------------------------------------------------------------------------------------------*/

/* This include has a dependency on */
/* {defSessionVariables.i} */ 

/* Global Variable Defs*/
DEFINE VARIABLE chrLongMonths  AS CHARACTER INITIAL "January,February,March,April,May,June,July,August,September,October,November,December".
DEFINE VARIABLE chrShortMonths AS CHARACTER INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
DEFINE VARIABLE chrDaysOfWeek  AS CHARACTER INITIAL "Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday".

DEFINE TEMP-TABLE ttRefReplacement NO-UNDO
   FIELD ReplacementNo  AS INT
   FIELD TextToReplace  AS CHAR
   INDEX ReplacementNo IS UNIQUE PRIMARY ReplacementNo.

DEFINE TEMP-TABLE ttUser           NO-UNDO
   LIKE GateUser.


/* This will only work for SynCore obviously */
FIND core.Config NO-LOCK.


FUNCTION fTimestamp RETURNS CHARACTER (INPUT datDateToConvert AS DATETIME-TZ):
   
   DEFINE VARIABLE chrTimestamp     AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE chrDateToConvert AS CHARACTER     NO-UNDO.
   
   ASSIGN chrDateToConvert = STRING(datDateToConvert).
   
   ASSIGN chrTimestamp = STRING(SUBSTRING(chrDateToConvert, 7,4),"9999") +
                         STRING(SUBSTRING(chrDateToConvert, 4,2),  "99") +
                         STRING(SUBSTRING(chrDateToConvert, 1,2),  "99") + 
                         STRING(SUBSTRING(chrDateToConvert,12,2),  "99") +
                         STRING(SUBSTRING(chrDateToConvert,15,2),  "99") +
                         STRING(SUBSTRING(chrDateToConvert,18,2),  "99") +
                         STRING(SUBSTRING(chrDateToConvert,21,3), "999").
   
   RETURN chrTimeStamp.
   
END FUNCTION. /*fTimestamp*/


/* Shorthand for fTranslate() */
FUNCTION fTL RETURNS CHARACTER (INPUT chrStringToTranslate AS CHARACTER):
   
   DEFINE VARIABLE intCount             AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intNumReplacements   AS INTEGER     NO-UNDO.
   DEFINE VARIABLE chrTextToReplace     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrTranslatedString  AS CHARACTER   NO-UNDO.

   IF TRIM(chrStringToTranslate) = "" THEN
      RETURN "".
   
   EMPTY TEMP-TABLE ttRefReplacement.
   
   /* Replace any untranslateable reference data here e.g. "StockPackage of Ref:[Pal009].." becomes "StockPackage of Ref:(1).." */
   intNumReplacements = NUM-ENTRIES(chrStringToTranslate,"[").
   
   ReplacementLoop:
   DO intCount = 1 TO intNumReplacements:
      
      IF NUM-ENTRIES(chrStringToTranslate, "[") < 2 THEN
         LEAVE ReplacementLoop.
      
      chrTextToReplace = ENTRY(2, chrStringToTranslate, "[").
      chrTextToReplace = ENTRY(1, chrTextToReplace,"]").
      chrStringToTranslate = REPLACE(chrStringToTranslate, "[" + chrTextToReplace + "]", "(" + STRING(intCount) + ")").
      
      CREATE ttRefReplacement.
      ASSIGN ttRefReplacement.ReplacementNo = intCount
             ttRefReplacement.TextToReplace = chrTextToReplace.
   END.
   
   /* First find an existing Translation Source for the string and then try to find a TranslationResult in the Language of the User*/
   FIND FIRST TranslationSource NO-LOCK /*idx=EnglishString*/
      WHERE TranslationSource.EnglishString = TRIM(chrStringToTranslate) NO-ERROR.
   IF AVAILABLE TranslationSource THEN
   DO:
      /* intGblLanguageID is defined in defSessionVariables.i and is set in either usrLogin.p OR the HTMLPage.cls */
      FIND FIRST TranslationResult OF TranslationSource NO-LOCK /*idx=SourceLanguage*/
         WHERE TranslationResult.LanguageID = intGblLanguageID NO-ERROR. 
      IF AVAILABLE TranslationResult THEN
         chrTranslatedString = TranslationResult.TranslationString.
      ELSE
         ERROR-STATUS:ERROR = FALSE. /* Need to clear this before we exit */
   END.
   
   /* If we can't find a source then create one now so it can be Translated for future use */
   IF NOT AVAILABLE TranslationSource THEN
   DO:
      ERROR-STATUS:ERROR = FALSE. /* Need to clear this before we exit */
      
      IF core.Config.CreateTranslations THEN 
      DO TRANSACTION ON ERROR UNDO, LEAVE:
         
         CREATE TranslationSource.
         ASSIGN TranslationSource.EnglishString        = TRIM(chrStringToTranslate)
                TranslationSource.MaxTranslationLength = 0 /* Not worried about Length of output */
                TranslationSource.TranslationSourceID  = NEXT-VALUE(TranslationSource,gate).
         
         RELEASE TranslationSource NO-ERROR.
      END.
   END. /*IF NOT AVAILABLE TranslationSource THEN*/
   
   IF chrTranslatedString = "" THEN
      chrTranslatedString = chrStringToTranslate.
   
   /* Now put the original reference data back into the Translated text string in the correct positions */
   FOR EACH ttRefReplacement:
      
      chrTranslatedString = REPLACE(chrTranslatedString,
                                    "(" + STRING(ttRefReplacement.ReplacementNo) + ")",
                                    ttRefReplacement.TextToReplace).
   END.
   
   RETURN chrTranslatedString.
   
END FUNCTION. /* FUNCTION fTL RETURNS CHARACTER (INPUT chrStringToTranslate AS CHARACTER) */


/* Shorthand for fTranslateLength() */
FUNCTION fTLL RETURNS CHARACTER (INPUT chrStringToTranslate AS CHARACTER,
                                 INPUT intMaxStringLength   AS INTEGER):
   
   IF TRIM(chrStringToTranslate) = "" THEN
      RETURN "".
   
   IF intMaxStringLength = 0 THEN
      intMaxStringLength = LENGTH(chrStringToTranslate).
   
   DEFINE VARIABLE chrTextToReplace     AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrTranslatedString  AS CHARACTER   NO-UNDO.
   
   /* First find an existing Translation Source for the string and then try to find a TranslationResult in the Language of the User*/
   FIND FIRST TranslationSource NO-LOCK
      WHERE TranslationSource.EnglishString = TRIM(chrStringToTranslate) NO-ERROR.
   IF AVAILABLE TranslationSource THEN
   DO:
      /* intGblLanguageID is defined in defSessionVariables.i and is set in either usrLogin.p OR the HTMLPage.cls */
      FIND FIRST TranslationResult OF TranslationSource NO-LOCK
         WHERE TranslationResult.LanguageID = intGblLanguageID NO-ERROR.
      IF AVAILABLE TranslationResult THEN
         chrTranslatedString = TranslationResult.TranslationString.
      ELSE
         ERROR-STATUS:ERROR = FALSE. /* Need to clear this before we exit */
   END.
   
   /* If we can't find a source then create one now so it can be Translated for future use */
   IF NOT AVAILABLE TranslationSource THEN
   DO:
      /* Need to clear this before we exit */
      ERROR-STATUS:ERROR = FALSE.
      
      IF core.Config.CreateTranslations THEN 
      DO TRANSACTION ON ERROR UNDO, LEAVE:
         
         /*CREATE TranslationSource.
         ASSIGN TranslationSource.EnglishString        = TRIM(chrStringToTranslate)
                TranslationSource.MaxTranslationLength = intMaxStringLength
                TranslationSource.TranslationSourceID  = NEXT-VALUE(TranslationSource,gate).*/
         
         RELEASE TranslationSource NO-ERROR.
      END.
   END. /*IF NOT AVAILABLE TranslationSource THEN*/
   
   IF chrTranslatedString = "" THEN
      chrTranslatedString = chrStringToTranslate.
   
   RETURN chrTranslatedString.
   
END FUNCTION. /* FUNCTION fTL RETURNS CHARACTER (INPUT chrStringToTranslate AS CHARACTER) */


/* Character only - Translates all the Widgets on the gun screen */
FUNCTION fTranslateWidget RETURNS CHARACTER (INPUT hdlParent AS WIDGET-HANDLE):
   
   DEFINE VARIABLE hdlChild AS HANDLE    NO-UNDO.
   
   hdlChild = hdlParent:FIRST-CHILD NO-ERROR.
   IF NOT VALID-HANDLE(hdlChild) THEN
   DO:
      hdlParent:LABEL = fTLL(hdlParent:LABEL, LENGTH(hdlParent:LABEL)).
      RETURN "".
   END.
   
   hdlParent:TITLE = fTLL(hdlParent:TITLE, LENGTH(hdlParent:TITLE)) NO-ERROR.
   
   ChildLoop:
   REPEAT:
      
      IF hdlChild:TYPE = "FIELD-GROUP" THEN
      DO:
         hdlChild = hdlChild:FIRST-CHILD.
         NEXT ChildLoop.
      END.      
     
      CASE hdlChild:TYPE:
         
         WHEN "FILL-IN" THEN 
         DO:
            IF hdlChild:SIDE-LABEL-HANDLE <> ? THEN 
               hdlChild:LABEL = fTLL(hdlChild:LABEL, LENGTH(hdlChild:LABEL)) NO-ERROR.   
         END.
         
         WHEN "BUTTON" OR WHEN "COMBO-BOX" THEN 
         DO:
            hdlChild:LABEL = fTLL(hdlChild:LABEL, LENGTH(hdlChild:LABEL)) NO-ERROR.   
         END.
         
         WHEN "LITERAL" THEN 
         DO:
            hdlChild:SCREEN-VAL = fTLL(hdlChild:SCREEN-VAL, LENGTH(hdlChild:SCREEN-VAL)) NO-ERROR.   
         END. 
         
      END CASE.
      
      hdlChild = hdlChild:NEXT-SIBLING.
      
      IF NOT VALID-HANDLE(hdlChild) THEN
         RETURN "".
      
   END. /*ChildLoop: REPEAT: */
   
END FUNCTION. /*fTranslateWidget*/


/* This is character only */
FUNCTION fWebLockCheck RETURNS LOGICAL(INPUT chrTableName AS CHARACTER,
                                       INPUT rowTableID   AS ROWID):
   
   DEFINE VARIABLE chrLockError AS CHARACTER NO-UNDO.
   
   /* Check for Web Locks */
   FIND FIRST ssnWebRecordLock NO-LOCK /*idx=TableRowIDCompleted*/
      WHERE ssnWebRecordLock.TableName   = chrTableName
      AND   ssnWebRecordLock.RecordRowID = STRING(rowTableID)
      AND   ssnWebRecordLock.Completed   = ""   NO-ERROR.
   IF AVAILABLE ssnWebRecordLock THEN 
   DO:
      /* Must specify the field to use to link these here as its in the temp Db with different Indexes */
      FIND FIRST ssnWebLock NO-LOCK
         WHERE ssnWebLock.WebLockID = ssnWebRecordLock.WebLockID NO-ERROR.
      FIND FIRST GateUser NO-LOCK
         WHERE GateUser.GateUserID = ssnWebLock.GateUserID NO-ERROR.
      
      chrLockError = "[" + chrTableName + "] Record is Locked by " + (IF AVAILABLE GateUser THEN "[" + GateUser.FullName + "]" 
                        ELSE " another User") + ". Please Refresh your screen and then try again.".
      
      /* Clean up finds just in case */
      RELEASE ssnWebRecordLock NO-ERROR.
      RELEASE ssnWebLock       NO-ERROR.
      RELEASE GateUser         NO-ERROR.
      
      RUN DisplayMessage("Web User Lock", chrLockError).
      
   END.
   
END FUNCTION. /*fWebLockCheck*/

FUNCTION fCanViewBusinessUnit RETURNS LOGICAL (INPUT intUserSessionID AS INTEGER,
                                               INPUT intBusinessUnitID AS INTEGER):
   
   IF intBusinessUnitID = 0 THEN
      RETURN TRUE.
   ELSE
      RETURN CAN-FIND(FIRST UserSessionBusUnitLink NO-LOCK 
                      WHERE UserSessionBusUnitLink.UserSessionID = intUserSessionID 
                      AND UserSessionBusUnitLink.BusinessUnitID = intBusinessUnitID).                                                
   
   /* If we need real-time checks then more code should be added here 
      to see if user is still belonging to BusinessUnit etc */

END FUNCTION. /* fCanViewBusinessUnit */                                              


FUNCTION fGetUserList RETURNS CHAR (INPUT intApplicationID            AS INTEGER,
                                    INPUT logInternalUsersOnly        AS LOGICAL,
                                    INPUT logExcludeMultipleAppGroups AS LOGICAL):
   
   EMPTY TEMP-TABLE ttUser.
   
   AppLoop:
   FOR EACH Application NO-LOCK:
      
      IF intApplicationID <> 0 AND Application.ApplicationID <> intApplicationID THEN
         NEXT AppLoop.
      
      AccessLoop:
      FOR EACH AccessAppLink OF Application NO-LOCK,
         EACH AccessGroup OF AccessAppLink NO-LOCK:
         
         IF logExcludeMultipleAppGroups AND AccessGroup.SpansMultipleApps THEN
            NEXT AccessLoop.
         
         UserLoop:
         FOR EACH AccessUserLink NO-LOCK 
            WHERE AccessUserLink.AccessGroupID = AccessAppLink.AccessGroupID,
               EACH GateUser OF AccessUserLink NO-LOCK
                  WHERE GateUser.Active
                  BY    GateUser.FullName:
                  
                  IF logInternalUsersOnly = TRUE AND GateUser.AccessType = "EXTERNAL" THEN
                     NEXT UserLoop.
                  
                  IF NOT CAN-FIND(FIRST ttUser WHERE ttUser.GateUserID = GateUser.GateUserID) THEN
                  DO:
                     CREATE ttUser.
                     BUFFER-COPY GateUser TO ttUser.
                  END.
         END. /*UserLoop: FOR EACH AccessUserLink NO-LOCK */
      END. /*AccessLoop: FOR EACH AccessAppLink OF Application NO-LOCK,*/
   END. /*AppLoop: FOR EACH Application NO-LOCK:*/
   
END FUNCTION. /* FunctionName */


