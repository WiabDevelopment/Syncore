&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: 

  Description: 
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
     
  Changes: 02/11/2015  ML  Adjusted logic for filtered action point.

------------------------------------------------------------------------*/
/*           This .W file was created with the Progress AppBuilder.     */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Custom Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{fncLoggingFunctions.i}
{fncStatusTypeFunctions.i}

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedQuestionnaireInstance    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectQuestionnaireRow           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionnaireRow         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupAnswer                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrQuestionnaireInstanceID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredQuestionnaireRef         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredOrderRef                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredLocationRef              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredStatus                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredRecords                  AS CHARACTER NO-UNDO INITIAL 500.
DEFINE VARIABLE logFiltering                        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrSelectedDate                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE intDisplayCount                     AS INTEGER   NO-UNDO.
DEFINE VARIABLE intAvailableStatus                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE intFilteredRecords                  AS INTEGER   NO-UNDO.

/* Definitions for System Options for Receiving */
{getReceivingOptions.i}

/* Objects */
DEFINE BUFFER bufQuestionnaire FOR Questionnaire. 

/* Main Browse */
DEFINE VARIABLE QuestionnaireBrowseFrame             AS pageFrame.
DEFINE VARIABLE QuestionnaireBrowse                  AS browseTable.
DEFINE VARIABLE QuestionnaireBrowseButtons           AS buttonBar.
DEFINE VARIABLE QuestionnaireDetailsForm             AS dataForm.
DEFINE VARIABLE QuestionnaireDetailsButtons          AS buttonBar.

/* Filter */
DEFINE VARIABLE QuestionnaireFilterForm              AS dataForm.
DEFINE VARIABLE QuestionnaireFilterButtons           AS buttonBar.

/* Contents Browse */
DEFINE VARIABLE AnswerBrowse                         AS browseTable.
DEFINE VARIABLE AnswerBrowseForm                     AS dataForm.
DEFINE VARIABLE AnswerBrowseButtons                  AS buttonBar.

/* Contents Browse */
DEFINE VARIABLE QuestionnaireHistoryBrowse          AS browseTable.
DEFINE VARIABLE QuestionnaireHistoryBrowseForm      AS dataForm.
DEFINE VARIABLE QuestionnaireHistoryBrowseButtons   AS buttonBar.

DEFINE STREAM strLog.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 19.43
         WIDTH              = 67.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{src/web2/wrap-cgi.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ************************  Main Code Block  *********************** */

  RUN process-web-request.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-outputHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE outputHeader Procedure 
PROCEDURE outputHeader :
/*------------------------------------------------------------------------------
  Purpose:     Output the MIME header, and any "cookie" information needed 
               by this procedure.  
  Parameters:  <none>
  Notes:       In the event that this Web object is state-aware, this is
               a good place to set the webState and webTimeout attributes.
------------------------------------------------------------------------------*/

  /* To make this a state-aware Web object, pass in the timeout period 
   * (in minutes) before running outputContentType.  If you supply a timeout 
   * period greater than 0, the Web object becomes state-aware and the 
   * following happens:
   *
   *   - 4GL variables webState and webTimeout are set
   *   - a cookie is created for the broker to id the client on the return trip
   *   - a cookie is created to id the correct procedure on the return trip
   *
   * If you supply a timeout period less than 1, the following happens:
   *
   *   - 4GL variables webState and webTimeout are set to an empty string
   *   - a cookie is killed for the broker to id the client on the return trip
   *   - a cookie is killed to id the correct procedure on the return trip
   *
   * Example: Timeout period of 5 minutes for this Web object.
   *
   *   setWebState (5.0).
   */
    
  /* 
   * Output additional cookie information here before running outputContentType.
   *      For more information about the Netscape Cookie Specification, see
   *      http://home.netscape.com/newsref/std/cookie_spec.html  
   *   
   *      Name         - name of the cookie
   *      Value        - value of the cookie
   *      Expires date - Date to expire (optional). See TODAY function.
   *      Expires time - Time to expire (optional). See TIME function.
   *      Path         - Override default URL path (optional)
   *      Domain       - Override default domain (optional)
   *      Secure       - "secure" or unknown (optional)
   * 
   *      The following example sets cust-num=23 and expires tomorrow at (about) the 
   *      same time but only for secure (https) connections.
   *      
   *      RUN SetCookie IN web-utilities-hdl 
   *        ("custNum":U, "23":U, TODAY + 1, TIME, ?, ?, "secure":U).
   */ 
   
  output-content-type ("text/html":U).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-pGetSystemOptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSystemOptions Procedure 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-pInboundBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundBrowse Procedure 
PROCEDURE pQuestionnaireBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionnaire_browse_form"}
   
   QuestionnaireBrowse = NEW browseTable("questionnaire_browse").
   QuestionnaireBrowse:BrowseWidth  = 965.
   QuestionnaireBrowse:BrowseHeight = 455.
   QuestionnaireBrowse:ExcelExport  = TRUE.
   QuestionnaireBrowse:SessionID    = intGblSessionID.
   QuestionnaireBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QuestionnaireBrowse:insertColumn(fTL("ID"), 50, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
/*   {webGetOptionalBrowseHeaders.i questionnaire}*/
   
   QuestionnaireBrowse:insertColumn(fTL("Questionnaire"),   130, "INTEGER",   "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Identifier"),      120, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Part"),            120, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Operation Type"),  120, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("User"),            130, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Started"),         130, "DATE",      "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Completed"),       135, "DATE",      "left", FALSE).
   
   /*Body*/
   QuestionnaireBrowse:startBody().
   
   intAvailableStatus = fGetStatusID("Questionnaire","Available").
/*   ASSIGN intFilteredRecords = INTEGER(chrFilteredRecords) NO-ERROR.*/
/*   IF ERROR-STATUS:ERROR THEN*/
/*   DO:                                                                                                                                          */
/*      intFilteredRecords = 0.                                                                                                                   */
/*   END.                                                                                                                                         */
/*   intDisplayCount = 0.                                                                                                                         */
/*   IF logFiltering THEN                                                                                                                         */
/*   DO:                                                                                                                                          */
/*      FilterLoop:                                                                                                                               */
/*      FOR EACH Questionnaire NO-LOCK:                                                                                                           */
/*         IF intDisplayCount >= intFilteredRecords THEN LEAVE FilterLoop.                                                                        */
/*         FIND FIRST QuestionnaireLineJourney NO-LOCK                                                                                            */
/*            WHERE QuestionnaireLineJourney.QuestionnaireID = Questionnaire.QuestionnaireID AND QuestionnaireLineJourney.Completed = "" NO-ERROR.*/
/*         FIND FIRST TaskLineWorkQuestionnaireLink WHERE TaskLineWorkQuestionnaireLink.QuestionnaireID =  Questionnaire.QuestionnaireID          */
/*                                           AND TaskLineWorkQuestionnaireLink.Completed = "" NO-LOCK NO-ERROR.                                   */
/*         FIND FIRST TaskLineWork WHERE TaskLineWork.TaskLineWorkID = TaskLineWorkQuestionnaireLink.TaskLineWorkID NO-LOCK NO-ERROR.             */
/*         FIND FIRST Shiporder WHERE ShipOrder.ShipOrderID = TaskLineWork.ShipOrderID NO-LOCK NO-ERROR.                                          */
/*         FIND FIRST Location   WHERE Location.LocationID = Questionnaire.LocationID NO-LOCK NO-ERROR.                                           */
/*         FIND FIRST QuestionnaireStatus WHERE QuestionnaireStatus.QuestionnaireStatusID = Questionnaire.QuestionnaireStatusID NO-LOCK NO-ERROR. */
/*                                                                                                                                                */
/*         IF chrFilteredQuestionnaireRef <> "" AND Questionnaire.QuestionnaireRef <> chrFilteredQuestionnaireRef THEN                            */
/*            NEXT FilterLoop.                                                                                                                    */
/*                                                                                                                                                */
/*         IF AVAILABLE ShipOrder THEN                                                                                                            */
/*         DO:                                                                                                                                    */
/*            IF chrFilteredOrderRef <> "" AND ShipOrder.OrderRef <> chrFilteredOrderRef THEN                                                     */
/*               NEXT FilterLoop.                                                                                                                 */
/*         END.                                                                                                                                   */
/*         ELSE DO:                                                                                                                               */
/*            IF chrFilteredOrderRef <> "" THEN                                                                                                   */
/*               NEXT FilterLoop.                                                                                                                 */
/*         END.                                                                                                                                   */
/*                                                                                                                                                */
/*         IF AVAILABLE Location THEN                                                                                                             */
/*         DO:                                                                                                                                    */
/*            IF chrFilteredLocationRef <> "" AND Location.LocationRef <> chrFilteredLocationRef THEN                                             */
/*               NEXT FilterLoop.                                                                                                                 */
/*         END.                                                                                                                                   */
/*         ELSE DO:                                                                                                                               */
/*            IF chrFilteredLocationRef <> "" THEN                                                                                                */
/*               NEXT FilterLoop.                                                                                                                 */
/*         END.                                                                                                                                   */
/*                                                                                                                                                */
/*         IF AVAILABLE QuestionnaireStatus THEN                                                                                                  */
/*         DO:                                                                                                                                    */
/*            IF chrFilteredStatus <> "" AND QuestionnaireStatus.StatusName <> chrFilteredStatus THEN                                             */
/*               NEXT FilterLoop.                                                                                                                 */
/*         END.                                                                                                                                   */
/*         ELSE DO:                                                                                                                               */
/*            IF chrFilteredStatus <> "" THEN                                                                                                     */
/*               NEXT FilterLoop.                                                                                                                 */
/*         END.                                                                                                                                   */
/*                                                                                                                                                */
/*         RUN pSetQuestionnaireRow.                                                                                                              */
/*         intDisplayCount = intDisplayCount + 1.                                                                                                 */
/*      END.                                                                                                                                      */
/*   END. /*IF logFiltering THEN*/                                                                                                                */
/*   ELSE                                                                                                                                         */
/*   DO:                                                                                                                                          */
      UnfilteredLoop:
      FOR EACH QuestionnaireInstance NO-LOCK:
/*         IF intDisplayCount >= intFilteredRecords THEN LEAVE UnfilteredLoop.*/
/*         FIND FIRST QuestionnaireType WHERE QuestionnaireType.QuestionnaireTypeID = Questionnaire.QuestionnaireTypeID NO-LOCK NO-ERROR.*/
/*         IF QuestionnaireType.ReUsable = YES THEN                                                                                      */
/*         DO:                                                                                                                           */
            MESSAGE "in".
            RUN pSetQuestionnaireRow.
            intDisplayCount = intDisplayCount + 1.
/*         END.*/
      END. /* UnfilteredLoop */
/*   END.*/
   
   QuestionnaireBrowse:endTable().
   
   /* Create a new frame */
   QuestionnaireBrowseFrame = NEW pageFrame().
   QuestionnaireBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QuestionnaireBrowseFrame:formOpen("questionnaire_browse_form").
   
   /* Start the Frame Header */
   QuestionnaireBrowseFrame:insertSpacer(5).
   QuestionnaireBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QuestionnaireBrowse:displayBrowse().  
   
   /* This creates an Excel File */
   /* No point in adding this as there's no room for a button to display it */
   /*InboundBrowse:generateExcel(intGblSessionID, "inbound_browse").  */

   /* End the Frame Header */
   QuestionnaireBrowseFrame:frameClose().
   QuestionnaireBrowseFrame:insertSpacer(10).
   
   QuestionnaireBrowseFrame:insertHiddenField("questionnaire_browse_scroll","").
   QuestionnaireBrowseFrame:insertHiddenField("QuestionnaireInstanceID","").
   QuestionnaireBrowseFrame:insertHiddenField("popup_answer_browse", "").
   QuestionnaireBrowseFrame:insertHiddenField("FilteredQuestionnaireRef", chrFilteredQuestionnaireRef).
   QuestionnaireBrowseFrame:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   QuestionnaireBrowseFrame:insertHiddenField("FilteredLocationRef", chrFilteredLocationRef).
   QuestionnaireBrowseFrame:insertHiddenField("FilteredStatus", chrFilteredStatus).
   QuestionnaireBrowseFrame:insertHiddenField("filtering", STRING(logFiltering)).
  
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireBrowseFrame}
   
   QuestionnaireBrowseFrame:formClose().
   
   /* Create Button Bar */
   QuestionnaireBrowseButtons = NEW buttonBar().
   QuestionnaireBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_filter",
                                  fTL("Filter"),
                                  "viewQuestionnaireFilter('questionnaire_filter_form');").
                                  
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_answer", 
                                  fTL("Answers"), 
                                  "viewAnswerBrowse('answer_browse_form');",
                                  "Disabled"). 
                                   
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_excel",
                                  fTL("Excel Export"),
                                  "excelExport('" + STRING(intGblSessionID) + "_questionnaire_browse.xml')").

   QuestionnaireBrowseButtons:closeBar().  
   QuestionnaireBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAnswerBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAnswerBrowse Procedure
PROCEDURE pAnswerBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
{webGetWebForm.i "answer_browse_form"}
   
   AnswerBrowseForm            = NEW dataForm("answer_browse_form").
   AnswerBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   
   AnswerBrowseForm:FormAction = "".
   
/*   FIND FIRST Questionnaire WHERE Questionnaire.QuestionnaireID = INT(QuestionnaireInstanceID) NO-LOCK NO-ERROR.*/
   
   /* Setup */
   AnswerBrowseForm:FormWidth  =  860.
   AnswerBrowseForm:FormHeight = 530.
   AnswerBrowseForm:FormTitle  = fTL("Answers") + (IF AVAILABLE Questionnaire THEN " for: " + STRING(Questionnaire.QuestionnaireName) ELSE "").
   AnswerBrowseForm:FormType   = "xxl_large".
   
   AnswerBrowse                = NEW browseTable("answer_browse").
   AnswerBrowse:BrowseWidth    = 840.
   AnswerBrowse:BrowseHeight   = 490.
   AnswerBrowse:ExcelExport    = TRUE.
   AnswerBrowse:SessionID      = intGblSessionID.
   
   AnswerBrowse:insertColumn(fTL("Question Name"), 200, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
/*   {webGetOptionalBrowseHeaders.i QuestionnaireLineStop}*/
   
   AnswerBrowse:insertColumn(fTL("Question Prompt"), 220, "INTEGER",   "left", FALSE).
   AnswerBrowse:insertColumn(fTL("Answer"),          220, "CHARACTER", "left", FALSE).
   AnswerBrowse:insertColumn(fTL("Created"),         180, "CHARACTER", "left", FALSE).

   /*Body*/
   AnswerBrowse:startBody().
   IF AVAILABLE QuestionnaireInstance THEN
   DO:
      FOR EACH QuestionAnswer WHERE QuestionAnswer.QuestionnaireInstanceID = QuestionnaireInstance.QuestionnaireInstanceID NO-LOCK:
         
         FIND FIRST Question WHERE Question.QuestionID = QuestionAnswer.QuestionID NO-LOCK NO-ERROR.

         AnswerBrowse:startRow(QuestionnaireInstance.QuestionnaireInstanceID, "selectAnswerRow(this," + '"' + 
                               STRING(QuestionnaireInstance.QuestionnaireInstanceID) +
                               '","adQuestionnaireEnquiry.p"' + ");", "").

         AnswerBrowse:insertData(IF AVAILABLE Question THEN Question.QuestionName ELSE "", "left").
         AnswerBrowse:insertData(IF AVAILABLE Question THEN Question.QuestionPrompt ELSE "", "left").
         AnswerBrowse:insertData(QuestionAnswer.AnswerValue, "left").
         AnswerBrowse:insertData(fDisplayDate&Time(QuestionAnswer.Created, "d/m/y H:M:S"), "left").
         
         AnswerBrowse:endRow().
    
      END. /* FOR EACH Question WHERE */
   END. /*IF AVAILABLE Task THEN*/
   
   AnswerBrowse:endTable().
   chrPageBuildError = chrPageBuildError + AnswerBrowse:getErrors().
   
   AnswerBrowse:insertData("FilteredQuestionnaireRef", chrFilteredQuestionnaireRef).
   AnswerBrowse:insertData("FilteredOrderRef", chrFilteredOrderRef).
   AnswerBrowse:insertData("FilteredLocationRef", chrFilteredLocationRef).
   AnswerBrowse:insertData("FilteredStatus", chrFilteredStatus).
   AnswerBrowse:insertData("filtering", STRING(logFiltering)).
   
   /* This adds all of the standard form field lists to the form */
/*   {webGetHiddenFormFields.i QuestionnaireLineStopBrowseForm}*/
   
   /* Create Button Bar */
   AnswerBrowseButtons = NEW buttonBar().


   AnswerBrowseButtons:addButton("answer_browse_form_btn_cancel",
                                   fTL("Cancel"),
                                   "hideAnswerBrowse();").

   AnswerBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   AnswerBrowseForm:FormBrowse  = AnswerBrowse.
   AnswerBrowseForm:FormButtons = AnswerBrowseButtons.
   
   AnswerBrowseForm:endForm(). 
   
   AnswerBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pInboundDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundDetailsFields Procedure 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundFilter Procedure 
PROCEDURE pQuestionnaireFilter :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   QuestionnaireFilterForm = NEW dataForm("questionnaire_filter_form").
   QuestionnaireFilterForm:WebStream  = STREAM WebStream:HANDLE.
   QuestionnaireFilterForm:FormAction = "adQuestionnaireEnquiry.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   QuestionnaireFilterForm:FormWidth   = 350.
   QuestionnaireFilterForm:FormHeight  = 200.
   QuestionnaireFilterForm:FormTitle   = "Questionnaire Filter".
   QuestionnaireFilterForm:FormType    = "small_wide".

   /* Column Layout */
   QuestionnaireFilterForm:insertPaddingColumn(10).
   QuestionnaireFilterForm:insertColumn(150).
   QuestionnaireFilterForm:insertColumn(200).
   QuestionnaireFilterForm:insertColumn(20).
   QuestionnaireFilterForm:insertColumn(2).

   /* Fields */
   QuestionnaireFilterForm:startRow().
   QuestionnaireFilterForm:insertLabel(fTL("Questionnaire")).
   QuestionnaireFilterForm:insertComboField("FilteredQuestionnaireRef", chrFilteredQuestionnaireRef, 210, TRUE).
   
   QuestionnaireFilterForm:startRow().
   QuestionnaireFilterForm:insertLabel(fTL("Identifier")).
   QuestionnaireFilterForm:insertTextField("FilteredOrderRef", chrFilteredOrderRef, 210, TRUE).
   
   QuestionnaireFilterForm:startRow().
   QuestionnaireFilterForm:insertLabel(fTL("Part")).
   QuestionnaireFilterForm:insertComboField("FilteredLocationRef", chrFilteredLocationRef, 210, TRUE).
   
   QuestionnaireFilterForm:startRow().
   QuestionnaireFilterForm:insertLabel(fTL("User")).
   QuestionnaireFilterForm:insertComboField("FilteredStatus", chrFilteredStatus, 210, TRUE).
   
   QuestionnaireFilterForm:startRow().
   QuestionnaireFilterForm:insertLabel(fTL("Date")).
   QuestionnaireFilterForm:insertDateField("FilteredRecords", chrFilteredRecords, 210, TRUE).

   /* Add Hidden Fields*/
   QuestionnaireFilterForm:insertHiddenField("form_name", "questionnaire_filter_form").
   QuestionnaireFilterForm:insertHiddenField("prog_name", "adQuestionnaireEnquiry.p").
   QuestionnaireFilterForm:insertHiddenField("FilteredQuestionnaireRef", "").
   QuestionnaireFilterForm:insertHiddenField("FilteredOrderRef", "").
   QuestionnaireFilterForm:insertHiddenField("FilteredLocationRef", "").
   QuestionnaireFilterForm:insertHiddenField("FilteredStatus", "").
   QuestionnaireFilterForm:insertHiddenField("FilteredRecords", "").
   QuestionnaireFilterForm:insertHiddenField("filtering", "yes").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireFilterForm}

   /* Create Button Bar */
   QuestionnaireFilterButtons = NEW buttonBar().

   QuestionnaireFilterButtons:addButton("questionnaire_filter_form_btn_search",
                                   fTL("Filter"),
                                   "filterQuestionnaire('questionnaire_filter_form');").

   QuestionnaireFilterButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireFilterForm:FormButtons = QuestionnaireFilterButtons.

   QuestionnaireFilterForm:endForm().
   QuestionnaireFilterForm:displayForm().

   /*   chrPageBuildError = chrPageBuildError + InboundFilterForm:getErrors().  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-process-web-request) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-web-request Procedure 
PROCEDURE process-web-request :
/*------------------------------------------------------------------------------
  Purpose:     Process the web request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* Output the MIME header and set up the object as state-less or state-aware. */
   /* This is required if any HTML is to be returned to the browser.             */
   RUN outputHeader.
   
   /* Process URL values */
   ASSIGN chrQuestionnaireInstanceID    = get-value("QuestionnaireInstanceID")
          intSelectedQuestionnaireInstance      = INT(chrQuestionnaireInstanceID)
          chrFilteredQuestionnaireRef   = get-value("FilteredQuestionnaireRef")
          chrFilteredOrderRef           = get-value("FilteredOrderRef")
          chrFilteredLocationRef        = get-value("FilteredLocationRef")
          chrFilteredStatus             = get-value("FilteredStatus")
          chrFilteredRecords            = (IF get-value("FilteredRecords") <> "" THEN get-value("FilteredRecords") ELSE chrFilteredRecords)
          logFiltering                  = (get-value("filtering") = "yes")
          chrScrollToQuestionnaireRow   = STRING(INT(get-value("questionnaire_browse_scroll"))) + ";" NO-ERROR.

   IF chrFilteredQuestionnaireRef     = "" AND
      chrFilteredOrderRef    = "" AND
      chrFilteredLocationRef = "" AND
      chrFilteredStatus      = "" THEN
      ASSIGN logFiltering    = FALSE.

   IF intSelectedQuestionnaireInstance > 0 THEN
      chrSelectQuestionnaireRow = 'selectQuestionnaireRow(document.getElementById("questionnaire_browse_row_' + chrQuestionnaireInstanceID + '"),"'
                              + chrQuestionnaireInstanceID + '","adQuestionnaireEnquiry.p' + '");'.
   
   IF get-value('popup_answer_browse') = "yes" THEN
      chrPopupAnswer = 'enablePopup("answer_browse_form_popup");'.
      
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("questionnaire_browse").scrollTop=' + chrScrollToQuestionnaireRow 
                             + chrSelectQuestionnaireRow + chrPopupAnswer.
     
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Questionnaire Enquiry".
   ThisPage:FrameTitle = "Questionnaire Enquiry".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("questionnaireenquiry.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
/*   IF logGblDebugging THEN                                                                                                    */
/*      fLog("GblSessionID:" + STRING(intGblSessionID) + " GblUserID:" + STRING(intGblUserID) + " GblUserName:" + chrGblUserName*/
/*                           + " GblLanguageID:" + STRING(intGblLanguageID) + " GblEnvironment:" + chrGblEnvironment).          */
   
   /******* Main Browser ********************/
   RUN pQuestionnaireBrowse.
   
   /******* Popup Browsers and Forms ********/    
/*   FIND FIRST Inbound WHERE Inbound.InboundID = intSelectedInbound NO-LOCK NO-ERROR.*/
   
   RUN pQuestionnaireFilter.
   RUN pAnswerBrowse.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Errors from an Update program are most important - get and show these first */
   chrFromURLError = get-value("error").
   
   /* This will display errors that came in from an Update program via the URL */
   IF chrFromURLError <> "" THEN
      {&OUT} '<script>systemAlert("message_alert","' + chrFromURLError + '","Error")</script>' SKIP.
   
   /* This will display errors that were generated when building the page */
   IF chrPageBuildError <> "" THEN
      {&OUT} '<script>systemAlert("message_alert","' + chrPageBuildError + '","Error")</script>' SKIP.
   
   IF chrFromURLError = "" THEN
      {&OUT} '<script>footerMessage("' + get-value("return-message") + '");</script>' SKIP.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetQuestionnaireRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetQuestionnaireRow Procedure
PROCEDURE pSetQuestionnaireRow:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        FIND FIRST Questionnaire WHERE Questionnaire.QuestionnaireID = QuestionnaireInstance.QuestionnaireID NO-LOCK NO-ERROR.
        FIND FIRST Part          WHERE Part.PartID = QuestionnaireInstance.PartID NO-LOCK NO-ERROR.
        FIND FIRST OperationType WHERE OperationType.OperationTypeID = QuestionnaireInstance.OperationTypeID NO-LOCK NO-ERROR.
        FIND FIRST GateUser      WHERE GateUser.GateUserID = QuestionnaireInstance.GateUserID NO-LOCK NO-ERROR.
    
        QuestionnaireBrowse:startRow(QuestionnaireInstanceID, "selectQuestionnaireRow(this," + '"' + 
                                                              STRING(QuestionnaireInstance.QuestionnaireInstanceID) + 
                                                              '"' + ");", "").

        QuestionnaireBrowse:insertData(QuestionnaireInstance.QuestionnaireInstanceID, "LEFT").


         /* add in optional & customer specific fields according to the webformfields associated with this webform */
/*         {webgetoptionalbrowsefields.i questionnaire}*/

         QuestionnaireBrowse:insertData((IF AVAILABLE Questionnaire THEN Questionnaire.QuestionnaireName ELSE ""),  "LEFT").
         QuestionnaireBrowse:insertData(QuestionnaireInstance.IdentifyingMarker,                                    "LEFT").
         QuestionnaireBrowse:insertData((IF AVAILABLE Part THEN Part.PartRef ELSE ""),                              "LEFT").
         QuestionnaireBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""),           "LEFT").
         QuestionnaireBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""),                     "LEFT").
         QuestionnaireBrowse:insertData(fDisplayDate&Time(QuestionnaireInstance.Started, "d/m/y H:M:S"),            "LEFT").
         QuestionnaireBrowse:insertData(fDisplayDate&Time(QuestionnaireInstance.Completed, "d/m/y H:M:S"),          "LEFT").


         QuestionnaireBrowse:endRow().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF
