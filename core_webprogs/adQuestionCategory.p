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

  Author: Anthony Ferrari

  Created: 19/01/2016

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

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedQuestionCategory              AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedQuestion                      AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQuestionCategoryRow             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionCategoryRow           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionCategoryID                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionID                            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionCategoryHistory          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestion                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectQuestionRow                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionRow                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFormTitle                             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionHistory                  AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QuestionCategoryBrowseFrame              AS pageFrame.
DEFINE VARIABLE QuestionCategoryBrowse                   AS browseTable.
DEFINE VARIABLE QuestionCategoryBrowseButtons            AS buttonBar.
DEFINE VARIABLE QuestionCategoryDetailsForm              AS dataForm.
DEFINE VARIABLE QuestionCategoryDetailsButtons           AS buttonBar.

DEFINE VARIABLE QuestionCategoryHistoryBrowseForm        AS dataForm.   
DEFINE VARIABLE QuestionCategoryHistoryBrowse            AS browseTable.
DEFINE VARIABLE QuestionCategoryHistoryBrowseButtons     AS buttonBar.

DEFINE VARIABLE QuestionCategoryHistoryDetailsForm       AS dataForm.
DEFINE VARIABLE QuestionCategoryHistoryDetailsButtons    AS buttonBar.

DEFINE VARIABLE QuestionBrowseForm                       AS dataForm.   
DEFINE VARIABLE QuestionBrowse                           AS browseTable.
DEFINE VARIABLE QuestionBrowseButtons                    AS buttonBar.

DEFINE VARIABLE QuestionDetailsForm                      AS dataForm.
DEFINE VARIABLE QuestionDetailsButtons                   AS buttonBar.

DEFINE VARIABLE QuestionHistoryBrowseForm                AS dataForm.   
DEFINE VARIABLE QuestionHistoryBrowse                    AS browseTable.
DEFINE VARIABLE QuestionHistoryBrowseButtons             AS buttonBar.

DEFINE VARIABLE QuestionHistoryDetailsForm               AS dataForm.
DEFINE VARIABLE QuestionHistoryDetailsButtons            AS buttonBar.

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
         HEIGHT             = 14.1
         WIDTH              = 60.4.
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
PROCEDURE pGetSystemOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE OUTPUT PARAMETER chrError AS CHARACTER NO-UNDO.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestion Procedure
PROCEDURE pQuestionBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "question_details_form"}

   FIND FIRST Question NO-LOCK
      WHERE Question.QuestionCategoryID = intSelectedQuestionCategory NO-ERROR.

   QuestionBrowseForm = NEW dataForm("question_browse_form").
   QuestionBrowseForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   QuestionBrowseForm:FormWidth   = 850.
   QuestionBrowseForm:FormHeight  = 540.
   QuestionBrowseForm:FormTitle   = fTL("Questions") + (IF AVAILABLE QuestionCategory THEN " for Category: "
                                                                  + STRING(QuestionCategory.QuestionCategoryName) ELSE "").
   QuestionBrowseForm:FormType    = "xxl_large".
   QuestionBrowseForm:FormAction  = "dbQuestionUpdate.p?" + TRIM(chrGblDefaultUrlValues, "&").

   QuestionBrowse = NEW browseTable("question_browse").
   QuestionBrowse:BrowseWidth  = 830.
   QuestionBrowse:BrowseHeight = 500.
   QuestionBrowse:WebStream    = STREAM WebStream:HANDLE.
   QuestionBrowse:ExcelExport  = TRUE.
   QuestionBrowse:SessionID    = intGblSessionID.


   QuestionBrowse:insertColumn(fTL("Question ID"),        70, "INTEGER",           FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Question}

   QuestionBrowse:insertColumn(fTL("Question Name"), 120, "CHARACTER", "left", FALSE).
   QuestionBrowse:insertColumn(fTL("Required"),       90, "LOGICAL",           FALSE).
   QuestionBrowse:insertColumn(fTL("Question Type"), 120, "CHARACTER", "left", FALSE).
   QuestionBrowse:insertColumn(fTL("Link Active"),    70, "LOGICAL",           FALSE).

   QuestionBrowse:StartBody().

   IF AVAILABLE QuestionCategory THEN
   DO:
      /*List the Question Links*/
      FOR EACH Question NO-LOCK
         WHERE Question.QuestionCategoryID = intSelectedQuestionCategory:

         FIND FIRST QuestionType OF Question NO-LOCK NO-ERROR. /* idx=QuestionTypeID */

         QuestionBrowse:startRow(Question.QuestionID, "selectQuestionRow(this," + '"'
                                                      + STRING(Question.QuestionID) + '"' + ");", "").

         QuestionBrowse:insertData(Question.QuestionID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i Question}

         QuestionBrowse:insertData((IF AVAILABLE Question THEN Question.QuestionName ELSE ""), "left").
         QuestionBrowse:insertData((IF AVAILABLE Question THEN STRING(Question.IsRequired,"Yes/No") ELSE ""), "left").
         QuestionBrowse:insertData((IF AVAILABLE QuestionType THEN QuestionType.QuestionTypeName ELSE ""), "left").
         QuestionBrowse:insertData(STRING(Question.Active, "Yes/No")).

         /* Add hidden fields */
         QuestionBrowse:insertHiddendata("QuestionCategoryID",QuestionCategory.QuestionCategoryID).
         QuestionBrowse:insertHiddendata("QuestionID",Question.QuestionID).
         QuestionBrowse:insertHiddendata("QuestionVersionID",Question.VersionID).

         QuestionBrowse:endRow().

      END. /* FOR EACH Question */
   END. /*IF AVAILABLE QuestionCategory THEN*/

   QuestionBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionBrowse:getErrors().

   QuestionBrowseForm:insertHiddenField("QuestionCategoryID",chrQuestionCategoryID).
   QuestionBrowseForm:insertHiddenField("QuestionCategoryVersionID","").
   QuestionBrowseForm:insertHiddenField("QuestionID","").
   QuestionBrowseForm:insertHiddenField("QuestionVersionID","").
   QuestionBrowseForm:insertHiddenField("question_browse_scroll","").
   QuestionBrowseForm:insertHiddenField("popup_question_browse","").
   QuestionBrowseForm:insertHiddenField("popup_questionhistory_browse","").
   QuestionBrowseForm:insertHiddenField("prog_name","adQuestionCategory.p").
   QuestionBrowseForm:insertHiddenField("form_name","question_browse_form").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionBrowseForm}

   /* Create Button Bar */
   QuestionBrowseButtons = NEW buttonBar().

   QuestionBrowseButtons:addButton("question_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQuestionDetails('question_details_form');",
                                                "Disabled").

   QuestionBrowseButtons:addButton("question_browse_form_btn_excel",
                                                fTL("Excel Export"),
                                                "excelExport('" + STRING(intGblSessionID) + "_question_browse.xml')").

   QuestionBrowseButtons:addButton("question_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('question_browse_form_popup');").

   QuestionBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   QuestionBrowseForm:FormBrowse  = QuestionBrowse.
   QuestionBrowseForm:FormButtons = QuestionBrowseButtons.
   QuestionBrowseForm:endForm().

   QuestionBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionCategoryLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionCategoryLinkDetails Procedure
PROCEDURE pQuestionDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "question_details_form"}

  ASSIGN chrDisplayFieldList  = "QuestionID,QuestionTypeID,QuestionCode,QuestionName,QuestionPrompt,IsRequired,Active"
                               + ",QuestionCategoryID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   QuestionDetailsForm = NEW dataForm("question_details_form").
   QuestionDetailsForm:WebStream = STREAM WebStream:HANDLE.

   QuestionDetailsForm:FormAction = "dbQuestionUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   QuestionDetailsForm:FormWidth   = 460.
   QuestionDetailsForm:FormHeight  = 300.
   QuestionDetailsForm:FormTitle   = "Question Details".
   QuestionDetailsForm:FormType    = "medium".

   /* Column Layout */
   QuestionDetailsForm:insertPaddingColumn(30).
   QuestionDetailsForm:insertColumn(150).
   QuestionDetailsForm:insertColumn(160).
   QuestionDetailsForm:insertColumn(20).
   QuestionDetailsForm:insertColumn(4).
   QuestionDetailsForm:insertColumn(110).

   /* Fields */
   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel("Question ID").
   QuestionDetailsForm:insertTextField("QuestionID", "", 200, TRUE).  
   
   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel("Question Code").
   QuestionDetailsForm:insertTextField("QuestionCode", "", 200, TRUE).  
   
   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel("Question Name").
   QuestionDetailsForm:insertTextField("QuestionName", "", 200, TRUE).
   
   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel("Question Prompt").
   QuestionDetailsForm:insertTextAreaField("QuestionPrompt", "", 200, TRUE).  
   
   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel("Question Type").
   QuestionDetailsForm:insertComboField("QuestionTypeID", "", 200, TRUE).  
   /* Question Type DropDown Options */
   FOR EACH QuestionType NO-LOCK /*idx=QuestionTypeID*/
      WHERE QuestionType.Active = TRUE:      
      QuestionDetailsForm:insertComboPairs("QuestionTypeID", STRING(QuestionType.QuestionTypeID), QuestionType.QuestionTypeName).    
   END.  
   
   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel("Question Category").
   QuestionDetailsForm:insertComboField("QuestionCategoryID", "", 200, TRUE).  
   /* Question Type DropDown Options */
   FOR EACH QuestionCategory NO-LOCK /*idx=QuestionCategoryID*/
      WHERE QuestionCategory.Active = TRUE:      
      QuestionDetailsForm:insertComboPairs("QuestionCategoryID", STRING(QuestionCategory.QuestionCategoryID), QuestionCategory.QuestionCategoryName).    
   END.
   
   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel(fTL("Is Required")). 
   QuestionDetailsForm:insertComboField("IsRequired", "", 200, TRUE).  
   QuestionDetailsForm:insertComboPairs("IsRequired", "yes", "Yes").
   QuestionDetailsForm:insertComboPairs("IsRequired", "no",  "No").

   QuestionDetailsForm:startRow().
   QuestionDetailsForm:insertLabel(fTL("Active")). 
   QuestionDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QuestionDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionDetailsForm:insertComboPairs("Active", "no",  "Not Active").

   {webGetOptionalFormFields.i pQuestionDetailsFields}

   /* Add Hidden Fields*/
   QuestionDetailsForm:insertHiddenField("question_browse_scroll", "").
   QuestionDetailsForm:insertHiddenField("form_name", "question_details_form").
   QuestionDetailsForm:insertHiddenField("prog_name", "adQuestionCategory.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionDetailsForm}

   /* Create Button Bar */
   QuestionDetailsButtons = NEW buttonBar().

   QuestionDetailsButtons:addButton("question_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('question_details_form_popup');").

   QuestionDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   QuestionDetailsForm:FormButtons = QuestionDetailsButtons.

   QuestionDetailsForm:endForm().

   QuestionDetailsForm:displayForm().

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
   
   /* Get the system options which relate to this program */
   RUN pGetSystemOptions(OUTPUT chrPageBuildError).  
   
   ASSIGN chrQuestionCategoryID              = get-value("QuestionCategoryID")
          intSelectedQuestionCategory        = INTEGER(chrQuestionCategoryID)
          chrQuestionID                      = get-value("QuestionID")
          intSelectedQuestion                = INTEGER(chrQuestionID)
          chrScrollToQuestionRow             = STRING(INTEGER(get-value("question_browse_scroll")))
          chrScrollToQuestionCategoryRow     = STRING(INTEGER(get-value("questioncategory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQuestionCategoryID <> "" THEN
     chrSelectQuestionCategoryRow = 'selectQuestionCategoryRow(document.getElementById("questioncategory_browse_row_' + chrQuestionCategoryID + '"),"' 
                                                         + chrQuestionCategoryID +  '");'.
                                                         
   IF chrQuestionID <> "" THEN
     chrSelectQuestionRow = 'selectQuestionRow(document.getElementById("question_browse_row_'
                                          + chrQuestionID + '"),"' + chrQuestionID +  '");'.
   
   IF get-value('popup_questioncategoryhistory_browse') = "yes" THEN
      chrPopupQuestionCategoryHistory = 'enablePopup("questioncategoryhistory_browse_form_popup");'.
      
   IF get-value('popup_question_browse') = "yes" THEN
      chrPopupQuestion = 'enablePopup("question_browse_form_popup");'.

/*   IF get-value('popup_questionhistory_browse') = "yes" THEN                         */
/*      chrPopupQuestionHistory  = 'enablePopup("questionhistory_browse_form_popup");'.*/
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("questioncategory_browse").scrollTop=' + chrScrollToQuestionCategoryRow 
                                                          + chrSelectQuestionCategoryRow 
                                                          + chrPopupQuestionCategoryHistory
                                                          + chrPopupQuestion.
                                                          
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Question Category Admin".
   ThisPage:FrameTitle    = "Question Category Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("questioncategory.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQuestionCategoryBrowse.
   
   FIND FIRST QuestionCategory NO-LOCK /* idx=QuestionCategoryID */
      WHERE QuestionCategory.QuestionCategoryID = intSelectedQuestionCategory NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQuestionCategoryDetails.
   RUN pQuestionCategoryHistory.
   RUN pQuestionCategoryHistoryDetails.
   RUN pQuestionBrowse.
   
   FIND FIRST Question NO-LOCK /* idx=QuestionID */
      WHERE Question.QuestionID = intSelectedQuestion NO-ERROR.

    RUN pQuestionDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QuestionCategoryBrowseFrame              NO-ERROR.
   DELETE OBJECT QuestionCategoryBrowse                   NO-ERROR.
   DELETE OBJECT QuestionCategoryBrowseButtons            NO-ERROR.
   DELETE OBJECT QuestionCategoryDetailsForm              NO-ERROR.
   DELETE OBJECT QuestionCategoryDetailsButtons           NO-ERROR.
   
   DELETE OBJECT QuestionCategoryHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT QuestionCategoryHistoryBrowse            NO-ERROR.
   DELETE OBJECT QuestionCategoryHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT QuestionCategoryHistoryDetailsForm       NO-ERROR.
   DELETE OBJECT QuestionCategoryHistoryDetailsButtons    NO-ERROR.
   
   DELETE OBJECT QuestionBrowseForm                       NO-ERROR.
   DELETE OBJECT QuestionBrowse                           NO-ERROR.
   DELETE OBJECT QuestionBrowseButtons                    NO-ERROR.
   DELETE OBJECT QuestionDetailsForm                      NO-ERROR.
   DELETE OBJECT QuestionDetailsButtons                   NO-ERROR.
   
/*   DELETE OBJECT QuestionHistoryBrowseForm                NO-ERROR.*/
/*   DELETE OBJECT QuestionHistoryBrowse                    NO-ERROR.*/
/*   DELETE OBJECT QuestionHistoryBrowseButtons             NO-ERROR.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionCategoryBrowse Procedure 
PROCEDURE pQuestionCategoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "questioncategory_details_form"}
   
   QuestionCategoryBrowse = NEW browseTable("questioncategory_browse").
   QuestionCategoryBrowse:BrowseWidth  = 965.
   QuestionCategoryBrowse:BrowseHeight = 455.
   QuestionCategoryBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QuestionCategoryBrowse:insertColumn(fTL("Category ID"),          70, "INTEGER",          FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionCategory}
   
   QuestionCategoryBrowse:insertColumn(fTL("Question Code"), 150, "CHARACTER", "left", FALSE).
   QuestionCategoryBrowse:insertColumn(fTL("Question Name"), 150, "CHARACTER", "left", FALSE).
   QuestionCategoryBrowse:insertColumn(fTL("Description"),   200, "CHARACTER", "left", FALSE).
   QuestionCategoryBrowse:insertColumn(fTL("Active"),         70, "LOGICAL",           FALSE).
   
   /*Body*/
   QuestionCategoryBrowse:startBody().
   
   FOR EACH QuestionCategory NO-LOCK /*idx=ActiveListingSequence*/
      BY QuestionCategory.Active DESC
      BY QuestionCategory.QuestionCategoryID:
      
      QuestionCategoryBrowse:startRow(QuestionCategory.QuestionCategoryID, "selectQuestionCategoryRow(this," + '"' + STRING(QuestionCategory.QuestionCategoryID) 
                                                                  + '"' + ");", "").
      QuestionCategoryBrowse:insertData(QuestionCategory.QuestionCategoryID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QuestionCategory}
      
      QuestionCategoryBrowse:insertData(QuestionCategory.QuestionCategoryCode, "left").
      QuestionCategoryBrowse:insertData(QuestionCategory.QuestionCategoryName, "left").
      QuestionCategoryBrowse:insertData(QuestionCategory.QuestionCategoryDescr, "left").
      QuestionCategoryBrowse:insertData(STRING(QuestionCategory.Active,"Yes/No")).
      
      /* Add hidden fields */
      QuestionCategoryBrowse:insertHiddenData("QuestionCategoryVersionID",QuestionCategory.VersionID).
      
      QuestionCategoryBrowse:endRow().
      
   END. /*FOR EACH QuestionCategory NO-LOCK */
   
   QuestionCategoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionCategoryBrowse:getErrors().
   
   /* Create a new frame */
   QuestionCategoryBrowseFrame = NEW pageFrame().
   QuestionCategoryBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QuestionCategoryBrowseFrame:FormAction="dbQuestionCategoryUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QuestionCategoryBrowseFrame:formOpen("questioncategory_browse_form").
   
   /* Start the Frame Header */
   QuestionCategoryBrowseFrame:insertSpacer(5).
   QuestionCategoryBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QuestionCategoryBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QuestionCategoryBrowseFrame:frameClose().
   QuestionCategoryBrowseFrame:insertSpacer(10).
   
   QuestionCategoryBrowseFrame:insertHiddenField("questioncategory_browse_scroll","").
   QuestionCategoryBrowseFrame:insertHiddenField("QuestionCategoryID",chrQuestionCategoryID).
   QuestionCategoryBrowseFrame:insertHiddenField("QuestionCategoryVersionID","").
   QuestionCategoryBrowseFrame:insertHiddenField("QuestionID",chrQuestionID).
   QuestionCategoryBrowseFrame:insertHiddenField("popup_questioncategoryhistory_browse","").
   QuestionCategoryBrowseFrame:insertHiddenField("popup_question_browse","").
   QuestionCategoryBrowseFrame:insertHiddenField("popup_questionhistory_browse","").
   QuestionCategoryBrowseFrame:insertHiddenField("form_name","questioncategory_browse_form").
   QuestionCategoryBrowseFrame:insertHiddenField("prog_name","adQuestionCategory.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionCategoryBrowseFrame}
   
   QuestionCategoryBrowseFrame:formClose().
   
   /* Create Button Bar */
   QuestionCategoryBrowseButtons = NEW buttonBar().
   QuestionCategoryBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   QuestionCategoryBrowseButtons:addButton("questioncategory_browse_form_btn_create",
                                           fTL("Create"),
                                           "createQuestionCategory('questioncategory_details_form');",
                                           "").
   
   QuestionCategoryBrowseButtons:addButton("questioncategory_browse_form_btn_details",
                                           fTL("Details"),
                                           "viewQuestionCategoryDetails('questioncategory_details_form');",
                                           "Disabled").
   
                                             
   QuestionCategoryBrowseButtons:addButton("questioncategory_browse_form_btn_question",
                                           fTL("Questions"),
                                           "viewQuestion();",
                                           "Disabled").
   
   QuestionCategoryBrowseButtons:addButton("questioncategory_browse_form_btn_history",
                                           fTL("History"),
                                           "viewHistory();",
                                           "Disabled").
                                             
   /*
   QuestionCategoryBrowseButtons:addButton("questioncategory_browse_form_btn_delete",
                                           fTL("Delete"),
                                           "confirmDeleteQuestionCategory();",
                                           (IF intSelectedQuestionCategory > 0 THEN "" ELSE "Disabled")).
   */
   
   QuestionCategoryBrowseButtons:closeBar().  
   QuestionCategoryBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionCategoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionCategoryDetails Procedure 
PROCEDURE pQuestionCategoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "questioncategory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionCategoryID,QuestionCategoryCode,QuestionCategoryName,QuestionCategoryDescr,Active" 
          chrEditFieldList     = "QuestionCategoryDescr,Active" 
          chrNewFieldList      = "QuestionCategoryCode,QuestionCategoryName,QuestionCategoryDescr,Active" 
          chrRequiredFieldList = "QuestionCategoryCode,QuestionCategoryName,QuestionCategoryDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QuestionCategoryDetailsForm = NEW dataForm("questioncategory_details_form").
   QuestionCategoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QuestionCategoryDetailsForm:FormAction = "dbQuestionCategoryUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionCategoryDetailsForm:FormWidth   = 580.
   QuestionCategoryDetailsForm:FormHeight  = 420.
   QuestionCategoryDetailsForm:FormTitle   = "QuestionCategory Details".
   QuestionCategoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QuestionCategoryDetailsForm:insertPaddingColumn(30).
   QuestionCategoryDetailsForm:insertColumn(150).
   QuestionCategoryDetailsForm:insertColumn(160).
   QuestionCategoryDetailsForm:insertColumn(20).
   QuestionCategoryDetailsForm:insertColumn(4).
   QuestionCategoryDetailsForm:insertColumn(110).
   
   /* Fields */
   QuestionCategoryDetailsForm:startRow().
   QuestionCategoryDetailsForm:insertLabel("Type ID").
   QuestionCategoryDetailsForm:insertTextField("QuestionCategoryID", "", 220, TRUE).  
   
   QuestionCategoryDetailsForm:startRow().
   QuestionCategoryDetailsForm:insertLabel("QuestionCategory Code").
   QuestionCategoryDetailsForm:insertTextField("QuestionCategoryCode", "", 220, TRUE).  
   
   QuestionCategoryDetailsForm:startRow().
   QuestionCategoryDetailsForm:insertLabel("QuestionCategory Name").
   QuestionCategoryDetailsForm:insertTextField("QuestionCategoryName", "", 220, TRUE).
   
   QuestionCategoryDetailsForm:startRow().
   QuestionCategoryDetailsForm:insertLabel("QuestionCategory Descr").
   QuestionCategoryDetailsForm:insertTextAreaField("QuestionCategoryDescr", "", 220, TRUE).  
   

   QuestionCategoryDetailsForm:startRow().
   QuestionCategoryDetailsForm:insertLabel(fTL("Active")). 
   QuestionCategoryDetailsForm:insertComboField("Active", "", 110, TRUE).  
   QuestionCategoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionCategoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQuestionCategoryDetailsFields}
   
   /* Add Hidden Fields*/
   QuestionCategoryDetailsForm:insertHiddenField("questioncategory_browse_scroll", "").
   QuestionCategoryDetailsForm:insertHiddenField("form_name", "questioncategory_details_form").
   QuestionCategoryDetailsForm:insertHiddenField("prog_name", "adQuestionCategory.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionCategoryDetailsForm}
   
   /* Create Button Bar */
   QuestionCategoryDetailsButtons = NEW buttonBar().
   
   QuestionCategoryDetailsButtons:addButton("questioncategory_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQuestionCategory('questioncategory_details_form');").
   
   QuestionCategoryDetailsButtons:addButton("questioncategory_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('questioncategory_details_form_popup');").
   
   QuestionCategoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionCategoryDetailsForm:FormButtons = QuestionCategoryDetailsButtons.
   
   QuestionCategoryDetailsForm:endForm(). 
   
   QuestionCategoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QuestionCategoryDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionCategoryDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionCategoryDetailsFields Procedure 
PROCEDURE pQuestionCategoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QuestionCategoryDetailsForm:startRow().
         QuestionCategoryDetailsForm:insertLabel(fTL("Field Label")).
         QuestionCategoryDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionCategoryHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionCategoryHistory Procedure
PROCEDURE pQuestionCategoryHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questioncategoryhistory_details_form"}
   
   FIND FIRST QuestionCategory WHERE QuestionCategory.QuestionCategoryID = intSelectedQuestionCategory NO-LOCK NO-ERROR.
   
   QuestionCategoryHistoryBrowseForm = NEW dataForm("questioncategoryhistory_browse_form").
   QuestionCategoryHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QuestionCategoryHistoryBrowseForm:FormWidth   = 850.
   QuestionCategoryHistoryBrowseForm:FormHeight  = 540.
   QuestionCategoryHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE QuestionCategory THEN " for QuestionCategoryID: " 
                                                                  + STRING(QuestionCategory.QuestionCategoryID) ELSE "").
   QuestionCategoryHistoryBrowseForm:FormType    = "xxl_large".
   
   QuestionCategoryHistoryBrowse = NEW browseTable("questioncategoryhistory_browse").
   QuestionCategoryHistoryBrowse:BrowseWidth  = 830.
   QuestionCategoryHistoryBrowse:BrowseHeight = 500.
   QuestionCategoryHistoryBrowse:ExcelExport  = TRUE.
   QuestionCategoryHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QuestionCategoryHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionCategoryHistory}

   QuestionCategoryHistoryBrowse:insertColumn(fTL("Category Code"),  120, "CHARACTER", "left", FALSE).
   QuestionCategoryHistoryBrowse:insertColumn(fTL("Category Name"),  120, "CHARACTER", "left", FALSE).
   QuestionCategoryHistoryBrowse:insertColumn(fTL("Category Descr"), 180, "CHARACTER", "left", FALSE).
   QuestionCategoryHistoryBrowse:insertColumn(fTL("Active"),          70, "LOGICAL",           FALSE).
   QuestionCategoryHistoryBrowse:insertColumn(fTL("User"),           120, "CHARACTER", "left", FALSE).
   QuestionCategoryHistoryBrowse:insertColumn(fTL("Created"),        135, "CHARACTER", "left", FALSE).
   
   QuestionCategoryHistoryBrowse:StartBody().
   
   IF AVAILABLE QuestionCategory THEN
   DO:
      /*List the QuestionCategoryHistory*/
      FOR EACH QuestionCategoryHistory NO-LOCK 
         WHERE  QuestionCategoryHistory.QuestionCategoryID = intSelectedQuestionCategory
         BY QuestionCategoryHistory.QuestionCategoryHistoryID:
         
         FIND FIRST GateUser OF QuestionCategoryHistory NO-LOCK NO-ERROR.
       
         QuestionCategoryHistoryBrowse:startRow(QuestionCategoryHistory.QuestionCategoryHistoryID, "selectHistoryRow(this," + '"' + STRING(QuestionCategoryHistory.QuestionCategoryHistoryID) 
                                                                     + '","questionCategoryHistory"' + ");", "").
         QuestionCategoryHistoryBrowse:insertData(QuestionCategoryHistory.QuestionCategoryHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionCategoryHistory}
         
         
         QuestionCategoryHistoryBrowse:insertData(QuestionCategoryHistory.QuestionCategoryCode, "left").
         QuestionCategoryHistoryBrowse:insertData(QuestionCategoryHistory.QuestionCategoryName, "left").
         QuestionCategoryHistoryBrowse:insertData(QuestionCategoryHistory.QuestionCategoryDescr, "left").                            
         QuestionCategoryHistoryBrowse:insertData(STRING(QuestionCategoryHistory.Active, "Yes/No")).
         QuestionCategoryHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QuestionCategoryHistoryBrowse:insertData(fDisplayDate&Time(QuestionCategoryHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         QuestionCategoryHistoryBrowse:insertHiddendata("QuestionCategoryHistoryID",QuestionCategoryHistory.QuestionCategoryHistoryID).
         
         QuestionCategoryHistoryBrowse:endRow().
      
      END. /* FOR EACH QuestionCategoryHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   QuestionCategoryHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionCategoryHistoryBrowse:getErrors().
   
   QuestionCategoryHistoryBrowseForm:insertHiddenField("QuestionCategoryHistoryID","").
   QuestionCategoryHistoryBrowseForm:insertHiddenField("popup_questioncategoryhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionCategoryHistoryBrowseForm}
   
   /* Create Button Bar */
   QuestionCategoryHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QuestionCategoryHistoryBrowseButtons:addButton("questioncategoryhistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQuestionCategoryHistoryDetails('questioncategoryhistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QuestionCategoryHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   QuestionCategoryHistoryBrowseButtons:addButton("questioncategoryhistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('questioncategoryhistory_browse_form_popup');").
   
   QuestionCategoryHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionCategoryHistoryBrowseForm:FormBrowse  = QuestionCategoryHistoryBrowse.
   QuestionCategoryHistoryBrowseForm:FormButtons = QuestionCategoryHistoryBrowseButtons.
   QuestionCategoryHistoryBrowseForm:endForm(). 
   
   QuestionCategoryHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQuestionCategoryHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionCategoryHistoryDetails Procedure
PROCEDURE pQuestionCategoryHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questioncategoryhistory_details_form"}
   
   chrDisplayFieldList  = "QuestionCategoryHistoryID,QuestionCategoryID,QuestionCategoryCode,QuestionCategoryName,QuestionCategoryDescr"
                        + ",Active,CreatedDate,CreatedHour,CreatedMins,GateUserID,TransactionID".
                             
   
   QuestionCategoryHistoryDetailsForm = NEW dataForm("questioncategoryhistory_details_form").
   QuestionCategoryHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QuestionCategoryHistoryDetailsForm:FormWidth   = 545.
   QuestionCategoryHistoryDetailsForm:FormHeight  = 440.
   QuestionCategoryHistoryDetailsForm:FormTitle   = "Question Category History Details".
   QuestionCategoryHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QuestionCategoryHistoryDetailsForm:insertPaddingColumn(40).
   QuestionCategoryHistoryDetailsForm:insertColumn(150).
   QuestionCategoryHistoryDetailsForm:insertColumn(120).
   QuestionCategoryHistoryDetailsForm:insertColumn(20).
   QuestionCategoryHistoryDetailsForm:insertColumn(4).
   QuestionCategoryHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel(fTL("History ID")).
   QuestionCategoryHistoryDetailsForm:insertTextField("QuestionCategoryHistoryID", "", 200, TRUE).    
   
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel(fTL("QuestionCategoryID")).
   QuestionCategoryHistoryDetailsForm:insertTextField("QuestionCategoryID", "", 200, TRUE).
   
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel(fTL("QuestionCategoryCode")).
   QuestionCategoryHistoryDetailsForm:insertTextField("QuestionCategoryCode", "", 200, TRUE).
   
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel(fTL("QuestionCategoryName")).
   QuestionCategoryHistoryDetailsForm:insertTextField("QuestionCategoryName", "", 200, TRUE).
   
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel(fTL("QuestionCategoryDescr")).
   QuestionCategoryHistoryDetailsForm:insertTextAreaField("QuestionCategoryDescr", "", 200, TRUE).
   
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel(fTL("Active")).
   QuestionCategoryHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionCategoryHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionCategoryHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel("User").
   QuestionCategoryHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QuestionCategoryHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QuestionCategoryHistoryDetailsForm:startRow().
   QuestionCategoryHistoryDetailsForm:insertLabel("Created").
   QuestionCategoryHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QuestionCategoryHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QuestionCategoryHistoryDetailsForm:insertLabel(":").
   QuestionCategoryHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QuestionCategoryHistoryDetailsForm:insertHiddenField("questioncategory_browse_scroll","").
   QuestionCategoryHistoryDetailsForm:insertHiddenField("popup_questioncategoryhistory_browse", "").
   QuestionCategoryHistoryDetailsForm:insertHiddenField("QuestionCategoryHistoryID","").
   QuestionCategoryHistoryDetailsForm:insertHiddenField("form_name","questioncategoryhistory_details_form").
   QuestionCategoryHistoryDetailsForm:insertHiddenField("prog_name","adQuestionCategory.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionCategoryHistoryDetailsForm}
   
   /* Create Button Bar */
   QuestionCategoryHistoryDetailsButtons = NEW buttonBar().
   
   QuestionCategoryHistoryDetailsButtons:addButton("questioncategoryhistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('questioncategoryhistory_details_form_popup');").
                                        
   QuestionCategoryHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionCategoryHistoryDetailsForm:FormButtons = QuestionCategoryHistoryDetailsButtons.
   
   QuestionCategoryHistoryDetailsForm:endForm(). 
   QuestionCategoryHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

