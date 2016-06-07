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

  Created: 20/01/2016

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

DEFINE VARIABLE intSelectedQuestion                       AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQuestionRow                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionRow                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionID                             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionHistory                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFormTitle                              AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QuestionBrowseFrame                       AS pageFrame.
DEFINE VARIABLE QuestionBrowse                            AS browseTable.
DEFINE VARIABLE QuestionBrowseButtons                     AS buttonBar.
DEFINE VARIABLE QuestionDetailsForm                       AS dataForm.
DEFINE VARIABLE QuestionDetailsButtons                    AS buttonBar.

DEFINE VARIABLE QuestionHistoryBrowseForm                 AS dataForm.   
DEFINE VARIABLE QuestionHistoryBrowse                     AS browseTable.
DEFINE VARIABLE QuestionHistoryBrowseButtons              AS buttonBar.

DEFINE VARIABLE QuestionHistoryDetailsForm                AS dataForm.
DEFINE VARIABLE QuestionHistoryDetailsButtons             AS buttonBar.

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
   
   ASSIGN chrQuestionID                       = get-value("QuestionID")
          intSelectedQuestion                 = INTEGER(chrQuestionID)
          chrScrollToQuestionRow              = STRING(INTEGER(get-value("question_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQuestionID <> "" THEN
     chrSelectQuestionRow = 'selectQuestionRow(document.getElementById("question_browse_row_' + chrQuestionID + '"),"' 
                                                         + chrQuestionID +  '");'.
                                                         
   /* Browse Popups */
   IF get-value('popup_questionhistory_browse') = "yes" THEN
      chrPopupQuestionHistory  = 'enablePopup("questionhistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("question_browse").scrollTop=' + chrScrollToQuestionRow 
                                                          + chrSelectQuestionRow 
                                                          + chrPopupQuestionHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Question Admin".
   ThisPage:FrameTitle    = "Question Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("question.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQuestionBrowse.
   
   FIND FIRST Question NO-LOCK /* idx=QuestionID */
      WHERE Question.QuestionID = intSelectedQuestion NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQuestionDetails.
   RUN pQuestionHistory.
   RUN pQuestionHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QuestionBrowseFrame                      NO-ERROR.
   DELETE OBJECT QuestionBrowse                           NO-ERROR.
   DELETE OBJECT QuestionBrowseButtons                    NO-ERROR.
   DELETE OBJECT QuestionDetailsForm                      NO-ERROR.
   DELETE OBJECT QuestionDetailsButtons                   NO-ERROR.
   
   DELETE OBJECT QuestionHistoryBrowseForm                NO-ERROR.   
   DELETE OBJECT QuestionHistoryBrowse                    NO-ERROR.
   DELETE OBJECT QuestionHistoryBrowseButtons             NO-ERROR.
   
   DELETE OBJECT QuestionHistoryDetailsForm               NO-ERROR.
   DELETE OBJECT QuestionHistoryDetailsButtons            NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionBrowse Procedure 
PROCEDURE pQuestionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "question_details_form"}
   
   QuestionBrowse = NEW browseTable("question_browse").
   QuestionBrowse:BrowseWidth  = 965.
   QuestionBrowse:BrowseHeight = 455.
   QuestionBrowse:WebStream = STREAM WebStream:HANDLE.
   QuestionBrowse:ExcelExport  = TRUE.
   QuestionBrowse:SessionID    = intGblSessionID.
   
   /* Add in the ID as first Column */
   QuestionBrowse:insertColumn(fTL("Question ID"), 90, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Question}
   
   
   QuestionBrowse:insertColumn(fTL("Question Code"),   120, "CHARACTER", "left", FALSE).
   QuestionBrowse:insertColumn(fTL("Question Name"),   120, "CHARACTER", "left", FALSE).
   QuestionBrowse:insertColumn(fTL("Question Prompt"), 200, "CHARACTER", "left", FALSE).
   QuestionBrowse:insertColumn(fTL("Question Type"),   100, "INTEGER", FALSE).
   QuestionBrowse:insertColumn(fTL("Is Required"),     75, "LOGICAL", FALSE).
   QuestionBrowse:insertColumn(fTL("Active"),          70, "LOGICAL", FALSE).
   
   /*Body*/
   QuestionBrowse:startBody().
   
   FOR EACH Question NO-LOCK: /*idx=QuestionID*/
   
      FIND FIRST QuestionType NO-LOCK /*idx=QuestionTypeID*/
         WHERE QuestionType.QuestionTypeID = Question.QuestionTypeID NO-ERROR.
      
      QuestionBrowse:startRow(Question.QuestionID, "selectQuestionRow(this," + '"' + STRING(Question.QuestionID) 
                                                                  + '"' + ");", "").
                                                                  
      QuestionBrowse:insertData(Question.QuestionID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Question}
      
      QuestionBrowse:insertData(Question.QuestionCode, "left").
      QuestionBrowse:insertData(Question.QuestionName, "left").
      QuestionBrowse:insertData(Question.QuestionPrompt, "left").
      QuestionBrowse:insertData(IF AVAILABLE QuestionType THEN QuestionType.QuestionTypeName ELSE "", "left").
      QuestionBrowse:insertData(STRING(Question.IsRequired,"Yes/No")).
      QuestionBrowse:insertData(STRING(Question.Active,"Yes/No")).
      
      /* Add hidden fields */
      QuestionBrowse:insertHiddenData("QuestionVersionID",Question.VersionID).
      
      QuestionBrowse:endRow().
      
   END. /*FOR EACH Question NO-LOCK */
   
   QuestionBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionBrowse:getErrors().
   
   /* Create a new frame */
   QuestionBrowseFrame = NEW pageFrame().
   QuestionBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QuestionBrowseFrame:FormAction="dbQuestionUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QuestionBrowseFrame:formOpen("question_browse_form").
   
   /* Start the Frame Header */
   QuestionBrowseFrame:insertSpacer(5).
   QuestionBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QuestionBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QuestionBrowseFrame:frameClose().
   QuestionBrowseFrame:insertSpacer(10).
   
   QuestionBrowseFrame:insertHiddenField("question_browse_scroll","").
   QuestionBrowseFrame:insertHiddenField("QuestionID",chrQuestionID).
   QuestionBrowseFrame:insertHiddenField("QuestionVersionID","").
   QuestionBrowseFrame:insertHiddenField("QuestionCategoryLinkVersionID","").
   QuestionBrowseFrame:insertHiddenField("popup_questionhistory_browse","").
   QuestionBrowseFrame:insertHiddenField("popup_questioncategorylink_browse","").
   QuestionBrowseFrame:insertHiddenField("popup_addquestioncategorylink_browse","").
   QuestionBrowseFrame:insertHiddenField("popup_questioncategorylinkhistory_browse","").
   QuestionBrowseFrame:insertHiddenField("form_name","question_browse_form").
   QuestionBrowseFrame:insertHiddenField("prog_name","adQuestion.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionBrowseFrame}
   
   QuestionBrowseFrame:formClose().
   
   /* Create Button Bar */
   QuestionBrowseButtons = NEW buttonBar().
   QuestionBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   QuestionBrowseButtons:addButton("question_browse_form_btn_create",
                                    fTL("Create"),
                                    "createQuestion('question_details_form');",
                                    "").
   
   QuestionBrowseButtons:addButton("question_browse_form_btn_details",
                                    fTL("Details"),
                                    "viewQuestionDetails('question_details_form');",
                                    "Disabled").
   
                                             
/*   QuestionBrowseButtons:addButton("question_browse_form_btn_categories",*/
/*                                    fTL("Categories"),                   */
/*                                    "viewQuestionCategoryLink();",       */
/*                                    "Disabled").                         */
   
   QuestionBrowseButtons:addButton("question_browse_form_btn_history",
                                    fTL("History"),
                                    "viewHistory();",
                                    "Disabled").
                                             
   QuestionBrowseButtons:addButton("question_browse_form_btn_excel",
                                    fTL("Excel Export"),
                                    "excelExport('" + STRING(intGblSessionID) + "_question_browse.xml')").
   
   QuestionBrowseButtons:closeBar().  
   QuestionBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionDetails Procedure 
PROCEDURE pQuestionDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "question_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionID,QuestionTypeID,QuestionCode,QuestionName,QuestionPrompt,IsRequired,Active"
                               + ",QuestionCategoryID" 
          chrEditFieldList     = "IsRequired,Active" 
          chrNewFieldList      = "QuestionTypeID,QuestionCode,QuestionName,QuestionPrompt,IsRequired,Active,QuestionCategoryID" 
          chrRequiredFieldList = "QuestionTypeID,QuestionCode,QuestionName,QuestionPrompt,QuestionCategoryID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QuestionDetailsForm = NEW dataForm("question_details_form").
   QuestionDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QuestionDetailsForm:FormAction = "dbQuestionUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionDetailsForm:FormWidth   = 580.
   QuestionDetailsForm:FormHeight  = 420.
   QuestionDetailsForm:FormTitle   = "Question Details".
   QuestionDetailsForm:FormType    = "large".
   
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
   QuestionDetailsForm:insertHiddenField("prog_name", "adQuestion.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionDetailsForm}
   
   /* Create Button Bar */
   QuestionDetailsButtons = NEW buttonBar().
   
   QuestionDetailsButtons:addButton("question_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQuestion('question_details_form');").
   
   QuestionDetailsButtons:addButton("question_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('question_details_form_popup');").
   
   QuestionDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionDetailsForm:FormButtons = QuestionDetailsButtons.
   
   QuestionDetailsForm:endForm(). 
   
   QuestionDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QuestionDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionDetailsFields Procedure 
PROCEDURE pQuestionDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QuestionDetailsForm:startRow().
         QuestionDetailsForm:insertLabel(fTL("Field Label")).
         QuestionDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionHistory Procedure
PROCEDURE pQuestionHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionhistory_details_form"}
   
   FIND FIRST Question WHERE Question.QuestionID = intSelectedQuestion NO-LOCK NO-ERROR.
   
   QuestionHistoryBrowseForm = NEW dataForm("questionhistory_browse_form").
   QuestionHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QuestionHistoryBrowseForm:FormWidth   = 850.
   QuestionHistoryBrowseForm:FormHeight  = 540.
   QuestionHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE Question THEN " for QuestionID: " 
                                                                  + STRING(Question.QuestionID) ELSE "").
   QuestionHistoryBrowseForm:FormType    = "xxl_large".
   
   QuestionHistoryBrowse = NEW browseTable("questionhistory_browse").
   QuestionHistoryBrowse:BrowseWidth  = 830.
   QuestionHistoryBrowse:BrowseHeight = 500.
   QuestionHistoryBrowse:ExcelExport  = TRUE.
   QuestionHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QuestionHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionHistory}
   
   QuestionHistoryBrowse:insertColumn(fTL("Question Code"),   120, "CHARACTER", "left", FALSE).
   QuestionHistoryBrowse:insertColumn(fTL("Question Name"),   120, "CHARACTER", "left", FALSE).
   QuestionHistoryBrowse:insertColumn(fTL("Question Prompt"), 140, "CHARACTER", "left", FALSE).
   QuestionHistoryBrowse:insertColumn(fTL("Is Required"),      70, "LOGICAL",           FALSE).
   QuestionHistoryBrowse:insertColumn(fTL("Active"),           50, "LOGICAL",           FALSE).
   QuestionHistoryBrowse:insertColumn(fTL("User"),            120, "CHARACTER", "left", FALSE).
   QuestionHistoryBrowse:insertColumn(fTL("Created"),         130, "CHARACTER", "left", FALSE).
   
   QuestionHistoryBrowse:StartBody().
   
   IF AVAILABLE Question THEN
   DO:
      /*List the QuestionHistory*/
      FOR EACH QuestionHistory NO-LOCK 
         WHERE  QuestionHistory.QuestionID = intSelectedQuestion
         BY QuestionHistory.Created DESC:
         
         FIND FIRST GateUser     OF QuestionHistory NO-LOCK NO-ERROR. /* idx=GateUserID */
         FIND FIRST QuestionType OF QuestionHistory NO-LOCK NO-ERROR. /* idx=QuestionTypeID */
       
         QuestionHistoryBrowse:startRow(QuestionHistory.QuestionHistoryID, "selectHistoryRow(this," + '"' + STRING(QuestionHistory.QuestionHistoryID) 
                                                                     + '","questionHistory"' + ");", "").
         QuestionHistoryBrowse:insertData(QuestionHistory.QuestionHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionHistory}
         
         QuestionHistoryBrowse:insertData(QuestionHistory.QuestionCode, "left").
         QuestionHistoryBrowse:insertData(QuestionHistory.QuestionName, "left").
         QuestionHistoryBrowse:insertData(QuestionHistory.QuestionPrompt, "left").                
         QuestionHistoryBrowse:insertData(STRING(QuestionHistory.IsRequired, "Yes/No")).       
         QuestionHistoryBrowse:insertData(STRING(QuestionHistory.Active, "Yes/No")).
         QuestionHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QuestionHistoryBrowse:insertData(fDisplayDate&Time(QuestionHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         QuestionHistoryBrowse:insertHiddendata("QuestionHistoryID",QuestionHistory.QuestionHistoryID).
         
         QuestionHistoryBrowse:endRow().
      
      END. /* FOR EACH QuestionHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   QuestionHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionHistoryBrowse:getErrors().
   
   QuestionHistoryBrowseForm:insertHiddenField("QuestionHistoryID","").
   QuestionHistoryBrowseForm:insertHiddenField("popup_questionhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionHistoryBrowseForm}
   
   /* Create Button Bar */
   QuestionHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QuestionHistoryBrowseButtons:addButton("questionhistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQuestionHistoryDetails('questionhistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QuestionHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   QuestionHistoryBrowseButtons:addButton("questionhistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('questionhistory_browse_form_popup');").
   
   QuestionHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionHistoryBrowseForm:FormBrowse  = QuestionHistoryBrowse.
   QuestionHistoryBrowseForm:FormButtons = QuestionHistoryBrowseButtons.
   QuestionHistoryBrowseForm:endForm(). 
   
   QuestionHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQuestionHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionHistoryDetails Procedure
PROCEDURE pQuestionHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questionhistory_details_form"}
   
   chrDisplayFieldList  = "QuestionHistoryID,QuestionID,QuestionCode,QuestionName,QuestionPrompt,IsRequired,"
                             + "QuestionTypeID,Active,CreatedDate,CreatedHour,CreatedMins,GateUserID,TransactionID".
                             
   
   QuestionHistoryDetailsForm = NEW dataForm("questionhistory_details_form").
   QuestionHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QuestionHistoryDetailsForm:FormWidth   = 545.
   QuestionHistoryDetailsForm:FormHeight  = 440.
   QuestionHistoryDetailsForm:FormTitle   = "Question History Details".
   QuestionHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QuestionHistoryDetailsForm:insertPaddingColumn(40).
   QuestionHistoryDetailsForm:insertColumn(110).
   QuestionHistoryDetailsForm:insertColumn(120).
   QuestionHistoryDetailsForm:insertColumn(20).
   QuestionHistoryDetailsForm:insertColumn(4).
   QuestionHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel(fTL("History ID")).
   QuestionHistoryDetailsForm:insertTextField("QuestionHistoryID", "", 200, TRUE).    
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("Question ID").
   QuestionHistoryDetailsForm:insertTextField("QuestionID", "", 200, TRUE).  
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("Question Code").
   QuestionHistoryDetailsForm:insertTextField("QuestionCode", "", 200, TRUE).  
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("Question Name").
   QuestionHistoryDetailsForm:insertTextField("QuestionName", "", 200, TRUE).
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("Question Prompt").
   QuestionHistoryDetailsForm:insertTextAreaField("QuestionPrompt", "", 200, TRUE).  
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("Question Type").
   QuestionHistoryDetailsForm:insertComboField("QuestionTypeID", "", 200, TRUE).  
   /* Question Type DropDown Options */
   FOR EACH QuestionType NO-LOCK /*idx=QuestionTypeID*/
      WHERE QuestionType.Active:      
      QuestionHistoryDetailsForm:insertComboPairs("QuestionTypeID", STRING(QuestionType.QuestionTypeID), QuestionType.QuestionTypeName).    
   END.  
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("Question Category").
   QuestionHistoryDetailsForm:insertComboField("QuestionCategoryID", "", 200, TRUE).  
   /* Question Type DropDown Options */
   FOR EACH QuestionCategory NO-LOCK /*idx=QuestionCategoryID*/
      WHERE QuestionCategory.Active = TRUE:      
      QuestionHistoryDetailsForm:insertComboPairs("QuestionCategoryID", STRING(QuestionCategory.QuestionCategoryID), QuestionCategory.QuestionCategoryName).    
   END.
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel(fTL("Is Required")). 
   QuestionHistoryDetailsForm:insertComboField("IsRequired", "", 200, TRUE).  
   QuestionHistoryDetailsForm:insertComboPairs("IsRequired", "yes", "Yes").
   QuestionHistoryDetailsForm:insertComboPairs("IsRequired", "no",  "No").
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel(fTL("Active")).
   QuestionHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("User").
   QuestionHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QuestionHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QuestionHistoryDetailsForm:startRow().
   QuestionHistoryDetailsForm:insertLabel("Created").
   QuestionHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QuestionHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QuestionHistoryDetailsForm:insertLabel(":").
   QuestionHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QuestionHistoryDetailsForm:insertHiddenField("question_browse_scroll","").
   QuestionHistoryDetailsForm:insertHiddenField("popup_questionhistory_browse", "").
   QuestionHistoryDetailsForm:insertHiddenField("QuestionHistoryID","").
   QuestionHistoryDetailsForm:insertHiddenField("form_name","questionhistory_details_form").
   QuestionHistoryDetailsForm:insertHiddenField("prog_name","adQuestion.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionHistoryDetailsForm}
   
   /* Create Button Bar */
   QuestionHistoryDetailsButtons = NEW buttonBar().
   
   QuestionHistoryDetailsButtons:addButton("questionhistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('questionhistory_details_form_popup');").
                                        
   QuestionHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionHistoryDetailsForm:FormButtons = QuestionHistoryDetailsButtons.
   
   QuestionHistoryDetailsForm:endForm(). 
   QuestionHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

