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

DEFINE VARIABLE intSelectedQuestionnaireConfig        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQuestionnaireConfigRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionnaireConfigRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionnaireConfigID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionnaireConfigHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QuestionnaireConfigBrowseFrame        AS pageFrame.
DEFINE VARIABLE QuestionnaireConfigBrowse             AS browseTable.
DEFINE VARIABLE QuestionnaireConfigBrowseButtons      AS buttonBar.
DEFINE VARIABLE QuestionnaireConfigDetailsForm        AS dataForm.
DEFINE VARIABLE QuestionnaireConfigDetailsButtons     AS buttonBar.

DEFINE VARIABLE QuestionnaireConfigHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE QuestionnaireConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE QuestionnaireConfigHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE QuestionnaireConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QuestionnaireConfigHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrQuestionnaireConfigID          = get-value("QuestionnaireConfigID")
          intSelectedQuestionnaireConfig    = INTEGER(chrQuestionnaireConfigID)
          chrScrollToQuestionnaireConfigRow = STRING(INTEGER(get-value("questionnaireconfig_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQuestionnaireConfigID <> "" THEN
     chrSelectQuestionnaireConfigRow = 'selectQuestionnaireConfigRow(document.getElementById("questionnaireconfig_browse_row_' + chrQuestionnaireConfigID + '"),"' 
                                                         + chrQuestionnaireConfigID +  '");'.
   
   IF get-value('popup_questionnaireconfighistory_browse') = "yes" THEN
      chrPopupQuestionnaireConfigHistory  = 'enablePopup("questionnaireconfighistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("questionnaireconfig_browse").scrollTop=' + chrScrollToQuestionnaireConfigRow 
                                                          + chrSelectQuestionnaireConfigRow + chrPopupQuestionnaireConfigHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "QuestionnaireConfig Admin".
   ThisPage:FrameTitle    = "QuestionnaireConfig Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("questionnaireconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQuestionnaireConfigBrowse.
   
   FIND FIRST QuestionnaireConfig NO-LOCK
      WHERE QuestionnaireConfig.QuestionnaireConfigID = intSelectedQuestionnaireConfig NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQuestionnaireConfigDetails.
   RUN pQuestionnaireConfigHistory.
   RUN pQuestionnaireConfigHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QuestionnaireConfigBrowseFrame           NO-ERROR.
   DELETE OBJECT QuestionnaireConfigBrowse                NO-ERROR.
   DELETE OBJECT QuestionnaireConfigBrowseButtons         NO-ERROR.
   DELETE OBJECT QuestionnaireConfigDetailsForm           NO-ERROR.
   DELETE OBJECT QuestionnaireConfigDetailsButtons        NO-ERROR.
   
   DELETE OBJECT QuestionnaireConfigHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT QuestionnaireConfigHistoryBrowse            NO-ERROR.
   DELETE OBJECT QuestionnaireConfigHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT QuestionnaireConfigHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT QuestionnaireConfigHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireConfigBrowse Procedure 
PROCEDURE pQuestionnaireConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "questionnaireconfig_details_form"}
   
   QuestionnaireConfigBrowse = NEW browseTable("questionnaireconfig_browse").
   QuestionnaireConfigBrowse:BrowseWidth  = 965.
   QuestionnaireConfigBrowse:BrowseHeight = 455.
   QuestionnaireConfigBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QuestionnaireConfigBrowse:insertColumn(fTL("TypeID"), 75, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionnaireConfig}
   
   QuestionnaireConfigBrowse:insertColumn(fTL("List Seq"),         60, "INTEGER", FALSE).
   QuestionnaireConfigBrowse:insertColumn(fTL("Type Code"),       120, "CHARACTER", "left", FALSE).
   QuestionnaireConfigBrowse:insertColumn(fTL("Type Name"),       120, "CHARACTER", "left", FALSE).
   QuestionnaireConfigBrowse:insertColumn(fTL("Type Descr"),      150, "CHARACTER", "left", FALSE).
   QuestionnaireConfigBrowse:insertColumn(fTL("CaseSensitive"),   100, "INTEGER", FALSE).
   QuestionnaireConfigBrowse:insertColumn(fTL("Active"),           50, "LOGICAL", FALSE).
   
   /*Body*/
   QuestionnaireConfigBrowse:startBody().
   
   FOR EACH QuestionnaireConfig NO-LOCK /*idx=ActiveListingSequence*/
      WHERE QuestionnaireConfig.Active
      BY    QuestionnaireConfig.ListingSequence:
      
      QuestionnaireConfigBrowse:startRow(QuestionnaireConfig.QuestionnaireConfigID, "selectQuestionnaireConfigRow(this," + '"' + STRING(QuestionnaireConfig.QuestionnaireConfigID) 
                                                                  + '"' + ");", "").
      QuestionnaireConfigBrowse:insertData(QuestionnaireConfig.QuestionnaireConfigID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QuestionnaireConfig}
      
      QuestionnaireConfigBrowse:insertData(STRING(QuestionnaireConfig.ListingSequence)).
      QuestionnaireConfigBrowse:insertData(QuestionnaireConfig.TypeCode, "left").
      QuestionnaireConfigBrowse:insertData(QuestionnaireConfig.TypeName, "left").
      QuestionnaireConfigBrowse:insertData(QuestionnaireConfig.TypeDescr, "left").
      QuestionnaireConfigBrowse:insertData(STRING(QuestionnaireConfig.CaseSensitive,"Yes/No")).
      QuestionnaireConfigBrowse:insertData(STRING(QuestionnaireConfig.Active,"Yes/No")).
      
      /* Add hidden fields */
      QuestionnaireConfigBrowse:insertHiddenData("QuestionnaireConfigVersionID",QuestionnaireConfig.VersionID).
      
      QuestionnaireConfigBrowse:endRow().
      
   END. /*FOR EACH QuestionnaireConfig NO-LOCK */
   
   QuestionnaireConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireConfigBrowse:getErrors().
   
   /* Create a new frame */
   QuestionnaireConfigBrowseFrame = NEW pageFrame().
   QuestionnaireConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QuestionnaireConfigBrowseFrame:FormAction="dbQuestionnaireConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QuestionnaireConfigBrowseFrame:formOpen("questionnaireconfig_browse_form").
   
   /* Start the Frame Header */
   QuestionnaireConfigBrowseFrame:insertSpacer(5).
   QuestionnaireConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QuestionnaireConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QuestionnaireConfigBrowseFrame:frameClose().
   QuestionnaireConfigBrowseFrame:insertSpacer(10).
   
   QuestionnaireConfigBrowseFrame:insertHiddenField("questionnaireconfig_browse_scroll","").
   QuestionnaireConfigBrowseFrame:insertHiddenField("QuestionnaireConfigID","").
   QuestionnaireConfigBrowseFrame:insertHiddenField("QuestionnaireConfigVersionID","").
   QuestionnaireConfigBrowseFrame:insertHiddenField("popup_questionnaireconfighistory_browse","").
   QuestionnaireConfigBrowseFrame:insertHiddenField("form_name","questionnaireconfig_browse_form").
   QuestionnaireConfigBrowseFrame:insertHiddenField("prog_name","adQuestionnaireConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireConfigBrowseFrame}
   
   QuestionnaireConfigBrowseFrame:formClose().
   
   /* Create Button Bar */
   QuestionnaireConfigBrowseButtons = NEW buttonBar().
   QuestionnaireConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   QuestionnaireConfigBrowseButtons:addButton("questionnaireconfig_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQuestionnaireConfigDetails('questionnaireconfig_details_form');",
                                             "Disabled").
   
   QuestionnaireConfigBrowseButtons:addButton("questionnaireconfig_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQuestionnaireConfig('questionnaireconfig_details_form');",
                                             "").
                                             
   QuestionnaireConfigBrowseButtons:addButton("questionnaireconfig_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   QuestionnaireConfigBrowseButtons:closeBar().  
   QuestionnaireConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireConfigDetails Procedure 
PROCEDURE pQuestionnaireConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "questionnaireconfig_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionnaireConfigID" 
          chrEditFieldList     = "" 
          chrNewFieldList      = "" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QuestionnaireConfigDetailsForm = NEW dataForm("questionnaireconfig_details_form").
   QuestionnaireConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QuestionnaireConfigDetailsForm:FormAction = "dbQuestionnaireConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireConfigDetailsForm:FormWidth   = 580.
   QuestionnaireConfigDetailsForm:FormHeight  = 420.
   QuestionnaireConfigDetailsForm:FormTitle   = "QuestionnaireConfig Details".
   QuestionnaireConfigDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QuestionnaireConfigDetailsForm:insertPaddingColumn(30).
   QuestionnaireConfigDetailsForm:insertColumn(150).
   QuestionnaireConfigDetailsForm:insertColumn(160).
   QuestionnaireConfigDetailsForm:insertColumn(20).
   QuestionnaireConfigDetailsForm:insertColumn(4).
   QuestionnaireConfigDetailsForm:insertColumn(110).
   
   /* Fields */
   QuestionnaireConfigDetailsForm:startRow().
   QuestionnaireConfigDetailsForm:insertLabel("Type ID").
   QuestionnaireConfigDetailsForm:insertTextField("QuestionnaireConfigID", "", 110, TRUE).  
   
   QuestionnaireConfigDetailsForm:startRow().
   QuestionnaireConfigDetailsForm:insertLabel("Listing Seq").
   QuestionnaireConfigDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   QuestionnaireConfigDetailsForm:startRow().
   QuestionnaireConfigDetailsForm:insertLabel("Type Code").
   QuestionnaireConfigDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   QuestionnaireConfigDetailsForm:startRow().
   QuestionnaireConfigDetailsForm:insertLabel("Type Name").
   QuestionnaireConfigDetailsForm:insertTextField("TypeName", "", 110, TRUE).
   
   QuestionnaireConfigDetailsForm:startRow().
   QuestionnaireConfigDetailsForm:insertLabel("Type Descr").
   QuestionnaireConfigDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).  
   
   QuestionnaireConfigDetailsForm:startRow().
   QuestionnaireConfigDetailsForm:insertLabel(fTL("Case Sensitive")). 
   QuestionnaireConfigDetailsForm:insertComboField("CaseSensitive", "", 180, TRUE).  
   QuestionnaireConfigDetailsForm:insertComboPairs("CaseSensitive", "yes", "Case Sensitive").
   QuestionnaireConfigDetailsForm:insertComboPairs("CaseSensitive", "no",  "Not Case Sensitive").

   QuestionnaireConfigDetailsForm:startRow().
   QuestionnaireConfigDetailsForm:insertLabel(fTL("Active")). 
   QuestionnaireConfigDetailsForm:insertComboField("Active", "", 110, TRUE).  
   QuestionnaireConfigDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireConfigDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQuestionnaireConfigDetailsFields}
   
   /* Add Hidden Fields*/
   QuestionnaireConfigDetailsForm:insertHiddenField("questionnaireconfig_browse_scroll", "").
   QuestionnaireConfigDetailsForm:insertHiddenField("form_name", "questionnaireconfig_details_form").
   QuestionnaireConfigDetailsForm:insertHiddenField("prog_name", "adQuestionnaireConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireConfigDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireConfigDetailsButtons = NEW buttonBar().
   
   QuestionnaireConfigDetailsButtons:addButton("questionnaireconfig_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQuestionnaireConfig('questionnaireconfig_details_form');").
   
   QuestionnaireConfigDetailsButtons:addButton("questionnaireconfig_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('questionnaireconfig_details_form_popup');").
   
   QuestionnaireConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireConfigDetailsForm:FormButtons = QuestionnaireConfigDetailsButtons.
   
   QuestionnaireConfigDetailsForm:endForm(). 
   
   QuestionnaireConfigDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QuestionnaireConfigDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireConfigDetailsFields Procedure 
PROCEDURE pQuestionnaireConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QuestionnaireConfigDetailsForm:startRow().
         QuestionnaireConfigDetailsForm:insertLabel(fTL("Field Label")).
         QuestionnaireConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adQuestionnaireConfigAdmin_questionnaireconfig_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireConfigHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireConfigHistory Procedure
PROCEDURE pQuestionnaireConfigHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionnaireconfighistory_details_form"}
   
   FIND FIRST QuestionnaireConfig WHERE QuestionnaireConfig.QuestionnaireConfigID = intSelectedQuestionnaireConfig NO-LOCK NO-ERROR.
   
   QuestionnaireConfigHistoryBrowseForm = NEW dataForm("questionnaireconfighistory_browse_form").
   QuestionnaireConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QuestionnaireConfigHistoryBrowseForm:FormWidth   = 850.
   QuestionnaireConfigHistoryBrowseForm:FormHeight  = 540.
   QuestionnaireConfigHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE QuestionnaireConfig THEN " for QuestionnaireConfigID: " 
                                                                  + STRING(QuestionnaireConfig.QuestionnaireConfigID) ELSE "").
   QuestionnaireConfigHistoryBrowseForm:FormType    = "xxl_large".
   
   QuestionnaireConfigHistoryBrowse = NEW browseTable("questionnaireconfighistory_browse").
   QuestionnaireConfigHistoryBrowse:BrowseWidth  = 830.
   QuestionnaireConfigHistoryBrowse:BrowseHeight = 500.
   QuestionnaireConfigHistoryBrowse:ExcelExport  = TRUE.
   QuestionnaireConfigHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionnaireConfigHistory}

   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("Listing Seq"),     70, "INTEGER",           FALSE).   
   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("User"),           120, "CHARACTER", "left", FALSE).
   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("Type Code"),       80, "CHARACTER", "left", FALSE).
   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("Type Name"),       80, "CHARACTER", "left", FALSE).
   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("Type Descr"),     140, "CHARACTER", "left", FALSE).
   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("Active"),          50, "LOGICAL",           FALSE).
   QuestionnaireConfigHistoryBrowse:insertColumn(fTL("Created"),        135, "CHARACTER", "left", FALSE).
   
   QuestionnaireConfigHistoryBrowse:StartBody().
   
   IF AVAILABLE QuestionnaireConfig THEN
   DO:
      /*List the QuestionnaireConfigHistory*/
      FOR EACH QuestionnaireConfigHistory NO-LOCK 
         WHERE  QuestionnaireConfigHistory.QuestionnaireConfigID = intSelectedQuestionnaireConfig
         BY QuestionnaireConfigHistory.QuestionnaireConfigHistoryID:
         
         FIND FIRST GateUser OF QuestionnaireConfigHistory NO-LOCK NO-ERROR.
       
         QuestionnaireConfigHistoryBrowse:startRow(QuestionnaireConfigHistory.QuestionnaireConfigHistoryID, "selectHistoryRow(this," + '"' + STRING(QuestionnaireConfigHistory.QuestionnaireConfigHistoryID) 
                                                                     + '","questionnaireConfigHistory"' + ");", "").
         QuestionnaireConfigHistoryBrowse:insertData(QuestionnaireConfigHistory.QuestionnaireConfigHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionnaireConfigHistory}
         
         QuestionnaireConfigHistoryBrowse:insertData(QuestionnaireConfigHistory.ListingSequence, "").
         QuestionnaireConfigHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QuestionnaireConfigHistoryBrowse:insertData(QuestionnaireConfigHistory.TypeCode, "left").
         QuestionnaireConfigHistoryBrowse:insertData(QuestionnaireConfigHistory.TypeName, "left").
         QuestionnaireConfigHistoryBrowse:insertData(QuestionnaireConfigHistory.TypeDescr, "left").                            
         QuestionnaireConfigHistoryBrowse:insertData(STRING(QuestionnaireConfigHistory.Active, "Yes/No")).
         QuestionnaireConfigHistoryBrowse:insertData(fDisplayDate&Time(QuestionnaireConfigHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         QuestionnaireConfigHistoryBrowse:insertHiddendata("QuestionnaireConfigHistoryID",QuestionnaireConfigHistory.QuestionnaireConfigHistoryID).
         
         QuestionnaireConfigHistoryBrowse:endRow().
      
      END. /* FOR EACH QuestionnaireConfigHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   QuestionnaireConfigHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireConfigHistoryBrowse:getErrors().
   
   QuestionnaireConfigHistoryBrowseForm:insertHiddenField("QuestionnaireConfigHistoryID","").
   QuestionnaireConfigHistoryBrowseForm:insertHiddenField("popup_questionnaireconfighistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   QuestionnaireConfigHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QuestionnaireConfigHistoryBrowseButtons:addButton("questionnaireconfighistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQuestionnaireConfigHistoryDetails('questionnaireconfighistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QuestionnaireConfigHistoryBrowseButtons:addButton("questionnaireconfighistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_questionnaireconfighistory_browse.xml')").*/
   
   QuestionnaireConfigHistoryBrowseButtons:addButton("questionnaireconfighistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('questionnaireconfighistory_browse_form_popup');").
   
   QuestionnaireConfigHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireConfigHistoryBrowseForm:FormBrowse  = QuestionnaireConfigHistoryBrowse.
   QuestionnaireConfigHistoryBrowseForm:FormButtons = QuestionnaireConfigHistoryBrowseButtons.
   QuestionnaireConfigHistoryBrowseForm:endForm(). 
   
   QuestionnaireConfigHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireConfigHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireConfigHistoryDetails Procedure
PROCEDURE pQuestionnaireConfigHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questionnaireconfighistory_details_form"}
   
   chrDisplayFieldList  = "QuestionnaireConfigHistoryID,QuestionnaireConfigID,"
                             + "CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QuestionnaireConfigHistoryDetailsForm = NEW dataForm("questionnaireconfighistory_details_form").
   QuestionnaireConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QuestionnaireConfigHistoryDetailsForm:FormWidth   = 545.
   QuestionnaireConfigHistoryDetailsForm:FormHeight  = 440.
   QuestionnaireConfigHistoryDetailsForm:FormTitle   = "Questionnaire Config History Details".
   QuestionnaireConfigHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QuestionnaireConfigHistoryDetailsForm:insertPaddingColumn(40).
   QuestionnaireConfigHistoryDetailsForm:insertColumn(110).
   QuestionnaireConfigHistoryDetailsForm:insertColumn(120).
   QuestionnaireConfigHistoryDetailsForm:insertColumn(20).
   QuestionnaireConfigHistoryDetailsForm:insertColumn(4).
   QuestionnaireConfigHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("HistoryID")).
   QuestionnaireConfigHistoryDetailsForm:insertTextField("QuestionnaireConfigHistoryID", "", 90, TRUE).    
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("QuestionnaireConfigID")).
   QuestionnaireConfigHistoryDetailsForm:insertTextField("QuestionnaireConfigID", "", 90, TRUE).
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("TypeCode")).
   QuestionnaireConfigHistoryDetailsForm:insertTextField("TypeCode", "", 90, TRUE).
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("TypeName")).
   QuestionnaireConfigHistoryDetailsForm:insertTextField("TypeName", "", 90, TRUE).
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("TypeDescr")).
   QuestionnaireConfigHistoryDetailsForm:insertTextField("TypeDescr", "", 90, TRUE).
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("CaseSensitive")). 
   QuestionnaireConfigHistoryDetailsForm:insertComboField("CaseSensitive", "", 110, TRUE).  
   QuestionnaireConfigHistoryDetailsForm:insertComboPairs("CaseSensitive", "yes", "Active").
   QuestionnaireConfigHistoryDetailsForm:insertComboPairs("CaseSensitive", "no",  "Not Active").
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("ListingSequence")).
   QuestionnaireConfigHistoryDetailsForm:insertTextField("ListingSequence", "", 90, TRUE).
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel(fTL("Active")).
   QuestionnaireConfigHistoryDetailsForm:insertComboField("Active", "", 110, TRUE).
   QuestionnaireConfigHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireConfigHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel("User").
   QuestionnaireConfigHistoryDetailsForm:insertComboField("GateUserID", "", 110, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QuestionnaireConfigHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QuestionnaireConfigHistoryDetailsForm:startRow().
   QuestionnaireConfigHistoryDetailsForm:insertLabel("Created").
   QuestionnaireConfigHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QuestionnaireConfigHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QuestionnaireConfigHistoryDetailsForm:insertLabel(":").
   QuestionnaireConfigHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QuestionnaireConfigHistoryDetailsForm:insertHiddenField("questionnaireconfig_browse_scroll","").
   QuestionnaireConfigHistoryDetailsForm:insertHiddenField("popup_questionnaireconfighistory_browse", "").
   QuestionnaireConfigHistoryDetailsForm:insertHiddenField("QuestionnaireConfigHistoryID","").
   QuestionnaireConfigHistoryDetailsForm:insertHiddenField("form_name","questionnaireconfighistory_details_form").
   QuestionnaireConfigHistoryDetailsForm:insertHiddenField("prog_name","adQuestionnaireConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireConfigHistoryDetailsButtons = NEW buttonBar().
   
   QuestionnaireConfigHistoryDetailsButtons:addButton("questionnaireconfighistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('questionnaireconfighistory_details_form_popup');").
                                        
   QuestionnaireConfigHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireConfigHistoryDetailsForm:FormButtons = QuestionnaireConfigHistoryDetailsButtons.
   
   QuestionnaireConfigHistoryDetailsForm:endForm(). 
   QuestionnaireConfigHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

