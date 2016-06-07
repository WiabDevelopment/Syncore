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

DEFINE VARIABLE intSelectedQuestionType           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQuestionTypeRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionTypeRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionTypeID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionTypeHistory       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QuestionTypeBrowseFrame           AS pageFrame.
DEFINE VARIABLE QuestionTypeBrowse                AS browseTable.
DEFINE VARIABLE QuestionTypeBrowseButtons         AS buttonBar.
DEFINE VARIABLE QuestionTypeDetailsForm           AS dataForm.
DEFINE VARIABLE QuestionTypeDetailsButtons        AS buttonBar.

DEFINE VARIABLE QuestionTypeHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE QuestionTypeHistoryBrowse         AS browseTable.
DEFINE VARIABLE QuestionTypeHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE QuestionTypeHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QuestionTypeHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrQuestionTypeID          = get-value("QuestionTypeID")
          intSelectedQuestionType    = INTEGER(chrQuestionTypeID)
          chrScrollToQuestionTypeRow = STRING(INTEGER(get-value("questiontype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQuestionTypeID <> "" THEN
     chrSelectQuestionTypeRow = 'selectQuestionTypeRow(document.getElementById("questiontype_browse_row_' + chrQuestionTypeID + '"),"' 
                                                         + chrQuestionTypeID +  '");'.
   
   IF get-value('popup_questiontypehistory_browse') = "yes" THEN
      chrPopupQuestionTypeHistory  = 'enablePopup("questiontypehistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("questiontype_browse").scrollTop=' + chrScrollToQuestionTypeRow 
                                                          + chrSelectQuestionTypeRow + chrPopupQuestionTypeHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "QuestionType Admin".
   ThisPage:FrameTitle    = "QuestionType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("questiontype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQuestionTypeBrowse.
   
   FIND FIRST QuestionType NO-LOCK
      WHERE QuestionType.QuestionTypeID = intSelectedQuestionType NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQuestionTypeDetails.
   RUN pQuestionTypeHistory.
   RUN pQuestionTypeHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QuestionTypeBrowseFrame           NO-ERROR.
   DELETE OBJECT QuestionTypeBrowse                NO-ERROR.
   DELETE OBJECT QuestionTypeBrowseButtons         NO-ERROR.
   DELETE OBJECT QuestionTypeDetailsForm           NO-ERROR.
   DELETE OBJECT QuestionTypeDetailsButtons        NO-ERROR.
   
   DELETE OBJECT QuestionTypeHistoryBrowseForm     NO-ERROR.   
   DELETE OBJECT QuestionTypeHistoryBrowse         NO-ERROR.
   DELETE OBJECT QuestionTypeHistoryBrowseButtons  NO-ERROR.
   
   DELETE OBJECT QuestionTypeHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT QuestionTypeHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionTypeBrowse Procedure 
PROCEDURE pQuestionTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "questiontype_details_form"}
   
   QuestionTypeBrowse = NEW browseTable("questiontype_browse").
   QuestionTypeBrowse:BrowseWidth  = 965.
   QuestionTypeBrowse:BrowseHeight = 455.
   QuestionTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QuestionTypeBrowse:insertColumn(fTL("TypeID"), 75, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionType}
   
   QuestionTypeBrowse:insertColumn(fTL("Type Code"),        120, "CHARACTER", "left", FALSE).
   QuestionTypeBrowse:insertColumn(fTL("Type Name"),        120, "CHARACTER", "left", FALSE).
   QuestionTypeBrowse:insertColumn(fTL("Question Options"), 150, "CHARACTER", "left", FALSE).
   QuestionTypeBrowse:insertColumn(fTL("Active"),            50, "LOGICAL",           FALSE).
   
   /*Body*/
   QuestionTypeBrowse:startBody().
   
   FOR EACH QuestionType NO-LOCK /*idx=QuestionTypeID*/
      BY QuestionType.QuestionTypeID
      BY QuestionType.Active DESC:
      
      QuestionTypeBrowse:startRow(QuestionType.QuestionTypeID, "selectQuestionTypeRow(this," + '"' + STRING(QuestionType.QuestionTypeID) 
                                                                  + '"' + ");", "").
      QuestionTypeBrowse:insertData(QuestionType.QuestionTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QuestionType}
      
      QuestionTypeBrowse:insertData(QuestionType.QuestionTypeCode, "left").
      QuestionTypeBrowse:insertData(QuestionType.QuestionTypeName, "left").
      QuestionTypeBrowse:insertData(QuestionType.QuestionOptions, "left").
      QuestionTypeBrowse:insertData(STRING(QuestionType.Active,"Yes/No")).
      
      /* Add hidden fields */
      QuestionTypeBrowse:insertHiddenData("QuestionTypeVersionID",QuestionType.VersionID).
      
      QuestionTypeBrowse:endRow().
      
   END. /*FOR EACH QuestionType NO-LOCK */
   
   QuestionTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionTypeBrowse:getErrors().
   
   /* Create a new frame */
   QuestionTypeBrowseFrame = NEW pageFrame().
   QuestionTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QuestionTypeBrowseFrame:FormAction="dbQuestionTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QuestionTypeBrowseFrame:formOpen("questiontype_browse_form").
   
   /* Start the Frame Header */
   QuestionTypeBrowseFrame:insertSpacer(5).
   QuestionTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QuestionTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QuestionTypeBrowseFrame:frameClose().
   QuestionTypeBrowseFrame:insertSpacer(10).
   
   QuestionTypeBrowseFrame:insertHiddenField("questiontype_browse_scroll","").
   QuestionTypeBrowseFrame:insertHiddenField("QuestionTypeID","").
   QuestionTypeBrowseFrame:insertHiddenField("QuestionTypeVersionID","").
   QuestionTypeBrowseFrame:insertHiddenField("popup_questiontypehistory_browse","").
   QuestionTypeBrowseFrame:insertHiddenField("form_name","questiontype_browse_form").
   QuestionTypeBrowseFrame:insertHiddenField("prog_name","adQuestionType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionTypeBrowseFrame}
   
   QuestionTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   QuestionTypeBrowseButtons = NEW buttonBar().
   QuestionTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   QuestionTypeBrowseButtons:addButton("questiontype_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQuestionTypeDetails('questiontype_details_form');",
                                             (IF intSelectedQuestionType > 0 THEN "" ELSE "Disabled")).
   
   QuestionTypeBrowseButtons:addButton("questiontype_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQuestionType('questiontype_details_form');",
                                             "").
                                             
   QuestionTypeBrowseButtons:addButton("questiontype_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   QuestionTypeBrowseButtons:addButton("questiontype_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteQuestionType();",
                                             (IF intSelectedQuestionType > 0 THEN "" ELSE "Disabled")).
   */
   
   QuestionTypeBrowseButtons:closeBar().  
   QuestionTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionTypeDetails Procedure 
PROCEDURE pQuestionTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "questiontype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionTypeID,QuestionTypeCode,QuestionTypeName,QuestionOptions,Active" 
          chrEditFieldList     = "QuestionOptions,Active" 
          chrNewFieldList      = "QuestionTypeCode,QuestionTypeName,QuestionOptions,Active" 
          chrRequiredFieldList = "QuestionTypeCode,QuestionTypeName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QuestionTypeDetailsForm = NEW dataForm("questiontype_details_form").
   QuestionTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QuestionTypeDetailsForm:FormAction = "dbQuestionTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionTypeDetailsForm:FormWidth   = 580.
   QuestionTypeDetailsForm:FormHeight  = 420.
   QuestionTypeDetailsForm:FormTitle   = "QuestionType Details".
   QuestionTypeDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QuestionTypeDetailsForm:insertPaddingColumn(30).
   QuestionTypeDetailsForm:insertColumn(150).
   QuestionTypeDetailsForm:insertColumn(160).
   QuestionTypeDetailsForm:insertColumn(20).
   QuestionTypeDetailsForm:insertColumn(4).
   QuestionTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   QuestionTypeDetailsForm:startRow().
   QuestionTypeDetailsForm:insertLabel("QuestionType ID").
   QuestionTypeDetailsForm:insertTextField("QuestionTypeID", "", 200, TRUE).  
   
   QuestionTypeDetailsForm:startRow().
   QuestionTypeDetailsForm:insertLabel("Question Type Code").
   QuestionTypeDetailsForm:insertTextField("QuestionTypeCode", "", 200, TRUE).  
   
   QuestionTypeDetailsForm:startRow().
   QuestionTypeDetailsForm:insertLabel("Question Type Name").
   QuestionTypeDetailsForm:insertTextField("QuestionTypeName", "", 200, TRUE).
   
   QuestionTypeDetailsForm:startRow().
   QuestionTypeDetailsForm:insertLabel("Question Options").
   QuestionTypeDetailsForm:insertTextAreaField("QuestionOptions", "", 200, TRUE).  

   QuestionTypeDetailsForm:startRow().
   QuestionTypeDetailsForm:insertLabel(fTL("Active")). 
   QuestionTypeDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QuestionTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQuestionTypeDetailsFields}
   
   /* Add Hidden Fields*/
   QuestionTypeDetailsForm:insertHiddenField("questiontype_browse_scroll", "").
   QuestionTypeDetailsForm:insertHiddenField("form_name", "questiontype_details_form").
   QuestionTypeDetailsForm:insertHiddenField("prog_name", "adQuestionType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionTypeDetailsForm}
   
   /* Create Button Bar */
   QuestionTypeDetailsButtons = NEW buttonBar().
   
   QuestionTypeDetailsButtons:addButton("questiontype_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQuestionType('questiontype_details_form');").
   
   QuestionTypeDetailsButtons:addButton("questiontype_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('questiontype_details_form_popup');").
   
   QuestionTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionTypeDetailsForm:FormButtons = QuestionTypeDetailsButtons.
   
   QuestionTypeDetailsForm:endForm(). 
   
   QuestionTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QuestionTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionTypeDetailsFields Procedure 
PROCEDURE pQuestionTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QuestionTypeDetailsForm:startRow().
         QuestionTypeDetailsForm:insertLabel(fTL("Field Label")).
         QuestionTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeHistory Procedure
PROCEDURE pQuestionTypeHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questiontypehistory_details_form"}
   
   FIND FIRST QuestionType WHERE QuestionType.QuestionTypeID = intSelectedQuestionType NO-LOCK NO-ERROR.
   
   QuestionTypeHistoryBrowseForm = NEW dataForm("questiontypehistory_browse_form").
   QuestionTypeHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QuestionTypeHistoryBrowseForm:FormWidth   = 850.
   QuestionTypeHistoryBrowseForm:FormHeight  = 540.
   QuestionTypeHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE QuestionType THEN " for QuestionTypeID: " 
                                                                  + STRING(QuestionType.QuestionTypeID) ELSE "").
   QuestionTypeHistoryBrowseForm:FormType    = "xxl_large".
   
   QuestionTypeHistoryBrowse = NEW browseTable("questiontypehistory_browse").
   QuestionTypeHistoryBrowse:BrowseWidth  = 830.
   QuestionTypeHistoryBrowse:BrowseHeight = 500.
   QuestionTypeHistoryBrowse:ExcelExport  = TRUE.
   QuestionTypeHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QuestionTypeHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionTypeHistory}

   QuestionTypeHistoryBrowse:insertColumn(fTL("Question Type Code"), 140, "CHARACTER", "left", FALSE).
   QuestionTypeHistoryBrowse:insertColumn(fTL("Question Type Name"), 140, "CHARACTER", "left", FALSE).
   QuestionTypeHistoryBrowse:insertColumn(fTL("Question Options"),   170, "CHARACTER", "left", FALSE).
   QuestionTypeHistoryBrowse:insertColumn(fTL("Active"),              50, "LOGICAL",           FALSE).
   QuestionTypeHistoryBrowse:insertColumn(fTL("User"),               120, "CHARACTER", "left", FALSE).
   QuestionTypeHistoryBrowse:insertColumn(fTL("Created"),            130, "CHARACTER", "left", FALSE).
   
   QuestionTypeHistoryBrowse:StartBody().
   
   IF AVAILABLE QuestionType THEN
   DO:
      /*List the QuestionTypeHistory*/
      FOR EACH QuestionTypeHistory NO-LOCK 
         WHERE  QuestionTypeHistory.QuestionTypeID = intSelectedQuestionType:
         
         FIND FIRST GateUser OF QuestionTypeHistory NO-LOCK NO-ERROR. /* idx=GateUserID */
       
         QuestionTypeHistoryBrowse:startRow(QuestionTypeHistory.QuestionTypeHistoryID, "selectHistoryRow(this," + '"' + STRING(QuestionTypeHistory.QuestionTypeHistoryID) 
                                                                     + '","questionTypeHistory"' + ");", "").
                                                                     
         QuestionTypeHistoryBrowse:insertData(QuestionTypeHistory.QuestionTypeHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionTypeHistory}
         
         QuestionTypeHistoryBrowse:insertData(QuestionTypeHistory.QuestionTypeCode, "left").
         QuestionTypeHistoryBrowse:insertData(QuestionTypeHistory.QuestionTypeName, "left").
         QuestionTypeHistoryBrowse:insertData(QuestionTypeHistory.QuestionOptions, "left").                            
         QuestionTypeHistoryBrowse:insertData(STRING(QuestionTypeHistory.Active, "Yes/No")).
         QuestionTypeHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QuestionTypeHistoryBrowse:insertData(fDisplayDate&Time(QuestionTypeHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         QuestionTypeHistoryBrowse:insertHiddendata("QuestionTypeHistoryID",QuestionTypeHistory.QuestionTypeHistoryID).
         
         QuestionTypeHistoryBrowse:endRow().
      
      END. /* FOR EACH QuestionTypeHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   QuestionTypeHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionTypeHistoryBrowse:getErrors().
   
   QuestionTypeHistoryBrowseForm:insertHiddenField("QuestionTypeHistoryID","").
   QuestionTypeHistoryBrowseForm:insertHiddenField("popup_questiontypehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionTypeHistoryBrowseForm}
   
   /* Create Button Bar */
   QuestionTypeHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QuestionTypeHistoryBrowseButtons:addButton("questiontypehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQuestionTypeHistoryDetails('questiontypehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QuestionTypeHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   QuestionTypeHistoryBrowseButtons:addButton("questiontypehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('questiontypehistory_browse_form_popup');").
   
   QuestionTypeHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionTypeHistoryBrowseForm:FormBrowse  = QuestionTypeHistoryBrowse.
   QuestionTypeHistoryBrowseForm:FormButtons = QuestionTypeHistoryBrowseButtons.
   QuestionTypeHistoryBrowseForm:endForm(). 
   
   QuestionTypeHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQuestionTypeHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionTypeHistoryDetails Procedure
PROCEDURE pQuestionTypeHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questiontypehistory_details_form"}
   
   chrDisplayFieldList  = "QuestionTypeHistoryID,QuestionTypeID,QuestionTypeCode,QuestionTypeName,QuestionOptions,"
                             + "Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QuestionTypeHistoryDetailsForm = NEW dataForm("questiontypehistory_details_form").
   QuestionTypeHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QuestionTypeHistoryDetailsForm:FormWidth   = 545.
   QuestionTypeHistoryDetailsForm:FormHeight  = 440.
   QuestionTypeHistoryDetailsForm:FormTitle   = "QuestionType History Details".
   QuestionTypeHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QuestionTypeHistoryDetailsForm:insertPaddingColumn(40).
   QuestionTypeHistoryDetailsForm:insertColumn(110).
   QuestionTypeHistoryDetailsForm:insertColumn(120).
   QuestionTypeHistoryDetailsForm:insertColumn(20).
   QuestionTypeHistoryDetailsForm:insertColumn(4).
   QuestionTypeHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel(fTL("History ID")).
   QuestionTypeHistoryDetailsForm:insertTextField("QuestionTypeHistoryID", "", 200, TRUE).    
   
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel(fTL("QuestionTypeID")).
   QuestionTypeHistoryDetailsForm:insertTextField("QuestionTypeID", "", 200, TRUE).
   
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel(fTL("Type Code")).
   QuestionTypeHistoryDetailsForm:insertTextField("QuestionTypeCode", "", 200, TRUE).
   
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel(fTL("Type Name")).
   QuestionTypeHistoryDetailsForm:insertTextField("QuestionTypeName", "", 200, TRUE).
   
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel(fTL("Question Options")).
   QuestionTypeHistoryDetailsForm:insertTextField("QuestionOptions", "", 200, TRUE).
   
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel(fTL("Active")).
   QuestionTypeHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionTypeHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionTypeHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel("User").
   QuestionTypeHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QuestionTypeHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QuestionTypeHistoryDetailsForm:startRow().
   QuestionTypeHistoryDetailsForm:insertLabel("Created").
   QuestionTypeHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QuestionTypeHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QuestionTypeHistoryDetailsForm:insertLabel(":").
   QuestionTypeHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   
   /* Add Hidden Fields*/
   QuestionTypeHistoryDetailsForm:insertHiddenField("questiontype_browse_scroll","").
   QuestionTypeHistoryDetailsForm:insertHiddenField("popup_questiontypehistory_browse", "").
   QuestionTypeHistoryDetailsForm:insertHiddenField("QuestionTypeHistoryID","").
   QuestionTypeHistoryDetailsForm:insertHiddenField("form_name","questiontypehistory_details_form").
   QuestionTypeHistoryDetailsForm:insertHiddenField("prog_name","adQuestionType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionTypeHistoryDetailsForm}
   
   /* Create Button Bar */
   QuestionTypeHistoryDetailsButtons = NEW buttonBar().
   
   QuestionTypeHistoryDetailsButtons:addButton("questiontypehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('questiontypehistory_details_form_popup');").
                                        
   QuestionTypeHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionTypeHistoryDetailsForm:FormButtons = QuestionTypeHistoryDetailsButtons.
   
   QuestionTypeHistoryDetailsForm:endForm(). 
   QuestionTypeHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

