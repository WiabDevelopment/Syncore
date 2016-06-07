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

  Created: 23/10/2015

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

DEFINE VARIABLE intSelectedNetworkError        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkErrorRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkErrorRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkErrorID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupNetworkErrorHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE NetworkErrorBrowseFrame        AS pageFrame.
DEFINE VARIABLE NetworkErrorBrowse             AS browseTable.
DEFINE VARIABLE NetworkErrorBrowseButtons      AS buttonBar.
DEFINE VARIABLE NetworkErrorDetailsForm        AS dataForm.
DEFINE VARIABLE NetworkErrorDetailsButtons     AS buttonBar.

DEFINE VARIABLE NetworkErrorHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE NetworkErrorHistoryBrowse         AS browseTable.
DEFINE VARIABLE NetworkErrorHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE NetworkErrorHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE NetworkErrorHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrNetworkErrorID          = get-value("NetworkErrorID")
          intSelectedNetworkError    = INTEGER(chrNetworkErrorID)
          chrScrollToNetworkErrorRow = STRING(INTEGER(get-value("networkerror_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrNetworkErrorID <> "" THEN
     chrSelectNetworkErrorRow = 'selectNetworkErrorRow(document.getElementById("networkerror_browse_row_' + chrNetworkErrorID + '"),"' 
                                                         + chrNetworkErrorID +  '");'.
   
   IF get-value('popup_networkerrorhistory_browse') = "yes" THEN
      chrPopupNetworkErrorHistory  = 'enablePopup("networkerrorhistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkerror_browse").scrollTop=' + chrScrollToNetworkErrorRow 
                                                          + chrSelectNetworkErrorRow + chrPopupNetworkErrorHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "NetworkError Admin".
   ThisPage:FrameTitle    = "NetworkError Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("networkerror.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkErrorBrowse.
   
   FIND FIRST NetworkError NO-LOCK
      WHERE NetworkError.NetworkErrorID = intSelectedNetworkError NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pNetworkErrorDetails.
   RUN pNetworkErrorHistory.
   RUN pNetworkErrorHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT NetworkErrorBrowseFrame           NO-ERROR.
   DELETE OBJECT NetworkErrorBrowse                NO-ERROR.
   DELETE OBJECT NetworkErrorBrowseButtons         NO-ERROR.
   DELETE OBJECT NetworkErrorDetailsForm           NO-ERROR.
   DELETE OBJECT NetworkErrorDetailsButtons        NO-ERROR.
   
   DELETE OBJECT NetworkErrorHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT NetworkErrorHistoryBrowse            NO-ERROR.
   DELETE OBJECT NetworkErrorHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT NetworkErrorHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT NetworkErrorHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorBrowse Procedure 
PROCEDURE pNetworkErrorBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkerror_details_form"}
   
   NetworkErrorBrowse = NEW browseTable("networkerror_browse").
   NetworkErrorBrowse:BrowseWidth  = 965.
   NetworkErrorBrowse:BrowseHeight = 455.
   NetworkErrorBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkErrorBrowse:insertColumn(fTL("Error ID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkError}
   
   NetworkErrorBrowse:insertColumn(fTL("Reader"),           100, "INTEGER",   "left", FALSE).
   NetworkErrorBrowse:insertColumn(fTL("ActionPoint"),      100, "INTEGER",   "left", FALSE).
   NetworkErrorBrowse:insertColumn(fTL("Error Type"),       100, "INTEGER",   "left", FALSE).
   NetworkErrorBrowse:insertColumn(fTL("Error Message"),    150, "CHARACTER", "left", FALSE).
   NetworkErrorBrowse:insertColumn(fTL("Raised"),           100, "CHARACTER", "left", FALSE).
   NetworkErrorBrowse:insertColumn(fTL("NotificationSent"), 100, "CHARACTER", "left", FALSE).
   NetworkErrorBrowse:insertColumn(fTL("Resolved"),         100, "CHARACTER",         FALSE).
   
   /*Body*/
   NetworkErrorBrowse:startBody().
   
   FOR EACH NetworkError NO-LOCK /*idx=ActiveListingSequence*/
      BY    NetworkError.NetworkErrorID:
         
      FIND FIRST NetworkReader OF NetworkError NO-LOCK NO-ERROR.
/*      FIND FIRST NetworkActionPoint OF NetworkError NO-LOCK NO-ERROR.*/
      FIND FIRST NetworkActionPoint OF NetworkError NO-LOCK NO-ERROR.
      FIND FIRST NetworkErrorType OF NetworkError NO-LOCK NO-ERROR.  
      
      NetworkErrorBrowse:startRow(NetworkError.NetworkErrorID, "selectNetworkErrorRow(this," + '"' + STRING(NetworkError.NetworkErrorID) 
                                                                  + '"' + ");", "").
      NetworkErrorBrowse:insertData(NetworkError.NetworkErrorID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkError}
      
      NetworkErrorBrowse:insertData((IF AVAILABLE NetworkReader THEN NetworkReader.NetworkReaderName ELSE ""), "left").
      NetworkErrorBrowse:insertData((IF AVAILABLE NetworkActionPoint THEN NetworkActionPoint.NetworkActionPointName ELSE ""), "left").
      NetworkErrorBrowse:insertData((IF AVAILABLE NetworkErrorType THEN NetworkErrorType.NetworkErrorTypeName ELSE ""), "left").
      NetworkErrorBrowse:insertData(NetworkError.ErrorMessage, "left").
      NetworkErrorBrowse:insertData(fDisplayDate&Time(NetworkError.Raised,"y/m/d H:M:S"), "left").
      NetworkErrorBrowse:insertData(fDisplayDate&Time(NetworkError.NotificationSent,"y/m/d H:M:S"), "left").
      NetworkErrorBrowse:insertData(fDisplayDate&Time(NetworkError.Resolved,"y/m/d H:M:S"), "left").
      
/*      (fDisplayDate&Time(NetworkErrorHistory.Created,"y/m/d H:M:S"), "left").*/
      
      /* Add hidden fields */
      NetworkErrorBrowse:insertHiddenData("NetworkErrorVersionID",NetworkError.VersionID).
      
      NetworkErrorBrowse:endRow().
      
   END. /*FOR EACH NetworkError NO-LOCK */
   
   NetworkErrorBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkErrorBrowse:getErrors().
   
   /* Create a new frame */
   NetworkErrorBrowseFrame = NEW pageFrame().
   NetworkErrorBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkErrorBrowseFrame:FormAction="dbNetworkErrorUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   NetworkErrorBrowseFrame:formOpen("networkerror_browse_form").
   
   /* Start the Frame Header */
   NetworkErrorBrowseFrame:insertSpacer(5).
   NetworkErrorBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkErrorBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkErrorBrowseFrame:frameClose().
   NetworkErrorBrowseFrame:insertSpacer(10).
   
   NetworkErrorBrowseFrame:insertHiddenField("networkerror_browse_scroll","").
   NetworkErrorBrowseFrame:insertHiddenField("NetworkErrorID","").
   NetworkErrorBrowseFrame:insertHiddenField("NetworkErrorVersionID","").
   NetworkErrorBrowseFrame:insertHiddenField("popup_networkerrorhistory_browse","").
   NetworkErrorBrowseFrame:insertHiddenField("form_name","networkerror_browse_form").
   NetworkErrorBrowseFrame:insertHiddenField("prog_name","adNetworkError.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorBrowseFrame}
   
   NetworkErrorBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkErrorBrowseButtons = NEW buttonBar().
   NetworkErrorBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   NetworkErrorBrowseButtons:addButton("networkerror_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewNetworkErrorDetails('networkerror_details_form');",
                                             "Disabled").
   
/*   NetworkErrorBrowseButtons:addButton("networkerror_browse_form_btn_create",                   */
/*                                             fTL("Create"),                                     */
/*                                             "createNetworkError('networkerror_details_form');",*/
/*                                             "").                                               */
                                             
   NetworkErrorBrowseButtons:addButton("networkerror_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   NetworkErrorBrowseButtons:addButton("networkerror_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteNetworkError();",
                                             (IF intSelectedNetworkError > 0 THEN "" ELSE "Disabled")).
   */
   
   NetworkErrorBrowseButtons:closeBar().  
   NetworkErrorBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorDetails Procedure 
PROCEDURE pNetworkErrorDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkerror_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkErrorID,NetworkReaderID,NetworkActionPointID,NetworkErrorTypeID,ErrorMessage"
                               + ",RaisedDate,RaisedHour,RaisedMins"
                               + ",NotificationSentDate,NotificationSentHour,NotificationSentMins,ResolvedDate,ResolvedHour"
                               + ",ResolvedMins"
          chrEditFieldList     = ""
          chrNewFieldList      = "" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkErrorDetailsForm = NEW dataForm("networkerror_details_form").
   NetworkErrorDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkErrorDetailsForm:FormAction = "dbNetworkErrorUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkErrorDetailsForm:FormWidth   = 580.
   NetworkErrorDetailsForm:FormHeight  = 420.
   NetworkErrorDetailsForm:FormTitle   = "NetworkError Details".
   NetworkErrorDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkErrorDetailsForm:insertPaddingColumn(30).
   NetworkErrorDetailsForm:insertColumn(150).
   NetworkErrorDetailsForm:insertColumn(160).
   NetworkErrorDetailsForm:insertColumn(20).
   NetworkErrorDetailsForm:insertColumn(4).
   NetworkErrorDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel("Error ID").
   NetworkErrorDetailsForm:insertTextField("NetworkErrorID", "", 200, TRUE).  
   
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel("Reader").
   NetworkErrorDetailsForm:insertComboField("NetworkReaderID", "", 200, TRUE).
    FOR EACH NetworkReader NO-LOCK
      BY NetworkReader.NetworkReaderName:
         
      NetworkErrorDetailsForm:insertComboPairs("NetworkReaderID", STRING(NetworkReader.NetworkReaderID), NetworkReader.NetworkReaderName).
   END.
   
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel("Action Point").
   NetworkErrorDetailsForm:insertComboField("NetworkActionPointID", "", 200, TRUE).  
    FOR EACH NetworkActionPoint NO-LOCK:
         
      NetworkErrorDetailsForm:insertComboPairs("NetworkActionPointID", STRING(NetworkActionPoint.NetworkActionPointID), NetworkActionPoint.NetworkActionPointName).
   END.
   
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel("Error Type").
   NetworkErrorDetailsForm:insertComboField("NetworkErrorTypeID", "", 200, TRUE).
    FOR EACH NetworkErrorType NO-LOCK
      BY NetworkErrorType.NetworkErrorTypeName:
         
      NetworkErrorDetailsForm:insertComboPairs("NetworkErrorTypeID", STRING(NetworkErrorType.NetworkErrorTypeID), NetworkErrorType.NetworkErrorTypeName).
   END.
   
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel("Error Message").
   NetworkErrorDetailsForm:insertTextField("ErrorMessage", "", 200, TRUE).
   
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel(fTL("Raised")).
   NetworkErrorDetailsForm:insertDateField("RaisedDate", "", 110, TRUE).
   /* Time fields have no label */
   NetworkErrorDetailsForm:insertTextField("RaisedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorDetailsForm:insertLabel(":").
   NetworkErrorDetailsForm:insertTextField("RaisedMins", "00", 18, TRUE).
   
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel(fTL("Notification Sent")).
   NetworkErrorDetailsForm:insertDateField("NotificationSentDate", "", 110, TRUE).
   /* Time fields have no label */
   NetworkErrorDetailsForm:insertTextField("NotificationSentHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorDetailsForm:insertLabel(":").
   NetworkErrorDetailsForm:insertTextField("NotificationSentMins", "00", 18, TRUE).
   
   NetworkErrorDetailsForm:startRow().
   NetworkErrorDetailsForm:insertLabel(fTL("Resolved")).
   NetworkErrorDetailsForm:insertDateField("ResolvedDate", "", 110, TRUE).
   /* Time fields have no label */
   NetworkErrorDetailsForm:insertTextField("ResolvedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorDetailsForm:insertLabel(":").
   NetworkErrorDetailsForm:insertTextField("ResolvedMins", "00", 18, TRUE).
   
   {webGetOptionalFormFields.i pNetworkErrorDetailsFields}
   
   /* Add Hidden Fields*/
   NetworkErrorDetailsForm:insertHiddenField("networkerror_browse_scroll", "").
   NetworkErrorDetailsForm:insertHiddenField("form_name", "networkerror_details_form").
   NetworkErrorDetailsForm:insertHiddenField("prog_name", "adNetworkError.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorDetailsForm}
   
   /* Create Button Bar */
   NetworkErrorDetailsButtons = NEW buttonBar().
   
/*   NetworkErrorDetailsButtons:addButton("networkerror_details_form_btn_save",                     */
/*                                              fTL("Save"),                                        */
/*                                              "updateNetworkError('networkerror_details_form');").*/
   
   NetworkErrorDetailsButtons:addButton("networkerror_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('networkerror_details_form_popup');").
   
   NetworkErrorDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorDetailsForm:FormButtons = NetworkErrorDetailsButtons.
   
   NetworkErrorDetailsForm:endForm(). 
   
   NetworkErrorDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkErrorDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorDetailsFields Procedure 
PROCEDURE pNetworkErrorDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         NetworkErrorDetailsForm:startRow().
         NetworkErrorDetailsForm:insertLabel(fTL("Field Label")).
         NetworkErrorDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adNetworkError_networkerror_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeHistory Procedure
PROCEDURE pNetworkErrorHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "networkerrorhistory_details_form"}
   
   FIND FIRST NetworkError WHERE NetworkError.NetworkErrorID = intSelectedNetworkError NO-LOCK NO-ERROR.
   
   NetworkErrorHistoryBrowseForm = NEW dataForm("networkerrorhistory_browse_form").
   NetworkErrorHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkErrorHistoryBrowseForm:FormWidth   = 850.
   NetworkErrorHistoryBrowseForm:FormHeight  = 540.
   NetworkErrorHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE NetworkError THEN " for NetworkErrorID: " 
                                                                  + STRING(NetworkError.NetworkErrorID) ELSE "").
   NetworkErrorHistoryBrowseForm:FormType    = "xxl_large".
   
   NetworkErrorHistoryBrowse = NEW browseTable("networkerrorhistory_browse").
   NetworkErrorHistoryBrowse:BrowseWidth  = 830.
   NetworkErrorHistoryBrowse:BrowseHeight = 500.
   NetworkErrorHistoryBrowse:ExcelExport  = TRUE.
   NetworkErrorHistoryBrowse:SessionID    = intGblSessionID.
   
   
   NetworkErrorHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkErrorHistory}

   NetworkErrorHistoryBrowse:insertColumn(fTL("Network Reader"),    100, "INTEGER",   "left", FALSE).  
   NetworkErrorHistoryBrowse:insertColumn(fTL("Network Action"),    100, "INTEGER",   "left", FALSE).   
   NetworkErrorHistoryBrowse:insertColumn(fTL("Error Type"),        100, "INTEGER",   "left", FALSE).    
   NetworkErrorHistoryBrowse:insertColumn(fTL("Error Message"),     100, "CHARACTER", "left", FALSE).
   NetworkErrorHistoryBrowse:insertColumn(fTL("User"),              100, "CHARACTER", "left", FALSE).
   NetworkErrorHistoryBrowse:insertColumn(fTL("Created"),           100, "CHARACTER", "left", FALSE).
   
   NetworkErrorHistoryBrowse:StartBody().
   
   IF AVAILABLE NetworkError THEN
   DO:
      /*List the NetworkErrorHistory*/
      FOR EACH NetworkErrorHistory NO-LOCK 
         WHERE  NetworkErrorHistory.NetworkErrorID = intSelectedNetworkError
         BY NetworkErrorHistory.NetworkErrorHistoryID:
         
         FIND FIRST GateUser OF NetworkErrorHistory NO-LOCK NO-ERROR.
         FIND FIRST NetworkReader OF NetworkErrorHistory NO-LOCK NO-ERROR.
         FIND FIRST NetworkActionPoint OF NetworkErrorHistory NO-LOCK NO-ERROR.
         FIND FIRST NetworkErrorType OF NetworkErrorHistory NO-LOCK NO-ERROR.  
       
         NetworkErrorHistoryBrowse:startRow(NetworkErrorHistory.NetworkErrorHistoryID, "selectHistoryRow(this," + '"' + STRING(NetworkErrorHistory.NetworkErrorHistoryID) 
                                                                     + '","networkErrorHistory"' + ");", "").
         NetworkErrorHistoryBrowse:insertData(NetworkErrorHistory.NetworkErrorHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i NetworkErrorHistory}
         
         NetworkErrorHistoryBrowse:insertData((IF AVAILABLE NetworkReader THEN NetworkReader.NetworkReaderName ELSE ""), "left").
         NetworkErrorHistoryBrowse:insertData((IF AVAILABLE NetworkActionPoint THEN NetworkActionPoint.NetworkActionPointName ELSE ""), "left").
         NetworkErrorHistoryBrowse:insertData((IF AVAILABLE NetworkErrorType THEN NetworkErrorType.NetworkErrorTypeName ELSE ""), "left").
         NetworkErrorHistoryBrowse:insertData(NetworkErrorHistory.ErrorMessage, "left").
         NetworkErrorHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         NetworkErrorHistoryBrowse:insertData(fDisplayDate&Time(NetworkErrorHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */         
         NetworkErrorHistoryBrowse:insertHiddendata("NetworkErrorHistoryID",NetworkErrorHistory.NetworkErrorHistoryID).
         
         NetworkErrorHistoryBrowse:endRow().
      
      END. /* FOR EACH NetworkErrorHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   NetworkErrorHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkErrorHistoryBrowse:getErrors().
   
   NetworkErrorHistoryBrowseForm:insertHiddenField("NetworkErrorHistoryID","").
   NetworkErrorHistoryBrowseForm:insertHiddenField("popup_networkerrorhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorHistoryBrowseForm}
   
   /* Create Button Bar */
   NetworkErrorHistoryBrowseButtons = NEW buttonBar().                                                 
   
   NetworkErrorHistoryBrowseButtons:addButton("networkerrorhistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewNetworkErrorHistoryDetails('networkerrorhistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   NetworkErrorHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   NetworkErrorHistoryBrowseButtons:addButton("networkerrorhistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('networkerrorhistory_browse_form_popup');").
   
   NetworkErrorHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorHistoryBrowseForm:FormBrowse  = NetworkErrorHistoryBrowse.
   NetworkErrorHistoryBrowseForm:FormButtons = NetworkErrorHistoryBrowseButtons.
   NetworkErrorHistoryBrowseForm:endForm(). 
   
   NetworkErrorHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorHistoryDetails Procedure
PROCEDURE pNetworkErrorHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "networkerrorhistory_details_form"}
   
   chrDisplayFieldList  = "NetworkErrorHistoryID,NetworkReaderID,NetworkActionPointID,NetworkErrorTypeID,ErrorMessage"
                        + ",CreatedDate,CreatedHour,CreatedMins,GateUserID,RaisedDate,RaisedHour,RaisedMins"
                        + ",NotificationSentDate,NotificationSentHour,NotificationSentMins,ResolvedDate,ResolvedHour"
                        + ",ResolvedMins,NetworkErrorID".
                             
   
   NetworkErrorHistoryDetailsForm = NEW dataForm("networkerrorhistory_details_form").
   NetworkErrorHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   NetworkErrorHistoryDetailsForm:FormWidth   = 545.
   NetworkErrorHistoryDetailsForm:FormHeight  = 440.
   NetworkErrorHistoryDetailsForm:FormTitle   = "Network Error History Details".
   NetworkErrorHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkErrorHistoryDetailsForm:insertPaddingColumn(40).
   NetworkErrorHistoryDetailsForm:insertColumn(110).
   NetworkErrorHistoryDetailsForm:insertColumn(120).
   NetworkErrorHistoryDetailsForm:insertColumn(20).
   NetworkErrorHistoryDetailsForm:insertColumn(4).
   NetworkErrorHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("History ID")).
   NetworkErrorHistoryDetailsForm:insertTextField("NetworkErrorHistoryID", "", 200, TRUE).    
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Error ID")).
   NetworkErrorHistoryDetailsForm:insertTextField("NetworkErrorID", "", 200, TRUE).
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Reader")).
   NetworkErrorHistoryDetailsForm:insertComboField("NetworkReaderID", "", 200, TRUE).
   FOR EACH NetworkReader NO-LOCK
      BY NetworkReader.NetworkReaderName:
      NetworkErrorHistoryDetailsForm:insertComboPairs("NetworkReaderID", STRING(NetworkReader.NetworkReaderID), NetworkReader.NetworkReaderName).
   END.
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Action Point")).
   NetworkErrorHistoryDetailsForm:insertComboField("NetworkActionPointID", "", 200, TRUE).
   FOR EACH NetworkActionPoint NO-LOCK
      BY NetworkActionPoint.NetworkActionPointName:
      NetworkErrorHistoryDetailsForm:insertComboPairs("NetworkActionPointID", STRING(NetworkActionPoint.NetworkActionPointID), NetworkActionPoint.NetworkActionPointName).
   END.
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Error Type")).
   NetworkErrorHistoryDetailsForm:insertComboField("NetworkErrorTypeID", "", 200, TRUE).
   FOR EACH NetworkErrorType NO-LOCK
      BY NetworkErrorType.NetworkErrorTypeName:
      NetworkErrorHistoryDetailsForm:insertComboPairs("NetworkErrorTypeID", STRING(NetworkErrorType.NetworkErrorTypeID), NetworkErrorType.NetworkErrorTypeName).
   END.
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Error Message")).
   NetworkErrorHistoryDetailsForm:insertTextField("ErrorMessage", "", 200, TRUE).
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel("User").
   NetworkErrorHistoryDetailsForm:insertComboField("GateUserID", "", 110, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      NetworkErrorHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Raised")).
   NetworkErrorHistoryDetailsForm:insertDateField("RaisedDate", "", 110, TRUE).
   /* Time fields have no label */
   NetworkErrorHistoryDetailsForm:insertTextField("RaisedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorHistoryDetailsForm:insertLabel(":").
   NetworkErrorHistoryDetailsForm:insertTextField("RaisedMins", "00", 18, TRUE).
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Notification Sent")).
   NetworkErrorHistoryDetailsForm:insertDateField("NotificationSentDate", "", 110, TRUE).
   /* Time fields have no label */
   NetworkErrorHistoryDetailsForm:insertTextField("NotificationSentHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorHistoryDetailsForm:insertLabel(":").
   NetworkErrorHistoryDetailsForm:insertTextField("NotificationSentMins", "00", 18, TRUE).
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel(fTL("Resolved")).
   NetworkErrorHistoryDetailsForm:insertDateField("ResolvedDate", "", 110, TRUE).
   /* Time fields have no label */
   NetworkErrorHistoryDetailsForm:insertTextField("ResolvedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorHistoryDetailsForm:insertLabel(":").
   NetworkErrorHistoryDetailsForm:insertTextField("ResolvedMins", "00", 18, TRUE).
   
   NetworkErrorHistoryDetailsForm:startRow().
   NetworkErrorHistoryDetailsForm:insertLabel("Created").
   NetworkErrorHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   NetworkErrorHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorHistoryDetailsForm:insertLabel(":").
   NetworkErrorHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   NetworkErrorHistoryDetailsForm:insertHiddenField("networkerror_browse_scroll","").
   NetworkErrorHistoryDetailsForm:insertHiddenField("popup_networkerrorhistory_browse", "").
   NetworkErrorHistoryDetailsForm:insertHiddenField("NetworkErrorHistoryID","").
   NetworkErrorHistoryDetailsForm:insertHiddenField("form_name","networkerrorhistory_details_form").
   NetworkErrorHistoryDetailsForm:insertHiddenField("prog_name","adNetworkError.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorHistoryDetailsForm}
   
   /* Create Button Bar */
   NetworkErrorHistoryDetailsButtons = NEW buttonBar().
   
   NetworkErrorHistoryDetailsButtons:addButton("networkerrorhistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('networkerrorhistory_details_form_popup');").
                                        
   NetworkErrorHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorHistoryDetailsForm:FormButtons = NetworkErrorHistoryDetailsButtons.
   
   NetworkErrorHistoryDetailsForm:endForm(). 
   NetworkErrorHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

