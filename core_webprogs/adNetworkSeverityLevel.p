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

  Created: 22/10/2015

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

DEFINE VARIABLE intSelectedNetworkErrorSeverityLevel        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkErrorSeverityLevelRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkErrorSeverityLevelRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkErrorSeverityLevelID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupNetworkErrorSeverityLevelHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE NetworkErrorSeverityLevelBrowseFrame        AS pageFrame.
DEFINE VARIABLE NetworkErrorSeverityLevelBrowse             AS browseTable.
DEFINE VARIABLE NetworkErrorSeverityLevelBrowseButtons      AS buttonBar.
DEFINE VARIABLE NetworkErrorSeverityLevelDetailsForm        AS dataForm.
DEFINE VARIABLE NetworkErrorSeverityLevelDetailsButtons     AS buttonBar.

DEFINE VARIABLE NetworkErrorSeverityLevelHistBrowseForm     AS dataForm.   
DEFINE VARIABLE NetworkErrorSeverityLevelHistBrowse         AS browseTable.
DEFINE VARIABLE NetworkErrorSeverityLevelHistBrowseButtons  AS buttonBar.

DEFINE VARIABLE NetworkErrorSeverityLevelHistDetailsForm    AS dataForm.
DEFINE VARIABLE NetworkErrorSeverityLevelHistDetailsButtons AS buttonBar.



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
   
   ASSIGN chrNetworkErrorSeverityLevelID          = get-value("NetworkErrorSeverityLevelID")
          intSelectedNetworkErrorSeverityLevel    = INTEGER(chrNetworkErrorSeverityLevelID)
          chrScrollToNetworkErrorSeverityLevelRow = STRING(INTEGER(get-value("networkerrorseveritylevel_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrNetworkErrorSeverityLevelID <> "" THEN
     chrSelectNetworkErrorSeverityLevelRow = 'selectNetworkErrorSeverityLevelRow(document.getElementById("networkerrorseveritylevel_browse_row_' + chrNetworkErrorSeverityLevelID + '"),"' 
                                                         + chrNetworkErrorSeverityLevelID +  '");'.
   
   IF get-value('popup_networkerrorseveritylevelhist_browse') = "yes" THEN
      chrPopupNetworkErrorSeverityLevelHistory  = 'enablePopup("networkerrorseveritylevelhist_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkerrorseveritylevel_browse").scrollTop=' + chrScrollToNetworkErrorSeverityLevelRow 
                                                          + chrSelectNetworkErrorSeverityLevelRow + chrPopupNetworkErrorSeverityLevelHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "NetworkErrorSeverityLevel Admin".
   ThisPage:FrameTitle    = "NetworkErrorSeverityLevel Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("networkerrorseveritylevel.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkErrorSeverityLevelBrowse.
   
   FIND FIRST NetworkErrorSeverityLevel NO-LOCK
      WHERE NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID = intSelectedNetworkErrorSeverityLevel NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pNetworkErrorSeverityLevelDetails.
   RUN pNetworkErrorSeverityLevelHistory.
   RUN pNetworkErrorSeverityLevelHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT NetworkErrorSeverityLevelBrowseFrame           NO-ERROR.
   DELETE OBJECT NetworkErrorSeverityLevelBrowse                NO-ERROR.
   DELETE OBJECT NetworkErrorSeverityLevelBrowseButtons         NO-ERROR.
   DELETE OBJECT NetworkErrorSeverityLevelDetailsForm           NO-ERROR.
   DELETE OBJECT NetworkErrorSeverityLevelDetailsButtons        NO-ERROR.
   
   DELETE OBJECT NetworkErrorSeverityLevelHistBrowseForm        NO-ERROR.   
   DELETE OBJECT NetworkErrorSeverityLevelHistBrowse            NO-ERROR.
   DELETE OBJECT NetworkErrorSeverityLevelHistBrowseButtons     NO-ERROR.
   
   DELETE OBJECT NetworkErrorSeverityLevelHistDetailsForm    NO-ERROR.
   DELETE OBJECT NetworkErrorSeverityLevelHistDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorSeverityLevelBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorSeverityLevelBrowse Procedure 
PROCEDURE pNetworkErrorSeverityLevelBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkerrorseveritylevel_details_form"}
   
   NetworkErrorSeverityLevelBrowse = NEW browseTable("networkerrorseveritylevel_browse").
   NetworkErrorSeverityLevelBrowse:BrowseWidth  = 965.
   NetworkErrorSeverityLevelBrowse:BrowseHeight = 455.
   NetworkErrorSeverityLevelBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkErrorSeverityLevelBrowse:insertColumn(fTL("Level ID"), 80, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkErrorSeverityLevel}
   
   NetworkErrorSeverityLevelBrowse:insertColumn(fTL("Level Code"),        200, "CHARACTER", "left", FALSE).
   NetworkErrorSeverityLevelBrowse:insertColumn(fTL("Level Name"),        200, "CHARACTER", "left", FALSE).
   NetworkErrorSeverityLevelBrowse:insertColumn(fTL("Email Group"), 200, "INTEGER", "left", FALSE).
   
   /*Body*/
   NetworkErrorSeverityLevelBrowse:startBody().
   
   FOR EACH NetworkErrorSeverityLevel NO-LOCK: /*idx=NetworkErrorSeverityLevelID*/
   
      FIND FIRST EmailGroup OF NetworkErrorSeverityLevel NO-LOCK NO-ERROR. /*idx=NetworkErrorSeverityLevelID*/
      
      NetworkErrorSeverityLevelBrowse:startRow(NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID, "selectNetworkErrorSeverityLevelRow(this," + '"' + STRING(NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID) 
                                                                  + '"' + ");", "").
      NetworkErrorSeverityLevelBrowse:insertData(NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkErrorSeverityLevel}
      
      
      NetworkErrorSeverityLevelBrowse:insertData(NetworkErrorSeverityLevel.SeverityLevelCode, "left").
      NetworkErrorSeverityLevelBrowse:insertData(NetworkErrorSeverityLevel.SeverityLevelName, "left").
      NetworkErrorSeverityLevelBrowse:insertData((IF AVAILABLE EmailGroup THEN EmailGroup.GroupName ELSE ""), "left").
      /* Add hidden fields */
      NetworkErrorSeverityLevelBrowse:insertHiddenData("NetworkErrorSeverityLevelVersionID",NetworkErrorSeverityLevel.VersionID).
      
      NetworkErrorSeverityLevelBrowse:endRow().
      
   END. /*FOR EACH NetworkErrorSeverityLevel NO-LOCK */
   
   NetworkErrorSeverityLevelBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkErrorSeverityLevelBrowse:getErrors().
   
   /* Create a new frame */
   NetworkErrorSeverityLevelBrowseFrame = NEW pageFrame().
   NetworkErrorSeverityLevelBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkErrorSeverityLevelBrowseFrame:FormAction="dbNetworkErrorSeverityLevelUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   NetworkErrorSeverityLevelBrowseFrame:formOpen("networkerrorseveritylevel_browse_form").
   
   /* Start the Frame Header */
   NetworkErrorSeverityLevelBrowseFrame:insertSpacer(5).
   NetworkErrorSeverityLevelBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkErrorSeverityLevelBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkErrorSeverityLevelBrowseFrame:frameClose().
   NetworkErrorSeverityLevelBrowseFrame:insertSpacer(10).
   
   NetworkErrorSeverityLevelBrowseFrame:insertHiddenField("networkerrorseveritylevel_browse_scroll","").
   NetworkErrorSeverityLevelBrowseFrame:insertHiddenField("NetworkErrorSeverityLevelID","").
   NetworkErrorSeverityLevelBrowseFrame:insertHiddenField("NetworkErrorSeverityLevelVersionID","").
   NetworkErrorSeverityLevelBrowseFrame:insertHiddenField("popup_networkerrorseveritylevelhist_browse","").
   NetworkErrorSeverityLevelBrowseFrame:insertHiddenField("form_name","networkerrorseveritylevel_browse_form").
   NetworkErrorSeverityLevelBrowseFrame:insertHiddenField("prog_name","adNetworkSeverityLevel.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorSeverityLevelBrowseFrame}
   
   NetworkErrorSeverityLevelBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkErrorSeverityLevelBrowseButtons = NEW buttonBar().
   NetworkErrorSeverityLevelBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   NetworkErrorSeverityLevelBrowseButtons:addButton("networkerrorseveritylevel_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewNetworkErrorSeverityLevelDetails('networkerrorseveritylevel_details_form');",
                                             "Disabled").
   
   NetworkErrorSeverityLevelBrowseButtons:addButton("networkerrorseveritylevel_browse_form_btn_create",
                                             fTL("Create"),
                                             "createNetworkErrorSeverityLevel('networkerrorseveritylevel_details_form');",
                                             "").
                                             
   NetworkErrorSeverityLevelBrowseButtons:addButton("networkerrorseveritylevel_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   NetworkErrorSeverityLevelBrowseButtons:addButton("networkerrorseveritylevel_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteNetworkErrorSeverityLevel();",
                                             (IF intSelectedNetworkErrorSeverityLevel > 0 THEN "" ELSE "Disabled")).
   */
   
   NetworkErrorSeverityLevelBrowseButtons:closeBar().  
   NetworkErrorSeverityLevelBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorSeverityLevelDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorSeverityLevelDetails Procedure 
PROCEDURE pNetworkErrorSeverityLevelDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkerrorseveritylevel_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkErrorSeverityLevelID,EmailGroupID,SeverityLevelCode,SeverityLevelName" 
          chrEditFieldList     = "EmailGroupID,SeverityLevelName" 
          chrNewFieldList      = "SeverityLevelCode,SeverityLevelName,EmailGroupID" 
          chrRequiredFieldList = "SeverityLevelCode,SeverityLevelName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkErrorSeverityLevelDetailsForm = NEW dataForm("networkerrorseveritylevel_details_form").
   NetworkErrorSeverityLevelDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkErrorSeverityLevelDetailsForm:FormAction = "dbNetworkErrorSeverityLevelUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkErrorSeverityLevelDetailsForm:FormWidth   = 580.
   NetworkErrorSeverityLevelDetailsForm:FormHeight  = 420.
   NetworkErrorSeverityLevelDetailsForm:FormTitle   = "NetworkErrorSeverityLevel Details".
   NetworkErrorSeverityLevelDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkErrorSeverityLevelDetailsForm:insertPaddingColumn(30).
   NetworkErrorSeverityLevelDetailsForm:insertColumn(150).
   NetworkErrorSeverityLevelDetailsForm:insertColumn(160).
   NetworkErrorSeverityLevelDetailsForm:insertColumn(20).
   NetworkErrorSeverityLevelDetailsForm:insertColumn(4).
   NetworkErrorSeverityLevelDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkErrorSeverityLevelDetailsForm:startRow().
   NetworkErrorSeverityLevelDetailsForm:insertLabel("Level ID").
   NetworkErrorSeverityLevelDetailsForm:insertTextField("NetworkErrorSeverityLevelID", "", 200, TRUE).  
   
   NetworkErrorSeverityLevelDetailsForm:startRow().
   NetworkErrorSeverityLevelDetailsForm:insertLabel("Level Code").
   NetworkErrorSeverityLevelDetailsForm:insertTextField("SeverityLevelCode", "", 200, TRUE).  
   
   NetworkErrorSeverityLevelDetailsForm:startRow().
   NetworkErrorSeverityLevelDetailsForm:insertLabel("Level Name").
   NetworkErrorSeverityLevelDetailsForm:insertTextField("SeverityLevelName", "", 200, TRUE).
   
   NetworkErrorSeverityLevelDetailsForm:startRow().
   NetworkErrorSeverityLevelDetailsForm:insertLabel("Email Group").
   NetworkErrorSeverityLevelDetailsForm:insertComboField("EmailGroupID", "", 200, TRUE).  
   NetworkErrorSeverityLevelDetailsForm:insertComboPairs("EmailGroupID", "0", "None Selected...").
   FOR EACH EmailGroup NO-LOCK /*idx=EmailGrouplID*/
      WHERE EmailGroup.Active:
      
      NetworkErrorSeverityLevelDetailsForm:insertComboPairs("EmailGroupID", STRING(EmailGroup.EmailGroupID), EmailGroup.GroupName).
   END.
   
   {webGetOptionalFormFields.i pNetworkErrorSeverityLevelDetailsFields}
   
   /* Add Hidden Fields*/
   NetworkErrorSeverityLevelDetailsForm:insertHiddenField("networkerrorseveritylevel_browse_scroll", "").
   NetworkErrorSeverityLevelDetailsForm:insertHiddenField("form_name", "networkerrorseveritylevel_details_form").
   NetworkErrorSeverityLevelDetailsForm:insertHiddenField("prog_name", "adNetworkSeverityLevel.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorSeverityLevelDetailsForm}
   
   /* Create Button Bar */
   NetworkErrorSeverityLevelDetailsButtons = NEW buttonBar().
   
   NetworkErrorSeverityLevelDetailsButtons:addButton("networkerrorseveritylevel_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateNetworkErrorSeverityLevel('networkerrorseveritylevel_details_form');").
   
   NetworkErrorSeverityLevelDetailsButtons:addButton("networkerrorseveritylevel_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('networkerrorseveritylevel_details_form_popup');").
   
   NetworkErrorSeverityLevelDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorSeverityLevelDetailsForm:FormButtons = NetworkErrorSeverityLevelDetailsButtons.
   
   NetworkErrorSeverityLevelDetailsForm:endForm(). 
   
   NetworkErrorSeverityLevelDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkErrorSeverityLevelDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorSeverityLevelDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorSeverityLevelDetailsFields Procedure 
PROCEDURE pNetworkErrorSeverityLevelDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         NetworkErrorSeverityLevelDetailsForm:startRow().
         NetworkErrorSeverityLevelDetailsForm:insertLabel(fTL("Field Label")).
         NetworkErrorSeverityLevelDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adNetworkSeverityLevel_networkerrorseveritylevel_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorSeverityLevelHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorSeverityLevelHistory Procedure
PROCEDURE pNetworkErrorSeverityLevelHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "networkerrorseveritylevelhist_details_form"}
   
   FIND FIRST NetworkErrorSeverityLevel NO-LOCK 
      WHERE NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID = intSelectedNetworkErrorSeverityLevel NO-ERROR.
   
   NetworkErrorSeverityLevelHistBrowseForm = NEW dataForm("networkerrorseveritylevelhist_browse_form").
   NetworkErrorSeverityLevelHistBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkErrorSeverityLevelHistBrowseForm:FormWidth   = 850.
   NetworkErrorSeverityLevelHistBrowseForm:FormHeight  = 540.
   NetworkErrorSeverityLevelHistBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE NetworkErrorSeverityLevel THEN " for Level ID: " 
                                                                  + STRING(NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID) ELSE "").
   NetworkErrorSeverityLevelHistBrowseForm:FormType    = "xxl_large".
   
   NetworkErrorSeverityLevelHistBrowse = NEW browseTable("networkerrorseveritylevelhist_browse").
   NetworkErrorSeverityLevelHistBrowse:BrowseWidth  = 830.
   NetworkErrorSeverityLevelHistBrowse:BrowseHeight = 500.
   NetworkErrorSeverityLevelHistBrowse:ExcelExport  = TRUE.
   NetworkErrorSeverityLevelHistBrowse:SessionID    = intGblSessionID.
   
   
   NetworkErrorSeverityLevelHistBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkErrorSeverityLevelHist}

   NetworkErrorSeverityLevelHistBrowse:insertColumn(fTL("Level Code"),        120, "CHARACTER", "left", FALSE).
   NetworkErrorSeverityLevelHistBrowse:insertColumn(fTL("Level Name"),        120, "CHARACTER", "left", FALSE).
   NetworkErrorSeverityLevelHistBrowse:insertColumn(fTL("Email Group"),       150, "INTEGER",   "left", FALSE).  
   NetworkErrorSeverityLevelHistBrowse:insertColumn(fTL("User"),              120, "CHARACTER", "left", FALSE).
   NetworkErrorSeverityLevelHistBrowse:insertColumn(fTL("Created"),           150, "CHARACTER", "left", FALSE).
   
   NetworkErrorSeverityLevelHistBrowse:StartBody().
   
      /*List the NetworkErrorSeverityLevelHistory*/
      FOR EACH NetworkErrorSeverityLevelHist NO-LOCK 
         WHERE  NetworkErrorSeverityLevelHist.NetworkErrorSeverityLevelID = intSelectedNetworkErrorSeverityLevel
         BY NetworkErrorSeverityLevelHist.Created DESC:
         
         FIND FIRST GateUser   OF NetworkErrorSeverityLevelHist NO-LOCK NO-ERROR.
         FIND FIRST EmailGroup OF NetworkErrorSeverityLevelHist NO-LOCK NO-ERROR.
       
         NetworkErrorSeverityLevelHistBrowse:startRow(NetworkErrorSeverityLevelHist.NetworkErrorSeverityLevelHistID, "selectHistoryRow(this," + '"'
                                         + STRING(NetworkErrorSeverityLevelHist.NetworkErrorSeverityLevelHistID)
                                         + '","networkErrorSeverityLevelHist"' + ");", "").
                                         
         NetworkErrorSeverityLevelHistBrowse:insertData(NetworkErrorSeverityLevelHist.NetworkErrorSeverityLevelHistID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i NetworkErrorSeverityLevelHist}
         
         NetworkErrorSeverityLevelHistBrowse:insertData(NetworkErrorSeverityLevelHist.SeverityLevelCode, "left").
         NetworkErrorSeverityLevelHistBrowse:insertData(NetworkErrorSeverityLevelHist.SeverityLevelName, "left").
         NetworkErrorSeverityLevelHistBrowse:insertData((IF AVAILABLE EmailGroup THEN EmailGroup.GroupName ELSE ""), "left").
         NetworkErrorSeverityLevelHistBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         NetworkErrorSeverityLevelHistBrowse:insertData(fDisplayDate&Time(NetworkErrorSeverityLevelHist.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         NetworkErrorSeverityLevelHistBrowse:insertHiddendata("NetworkErrorSeverityLevelHistID",NetworkErrorSeverityLevelHist.NetworkErrorSeverityLevelHistID).
         
         NetworkErrorSeverityLevelHistBrowse:endRow().
      
      END. /* FOR EACH NetworkErrorSeverityLevelHistory NO-LOCK, */
   
   NetworkErrorSeverityLevelHistBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkErrorSeverityLevelHistBrowse:getErrors().
   
   NetworkErrorSeverityLevelHistBrowseForm:insertHiddenField("NetworkErrorSeverityLevelHistID","").
   NetworkErrorSeverityLevelHistBrowseForm:insertHiddenField("popup_networkerrorseveritylevelhist_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorSeverityLevelHistBrowseForm}
   
   /* Create Button Bar */
   NetworkErrorSeverityLevelHistBrowseButtons = NEW buttonBar().                                                 
   
   NetworkErrorSeverityLevelHistBrowseButtons:addButton("networkerrorseveritylevelhist_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewNetworkErrorSeverityLevelHistoryDetails('networkerrorseveritylevelhist_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   NetworkErrorSeverityLevelHistBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   NetworkErrorSeverityLevelHistBrowseButtons:addButton("networkerrorseveritylevelhist_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('networkerrorseveritylevelhist_browse_form_popup');").
   
   NetworkErrorSeverityLevelHistBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorSeverityLevelHistBrowseForm:FormBrowse  = NetworkErrorSeverityLevelHistBrowse.
   NetworkErrorSeverityLevelHistBrowseForm:FormButtons = NetworkErrorSeverityLevelHistBrowseButtons.
   NetworkErrorSeverityLevelHistBrowseForm:endForm(). 
   
   NetworkErrorSeverityLevelHistBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorSeverityLevelHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorSeverityLevelHistoryDetails Procedure
PROCEDURE pNetworkErrorSeverityLevelHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "networkerrorseveritylevelhist_details_form"}
   
   chrDisplayFieldList  = "NetworkErrorSeverityLevelHistID,NetworkErrorSeverityLevelID,SeverityLevelCode,SeverityLevelName"
                        + ",EmailGroupID,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   NetworkErrorSeverityLevelHistDetailsForm = NEW dataForm("networkerrorseveritylevelhist_details_form").
   NetworkErrorSeverityLevelHistDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   NetworkErrorSeverityLevelHistDetailsForm:FormWidth   = 545.
   NetworkErrorSeverityLevelHistDetailsForm:FormHeight  = 440.
   NetworkErrorSeverityLevelHistDetailsForm:FormTitle   = "Network Error Severity Level History Details".
   NetworkErrorSeverityLevelHistDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkErrorSeverityLevelHistDetailsForm:insertPaddingColumn(40).
   NetworkErrorSeverityLevelHistDetailsForm:insertColumn(110).
   NetworkErrorSeverityLevelHistDetailsForm:insertColumn(120).
   NetworkErrorSeverityLevelHistDetailsForm:insertColumn(20).
   NetworkErrorSeverityLevelHistDetailsForm:insertColumn(4).
   NetworkErrorSeverityLevelHistDetailsForm:insertColumn(40).  
   
   /* Fields */
   NetworkErrorSeverityLevelHistDetailsForm:startRow().
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel(fTL("History ID")).
   NetworkErrorSeverityLevelHistDetailsForm:insertTextField("NetworkErrorSeverityLevelHistID", "", 200, TRUE).    
   
   NetworkErrorSeverityLevelHistDetailsForm:startRow().
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel(fTL("Level ID")).
   NetworkErrorSeverityLevelHistDetailsForm:insertTextField("NetworkErrorSeverityLevelID", "", 200, TRUE).
   NetworkErrorSeverityLevelHistDetailsForm:startRow().
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel(fTL("Level Code")).
   NetworkErrorSeverityLevelHistDetailsForm:insertTextField("SeverityLevelCode", "", 200, TRUE).
   
   NetworkErrorSeverityLevelHistDetailsForm:startRow().
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel(fTL("Level Name")).
   NetworkErrorSeverityLevelHistDetailsForm:insertTextField("SeverityLevelName", "", 200, TRUE).
   
   NetworkErrorSeverityLevelHistDetailsForm:startRow().
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel(fTL("EmailGroup")).
   NetworkErrorSeverityLevelHistDetailsForm:insertComboField("EmailGroupID", "", 200, TRUE).
   NetworkErrorSeverityLevelHistDetailsForm:insertComboPairs("EmailGroupID", "0", "None Selected...").
   FOR EACH EmailGroup NO-LOCK /*idx=EmailGrouplID*/
      WHERE EmailGroup.Active:
      
      NetworkErrorSeverityLevelHistDetailsForm:insertComboPairs("EmailGroupID", STRING(EmailGroup.EmailGroupID), EmailGroup.GroupName).
   END.
   
   NetworkErrorSeverityLevelHistDetailsForm:startRow().
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel("User").
   NetworkErrorSeverityLevelHistDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      NetworkErrorSeverityLevelHistDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   NetworkErrorSeverityLevelHistDetailsForm:startRow().
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel("Created").
   NetworkErrorSeverityLevelHistDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   NetworkErrorSeverityLevelHistDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorSeverityLevelHistDetailsForm:insertLabel(":").
   NetworkErrorSeverityLevelHistDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   NetworkErrorSeverityLevelHistDetailsForm:insertHiddenField("networkerrorseveritylevel_browse_scroll","").
   NetworkErrorSeverityLevelHistDetailsForm:insertHiddenField("popup_networkerrorseveritylevelhist_browse", "").
   NetworkErrorSeverityLevelHistDetailsForm:insertHiddenField("NetworkErrorSeverityLevelHistID","").
   NetworkErrorSeverityLevelHistDetailsForm:insertHiddenField("form_name","networkerrorseveritylevelhist_details_form").
   NetworkErrorSeverityLevelHistDetailsForm:insertHiddenField("prog_name","adNetworkSeverityLevel.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorSeverityLevelHistDetailsForm}
   
   /* Create Button Bar */
   NetworkErrorSeverityLevelHistDetailsButtons = NEW buttonBar().
   
   NetworkErrorSeverityLevelHistDetailsButtons:addButton("networkerrorseveritylevelhist_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('networkerrorseveritylevelhist_details_form_popup');").
                                        
   NetworkErrorSeverityLevelHistDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorSeverityLevelHistDetailsForm:FormButtons = NetworkErrorSeverityLevelHistDetailsButtons.
   
   NetworkErrorSeverityLevelHistDetailsForm:endForm(). 
   NetworkErrorSeverityLevelHistDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

