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

DEFINE VARIABLE intSelectedNetworkErrorType        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkErrorTypeRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkErrorTypeRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkErrorTypeID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupNetworkErrorTypeHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE NetworkErrorTypeBrowseFrame        AS pageFrame.
DEFINE VARIABLE NetworkErrorTypeBrowse             AS browseTable.
DEFINE VARIABLE NetworkErrorTypeBrowseButtons      AS buttonBar.
DEFINE VARIABLE NetworkErrorTypeDetailsForm        AS dataForm.
DEFINE VARIABLE NetworkErrorTypeDetailsButtons     AS buttonBar.

DEFINE VARIABLE NetworkErrorTypeHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE NetworkErrorTypeHistoryBrowse         AS browseTable.
DEFINE VARIABLE NetworkErrorTypeHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE NetworkErrorTypeHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE NetworkErrorTypeHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrNetworkErrorTypeID          = get-value("NetworkErrorTypeID")
          intSelectedNetworkErrorType    = INTEGER(chrNetworkErrorTypeID)
          chrScrollToNetworkErrorTypeRow = STRING(INTEGER(get-value("networkerrortype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrNetworkErrorTypeID <> "" THEN
     chrSelectNetworkErrorTypeRow = 'selectNetworkErrorTypeRow(document.getElementById("networkerrortype_browse_row_' + chrNetworkErrorTypeID + '"),"' 
                                                         + chrNetworkErrorTypeID +  '");'.
   
   IF get-value('popup_networkerrortypehistory_browse') = "yes" THEN
      chrPopupNetworkErrorTypeHistory  = 'enablePopup("networkerrortypehistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkerrortype_browse").scrollTop=' + chrScrollToNetworkErrorTypeRow 
                                                          + chrSelectNetworkErrorTypeRow + chrPopupNetworkErrorTypeHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "NetworkErrorType Admin".
   ThisPage:FrameTitle    = "NetworkErrorType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("networkerrortype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkErrorTypeBrowse.
   
   FIND FIRST NetworkErrorType NO-LOCK
      WHERE NetworkErrorType.NetworkErrorTypeID = intSelectedNetworkErrorType NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pNetworkErrorTypeDetails.
   RUN pNetworkErrorTypeHistory.
   RUN pNetworkErrorTypeHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT NetworkErrorTypeBrowseFrame           NO-ERROR.
   DELETE OBJECT NetworkErrorTypeBrowse                NO-ERROR.
   DELETE OBJECT NetworkErrorTypeBrowseButtons         NO-ERROR.
   DELETE OBJECT NetworkErrorTypeDetailsForm           NO-ERROR.
   DELETE OBJECT NetworkErrorTypeDetailsButtons        NO-ERROR.
   
   DELETE OBJECT NetworkErrorTypeHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT NetworkErrorTypeHistoryBrowse            NO-ERROR.
   DELETE OBJECT NetworkErrorTypeHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT NetworkErrorTypeHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT NetworkErrorTypeHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeBrowse Procedure 
PROCEDURE pNetworkErrorTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkerrortype_details_form"}
   
   NetworkErrorTypeBrowse = NEW browseTable("networkerrortype_browse").
   NetworkErrorTypeBrowse:BrowseWidth  = 965.
   NetworkErrorTypeBrowse:BrowseHeight = 455.
   NetworkErrorTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkErrorTypeBrowse:insertColumn(fTL("TypeID"), 80, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkErrorType}
   
   NetworkErrorTypeBrowse:insertColumn(fTL("Type Code"),       200, "CHARACTER", "left", FALSE).
   NetworkErrorTypeBrowse:insertColumn(fTL("Type Name"),       200, "CHARACTER", "left", FALSE).
   NetworkErrorTypeBrowse:insertColumn(fTL("Level"),           200, "CHARACTER", "left", FALSE).
   
   /*Body*/
   NetworkErrorTypeBrowse:startBody().
   
   FOR EACH NetworkErrorType NO-LOCK
      BY NetworkErrorType.NetworkErrorTypeID : /*idx=NetworkErrorTypeID*/
   
      FIND FIRST NetworkErrorSeverityLevel OF NetworkErrorType NO-LOCK NO-ERROR.
      
      NetworkErrorTypeBrowse:startRow(NetworkErrorType.NetworkErrorTypeID, "selectNetworkErrorTypeRow(this," + '"' + STRING(NetworkErrorType.NetworkErrorTypeID) 
                                                                  + '"' + ");", "").
      NetworkErrorTypeBrowse:insertData(NetworkErrorType.NetworkErrorTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkErrorType}
      
      NetworkErrorTypeBrowse:insertData(NetworkErrorType.NetworkErrorTypeCode, "left").
      NetworkErrorTypeBrowse:insertData(NetworkErrorType.NetworkErrorTypeName, "left").
      NetworkErrorTypeBrowse:insertData((IF AVAILABLE NetworkErrorSeverityLevel THEN NetworkErrorSeverityLevel.SeverityLevelName ELSE ""), "left").
      
      /* Add hidden fields */
      NetworkErrorTypeBrowse:insertHiddenData("NetworkErrorTypeVersionID",NetworkErrorType.VersionID).
      
      NetworkErrorTypeBrowse:endRow().
      
   END. /*FOR EACH NetworkErrorType NO-LOCK */
   
   NetworkErrorTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkErrorTypeBrowse:getErrors().
   
   /* Create a new frame */
   NetworkErrorTypeBrowseFrame = NEW pageFrame().
   NetworkErrorTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkErrorTypeBrowseFrame:FormAction="dbNetworkErrorTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   NetworkErrorTypeBrowseFrame:formOpen("networkerrortype_browse_form").
   
   /* Start the Frame Header */
   NetworkErrorTypeBrowseFrame:insertSpacer(5).
   NetworkErrorTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkErrorTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkErrorTypeBrowseFrame:frameClose().
   NetworkErrorTypeBrowseFrame:insertSpacer(10).
   
   NetworkErrorTypeBrowseFrame:insertHiddenField("networkerrortype_browse_scroll","").
   NetworkErrorTypeBrowseFrame:insertHiddenField("NetworkErrorTypeID","").
   NetworkErrorTypeBrowseFrame:insertHiddenField("NetworkErrorTypeVersionID","").
   NetworkErrorTypeBrowseFrame:insertHiddenField("popup_networkerrortypehistory_browse","").
   NetworkErrorTypeBrowseFrame:insertHiddenField("form_name","networkerrortype_browse_form").
   NetworkErrorTypeBrowseFrame:insertHiddenField("prog_name","adNetworkErrorType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorTypeBrowseFrame}
   
   NetworkErrorTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkErrorTypeBrowseButtons = NEW buttonBar().
   NetworkErrorTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   NetworkErrorTypeBrowseButtons:addButton("networkerrortype_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewNetworkErrorTypeDetails('networkerrortype_details_form');",
                                             "Disabled").
   
   NetworkErrorTypeBrowseButtons:addButton("networkerrortype_browse_form_btn_create",
                                             fTL("Create"),
                                             "createNetworkErrorType('networkerrortype_details_form');",
                                             "").
                                             
   NetworkErrorTypeBrowseButtons:addButton("networkerrortype_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   NetworkErrorTypeBrowseButtons:addButton("networkerrortype_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteNetworkErrorType();",
                                             (IF intSelectedNetworkErrorType > 0 THEN "" ELSE "Disabled")).
   */
   
   NetworkErrorTypeBrowseButtons:closeBar().  
   NetworkErrorTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorTypeDetails Procedure 
PROCEDURE pNetworkErrorTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkerrortype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkErrorTypeID,NetworkErrorSeverityLevelID,NetworkErrorTypeCode,NetworkErrorTypeName" 
          chrEditFieldList     = "NetworkErrorSeverityLevelID,NetworkErrorTypeName" 
          chrNewFieldList      = "NetworkErrorTypeCode,NetworkErrorSeverityLevelID,NetworkErrorTypeName" 
          chrRequiredFieldList = "NetworkErrorTypeCode,NetworkErrorTypeName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkErrorTypeDetailsForm = NEW dataForm("networkerrortype_details_form").
   NetworkErrorTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkErrorTypeDetailsForm:FormAction = "dbNetworkErrorTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkErrorTypeDetailsForm:FormWidth   = 580.
   NetworkErrorTypeDetailsForm:FormHeight  = 420.
   NetworkErrorTypeDetailsForm:FormTitle   = "NetworkErrorType Details".
   NetworkErrorTypeDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkErrorTypeDetailsForm:insertPaddingColumn(30).
   NetworkErrorTypeDetailsForm:insertColumn(150).
   NetworkErrorTypeDetailsForm:insertColumn(160).
   NetworkErrorTypeDetailsForm:insertColumn(20).
   NetworkErrorTypeDetailsForm:insertColumn(4).
   NetworkErrorTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkErrorTypeDetailsForm:startRow().
   NetworkErrorTypeDetailsForm:insertLabel("Type ID").
   NetworkErrorTypeDetailsForm:insertTextField("NetworkErrorTypeID", "", 200, TRUE).  
   
   NetworkErrorTypeDetailsForm:startRow().
   NetworkErrorTypeDetailsForm:insertLabel("Type Code").
   NetworkErrorTypeDetailsForm:insertTextField("NetworkErrorTypeCode", "", 200, TRUE).  
   
   NetworkErrorTypeDetailsForm:startRow().
   NetworkErrorTypeDetailsForm:insertLabel("Type Name").
   NetworkErrorTypeDetailsForm:insertTextField("NetworkErrorTypeName", "", 200, TRUE).
   
   NetworkErrorTypeDetailsForm:startRow().
   NetworkErrorTypeDetailsForm:insertLabel("Level").
   NetworkErrorTypeDetailsForm:insertComboField("NetworkErrorSeverityLevelID", "", 200, TRUE).
   FOR EACH NetworkErrorSeverityLevel NO-LOCK: /*idx=NetworkReaderTypeID*/
      
      NetworkErrorTypeDetailsForm:insertComboPairs("NetworkErrorSeverityLevelID", STRING(NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID), NetworkErrorSeverityLevel.SeverityLevelName). 
   END. 
   
   {webGetOptionalFormFields.i pNetworkErrorTypeDetailsFields}
   
   /* Add Hidden Fields*/
   NetworkErrorTypeDetailsForm:insertHiddenField("networkerrortype_browse_scroll", "").
   NetworkErrorTypeDetailsForm:insertHiddenField("form_name", "networkerrortype_details_form").
   NetworkErrorTypeDetailsForm:insertHiddenField("prog_name", "adNetworkErrorType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorTypeDetailsForm}
   
   /* Create Button Bar */
   NetworkErrorTypeDetailsButtons = NEW buttonBar().
   
   NetworkErrorTypeDetailsButtons:addButton("networkerrortype_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateNetworkErrorType('networkerrortype_details_form');").
   
   NetworkErrorTypeDetailsButtons:addButton("networkerrortype_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('networkerrortype_details_form_popup');").
   
   NetworkErrorTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorTypeDetailsForm:FormButtons = NetworkErrorTypeDetailsButtons.
   
   NetworkErrorTypeDetailsForm:endForm(). 
   
   NetworkErrorTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkErrorTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorTypeDetailsFields Procedure 
PROCEDURE pNetworkErrorTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         NetworkErrorTypeDetailsForm:startRow().
         NetworkErrorTypeDetailsForm:insertLabel(fTL("Field Label")).
         NetworkErrorTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adNetworkErrorType_networkerrortype_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorTypeHistory Procedure
PROCEDURE pNetworkErrorTypeHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "networkerrortypehistory_details_form"}
   
   FIND FIRST NetworkErrorType WHERE NetworkErrorType.NetworkErrorTypeID = intSelectedNetworkErrorType NO-LOCK NO-ERROR.
   
   NetworkErrorTypeHistoryBrowseForm = NEW dataForm("networkerrortypehistory_browse_form").
   NetworkErrorTypeHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkErrorTypeHistoryBrowseForm:FormWidth   = 850.
   NetworkErrorTypeHistoryBrowseForm:FormHeight  = 540.
   NetworkErrorTypeHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE NetworkErrorType THEN " for NetworkErrorType ID: " 
                                                                  + STRING(NetworkErrorType.NetworkErrorTypeID) ELSE "").
   NetworkErrorTypeHistoryBrowseForm:FormType    = "xxl_large".
   
   NetworkErrorTypeHistoryBrowse = NEW browseTable("networkerrortypehistory_browse").
   NetworkErrorTypeHistoryBrowse:BrowseWidth  = 830.
   NetworkErrorTypeHistoryBrowse:BrowseHeight = 500.
   NetworkErrorTypeHistoryBrowse:ExcelExport  = TRUE.
   NetworkErrorTypeHistoryBrowse:SessionID    = intGblSessionID.
   
   
   NetworkErrorTypeHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkErrorTypeHistory}

   NetworkErrorTypeHistoryBrowse:insertColumn(fTL("Type Code"),       120, "CHARACTER", "left", FALSE).
   NetworkErrorTypeHistoryBrowse:insertColumn(fTL("Type Name"),       120, "CHARACTER", "left", FALSE).
   NetworkErrorTypeHistoryBrowse:insertColumn(fTL("Level"),           120, "INTERGER", "left", FALSE).
   NetworkErrorTypeHistoryBrowse:insertColumn(fTL("User"),            150, "INTERGER", "left", FALSE).
   NetworkErrorTypeHistoryBrowse:insertColumn(fTL("Created"),         150, "CHARACTER", "left", FALSE).
   
   NetworkErrorTypeHistoryBrowse:StartBody().
   
   IF AVAILABLE NetworkErrorType THEN
   DO:
      /*List the NetworkErrorTypeHistory*/
      FOR EACH NetworkErrorTypeHistory NO-LOCK 
         WHERE  NetworkErrorTypeHistory.NetworkErrorTypeID = intSelectedNetworkErrorType
         BY NetworkErrorTypeHistory.Created DESC:
         
         FIND FIRST GateUser OF NetworkErrorTypeHistory NO-LOCK NO-ERROR.
         FIND FIRST NetworkErrorSeverityLevel OF NetworkErrorTypeHistory NO-LOCK NO-ERROR.
       
         NetworkErrorTypeHistoryBrowse:startRow(NetworkErrorTypeHistory.NetworkErrorTypeHistoryID, "selectHistoryRow(this," + '"' + STRING(NetworkErrorTypeHistory.NetworkErrorTypeHistoryID) 
                                                                     + '","networkErrorTypeHistory"' + ");", "").
         NetworkErrorTypeHistoryBrowse:insertData(NetworkErrorTypeHistory.NetworkErrorTypeHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i NetworkErrorTypeHistory}
         
         NetworkErrorTypeHistoryBrowse:insertData(NetworkErrorTypeHistory.NetworkErrorTypeCode, "left").
         NetworkErrorTypeHistoryBrowse:insertData(NetworkErrorTypeHistory.NetworkErrorTypeName, "left").        
         NetworkErrorTypeHistoryBrowse:insertData((IF AVAILABLE NetworkErrorSeverityLevel THEN NetworkErrorSeverityLevel.SeverityLevelName ELSE ""), "left").
         NetworkErrorTypeHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         NetworkErrorTypeHistoryBrowse:insertData(fDisplayDate&Time(NetworkErrorTypeHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         NetworkErrorTypeHistoryBrowse:insertHiddendata("NetworkErrorTypeHistoryID",NetworkErrorTypeHistory.NetworkErrorTypeHistoryID).
         
         NetworkErrorTypeHistoryBrowse:endRow().
      
      END. /* FOR EACH NetworkErrorTypeHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   NetworkErrorTypeHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkErrorTypeHistoryBrowse:getErrors().
   
   NetworkErrorTypeHistoryBrowseForm:insertHiddenField("NetworkErrorTypeID","").
   NetworkErrorTypeHistoryBrowseForm:insertHiddenField("NetworkErrorTypeHistoryID","").
   NetworkErrorTypeHistoryBrowseForm:insertHiddenField("popup_networkerrortypehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorTypeHistoryBrowseForm}
   
   /* Create Button Bar */
   NetworkErrorTypeHistoryBrowseButtons = NEW buttonBar().                                                 
   
   NetworkErrorTypeHistoryBrowseButtons:addButton("networkerrortypehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewNetworkErrorTypeHistoryDetails('networkerrortypehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   NetworkErrorTypeHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   NetworkErrorTypeHistoryBrowseButtons:addButton("networkerrortypehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('networkerrortypehistory_browse_form_popup');").
   
   NetworkErrorTypeHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorTypeHistoryBrowseForm:FormBrowse  = NetworkErrorTypeHistoryBrowse.
   NetworkErrorTypeHistoryBrowseForm:FormButtons = NetworkErrorTypeHistoryBrowseButtons.
   NetworkErrorTypeHistoryBrowseForm:endForm(). 
   
   NetworkErrorTypeHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorTypeHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkErrorTypeHistoryDetails Procedure
PROCEDURE pNetworkErrorTypeHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "networkerrortypehistory_details_form"}
   
   chrDisplayFieldList  = "NetworkErrorTypeHistoryID,NetworkErrorTypeID,NetworkErrorTypeCode,NetworkErrorTypeName"
                        + ",NetworkErrorSeverityLevelID,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   NetworkErrorTypeHistoryDetailsForm = NEW dataForm("networkerrortypehistory_details_form").
   NetworkErrorTypeHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   NetworkErrorTypeHistoryDetailsForm:FormWidth   = 545.
   NetworkErrorTypeHistoryDetailsForm:FormHeight  = 440.
   NetworkErrorTypeHistoryDetailsForm:FormTitle   = "Network Error Type History Details".
   NetworkErrorTypeHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkErrorTypeHistoryDetailsForm:insertPaddingColumn(40).
   NetworkErrorTypeHistoryDetailsForm:insertColumn(110).
   NetworkErrorTypeHistoryDetailsForm:insertColumn(120).
   NetworkErrorTypeHistoryDetailsForm:insertColumn(20).
   NetworkErrorTypeHistoryDetailsForm:insertColumn(4).
   NetworkErrorTypeHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   NetworkErrorTypeHistoryDetailsForm:startRow().
   NetworkErrorTypeHistoryDetailsForm:insertLabel(fTL("History ID")).
   NetworkErrorTypeHistoryDetailsForm:insertTextField("NetworkErrorTypeHistoryID", "", 200, TRUE).    
   
   NetworkErrorTypeHistoryDetailsForm:startRow().
   NetworkErrorTypeHistoryDetailsForm:insertLabel(fTL("Type ID")).
   NetworkErrorTypeHistoryDetailsForm:insertTextField("NetworkErrorTypeID", "", 200, TRUE).
   
   NetworkErrorTypeHistoryDetailsForm:startRow().
   NetworkErrorTypeHistoryDetailsForm:insertLabel(fTL("Type Code")).
   NetworkErrorTypeHistoryDetailsForm:insertTextField("NetworkErrorTypeCode", "", 200, TRUE).
   
   NetworkErrorTypeHistoryDetailsForm:startRow().
   NetworkErrorTypeHistoryDetailsForm:insertLabel(fTL("Type Name")).
   NetworkErrorTypeHistoryDetailsForm:insertTextField("NetworkErrorTypeName", "", 200, TRUE).
   
   NetworkErrorTypeHistoryDetailsForm:startRow().
   NetworkErrorTypeHistoryDetailsForm:insertLabel(fTL("Level")).
   NetworkErrorTypeHistoryDetailsForm:insertComboField("NetworkErrorSeverityLevelID", "", 200, TRUE).
   FOR EACH NetworkErrorSeverityLevel NO-LOCK: /*idx=NetworkReaderTypeID*/
      
      NetworkErrorTypeHistoryDetailsForm:insertComboPairs("NetworkErrorSeverityLevelID", STRING(NetworkErrorSeverityLevel.NetworkErrorSeverityLevelID), NetworkErrorSeverityLevel.SeverityLevelName). 
   
   END.
   
   NetworkErrorTypeHistoryDetailsForm:startRow().
   NetworkErrorTypeHistoryDetailsForm:insertLabel("User").
   NetworkErrorTypeHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      NetworkErrorTypeHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   NetworkErrorTypeHistoryDetailsForm:startRow().
   NetworkErrorTypeHistoryDetailsForm:insertLabel("Created").
   NetworkErrorTypeHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   NetworkErrorTypeHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkErrorTypeHistoryDetailsForm:insertLabel(":").
   NetworkErrorTypeHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   NetworkErrorTypeHistoryDetailsForm:insertHiddenField("networkerrortype_browse_scroll","").
   NetworkErrorTypeHistoryDetailsForm:insertHiddenField("popup_networkerrortypehistory_browse", "").
   NetworkErrorTypeHistoryDetailsForm:insertHiddenField("NetworkErrorTypeHistoryID","").
   NetworkErrorTypeHistoryDetailsForm:insertHiddenField("form_name","networkerrortypehistory_details_form").
   NetworkErrorTypeHistoryDetailsForm:insertHiddenField("prog_name","adNetworkErrorType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkErrorTypeHistoryDetailsForm}
   
   /* Create Button Bar */
   NetworkErrorTypeHistoryDetailsButtons = NEW buttonBar().
   
   NetworkErrorTypeHistoryDetailsButtons:addButton("networkerrortypehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('networkerrortypehistory_details_form_popup');").
                                        
   NetworkErrorTypeHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkErrorTypeHistoryDetailsForm:FormButtons = NetworkErrorTypeHistoryDetailsButtons.
   
   NetworkErrorTypeHistoryDetailsForm:endForm(). 
   NetworkErrorTypeHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

