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

  Created: 28/09/2015

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

DEFINE VARIABLE intSelectedNetworkReaderType        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkReaderTypeRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkReaderTypeRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkReaderTypeID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupNetworkReaderTypeHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE NetworkReaderTypeBrowseFrame        AS pageFrame.
DEFINE VARIABLE NetworkReaderTypeBrowse             AS browseTable.
DEFINE VARIABLE NetworkReaderTypeBrowseButtons      AS buttonBar.
DEFINE VARIABLE NetworkReaderTypeDetailsForm        AS dataForm.
DEFINE VARIABLE NetworkReaderTypeDetailsButtons     AS buttonBar.

DEFINE VARIABLE NetworkReaderTypeHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE NetworkReaderTypeHistoryBrowse         AS browseTable.
DEFINE VARIABLE NetworkReaderTypeHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE NetworkReaderTypeHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE NetworkReaderTypeHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrNetworkReaderTypeID          = get-value("NetworkReaderTypeID")
          intSelectedNetworkReaderType    = INTEGER(chrNetworkReaderTypeID)
          chrScrollToNetworkReaderTypeRow = STRING(INTEGER(get-value("networkreadertype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrNetworkReaderTypeID <> "" THEN
     chrSelectNetworkReaderTypeRow = 'selectNetworkReaderTypeRow(document.getElementById("networkreadertype_browse_row_' + chrNetworkReaderTypeID + '"),"' 
                                                         + chrNetworkReaderTypeID +  '");'.
   
   IF get-value('popup_networkreadertypehistory_browse') = "yes" THEN
      chrPopupNetworkReaderTypeHistory  = 'enablePopup("networkreadertypehistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkreadertype_browse").scrollTop=' + chrScrollToNetworkReaderTypeRow 
                                                          + chrSelectNetworkReaderTypeRow + chrPopupNetworkReaderTypeHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "NetworkReaderType Admin".
   ThisPage:FrameTitle    = "NetworkReaderType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("networkreadertype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkReaderTypeBrowse.
   
   FIND FIRST NetworkReaderType NO-LOCK
      WHERE NetworkReaderType.NetworkReaderTypeID = intSelectedNetworkReaderType NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pNetworkReaderTypeDetails.
   RUN pNetworkReaderTypeHistory.
   RUN pNetworkReaderTypeHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT NetworkReaderTypeBrowseFrame           NO-ERROR.
   DELETE OBJECT NetworkReaderTypeBrowse                NO-ERROR.
   DELETE OBJECT NetworkReaderTypeBrowseButtons         NO-ERROR.
   DELETE OBJECT NetworkReaderTypeDetailsForm           NO-ERROR.
   DELETE OBJECT NetworkReaderTypeDetailsButtons        NO-ERROR.
   
   DELETE OBJECT NetworkReaderTypeHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT NetworkReaderTypeHistoryBrowse            NO-ERROR.
   DELETE OBJECT NetworkReaderTypeHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT NetworkReaderTypeHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT NetworkReaderTypeHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderTypeBrowse Procedure 
PROCEDURE pNetworkReaderTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkreadertype_details_form"}
   
   NetworkReaderTypeBrowse = NEW browseTable("networkreadertype_browse").
   NetworkReaderTypeBrowse:BrowseWidth  = 965.
   NetworkReaderTypeBrowse:BrowseHeight = 455.
   NetworkReaderTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkReaderTypeBrowse:insertColumn(fTL("TypeID"), 75, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkReaderType}
   
   NetworkReaderTypeBrowse:insertColumn(fTL("List Seq"),          60, "INTEGER", FALSE).
   NetworkReaderTypeBrowse:insertColumn(fTL("Type Code"),        170, "CHARACTER", "left", FALSE).
   NetworkReaderTypeBrowse:insertColumn(fTL("Type Name"),        180, "CHARACTER", "left", FALSE).
   NetworkReaderTypeBrowse:insertColumn(fTL("Type Description"), 190, "CHARACTER", "left", FALSE).
   NetworkReaderTypeBrowse:insertColumn(fTL("Active"),            50, "LOGICAL", FALSE).
   
   /*Body*/
   NetworkReaderTypeBrowse:startBody().
   
   FOR EACH NetworkReaderType NO-LOCK /*idx=ActiveListingSequence*/
      BY    NetworkReaderType.Active DESCENDING 
      BY    NetworkReaderType.ListingSequence:
      
      NetworkReaderTypeBrowse:startRow(NetworkReaderType.NetworkReaderTypeID, "selectNetworkReaderTypeRow(this," + '"' + STRING(NetworkReaderType.NetworkReaderTypeID) 
                                                                  + '"' + ");", "").
      NetworkReaderTypeBrowse:insertData(NetworkReaderType.NetworkReaderTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkReaderType}
      
      NetworkReaderTypeBrowse:insertData(STRING(NetworkReaderType.ListingSequence)).
      NetworkReaderTypeBrowse:insertData(NetworkReaderType.TypeCode, "left").
      NetworkReaderTypeBrowse:insertData(NetworkReaderType.TypeName, "left").
      NetworkReaderTypeBrowse:insertData(NetworkReaderType.TypeDescr, "left").
      NetworkReaderTypeBrowse:insertData(STRING(NetworkReaderType.Active,"Yes/No")).
      
      /* Add hidden fields */
      NetworkReaderTypeBrowse:insertHiddenData("NetworkReaderTypeVersionID",NetworkReaderType.VersionID).
      
      NetworkReaderTypeBrowse:endRow().
      
   END. /*FOR EACH NetworkReaderType NO-LOCK */
   
   NetworkReaderTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkReaderTypeBrowse:getErrors().
   
   /* Create a new frame */
   NetworkReaderTypeBrowseFrame = NEW pageFrame().
   NetworkReaderTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkReaderTypeBrowseFrame:FormAction="dbNetworkReaderTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   NetworkReaderTypeBrowseFrame:formOpen("networkreadertype_browse_form").
   
   /* Start the Frame Header */
   NetworkReaderTypeBrowseFrame:insertSpacer(5).
   NetworkReaderTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkReaderTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkReaderTypeBrowseFrame:frameClose().
   NetworkReaderTypeBrowseFrame:insertSpacer(10).
   
   NetworkReaderTypeBrowseFrame:insertHiddenField("networkreadertype_browse_scroll","").
   NetworkReaderTypeBrowseFrame:insertHiddenField("NetworkReaderTypeID","").
   NetworkReaderTypeBrowseFrame:insertHiddenField("NetworkReaderTypeVersionID","").
   NetworkReaderTypeBrowseFrame:insertHiddenField("popup_networkreadertypehistory_browse","").
   NetworkReaderTypeBrowseFrame:insertHiddenField("form_name","networkreadertype_browse_form").
   NetworkReaderTypeBrowseFrame:insertHiddenField("prog_name","adNetworkReaderType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderTypeBrowseFrame}
   
   NetworkReaderTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkReaderTypeBrowseButtons = NEW buttonBar().
   NetworkReaderTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   NetworkReaderTypeBrowseButtons:addButton("networkreadertype_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewNetworkReaderTypeDetails('networkreadertype_details_form');",
                                             "Disabled").
   
   NetworkReaderTypeBrowseButtons:addButton("networkreadertype_browse_form_btn_create",
                                             fTL("Create"),
                                             "createNetworkReaderType('networkreadertype_details_form');",
                                             "").
                                             
   NetworkReaderTypeBrowseButtons:addButton("networkreadertype_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   NetworkReaderTypeBrowseButtons:addButton("networkreadertype_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteNetworkReaderType();",
                                             (IF intSelectedNetworkReaderType > 0 THEN "" ELSE "Disabled")).
   */
   
   NetworkReaderTypeBrowseButtons:closeBar().  
   NetworkReaderTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderTypeDetails Procedure 
PROCEDURE pNetworkReaderTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkreadertype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkReaderTypeID,ListingSequence,TypeCode,TypeName,TypeDescr,Active" 
          chrEditFieldList     = "ListingSequence,TypeName,TypeDescr,Active" 
          chrNewFieldList      = "TypeCode,ListingSequence,TypeName,TypeDescr,Active" 
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkReaderTypeDetailsForm = NEW dataForm("networkreadertype_details_form").
   NetworkReaderTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkReaderTypeDetailsForm:FormAction = "dbNetworkReaderTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkReaderTypeDetailsForm:FormWidth   = 580.
   NetworkReaderTypeDetailsForm:FormHeight  = 420.
   NetworkReaderTypeDetailsForm:FormTitle   = "NetworkReaderType Details".
   NetworkReaderTypeDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkReaderTypeDetailsForm:insertPaddingColumn(30).
   NetworkReaderTypeDetailsForm:insertColumn(150).
   NetworkReaderTypeDetailsForm:insertColumn(160).
   NetworkReaderTypeDetailsForm:insertColumn(20).
   NetworkReaderTypeDetailsForm:insertColumn(4).
   NetworkReaderTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkReaderTypeDetailsForm:startRow().
   NetworkReaderTypeDetailsForm:insertLabel("Type ID").
   NetworkReaderTypeDetailsForm:insertTextField("NetworkReaderTypeID", "", 200, TRUE).  
   
   NetworkReaderTypeDetailsForm:startRow().
   NetworkReaderTypeDetailsForm:insertLabel("Listing Seq").
   NetworkReaderTypeDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).  
   
   NetworkReaderTypeDetailsForm:startRow().
   NetworkReaderTypeDetailsForm:insertLabel("Type Code").
   NetworkReaderTypeDetailsForm:insertTextField("TypeCode", "", 200, TRUE).  
   
   NetworkReaderTypeDetailsForm:startRow().
   NetworkReaderTypeDetailsForm:insertLabel("Type Name").
   NetworkReaderTypeDetailsForm:insertTextField("TypeName", "", 200, TRUE).
   
   NetworkReaderTypeDetailsForm:startRow().
   NetworkReaderTypeDetailsForm:insertLabel("Type Description").
   NetworkReaderTypeDetailsForm:insertTextAreaField("TypeDescr", "", 250, TRUE).  
   
   NetworkReaderTypeDetailsForm:startRow().
   NetworkReaderTypeDetailsForm:insertLabel(fTL("Active")). 
   NetworkReaderTypeDetailsForm:insertComboField("Active", "", 200, TRUE).  
   NetworkReaderTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   NetworkReaderTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pNetworkReaderTypeDetailsFields}
   
   /* Add Hidden Fields*/
   NetworkReaderTypeDetailsForm:insertHiddenField("networkreadertype_browse_scroll", "").
   NetworkReaderTypeDetailsForm:insertHiddenField("form_name", "networkreadertype_details_form").
   NetworkReaderTypeDetailsForm:insertHiddenField("prog_name", "adNetworkReaderType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderTypeDetailsForm}
   
   /* Create Button Bar */
   NetworkReaderTypeDetailsButtons = NEW buttonBar().
   
   NetworkReaderTypeDetailsButtons:addButton("networkreadertype_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateNetworkReaderType('networkreadertype_details_form');").
   
   NetworkReaderTypeDetailsButtons:addButton("networkreadertype_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('networkreadertype_details_form_popup');").
   
   NetworkReaderTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkReaderTypeDetailsForm:FormButtons = NetworkReaderTypeDetailsButtons.
   
   NetworkReaderTypeDetailsForm:endForm(). 
   
   NetworkReaderTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkReaderTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderTypeDetailsFields Procedure 
PROCEDURE pNetworkReaderTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         NetworkReaderTypeDetailsForm:startRow().
         NetworkReaderTypeDetailsForm:insertLabel(fTL("Field Label")).
         NetworkReaderTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adNetworkReaderType_networkreadertype_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderTypeHistory Procedure
PROCEDURE pNetworkReaderTypeHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "networkreaderhistory_details_form"}
   
   FIND FIRST NetworkReaderType WHERE NetworkReaderType.NetworkReaderTypeID = intSelectedNetworkReaderType NO-LOCK NO-ERROR.
   
   NetworkReaderTypeHistoryBrowseForm = NEW dataForm("networkreadertypehistory_browse_form").
   NetworkReaderTypeHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkReaderTypeHistoryBrowseForm:FormWidth   = 850.
   NetworkReaderTypeHistoryBrowseForm:FormHeight  = 540.
   NetworkReaderTypeHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE NetworkReaderType THEN " for MaskRuleTypeID: " 
                                                                  + STRING(NetworkReaderType.NetworkReaderTypeID) ELSE "").
   NetworkReaderTypeHistoryBrowseForm:FormType    = "xxl_large".
   
   NetworkReaderTypeHistoryBrowse = NEW browseTable("networkreadertypehistory_browse").
   NetworkReaderTypeHistoryBrowse:BrowseWidth  = 830.
   NetworkReaderTypeHistoryBrowse:BrowseHeight = 500.
   NetworkReaderTypeHistoryBrowse:ExcelExport  = TRUE.
   NetworkReaderTypeHistoryBrowse:SessionID    = intGblSessionID.
   
   
   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkReaderTypeHistory}

   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("Listing Seq"),       70, "INTEGER",           FALSE).   
   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("User"),             120, "CHARACTER", "left", FALSE).
   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("Type Code"),         80, "CHARACTER", "left", FALSE).
   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("Type Name"),         80, "CHARACTER", "left", FALSE).
   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("Type Description"), 140, "CHARACTER", "left", FALSE).
   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("Active"),            50, "LOGICAL",           FALSE).
   NetworkReaderTypeHistoryBrowse:insertColumn(fTL("Created"),          135, "CHARACTER", "left", FALSE).
   
   NetworkReaderTypeHistoryBrowse:StartBody().
   
   IF AVAILABLE NetworkReaderType THEN
   DO:
      /*List the NetworkReaderTypeHistoryory*/
      FOR EACH NetworkReaderTypeHistory NO-LOCK 
         WHERE  NetworkReaderTypeHistory.NetworkReaderTypeID = intSelectedNetworkReaderType
         BY NetworkReaderTypeHistory.NetworkReaderTypeHistoryID:
         
         FIND FIRST GateUser OF NetworkReaderTypeHistory NO-LOCK NO-ERROR.
       
         NetworkReaderTypeHistoryBrowse:startRow(NetworkReaderTypeHistory.NetworkReaderTypeHistoryID, "selectHistoryRow(this," + '"' + STRING(NetworkReaderTypeHistory.NetworkReaderTypeHistoryID) 
                                                                     + '","networkReaderTypeHistory"' + ");", "").
         NetworkReaderTypeHistoryBrowse:insertData(NetworkReaderTypeHistory.NetworkReaderTypeHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i NetworkReaderTypeHistory}
         
         NetworkReaderTypeHistoryBrowse:insertData(NetworkReaderTypeHistory.ListingSequence, "").
         NetworkReaderTypeHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         NetworkReaderTypeHistoryBrowse:insertData(NetworkReaderTypeHistory.TypeCode, "left").
         NetworkReaderTypeHistoryBrowse:insertData(NetworkReaderTypeHistory.TypeName, "left").
         NetworkReaderTypeHistoryBrowse:insertData(NetworkReaderTypeHistory.TypeDescr, "left").                            
         NetworkReaderTypeHistoryBrowse:insertData(STRING(NetworkReaderTypeHistory.Active, "Yes/No")).
         NetworkReaderTypeHistoryBrowse:insertData(fDisplayDate&Time(NetworkReaderTypeHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */         
         NetworkReaderTypeHistoryBrowse:insertHiddendata("NetworkReaderTypeHistoryID",NetworkReaderTypeHistory.NetworkReaderTypeHistoryID).
         
         NetworkReaderTypeHistoryBrowse:endRow().
      
      END. /* FOR EACH NetworkReaderTypeHistory OF NetworkReaderType NO-LOCK, */
   END. /*IF AVAILABLE NetworkReaderType THEN*/
   
   NetworkReaderTypeHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkReaderTypeHistoryBrowse:getErrors().
   
   NetworkReaderTypeHistoryBrowseForm:insertHiddenField("NetworkReaderTypeHistoryID","").
   NetworkReaderTypeHistoryBrowseForm:insertHiddenField("popup_networkreadertypehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderTypeHistoryBrowseForm}
   
   /* Create Button Bar */
   NetworkReaderTypeHistoryBrowseButtons = NEW buttonBar().                                                 
   
   NetworkReaderTypeHistoryBrowseButtons:addButton("networkreadertypehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewNetworkReaderTypeHistoryDetails('networkreadertypehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   NetworkReaderTypeHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   NetworkReaderTypeHistoryBrowseButtons:addButton("networkreadertypehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('networkreadertypehistory_browse_form_popup');").
   
   NetworkReaderTypeHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkReaderTypeHistoryBrowseForm:FormBrowse  = NetworkReaderTypeHistoryBrowse.
   NetworkReaderTypeHistoryBrowseForm:FormButtons = NetworkReaderTypeHistoryBrowseButtons.
   NetworkReaderTypeHistoryBrowseForm:endForm(). 
   
   NetworkReaderTypeHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderTypeHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderTypeHistoryDetails Procedure
PROCEDURE pNetworkReaderTypeHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "networkreaderhistory_details_form"}
   
   chrDisplayFieldList  = "NetworkReaderTypeHistoryID,NetworkReaderTypeID,TypeCode,TypeName,TypeDescr"
                        + ",ListingSequence,Active,CreatedDate,CreatedHour,CreatedMins,GateUserID,TransactionID".
                             
   
   NetworkReaderTypeHistoryDetailsForm = NEW dataForm("networkreadertypehistory_details_form").
   NetworkReaderTypeHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   NetworkReaderTypeHistoryDetailsForm:FormWidth   = 545.
   NetworkReaderTypeHistoryDetailsForm:FormHeight  = 440.
   NetworkReaderTypeHistoryDetailsForm:FormTitle   = "Network Reader Type History Details".
   NetworkReaderTypeHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkReaderTypeHistoryDetailsForm:insertPaddingColumn(40).
   NetworkReaderTypeHistoryDetailsForm:insertColumn(140).
   NetworkReaderTypeHistoryDetailsForm:insertColumn(120).
   NetworkReaderTypeHistoryDetailsForm:insertColumn(20).
   NetworkReaderTypeHistoryDetailsForm:insertColumn(4).
   NetworkReaderTypeHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel(fTL("History ID")).
   NetworkReaderTypeHistoryDetailsForm:insertTextField("NetworkReaderTypeHistoryID", "", 200, TRUE).    
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel(fTL("NetworkReaderTypeID")).
   NetworkReaderTypeHistoryDetailsForm:insertTextField("NetworkReaderTypeID", "", 200, TRUE).
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel(fTL("TypeCode")).
   NetworkReaderTypeHistoryDetailsForm:insertTextField("TypeCode", "", 200, TRUE).
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel(fTL("TypeName")).
   NetworkReaderTypeHistoryDetailsForm:insertTextField("TypeName", "", 200, TRUE).
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel(fTL("Type Description")).
   NetworkReaderTypeHistoryDetailsForm:insertTextAreaField("TypeDescr", "", 250, TRUE).
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel(fTL("ListingSequence")).
   NetworkReaderTypeHistoryDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel(fTL("Active")).
   NetworkReaderTypeHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   NetworkReaderTypeHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   NetworkReaderTypeHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel("User").
   NetworkReaderTypeHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      NetworkReaderTypeHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   NetworkReaderTypeHistoryDetailsForm:startRow().
   NetworkReaderTypeHistoryDetailsForm:insertLabel("Created").
   NetworkReaderTypeHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   NetworkReaderTypeHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkReaderTypeHistoryDetailsForm:insertLabel(":").
   NetworkReaderTypeHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   NetworkReaderTypeHistoryDetailsForm:insertHiddenField("networkreadertype_browse_scroll","").
   NetworkReaderTypeHistoryDetailsForm:insertHiddenField("popup_networkreadertypehistory_browse", "").
   NetworkReaderTypeHistoryDetailsForm:insertHiddenField("NetworkReaderTypeHistoryID","").
   NetworkReaderTypeHistoryDetailsForm:insertHiddenField("form_name","networkreadertypehistory_details_form").
   NetworkReaderTypeHistoryDetailsForm:insertHiddenField("prog_name","adNetworkReaderType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderTypeHistoryDetailsForm}
   
   /* Create Button Bar */
   NetworkReaderTypeHistoryDetailsButtons = NEW buttonBar().
   
   NetworkReaderTypeHistoryDetailsButtons:addButton("networkreadertypehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('networkreadertypehistory_details_form_popup');").
                                        
   NetworkReaderTypeHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkReaderTypeHistoryDetailsForm:FormButtons = NetworkReaderTypeHistoryDetailsButtons.
   
   NetworkReaderTypeHistoryDetailsForm:endForm(). 
   NetworkReaderTypeHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

