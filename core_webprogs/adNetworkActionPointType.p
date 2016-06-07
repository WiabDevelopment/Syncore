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

DEFINE VARIABLE intSelectedNetworkActionPointType        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkActionPointTypeRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkActionPointTypeRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkActionPointTypeID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupNetworkActionPointTypeHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE NetworkActionPointTypeBrowseFrame        AS pageFrame.
DEFINE VARIABLE NetworkActionPointTypeBrowse             AS browseTable.
DEFINE VARIABLE NetworkActionPointTypeBrowseButtons      AS buttonBar.
DEFINE VARIABLE NetworkActionPointTypeDetailsForm        AS dataForm.
DEFINE VARIABLE NetworkActionPointTypeDetailsButtons     AS buttonBar.

DEFINE VARIABLE NetworkActionPointTypeHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE NetworkActionPointTypeHistoryBrowse         AS browseTable.
DEFINE VARIABLE NetworkActionPointTypeHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE NetworkActionPointTypeHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE NetworkActionPointTypeHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrNetworkActionPointTypeID          = get-value("NetworkActionPointTypeID")
          intSelectedNetworkActionPointType    = INTEGER(chrNetworkActionPointTypeID)
          chrScrollToNetworkActionPointTypeRow = STRING(INTEGER(get-value("networkactionpointtype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrNetworkActionPointTypeID <> "" THEN
     chrSelectNetworkActionPointTypeRow = 'selectNetworkActionPointTypeRow(document.getElementById("networkactionpointtype_browse_row_' + chrNetworkActionPointTypeID + '"),"' 
                                                         + chrNetworkActionPointTypeID +  '");'.
   
   IF get-value('popup_networkactionpointtypehistory_browse') = "yes" THEN
      chrPopupNetworkActionPointTypeHistory  = 'enablePopup("networkactionpointtypehistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkactionpointtype_browse").scrollTop=' + chrScrollToNetworkActionPointTypeRow 
                                                          + chrSelectNetworkActionPointTypeRow + chrPopupNetworkActionPointTypeHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "NetworkActionPointType Admin".
   ThisPage:FrameTitle    = "NetworkActionPointType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("networkactionpointtype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkActionPointTypeBrowse.
   
   FIND FIRST NetworkActionPointType NO-LOCK
      WHERE NetworkActionPointType.NetworkActionPointTypeID = intSelectedNetworkActionPointType NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pNetworkActionPointTypeDetails.
   RUN pNetworkActionPointTypeHistory.
   RUN pNetworkActionPointTypeHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT NetworkActionPointTypeBrowseFrame           NO-ERROR.
   DELETE OBJECT NetworkActionPointTypeBrowse                NO-ERROR.
   DELETE OBJECT NetworkActionPointTypeBrowseButtons         NO-ERROR.
   DELETE OBJECT NetworkActionPointTypeDetailsForm           NO-ERROR.
   DELETE OBJECT NetworkActionPointTypeDetailsButtons        NO-ERROR.
   
   DELETE OBJECT NetworkActionPointTypeHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT NetworkActionPointTypeHistoryBrowse            NO-ERROR.
   DELETE OBJECT NetworkActionPointTypeHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT NetworkActionPointTypeHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT NetworkActionPointTypeHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointTypeBrowse Procedure 
PROCEDURE pNetworkActionPointTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkactionpointtype_details_form"}
   
   NetworkActionPointTypeBrowse = NEW browseTable("networkactionpointtype_browse").
   NetworkActionPointTypeBrowse:BrowseWidth  = 965.
   NetworkActionPointTypeBrowse:BrowseHeight = 455.
   NetworkActionPointTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkActionPointTypeBrowse:insertColumn(fTL("TypeID"), 75, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionPointType}
   
   NetworkActionPointTypeBrowse:insertColumn(fTL("List Seq"),         60, "INTEGER", FALSE).
   NetworkActionPointTypeBrowse:insertColumn(fTL("Type Code"),       170, "CHARACTER", "left", FALSE).
   NetworkActionPointTypeBrowse:insertColumn(fTL("Type Name"),       180, "CHARACTER", "left", FALSE).
   NetworkActionPointTypeBrowse:insertColumn(fTL("Type Description"),      190, "CHARACTER", "left", FALSE).
   NetworkActionPointTypeBrowse:insertColumn(fTL("Active"),           50, "LOGICAL", FALSE).
   
   /*Body*/
   NetworkActionPointTypeBrowse:startBody().
   
   FOR EACH NetworkActionPointType NO-LOCK /*idx=ActiveListingSequence*/
      BY    NetworkActionPointType.Active DESCENDING
      BY    NetworkActionPointType.ListingSequence:
      
      NetworkActionPointTypeBrowse:startRow(NetworkActionPointType.NetworkActionPointTypeID, "selectNetworkActionPointTypeRow(this," + '"' + STRING(NetworkActionPointType.NetworkActionPointTypeID) 
                                                                  + '"' + ");", "").
      NetworkActionPointTypeBrowse:insertData(NetworkActionPointType.NetworkActionPointTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkActionPointType}
      
      NetworkActionPointTypeBrowse:insertData(STRING(NetworkActionPointType.ListingSequence)).
      NetworkActionPointTypeBrowse:insertData(NetworkActionPointType.TypeCode, "left").
      NetworkActionPointTypeBrowse:insertData(NetworkActionPointType.TypeName, "left").
      NetworkActionPointTypeBrowse:insertData(NetworkActionPointType.TypeDescr, "left").
      NetworkActionPointTypeBrowse:insertData(STRING(NetworkActionPointType.Active,"Yes/No")).
      
      /* Add hidden fields */
      NetworkActionPointTypeBrowse:insertHiddenData("NetworkActionPointTypeVersionID",NetworkActionPointType.VersionID).
      
      NetworkActionPointTypeBrowse:endRow().
      
   END. /*FOR EACH NetworkActionPointType NO-LOCK */
   
   NetworkActionPointTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkActionPointTypeBrowse:getErrors().
   
   /* Create a new frame */
   NetworkActionPointTypeBrowseFrame = NEW pageFrame().
   NetworkActionPointTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkActionPointTypeBrowseFrame:FormAction="dbNetworkActionPointTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   NetworkActionPointTypeBrowseFrame:formOpen("networkactionpointtype_browse_form").
   
   /* Start the Frame Header */
   NetworkActionPointTypeBrowseFrame:insertSpacer(5).
   NetworkActionPointTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkActionPointTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkActionPointTypeBrowseFrame:frameClose().
   NetworkActionPointTypeBrowseFrame:insertSpacer(10).
   
   NetworkActionPointTypeBrowseFrame:insertHiddenField("networkactionpointtype_browse_scroll","").
   NetworkActionPointTypeBrowseFrame:insertHiddenField("NetworkActionPointTypeID","").
   NetworkActionPointTypeBrowseFrame:insertHiddenField("NetworkActionPointTypeVersionID","").
   NetworkActionPointTypeBrowseFrame:insertHiddenField("popup_networkactionpointtypehistory_browse","").
   NetworkActionPointTypeBrowseFrame:insertHiddenField("form_name","networkactionpointtype_browse_form").
   NetworkActionPointTypeBrowseFrame:insertHiddenField("prog_name","adNetworkActionPointType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointTypeBrowseFrame}
   
   NetworkActionPointTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkActionPointTypeBrowseButtons = NEW buttonBar().
   NetworkActionPointTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   NetworkActionPointTypeBrowseButtons:addButton("networkactionpointtype_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewNetworkActionPointTypeDetails('networkactionpointtype_details_form');",
                                             "Disabled").
   
   NetworkActionPointTypeBrowseButtons:addButton("networkactionpointtype_browse_form_btn_create",
                                             fTL("Create"),
                                             "createNetworkActionPointType('networkactionpointtype_details_form');",
                                             "").
                                             
   NetworkActionPointTypeBrowseButtons:addButton("networkactionpointtype_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   NetworkActionPointTypeBrowseButtons:addButton("networkactionpointtype_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteNetworkActionPointType();",
                                             (IF intSelectedNetworkActionPointType > 0 THEN "" ELSE "Disabled")).
   */
   
   NetworkActionPointTypeBrowseButtons:closeBar().  
   NetworkActionPointTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointTypeDetails Procedure 
PROCEDURE pNetworkActionPointTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkactionpointtype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkActionPointTypeID,ListingSequence,TypeCode,TypeName,TypeDescr,Active" 
          chrEditFieldList     = "ListingSequence,TypeName,TypeDescr,Active" 
          chrNewFieldList      = "TypeCode,ListingSequence,TypeName,TypeDescr,Active" 
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkActionPointTypeDetailsForm = NEW dataForm("networkactionpointtype_details_form").
   NetworkActionPointTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkActionPointTypeDetailsForm:FormAction = "dbNetworkActionPointTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkActionPointTypeDetailsForm:FormWidth   = 580.
   NetworkActionPointTypeDetailsForm:FormHeight  = 420.
   NetworkActionPointTypeDetailsForm:FormTitle   = "NetworkActionPointType Details".
   NetworkActionPointTypeDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkActionPointTypeDetailsForm:insertPaddingColumn(30).
   NetworkActionPointTypeDetailsForm:insertColumn(150).
   NetworkActionPointTypeDetailsForm:insertColumn(160).
   NetworkActionPointTypeDetailsForm:insertColumn(20).
   NetworkActionPointTypeDetailsForm:insertColumn(4).
   NetworkActionPointTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkActionPointTypeDetailsForm:startRow().
   NetworkActionPointTypeDetailsForm:insertLabel("Type ID").
   NetworkActionPointTypeDetailsForm:insertTextField("NetworkActionPointTypeID", "", 200, TRUE).  
   
   NetworkActionPointTypeDetailsForm:startRow().
   NetworkActionPointTypeDetailsForm:insertLabel("Listing Seq").
   NetworkActionPointTypeDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).  
   
   NetworkActionPointTypeDetailsForm:startRow().
   NetworkActionPointTypeDetailsForm:insertLabel("Type Code").
   NetworkActionPointTypeDetailsForm:insertTextField("TypeCode", "", 200, TRUE).  
   
   NetworkActionPointTypeDetailsForm:startRow().
   NetworkActionPointTypeDetailsForm:insertLabel("Type Name").
   NetworkActionPointTypeDetailsForm:insertTextField("TypeName", "", 200, TRUE).
   
   NetworkActionPointTypeDetailsForm:startRow().
   NetworkActionPointTypeDetailsForm:insertLabel("Type Description").
   NetworkActionPointTypeDetailsForm:insertTextAreaField("TypeDescr", "", 250, TRUE).  
   
   NetworkActionPointTypeDetailsForm:startRow().
   NetworkActionPointTypeDetailsForm:insertLabel(fTL("Active")). 
   NetworkActionPointTypeDetailsForm:insertComboField("Active", "", 200, TRUE).  
   NetworkActionPointTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   NetworkActionPointTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pNetworkActionPointTypeDetailsFields}
   
   /* Add Hidden Fields*/
   NetworkActionPointTypeDetailsForm:insertHiddenField("networkactionpointtype_browse_scroll", "").
   NetworkActionPointTypeDetailsForm:insertHiddenField("form_name", "networkactionpointtype_details_form").
   NetworkActionPointTypeDetailsForm:insertHiddenField("prog_name", "adNetworkActionPointType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointTypeDetailsForm}
   
   /* Create Button Bar */
   NetworkActionPointTypeDetailsButtons = NEW buttonBar().
   
   NetworkActionPointTypeDetailsButtons:addButton("networkactionpointtype_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateNetworkActionPointType('networkactionpointtype_details_form');").
   
   NetworkActionPointTypeDetailsButtons:addButton("networkactionpointtype_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); 
                                               disablePopup('networkactionpointtype_details_form_popup');").
   
   NetworkActionPointTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionPointTypeDetailsForm:FormButtons = NetworkActionPointTypeDetailsButtons.
   
   NetworkActionPointTypeDetailsForm:endForm(). 
   
   NetworkActionPointTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkActionPointTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointTypeDetailsFields Procedure 
PROCEDURE pNetworkActionPointTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         NetworkActionPointTypeDetailsForm:startRow().
         NetworkActionPointTypeDetailsForm:insertLabel(fTL("Field Label")).
         NetworkActionPointTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adNetworkActionPointType_networkactionpointtype_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointTypeHistory Procedure
PROCEDURE pNetworkActionPointTypeHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "networkactionpointhistory_details_form"}
   
   FIND FIRST NetworkActionPointType WHERE NetworkActionPointType.NetworkActionPointTypeID = intSelectedNetworkActionPointType NO-LOCK NO-ERROR.
   
   NetworkActionPointTypeHistoryBrowseForm = NEW dataForm("networkactionpointtypehistory_browse_form").
   NetworkActionPointTypeHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkActionPointTypeHistoryBrowseForm:FormWidth   = 850.
   NetworkActionPointTypeHistoryBrowseForm:FormHeight  = 540.
   NetworkActionPointTypeHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE NetworkActionPointType THEN " for MaskRuleTypeID: " 
                                                                  + STRING(NetworkActionPointType.NetworkActionPointTypeID) ELSE "").
   NetworkActionPointTypeHistoryBrowseForm:FormType    = "xxl_large".
   
   NetworkActionPointTypeHistoryBrowse = NEW browseTable("networkactionpointtypehistory_browse").
   NetworkActionPointTypeHistoryBrowse:BrowseWidth  = 830.
   NetworkActionPointTypeHistoryBrowse:BrowseHeight = 500.
   NetworkActionPointTypeHistoryBrowse:ExcelExport  = TRUE.
   NetworkActionPointTypeHistoryBrowse:SessionID    = intGblSessionID.
   
   
   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionPointTypeHistory}

   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("Listing Seq"),     70, "INTEGER",           FALSE).   
   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("User"),           120, "CHARACTER", "left", FALSE).
   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("Type Code"),       80, "CHARACTER", "left", FALSE).
   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("Type Name"),       80, "CHARACTER", "left", FALSE).
   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("Type Description"),     140, "CHARACTER", "left", FALSE).
   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("Active"),          50, "LOGICAL",           FALSE).
   NetworkActionPointTypeHistoryBrowse:insertColumn(fTL("Created"),        135, "CHARACTER", "left", FALSE).
   
   NetworkActionPointTypeHistoryBrowse:StartBody().
   
   IF AVAILABLE NetworkActionPointType THEN
   DO:
      /*List the NetworkActionPointTypeHistoryory*/
      FOR EACH NetworkActionPointTypeHistory NO-LOCK 
         WHERE  NetworkActionPointTypeHistory.NetworkActionPointTypeID = intSelectedNetworkActionPointType
         BY NetworkActionPointTypeHistory.NetworkActionPointTypeHistoryID:
         
         FIND FIRST GateUser OF NetworkActionPointTypeHistory NO-LOCK NO-ERROR.
       
         NetworkActionPointTypeHistoryBrowse:startRow(NetworkActionPointTypeHistory.NetworkActionPointTypeHistoryID, "selectHistoryRow(this," + '"' + STRING(NetworkActionPointTypeHistory.NetworkActionPointTypeHistoryID) 
                                                                     + '","networkActionPointTypeHistory"' + ");", "").
         NetworkActionPointTypeHistoryBrowse:insertData(NetworkActionPointTypeHistory.NetworkActionPointTypeHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i NetworkActionPointTypeHistory}
         
         NetworkActionPointTypeHistoryBrowse:insertData(NetworkActionPointTypeHistory.ListingSequence, "").
         NetworkActionPointTypeHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         NetworkActionPointTypeHistoryBrowse:insertData(NetworkActionPointTypeHistory.TypeCode, "left").
         NetworkActionPointTypeHistoryBrowse:insertData(NetworkActionPointTypeHistory.TypeName, "left").
         NetworkActionPointTypeHistoryBrowse:insertData(NetworkActionPointTypeHistory.TypeDescr, "left").                            
         NetworkActionPointTypeHistoryBrowse:insertData(STRING(NetworkActionPointTypeHistory.Active, "Yes/No")).
         NetworkActionPointTypeHistoryBrowse:insertData(fDisplayDate&Time(NetworkActionPointTypeHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         NetworkActionPointTypeHistoryBrowse:insertHiddendata("NetworkActionPointTypeHistoryID",NetworkActionPointTypeHistory.NetworkActionPointTypeHistoryID).
         
         NetworkActionPointTypeHistoryBrowse:endRow().
      
      END. /* FOR EACH NetworkActionPointTypeHistory OF NetworkActionPointType NO-LOCK, */
   END. /*IF AVAILABLE NetworkActionPointType THEN*/
   
   NetworkActionPointTypeHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkActionPointTypeHistoryBrowse:getErrors().
   
   NetworkActionPointTypeHistoryBrowseForm:insertHiddenField("NetworkActionPointTypeHistoryID","").
   NetworkActionPointTypeHistoryBrowseForm:insertHiddenField("popup_networkactionpointtypehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointTypeHistoryBrowseForm}
   
   /* Create Button Bar */
   NetworkActionPointTypeHistoryBrowseButtons = NEW buttonBar().                                                 
   
   NetworkActionPointTypeHistoryBrowseButtons:addButton("networkactionpointtypehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewNetworkActionPointTypeHistoryDetails('networkactionpointtypehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   NetworkActionPointTypeHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   NetworkActionPointTypeHistoryBrowseButtons:addButton("networkactionpointtypehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('networkactionpointtypehistory_browse_form_popup');").
   
   NetworkActionPointTypeHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionPointTypeHistoryBrowseForm:FormBrowse  = NetworkActionPointTypeHistoryBrowse.
   NetworkActionPointTypeHistoryBrowseForm:FormButtons = NetworkActionPointTypeHistoryBrowseButtons.
   NetworkActionPointTypeHistoryBrowseForm:endForm(). 
   
   NetworkActionPointTypeHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointTypeHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointTypeHistoryDetails Procedure
PROCEDURE pNetworkActionPointTypeHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "networkactionpointhistory_details_form"}
   
   chrDisplayFieldList  = "NetworkActionPointTypeHistoryID,NetworkActionPointTypeID,TypeCode,TypeName,TypeDescr"
                        + ",ListingSequence,Active,CreatedDate,CreatedHour,CreatedMins,GateUserID,TransactionID".
                             
   
   NetworkActionPointTypeHistoryDetailsForm = NEW dataForm("networkactionpointtypehistory_details_form").
   NetworkActionPointTypeHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   NetworkActionPointTypeHistoryDetailsForm:FormWidth   = 545.
   NetworkActionPointTypeHistoryDetailsForm:FormHeight  = 440.
   NetworkActionPointTypeHistoryDetailsForm:FormTitle   = "Network Action Point Type History Details".
   NetworkActionPointTypeHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkActionPointTypeHistoryDetailsForm:insertPaddingColumn(40).
   NetworkActionPointTypeHistoryDetailsForm:insertColumn(160).
   NetworkActionPointTypeHistoryDetailsForm:insertColumn(120).
   NetworkActionPointTypeHistoryDetailsForm:insertColumn(20).
   NetworkActionPointTypeHistoryDetailsForm:insertColumn(4).
   NetworkActionPointTypeHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(fTL("History ID")).
   NetworkActionPointTypeHistoryDetailsForm:insertTextField("NetworkActionPointTypeHistoryID", "", 200, TRUE).    
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(fTL("NetworkActionPointTypeID")).
   NetworkActionPointTypeHistoryDetailsForm:insertTextField("NetworkActionPointTypeID", "", 200, TRUE).
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(fTL("TypeCode")).
   NetworkActionPointTypeHistoryDetailsForm:insertTextField("TypeCode", "", 200, TRUE).
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(fTL("TypeName")).
   NetworkActionPointTypeHistoryDetailsForm:insertTextField("TypeName", "", 200, TRUE).
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(fTL("Type Description")).
   NetworkActionPointTypeHistoryDetailsForm:insertTextAreaField("TypeDescr", "", 250, TRUE).
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(fTL("ListingSequence")).
   NetworkActionPointTypeHistoryDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(fTL("Active")).
   NetworkActionPointTypeHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   NetworkActionPointTypeHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   NetworkActionPointTypeHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel("User").
   NetworkActionPointTypeHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      NetworkActionPointTypeHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   NetworkActionPointTypeHistoryDetailsForm:startRow().
   NetworkActionPointTypeHistoryDetailsForm:insertLabel("Created").
   NetworkActionPointTypeHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   NetworkActionPointTypeHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkActionPointTypeHistoryDetailsForm:insertLabel(":").
   NetworkActionPointTypeHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   NetworkActionPointTypeHistoryDetailsForm:insertHiddenField("networkactionpointtype_browse_scroll","").
   NetworkActionPointTypeHistoryDetailsForm:insertHiddenField("popup_networkactionpointtypehistory_browse", "").
   NetworkActionPointTypeHistoryDetailsForm:insertHiddenField("NetworkActionPointTypeHistoryID","").
   NetworkActionPointTypeHistoryDetailsForm:insertHiddenField("form_name","networkactionpointtypehistory_details_form").
   NetworkActionPointTypeHistoryDetailsForm:insertHiddenField("prog_name","adNetworkActionPointType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointTypeHistoryDetailsForm}
   
   /* Create Button Bar */
   NetworkActionPointTypeHistoryDetailsButtons = NEW buttonBar().
   
   NetworkActionPointTypeHistoryDetailsButtons:addButton("networkactionpointtypehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('networkactionpointtypehistory_details_form_popup');").
                                        
   NetworkActionPointTypeHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionPointTypeHistoryDetailsForm:FormButtons = NetworkActionPointTypeHistoryDetailsButtons.
   
   NetworkActionPointTypeHistoryDetailsForm:endForm(). 
   NetworkActionPointTypeHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

