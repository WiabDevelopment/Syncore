&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adInboundConfigAdmin.p 

  Description: ad file for the Inbound Config Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Shawn Hilts

  Created: 24/11/2014

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
{defWebDefinitions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Definitions for System Options for Receiving */
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedInboundConfig        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrInboundConfigRow             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToInboundConfigRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrInboundConfigID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                 AS CHARACTER   NO-UNDO.

/* Buffers */

/* Objects */
DEFINE VARIABLE InboundConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE InboundConfigBrowse                AS browseTable.
DEFINE VARIABLE InboundConfigBrowseButtons         AS buttonBar.
DEFINE VARIABLE InboundConfigDetailsForm           AS dataForm.
DEFINE VARIABLE InboundConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE InboundConfigDetailsButtons        AS buttonBar.
DEFINE VARIABLE InboundConfigHistoryBrowseForm     AS dataform. 
DEFINE VARIABLE InboundConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE InboundConfigHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE InboundConfigHistoryBrowseButtons  AS buttonBar.



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
         HEIGHT             = 17.71
         WIDTH              = 67.2.
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
   
   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigHistoryDetails Procedure
PROCEDURE pInboundConfigHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "inboundconfighistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "InboundConfigHistoryID,InboundConfigID,DefaultMaxNumUsersPerTask," +
                                 "AllowUnderReceiving,AllowOverReceiving,UsingMultiAsns," +
                                 "ConsolidateAsnLinesAtReceiving,UsingFaiFunctionality," +
                                 "PutStockOnHoldDuringFAI,UsingCustoms,AllowPartialRelease," +
                                 "AllowPartialReconcile,PrintFloorSheetsAtInbound,UsingReceivingPO," +
                                 "ManuallyReleaseStockAfterInbound,PutStockOnHoldAfterInbound," +
                                 "PostReconcileStockStatusID,PostReconcileInboundStatusID," +
                                 "PostReleaseStockStatusID,PostReleaseInboundStatusID,UseCustomerPackageLabelAtInbound"    
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   InboundConfigHistoryDetailsForm = NEW dataForm("inboundconfighistory_details_form").
   InboundConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   /*InboundConfigHistoryDetailsForm:FormAction = "dbInboundConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").*/
   
   /* Setup */
   InboundConfigHistoryDetailsForm:FormWidth   = 700.
   InboundConfigHistoryDetailsForm:FormHeight  = 500.
   InboundConfigHistoryDetailsForm:FormTitle   = fTL("InboundConfigHistory Details").
   InboundConfigHistoryDetailsForm:FormType    = "xl_large".
   
   /* Column Layout */
   InboundConfigHistoryDetailsForm:insertPaddingColumn(30).
   InboundConfigHistoryDetailsForm:insertColumn(130).
   InboundConfigHistoryDetailsForm:insertColumn(120).
   InboundConfigHistoryDetailsForm:insertColumn(20).
   InboundConfigHistoryDetailsForm:insertColumn(4).
   InboundConfigHistoryDetailsForm:insertColumn(100).
   
   /* Fields */ 
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel("InboundConfig ID").
   InboundConfigHistoryDetailsForm:insertTextField("InboundConfigID", "", 110, TRUE). 
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("DefaultMaxNumUsersPerTask")).
   InboundConfigHistoryDetailsForm:insertTextField("DefaultMaxNumUsersPerTask", "", 190, TRUE).
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("AllowUnderReceiving")).
   InboundConfigHistoryDetailsForm:insertComboField("AllowUnderReceiving", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowUnderReceiving", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowUnderReceiving", "no",  "No").

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("AllowOverReceiving")).
   InboundConfigHistoryDetailsForm:insertComboField("AllowOverReceiving", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowOverReceiving", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowOverReceiving", "no",  "No").
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("UsingMultiAsns")).
   InboundConfigHistoryDetailsForm:insertComboField("UsingMultiAsns", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingMultiAsns", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingMultiAsns", "no",  "No").

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("ConsolidateAsnLinesAtReceiving")).
   InboundConfigHistoryDetailsForm:insertComboField("ConsolidateAsnLinesAtReceiving", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("ConsolidateAsnLinesAtReceiving", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("ConsolidateAsnLinesAtReceiving", "no",  "No").

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("UsingFaiFunctionality")).
   InboundConfigHistoryDetailsForm:insertComboField("UsingFaiFunctionality", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingFaiFunctionality", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingFaiFunctionality", "no",  "No").

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("PutStockOnHoldDuringFAI")).
   InboundConfigHistoryDetailsForm:insertComboField("PutStockOnHoldDuringFAI", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("PutStockOnHoldDuringFAI", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("PutStockOnHoldDuringFAI", "no",  "No").   

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("UsingCustoms")).
   InboundConfigHistoryDetailsForm:insertComboField("UsingCustoms", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingCustoms", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingCustoms", "no",  "No").
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("AllowPartialRelease")).
   InboundConfigHistoryDetailsForm:insertComboField("AllowPartialRelease", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowPartialRelease", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowPartialRelease", "no",  "No").
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("AllowPartialReconcile")).
   InboundConfigHistoryDetailsForm:insertComboField("AllowPartialReconcile", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowPartialReconcile", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("AllowPartialReconcile", "no",  "No").

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("PrintFloorSheetsAtInbound")).
   InboundConfigHistoryDetailsForm:insertComboField("PrintFloorSheetsAtInbound", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("PrintFloorSheetsAtInbound", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("PrintFloorSheetsAtInbound", "no",  "No").
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("UsingReceivingPOs")).
   InboundConfigHistoryDetailsForm:insertComboField("UsingReceivingPOs", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingReceivingPOs", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("UsingReceivingPOs", "no",  "No").   
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("PutStockOnHoldAfterInbound")).
   InboundConfigHistoryDetailsForm:insertComboField("PutStockOnHoldAfterInbound", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("PutStockOnHoldAfterInbound", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("PutStockOnHoldAfterInbound", "no",  "No").   

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("ManuallyReleaseStockAfterInbound")).
   InboundConfigHistoryDetailsForm:insertComboField("ManuallyReleaseStockAfterInbound", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("ManuallyReleaseStockAfterInbound", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("ManuallyReleaseStockAfterInbound", "no",  "No").  
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("UseCustomerPackageLabelAtInbound")).
   InboundConfigHistoryDetailsForm:insertComboField("UseCustomerPackageLabelAtInbound", "", 190, TRUE).  
   InboundConfigHistoryDetailsForm:insertComboPairs("UseCustomerPackageLabelAtInbound", "yes", "Yes").
   InboundConfigHistoryDetailsForm:insertComboPairs("UseCustomerPackageLabelAtInbound", "no",  "No").  

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("PostReconcileStockStatusID")).
   InboundConfigHistoryDetailsForm:insertTextField("PostReconcileStockStatusID", "", 190, TRUE).
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("PostReconcileInboundStatusID")).
   InboundConfigHistoryDetailsForm:insertTextField("PostReconcileInboundStatusID", "", 190, TRUE).

   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("PostReleaseStockStatusID")).
   InboundConfigHistoryDetailsForm:insertTextField("PostReleaseStockStatusID", "", 190, TRUE).
   
   InboundConfigHistoryDetailsForm:startRow().
   InboundConfigHistoryDetailsForm:insertLabel(fTL("PostReleaseInboundStatusID")).
   InboundConfigHistoryDetailsForm:insertTextField("PostReleaseInboundStatusID", "", 190, TRUE).
         
   {webGetOptionalFormFields.i pInboundConfigHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   InboundConfigHistoryDetailsForm:insertHiddenField("inboundconfig_browse_scroll", "").
   InboundConfigHistoryDetailsForm:insertHiddenField("form_name", "inboundconfig_details_form").
   InboundConfigHistoryDetailsForm:insertHiddenField("prog_name", "adInboundConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   InboundConfigHistoryDetailsButtons = NEW buttonBar().
   InboundConfigHistoryDetailsButtons:addButton("inboundconfighistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('inboundconfighistory_details_form_popup');").
   InboundConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   InboundConfigHistoryDetailsForm:FormButtons = InboundConfigHistoryDetailsButtons.
   
   InboundConfigHistoryDetailsForm:endForm(). 
   
   InboundConfigHistoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + InboundConfigHistoryDetailsForm:getErrors().  */

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
   RUN pGetSystemOptions.  
   
   ASSIGN chrInboundConfigID = get-value("InboundConfigID")
          intSelectedInboundConfig = INTEGER(chrInboundConfigID)
          chrScrollToInboundConfigRow = STRING(INTEGER(get-value("inboundconfig_browse_scroll"))) + ";".
   
   /* Process URL values */   
   IF chrInboundConfigID <> "" THEN
      chrInboundConfigRow = 'selectInboundConfigRow(document.getElementById("inboundconfig_browse_row_' + chrInboundConfigID + '"),"' 
                                                          + chrInboundConfigID +  '");'.
                                                          
   IF get-value('popup_inboundconfighistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("inboundconfighistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("inboundconfig_browse").scrollTop=' + chrScrollToInboundConfigRow 
                                                           + chrInboundConfigRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Inbound Config Admin".
   ThisPage:FrameTitle = "Inbound Config Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Inbound Config */
   ThisPage:addJavaScript("inboundconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pInboundConfigBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pInboundConfigDetails.
   
   RUN pInboundConfigHistoryBrowse.
   
   RUN pInboundConfigHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT InboundConfigBrowseFrame           NO-ERROR.
   DELETE OBJECT InboundConfigBrowse                NO-ERROR.
   DELETE OBJECT InboundConfigBrowseButtons         NO-ERROR.
   DELETE OBJECT InboundConfigDetailsForm           NO-ERROR.
   DELETE OBJECT InboundConfigDetailsButtons        NO-ERROR.
   DELETE OBJECT InboundConfigHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT InboundConfigHistoryDetailsButtons NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigBrowse Procedure 
PROCEDURE pInboundConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "inboundconfig_details_form"}
   
   InboundConfigBrowse = NEW browseTable("inboundconfig_browse").
   InboundConfigBrowse:BrowseWidth  = 965.
   InboundConfigBrowse:BrowseHeight = 455.
   InboundConfigBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the InboundConfig ID as first Column */
   InboundConfigBrowse:insertColumn(fTL("InboundConfig ID"), 130, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i InboundConfig}
   
   InboundConfigBrowse:insertColumn(fTL("DefaultMaxNumUsersPerTask"), 390, "CHARACTER", "LEFT", FALSE).
   InboundConfigBrowse:insertColumn(fTL("AllowUnderReceiving"),     390, "CHARACTER", "LEFT", FALSE).
   
   /*Body*/
   InboundConfigBrowse:startBody().
   
   FOR EACH InboundConfig NO-LOCK: /*idx=ActiveListingSequence*/

      InboundConfigBrowse:startRow(InboundConfig.InboundConfigID, "selectInboundConfigRow(this," + '"' + STRING(InboundConfig.InboundConfigID) + '"' + ");", "").
      InboundConfigBrowse:insertData(InboundConfig.InboundConfigID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i InboundConfig}
      
      InboundConfigBrowse:insertData(IF AVAILABLE InboundConfig THEN STRING(InboundConfig.DefaultMaxNumUsersPerTask) ELSE "","LEFT").
      InboundConfigBrowse:insertData(IF AVAILABLE InboundConfig THEN STRING(InboundConfig.AllowUnderReceiving, "Yes/No") ELSE "","LEFT").
      
      /* Add hidden fields */
      InboundConfigBrowse:insertHiddenData("InboundConfigVersionID",InboundConfig.VersionID).
      
      InboundConfigBrowse:endRow().
      
   END. /*FOR EACH InboundConfig NO-LOCK */
   
   InboundConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + InboundConfigBrowse:getErrors().
   
   /* Create a new frame */
   InboundConfigBrowseFrame = NEW pageFrame().
   InboundConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   InboundConfigBrowseFrame:FormAction="dbInboundConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   InboundConfigBrowseFrame:formOpen("inboundconfig_browse_form").
   
   /* Start the Frame Header */
   InboundConfigBrowseFrame:insertSpacer(5).
   InboundConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   InboundConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   InboundConfigBrowseFrame:frameClose().
   InboundConfigBrowseFrame:insertSpacer(10).
   
   InboundConfigBrowseFrame:insertHiddenField("inboundconfig_browse_scroll","").
   InboundConfigBrowseFrame:insertHiddenField("InboundConfigID","").
   InboundConfigBrowseFrame:insertHiddenField("InboundConfigVersionID","").
   InboundConfigBrowseFrame:insertHiddenField("form_name","inboundconfig_browse_form").
   InboundConfigBrowseFrame:insertHiddenField("popup_inboundconfighistory_browse","").
   InboundConfigBrowseFrame:insertHiddenField("prog_name","adInboundConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundConfigBrowseFrame}
   
   InboundConfigBrowseFrame:formClose().
   
   /* Create Button Bar */
   InboundConfigBrowseButtons = NEW buttonBar().
   InboundConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   InboundConfigBrowseButtons:addButton("inboundconfig_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewInboundConfigDetails('inboundconfig_details_form');",
                                         (IF intSelectedInboundConfig > 0 THEN "" ELSE "Disabled")).
   
   InboundConfigBrowseButtons:addButton("inboundconfig_browse_form_btn_create",
                                         fTL("Create"),
                                         "createInboundConfig('inboundconfig_details_form');",
                                         IF CAN-FIND(FIRST InboundConfig) THEN "Disabled" ELSE "").
   
   InboundConfigBrowseButtons:addButton("inboundconfig_browse_form_btn_history",
                                         fTL("History"),
                                         "viewInboundConfigHistory('inboundconfig_browse_form');",
                                         (IF intSelectedInboundConfig > 0 THEN "" ELSE "Disabled")).
   
   InboundConfigBrowseButtons:closeBar().  
   InboundConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigDetails Procedure 
PROCEDURE pInboundConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "inboundconfig_details_form"}
   
   ASSIGN chrDisplayFieldList  = "InboundConfigID,DefaultMaxNumUsersPerTask," +
                                 "AllowUnderReceiving,AllowOverReceiving,UsingMultiAsns," +
                                 "ConsolidateAsnLinesAtReceiving,UsingFaiFunctionality," +
                                 "PutStockOnHoldDuringFAI,UsingCustoms,AllowPartialRelease," +
                                 "AllowPartialReconcile,PrintFloorSheetsAtInbound,UsingReceivingPOs," +
                                 "PutStockOnHoldAfterInbound,ManuallyReleaseStockAfterInbound," +
                                 "PostReconcileStockStatusID,PostReconcileInboundStatusID," +
                                 "PostReleaseStockStatusID,PostReleaseInboundStatusID,UseCustomerPackageLabelAtInbound"                          
          chrEditFieldList     = "DefaultMaxNumUsersPerTask,AllowUnderReceiving,AllowOverReceiving," +
                                 "UsingMultiAsns,ConsolidateAsnLinesAtReceiving,UsingFaiFunctionality," +
                                 "PutStockOnHoldDuringFAI,UsingCustoms,AllowPartialRelease," +
                                 "AllowPartialReconcile,PrintFloorSheetsAtInbound,UsingReceivingPOs," +
                                 "PutStockOnHoldAfterInbound,ManuallyReleaseStockAfterInbound," +
                                 "PostReconcileStockStatusID,PostReconcileInboundStatusID," +
                                 "PostReleaseStockStatusID,PostReleaseInboundStatusID,UseCustomerPackageLabelAtInbound"
          chrNewFieldList      = "DefaultMaxNumUsersPerTask,AllowUnderReceiving,AllowOverReceiving," +
                                 "UsingMultiAsns,ConsolidateAsnLinesAtReceiving,UsingFaiFunctionality," +
                                 "PutStockOnHoldDuringFAI,UsingCustoms,AllowPartialRelease," +
                                 "AllowPartialReconcile,PrintFloorSheetsAtInbound,UsingReceivingPOs," +
                                 "PutStockOnHoldAfterInbound,ManuallyReleaseStockAfterInbound," +
                                 "PostReconcileStockStatusID,PostReconcileInboundStatusID," +
                                 "PostReleaseStockStatusID,PostReleaseInboundStatusID,UseCustomerPackageLabelAtInbound"
          chrRequiredFieldList = "DefaultMaxNumUsersPerTask,AllowUnderReceiving"
          chrExtraFieldList    = ""
          chrValidateFieldList = "DefaultMaxNumUsersPerTask:INTEGER,PostReconcileStockStatusID:INTEGER," + 
                                 "PostReconcileInboundStatusID:INTEGER,PostReleaseStockStatusID:INTEGER," +
                                 "PostReleaseInboundStatusID:INTEGER".
   
   InboundConfigDetailsForm = NEW dataForm("inboundconfig_details_form").
   InboundConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   InboundConfigDetailsForm:FormAction = "dbInboundConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   InboundConfigDetailsForm:FormWidth   = 700.
   InboundConfigDetailsForm:FormHeight  = 500.
   InboundConfigDetailsForm:FormTitle   = "Inbound Config Details".
   InboundConfigDetailsForm:FormType    = "xl_large".
   
   /* Column Layout */
   InboundConfigDetailsForm:insertPaddingColumn(30).
   InboundConfigDetailsForm:insertColumn(185).
   
   /* Fields */
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("Inbound Config ID")).
   InboundConfigDetailsForm:insertTextField("InboundConfigID", "", 190, TRUE).  
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("DefaultMaxNumUsersPerTask")).
   InboundConfigDetailsForm:insertTextField("DefaultMaxNumUsersPerTask", "", 190, TRUE).
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("AllowUnderReceiving")).
   InboundConfigDetailsForm:insertComboField("AllowUnderReceiving", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("AllowUnderReceiving", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("AllowUnderReceiving", "no",  "No").

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("AllowOverReceiving")).
   InboundConfigDetailsForm:insertComboField("AllowOverReceiving", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("AllowOverReceiving", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("AllowOverReceiving", "no",  "No").
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("UsingMultiAsns")).
   InboundConfigDetailsForm:insertComboField("UsingMultiAsns", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("UsingMultiAsns", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("UsingMultiAsns", "no",  "No").

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("ConsolidateAsnLinesAtReceiving")).
   InboundConfigDetailsForm:insertComboField("ConsolidateAsnLinesAtReceiving", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("ConsolidateAsnLinesAtReceiving", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("ConsolidateAsnLinesAtReceiving", "no",  "No").

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("UsingFaiFunctionality")).
   InboundConfigDetailsForm:insertComboField("UsingFaiFunctionality", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("UsingFaiFunctionality", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("UsingFaiFunctionality", "no",  "No").

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("PutStockOnHoldDuringFAI")).
   InboundConfigDetailsForm:insertComboField("PutStockOnHoldDuringFAI", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("PutStockOnHoldDuringFAI", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("PutStockOnHoldDuringFAI", "no",  "No").   

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("UsingCustoms")).
   InboundConfigDetailsForm:insertComboField("UsingCustoms", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("UsingCustoms", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("UsingCustoms", "no",  "No").
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("AllowPartialRelease")).
   InboundConfigDetailsForm:insertComboField("AllowPartialRelease", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("AllowPartialRelease", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("AllowPartialRelease", "no",  "No").
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("AllowPartialReconcile")).
   InboundConfigDetailsForm:insertComboField("AllowPartialReconcile", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("AllowPartialReconcile", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("AllowPartialReconcile", "no",  "No").

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("PrintFloorSheetsAtInbound")).
   InboundConfigDetailsForm:insertComboField("PrintFloorSheetsAtInbound", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("PrintFloorSheetsAtInbound", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("PrintFloorSheetsAtInbound", "no",  "No").
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("UsingReceivingPOs")).
   InboundConfigDetailsForm:insertComboField("UsingReceivingPOs", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("UsingReceivingPOs", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("UsingReceivingPOs", "no",  "No").   

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("PutStockOnHoldAfterInbound")).
   InboundConfigDetailsForm:insertComboField("PutStockOnHoldAfterInbound", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("PutStockOnHoldAfterInbound", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("PutStockOnHoldAfterInbound", "no",  "No").  
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("ManuallyReleaseStockAfterInbound")).
   InboundConfigDetailsForm:insertComboField("ManuallyReleaseStockAfterInbound", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("ManuallyReleaseStockAfterInbound", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("ManuallyReleaseStockAfterInbound", "no",  "No").  

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("UseCustomerPackageLabelAtInbound")).
   InboundConfigDetailsForm:insertComboField("UseCustomerPackageLabelAtInbound", "", 190, TRUE).  
   InboundConfigDetailsForm:insertComboPairs("UseCustomerPackageLabelAtInbound", "yes", "Yes").
   InboundConfigDetailsForm:insertComboPairs("UseCustomerPackageLabelAtInbound", "no",  "No").  

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("PostReconcileStockStatusID")).
   InboundConfigDetailsForm:insertTextField("PostReconcileStockStatusID", "", 190, TRUE).
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("PostReconcileInboundStatusID")).
   InboundConfigDetailsForm:insertTextField("PostReconcileInboundStatusID", "", 190, TRUE).

   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("PostReleaseStockStatusID")).
   InboundConfigDetailsForm:insertTextField("PostReleaseStockStatusID", "", 190, TRUE).
   
   InboundConfigDetailsForm:startRow().
   InboundConfigDetailsForm:insertLabel(fTL("PostReleaseInboundStatusID")).
   InboundConfigDetailsForm:insertTextField("PostReleaseInboundStatusID", "", 190, TRUE).
      
   {webGetOptionalFormFields.i pInboundConfigDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   InboundConfigDetailsForm:insertHiddenField("inboundconfig_browse_scroll", "").
   InboundConfigDetailsForm:insertHiddenField("form_name", "inboundconfig_details_form").
   InboundConfigDetailsForm:insertHiddenField("prog_name", "adInboundConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundConfigDetailsForm}
   
   /* Create Button Bar */
   InboundConfigDetailsButtons = NEW buttonBar().
   InboundConfigDetailsButtons:addButton("inboundconfig_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateInboundConfig('inboundconfig_details_form');").
   InboundConfigDetailsButtons:addButton("inboundconfig_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('inboundconfig_details_form_popup');").
   InboundConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   InboundConfigDetailsForm:FormButtons = InboundConfigDetailsButtons.
   
   InboundConfigDetailsForm:endForm(). 
   
   InboundConfigDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + InboundConfigDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigDetailsFields Procedure 
PROCEDURE pInboundConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      InboundConfigDetailsForm:startRow().
      InboundConfigDetailsForm:insertLabel(fTL("Field Label")).
      InboundConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
    {adInboundConfig_inboundconfig_details_form.i}
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigHistoryBrowse Procedure 
PROCEDURE pInboundConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   InboundConfigHistoryBrowseForm           = NEW dataForm("inboundconfighistory_browse_form").
   InboundConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   InboundConfigHistoryBrowseForm:FormWidth  = 860.
   InboundConfigHistoryBrowseForm:FormHeight = 530.
   InboundConfigHistoryBrowseForm:FormTitle  = fTL("Inbound Config History").
   InboundConfigHistoryBrowseForm:FormType   = "xxl_large".
   InboundConfigHistoryBrowse                = NEW browseTable("inboundconfighistory_browse").
   InboundConfigHistoryBrowse:BrowseWidth    = 840.
   InboundConfigHistoryBrowse:BrowseHeight   = 490.
   
   InboundConfigHistoryBrowse:insertColumn(fTL("Hist Config ID"),              125, "INTEGER",   "LEFT", FALSE).
   InboundConfigHistoryBrowse:insertColumn(fTL("DefaultMaxNumUsersPerTask"),   200, "CHARACTER", "LEFT", FALSE).
   InboundConfigHistoryBrowse:insertColumn(fTL("AllowUnderReceiving"),         200, "CHARACTER", "LEFT", FALSE).
   InboundConfigHistoryBrowse:insertColumn(fTL("User"),                        125, "CHARACTER", "LEFT", FALSE).
   InboundConfigHistoryBrowse:insertColumn(fTL("Created"),                     150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i InboundConfigHistory}
   
   InboundConfigHistoryBrowse:StartBody().
   
   FOR EACH InboundConfigHistory NO-LOCK
      WHERE InboundConfigHistory.InboundConfigID = intSelectedInboundConfig
         BY InboundConfigHistory.InboundConfigHistoryID:

      InboundConfigHistoryBrowse:startRow(InboundConfigHistory.InboundConfigHistoryID, 
                                        "selectInboundConfigHistoryRow(this," + '"' + STRING(InboundConfigHistory.InboundConfigHistoryID) + '"' + ");","").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i InboundConfigHistory}
      
      InboundConfigHistoryBrowse:insertData(InboundConfigHistory.InboundConfigHistoryID, "LEFT").
      InboundConfigHistoryBrowse:insertData((IF AVAILABLE InboundConfigHistory THEN STRING(InboundConfigHistory.DefaultMaxNumUsersPerTask) ELSE ""),"LEFT").
      InboundConfigHistoryBrowse:insertData((IF AVAILABLE InboundConfigHistory THEN STRING(InboundConfigHistory.AllowUnderReceiving, "Yes/No") ELSE ""),"LEFT").
      InboundConfigHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      InboundConfigHistoryBrowse:insertData(fDisplayDate&Time(InboundConfigHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      InboundConfigHistoryBrowse:endRow().
   END. /* FOR EACH InboundConfigHistory */
   
   InboundConfigHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + InboundConfigHistoryBrowse:getErrors().
   InboundConfigHistoryBrowseForm:insertHiddenField("InboundConfigHistoryID", "").
   InboundConfigHistoryBrowseForm:insertHiddenField("popup_inboundconfighistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   InboundConfigHistoryBrowseButtons = NEW buttonBar().
      
   InboundConfigHistoryBrowseButtons:addButton("inboundconfighistory_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewInboundConfigHistoryDetails('inboundconfighistory_details_form');",
                                             "Disabled").
                                             
   InboundConfigHistoryBrowseButtons:addButton("inboundconfighistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('inboundconfighistory_browse_form_popup');").
   
   InboundConfigHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   InboundConfigHistoryBrowseForm:FormBrowse  = InboundConfigHistoryBrowse.
   InboundConfigHistoryBrowseForm:FormButtons = InboundConfigHistoryBrowseButtons.
   InboundConfigHistoryBrowseForm:endForm(). 
   
   InboundConfigHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


