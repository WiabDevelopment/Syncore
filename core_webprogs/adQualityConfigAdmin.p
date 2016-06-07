&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adQualityConfigAdmin.p 

  Description: ad file for the Quality Config Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Shawn Hilts

  Created: 24/11/2014
  
  Revisions:
     13/10/2015 - twierzch - change StatusIDs to dropdowns instead of text boxes

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
DEFINE VARIABLE intSelectedQualityConfig        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrQualityConfigRow             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQualityConfigRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQualityConfigID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                 AS CHARACTER   NO-UNDO.

/* Buffers */
DEFINE BUFFER inProcessStockStatus     FOR StockStatus.
DEFINE BUFFER postProcessStockStatus   FOR StockStatus.
DEFINE BUFFER replacedPartStockStatus  FOR StockStatus.

/* Objects */
DEFINE VARIABLE QualityConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE QualityConfigBrowse                AS browseTable.
DEFINE VARIABLE QualityConfigBrowseButtons         AS buttonBar.
DEFINE VARIABLE QualityConfigDetailsForm           AS dataForm.
DEFINE VARIABLE QualityConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QualityConfigDetailsButtons        AS buttonBar.
DEFINE VARIABLE QualityConfigHistoryBrowseForm     AS dataform. 
DEFINE VARIABLE QualityConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE QualityConfigHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE QualityConfigHistoryBrowseButtons  AS buttonBar.


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
PROCEDURE pQualityConfigHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualityconfighistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualityConfigHistoryID,QualityConfigID,InProcessStockStatusID," +
                                 "PostProcessStockStatusID,AutoReplaceComponents,ReplacedPartStockStatusID," +
                                 "GateUserID"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualityConfigHistoryDetailsForm = NEW dataForm("qualityconfighistory_details_form").
   QualityConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   /*QualityConfigHistoryDetailsForm:FormAction = "dbQualityConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").*/
   
   /* Setup */
   QualityConfigHistoryDetailsForm:FormWidth   = 460.
   QualityConfigHistoryDetailsForm:FormHeight  = 300.
   QualityConfigHistoryDetailsForm:FormTitle   = fTL("QualityConfigHistory Details").
   QualityConfigHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QualityConfigHistoryDetailsForm:insertPaddingColumn(30).
   QualityConfigHistoryDetailsForm:insertColumn(185).
   QualityConfigHistoryDetailsForm:insertColumn(120).
   QualityConfigHistoryDetailsForm:insertColumn(20).
   QualityConfigHistoryDetailsForm:insertColumn(4).
   QualityConfigHistoryDetailsForm:insertColumn(100).
   
   /* Fields */ 
   
   QualityConfigHistoryDetailsForm:startRow().
   QualityConfigHistoryDetailsForm:insertLabel("History ID").
   QualityConfigHistoryDetailsForm:insertTextField("QualityConfigHistoryID", "", 110, TRUE). 
   
   QualityConfigHistoryDetailsForm:startRow().
   QualityConfigHistoryDetailsForm:insertLabel(fTL("QualityConfigID")).
   QualityConfigHistoryDetailsForm:insertTextField("QualityConfigID", "", 190, TRUE).
   
   QualityConfigHistoryDetailsForm:startRow().
   QualityConfigHistoryDetailsForm:insertLabel(fTL("InProcess Stock Status")).
   QualityConfigHistoryDetailsForm:insertComboField("InProcessStockStatusID", "", 190, TRUE).
   FOR EACH StockStatus NO-LOCK:
      QualityConfigHistoryDetailsForm:insertComboPairs("InProcessStockStatusID", 
                                                       STRING(StockStatus.StockStatusID), StockStatus.StatusCode).
   END.
   
   QualityConfigHistoryDetailsForm:startRow().
   QualityConfigHistoryDetailsForm:insertLabel(fTL("PostProcess Stock Status")).
   QualityConfigHistoryDetailsForm:insertComboField("PostProcessStockStatusID", "", 190, TRUE).
   FOR EACH StockStatus NO-LOCK:
      QualityConfigHistoryDetailsForm:insertComboPairs("PostProcessStockStatusID", 
                                                       STRING(StockStatus.StockStatusID), StockStatus.StatusCode).
   END.
   
   QualityConfigHistoryDetailsForm:startRow().
   QualityConfigHistoryDetailsForm:insertLabel(fTL("AutoReplace Components")).
   QualityConfigHistoryDetailsForm:insertComboField("AutoReplaceComponents", "", 190, TRUE).  
   QualityConfigHistoryDetailsForm:insertComboPairs("AutoReplaceComponents", "yes", "Yes").
   QualityConfigHistoryDetailsForm:insertComboPairs("AutoReplaceComponents", "no",  "No").

   QualityConfigHistoryDetailsForm:startRow().
   QualityConfigHistoryDetailsForm:insertLabel(fTL("Replaced Part Stock Status")).
   QualityConfigHistoryDetailsForm:insertComboField("ReplacedPartStockStatusID", "", 190, TRUE).
   FOR EACH StockStatus NO-LOCK:
      QualityConfigHistoryDetailsForm:insertComboPairs("ReplacedPartStockStatusID", 
                                                       STRING(StockStatus.StockStatusID), StockStatus.StatusCode).
   END.
   
   QualityConfigHistoryDetailsForm:startRow().
   QualityConfigHistoryDetailsForm:insertLabel(fTL("GateUser")).
   QualityConfigHistoryDetailsForm:insertComboField("GateUserID", "", 190, TRUE).
   FOR EACH GateUser NO-LOCK:
      QualityConfigHistoryDetailsForm:insertComboPairs("GateUserID", 
                                                       STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   {webGetOptionalFormFields.i pQualityConfigHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   QualityConfigHistoryDetailsForm:insertHiddenField("qualityconfig_browse_scroll", "").
   QualityConfigHistoryDetailsForm:insertHiddenField("form_name", "qualityconfig_details_form").
   QualityConfigHistoryDetailsForm:insertHiddenField("prog_name", "adQualityConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   QualityConfigHistoryDetailsButtons = NEW buttonBar().
   QualityConfigHistoryDetailsButtons:addButton("qualityconfighistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualityconfighistory_details_form_popup');").
   QualityConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualityConfigHistoryDetailsForm:FormButtons = QualityConfigHistoryDetailsButtons.
   
   QualityConfigHistoryDetailsForm:endForm(). 
   
   QualityConfigHistoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QualityConfigHistoryDetailsForm:getErrors().  */

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
   
   ASSIGN chrQualityConfigID = get-value("QualityConfigID")
          intSelectedQualityConfig = INTEGER(chrQualityConfigID)
          chrScrollToQualityConfigRow = STRING(INTEGER(get-value("qualityconfig_browse_scroll"))) + ";".
   
   /* Process URL values */   
   IF chrQualityConfigID <> "" THEN
      chrQualityConfigRow = 'selectQualityConfigRow(document.getElementById("qualityconfig_browse_row_' + chrQualityConfigID + '"),"' 
                                                          + chrQualityConfigID +  '");'.
                                                          
   IF get-value('popup_qualityconfighistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("qualityconfighistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("qualityconfig_browse").scrollTop=' + chrScrollToQualityConfigRow 
                                                           + chrQualityConfigRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Quality Config Admin".
   ThisPage:FrameTitle = "Quality Config Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Quality Config */
   ThisPage:addJavaScript("qualityconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQualityConfigBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pQualityConfigDetails.
   
   RUN pQualityConfigHistoryBrowse.
   
   RUN pQualityConfigHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT QualityConfigBrowseFrame           NO-ERROR.
   DELETE OBJECT QualityConfigBrowse                NO-ERROR.
   DELETE OBJECT QualityConfigBrowseButtons         NO-ERROR.
   DELETE OBJECT QualityConfigDetailsForm           NO-ERROR.
   DELETE OBJECT QualityConfigDetailsButtons        NO-ERROR.
   DELETE OBJECT QualityConfigHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT QualityConfigHistoryDetailsButtons NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigBrowse Procedure 
PROCEDURE pQualityConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "qualityconfig_details_form"}
   
   QualityConfigBrowse = NEW browseTable("qualityconfig_browse").
   QualityConfigBrowse:BrowseWidth  = 965.
   QualityConfigBrowse:BrowseHeight = 455.
   QualityConfigBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the QualityConfig ID as first Column */
   QualityConfigBrowse:insertColumn(fTL("QualityConfig ID"), 130, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualityConfig}
   
   QualityConfigBrowse:insertColumn(fTL("In ProcessStock Status"),   200, "CHARACTER", "LEFT", FALSE).
   QualityConfigBrowse:insertColumn(fTL("Post ProcessStock Status"), 200, "CHARACTER", "LEFT", FALSE).
   QualityConfigBrowse:insertColumn(fTL("AutoReplace Components"),   200, "CHARACTER", "LEFT", FALSE).
   QualityConfigBrowse:insertColumn(fTL("Replaced Part Status"),     200, "CHARACTER", "LEFT", FALSE).
   
   
   /*Body*/
   QualityConfigBrowse:startBody().
   
   FOR EACH QualityConfig NO-LOCK: /*idx=ActiveListingSequence*/
   
      FIND FIRST inProcessStockStatus NO-LOCK
         WHERE inProcessStockStatus.StockStatusID = QualityConfig.InProcessStockStatusID NO-ERROR.
         
      FIND FIRST postProcessStockStatus NO-LOCK
         WHERE postProcessStockStatus.StockStatusID = QualityConfig.PostProcessStockStatusID NO-ERROR.
      
      FIND FIRST replacedPartStockStatus NO-LOCK
         WHERE replacedPartStockStatus.StockStatusID = QualityConfig.ReplacedPartStockStatusID NO-ERROR.
      
      QualityConfigBrowse:startRow(QualityConfig.QualityConfigID, "selectQualityConfigRow(this," + '"' + STRING(QualityConfig.QualityConfigID) + '"' + ");", "").
      QualityConfigBrowse:insertData(QualityConfig.QualityConfigID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QualityConfig}
      
      QualityConfigBrowse:insertData(IF AVAILABLE inProcessStockStatus THEN inProcessStockStatus.StatusCode ELSE "","LEFT").
      QualityConfigBrowse:insertData(IF AVAILABLE postProcessStockStatus THEN postProcessStockStatus.StatusCode ELSE "","LEFT").
      QualityConfigBrowse:insertData(STRING(QualityConfig.AutoReplaceComponents, "Yes/No"),"LEFT").
      QualityConfigBrowse:insertData(IF AVAILABLE replacedPartStockStatus THEN replacedPartStockStatus.StatusCode ELSE "","LEFT").
      
      /* Add hidden fields */
      QualityConfigBrowse:insertHiddenData("QualityConfigVersionID",QualityConfig.VersionID).
      
      QualityConfigBrowse:endRow().
      
   END. /*FOR EACH QualityConfig NO-LOCK */
   
   QualityConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualityConfigBrowse:getErrors().
   
   /* Create a new frame */
   QualityConfigBrowseFrame = NEW pageFrame().
   QualityConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QualityConfigBrowseFrame:FormAction="dbQualityConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QualityConfigBrowseFrame:formOpen("qualityconfig_browse_form").
   
   /* Start the Frame Header */
   QualityConfigBrowseFrame:insertSpacer(5).
   QualityConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QualityConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QualityConfigBrowseFrame:frameClose().
   QualityConfigBrowseFrame:insertSpacer(10).
   
   QualityConfigBrowseFrame:insertHiddenField("qualityconfig_browse_scroll","").
   QualityConfigBrowseFrame:insertHiddenField("QualityConfigID","").
   QualityConfigBrowseFrame:insertHiddenField("QualityConfigVersionID","").
   QualityConfigBrowseFrame:insertHiddenField("form_name","qualityconfig_browse_form").
   QualityConfigBrowseFrame:insertHiddenField("popup_qualityconfighistory_browse","").
   QualityConfigBrowseFrame:insertHiddenField("prog_name","adQualityConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityConfigBrowseFrame}
   
   QualityConfigBrowseFrame:formClose().
   
   /* Create Button Bar */
   QualityConfigBrowseButtons = NEW buttonBar().
   QualityConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   QualityConfigBrowseButtons:addButton("qualityconfig_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewQualityConfigDetails('qualityconfig_details_form');",
                                         (IF intSelectedQualityConfig > 0 THEN "" ELSE "Disabled")).
   
   QualityConfigBrowseButtons:addButton("qualityconfig_browse_form_btn_create",
                                         fTL("Create"),
                                         "createQualityConfig('qualityconfig_details_form');",
                                         IF CAN-FIND(FIRST QualityConfig) THEN "Disabled" ELSE "").
   
   QualityConfigBrowseButtons:addButton("qualityconfig_browse_form_btn_history",
                                         fTL("History"),
                                         "viewQualityConfigHistory('qualityconfig_browse_form');",
                                         (IF intSelectedQualityConfig > 0 THEN "" ELSE "Disabled")).
   
   QualityConfigBrowseButtons:closeBar().  
   QualityConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigDetails Procedure 
PROCEDURE pQualityConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualityconfig_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualityConfigID,InProcessStockStatusID," +
                                 "PostProcessStockStatusID,AutoReplaceComponents,ReplacedPartStockStatusID"                          
          chrEditFieldList     = "InProcessStockStatusID,PostProcessStockStatusID,AutoReplaceComponents,ReplacedPartStockStatusID"
          chrNewFieldList      = "InProcessStockStatusID,PostProcessStockStatusID,AutoReplaceComponents,ReplacedPartStockStatusID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualityConfigDetailsForm = NEW dataForm("qualityconfig_details_form").
   QualityConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QualityConfigDetailsForm:FormAction = "dbQualityConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualityConfigDetailsForm:FormWidth   = 460.
   QualityConfigDetailsForm:FormHeight  = 200.
   QualityConfigDetailsForm:FormTitle   = "Quality Config Details".
   QualityConfigDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QualityConfigDetailsForm:insertPaddingColumn(30).
   QualityConfigDetailsForm:insertColumn(185).
   
   /* Fields */
   QualityConfigDetailsForm:startRow().
   QualityConfigDetailsForm:insertLabel(fTL("Quality Config ID")).
   QualityConfigDetailsForm:insertTextField("QualityConfigID", "", 190, TRUE).  
   
   QualityConfigDetailsForm:startRow().
   QualityConfigDetailsForm:insertLabel(fTL("InProcess Stock Status")).
   QualityConfigDetailsForm:insertComboField("InProcessStockStatusID", "", 190, TRUE).
   FOR EACH StockStatus NO-LOCK:
      QualityConfigDetailsForm:insertComboPairs("InProcessStockStatusID", 
                                                       STRING(StockStatus.StockStatusID), StockStatus.StatusCode).
   END.
   
   QualityConfigDetailsForm:startRow().
   QualityConfigDetailsForm:insertLabel(fTL("Post Process Stock Status")).
   QualityConfigDetailsForm:insertComboField("PostProcessStockStatusID", "", 190, TRUE).
   FOR EACH StockStatus NO-LOCK:
      QualityConfigDetailsForm:insertComboPairs("PostProcessStockStatusID", 
                                                       STRING(StockStatus.StockStatusID), StockStatus.StatusCode).
   END.
   
   QualityConfigDetailsForm:startRow().
   QualityConfigDetailsForm:insertLabel(fTL("AutoReplace Components")).
   QualityConfigDetailsForm:insertComboField("AutoReplaceComponents", "", 190, TRUE).  
   QualityConfigDetailsForm:insertComboPairs("AutoReplaceComponents", "yes", "Yes").
   QualityConfigDetailsForm:insertComboPairs("AutoReplaceComponents", "no",  "No").
   
   QualityConfigDetailsForm:startRow().
   QualityConfigDetailsForm:insertLabel(fTL("Replaced Part Stock Status")).
   QualityConfigDetailsForm:insertComboField("ReplacedPartStockStatusID", "", 190, TRUE).
   FOR EACH StockStatus NO-LOCK:
      QualityConfigDetailsForm:insertComboPairs("ReplacedPartStockStatusID", 
                                                       STRING(StockStatus.StockStatusID), StockStatus.StatusCode).
   END.

   {webGetOptionalFormFields.i pQualityConfigDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   QualityConfigDetailsForm:insertHiddenField("qualityconfig_browse_scroll", "").
   QualityConfigDetailsForm:insertHiddenField("form_name", "qualityconfig_details_form").
   QualityConfigDetailsForm:insertHiddenField("prog_name", "adQualityConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityConfigDetailsForm}
   
   /* Create Button Bar */
   QualityConfigDetailsButtons = NEW buttonBar().
   QualityConfigDetailsButtons:addButton("qualityconfig_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateQualityConfig('qualityconfig_details_form');").
   QualityConfigDetailsButtons:addButton("qualityconfig_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualityconfig_details_form_popup');").
   QualityConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualityConfigDetailsForm:FormButtons = QualityConfigDetailsButtons.
   
   QualityConfigDetailsForm:endForm(). 
   
   QualityConfigDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QualityConfigDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigDetailsFields Procedure 
PROCEDURE pQualityConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      QualityConfigDetailsForm:startRow().
      QualityConfigDetailsForm:insertLabel(fTL("Field Label")).
      QualityConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*    {adQualityConfig_qualityconfig_details_form.i}*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundConfigHistoryBrowse Procedure 
PROCEDURE pQualityConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   QualityConfigHistoryBrowseForm           = NEW dataForm("qualityconfighistory_browse_form").
   QualityConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QualityConfigHistoryBrowseForm:FormWidth  = 860.
   QualityConfigHistoryBrowseForm:FormHeight = 530.
   QualityConfigHistoryBrowseForm:FormTitle  = fTL("Quality Config History").
   QualityConfigHistoryBrowseForm:FormType   = "xxl_large".
   QualityConfigHistoryBrowse                = NEW browseTable("qualityconfighistory_browse").
   QualityConfigHistoryBrowse:BrowseWidth    = 840.
   QualityConfigHistoryBrowse:BrowseHeight   = 490.
   
   QualityConfigHistoryBrowse:insertColumn(fTL("Hist Config ID"),              125, "INTEGER",   "LEFT", FALSE).
   QualityConfigHistoryBrowse:insertColumn(fTL("InProcess Stock Status"),      200, "CHARACTER", "LEFT", FALSE).
   QualityConfigHistoryBrowse:insertColumn(fTL("Post Process Stock Status"),   200, "CHARACTER", "LEFT", FALSE).
   QualityConfigHistoryBrowse:insertColumn(fTL("User"),                        125, "CHARACTER", "LEFT", FALSE).
   QualityConfigHistoryBrowse:insertColumn(fTL("Created"),                     150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualityConfigHistory}
   
   QualityConfigHistoryBrowse:StartBody().
   
   FOR EACH QualityConfigHistory NO-LOCK
      WHERE QualityConfigHistory.QualityConfigID = intSelectedQualityConfig
         BY QualityConfigHistory.QualityConfigHistoryID:
            
      FIND FIRST inProcessStockStatus NO-LOCK
         WHERE inProcessStockStatus.StockStatusID = QualityConfigHistory.InProcessStockStatusID NO-ERROR.
         
      FIND FIRST postProcessStockStatus NO-LOCK
         WHERE postProcessStockStatus.StockStatusID = QualityConfigHistory.PostProcessStockStatusID NO-ERROR.
      
      FIND FIRST replacedPartStockStatus NO-LOCK
         WHERE replacedPartStockStatus.StockStatusID = QualityConfigHistory.ReplacedPartStockStatusID NO-ERROR.

      QualityConfigHistoryBrowse:startRow(QualityConfigHistory.QualityConfigHistoryID, 
                                        "selectQualityConfigHistoryRow(this," + '"' + STRING(QualityConfigHistory.QualityConfigHistoryID) + '"' + ");","").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i QualityConfigHistory}
      
      QualityConfigHistoryBrowse:insertData(QualityConfigHistory.QualityConfigHistoryID, "LEFT").
      QualityConfigHistoryBrowse:insertData(IF AVAILABLE inProcessStockStatus THEN inProcessStockStatus.StatusCode ELSE "","LEFT").
      QualityConfigHistoryBrowse:insertData(IF AVAILABLE postProcessStockStatus THEN postProcessStockStatus.StatusCode ELSE "","LEFT").
      QualityConfigHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      QualityConfigHistoryBrowse:insertData(fDisplayDate&Time(QualityConfigHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      QualityConfigHistoryBrowse:endRow().
   END. /* FOR EACH QualityConfigHistory */
   
   QualityConfigHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualityConfigHistoryBrowse:getErrors().
   QualityConfigHistoryBrowseForm:insertHiddenField("QualityConfigHistoryID", "").
   QualityConfigHistoryBrowseForm:insertHiddenField("popup_qualityconfighistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   QualityConfigHistoryBrowseButtons = NEW buttonBar().
      
   QualityConfigHistoryBrowseButtons:addButton("qualityconfighistory_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQualityConfigHistoryDetails('qualityconfighistory_details_form');",
                                             "Disabled").
                                             
   QualityConfigHistoryBrowseButtons:addButton("qualityconfighistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('qualityconfighistory_browse_form_popup');").
   
   QualityConfigHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualityConfigHistoryBrowseForm:FormBrowse  = QualityConfigHistoryBrowse.
   QualityConfigHistoryBrowseForm:FormButtons = QualityConfigHistoryBrowseButtons.
   QualityConfigHistoryBrowseForm:endForm(). 
   
   QualityConfigHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


