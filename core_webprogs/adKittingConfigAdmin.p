&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adKittingConfigAdmin.p 

  Description: ad file for the Kitting Config Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Todd Wierzchowski

  Created: 30/10/2014

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
DEFINE VARIABLE intSelectedKittingConfig           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedKittingConfigHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrKittingConfigHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrKittingConfigRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToKittingConfigRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToKittingConfigHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrKittingConfigID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intIsRecord                        AS INTEGER     NO-UNDO.

/* Objects */
DEFINE VARIABLE KittingConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE KittingConfigBrowse                AS browseTable.
DEFINE VARIABLE KittingConfigBrowseButtons         AS buttonBar.
DEFINE VARIABLE KittingConfigDetailsForm           AS dataForm.
DEFINE VARIABLE KittingConfigDetailsButtons        AS buttonBar.
DEFINE VARIABLE KittingConfigHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE KittingConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE KittingConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE KittingConfigHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE KittingConfigHistoryButtons        AS buttonBar.

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
   
   ASSIGN chrKittingConfigID          = get-value("KittingConfigID")
          intSelectedKittingConfig    = INTEGER(chrKittingConfigID)
          chrScrollToKittingConfigRow = STRING(INTEGER(get-value("kittingconfig_browse_scroll"))) + ";"
          /*History details button*/
          chrKittingConfigHistoryID          = get-value("KittingConfigHistoryID")
          intSelectedKittingConfigHistory    = INTEGER(chrKittingConfigHistoryID)
          chrScrollToKittingConfigHistoryRow = STRING(INTEGER(get-value("kittingconfigHistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrKittingConfigID <> "" THEN
      chrKittingConfigRow = 'selectKittingConfigRow(document.getElementById("kittingconfig_browse_row_' + chrKittingConfigID + '"),"' 
                                                          + chrKittingConfigID +  '");'.
                                                          
   IF get-value('popup_kittingconfighistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("kittingconfighistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("kittingconfig_browse").scrollTop=' + chrScrollToKittingConfigRow 
                                                           + chrKittingConfigRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Kitting Config Admin".
   ThisPage:FrameTitle = "Kitting Config Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("kittingconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pKittingConfigBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pKittingConfigDetails.
   
   RUN pKittingConfigHistoryBrowse.
   
   RUN pKittingConfigHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT KittingConfigBrowseFrame        NO-ERROR.
   DELETE OBJECT KittingConfigBrowse             NO-ERROR.
   DELETE OBJECT KittingConfigBrowseButtons      NO-ERROR.
   DELETE OBJECT KittingConfigDetailsForm        NO-ERROR.
   DELETE OBJECT KittingConfigDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pKittingConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "kittingconfig_details_form"}
   
   KittingConfigBrowse = NEW browseTable("kittingconfig_browse").
   KittingConfigBrowse:BrowseWidth  = 965.
   KittingConfigBrowse:BrowseHeight = 455.
   KittingConfigBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   KittingConfigBrowse:insertColumn(fTL("ID"), 70, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i KittingConfig}
   
   KittingConfigBrowse:insertColumn(fTL("PostKit WorkOrder Status"), 140, "CHARACTER", "LEFT", FALSE). 
   KittingConfigBrowse:insertColumn(fTL("PostKit Stock Status"),     140, "CHARACTER", "LEFT", FALSE).   
   KittingConfigBrowse:insertColumn(fTL("Scan Component"),           115, "CHARACTER", "LEFT", FALSE). 
   KittingConfigBrowse:insertColumn(fTL("Scan FG Part"),              95, "CHARACTER", "LEFT", FALSE).
   KittingConfigBrowse:insertColumn(fTL("Mixed Pallet"),             100, "CHARACTER", "LEFT", FALSE).
   KittingConfigBrowse:insertColumn(fTL("Multi Kit Line"),           100, "CHARACTER", "LEFT", FALSE).   
   KittingConfigBrowse:insertColumn(fTL("BackFlush"),                 80, "CHARACTER", "LEFT", FALSE).   
   KittingConfigBrowse:insertColumn(fTL("Rounding"),                  80, "CHARACTER", "LEFT", FALSE).
            
   /*Body*/
   KittingConfigBrowse:startBody().
   
   FOR EACH KittingConfig NO-LOCK: /*idx=ActiveListingSequence*/
      
      FIND FIRST WorkOrderStatus NO-LOCK
         WHERE WorkOrderStatus.WorkOrderStatusID = KittingConfig.PostKittingWorkOrderStatusID NO-ERROR.
         
      FIND FIRST StockStatus NO-LOCK
         WHERE StockStatus.StockStatusID = KittingConfig.PostKittingStockStatusID NO-ERROR.
      
      KittingConfigBrowse:startRow(KittingConfig.KittingConfigID, "selectKittingConfigRow(this," + '"' + STRING(KittingConfig.KittingConfigID) + '"' + ");", "").
      KittingConfigBrowse:insertData(KittingConfig.KittingConfigID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i KittingConfig}
      
      KittingConfigBrowse:insertData(IF AVAILABLE WorkOrderStatus THEN WorkOrderStatus.StatusName ELSE "","LEFT").
      KittingConfigBrowse:insertData(IF AVAILABLE StockStatus     THEN StockStatus.StatusCode ELSE "","LEFT").
      KittingConfigBrowse:insertData(STRING(KittingConfig.ForceScanningOfComponentParts, "Yes/No"), "LEFT").
      KittingConfigBrowse:insertData(STRING(KittingConfig.ForceScanningOfFgPart, "Yes/No"), "LEFT"). 
      KittingConfigBrowse:insertData(STRING(KittingConfig.AllowMixedWorkOrdersOnOnePallet, "Yes/No"), "LEFT").
      KittingConfigBrowse:insertData(STRING(KittingConfig.AllowMultipleKitLinePerWorkOrder, "Yes/No"), "LEFT").      
      KittingConfigBrowse:insertData(STRING(KittingConfig.BackflushPerFullFGStockEntity, "Yes/No"), "LEFT").      
      KittingConfigBrowse:insertData(STRING(KittingConfig.MaxVarianceForBomQtyRounding, "9.9999"), "RIGHT").
      
      /* Add hidden fields */
      KittingConfigBrowse:insertHiddenData("KittingConfigVersionID", KittingConfig.VersionID).
      
      KittingConfigBrowse:endRow().
      
   END. /*FOR EACH KittingConfig NO-LOCK */
   
   KittingConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + KittingConfigBrowse:getErrors().
   
   /* Create a new frame */
   KittingConfigBrowseFrame = NEW pageFrame().
   KittingConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   KittingConfigBrowseFrame:FormAction="dbKittingConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   KittingConfigBrowseFrame:formOpen("kittingconfig_browse_form").
   
   /* Start the Frame Header */
   KittingConfigBrowseFrame:insertSpacer(5).
   KittingConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   KittingConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   KittingConfigBrowseFrame:frameClose().
   KittingConfigBrowseFrame:insertSpacer(10).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingConfigBrowseFrame}

   KittingConfigBrowseFrame:insertHiddenField("form_name", "kittingconfig_browse_form").   
   KittingConfigBrowseFrame:insertHiddenField("prog_name", "adKittingConfigAdmin.p").
   KittingConfigBrowseFrame:insertHiddenField("kittingconfig_browse_scroll", "").
   KittingConfigBrowseFrame:insertHiddenField("KittingConfigID", "").
   KittingConfigBrowseFrame:insertHiddenField("KittingConfigVersionID", "").
   KittingConfigBrowseFrame:insertHiddenField("popup_kittingconfighistory_browse", "").
     
   KittingConfigBrowseFrame:formClose().
   
   /* Create Button Bar */
   KittingConfigBrowseButtons = NEW buttonBar().
   KittingConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   FOR EACH KittingConfig NO-LOCK:
      intIsRecord = KittingConfig.KittingConfigID.
   END.
   
   KittingConfigBrowseButtons:addButton("kittingconfig_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewKittingConfigDetails('kittingconfig_details_form');",
                                         "Disabled").
   
   KittingConfigBrowseButtons:addButton("kittingconfig_browse_form_btn_create",
                                         fTL("Create"),
                                         "createKittingConfig('kittingconfig_details_form');",
                                         (IF intIsRecord < 1 THEN "" ELSE "Disabled")).
   
   KittingConfigBrowseButtons:addButton("kittingconfig_browse_form_btn_history",
                                         fTL("History"),
                                         "viewKittingConfigHistory('kittingconfig_browse_form');",
                                         "Disabled").
   
   KittingConfigBrowseButtons:closeBar().  
   KittingConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pKittingConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "kittingconfig_details_form"}
   /*FINISH THIS AND CREATE MONDAY (look at todd chat)*/
   ASSIGN chrDisplayFieldList  = "KittingConfigID,PostKittingWorkOrderStatusID,PostKittingStockStatusID,MaxWeightPercentageVariation," 
                                    + "ForceScanningOfComponentParts,ForceScanningOfFgPart,BackflushPerFullFGStockEntity," 
                                    + "AllowMixedWorkOrdersOnOnePallet,MaxVarianceForBomQtyRounding,AllowMultipleKitLinePerWorkOrder,"
                                    + "AllowMultiUserPalletBuild,PostPalletBuildLocationID,PostPalletBuildStockStatusID,TypicalWorkOrderQty"
          chrEditFieldList     = "PostKittingWorkOrderStatusID,PostKittingStockStatusID,MaxWeightPercentageVariation," 
                                    + "ForceScanningOfComponentParts,ForceScanningOfFgPart,BackflushPerFullFGStockEntity," 
                                    + "AllowMixedWorkOrdersOnOnePallet,AllowMultipleKitLinePerWorkOrder,"
                                    + "AllowMultiUserPalletBuild,PostPalletBuildLocationID,PostPalletBuildStockStatusID,TypicalWorkOrderQty"
          chrNewFieldList      = "PostKittingWorkOrderStatusID,PostKittingStockStatusID,MaxWeightPercentageVariation," 
                                    + "ForceScanningOfComponentParts,ForceScanningOfFgPart,BackflushPerFullFGStockEntity," 
                                    + "AllowMixedWorkOrdersOnOnePallet,MaxVarianceForBomQtyRounding,AllowMultipleKitLinePerWorkOrder,"
                                    + "AllowMultiUserPalletBuild,PostPalletBuildLocationID,PostPalletBuildStockStatusID,TypicalWorkOrderQty"
          chrRequiredFieldList = "PostKittingWorkOrderStatusID,PostKittingStockStatusID,MaxWeightPercentageVariation," 
                                    + "ForceScanningOfComponentParts,ForceScanningOfFgPart,BackflushPerFullFGStockEntity"                                    
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   KittingConfigDetailsForm = NEW dataForm("kittingconfig_details_form").
   KittingConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   KittingConfigDetailsForm:FormAction = "dbKittingConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   KittingConfigDetailsForm:FormWidth   = 580.
   KittingConfigDetailsForm:FormHeight  = 420.
   KittingConfigDetailsForm:FormTitle   = "Kitting Config Details".
   KittingConfigDetailsForm:FormType    = "large".
   
   /* Column Layout */
   KittingConfigDetailsForm:insertPaddingColumn(10).
   KittingConfigDetailsForm:insertColumn(210).
   
   /* Fields */
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Kitting Config ID")).
   KittingConfigDetailsForm:insertTextField("KittingConfigID", "", 190, TRUE).  
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Post Kitting WorkOrder Status")).
   KittingConfigDetailsForm:insertComboField("PostKittingWorkOrderStatusID", "", 190, TRUE).  
   
   /*May need to change*/
   FOR EACH WorkOrderStatus NO-LOCK
      WHERE WorkOrderStatus.Active:
      KittingConfigDetailsForm:insertComboPairs("PostKittingWorkOrderStatusID", STRING(WorkOrderStatus.WorkOrderStatusID), WorkOrderStatus.StatusName).
   END.
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Post Kitting Stock Status")).
   KittingConfigDetailsForm:insertComboField("PostKittingStockStatusID", "", 190, TRUE).  

   FOR EACH StockStatus NO-LOCK
      WHERE StockStatus.Active:
      KittingConfigDetailsForm:insertComboPairs("PostKittingStockStatusID", STRING(StockStatus.StockStatusID), StockStatus.StatusName).
   END.

   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Max Weight % Variation")).
   KittingConfigDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 190, TRUE).

   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Max Var for BOM Qty Rounding")).
   KittingConfigDetailsForm:insertTextField("MaxVarianceForBomQtyRounding", "", 190, TRUE).

   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Force Scan Component Parts")).
   KittingConfigDetailsForm:insertComboField("ForceScanningOfComponentParts", "", 190, TRUE).
   KittingConfigDetailsForm:insertComboPairs("ForceScanningOfComponentParts", "yes", "Yes").
   KittingConfigDetailsForm:insertComboPairs("ForceScanningOfComponentParts", "no",  "No").
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Force Scanning Of FgPart")).
   KittingConfigDetailsForm:insertComboField("ForceScanningOfFgPart", "", 190, TRUE).
   KittingConfigDetailsForm:insertComboPairs("ForceScanningOfFgPart", "yes", "Yes").
   KittingConfigDetailsForm:insertComboPairs("ForceScanningOfFgPart", "no",  "No").
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("BackflushPerFullFGStockEntity")).
   KittingConfigDetailsForm:insertComboField("BackflushPerFullFGStockEntity", "", 190, TRUE).
   KittingConfigDetailsForm:insertComboPairs("BackflushPerFullFGStockEntity", "yes", "Yes").
   KittingConfigDetailsForm:insertComboPairs("BackflushPerFullFGStockEntity", "no",  "No").
  
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("AllowMultipleKitLinePerWorkOrder")).
   KittingConfigDetailsForm:insertComboField("AllowMultipleKitLinePerWorkOrder", "", 190, TRUE).
   KittingConfigDetailsForm:insertComboPairs("AllowMultipleKitLinePerWorkOrder", "yes", "Yes").
   KittingConfigDetailsForm:insertComboPairs("AllowMultipleKitLinePerWorkOrder", "no",  "No").
    
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("AllowMixedWorkOrdersOnOnePallet")).
   KittingConfigDetailsForm:insertComboField("AllowMixedWorkOrdersOnOnePallet", "", 190, TRUE).
   KittingConfigDetailsForm:insertComboPairs("AllowMixedWorkOrdersOnOnePallet", "yes", "Yes").
   KittingConfigDetailsForm:insertComboPairs("AllowMixedWorkOrdersOnOnePallet", "no",  "No").
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("AllowMultiUserPalletBuild")).
   KittingConfigDetailsForm:insertComboField("AllowMultiUserPalletBuild", "", 190, TRUE).
   KittingConfigDetailsForm:insertComboPairs("AllowMultiUserPalletBuild", "yes", "Yes").
   KittingConfigDetailsForm:insertComboPairs("AllowMultiUserPalletBuild", "no",  "No").
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Post PalletBuild StockStatus")).
   KittingConfigDetailsForm:insertComboField("PostPalletBuildStockStatusID", "", 190, TRUE).  

   FOR EACH StockStatus NO-LOCK
      WHERE StockStatus.Active:
      KittingConfigDetailsForm:insertComboPairs("PostPalletBuildStockStatusID", STRING(StockStatus.StockStatusID), StockStatus.StatusName).
   END.
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Post PalletBuild Location")).
   KittingConfigDetailsForm:insertComboField("PostPalletBuildLocationID", "", 190, TRUE).  

   FOR EACH Location NO-LOCK
      WHERE Location.Active:
      KittingConfigDetailsForm:insertComboPairs("PostPalletBuildLocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingConfigDetailsForm:startRow().
   KittingConfigDetailsForm:insertLabel(fTL("Typical WorkOrder Qty")).
   KittingConfigDetailsForm:insertTextField("TypicalWorkOrderQty", "", 190, TRUE).

   {webGetOptionalFormFields.i pKittingConfigDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   KittingConfigDetailsForm:insertHiddenField("kittingconfig_browse_scroll", "").
   KittingConfigDetailsForm:insertHiddenField("form_name", "kittingconfig_details_form").
   KittingConfigDetailsForm:insertHiddenField("prog_name", "adKittingConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingConfigDetailsForm}
   
   /* Create Button Bar */
   KittingConfigDetailsButtons = NEW buttonBar().
   KittingConfigDetailsButtons:addButton("kittingconfig_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateKittingConfig('kittingconfig_details_form');").
   KittingConfigDetailsButtons:addButton("kittingconfig_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "cancelUpdate('UserCancelled','process_mode'); disablePopup('kittingconfig_details_form_popup');").
   KittingConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingConfigDetailsForm:FormButtons = KittingConfigDetailsButtons.
   
   KittingConfigDetailsForm:endForm(). 
   
   KittingConfigDetailsForm:displayForm(). 
     
END PROCEDURE.


PROCEDURE pKittingConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      KittingConfigDetailsForm:startRow().
      KittingConfigDetailsForm:insertLabel(fTL("Field Label")).
      KittingConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
    {adKittingConfig_kittingconfig_details_form.i}
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pKittingConfigHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "kittingconfighistory_details_form"}
   ASSIGN chrDisplayFieldList  = "KittingConfigHistoryID,KittingConfigID,PostKittingWorkOrderStatusID,PostKittingStockStatusID," +
                                  "MaxWeightPercentageVariation,ForceScanningOfComponentParts,ForceScanningOfFgPart,OperationTypeID," +
                                  "BackflushPerFullFGStockEntity,AllowMixedWorkOrdersOnOnePallet,AllowMultipleKitLinePerWorkOrder,MaxVarianceForBomQtyRounding," +
                                  "AllowMultiUserPalletBuild,PostPalletBuildLocationID,PostPalletBuildStockStatusID,TypicalWorkOrderQty"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   KittingConfigHistoryDetailsForm = NEW dataForm("kittingconfighistory_details_form").
   KittingConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   KittingConfigHistoryDetailsForm:FormAction = "dbKittingConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   KittingConfigHistoryDetailsForm:FormWidth   = 580.
   KittingConfigHistoryDetailsForm:FormHeight  = 420.
   KittingConfigHistoryDetailsForm:FormTitle   = "Kitting Config History Details".
   KittingConfigHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   KittingConfigHistoryDetailsForm:insertPaddingColumn(10).
   KittingConfigHistoryDetailsForm:insertColumn(210).
   
   /* Fields */
    KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Kitting Config History ID")).
   KittingConfigHistoryDetailsForm:insertTextField("KittingConfigHistoryID", "", 190, TRUE).
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Kitting Config ID")).
   KittingConfigHistoryDetailsForm:insertTextField("KittingConfigID", "", 190, TRUE). 

   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Operation Type")).   
   KittingConfigHistoryDetailsForm:insertComboField("OperationTypeID", "", 190, TRUE).
   FOR EACH OperationType NO-LOCK:
         
      KittingConfigHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END.    

   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Post Kitting WorkOrder Status")).
   KittingConfigHistoryDetailsForm:insertComboField("PostKittingWorkOrderStatusID", "", 190, TRUE).  
   
   /*May need to change*/
   FOR EACH WorkOrderStatus NO-LOCK:
      KittingConfigHistoryDetailsForm:insertComboPairs("PostKittingWorkOrderStatusID", STRING(WorkOrderStatus.WorkOrderStatusID), WorkOrderStatus.StatusName).
   END.
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Post Kitting Stock Status")).
   KittingConfigHistoryDetailsForm:insertComboField("PostKittingStockStatusID", "", 190, TRUE).  

   FOR EACH StockStatus NO-LOCK:
      KittingConfigHistoryDetailsForm:insertComboPairs("PostKittingStockStatusID", STRING(StockStatus.StockStatusID), StockStatus.StatusName).
   END.

   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Max Weight % Variation")).
   KittingConfigHistoryDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 190, TRUE).

   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Force Scan Component Parts")).
   KittingConfigHistoryDetailsForm:insertComboField("ForceScanningOfComponentParts", "", 190, TRUE).
   KittingConfigHistoryDetailsForm:insertComboPairs("ForceScanningOfComponentParts", "yes", "Yes").
   KittingConfigHistoryDetailsForm:insertComboPairs("ForceScanningOfComponentParts", "no",  "No").
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Force Scanning Of FgPart")).
   KittingConfigHistoryDetailsForm:insertComboField("ForceScanningOfFgPart", "", 190, TRUE).
   KittingConfigHistoryDetailsForm:insertComboPairs("ForceScanningOfFgPart", "yes", "Yes").
   KittingConfigHistoryDetailsForm:insertComboPairs("ForceScanningOfFgPart", "no",  "No").
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("BackflushPerFullFGStockEntity")).
   KittingConfigHistoryDetailsForm:insertComboField("BackflushPerFullFGStockEntity", "", 190, TRUE).
   KittingConfigHistoryDetailsForm:insertComboPairs("BackflushPerFullFGStockEntity", "yes", "Yes").
   KittingConfigHistoryDetailsForm:insertComboPairs("BackflushPerFullFGStockEntity", "no",  "No").
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("AllowMixedWorkOrdersOnOnePallet")).
   KittingConfigHistoryDetailsForm:insertComboField("AllowMixedWorkOrdersOnOnePallet", "", 190, TRUE).
   KittingConfigHistoryDetailsForm:insertComboPairs("AllowMixedWorkOrdersOnOnePallet", "yes", "Yes").
   KittingConfigHistoryDetailsForm:insertComboPairs("AllowMixedWorkOrdersOnOnePallet", "no",  "No").
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("AllowMultipleKitLinePerWorkOrder")).
   KittingConfigHistoryDetailsForm:insertComboField("AllowMultipleKitLinePerWorkOrder", "", 190, TRUE).
   KittingConfigHistoryDetailsForm:insertComboPairs("AllowMultipleKitLinePerWorkOrder", "yes", "Yes").
   KittingConfigHistoryDetailsForm:insertComboPairs("AllowMultipleKitLinePerWorkOrder", "no",  "No").
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Max Var for BOM Qty Rounding")).
   KittingConfigHistoryDetailsForm:insertTextField("MaxVarianceForBomQtyRounding", "", 190, TRUE).
   
   /* Add new fields for pallet build */
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("AllowMultiUserPalletBuild")).
   KittingConfigHistoryDetailsForm:insertComboField("AllowMultiUserPalletBuild", "", 190, TRUE).
   KittingConfigHistoryDetailsForm:insertComboPairs("AllowMultiUserPalletBuild", "yes", "Yes").
   KittingConfigHistoryDetailsForm:insertComboPairs("AllowMultiUserPalletBuild", "no",  "No").
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Post PalletBuild StockStatus")).
   KittingConfigHistoryDetailsForm:insertComboField("PostPalletBuildStockStatusID", "", 190, TRUE).  

   FOR EACH StockStatus NO-LOCK
      WHERE StockStatus.Active:
      KittingConfigHistoryDetailsForm:insertComboPairs("PostPalletBuildStockStatusID", STRING(StockStatus.StockStatusID), StockStatus.StatusName).
   END.
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Post PalletBuild Location")).
   KittingConfigHistoryDetailsForm:insertComboField("PostPalletBuildLocationID", "", 190, TRUE).  

   FOR EACH Location NO-LOCK
      WHERE Location.Active:
      KittingConfigHistoryDetailsForm:insertComboPairs("PostPalletBuildLocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingConfigHistoryDetailsForm:startRow().
   KittingConfigHistoryDetailsForm:insertLabel(fTL("Typical WorkOrder Qty")).
   KittingConfigHistoryDetailsForm:insertTextField("TypicalWorkOrderQty", "", 190, TRUE).
   
   {webGetOptionalFormFields.i pKittingConfigHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   KittingConfigHistoryDetailsForm:insertHiddenField("kittingconfighistory_browse_scroll", "").
   KittingConfigHistoryDetailsForm:insertHiddenField("form_name", "kittingconfighistory_details_form").
   KittingConfigHistoryDetailsForm:insertHiddenField("prog_name", "adKittingConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   KittingConfigHistoryDetailsButtons = NEW buttonBar().

   KittingConfigHistoryDetailsButtons:addButton("kittingconfighistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "disablePopup('kittingconfighistory_details_form_popup');").
   KittingConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingConfigHistoryDetailsForm:FormButtons = KittingConfigHistoryDetailsButtons.
   
   KittingConfigHistoryDetailsForm:endForm(). 
   
   KittingConfigHistoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + KittingConfigDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pKittingConfigHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      KittingConfigDetailsForm:startRow().
      KittingConfigDetailsForm:insertLabel(fTL("Field Label")).
      KittingConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
    {adKittingConfig_kittingconfig_details_form.i}
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pKittingConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   KittingConfigHistoryBrowseForm           = NEW dataForm("kittingconfighistory_browse_form").
   KittingConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   KittingConfigHistoryBrowseForm:FormWidth  = 860.
   KittingConfigHistoryBrowseForm:FormHeight = 530.
   KittingConfigHistoryBrowseForm:FormTitle  = fTL("Kitting Config History").
   KittingConfigHistoryBrowseForm:FormType   = "xxl_large".
   KittingConfigHistoryBrowse                = NEW browseTable("kittingconfighistory_browse").
   KittingConfigHistoryBrowse:BrowseWidth    = 840.
   KittingConfigHistoryBrowse:BrowseHeight   = 490.
   
   KittingConfigHistoryBrowse:insertColumn(fTL("Hist ID"),        90, "INTEGER",   "LEFT", FALSE).
   KittingConfigHistoryBrowse:insertColumn(fTL("Scan CP"),        80, "CHARACTER", "LEFT", FALSE).
   KittingConfigHistoryBrowse:insertColumn(fTL("Scan FGP"),       80, "CHARACTER", "LEFT", FALSE).
   KittingConfigHistoryBrowse:insertColumn(fTL("BackFlush"),      80, "CHARACTER", "LEFT", FALSE).
   KittingConfigHistoryBrowse:insertColumn(fTL("Multi KitLine"),  80, "CHARACTER", "LEFT", FALSE).
   KittingConfigHistoryBrowse:insertColumn(fTL("Mix Pallet"),     80, "CHARACTER", "LEFT", FALSE).
   KittingConfigHistoryBrowse:insertColumn(fTL("User"),          125, "CHARACTER", "LEFT", FALSE).
   KittingConfigHistoryBrowse:insertColumn(fTL("Created"),       115, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i KittingConfigHistory}
   
   KittingConfigHistoryBrowse:StartBody().
   
   FOR EACH KittingConfigHistory NO-LOCK /*idx=KittingConfigID*/
      WHERE KittingConfigHistory.KittingConfigID = intSelectedKittingConfig
         BY KittingConfigHistory.KittingConfigHistoryID DESCENDING:
          
      FIND FIRST OperationType OF KittingConfigHistory NO-LOCK NO-ERROR.
      FIND FIRST GateUser      OF KittingConfigHistory NO-LOCK NO-ERROR.
      
      KittingConfigHistoryBrowse:startRow (KittingConfigHistory.KittingConfigHistoryID, 
         "selectKittingConfigHistoryRow(this," + '"' + STRING(KittingConfigHistory.KittingConfigHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i KittingConfigHistory}
      
      KittingConfigHistoryBrowse:insertData(KittingConfigHistory.KittingConfigHistoryID, "LEFT").
      KittingConfigHistoryBrowse:insertData(STRING(KittingConfigHistory.ForceScanningOfComponentParts, "Yes/No"), "LEFT").
      KittingConfigHistoryBrowse:insertData(STRING(KittingConfigHistory.ForceScanningOfFgPart, "Yes/No"), "LEFT").
      KittingConfigHistoryBrowse:insertData(STRING(KittingConfigHistory.BackflushPerFullFGStockEntity, "Yes/No"), "LEFT").
      KittingConfigHistoryBrowse:insertData(STRING(KittingConfigHistory.AllowMultipleKitLinePerWorkOrder, "Yes/No"), "LEFT").
      KittingConfigHistoryBrowse:insertData(STRING(KittingConfigHistory.AllowMixedWorkOrdersOnOnePallet, "Yes/No"), "LEFT").
      KittingConfigHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      KittingConfigHistoryBrowse:insertData(fDisplayDate&Time(KittingConfigHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      KittingConfigHistoryBrowse:endRow().
   END. /* FOR EACH KittingConfigHistory */
   
   KittingConfigHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + KittingConfigHistoryBrowse:getErrors().
   
   KittingConfigHistoryBrowseForm:insertHiddenField("popup_kittingconfighistory_browse","").
   KittingConfigHistoryBrowseForm:insertHiddenField("KittingConfigHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   KittingConfigHistoryButtons = NEW buttonBar().
   
      KittingConfigHistoryButtons:addButton("kittingconfighistory_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewKittingConfigHistoryDetails('kittingconfighistory_details_form');",/*maybe*/
                                         (IF intSelectedKittingConfigHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   KittingConfigHistoryButtons:addButton("kittingconfighistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('kittingconfighistory_browse_form_popup');").
   KittingConfigHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingConfigHistoryBrowseForm:FormBrowse  = KittingConfigHistoryBrowse.
   KittingConfigHistoryBrowseForm:FormButtons = KittingConfigHistoryButtons.
   KittingConfigHistoryBrowseForm:endForm(). 
   
   KittingConfigHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


