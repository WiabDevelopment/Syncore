&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adFastPickPackConfigAdmin.p 

  Description: ad file for the File Fast Pick Pack Config screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Alex Litas

  Created: 26/03/2015

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
{defDataMigrationVariables.i}

/* Definitions for System Options for Receiving */
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedFastPickPackConfig           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedFastPickPackConfigHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrFastPickPackConfigHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFastPickPackConfigRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFastPickPackConfigRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFastPickPackConfigHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFastPickPackConfigID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logRecordFoundFastPickPackConfig        AS LOGICAL     NO-UNDO.

/* ConfigBrowse Objects */
DEFINE VARIABLE FastPickPackConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE FastPickPackConfigBrowse                AS browseTable.
DEFINE VARIABLE FastPickPackConfigBrowseButtons         AS buttonBar.

/* ConfigDetails Objects */
DEFINE VARIABLE FastPickPackConfigDetailsForm           AS dataForm.
DEFINE VARIABLE FastPickPackConfigDetailsButtons        AS buttonBar.

/* History Objects */
DEFINE VARIABLE FastPickPackConfigHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE FastPickPackConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE FastPickPackConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE FastPickPackConfigHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE FastPickPackConfigHistoryButtons        AS buttonBar.


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
   
   ASSIGN chrFastPickPackConfigID                 = get-value("FastPickPackConfigID")
          intSelectedFastPickPackConfig           = INTEGER(chrFastPickPackConfigID)
          chrScrollToFastPickPackConfigRow        = STRING(INTEGER(get-value("fastpickpackconfig_browse_scroll"))) + ";"
          chrFastPickPackConfigHistoryID          = get-value("FastPickPackConfigHistoryID")
          intSelectedFastPickPackConfigHistory    = INTEGER(chrFastPickPackConfigHistoryID)
          chrScrollToFastPickPackConfigHistoryRow = STRING(INTEGER(get-value("fastpickpackconfigHistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrFastPickPackConfigID <> "" THEN
      chrFastPickPackConfigRow = 'selectFastPickPackConfigRow(document.getElementById("fastpickpackconfig_browse_row_' + chrFastPickPackConfigID + '"),"' 
         + chrFastPickPackConfigID +  '");'.
                                                          
   IF get-value('popup_fastpickpackconfighistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("fastpickpackconfighistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("fastpickpackconfig_browse").scrollTop=' + chrScrollToFastPickPackConfigRow 
      + chrFastPickPackConfigRow 
      + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage                = NEW HTMLPage().
   ThisPage:WebStream      = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID  = get-value("SESSIONID").
   ThisPage:ParentMenu     = get-value("menu_no").
   ThisPage:PageTitle      = "Fast Pick Pack Config Admin".
   ThisPage:FrameTitle     = "Fast Pick Pack Config Admin".
   ThisPage:OnBodyLoad     = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Fast Pick Pack Config */
   ThisPage:addJavaScript("fastpickpackconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pFastPickPackConfigBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pFastPickPackConfigDetails.
   
   RUN pFastPickPackConfigHistoryBrowse.
   
   RUN pFastPickPackConfigHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT FastPickPackConfigBrowseFrame           NO-ERROR.
   DELETE OBJECT FastPickPackConfigBrowse                NO-ERROR.
   DELETE OBJECT FastPickPackConfigBrowseButtons         NO-ERROR.
   DELETE OBJECT FastPickPackConfigDetailsForm           NO-ERROR.
   DELETE OBJECT FastPickPackConfigDetailsButtons        NO-ERROR.
   DELETE OBJECT FastPickPackConfigHistoryBrowseForm     NO-ERROR.  
   DELETE OBJECT FastPickPackConfigHistoryBrowse         NO-ERROR.
   DELETE OBJECT FastPickPackConfigHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT FastPickPackConfigHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT FastPickPackConfigHistoryButtons        NO-ERROR.
   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFastPickPackConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFastPickPackConfigBrowse Procedure 
PROCEDURE pFastPickPackConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "fastpickpackconfig_details_form"}
   
   FastPickPackConfigBrowse = NEW browseTable("fastpickpackconfig_browse").
   FastPickPackConfigBrowse:BrowseWidth  = 965.
   FastPickPackConfigBrowse:BrowseHeight = 455.
   FastPickPackConfigBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   FastPickPackConfigBrowse:insertColumn(fTL("Config ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FastPickPackConfig}
   
   FastPickPackConfigBrowse:insertColumn(fTL("Ship Order Status"),        150, "CHARACTER", FALSE).   
   FastPickPackConfigBrowse:insertColumn(fTL("Ship Status"),              150, "CHARACTER", FALSE).   
   FastPickPackConfigBrowse:insertColumn(fTL("Confirm Ship Order Status"),160, "CHARACTER", FALSE).   
   FastPickPackConfigBrowse:insertColumn(fTL("Confirm Ship Status"),      160, "CHARACTER", FALSE).   
   FastPickPackConfigBrowse:insertColumn(fTL("Print Slip At Packout"),    150, "CHARACTER", FALSE).
      
   /*Body*/
   FastPickPackConfigBrowse:startBody().
   
   FOR EACH FastPickPackConfig NO-LOCK: /*idx = FastPickPackConfigID*/
      logRecordFoundFastPickPackConfig = TRUE.
      
      FastPickPackConfigBrowse:startRow(FastPickPackConfig.FastPickPackConfigID, "selectFastPickPackConfigRow(this," 
                                        + '"' + STRING(FastPickPackConfig.FastPickPackConfigID) + '"' + ");", "").
      FastPickPackConfigBrowse:insertData(FastPickPackConfig.FastPickPackConfigID).
      
      FIND FIRST ShipOrderStatus NO-LOCK
         WHERE ShipOrderStatus.ShipOrderStatusID = FastPickPackConfig.PostPackoutShipOrderStatusID NO-ERROR.
      FIND FIRST ShipStatus NO-LOCK 
         WHERE ShipStatus.ShipStatusID = FastPickPackConfig.PostPackoutShipStatusID NO-ERROR.
         
      FastPickPackConfigBrowse:insertData((IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusName ELSE "")).
      FastPickPackConfigBrowse:insertData((IF AVAILABLE ShipStatus THEN ShipStatus.StatusName ELSE "")).
            
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FastPickPackConfig}      

      FIND FIRST ShipOrderStatus NO-LOCK
         WHERE ShipOrderStatus.ShipOrderStatusID = FastPickPackConfig.PostPickConfirmShipOrderStatusID NO-ERROR.
      FIND FIRST ShipStatus NO-LOCK 
         WHERE ShipStatus.ShipStatusID = FastPickPackConfig.PostPickConfirmShipStatusID NO-ERROR.
        
      FastPickPackConfigBrowse:insertData((IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusName ELSE "")).
      FastPickPackConfigBrowse:insertData((IF AVAILABLE ShipStatus THEN ShipStatus.StatusName ELSE "")).
      
      FastPickPackConfigBrowse:insertData(STRING(FastPickPackConfig.PrintPackingSlipAtStartOfPackout,"Yes/No")).
      
            
      /* Add hidden fields */
      FastPickPackConfigBrowse:insertHiddenData("FastPickPackConfigID",FastPickPackConfig.FastPickPackConfigID).
      FastPickPackConfigBrowse:insertHiddenData("FastPickPackConfigVersionID",FastPickPackConfig.VersionID).
      
      FastPickPackConfigBrowse:endRow().
      
   END. /*FOR EACH FastPickPackConfig NO-LOCK */
   
   FastPickPackConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FastPickPackConfigBrowse:getErrors().
   
   /* Create a new frame */
   FastPickPackConfigBrowseFrame = NEW pageFrame().
   FastPickPackConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FastPickPackConfigBrowseFrame:FormAction="dbFastPickPackConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FastPickPackConfigBrowseFrame:formOpen("fastpickpackconfig_browse_form").
   
   /* Start the Frame Header */
   FastPickPackConfigBrowseFrame:insertSpacer(5).
   FastPickPackConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FastPickPackConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   FastPickPackConfigBrowseFrame:frameClose().
   FastPickPackConfigBrowseFrame:insertSpacer(10).
   
   FastPickPackConfigBrowseFrame:insertHiddenField("fastpickpackconfig_browse_scroll","").
   FastPickPackConfigBrowseFrame:insertHiddenField("FastPickPackConfigID","").
   FastPickPackConfigBrowseFrame:insertHiddenField("FastPickPackConfigVersionID","").
   FastPickPackConfigBrowseFrame:insertHiddenField("form_name","fastpickpackconfig_browse_form").
   FastPickPackConfigBrowseFrame:insertHiddenField("popup_fastpickpackconfighistory_browse","").
   FastPickPackConfigBrowseFrame:insertHiddenField("prog_name","adFastPickPackConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackConfigBrowseFrame}
   
   FastPickPackConfigBrowseFrame:formClose().
   
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   FastPickPackConfigBrowseButtons = NEW buttonBar().
   FastPickPackConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   FastPickPackConfigBrowseButtons:addButton("fastpickpackconfig_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewFastPickPackConfigDetails('fastpickpackconfig_details_form');",
                                             (IF intSelectedFastPickPackConfig > 0 THEN "" ELSE "Disabled")).
  
   IF NOT logPreventDataCreates THEN
   DO:  
      FastPickPackConfigBrowseButtons:addButton("fastpickpackconfig_browse_form_btn_create",
                                                fTL("Create"),
                                                "createFastPickPackConfig('fastpickpackconfig_details_form');",
                                                (IF logRecordFoundFastPickPackConfig THEN "Disabled" ELSE "")).
   END.                                         
   
   FastPickPackConfigBrowseButtons:addButton("fastpickpackconfig_browse_form_btn_history",
                                             fTL("History"),
                                             "viewFastPickPackConfigHistory('fastpickpackconfig_browse_form');",
                                             (IF intSelectedFastPickPackConfig > 0 THEN "" ELSE "Disabled")).
                                       
   FastPickPackConfigBrowseButtons:closeBar().  
   FastPickPackConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFastPickPackConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFastPickPackConfigDetails Procedure 
PROCEDURE pFastPickPackConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "fastpickpackconfig_details_form"}
   ASSIGN chrDisplayFieldList  = "FastPickPackConfigID,MaxWeightPercentageVariation,PostPackoutShipOrderStatusID," +
                                 "PostPackoutShipStatusID,PostPickConfirmShipOrderStatusID,EmptyLocationInterval," +
                                 "PostPickConfirmShipStatusID,PrintPackingSlipAtStartOfPackout,PrintLabelWhenPackageIsCreated," +
                                 "ForceShipPackageLabelScan,MaxNumberOrdersPerTask,MaxNumberRetriesForAutoGenerate," +
                                 "RefreshTimerInterval" 
          chrEditFieldList     = "MaxWeightPercentageVariation,PostPackoutShipOrderStatusID," +
                                 "PostPackoutShipStatusID,PostPickConfirmShipOrderStatusID,EmptyLocationInterval," +
                                 "PostPickConfirmShipStatusID,PrintPackingSlipAtStartOfPackout,PrintLabelWhenPackageIsCreated," +
                                 "ForceShipPackageLabelScan,MaxNumberOrdersPerTask,MaxNumberRetriesForAutoGenerate," + 
                                 "RefreshTimerInterval"
          chrNewFieldList      = "MaxWeightPercentageVariation,PostPackoutShipOrderStatusID," +
                                 "PostPackoutShipStatusID,PostPickConfirmShipOrderStatusID,EmptyLocationInterval," +
                                 "PostPickConfirmShipStatusID,PrintPackingSlipAtStartOfPackout,PrintLabelWhenPackageIsCreated," +
                                 "ForceShipPackageLabelScan,MaxNumberOrdersPerTask,MaxNumberRetriesForAutoGenerate," + 
                                 "RefreshTimerInterval"
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   FastPickPackConfigDetailsForm             = NEW dataForm("fastpickpackconfig_details_form").
   FastPickPackConfigDetailsForm:WebStream   = STREAM WebStream:HANDLE.
   
   FastPickPackConfigDetailsForm:FormAction  = "dbFastPickPackConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FastPickPackConfigDetailsForm:FormWidth   = 580.
   FastPickPackConfigDetailsForm:FormHeight  = 420.
   FastPickPackConfigDetailsForm:FormTitle   = "Fast Pick Pack Config details".
   FastPickPackConfigDetailsForm:FormType    = "large".
   
   /* Column Layout */
   FastPickPackConfigDetailsForm:insertPaddingColumn(30).
   FastPickPackConfigDetailsForm:insertColumn(265).
   
   /* Fields */
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Config ID")).
   FastPickPackConfigDetailsForm:insertTextField("FastPickPackConfigID", "", 75, TRUE).  
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Max Weight Percentage Variation")).
   FastPickPackConfigDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 75, TRUE).  
    
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Ship Order Status")).
   FastPickPackConfigDetailsForm:insertComboField("PostPackoutShipOrderStatusID", "", 168, TRUE).
   
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY ShipOrderStatus.ShipOrderStatusID:
         
      FastPickPackConfigDetailsForm:insertComboPairs("PostPackoutShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   
   END.
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Ship Status")).
   FastPickPackConfigDetailsForm:insertComboField("PostPackoutShipStatusID", "", 168, TRUE).
   
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY ShipStatus.ShipStatusID:
         
      FastPickPackConfigDetailsForm:insertComboPairs("PostPackoutShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   
   END.
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Confirm Ship Order Status")).
   FastPickPackConfigDetailsForm:insertComboField("PostPickConfirmShipOrderStatusID", "", 168, TRUE).
   
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY ShipOrderStatus.ShipOrderStatusID:
         
      FastPickPackConfigDetailsForm:insertComboPairs("PostPickConfirmShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   
   END.
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Confirm Ship Status")).
   FastPickPackConfigDetailsForm:insertComboField("PostPickConfirmShipStatusID", "", 168, TRUE).
   
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY ShipStatus.ShipStatusID:
         
      FastPickPackConfigDetailsForm:insertComboPairs("PostPickConfirmShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   
   END.
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Print Pack Slip At Start Of Packout")).
   FastPickPackConfigDetailsForm:insertComboField("PrintPackingSlipAtStartOfPackout", "", 168, TRUE).
   FastPickPackConfigDetailsForm:insertComboPairs("PrintPackingSlipAtStartOfPackout", "yes", "Yes").
   FastPickPackConfigDetailsForm:insertComboPairs("PrintPackingSlipAtStartOfPackout","no", "No").
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Print Label At Package Creation")).
   FastPickPackConfigDetailsForm:insertComboField("PrintLabelWhenPackageIsCreated", "", 168, TRUE).
   FastPickPackConfigDetailsForm:insertComboPairs("PrintLabelWhenPackageIsCreated", "yes", "Yes").
   FastPickPackConfigDetailsForm:insertComboPairs("PrintLabelWhenPackageIsCreated","no", "No").
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Force ShipPackage Label Scan")).
   FastPickPackConfigDetailsForm:insertComboField("ForceShipPackageLabelScan", "", 168, TRUE).
   FastPickPackConfigDetailsForm:insertComboPairs("ForceShipPackageLabelScan", "yes", "Yes").
   FastPickPackConfigDetailsForm:insertComboPairs("ForceShipPackageLabelScan","no", "No").
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Max Number Orders Per Task")).
   FastPickPackConfigDetailsForm:insertTextField("MaxNumberOrdersPerTask", "", 75, TRUE). 
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Max Number Retries For AutoGenerate")).
   FastPickPackConfigDetailsForm:insertTextField("MaxNumberRetriesForAutoGenerate", "", 75, TRUE). 
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Empty Location Interval")).
   FastPickPackConfigDetailsForm:insertTextField("EmptyLocationInterval", "", 75, TRUE). 
   
   FastPickPackConfigDetailsForm:startRow().
   FastPickPackConfigDetailsForm:insertLabel(fTL("Refresh Timer (sec)")).
   FastPickPackConfigDetailsForm:insertTextField("RefreshTimerInterval", "", 75, TRUE). 
   
      
   {webGetOptionalFormFields.i pFastPickPackConfigDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FastPickPackConfigDetailsForm:insertHiddenField("fastpickpackconfig_browse_scroll", "").
   FastPickPackConfigDetailsForm:insertHiddenField("form_name", "fastpickpackconfig_details_form").
   FastPickPackConfigDetailsForm:insertHiddenField("prog_name", "adFastPickPackConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackConfigDetailsForm}
   
   /* Create Button Bar */
   FastPickPackConfigDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataCreates THEN 
   DO:
      FastPickPackConfigDetailsButtons:addButton("fastpickpackconfig_details_form_btn_save", 
                                                fTL("Save"), 
                                                "updateFastPickPackConfig('fastpickpackconfig_details_form');").
      FastPickPackConfigDetailsButtons:addButton("fastpickpackconfig_details_form_btn_cancel", 
                                                fTL("Cancel"), 
                                                "cancelUpdate('UserCancelled','process_mode'); disablePopup('fastpickpackconfig_details_form_popup');").
      FastPickPackConfigDetailsButtons:closeBar().  
   END.
   
   /* Assign the Button Bar Object to the Form Object */
   FastPickPackConfigDetailsForm:FormButtons = FastPickPackConfigDetailsButtons.
   
   FastPickPackConfigDetailsForm:endForm(). 
   
   FastPickPackConfigDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pFastPickPackConfigDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            FastPickPackConfigDetailsForm:startRow().
            FastPickPackConfigDetailsForm:insertLabel(fTL("Field Label")).
            FastPickPackConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFastPickPackConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFastPickPackConfigHistoryBrowse Procedure 

PROCEDURE pFastPickPackConfigHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "fastpickpackconfighistory_details_form"}
   ASSIGN chrDisplayFieldList  = "FastPickPackConfigHistoryID,FastPickPackConfigID,MaxWeightPercentageVariation,"+
                                 "PostPackoutShipOrderStatusID,PostPackoutShipStatusID,PostPickConfirmShipOrderStatusID,"+
                                 "PostPickConfirmShipStatusID,PrintPackingSlipAtStartOfPackout,PrintLabelWhenPackageIsCreated,GateUserID,"+
                                 "ForceShipPackageLabelScan,MaxNumberOrdersPerTask,MaxNumberRetriesForAutoGenerate,EmptyLocationInterval," +
                                 "RefreshTimerInterval,CreatedDate,CreatedHour,CreatedMins" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   FastPickPackConfigHistoryDetailsForm             = NEW dataForm("fastpickpackconfighistory_details_form").
   FastPickPackConfigHistoryDetailsForm:WebStream   = STREAM WebStream:HANDLE.
   
   FastPickPackConfigHistoryDetailsForm:FormAction  = "dbFastPickPackConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FastPickPackConfigHistoryDetailsForm:FormWidth   = 580.
   FastPickPackConfigHistoryDetailsForm:FormHeight  = 420.
   FastPickPackConfigHistoryDetailsForm:FormTitle   = "Fast Pick Pack Config History Details".
   FastPickPackConfigHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   FastPickPackConfigHistoryDetailsForm:insertPaddingColumn(30).
   FastPickPackConfigHistoryDetailsForm:insertColumn(220).
   FastPickPackConfigHistoryDetailsForm:insertColumn(125).
   FastPickPackConfigHistoryDetailsForm:insertColumn(20).
   FastPickPackConfigHistoryDetailsForm:insertColumn(4).
   FastPickPackConfigHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Config History ID")).
   FastPickPackConfigHistoryDetailsForm:insertTextField("FastPickPackConfigHistoryID", "", 100, TRUE).
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Config ID")).
   FastPickPackConfigHistoryDetailsForm:insertTextField("FastPickPackConfigID", "", 100, TRUE).
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Max Weight Percentage Variation")).
   FastPickPackConfigHistoryDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 100, TRUE).
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Ship Order Status")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("PostPackoutShipOrderStatusID", "", 168, TRUE).
   
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY ShipOrderStatus.ShipOrderStatusID:
         
      FastPickPackConfigHistoryDetailsForm:insertComboPairs("PostPackoutShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   
   END.

   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Ship Status")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("PostPackoutShipStatusID", "", 168, TRUE).
   
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY ShipStatus.ShipStatusID:
         
      FastPickPackConfigHistoryDetailsForm:insertComboPairs("PostPackoutShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   
   END.
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Confirm Ship Order Status")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("PostPickConfirmShipOrderStatusID", "", 168, TRUE).
   
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY ShipOrderStatus.ShipOrderStatusID:
         
      FastPickPackConfigHistoryDetailsForm:insertComboPairs("PostPickConfirmShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   
   END.
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Confirm Ship Status")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("PostPickConfirmShipStatusID", "", 168, TRUE).
   
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY ShipStatus.ShipStatusID:
         
      FastPickPackConfigHistoryDetailsForm:insertComboPairs("PostPickConfirmShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   
   END.
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Print Pack Slip At Start Of Packout")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("PrintPackingSlipAtStartOfPackout", "", 168, TRUE).
   FastPickPackConfigHistoryDetailsForm:insertComboPairs("PrintPackingSlipAtStartOfPackout", "yes", "Yes").
   FastPickPackConfigHistoryDetailsForm:insertComboPairs("PrintPackingSlipAtStartOfPackout", "no", "No").
     
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Print Label At Package Creation")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("PrintLabelWhenPackageIsCreated", "", 168, TRUE).
   FastPickPackConfigHistoryDetailsForm:insertComboPairs("PrintLabelWhenPackageIsCreated", "yes", "Yes").
   FastPickPackConfigHistoryDetailsForm:insertComboPairs("PrintLabelWhenPackageIsCreated", "no", "No").
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Force ShipPackage Label Scan")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("ForceShipPackageLabelScan", "", 168, TRUE).
   FastPickPackConfigHistoryDetailsForm:insertComboPairs("ForceShipPackageLabelScan", "yes", "Yes").
   FastPickPackConfigHistoryDetailsForm:insertComboPairs("ForceShipPackageLabelScan","no", "No").
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Max Number Orders Per Task")).
   FastPickPackConfigHistoryDetailsForm:insertTextField("MaxNumberOrdersPerTask", "", 100, TRUE). 
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Max Number Retries For AutoGenerate")).
   FastPickPackConfigHistoryDetailsForm:insertTextField("MaxNumberRetriesForAutoGenerate", "", 100, TRUE). 
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Empty Location Interval")).
   FastPickPackConfigHistoryDetailsForm:insertTextField("EmptyLocationInterval", "", 100, TRUE).
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Refresh Timer (sec)")).
   FastPickPackConfigHistoryDetailsForm:insertTextField("RefreshTimerInterval", "", 100, TRUE). 
   
   
   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Gate User")).
   FastPickPackConfigHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).   
   
   FOR EACH GateUser NO-LOCK /*idx=FullName*/
      BY GateUser.FullName:
         
      FastPickPackConfigHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   
   END.

   FastPickPackConfigHistoryDetailsForm:startRow().
   FastPickPackConfigHistoryDetailsForm:insertLabel(fTL("Created")).
   FastPickPackConfigHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).

   /* Time fields have no label */
   FastPickPackConfigHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).
   /* This has a label to separate the time */
   FastPickPackConfigHistoryDetailsForm:insertLabel(":").
   FastPickPackConfigHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   {webGetOptionalFormFields.i pFastPickPackConfigHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FastPickPackConfigHistoryDetailsForm:insertHiddenField("fastpickpackconfighistory_browse_scroll", "").
   FastPickPackConfigHistoryDetailsForm:insertHiddenField("form_name", "fastpickpackconfighistory_details_form").
   FastPickPackConfigHistoryDetailsForm:insertHiddenField("prog_name", "adFastPickPackConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   FastPickPackConfigHistoryDetailsButtons = NEW buttonBar().

   FastPickPackConfigHistoryDetailsButtons:addButton("fastpickpackconfighistory_details_form_btn_cancel", 
                                                     fTL("Cancel"), 
                                                     "disablePopup('fastpickpackconfighistory_details_form_popup');").
   FastPickPackConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FastPickPackConfigHistoryDetailsForm:FormButtons = FastPickPackConfigHistoryDetailsButtons.
   
   FastPickPackConfigHistoryDetailsForm:endForm(). 
   
   FastPickPackConfigHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFastPickPackConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFastPickPackConfigDetailsFields Procedure 

PROCEDURE pFastPickPackConfigHistoryDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            FastPickPackConfigDetailsForm:startRow().
            FastPickPackConfigDetailsForm:insertLabel(fTL("Field Label")).
            FastPickPackConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   /* This will be held in customer specific code repository */
   /*/*    {adFastPickPackConfig_fastpickpackconfig_details_form.i}*/ COMEBACK START COMEBACK END*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pFastPickPackConfigHistoryBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   FastPickPackConfigHistoryBrowseForm            = NEW dataForm("fastpickpackconfighistory_browse_form").
   FastPickPackConfigHistoryBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   
   /* Setup */
   FastPickPackConfigHistoryBrowseForm:FormWidth  = 860.
   FastPickPackConfigHistoryBrowseForm:FormHeight = 530.
   FastPickPackConfigHistoryBrowseForm:FormTitle  = fTL("Fast Pick Pack Config History").
   FastPickPackConfigHistoryBrowseForm:FormType   = "xxl_large".
   FastPickPackConfigHistoryBrowse                = NEW browseTable("fastpickpackconfighistory_browse").
   FastPickPackConfigHistoryBrowse:BrowseWidth    = 840.
   FastPickPackConfigHistoryBrowse:BrowseHeight   = 490.
   
   FastPickPackConfigHistoryBrowse:insertColumn(fTL("History ID"),                80, "INTEGER",   FALSE).
   FastPickPackConfigHistoryBrowse:insertColumn(fTL("Ship Order Status"),        120, "CHARACTER", FALSE).   
   FastPickPackConfigHistoryBrowse:insertColumn(fTL("Ship Status"),              125, "CHARACTER", FALSE).   
   FastPickPackConfigHistoryBrowse:insertColumn(fTL("Confirm ShipOrder Status"), 150, "CHARACTER", FALSE).   
   FastPickPackConfigHistoryBrowse:insertColumn(fTL("Confirm Ship Status"),      150, "CHARACTER", FALSE). 
   FastPickPackConfigHistoryBrowse:insertColumn(fTL("Created"),                  150, "CHARACTER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FastPickPackConfigHistory}
   
   FastPickPackConfigHistoryBrowse:StartBody().
   
      /* List the FastPickPackConfigHistory for the FastPickPackConfig */
      FOR EACH FastPickPackConfigHistory NO-LOCK /*idx = FastPickPackConfigHistoryID*/
         WHERE FastPickPackConfigHistory.FastPickPackConfigID = intSelectedFastPickPackConfig 
         BY FastPickPackConfigHistory.Created DESCENDING:
         
         FIND FIRST ShipOrderStatus NO-LOCK 
         WHERE ShipOrderStatus.ShipOrderStatusID = FastPickPackConfigHistory.PostPackoutShipOrderStatusID NO-ERROR.
      
         FIND FIRST ShipStatus NO-LOCK 
            WHERE ShipStatus.ShipStatusID = FastPickPackConfigHistory.PostPackoutShipStatusID NO-ERROR.
         
          FastPickPackConfigHistoryBrowse:startRow (FastPickPackConfigHistory.FastPickPackConfigHistoryID, 
              "selectFastPickPackConfigHistoryRow(this," + '"' + STRING(FastPickPackConfigHistory.FastPickPackConfigHistoryID) + '"' + ");", "").
         
         FastPickPackConfigHistoryBrowse:insertData(FastPickPackConfigHistory.FastPickPackConfigHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i FastPickPackConfigHistory}

         FastPickPackConfigHistoryBrowse:insertData(IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusCode ELSE "").
         FastPickPackConfigHistoryBrowse:insertData(IF AVAILABLE ShipStatus THEN ShipStatus.StatusCode ELSE "").
         
         FIND FIRST ShipOrderStatus NO-LOCK 
            WHERE ShipOrderStatus.ShipOrderStatusID = FastPickPackConfigHistory.PostPickConfirmShipOrderStatusID NO-ERROR.
         
         FIND FIRST ShipStatus NO-LOCK 
            WHERE ShipStatus.ShipStatusID = FastPickPackConfigHistory.PostPickConfirmShipStatusID NO-ERROR.
                    
         FastPickPackConfigHistoryBrowse:insertData(IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusCode ELSE "").
         FastPickPackConfigHistoryBrowse:insertData(IF AVAILABLE ShipStatus THEN ShipStatus.StatusCode ELSE "").

         
         FastPickPackConfigHistoryBrowse:insertData(fDisplayDate&Time(FastPickPackConfigHistory.Created,"y/m/d H:M:S")).                  

         FastPickPackConfigHistoryBrowse:endRow().
      
      END. /* FOR EACH FastPickPackConfigHistory OF FastPickPackConfig NO-LOCK */
   
   FastPickPackConfigHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FastPickPackConfigHistoryBrowse:getErrors().
   
   FastPickPackConfigHistoryBrowseForm:insertHiddenField("popup_fastpickpackconfighistory_browse","").
   FastPickPackConfigHistoryBrowseForm:insertHiddenField("FastPickPackConfigHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   FastPickPackConfigHistoryButtons = NEW buttonBar().
   
   FastPickPackConfigHistoryButtons:addButton("fastpickpackconfighistory_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewFastPickPackConfigHistoryDetails('fastpickpackconfighistory_details_form');",
                                             (IF intSelectedFastPickPackConfigHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   FastPickPackConfigHistoryButtons:addButton("fastpickpackconfighistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('fastpickpackconfighistory_browse_form_popup');").
   FastPickPackConfigHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FastPickPackConfigHistoryBrowseForm:FormBrowse  = FastPickPackConfigHistoryBrowse.
   FastPickPackConfigHistoryBrowseForm:FormButtons = FastPickPackConfigHistoryButtons.
   FastPickPackConfigHistoryBrowseForm:endForm(). 
   
   FastPickPackConfigHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


