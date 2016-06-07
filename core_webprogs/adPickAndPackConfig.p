&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adPickAndPackConfig.p 

  Description: ad file for the PickAndPack Config screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Nick Diessner

  Created: 27/03/2015

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
DEFINE VARIABLE intSelectedPickAndPackConfig           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedPickAndPackConfigHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPickAndPackConfigHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPickAndPackConfigRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickAndPackConfigRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickAndPackConfigHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPickAndPackConfigID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logPickAndPackRecord                   AS LOGICAL     NO-UNDO INITIAL YES.

/* Buffers */
/* Objects */ 
DEFINE VARIABLE PickAndPackConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE PickAndPackConfigBrowse                AS browseTable.
DEFINE VARIABLE PickAndPackConfigBrowseButtons         AS buttonBar.
DEFINE VARIABLE PickAndPackConfigDetailsForm           AS dataForm.
DEFINE VARIABLE PickAndPackConfigDetailsButtons        AS buttonBar.
DEFINE VARIABLE PickAndPackConfigHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE PickAndPackConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE PickAndPackConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE PickAndPackConfigHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE PickAndPackConfigHistoryButtons        AS buttonBar.


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
   
   ASSIGN chrPickAndPackConfigID                  = get-value("PickAndPackConfigID")
          intSelectedPickAndPackConfig            = INTEGER(chrPickAndPackConfigID)
          chrScrollToPickAndPackConfigRow         = STRING(INTEGER(get-value("pickandpackconfig_browse_scroll"))) + ";"
          chrPickAndPackConfigHistoryID           = get-value("PickAndPackConfigHistoryID")
          intSelectedPickAndPackConfigHistory     = INTEGER(chrPickAndPackConfigHistoryID)
          chrScrollToPickAndPackConfigHistoryRow  = STRING(INTEGER(get-value("pickandpackconfigHistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPickAndPackConfigID <> "" THEN
      chrPickAndPackConfigRow = 'selectPickAndPackConfigRow(document.getElementById("pickandpackconfig_browse_row_' + chrPickAndPackConfigID + '"),"' 
                                                          + chrPickAndPackConfigID +  '");'.
                                                          
   IF get-value('popup_pickandpackconfighistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("pickandpackconfighistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("pickandpackconfig_browse").scrollTop=' + chrScrollToPickAndPackConfigRow 
                                                           + chrPickAndPackConfigRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "PickAndPack Config".
   ThisPage:FrameTitle = "PickAndPack Config".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for PickAndPack Config */
   ThisPage:addJavaScript("pickandpackconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPickAndPackConfigBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pPickAndPackConfigDetails.
   
   RUN pPickAndPackConfigHistoryBrowse.
   
   RUN pPickAndPackConfigHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */ 
   DELETE OBJECT PickAndPackConfigBrowseFrame        NO-ERROR.
   DELETE OBJECT PickAndPackConfigBrowse             NO-ERROR.
   DELETE OBJECT PickAndPackConfigBrowseButtons      NO-ERROR.
   DELETE OBJECT PickAndPackConfigDetailsForm        NO-ERROR.
   DELETE OBJECT PickAndPackConfigDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pPickAndPackConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "pickandpackconfig_details_form"}
   
   PickAndPackConfigBrowse = NEW browseTable("pickandpackconfig_browse").
   PickAndPackConfigBrowse:BrowseWidth  = 965.
   PickAndPackConfigBrowse:BrowseHeight = 455.
   PickAndPackConfigBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   PickAndPackConfigBrowse:insertColumn(fTL("Config ID"), 75, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickAndPackConfig}
   
   PickAndPackConfigBrowse:insertColumn(fTL("PostPackShipOrderStatus"),  150, "CHARACTER", FALSE).   
   PickAndPackConfigBrowse:insertColumn(fTL("PostPackShipStatus"),       150, "CHARACTER", FALSE).   
   PickAndPackConfigBrowse:insertColumn(fTL("Confirm ShipOrder Status"), 150, "CHARACTER", FALSE).   
   PickAndPackConfigBrowse:insertColumn(fTL("Confirm Ship Status"),      150, "CHARACTER", FALSE).   
   PickAndPackConfigBrowse:insertColumn(fTL("PrintPackSlipAtBegin"),     125, "LOGICAL",   FALSE).   
   
   /*Body*/
   PickAndPackConfigBrowse:startBody().
   
   FOR EACH PickAndPackConfig NO-LOCK: 
      
      logPickAndPackRecord = FALSE.
      
      PickAndPackConfigBrowse:startRow(PickAndPackConfig.PickAndPackConfigID, "selectPickAndPackConfigRow(this," + '"' + STRING(PickAndPackConfig.PickAndPackConfigID) + '"' + ");", "").
      PickAndPackConfigBrowse:insertData(PickAndPackConfig.PickAndPackConfigID).
      
      FIND FIRST ShipStatus NO-LOCK 
         WHERE ShipStatus.ShipStatusID = PickAndPackConfig.PostPackoutShipStatusID NO-ERROR.
      FIND FIRST ShipOrderStatus NO-LOCK
         WHERE ShipOrderStatus.ShipOrderStatusID = PickAndPackConfig.PostPackoutShipOrderStatusID NO-ERROR.
         
      PickAndPackConfigBrowse:insertData((IF AVAILABLE ShipStatus THEN ShipStatus.StatusName ELSE "")).
      PickAndPackConfigBrowse:insertData((IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusName ELSE "")).
            
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PickAndPackConfig}      

      FIND FIRST ShipStatus NO-LOCK 
         WHERE ShipStatus.ShipStatusID = PickAndPackConfig.PostPickConfirmShipStatusID NO-ERROR.
      FIND FIRST ShipOrderStatus NO-LOCK
         WHERE ShipOrderStatus.ShipOrderStatusID = PickAndPackConfig.PostPickConfirmShipOrderStatusID NO-ERROR.
         
      PickAndPackConfigBrowse:insertData((IF AVAILABLE ShipStatus THEN ShipStatus.StatusName ELSE "")).
      PickAndPackConfigBrowse:insertData((IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusName ELSE "")).
      
      PickAndPackConfigBrowse:insertData(STRING(PickAndPackConfig.PrintPackSlipAtBegin,"Yes/No")).
            
      /* Add hidden fields */
      PickAndPackConfigBrowse:insertHiddenData("PickAndPackConfigID",PickAndPackConfig.PickAndPackConfigID).
      PickAndPackConfigBrowse:insertHiddenData("PickAndPackConfigVersionID",PickAndPackConfig.VersionID).
      
  PickAndPackConfigBrowse:endRow().
      
   END. /*FOR EACH PickAndPackConfig NO-LOCK */
   
   PickAndPackConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PickAndPackConfigBrowse:getErrors().
   
   /* Create a new frame */
   PickAndPackConfigBrowseFrame = NEW pageFrame().
   PickAndPackConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PickAndPackConfigBrowseFrame:FormAction="dbPickAndPackConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PickAndPackConfigBrowseFrame:formOpen("pickandpackconfig_browse_form").
   
   /* Start the Frame Header */
   PickAndPackConfigBrowseFrame:insertSpacer(5).
   PickAndPackConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PickAndPackConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PickAndPackConfigBrowseFrame:frameClose().
   PickAndPackConfigBrowseFrame:insertSpacer(10).
   
   PickAndPackConfigBrowseFrame:insertHiddenField("pickandpackconfig_browse_scroll","").
   PickAndPackConfigBrowseFrame:insertHiddenField("PickAndPackConfigID","").
   PickAndPackConfigBrowseFrame:insertHiddenField("PickAndPackConfigVersionID","").
   PickAndPackConfigBrowseFrame:insertHiddenField("form_name","pickandpackconfig_browse_form").
   PickAndPackConfigBrowseFrame:insertHiddenField("popup_pickandpackconfighistory_browse","").
   PickAndPackConfigBrowseFrame:insertHiddenField("prog_name","adPickAndPackConfig.p").
   
   /* This adds all of the standard form field lists to the form */
/*   {webGetHiddenFormFields.i PickAndPackConfigFrame}*/
   
   PickAndPackConfigBrowseFrame:formClose().
   
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   PickAndPackConfigBrowseButtons = NEW buttonBar().
   PickAndPackConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   PickAndPackConfigBrowseButtons:addButton("pickandpackconfig_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewPickAndPackConfigDetails('pickandpackconfig_details_form');",
                                         (IF intSelectedPickAndPackConfig > 0 THEN "" ELSE "Disabled")).
  
   IF NOT logPreventDataCreates THEN
   DO:  
   PickAndPackConfigBrowseButtons:addButton("pickandpackconfig_browse_form_btn_create",
                                         fTL("Create"),
                                         "createPickAndPackConfig('pickandpackconfig_details_form');",
                                         (IF logPickAndPackRecord THEN "" ELSE "Disabled")).
   END.                                         
   
   PickAndPackConfigBrowseButtons:addButton("pickandpackconfig_browse_form_btn_history",
                                         fTL("History"),
                                         "viewPickAndPackConfigHistory('pickandpackconfig_browse_form');",
                                         (IF intSelectedPickAndPackConfig > 0 THEN "" ELSE "Disabled")).
   
   PickAndPackConfigBrowseButtons:closeBar().  
   PickAndPackConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pPickAndPackConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "pickandpackconfig_details_form"}
   ASSIGN chrDisplayFieldList  = "PickAndPackConfigID,MaxWeightPercentageVariation,PostPackoutShipOrderStatusID,PostPackoutShipStatusID," 
                                 + "PostPickConfirmShipOrderStatusID,PostPickConfirmShipStatusID,PrintPackSlipAtBegin"
          chrEditFieldList     = "MaxWeightPercentageVariation,PostPackoutShipOrderStatusID,PostPackoutShipStatusID,"
                                 + "PostPickConfirmShipOrderStatusID,PostPickConfirmShipStatusID,PrintPackSlipAtBegin"
          chrNewFieldList      = "MaxWeightPercentageVariation,PostPackoutShipOrderStatusID,PostPackoutShipStatusID,"
                                 + "PostPickConfirmShipOrderStatusID,PostPickConfirmShipStatusID,PrintPackSlipAtBegin"
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   
   PickAndPackConfigDetailsForm = NEW dataForm("pickandpackconfig_details_form").
   PickAndPackConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PickAndPackConfigDetailsForm:FormAction = "dbPickAndPackConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PickAndPackConfigDetailsForm:FormWidth   = 460.
   PickAndPackConfigDetailsForm:FormHeight  = 300.
   PickAndPackConfigDetailsForm:FormTitle   = "PickAndPack Config details".
   PickAndPackConfigDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PickAndPackConfigDetailsForm:insertPaddingColumn(30).
   PickAndPackConfigDetailsForm:insertColumn(200).
   
   /* Fields */
   PickAndPackConfigDetailsForm:startRow().
   PickAndPackConfigDetailsForm:insertLabel(fTL("PickAndPackConfig ID")).
   PickAndPackConfigDetailsForm:insertTextField("PickAndPackConfigID", "", 190, TRUE).  
   
   PickAndPackConfigDetailsForm:startRow().
   PickAndPackConfigDetailsForm:insertLabel(fTL("Max Weight Variation (%)")).
   PickAndPackConfigDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 190, TRUE).  
   
   PickAndPackConfigDetailsForm:startRow().
   PickAndPackConfigDetailsForm:insertLabel(fTL("Post Packout ShipOrder Status")).
   PickAndPackConfigDetailsForm:insertComboField("PostPackoutShipOrderStatusID", "", 190, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY    ShipOrderStatus.ListingSequence:
         
      PickAndPackConfigDetailsForm:insertComboPairs("PostPackoutShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   END.
         
   
   PickAndPackConfigDetailsForm:startRow().
   PickAndPackConfigDetailsForm:insertLabel(fTL("Post Packout Ship Status")).
   PickAndPackConfigDetailsForm:insertComboField("PostPackoutShipStatusID", "", 190, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY    ShipStatus.ListingSequence:
         
      PickAndPackConfigDetailsForm:insertComboPairs("PostPackoutShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   END.  
   
   PickAndPackConfigDetailsForm:startRow().
   PickAndPackConfigDetailsForm:insertLabel(fTL("Post Pick Confirm ShipOrder Status")).
   PickAndPackConfigDetailsForm:insertComboField("PostPickConfirmShipOrderStatusID", "", 190, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY    ShipOrderStatus.ListingSequence:
         
      PickAndPackConfigDetailsForm:insertComboPairs("PostPickConfirmShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   END. 
   
   PickAndPackConfigDetailsForm:startRow().
   PickAndPackConfigDetailsForm:insertLabel(fTL("Post Pick Confirm Ship Status")).
   PickAndPackConfigDetailsForm:insertComboField("PostPickConfirmShipStatusID", "", 190, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY    ShipStatus.ListingSequence:
         
      PickAndPackConfigDetailsForm:insertComboPairs("PostPickConfirmShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   END. 
   
   PickAndPackConfigDetailsForm:startRow().
   PickAndPackConfigDetailsForm:insertLabel(fTL("Print Pack Slip At Begin")).
   PickAndPackConfigDetailsForm:insertComboField("PrintPackSlipAtBegin", "", 190, TRUE).  
   PickAndPackConfigDetailsForm:insertComboPairs("PrintPackSlipAtBegin", "yes", "Yes").  
   PickAndPackConfigDetailsForm:insertComboPairs("PrintPackSlipAtBegin", "no", "No").  
   


   {webGetOptionalFormFields.i pPickAndPackConfigDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PickAndPackConfigDetailsForm:insertHiddenField("pickandpackconfig_browse_scroll", "").
   PickAndPackConfigDetailsForm:insertHiddenField("form_name", "pickandpackconfig_details_form").
   PickAndPackConfigDetailsForm:insertHiddenField("prog_name", "adPickAndPackConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickAndPackConfigDetailsForm}
   
   /* Create Button Bar */
   PickAndPackConfigDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataCreates THEN 
      PickAndPackConfigDetailsButtons:addButton("pickandpackconfig_details_form_btn_save", 
                                                fTL("Save"), 
                                                "updatePickAndPackConfig('pickandpackconfig_details_form');").
                                                
   PickAndPackConfigDetailsButtons:addButton("pickandpackconfig_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); disablePopup('pickandpackconfig_details_form_popup');").
   PickAndPackConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickAndPackConfigDetailsForm:FormButtons = PickAndPackConfigDetailsButtons.
   
   PickAndPackConfigDetailsForm:endForm(). 
   
   PickAndPackConfigDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pPickAndPackConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN 
    DO:
      PickAndPackConfigDetailsForm:startRow().
      PickAndPackConfigDetailsForm:insertLabel(fTL("Field Label")).
      PickAndPackConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pPickAndPackConfigHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "pickandpackconfighistory_details_form"}
   ASSIGN chrDisplayFieldList  = "PickAndPackConfigHistoryID,MaxWeightPercentageVariation,PostPackoutShipOrderStatusID,PostPackoutShipStatusID," +
                                 "PostPickConfirmShipOrderStatusID,PostPickConfirmShipStatusID,PrintPackSlipAtBegin," +
                                 "GateUserID,CreatedDate,CreatedHour,CreatedMins" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PickAndPackConfigHistoryDetailsForm = NEW dataForm("pickandpackconfighistory_details_form").
   PickAndPackConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PickAndPackConfigHistoryDetailsForm:FormAction = "dbPickAndPackConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PickAndPackConfigHistoryDetailsForm:FormWidth   = 460.
   PickAndPackConfigHistoryDetailsForm:FormHeight  = 300.
   PickAndPackConfigHistoryDetailsForm:FormTitle   = "PickAndPack Config History Details".
   PickAndPackConfigHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PickAndPackConfigHistoryDetailsForm:insertPaddingColumn(30).
   PickAndPackConfigHistoryDetailsForm:insertColumn(200).
   PickAndPackConfigHistoryDetailsForm:insertColumn(125).
   PickAndPackConfigHistoryDetailsForm:insertColumn(20).
   PickAndPackConfigHistoryDetailsForm:insertColumn(4).
   PickAndPackConfigHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("History ID")).
   PickAndPackConfigHistoryDetailsForm:insertTextField("PickAndPackConfigHistoryID", "", 190, TRUE).  
   
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Max Weight Variation (%)")).
   PickAndPackConfigHistoryDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 190, TRUE).  
   
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Post Packout ShipOrder Status")).
   PickAndPackConfigHistoryDetailsForm:insertComboField("PostPackoutShipOrderStatusID", "", 190, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY    ShipOrderStatus.ListingSequence:
         
      PickAndPackConfigHistoryDetailsForm:insertComboPairs("PostPackoutShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   END.
         
   
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Post Packout Ship Status")).
   PickAndPackConfigHistoryDetailsForm:insertComboField("PostPackoutShipStatusID", "", 190, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY    ShipStatus.ListingSequence:
         
      PickAndPackConfigHistoryDetailsForm:insertComboPairs("PostPackoutShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   END.  
   
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Post Pick Confirm ShipOrder Status")).
   PickAndPackConfigHistoryDetailsForm:insertComboField("PostPickConfirmShipOrderStatusID", "", 190, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active
      BY    ShipOrderStatus.ListingSequence:
         
      PickAndPackConfigHistoryDetailsForm:insertComboPairs("PostPickConfirmShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).
   END. 
   
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Post Pick Confirm Ship Status")).
   PickAndPackConfigHistoryDetailsForm:insertComboField("PostPickConfirmShipStatusID", "", 190, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active
      BY    ShipStatus.ListingSequence:
         
      PickAndPackConfigHistoryDetailsForm:insertComboPairs("PostPickConfirmShipStatusID", STRING(ShipStatus.ShipStatusID), ShipStatus.StatusName).
   END. 
   
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Print Pack Slip At Begin")).
   PickAndPackConfigHistoryDetailsForm:insertComboField("PrintPackSlipAtBegin", "", 190, TRUE).  
   PickAndPackConfigHistoryDetailsForm:insertComboPairs("PrintPackSlipAtBegin", "yes", "Yes").  
   PickAndPackConfigHistoryDetailsForm:insertComboPairs("PrintPackSlipAtBegin", "no", "No").  
   
   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Created")).
   PickAndPackConfigHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).  
   /* Time fields have no label */
   PickAndPackConfigHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   PickAndPackConfigHistoryDetailsForm:insertLabel(":").
   PickAndPackConfigHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE). 

   PickAndPackConfigHistoryDetailsForm:startRow().
   PickAndPackConfigHistoryDetailsForm:insertLabel(fTL("Gate User ID")).
   PickAndPackConfigHistoryDetailsForm:insertComboField("GateUserID", "", 190, TRUE).   
   FOR EACH GateUser NO-LOCK 
      BY GateUser.FullName:
      PickAndPackConfigHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.

   {webGetOptionalFormFields.i pPickAndPackConfigHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PickAndPackConfigHistoryDetailsForm:insertHiddenField("pickandpackconfighistory_browse_scroll", "").
   PickAndPackConfigHistoryDetailsForm:insertHiddenField("form_name", "pickandpackconfighistory_details_form").
   PickAndPackConfigHistoryDetailsForm:insertHiddenField("prog_name", "adPickAndPackConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickAndPackConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   PickAndPackConfigHistoryDetailsButtons = NEW buttonBar().

   PickAndPackConfigHistoryDetailsButtons:addButton("pickandpackconfighistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "disablePopup('pickandpackconfighistory_details_form_popup');").
                                    
   PickAndPackConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickAndPackConfigHistoryDetailsForm:FormButtons = PickAndPackConfigHistoryDetailsButtons.
   
   PickAndPackConfigHistoryDetailsForm:endForm(). 
   
   PickAndPackConfigHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pPickAndPackConfigHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      PickAndPackConfigDetailsForm:startRow().
      PickAndPackConfigDetailsForm:insertLabel(fTL("Field Label")).
      PickAndPackConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adPickAndPackConfig_pickandpackconfig_details_form.i}*/ COMEBACK START COMEBACK END*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pPickAndPackConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   PickAndPackConfigHistoryBrowseForm           = NEW dataForm("pickandpackconfighistory_browse_form").
   PickAndPackConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PickAndPackConfigHistoryBrowseForm:FormWidth  = 860.
   PickAndPackConfigHistoryBrowseForm:FormHeight = 530.
   PickAndPackConfigHistoryBrowseForm:FormTitle  = fTL("PickAndPack Config History").
   PickAndPackConfigHistoryBrowseForm:FormType   = "xxl_large".
   PickAndPackConfigHistoryBrowse                = NEW browseTable("pickandpackconfighistory_browse").
   PickAndPackConfigHistoryBrowse:BrowseWidth    = 840.
   PickAndPackConfigHistoryBrowse:BrowseHeight   = 490.
   
   PickAndPackConfigHistoryBrowse:insertColumn(fTL("History ID"),               100, "INTEGER",           FALSE).
   PickAndPackConfigHistoryBrowse:insertColumn(fTL("Max Weight Variation(%)"),  175, "DECIMAL",           FALSE).
   PickAndPackConfigHistoryBrowse:insertColumn(fTL("Print Pack Slip At Begin"), 200, "LOGICAL",           FALSE).
   PickAndPackConfigHistoryBrowse:insertColumn(fTL("User"),                     175, "CHARACTER", "LEFT", FALSE).
   PickAndPackConfigHistoryBrowse:insertColumn(fTL("Created"),                  150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickAndPackConfigHistory}
   
   PickAndPackConfigHistoryBrowse:StartBody().
   
   FOR EACH PickAndPackConfigHistory NO-LOCK
      WHERE PickAndPackConfigHistory.PickAndPackConfigID = intSelectedPickAndPackConfig
      BY    PickAndPackConfigHistory.PickAndPackConfigHistoryID:
          
      FIND FIRST OperationType OF PickAndPackConfigHistory NO-LOCK NO-ERROR.
      FIND FIRST GateUser      OF PickAndPackConfigHistory NO-LOCK NO-ERROR.

      PickAndPackConfigHistoryBrowse:startRow (PickAndPackConfigHistory.PickAndPackConfigHistoryID, 
         "selectPickAndPackConfigHistoryRow(this," + '"' + STRING(PickAndPackConfigHistory.PickAndPackConfigHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PickAndPackConfigHistory}
      
      PickAndPackConfigHistoryBrowse:insertData(PickAndPackConfigHistory.PickAndPackConfigHistoryID).
      PickAndPackConfigHistoryBrowse:insertData(PickAndPackConfigHistory.MaxWeightPercentageVariation).
      PickAndPackConfigHistoryBrowse:insertData(STRING(PickAndPackConfigHistory.PrintPackSlipAtBegin, "Yes/No")).
      PickAndPackConfigHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      PickAndPackConfigHistoryBrowse:insertData(fDisplayDate&Time(PickAndPackConfigHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      PickAndPackConfigHistoryBrowse:endRow().
   END. /* FOR EACH PickAndPackConfigHistory */
   
   PickAndPackConfigHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PickAndPackConfigHistoryBrowse:getErrors().
   
   PickAndPackConfigHistoryBrowseForm:insertHiddenField("popup_pickandpackconfighistory_browse","").
   PickAndPackConfigHistoryBrowseForm:insertHiddenField("PickAndPackConfigHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickAndPackConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   PickAndPackConfigHistoryButtons = NEW buttonBar().
   
   PickAndPackConfigHistoryButtons:addButton("pickandpackconfighistory_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewPickAndPackConfigHistoryDetails('pickandpackconfighistory_details_form');",
                                             (IF intSelectedPickAndPackConfigHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   PickAndPackConfigHistoryButtons:addButton("pickandpackconfighistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('pickandpackconfighistory_browse_form_popup');").
   PickAndPackConfigHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickAndPackConfigHistoryBrowseForm:FormBrowse  = PickAndPackConfigHistoryBrowse.
   PickAndPackConfigHistoryBrowseForm:FormButtons = PickAndPackConfigHistoryButtons.
   PickAndPackConfigHistoryBrowseForm:endForm(). 
   
   PickAndPackConfigHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


