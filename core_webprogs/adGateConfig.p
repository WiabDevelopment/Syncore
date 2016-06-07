&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adGateConfig.p 

  Description: ad file for the GateConfig Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Anthony Ferrari

  Created: 20/07/2015

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
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedGateConfig           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedGateConfigHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrGateConfigHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrGateConfigRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToGateConfigRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToGateConfigHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrGateConfigID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                 AS CHARACTER   NO-UNDO.

/* Buffers */
/* Objects */
DEFINE VARIABLE GateConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE GateConfigBrowse                AS browseTable.
DEFINE VARIABLE GateConfigBrowseButtons         AS buttonBar.
DEFINE VARIABLE GateConfigDetailsForm           AS dataForm.
DEFINE VARIABLE GateConfigDetailsButtons        AS buttonBar.

DEFINE VARIABLE GateConfigHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE GateConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE GateConfigHistoryButtons        AS buttonBar.
DEFINE VARIABLE GateConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE GateConfigHistoryDetailsButtons AS buttonBar.


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
   
   ASSIGN chrGateConfigID = get-value("GateConfigID")
          intSelectedGateConfig = INTEGER(chrGateConfigID)
          chrScrollToGateConfigRow = STRING(INTEGER(get-value("gateconfig_browse_scroll"))) + ";"
          /*History details button*/
          chrGateConfigHistoryID = get-value("GateConfigHistoryID")
          intSelectedGateConfigHistory = INTEGER(chrGateConfigHistoryID)
          chrScrollToGateConfigHistoryRow = STRING(INTEGER(get-value("gateconfighistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrGateConfigID <> "" THEN
      chrGateConfigRow = 'selectGateConfigRow(document.getElementById("gateconfig_browse_row_'
                               + chrGateConfigID + '"),"' + chrGateConfigID +  '");'.
                                                          
   IF get-value('popup_gateconfighistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("gateconfighistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("gateconfig_browse").scrollTop=' + chrScrollToGateConfigRow 
                                                           + chrGateConfigRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "GateConfig Admin".
   ThisPage:FrameTitle = "GateConfig Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for GateConfig Admin */
   ThisPage:addJavaScript("gateconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pGateConfigBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pGateConfigDetails.
   
   RUN pGateConfigHistoryBrowse.
   
   RUN pGateConfigHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT GateConfigBrowseFrame        NO-ERROR.
   DELETE OBJECT GateConfigBrowse             NO-ERROR.
   DELETE OBJECT GateConfigBrowseButtons      NO-ERROR.
   DELETE OBJECT GateConfigDetailsForm        NO-ERROR.
   DELETE OBJECT GateConfigDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pGateConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "gateconfig_details_form"}
   
   GateConfigBrowse = NEW browseTable("gateconfig_browse").
   GateConfigBrowse:BrowseWidth  = 965.
   GateConfigBrowse:BrowseHeight = 455.
   GateConfigBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   GateConfigBrowse:insertColumn(fTL("GateConfigID"), 150, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i GateConfig}
   
   GateConfigBrowse:insertColumn(fTL("Translations Being Written"), 235, "LOGICAL", "LEFT", FALSE).   
   GateConfigBrowse:insertColumn(fTL("Allow Direct Logins"),        235, "LOGICAL", "LEFT", FALSE). 
   
   
   /*Body*/
   GateConfigBrowse:startBody().
   
   FOR EACH GateConfig NO-LOCK: /*idx=GateConfigID*/
            
      GateConfigBrowse:startRow(GateConfig.GateConfigID, "selectGateConfigRow(this," + '"' 
                                       + STRING(GateConfig.GateConfigID) + '"' + ");", "").
                                     
      GateConfigBrowse:insertData(GateConfig.GateConfigID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i GateConfig}      
 
      GateConfigBrowse:insertData(STRING(GateConfig.TranslationsBeingWritten, "Yes/No"),  "LEFT"). 
      GateConfigBrowse:insertData(STRING(GateConfig.AllowDirectLogins, "Yes/No"), "LEFT").
                  
      /* Add hidden fields */
      GateConfigBrowse:insertHiddenData("GateConfigVersionID",GateConfig.VersionID).
      
      GateConfigBrowse:endRow().
      
   END. /*FOR EACH GateConfig NO-LOCK */
   
   GateConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + GateConfigBrowse:getErrors().
   
   /* Create a new frame */
   GateConfigBrowseFrame = NEW pageFrame().
   GateConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   GateConfigBrowseFrame:FormAction="dbGateConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   GateConfigBrowseFrame:formOpen("gateconfig_browse_form").
   
   /* Start the Frame Header */
   GateConfigBrowseFrame:insertSpacer(5).
   GateConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   GateConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   GateConfigBrowseFrame:frameClose().
   GateConfigBrowseFrame:insertSpacer(10).
   
   GateConfigBrowseFrame:insertHiddenField("gateconfig_browse_scroll","").
   GateConfigBrowseFrame:insertHiddenField("GateConfigID","").
   GateConfigBrowseFrame:insertHiddenField("GateConfigVersionID","").
   GateConfigBrowseFrame:insertHiddenField("form_name","gateconfig_browse_form").
   GateConfigBrowseFrame:insertHiddenField("popup_gateconfighistory_browse","").
   GateConfigBrowseFrame:insertHiddenField("prog_name","adGateConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i GateConfigBrowseFrame}
   
   GateConfigBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   GateConfigBrowseButtons = NEW buttonBar().
   GateConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   GateConfigBrowseButtons:addButton("gateconfig_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewGateConfigDetails('gateconfig_details_form');",
                                         (IF intSelectedGateConfig > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
   GateConfigBrowseButtons:addButton("gateconfig_browse_form_btn_create",
                                         fTL("Create"),
                                         "createGateConfig('gateconfig_details_form');",
                                         "").
   END.
   
   GateConfigBrowseButtons:addButton("gateconfig_browse_form_btn_history",
                                         fTL("History"),
                                         "viewGateConfigHistory('gateconfighistory_browse_form');",
                                         (IF intSelectedGateConfig > 0 THEN "" ELSE "Disabled")).
   
   GateConfigBrowseButtons:closeBar().  
   GateConfigBrowseButtons:displayButtonBar(). 
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pGateConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "gateconfig_details_form"}
   ASSIGN chrDisplayFieldList  = "GateConfigID,AllowDirectLogins,TranslationsBeingWritten"
          chrEditFieldList     = "AllowDirectLogins,TranslationsBeingWritten"
          chrNewFieldList      = "AllowDirectLogins,TranslationsBeingWritten"
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   
   GateConfigDetailsForm = NEW dataForm("gateconfig_details_form").
   GateConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   GateConfigDetailsForm:FormAction = "dbGateConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   GateConfigDetailsForm:FormWidth   = 460.
   GateConfigDetailsForm:FormHeight  = 200.
   GateConfigDetailsForm:FormTitle   = "GateConfig Details".
   GateConfigDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   GateConfigDetailsForm:insertPaddingColumn(30).
   GateConfigDetailsForm:insertColumn(185).
   
   /* Fields */
   GateConfigDetailsForm:startRow().
   GateConfigDetailsForm:insertLabel(fTL("GateConfig ID")).
   GateConfigDetailsForm:insertTextField("GateConfigID", "", 190, TRUE).  
   
   GateConfigDetailsForm:startRow().
   GateConfigDetailsForm:insertLabel(fTL("Translations Being Written")).
   GateConfigDetailsForm:insertComboField("TranslationsBeingWritten", "", 190, TRUE).
   GateConfigDetailsForm:insertComboPairs("TranslationsBeingWritten", "yes", "Yes").
   GateConfigDetailsForm:insertComboPairs("TranslationsBeingWritten", "no", "No").
   
   
   GateConfigDetailsForm:startRow().
   GateConfigDetailsForm:insertLabel(fTL("Allow Direct Logins")).
   GateConfigDetailsForm:insertComboField("AllowDirectLogins", "", 190, TRUE).
   GateConfigDetailsForm:insertComboPairs("AllowDirectLogins", "yes", "Yes").    
   GateConfigDetailsForm:insertComboPairs("AllowDirectLogins", "no", "No"). 


   {webGetOptionalFormFields.i pGateConfigDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   GateConfigDetailsForm:insertHiddenField("gateconfig_browse_scroll", "").
   GateConfigDetailsForm:insertHiddenField("form_name", "gateconfig_details_form").
   GateConfigDetailsForm:insertHiddenField("prog_name", "adGateConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i GateConfigDetailsForm}
   
   /* Create Button Bar */
   GateConfigDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataUpdates THEN
   DO:
   GateConfigDetailsButtons:addButton("gateconfig_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateGateConfig('gateconfig_details_form');").
   END.
   GateConfigDetailsButtons:addButton("gateconfig_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode');" + 
                                    "disablePopup('gateconfig_details_form_popup');").
   GateConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   GateConfigDetailsForm:FormButtons = GateConfigDetailsButtons.
   
   GateConfigDetailsForm:endForm(). 
   
   GateConfigDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pGateConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      GateConfigDetailsForm:startRow().
      GateConfigDetailsForm:insertLabel(fTL("Field Label")).
      GateConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pGateConfigHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "gateconfighistory_details_form"}
   ASSIGN chrDisplayFieldList  = "GateConfigHistoryID,GateConfigID,TranslationsBeingWrittenAllowDirectLogins" 
                                 + ",CreatedDate,CreatedHour,CreatedMins,OperationTypeID,GateUserID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   GateConfigHistoryDetailsForm = NEW dataForm("gateconfighistory_details_form").
   GateConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   GateConfigHistoryDetailsForm:FormAction = "dbGateConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   GateConfigHistoryDetailsForm:FormWidth   = 460.
   GateConfigHistoryDetailsForm:FormHeight  = 300.
   GateConfigHistoryDetailsForm:FormTitle   = "GateConfig History Details".
   GateConfigHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   GateConfigHistoryDetailsForm:insertPaddingColumn(30).
   GateConfigHistoryDetailsForm:insertColumn(150).
   GateConfigHistoryDetailsForm:insertColumn(125).
   GateConfigHistoryDetailsForm:insertColumn(20).
   GateConfigHistoryDetailsForm:insertColumn(4).
   GateConfigHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   GateConfigHistoryDetailsForm:startRow().
   GateConfigHistoryDetailsForm:insertLabel(fTL("History ID")).
   GateConfigHistoryDetailsForm:insertTextField("GateConfigHistoryID", "", 100, TRUE).
   
   GateConfigHistoryDetailsForm:startRow().
   GateConfigHistoryDetailsForm:insertLabel(fTL("GateConfig ID")).
   GateConfigHistoryDetailsForm:insertTextField("GateConfigID", "", 100, TRUE).
   
   GateConfigHistoryDetailsForm:startRow().
   GateConfigHistoryDetailsForm:insertLabel(fTL("Translations Being Written")).
   GateConfigHistoryDetailsForm:insertComboField("TranslationsBeingWritten", "", 190, TRUE).
   GateConfigHistoryDetailsForm:insertComboPairs("TranslationsBeingWritten", "yes", "Yes").
   GateConfigHistoryDetailsForm:insertComboPairs("TranslationsBeingWritten", "no", "No").
   
   
   GateConfigHistoryDetailsForm:startRow().
   GateConfigHistoryDetailsForm:insertLabel(fTL("Allow Direct Logins")).
   GateConfigHistoryDetailsForm:insertComboField("AllowDirectLogins", "", 190, TRUE).
   GateConfigHistoryDetailsForm:insertComboPairs("AllowDirectLogins", "yes", "Yes").    
   GateConfigHistoryDetailsForm:insertComboPairs("AllowDirectLogins", "no", "No"). 

   GateConfigHistoryDetailsForm:startRow().                                                                                           
   GateConfigHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   GateConfigHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   GateConfigHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   GateConfigHistoryDetailsForm:insertLabel(":").                                                                                     
   GateConfigHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                                         
   GateConfigHistoryDetailsForm:startRow().                                                                                           
   GateConfigHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   GateConfigHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/                                                                                                      
      BY OperationType.OperationTypeID:                                                                                                  
      GateConfigHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                          OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   GateConfigHistoryDetailsForm:startRow().                                                                                           
   GateConfigHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   GateConfigHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*GateUserID*/                                                                                                        
      BY GateUser.FullName:                                                                                                              
      GateConfigHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pGateConfigHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   GateConfigHistoryDetailsForm:insertHiddenField("gateconfighistory_browse_scroll", "").
   GateConfigHistoryDetailsForm:insertHiddenField("form_name", "gateconfighistory_details_form").
   GateConfigHistoryDetailsForm:insertHiddenField("prog_name", "adGateConfig.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i GateConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   GateConfigHistoryDetailsButtons = NEW buttonBar().

   GateConfigHistoryDetailsButtons:addButton("gateconfighistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "disablePopup('gateconfighistory_details_form_popup');").
   GateConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   GateConfigHistoryDetailsForm:FormButtons = GateConfigHistoryDetailsButtons.
   
   GateConfigHistoryDetailsForm:endForm(). 
   
   GateConfigHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pGateConfigHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      GateConfigHistoryDetailsForm:startRow().
      GateConfigHistoryDetailsForm:insertLabel(fTL("Field Label")).
      GateConfigHistoryDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adStockAdminOption_stockadminoption_details_form.i}*/ COMEBACK START COMEBACK END*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pGateConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   GateConfigHistoryBrowseForm           = NEW dataForm("gateconfighistory_browse_form").
   GateConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   GateConfigHistoryBrowseForm:FormWidth  = 860.
   GateConfigHistoryBrowseForm:FormHeight = 530.
   GateConfigHistoryBrowseForm:FormTitle  = fTL("GateConfig History").
   GateConfigHistoryBrowseForm:FormType   = "xxl_large".
   GateConfigHistoryBrowse                = NEW browseTable("gateconfighistory_browse").
   GateConfigHistoryBrowse:BrowseWidth    = 840.
   GateConfigHistoryBrowse:BrowseHeight   = 490.
   
   GateConfigHistoryBrowse:insertColumn(fTL("History ID"),                 100, "INTEGER", FALSE).
   GateConfigHistoryBrowse:insertColumn(fTL("Translations Being Written"), 200, "Logical", "LEFT", FALSE).
   GateConfigHistoryBrowse:insertColumn(fTL("Allow Direct Logins"),        200, "Logical", "LEFT", FALSE). 
   GateConfigHistoryBrowse:insertColumn(fTL("User"),                       150, "CHARACTER", "LEFT", FALSE).
   GateConfigHistoryBrowse:insertColumn(fTL("Created"),                    150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i GateConfigHistory}
   
   GateConfigHistoryBrowse:StartBody().
   
   FOR EACH GateConfigHistory NO-LOCK /*idx=GateConfigID and GateConfigHistoryID*/
      WHERE GateConfigHistory.GateConfigID = intSelectedGateConfig
         BY GateConfigHistory.GateConfigHistoryID:
          
      FIND FIRST OperationType OF GateConfigHistory NO-LOCK NO-ERROR. /*idx=OperationTypeID*/
      FIND FIRST GateUser      OF GateConfigHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/

      GateConfigHistoryBrowse:startRow (GateConfigHistory.GateConfigHistoryID, 
         "selectGateConfigHistoryRow(this," + '"' + STRING(GateConfigHistory.GateConfigHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i GateConfigHistory}
      
      GateConfigHistoryBrowse:insertData(GateConfigHistory.GateConfigID,                                   "").
      GateConfigHistoryBrowse:insertData(STRING(GateConfigHistory.TranslationsBeingWritten, "Yes/No"), "LEFT").
      GateConfigHistoryBrowse:insertData(STRING(GateConfigHistory.AllowDirectLogins, "Yes/No"),        "LEFT").
      GateConfigHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""),       "LEFT").
      GateConfigHistoryBrowse:insertData(fDisplayDate&Time(GateConfigHistory.Created,"y/m/d H:M:S"),   "LEFT").
      
      GateConfigHistoryBrowse:endRow().
   END. /* FOR EACH GateConfigHistory */
   
   GateConfigHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + GateConfigHistoryBrowse:getErrors().
   
   GateConfigHistoryBrowseForm:insertHiddenField("popup_gateconfighistory_browse","").
   GateConfigHistoryBrowseForm:insertHiddenField("GateConfigHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i GateConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   GateConfigHistoryButtons = NEW buttonBar().
   
   GateConfigHistoryButtons:addButton("gateconfighistory_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewGateConfigHistoryDetails('gateconfighistory_details_form');",
                                        (IF intSelectedGateConfigHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   GateConfigHistoryButtons:addButton("gateconfighistory_browse_form_btn_cancel",
                                        fTL("Cancel"),
                                        "disablePopup('gateconfighistory_browse_form_popup');").
   GateConfigHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   GateConfigHistoryBrowseForm:FormBrowse  = GateConfigHistoryBrowse.
   GateConfigHistoryBrowseForm:FormButtons = GateConfigHistoryButtons.
   GateConfigHistoryBrowseForm:endForm(). 
   
   GateConfigHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


