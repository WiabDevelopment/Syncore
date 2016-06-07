&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adNetworkActionCommand.p

  Description: Admin screen for NetworkAction Command words, and linking of them to NetworkActionPoints
               as well as their resulting value to be sent.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Todd Wierzchowski

  Created: 26/10/2015

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
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedNetworkActionCommand        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkActionCommandRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkActionCommandRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkActionCommandID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupNetworkActionCommandHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE NetworkActionCommandBrowseFrame        AS pageFrame.
DEFINE VARIABLE NetworkActionCommandBrowse             AS browseTable.
DEFINE VARIABLE NetworkActionCommandBrowseButtons      AS buttonBar.
DEFINE VARIABLE NetworkActionCommandDetailsForm        AS dataForm.
DEFINE VARIABLE NetworkActionCommandDetailsButtons     AS buttonBar.

DEFINE VARIABLE NetworkActionCommandHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE NetworkActionCommandHistoryBrowse         AS browseTable.
DEFINE VARIABLE NetworkActionCommandHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE NetworkActionCommandHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE NetworkActionCommandHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrNetworkActionCommandID          = get-value("NetworkActionCommandID")
          intSelectedNetworkActionCommand    = INTEGER(chrNetworkActionCommandID)
          chrScrollToNetworkActionCommandRow = STRING(INTEGER(get-value("networkactioncommand_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrNetworkActionCommandID <> "" THEN
     chrSelectNetworkActionCommandRow = 'selectNetworkActionCommandRow(document.getElementById("networkactioncommand_browse_row_' + chrNetworkActionCommandID + '"),"' 
                                                         + chrNetworkActionCommandID +  '");'.
   
   IF get-value('popup_networkactioncommandhistory_browse') = "yes" THEN
      chrPopupNetworkActionCommandHistory  = 'enablePopup("networkactioncommandhistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkactioncommand_browse").scrollTop=' + chrScrollToNetworkActionCommandRow 
                                                          + chrSelectNetworkActionCommandRow + chrPopupNetworkActionCommandHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "NetworkActionCommand Admin".
   ThisPage:FrameTitle    = "NetworkActionCommand Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("networkactioncommand.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkActionCommandBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pNetworkActionCommandDetails.
   RUN pNetworkActionCommandHistory.
   RUN pNetworkActionCommandHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Setup SynCore control of create/updates */
   {webDataMigrationSecurity.i}
   
   /* Delete objects definied locally */
   DELETE OBJECT NetworkActionCommandBrowseFrame           NO-ERROR.
   DELETE OBJECT NetworkActionCommandBrowse                NO-ERROR.
   DELETE OBJECT NetworkActionCommandBrowseButtons         NO-ERROR.
   DELETE OBJECT NetworkActionCommandDetailsForm           NO-ERROR.
   DELETE OBJECT NetworkActionCommandDetailsButtons        NO-ERROR.
   
   DELETE OBJECT NetworkActionCommandHistoryBrowseForm     NO-ERROR.   
   DELETE OBJECT NetworkActionCommandHistoryBrowse         NO-ERROR.
   DELETE OBJECT NetworkActionCommandHistoryBrowseButtons  NO-ERROR.
   
   DELETE OBJECT NetworkActionCommandHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT NetworkActionCommandHistoryDetailsButtons NO-ERROR.
   
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkErrorBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionCommandBrowse Procedure 
PROCEDURE pNetworkActionCommandBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkactioncommand_details_form"}
   
   NetworkActionCommandBrowse = NEW browseTable("networkactioncommand_browse").
   NetworkActionCommandBrowse:BrowseWidth  = 965.
   NetworkActionCommandBrowse:BrowseHeight = 455.
   NetworkActionCommandBrowse:WebStream = STREAM WebStream:HANDLE.
   
      /* Create a new frame */
   NetworkActionCommandBrowseFrame = NEW pageFrame().
   NetworkActionCommandBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkActionCommandBrowseFrame:FormAction= "".
   NetworkActionCommandBrowseFrame:formOpen("networkactioncommand_browse_form").
   
   
   /* Add in the ID as first Column */
   NetworkActionCommandBrowse:insertColumn(fTL("Command ID"), 100, "INTEGER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionCommand}
   
   NetworkActionCommandBrowse:insertColumn(fTL("Command Code"),  150, "CHARACTER", "LEFT", FALSE).
   NetworkActionCommandBrowse:insertColumn(fTL("Command Name"),  150, "CHARACTER", "LEFT", FALSE).
   NetworkActionCommandBrowse:insertColumn(fTL("Command Descr"), 200, "CHARACTER", "LEFT", FALSE).
   
   /*Body*/
   NetworkActionCommandBrowse:startBody().
   
   FOR EACH NetworkActionCommand NO-LOCK /*idx=ActiveListingSequence*/
      BY    NetworkActionCommand.NetworkActionCommandID:
         
      NetworkActionCommandBrowse:startRow(NetworkActionCommand.NetworkActionCommandID, "selectNetworkActionCommandRow(this," + '"' + STRING(NetworkActionCommand.NetworkActionCommandID) 
                                                                  + '"' + ");", "").
      NetworkActionCommandBrowse:insertData(NetworkActionCommand.NetworkActionCommandID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkActionCommand}
      
      NetworkActionCommandBrowse:insertData(NetworkActionCommand.CommandCode, "left").
      NetworkActionCommandBrowse:insertData(NetworkActionCommand.CommandName, "left").
      NetworkActionCommandBrowse:insertData(NetworkActionCommand.CommandDescr, "left").
      
      /* Add hidden fields */
      NetworkActionCommandBrowse:insertHiddenData("NetworkActionCommandActionVersionID",NetworkActionCommand.VersionID).
      
      NetworkActionCommandBrowse:endRow().
      
   END. /*FOR EACH NetworkActionCommand NO-LOCK */
   
   NetworkActionCommandBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkActionCommandBrowse:getErrors().
   
   
   /* Start the Frame Header */
   NetworkActionCommandBrowseFrame:insertSpacer(5).
   NetworkActionCommandBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkActionCommandBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkActionCommandBrowseFrame:frameClose().
   NetworkActionCommandBrowseFrame:insertSpacer(10).
   
   NetworkActionCommandBrowseFrame:insertHiddenField("networkactioncommand_browse_scroll","").
   NetworkActionCommandBrowseFrame:insertHiddenField("NetworkActionCommandID","").
   NetworkActionCommandBrowseFrame:insertHiddenField("NetworkActionCommandVersionID","").
   NetworkActionCommandBrowseFrame:insertHiddenField("popup_networkactioncommandhistory_browse","").
   NetworkActionCommandBrowseFrame:insertHiddenField("form_name","networkactioncommand_browse_form").
   NetworkActionCommandBrowseFrame:insertHiddenField("prog_name","adNetworkActionCommand.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionCommandBrowseFrame}
   
   NetworkActionCommandBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkActionCommandBrowseButtons = NEW buttonBar().
   NetworkActionCommandBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   NetworkActionCommandBrowseButtons:addButton("networkactioncommand_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewNetworkActionCommandDetails('networkactioncommand_details_form');",
                                             "Disabled").
   
/*   NetworkActionCommandBrowseButtons:addButton("networkactioncommand_browse_form_btn_create",                   */
/*                                             fTL("Create"),                                     */
/*                                             "createNetworkActionCommand('networkactioncommand_details_form');",*/
/*                                             "").                                               */
                                             
   NetworkActionCommandBrowseButtons:addButton("networkactioncommand_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   NetworkActionCommandBrowseButtons:addButton("networkactioncommand_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteNetworkActionCommand();",
                                             (IF intSelectedNetworkActionCommand > 0 THEN "" ELSE "Disabled")).
   */
   
   NetworkActionCommandBrowseButtons:closeBar().  
   NetworkActionCommandBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionCommandDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionCommandDetails Procedure 
PROCEDURE pNetworkActionCommandDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkactioncommand_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkActionCommandID,CommandCode,CommandName,CommandDescr"
          chrEditFieldList     = "CommandName,CommandDescr"
          chrNewFieldList      = "CommandCode,CommandName,CommandDescr" 
          chrRequiredFieldList = "CommandCode,CommandName,CommandDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkActionCommandDetailsForm = NEW dataForm("networkactioncommand_details_form").
   NetworkActionCommandDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkActionCommandDetailsForm:FormAction = "dbNetworkActionCommandUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkActionCommandDetailsForm:FormWidth   = 580.
   NetworkActionCommandDetailsForm:FormHeight  = 420.
   NetworkActionCommandDetailsForm:FormTitle   = "NetworkActionCommand Details".
   NetworkActionCommandDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkActionCommandDetailsForm:insertPaddingColumn(30).
   NetworkActionCommandDetailsForm:insertColumn(150).
   NetworkActionCommandDetailsForm:insertColumn(160).
   NetworkActionCommandDetailsForm:insertColumn(20).
   NetworkActionCommandDetailsForm:insertColumn(4).
   NetworkActionCommandDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkActionCommandDetailsForm:startRow().
   NetworkActionCommandDetailsForm:insertLabel("Command ID").
   NetworkActionCommandDetailsForm:insertTextField("NetworkActionCommandID", "", 200, TRUE). 
   
   NetworkActionCommandDetailsForm:startRow().
   NetworkActionCommandDetailsForm:insertLabel("CommandCode").
   NetworkActionCommandDetailsForm:insertTextField("CommandCode", "", 200, TRUE).
   
   NetworkActionCommandDetailsForm:startRow().
   NetworkActionCommandDetailsForm:insertLabel("CommandName").
   NetworkActionCommandDetailsForm:insertTextField("CommandName", "", 200, TRUE).
   
   NetworkActionCommandDetailsForm:startRow().
   NetworkActionCommandDetailsForm:insertLabel("CommandDescr").
   NetworkActionCommandDetailsForm:insertTextField("CommandDescr", "", 200, TRUE).
   
   {webGetOptionalFormFields.i pNetworkActionCommandDetailsFields}
   
   /* Add Hidden Fields*/
   NetworkActionCommandDetailsForm:insertHiddenField("networkactioncommand_browse_scroll", "").
   NetworkActionCommandDetailsForm:insertHiddenField("form_name", "network_action_command_details_form").
   NetworkActionCommandDetailsForm:insertHiddenField("prog_name", "adNetworkActionCommand.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionCommandDetailsForm}
   
   /* Create Button Bar */
   NetworkActionCommandDetailsButtons = NEW buttonBar().
   
/*   NetworkActionCommandDetailsButtons:addButton("networkactioncommand_details_form_btn_save",                     */
/*                                              fTL("Save"),                                        */
/*                                              "updateNetworkActionCommand('networkactioncommand_details_form');").*/
   
   NetworkActionCommandDetailsButtons:addButton("networkactioncommand_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('networkactioncommand_details_form_popup');").
   
   NetworkActionCommandDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionCommandDetailsForm:FormButtons = NetworkActionCommandDetailsButtons.
   
   NetworkActionCommandDetailsForm:endForm(). 
   
   NetworkActionCommandDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkActionCommandDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionCommandDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionCommandDetailsFields Procedure 
PROCEDURE pNetworkActionCommandDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         NetworkActionCommandDetailsForm:startRow().
         NetworkActionCommandDetailsForm:insertLabel(fTL("Field Label")).
         NetworkActionCommandDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adNetworkActionCommand_networkactioncommand_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeHistory Procedure
PROCEDURE pNetworkActionCommandHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "networkactioncommandhistory_details_form"}
   
   FIND FIRST NetworkActionCommand WHERE NetworkActionCommand.NetworkActionCommandID = intSelectedNetworkActionCommand NO-LOCK NO-ERROR.
   
   NetworkActionCommandHistoryBrowseForm = NEW dataForm("networkactioncommandhistory_browse_form").
   NetworkActionCommandHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkActionCommandHistoryBrowseForm:FormWidth   = 850.
   NetworkActionCommandHistoryBrowseForm:FormHeight  = 540.
   NetworkActionCommandHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE NetworkActionCommand THEN " for NetworkActionCommandID: " 
                                                                  + STRING(NetworkActionCommand.NetworkActionCommandID) ELSE "").
   NetworkActionCommandHistoryBrowseForm:FormType    = "xxl_large".
   
   NetworkActionCommandHistoryBrowse = NEW browseTable("networkactioncommandhistory_browse").
   NetworkActionCommandHistoryBrowse:BrowseWidth  = 830.
   NetworkActionCommandHistoryBrowse:BrowseHeight = 500.
   NetworkActionCommandHistoryBrowse:ExcelExport  = TRUE.
   NetworkActionCommandHistoryBrowse:SessionID    = intGblSessionID.
   
   
   NetworkActionCommandHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionCommandHistory}

   NetworkActionCommandHistoryBrowse:insertColumn(fTL("CommandID"),    100, "INTEGER",   "left", FALSE).  
   NetworkActionCommandHistoryBrowse:insertColumn(fTL("CommandCode"),  100, "INTEGER",   "left", FALSE).   
   NetworkActionCommandHistoryBrowse:insertColumn(fTL("CommandName"),  100, "INTEGER",   "left", FALSE).    
   NetworkActionCommandHistoryBrowse:insertColumn(fTL("CommandDescr"), 100, "CHARACTER", "left", FALSE).
   NetworkActionCommandHistoryBrowse:insertColumn(fTL("User"),         100, "CHARACTER", "left", FALSE).
   NetworkActionCommandHistoryBrowse:insertColumn(fTL("Created"),      100, "CHARACTER", "left", FALSE).
   
   NetworkActionCommandHistoryBrowse:StartBody().
   
   IF AVAILABLE NetworkActionCommand THEN
   DO:
      /*List the NetworkActionCommandHistory*/
      FOR EACH NetworkActionCommandHistory NO-LOCK 
         WHERE  NetworkActionCommandHistory.NetworkActionCommandID = intSelectedNetworkActionCommand
         BY NetworkActionCommandHistory.NetworkActionCommandHistoryID:
         
         FIND FIRST GateUser OF NetworkActionCommandHistory NO-LOCK NO-ERROR.
       
         NetworkActionCommandHistoryBrowse:startRow(NetworkActionCommandHistory.NetworkActionCommandHistoryID, "selectHistoryRow(this," + '"' + STRING(NetworkActionCommandHistory.NetworkActionCommandHistoryID) 
                                                                     + '","NetworkActionCommandHistory"' + ");", "").
         NetworkActionCommandHistoryBrowse:insertData(NetworkActionCommandHistory.NetworkActionCommandHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i NetworkActionCommandHistory}
         
         NetworkActionCommandHistoryBrowse:insertData(NetworkActionCommandHistory.NetworkActionCommandID, "left").
         NetworkActionCommandHistoryBrowse:insertData(NetworkActionCommandHistory.CommandCode, "left").
         NetworkActionCommandHistoryBrowse:insertData(NetworkActionCommandHistory.CommandName, "left").
         NetworkActionCommandHistoryBrowse:insertData(NetworkActionCommandHistory.CommandDescr, "left").
         NetworkActionCommandHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         NetworkActionCommandHistoryBrowse:insertData(fDisplayDate&Time(NetworkActionCommandHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         NetworkActionCommandHistoryBrowse:insertHiddendata("NetworkActionCommandHistoryID",NetworkActionCommandHistory.NetworkActionCommandHistoryID).
         
         NetworkActionCommandHistoryBrowse:endRow().
      
      END. /* FOR EACH NetworkActionCommandHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   NetworkActionCommandHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkActionCommandHistoryBrowse:getErrors().
   
   NetworkActionCommandHistoryBrowseForm:insertHiddenField("NetworkActionCommandHistoryID","").
   NetworkActionCommandHistoryBrowseForm:insertHiddenField("popup_networkactioncommandhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionCommandHistoryBrowseForm}
   
   /* Create Button Bar */
   NetworkActionCommandHistoryBrowseButtons = NEW buttonBar().                                                 
   
   NetworkActionCommandHistoryBrowseButtons:addButton("networkactioncommandhistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewNetworkActionCommandHistoryDetails('networkactioncommandhistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   NetworkActionCommandHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   NetworkActionCommandHistoryBrowseButtons:addButton("networkactioncommandhistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('networkactioncommandhistory_browse_form_popup');").
   
   NetworkActionCommandHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionCommandHistoryBrowseForm:FormBrowse  = NetworkActionCommandHistoryBrowse.
   NetworkActionCommandHistoryBrowseForm:FormButtons = NetworkActionCommandHistoryBrowseButtons.
   NetworkActionCommandHistoryBrowseForm:endForm(). 
   
   NetworkActionCommandHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionCommandHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionCommandHistoryDetails Procedure
PROCEDURE pNetworkActionCommandHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "networkactioncommandhistory_details_form"}
   
   chrDisplayFieldList  = "NetworkActionCommandHistoryID,NetworkActionCommandID,CommandCode,CommandName,CommandDescr"
                          + ",CreatedDate,CreatedHour,CreatedMins,GateUserID,TransactionID".
                             
   
   NetworkActionCommandHistoryDetailsForm = NEW dataForm("networkactioncommandhistory_details_form").
   NetworkActionCommandHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   NetworkActionCommandHistoryDetailsForm:FormWidth   = 545.
   NetworkActionCommandHistoryDetailsForm:FormHeight  = 440.
   NetworkActionCommandHistoryDetailsForm:FormTitle   = "Network Error History Details".
   NetworkActionCommandHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkActionCommandHistoryDetailsForm:insertPaddingColumn(40).
   NetworkActionCommandHistoryDetailsForm:insertColumn(110).
   NetworkActionCommandHistoryDetailsForm:insertColumn(120).
   NetworkActionCommandHistoryDetailsForm:insertColumn(20).
   NetworkActionCommandHistoryDetailsForm:insertColumn(4).
   NetworkActionCommandHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   NetworkActionCommandHistoryDetailsForm:startRow().
   NetworkActionCommandHistoryDetailsForm:insertLabel(fTL("History ID")).
   NetworkActionCommandHistoryDetailsForm:insertTextField("NetworkActionCommandHistoryID", "", 200, TRUE).    
   
   NetworkActionCommandHistoryDetailsForm:startRow().
   NetworkActionCommandHistoryDetailsForm:insertLabel("Command ID").
   NetworkActionCommandHistoryDetailsForm:insertTextField("NetworkActionCommandID", "", 200, TRUE). 
   
   NetworkActionCommandHistoryDetailsForm:startRow().
   NetworkActionCommandHistoryDetailsForm:insertLabel("CommandCode").
   NetworkActionCommandHistoryDetailsForm:insertTextField("CommandCode", "", 200, TRUE).
   
   NetworkActionCommandHistoryDetailsForm:startRow().
   NetworkActionCommandHistoryDetailsForm:insertLabel("CommandName").
   NetworkActionCommandHistoryDetailsForm:insertTextField("CommandName", "", 200, TRUE).
   
   NetworkActionCommandHistoryDetailsForm:startRow().
   NetworkActionCommandHistoryDetailsForm:insertLabel("CommandDescr").
   NetworkActionCommandHistoryDetailsForm:insertTextField("CommandDescr", "", 200, TRUE).
   
   NetworkActionCommandHistoryDetailsForm:startRow().
   NetworkActionCommandHistoryDetailsForm:insertLabel("User").
   NetworkActionCommandHistoryDetailsForm:insertComboField("GateUserID", "", 110, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      NetworkActionCommandHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   NetworkActionCommandHistoryDetailsForm:startRow().
   NetworkActionCommandHistoryDetailsForm:insertLabel("Created").
   NetworkActionCommandHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   NetworkActionCommandHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkActionCommandHistoryDetailsForm:insertLabel(":").
   NetworkActionCommandHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   NetworkActionCommandHistoryDetailsForm:insertHiddenField("networkactioncommand_browse_scroll","").
   NetworkActionCommandHistoryDetailsForm:insertHiddenField("popup_networkactioncommandhistory_browse", "").
   NetworkActionCommandHistoryDetailsForm:insertHiddenField("NetworkActionCommandHistoryID","").
   NetworkActionCommandHistoryDetailsForm:insertHiddenField("form_name","networkactioncommandhistory_details_form").
   NetworkActionCommandHistoryDetailsForm:insertHiddenField("prog_name","adNetworkActionCommand.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionCommandHistoryDetailsForm}
   
   /* Create Button Bar */
   NetworkActionCommandHistoryDetailsButtons = NEW buttonBar().
   
   NetworkActionCommandHistoryDetailsButtons:addButton("networkactioncommandhistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('networkactioncommandhistory_details_form_popup');").
                                        
   NetworkActionCommandHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionCommandHistoryDetailsForm:FormButtons = NetworkActionCommandHistoryDetailsButtons.
   
   NetworkActionCommandHistoryDetailsForm:endForm(). 
   NetworkActionCommandHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

