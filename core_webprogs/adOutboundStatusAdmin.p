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

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*           This .W file was created with the Progress AppBuilder.     */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     will execute in this procedure's storage, and that proper
     clean up will occur on deletion of the procedure. */
     
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Custom Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{defWebDefinitions.i}
{defDataMigrationVariables.i}


/* ShipOrderStatus Local Variables */
DEFINE VARIABLE intSelectedOutboundStatus    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectOutboundStatusRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToOutboundStatusRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOutboundStatusID          AS CHARACTER NO-UNDO.

/* ShipOrderStatus Objects */
DEFINE VARIABLE OutboundStatusBrowseFrame    AS pageFrame.
DEFINE VARIABLE OutboundStatusBrowse         AS browseTable.
DEFINE VARIABLE OutboundStatusBrowseButtons  AS buttonBar.
DEFINE VARIABLE OutboundStatusDetailsForm    AS dataForm.
DEFINE VARIABLE OutboundStatusDetailsButtons AS buttonBar.

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
  {prcDataMigrationProcedures.i}
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
   
   ASSIGN chrOutboundStatusID          = get-value("OutboundStatusID")
          intSelectedOutboundStatus    = INTEGER(chrOutboundStatusID)
          chrScrollToOutboundStatusRow = STRING(INTEGER(get-value("outboundstatus_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrOutboundStatusID <> "" THEN
      chrSelectOutboundStatusRow = 'selectOutboundStatusRow(document.getElementById("outboundstatus_browse_row_'
                                      + chrOutboundStatusID + '"),"' + chrOutboundStatusID +  '");'.
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad 
                   + 'document.getElementById("outboundstatus_browse").scrollTop=' + chrScrollToOutboundStatusRow
                   + chrSelectOutboundStatusRow.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Outbound Status Admin".
   ThisPage:FrameTitle    = "Outbound Status Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("outboundstatus.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
      
   /******* Main Browser ********************/
   RUN pOutboundStatusBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pOutboundStatusDetails.
   
   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT OutboundStatusBrowseFrame         NO-ERROR.
   DELETE OBJECT OutboundStatusBrowse              NO-ERROR.
   DELETE OBJECT OutboundStatusBrowseButtons       NO-ERROR.
   DELETE OBJECT OutboundStatusDetailsForm         NO-ERROR.
   DELETE OBJECT OutboundStatusDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusBrowse Procedure 
PROCEDURE pOutboundStatusBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "outboundstatus_details_form"}
   
   OutboundStatusBrowse              = NEW browseTable("outboundstatus_browse").
   OutboundStatusBrowse:BrowseWidth  = 965.
   OutboundStatusBrowse:BrowseHeight = 455.
   OutboundStatusBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   OutboundStatusBrowse:insertColumn(fTL("Status ID"),70, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i OutboundStatus}
   
   OutboundStatusBrowse:insertColumn(fTL("Listing"),      80, "INTEGER", FALSE).
   OutboundStatusBrowse:insertColumn(fTL("Status Code"), 120, "CHARACTER", "left", FALSE).
   OutboundStatusBrowse:insertColumn(fTL("Name"),        150, "CHARACTER", "left", FALSE).
   OutboundStatusBrowse:insertColumn(fTL("Description"), 305, "CHARACTER", "left", FALSE).
   OutboundStatusBrowse:insertColumn(fTL("Active"),       70, "LOGICAL", "left", FALSE).
   
   /*Body*/
   OutboundStatusBrowse:startBody().
   
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH OutboundStatus NO-LOCK
      BY    OutboundStatus.Active DESCENDING 
      BY    OutboundStatus.ListingSequence 
      BY    OutboundStatus.OutboundStatusID:
      
      OutboundStatusBrowse:startRow(OutboundStatus.OutboundStatusID, 
                                    "selectOutboundStatusRow(this," + '"' 
                                       + STRING(OutboundStatus.OutboundStatusID) + '"' + ");", "").
      OutboundStatusBrowse:insertData(OutboundStatus.OutboundStatusID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i OutboundStatus}

      OutboundStatusBrowse:insertData(STRING(OutboundStatus.ListingSequence)).
      OutboundStatusBrowse:insertData(OutboundStatus.StatusCode, "left").
      OutboundStatusBrowse:insertData(OutboundStatus.StatusName, "left").
      OutboundStatusBrowse:insertData(OutboundStatus.StatusDescr, "left").
      OutboundStatusBrowse:insertData(STRING(OutboundStatus.Active, "Yes/No")).
      
      /* Add hidden fields */
      OutboundStatusBrowse:insertHiddenData("OutboundStatusVersionID",OutboundStatus.VersionID).
      
      OutboundStatusBrowse:endRow().
      
   END. /* FOR EACH OutboundStatus NO-LOCK */
   
   OutboundStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + OutboundStatusBrowse:getErrors().
   
   /* Create a new frame */
   OutboundStatusBrowseFrame           = NEW pageFrame().
   OutboundStatusBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   OutboundStatusBrowseFrame:FormAction="dbOutboundStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   OutboundStatusBrowseFrame:formOpen("outboundstatus_browse_form").
   
   /* Start the Frame Header */
   OutboundStatusBrowseFrame:insertSpacer(5).
   OutboundStatusBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   OutboundStatusBrowse:displayBrowse().  
   
   /* End the Frame Header */
   OutboundStatusBrowseFrame:frameClose().
   OutboundStatusBrowseFrame:insertSpacer(10).
   
   OutboundStatusBrowseFrame:insertHiddenField("outboundstatus_browse_scroll","").
   OutboundStatusBrowseFrame:insertHiddenField("OutboundStatusID","").
   OutboundStatusBrowseFrame:insertHiddenField("OutboundStatusVersionID","").
   OutboundStatusBrowseFrame:insertHiddenField("form_name","outboundstatus_browse_form").
   OutboundStatusBrowseFrame:insertHiddenField("prog_name","adOutboundStatusAdmin.p").

   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OutboundStatusBrowseFrame}
   
   OutboundStatusBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   OutboundStatusBrowseButtons           = NEW buttonBar().
   OutboundStatusBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   OutboundStatusBrowseButtons:addButton("outboundstatus_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewOutboundStatusDetails('outboundstatus_details_form');",
                                         "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO: 
      OutboundStatusBrowseButtons:addButton("outboundstatus_browse_form_btn_create",
                                            fTL("Create"),
                                            "createOutboundStatus('outboundstatus_details_form');",
                                            "").
                                             
      OutboundStatusBrowseButtons:addButton("outboundstatus_browse_form_btn_applinks",
                                            fTL("App Links"),
                                            "viewAppLinkBrowse('outboundstatus_browse_form','OutboundStatus');",
                                            "Disabled").
                                             
   END.
   
   
   OutboundStatusBrowseButtons:closeBar().  
   OutboundStatusBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusDetails Procedure 
PROCEDURE pOutboundStatusDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "outboundstatus_details_form"}
   
   ASSIGN chrDisplayFieldList  = "OutboundStatusID,ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrEditFieldList     = "ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrNewFieldList      = "ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrRequiredFieldList = "ListingSequence,StatusCode,StatusName,StatusDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   OutboundStatusDetailsForm           = NEW dataForm("outboundstatus_details_form").
   OutboundStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   OutboundStatusDetailsForm:FormAction = "dbOutboundStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   OutboundStatusDetailsForm:FormWidth  = 460.
   OutboundStatusDetailsForm:FormHeight = 300.
   OutboundStatusDetailsForm:FormTitle  = "OutboundStatus Details".
   OutboundStatusDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   OutboundStatusDetailsForm:insertPaddingColumn(20).
   OutboundStatusDetailsForm:insertColumn(115).
   OutboundStatusDetailsForm:insertColumn(140).
   OutboundStatusDetailsForm:insertColumn(20).
   OutboundStatusDetailsForm:insertColumn(4).
   OutboundStatusDetailsForm:insertColumn(20).
   
   /* Fields */
   OutboundStatusDetailsForm:startRow().
   OutboundStatusDetailsForm:insertLabel("OutboundStatus ID").
   OutboundStatusDetailsForm:insertTextField("OutboundStatusID", "", 110, TRUE).  
   
   OutboundStatusDetailsForm:startRow().
   OutboundStatusDetailsForm:insertLabel("Listing Seq").
   OutboundStatusDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   OutboundStatusDetailsForm:startRow().
   OutboundStatusDetailsForm:insertLabel("Status Code").
   OutboundStatusDetailsForm:insertTextField("StatusCode", "", 200, TRUE).  
   
   OutboundStatusDetailsForm:startRow().
   OutboundStatusDetailsForm:insertLabel("Name").
   OutboundStatusDetailsForm:insertTextField("StatusName", "", 200, TRUE).
   
   OutboundStatusDetailsForm:startRow().
   OutboundStatusDetailsForm:insertLabel("Description").
   OutboundStatusDetailsForm:insertTextField("StatusDescr", "", 270, TRUE).
   
   OutboundStatusDetailsForm:startRow().
   OutboundStatusDetailsForm:insertLabel(fTL("Active")). 
   OutboundStatusDetailsForm:insertComboField("Active", "", 110, TRUE).  
   OutboundStatusDetailsForm:insertComboPairs("Active", "yes", "Active").
   OutboundStatusDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pShipOrderStatusSplitDetailsFields}
   
   /* Add Hidden Fields*/
   OutboundStatusDetailsForm:insertHiddenField("outboundstatus_browse_scroll", "").
   OutboundStatusDetailsForm:insertHiddenField("form_name", "outboundstatus_details_form").
   OutboundStatusDetailsForm:insertHiddenField("prog_name", "adOutboundStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OutboundStatusDetailsForm}
   
   /* Create Button Bar */
   OutboundStatusDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:
      OutboundStatusDetailsButtons:addButton("outboundstatus_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateOutboundStatus('outboundstatus_details_form');").
   END.     
   
   OutboundStatusDetailsButtons:addButton("outboundstatus_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode'); disablePopup('outboundstatus_details_form_popup');").
   
   OutboundStatusDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   OutboundStatusDetailsForm:FormButtons = OutboundStatusDetailsButtons.
   
   OutboundStatusDetailsForm:endForm(). 
   
   OutboundStatusDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusDetailsFields Procedure 
PROCEDURE pOutboundStatusDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         OutboundStatusDetailsForm:startRow().
         OutboundStatusDetailsForm:insertLabel(fTL("Field Label")).
         OutboundStatusDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adOutboundStatusAdmin_outboundstatus_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

