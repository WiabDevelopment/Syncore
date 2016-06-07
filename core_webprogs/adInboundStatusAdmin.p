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
DEFINE VARIABLE intSelectedInboundStatus    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectInboundStatusRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToInboundStatusRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrInboundStatusID          AS CHARACTER NO-UNDO.

/* ShipOrderStatus Objects */
DEFINE VARIABLE InboundStatusBrowseFrame    AS pageFrame.
DEFINE VARIABLE InboundStatusBrowse         AS browseTable.
DEFINE VARIABLE InboundStatusBrowseButtons  AS buttonBar.
DEFINE VARIABLE InboundStatusDetailsForm    AS dataForm.
DEFINE VARIABLE InboundStatusDetailsButtons AS buttonBar.

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
   
   ASSIGN chrInboundStatusID          = get-value("InboundStatusID")
          intSelectedInboundStatus    = INTEGER(chrInboundStatusID)
          chrScrollToInboundStatusRow = STRING(INTEGER(get-value("inboundstatus_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrInboundStatusID <> "" THEN
      chrSelectInboundStatusRow = 'selectInboundStatusRow(document.getElementById("inboundstatus_browse_row_'
                                      + chrInboundStatusID + '"),"' + chrInboundStatusID +  '");'.
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad 
                   + 'document.getElementById("inboundstatus_browse").scrollTop=' + chrScrollToInboundStatusRow
                   + chrSelectInboundStatusRow.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Inbound Status Admin".
   ThisPage:FrameTitle    = "Inbound Status Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("inboundstatus.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
      
   /******* Main Browser ********************/
   RUN pInboundStatusBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pInboundStatusDetails.
   
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
   DELETE OBJECT InboundStatusBrowseFrame         NO-ERROR.
   DELETE OBJECT InboundStatusBrowse              NO-ERROR.
   DELETE OBJECT InboundStatusBrowseButtons       NO-ERROR.
   DELETE OBJECT InboundStatusDetailsForm         NO-ERROR.
   DELETE OBJECT InboundStatusDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusBrowse Procedure 
PROCEDURE pInboundStatusBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "inboundstatus_details_form"}
   
   InboundStatusBrowse              = NEW browseTable("inboundstatus_browse").
   InboundStatusBrowse:BrowseWidth  = 965.
   InboundStatusBrowse:BrowseHeight = 455.
   InboundStatusBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   InboundStatusBrowse:insertColumn(fTL("Status ID"),70, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i InboundStatus}
   
   InboundStatusBrowse:insertColumn(fTL("Listing"),      80, "INTEGER", FALSE).
   InboundStatusBrowse:insertColumn(fTL("Status Code"), 120, "CHARACTER", "left", FALSE).
   InboundStatusBrowse:insertColumn(fTL("Name"),        150, "CHARACTER", "left", FALSE).
   InboundStatusBrowse:insertColumn(fTL("Description"), 305, "CHARACTER", "left", FALSE).
   InboundStatusBrowse:insertColumn(fTL("Active"),       70, "LOGICAL", "left", FALSE).
   
   /*Body*/
   InboundStatusBrowse:startBody().
   
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH InboundStatus NO-LOCK
      BY    InboundStatus.Active DESCENDING 
      BY    InboundStatus.ListingSequence 
      BY    InboundStatus.InboundStatusID:
      
      InboundStatusBrowse:startRow(InboundStatus.InboundStatusID, 
                                    "selectInboundStatusRow(this," + '"' 
                                       + STRING(InboundStatus.InboundStatusID) + '"' + ");", "").
      InboundStatusBrowse:insertData(InboundStatus.InboundStatusID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i InboundStatus}

      InboundStatusBrowse:insertData(STRING(InboundStatus.ListingSequence)).
      InboundStatusBrowse:insertData(InboundStatus.StatusCode, "left").
      InboundStatusBrowse:insertData(InboundStatus.StatusName, "left").
      InboundStatusBrowse:insertData(InboundStatus.StatusDescr, "left").
      InboundStatusBrowse:insertData(STRING(InboundStatus.Active, "Yes/No")).
      
      /* Add hidden fields */
      InboundStatusBrowse:insertHiddenData("InboundStatusVersionID",InboundStatus.VersionID).
      
      InboundStatusBrowse:endRow().
      
   END. /* FOR EACH InboundStatus NO-LOCK */
   
   InboundStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + InboundStatusBrowse:getErrors().
   
   /* Create a new frame */
   InboundStatusBrowseFrame           = NEW pageFrame().
   InboundStatusBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   InboundStatusBrowseFrame:FormAction="dbInboundStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   InboundStatusBrowseFrame:formOpen("inboundstatus_browse_form").
   
   /* Start the Frame Header */
   InboundStatusBrowseFrame:insertSpacer(5).
   InboundStatusBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   InboundStatusBrowse:displayBrowse().  
   
   /* End the Frame Header */
   InboundStatusBrowseFrame:frameClose().
   InboundStatusBrowseFrame:insertSpacer(10).
   
   InboundStatusBrowseFrame:insertHiddenField("inboundstatus_browse_scroll","").
   InboundStatusBrowseFrame:insertHiddenField("InboundStatusID","").
   InboundStatusBrowseFrame:insertHiddenField("InboundStatusVersionID","").
   InboundStatusBrowseFrame:insertHiddenField("form_name","inboundstatus_browse_form").
   InboundStatusBrowseFrame:insertHiddenField("prog_name","adInboundStatusAdmin.p").

   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundStatusBrowseFrame}
   
   InboundStatusBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   InboundStatusBrowseButtons           = NEW buttonBar().
   InboundStatusBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   InboundStatusBrowseButtons:addButton("inboundstatus_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewInboundStatusDetails('inboundstatus_details_form');",
                                         "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO: 
      InboundStatusBrowseButtons:addButton("inboundstatus_browse_form_btn_create",
                                            fTL("Create"),
                                            "createInboundStatus('inboundstatus_details_form');",
                                            "").
                                             
      InboundStatusBrowseButtons:addButton("inboundstatus_browse_form_btn_applinks",
                                            fTL("App Links"),
                                            "viewAppLinkBrowse('inboundstatus_browse_form','InboundStatus');",
                                            "Disabled").
                                             
   END.
   
   
   InboundStatusBrowseButtons:closeBar().  
   InboundStatusBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusDetails Procedure 
PROCEDURE pInboundStatusDetails:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "inboundstatus_details_form"}
   
   ASSIGN chrDisplayFieldList  = "InboundStatusID,ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrEditFieldList     = "ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrNewFieldList      = "ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrRequiredFieldList = "ListingSequence,StatusCode,StatusName,StatusDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   InboundStatusDetailsForm           = NEW dataForm("inboundstatus_details_form").
   InboundStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   InboundStatusDetailsForm:FormAction = "dbInboundStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   InboundStatusDetailsForm:FormWidth  = 460.
   InboundStatusDetailsForm:FormHeight = 300.
   InboundStatusDetailsForm:FormTitle  = "InboundStatus Details".
   InboundStatusDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   InboundStatusDetailsForm:insertPaddingColumn(20).
   InboundStatusDetailsForm:insertColumn(115).
   InboundStatusDetailsForm:insertColumn(140).
   InboundStatusDetailsForm:insertColumn(20).
   InboundStatusDetailsForm:insertColumn(4).
   InboundStatusDetailsForm:insertColumn(20).
   
   /* Fields */
   InboundStatusDetailsForm:startRow().
   InboundStatusDetailsForm:insertLabel("InboundStatus ID").
   InboundStatusDetailsForm:insertTextField("InboundStatusID", "", 110, TRUE).  
   
   InboundStatusDetailsForm:startRow().
   InboundStatusDetailsForm:insertLabel("Listing Seq").
   InboundStatusDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   InboundStatusDetailsForm:startRow().
   InboundStatusDetailsForm:insertLabel("Status Code").
   InboundStatusDetailsForm:insertTextField("StatusCode", "", 200, TRUE).  
   
   InboundStatusDetailsForm:startRow().
   InboundStatusDetailsForm:insertLabel("Name").
   InboundStatusDetailsForm:insertTextField("StatusName", "", 200, TRUE).
   
   InboundStatusDetailsForm:startRow().
   InboundStatusDetailsForm:insertLabel("Description").
   InboundStatusDetailsForm:insertTextField("StatusDescr", "", 270, TRUE).
   
   InboundStatusDetailsForm:startRow().
   InboundStatusDetailsForm:insertLabel(fTL("Active")). 
   InboundStatusDetailsForm:insertComboField("Active", "", 110, TRUE).  
   InboundStatusDetailsForm:insertComboPairs("Active", "yes", "Active").
   InboundStatusDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pShipOrderStatusSplitDetailsFields}
   
   /* Add Hidden Fields*/
   InboundStatusDetailsForm:insertHiddenField("inboundstatus_browse_scroll", "").
   InboundStatusDetailsForm:insertHiddenField("form_name", "inboundstatus_details_form").
   InboundStatusDetailsForm:insertHiddenField("prog_name", "adInboundStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundStatusDetailsForm}
   
   /* Create Button Bar */
   InboundStatusDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:
      InboundStatusDetailsButtons:addButton("inboundstatus_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateInboundStatus('inboundstatus_details_form');").
   END.     
   
   InboundStatusDetailsButtons:addButton("inboundstatus_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode'); disablePopup('inboundstatus_details_form_popup');").
   
   InboundStatusDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   InboundStatusDetailsForm:FormButtons = InboundStatusDetailsButtons.
   
   InboundStatusDetailsForm:endForm(). 
   
   InboundStatusDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusDetailsFields Procedure 
PROCEDURE pInboundStatusDetailsFields:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         InboundStatusDetailsForm:startRow().
         InboundStatusDetailsForm:insertLabel(fTL("Field Label")).
         InboundStatusDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adInboundStatusAdmin_inboundstatus_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

