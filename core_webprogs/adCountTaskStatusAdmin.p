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
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     clean up will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Custom Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{defWebDefinitions.i}
{defDataMigrationVariables.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedCountTaskStatus    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountTaskStatusRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountTaskStatusRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskStatusID          AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE CountTaskStatusBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskStatusBrowse         AS browseTable.
DEFINE VARIABLE CountTaskStatusBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskStatusDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskStatusDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pCountTaskStatusBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskStatusBrowse Procedure 
PROCEDURE pCountTaskStatusBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "counttaskstatus_details_form"}
   
   CountTaskStatusBrowse              = NEW browseTable("counttaskstatus_browse").
   CountTaskStatusBrowse:BrowseWidth  = 965.
   CountTaskStatusBrowse:BrowseHeight = 455.
   CountTaskStatusBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountTaskStatusBrowse:insertColumn(fTL("StatusID"), 70, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskStatus}
   
   CountTaskStatusBrowse:insertColumn(fTL("List Seq"),      60, "INTEGER", FALSE).
   CountTaskStatusBrowse:insertColumn(fTL("Status Code"),  120, "CHARACTER", "left", FALSE).
   CountTaskStatusBrowse:insertColumn(fTL("Status Name"),  150, "CHARACTER", "left", FALSE).
   CountTaskStatusBrowse:insertColumn(fTL("Status Descr"), 220, "CHARACTER", "left", FALSE).
   CountTaskStatusBrowse:insertColumn(fTL("Active"),        50, "LOGICAL", FALSE).
   
   /*Body*/
   CountTaskStatusBrowse:startBody().
   
   /* Find all Count Task Statuss then sort by Active, Listing Sequence, and Status ID in case Sequences are the same */
   FOR EACH CountTaskStatus NO-LOCK
      BY    CountTaskStatus.Active 
      BY    CountTaskStatus.ListingSequence 
      BY    CountTaskStatus.CountTaskStatusID:
      
      CountTaskStatusBrowse:startRow(CountTaskStatus.CountTaskStatusID, "selectCountTaskStatusRow(this," + '"' 
                                                                     + STRING(CountTaskStatus.CountTaskStatusID) + '"' + ");", "").
      CountTaskStatusBrowse:insertData(CountTaskStatus.CountTaskStatusID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountTaskStatus}
      
      CountTaskStatusBrowse:insertData(STRING(CountTaskStatus.ListingSequence)).
      CountTaskStatusBrowse:insertData(CountTaskStatus.StatusCode, "left").
      CountTaskStatusBrowse:insertData(CountTaskStatus.StatusName, "left").
      CountTaskStatusBrowse:insertData(CountTaskStatus.StatusDescr, "left").
      CountTaskStatusBrowse:insertData(STRING(CountTaskStatus.Active, "Yes/No")).
      
      /* Add hidden fields */
      CountTaskStatusBrowse:insertHiddenData("CountTaskStatusVersionID",CountTaskStatus.VersionID).
      
      CountTaskStatusBrowse:endRow().
      
   END. /* FOR EACH CountTaskStatus NO-LOCK */
   
   CountTaskStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskStatusBrowse:getErrors().
   
   /* Create a new frame */
   CountTaskStatusBrowseFrame           = NEW pageFrame().
   CountTaskStatusBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CountTaskStatusBrowseFrame:FormAction="dbCountTaskStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CountTaskStatusBrowseFrame:formOpen("counttaskstatus_browse_form").
   
   /* Start the Frame Header */
   CountTaskStatusBrowseFrame:insertSpacer(5).
   CountTaskStatusBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CountTaskStatusBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CountTaskStatusBrowseFrame:frameClose().
   CountTaskStatusBrowseFrame:insertSpacer(10).
   
   CountTaskStatusBrowseFrame:insertHiddenField("counttaskstatus_browse_scroll","").
   CountTaskStatusBrowseFrame:insertHiddenField("CountTaskStatusID","").
   CountTaskStatusBrowseFrame:insertHiddenField("CountTaskStatusVersionID","").
   CountTaskStatusBrowseFrame:insertHiddenField("form_name","counttaskstatus_browse_form").
   CountTaskStatusBrowseFrame:insertHiddenField("prog_name","adCountTaskStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskStatusBrowseFrame}
   
   CountTaskStatusBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   CountTaskStatusBrowseButtons           = NEW buttonBar().
   CountTaskStatusBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   CountTaskStatusBrowseButtons:addButton("counttaskstatus_browse_form_btn_details",
                                          fTL("Details"),
                                          "viewCountTaskStatusDetails('counttaskstatus_details_form');",
                                          "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO:   
      CountTaskStatusBrowseButtons:addButton("counttaskstatus_browse_form_btn_create",
                                             fTL("Create"),
                                             "createCountTaskStatus('counttaskstatus_details_form');",
                                             "").
                                             
      CountTaskStatusBrowseButtons:addButton("counttaskstatus_browse_form_btn_applinks",
                                             fTL("App Links"),
                                             "viewAppLinkBrowse('counttaskstatus_browse_form','CountTaskStatus');",
                                             "Disabled").
   END.                                             
                                             
   
   CountTaskStatusBrowseButtons:closeBar().  
   CountTaskStatusBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskStatusDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskStatusDetails Procedure 
PROCEDURE pCountTaskStatusDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "counttaskstatus_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountTaskStatusID,ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrEditFieldList     = "ListingSequence,StatusName,StatusDescr,Active"
          chrNewFieldList      = "ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrRequiredFieldList = "ListingSequence,StatusCode,StatusName,StatusDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   CountTaskStatusDetailsForm           = NEW dataForm("counttaskstatus_details_form").
   CountTaskStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskStatusDetailsForm:FormAction = "dbCountTaskStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskStatusDetailsForm:FormWidth  = 460.
   CountTaskStatusDetailsForm:FormHeight = 300.
   CountTaskStatusDetailsForm:FormTitle  = "CountTaskStatus Details".
   CountTaskStatusDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountTaskStatusDetailsForm:insertPaddingColumn(50).
   CountTaskStatusDetailsForm:insertColumn(100).
   CountTaskStatusDetailsForm:insertColumn(120).
   CountTaskStatusDetailsForm:insertColumn(20).
   CountTaskStatusDetailsForm:insertColumn(4).
   CountTaskStatusDetailsForm:insertColumn(110).
   
   /* Fields */
   CountTaskStatusDetailsForm:startRow().
   CountTaskStatusDetailsForm:insertLabel("Status ID").
   CountTaskStatusDetailsForm:insertTextField("CountTaskStatusID", "", 110, TRUE).  
   
   CountTaskStatusDetailsForm:startRow().
   CountTaskStatusDetailsForm:insertLabel("Listing Seq").
   CountTaskStatusDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   CountTaskStatusDetailsForm:startRow().
   CountTaskStatusDetailsForm:insertLabel("Status Code").
   CountTaskStatusDetailsForm:insertTextField("StatusCode", "", 110, TRUE).  
   
   CountTaskStatusDetailsForm:startRow().
   CountTaskStatusDetailsForm:insertLabel("Status Name").
   CountTaskStatusDetailsForm:insertTextField("StatusName", "", 110, TRUE).
   
   CountTaskStatusDetailsForm:startRow().
   CountTaskStatusDetailsForm:insertLabel("Status Descr").
   CountTaskStatusDetailsForm:insertTextField("StatusDescr", "", 220, TRUE).

   CountTaskStatusDetailsForm:startRow().
   CountTaskStatusDetailsForm:insertLabel(fTL("Active")). 
   CountTaskStatusDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CountTaskStatusDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountTaskStatusDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCountTaskStatusDetailsFields}
   
   /* Add Hidden Fields*/
   CountTaskStatusDetailsForm:insertHiddenField("counttaskstatus_browse_scroll", "").
   CountTaskStatusDetailsForm:insertHiddenField("form_name", "counttaskstatus_details_form").
   CountTaskStatusDetailsForm:insertHiddenField("prog_name", "adCountTaskStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskStatusDetailsForm}
   
   /* Create Button Bar */
   CountTaskStatusDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      CountTaskStatusDetailsButtons:addButton("counttaskstatus_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateCountTaskStatus('counttaskstatus_details_form');").
   
   CountTaskStatusDetailsButtons:addButton("counttaskstatus_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode'); disablePopup('counttaskstatus_details_form_popup');").
   
   CountTaskStatusDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskStatusDetailsForm:FormButtons = CountTaskStatusDetailsButtons.
   
   CountTaskStatusDetailsForm:endForm(). 
   
   CountTaskStatusDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskStatusDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskStatusDetailsFields Procedure 
PROCEDURE pCountTaskStatusDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         CountTaskStatusDetailsForm:startRow().
         CountTaskStatusDetailsForm:insertLabel(fTL("Field Label")).
         CountTaskStatusDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adCountTaskStatusAdmin_counttaskstatus_details_form.i}
      
   END CASE. /*chrOption:*/
   
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
   
   ASSIGN chrCountTaskStatusID          = get-value("CountTaskStatusID")
          intSelectedCountTaskStatus    = INTEGER(chrCountTaskStatusID)
          chrScrollToCountTaskStatusRow = STRING(INTEGER(get-value("counttaskstatus_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrCountTaskStatusID <> "" THEN
      chrSelectCountTaskStatusRow = 'selectCountTaskStatusRow(document.getElementById("counttaskstatus_browse_row_' 
                                       + chrCountTaskStatusID + '"),"' + chrCountTaskStatusID +  '");'.
                                          
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                    + 'document.getElementById("counttaskstatus_browse").scrollTop=' + chrScrollToCountTaskStatusRow 
                    + chrSelectCountTaskStatusRow.
                    
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                          
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "CountTaskStatus Admin".
   ThisPage:FrameTitle    = "CountTaskStatus Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("counttaskstatus.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pCountTaskStatusBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pCountTaskStatusDetails.
   
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
   DELETE OBJECT CountTaskStatusBrowseFrame         NO-ERROR.
   DELETE OBJECT CountTaskStatusBrowse              NO-ERROR.
   DELETE OBJECT CountTaskStatusBrowseButtons       NO-ERROR.
   DELETE OBJECT CountTaskStatusDetailsForm         NO-ERROR.
   DELETE OBJECT CountTaskStatusDetailsButtons      NO-ERROR.
      
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

