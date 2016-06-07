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
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{defWebDefinitions.i}
{defDataMigrationVariables.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedProcessOption        AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectProcessOptionRow       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToProcessOptionRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrProcessOptionID              AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE ProcessOptionBrowseFrame        AS pageFrame.
DEFINE VARIABLE ProcessOptionBrowse             AS browseTable.
DEFINE VARIABLE ProcessOptionBrowseButtons      AS buttonBar.
DEFINE VARIABLE ProcessOptionDetailsForm        AS dataForm.
DEFINE VARIABLE ProcessOptionDetailsButtons     AS buttonBar.

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
         WIDTH              = 60.
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

&IF DEFINED(EXCLUDE-pProcessOptionBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessOptionBrowse Procedure 
PROCEDURE pProcessOptionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "processoption_details_form"}
   
   ProcessOptionBrowse              = NEW browseTable("processoption_browse").
   ProcessOptionBrowse:BrowseWidth  = 965.
   ProcessOptionBrowse:BrowseHeight = 455.
   ProcessOptionBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   ProcessOptionBrowse:insertColumn(fTL("Option ID"),           60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ProcessOption}
   
   ProcessOptionBrowse:insertColumn(fTL("List Seq"),            60, "INTEGER", FALSE).
   ProcessOptionBrowse:insertColumn(fTL("Map"),                120, "CHARACTER", "left", FALSE).  
   ProcessOptionBrowse:insertColumn(fTL("Option Name"),        220, "CHARACTER", "left", FALSE).
   ProcessOptionBrowse:insertColumn(fTL("Option Description"), 230, "CHARACTER", "left", FALSE).
   ProcessOptionBrowse:insertColumn(fTL("CustSpc"),             50, "LOGICAL", FALSE).
   ProcessOptionBrowse:insertColumn(fTL("Active"),              50, "LOGICAL", FALSE).
   
   /*Body*/
   ProcessOptionBrowse:startBody().
   
   /* Find all ProcessOptions then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH ProcessOption NO-LOCK,
       EACH ProcessMap OF ProcessOption NO-LOCK
      BY    ProcessMap.ListingSequence
      BY    ProcessOption.ListingSequence 
      BY    ProcessOption.OptionName
      BY    ProcessOption.CustomerSpecific:
      
      ProcessOptionBrowse:startRow(ProcessOption.ProcessOptionID, "selectProcessOptionRow(this," + '"' 
                                                                     + STRING(ProcessOption.ProcessOptionID) + '"' + ");", "").
      ProcessOptionBrowse:insertData(ProcessOption.ProcessOptionID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      /*
      {webGetOptionalBrowseFields.i ProcessOption}
      */
      ProcessOptionBrowse:insertData(STRING(ProcessOption.ListingSequence)).
      ProcessOptionBrowse:insertData(ProcessMap.MapName, "left"). 
      ProcessOptionBrowse:insertData(ProcessOption.OptionName, "left").
      ProcessOptionBrowse:insertData(ProcessOption.OptionDescr, "left").
      ProcessOptionBrowse:insertData(STRING(ProcessOption.CustomerSpecific,"Yes/No")). 
      ProcessOptionBrowse:insertData(STRING(ProcessOption.Active, "Yes/No")).
      /* Add hidden fields */
      ProcessOptionBrowse:insertHiddenData("ProcessOptionVersionID",ProcessOption.VersionID).
      
      ProcessOptionBrowse:endRow().
      
   END. /* FOR EACH ProcessOption NO-LOCK */
   
   ProcessOptionBrowse:endTable().
   chrPageBuildError = chrPageBuildError + ProcessOptionBrowse:getErrors().
   
   /* Create a new frame */
   ProcessOptionBrowseFrame           = NEW pageFrame().
   ProcessOptionBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   ProcessOptionBrowseFrame:FormAction="dbProcessOptionUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   ProcessOptionBrowseFrame:formOpen("processoption_browse_form").
   
   /* Start the Frame Header */
   ProcessOptionBrowseFrame:insertSpacer(5).
   ProcessOptionBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   ProcessOptionBrowse:displayBrowse().  
   
   /* End the Frame Header */
   ProcessOptionBrowseFrame:frameClose().
   ProcessOptionBrowseFrame:insertSpacer(10).
   
   ProcessOptionBrowseFrame:insertHiddenField("processoption_browse_scroll","").
   ProcessOptionBrowseFrame:insertHiddenField("ProcessOptionID","").
   ProcessOptionBrowseFrame:insertHiddenField("ProcessOptionVersionID","").
   ProcessOptionBrowseFrame:insertHiddenField("processoptionlink_browse_scroll","").
   ProcessOptionBrowseFrame:insertHiddenField("popup_processoptionlink_browse","").
   ProcessOptionBrowseFrame:insertHiddenField("ProcessOptionLinkID","").
   ProcessOptionBrowseFrame:insertHiddenField("form_name","processoption_browse_form").
   ProcessOptionBrowseFrame:insertHiddenField("prog_name","adProcessOptionAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ProcessOptionBrowseFrame}
   
   ProcessOptionBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   ProcessOptionBrowseButtons           = NEW buttonBar().
   ProcessOptionBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   ProcessOptionBrowseButtons:addButton("processoption_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewProcessOptionDetails('processoption_details_form');",
                                        (IF intSelectedProcessOption > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
      ProcessOptionBrowseButtons:addButton("processoption_browse_form_btn_create",
                                           fTL("Create"),
                                           "createProcessOption('processoption_details_form');",
                                           "").
                                           
      ProcessOptionBrowseButtons:addButton("processoption_browse_form_btn_applinks",
                                           fTL("App Links"),
                                           "viewAppLinkBrowse('processoption_browse_form','ProcessOption');",
                                           "Disabled").
                                                                                   
   END.                                     
   
   ProcessOptionBrowseButtons:closeBar().  
   ProcessOptionBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pProcessOptionDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessOptionDetails Procedure 
PROCEDURE pProcessOptionDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "processoption_details_form"}
   
   ASSIGN chrDisplayFieldList  = "ProcessOptionID,ListingSequence,ProcessMapID,OptionName,OptionDescr,CustomerSpecific,Active"
          chrEditFieldList     = "Active"
          chrNewFieldList      = "ListingSequence,ProcessMapID,OptionName,OptionDescr,CustomerSpecific,Active"
          chrRequiredFieldList = "ListingSequence,ProcessMapID,OptionName,OptionDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   ProcessOptionDetailsForm           = NEW dataForm("processoption_details_form").
   ProcessOptionDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   ProcessOptionDetailsForm:FormAction = "dbProcessOptionUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ProcessOptionDetailsForm:FormWidth  = 460.
   ProcessOptionDetailsForm:FormHeight = 300.
   ProcessOptionDetailsForm:FormTitle  = "ProcessOption Details".
   ProcessOptionDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   ProcessOptionDetailsForm:insertPaddingColumn(10).
   ProcessOptionDetailsForm:insertColumn(100).
   ProcessOptionDetailsForm:insertColumn(120).
   ProcessOptionDetailsForm:insertColumn(20).
   ProcessOptionDetailsForm:insertColumn(4).
   ProcessOptionDetailsForm:insertColumn(110).
   
   /* Fields */
   ProcessOptionDetailsForm:startRow().
   ProcessOptionDetailsForm:insertLabel("Option ID").
   ProcessOptionDetailsForm:insertTextField("ProcessOptionID", "", 110, TRUE).  
   
   ProcessOptionDetailsForm:startRow().
   ProcessOptionDetailsForm:insertLabel("Listing Seq").
   ProcessOptionDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   ProcessOptionDetailsForm:startRow().
   ProcessOptionDetailsForm:insertLabel(fTL("Process Map")).
   ProcessOptionDetailsForm:insertComboField("ProcessMapID", "", 200, TRUE).  
   FOR EACH ProcessMap NO-LOCK /*idx=ActiveListingSequence*/
      BY ProcessMap.Active DESC
      BY ProcessMap.MapName:
      
      ProcessOptionDetailsForm:insertComboPairs("ProcessMapID", STRING(ProcessMap.ProcessMapID), ProcessMap.MapName).
   END.   

   ProcessOptionDetailsForm:startRow().
   ProcessOptionDetailsForm:insertLabel("Option Name").
   ProcessOptionDetailsForm:insertTextField("OptionName", "", 300, TRUE).
   
   ProcessOptionDetailsForm:startRow().
   ProcessOptionDetailsForm:insertLabel("Option Descr").
   ProcessOptionDetailsForm:insertTextAreaField("OptionDescr", "", 300, TRUE).

   ProcessOptionDetailsForm:startRow().
   ProcessOptionDetailsForm:insertLabel(fTL("Cust Specific")). 
   ProcessOptionDetailsForm:insertComboField("CustomerSpecific", "", 180, TRUE).  
   ProcessOptionDetailsForm:insertComboPairs("CustomerSpecific", "yes", "Customer Specific").
   ProcessOptionDetailsForm:insertComboPairs("CustomerSpecific", "no",  "Not Customer Specific").

   ProcessOptionDetailsForm:startRow().
   ProcessOptionDetailsForm:insertLabel(fTL("Active")). 
   ProcessOptionDetailsForm:insertComboField("Active", "", 110, TRUE).  
   ProcessOptionDetailsForm:insertComboPairs("Active", "yes", "Active").
   ProcessOptionDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pProcessOptionDetailsFields}
   
   /* Add Hidden Fields*/
   ProcessOptionDetailsForm:insertHiddenField("processoption_browse_scroll", "").
   ProcessOptionDetailsForm:insertHiddenField("form_name", "processoption_details_form").
   ProcessOptionDetailsForm:insertHiddenField("prog_name", "adProcessOptionAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ProcessOptionDetailsForm}
   
   /* Create Button Bar */
   ProcessOptionDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      ProcessOptionDetailsButtons:addButton("processoption_details_form_btn_save", 
                                            fTL("Save"), 
                                            "updateProcessOption('processoption_details_form');").
   
   ProcessOptionDetailsButtons:addButton("processoption_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "cancelUpdate('UserCancelled','process_mode'); disablePopup('processoption_details_form_popup');").
   
   ProcessOptionDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ProcessOptionDetailsForm:FormButtons = ProcessOptionDetailsButtons.
   
   ProcessOptionDetailsForm:endForm(). 
   
   ProcessOptionDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pProcessOptionDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessOptionDetailsFields Procedure 
PROCEDURE pProcessOptionDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         ProcessOptionDetailsForm:startRow().
         ProcessOptionDetailsForm:insertLabel(fTL("Field Label")).
         ProcessOptionDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adProcessOptionAdmin_processoption_details_form.i}
      
   END CASE. /*chrOption:*/
   
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
   
   ASSIGN chrProcessOptionID          = get-value("ProcessOptionID")
          intSelectedProcessOption    = INTEGER(chrProcessOptionID)
          chrScrollToProcessOptionRow = STRING(INTEGER(get-value("processoption_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrProcessOptionID <> "" THEN
      chrSelectProcessOptionRow = 'selectProcessOptionRow(document.getElementById("processoption_browse_row_' + chrProcessOptionID + '"),"' 
                                                         + chrProcessOptionID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("processoption_browse").scrollTop=' + chrScrollToProcessOptionRow 
                             + chrSelectProcessOptionRow.
                             
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                                
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "ProcessOption Admin".
   ThisPage:FrameTitle    = "ProcessOption Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("processoption.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pProcessOptionBrowse.
   
/*    FIND FIRST ProcessOption NO-LOCK                                             */
/*       WHERE ProcessOption.ProcessOptionID = intSelectedProcessOption NO-ERROR.  */
/*                                                                                 */
   /******* Pop-up Browsers and Forms ********/    
   RUN pProcessOptionDetails.
   
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
   DELETE OBJECT ProcessOptionBrowseFrame        NO-ERROR.
   DELETE OBJECT ProcessOptionBrowse             NO-ERROR.
   DELETE OBJECT ProcessOptionBrowseButtons      NO-ERROR.
   DELETE OBJECT ProcessOptionDetailsForm        NO-ERROR.
   DELETE OBJECT ProcessOptionDetailsButtons     NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

