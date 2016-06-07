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


/* Currency Local Variables */
DEFINE VARIABLE intSelectedCurrency    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCurrencyRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCurrencyRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCurrencyID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCurrencyCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCurrencyName        AS CHARACTER NO-UNDO.


/* Currency Objects */
DEFINE VARIABLE CurrencyBrowseFrame    AS pageFrame.
DEFINE VARIABLE CurrencyBrowse         AS browseTable.
DEFINE VARIABLE CurrencyBrowseButtons  AS buttonBar.
DEFINE VARIABLE CurrencyDetailsForm    AS dataForm.
DEFINE VARIABLE CurrencyDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pCurrencyDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCurrencyDetails Procedure
PROCEDURE pCurrencyDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "currency_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CurrencyID,CurrencyName,CurrencyCode,CurrencySymbol,Active"
          chrEditFieldList     = "CurrencyName,CurrencyCode,CurrencySymbol,Active"
          chrNewFieldList      = "CurrencyName,CurrencyCode,CurrencySymbol,Active"
          chrRequiredFieldList = "CurrencyName,CurrencyCode,CurrencySymbol"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CurrencyDetailsForm           = NEW dataForm("currency_details_form").
   CurrencyDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CurrencyDetailsForm:FormAction = "dbCurrencyUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CurrencyDetailsForm:FormWidth  = 460.
   CurrencyDetailsForm:FormHeight = 300.
   CurrencyDetailsForm:FormTitle  = "currency Details".
   CurrencyDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CurrencyDetailsForm:insertPaddingColumn(50).
   CurrencyDetailsForm:insertColumn(100).
   CurrencyDetailsForm:insertColumn(120).
   CurrencyDetailsForm:insertColumn(20).
   CurrencyDetailsForm:insertColumn(4).
   CurrencyDetailsForm:insertColumn(110).
   
   /* Fields */
   CurrencyDetailsForm:startRow().
   CurrencyDetailsForm:insertLabel("Currency ID").
   CurrencyDetailsForm:insertTextField("CurrencyID", "", 110, TRUE).  

   CurrencyDetailsForm:startRow().
   CurrencyDetailsForm:insertLabel("Currency Name").
   CurrencyDetailsForm:insertTextField("CurrencyName", "", 220, TRUE).
   
   CurrencyDetailsForm:startRow().
   CurrencyDetailsForm:insertLabel("Currency Code").
   CurrencyDetailsForm:insertTextField("CurrencyCode", "", 220, TRUE).

   CurrencyDetailsForm:startRow().
   CurrencyDetailsForm:insertLabel("Currency Symbol").
   CurrencyDetailsForm:insertTextField("CurrencySymbol", "", 220, TRUE).
   
   CurrencyDetailsForm:startRow().
   CurrencyDetailsForm:insertLabel(fTL("Active")). 
   CurrencyDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CurrencyDetailsForm:insertComboPairs("Active", "yes", "Active").
   CurrencyDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCurrencyDetailsFields}
   
   /* Add Hidden Fields*/
   CurrencyDetailsForm:insertHiddenField("currency_browse_scroll", "").
   CurrencyDetailsForm:insertHiddenField("form_name", "currency_details_form").
   CurrencyDetailsForm:insertHiddenField("prog_name", "adCurrencyAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CurrencyDetailsForm}
   
   /* Create Button Bar */
   CurrencyDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:
      CurrencyDetailsButtons:addButton("currency_details_form_btn_save", 
                                      fTL("Save"), 
                                      "updateCurrency('currency_details_form');").
   END.     
   
   CurrencyDetailsButtons:addButton("currency_details_form_btn_cancel", 
                                   fTL("Cancel"), 
                                   "cancelUpdate('UserCancelled','process_mode'); disablePopup('currency_details_form_popup');").
   
   CurrencyDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CurrencyDetailsForm:FormButtons = CurrencyDetailsButtons.
   
   CurrencyDetailsForm:endForm(). 
   
   CurrencyDetailsForm:displayForm(). 
   
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCurrencyDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCurrencyDetailsFields Procedure
PROCEDURE pCurrencyDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         CurrencyDetailsForm:startRow().
         CurrencyDetailsForm:insertLabel(fTL("Field Label")).
         CurrencyDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adCurrencyAdmin_currency_details_form.i}
      
      
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
   
   ASSIGN chrCurrencyID          = get-value("CurrencyID")
          intSelectedCurrency    = INTEGER(chrCurrencyID)
          chrScrollToCurrencyRow = STRING(INTEGER(get-value("currency_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrCurrencyID <> "" THEN
      chrSelectCurrencyRow = 'selectCurrencyRow(document.getElementById("currency_browse_row_' + chrCurrencyID + '"),"' 
                               + chrCurrencyID + '");'.
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad 
                   + 'document.getElementById("currency_browse").scrollTop=' + chrScrollToCurrencyRow
                   + chrSelectCurrencyRow.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Currency Admin". 
   ThisPage:FrameTitle    = "Currency Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("currency.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
      
   /******* Main Browser ********************/
   RUN pCurrencyBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pCurrencyDetails.
   
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
   DELETE OBJECT CurrencyBrowseFrame         NO-ERROR.
   DELETE OBJECT CurrencyBrowse              NO-ERROR.
   DELETE OBJECT CurrencyBrowseButtons       NO-ERROR.
   DELETE OBJECT CurrencyDetailsForm         NO-ERROR.
   DELETE OBJECT CurrencyDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCurrencyBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCurrencyBrowse Procedure 
PROCEDURE pCurrencyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "currency_details_form"}
   
   CurrencyBrowse              = NEW browseTable("currency_browse").
   CurrencyBrowse:BrowseWidth  = 965.
   CurrencyBrowse:BrowseHeight = 455.
   CurrencyBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CurrencyBrowse:insertColumn(fTL("Currency ID"),     100, "INTEGER", "left", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Currency}
   
   CurrencyBrowse:insertColumn(fTL("Currency Name"),   300, "CHARACTER", "left", FALSE).
   CurrencyBrowse:insertColumn(fTL("Currency Code"),   300, "CHARACTER", "left", FALSE).
   CurrencyBrowse:insertColumn(fTL("Currency Symbol"), 160, "CHARACTER", "left", FALSE).
   CurrencyBrowse:insertColumn(fTL("Active"),           60, "LOGICAL", "left", FALSE).
   
   /*Body*/
   CurrencyBrowse:startBody().
   
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH Currency NO-LOCK
      BY    Currency.Active DESCENDING 
      BY    Currency.CurrencyID:
      
      CurrencyBrowse:startRow(Currency.CurrencyID, "selectCurrencyRow(this," + '"' + STRING(Currency.CurrencyID) + '"' + ");", "").
      CurrencyBrowse:insertData(Currency.CurrencyID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Currency}

      CurrencyBrowse:insertData(Currency.CurrencyName, "left").
      CurrencyBrowse:insertData(Currency.CurrencyCode, "left").
      CurrencyBrowse:insertData(Currency.CurrencySymbol, "left").
      CurrencyBrowse:insertData(STRING(Currency.Active, "Yes/No")).
      
      /* Add hidden fields */
      CurrencyBrowse:insertHiddenData("CurrencyVersionID", Currency.VersionID).
      
      CurrencyBrowse:endRow().
      
   END. /* FOR EACH Currency NO-LOCK */
   
   CurrencyBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CurrencyBrowse:getErrors().
   
   /* Create a new frame */
   CurrencyBrowseFrame           = NEW pageFrame().
   CurrencyBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CurrencyBrowseFrame:FormAction="dbCurrencyUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CurrencyBrowseFrame:formOpen("currency_browse_form").
   
   /* Start the Frame Header */
   CurrencyBrowseFrame:insertSpacer(5).
   CurrencyBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CurrencyBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CurrencyBrowseFrame:frameClose().
   CurrencyBrowseFrame:insertSpacer(10).
   
   CurrencyBrowseFrame:insertHiddenField("currency_browse_scroll", "").
   CurrencyBrowseFrame:insertHiddenField("CurrencyID", "").
   CurrencyBrowseFrame:insertHiddenField("CurrencyVersionID", "").
   CurrencyBrowseFrame:insertHiddenField("form_name", "currency_browse_form").
   CurrencyBrowseFrame:insertHiddenField("prog_name", "adCurrencyAdmin.p").

   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CurrencyBrowseFrame}
   
   CurrencyBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   CurrencyBrowseButtons           = NEW buttonBar().
   CurrencyBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   CurrencyBrowseButtons:addButton("currency_browse_form_btn_details",
                                  fTL("Details"),
                                  "viewCurrencyDetails('currency_details_form');",
                                  "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO: 
      CurrencyBrowseButtons:addButton("currency_browse_form_btn_create",
                                     fTL("Create"),
                                     "createCurrency('currency_details_form');",
                                     "").

      CurrencyBrowseButtons:addButton("currency_browse_form_btn_applinks",
                                     fTL("App Links"),
                                     "viewAppLinkBrowse('currency_browse_form','Currency');",
                                     "Disabled").
                                     
   END.
   
   
   CurrencyBrowseButtons:closeBar().  
   CurrencyBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
