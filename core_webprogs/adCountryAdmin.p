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


/* Country Local Variables */
DEFINE VARIABLE intSelectedCountry    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountryRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountryRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountryID          AS CHARACTER NO-UNDO.

/* Country Objects */
DEFINE VARIABLE CountryBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountryBrowse         AS browseTable.
DEFINE VARIABLE CountryBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountryDetailsForm    AS dataForm.
DEFINE VARIABLE CountryDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pCountryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountryDetails Procedure
PROCEDURE pCountryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "country_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountryID,CountryName,ListingSequence,Alpha2Code,Alpha3Code,NumericCode"
                                    + ",ImageName,Active"
          chrEditFieldList     = "CountryName,ListingSequence,Alpha2Code,Alpha3Code,NumericCode,ImageName,Active"
          chrNewFieldList      = "CountryName,ListingSequence,Alpha2Code,Alpha3Code,NumericCode,ImageName,Active"
          chrRequiredFieldList = "CountryName,ListingSequence,Alpha2Code,Alpha3Code,NumericCode"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER,NumericCode:INTEGER".
   
   CountryDetailsForm           = NEW dataForm("country_details_form").
   CountryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountryDetailsForm:FormAction = "dbCountryUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountryDetailsForm:FormWidth  = 460.
   CountryDetailsForm:FormHeight = 300.
   CountryDetailsForm:FormTitle  = "Country Details".
   CountryDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountryDetailsForm:insertPaddingColumn(50).
   CountryDetailsForm:insertColumn(100).
   CountryDetailsForm:insertColumn(120).
   CountryDetailsForm:insertColumn(20).
   CountryDetailsForm:insertColumn(4).
   CountryDetailsForm:insertColumn(110).
   
   /* Fields */
   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel("Country ID").
   CountryDetailsForm:insertTextField("CountryID", "", 110, TRUE).  

   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel("Country Name").
   CountryDetailsForm:insertTextField("CountryName", "", 220, TRUE).
      
   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel("Alpha Code-2").
   CountryDetailsForm:insertTextField("Alpha2Code", "", 110, TRUE).  
   
   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel("Alpha Code-3").
   CountryDetailsForm:insertTextField("Alpha3Code", "", 110, TRUE).
   
   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel("Numeric Code").
   CountryDetailsForm:insertTextField("NumericCode", "", 110, TRUE).

   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel("Image Name").
   CountryDetailsForm:insertTextField("Image Name", "", 220, TRUE).

   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel("Listing Seq").
   CountryDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   CountryDetailsForm:startRow().
   CountryDetailsForm:insertLabel(fTL("Active")). 
   CountryDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CountryDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCountryDetailsFields}
   
   /* Add Hidden Fields*/
   CountryDetailsForm:insertHiddenField("country_browse_scroll", "").
   CountryDetailsForm:insertHiddenField("form_name", "country_details_form").
   CountryDetailsForm:insertHiddenField("prog_name", "adCountryAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountryDetailsForm}
   
   /* Create Button Bar */
   CountryDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:
      CountryDetailsButtons:addButton("country_details_form_btn_save", 
                                      fTL("Save"), 
                                      "updateCountry('country_details_form');").
   END.     
   
   CountryDetailsButtons:addButton("country_details_form_btn_cancel", 
                                   fTL("Cancel"), 
                                   "cancelUpdate('UserCancelled','process_mode'); disablePopup('country_details_form_popup');").
   
   CountryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountryDetailsForm:FormButtons = CountryDetailsButtons.
   
   CountryDetailsForm:endForm(). 
   
   CountryDetailsForm:displayForm(). 
   
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCountryDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountryDetailsFields Procedure
PROCEDURE pCountryDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         CountryDetailsForm:startRow().
         CountryDetailsForm:insertLabel(fTL("Field Label")).
         CountryDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adCountryAdmin_country_details_form.i}
      
      
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
   
   ASSIGN chrCountryID          = get-value("CountryID")
          intSelectedCountry    = INTEGER(chrCountryID)
          chrScrollToCountryRow = STRING(INTEGER(get-value("country_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrCountryID <> "" THEN
      chrSelectCountryRow = 'selectCountryRow(document.getElementById("country_browse_row_' + chrCountryID + '"),"' 
                               + chrCountryID + '");'.
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad 
                   + 'document.getElementById("country_browse").scrollTop=' + chrScrollToCountryRow
                   + chrSelectCountryRow.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Country Admin".
   ThisPage:FrameTitle    = "Country Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("country.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
      
   /******* Main Browser ********************/
   RUN pCountryBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pCountryDetails.
   
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
   DELETE OBJECT CountryBrowseFrame         NO-ERROR.
   DELETE OBJECT CountryBrowse              NO-ERROR.
   DELETE OBJECT CountryBrowseButtons       NO-ERROR.
   DELETE OBJECT CountryDetailsForm         NO-ERROR.
   DELETE OBJECT CountryDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountryBrowse Procedure 
PROCEDURE pCountryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "country_details_form"}
   
   CountryBrowse              = NEW browseTable("country_browse").
   CountryBrowse:BrowseWidth  = 965.
   CountryBrowse:BrowseHeight = 455.
   CountryBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountryBrowse:insertColumn(fTL("ID"),70, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Country}
   
   CountryBrowse:insertColumn(fTL("Name"),        120, "CHARACTER", FALSE).
   CountryBrowse:insertColumn(fTL("Code-2"),       70, "CHARACTER", "left", FALSE).
   CountryBrowse:insertColumn(fTL("Code-3"),       70, "CHARACTER", "left", FALSE).
   CountryBrowse:insertColumn(fTL("Numeric Code"), 90, "INTEGER", "right", FALSE).
   CountryBrowse:insertColumn(fTL("Listing Seq"),  70, "INTEGER", "right", FALSE).
   CountryBrowse:insertColumn(fTL("Active"),       70, "LOGICAL", "left", FALSE).
   
   /*Body*/
   CountryBrowse:startBody().
   
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH Country NO-LOCK
      BY    Country.Active DESCENDING 
      BY    Country.ListingSequence 
      BY    Country.CountryID:
      
      CountryBrowse:startRow(Country.CountryID, "selectCountryRow(this," + '"' + STRING(Country.CountryID) + '"' + ");", "").
      CountryBrowse:insertData(Country.CountryID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Country}

      CountryBrowse:insertData(Country.CountryName, "left").
      CountryBrowse:insertData(Country.Alpha2Code, "left").
      CountryBrowse:insertData(Country.Alpha3Code, "left").
      CountryBrowse:insertData(STRING(Country.NumericCode), "right").
      CountryBrowse:insertData(STRING(Country.ListingSequence), "right").
      CountryBrowse:insertData(STRING(Country.Active, "Yes/No")).
      
      /* Add hidden fields */
      CountryBrowse:insertHiddenData("CountryVersionID", Country.VersionID).
      
      CountryBrowse:endRow().
      
   END. /* FOR EACH Country NO-LOCK */
   
   CountryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountryBrowse:getErrors().
   
   /* Create a new frame */
   CountryBrowseFrame           = NEW pageFrame().
   CountryBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CountryBrowseFrame:FormAction="dbCountryUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CountryBrowseFrame:formOpen("country_browse_form").
   
   /* Start the Frame Header */
   CountryBrowseFrame:insertSpacer(5).
   CountryBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CountryBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CountryBrowseFrame:frameClose().
   CountryBrowseFrame:insertSpacer(10).
   
   CountryBrowseFrame:insertHiddenField("country_browse_scroll", "").
   CountryBrowseFrame:insertHiddenField("CountryID", "").
   CountryBrowseFrame:insertHiddenField("CountryVersionID", "").
   CountryBrowseFrame:insertHiddenField("form_name", "country_browse_form").
   CountryBrowseFrame:insertHiddenField("prog_name", "adCountryAdmin.p").

   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountryBrowseFrame}
   
   CountryBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   CountryBrowseButtons           = NEW buttonBar().
   CountryBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   CountryBrowseButtons:addButton("country_browse_form_btn_details",
                                  fTL("Details"),
                                  "viewCountryDetails('country_details_form');",
                                  "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO: 
      CountryBrowseButtons:addButton("country_browse_form_btn_create",
                                     fTL("Create"),
                                     "createCountry('country_details_form');",
                                     "").

      CountryBrowseButtons:addButton("country_browse_form_btn_applinks",
                                     fTL("App Links"),
                                     "viewAppLinkBrowse('country_browse_form','Country');",
                                     "Disabled").
                                     
   END.
   
   
   CountryBrowseButtons:closeBar().  
   CountryBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
