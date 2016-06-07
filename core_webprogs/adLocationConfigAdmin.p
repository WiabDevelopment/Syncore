&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adLocationConfigAdmin.p 

  Description: ad file for the File Location Config screen

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
DEFINE VARIABLE intSelectedLocationConfig           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrLocationConfigRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLocationConfigRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLocationConfigID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logRecordFoundLocationConfig        AS LOGICAL     NO-UNDO.

/* ConfigBrowse Objects */
DEFINE VARIABLE LocationConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE LocationConfigBrowse                AS browseTable.
DEFINE VARIABLE LocationConfigBrowseButtons         AS buttonBar.

/* ConfigDetails Objects */
DEFINE VARIABLE LocationConfigDetailsForm           AS dataForm.
DEFINE VARIABLE LocationConfigDetailsButtons        AS buttonBar.


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
   
   ASSIGN chrLocationConfigID                 = get-value("LocationConfigID")
          intSelectedLocationConfig           = INTEGER(chrLocationConfigID)
          chrScrollToLocationConfigRow        = STRING(INTEGER(get-value("locationconfig_browse_scroll"))) + ";".
          
   /* Process URL values */
   IF chrLocationConfigID <> "" THEN
      chrLocationConfigRow = 'selectLocationConfigRow(document.getElementById("locationconfig_browse_row_' + chrLocationConfigID + '"),"' 
                             + chrLocationConfigID +  '");'.                                                                                                                
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("locationconfig_browse").scrollTop=' + chrScrollToLocationConfigRow 
      + chrLocationConfigRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage                = NEW HTMLPage().
   ThisPage:WebStream      = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID  = get-value("SESSIONID").
   ThisPage:ParentMenu     = get-value("menu_no").
   ThisPage:PageTitle      = "Location Config Admin".
   ThisPage:FrameTitle     = "Location Config Admin".
   ThisPage:OnBodyLoad     = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Location Config */
   ThisPage:addJavaScript("locationconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pLocationConfigBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pLocationConfigDetails.
         
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT LocationConfigBrowseFrame           NO-ERROR.
   DELETE OBJECT LocationConfigBrowse                NO-ERROR.
   DELETE OBJECT LocationConfigBrowseButtons         NO-ERROR.
   DELETE OBJECT LocationConfigDetailsForm           NO-ERROR.
   DELETE OBJECT LocationConfigDetailsButtons        NO-ERROR.
   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationConfigBrowse Procedure 
PROCEDURE pLocationConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "locationconfig_details_form"}
   
   LocationConfigBrowse              = NEW browseTable("locationconfig_browse").
   LocationConfigBrowse:BrowseWidth  = 965.
   LocationConfigBrowse:BrowseHeight = 455.
   LocationConfigBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   LocationConfigBrowse:insertColumn(fTL("Config ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LocationConfig}
   
   LocationConfigBrowse:insertColumn(fTL("Direct User To Suggested Location"), 400, "CHARACTER", "LEFT", FALSE). 
     
   /*Body*/
   LocationConfigBrowse:startBody().
   
   FOR EACH LocationConfig NO-LOCK: /*idx = LocationConfigID*/
      logRecordFoundLocationConfig = TRUE.      
      LocationConfigBrowse:startRow(LocationConfig.LocationConfigID, "selectLocationConfigRow(this," + '"' 
                                    + STRING(LocationConfig.LocationConfigID) + '"' + ");", "").
      LocationConfigBrowse:insertData(LocationConfig.LocationConfigID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i LocationConfig}

      LocationConfigBrowse:insertData(STRING(LocationConfig.DirectUserToSuggestedLocation, "Yes/No"), "LEFT").
     
      /* Add hidden fields */
      LocationConfigBrowse:insertHiddenData("LocationConfigVersionID",LocationConfig.VersionID).
      
      LocationConfigBrowse:endRow().
      
   END. /*FOR EACH LocationConfig NO-LOCK */
   
   LocationConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationConfigBrowse:getErrors().
   
   /* Create a new frame */
   LocationConfigBrowseFrame = NEW pageFrame().
   LocationConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LocationConfigBrowseFrame:FormAction="dbLocationConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LocationConfigBrowseFrame:formOpen("locationconfig_browse_form").
   
   /* Start the Frame Header */
   LocationConfigBrowseFrame:insertSpacer(5).
   LocationConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LocationConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LocationConfigBrowseFrame:frameClose().
   LocationConfigBrowseFrame:insertSpacer(10).
   
   LocationConfigBrowseFrame:insertHiddenField("locationconfig_browse_scroll","").
   LocationConfigBrowseFrame:insertHiddenField("LocationConfigID","").
   LocationConfigBrowseFrame:insertHiddenField("LocationConfigVersionID","").
   LocationConfigBrowseFrame:insertHiddenField("form_name","locationconfig_browse_form").
   LocationConfigBrowseFrame:insertHiddenField("prog_name","adLocationConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationConfigBrowseFrame}
   
   LocationConfigBrowseFrame:formClose().
   
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   LocationConfigBrowseButtons = NEW buttonBar().
   LocationConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   LocationConfigBrowseButtons:addButton("locationconfig_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewLocationConfigDetails('locationconfig_details_form');",
                                         (IF intSelectedLocationConfig > 0 THEN "" ELSE "Disabled")).
  
   IF NOT logPreventDataCreates THEN
   DO:  
      LocationConfigBrowseButtons:addButton("locationconfig_browse_form_btn_create",
                                            fTL("Create"),
                                            "createLocationConfig('locationconfig_details_form');",
                                            (IF logRecordFoundLocationConfig THEN "Disabled" ELSE "")).
   END.                                            
                                       
   LocationConfigBrowseButtons:closeBar().  
   LocationConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationConfigDetails Procedure 
PROCEDURE pLocationConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "locationconfig_details_form"}
   ASSIGN chrDisplayFieldList  = "LocationConfigID,DirectUserToSuggestedLocation"
          chrEditFieldList     = "DirectUserToSuggestedLocation"
          chrNewFieldList      = "DirectUserToSuggestedLocation"
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   LocationConfigDetailsForm             = NEW dataForm("locationconfig_details_form").
   LocationConfigDetailsForm:WebStream   = STREAM WebStream:HANDLE.
   
   LocationConfigDetailsForm:FormAction  = "dbLocationConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationConfigDetailsForm:FormWidth   = 460.
   LocationConfigDetailsForm:FormHeight  = 200.
   LocationConfigDetailsForm:FormTitle   = "Location Config details".
   LocationConfigDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   LocationConfigDetailsForm:insertPaddingColumn(30).
   LocationConfigDetailsForm:insertColumn(265).
   
   /* Fields */
   LocationConfigDetailsForm:startRow().
   LocationConfigDetailsForm:insertLabel(fTL("Config ID")).
   LocationConfigDetailsForm:insertTextField("LocationConfigID", "", 75, TRUE).  
   
   LocationConfigDetailsForm:startRow().
   LocationConfigDetailsForm:insertLabel(fTL("Direct User To Suggested Location")).
   LocationConfigDetailsForm:insertComboField("DirectUserToSuggestedLocation", "", 168, TRUE).
   LocationConfigDetailsForm:insertComboPairs("DirectUserToSuggestedLocation", "yes", "Yes").
   LocationConfigDetailsForm:insertComboPairs("DirectUserToSuggestedLocation","no", "No").
      
   {webGetOptionalFormFields.i pLocationConfigDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LocationConfigDetailsForm:insertHiddenField("locationconfig_browse_scroll", "").
   LocationConfigDetailsForm:insertHiddenField("form_name", "locationconfig_details_form").
   LocationConfigDetailsForm:insertHiddenField("prog_name", "adLocationConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationConfigDetailsForm}
   
   /* Create Button Bar */
   LocationConfigDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataCreates THEN 
   DO:
      LocationConfigDetailsButtons:addButton("locationconfig_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateLocationConfig('locationconfig_details_form');").
                                             
      LocationConfigDetailsButtons:addButton("locationconfig_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); disablePopup('locationconfig_details_form_popup');").
      
      LocationConfigDetailsButtons:closeBar().  
   END.
   
   /* Assign the Button Bar Object to the Form Object */
   LocationConfigDetailsForm:FormButtons = LocationConfigDetailsButtons.
   
   LocationConfigDetailsForm:endForm(). 
   
   LocationConfigDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pLocationConfigDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            LocationConfigDetailsForm:startRow().
            LocationConfigDetailsForm:insertLabel(fTL("Field Label")).
            LocationConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(EXCLUDE-pLocationConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationConfigDetailsFields Procedure 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


