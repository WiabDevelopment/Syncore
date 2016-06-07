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

  Author: Nicholas Diessner

  Created: 14/01/2015

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
DEFINE VARIABLE intSelectedPsFont    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPsFontRow         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPsFontRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intPsFontID          AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPsFontID          AS CHARACTER   NO-UNDO.


/* Objects */
DEFINE VARIABLE PsFontBrowseFrame    AS pageFrame.
DEFINE VARIABLE PsFontBrowse         AS browseTable.
DEFINE VARIABLE PsFontBrowseButtons  AS buttonBar.
DEFINE VARIABLE PsFontDetailsForm    AS dataForm.
DEFINE VARIABLE PsFontDetailsButtons AS buttonBar.

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
   
   ASSIGN 
      chrPsFontID          = get-value("PsFontID")
      intPsFontID          = INTEGER(chrPsFontID)
      intSelectedPsFont    = INTEGER(intPsFontID)
      chrScrollToPsFontRow = STRING(INTEGER(get-value("psfont_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPsFontID <> "" THEN
      chrPsFontRow = 'selectPsFontRow(document.getElementById("psfont_browse_row_' + chrPsFontID + '"),"' 
         + chrPsFontID +  '");'.
                                                          
  
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("psfont_browse").scrollTop=' + chrScrollToPsFontRow 
      + chrPsFontRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "PsFont Admin".
   ThisPage:FrameTitle    = "PsFont Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for PsFont */
   ThisPage:addJavaScript("psfont.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPsFontBrowse.
   
   /******* Popup Browsers and Forms ********/
   IF intPsFontID <> 0 THEN
      FIND FIRST PsFont NO-LOCK 
         WHERE PsFont.PsFontID = intSelectedPsFont NO-ERROR.
         
   RUN pPsFontDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT PsFontBrowseFrame    NO-ERROR.
   DELETE OBJECT PsFontBrowse         NO-ERROR.
   DELETE OBJECT PsFontBrowseButtons  NO-ERROR.
   DELETE OBJECT PsFontDetailsForm    NO-ERROR.
   DELETE OBJECT PsFontDetailsButtons NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationBrowse Procedure 
PROCEDURE pPsFontBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "psfont_details_form"}
   
   PsFontBrowse              = NEW browseTable("psfont_browse").
   PsFontBrowse:BrowseWidth  = 965.
   PsFontBrowse:BrowseHeight = 455.
   PsFontBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the PsFont ID as first Column */
   PsFontBrowse:insertColumn(fTL("PsFont ID"),100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PsFont}
   
   PsFontBrowse:insertColumn(fTL("Font Name"),345, "CHARACTER", "LEFT", FALSE).
   PsFontBrowse:insertColumn(fTL("Font Size"),400, "INTEGER", FALSE).
   PsFontBrowse:insertColumn(fTL("Active"),   100, "LOGICAL", FALSE).
   
  
   /*Body*/
   PsFontBrowse:startBody().
   
   
   /* Query for getting all the information from the table PsFont*/
   FOR EACH PsFont NO-LOCK /*idx=ActiveListingSequence*/
      BY PsFont.PsFontID:   
            
      PsFontBrowse:startRow(PsFont.PsFontID, "selectPsFontRow(this," + '"' + STRING(PsFont.PsFontID) + '"' + ");", "").
      
      /* Set the first column value as PsFont ID*/
      PsFontBrowse:insertData(PsFont.PsFontID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PsFont}
      PsFontBrowse:insertData(PsFont.FontName, "LEFT").
      PsFontBrowse:insertData((IF AVAILABLE PsFont THEN STRING(PsFont.FontSize) ELSE "")).
      PsFontBrowse:insertData((IF AVAILABLE PsFont THEN STRING(PsFont.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      PsFontBrowse:insertHiddenData("PsFontVersionID",PsFont.VersionID).
      PsFontBrowse:insertHiddenData("PsFontID", PsFont.PsFontID).
      
      PsFontBrowse:endRow().
      
   END. /*FOR EACH PsFont NO-LOCK*/
   
   PsFontBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PsFontBrowse:getErrors().
   
   /* Create a new frame */
   PsFontBrowseFrame           = NEW pageFrame().
   PsFontBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PsFontBrowseFrame:FormAction="dbPsFontUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PsFontBrowseFrame:formOpen("psfont_browse_form").
   
   /* Start the Frame Header */
   PsFontBrowseFrame:insertSpacer(5).
   PsFontBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PsFontBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PsFontBrowseFrame:frameClose().
   PsFontBrowseFrame:insertSpacer(10).
   
   PsFontBrowseFrame:insertHiddenField("psfont_browse_scroll","").
   PsFontBrowseFrame:insertHiddenField("PsFontID","").
   PsFontBrowseFrame:insertHiddenField("PsFontVersionID","").
   PsFontBrowseFrame:insertHiddenField("form_name","psfont_browse_form").
   PsFontBrowseFrame:insertHiddenField("prog_name","adPsFontAdmin.p").
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PsFontBrowseFrame}
   
   PsFontBrowseFrame:formClose().
   
   /* Create Button Bar */
   PsFontBrowseButtons           = NEW buttonBar().
   PsFontBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   PsFontBrowseButtons:addButton("psfont_browse_form_btn_details",
      fTL("Details"),
      "viewPsFontDetails('psfont_details_form');",
      (IF intSelectedPsFont > 0 THEN "" ELSE "Disabled")).
   
   PsFontBrowseButtons:addButton("psfont_browse_form_btn_create",
      fTL("Create"),
      "createPsFont('psfont_details_form');",
      "").
   
   PsFontBrowseButtons:closeBar().  
   PsFontBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetails Procedure 
PROCEDURE pPsFontDetails :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "psfont_details_form"}
   
   
   /* Some fields will have to be changed to fit into the PsFont table fields  */
   
   ASSIGN 
      chrDisplayFieldList  = "PsFontID,FontName,FontSize,Active"
      chrEditFieldList     = "FontName,FontSize,Active"
      chrNewFieldList      = "FontName,FontSize,Active"
      chrRequiredFieldList = "FontName,FontSize,Active"
      chrExtraFieldList    = ""
      chrValidateFieldList = "".
      
   
   PsFontDetailsForm = NEW dataForm("psfont_details_form").
   PsFontDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PsFontDetailsForm:FormAction = "dbPsFontUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PsFontDetailsForm:FormWidth   = 460.
   PsFontDetailsForm:FormHeight  = 300.
   PsFontDetailsForm:FormTitle   = fTL("PsFont Details").
   PsFontDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PsFontDetailsForm:insertPaddingColumn(30).
   PsFontDetailsForm:insertColumn(130).
   PsFontDetailsForm:insertColumn(120).
   PsFontDetailsForm:insertColumn(20).
   PsFontDetailsForm:insertColumn(4).
  
   /* Fields */
   PsFontDetailsForm:startRow().
   PsFontDetailsForm:insertLabel("PsFont ID").
   PsFontDetailsForm:insertTextField("PsFontID", "", 110, TRUE). 
      
   PsFontDetailsForm:startRow().
   PsFontDetailsForm:insertLabel("Font Name").
   PsFontDetailsForm:insertTextField("FontName", "", 110, TRUE).  
   
   PsFontDetailsForm:startRow().
   PsFontDetailsForm:insertLabel("Font Size").
   PsFontDetailsForm:insertTextField("FontSize", "", 110, TRUE).                                                                                                  
   
   PsFontDetailsForm:startRow().
   PsFontDetailsForm:insertLabel(fTL("Active")). 
   PsFontDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PsFontDetailsForm:insertComboPairs("Active", "yes", "Active").
   PsFontDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPsFontDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PsFontDetailsForm:insertHiddenField("psfont_browse_scroll", "").
   PsFontDetailsForm:insertHiddenField("form_name", "psfont_details_form").
   PsFontDetailsForm:insertHiddenField("prog_name", "adPsFontAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PsFontDetailsForm}
   
   /* Create Button Bar */
   PsFontDetailsButtons = NEW buttonBar().
   PsFontDetailsButtons:addButton("psfont_details_form_btn_save", 
      fTL("Save"), 
      "updatePsFont('psfont_details_form');").
   PsFontDetailsButtons:addButton("psfont_details_form_btn_cancel", 
      fTL("Cancel"), 
      "cancelUpdate('UserCancelled','process_mode'); disablePopup('psfont_details_form_popup');").
   PsFontDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PsFontDetailsForm:FormButtons = PsFontDetailsButtons.
   
   PsFontDetailsForm:endForm(). 
   
   PsFontDetailsForm:displayForm(). 
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetailsFields Procedure 
PROCEDURE pPsFontDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            PsFontDetailsForm:startRow().
            PsFontDetailsForm:insertLabel(fTL("Field Label")).
            PsFontDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
