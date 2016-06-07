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

  Created: 15/01/2015

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
DEFINE VARIABLE intSelectedPsFontCharacter    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPsFontCharacterRow         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPsFontCharacterRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intPsFontCharacterID          AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPsFontCharacterID          AS CHARACTER   NO-UNDO.


/* Objects */
DEFINE VARIABLE PsFontCharacterBrowseFrame    AS pageFrame.
DEFINE VARIABLE PsFontCharacterBrowse         AS browseTable.
DEFINE VARIABLE PsFontCharacterBrowseButtons  AS buttonBar.
DEFINE VARIABLE PsFontCharacterDetailsForm    AS dataForm.
DEFINE VARIABLE PsFontCharacterDetailsButtons AS buttonBar.

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
      chrPsFontCharacterID          = get-value("PsFontCharacterID")
      intPsFontCharacterID          = INTEGER(chrPsFontCharacterID)
      intSelectedPsFontCharacter    = INTEGER(intPsFontCharacterID)
      chrScrollToPsFontCharacterRow = STRING(INTEGER(get-value("psfont_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPsFontCharacterID <> "" THEN
      chrPsFontCharacterRow = 'selectPsFontCharacterRow(document.getElementById("psfontcharacter_browse_row_' + chrPsFontCharacterID + '"),"' 
         + chrPsFontCharacterID +  '");'.
                                                          
  
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("psfontcharacter_browse").scrollTop=' + chrScrollToPsFontCharacterRow 
      + chrPsFontCharacterRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "PsFont Character".
   ThisPage:FrameTitle    = "PsFont Character".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for PsFont Character */
   ThisPage:addJavaScript("psfontcharacter.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPsFontCharacterBrowse.
   
   /******* Popup Browsers and Forms ********/
   IF intPsFontCharacterID <> 0 THEN
      FIND FIRST PsFontCharacter NO-LOCK 
         WHERE PsFontCharacter.PsFontCharacterID = intSelectedPsFontCharacter NO-ERROR.
         
   RUN pPsFontCharacterDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT PsFontCharacterBrowseFrame     NO-ERROR.
   DELETE OBJECT PsFontCharacterBrowse          NO-ERROR.
   DELETE OBJECT PsFontCharacterBrowseButtons   NO-ERROR.
   DELETE OBJECT PsFontCharacterDetailsForm     NO-ERROR.
   DELETE OBJECT PsFontCharacterDetailsButtons  NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationBrowse Procedure 
PROCEDURE pPsFontCharacterBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "psfontcharacter_details_form"}
   
   PsFontCharacterBrowse              = NEW browseTable("psfontcharacter_browse").
   PsFontCharacterBrowse:BrowseWidth  = 965.
   PsFontCharacterBrowse:BrowseHeight = 455.
   PsFontCharacterBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the PsFont Character ID as first Column */
   PsFontCharacterBrowse:insertColumn(fTL("PsFont Character ID"),125, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PsFontCharacter}
   PsFontCharacterBrowse:insertColumn(fTL("Ps Font ID"), 100, "INTEGER", FALSE).
   PsFontCharacterBrowse:insertColumn(fTL("Character"),  125, "CHARACTER", FALSE).
   PsFontCharacterBrowse:insertColumn(fTL("Spacing"),    150, "INTEGER", FALSE).
   PsFontCharacterBrowse:insertColumn(fTL("Post Script"),190, "CHARACTER", FALSE).
   PsFontCharacterBrowse:insertColumn(fTL("Active"),     205, "LOGICAL", FALSE).
   
   
  
   /*Body*/
   PsFontCharacterBrowse:startBody().
   
   
   /* Body for getting all the information from the DB must change to fit in with PsFont */
   FOR EACH PsFontCharacter NO-LOCK /*idx=ActiveListingSequence*/
      BY PsFontCharacter.PsFontCharacterID:   
      
      /*      FOR EACH PsFont NO-LOCK NO-ERROR.*/
      
      PsFontCharacterBrowse:startRow(PsFontCharacter.PsFontCharacterID, "selectPsFontCharacterRow(this," + '"' + STRING(PsFontCharacter.PsFontCharacterID) + '"' + ");", "").
      PsFontCharacterBrowse:insertData(PsFontCharacter.PsFontCharacterID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PsFontCharacter}
      PsFontCharacterBrowse:insertData(PsFontCharacter.PsFontID).
      PsFontCharacterBrowse:insertData((IF AVAILABLE PsFontCharacter THEN STRING(PsFontCharacter.Character) ELSE "")).
      PsFontCharacterBrowse:insertData((IF AVAILABLE PsFontCharacter THEN STRING(PsFontCharacter.Spacing) ELSE "")).
      PsFontCharacterBrowse:insertData((IF AVAILABLE PsFontCharacter THEN STRING(PsFontCharacter.PostScript) ELSE "")).
      PsFontCharacterBrowse:insertData((IF AVAILABLE PsFontCharacter THEN STRING(PsFontCharacter.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      PsFontCharacterBrowse:insertHiddenData("PsFontCharacterVersionID",PsFontCharacter.VersionID).
      PsFontCharacterBrowse:insertHiddenData("PsFontCharacterID", PsFontCharacter.PsFontCharacterID).
      
      PsFontCharacterBrowse:endRow().
      
   END. /*FOR EACH PsFont NO-LOCK*/
   
   PsFontCharacterBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PsFontCharacterBrowse:getErrors().
   
   /* Create a new frame */
   PsFontCharacterBrowseFrame = NEW pageFrame().
   PsFontCharacterBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PsFontCharacterBrowseFrame:FormAction="dbPsFontCharacterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PsFontCharacterBrowseFrame:formOpen("psfontcharacter_browse_form").
   
   /* Start the Frame Header */
   PsFontCharacterBrowseFrame:insertSpacer(5).
   PsFontCharacterBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PsFontCharacterBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PsFontCharacterBrowseFrame:frameClose().
   PsFontCharacterBrowseFrame:insertSpacer(10).
   
   PsFontCharacterBrowseFrame:insertHiddenField("psfontcharacter_browse_scroll","").
   PsFontCharacterBrowseFrame:insertHiddenField("PsFontCharacterID","").
   PsFontCharacterBrowseFrame:insertHiddenField("PsFontCharacterVersionID","").
   PsFontCharacterBrowseFrame:insertHiddenField("form_name","psfontcharacter_browse_form").
   PsFontCharacterBrowseFrame:insertHiddenField("prog_name","adPsFontCharacter.p").
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PsFontCharacterBrowseFrame}
   
   PsFontCharacterBrowseFrame:formClose().
   
   /* Create Button Bar */
   PsFontCharacterBrowseButtons = NEW buttonBar().
   PsFontCharacterBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   PsFontCharacterBrowseButtons:addButton("psfontcharacter_browse_form_btn_details",
      fTL("Details"),
      "viewPsFontCharacterDetails('psfontcharacter_details_form');",
      (IF intSelectedPsFontCharacter > 0 THEN "" ELSE "Disabled")).
   
   PsFontCharacterBrowseButtons:addButton("psfontcharacter_browse_form_btn_create",
      fTL("Create"),
      "createPsFontCharacter('psfontcharacter_details_form');",
      "").
   
   PsFontCharacterBrowseButtons:closeBar().  
   PsFontCharacterBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetails Procedure 
PROCEDURE pPsFontCharacterDetails :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "psfontcharacter_details_form"}
   
   /* Assigns the corresponding variables to the lists or groups they are included in */   
   ASSIGN 
      chrDisplayFieldList  = "PsFontCharacterID,PsFontID,Character,Spacing,PostScript,Active"
      chrEditFieldList     = "PsFontID,Character,Spacing,PostScript,Active"
      chrNewFieldList      = "PsFontID,Character,Spacing,PostScript,Active"
      chrRequiredFieldList = "PsFontID,Character,Spacing,PostScript,Active"
      chrExtraFieldList    = ""
      chrValidateFieldList = "".
      
   
   PsFontCharacterDetailsForm = NEW dataForm("psfontcharacter_details_form").
   PsFontCharacterDetailsForm:WebStream = STREAM WebStream:HANDLE.
   PsFontCharacterDetailsForm:FormAction = "dbPsFontCharacterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PsFontCharacterDetailsForm:FormWidth   = 460.
   PsFontCharacterDetailsForm:FormHeight  = 300.
   PsFontCharacterDetailsForm:FormTitle   = fTL("PsFont Character Details").
   PsFontCharacterDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PsFontCharacterDetailsForm:insertPaddingColumn(30).
   PsFontCharacterDetailsForm:insertColumn(130).
   PsFontCharacterDetailsForm:insertColumn(120).
   PsFontCharacterDetailsForm:insertColumn(20).
   PsFontCharacterDetailsForm:insertColumn(4).
  
   /* Fields */
   PsFontCharacterDetailsForm:startRow().
   PsFontCharacterDetailsForm:insertLabel("PsFont Character ID").
   PsFontCharacterDetailsForm:insertTextField("PsFontCharacterID", "", 110, TRUE). 
   
   PsFontCharacterDetailsForm:startRow().
   PsFontCharacterDetailsForm:insertLabel("PsFont ID").
   PsFontCharacterDetailsForm:insertTextField("PsFontID", "", 110, TRUE). 
   
   PsFontCharacterDetailsForm:startRow().
   PsFontCharacterDetailsForm:insertLabel("Character").
   PsFontCharacterDetailsForm:insertTextField("Character", "", 110, TRUE). 
      
   PsFontCharacterDetailsForm:startRow().
   PsFontCharacterDetailsForm:insertLabel("Spacing").
   PsFontCharacterDetailsForm:insertTextField("Spacing", "", 110, TRUE).  
   
   PsFontCharacterDetailsForm:startRow().
   PsFontCharacterDetailsForm:insertLabel("Post Script").
   PsFontCharacterDetailsForm:insertTextField("PostScript", "", 110, TRUE).                                                                                                  
   
   PsFontCharacterDetailsForm:startRow().
   PsFontCharacterDetailsForm:insertLabel(fTL("Active")). 
   PsFontCharacterDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PsFontCharacterDetailsForm:insertComboPairs("Active", "yes", "Active").
   PsFontCharacterDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPsFontCharacterDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PsFontCharacterDetailsForm:insertHiddenField("psfontcharacter_browse_scroll", "").
   PsFontCharacterDetailsForm:insertHiddenField("form_name", "psfontcharacter_details_form").
   PsFontCharacterDetailsForm:insertHiddenField("prog_name", "adPsFontCharacter.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PsFontCharacterDetailsForm}
   
   /* Create Button Bar */
   PsFontCharacterDetailsButtons = NEW buttonBar().
   PsFontCharacterDetailsButtons:addButton("psfontcharacter_details_form_btn_save", 
      fTL("Save"), 
      "updatePsFontCharacter('psfontcharacter_details_form');").
   PsFontCharacterDetailsButtons:addButton("psfontcharacter_details_form_btn_cancel", 
      fTL("Cancel"), 
      "cancelUpdate('UserCancelled','process_mode'); disablePopup('psfontcharacter_details_form_popup');").
   PsFontCharacterDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PsFontCharacterDetailsForm:FormButtons = PsFontCharacterDetailsButtons.
   
   PsFontCharacterDetailsForm:endForm(). 
   
   PsFontCharacterDetailsForm:displayForm(). 
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetailsFields Procedure 
PROCEDURE pPsFontCharacterDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            PsFontCharacterDetailsForm:startRow().
            PsFontCharacterDetailsForm:insertLabel(fTL("Field Label")).
            PsFontCharacterDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
