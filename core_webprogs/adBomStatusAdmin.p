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

  Author: Lily Tran

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

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedBomStatus    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectBomStatusRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToBomStatusRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrBomStatusID          AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE BomStatusBrowseFrame    AS pageFrame.
DEFINE VARIABLE BomStatusBrowse         AS browseTable.
DEFINE VARIABLE BomStatusBrowseButtons  AS buttonBar.
DEFINE VARIABLE BomStatusDetailsForm    AS dataForm.
DEFINE VARIABLE BomStatusDetailsButtons AS buttonBar.

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
   
   ASSIGN chrBomStatusID          = get-value("BomStatusID")
          intSelectedBomStatus    = INTEGER(chrBomStatusID)
          chrScrollToBomStatusRow = STRING(INTEGER(get-value("bomstatus_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrBomStatusID <> "" THEN
      chrSelectBomStatusRow = 'selectBomStatusRow(document.getElementById("bomstatus_browse_row_' 
                                 + chrBomStatusID + '"),"' + chrBomStatusID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                    + 'document.getElementById("bomstatus_browse").scrollTop=' + chrScrollToBomStatusRow 
                    + chrSelectBomStatusRow.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
      
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Bom Status Admin".
   ThisPage:FrameTitle    = "Bom Status Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("bomstatus.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pBomStatusBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pBomStatusDetails.
   
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
   DELETE OBJECT BomStatusBrowseFrame         NO-ERROR.
   DELETE OBJECT BomStatusBrowse              NO-ERROR.
   DELETE OBJECT BomStatusBrowseButtons       NO-ERROR.
   DELETE OBJECT BomStatusDetailsForm         NO-ERROR.
   DELETE OBJECT BomStatusDetailsButtons      NO-ERROR.

   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusBrowse Procedure 
PROCEDURE pBomStatusBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "bomstatus_details_form"}
   
   BomStatusBrowse              = NEW browseTable("bomstatus_browse").
   BomStatusBrowse:BrowseWidth  = 965.
   BomStatusBrowse:BrowseHeight = 455.
   BomStatusBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   BomStatusBrowse:insertColumn(fTL("BomStatus ID"),90, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i BomStatus}
   
   BomStatusBrowse:insertColumn(fTL("Listing Seq"),  80, "INTEGER", FALSE).
   BomStatusBrowse:insertColumn(fTL("Status Code"), 150, "CHARACTER", "left", FALSE).
   BomStatusBrowse:insertColumn(fTL("Name"),        150, "CHARACTER", "left", FALSE).
   BomStatusBrowse:insertColumn(fTL("Description"), 300, "CHARACTER", "left", FALSE).
   BomStatusBrowse:insertColumn(fTL("Active"),       70, "LOGICAL",   "left", FALSE).
   
   /*Body*/
   BomStatusBrowse:startBody().
   
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH BomStatus NO-LOCK
      BY    BomStatus.Active DESCENDING 
      BY    BomStatus.ListingSequence 
      BY    BomStatus.BomStatusID:
      
      BomStatusBrowse:startRow(BomStatus.BomStatusID,
                               "selectBomStatusRow(this," + '"' + STRING(BomStatus.BomStatusID) + '"' + ");",
                               "").
      BomStatusBrowse:insertData(BomStatus.BomStatusID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i BomStatus}

      BomStatusBrowse:insertData(STRING(BomStatus.ListingSequence)).
      BomStatusBrowse:insertData(BomStatus.StatusCode, "left").
      BomStatusBrowse:insertData(BomStatus.StatusName, "left").
      BomStatusBrowse:insertData(BomStatus.StatusDescr, "left").
      BomStatusBrowse:insertData(STRING(BomStatus.Active, "Yes/No")).
      
      /* Add hidden fields */
      BomStatusBrowse:insertHiddenData("BomStatusVersionID",BomStatus.VersionID).
      
      BomStatusBrowse:endRow().
      
   END. /* FOR EACH BomStatus NO-LOCK */
   
   BomStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + BomStatusBrowse:getErrors().
   
   /* Create a new frame */
   BomStatusBrowseFrame           = NEW pageFrame().
   BomStatusBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   BomStatusBrowseFrame:FormAction="dbBomStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   BomStatusBrowseFrame:formOpen("bomstatus_browse_form").
   
   /* Start the Frame Header */
   BomStatusBrowseFrame:insertSpacer(5).
   BomStatusBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   BomStatusBrowse:displayBrowse().  
   
   /* End the Frame Header */
   BomStatusBrowseFrame:frameClose().
   BomStatusBrowseFrame:insertSpacer(10).
   
   BomStatusBrowseFrame:insertHiddenField("bomstatus_browse_scroll","").
   BomStatusBrowseFrame:insertHiddenField("BomStatusID","").
   BomStatusBrowseFrame:insertHiddenField("BomStatusVersionID","").
   BomStatusBrowseFrame:insertHiddenField("form_name","bomstatus_browse_form").
   BomStatusBrowseFrame:insertHiddenField("prog_name","adBomStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomStatusBrowseFrame}
   
   BomStatusBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   BomStatusBrowseButtons           = NEW buttonBar().
   BomStatusBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   BomStatusBrowseButtons:addButton("bomstatus_browse_form_btn_details",
                                    fTL("Details"),
                                    "viewBomStatusDetails('bomstatus_details_form');",
                                    "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO:
      BomStatusBrowseButtons:addButton("bomstatus_browse_form_btn_create",
                                       fTL("Create"),
                                       "createBomStatus('bomstatus_details_form');").
                                         
      BomStatusBrowseButtons:addButton("bomstatus_browse_form_btn_applinks",
                                       fTL("App Links"),
                                       "viewAppLinkBrowse('bomstatus_browse_form','BomStatus');",
                                       "Disabled").
   END.                                         
                                         
   
   BomStatusBrowseButtons:closeBar().  
   BomStatusBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStockStatusDetails Procedure 
PROCEDURE pBomStatusDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "bomstatus_details_form"}
   
   ASSIGN chrDisplayFieldList  = "BomStatusID,ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrEditFieldList     = "ListingSequence,StatusName,StatusDescr,Active"
          chrNewFieldList      = "ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrRequiredFieldList = "ListingSequence,StatusCode,StatusName,StatusDescr,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   BomStatusDetailsForm           = NEW dataForm("bomstatus_details_form").
   BomStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   BomStatusDetailsForm:FormAction = "dbBomStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BomStatusDetailsForm:FormWidth  = 460.
   BomStatusDetailsForm:FormHeight = 300.
   BomStatusDetailsForm:FormTitle  = "Bom Status Details".
   BomStatusDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   BomStatusDetailsForm:insertPaddingColumn(50).
   BomStatusDetailsForm:insertColumn(100).
   BomStatusDetailsForm:insertColumn(120).
   BomStatusDetailsForm:insertColumn(20).
   BomStatusDetailsForm:insertColumn(4).
   BomStatusDetailsForm:insertColumn(110).
   
   /* Fields */
   BomStatusDetailsForm:startRow().
   BomStatusDetailsForm:insertLabel("BomStatus ID").
   BomStatusDetailsForm:insertTextField("BomStatusID", "", 110, TRUE).  
   
   BomStatusDetailsForm:startRow().
   BomStatusDetailsForm:insertLabel("Listing Seq").
   BomStatusDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   BomStatusDetailsForm:startRow().
   BomStatusDetailsForm:insertLabel("Status Code").
   BomStatusDetailsForm:insertTextField("StatusCode", "", 110, TRUE).  
   
   BomStatusDetailsForm:startRow().
   BomStatusDetailsForm:insertLabel("Name").
   BomStatusDetailsForm:insertTextField("StatusName", "", 110, TRUE).
   
   BomStatusDetailsForm:startRow().
   BomStatusDetailsForm:insertLabel("Description").
   BomStatusDetailsForm:insertTextField("StatusDescr", "", 220, TRUE).
   
   BomStatusDetailsForm:startRow().
   BomStatusDetailsForm:insertLabel(fTL("Active")). 
   BomStatusDetailsForm:insertComboField("Active", "", 110, TRUE).  
   BomStatusDetailsForm:insertComboPairs("Active", "yes", "Active").
   BomStatusDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pBomStatusDetailsFields}
   
   /* Add Hidden Fields*/
   BomStatusDetailsForm:insertHiddenField("bomstatus_browse_scroll", "").
   BomStatusDetailsForm:insertHiddenField("form_name", "bomstatus_details_form").
   BomStatusDetailsForm:insertHiddenField("prog_name", "adBomStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomStatusDetailsForm}
   
   /* Create Button Bar */
   BomStatusDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      BomStatusDetailsButtons:addButton("bomstatus_details_form_btn_save", 
                                        fTL("Save"), 
                                        "updateBomStatus('bomstatus_details_form');").
   
   BomStatusDetailsButtons:addButton("bomstatus_details_form_btn_cancel", 
                                     fTL("Cancel"), 
                                     "cancelUpdate('UserCancelled','process_mode'); disablePopup('bomstatus_details_form_popup');").
   
   BomStatusDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   BomStatusDetailsForm:FormButtons = BomStatusDetailsButtons.
   
   BomStatusDetailsForm:endForm(). 
   
   BomStatusDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusDetailsFields Procedure 
PROCEDURE pBomStatusDetailsFields:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         BomStatusDetailsForm:startRow().
         BomStatusDetailsForm:insertLabel(fTL("Field Label")).
         BomStatusDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adBomStatusAdmin_bomstatus_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

