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

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedBomType    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectBomTypeRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToBomTypeRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrBomTypeID          AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE BomTypeBrowseFrame    AS pageFrame.
DEFINE VARIABLE BomTypeBrowse         AS browseTable.
DEFINE VARIABLE BomTypeBrowseButtons  AS buttonBar.
DEFINE VARIABLE BomTypeDetailsForm    AS dataForm.
DEFINE VARIABLE BomTypeDetailsButtons AS buttonBar.

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
   
   ASSIGN chrBomTypeID          = get-value("BomTypeID")
          intSelectedBomType    = INTEGER(chrBomTypeID)
          chrScrollToBomTypeRow = STRING(INTEGER(get-value("bomtype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrBomTypeID <> "" THEN
      chrSelectBomTypeRow = 'selectBomTypeRow(document.getElementById("bomtype_browse_row_' + chrBomTypeID + '"),"' 
                                + chrBomTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("bomtype_browse").scrollTop=' + chrScrollToBomTypeRow 
                             + chrSelectBomTypeRow.
                             
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                                
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Bom Type Admin".
   ThisPage:FrameTitle    = "Bom Type Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("bomtype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pBomTypeBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pBomTypeDetails.

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
   DELETE OBJECT BomTypeBrowseFrame         NO-ERROR.
   DELETE OBJECT BomTypeBrowse              NO-ERROR.
   DELETE OBJECT BomTypeBrowseButtons       NO-ERROR.
   DELETE OBJECT BomTypeDetailsForm         NO-ERROR.
   DELETE OBJECT BomTypeDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomTypeBrowse Procedure 
PROCEDURE pBomTypeBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "bomtype_details_form"}
   
   BomTypeBrowse              = NEW browseTable("bomtype_browse").
   BomTypeBrowse:BrowseWidth  = 965.
   BomTypeBrowse:BrowseHeight = 455.
   BomTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   BomTypeBrowse:insertColumn(fTL("BomType"),        60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i BomType}
   
   BomTypeBrowse:insertColumn(fTL("Type Code"),  130, "CHARACTER", "left", FALSE).
   BomTypeBrowse:insertColumn(fTL("Type Name"),  150, "CHARACTER", "left", FALSE).
   BomTypeBrowse:insertColumn(fTL("Type Descr"), 180, "CHARACTER", "left", FALSE).
   BomTypeBrowse:insertColumn(fTL("Active"),      80, "LOGICAL", FALSE).
   BomTypeBrowse:insertColumn(fTL("List Seq"),    80, "INTEGER", "right", FALSE).
   
   /*Body*/
   BomTypeBrowse:startBody().
   
   FOR EACH BomType NO-LOCK
      BY    BomType.Active DESC 
      BY    BomType.BomTypeID:
      
      BomTypeBrowse:startRow(BomType.BomTypeID, 
                             "selectBomTypeRow(this," + '"' + STRING(BomType.BomTypeID) + '"' + ");", 
                             "").
      BomTypeBrowse:insertData(BomType.BomTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i BomType}
      
      BomTypeBrowse:insertData(BomType.TypeCode, "left").
      BomTypeBrowse:insertData(BomType.TypeName, "left").
      BomTypeBrowse:insertData(BomType.TypeDescr, "left").
      BomTypeBrowse:insertData(STRING(BomType.Active, "Yes/No")).
      BomTypeBrowse:insertData(STRING(BomType.ListingSequence), "right").
      
      /* Add hidden fields */
      BomTypeBrowse:insertHiddenData("BomTypeVersionID",BomType.VersionID).
      
      BomTypeBrowse:endRow().
      
   END. /* FOR EACH BomType NO-LOCK */
   
   BomTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + BomTypeBrowse:getErrors().
   
   /* Create a new frame */
   BomTypeBrowseFrame           = NEW pageFrame().
   BomTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   BomTypeBrowseFrame:FormAction="dbBomTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   BomTypeBrowseFrame:formOpen("bomtype_browse_form").
   
   /* Start the Frame Header */
   BomTypeBrowseFrame:insertSpacer(5).
   BomTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   BomTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   BomTypeBrowseFrame:frameClose().
   BomTypeBrowseFrame:insertSpacer(10).
   
   BomTypeBrowseFrame:insertHiddenField("bomtype_browse_scroll","").
   BomTypeBrowseFrame:insertHiddenField("BomTypeID","").
   BomTypeBrowseFrame:insertHiddenField("BomTypeVersionID","").
   BomTypeBrowseFrame:insertHiddenField("form_name","bomtype_browse_form").
   BomTypeBrowseFrame:insertHiddenField("prog_name","adBomTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomTypeBrowseFrame}
   
   BomTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   BomTypeBrowseButtons           = NEW buttonBar().
   BomTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   BomTypeBrowseButtons:addButton("bomtype_browse_form_btn_details",
                                   fTL("Details"),
                                   "viewBomTypeDetails('bomtype_details_form');",
                                   "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO:   
      BomTypeBrowseButtons:addButton("bomtype_browse_form_btn_create",
                                      fTL("Create"),
                                      "createBomType('bomtype_details_form');").
                                      
      BomTypeBrowseButtons:addButton("bomtype_browse_form_btn_applinks",
                                      fTL("App Links"),
                                      "viewAppLinkBrowse('bomtype_browse_form','BomType');",
                                      "Disabled").
                                      
   END.                                
   
   BomTypeBrowseButtons:closeBar().  
   BomTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomTypeDetails Procedure 
PROCEDURE pBomTypeDetails:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "bomtype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "BomTypeID,TypeCode,TypeName,TypeDescr,Active,ListingSequence"
          chrEditFieldList     = "TypeName,TypeDescr,Active,ListingSequence"
          chrNewFieldList      = "TypeCode,TypeName,TypeDescr,Active,ListingSequence"
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   BomTypeDetailsForm           = NEW dataForm("bomtype_details_form").
   BomTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   BomTypeDetailsForm:FormAction = "dbBomTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BomTypeDetailsForm:FormWidth  = 460.
   BomTypeDetailsForm:FormHeight = 300.
   BomTypeDetailsForm:FormTitle  = "Bom Type Details".
   BomTypeDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   BomTypeDetailsForm:insertPaddingColumn(50).
   BomTypeDetailsForm:insertColumn(100).
   BomTypeDetailsForm:insertColumn(120).
   BomTypeDetailsForm:insertColumn(20).
   BomTypeDetailsForm:insertColumn(4).
  
   /* Fields */
   BomTypeDetailsForm:startRow().
   BomTypeDetailsForm:insertLabel("Type ID").
   BomTypeDetailsForm:insertTextField("BomTypeID", "", 110, TRUE).  
      
   BomTypeDetailsForm:startRow().
   BomTypeDetailsForm:insertLabel(fTL("Type Code")).
   BomTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   BomTypeDetailsForm:startRow().
   BomTypeDetailsForm:insertLabel(fTL("Type Name")).
   BomTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).
   
   BomTypeDetailsForm:startRow().
   BomTypeDetailsForm:insertLabel(fTL("Type Descr")).
   BomTypeDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).
   
   BomTypeDetailsForm:startRow().
   BomTypeDetailsForm:insertLabel(fTL("List Seq")).
   BomTypeDetailsForm:insertTextField("ListingSequence", "", 220, TRUE).
   
   BomTypeDetailsForm:startRow().
   BomTypeDetailsForm:insertLabel(fTL("Active")). 
   BomTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   BomTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   BomTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pBomTypeDetailsFields}
   
   /* Add Hidden Fields*/
   BomTypeDetailsForm:insertHiddenField("bomtype_browse_scroll", "").
   BomTypeDetailsForm:insertHiddenField("form_name", "bomtype_details_form").
   BomTypeDetailsForm:insertHiddenField("prog_name", "adBomTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomTypeDetailsForm}
   
   /* Create Button Bar */
   BomTypeDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      BomTypeDetailsButtons:addButton("bomtype_details_form_btn_save", 
                                      fTL("Save"), 
                                      "updateBomType('bomtype_details_form');").
   
   BomTypeDetailsButtons:addButton("bomtype_details_form_btn_cancel", 
                                   fTL("Cancel"), 
                                   "cancelUpdate('UserCancelled','process_mode'); disablePopup('bomtype_details_form_popup');").
   
   BomTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   BomTypeDetailsForm:FormButtons = BomTypeDetailsButtons.
   
   BomTypeDetailsForm:endForm(). 
   
   BomTypeDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomTypeDetailsFields Procedure 
PROCEDURE pBomTypeDetailsFields:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         BomTypeDetailsForm:startRow().
         BomTypeDetailsForm:insertLabel(fTL("Field Label")).
         BomTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adBomTypeAdmin_bomtype_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

