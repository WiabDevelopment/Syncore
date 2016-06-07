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

DEFINE VARIABLE intSelectedOperationType    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectOperationTypeRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToOperationTypeRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOperationTypeID          AS CHARACTER NO-UNDO.

/* Definitions for System Options for Admin */
{getAdminOptions.i}

/* Objects */
DEFINE VARIABLE OperationTypeBrowseFrame    AS pageFrame.
DEFINE VARIABLE OperationTypeBrowse         AS browseTable.
DEFINE VARIABLE OperationTypeBrowseButtons  AS buttonBar.
DEFINE VARIABLE OperationTypeDetailsForm    AS dataForm.
DEFINE VARIABLE OperationTypeDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pOperationTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOperationTypeBrowse Procedure 
PROCEDURE pOperationTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "operationtype_details_form"}
   
   OperationTypeBrowse              = NEW browseTable("operationtype_browse").
   OperationTypeBrowse:BrowseWidth  = 965.
   OperationTypeBrowse:BrowseHeight = 455.
   OperationTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   OperationTypeBrowse:insertColumn(fTL("OptType"),        60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i OperationType}
   
   OperationTypeBrowse:insertColumn(fTL("List Seq"),       60, "INTEGER", FALSE).
   OperationTypeBrowse:insertColumn(fTL("OptType Code"),  120, "CHARACTER", "left", FALSE).
   OperationTypeBrowse:insertColumn(fTL("OptType Name"),  150, "CHARACTER", "left", FALSE).
   OperationTypeBrowse:insertColumn(fTL("OptType Descr"), 220, "CHARACTER", "left", FALSE).
   OperationTypeBrowse:insertColumn(fTL("Debug"),          80, "LOGICAL", FALSE).
   OperationTypeBrowse:insertColumn(fTL("Active"),         50, "LOGICAL", FALSE).
   
   /*Body*/
   OperationTypeBrowse:startBody().
   
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH OperationType NO-LOCK
      BY    OperationType.Active DESCENDING 
      BY    OperationType.ListingSequence 
      BY    OperationType.OperationTypeID:
      
      OperationTypeBrowse:startRow(OperationType.OperationTypeID, "selectOperationTypeRow(this," + '"' 
                                                                     + STRING(OperationType.OperationTypeID) + '"' + ");", "").
      OperationTypeBrowse:insertData(OperationType.OperationTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i OperationType}
      
      OperationTypeBrowse:insertData(STRING(OperationType.ListingSequence)).
      OperationTypeBrowse:insertData(OperationType.TypeCode, "left").
      OperationTypeBrowse:insertData(OperationType.TypeName, "left").
      OperationTypeBrowse:insertData(OperationType.TypeDescr, "left").
      OperationTypeBrowse:insertData(STRING(OperationType.DebuggingOn, "Yes/No")).
      OperationTypeBrowse:insertData(STRING(OperationType.Active, "Yes/No")).
      
      /* Add hidden fields */
      OperationTypeBrowse:insertHiddenData("OperationTypeVersionID",OperationType.VersionID).
      
      OperationTypeBrowse:endRow().
      
   END. /* FOR EACH OperationType NO-LOCK */
   
   OperationTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + OperationTypeBrowse:getErrors().
   
   /* Create a new frame */
   OperationTypeBrowseFrame           = NEW pageFrame().
   OperationTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   OperationTypeBrowseFrame:FormAction="dbOperationTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   OperationTypeBrowseFrame:formOpen("operationtype_browse_form").
   
   /* Start the Frame Header */
   OperationTypeBrowseFrame:insertSpacer(5).
   OperationTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   OperationTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   OperationTypeBrowseFrame:frameClose().
   OperationTypeBrowseFrame:insertSpacer(10).
   
   OperationTypeBrowseFrame:insertHiddenField("operationtype_browse_scroll","").
   OperationTypeBrowseFrame:insertHiddenField("OperationTypeID","").
   OperationTypeBrowseFrame:insertHiddenField("OperationTypeVersionID","").
   OperationTypeBrowseFrame:insertHiddenField("form_name","operationtype_browse_form").
   OperationTypeBrowseFrame:insertHiddenField("prog_name","adOperationTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OperationTypeBrowseFrame}
   
   OperationTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   OperationTypeBrowseButtons           = NEW buttonBar().
   OperationTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   OperationTypeBrowseButtons:addButton("operationtype_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewOperationTypeDetails('operationtype_details_form');",
                                        "Disabled").
                                        
   IF NOT logPreventDataCreates THEN
   DO:   
      OperationTypeBrowseButtons:addButton("operationtype_browse_form_btn_create",
                                           fTL("Create"),
                                           "createOperationType('operationtype_details_form');",
                                           "").
                                           
      OperationTypeBrowseButtons:addButton("operationtype_browse_form_btn_applinks",
                                           fTL("App Links"),
                                           "viewAppLinkBrowse('operationtype_browse_form','OperationType');",
                                           "Disabled").
   END.                                        
   
   OperationTypeBrowseButtons:closeBar().  
   OperationTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOperationTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOperationTypeDetails Procedure 
PROCEDURE pOperationTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "operationtype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "OperationTypeID,ListingSequence,TypeCode,TypeName,TypeDescr,DebuggingOn,Active"
          chrEditFieldList     = "ListingSequence,TypeName,TypeDescr,DebuggingOn,Active"
          chrNewFieldList      = "ListingSequence,TypeCode,TypeName,TypeDescr,DebuggingOn,Active"
          chrRequiredFieldList = "ListingSequence,TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   OperationTypeDetailsForm           = NEW dataForm("operationtype_details_form").
   OperationTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   OperationTypeDetailsForm:FormAction = "dbOperationTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   OperationTypeDetailsForm:FormWidth  = 460.
   OperationTypeDetailsForm:FormHeight = 300.
   OperationTypeDetailsForm:FormTitle  = "OperationType Details".
   OperationTypeDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   OperationTypeDetailsForm:insertPaddingColumn(50).
   OperationTypeDetailsForm:insertColumn(100).
   OperationTypeDetailsForm:insertColumn(120).
   OperationTypeDetailsForm:insertColumn(20).
   OperationTypeDetailsForm:insertColumn(4).
   OperationTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   OperationTypeDetailsForm:startRow().
   OperationTypeDetailsForm:insertLabel("OptType ID").
   OperationTypeDetailsForm:insertTextField("OperationTypeID", "", 110, TRUE).  
   
   OperationTypeDetailsForm:startRow().
   OperationTypeDetailsForm:insertLabel("Listing Seq").
   OperationTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   OperationTypeDetailsForm:startRow().
   OperationTypeDetailsForm:insertLabel("OptType Code").
   OperationTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   OperationTypeDetailsForm:startRow().
   OperationTypeDetailsForm:insertLabel("OptType Name").
   OperationTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).
   
   OperationTypeDetailsForm:startRow().
   OperationTypeDetailsForm:insertLabel("OptType Descr").
   OperationTypeDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).

   OperationTypeDetailsForm:startRow().
   OperationTypeDetailsForm:insertLabel(fTL("Debug Mode")). 
   OperationTypeDetailsForm:insertComboField("DebuggingOn", "no", 110, TRUE).  
   OperationTypeDetailsForm:insertComboPairs("DebuggingOn", "yes", "Active").
   OperationTypeDetailsForm:insertComboPairs("DebuggingOn", "no",  "Not Active").
   
   OperationTypeDetailsForm:startRow().
   OperationTypeDetailsForm:insertLabel(fTL("Active")). 
   OperationTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   OperationTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   OperationTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pOperationTypeDetailsFields}
   
   /* Add Hidden Fields*/
   OperationTypeDetailsForm:insertHiddenField("operationtype_browse_scroll", "").
   OperationTypeDetailsForm:insertHiddenField("form_name", "operationtype_details_form").
   OperationTypeDetailsForm:insertHiddenField("prog_name", "adOperationTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OperationTypeDetailsForm}
   
   /* Create Button Bar */
   OperationTypeDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      OperationTypeDetailsButtons:addButton("operationtype_details_form_btn_save", 
                                            fTL("Save"), 
                                            "updateOperationType('operationtype_details_form');").
   
   OperationTypeDetailsButtons:addButton("operationtype_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "cancelUpdate('UserCancelled','process_mode'); disablePopup('operationtype_details_form_popup');").
   
   OperationTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   OperationTypeDetailsForm:FormButtons = OperationTypeDetailsButtons.
   
   OperationTypeDetailsForm:endForm(). 
   
   OperationTypeDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOperationTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOperationTypeDetailsFields Procedure 
PROCEDURE pOperationTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         OperationTypeDetailsForm:startRow().
         OperationTypeDetailsForm:insertLabel(fTL("Field Label")).
         OperationTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adOperationTypeAdmin_operationtype_details_form.i}
      
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
   
   ASSIGN chrOperationTypeID          = get-value("OperationTypeID")
          intSelectedOperationType    = INTEGER(chrOperationTypeID)
          chrScrollToOperationTypeRow = STRING(INTEGER(get-value("operationtype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrOperationTypeID <> "" THEN
      chrSelectOperationTypeRow = 'selectOperationTypeRow(document.getElementById("operationtype_browse_row_' 
                                     + chrOperationTypeID + '"),"' + chrOperationTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                     + 'document.getElementById("operationtype_browse").scrollTop=' + chrScrollToOperationTypeRow 
                     + chrSelectOperationTypeRow.
                     
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                        
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "OperationType Admin".
   ThisPage:FrameTitle    = "OperationType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("operationtype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pOperationTypeBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pOperationTypeDetails.
   
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
   DELETE OBJECT OperationTypeBrowseFrame         NO-ERROR.
   DELETE OBJECT OperationTypeBrowse              NO-ERROR.
   DELETE OBJECT OperationTypeBrowseButtons       NO-ERROR.
   DELETE OBJECT OperationTypeDetailsForm         NO-ERROR.
   DELETE OBJECT OperationTypeDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

