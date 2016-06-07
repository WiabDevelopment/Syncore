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

DEFINE VARIABLE intSelectedCountTaskType    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountTaskTypeRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountTaskTypeRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskTypeID          AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE CountTaskTypeBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskTypeBrowse         AS browseTable.
DEFINE VARIABLE CountTaskTypeBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskTypeDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskTypeDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pCountTaskTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskTypeBrowse Procedure 
PROCEDURE pCountTaskTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "counttasktype_details_form"}
   
   CountTaskTypeBrowse              = NEW browseTable("counttasktype_browse").
   CountTaskTypeBrowse:BrowseWidth  = 965.
   CountTaskTypeBrowse:BrowseHeight = 455.
   CountTaskTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountTaskTypeBrowse:insertColumn(fTL("Type"),        60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskType}
   
   CountTaskTypeBrowse:insertColumn(fTL("List Seq"),    60, "INTEGER", FALSE).
   CountTaskTypeBrowse:insertColumn(fTL("Type Code"),  120, "CHARACTER", "left", FALSE).
   CountTaskTypeBrowse:insertColumn(fTL("Type Name"),  150, "CHARACTER", "left", FALSE).
   CountTaskTypeBrowse:insertColumn(fTL("Type Descr"), 220, "CHARACTER", "left", FALSE).
   CountTaskTypeBrowse:insertColumn(fTL("Active"),      50, "LOGICAL", FALSE).
   
   /*Body*/
   CountTaskTypeBrowse:startBody().
   
   /* Find all Count Task Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH CountTaskType NO-LOCK
      BY    CountTaskType.Active DESCENDING
      BY    CountTaskType.ListingSequence 
      BY    CountTaskType.CountTaskTypeID:
      
      CountTaskTypeBrowse:startRow(CountTaskType.CountTaskTypeID, "selectCountTaskTypeRow(this," + '"' 
                                                                     + STRING(CountTaskType.CountTaskTypeID) + '"' + ");", "").
      CountTaskTypeBrowse:insertData(CountTaskType.CountTaskTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountTaskType}
      
      CountTaskTypeBrowse:insertData(STRING(CountTaskType.ListingSequence)).
      CountTaskTypeBrowse:insertData(CountTaskType.TypeCode, "left").
      CountTaskTypeBrowse:insertData(CountTaskType.TypeName, "left").
      CountTaskTypeBrowse:insertData(CountTaskType.TypeDescr, "left").
      CountTaskTypeBrowse:insertData(STRING(CountTaskType.Active, "Yes/No")).
      
      /* Add hidden fields */
      CountTaskTypeBrowse:insertHiddenData("CountTaskTypeVersionID",CountTaskType.VersionID).
      
      CountTaskTypeBrowse:endRow().
      
   END. /* FOR EACH CountTaskType NO-LOCK */
   
   CountTaskTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskTypeBrowse:getErrors().
   
   /* Create a new frame */
   CountTaskTypeBrowseFrame           = NEW pageFrame().
   CountTaskTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CountTaskTypeBrowseFrame:FormAction="dbCountTaskTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CountTaskTypeBrowseFrame:formOpen("counttasktype_browse_form").
   
   /* Start the Frame Header */
   CountTaskTypeBrowseFrame:insertSpacer(5).
   CountTaskTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CountTaskTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CountTaskTypeBrowseFrame:frameClose().
   CountTaskTypeBrowseFrame:insertSpacer(10).
   
   CountTaskTypeBrowseFrame:insertHiddenField("counttasktype_browse_scroll","").
   CountTaskTypeBrowseFrame:insertHiddenField("CountTaskTypeID","").
   CountTaskTypeBrowseFrame:insertHiddenField("CountTaskTypeVersionID","").
   CountTaskTypeBrowseFrame:insertHiddenField("form_name","counttasktype_browse_form").
   CountTaskTypeBrowseFrame:insertHiddenField("prog_name","adCountTaskTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskTypeBrowseFrame}
   
   CountTaskTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   CountTaskTypeBrowseButtons           = NEW buttonBar().
   CountTaskTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   CountTaskTypeBrowseButtons:addButton("counttasktype_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewCountTaskTypeDetails('counttasktype_details_form');",
                                         "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO:
      CountTaskTypeBrowseButtons:addButton("counttasktype_browse_form_btn_create",
                                           fTL("Create"),
                                           "createCountTaskType('counttasktype_details_form');",
                                           "").
                                           
      CountTaskTypeBrowseButtons:addButton("counttasktype_browse_form_btn_applinks",
                                           fTL("App Links"),
                                           "viewAppLinkBrowse('counttasktype_browse_form','CountTaskType');",
                                           "Disabled").
   END.                                        
                                           
   
   CountTaskTypeBrowseButtons:closeBar().  
   CountTaskTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskTypeDetails Procedure 
PROCEDURE pCountTaskTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "counttasktype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountTaskTypeID,ListingSequence,TypeCode,TypeName,TypeDescr,Active"
          chrEditFieldList     = "ListingSequence,TypeName,TypeDescr,Active"
          chrNewFieldList      = "ListingSequence,TypeCode,TypeName,TypeDescr,Active"
          chrRequiredFieldList = "ListingSequence,TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   CountTaskTypeDetailsForm           = NEW dataForm("counttasktype_details_form").
   CountTaskTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskTypeDetailsForm:FormAction = "dbCountTaskTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskTypeDetailsForm:FormWidth  = 460.
   CountTaskTypeDetailsForm:FormHeight = 300.
   CountTaskTypeDetailsForm:FormTitle  = "CountTaskType Details".
   CountTaskTypeDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountTaskTypeDetailsForm:insertPaddingColumn(50).
   CountTaskTypeDetailsForm:insertColumn(100).
   CountTaskTypeDetailsForm:insertColumn(120).
   CountTaskTypeDetailsForm:insertColumn(20).
   CountTaskTypeDetailsForm:insertColumn(4).
   CountTaskTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   CountTaskTypeDetailsForm:startRow().
   CountTaskTypeDetailsForm:insertLabel("Type ID").
   CountTaskTypeDetailsForm:insertTextField("CountTaskTypeID", "", 110, TRUE).  
   
   CountTaskTypeDetailsForm:startRow().
   CountTaskTypeDetailsForm:insertLabel("Listing Seq").
   CountTaskTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   CountTaskTypeDetailsForm:startRow().
   CountTaskTypeDetailsForm:insertLabel("Type Code").
   CountTaskTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   CountTaskTypeDetailsForm:startRow().
   CountTaskTypeDetailsForm:insertLabel("Type Name").
   CountTaskTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).
   
   CountTaskTypeDetailsForm:startRow().
   CountTaskTypeDetailsForm:insertLabel("Type Descr").
   CountTaskTypeDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).

   CountTaskTypeDetailsForm:startRow().
   CountTaskTypeDetailsForm:insertLabel(fTL("Active")). 
   CountTaskTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CountTaskTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountTaskTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCountTaskTypeDetailsFields}
   
   /* Add Hidden Fields*/
   CountTaskTypeDetailsForm:insertHiddenField("counttasktype_browse_scroll", "").
   CountTaskTypeDetailsForm:insertHiddenField("form_name", "counttasktype_details_form").
   CountTaskTypeDetailsForm:insertHiddenField("prog_name", "adCountTaskTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskTypeDetailsForm}
   
   /* Create Button Bar */
   CountTaskTypeDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      CountTaskTypeDetailsButtons:addButton("counttasktype_details_form_btn_save", 
                                            fTL("Save"), 
                                            "updateCountTaskType('counttasktype_details_form');").
   
   CountTaskTypeDetailsButtons:addButton("counttasktype_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode'); disablePopup('counttasktype_details_form_popup');").
   
   CountTaskTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskTypeDetailsForm:FormButtons = CountTaskTypeDetailsButtons.
   
   CountTaskTypeDetailsForm:endForm(). 
   
   CountTaskTypeDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskTypeDetailsFields Procedure 
PROCEDURE pCountTaskTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         CountTaskTypeDetailsForm:startRow().
         CountTaskTypeDetailsForm:insertLabel(fTL("Field Label")).
         CountTaskTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adCountTaskTypeAdmin_counttasktype_details_form.i}
      
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
   
   ASSIGN chrCountTaskTypeID          = get-value("CountTaskTypeID")
          intSelectedCountTaskType    = INTEGER(chrCountTaskTypeID)
          chrScrollToCountTaskTypeRow = STRING(INTEGER(get-value("counttasktype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrCountTaskTypeID <> "" THEN
      chrSelectCountTaskTypeRow = 'selectCountTaskTypeRow(document.getElementById("counttasktype_browse_row_' 
                                     + chrCountTaskTypeID + '"),"' + chrCountTaskTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad
                    + 'document.getElementById("counttasktype_browse").scrollTop=' + chrScrollToCountTaskTypeRow 
                    + chrSelectCountTaskTypeRow.
                    
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                       
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "CountTaskType Admin".
   ThisPage:FrameTitle    = "CountTaskType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("counttasktype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pCountTaskTypeBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pCountTaskTypeDetails.
   
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
   DELETE OBJECT CountTaskTypeBrowseFrame         NO-ERROR.
   DELETE OBJECT CountTaskTypeBrowse              NO-ERROR.
   DELETE OBJECT CountTaskTypeBrowseButtons       NO-ERROR.
   DELETE OBJECT CountTaskTypeDetailsForm         NO-ERROR.
   DELETE OBJECT CountTaskTypeDetailsButtons      NO-ERROR.

   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

