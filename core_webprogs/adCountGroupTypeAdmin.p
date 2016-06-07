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

DEFINE VARIABLE intSelectedCountGroupType    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountGroupTypeRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountGroupTypeRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountGroupTypeID          AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE CountGroupTypeBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountGroupTypeBrowse         AS browseTable.
DEFINE VARIABLE CountGroupTypeBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountGroupTypeDetailsForm    AS dataForm.
DEFINE VARIABLE CountGroupTypeDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pCountGroupTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupTypeBrowse Procedure 
PROCEDURE pCountGroupTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "countgrouptype_details_form"}
   
   CountGroupTypeBrowse              = NEW browseTable("countgrouptype_browse").
   CountGroupTypeBrowse:BrowseWidth  = 965.
   CountGroupTypeBrowse:BrowseHeight = 455.
   CountGroupTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountGroupTypeBrowse:insertColumn(fTL("Type"),        60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountGroupType}
   
   CountGroupTypeBrowse:insertColumn(fTL("List Seq"),    60, "INTEGER", FALSE).
   CountGroupTypeBrowse:insertColumn(fTL("Type Code"),  120, "CHARACTER", "left", FALSE).
   CountGroupTypeBrowse:insertColumn(fTL("Type Name"),  150, "CHARACTER", "left", FALSE).
   CountGroupTypeBrowse:insertColumn(fTL("Type Descr"), 220, "CHARACTER", "left", FALSE).
   CountGroupTypeBrowse:insertColumn(fTL("Active"),      50, "LOGICAL", FALSE).
   
   /*Body*/
   CountGroupTypeBrowse:startBody().
   
   /* Find all Count Group Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH CountGroupType NO-LOCK
      BY    CountGroupType.Active Desc
      BY    CountGroupType.ListingSequence 
      BY    CountGroupType.CountGroupTypeID:
      
      CountGroupTypeBrowse:startRow(CountGroupType.CountGroupTypeID, "selectCountGroupTypeRow(this," + '"' 
                                                                     + STRING(CountGroupType.CountGroupTypeID) + '"' + ");", "").
      CountGroupTypeBrowse:insertData(CountGroupType.CountGroupTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountGroupType}
      
      CountGroupTypeBrowse:insertData(STRING(CountGroupType.ListingSequence)).
      CountGroupTypeBrowse:insertData(CountGroupType.TypeCode, "left").
      CountGroupTypeBrowse:insertData(CountGroupType.TypeName, "left").
      CountGroupTypeBrowse:insertData(CountGroupType.TypeDescr, "left").
      CountGroupTypeBrowse:insertData(STRING(CountGroupType.Active, "Yes/No")).
      
      /* Add hidden fields */
      CountGroupTypeBrowse:insertHiddenData("CountGroupTypeVersionID",CountGroupType.VersionID).
      
      CountGroupTypeBrowse:endRow().
      
   END. /* FOR EACH CountGroupType NO-LOCK */
   
   CountGroupTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountGroupTypeBrowse:getErrors().
   
   /* Create a new frame */
   CountGroupTypeBrowseFrame           = NEW pageFrame().
   CountGroupTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CountGroupTypeBrowseFrame:FormAction="dbCountGroupTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CountGroupTypeBrowseFrame:formOpen("countgrouptype_browse_form").
   
   /* Start the Frame Header */
   CountGroupTypeBrowseFrame:insertSpacer(5).
   CountGroupTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CountGroupTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CountGroupTypeBrowseFrame:frameClose().
   CountGroupTypeBrowseFrame:insertSpacer(10).
   
   CountGroupTypeBrowseFrame:insertHiddenField("countgrouptype_browse_scroll","").
   CountGroupTypeBrowseFrame:insertHiddenField("CountGroupTypeID","").
   CountGroupTypeBrowseFrame:insertHiddenField("CountGroupTypeVersionID","").
   CountGroupTypeBrowseFrame:insertHiddenField("form_name","countgrouptype_browse_form").
   CountGroupTypeBrowseFrame:insertHiddenField("prog_name","adCountGroupTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupTypeBrowseFrame}
   
   CountGroupTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   CountGroupTypeBrowseButtons           = NEW buttonBar().
   CountGroupTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   CountGroupTypeBrowseButtons:addButton("countgrouptype_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewCountGroupTypeDetails('countgrouptype_details_form');",
                                         (IF intSelectedCountGroupType > 0 THEN "" ELSE "Disabled")).
   

   IF NOT logPreventDataCreates THEN
   DO:   
      CountGroupTypeBrowseButtons:addButton("countgrouptype_browse_form_btn_create",
                                            fTL("Create"),
                                            "createCountGroupType('countgrouptype_details_form');",
                                            "").
                                             
      CountGroupTypeBrowseButtons:addButton("countgrouptype_browse_form_btn_applinks",
                                            fTL("App Links"),
                                            "viewAppLinkBrowse('countgrouptype_browse_form','CountGroupType');",
                                            "Disabled").

   END.                                        
                                                                                     
   CountGroupTypeBrowseButtons:closeBar().  
   CountGroupTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupTypeDetails Procedure 
PROCEDURE pCountGroupTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "countgrouptype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountGroupTypeID,ListingSequence,TypeCode,TypeName,TypeDescr,Active"
          chrEditFieldList     = "ListingSequence,TypeName,TypeDescr,Active"
          chrNewFieldList      = "ListingSequence,TypeCode,TypeName,TypeDescr,Active"
          chrRequiredFieldList = "ListingSequence,TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   CountGroupTypeDetailsForm           = NEW dataForm("countgrouptype_details_form").
   CountGroupTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountGroupTypeDetailsForm:FormAction = "dbCountGroupTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountGroupTypeDetailsForm:FormWidth  = 460.
   CountGroupTypeDetailsForm:FormHeight = 300.
   CountGroupTypeDetailsForm:FormTitle  = "CountGroupType Details".
   CountGroupTypeDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountGroupTypeDetailsForm:insertPaddingColumn(50).
   CountGroupTypeDetailsForm:insertColumn(100).
   CountGroupTypeDetailsForm:insertColumn(120).
   CountGroupTypeDetailsForm:insertColumn(20).
   CountGroupTypeDetailsForm:insertColumn(4).
   CountGroupTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   CountGroupTypeDetailsForm:startRow().
   CountGroupTypeDetailsForm:insertLabel("Type ID").
   CountGroupTypeDetailsForm:insertTextField("CountGroupTypeID", "", 110, TRUE).  
   
   CountGroupTypeDetailsForm:startRow().
   CountGroupTypeDetailsForm:insertLabel("Listing Seq").
   CountGroupTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   CountGroupTypeDetailsForm:startRow().
   CountGroupTypeDetailsForm:insertLabel("Type Code").
   CountGroupTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   CountGroupTypeDetailsForm:startRow().
   CountGroupTypeDetailsForm:insertLabel("Type Name").
   CountGroupTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).
   
   CountGroupTypeDetailsForm:startRow().
   CountGroupTypeDetailsForm:insertLabel("Type Descr").
   CountGroupTypeDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).

   CountGroupTypeDetailsForm:startRow().
   CountGroupTypeDetailsForm:insertLabel(fTL("Active")). 
   CountGroupTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CountGroupTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountGroupTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCountGroupTypeDetailsFields}
   
   /* Add Hidden Fields*/
   CountGroupTypeDetailsForm:insertHiddenField("countgrouptype_browse_scroll", "").
   CountGroupTypeDetailsForm:insertHiddenField("form_name", "countgrouptype_details_form").
   CountGroupTypeDetailsForm:insertHiddenField("prog_name", "adCountGroupTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupTypeDetailsForm}
   
   /* Create Button Bar */
   CountGroupTypeDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      CountGroupTypeDetailsButtons:addButton("countgrouptype_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateCountGroupType('countgrouptype_details_form');").
   
   CountGroupTypeDetailsButtons:addButton("countgrouptype_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode'); disablePopup('countgrouptype_details_form_popup');").
   
   CountGroupTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountGroupTypeDetailsForm:FormButtons = CountGroupTypeDetailsButtons.
   
   CountGroupTypeDetailsForm:endForm(). 
   
   CountGroupTypeDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupTypeDetailsFields Procedure 
PROCEDURE pCountGroupTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         CountGroupTypeDetailsForm:startRow().
         CountGroupTypeDetailsForm:insertLabel(fTL("Field Label")).
         CountGroupTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adCountGroupTypeAdmin_countgrouptype_details_form.i}
      
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
   
   ASSIGN chrCountGroupTypeID          = get-value("CountGroupTypeID")
          intSelectedCountGroupType    = INTEGER(chrCountGroupTypeID)
          chrScrollToCountGroupTypeRow = STRING(INTEGER(get-value("countgrouptype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrCountGroupTypeID <> "" THEN
      chrSelectCountGroupTypeRow = 'selectCountGroupTypeRow(document.getElementById("countgrouptype_browse_row_' + chrCountGroupTypeID + '"),"' 
                                                         + chrCountGroupTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                    + 'document.getElementById("countgrouptype_browse").scrollTop=' + chrScrollToCountGroupTypeRow 
                    + chrSelectCountGroupTypeRow.

   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                               
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "CountGroupType Admin".
   ThisPage:FrameTitle    = "CountGroupType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("countgrouptype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pCountGroupTypeBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pCountGroupTypeDetails.
   
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
   DELETE OBJECT CountGroupTypeBrowseFrame         NO-ERROR.
   DELETE OBJECT CountGroupTypeBrowse              NO-ERROR.
   DELETE OBJECT CountGroupTypeBrowseButtons       NO-ERROR.
   DELETE OBJECT CountGroupTypeDetailsForm         NO-ERROR.
   DELETE OBJECT CountGroupTypeDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
   
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
