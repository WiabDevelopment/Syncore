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

DEFINE VARIABLE intSelectedFileType    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectFileTypeRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToFileTypeRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileTypeID          AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE FileTypeBrowseFrame    AS pageFrame.
DEFINE VARIABLE FileTypeBrowse         AS browseTable.
DEFINE VARIABLE FileTypeBrowseButtons  AS buttonBar.
DEFINE VARIABLE FileTypeDetailsForm    AS dataForm.
DEFINE VARIABLE FileTypeDetailsButtons AS buttonBar.

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
   
   ASSIGN chrFileTypeID          = get-value("FileTypeID")
          intSelectedFileType    = INTEGER(chrFileTypeID)
          chrScrollToFileTypeRow = STRING(INTEGER(get-value("filetype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrFileTypeID <> "" THEN
      chrSelectFileTypeRow = 'selectFileTypeRow(document.getElementById("filetype_browse_row_' + chrFileTypeID + '"),"' 
                                + chrFileTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("filetype_browse").scrollTop=' + chrScrollToFileTypeRow 
                             + chrSelectFileTypeRow.
                             
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                                
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "FileType Admin".
   ThisPage:FrameTitle    = "FileType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("filetype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pFileTypeBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pFileTypeDetails.

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
   DELETE OBJECT FileTypeBrowseFrame         NO-ERROR.
   DELETE OBJECT FileTypeBrowse              NO-ERROR.
   DELETE OBJECT FileTypeBrowseButtons       NO-ERROR.
   DELETE OBJECT FileTypeDetailsForm         NO-ERROR.
   DELETE OBJECT FileTypeDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileTypeBrowse Procedure 
PROCEDURE pFileTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "filetype_details_form"}
   
   FileTypeBrowse              = NEW browseTable("filetype_browse").
   FileTypeBrowse:BrowseWidth  = 965.
   FileTypeBrowse:BrowseHeight = 455.
   FileTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   FileTypeBrowse:insertColumn(fTL("FileType"),        60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileType}
   
   FileTypeBrowse:insertColumn(fTL("Type Code"),  130, "CHARACTER", "left", FALSE).
   FileTypeBrowse:insertColumn(fTL("Type Name"),  150, "CHARACTER", "left", FALSE).
   FileTypeBrowse:insertColumn(fTL("Type Descr"), 180, "CHARACTER", "left", FALSE).
   FileTypeBrowse:insertColumn(fTL("Extension"),   80, "CHARACTER", "left", FALSE).
   FileTypeBrowse:insertColumn(fTL("Active"),      80, "LOGICAL", FALSE).
   
   /*Body*/
   FileTypeBrowse:startBody().
   
   FOR EACH FileType NO-LOCK
      BY    FileType.Active DESC 
      BY    FileType.FileTypeID:
      
      FileTypeBrowse:startRow(FileType.FileTypeID, "selectFileTypeRow(this," + '"' 
                                                      + STRING(FileType.FileTypeID) + '"' + ");", "").
      FileTypeBrowse:insertData(FileType.FileTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FileType}
      
      FileTypeBrowse:insertData(FileType.TypeCode, "left").
      FileTypeBrowse:insertData(FileType.TypeName, "left").
      FileTypeBrowse:insertData(FileType.TypeDescr, "left").
      FileTypeBrowse:insertData(FileType.Extension, "left").
      FileTypeBrowse:insertData(STRING(FileType.Active, "Yes/No")).
      
      /* Add hidden fields */
      FileTypeBrowse:insertHiddenData("FileTypeVersionID",FileType.VersionID).
      
      FileTypeBrowse:endRow().
      
   END. /* FOR EACH FileType NO-LOCK */
   
   FileTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FileTypeBrowse:getErrors().
   
   /* Create a new frame */
   FileTypeBrowseFrame           = NEW pageFrame().
   FileTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FileTypeBrowseFrame:FormAction="dbFileTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FileTypeBrowseFrame:formOpen("filetype_browse_form").
   
   /* Start the Frame Header */
   FileTypeBrowseFrame:insertSpacer(5).
   FileTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FileTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   FileTypeBrowseFrame:frameClose().
   FileTypeBrowseFrame:insertSpacer(10).
   
   FileTypeBrowseFrame:insertHiddenField("filetype_browse_scroll","").
   FileTypeBrowseFrame:insertHiddenField("FileTypeID","").
   FileTypeBrowseFrame:insertHiddenField("FileTypeVersionID","").
   FileTypeBrowseFrame:insertHiddenField("form_name","filetype_browse_form").
   FileTypeBrowseFrame:insertHiddenField("prog_name","adFileTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileTypeBrowseFrame}
   
   FileTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   FileTypeBrowseButtons           = NEW buttonBar().
   FileTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   FileTypeBrowseButtons:addButton("filetype_browse_form_btn_details",
                                   fTL("Details"),
                                   "viewFileTypeDetails('filetype_details_form');",
                                   "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO:   
      FileTypeBrowseButtons:addButton("filetype_browse_form_btn_create",
                                      fTL("Create"),
                                      "createFileType('filetype_details_form');",
                                      "").
                                      
      FileTypeBrowseButtons:addButton("filetype_browse_form_btn_applinks",
                                      fTL("App Links"),
                                      "viewAppLinkBrowse('filetype_browse_form','FileType');",
                                      "Disabled").
                                      
   END.                                
   
   FileTypeBrowseButtons:closeBar().  
   FileTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileTypeDetails Procedure 
PROCEDURE pFileTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "filetype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "FileTypeID,TypeCode,TypeName,TypeDescr,Active,Extension"
          chrEditFieldList     = "TypeName,TypeDescr,Active,Extension"
          chrNewFieldList      = "TypeCode,TypeName,TypeDescr,Active,Extension"
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr,Extension"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   FileTypeDetailsForm           = NEW dataForm("filetype_details_form").
   FileTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileTypeDetailsForm:FormAction = "dbFileTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileTypeDetailsForm:FormWidth  = 460.
   FileTypeDetailsForm:FormHeight = 300.
   FileTypeDetailsForm:FormTitle  = "FileType Details".
   FileTypeDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   FileTypeDetailsForm:insertPaddingColumn(50).
   FileTypeDetailsForm:insertColumn(100).
   FileTypeDetailsForm:insertColumn(120).
   FileTypeDetailsForm:insertColumn(20).
   FileTypeDetailsForm:insertColumn(4).

   
   /* Fields */
   FileTypeDetailsForm:startRow().
   FileTypeDetailsForm:insertLabel("Type ID").
   FileTypeDetailsForm:insertTextField("FileTypeID", "", 110, TRUE).  
      
   FileTypeDetailsForm:startRow().
   FileTypeDetailsForm:insertLabel(fTL("Type Code")).
   FileTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   FileTypeDetailsForm:startRow().
   FileTypeDetailsForm:insertLabel(fTL("Type Name")).
   FileTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).
   
   FileTypeDetailsForm:startRow().
   FileTypeDetailsForm:insertLabel(fTL("Type Descr")).
   FileTypeDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).
   
   FileTypeDetailsForm:startRow().
   FileTypeDetailsForm:insertLabel(fTL("Extension")).
   FileTypeDetailsForm:insertTextField("Extension", "", 220, TRUE).
   
   FileTypeDetailsForm:startRow().
   FileTypeDetailsForm:insertLabel(fTL("Active")). 
   FileTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   FileTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   FileTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pFileTypeDetailsFields}
   
   /* Add Hidden Fields*/
   FileTypeDetailsForm:insertHiddenField("filetype_browse_scroll", "").
   FileTypeDetailsForm:insertHiddenField("form_name", "filetype_details_form").
   FileTypeDetailsForm:insertHiddenField("prog_name", "adFileTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileTypeDetailsForm}
   
   /* Create Button Bar */
   FileTypeDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      FileTypeDetailsButtons:addButton("filetype_details_form_btn_save", 
                                       fTL("Save"), 
                                       "updateFileType('filetype_details_form');").
   
   FileTypeDetailsButtons:addButton("filetype_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('filetype_details_form_popup');").
   
   FileTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileTypeDetailsForm:FormButtons = FileTypeDetailsButtons.
   
   FileTypeDetailsForm:endForm(). 
   
   FileTypeDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileTypeDetailsFields Procedure 
PROCEDURE pFileTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         FileTypeDetailsForm:startRow().
         FileTypeDetailsForm:insertLabel(fTL("Field Label")).
         FileTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adFileTypeAdmin_filetype_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

