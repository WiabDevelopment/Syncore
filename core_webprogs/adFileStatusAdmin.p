&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adFileStatusAdmin.p 

  Description: ad file for the File Status Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Anthony Ferrari 

  Created: 06/03/2015

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
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedFileStatus           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedFileStatusHist       AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrFileStatusHistID             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFileStatusRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFileStatusRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFileStatusHistRow    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFileStatusID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                 AS CHARACTER   NO-UNDO.

/* Buffers */
/* Objects */
DEFINE VARIABLE FileStatusBrowseFrame           AS pageFrame.
DEFINE VARIABLE FileStatusBrowse                AS browseTable.
DEFINE VARIABLE FileStatusBrowseButtons         AS buttonBar.
DEFINE VARIABLE FileStatusDetailsForm           AS dataForm.
DEFINE VARIABLE FileStatusDetailsButtons        AS buttonBar.
DEFINE VARIABLE FileStatusHistBrowseForm        AS dataForm.  
DEFINE VARIABLE FileStatusHistBrowse            AS browseTable.
DEFINE VARIABLE FileStatusHistDetailsForm       AS dataForm.
DEFINE VARIABLE FileStatusHistDetailsButtons    AS buttonBar.
DEFINE VARIABLE FileStatusHistButtons           AS buttonBar.


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
   
   ASSIGN chrFileStatusID = get-value("FileStatusID")
          intSelectedFileStatus = INTEGER(chrFileStatusID)
          chrScrollToFileStatusRow = STRING(INTEGER(get-value("filestatus_browse_scroll"))) + ";"
          /*History details button*/
          chrFileStatusHistID = get-value("FileStatusHistID")
          intSelectedFileStatusHist = INTEGER(chrFileStatusHistID)
          chrScrollToFileStatusHistRow = STRING(INTEGER(get-value("filestatushist_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrFileStatusID <> "" THEN
      chrFileStatusRow = 'selectFileStatusRow(document.getElementById("filestatus_browse_row_'
                               + chrFileStatusID + '"),"' + chrFileStatusID +  '");'.
                                                          
   IF get-value('popup_filestatushist_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("filestatushist_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("filestatus_browse").scrollTop=' + chrScrollToFileStatusRow 
                                                           + chrFileStatusRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "File Status Admin".
   ThisPage:FrameTitle = "File Status Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("filestatusadmin.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pFileStatusBrowse .
   
   /******* Popup Browsers and Forms ********/    
   RUN pFileStatusDetails.
   
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT FileStatusBrowseFrame             NO-ERROR.
   DELETE OBJECT FileStatusBrowse                  NO-ERROR.
   DELETE OBJECT FileStatusBrowseButtons           NO-ERROR.
   DELETE OBJECT FileStatusDetailsForm        NO-ERROR.
   DELETE OBJECT FileStatusDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pFileStatusBrowse  :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "filestatus_details_form"}
   
   FileStatusBrowse  = NEW browseTable("filestatus_browse").
   FileStatusBrowse :BrowseWidth  = 965.
   FileStatusBrowse :BrowseHeight = 455.
   FileStatusBrowse :WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   FileStatusBrowse :insertColumn(fTL("File Status ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileStatus}
   
   FileStatusBrowse:insertColumn(fTL("Status Name"),        100, "CHARACTER", "LEFT", FALSE).   
   FileStatusBrowse:insertColumn(fTL("Status Description"), 250, "CHARACTER", "LEFT", FALSE).
   FileStatusBrowse:insertColumn(fTL("Listing Sequence"),   125, "CHARACTER", "LEFT", FALSE).    
   FileStatusBrowse:insertColumn(fTL("Active"),             100, "LOGICAL", "LEFT", FALSE).
   
   
   /*Body*/
   FileStatusBrowse :startBody().
   
   FOR EACH FileStatus NO-LOCK: /*idx=FileStatusID*/
            
      FileStatusBrowse:startRow(FileStatus.FileStatusID, "selectFileStatusRow(this," + '"' 
                                       + STRING(FileStatus.FileStatusID) + '"' + ");", "").
                                     
      FileStatusBrowse:insertData(FileStatus.FileStatusID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FileStatus}      
 
      FileStatusBrowse:insertData(FileStatus.StatusName,              "LEFT"). 
      FileStatusBrowse:insertData(STRING(FileStatus.StatusDescr),     "LEFT").
      FileStatusBrowse:insertData(STRING(FileStatus.ListingSequence), "LEFT").
      FileStatusBrowse:insertData(STRING(FileStatus.Active),          "LEFT").
      
            
      /* Add hidden fields */
      FileStatusBrowse:insertHiddenData("FileStatusVersionID",FileStatus.VersionID).
      
      FileStatusBrowse:endRow().
      
   END. /*FOR EACH FileStatus NO-LOCK */
   
   FileStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FileStatusBrowse :getErrors().
   
   /* Create a new frame */
   FileStatusBrowseFrame = NEW pageFrame().
   FileStatusBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FileStatusBrowseFrame:FormAction="dbFileStatusAdminUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FileStatusBrowseFrame:formOpen("filestatus_browse_form").
   
   /* Start the Frame Header */
   FileStatusBrowseFrame:insertSpacer(5).
   FileStatusBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FileStatusBrowse :displayBrowse().  
   
   /* End the Frame Header */
   FileStatusBrowseFrame:frameClose().
   FileStatusBrowseFrame:insertSpacer(10).
   
   FileStatusBrowseFrame:insertHiddenField("filestatus_browse_scroll","").
   FileStatusBrowseFrame:insertHiddenField("FileStatusID","").
   FileStatusBrowseFrame:insertHiddenField("FileStatusVersionID","").
   FileStatusBrowseFrame:insertHiddenField("form_name","filestatus_browse_form").
   FileStatusBrowseFrame:insertHiddenField("popup_filestatushist_browse","").
   FileStatusBrowseFrame:insertHiddenField("prog_name","adFileStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileStatusBrowseFrame}
   
   FileStatusBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   FileStatusBrowseButtons = NEW buttonBar().
   FileStatusBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   FileStatusBrowseButtons:addButton("filestatus_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewFileStatusDetails('filestatus_details_form');",
                                         (IF intSelectedFileStatus > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
   FileStatusBrowseButtons:addButton("filestatus_browse_form_btn_create",
                                         fTL("Create"),
                                         "createFileStatus('filestatus_details_form');",
                                         "").
   END.
   
   FileStatusBrowseButtons:closeBar().  
   FileStatusBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pFileStatusDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "filestatus_details_form"}
   ASSIGN chrDisplayFieldList  = "FileStatusID,StatusName,StatusDescr,ListingSequence,Active"
          chrEditFieldList     = "StatusName,StatusDescr,ListingSequence,Active"
          chrNewFieldList      = "FileStatusID,StatusName,StatusDescr,ListingSequence,Active"
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   
   FileStatusDetailsForm = NEW dataForm("filestatus_details_form").
   FileStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileStatusDetailsForm:FormAction = "dbFileStatusAdminUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileStatusDetailsForm:FormWidth   = 460.
   FileStatusDetailsForm:FormHeight  = 200.
   FileStatusDetailsForm:FormTitle   = "File Status Admin Details".
   FileStatusDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   FileStatusDetailsForm:insertPaddingColumn(30).
   FileStatusDetailsForm:insertColumn(185).
   
   /* Fields */
   FileStatusDetailsForm:startRow().
   FileStatusDetailsForm:insertLabel(fTL("File Status Admin ID")).
   FileStatusDetailsForm:insertTextField("FileStatusID", "", 190, TRUE).  
   
   FileStatusDetailsForm:startRow().
   FileStatusDetailsForm:insertLabel(fTL("Status Name")).
   FileStatusDetailsForm:insertTextField("StatusName", "", 190, TRUE).  
   
   FileStatusDetailsForm:startRow().
   FileStatusDetailsForm:insertLabel(fTL("Listing Sequence")).
   FileStatusDetailsForm:insertTextField("ListingSequence", "", 190, TRUE).
   
   FileStatusDetailsForm:startRow().
   FileStatusDetailsForm:insertLabel(fTL("Status Description")).
   FileStatusDetailsForm:insertTextAreaField("StatusDescr", "", 190, TRUE).
   
   FileStatusDetailsForm:startRow().
   FileStatusDetailsForm:insertLabel(fTL("Active")).
   FileStatusDetailsForm:insertComboField("Active", "", 190, TRUE).
   FileStatusDetailsForm:insertComboPairs("Active", "yes", "Yes").    
   FileStatusDetailsForm:insertComboPairs("Active", "no", "No"). 


   {webGetOptionalFormFields.i pFileStatusDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FileStatusDetailsForm:insertHiddenField("filestatus_browse_scroll", "").
   FileStatusDetailsForm:insertHiddenField("form_name", "filestatus_details_form").
   FileStatusDetailsForm:insertHiddenField("prog_name", "adFileStatusAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileStatusDetailsForm}
   
   /* Create Button Bar */
   FileStatusDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataUpdates THEN
   DO:
   FileStatusDetailsButtons:addButton("filestatus_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateFileStatus('filestatus_details_form');").
   END.
   FileStatusDetailsButtons:addButton("filestatus_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode');" + 
                                    "disablePopup('filestatus_details_form_popup');").
   FileStatusDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileStatusDetailsForm:FormButtons = FileStatusDetailsButtons.
   
   FileStatusDetailsForm:endForm(). 
   
   FileStatusDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pFileStatusDetailsFields:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrStatus AS CHARACTER NO-UNDO.
  
  CASE chrStatus:
    
    WHEN "FieldName" THEN
    DO:
      FileStatusDetailsForm:startRow().
      FileStatusDetailsForm:insertLabel(fTL("Field Label")).
      FileStatusDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
