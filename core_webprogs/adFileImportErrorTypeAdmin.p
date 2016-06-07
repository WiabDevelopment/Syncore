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

  Author: Michael Landess

  Created: 11/02/2012

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
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}



/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedFileImportErrorType     AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectFileImportErrorTypeRow    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToFileImportErrorTypeRow  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileImportErrorTypeID           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFileImportErrorTypeHistory AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE FileImportErrorTypeBrowseFrame     AS pageFrame.
DEFINE VARIABLE FileImportErrorTypeBrowse          AS browseTable.
DEFINE VARIABLE FileImportErrorTypeBrowseButtons   AS buttonBar.

DEFINE VARIABLE FileImportErrorTypeDetailsForm     AS dataForm.
DEFINE VARIABLE FileImportErrorTypeDetailsButtons  AS buttonBar.


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
         HEIGHT             = 14.1
         WIDTH              = 60.4.
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
   
   ASSIGN chrFileImportErrorTypeID          = get-value("FileImportErrorTypeID")
          intSelectedFileImportErrorType    = INTEGER(chrFileImportErrorTypeID)
          chrScrollToFileImportErrorTypeRow = STRING(INTEGER(get-value("fileimporterrortype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrFileImportErrorTypeID <> "" THEN
     chrSelectFileImportErrorTypeRow = 'selectFileImportErrorTypeRow(document.getElementById("fileimporterrortype_browse_row_' 
                                                                   + chrFileImportErrorTypeID + '"),"' + chrFileImportErrorTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("fileimporterrortype_browse").scrollTop=' + chrScrollToFileImportErrorTypeRow 
                                                          + chrSelectFileImportErrorTypeRow + chrPopupFileImportErrorTypeHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "File Import Error Type Admin".
   ThisPage:FrameTitle    = "File Import Error Type Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("fileimporterrortype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pFileImportErrorTypeBrowse.
   
   FIND FIRST FileImportErrorType NO-LOCK
      WHERE FileImportErrorType.FileImportErrorTypeID = intSelectedFileImportErrorType NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pFileImportErrorTypeDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT FileImportErrorTypeBrowseFrame           NO-ERROR.
   DELETE OBJECT FileImportErrorTypeBrowse                NO-ERROR.
   DELETE OBJECT FileImportErrorTypeBrowseButtons         NO-ERROR.
   DELETE OBJECT FileImportErrorTypeDetailsForm           NO-ERROR.
   DELETE OBJECT FileImportErrorTypeDetailsButtons        NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileImportErrorTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileImportErrorTypeBrowse Procedure 
PROCEDURE pFileImportErrorTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "fileimporterrortype_details_form"}
   
   FileImportErrorTypeBrowse              = NEW browseTable("fileimporterrortype_browse").
   FileImportErrorTypeBrowse:BrowseWidth  = 965.
   FileImportErrorTypeBrowse:BrowseHeight = 455.
   FileImportErrorTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   FileImportErrorTypeBrowse:insertColumn(fTL("TypeID"),            80, "INTEGER",   "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileImportErrorType}
   
   FileImportErrorTypeBrowse:insertColumn(fTL("File Master"),      120, "CHARACTER", "left", FALSE).
   FileImportErrorTypeBrowse:insertColumn(fTL("File Status"),      120, "CHARACTER", "left", FALSE).
   FileImportErrorTypeBrowse:insertColumn(fTL("Type Name"),        120, "CHARACTER", "left", FALSE).
   FileImportErrorTypeBrowse:insertColumn(fTL("Reject FIle"),       80, "LOGICAL",           FALSE).
   FileImportErrorTypeBrowse:insertColumn(fTL("Prevent Creation"),  80, "LOGICAL",           FALSE).
   
   /*Body*/
   FileImportErrorTypeBrowse:startBody().
   
   FOR EACH FileImportErrorType NO-LOCK /*idx=FileImportErrorTypeID*/
      BY    FileImportErrorType.FileImportErrorTypeID:
         
      FIND FIRST FileMaster OF FileImportErrorType NO-LOCK NO-ERROR.
      FIND FIRST FileStatus OF FileImportErrorType NO-LOCK NO-ERROR.
      
      FileImportErrorTypeBrowse:startRow(FileImportErrorType.FileImportErrorTypeID, "selectFileImportErrorTypeRow(this," + '"' 
                                         + STRING(FileImportErrorType.FileImportErrorTypeID) + '"' + ");", "").
                                         
      FileImportErrorTypeBrowse:insertData(FileImportErrorType.FileImportErrorTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FileImportErrorType}
      
      FileImportErrorTypeBrowse:insertData((IF AVAILABLE FileMaster THEN FileMaster.MasterName ELSE ""), "left").
      FileImportErrorTypeBrowse:insertData((IF AVAILABLE FileStatus THEN FileStatus.StatusName ELSE ""), "left").
      FileImportErrorTypeBrowse:insertData(FileImportErrorType.TypeName, "left").
      FileImportErrorTypeBrowse:insertData(STRING(FileImportErrorType.RejectWholeFile,"Yes/No")).
      FileImportErrorTypeBrowse:insertData(STRING(FileImportErrorType.PreventRecordCreation,"Yes/No")).
      
      /* Add hidden fields */
/*      FileImportErrorTypeBrowse:insertHiddenData("FileImportErrorTypeVersionID", FileImportErrorType.VersionID).*/
      
      FileImportErrorTypeBrowse:endRow().
      
   END. /*FOR EACH FileImportErrorType NO-LOCK */
   
   FileImportErrorTypeBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + FileImportErrorTypeBrowse:getErrors().
   
   /* Create a new frame */
   FileImportErrorTypeBrowseFrame           = NEW pageFrame().
   FileImportErrorTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FileImportErrorTypeBrowseFrame:FormAction = "dbFileImportErrorTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FileImportErrorTypeBrowseFrame:formOpen("fileimporterrortype_browse_form").
   
   /* Start the Frame Header */
   FileImportErrorTypeBrowseFrame:insertSpacer(5).
   FileImportErrorTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FileImportErrorTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   FileImportErrorTypeBrowseFrame:frameClose().
   FileImportErrorTypeBrowseFrame:insertSpacer(10).
   
   FileImportErrorTypeBrowseFrame:insertHiddenField("fileimporterrortype_browse_scroll", "").
   FileImportErrorTypeBrowseFrame:insertHiddenField("FileImportErrorTypeID", "").
   FileImportErrorTypeBrowseFrame:insertHiddenField("form_name", "fileimporterrortype_browse_form").
   FileImportErrorTypeBrowseFrame:insertHiddenField("prog_name", "adFileImportErrorTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileImportErrorTypeBrowseFrame}
   
   FileImportErrorTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   FileImportErrorTypeBrowseButtons           = NEW buttonBar().
   FileImportErrorTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
     
   FileImportErrorTypeBrowseButtons:addButton("fileimporterrortype_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewFileImportErrorTypeDetails('fileimporterrortype_details_form');",
                                             (IF intSelectedFileImportErrorType > 0 THEN "" ELSE "Disabled")).
   
   FileImportErrorTypeBrowseButtons:addButton("fileimporterrortype_browse_form_btn_create",
                                             fTL("Create"),
                                             "createFileImportErrorType('fileimporterrortype_details_form');",
                                             "").
   
   FileImportErrorTypeBrowseButtons:closeBar(). 
    
   FileImportErrorTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileImportErrorTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileImportErrorTypeDetails Procedure 
PROCEDURE pFileImportErrorTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "fileimporterrortype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "FileImportErrorTypeID,FileMasterID,FileStatusID,TypeCode,TypeName,RejectWholeFile,PreventRecordCreation" 
          chrEditFieldList     = "FileMasterID,FileStatusID,TypeCode,TypeName,RejectWholeFile,PreventRecordCreation" 
          chrNewFieldList      = "FileMasterID,FileStatusID,TypeCode,TypeName,RejectWholeFile,PreventRecordCreation" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   FileImportErrorTypeDetailsForm           = NEW dataForm("fileimporterrortype_details_form").
   FileImportErrorTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileImportErrorTypeDetailsForm:FormAction = "dbFileImportErrorTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileImportErrorTypeDetailsForm:FormWidth  = 580.
   FileImportErrorTypeDetailsForm:FormHeight = 420.
   FileImportErrorTypeDetailsForm:FormTitle  = "File Import Error Type Details".
   FileImportErrorTypeDetailsForm:FormType   = "large".
   
   /* Column Layout */
   FileImportErrorTypeDetailsForm:insertPaddingColumn(30).
   FileImportErrorTypeDetailsForm:insertColumn(150).
   FileImportErrorTypeDetailsForm:insertColumn(160).
   FileImportErrorTypeDetailsForm:insertColumn(20).
   FileImportErrorTypeDetailsForm:insertColumn(4).
   FileImportErrorTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   FileImportErrorTypeDetailsForm:startRow().
   FileImportErrorTypeDetailsForm:insertLabel("Type ID").
   FileImportErrorTypeDetailsForm:insertTextField("FileImportErrorTypeID", "", 110, TRUE).  
   
   FileImportErrorTypeDetailsForm:startRow().
   FileImportErrorTypeDetailsForm:insertLabel("File Master").
   FileImportErrorTypeDetailsForm:insertComboField("FileMasterID", "", 110, TRUE).
   /* Insert the File Master */
   FileImportErrorTypeDetailsForm:insertComboPairs("FileMasterID", "0" , "None Selected...").
   FOR EACH FileMaster NO-LOCK /*idx=MasterName*/
      BY    FileMaster.MasterName:
      
      FileImportErrorTypeDetailsForm:insertComboPairs("FileMasterID", 
                                                      STRING(FileMaster.FileMasterID), 
                                                      FileMaster.MasterName).
                                                      
   END.  /*FOR EACH FileMaster NO-LOCK*/  
   
   FileImportErrorTypeDetailsForm:startRow().
   FileImportErrorTypeDetailsForm:insertLabel("File Status").
   FileImportErrorTypeDetailsForm:insertComboField("FileStatusID", "", 110, TRUE).
   /* Insert the File Status */
   FileImportErrorTypeDetailsForm:insertComboPairs("FileStatusID", "0" , "None Selected...").
   FOR EACH FileStatus NO-LOCK /*idx=StatusName*/
      BY    FileStatus.StatusName:
      
      FileImportErrorTypeDetailsForm:insertComboPairs("FileStatusID", 
                                                      STRING(FileStatus.FileStatusID), 
                                                      FileStatus.StatusName).
                                                                                                            
   END.  /*FOR EACH File Status NO-LOCK*/  
   
   FileImportErrorTypeDetailsForm:startRow().
   FileImportErrorTypeDetailsForm:insertLabel("Type Code").
   FileImportErrorTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).
   
   FileImportErrorTypeDetailsForm:startRow().
   FileImportErrorTypeDetailsForm:insertLabel("Type Name").
   FileImportErrorTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).  
   
   FileImportErrorTypeDetailsForm:startRow().
   FileImportErrorTypeDetailsForm:insertLabel(fTL("Reject File")). 
   FileImportErrorTypeDetailsForm:insertComboField("RejectWholeFile", "", 110, TRUE).  
   FileImportErrorTypeDetailsForm:insertComboPairs("RejectWholeFile", "yes", "Yes").
   FileImportErrorTypeDetailsForm:insertComboPairs("RejectWholeFile", "no",  "No").

   FileImportErrorTypeDetailsForm:startRow().
   FileImportErrorTypeDetailsForm:insertLabel(fTL("Prevent Creation")). 
   FileImportErrorTypeDetailsForm:insertComboField("PreventRecordCreation", "", 110, TRUE).  
   FileImportErrorTypeDetailsForm:insertComboPairs("PreventRecordCreation", "yes", "Yes").
   FileImportErrorTypeDetailsForm:insertComboPairs("PreventRecordCreation", "no",  "No").
   
   {webGetOptionalFormFields.i pFileImportErrorTypeDetailsFields}
   
   /* Add Hidden Fields*/
   FileImportErrorTypeDetailsForm:insertHiddenField("fileimporterrortype_browse_scroll", "").
   FileImportErrorTypeDetailsForm:insertHiddenField("form_name", "fileimporterrortype_details_form").
   FileImportErrorTypeDetailsForm:insertHiddenField("prog_name", "adFileImportErrorTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileImportErrorTypeDetailsForm}
   
   /* Create Button Bar */
   FileImportErrorTypeDetailsButtons = NEW buttonBar().
   
   FileImportErrorTypeDetailsButtons:addButton("fileimporterrortype_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateFileImportErrorType('fileimporterrortype_details_form');").
   
   FileImportErrorTypeDetailsButtons:addButton("fileimporterrortype_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); 
                                              disablePopup('fileimporterrortype_details_form_popup');").
   
   FileImportErrorTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileImportErrorTypeDetailsForm:FormButtons = FileImportErrorTypeDetailsButtons.
   
   FileImportErrorTypeDetailsForm:endForm(). 
   
   FileImportErrorTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + FileImportErrorTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileImportErrorTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileImportErrorTypeDetailsFields Procedure 
PROCEDURE pFileImportErrorTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
/*      {adFileImportErrorTypeAdmin_fileimporterrortype_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
