&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adFileDelimiterAdmin.p 

  Description: ad file for the File Delimiter Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Evan Urzen

  Created: 24/02/2015

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
{defDataMigrationVariables.i}

/* Definitions for System Options for Receiving */
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedFileDelimiter           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedFileDelimiterHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrFileDelimiterHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFileDelimiterRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFileDelimiterRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFileDelimiterHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFileDelimiterID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                    AS CHARACTER   NO-UNDO.

/* Buffers */
/* Objects */
DEFINE VARIABLE FileDelimiterBrowseFrame           AS pageFrame.
DEFINE VARIABLE FileDelimiterBrowse                AS browseTable.
DEFINE VARIABLE FileDelimiterBrowseButtons         AS buttonBar.
DEFINE VARIABLE FileDelimiterDetailsForm           AS dataForm.
DEFINE VARIABLE FileDelimiterDetailsButtons        AS buttonBar.
DEFINE VARIABLE FileDelimiterHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE FileDelimiterHistoryBrowse         AS browseTable.
DEFINE VARIABLE FileDelimiterHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE FileDelimiterHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE FileDelimiterHistoryButtons        AS buttonBar.


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
   
   ASSIGN chrFileDelimiterID = get-value("FileDelimiterID")
          intSelectedFileDelimiter = INTEGER(chrFileDelimiterID)
          chrScrollToFileDelimiterRow = STRING(INTEGER(get-value("filedelimiter_browse_scroll"))) + ";"
          /*History details button*/
          chrFileDelimiterHistoryID = get-value("FileDelimiterHistoryID")
          intSelectedFileDelimiterHistory = INTEGER(chrFileDelimiterHistoryID)
          chrScrollToFileDelimiterHistoryRow = STRING(INTEGER(get-value("filedelimiterHistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrFileDelimiterID <> "" THEN
      chrFileDelimiterRow = 'selectFileDelimiterRow(document.getElementById("filedelimiter_browse_row_' + chrFileDelimiterID + '"),"' 
                                                          + chrFileDelimiterID +  '");'.
                                                          
   IF get-value('popup_filedelimiterhistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("filedelimiterhistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("filedelimiter_browse").scrollTop=' + chrScrollToFileDelimiterRow 
                                                           + chrFileDelimiterRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "File Delimiter Admin".
   ThisPage:FrameTitle = "File Delimiter Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("filedelimiter.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pFileDelimiterBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pFileDelimiterDetails.
   
   RUN pFileDelimiterHistoryBrowse.
   
   RUN pFileDelimiterHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT FileDelimiterBrowseFrame        NO-ERROR.
   DELETE OBJECT FileDelimiterBrowse             NO-ERROR.
   DELETE OBJECT FileDelimiterBrowseButtons      NO-ERROR.
   DELETE OBJECT FileDelimiterDetailsForm        NO-ERROR.
   DELETE OBJECT FileDelimiterDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pFileDelimiterBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "filedelimiter_details_form"}
   
   FileDelimiterBrowse = NEW browseTable("filedelimiter_browse").
   FileDelimiterBrowse:BrowseWidth  = 965.
   FileDelimiterBrowse:BrowseHeight = 455.
   FileDelimiterBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   FileDelimiterBrowse:insertColumn(fTL("FileDelimiter ID"), 235, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileDelimiter}
   
   FileDelimiterBrowse:insertColumn(fTL("Delimiter Code"),        235, "CHARACTER", "LEFT", FALSE). 
   FileDelimiterBrowse:insertColumn(fTL("Delimiter Name"),         235, "CHARACTER", "LEFT", FALSE).   
   FileDelimiterBrowse:insertColumn(fTL("Delimiter ASCII Value"),  235, "CHARACTER", "LEFT", FALSE). 
   
   
   
   /*Body*/
   FileDelimiterBrowse:startBody().
   
   FOR EACH FileDelimiter NO-LOCK: 
            
      FileDelimiterBrowse:startRow(FileDelimiter.FileDelimiterID, "selectFileDelimiterRow(this," + '"' + STRING(FileDelimiter.FileDelimiterID) + '"' + ");", "").
      FileDelimiterBrowse:insertData(FileDelimiter.FileDelimiterID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FileDelimiter}      

      FileDelimiterBrowse:insertData(IF AVAILABLE FileDelimiter   THEN STRING(FileDelimiter.DelimiterCode)         ELSE  "","LEFT"). 
      FileDelimiterBrowse:insertData(IF AVAILABLE FileDelimiter   THEN STRING(FileDelimiter.DelimiterName)         ELSE  "","LEFT"). 
      FileDelimiterBrowse:insertData(IF AVAILABLE FileDelimiter   THEN STRING(FileDelimiter.DelimiterASCIIValue)   ELSE  "","LEFT").
            
      /* Add hidden fields */
      FileDelimiterBrowse:insertHiddenData("FileDelimiterVersionID",FileDelimiter.VersionID  ).
      
      FileDelimiterBrowse:endRow().
      
   END. /*FOR EACH FileDelimiter NO-LOCK */
   
   FileDelimiterBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FileDelimiterBrowse:getErrors().
   
   /* Create a new frame */
   FileDelimiterBrowseFrame = NEW pageFrame().
   FileDelimiterBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FileDelimiterBrowseFrame:FormAction="dbFileDelimiterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FileDelimiterBrowseFrame:formOpen("filedelimiter_browse_form").
   
   /* Start the Frame Header */
   FileDelimiterBrowseFrame:insertSpacer(5).
   FileDelimiterBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FileDelimiterBrowse:displayBrowse().  
   
   /* End the Frame Header */
   FileDelimiterBrowseFrame:frameClose().
   FileDelimiterBrowseFrame:insertSpacer(10).
   
   FileDelimiterBrowseFrame:insertHiddenField("filedelimiter_browse_scroll","").
   FileDelimiterBrowseFrame:insertHiddenField("FileDelimiterID","").
   FileDelimiterBrowseFrame:insertHiddenField("FileDelimiterVersionID","").
   FileDelimiterBrowseFrame:insertHiddenField("form_name","filedelimiter_browse_form").
   FileDelimiterBrowseFrame:insertHiddenField("popup_filedelimiterhistory_browse","").
   FileDelimiterBrowseFrame:insertHiddenField("prog_name","adFileDelimiterAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileDelimiterBrowseFrame}
   
   FileDelimiterBrowseFrame:formClose().
   
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   FileDelimiterBrowseButtons = NEW buttonBar().
   FileDelimiterBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   FileDelimiterBrowseButtons:addButton("filedelimiter_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewFileDelimiterDetails('filedelimiter_details_form');",
                                         (IF intSelectedFileDelimiter > 0 THEN "" ELSE "Disabled")).
  
   IF NOT logPreventDataCreates THEN
   DO:  
   FileDelimiterBrowseButtons:addButton("filedelimiter_browse_form_btn_create",
                                         fTL("Create"),
                                         "createFileDelimiter('filedelimiter_details_form');",
                                         "").
   END.                                         
   
   FileDelimiterBrowseButtons:addButton("filedelimiter_browse_form_btn_history",
                                         fTL("History"),
                                         "viewFileDelimiterHistory('filedelimiter_browse_form');",
                                         (IF intSelectedFileDelimiter > 0 THEN "" ELSE "Disabled")).
   
   FileDelimiterBrowseButtons:closeBar().  
   FileDelimiterBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pFileDelimiterDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "filedelimiter_details_form"}
   ASSIGN chrDisplayFieldList  = "FileDelimiterID,DelimiterCode,DelimiterName,DelimiterASCIIValue,DelimiterValue"
          chrEditFieldList     = ""
          chrNewFieldList      = "DelimiterCode,DelimiterName,DelimiterASCIIValue,DelimiterValue"
          chrRequiredFieldList = "DelimiterCode,DelimiterName,DelimiterASCIIValue,DelimiterValue"
          chrExtraFieldList    = ""
          chrValidateFieldList = "DelimiterASCIIValue:INTEGER".

   
   FileDelimiterDetailsForm = NEW dataForm("filedelimiter_details_form").
   FileDelimiterDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileDelimiterDetailsForm:FormAction = "dbFileDelimiterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileDelimiterDetailsForm:FormWidth   = 460.
   FileDelimiterDetailsForm:FormHeight  = 200.
   FileDelimiterDetailsForm:FormTitle   = "File Delimiter details".
   FileDelimiterDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   FileDelimiterDetailsForm:insertPaddingColumn(30).
   FileDelimiterDetailsForm:insertColumn(185).
   
   /* Fields */
   FileDelimiterDetailsForm:startRow().
   FileDelimiterDetailsForm:insertLabel(fTL("File Delimiter ID")).
   FileDelimiterDetailsForm:insertTextField("FileDelimiterID", "", 190, TRUE).  
   
   FileDelimiterDetailsForm:startRow().
   FileDelimiterDetailsForm:insertLabel(fTL("Delimiter Code")).
   FileDelimiterDetailsForm:insertTextField("DelimiterCode", "", 190, TRUE).  
   
   FileDelimiterDetailsForm:startRow().
   FileDelimiterDetailsForm:insertLabel(fTL("Delimiter Name")).
   FileDelimiterDetailsForm:insertTextField("DelimiterName", "", 190, TRUE).  
   
   FileDelimiterDetailsForm:startRow().
   FileDelimiterDetailsForm:insertLabel(fTL("Delimiter ASCII Value")).
   FileDelimiterDetailsForm:insertTextField("DelimiterASCIIValue", "", 190, TRUE).  
   
   FileDelimiterDetailsForm:startRow().
   FileDelimiterDetailsForm:insertLabel(fTL("Delimiter Value")).
   FileDelimiterDetailsForm:insertTextField("DelimiterValue", "", 190, TRUE).  


   {webGetOptionalFormFields.i pFileDelimiterDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FileDelimiterDetailsForm:insertHiddenField("filedelimiter_browse_scroll", "").
   FileDelimiterDetailsForm:insertHiddenField("form_name", "filedelimiter_details_form").
   FileDelimiterDetailsForm:insertHiddenField("prog_name", "adFileDelimiterAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileDelimiterDetailsForm}
   
   /* Create Button Bar */
   FileDelimiterDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataCreates THEN 
      FileDelimiterDetailsButtons:addButton("filedelimiter_details_form_btn_save", 
                                       fTL("Save"), 
                                       "updateFileDelimiter('filedelimiter_details_form');").
   FileDelimiterDetailsButtons:addButton("filedelimiter_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('filedelimiter_details_form_popup');").
   FileDelimiterDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileDelimiterDetailsForm:FormButtons = FileDelimiterDetailsButtons.
   
   FileDelimiterDetailsForm:endForm(). 
   
   FileDelimiterDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pFileDelimiterDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      FileDelimiterDetailsForm:startRow().
      FileDelimiterDetailsForm:insertLabel(fTL("Field Label")).
      FileDelimiterDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pFileDelimiterHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "filedelimiterhistory_details_form"}
   ASSIGN chrDisplayFieldList  = "FileDelimiterHistoryID,FileDelimiterID,DelimiterCode,DelimiterName,DelimiterASCIIValue,"+
                                 "DelimiterValue,TransactionID,OperationTypeID,GateUserID,CreatedDate,CreatedHour,CreatedMins" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   FileDelimiterHistoryDetailsForm = NEW dataForm("filedelimiterhistory_details_form").
   FileDelimiterHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileDelimiterHistoryDetailsForm:FormAction = "dbFileDelimiterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileDelimiterHistoryDetailsForm:FormWidth   = 460.
   FileDelimiterHistoryDetailsForm:FormHeight  = 300.
   FileDelimiterHistoryDetailsForm:FormTitle   = "File Delimiter History Details".
   FileDelimiterHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   FileDelimiterHistoryDetailsForm:insertPaddingColumn(30).
   FileDelimiterHistoryDetailsForm:insertColumn(150).
   FileDelimiterHistoryDetailsForm:insertColumn(125).
   FileDelimiterHistoryDetailsForm:insertColumn(20).
   FileDelimiterHistoryDetailsForm:insertColumn(4).
   FileDelimiterHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("File Delimiter History ID")).
   FileDelimiterHistoryDetailsForm:insertTextField("FileDelimiterHistoryID", "", 100, TRUE).
   
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("File Delimiter ID")).
   FileDelimiterHistoryDetailsForm:insertTextField("FileDelimiterID", "", 100, TRUE).
   
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("Delimiter Code")).
   FileDelimiterHistoryDetailsForm:insertTextField("DelimiterCode", "", 100, TRUE).
   
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("Delimiter Name")).
   FileDelimiterHistoryDetailsForm:insertTextField("DelimiterName", "", 100, TRUE).
   
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("Delimiter ASCII Value")).
   FileDelimiterHistoryDetailsForm:insertTextField("DelimiterASCIIValue", "", 100, TRUE).
   
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("Delimiter Value")).
   FileDelimiterHistoryDetailsForm:insertTextField("DelimiterValue", "", 100, TRUE).
   
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("Created")).
   FileDelimiterHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).  
   /* Time fields have no label */
   FileDelimiterHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   FileDelimiterHistoryDetailsForm:insertLabel(":").
   FileDelimiterHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE). 
   
   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).
   FileDelimiterHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).
   FOR EACH OperationType NO-LOCK
      BY OperationType.OperationTypeID:
      FileDelimiterHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END.

   FileDelimiterHistoryDetailsForm:startRow().
   FileDelimiterHistoryDetailsForm:insertLabel(fTL("Gate User ID")).
   FileDelimiterHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).   
   FOR EACH GateUser NO-LOCK 
      BY GateUser.FullName:
      FileDelimiterHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.

   {webGetOptionalFormFields.i pFileDelimiterHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FileDelimiterHistoryDetailsForm:insertHiddenField("filedelimiterhistory_browse_scroll", "").
   FileDelimiterHistoryDetailsForm:insertHiddenField("form_name", "filedelimiterhistory_details_form").
   FileDelimiterHistoryDetailsForm:insertHiddenField("prog_name", "adFileDelimiterAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileDelimiterHistoryDetailsForm}
   
   /* Create Button Bar */
   FileDelimiterHistoryDetailsButtons = NEW buttonBar().

   FileDelimiterHistoryDetailsButtons:addButton("filedelimiterhistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "disablePopup('filedelimiterhistory_details_form_popup');").
   FileDelimiterHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileDelimiterHistoryDetailsForm:FormButtons = FileDelimiterHistoryDetailsButtons.
   
   FileDelimiterHistoryDetailsForm:endForm(). 
   
   FileDelimiterHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pFileDelimiterHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      FileDelimiterDetailsForm:startRow().
      FileDelimiterDetailsForm:insertLabel(fTL("Field Label")).
      FileDelimiterDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adFileDelimiter_filedelimiter_details_form.i}*/ COMEBACK START COMEBACK END*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pFileDelimiterHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   FileDelimiterHistoryBrowseForm           = NEW dataForm("filedelimiterhistory_browse_form").
   FileDelimiterHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   FileDelimiterHistoryBrowseForm:FormWidth  = 860.
   FileDelimiterHistoryBrowseForm:FormHeight = 530.
   FileDelimiterHistoryBrowseForm:FormTitle  = fTL("File Delimiter History").
   FileDelimiterHistoryBrowseForm:FormType   = "xxl_large".
   FileDelimiterHistoryBrowse                = NEW browseTable("filedelimiterhistory_browse").
   FileDelimiterHistoryBrowse:BrowseWidth    = 840.
   FileDelimiterHistoryBrowse:BrowseHeight   = 490.
   
   FileDelimiterHistoryBrowse:insertColumn(fTL("Hist Delimiter ID"),       150, "INTEGER",   "LEFT", FALSE).
   FileDelimiterHistoryBrowse:insertColumn(fTL("Delimiter Code"),          175, "CHARACTER", "LEFT", FALSE).
   FileDelimiterHistoryBrowse:insertColumn(fTL("Delimiter ASCII Value"),   200, "CHARACTER", "LEFT", FALSE).
   FileDelimiterHistoryBrowse:insertColumn(fTL("User"),                    125, "CHARACTER", "LEFT", FALSE).
   FileDelimiterHistoryBrowse:insertColumn(fTL("Created"),                 150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileDelimiterHistory}
   
   FileDelimiterHistoryBrowse:StartBody().
   
   FOR EACH FileDelimiterHistory NO-LOCK
      WHERE FileDelimiterHistory.FileDelimiterID = intSelectedFileDelimiter
         BY FileDelimiterHistory.FileDelimiterHistoryID:
          
      FIND FIRST OperationType OF FileDelimiterHistory NO-LOCK NO-ERROR.
      FIND FIRST GateUser      OF FileDelimiterHistory NO-LOCK NO-ERROR.

      FileDelimiterHistoryBrowse:startRow (FileDelimiterHistory.FileDelimiterHistoryID, 
         "selectFileDelimiterHistoryRow(this," + '"' + STRING(FileDelimiterHistory.FileDelimiterHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i FileDelimiterHistory}
      
      FileDelimiterHistoryBrowse:insertData(FileDelimiterHistory.FileDelimiterHistoryID, "LEFT").
      FileDelimiterHistoryBrowse:insertData(FileDelimiterHistory.DelimiterCode,"LEFT").
      FileDelimiterHistoryBrowse:insertData(STRING(FileDelimiterHistory.DelimiterASCIIValue),"LEFT").
      FileDelimiterHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      FileDelimiterHistoryBrowse:insertData(fDisplayDate&Time(FileDelimiterHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      FileDelimiterHistoryBrowse:endRow().
   END. /* FOR EACH FileDelimiterHistory */
   
   FileDelimiterHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FileDelimiterHistoryBrowse:getErrors().
   
   FileDelimiterHistoryBrowseForm:insertHiddenField("popup_filedelimiterhistory_browse","").
   FileDelimiterHistoryBrowseForm:insertHiddenField("FileDelimiterHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileDelimiterHistoryBrowseForm}
   
   /* Create Button Bar */
   FileDelimiterHistoryButtons = NEW buttonBar().
   
      FileDelimiterHistoryButtons:addButton("filedelimiterhistory_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewFileDelimiterHistoryDetails('filedelimiterhistory_details_form');",/*maybe*/
                                         (IF intSelectedFileDelimiterHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   FileDelimiterHistoryButtons:addButton("filedelimiterhistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('filedelimiterhistory_browse_form_popup');").
   FileDelimiterHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileDelimiterHistoryBrowseForm:FormBrowse  = FileDelimiterHistoryBrowse.
   FileDelimiterHistoryBrowseForm:FormButtons = FileDelimiterHistoryButtons.
   FileDelimiterHistoryBrowseForm:endForm(). 
   
   FileDelimiterHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


