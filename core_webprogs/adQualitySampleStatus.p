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

  Author: Anthony Ferrari

  Created: 03/02/2016

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

DEFINE VARIABLE intSelectedQualitySampleStatus           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQualitySampleStatusRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQualitySampleStatusRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQualitySampleStatusID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQualitySampleStatusHistory       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QualitySampleStatusBrowseFrame           AS pageFrame.
DEFINE VARIABLE QualitySampleStatusBrowse                AS browseTable.
DEFINE VARIABLE QualitySampleStatusBrowseButtons         AS buttonBar.
DEFINE VARIABLE QualitySampleStatusDetailsForm           AS dataForm.
DEFINE VARIABLE QualitySampleStatusDetailsButtons        AS buttonBar.

DEFINE VARIABLE QualitySampleStatusHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE QualitySampleStatusHistoryBrowse         AS browseTable.
DEFINE VARIABLE QualitySampleStatusHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE QualitySampleStatusHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QualitySampleStatusHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrQualitySampleStatusID          = get-value("QualitySampleStatusID")
          intSelectedQualitySampleStatus    = INTEGER(chrQualitySampleStatusID)
          chrScrollToQualitySampleStatusRow = STRING(INTEGER(get-value("qualitysamplestatus_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQualitySampleStatusID <> "" THEN
     chrSelectQualitySampleStatusRow = 'selectQualitySampleStatusRow(document.getElementById("qualitysamplestatus_browse_row_' + chrQualitySampleStatusID + '"),"' 
                                                         + chrQualitySampleStatusID +  '");'.
   
   IF get-value('popup_qualitysamplestatushistory_browse') = "yes" THEN
      chrPopupQualitySampleStatusHistory  = 'enablePopup("qualitysamplestatushistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("qualitysamplestatus_browse").scrollTop=' + chrScrollToQualitySampleStatusRow 
                                                          + chrSelectQualitySampleStatusRow + chrPopupQualitySampleStatusHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "QualitySampleStatus Admin".
   ThisPage:FrameTitle    = "QualitySampleStatus Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("qualitysamplestatus.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQualitySampleStatusBrowse.
   
   FIND FIRST QualitySampleStatus NO-LOCK
      WHERE QualitySampleStatus.QualitySampleStatusID = intSelectedQualitySampleStatus NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQualitySampleStatusDetails.
   RUN pQualitySampleStatusHistory.
   RUN pQualitySampleStatusHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QualitySampleStatusBrowseFrame           NO-ERROR.
   DELETE OBJECT QualitySampleStatusBrowse                NO-ERROR.
   DELETE OBJECT QualitySampleStatusBrowseButtons         NO-ERROR.
   DELETE OBJECT QualitySampleStatusDetailsForm           NO-ERROR.
   DELETE OBJECT QualitySampleStatusDetailsButtons        NO-ERROR.
   
   DELETE OBJECT QualitySampleStatusHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT QualitySampleStatusHistoryBrowse            NO-ERROR.
   DELETE OBJECT QualitySampleStatusHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT QualitySampleStatusHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT QualitySampleStatusHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleStatusBrowse Procedure 
PROCEDURE pQualitySampleStatusBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "qualitysamplestatus_details_form"}
   
   QualitySampleStatusBrowse = NEW browseTable("qualitysamplestatus_browse").
   QualitySampleStatusBrowse:BrowseWidth  = 965.
   QualitySampleStatusBrowse:BrowseHeight = 455.
   QualitySampleStatusBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QualitySampleStatusBrowse:insertColumn(fTL("Status ID"),     75, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySampleStatus}
   
   QualitySampleStatusBrowse:insertColumn(fTL("Status Code"),  120, "CHARACTER", "left", FALSE).
   QualitySampleStatusBrowse:insertColumn(fTL("Status Name"),  120, "CHARACTER", "left", FALSE).
   QualitySampleStatusBrowse:insertColumn(fTL("Status Descr"), 150, "CHARACTER", "left", FALSE).
   QualitySampleStatusBrowse:insertColumn(fTL("Active"),        50, "LOGICAL",           FALSE).
   
   /*Body*/
   QualitySampleStatusBrowse:startBody().
   
   FOR EACH QualitySampleStatus NO-LOCK: /*idx=QualitySampleStatusID*/
      
      QualitySampleStatusBrowse:startRow(QualitySampleStatus.QualitySampleStatusID, "selectQualitySampleStatusRow(this," + '"' + STRING(QualitySampleStatus.QualitySampleStatusID) 
                                                                  + '"' + ");", "").
      QualitySampleStatusBrowse:insertData(QualitySampleStatus.QualitySampleStatusID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QualitySampleStatus}
      
      QualitySampleStatusBrowse:insertData(QualitySampleStatus.StatusCode, "left").
      QualitySampleStatusBrowse:insertData(QualitySampleStatus.StatusName, "left").
      QualitySampleStatusBrowse:insertData(QualitySampleStatus.StatusDescr, "left").
      QualitySampleStatusBrowse:insertData(STRING(QualitySampleStatus.Active,"Yes/No")).
      
      /* Add hidden fields */
      QualitySampleStatusBrowse:insertHiddenData("QualitySampleStatusVersionID",QualitySampleStatus.VersionID).
      
      QualitySampleStatusBrowse:endRow().
      
   END. /*FOR EACH QualitySampleStatus NO-LOCK */
   
   QualitySampleStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleStatusBrowse:getErrors().
   
   /* Create a new frame */
   QualitySampleStatusBrowseFrame = NEW pageFrame().
   QualitySampleStatusBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QualitySampleStatusBrowseFrame:FormAction="dbQualitySampleStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QualitySampleStatusBrowseFrame:formOpen("qualitysamplestatus_browse_form").
   
   /* Start the Frame Header */
   QualitySampleStatusBrowseFrame:insertSpacer(5).
   QualitySampleStatusBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QualitySampleStatusBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QualitySampleStatusBrowseFrame:frameClose().
   QualitySampleStatusBrowseFrame:insertSpacer(10).
   
   QualitySampleStatusBrowseFrame:insertHiddenField("qualitysamplestatus_browse_scroll","").
   QualitySampleStatusBrowseFrame:insertHiddenField("QualitySampleStatusID","").
   QualitySampleStatusBrowseFrame:insertHiddenField("QualitySampleStatusVersionID","").
   QualitySampleStatusBrowseFrame:insertHiddenField("popup_qualitysamplestatushistory_browse","").
   QualitySampleStatusBrowseFrame:insertHiddenField("form_name","qualitysamplestatus_browse_form").
   QualitySampleStatusBrowseFrame:insertHiddenField("prog_name","adQualitySampleStatus.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleStatusBrowseFrame}
   
   QualitySampleStatusBrowseFrame:formClose().
   
   /* Create Button Bar */
   QualitySampleStatusBrowseButtons = NEW buttonBar().
   QualitySampleStatusBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   QualitySampleStatusBrowseButtons:addButton("qualitysamplestatus_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQualitySampleStatusDetails('qualitysamplestatus_details_form');",
                                             (IF intSelectedQualitySampleStatus > 0 THEN "" ELSE "Disabled")).
   
   QualitySampleStatusBrowseButtons:addButton("qualitysamplestatus_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQualitySampleStatus('qualitysamplestatus_details_form');",
                                             "").
                                             
   QualitySampleStatusBrowseButtons:addButton("qualitysamplestatus_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   QualitySampleStatusBrowseButtons:addButton("qualitysamplestatus_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteQualitySampleStatus();",
                                             (IF intSelectedQualitySampleStatus > 0 THEN "" ELSE "Disabled")).
   */
   
   QualitySampleStatusBrowseButtons:closeBar().  
   QualitySampleStatusBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleStatusDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleStatusDetails Procedure 
PROCEDURE pQualitySampleStatusDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualitysamplestatus_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualitySampleStatusID,StatusCode,StatusName,StatusDescr,Active" 
          chrEditFieldList     = "StatusDescr,Active" 
          chrNewFieldList      = "StatusCode,StatusName,StatusDescr,Active" 
          chrRequiredFieldList = "StatusCode,StatusName,StatusDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualitySampleStatusDetailsForm = NEW dataForm("qualitysamplestatus_details_form").
   QualitySampleStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QualitySampleStatusDetailsForm:FormAction = "dbQualitySampleStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualitySampleStatusDetailsForm:FormWidth   = 460.
   QualitySampleStatusDetailsForm:FormHeight  = 200.
   QualitySampleStatusDetailsForm:FormTitle   = "QualitySample Status Details".
   QualitySampleStatusDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QualitySampleStatusDetailsForm:insertPaddingColumn(30).
   QualitySampleStatusDetailsForm:insertColumn(150).
   QualitySampleStatusDetailsForm:insertColumn(160).
   QualitySampleStatusDetailsForm:insertColumn(20).
   QualitySampleStatusDetailsForm:insertColumn(4).
   QualitySampleStatusDetailsForm:insertColumn(110).
   
   /* Fields */
   QualitySampleStatusDetailsForm:startRow().
   QualitySampleStatusDetailsForm:insertLabel("Status ID").
   QualitySampleStatusDetailsForm:insertTextField("QualitySampleStatusID", "", 200, TRUE).  
   
   QualitySampleStatusDetailsForm:startRow().
   QualitySampleStatusDetailsForm:insertLabel("Status Code").
   QualitySampleStatusDetailsForm:insertTextField("StatusCode", "", 200, TRUE).  
   
   QualitySampleStatusDetailsForm:startRow().
   QualitySampleStatusDetailsForm:insertLabel("Status Name").
   QualitySampleStatusDetailsForm:insertTextField("StatusName", "", 200, TRUE).
   
   QualitySampleStatusDetailsForm:startRow().
   QualitySampleStatusDetailsForm:insertLabel("Status Descr").
   QualitySampleStatusDetailsForm:insertTextAreaField("StatusDescr", "", 200, TRUE).  

   QualitySampleStatusDetailsForm:startRow().
   QualitySampleStatusDetailsForm:insertLabel(fTL("Active")). 
   QualitySampleStatusDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QualitySampleStatusDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualitySampleStatusDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQualitySampleStatusDetailsFields}
   
   /* Add Hidden Fields*/
   QualitySampleStatusDetailsForm:insertHiddenField("qualitysamplestatus_browse_scroll", "").
   QualitySampleStatusDetailsForm:insertHiddenField("form_name", "qualitysamplestatus_details_form").
   QualitySampleStatusDetailsForm:insertHiddenField("prog_name", "adQualitySampleStatus.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleStatusDetailsForm}
   
   /* Create Button Bar */
   QualitySampleStatusDetailsButtons = NEW buttonBar().
   
   QualitySampleStatusDetailsButtons:addButton("qualitysamplestatus_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQualitySampleStatus('qualitysamplestatus_details_form');").
   
   QualitySampleStatusDetailsButtons:addButton("qualitysamplestatus_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualitysamplestatus_details_form_popup');").
   
   QualitySampleStatusDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleStatusDetailsForm:FormButtons = QualitySampleStatusDetailsButtons.
   
   QualitySampleStatusDetailsForm:endForm(). 
   
   QualitySampleStatusDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QualitySampleStatusDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleStatusDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleStatusDetailsFields Procedure 
PROCEDURE pQualitySampleStatusDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QualitySampleStatusDetailsForm:startRow().
         QualitySampleStatusDetailsForm:insertLabel(fTL("Field Label")).
         QualitySampleStatusDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleStatusHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleStatusHistory Procedure
PROCEDURE pQualitySampleStatusHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "qualitysamplestatushistory_details_form"}
   
   FIND FIRST QualitySampleStatus WHERE QualitySampleStatus.QualitySampleStatusID = intSelectedQualitySampleStatus NO-LOCK NO-ERROR.
   
   QualitySampleStatusHistoryBrowseForm = NEW dataForm("qualitysamplestatushistory_browse_form").
   QualitySampleStatusHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QualitySampleStatusHistoryBrowseForm:FormWidth   = 850.
   QualitySampleStatusHistoryBrowseForm:FormHeight  = 540.
   QualitySampleStatusHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE QualitySampleStatus THEN " for MaskRuleTypeID: " 
                                                                  + STRING(QualitySampleStatus.QualitySampleStatusID) ELSE "").
   QualitySampleStatusHistoryBrowseForm:FormType    = "xxl_large".
   
   QualitySampleStatusHistoryBrowse = NEW browseTable("qualitysamplestatushistory_browse").
   QualitySampleStatusHistoryBrowse:BrowseWidth  = 830.
   QualitySampleStatusHistoryBrowse:BrowseHeight = 500.
   QualitySampleStatusHistoryBrowse:ExcelExport  = TRUE.
   QualitySampleStatusHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QualitySampleStatusHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySampleStatusHistory}

   QualitySampleStatusHistoryBrowse:insertColumn(fTL("Status Code"),    120, "CHARACTER", "left", FALSE).
   QualitySampleStatusHistoryBrowse:insertColumn(fTL("Status Name"),    120, "CHARACTER", "left", FALSE).
   QualitySampleStatusHistoryBrowse:insertColumn(fTL("Status Descr"),   140, "CHARACTER", "left", FALSE).
   QualitySampleStatusHistoryBrowse:insertColumn(fTL("Active"),          50, "LOGICAL",           FALSE).
   QualitySampleStatusHistoryBrowse:insertColumn(fTL("User"),           120, "CHARACTER", "left", FALSE).
   QualitySampleStatusHistoryBrowse:insertColumn(fTL("Created"),        120, "CHARACTER", "left", FALSE).
   
   QualitySampleStatusHistoryBrowse:StartBody().
   
   IF AVAILABLE QualitySampleStatus THEN
   DO:
      /*List the QualitySampleStatusHistory*/
      FOR EACH QualitySampleStatusHistory NO-LOCK 
         WHERE  QualitySampleStatusHistory.QualitySampleStatusID = intSelectedQualitySampleStatus
         BY QualitySampleStatusHistory.QualitySampleStatusHistoryID:
         
         FIND FIRST GateUser OF QualitySampleStatusHistory NO-LOCK NO-ERROR.
       
         QualitySampleStatusHistoryBrowse:startRow(QualitySampleStatusHistory.QualitySampleStatusHistoryID, "selectHistoryRow(this," + '"' + STRING(QualitySampleStatusHistory.QualitySampleStatusHistoryID) 
                                                                     + '","qualitySampleStatusHistory"' + ");", "").
         QualitySampleStatusHistoryBrowse:insertData(QualitySampleStatusHistory.QualitySampleStatusHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QualitySampleStatusHistory}
         
         QualitySampleStatusHistoryBrowse:insertData(QualitySampleStatusHistory.StatusCode, "left").
         QualitySampleStatusHistoryBrowse:insertData(QualitySampleStatusHistory.StatusName, "left").
         QualitySampleStatusHistoryBrowse:insertData(QualitySampleStatusHistory.StatusDescr, "left").                            
         QualitySampleStatusHistoryBrowse:insertData(STRING(QualitySampleStatusHistory.Active, "Yes/No")).
         QualitySampleStatusHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QualitySampleStatusHistoryBrowse:insertData(fDisplayDate&Time(QualitySampleStatusHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         QualitySampleStatusHistoryBrowse:insertHiddendata("QualitySampleStatusHistoryID",QualitySampleStatusHistory.QualitySampleStatusHistoryID).
         
         QualitySampleStatusHistoryBrowse:endRow().
      
      END. /* FOR EACH QualitySampleStatusHistory OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   QualitySampleStatusHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleStatusHistoryBrowse:getErrors().
   
   QualitySampleStatusHistoryBrowseForm:insertHiddenField("QualitySampleStatusHistoryID","").
   QualitySampleStatusHistoryBrowseForm:insertHiddenField("popup_qualitysamplestatushistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleStatusHistoryBrowseForm}
   
   /* Create Button Bar */
   QualitySampleStatusHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QualitySampleStatusHistoryBrowseButtons:addButton("qualitysamplestatushistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQualitySampleStatusHistoryDetails('qualitysamplestatushistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QualitySampleStatusHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   QualitySampleStatusHistoryBrowseButtons:addButton("qualitysamplestatushistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('qualitysamplestatushistory_browse_form_popup');").
   
   QualitySampleStatusHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleStatusHistoryBrowseForm:FormBrowse  = QualitySampleStatusHistoryBrowse.
   QualitySampleStatusHistoryBrowseForm:FormButtons = QualitySampleStatusHistoryBrowseButtons.
   QualitySampleStatusHistoryBrowseForm:endForm(). 
   
   QualitySampleStatusHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleStatusHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleStatusHistoryDetails Procedure
PROCEDURE pQualitySampleStatusHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "qualitysamplestatushistory_details_form"}
   
   chrDisplayFieldList  = "QualitySampleStatusHistoryID,QualitySampleStatusID,StatusCode,StatusName,StatusDescr,"
                             + "Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QualitySampleStatusHistoryDetailsForm = NEW dataForm("qualitysamplestatushistory_details_form").
   QualitySampleStatusHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QualitySampleStatusHistoryDetailsForm:FormWidth   = 460.
   QualitySampleStatusHistoryDetailsForm:FormHeight  = 300.
   QualitySampleStatusHistoryDetailsForm:FormTitle   = "Quality Sample Status History Details".
   QualitySampleStatusHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QualitySampleStatusHistoryDetailsForm:insertPaddingColumn(40).
   QualitySampleStatusHistoryDetailsForm:insertColumn(120).
   QualitySampleStatusHistoryDetailsForm:insertColumn(120).
   QualitySampleStatusHistoryDetailsForm:insertColumn(20).
   QualitySampleStatusHistoryDetailsForm:insertColumn(4).
   QualitySampleStatusHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel(fTL("History ID")).
   QualitySampleStatusHistoryDetailsForm:insertTextField("QualitySampleStatusHistoryID", "", 200, TRUE).    
   
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel(fTL("StatusID")).
   QualitySampleStatusHistoryDetailsForm:insertTextField("QualitySampleStatusID", "", 200, TRUE).
   
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel(fTL("StatusCode")).
   QualitySampleStatusHistoryDetailsForm:insertTextField("StatusCode", "", 200, TRUE).
   
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel(fTL("StatusName")).
   QualitySampleStatusHistoryDetailsForm:insertTextField("StatusName", "", 200, TRUE).
   
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel(fTL("StatusDescr")).
   QualitySampleStatusHistoryDetailsForm:insertTextAreaField("StatusDescr", "", 200, TRUE).
   
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel("User").
   QualitySampleStatusHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QualitySampleStatusHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel(fTL("Active")).
   QualitySampleStatusHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QualitySampleStatusHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualitySampleStatusHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QualitySampleStatusHistoryDetailsForm:startRow().
   QualitySampleStatusHistoryDetailsForm:insertLabel("Created").
   QualitySampleStatusHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QualitySampleStatusHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QualitySampleStatusHistoryDetailsForm:insertLabel(":").
   QualitySampleStatusHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QualitySampleStatusHistoryDetailsForm:insertHiddenField("qualitysamplestatus_browse_scroll","").
   QualitySampleStatusHistoryDetailsForm:insertHiddenField("popup_qualitysamplestatushistory_browse", "").
   QualitySampleStatusHistoryDetailsForm:insertHiddenField("QualitySampleStatusHistoryID","").
   QualitySampleStatusHistoryDetailsForm:insertHiddenField("form_name","qualitysamplestatushistory_details_form").
   QualitySampleStatusHistoryDetailsForm:insertHiddenField("prog_name","adQualitySampleStatus.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleStatusHistoryDetailsForm}
   
   /* Create Button Bar */
   QualitySampleStatusHistoryDetailsButtons = NEW buttonBar().
   
   QualitySampleStatusHistoryDetailsButtons:addButton("qualitysamplestatushistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('qualitysamplestatushistory_details_form_popup');").
                                        
   QualitySampleStatusHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleStatusHistoryDetailsForm:FormButtons = QualitySampleStatusHistoryDetailsButtons.
   
   QualitySampleStatusHistoryDetailsForm:endForm(). 
   QualitySampleStatusHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

