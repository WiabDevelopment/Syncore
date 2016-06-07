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

  Created: 04/02/2016

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

DEFINE VARIABLE intSelectedQualityStationType           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQualityStationTypeRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQualityStationTypeRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQualityStationTypeID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQualityStationTypeHistory       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QualityStationTypeBrowseFrame           AS pageFrame.
DEFINE VARIABLE QualityStationTypeBrowse                AS browseTable.
DEFINE VARIABLE QualityStationTypeBrowseButtons         AS buttonBar.
DEFINE VARIABLE QualityStationTypeDetailsForm           AS dataForm.
DEFINE VARIABLE QualityStationTypeDetailsButtons        AS buttonBar.

DEFINE VARIABLE QualityStationTypeHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE QualityStationTypeHistoryBrowse         AS browseTable.
DEFINE VARIABLE QualityStationTypeHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE QualityStationTypeHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QualityStationTypeHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrQualityStationTypeID          = get-value("QualityStationTypeID")
          intSelectedQualityStationType    = INTEGER(chrQualityStationTypeID)
          chrScrollToQualityStationTypeRow = STRING(INTEGER(get-value("qualitystationtype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQualityStationTypeID <> "" THEN
     chrSelectQualityStationTypeRow = 'selectQualityStationTypeRow(document.getElementById("qualitystationtype_browse_row_' + chrQualityStationTypeID + '"),"' 
                                                         + chrQualityStationTypeID +  '");'.
   
   IF get-value('popup_qualitystationtypehistory_browse') = "yes" THEN
      chrPopupQualityStationTypeHistory  = 'enablePopup("qualitystationtypehistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("qualitystationtype_browse").scrollTop=' + chrScrollToQualityStationTypeRow 
                                                          + chrSelectQualityStationTypeRow + chrPopupQualityStationTypeHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "QualityStationType Admin".
   ThisPage:FrameTitle    = "QualityStationType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("qualitystationtype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQualityStationTypeBrowse.
   
   FIND FIRST QualityStationType NO-LOCK
      WHERE QualityStationType.QualityStationTypeID = intSelectedQualityStationType NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQualityStationTypeDetails.
   RUN pQualityStationTypeHistory.
   RUN pQualityStationTypeHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QualityStationTypeBrowseFrame           NO-ERROR.
   DELETE OBJECT QualityStationTypeBrowse                NO-ERROR.
   DELETE OBJECT QualityStationTypeBrowseButtons         NO-ERROR.
   DELETE OBJECT QualityStationTypeDetailsForm           NO-ERROR.
   DELETE OBJECT QualityStationTypeDetailsButtons        NO-ERROR.
   
   DELETE OBJECT QualityStationTypeHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT QualityStationTypeHistoryBrowse            NO-ERROR.
   DELETE OBJECT QualityStationTypeHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT QualityStationTypeHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT QualityStationTypeHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeBrowse Procedure 
PROCEDURE pQualityStationTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "qualitystationtype_details_form"}
   
   QualityStationTypeBrowse = NEW browseTable("qualitystationtype_browse").
   QualityStationTypeBrowse:BrowseWidth  = 965.
   QualityStationTypeBrowse:BrowseHeight = 455.
   QualityStationTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QualityStationTypeBrowse:insertColumn(fTL("TypeID"),           70, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualityStationType}
   
   QualityStationTypeBrowse:insertColumn(fTL("Type Code"),       120, "CHARACTER", "left", FALSE).
   QualityStationTypeBrowse:insertColumn(fTL("Type Name"),       120, "CHARACTER", "left", FALSE).
   QualityStationTypeBrowse:insertColumn(fTL("Type Descr"),      180, "CHARACTER", "left", FALSE).
   QualityStationTypeBrowse:insertColumn(fTL("Active"),           70, "LOGICAL",           FALSE).
   
   /*Body*/
   QualityStationTypeBrowse:startBody().
   
   FOR EACH QualityStationType NO-LOCK: /*idx=ActiveListingSequence*/
/*      WHERE QualityStationType.Active*/
      
      QualityStationTypeBrowse:startRow(QualityStationType.QualityStationTypeID, "selectQualityStationTypeRow(this," + '"' + STRING(QualityStationType.QualityStationTypeID) 
                                                                  + '"' + ");", "").
      QualityStationTypeBrowse:insertData(QualityStationType.QualityStationTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QualityStationType}
      
      QualityStationTypeBrowse:insertData(QualityStationType.TypeCode, "left").
      QualityStationTypeBrowse:insertData(QualityStationType.TypeName, "left").
      QualityStationTypeBrowse:insertData(QualityStationType.TypeDescr, "left").
      QualityStationTypeBrowse:insertData(STRING(QualityStationType.Active,"Yes/No")).
      
      /* Add hidden fields */
      QualityStationTypeBrowse:insertHiddenData("QualityStationTypeVersionID",QualityStationType.VersionID).
      
      QualityStationTypeBrowse:endRow().
      
   END. /*FOR EACH QualityStationType NO-LOCK */
   
   QualityStationTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualityStationTypeBrowse:getErrors().
   
   /* Create a new frame */
   QualityStationTypeBrowseFrame = NEW pageFrame().
   QualityStationTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QualityStationTypeBrowseFrame:FormAction="dbQualityStationTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QualityStationTypeBrowseFrame:formOpen("qualitystationtype_browse_form").
   
   /* Start the Frame Header */
   QualityStationTypeBrowseFrame:insertSpacer(5).
   QualityStationTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QualityStationTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QualityStationTypeBrowseFrame:frameClose().
   QualityStationTypeBrowseFrame:insertSpacer(10).
   
   QualityStationTypeBrowseFrame:insertHiddenField("qualitystationtype_browse_scroll","").
   QualityStationTypeBrowseFrame:insertHiddenField("QualityStationTypeID","").
   QualityStationTypeBrowseFrame:insertHiddenField("QualityStationTypeVersionID","").
   QualityStationTypeBrowseFrame:insertHiddenField("popup_qualitystationtypehistory_browse","").
   QualityStationTypeBrowseFrame:insertHiddenField("form_name","qualitystationtype_browse_form").
   QualityStationTypeBrowseFrame:insertHiddenField("prog_name","adQualityStationType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationTypeBrowseFrame}
   
   QualityStationTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   QualityStationTypeBrowseButtons = NEW buttonBar().
   QualityStationTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   QualityStationTypeBrowseButtons:addButton("qualitystationtype_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQualityStationTypeDetails('qualitystationtype_details_form');",
                                              "Disabled").
   
   QualityStationTypeBrowseButtons:addButton("qualitystationtype_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQualityStationType('qualitystationtype_details_form');",
                                             "").
                                             
   QualityStationTypeBrowseButtons:addButton("qualitystationtype_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   QualityStationTypeBrowseButtons:closeBar().  
   QualityStationTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeDetails Procedure 
PROCEDURE pQualityStationTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualitystationtype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualityStationTypeID,TypeCode,TypeName,TypeDescr,Active" 
          chrEditFieldList     = "TypeDescr,Active" 
          chrNewFieldList      = "TypeCode,TypeName,TypeDescr,Active" 
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualityStationTypeDetailsForm = NEW dataForm("qualitystationtype_details_form").
   QualityStationTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QualityStationTypeDetailsForm:FormAction = "dbQualityStationTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualityStationTypeDetailsForm:FormWidth   = 460.
   QualityStationTypeDetailsForm:FormHeight  = 200.
   QualityStationTypeDetailsForm:FormTitle   = "Quality Station Type Details".
   QualityStationTypeDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QualityStationTypeDetailsForm:insertPaddingColumn(30).
   QualityStationTypeDetailsForm:insertColumn(120).
   QualityStationTypeDetailsForm:insertColumn(160).
   QualityStationTypeDetailsForm:insertColumn(20).
   QualityStationTypeDetailsForm:insertColumn(4).
   QualityStationTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   QualityStationTypeDetailsForm:startRow().
   QualityStationTypeDetailsForm:insertLabel("Type ID").
   QualityStationTypeDetailsForm:insertTextField("QualityStationTypeID", "", 200, TRUE).  
   
   QualityStationTypeDetailsForm:startRow().
   QualityStationTypeDetailsForm:insertLabel("Type Code").
   QualityStationTypeDetailsForm:insertTextField("TypeCode", "", 200, TRUE).  
   
   QualityStationTypeDetailsForm:startRow().
   QualityStationTypeDetailsForm:insertLabel("Type Name").
   QualityStationTypeDetailsForm:insertTextField("TypeName", "", 200, TRUE).
   
   QualityStationTypeDetailsForm:startRow().
   QualityStationTypeDetailsForm:insertLabel("Type Descr").
   QualityStationTypeDetailsForm:insertTextAreaField("TypeDescr", "", 200, TRUE).  
   
   QualityStationTypeDetailsForm:startRow().
   QualityStationTypeDetailsForm:insertLabel(fTL("Active")). 
   QualityStationTypeDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QualityStationTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualityStationTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQualityStationTypeDetailsFields}
   
   /* Add Hidden Fields*/
   QualityStationTypeDetailsForm:insertHiddenField("qualitystationtype_browse_scroll", "").
   QualityStationTypeDetailsForm:insertHiddenField("form_name", "qualitystationtype_details_form").
   QualityStationTypeDetailsForm:insertHiddenField("prog_name", "adQualityStationType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationTypeDetailsForm}
   
   /* Create Button Bar */
   QualityStationTypeDetailsButtons = NEW buttonBar().
   
   QualityStationTypeDetailsButtons:addButton("qualitystationtype_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQualityStationType('qualitystationtype_details_form');").
   
   QualityStationTypeDetailsButtons:addButton("qualitystationtype_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualitystationtype_details_form_popup');").
   
   QualityStationTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualityStationTypeDetailsForm:FormButtons = QualityStationTypeDetailsButtons.
   
   QualityStationTypeDetailsForm:endForm(). 
   
   QualityStationTypeDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeDetailsFields Procedure 
PROCEDURE pQualityStationTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QualityStationTypeDetailsForm:startRow().
         QualityStationTypeDetailsForm:insertLabel(fTL("Field Label")).
         QualityStationTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeHistory Procedure
PROCEDURE pQualityStationTypeHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "qualitystationtypehistory_details_form"}
   
   FIND FIRST QualityStationType WHERE QualityStationType.QualityStationTypeID = intSelectedQualityStationType NO-LOCK NO-ERROR.
   
   QualityStationTypeHistoryBrowseForm = NEW dataForm("qualitystationtypehistory_browse_form").
   QualityStationTypeHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QualityStationTypeHistoryBrowseForm:FormWidth   = 850.
   QualityStationTypeHistoryBrowseForm:FormHeight  = 540.
   QualityStationTypeHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE QualityStationType THEN " for QualityStationTypeID: " 
                                                                  + STRING(QualityStationType.QualityStationTypeID) ELSE "").
   QualityStationTypeHistoryBrowseForm:FormType    = "xxl_large".
   
   QualityStationTypeHistoryBrowse = NEW browseTable("qualitystationtypehistory_browse").
   QualityStationTypeHistoryBrowse:BrowseWidth  = 830.
   QualityStationTypeHistoryBrowse:BrowseHeight = 500.
   QualityStationTypeHistoryBrowse:ExcelExport  = TRUE.
   QualityStationTypeHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QualityStationTypeHistoryBrowse:insertColumn(fTL("History ID"),      70, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualityStationTypeHistory}

   QualityStationTypeHistoryBrowse:insertColumn(fTL("Type Code"),      100, "CHARACTER", "left", FALSE).
   QualityStationTypeHistoryBrowse:insertColumn(fTL("Type Name"),      100, "CHARACTER", "left", FALSE).
   QualityStationTypeHistoryBrowse:insertColumn(fTL("Type Descr"),     150, "CHARACTER", "left", FALSE).
   QualityStationTypeHistoryBrowse:insertColumn(fTL("Active"),          70, "LOGICAL",           FALSE).
   QualityStationTypeHistoryBrowse:insertColumn(fTL("User"),           120, "CHARACTER", "left", FALSE).
   QualityStationTypeHistoryBrowse:insertColumn(fTL("Created"),        130, "CHARACTER", "left", FALSE).
   
   QualityStationTypeHistoryBrowse:StartBody().
   
   IF AVAILABLE QualityStationType THEN
   DO:
      /*List the QualityStationTypeHistory*/
      FOR EACH QualityStationTypeHistory NO-LOCK 
         WHERE  QualityStationTypeHistory.QualityStationTypeID = intSelectedQualityStationType
         BY QualityStationTypeHistory.QualityStationTypeHistoryID:
         
         FIND FIRST GateUser OF QualityStationTypeHistory NO-LOCK NO-ERROR.
       
         QualityStationTypeHistoryBrowse:startRow(QualityStationTypeHistory.QualityStationTypeHistoryID, "selectHistoryRow(this," + '"' + STRING(QualityStationTypeHistory.QualityStationTypeHistoryID) 
                                                                     + '","qualityStationTypeHistory"' + ");", "").
         QualityStationTypeHistoryBrowse:insertData(QualityStationTypeHistory.QualityStationTypeHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QualityStationTypeHistory}
         
         QualityStationTypeHistoryBrowse:insertData(QualityStationTypeHistory.TypeCode, "left").
         QualityStationTypeHistoryBrowse:insertData(QualityStationTypeHistory.TypeName, "left").
         QualityStationTypeHistoryBrowse:insertData(QualityStationTypeHistory.TypeDescr, "left").                            
         QualityStationTypeHistoryBrowse:insertData(STRING(QualityStationTypeHistory.Active, "Yes/No")).
         QualityStationTypeHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QualityStationTypeHistoryBrowse:insertData(fDisplayDate&Time(QualityStationTypeHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */         
         QualityStationTypeHistoryBrowse:insertHiddendata("QualityStationTypeHistoryID",QualityStationTypeHistory.QualityStationTypeHistoryID).
         
         QualityStationTypeHistoryBrowse:endRow().
      
      END. /* FOR EACH QualityStationTypeHistory */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   QualityStationTypeHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualityStationTypeHistoryBrowse:getErrors().
   
   QualityStationTypeHistoryBrowseForm:insertHiddenField("QualityStationTypeHistoryID","").
   QualityStationTypeHistoryBrowseForm:insertHiddenField("popup_qualitystationtypehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationTypeHistoryBrowseForm}
   
   /* Create Button Bar */
   QualityStationTypeHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QualityStationTypeHistoryBrowseButtons:addButton("qualitystationtypehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQualityStationTypeHistoryDetails('qualitystationtypehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QualityStationTypeHistoryBrowseButtons:addButton("qualitystationtypehistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_qualitystationtypehistory_browse.xml')").*/
   
   QualityStationTypeHistoryBrowseButtons:addButton("qualitystationtypehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('qualitystationtypehistory_browse_form_popup');").
   
   QualityStationTypeHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualityStationTypeHistoryBrowseForm:FormBrowse  = QualityStationTypeHistoryBrowse.
   QualityStationTypeHistoryBrowseForm:FormButtons = QualityStationTypeHistoryBrowseButtons.
   QualityStationTypeHistoryBrowseForm:endForm(). 
   
   QualityStationTypeHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSerialMaskRuleTypeHistoryDetails Procedure
PROCEDURE pQualityStationTypeHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "qualitystationtypehistory_details_form"}
   
   chrDisplayFieldList  = "QualityStationTypeHistoryID,QualityStationTypeID,TypeCode,TypeName,TypeDescr,"
                             + "Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QualityStationTypeHistoryDetailsForm = NEW dataForm("qualitystationtypehistory_details_form").
   QualityStationTypeHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QualityStationTypeHistoryDetailsForm:FormWidth   = 460.
   QualityStationTypeHistoryDetailsForm:FormHeight  = 300.
   QualityStationTypeHistoryDetailsForm:FormTitle   = "Quality Station Type History Details".
   QualityStationTypeHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QualityStationTypeHistoryDetailsForm:insertPaddingColumn(40).
   QualityStationTypeHistoryDetailsForm:insertColumn(120).
   QualityStationTypeHistoryDetailsForm:insertColumn(120).
   QualityStationTypeHistoryDetailsForm:insertColumn(20).
   QualityStationTypeHistoryDetailsForm:insertColumn(4).
   QualityStationTypeHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel(fTL("History ID")).
   QualityStationTypeHistoryDetailsForm:insertTextField("QualityStationTypeHistoryID", "", 200, TRUE).    
   
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel(fTL("TypeID")).
   QualityStationTypeHistoryDetailsForm:insertTextField("QualityStationTypeID", "", 200, TRUE).
   
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel(fTL("TypeCode")).
   QualityStationTypeHistoryDetailsForm:insertTextField("TypeCode", "", 200, TRUE).
   
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel(fTL("TypeName")).
   QualityStationTypeHistoryDetailsForm:insertTextField("TypeName", "", 200, TRUE).
   
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel(fTL("TypeDescr")).
   QualityStationTypeHistoryDetailsForm:insertTextAreaField("TypeDescr", "", 200, TRUE).
   
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel(fTL("Active")).
   QualityStationTypeHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QualityStationTypeHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualityStationTypeHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel("User").
   QualityStationTypeHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QualityStationTypeHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QualityStationTypeHistoryDetailsForm:startRow().
   QualityStationTypeHistoryDetailsForm:insertLabel("Created").
   QualityStationTypeHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QualityStationTypeHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QualityStationTypeHistoryDetailsForm:insertLabel(":").
   QualityStationTypeHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QualityStationTypeHistoryDetailsForm:insertHiddenField("qualitystationtype_browse_scroll","").
   QualityStationTypeHistoryDetailsForm:insertHiddenField("popup_qualitystationtypehistory_browse", "").
   QualityStationTypeHistoryDetailsForm:insertHiddenField("QualityStationTypeHistoryID","").
   QualityStationTypeHistoryDetailsForm:insertHiddenField("form_name","qualitystationtypehistory_details_form").
   QualityStationTypeHistoryDetailsForm:insertHiddenField("prog_name","adQualityStationType.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationTypeHistoryDetailsForm}
   
   /* Create Button Bar */
   QualityStationTypeHistoryDetailsButtons = NEW buttonBar().
   
   QualityStationTypeHistoryDetailsButtons:addButton("qualitystationtypehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('qualitystationtypehistory_details_form_popup');").
                                        
   QualityStationTypeHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QualityStationTypeHistoryDetailsForm:FormButtons = QualityStationTypeHistoryDetailsButtons.
   
   QualityStationTypeHistoryDetailsForm:endForm(). 
   QualityStationTypeHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

