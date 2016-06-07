&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adQualitySampleTypeAdmin.p 

  Description: ad file for the Quality SampleType Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Shawn Hilts

  Created: 24/11/2014
  
  Revisions:
     13/10/2015 - twierzch - change StatusIDs to dropdowns instead of text boxes

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
DEFINE VARIABLE intSelectedQualitySampleType        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrQualitySampleTypeRow             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQualitySampleTypeRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQualitySampleTypeID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                 AS CHARACTER   NO-UNDO.

/* Buffers */
DEFINE BUFFER inProcessStockStatus     FOR StockStatus.
DEFINE BUFFER postProcessStockStatus   FOR StockStatus.
DEFINE BUFFER replacedPartStockStatus  FOR StockStatus.

/* Objects */
DEFINE VARIABLE QualitySampleTypeBrowseFrame           AS pageFrame.
DEFINE VARIABLE QualitySampleTypeBrowse                AS browseTable.
DEFINE VARIABLE QualitySampleTypeBrowseButtons         AS buttonBar.
DEFINE VARIABLE QualitySampleTypeDetailsForm           AS dataForm.
DEFINE VARIABLE QualitySampleTypeHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QualitySampleTypeDetailsButtons        AS buttonBar.
DEFINE VARIABLE QualitySampleTypeHistoryBrowseForm     AS dataform. 
DEFINE VARIABLE QualitySampleTypeHistoryBrowse         AS browseTable.
DEFINE VARIABLE QualitySampleTypeHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE QualitySampleTypeHistoryBrowseButtons  AS buttonBar.


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

&IF DEFINED(EXCLUDE-pInboundConfigHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundSampleTypeHistoryDetails Procedure
PROCEDURE pQualitySampleTypeHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualitysampletypehistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualitySampleTypeHistoryID,QualitySampleTypeID,TypeCode," +
                                 "TypeName,TypeDescr,Active,GateUserID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualitySampleTypeHistoryDetailsForm = NEW dataForm("qualitysampletypehistory_details_form").
   QualitySampleTypeHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   /*QualitySampleTypeHistoryDetailsForm:FormAction = "dbQualitySampleTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").*/
   
   /* Setup */
   QualitySampleTypeHistoryDetailsForm:FormWidth   = 460.
   QualitySampleTypeHistoryDetailsForm:FormHeight  = 300.
   QualitySampleTypeHistoryDetailsForm:FormTitle   = fTL("Quality SampleType History Details").
   QualitySampleTypeHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QualitySampleTypeHistoryDetailsForm:insertPaddingColumn(30).
   QualitySampleTypeHistoryDetailsForm:insertColumn(185).
   QualitySampleTypeHistoryDetailsForm:insertColumn(120).
   QualitySampleTypeHistoryDetailsForm:insertColumn(20).
   QualitySampleTypeHistoryDetailsForm:insertColumn(4).
   QualitySampleTypeHistoryDetailsForm:insertColumn(100).
   
   /* Fields */ 
   
   QualitySampleTypeHistoryDetailsForm:startRow().
   QualitySampleTypeHistoryDetailsForm:insertLabel("History ID").
   QualitySampleTypeHistoryDetailsForm:insertTextField("QualitySampleTypeHistoryID", "", 110, TRUE). 
   
   QualitySampleTypeHistoryDetailsForm:startRow().
   QualitySampleTypeHistoryDetailsForm:insertLabel(fTL("QualitySampleTypeID")).
   QualitySampleTypeHistoryDetailsForm:insertTextField("QualitySampleTypeID", "", 190, TRUE).
   
   QualitySampleTypeHistoryDetailsForm:startRow().
   QualitySampleTypeHistoryDetailsForm:insertLabel(fTL("Type Code")).
   QualitySampleTypeHistoryDetailsForm:insertTextField("TypeCode", "", 190, TRUE).
   
   QualitySampleTypeHistoryDetailsForm:startRow().
   QualitySampleTypeHistoryDetailsForm:insertLabel(fTL("Type Name")).
   QualitySampleTypeHistoryDetailsForm:insertTextField("TypeName", "", 190, TRUE).
   
   QualitySampleTypeHistoryDetailsForm:startRow().
   QualitySampleTypeHistoryDetailsForm:insertLabel(fTL("TypeDescr")).
   QualitySampleTypeHistoryDetailsForm:insertTextAreaField("TypeDescr", "", 190, TRUE).
   
   QualitySampleTypeHistoryDetailsForm:startRow().
   QualitySampleTypeHistoryDetailsForm:insertLabel(fTL("Active")).
   QualitySampleTypeHistoryDetailsForm:insertComboField("Active", "", 190, TRUE).  
   QualitySampleTypeHistoryDetailsForm:insertComboPairs("Active", "yes", "Yes").
   QualitySampleTypeHistoryDetailsForm:insertComboPairs("Active", "no",  "No").

   QualitySampleTypeHistoryDetailsForm:startRow().
   QualitySampleTypeHistoryDetailsForm:insertLabel(fTL("GateUser")).
   QualitySampleTypeHistoryDetailsForm:insertComboField("GateUserID", "", 190, TRUE).
   FOR EACH GateUser NO-LOCK:
      QualitySampleTypeHistoryDetailsForm:insertComboPairs("GateUserID", 
                                                       STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   {webGetOptionalFormFields.i pQualitySampleTypeHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   QualitySampleTypeHistoryDetailsForm:insertHiddenField("qualitysampletype_browse_scroll", "").
   QualitySampleTypeHistoryDetailsForm:insertHiddenField("form_name", "qualitysampletype_details_form").
   QualitySampleTypeHistoryDetailsForm:insertHiddenField("prog_name", "adQualitySampleTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleTypeHistoryDetailsForm}
   
   /* Create Button Bar */
   QualitySampleTypeHistoryDetailsButtons = NEW buttonBar().
   QualitySampleTypeHistoryDetailsButtons:addButton("qualitysampletypehistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualitysampletypehistory_details_form_popup');").
   QualitySampleTypeHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleTypeHistoryDetailsForm:FormButtons = QualitySampleTypeHistoryDetailsButtons.
   
   QualitySampleTypeHistoryDetailsForm:endForm(). 
   
   QualitySampleTypeHistoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QualitySampleTypeHistoryDetailsForm:getErrors().  */

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
   
   ASSIGN chrQualitySampleTypeID = get-value("QualitySampleTypeID")
          intSelectedQualitySampleType = INTEGER(chrQualitySampleTypeID)
          chrScrollToQualitySampleTypeRow = STRING(INTEGER(get-value("qualitysampletype_browse_scroll"))) + ";".
   
   /* Process URL values */   
   IF chrQualitySampleTypeID <> "" THEN
      chrQualitySampleTypeRow = 'selectQualitySampleTypeRow(document.getElementById("qualitysampletype_browse_row_' + chrQualitySampleTypeID + '"),"' 
                                                          + chrQualitySampleTypeID +  '");'.
                                                          
   IF get-value('popup_qualitysampletypehistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("qualitysampletypehistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("qualitysampletype_browse").scrollTop=' + chrScrollToQualitySampleTypeRow 
                                                           + chrQualitySampleTypeRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Quality Sample Type Admin".
   ThisPage:FrameTitle = "Quality Sample Type Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Quality SampleType */
   ThisPage:addJavaScript("qualitysampletype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQualitySampleTypeBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pQualitySampleTypeDetails.
   
   RUN pQualitySampleTypeHistoryBrowse.
   
   RUN pQualitySampleTypeHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT QualitySampleTypeBrowseFrame           NO-ERROR.
   DELETE OBJECT QualitySampleTypeBrowse                NO-ERROR.
   DELETE OBJECT QualitySampleTypeBrowseButtons         NO-ERROR.
   DELETE OBJECT QualitySampleTypeDetailsForm           NO-ERROR.
   DELETE OBJECT QualitySampleTypeDetailsButtons        NO-ERROR.
   DELETE OBJECT QualitySampleTypeHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT QualitySampleTypeHistoryDetailsButtons NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundSampleTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundSampleTypeBrowse Procedure 
PROCEDURE pQualitySampleTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "qualitysampletype_details_form"}
   
   QualitySampleTypeBrowse = NEW browseTable("qualitysampletype_browse").
   QualitySampleTypeBrowse:BrowseWidth  = 965.
   QualitySampleTypeBrowse:BrowseHeight = 455.
   QualitySampleTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the QualitySampleType ID as first Column */
   QualitySampleTypeBrowse:insertColumn(fTL("QualitySampleType ID"), 130, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySampleType}
   
   QualitySampleTypeBrowse:insertColumn(fTL("Type Code"),   200, "CHARACTER", "LEFT", FALSE).
   QualitySampleTypeBrowse:insertColumn(fTL("Type Name"),   200, "CHARACTER", "LEFT", FALSE).
   QualitySampleTypeBrowse:insertColumn(fTL("Type Descr"),  210, "CHARACTER", "LEFT", FALSE).
   QualitySampleTypeBrowse:insertColumn(fTL("Active"),      200, "CHARACTER", "LEFT", FALSE).
   
   
   /*Body*/
   QualitySampleTypeBrowse:startBody().
   
   FOR EACH QualitySampleType NO-LOCK: /*idx=ActiveListingSequence*/
   
      QualitySampleTypeBrowse:startRow(QualitySampleType.QualitySampleTypeID, "selectQualitySampleTypeRow(this," + '"' + STRING(QualitySampleType.QualitySampleTypeID) + '"' + ");", "").
      QualitySampleTypeBrowse:insertData(QualitySampleType.QualitySampleTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QualitySampleType}
      
      QualitySampleTypeBrowse:insertData(QualitySampleType.TypeCode,"LEFT").
      QualitySampleTypeBrowse:insertData(QualitySampleType.TypeName,"LEFT").
      QualitySampleTypeBrowse:insertData(QualitySampleType.TypeDescr,"LEFT").
      QualitySampleTypeBrowse:insertData(STRING(QualitySampleType.Active, "Yes/No"),"LEFT").
      
      /* Add hidden fields */
      QualitySampleTypeBrowse:insertHiddenData("QualitySampleTypeVersionID",QualitySampleType.VersionID).
      
      QualitySampleTypeBrowse:endRow().
      
   END. /*FOR EACH QualitySampleType NO-LOCK */
   
   QualitySampleTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleTypeBrowse:getErrors().
   
   /* Create a new frame */
   QualitySampleTypeBrowseFrame = NEW pageFrame().
   QualitySampleTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QualitySampleTypeBrowseFrame:FormAction="dbQualitySampleTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QualitySampleTypeBrowseFrame:formOpen("qualitysampletype_browse_form").
   
   /* Start the Frame Header */
   QualitySampleTypeBrowseFrame:insertSpacer(5).
   QualitySampleTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QualitySampleTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QualitySampleTypeBrowseFrame:frameClose().
   QualitySampleTypeBrowseFrame:insertSpacer(10).
   
   QualitySampleTypeBrowseFrame:insertHiddenField("qualitysampletype_browse_scroll","").
   QualitySampleTypeBrowseFrame:insertHiddenField("QualitySampleTypeID","").
   QualitySampleTypeBrowseFrame:insertHiddenField("QualitySampleTypeVersionID","").
   QualitySampleTypeBrowseFrame:insertHiddenField("form_name","qualitysampletype_browse_form").
   QualitySampleTypeBrowseFrame:insertHiddenField("popup_qualitysampletypehistory_browse","").
   QualitySampleTypeBrowseFrame:insertHiddenField("prog_name","adQualitySampleTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleTypeBrowseFrame}
   
   QualitySampleTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   QualitySampleTypeBrowseButtons = NEW buttonBar().
   QualitySampleTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   QualitySampleTypeBrowseButtons:addButton("qualitysampletype_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewQualitySampleTypeDetails('qualitysampletype_details_form');",
                                         (IF intSelectedQualitySampleType > 0 THEN "" ELSE "Disabled")).
   
   QualitySampleTypeBrowseButtons:addButton("qualitysampletype_browse_form_btn_create",
                                         fTL("Create"),
                                         "createQualitySampleType('qualitysampletype_details_form');",
                                         IF CAN-FIND(FIRST QualitySampleType) THEN "Disabled" ELSE "").
   
   QualitySampleTypeBrowseButtons:addButton("qualitysampletype_browse_form_btn_history",
                                         fTL("History"),
                                         "viewQualitySampleTypeHistory('qualitysampletype_browse_form');",
                                         (IF intSelectedQualitySampleType > 0 THEN "" ELSE "Disabled")).
   
   QualitySampleTypeBrowseButtons:closeBar().  
   QualitySampleTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundSampleTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundSampleTypeDetails Procedure 
PROCEDURE pQualitySampleTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualitysampletype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualitySampleTypeID,TypeCode,TypeName,TypeDescr,Active" 
          chrEditFieldList     = "TypeCode,TypeName,TypeDescr,Active" 
          chrNewFieldList      = "TypeCode,TypeName,TypeDescr,Active" 
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualitySampleTypeDetailsForm = NEW dataForm("qualitysampletype_details_form").
   QualitySampleTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QualitySampleTypeDetailsForm:FormAction = "dbQualitySampleTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualitySampleTypeDetailsForm:FormWidth   = 460.
   QualitySampleTypeDetailsForm:FormHeight  = 200.
   QualitySampleTypeDetailsForm:FormTitle   = "Quality SampleType Details".
   QualitySampleTypeDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QualitySampleTypeDetailsForm:insertPaddingColumn(30).
   QualitySampleTypeDetailsForm:insertColumn(185).
   
   /* Fields */
   QualitySampleTypeDetailsForm:startRow().
   QualitySampleTypeDetailsForm:insertLabel(fTL("Quality SampleType ID")).
   QualitySampleTypeDetailsForm:insertTextField("QualitySampleTypeID", "", 190, TRUE).  
   
   QualitySampleTypeDetailsForm:startRow().
   QualitySampleTypeDetailsForm:insertLabel(fTL("Type Code")).
   QualitySampleTypeDetailsForm:insertTextField("TypeCode", "", 190, TRUE).
   
   QualitySampleTypeDetailsForm:startRow().
   QualitySampleTypeDetailsForm:insertLabel(fTL("Type Name")).
   QualitySampleTypeDetailsForm:insertTextField("TypeName", "", 190, TRUE).
   
   QualitySampleTypeDetailsForm:startRow().
   QualitySampleTypeDetailsForm:insertLabel(fTL("Type Descr")).
   QualitySampleTypeDetailsForm:insertTextAreaField("TypeDescr", "", 190, TRUE).
   
   QualitySampleTypeDetailsForm:startRow().
   QualitySampleTypeDetailsForm:insertLabel(fTL("Active")).
   QualitySampleTypeDetailsForm:insertComboField("Active", "", 190, TRUE).  
   QualitySampleTypeDetailsForm:insertComboPairs("Active", "yes", "Yes").
   QualitySampleTypeDetailsForm:insertComboPairs("Active", "no",  "No").
   
   {webGetOptionalFormFields.i pQualitySampleTypeDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   QualitySampleTypeDetailsForm:insertHiddenField("qualitysampletype_browse_scroll", "").
   QualitySampleTypeDetailsForm:insertHiddenField("form_name", "qualitysampletype_details_form").
   QualitySampleTypeDetailsForm:insertHiddenField("prog_name", "adQualitySampleTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleTypeDetailsForm}
   
   /* Create Button Bar */
   QualitySampleTypeDetailsButtons = NEW buttonBar().
   QualitySampleTypeDetailsButtons:addButton("qualitysampletype_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateQualitySampleType('qualitysampletype_details_form');").
   QualitySampleTypeDetailsButtons:addButton("qualitysampletype_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualitysampletype_details_form_popup');").
   QualitySampleTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleTypeDetailsForm:FormButtons = QualitySampleTypeDetailsButtons.
   
   QualitySampleTypeDetailsForm:endForm(). 
   
   QualitySampleTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QualitySampleTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundSampleTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundSampleTypeDetailsFields Procedure 
PROCEDURE pQualitySampleTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      QualitySampleTypeDetailsForm:startRow().
      QualitySampleTypeDetailsForm:insertLabel(fTL("Field Label")).
      QualitySampleTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*    {adQualitySampleType_qualityconfig_details_form.i}*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundSampleTypeHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundSampleTypeHistoryBrowse Procedure 
PROCEDURE pQualitySampleTypeHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   QualitySampleTypeHistoryBrowseForm           = NEW dataForm("qualitysampletypehistory_browse_form").
   QualitySampleTypeHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QualitySampleTypeHistoryBrowseForm:FormWidth  = 860.
   QualitySampleTypeHistoryBrowseForm:FormHeight = 530.
   QualitySampleTypeHistoryBrowseForm:FormTitle  = fTL("Quality SampleType History").
   QualitySampleTypeHistoryBrowseForm:FormType   = "xxl_large".
   QualitySampleTypeHistoryBrowse                = NEW browseTable("qualitysampletypehistory_browse").
   QualitySampleTypeHistoryBrowse:BrowseWidth    = 840.
   QualitySampleTypeHistoryBrowse:BrowseHeight   = 490.
   
   QualitySampleTypeHistoryBrowse:insertColumn(fTL("Hist ID"),                     125, "INTEGER",   "LEFT", FALSE).
   QualitySampleTypeHistoryBrowse:insertColumn(fTL("Type Code"),                   125, "CHARACTER", "LEFT", FALSE).
   QualitySampleTypeHistoryBrowse:insertColumn(fTL("Type Name"),                   125, "CHARACTER", "LEFT", FALSE).
   QualitySampleTypeHistoryBrowse:insertColumn(fTL("Active"),                       80, "CHARACTER", "LEFT", FALSE).
   QualitySampleTypeHistoryBrowse:insertColumn(fTL("User"),                        150, "CHARACTER", "LEFT", FALSE).
   QualitySampleTypeHistoryBrowse:insertColumn(fTL("Created"),                     150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySampleTypeHistory}
   
   QualitySampleTypeHistoryBrowse:StartBody().
   
   FOR EACH QualitySampleTypeHistory NO-LOCK
      WHERE QualitySampleTypeHistory.QualitySampleTypeID = intSelectedQualitySampleType
         BY QualitySampleTypeHistory.QualitySampleTypeHistoryID:
            
      FIND FIRST GateUser NO-LOCK
         WHERE GateUser.GateUserID = QualitySampleTypeHistory.GateUserID NO-ERROR.
         
      QualitySampleTypeHistoryBrowse:startRow(QualitySampleTypeHistory.QualitySampleTypeHistoryID, 
                                        "selectQualitySampleTypeHistoryRow(this," + '"' + STRING(QualitySampleTypeHistory.QualitySampleTypeHistoryID) + '"' + ");","").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i QualitySampleTypeHistory}
      
      QualitySampleTypeHistoryBrowse:insertData(QualitySampleTypeHistory.QualitySampleTypeHistoryID, "LEFT").
      QualitySampleTypeHistoryBrowse:insertData(QualitySampleTypeHistory.TypeCode, "LEFT").
      QualitySampleTypeHistoryBrowse:insertData(QualitySampleTypeHistory.TypeName, "LEFT").
      QualitySampleTypeHistoryBrowse:insertData(STRING(QualitySampleTypeHistory.Active, "Yes/No"), "LEFT").
      QualitySampleTypeHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "LEFT").
      QualitySampleTypeHistoryBrowse:insertData(fDisplayDate&Time(QualitySampleTypeHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      QualitySampleTypeHistoryBrowse:endRow().
   END. /* FOR EACH QualitySampleTypeHistory */
   
   QualitySampleTypeHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleTypeHistoryBrowse:getErrors().
   QualitySampleTypeHistoryBrowseForm:insertHiddenField("QualitySampleTypeHistoryID", "").
   QualitySampleTypeHistoryBrowseForm:insertHiddenField("popup_qualitysampletypehistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleTypeHistoryBrowseForm}
   
   /* Create Button Bar */
   QualitySampleTypeHistoryBrowseButtons = NEW buttonBar().
      
   QualitySampleTypeHistoryBrowseButtons:addButton("qualitysampletypehistory_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQualitySampleTypeHistoryDetails('qualitysampletypehistory_details_form');",
                                             "Disabled").
                                             
   QualitySampleTypeHistoryBrowseButtons:addButton("qualitysampletypehistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('qualitysampletypehistory_browse_form_popup');").
   
   QualitySampleTypeHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleTypeHistoryBrowseForm:FormBrowse  = QualitySampleTypeHistoryBrowse.
   QualitySampleTypeHistoryBrowseForm:FormButtons = QualitySampleTypeHistoryBrowseButtons.
   QualitySampleTypeHistoryBrowseForm:endForm(). 
   
   QualitySampleTypeHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


