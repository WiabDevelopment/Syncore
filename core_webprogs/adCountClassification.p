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

  Created: 02/05/2016

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

DEFINE VARIABLE intSelectedCountClassification           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectCountClassificationRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToCountClassificationRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCountClassificationID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCountClassificationHist       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE CountClassificationBrowseFrame           AS pageFrame.
DEFINE VARIABLE CountClassificationBrowse                AS browseTable.
DEFINE VARIABLE CountClassificationBrowseButtons         AS buttonBar.
DEFINE VARIABLE CountClassificationDetailsForm           AS dataForm.
DEFINE VARIABLE CountClassificationDetailsButtons        AS buttonBar.

DEFINE VARIABLE CountClassificationHistBrowseForm     AS dataForm.   
DEFINE VARIABLE CountClassificationHistBrowse         AS browseTable.
DEFINE VARIABLE CountClassificationHistBrowseButtons  AS buttonBar.

DEFINE VARIABLE CountClassificationHistDetailsForm    AS dataForm.
DEFINE VARIABLE CountClassificationHistDetailsButtons AS buttonBar.



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
   
   ASSIGN chrCountClassificationID          = get-value("CountClassificationID")
          intSelectedCountClassification    = INTEGER(chrCountClassificationID)
          chrScrollToCountClassificationRow = STRING(INTEGER(get-value("countclassification_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrCountClassificationID <> "" THEN
     chrSelectCountClassificationRow = 'selectCountClassificationRow(document.getElementById("countclassification_browse_row_' + chrCountClassificationID + '"),"' 
                                                         + chrCountClassificationID +  '");'.
   
   IF get-value('popup_countclassificationhist_browse') = "yes" THEN
      chrPopupCountClassificationHist  = 'enablePopup("countclassificationhist_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("countclassification_browse").scrollTop=' + chrScrollToCountClassificationRow 
                                                          + chrSelectCountClassificationRow + chrPopupCountClassificationHist.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "CountClassification Admin".
   ThisPage:FrameTitle    = "CountClassification Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("countclassification.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pCountClassificationBrowse.
   
   FIND FIRST CountClassification NO-LOCK /* idx=CountClassificationID */
      WHERE CountClassification.CountClassificationID = intSelectedCountClassification NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pCountClassificationDetails.
   RUN pCountClassificationHist.
   RUN pCountClassificationHistDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT CountClassificationBrowseFrame           NO-ERROR.
   DELETE OBJECT CountClassificationBrowse                NO-ERROR.
   DELETE OBJECT CountClassificationBrowseButtons         NO-ERROR.
   DELETE OBJECT CountClassificationDetailsForm           NO-ERROR.
   DELETE OBJECT CountClassificationDetailsButtons        NO-ERROR.
   
   DELETE OBJECT CountClassificationHistBrowseForm     NO-ERROR.   
   DELETE OBJECT CountClassificationHistBrowse         NO-ERROR.
   DELETE OBJECT CountClassificationHistBrowseButtons  NO-ERROR.
   
   DELETE OBJECT CountClassificationHistDetailsForm    NO-ERROR.
   DELETE OBJECT CountClassificationHistDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountClassificationBrowse Procedure 
PROCEDURE pCountClassificationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "countclassification_details_form"}
   
   CountClassificationBrowse = NEW browseTable("countclassification_browse").
   CountClassificationBrowse:BrowseWidth  = 965.
   CountClassificationBrowse:BrowseHeight = 455.
   CountClassificationBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountClassificationBrowse:insertColumn(fTL("ClassificationID"),     120, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountClassification}
   
   CountClassificationBrowse:insertColumn(fTL("List Seq"),              60, "INTEGER", FALSE).
   CountClassificationBrowse:insertColumn(fTL("Classification Code"),  120, "CHARACTER", "left", FALSE).
   CountClassificationBrowse:insertColumn(fTL("Classification Name"),  120, "CHARACTER", "left", FALSE).
   CountClassificationBrowse:insertColumn(fTL("Classification Descr"), 150, "CHARACTER", "left", FALSE).
   CountClassificationBrowse:insertColumn(fTL("Active"),                50, "LOGICAL", FALSE).
   
   /*Body*/
   CountClassificationBrowse:startBody().
   
   FOR EACH CountClassification NO-LOCK /*idx=ActiveListingSequence*/
      WHERE CountClassification.Active
      BY    CountClassification.ListingSequence:
      
      CountClassificationBrowse:startRow(CountClassification.CountClassificationID, "selectCountClassificationRow(this," + '"' + STRING(CountClassification.CountClassificationID) 
                                                                  + '"' + ");", "").
                                                                  
      CountClassificationBrowse:insertData(CountClassification.CountClassificationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountClassification}
      
      CountClassificationBrowse:insertData(STRING(CountClassification.ListingSequence)).
      CountClassificationBrowse:insertData(CountClassification.ClassificationCode, "left").
      CountClassificationBrowse:insertData(CountClassification.ClassificationName, "left").
      CountClassificationBrowse:insertData(CountClassification.ClassificationDescr, "left").
      CountClassificationBrowse:insertData(STRING(CountClassification.Active,"Yes/No")).
      
      /* Add hidden fields */
      CountClassificationBrowse:insertHiddenData("CountClassificationID",CountClassification.CountClassificationID).
      CountClassificationBrowse:insertHiddenData("CountClassificationVersionID",CountClassification.VersionID).
      
      CountClassificationBrowse:endRow().
      
   END. /*FOR EACH CountClassification NO-LOCK */
   
   CountClassificationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountClassificationBrowse:getErrors().
   
   /* Create a new frame */
   CountClassificationBrowseFrame = NEW pageFrame().
   CountClassificationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CountClassificationBrowseFrame:FormAction="dbCountClassificationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CountClassificationBrowseFrame:formOpen("countclassification_browse_form").
   
   /* Start the Frame Header */
   CountClassificationBrowseFrame:insertSpacer(5).
   CountClassificationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CountClassificationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CountClassificationBrowseFrame:frameClose().
   CountClassificationBrowseFrame:insertSpacer(10).
   
   CountClassificationBrowseFrame:insertHiddenField("countclassification_browse_scroll","").
   CountClassificationBrowseFrame:insertHiddenField("CountClassificationID","").
   CountClassificationBrowseFrame:insertHiddenField("CountClassificationVersionID","").
   CountClassificationBrowseFrame:insertHiddenField("popup_countclassificationhist_browse","").
   CountClassificationBrowseFrame:insertHiddenField("form_name","countclassification_browse_form").
   CountClassificationBrowseFrame:insertHiddenField("prog_name","adCountClassification.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountClassificationBrowseFrame}
   
   CountClassificationBrowseFrame:formClose().
   
   /* Create Button Bar */
   CountClassificationBrowseButtons = NEW buttonBar().
   CountClassificationBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   CountClassificationBrowseButtons:addButton("countclassification_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewCountClassificationDetails('countclassification_details_form');",
                                             (IF intSelectedCountClassification > 0 THEN "" ELSE "Disabled")).
   
   CountClassificationBrowseButtons:addButton("countclassification_browse_form_btn_create",
                                             fTL("Create"),
                                             "createCountClassification('countclassification_details_form');",
                                             "").
                                             
   CountClassificationBrowseButtons:addButton("countclassification_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   CountClassificationBrowseButtons:addButton("countclassification_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteCountClassification();",
                                             (IF intSelectedCountClassification > 0 THEN "" ELSE "Disabled")).
   */
   
   CountClassificationBrowseButtons:closeBar().  
   CountClassificationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountClassificationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountClassificationDetails Procedure 
PROCEDURE pCountClassificationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "countclassification_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountClassificationID,ListingSequence,ClassificationCode,ClassificationName,ClassificationDescr,CaseSensitive,Active" 
          chrEditFieldList     = "ListingSequence,ClassificationName,ClassificationDescr,CaseSensitive,Active" 
          chrNewFieldList      = "ClassificationCode,ListingSequence,ClassificationName,ClassificationDescr,CaseSensitive,Active" 
          chrRequiredFieldList = "ClassificationCode,ClassificationName,ClassificationDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CountClassificationDetailsForm = NEW dataForm("countclassification_details_form").
   CountClassificationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountClassificationDetailsForm:FormAction = "dbCountClassificationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountClassificationDetailsForm:FormWidth   = 580.
   CountClassificationDetailsForm:FormHeight  = 420.
   CountClassificationDetailsForm:FormTitle   = "CountClassification Details".
   CountClassificationDetailsForm:FormType    = "large".
   
   /* Column Layout */
   CountClassificationDetailsForm:insertPaddingColumn(30).
   CountClassificationDetailsForm:insertColumn(150).
   CountClassificationDetailsForm:insertColumn(160).
   CountClassificationDetailsForm:insertColumn(20).
   CountClassificationDetailsForm:insertColumn(4).
   CountClassificationDetailsForm:insertColumn(110).
   
   /* Fields */
   CountClassificationDetailsForm:startRow().
   CountClassificationDetailsForm:insertLabel("Classification ID").
   CountClassificationDetailsForm:insertTextField("CountClassificationID", "", 200, TRUE).  
   
   CountClassificationDetailsForm:startRow().
   CountClassificationDetailsForm:insertLabel("Listing Seq").
   CountClassificationDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).  
   
   CountClassificationDetailsForm:startRow().
   CountClassificationDetailsForm:insertLabel("Classification Code").
   CountClassificationDetailsForm:insertTextField("ClassificationCode", "", 200, TRUE).  
   
   CountClassificationDetailsForm:startRow().
   CountClassificationDetailsForm:insertLabel("Classificatione Name").
   CountClassificationDetailsForm:insertTextField("ClassificationName", "", 200, TRUE).
   
   CountClassificationDetailsForm:startRow().
   CountClassificationDetailsForm:insertLabel("Classification Descr").
   CountClassificationDetailsForm:insertTextField("ClassificationDescr", "", 200, TRUE).  
   
   CountClassificationDetailsForm:startRow().
   CountClassificationDetailsForm:insertLabel(fTL("Active")). 
   CountClassificationDetailsForm:insertComboField("Active", "", 200, TRUE).  
   CountClassificationDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountClassificationDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCountClassificationDetailsFields}
   
   /* Add Hidden Fields*/
   CountClassificationDetailsForm:insertHiddenField("countclassification_browse_scroll", "").
   CountClassificationDetailsForm:insertHiddenField("form_name", "countclassification_details_form").
   CountClassificationDetailsForm:insertHiddenField("prog_name", "adCountClassification.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountClassificationDetailsForm}
   
   /* Create Button Bar */
   CountClassificationDetailsButtons = NEW buttonBar().
   
   CountClassificationDetailsButtons:addButton("countclassification_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateCountClassification('countclassification_details_form');").
   
   CountClassificationDetailsButtons:addButton("countclassification_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('countclassification_details_form_popup');").
   
   CountClassificationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountClassificationDetailsForm:FormButtons = CountClassificationDetailsButtons.
   
   CountClassificationDetailsForm:endForm(). 
   
   CountClassificationDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountClassificationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountClassificationDetailsFields Procedure 
PROCEDURE pCountClassificationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         CountClassificationDetailsForm:startRow().
         CountClassificationDetailsForm:insertLabel(fTL("Field Label")).
         CountClassificationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountClassificationHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountClassificationHistory Procedure
PROCEDURE pCountClassificationHist:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "countclassificationhist_details_form"}
   
   FIND FIRST CountClassification WHERE CountClassification.CountClassificationID = intSelectedCountClassification NO-LOCK NO-ERROR.
   
   CountClassificationHistBrowseForm = NEW dataForm("countclassificationhist_browse_form").
   CountClassificationHistBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CountClassificationHistBrowseForm:FormWidth   = 850.
   CountClassificationHistBrowseForm:FormHeight  = 540.
   CountClassificationHistBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE CountClassification THEN " for Classification: " 
                                                                  + CountClassification.CountClassificationName ELSE "").
   CountClassificationHistBrowseForm:FormType    = "xxl_large".
   
   CountClassificationHistBrowse = NEW browseTable("countclassificationhist_browse").
   CountClassificationHistBrowse:BrowseWidth  = 830.
   CountClassificationHistBrowse:BrowseHeight = 500.
   CountClassificationHistBrowse:ExcelExport  = TRUE.
   CountClassificationHistBrowse:SessionID    = intGblSessionID.
   
   
   CountClassificationHistBrowse:insertColumn(fTL("HistoryID"),             70, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountClassificationHist}

   CountClassificationHistBrowse:insertColumn(fTL("Listing Seq"),           70, "INTEGER",           FALSE).   
   CountClassificationHistBrowse:insertColumn(fTL("Classification Code"),  120, "CHARACTER", "left", FALSE).
   CountClassificationHistBrowse:insertColumn(fTL("Classification Name"),  120, "CHARACTER", "left", FALSE).
   CountClassificationHistBrowse:insertColumn(fTL("Classification Descr"), 140, "CHARACTER", "left", FALSE).
   CountClassificationHistBrowse:insertColumn(fTL("Active"),                50, "LOGICAL",           FALSE).
   CountClassificationHistBrowse:insertColumn(fTL("User"),                 120, "CHARACTER", "left", FALSE).
   CountClassificationHistBrowse:insertColumn(fTL("Created"),              120, "CHARACTER", "left", FALSE).
   
   CountClassificationHistBrowse:StartBody().
   
   IF AVAILABLE CountClassification THEN
   DO:
      /*List the CountClassificationHist*/
      FOR EACH CountClassificationHist NO-LOCK 
         WHERE CountClassificationHist.CountClassificationID = intSelectedCountClassification
         BY CountClassificationHist.CountClassificationHistID:
         
         FIND FIRST GateUser NO-LOCK /* idx=GateUserID */
         WHERE GateUser.GateUserID = CountClassificationHist.GateUserID NO-ERROR.
       
         CountClassificationHistBrowse:startRow(CountClassificationHist.CountClassificationHistID, "selectHistoryRow(this," + '"' + STRING(CountClassificationHist.CountClassificationHistID) 
                                                                     + '","countClassificationHist"' + ");", "").
         CountClassificationHistBrowse:insertData(CountClassificationHist.CountClassificationHistID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CountClassificationHist}
         
         CountClassificationHistBrowse:insertData(CountClassificationHist.ListingSequence, "").
         CountClassificationHistBrowse:insertData(CountClassificationHist.ClassificationCode, "left").
         CountClassificationHistBrowse:insertData(CountClassificationHist.ClassificationName, "left").
         CountClassificationHistBrowse:insertData(CountClassificationHist.ClassificationDescr, "left").                            
         CountClassificationHistBrowse:insertData(STRING(CountClassificationHist.Active, "Yes/No")).
         CountClassificationHistBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         CountClassificationHistBrowse:insertData(fDisplayDate&Time(CountClassificationHist.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         CountClassificationHistBrowse:insertHiddendata("CountClassificationHistID",CountClassificationHist.CountClassificationHistID).
         
         CountClassificationHistBrowse:endRow().
      
      END. /* FOR EACH CountClassificationHist OF SerialMaskType NO-LOCK, */
   END. /*IF AVAILABLE SerialMaskType THEN*/
   
   CountClassificationHistBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountClassificationHistBrowse:getErrors().
   
   CountClassificationHistBrowseForm:insertHiddenField("CountClassificationHistID","").
   CountClassificationHistBrowseForm:insertHiddenField("popup_countclassificationhist_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountClassificationHistBrowseForm}
   
   /* Create Button Bar */
   CountClassificationHistBrowseButtons = NEW buttonBar().                                                 
   
   CountClassificationHistBrowseButtons:addButton("countclassificationhist_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewCountClassificationHistDetails('countclassificationhist_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   CountClassificationHistBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_countclassificationhist_browse.xml')").*/
   
   CountClassificationHistBrowseButtons:addButton("countclassificationhist_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('countclassificationhist_browse_form_popup');").
   
   CountClassificationHistBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountClassificationHistBrowseForm:FormBrowse  = CountClassificationHistBrowse.
   CountClassificationHistBrowseForm:FormButtons = CountClassificationHistBrowseButtons.
   CountClassificationHistBrowseForm:endForm(). 
   
   CountClassificationHistBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pCountClassificationHistDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountClassificationHistDetails Procedure
PROCEDURE pCountClassificationHistDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "countclassificationhist_details_form"}
   
   chrDisplayFieldList  = "CountClassificationHistID,CountClassificationID,ClassificationCode,ClassificationName,ClassificationDescr"
                        + ",ListingSequence,Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   CountClassificationHistDetailsForm = NEW dataForm("countclassificationhist_details_form").
   CountClassificationHistDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   CountClassificationHistDetailsForm:FormWidth   = 545.
   CountClassificationHistDetailsForm:FormHeight  = 440.
   CountClassificationHistDetailsForm:FormTitle   = "Count Classification History Details".
   CountClassificationHistDetailsForm:FormType    = "large".
   
   /* Column Layout */
   CountClassificationHistDetailsForm:insertPaddingColumn(40).
   CountClassificationHistDetailsForm:insertColumn(110).
   CountClassificationHistDetailsForm:insertColumn(120).
   CountClassificationHistDetailsForm:insertColumn(20).
   CountClassificationHistDetailsForm:insertColumn(4).
   CountClassificationHistDetailsForm:insertColumn(40).  
   
   /* Fields */
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel(fTL("HistoryID")).
   CountClassificationHistDetailsForm:insertTextField("CountClassificationHistID", "", 200, TRUE).    
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel(fTL("CountClassificationID")).
   CountClassificationHistDetailsForm:insertTextField("CountClassificationID", "", 92000, TRUE).
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel(fTL("ListingSequence")).
   CountClassificationHistDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel(fTL("Classification Code")).
   CountClassificationHistDetailsForm:insertTextField("ClassificationCode", "", 200, TRUE).
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel(fTL("Classification Name")).
   CountClassificationHistDetailsForm:insertTextField("ClassificationName", "", 200, TRUE).
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel(fTL("Classification Descr")).
   CountClassificationHistDetailsForm:insertTextField("ClassificationDescr", "", 200, TRUE).
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel(fTL("Active")).
   CountClassificationHistDetailsForm:insertComboField("Active", "", 200, TRUE).
   CountClassificationHistDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountClassificationHistDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel("User").
   CountClassificationHistDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      CountClassificationHistDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   CountClassificationHistDetailsForm:startRow().
   CountClassificationHistDetailsForm:insertLabel("Created").
   CountClassificationHistDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   CountClassificationHistDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   CountClassificationHistDetailsForm:insertLabel(":").
   CountClassificationHistDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   CountClassificationHistDetailsForm:insertHiddenField("countclassification_browse_scroll","").
   CountClassificationHistDetailsForm:insertHiddenField("popup_countclassificationhist_browse", "").
   CountClassificationHistDetailsForm:insertHiddenField("CountClassificationHistID","").
   CountClassificationHistDetailsForm:insertHiddenField("form_name","countclassificationhist_details_form").
   CountClassificationHistDetailsForm:insertHiddenField("prog_name","adCountClassification.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountClassificationHistDetailsForm}
   
   /* Create Button Bar */
   CountClassificationHistDetailsButtons = NEW buttonBar().
   
   CountClassificationHistDetailsButtons:addButton("countclassificationhist_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('countclassificationhist_details_form_popup');").
                                        
   CountClassificationHistDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CountClassificationHistDetailsForm:FormButtons = CountClassificationHistDetailsButtons.
   
   CountClassificationHistDetailsForm:endForm(). 
   CountClassificationHistDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

