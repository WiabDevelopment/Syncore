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

DEFINE VARIABLE intSelectedQualityStation           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQualityStationRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQualityStationRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQualityStationID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQualityStationHistory       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QualityStationBrowseFrame           AS pageFrame.
DEFINE VARIABLE QualityStationBrowse                AS browseTable.
DEFINE VARIABLE QualityStationBrowseButtons         AS buttonBar.
DEFINE VARIABLE QualityStationDetailsForm           AS dataForm.
DEFINE VARIABLE QualityStationDetailsButtons        AS buttonBar.

DEFINE VARIABLE QualityStationHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE QualityStationHistoryBrowse         AS browseTable.
DEFINE VARIABLE QualityStationHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE QualityStationHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QualityStationHistoryDetailsButtons AS buttonBar.

/* Buffers */
DEFINE BUFFER postInspectionLocation FOR Location.


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
   
   ASSIGN chrQualityStationID          = get-value("QualityStationID")
          intSelectedQualityStation    = INTEGER(chrQualityStationID)
          chrScrollToQualityStationRow = STRING(INTEGER(get-value("qualitystation_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQualityStationID <> "" THEN
     chrSelectQualityStationRow = 'selectQualityStationRow(document.getElementById("qualitystation_browse_row_' + chrQualityStationID + '"),"' 
                                                         + chrQualityStationID +  '");'.
   
   IF get-value('popup_qualitystationhistory_browse') = "yes" THEN
      chrPopupQualityStationHistory  = 'enablePopup("qualitystationhistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("qualitystation_browse").scrollTop=' + chrScrollToQualityStationRow 
                                                          + chrSelectQualityStationRow + chrPopupQualityStationHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "QualityStation Admin".
   ThisPage:FrameTitle    = "QualityStation Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("qualitystation.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQualityStationBrowse.
   
   FIND FIRST QualityStation NO-LOCK
      WHERE QualityStation.QualityStationID = intSelectedQualityStation NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQualityStationDetails.
   RUN pQualityStationHistory.
   RUN pQualityStationHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QualityStationBrowseFrame           NO-ERROR.
   DELETE OBJECT QualityStationBrowse                NO-ERROR.
   DELETE OBJECT QualityStationBrowseButtons         NO-ERROR.
   DELETE OBJECT QualityStationDetailsForm           NO-ERROR.
   DELETE OBJECT QualityStationDetailsButtons        NO-ERROR.
   
   DELETE OBJECT QualityStationHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT QualityStationHistoryBrowse            NO-ERROR.
   DELETE OBJECT QualityStationHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT QualityStationHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT QualityStationHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualityStationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualityStationBrowse Procedure 
PROCEDURE pQualityStationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "qualitystation_details_form"}
   
   QualityStationBrowse = NEW browseTable("qualitystation_browse").
   QualityStationBrowse:BrowseWidth  = 965.
   QualityStationBrowse:BrowseHeight = 455.
   QualityStationBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QualityStationBrowse:insertColumn(fTL("Station ID"), 80, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualityStation}
   
   QualityStationBrowse:insertColumn(fTL("Station Name"),            130, "CHARACTER", "left", FALSE).
   QualityStationBrowse:insertColumn(fTL("Location"),                130, "INTEGER", "left",   FALSE).
   QualityStationBrowse:insertColumn(fTL("PostInspection Location"), 140, "INTEGER", "left",   FALSE).
   QualityStationBrowse:insertColumn(fTL("Station Type"),            130, "INTEGER", "left",   FALSE).
   QualityStationBrowse:insertColumn(fTL("Active"),                   70, "LOGICAL",            FALSE).
   
   /*Body*/
   QualityStationBrowse:startBody().
   
   FOR EACH QualityStation NO-LOCK: /*idx=QualityStationID*/
/*      WHERE QualityStation.Active*/
      
      FIND FIRST Location NO-LOCK /* idx=LocationID */
         WHERE Location.LocationID = QualityStation.LocationID NO-ERROR.
         
      FIND FIRST postInspectionLocation NO-LOCK /* idx=LocationID */
         WHERE postInspectionLocation.LocationID = QualityStation.PostQualityLocationID NO-ERROR.   
         
      FIND FIRST QualityStationType NO-LOCK /* idx=QualityStationTypeID */
         WHERE QualityStationType.QualityStationTypeID = QualityStation.QualityStationTypeID NO-ERROR.      
      
      QualityStationBrowse:startRow(QualityStation.QualityStationID, "selectQualityStationRow(this," + '"' + STRING(QualityStation.QualityStationID) 
                                                                  + '"' + ");", "").
      QualityStationBrowse:insertData(QualityStation.QualityStationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QualityStation}
      
      QualityStationBrowse:insertData(QualityStation.StationName, "left").
      QualityStationBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE "") ,"left").
      QualityStationBrowse:insertData((IF AVAILABLE postInspectionLocation THEN postInspectionLocation.LocationRef ELSE "") ,"left").
      QualityStationBrowse:insertData((IF AVAILABLE QualityStationType THEN QualityStationType.TypeName ELSE "") ,"left").
      QualityStationBrowse:insertData(STRING(QualityStation.Active,"Yes/No")).
      
      /* Add hidden fields */
      QualityStationBrowse:insertHiddenData("QualityStationVersionID",QualityStation.VersionID).
      
      QualityStationBrowse:endRow().
      
   END. /*FOR EACH QualityStation NO-LOCK */
   
   QualityStationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualityStationBrowse:getErrors().
   
   /* Create a new frame */
   QualityStationBrowseFrame = NEW pageFrame().
   QualityStationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QualityStationBrowseFrame:FormAction="dbQualityStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QualityStationBrowseFrame:formOpen("qualitystation_browse_form").
   
   /* Start the Frame Header */
   QualityStationBrowseFrame:insertSpacer(5).
   QualityStationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QualityStationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QualityStationBrowseFrame:frameClose().
   QualityStationBrowseFrame:insertSpacer(10).
   
   QualityStationBrowseFrame:insertHiddenField("qualitystation_browse_scroll","").
   QualityStationBrowseFrame:insertHiddenField("QualityStationID","").
   QualityStationBrowseFrame:insertHiddenField("QualityStationVersionID","").
   QualityStationBrowseFrame:insertHiddenField("popup_qualitystationhistory_browse","").
   QualityStationBrowseFrame:insertHiddenField("form_name","qualitystation_browse_form").
   QualityStationBrowseFrame:insertHiddenField("prog_name","adQualityStation.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationBrowseFrame}
   
   QualityStationBrowseFrame:formClose().
   
   /* Create Button Bar */
   QualityStationBrowseButtons = NEW buttonBar().
   QualityStationBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   QualityStationBrowseButtons:addButton("qualitystation_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQualityStationDetails('qualitystation_details_form');",
                                             "Disabled").
   
   QualityStationBrowseButtons:addButton("qualitystation_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQualityStation('qualitystation_details_form');",
                                             "").
                                             
   QualityStationBrowseButtons:addButton("qualitystation_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   QualityStationBrowseButtons:addButton("qualitystation_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteQualityStation();",
                                             (IF intSelectedQualityStation > 0 THEN "" ELSE "Disabled")).
   */
   
   QualityStationBrowseButtons:closeBar().  
   QualityStationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualityStationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualityStationDetails Procedure 
PROCEDURE pQualityStationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualitystation_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualityStationID,LocationID,StationName,OnHoldLocationID,PostQualityLocationID,ReplacedPartLocationID," 
                               + "QualityStationTypeID,Active"
          chrEditFieldList     = "LocationID,OnHoldLocationID,PostQualityLocationID,Active,"
                               + "ReplacedPartLocationID,QualityStationTypeID"
          chrNewFieldList      = "LocationID,StationName,OnHoldLocationID,PostQualityLocationID,ReplacedPartLocationID," 
                               + "QualityStationTypeID,Active" 
          chrRequiredFieldList = "StationName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualityStationDetailsForm = NEW dataForm("qualitystation_details_form").
   QualityStationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QualityStationDetailsForm:FormAction = "dbQualityStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualityStationDetailsForm:FormWidth   = 460.
   QualityStationDetailsForm:FormHeight  = 300.
   QualityStationDetailsForm:FormTitle   = "Quality Station Details".
   QualityStationDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QualityStationDetailsForm:insertPaddingColumn(30).
   QualityStationDetailsForm:insertColumn(150).
   QualityStationDetailsForm:insertColumn(160).
   QualityStationDetailsForm:insertColumn(20).
   QualityStationDetailsForm:insertColumn(4).
   QualityStationDetailsForm:insertColumn(110).
   
   /* Fields */
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel("Station ID").
   QualityStationDetailsForm:insertTextField("QualityStationID", "", 200, TRUE).  
   
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel("Station Name").
   QualityStationDetailsForm:insertTextField("StationName", "", 200, TRUE).  
   
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel("Location").
   QualityStationDetailsForm:insertComboField("LocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "QualityStation"
      AND   LocationType.TypeCode <> "PostQuality" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
   
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel("Post Inspection Location").
   QualityStationDetailsForm:insertComboField("PostQualityLocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PostQuality" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationDetailsForm:insertComboPairs("PostQualityLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.  
   
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel("Replaced Part Location").
   QualityStationDetailsForm:insertComboField("ReplacedPartLocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "ReplacedPartLoc" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationDetailsForm:insertComboPairs("ReplacedPartLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
   
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel("OnHold Location").
   QualityStationDetailsForm:insertComboField("OnHoldLocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "OnHold" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
   
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel("Station Type").
   QualityStationDetailsForm:insertComboField("QualityStationTypeID", "", 200, TRUE).
   FOR EACH QualityStationType NO-LOCK /*idx=QualityStationTypeID*/
      WHERE QualityStationType.Active:
      
      QualityStationDetailsForm:insertComboPairs("QualityStationTypeID", STRING(QualityStationType.QualityStationTypeID), QualityStationType.TypeName). 
   END.  
   
   QualityStationDetailsForm:startRow().
   QualityStationDetailsForm:insertLabel(fTL("Active")). 
   QualityStationDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QualityStationDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualityStationDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQualityStationDetailsFields}
   
   /* Add Hidden Fields*/
   QualityStationDetailsForm:insertHiddenField("qualitystation_browse_scroll", "").
   QualityStationDetailsForm:insertHiddenField("form_name", "qualitystation_details_form").
   QualityStationDetailsForm:insertHiddenField("prog_name", "adQualityStation.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationDetailsForm}
   
   /* Create Button Bar */
   QualityStationDetailsButtons = NEW buttonBar().
   
   QualityStationDetailsButtons:addButton("qualitystation_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQualityStation('qualitystation_details_form');").
   
   QualityStationDetailsButtons:addButton("qualitystation_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualitystation_details_form_popup');").
   
   QualityStationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualityStationDetailsForm:FormButtons = QualityStationDetailsButtons.
   
   QualityStationDetailsForm:endForm(). 
   
   QualityStationDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QualityStationDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualityStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualityStationDetailsFields Procedure 
PROCEDURE pQualityStationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QualityStationDetailsForm:startRow().
         QualityStationDetailsForm:insertLabel(fTL("Field Label")).
         QualityStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualityStationHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualityStationHistory Procedure
PROCEDURE pQualityStationHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "qualitystationhistory_details_form"}
   
   FIND FIRST QualityStation WHERE QualityStation.QualityStationID = intSelectedQualityStation NO-LOCK NO-ERROR.
   
   QualityStationHistoryBrowseForm = NEW dataForm("qualitystationhistory_browse_form").
   QualityStationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QualityStationHistoryBrowseForm:FormWidth   = 850.
   QualityStationHistoryBrowseForm:FormHeight  = 540.
   QualityStationHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE QualityStation THEN " for MaskRuleTypeID: " 
                                                                  + STRING(QualityStation.QualityStationID) ELSE "").
   QualityStationHistoryBrowseForm:FormType    = "xxl_large".
   
   QualityStationHistoryBrowse = NEW browseTable("qualitystationhistory_browse").
   QualityStationHistoryBrowse:BrowseWidth  = 830.
   QualityStationHistoryBrowse:BrowseHeight = 500.
   QualityStationHistoryBrowse:ExcelExport  = TRUE.
   QualityStationHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QualityStationHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualityStationHistory}

   QualityStationHistoryBrowse:insertColumn(fTL("Station Name"),            100, "CHARACTER", "left", FALSE).
   QualityStationHistoryBrowse:insertColumn(fTL("Location"),                100, "INTEGER",   "left", FALSE).
   QualityStationHistoryBrowse:insertColumn(fTL("PostInspection Location"), 150, "INTEGER",   "left", FALSE).
   QualityStationHistoryBrowse:insertColumn(fTL("Station Type"),            100, "INTEGER",   "left", FALSE).
   QualityStationHistoryBrowse:insertColumn(fTL("Active"),                   50, "LOGICAL",           FALSE).
   QualityStationHistoryBrowse:insertColumn(fTL("User"),                    120, "CHARACTER", "left", FALSE).
   QualityStationHistoryBrowse:insertColumn(fTL("Created"),                 120, "CHARACTER", "left", FALSE).
   
   QualityStationHistoryBrowse:StartBody().
   
   IF AVAILABLE QualityStation THEN
   DO:
      /*List the QualityStationHistory*/
      FOR EACH QualityStationHistory NO-LOCK 
         WHERE  QualityStationHistory.QualityStationID = intSelectedQualityStation
         BY QualityStationHistory.QualityStationHistoryID:
         
         FIND FIRST GateUser OF QualityStationHistory NO-LOCK NO-ERROR.
         
         FIND FIRST Location NO-LOCK /* idx=LocationID */
         WHERE Location.LocationID = QualityStation.LocationID NO-ERROR.
         
         FIND FIRST postInspectionLocation NO-LOCK /* idx=LocationID */
            WHERE postInspectionLocation.LocationID = QualityStationHistory.PostQualityLocationID NO-ERROR.   
            
         FIND FIRST QualityStationType NO-LOCK /* idx=QualityStationTypeID */
            WHERE QualityStationType.QualityStationTypeID = QualityStationHistory.QualityStationTypeID NO-ERROR.    
       
         QualityStationHistoryBrowse:startRow(QualityStationHistory.QualityStationHistoryID, "selectHistoryRow(this," + '"' + STRING(QualityStationHistory.QualityStationHistoryID) 
                                                                     + '","qualityStationHistory"' + ");", "").
         QualityStationHistoryBrowse:insertData(QualityStationHistory.QualityStationHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QualityStationHistory}
         
         QualityStationHistoryBrowse:insertData(QualityStationHistory.StationName, "left").
         QualityStationHistoryBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE "") ,"left").
         QualityStationHistoryBrowse:insertData((IF AVAILABLE postInspectionLocation THEN postInspectionLocation.LocationRef ELSE "") ,"left").
         QualityStationHistoryBrowse:insertData((IF AVAILABLE QualityStationType THEN QualityStationType.TypeName ELSE "") ,"left").
         QualityStationHistoryBrowse:insertData(STRING(QualityStationHistory.Active, "Yes/No")).
         QualityStationHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QualityStationHistoryBrowse:insertData(fDisplayDate&Time(QualityStationHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         QualityStationHistoryBrowse:insertHiddendata("QualityStationHistoryID",QualityStationHistory.QualityStationHistoryID).
         
         QualityStationHistoryBrowse:endRow().
      
      END. /* FOR EACH QualityStationHistory NO-LOCK, */
   END. /*IF AVAILABLE QualityStation THEN*/
   
   QualityStationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualityStationHistoryBrowse:getErrors().
   
   QualityStationHistoryBrowseForm:insertHiddenField("QualityStationHistoryID","").
   QualityStationHistoryBrowseForm:insertHiddenField("popup_qualitystationhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationHistoryBrowseForm}
   
   /* Create Button Bar */
   QualityStationHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QualityStationHistoryBrowseButtons:addButton("qualitystationhistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQualityStationHistoryDetails('qualitystationhistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QualityStationHistoryBrowseButtons:addButton("qualitystationhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_qualitystationhistory_browse.xml')").*/
   
   QualityStationHistoryBrowseButtons:addButton("qualitystationhistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('qualitystationhistory_browse_form_popup');").
   
   QualityStationHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualityStationHistoryBrowseForm:FormBrowse  = QualityStationHistoryBrowse.
   QualityStationHistoryBrowseForm:FormButtons = QualityStationHistoryBrowseButtons.
   QualityStationHistoryBrowseForm:endForm(). 
   
   QualityStationHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQualityStationHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualityStationHistoryDetails Procedure
PROCEDURE pQualityStationHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "qualitystationhistory_details_form"}
   
   chrDisplayFieldList  = "QualityStationHistoryID,QualityStationID,LocationID,StationName,OnHoldLocationID,PostQualityLocationID,"
                        + "Active,CreatedDate,CreatedHour,CreatedMins,GateUserID,ReplacedPartLocationID,QualityStationTypeID".
                             
   
   QualityStationHistoryDetailsForm = NEW dataForm("qualitystationhistory_details_form").
   QualityStationHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QualityStationHistoryDetailsForm:FormWidth   = 545.
   QualityStationHistoryDetailsForm:FormHeight  = 440.
   QualityStationHistoryDetailsForm:FormTitle   = "Quality Station History Details".
   QualityStationHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QualityStationHistoryDetailsForm:insertPaddingColumn(40).
   QualityStationHistoryDetailsForm:insertColumn(150).
   QualityStationHistoryDetailsForm:insertColumn(120).
   QualityStationHistoryDetailsForm:insertColumn(20).
   QualityStationHistoryDetailsForm:insertColumn(4).
   QualityStationHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel(fTL("History ID")).
   QualityStationHistoryDetailsForm:insertTextField("QualityStationHistoryID", "", 200, TRUE).  
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("Station ID").
   QualityStationHistoryDetailsForm:insertTextField("QualityStationID", "", 200, TRUE).  
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("Station Name").
   QualityStationHistoryDetailsForm:insertTextField("StationName", "", 200, TRUE).  
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("Location").
   QualityStationHistoryDetailsForm:insertComboField("LocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "QualityStation"
      AND   LocationType.TypeCode <> "PostQuality" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationHistoryDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("Post Inspection Location").
   QualityStationHistoryDetailsForm:insertComboField("PostQualityLocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PostQuality" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationHistoryDetailsForm:insertComboPairs("PostQualityLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.  
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("Replaced Part Location").
   QualityStationHistoryDetailsForm:insertComboField("ReplacedPartLocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "ReplacedPartLoc" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationHistoryDetailsForm:insertComboPairs("ReplacedPartLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("Station Type").
   QualityStationHistoryDetailsForm:insertComboField("QualityStationTypeID", "", 200, TRUE).
   FOR EACH QualityStationType NO-LOCK /*idx=QualityStationTypeID*/
      WHERE QualityStationType.Active:
      
      QualityStationHistoryDetailsForm:insertComboPairs("QualityStationTypeID", STRING(QualityStationType.QualityStationTypeID), QualityStationType.TypeName). 
   END.
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("OnHold Location").
   QualityStationHistoryDetailsForm:insertComboField("OnHoldLocationID", "", 200, TRUE).
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "OnHold" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      QualityStationHistoryDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.  
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel(fTL("Active")). 
   QualityStationHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QualityStationHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualityStationHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").  
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("User").
   QualityStationHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QualityStationHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QualityStationHistoryDetailsForm:startRow().
   QualityStationHistoryDetailsForm:insertLabel("Created").
   QualityStationHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QualityStationHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QualityStationHistoryDetailsForm:insertLabel(":").
   QualityStationHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QualityStationHistoryDetailsForm:insertHiddenField("qualitystation_browse_scroll","").
   QualityStationHistoryDetailsForm:insertHiddenField("popup_qualitystationhistory_browse", "").
   QualityStationHistoryDetailsForm:insertHiddenField("QualityStationHistoryID","").
   QualityStationHistoryDetailsForm:insertHiddenField("form_name","qualitystationhistory_details_form").
   QualityStationHistoryDetailsForm:insertHiddenField("prog_name","adQualityStation.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityStationHistoryDetailsForm}
   
   /* Create Button Bar */
   QualityStationHistoryDetailsButtons = NEW buttonBar().
   
   QualityStationHistoryDetailsButtons:addButton("qualitystationhistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('qualitystationhistory_details_form_popup');").
                                        
   QualityStationHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QualityStationHistoryDetailsForm:FormButtons = QualityStationHistoryDetailsButtons.
   
   QualityStationHistoryDetailsForm:endForm(). 
   QualityStationHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

