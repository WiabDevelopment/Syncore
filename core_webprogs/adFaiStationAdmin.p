&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adFaiStationAdmin.p 

  Description: ad file for the Fai Station Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Alex Litas

  Created: 11/06/2015

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

DEFINE VARIABLE intSelectedFaiStation           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedFaiStationHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrFaiStationHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFaiStationRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFaiStationRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFaiStationHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFaiStationID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                 AS CHARACTER   NO-UNDO.

/* Fai Station Objects */
DEFINE VARIABLE FaiStationBrowseFrame           AS pageFrame.
DEFINE VARIABLE FaiStationBrowse                AS browseTable.
DEFINE VARIABLE FaiStationBrowseButtons         AS buttonBar.
DEFINE VARIABLE FaiStationDetailsForm           AS dataForm.
DEFINE VARIABLE FaiStationDetailsButtons        AS buttonBar.

/* Fai Station Hisory Objects*/
DEFINE VARIABLE FaiStationHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE FaiStationHistoryBrowse         AS browseTable.
DEFINE VARIABLE FaiStationHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE FaiStationHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE FaiStationHistoryButtons        AS buttonBar.


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

&IF DEFINED(EXCLUDE-pLaserFileReplacementDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLaserFileReplacementDetails Procedure

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pLaserFileReplacementHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLaserFileReplacementHistoryBrowse Procedure

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pLaserFileReplacementHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLaserFileReplacementHistoryDetails Procedure

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
   
   ASSIGN 
      chrFaiStationID                            = get-value("FaiStationID")
      intSelectedFaiStation                      = INTEGER(chrFaiStationID)
      chrScrollToFaiStationRow                   = STRING(INTEGER(get-value("faistation_browse_scroll"))) + ";"
      chrFaiStationHistoryID                     = get-value("FaiStationHistoryID")
      intSelectedFaiStationHistory               = INTEGER(chrFaiStationHistoryID)
      chrScrollToFaiStationHistoryRow            = STRING(INTEGER(get-value("faistationhistory_browse_scroll"))) + ";".
          
   /* Process URL values */
   IF chrFaiStationID <> "" THEN
      chrFaiStationRow = 'selectFaiStationRow(document.getElementById("faistation_browse_row_'
                         + chrFaiStationID + '"),"' + chrFaiStationID +  '");'.
                                                          
   IF get-value('popup_faistationhistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("faistationhistory_browse_form_popup");'.                                                                                                                         
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("faistation_browse").scrollTop=' 
                 + chrScrollToFaiStationRow + chrFaiStationRow + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "FAI Station Admin".
   ThisPage:FrameTitle    = "FAI Station Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("faistationadmin.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pFaiStationBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pFaiStationDetails.
   
   RUN pFaiStationHistoryBrowse.
   
   RUN pFaiStationHistoryDetails.
    
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT FaiStationBrowseFrame           NO-ERROR.
   DELETE OBJECT FaiStationBrowse                NO-ERROR.
   DELETE OBJECT FaiStationBrowseButtons         NO-ERROR.
   DELETE OBJECT FaiStationDetailsForm           NO-ERROR.
   DELETE OBJECT FaiStationDetailsButtons        NO-ERROR.
   
   DELETE OBJECT FaiStationHistoryBrowseForm     NO-ERROR.
   DELETE OBJECT FaiStationHistoryBrowse         NO-ERROR.
   DELETE OBJECT FaiStationHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT FaiStationHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT FaiStationHistoryButtons        NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pFaiStationBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "faistation_details_form"}
   
   FaiStationBrowse               = NEW browseTable("faistation_browse").
   FaiStationBrowse:BrowseWidth   = 965.
   FaiStationBrowse:BrowseHeight  = 455.
   FaiStationBrowse:WebStream     = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   FaiStationBrowse:insertColumn(fTL(" Fai Station ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FaiStation}
   
   FaiStationBrowse:insertColumn(fTL("Station Name"),      245, "CHARACTER", "LEFT", FALSE).
   FaiStationBrowse:insertColumn(fTL("Location"),          200, "DECIMAL",   "LEFT", FALSE).
   FaiStationBrowse:insertColumn(fTL("Post Fai Location"), 200, "DECIMAL",   "LEFT", FALSE).
   FaiStationBrowse:insertColumn(fTL("Active"),            200, "LOGICAL",   "LEFT", FALSE).
   
   /*Body*/
   FaiStationBrowse:startBody().
   
   FOR EACH FaiStation NO-LOCK: /*idx=FaiStationID*/          
      FaiStationBrowse:startRow(FaiStation.FaiStationID, "selectFaiStationRow(this," + '"' 
                               + STRING(FaiStation.FaiStationID) + '"' + ");", "").
                                     
      FaiStationBrowse:insertData(FaiStation.FaiStationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FaiStation}      
 
      FIND FIRST Location WHERE Location.LocationId = FaiStation.LocationID NO-LOCK NO-ERROR. /*idx=GateUserID*/
      FaiStationBrowse:insertData(STRING(FaiStation.StationName),       "LEFT").
      FaiStationBrowse:insertData(Location.LocationRef,                 "LEFT").
      
      FIND FIRST Location WHERE Location.LocationId = FaiStation.PostFaiLocationID NO-LOCK NO-ERROR. /*idx=GateUserID*/
      FaiStationBrowse:insertData(Location.LocationRef,                 "LEFT").
      FaiStationBrowse:insertData(STRING(FaiStation.Active, "Yes/No"),  "LEFT").
            
      /* Add hidden fields */
      FaiStationBrowse:insertHiddenData("FaiStationVersionID", FaiStation.VersionID).
      FaiStationBrowse:endRow().
      
   END. /*FOR EACH FaiStationAdmin NO-LOCK */
   
   FaiStationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FaiStationBrowse:getErrors().
   
   /* Create a new frame */
   FaiStationBrowseFrame = NEW pageFrame().
   FaiStationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FaiStationBrowseFrame:FormAction="dbFaiStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FaiStationBrowseFrame:formOpen("faistation_browse_form").
   
   /* Start the Frame Header */
   FaiStationBrowseFrame:insertSpacer(5).
   FaiStationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FaiStationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   FaiStationBrowseFrame:frameClose().
   FaiStationBrowseFrame:insertSpacer(10).
   
   FaiStationBrowseFrame:insertHiddenField("faistation_browse_scroll","").
   FaiStationBrowseFrame:insertHiddenField("FaiStationID","").
   FaiStationBrowseFrame:insertHiddenField("FaiStationVersionID","").
   FaiStationBrowseFrame:insertHiddenField("form_name","faistation_browse_form").
   FaiStationBrowseFrame:insertHiddenField("popup_faistationhistory_browse","").
   FaiStationBrowseFrame:insertHiddenField("prog_name","adFaiStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FaiStationBrowseFrame}
   
   FaiStationBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   FaiStationBrowseButtons = NEW buttonBar().
   FaiStationBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   FaiStationBrowseButtons:addButton("faistation_browse_form_btn_details",
                                     fTL("Details"),
                                     "viewFaiStationDetails('faistation_details_form');",
                                     (IF intSelectedFaiStation > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
      FaiStationBrowseButtons:addButton("faistation_browse_form_btn_create",
                                        fTL("Create"),
                                        "createFaiStation('faistation_details_form');","").
   END.
   
   FaiStationBrowseButtons:addButton("faistation_browse_form_btn_history",
                                     fTL("History"),
                                     "viewFaiStationHistory('faistationhistory_browse_form');",
                                     (IF intSelectedFaiStation > 0 THEN "" ELSE "Disabled")).  

   FaiStationBrowseButtons:closeBar().  
   FaiStationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pFaiStationDetails :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "faistation_details_form"}
   ASSIGN 
      chrDisplayFieldList  = "FaiStationID,StationName,LocationID,PostFaiLocationID,Active"
      chrEditFieldList     = "StationName,LocationID,PostFaiLocationID,Active"
      chrNewFieldList      = "StationName,LocationID,PostFaiLocationID,Active"
      chrRequiredFieldList = ""
      chrExtraFieldList    = ""
      chrValidateFieldList = "".

   
   FaiStationDetailsForm = NEW dataForm("faistation_details_form").
   FaiStationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FaiStationDetailsForm:FormAction = "dbFaiStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FaiStationDetailsForm:FormWidth   = 350.
   FaiStationDetailsForm:FormHeight  = 210.
   FaiStationDetailsForm:FormTitle   = "FAI Station Admin Details".
   FaiStationDetailsForm:FormType    = "small_wide".
   
   /* Column Layout */
   FaiStationDetailsForm:insertPaddingColumn(30).
   FaiStationDetailsForm:insertColumn(185).
   
   /* Fields */
   FaiStationDetailsForm:startRow().
   FaiStationDetailsForm:insertLabel(fTL("Fai Station ID")).
   FaiStationDetailsForm:insertTextField("FaiStationID", "", 100, TRUE).    
   
   FaiStationDetailsForm:startRow().
   FaiStationDetailsForm:insertLabel(fTL("Station Name")).
   FaiStationDetailsForm:insertTextField("StationName", "", 100, TRUE).  
   
   FaiStationDetailsForm:startRow().
   FaiStationDetailsForm:insertLabel(fTL("Location")).
   FaiStationDetailsForm:insertComboField("LocationID", "", 168, TRUE).
   FOR EACH Location NO-LOCK:
      FaiStationDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
   
   FaiStationDetailsForm:startRow().
   FaiStationDetailsForm:insertLabel(fTL("Post Fai Location")).
   FaiStationDetailsForm:insertComboField("PostFaiLocationID", "", 168, TRUE).
   FOR EACH Location NO-LOCK:
      FaiStationDetailsForm:insertComboPairs("PostFaiLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
   
   FaiStationDetailsForm:startRow().
   FaiStationDetailsForm:insertLabel(fTL("Active")).
   FaiStationDetailsForm:insertComboField("Active", "", 168, TRUE).
   FaiStationDetailsForm:insertComboPairs("Active", "yes", "Yes").    
   FaiStationDetailsForm:insertComboPairs("Active", "no", "No"). 
   
   {webGetOptionalFormFields.i pFaiStationDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FaiStationDetailsForm:insertHiddenField("faistation_browse_scroll", "").
   FaiStationDetailsForm:insertHiddenField("form_name", "faistation_details_form").
   FaiStationDetailsForm:insertHiddenField("prog_name", "adFaiStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FaiStationDetailsForm}
   
   /* Create Button Bar */
   FaiStationDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataUpdates THEN
   DO:
      FaiStationDetailsButtons:addButton("faistation_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateFaiStation('faistation_details_form');").
   END.
   FaiStationDetailsButtons:addButton("faistation_details_form_btn_cancel", 
                                      fTL("Cancel"), 
                                      "cancelUpdate('UserCancelled','process_mode');" + 
                                      "disablePopup('faistation_details_form_popup');").
   FaiStationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FaiStationDetailsForm:FormButtons = FaiStationDetailsButtons.
   
   FaiStationDetailsForm:endForm(). 
   
   FaiStationDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pFaiStationDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            FaiStationDetailsForm:startRow().
            FaiStationDetailsForm:insertLabel(fTL("Field Label")).
            FaiStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pFaiStationHistoryDetails :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "faistationhistory_details_form"}
   ASSIGN 
      chrDisplayFieldList  = "FaiStationHistoryID,StationName,LocationID,PostFaiLocationID," +
                             "Active,OperationTypeID,CreatedDate,GateUserID,CreatedHour,CreatedMins"
      chrEditFieldList     = ""
      chrNewFieldList      = ""
      chrRequiredFieldList = ""
      chrExtraFieldList    = ""
      chrValidateFieldList = "".
   
   FaiStationHistoryDetailsForm = NEW dataForm("faistationhistory_details_form").
   FaiStationHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FaiStationHistoryDetailsForm:FormAction = "dbFaiStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FaiStationHistoryDetailsForm:FormWidth   = 460.
   FaiStationHistoryDetailsForm:FormHeight  = 300.
   FaiStationHistoryDetailsForm:FormTitle   = "FAI Station Admin Details".
   FaiStationHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   FaiStationHistoryDetailsForm:insertPaddingColumn(30).
   FaiStationHistoryDetailsForm:insertColumn(150).
   FaiStationHistoryDetailsForm:insertColumn(125).
   FaiStationHistoryDetailsForm:insertColumn(20).
   FaiStationHistoryDetailsForm:insertColumn(4).
   FaiStationHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   FaiStationHistoryDetailsForm:startRow().
   FaiStationHistoryDetailsForm:insertLabel(fTL("Fai Station History ID")).
   FaiStationHistoryDetailsForm:insertTextField("FaiStationHistoryID", "", 100, TRUE).
   
   FaiStationHistoryDetailsForm:startRow().
   FaiStationHistoryDetailsForm:insertLabel(fTL("Station Name")).
   FaiStationHistoryDetailsForm:insertTextField("StationName", "", 100, TRUE).

   FaiStationHistoryDetailsForm:startRow().
   FaiStationHistoryDetailsForm:insertLabel(fTL("Location")).
   FaiStationHistoryDetailsForm:insertComboField("LocationID", "", 168, TRUE).
   FOR EACH Location NO-LOCK:
      FaiStationHistoryDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
    
   FaiStationHistoryDetailsForm:startRow().
   FaiStationHistoryDetailsForm:insertLabel(fTL("Post Fai Location")).
   FaiStationHistoryDetailsForm:insertComboField("PostFaiLocationID", "", 168, TRUE).
   FOR EACH Location NO-LOCK:
      FaiStationHistoryDetailsForm:insertComboPairs("PostFaiLocationID", STRING(Location.LocationID), Location.LocationRef). 
   END.
    
   FaiStationHistoryDetailsForm:startRow().
   FaiStationHistoryDetailsForm:insertLabel(fTL("Active")).
   FaiStationHistoryDetailsForm:insertComboField("Active", "", 168, TRUE).
   FaiStationHistoryDetailsForm:insertComboPairs("Active", "yes", "Yes").    
   FaiStationHistoryDetailsForm:insertComboPairs("Active", "no", "No"). 
   
   FaiStationHistoryDetailsForm:startRow().                                                                                           
   FaiStationHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   FaiStationHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   FaiStationHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   FaiStationHistoryDetailsForm:insertLabel(":").                                                                                     
   FaiStationHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                                                                                                                          
   FaiStationHistoryDetailsForm:startRow().                                                                                           
   FaiStationHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   FaiStationHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/                                                                                                      
      BY    OperationType.OperationTypeID:                                                                                                  
      FaiStationHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                    OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   FaiStationHistoryDetailsForm:startRow().                                                                                           
   FaiStationHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   FaiStationHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*GateUserID*/                                                                                                        
      BY    GateUser.FullName:                                                                                                              
      FaiStationHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pFaiStationHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FaiStationHistoryDetailsForm:insertHiddenField("faistationhistory_browse_scroll", "").
   FaiStationHistoryDetailsForm:insertHiddenField("form_name", "faistationhistory_details_form").
   FaiStationHistoryDetailsForm:insertHiddenField("prog_name", "adFaiStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FaiStationHistoryDetailsForm}
   
   /* Create Button Bar */
   FaiStationHistoryDetailsButtons = NEW buttonBar().

   FaiStationHistoryDetailsButtons:addButton("faistationhistory_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "disablePopup('faistationhistory_details_form_popup');").
   FaiStationHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FaiStationHistoryDetailsForm:FormButtons = FaiStationHistoryDetailsButtons.
   FaiStationHistoryDetailsForm:endForm(). 
   FaiStationHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pFaiStationHistoryDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            FaiStationDetailsForm:startRow().
            FaiStationDetailsForm:insertLabel(fTL("Field Label")).
            FaiStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pFaiStationHistoryBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   FaiStationHistoryBrowseForm            = NEW dataForm("faistationhistory_browse_form").
   FaiStationHistoryBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   
   /* Setup */
   FaiStationHistoryBrowseForm:FormWidth  = 860.
   FaiStationHistoryBrowseForm:FormHeight = 530.
   FaiStationHistoryBrowseForm:FormTitle  = fTL("FAI Station Admin History").
   FaiStationHistoryBrowseForm:FormType   = "xxl_large".
   FaiStationHistoryBrowse                = NEW browseTable("faistationhistory_browse").
   FaiStationHistoryBrowse:BrowseWidth    = 840.
   FaiStationHistoryBrowse:BrowseHeight   = 490.
   
   FaiStationHistoryBrowse:insertColumn(fTL("History ID"),        100,  "INTEGER",             FALSE).
   FaiStationHistoryBrowse:insertColumn(fTL("Station Name"),      120,  "CHARACTER",   "LEFT", FALSE).
   FaiStationHistoryBrowse:insertColumn(fTL("Operation Type"),    120,  "CHARACTER",   "LEFT", FALSE).
   FaiStationHistoryBrowse:insertColumn(fTL("Active"),            100,  "LOGICAL",     "LEFT", FALSE).
   FaiStationHistoryBrowse:insertColumn(fTL("Location"),          120,  "LOGICAL",     "LEFT", FALSE).
   FaiStationHistoryBrowse:insertColumn(fTL("Post Fai Location"), 120,  "DECIMAL",     "LEFT", FALSE).
   FaiStationHistoryBrowse:insertColumn(fTL("Gate User ID"),      120,  "CHARACTER",   "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FaiStationHistory}
   
   FaiStationHistoryBrowse:StartBody().
   
   FOR EACH FaiStationHistory NO-LOCK /*idx=FaiStationID and FaiStationHistoryID*/
      WHERE FaiStationHistory.FaiStationID = intSelectedFaiStation
      BY    FaiStationHistory.FaiStationHistoryID:

      FIND FIRST GateUser OF FaiStationHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/

      FaiStationHistoryBrowse:startRow (FaiStationHistory.FaiStationHistoryID,
                                        "selectFaiStationHistoryRow(this," + '"' 
                                        + STRING(FaiStationHistory.FaiStationHistoryID)
                                        + '"' + ");", "").
                                        
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i FaiStationHistory}
      
      FIND FIRST Location WHERE Location.LocationId = FaiStationHistory.LocationID NO-LOCK NO-ERROR. /*idx=LocationID*/
      
      FaiStationHistoryBrowse:insertData(FaiStationHistory.FaiStationHistoryID,     "").
      FaiStationHistoryBrowse:insertData(STRING(FaiStationHistory.StationName),     "LEFT").
      FaiStationHistoryBrowse:insertData(STRING(FaiStationHistory.OperationTypeID), "LEFT").
      FaiStationHistoryBrowse:insertData(STRING(FaiStationHistory.Active,"Yes/No"), "LEFT").
      FaiStationHistoryBrowse:insertData(Location.LocationRef,                      "LEFT").
      
      FIND FIRST Location WHERE Location.LocationId = FaiStationHistory.PostFaiLocationID NO-LOCK NO-ERROR. /*idx=LocationID*/
      FaiStationHistoryBrowse:insertData(Location.LocationRef,                      "LEFT").
      FaiStationHistoryBrowse:insertData(STRING(IF AVAILABLE GateUser 
                                                THEN GateUser.FullName ELSE ""),    "LEFT").

      FaiStationHistoryBrowse:endRow().
   END. /* FOR EACH FaiStationHistory */
   
   FaiStationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FaiStationHistoryBrowse:getErrors().
   
   FaiStationHistoryBrowseForm:insertHiddenField("popup_faistationhistory_browse","").
   FaiStationHistoryBrowseForm:insertHiddenField("FaiStationHistoryID","").
      
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FaiStationHistoryBrowseForm}
   
   /* Create Button Bar */
   FaiStationHistoryButtons = NEW buttonBar().
   
   FaiStationHistoryButtons:addButton("faistationhistory_browse_form_btn_details",
                                      fTL("Details"),
                                      "viewFaiStationHistoryDetails('faistationhistory_details_form');",
                                      (IF intSelectedFaiStationHistory > 0 THEN "" ELSE "Disabled")).    
                                                                       
   FaiStationHistoryButtons:addButton("faistationhistory_browse_form_btn_cancel",
                                      fTL("Cancel"),
                                      "disablePopup('faistationhistory_browse_form_popup');").
   FaiStationHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FaiStationHistoryBrowseForm:FormBrowse  = FaiStationHistoryBrowse.
   FaiStationHistoryBrowseForm:FormButtons = FaiStationHistoryButtons.
   FaiStationHistoryBrowseForm:endForm(). 
   
   FaiStationHistoryBrowseForm:displayForm().   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


