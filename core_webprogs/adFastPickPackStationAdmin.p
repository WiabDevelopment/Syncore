&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adFastPickPackStationAdmin.p 

  Description: ad file for the FastPickPack Station Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Nick Diessner

  Created: 27/03/2015

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
DEFINE VARIABLE intSelectedFastPickPackStation           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedFastPickPackStationHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrFastPickPackStationHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFastPickPackStationRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFastPickPackStationRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToFastPickPackStationHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFastPickPackStationID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                          AS CHARACTER   NO-UNDO.

/* Buffers */
/* Objects */
DEFINE VARIABLE FastPickPackStationBrowseFrame           AS pageFrame.
DEFINE VARIABLE FastPickPackStationBrowse                AS browseTable.
DEFINE VARIABLE FastPickPackStationBrowseButtons         AS buttonBar.
DEFINE VARIABLE FastPickPackStationDetailsForm           AS dataForm.
DEFINE VARIABLE FastPickPackStationDetailsButtons        AS buttonBar.
DEFINE VARIABLE FastPickPackStationHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE FastPickPackStationHistoryBrowse         AS browseTable.
DEFINE VARIABLE FastPickPackStationHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE FastPickPackStationHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE FastPickPackStationHistoryButtons        AS buttonBar.


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
   
   ASSIGN chrFastPickPackStationID                 = get-value("FastPickPackStationID")
          intSelectedFastPickPackStation           = INTEGER(chrFastPickPackStationID)
          chrScrollToFastPickPackStationRow        = STRING(INTEGER(get-value("fastpickpackstation_browse_scroll"))) + ";"
          chrFastPickPackStationHistoryID          = get-value("FastPickPackStationHistoryID")
          intSelectedFastPickPackStationHistory    = INTEGER(chrFastPickPackStationHistoryID)
          chrScrollToFastPickPackStationHistoryRow = STRING(INTEGER(get-value("fastpickpackstationhistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrFastPickPackStationID <> "" THEN
      chrFastPickPackStationRow = 'selectFastPickPackStationRow(document.getElementById("fastpickpackstation_browse_row_' + chrFastPickPackStationID + '"),"' 
                                                          + chrFastPickPackStationID +  '");'.
                                                          
   IF get-value('popup_fastpickpackstationhistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("fastpickpackstationhistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("fastpickpackstation_browse").scrollTop=' + chrScrollToFastPickPackStationRow 
                                                           + chrFastPickPackStationRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "FastPickPack Station Admin".
   ThisPage:FrameTitle = "FastPickPack Station Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("fastpickpackstation.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pFastPickPackStationBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pFastPickPackStationDetails.
   
   RUN pFastPickPackStationHistoryBrowse.
   
   RUN pFastPickPackStationHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT FastPickPackStationBrowseFrame        NO-ERROR.
   DELETE OBJECT FastPickPackStationBrowse             NO-ERROR.
   DELETE OBJECT FastPickPackStationBrowseButtons      NO-ERROR.
   DELETE OBJECT FastPickPackStationDetailsForm        NO-ERROR.
   DELETE OBJECT FastPickPackStationDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pFastPickPackStationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "fastpickpackstation_details_form"}
   
   FastPickPackStationBrowse = NEW browseTable("fastpickpackstation_browse").
   FastPickPackStationBrowse:BrowseWidth  = 965.
   FastPickPackStationBrowse:BrowseHeight = 455.
   FastPickPackStationBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   FastPickPackStationBrowse:insertColumn(fTL("Station ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FastPickPackStation}
   
   FastPickPackStationBrowse:insertColumn(fTL("Station Name"), 200, "CHARACTER", "LEFT", FALSE). 
   FastPickPackStationBrowse:insertColumn(fTL("Location"),     235, "CHARACTER", "LEFT", FALSE).   
   FastPickPackStationBrowse:insertColumn(fTL("Active"),       100, "LOGICAL",   "LEFT", FALSE). 
   
   
   
   /*Body*/
   FastPickPackStationBrowse:startBody().
   
   FOR EACH FastPickPackStation NO-LOCK /*idx=ActiveStationName*/
      WHERE FastPickPackStation.Active
      BY    FastPickPackStation.FastPickPackStationID: 
         
      FIND FIRST Location OF FastPickPackStation NO-LOCK NO-ERROR.
            
      FastPickPackStationBrowse:startRow(FastPickPackStation.FastPickPackStationID, "selectFastPickPackStationRow(this," + '"' 
                                          + STRING(FastPickPackStation.FastPickPackStationID) + '"' + ");", "").
                                          
      FastPickPackStationBrowse:insertData(FastPickPackStation.FastPickPackStationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FastPickPackStation}      

      FastPickPackStationBrowse:insertData(FastPickPackStation.StationName,"LEFT"). 
      FastPickPackStationBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE "") ,"LEFT"). 
      FastPickPackStationBrowse:insertData(STRING(FastPickPackStation.Active, "Yes/No"),"LEFT").
            
      /* Add hidden fields */
      FastPickPackStationBrowse:insertHiddenData("FastPickPackStationID",FastPickPackStation.FastPickPackStationID).      
      FastPickPackStationBrowse:insertHiddenData("FastPickPackStationVersionID",FastPickPackStation.VersionID).
      
      FastPickPackStationBrowse:endRow().
      
   END. /*FOR EACH FastPickPackStation NO-LOCK */
   
   FastPickPackStationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FastPickPackStationBrowse:getErrors().
   
   /* Create a new frame */
   FastPickPackStationBrowseFrame = NEW pageFrame().
   FastPickPackStationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FastPickPackStationBrowseFrame:FormAction="dbFastPickPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FastPickPackStationBrowseFrame:formOpen("fastpickpackstation_browse_form").
   
   /* Start the Frame Header */
   FastPickPackStationBrowseFrame:insertSpacer(5).
   FastPickPackStationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FastPickPackStationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   FastPickPackStationBrowseFrame:frameClose().
   FastPickPackStationBrowseFrame:insertSpacer(10).
   
   FastPickPackStationBrowseFrame:insertHiddenField("fastpickpackstation_browse_scroll","").
   FastPickPackStationBrowseFrame:insertHiddenField("FastPickPackStationID","").
   FastPickPackStationBrowseFrame:insertHiddenField("FastPickPackStationVersionID","").
   FastPickPackStationBrowseFrame:insertHiddenField("form_name","fastpickpackstation_browse_form").
   FastPickPackStationBrowseFrame:insertHiddenField("popup_fastpickpackstationhistory_browse","").
   FastPickPackStationBrowseFrame:insertHiddenField("prog_name","adFastPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackStationBrowseFrame}
   
   FastPickPackStationBrowseFrame:formClose().
   
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   FastPickPackStationBrowseButtons = NEW buttonBar().
   FastPickPackStationBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   FastPickPackStationBrowseButtons:addButton("fastpickpackstation_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewFastPickPackStationDetails('fastpickpackstation_details_form');",
                                             (IF intSelectedFastPickPackStation > 0 THEN "" ELSE "Disabled")).
  
   IF NOT logPreventDataCreates THEN
   DO:  
   FastPickPackStationBrowseButtons:addButton("fastpickpackstation_browse_form_btn_create",
                                             fTL("Create"),
                                             "createFastPickPackStation('fastpickpackstation_details_form');",
                                             "").
   END.                                         
   
   FastPickPackStationBrowseButtons:addButton("fastpickpackstation_browse_form_btn_history",
                                             fTL("History"),
                                             "viewFastPickPackStationHistory('fastpickpackstation_browse_form');",
                                             (IF intSelectedFastPickPackStation > 0 THEN "" ELSE "Disabled")).
   
   FastPickPackStationBrowseButtons:closeBar().  
   FastPickPackStationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pFastPickPackStationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "fastpickpackstation_details_form"}
   ASSIGN chrDisplayFieldList  = "FastPickPackStationID,LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active"
          chrEditFieldList     = "LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active"
          chrNewFieldList      = "LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active"
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   
   FastPickPackStationDetailsForm = NEW dataForm("fastpickpackstation_details_form").
   FastPickPackStationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FastPickPackStationDetailsForm:FormAction = "dbFastPickPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FastPickPackStationDetailsForm:FormWidth   = 460.
   FastPickPackStationDetailsForm:FormHeight  = 200.
   FastPickPackStationDetailsForm:FormTitle   = "FastPickPack Station details".
   FastPickPackStationDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   FastPickPackStationDetailsForm:insertPaddingColumn(30).
   FastPickPackStationDetailsForm:insertColumn(185).
   
   /* Fields */
   FastPickPackStationDetailsForm:startRow().
   FastPickPackStationDetailsForm:insertLabel(fTL("Station ID")).
   FastPickPackStationDetailsForm:insertTextField("FastPickPackStationID", "", 190, TRUE).  
   
   FastPickPackStationDetailsForm:startRow().
   FastPickPackStationDetailsForm:insertLabel(fTL("Station Name")).
   FastPickPackStationDetailsForm:insertTextField("StationName", "", 190, TRUE).  
   
   FastPickPackStationDetailsForm:startRow().
   FastPickPackStationDetailsForm:insertLabel(fTL("Location")).
   FastPickPackStationDetailsForm:insertComboField("LocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "FastPickPackStation" NO-ERROR.
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      FastPickPackStationDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END. 
   
   FastPickPackStationDetailsForm:startRow().
   FastPickPackStationDetailsForm:insertLabel(fTL("OnHold Location")).
   FastPickPackStationDetailsForm:insertComboField("OnHoldLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "FastPickPackOnHold" NO-ERROR.
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      FastPickPackStationDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef).  
   END.
   
   FastPickPackStationDetailsForm:startRow().
   FastPickPackStationDetailsForm:insertLabel(fTL("PostPackout Location")).
   FastPickPackStationDetailsForm:insertComboField("PostPackoutLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "FastPickPackPostPackout" NO-ERROR.
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      FastPickPackStationDetailsForm:insertComboPairs("PostPackoutLocationID", STRING(Location.LocationID), Location.LocationRef).
   END. 
   
   FastPickPackStationDetailsForm:startRow().
   FastPickPackStationDetailsForm:insertLabel(fTL("Active")).
   FastPickPackStationDetailsForm:insertComboField("Active", "", 190, TRUE).
   FastPickPackStationDetailsForm:insertComboPairs("Active", "yes", "Yes").
   FastPickPackStationDetailsForm:insertComboPairs("Active", "no", "No").


   {webGetOptionalFormFields.i pFastPickPackStationDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FastPickPackStationDetailsForm:insertHiddenField("fastpickpackstation_browse_scroll", "").
   FastPickPackStationDetailsForm:insertHiddenField("form_name", "fastpickpackstation_details_form").
   FastPickPackStationDetailsForm:insertHiddenField("prog_name", "adFastPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackStationDetailsForm}
   
   /* Create Button Bar */
   FastPickPackStationDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataCreates THEN 
      FastPickPackStationDetailsButtons:addButton("fastpickpackstation_details_form_btn_save", 
                                                 fTL("Save"), 
                                                 "updateFastPickPackStation('fastpickpackstation_details_form');").
                                                   
   FastPickPackStationDetailsButtons:addButton("fastpickpackstation_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('fastpickpackstation_details_form_popup');").
   FastPickPackStationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FastPickPackStationDetailsForm:FormButtons = FastPickPackStationDetailsButtons.
   
   FastPickPackStationDetailsForm:endForm(). 
   
   FastPickPackStationDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pFastPickPackStationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      FastPickPackStationDetailsForm:startRow().
      FastPickPackStationDetailsForm:insertLabel(fTL("Field Label")).
      FastPickPackStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pFastPickPackStationHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "fastpickpackstationhistory_details_form"}
   ASSIGN chrDisplayFieldList  = "FastPickPackStationHistoryID,LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,"+
                                 "Active,GateUserID,CreatedDate,CreatedHour,CreatedMins"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   FastPickPackStationHistoryDetailsForm = NEW dataForm("fastpickpackstationhistory_details_form").
   FastPickPackStationHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FastPickPackStationHistoryDetailsForm:FormAction = "dbFastPickPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FastPickPackStationHistoryDetailsForm:FormWidth   = 460.
   FastPickPackStationHistoryDetailsForm:FormHeight  = 300.
   FastPickPackStationHistoryDetailsForm:FormTitle   = "FastPickPack Station History Details".
   FastPickPackStationHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   FastPickPackStationHistoryDetailsForm:insertPaddingColumn(30).
   FastPickPackStationHistoryDetailsForm:insertColumn(150).
   FastPickPackStationHistoryDetailsForm:insertColumn(125).
   FastPickPackStationHistoryDetailsForm:insertColumn(20).
   FastPickPackStationHistoryDetailsForm:insertColumn(4).
   FastPickPackStationHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("Station ID")).
   FastPickPackStationHistoryDetailsForm:insertTextField("FastPickPackStationHistoryID", "", 190, TRUE).  
   
   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("Station Name")).
   FastPickPackStationHistoryDetailsForm:insertTextField("StationName", "", 190, TRUE).  
   
   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("Location")).
   FastPickPackStationHistoryDetailsForm:insertComboField("LocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "FastPickPackStation" NO-ERROR.
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      FastPickPackStationHistoryDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END. 
   
   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("OnHold Location")).
   FastPickPackStationHistoryDetailsForm:insertComboField("OnHoldLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "FastPickPackOnHold" NO-ERROR.
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      FastPickPackStationHistoryDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef).  
   END.
   
   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("PostPackout Location")).
   FastPickPackStationHistoryDetailsForm:insertComboField("PostPackoutLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "FastPickPackPostPackout" NO-ERROR.
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      FastPickPackStationHistoryDetailsForm:insertComboPairs("PostPackoutLocationID", STRING(Location.LocationID), Location.LocationRef).
   END. 
   
   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("Active")).
   FastPickPackStationHistoryDetailsForm:insertComboField("Active", "", 190, TRUE).
   FastPickPackStationHistoryDetailsForm:insertComboPairs("Active", "yes", "Yes").
   FastPickPackStationHistoryDetailsForm:insertComboPairs("Active", "no", "No").
   
   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("Created")).
   FastPickPackStationHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).  
   /* Time fields have no label */
   FastPickPackStationHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   FastPickPackStationHistoryDetailsForm:insertLabel(":").
   FastPickPackStationHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE). 

   FastPickPackStationHistoryDetailsForm:startRow().
   FastPickPackStationHistoryDetailsForm:insertLabel(fTL("Gate User ID")).
   FastPickPackStationHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE). 
     
   FOR EACH GateUser NO-LOCK 
      BY GateUser.FullName:
      FastPickPackStationHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.

   {webGetOptionalFormFields.i pFastPickPackStationHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   FastPickPackStationHistoryDetailsForm:insertHiddenField("fastpickpackstationhistory_browse_scroll", "").
   FastPickPackStationHistoryDetailsForm:insertHiddenField("form_name", "fastpickpackstationhistory_details_form").
   FastPickPackStationHistoryDetailsForm:insertHiddenField("prog_name", "adFastPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackStationHistoryDetailsForm}
   
   /* Create Button Bar */
   FastPickPackStationHistoryDetailsButtons = NEW buttonBar().

   FastPickPackStationHistoryDetailsButtons:addButton("fastpickpackstationhistory_details_form_btn_cancel", 
                                                     fTL("Cancel"), 
                                                     "disablePopup('fastpickpackstationhistory_details_form_popup');").
   FastPickPackStationHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FastPickPackStationHistoryDetailsForm:FormButtons = FastPickPackStationHistoryDetailsButtons.
   
   FastPickPackStationHistoryDetailsForm:endForm(). 
   
   FastPickPackStationHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pFastPickPackStationHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      FastPickPackStationDetailsForm:startRow().
      FastPickPackStationDetailsForm:insertLabel(fTL("Field Label")).
      FastPickPackStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adFastPickPackStation_fastpickpackstation_details_form.i}*/ COMEBACK START COMEBACK END*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pFastPickPackStationHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   FastPickPackStationHistoryBrowseForm           = NEW dataForm("fastpickpackstationhistory_browse_form").
   FastPickPackStationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   FastPickPackStationHistoryBrowseForm:FormWidth  = 860.
   FastPickPackStationHistoryBrowseForm:FormHeight = 530.
   FastPickPackStationHistoryBrowseForm:FormTitle  = fTL("FastPickPack Station History").
   FastPickPackStationHistoryBrowseForm:FormType   = "xxl_large".
   FastPickPackStationHistoryBrowse                = NEW browseTable("fastpickpackstationhistory_browse").
   FastPickPackStationHistoryBrowse:BrowseWidth    = 840.
   FastPickPackStationHistoryBrowse:BrowseHeight   = 490.
   
   FastPickPackStationHistoryBrowse:insertColumn(fTL("History ID"),       100, "INTEGER",           FALSE).
   FastPickPackStationHistoryBrowse:insertColumn(fTL("Station Name"),     150, "CHARACTER", "LEFT", FALSE).
   FastPickPackStationHistoryBrowse:insertColumn(fTL("Location"),         175, "CHARACTER", "LEFT", FALSE).
   FastPickPackStationHistoryBrowse:insertColumn(fTL("User"),             200, "CHARACTER", "LEFT", FALSE).
   FastPickPackStationHistoryBrowse:insertColumn(fTL("Created"),          150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FastPickPackStationHistory}
   
   FastPickPackStationHistoryBrowse:StartBody().
   
   FOR EACH FastPickPackStationHistory NO-LOCK /*idx=FastPickPackStationID*/
      WHERE FastPickPackStationHistory.FastPickPackStationID = intSelectedFastPickPackStation
         BY FastPickPackStationHistory.FastPickPackStationHistoryID:
          
      FIND FIRST OperationType OF FastPickPackStationHistory NO-LOCK NO-ERROR.
      FIND FIRST GateUser      OF FastPickPackStationHistory NO-LOCK NO-ERROR.
      FIND FIRST Location      OF FastPickPackStationHistory NO-LOCK NO-ERROR.

      FastPickPackStationHistoryBrowse:startRow (FastPickPackStationHistory.FastPickPackStationHistoryID, 
         "selectFastPickPackStationHistoryRow(this," + '"' + STRING(FastPickPackStationHistory.FastPickPackStationHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i FastPickPackStationHistory}
      
      FastPickPackStationHistoryBrowse:insertData(FastPickPackStationHistory.FastPickPackStationHistoryID).
      FastPickPackStationHistoryBrowse:insertData(FastPickPackStationHistory.StationName,"LEFT").
      FastPickPackStationHistoryBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""),"LEFT").
      FastPickPackStationHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      FastPickPackStationHistoryBrowse:insertData(fDisplayDate&Time(FastPickPackStationHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      FastPickPackStationHistoryBrowse:endRow().
   END. /* FOR EACH FastPickPackStationHistory */
   
   FastPickPackStationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FastPickPackStationHistoryBrowse:getErrors().
   
   FastPickPackStationHistoryBrowseForm:insertHiddenField("popup_fastpickpackstationhistory_browse","").
   FastPickPackStationHistoryBrowseForm:insertHiddenField("FastPickPackStationHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FastPickPackStationHistoryBrowseForm}
   
   /* Create Button Bar */
   FastPickPackStationHistoryButtons = NEW buttonBar().
   
      FastPickPackStationHistoryButtons:addButton("fastpickpackstationhistory_browse_form_btn_details",
                                                 fTL("Details"),
                                                 "viewFastPickPackStationHistoryDetails('fastpickpackstationhistory_details_form');",
                                                 (IF intSelectedFastPickPackStationHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   FastPickPackStationHistoryButtons:addButton("fastpickpackstationhistory_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('fastpickpackstationhistory_browse_form_popup');").
   FastPickPackStationHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FastPickPackStationHistoryBrowseForm:FormBrowse  = FastPickPackStationHistoryBrowse.
   FastPickPackStationHistoryBrowseForm:FormButtons = FastPickPackStationHistoryButtons.
   FastPickPackStationHistoryBrowseForm:endForm(). 
   
   FastPickPackStationHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


