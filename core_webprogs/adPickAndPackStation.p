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

  Created: 25/03/2015

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

DEFINE VARIABLE intSelectedPickAndPackStation           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectPickAndPackStation             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickAndPackStation           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPickAndPackStationID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPickAndPackStationHistory       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE PickAndPackStationBrowseFrame           AS pageFrame.
DEFINE VARIABLE PickAndPackStationBrowse                AS browseTable.
DEFINE VARIABLE PickAndPackStationBrowseButtons         AS buttonBar.
DEFINE VARIABLE PickAndPackStationDetailsForm           AS dataForm.
DEFINE VARIABLE PickAndPackStationButtons               AS buttonBar.

DEFINE VARIABLE PickAndPackStationHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE PickAndPackStationHistoryBrowse         AS browseTable.
DEFINE VARIABLE PickAndPackStationHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE PickAndPackStationHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE PickAndPackStationHistoryDetailsButtons AS buttonBar.

/* Define Buffers */
DEFINE BUFFER onHoldLocation      FOR Location.
DEFINE BUFFER postPackOutLocation FOR Location.



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
   
   ASSIGN chrPickAndPackStationID       = get-value("PickAndPackStationID")
          intSelectedPickAndPackStation = INTEGER(chrPickAndPackStationID)
          chrScrollToPickAndPackStation = STRING(INTEGER(get-value("pickandpackstation_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPickAndPackStationID <> "" THEN
      chrSelectPickAndPackStation = 'selectPickAndPackStationRow(document.getElementById("pickandpackstation_browse_row_' + chrPickAndPackStationID + '"),"' 
                                        + chrPickAndPackStationID +  '");'.
   
   IF get-value('popup_pickandpackstationhistory_browse') = "yes" THEN
      chrPopupPickAndPackStationHistory  = 'enablePopup("pickandpackstationhistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("pickandpackstation_browse").scrollTop=' + chrScrollToPickAndPackStation 
                                                           + chrSelectPickAndPackStation 
                                                           + chrPopupPickAndPackStationHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Pick And Pack Station".
   ThisPage:FrameTitle    = "Pick And Pack Station".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("pickandpackstation.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPickAndPackStationBrowse.
   
   FIND FIRST PickAndPackStation NO-LOCK /* idx=PickAndPackStationID */
      WHERE PickAndPackStation.PickAndPackStationID = intSelectedPickAndPackStation NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pPickAndPackStationDetails.
   RUN pPickAndPackStationHistory.
   RUN pPickAndPackStationHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT PickAndPackStationBrowseFrame           NO-ERROR.
   DELETE OBJECT PickAndPackStationBrowse                NO-ERROR.
   DELETE OBJECT PickAndPackStationBrowseButtons         NO-ERROR.
   
   DELETE OBJECT PickAndPackStationDetailsForm           NO-ERROR.
   DELETE OBJECT PickAndPackStationButtons               NO-ERROR.
   
   DELETE OBJECT PickAndPackStationHistoryBrowseForm     NO-ERROR.   
   DELETE OBJECT PickAndPackStationHistoryBrowse         NO-ERROR.
   DELETE OBJECT PickAndPackStationHistoryBrowseButtons  NO-ERROR.
   
   DELETE OBJECT PickAndPackStationHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT PickAndPackStationHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPickAndPackStationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickAndPackStationBrowse Procedure 
PROCEDURE pPickAndPackStationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "pickandpackstation_details_form"}
   
   PickAndPackStationBrowse              = NEW browseTable("pickandpackstation_browse").
   PickAndPackStationBrowse:BrowseWidth  = 965.
   PickAndPackStationBrowse:BrowseHeight = 455.
   PickAndPackStationBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   PickAndPackStationBrowse:insertColumn(fTL("ID"),                    80, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickAndPackStation}
   
   PickAndPackStationBrowse:insertColumn(fTL("Location"),             120, "CHARACTER", "left", FALSE).
   PickAndPackStationBrowse:insertColumn(fTL("Station"),              150, "CHARACTER", "left", FALSE).
   PickAndPackStationBrowse:insertColumn(fTL("OnHold Location"),      180, "CHARACTER", "left", FALSE).
   PickAndPackStationBrowse:insertColumn(fTL("PostPackOut Location"), 190, "CHARACTER", "left", FALSE).
   PickAndPackStationBrowse:insertColumn(fTL("Active"),                80, "LOGICAL",           FALSE).
   
   /*Body*/
   PickAndPackStationBrowse:startBody().
   
   FOR EACH PickAndPackStation NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PickAndPackStation.Active:
         
      FIND FIRST Location OF PickAndPackStation NO-LOCK NO-ERROR.
      
      FIND FIRST onHoldLocation NO-LOCK /* idx=LocationID */
         WHERE onHoldLocation.LocationID = PickAndPackStation.OnHoldLocationID NO-ERROR. 
         
      FIND FIRST postPackOutLocation NO-LOCK /* idx=LocationID */
         WHERE postPackOutLocation.LocationID = PickAndPackStation.PostPackoutLocationID NO-ERROR.  
      
      PickAndPackStationBrowse:startRow(PickAndPackStation.PickAndPackStationID, "selectPickAndPackStationRow(this," + '"' 
                                           + STRING(PickAndPackStation.PickAndPackStationID) + '"' + ");", "").
                                                                  
      PickAndPackStationBrowse:insertData(PickAndPackStation.PickAndPackStationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PickAndPackStation}
      
      PickAndPackStationBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "left").
      PickAndPackStationBrowse:insertData(PickAndPackStation.StationName, "left").
      PickAndPackStationBrowse:insertData((IF AVAILABLE onHoldLocation THEN onHoldLocation.LocationRef ELSE ""), "left").
      PickAndPackStationBrowse:insertData((IF AVAILABLE postPackOutLocation THEN postPackOutLocation.LocationRef ELSE ""), "left").
      PickAndPackStationBrowse:insertData(STRING(PickAndPackStation.Active,"Yes/No")).
      
      /* Add hidden fields */
      PickAndPackStationBrowse:insertHiddenData("PickAndPackStationVersionID", PickAndPackStation.VersionID).
      
      PickAndPackStationBrowse:endRow().
      
   END. /*FOR EACH PickAndPackStation NO-LOCK */
   
   PickAndPackStationBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PickAndPackStationBrowse:getErrors().
   
   /* Create a new frame */
   PickAndPackStationBrowseFrame           = NEW pageFrame().
   PickAndPackStationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PickAndPackStationBrowseFrame:FormAction = "dbPickAndPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PickAndPackStationBrowseFrame:formOpen("pickandpackstation_browse_form").
   
   /* Start the Frame Header */
   PickAndPackStationBrowseFrame:insertSpacer(5).
   PickAndPackStationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PickAndPackStationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PickAndPackStationBrowseFrame:frameClose().
   PickAndPackStationBrowseFrame:insertSpacer(10).
   
   PickAndPackStationBrowseFrame:insertHiddenField("pickandpackstation_browse_scroll", "").
   PickAndPackStationBrowseFrame:insertHiddenField("PickAndPackStationID", "").
   PickAndPackStationBrowseFrame:insertHiddenField("PickAndPackStationVersionID", "").
   PickAndPackStationBrowseFrame:insertHiddenField("popup_pickandpackstationhistory_browse", "").
   PickAndPackStationBrowseFrame:insertHiddenField("form_name", "pickandpackstation_browse_form").
   PickAndPackStationBrowseFrame:insertHiddenField("prog_name", "adPickAndPackStation.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickAndPackStationBrowseFrame}
   
   PickAndPackStationBrowseFrame:formClose().
   
   /* Create Button Bar */
   PickAndPackStationBrowseButtons           = NEW buttonBar().
   PickAndPackStationBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   PickAndPackStationBrowseButtons:addButton("pickandpackstation_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewPickAndPackStationDetails('pickandpackstation_details_form');",
                                             (IF intSelectedPickAndPackStation > 0 THEN "" ELSE "Disabled")).
   
   PickAndPackStationBrowseButtons:addButton("pickandpackstation_browse_form_btn_create",
                                             fTL("Create"),
                                             "createPickAndPackStation('pickandpackstation_details_form');",
                                             "").
                                             
   PickAndPackStationBrowseButtons:addButton("pickandpackstation_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
   
   PickAndPackStationBrowseButtons:closeBar().
     
   PickAndPackStationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPickAndPackStationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickAndPackStationDetails Procedure 
PROCEDURE pPickAndPackStationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "pickandpackstation_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PickAndPackStationID,LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active" 
          chrEditFieldList     = "LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active" 
          chrNewFieldList      = "LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active" 
          chrRequiredFieldList = "LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PickAndPackStationDetailsForm           = NEW dataForm("pickandpackstation_details_form").
   PickAndPackStationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PickAndPackStationDetailsForm:FormAction = "dbPickAndPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PickAndPackStationDetailsForm:FormWidth  = 460.
   PickAndPackStationDetailsForm:FormHeight = 300.
   PickAndPackStationDetailsForm:FormTitle  = "PickAndPackStation Details".
   PickAndPackStationDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   PickAndPackStationDetailsForm:insertPaddingColumn(30).
   PickAndPackStationDetailsForm:insertColumn(150).
   PickAndPackStationDetailsForm:insertColumn(160).
   PickAndPackStationDetailsForm:insertColumn(20).
   PickAndPackStationDetailsForm:insertColumn(4).
   PickAndPackStationDetailsForm:insertColumn(110).
   
   /* Fields */
   PickAndPackStationDetailsForm:startRow().
   PickAndPackStationDetailsForm:insertLabel("Station ID").
   PickAndPackStationDetailsForm:insertTextField("PickAndPackStationID", "", 110, TRUE).  
   
   PickAndPackStationDetailsForm:startRow().
   PickAndPackStationDetailsForm:insertLabel("Location").
   PickAndPackStationDetailsForm:insertComboField("LocationID", "", 110, TRUE).
   FIND FIRST LocationType NO-LOCK /* idx=TypeCode */
      WHERE LocationType.TypeCode = "PickAndPackStation" NO-ERROR.
   IF AVAILABLE LocationType THEN 
   DO:
      /* Insert the LocationRef */
      FOR EACH Location NO-LOCK /* idx=LocationTypeActive */
         WHERE Location.LocationTypeID = LocationType.LocationTypeID 
         BY    Location.Active:
            
         PickAndPackStationDetailsForm:insertComboPairs("LocationID", 
                                                        STRING(Location.LocationID), 
                                                        Location.LocationRef).
      
      END. /* FOR EACH Location NO-LOCK */ 
   END. /* IF AVAILABLE LocationType THEN */
   
   PickAndPackStationDetailsForm:startRow().
   PickAndPackStationDetailsForm:insertLabel("StationName").
   PickAndPackStationDetailsForm:insertTextField("StationName", "", 110, TRUE).
   
   PickAndPackStationDetailsForm:startRow().
   PickAndPackStationDetailsForm:insertLabel("OnHoldLocation").
   PickAndPackStationDetailsForm:insertComboField("OnHoldLocationID", "", 110, TRUE).
   FIND FIRST LocationType NO-LOCK /* idx=TypeCode */
      WHERE LocationType.TypeCode = "PickAndPackOnHold" NO-ERROR.
   IF AVAILABLE LocationType THEN
   DO:
      /* Insert the LocationRef */
      FOR EACH onHoldLocation NO-LOCK /* idx=LocationTypeActive */
         WHERE onHoldLocation.LocationTypeID = LocationType.LocationTypeID
         BY    onHoldLocation.Active:

         PickAndPackStationDetailsForm:insertComboPairs("OnHoldLocationID",
                                                        STRING(onHoldLocation.LocationID),
                                                        onHoldLocation.LocationRef).

      END. /* FOR EACH onHoldLocation NO-LOCK */
   END. /* IF AVAILABLE LocationType THEN */
   
   PickAndPackStationDetailsForm:startRow().
   PickAndPackStationDetailsForm:insertLabel("PostPackoutLocation").
   PickAndPackStationDetailsForm:insertComboField("PostPackoutLocationID", "", 110, TRUE).
   FIND FIRST LocationType NO-LOCK /* idx=TypeCode */
      WHERE LocationType.TypeCode = "PickAndPackPostPackout" NO-ERROR.
   IF AVAILABLE LocationType THEN 
   DO:
      /* Insert the LocationRef */
      FOR EACH postPackOutLocation NO-LOCK /* idx=LocationTypeActive */
         WHERE postPackOutLocation.LocationTypeID = LocationType.LocationTypeID
         BY    postPackOutLocation.Active:
            
         PickAndPackStationDetailsForm:insertComboPairs("PostPackoutLocationID", 
                                                        STRING(postPackOutLocation.LocationID), 
                                                        postPackOutLocation.LocationRef).
      
      END. /* FOR EACH postPackOutLocation NO-LOCK */ 
   END. /* IF AVAILABLE LocationType THEN */ 
   
   PickAndPackStationDetailsForm:startRow().
   PickAndPackStationDetailsForm:insertLabel(fTL("Active")). 
   PickAndPackStationDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PickAndPackStationDetailsForm:insertComboPairs("Active", "yes", "Active").
   PickAndPackStationDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPickAndPackStationDetailsFields}
   
   /* Add Hidden Fields*/
   PickAndPackStationDetailsForm:insertHiddenField("pickandpackstation_browse_scroll", "").
   PickAndPackStationDetailsForm:insertHiddenField("form_name", "pickandpackstation_details_form").
   PickAndPackStationDetailsForm:insertHiddenField("prog_name", "adPickAndPackStation.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickAndPackStationDetailsForm}
   
   /* Create Button Bar */
   PickAndPackStationButtons = NEW buttonBar().
   
   PickAndPackStationButtons:addButton("pickandpackstation_details_form_btn_save", 
                                       fTL("Save"), 
                                       "updatePickAndPackStation('pickandpackstation_details_form');").
   
   PickAndPackStationButtons:addButton("pickandpackstation_details_form_btn_cancel", 
                                       fTL("Cancel"), 
                                       "cancelUpdate('UserCancelled','process_mode'); 
                                       disablePopup('pickandpackstation_details_form_popup');").
   
   PickAndPackStationButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickAndPackStationDetailsForm:FormButtons = PickAndPackStationButtons.
   
   PickAndPackStationDetailsForm:endForm(). 
   
   PickAndPackStationDetailsForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPickAndPackStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickAndPackStationDetailsFields Procedure 
PROCEDURE pPickAndPackStationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPickAndPackStationHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickAndPackStationHistory Procedure
PROCEDURE pPickAndPackStationHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "pickandpackstationhistory_details_form"}
   
   FIND FIRST PickAndPackStation NO-LOCK /* idx=PickAndPackStationID */
      WHERE PickAndPackStation.PickAndPackStationID = intSelectedPickAndPackStation NO-ERROR.
   
   PickAndPackStationHistoryBrowseForm           = NEW dataForm("pickandpackstationhistory_browse_form").
   PickAndPackStationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PickAndPackStationHistoryBrowseForm:FormWidth  = 850.
   PickAndPackStationHistoryBrowseForm:FormHeight = 540.
   PickAndPackStationHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE PickAndPackStation THEN " for PickAndPack StationID: " 
                                                                   + STRING(PickAndPackStation.PickAndPackStationID) ELSE "").
   PickAndPackStationHistoryBrowseForm:FormType   = "xxl_large".
   
   PickAndPackStationHistoryBrowse              = NEW browseTable("pickandpackstationhistory_browse").
   PickAndPackStationHistoryBrowse:BrowseWidth  = 830.
   PickAndPackStationHistoryBrowse:BrowseHeight = 500.
   PickAndPackStationHistoryBrowse:ExcelExport  = TRUE.
   PickAndPackStationHistoryBrowse:SessionID    = intGblSessionID.
   
   
   PickAndPackStationHistoryBrowse:insertColumn(fTL("Hist ID"),               60, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickAndPackStationHistory}

   PickAndPackStationHistoryBrowse:insertColumn(fTL("Pick&PackID"),           90, "INTEGER",           FALSE).
   PickAndPackStationHistoryBrowse:insertColumn(fTL("Location"),             100, "CHARACTER", "left", FALSE).
   PickAndPackStationHistoryBrowse:insertColumn(fTL("Station Name"),         120, "CHARACTER", "left", FALSE).
   PickAndPackStationHistoryBrowse:insertColumn(fTL("OnHold Location"),       95, "INTEGER",           FALSE).
   PickAndPackStationHistoryBrowse:insertColumn(fTL("PostPackout Location"), 140, "INTEGER",           FALSE).
   PickAndPackStationHistoryBrowse:insertColumn(fTL("Active"),                70, "LOGICAL",           FALSE).
   PickAndPackStationHistoryBrowse:insertColumn(fTL("Created"),              135, "CHARACTER", "left", FALSE).
   
   PickAndPackStationHistoryBrowse:StartBody().
   
   IF AVAILABLE PickAndPackStation THEN
   DO:
      /*List the PickAndPackStationHistory*/
      FOR EACH PickAndPackStationHistory NO-LOCK /* idx=PickAndPackStationIDCreated */
         WHERE PickAndPackStationHistory.PickAndPackStationID = intSelectedPickAndPackStation
         BY    PickAndPackStationHistory.PickAndPackStationHistoryID:
         
         FIND FIRST Location OF PickAndPackStationHistory NO-LOCK NO-ERROR.
         
         FIND FIRST onHoldLocation NO-LOCK /* idx=LocationID */
            WHERE onHoldLocation.LocationID = PickAndPackStationHistory.OnHoldLocationID NO-ERROR. 
         
         FIND FIRST postPackOutLocation NO-LOCK /* idx=LocationID */
            WHERE postPackOutLocation.LocationID = PickAndPackStationHistory.PostPackoutLocationID NO-ERROR.
       
         PickAndPackStationHistoryBrowse:startRow(PickAndPackStationHistory.PickAndPackStationHistoryID, "selectHistoryRow(this," + '"' + STRING(PickAndPackStationHistory.PickAndPackStationHistoryID) 
                                                     + '","pickAndPackStationHistory"' + ");", "").
         
         PickAndPackStationHistoryBrowse:insertData(PickAndPackStationHistory.PickAndPackStationHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PickAndPackStationHistory}
         
         PickAndPackStationHistoryBrowse:insertData(STRING(PickAndPackStationHistory.PickAndPackStationID)).
         PickAndPackStationHistoryBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "left").
         PickAndPackStationHistoryBrowse:insertData(PickAndPackStationHistory.StationName, "left").
         PickAndPackStationHistoryBrowse:insertData(IF AVAILABLE onHoldLocation THEN onHoldLocation.LocationRef ELSE "").                            
         PickAndPackStationHistoryBrowse:insertData(IF AVAILABLE postPackOutLocation THEN postPackOutLocation.LocationRef ELSE "").
         PickAndPackStationHistoryBrowse:insertData(STRING(PickAndPackStationHistory.Active, "Yes/No")).
         PickAndPackStationHistoryBrowse:insertData(fDisplayDate&Time(PickAndPackStationHistory.Created,"y/m/d H:M:S"), "right").
         
         
         /* Add hidden fields */         
         PickAndPackStationHistoryBrowse:insertHiddendata("PickAndPackStationHistoryID",PickAndPackStationHistory.PickAndPackStationHistoryID).
         
         PickAndPackStationHistoryBrowse:endRow().
      
      END. /* FOR EACH PickAndPackStationHistory OF PickAndPackStation NO-LOCK, */
   END. /*IF AVAILABLE PickAndPackStation THEN*/
   
   PickAndPackStationHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PickAndPackStationHistoryBrowse:getErrors().
   
   PickAndPackStationHistoryBrowseForm:insertHiddenField("PickAndPackStationHistoryID","").
   PickAndPackStationHistoryBrowseForm:insertHiddenField("popup_pickandpackstationhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickAndPackStationHistoryBrowseForm}
   
   /* Create Button Bar */
   PickAndPackStationHistoryBrowseButtons = NEW buttonBar().                                                 
   
   PickAndPackStationHistoryBrowseButtons:addButton("pickandpackstationhistory_browse_form_btn_details",
                                                    fTL("Details"),
                                                    "viewPickAndPackStationHistoryDetails('pickandpackstationhistory_details_form');",
                                                    "Disabled").
   
   PickAndPackStationHistoryBrowseButtons:addButton("pickandpackstationhistory_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('pickandpackstationhistory_browse_form_popup');").
   
   PickAndPackStationHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickAndPackStationHistoryBrowseForm:FormBrowse  = PickAndPackStationHistoryBrowse.
   PickAndPackStationHistoryBrowseForm:FormButtons = PickAndPackStationHistoryBrowseButtons.
   
   PickAndPackStationHistoryBrowseForm:endForm(). 
   
   PickAndPackStationHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pPickAndPackStationHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickAndPackStationHistoryDetails Procedure
PROCEDURE pPickAndPackStationHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "pickandpackstationhistory_details_form"}
   
   chrDisplayFieldList  = "PickAndPackStationHistoryID,PickAndPackStationID,LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,"
                             + "Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                              
   PickAndPackStationHistoryDetailsForm           = NEW dataForm("pickandpackstationhistory_details_form").
   PickAndPackStationHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   PickAndPackStationHistoryDetailsForm:FormWidth  = 460.
   PickAndPackStationHistoryDetailsForm:FormHeight = 300.
   PickAndPackStationHistoryDetailsForm:FormTitle  = "Pick And Pack Station History Details".
   PickAndPackStationHistoryDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   PickAndPackStationHistoryDetailsForm:insertPaddingColumn(40).
   PickAndPackStationHistoryDetailsForm:insertColumn(110).
   PickAndPackStationHistoryDetailsForm:insertColumn(120).
   PickAndPackStationHistoryDetailsForm:insertColumn(20).
   PickAndPackStationHistoryDetailsForm:insertColumn(4).
   PickAndPackStationHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel(fTL("ID")).
   PickAndPackStationHistoryDetailsForm:insertTextField("PickAndPackStationHistoryID", "", 90, TRUE).    
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel(fTL("PickAndPackStationID")).
   PickAndPackStationHistoryDetailsForm:insertTextField("PickAndPackStationID", "", 90, TRUE).
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel("User").
   PickAndPackStationHistoryDetailsForm:insertComboField("GateUserID", "", 130, TRUE).
   /* Insert the GateUser FullName */
   FOR EACH GateUser NO-LOCK 
      BY GateUser.Active:
         
      PickAndPackStationHistoryDetailsForm:insertComboPairs("GateUserID", 
                                                            STRING(GateUser.GateUserID), 
                                                            GateUser.FullName).
   
   END.
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel("Location").
   PickAndPackStationHistoryDetailsForm:insertComboField("LocationID", "", 130, TRUE).
   /* Insert the LocationRef */
   FOR EACH Location NO-LOCK /* idx=LocationTypeActive */
      BY Location.Active:
         
      PickAndPackStationHistoryDetailsForm:insertComboPairs("LocationID", 
                                                            STRING(Location.LocationID), 
                                                            Location.LocationRef).
   
   END.
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel(fTL("StationName")).
   PickAndPackStationHistoryDetailsForm:insertTextField("StationName", "", 90, TRUE).
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel(fTL("OnHoldLocationID")).
   PickAndPackStationHistoryDetailsForm:insertTextField("OnHoldLocationID", "", 90, TRUE).
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel(fTL("PostPackoutLocationID")).
   PickAndPackStationHistoryDetailsForm:insertTextField("PostPackoutLocationID", "", 90, TRUE).
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel(fTL("Active")).
   PickAndPackStationHistoryDetailsForm:insertComboField("Active", "", 110, TRUE).
   PickAndPackStationHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   PickAndPackStationHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   PickAndPackStationHistoryDetailsForm:startRow().
   PickAndPackStationHistoryDetailsForm:insertLabel("Created").
   PickAndPackStationHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   PickAndPackStationHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   PickAndPackStationHistoryDetailsForm:insertLabel(":").
   PickAndPackStationHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   PickAndPackStationHistoryDetailsForm:insertHiddenField("pickandpackstation_browse_scroll","").
   PickAndPackStationHistoryDetailsForm:insertHiddenField("popup_pickandpackstationhistory_browse", "").
   PickAndPackStationHistoryDetailsForm:insertHiddenField("PickAndPackStationHistoryID","").
   PickAndPackStationHistoryDetailsForm:insertHiddenField("form_name","pickandpackstationhistory_details_form").
   PickAndPackStationHistoryDetailsForm:insertHiddenField("prog_name","adPickAndPackStation.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickAndPackStationHistoryDetailsForm}
   
   /* Create Button Bar */
   PickAndPackStationHistoryDetailsButtons = NEW buttonBar().
   
   PickAndPackStationHistoryDetailsButtons:addButton("pickandpackstationhistory_details_form_btn_cancel",
                                                     fTL("Cancel"),
                                                     "disablePopup('pickandpackstationhistory_details_form_popup');").
                                        
   PickAndPackStationHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   PickAndPackStationHistoryDetailsForm:FormButtons = PickAndPackStationHistoryDetailsButtons.
   
   PickAndPackStationHistoryDetailsForm:endForm().
    
   PickAndPackStationHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

