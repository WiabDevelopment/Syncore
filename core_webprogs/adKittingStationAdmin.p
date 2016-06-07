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

  Author: Shawn Hilts

  Created: 31/10/2014
  
  Revisions:
     30/04/2015 - twierzch - copied code from Canon to update to latest version

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
DEFINE VARIABLE intSelectedKittingStation        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrKittingStationRow             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToKittingStationRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrKittingStationID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                  AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE KittingStationBrowseFrame           AS pageFrame.
DEFINE VARIABLE KittingStationBrowse                AS browseTable.
DEFINE VARIABLE KittingStationBrowseButtons         AS buttonBar.
DEFINE VARIABLE KittingStationDetailsForm           AS dataForm.
DEFINE VARIABLE KittingStationDetailsButtons        AS buttonBar.
DEFINE VARIABLE KittingStationHistoryBrowseForm     AS dataform.
DEFINE VARIABLE KittingStationHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE KittingStationHistoryDetailsButtons AS buttonBar. 
DEFINE VARIABLE KittingStationHistoryBrowse         AS browseTable.
DEFINE VARIABLE KittingStationHistoryBrowseButtons  AS buttonBar.

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

&IF DEFINED(EXCLUDE-pKittingStationHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationHistoryDetails Procedure
PROCEDURE pKittingStationHistoryDetails:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "kittingstationhistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "KittingStationHistoryID,KittingStationID,StationCode,StationName,StationSequence,"
                                        + "LocationID,PostKittingLocationID,OnHoldLocationID,OperationTypeID,"
                                        + "KittingLineID,IsFinishedGoodStation,Active"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   KittingStationHistoryDetailsForm = NEW dataForm("kittingstationhistory_details_form").
   KittingStationHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   /*KittingStationHistoryDetailsForm:FormAction = "dbKittingStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").*/
   
   /* Setup */
   KittingStationHistoryDetailsForm:FormWidth   = 460.
   KittingStationHistoryDetailsForm:FormHeight  = 300.
   KittingStationHistoryDetailsForm:FormTitle   = fTL("KittingStationHistory Details").
   KittingStationHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   KittingStationHistoryDetailsForm:insertPaddingColumn(30).
   KittingStationHistoryDetailsForm:insertColumn(130).
   KittingStationHistoryDetailsForm:insertColumn(120).
   KittingStationHistoryDetailsForm:insertColumn(20).
   KittingStationHistoryDetailsForm:insertColumn(4).
   KittingStationHistoryDetailsForm:insertColumn(100).
   
   /* Fields */
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("History ID").
   KittingStationHistoryDetailsForm:insertTextField("KittingStationHistoryID", "", 110, TRUE). 
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("Station ID").
   KittingStationHistoryDetailsForm:insertTextField("KittingStationID", "", 110, TRUE). 
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("StationCode").
   KittingStationHistoryDetailsForm:insertTextField("StationCode", "", 110, TRUE).  
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("StationName").
   KittingStationHistoryDetailsForm:insertTextField("StationName", "", 110, TRUE).  

   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("Sequence").
   KittingStationHistoryDetailsForm:insertTextField("StationSequence", "", 110, TRUE).
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("Location").
   KittingStationHistoryDetailsForm:insertComboField("LocationID", "", 110, TRUE).  

   FIND FIRST LocationType NO-LOCK /*Inx=TypeCode*/
      WHERE LocationType.TypeCode = "Kitting"
      AND   LocationType.Active NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: Kitting".
   FOR EACH Location OF LocationType NO-LOCK /*idx=LocationRef*/
      WHERE Location.Active
      BY    Location.LocationRef:

      KittingStationHistoryDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("Post Kit Location").
   KittingStationHistoryDetailsForm:insertComboField("PostKittingLocationID", "", 110, TRUE).  

   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PostKittingLocations"
      AND   LocationType.Active NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: PostKittingLocations".
   FOR EACH Location OF LocationType NO-LOCK /*idx=LocationRef*/
      WHERE Location.Active
      BY    Location.LocationRef:

      KittingStationHistoryDetailsForm:insertComboPairs("PostKittingLocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("OnHold Location").
   KittingStationHistoryDetailsForm:insertComboField("OnHoldLocationID", "", 110, TRUE).  

   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "KittingOnHold"
      AND   LocationType.Active NO-ERROR.
      
   IF NOT AVAILABLE LocationType THEN
      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: KittingOnHold".
   FOR EACH Location NO-LOCK /*idx=LocationTypeID*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:

      KittingStationHistoryDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("Kitting Line").
   KittingStationHistoryDetailsForm:insertComboField("KittingLineID", "", 110, TRUE).
   
   FOR EACH KittingLine NO-LOCK /*idx=KittingLineID*/
      WHERE KittingLine.KittingLineID = KittingStation.KittingLineID:
         
      KittingStationHistoryDetailsForm:insertComboPairs("KittingLineID", STRING(KittingLine.KittingLineID), KittingLine.LineName).  
   END. 
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("Operation Type").
   KittingStationHistoryDetailsForm:insertComboField("OperationTypeID", "", 110, TRUE).
   
   FOR EACH OperationType NO-LOCK:
         
         KittingStationHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
      END.
   

/*   KittingStationHistoryDetailsForm:startRow().                                                                                                               */
/*   KittingStationHistoryDetailsForm:insertLabel("PostKitLocation").                                                                                           */
/*   KittingStationHistoryDetailsForm:insertComboField("PostKittingLocationID", "", 110, TRUE).                                                                 */
/*                                                                                                                                                              */
/*   FIND FIRST LocationTypeGroup NO-LOCK                                                                                                                       */
/*      WHERE LocationTypeGroup.GroupCode = "PostKittingLocations"                                                                                              */
/*      AND   LocationTypeGroup.Active NO-ERROR.                                                                                                                */
/*   IF NOT AVAILABLE LocationTypeGroup THEN                                                                                                                    */
/*      chrPageBuildError = chrPageBuildError + "No LocationTypeGroup is available for Code: PostKittingLocations".                                             */
/*   FOR EACH LocationTypeGroupLink OF LocationTypeGroup NO-LOCK                                                                                                */
/*      WHERE LocationTypeGroupLink.Active,                                                                                                                     */
/*         EACH postKitLocationType OF LocationTypeGroupLink NO-LOCK                                                                                            */
/*            WHERE postKitLocationType.Active,                                                                                                                 */
/*               EACH postKitLocation OF postKitLocationType NO-LOCK                                                                                            */
/*                  WHERE postKitLocationType.Active                                                                                                            */
/*                  BY    postKitLocation.LocationRef:                                                                                                          */
/*                                                                                                                                                              */
/*                  KittingStationHistoryDetailsForm:insertComboPairs("PostKittingLocationID", STRING(postKitLocation.LocationID), postKitLocation.LocationRef).*/
/*   END.                                                                                                                                                       */
/*                                                                                                                                                              */
/*   KittingStationHistoryDetailsForm:startRow().                                                                                                               */
/*   KittingStationHistoryDetailsForm:insertLabel("OnHoldLocation").                                                                                            */
/*   KittingStationHistoryDetailsForm:insertComboField("OnHoldLocationID", "", 110, TRUE).                                                                      */
/*                                                                                                                                                              */
/*   FIND FIRST kittingOnHoldLocationType NO-LOCK                                                                                                               */
/*      WHERE kittingOnHoldLocationType.TypeCode = "KittingOnHold"                                                                                              */
/*      AND   kittingOnHoldLocationType.Active NO-ERROR.                                                                                                        */
/*   IF NOT AVAILABLE kittingOnHoldLocationType THEN                                                                                                            */
/*      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: KittingOnHold".                                                         */
/*   FOR EACH kittingOnHoldLocation OF kittingOnHoldLocationType NO-LOCK                                                                                        */
/*      WHERE kittingOnHoldLocation.Active                                                                                                                      */
/*      BY    kittingOnHoldLocation.LocationRef:                                                                                                                */
/*                                                                                                                                                              */
/*      KittingStationHistoryDetailsForm:insertComboPairs("OnHoldLocationID", STRING(kittingOnHoldLocation.LocationID), kittingOnHoldLocation.LocationRef).     */
/*   END.                                                                                                                                                       */
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel("Finished").
   KittingStationHistoryDetailsForm:insertComboField("IsFinishedGoodStation", "", 180, TRUE).  
   KittingStationHistoryDetailsForm:insertComboPairs("IsFinishedGoodStation", "yes", "FinishedGoodStation").
   KittingStationHistoryDetailsForm:insertComboPairs("IsFinishedGoodStation", "no",  "NotFinishedGoodStation").      
   
   KittingStationHistoryDetailsForm:startRow().
   KittingStationHistoryDetailsForm:insertLabel(fTL("Active")). 
   KittingStationHistoryDetailsForm:insertComboField("Active", "", 110, TRUE).  
   KittingStationHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   KittingStationHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /*"Create" field*/
   
   {webGetOptionalFormFields.i pKittingStationHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   KittingStationHistoryDetailsForm:insertHiddenField("kittingstation_browse_scroll", "").
   KittingStationHistoryDetailsForm:insertHiddenField("form_name", "kittingstation_details_form").
   KittingStationHistoryDetailsForm:insertHiddenField("prog_name", "adKittingStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingStationHistoryDetailsForm}
   
   /* Create Button Bar */
   KittingStationHistoryDetailsButtons = NEW buttonBar().
   KittingStationHistoryDetailsButtons:addButton("kittingstationhistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('kittingstationhistory_details_form_popup');").
   KittingStationHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingStationHistoryDetailsForm:FormButtons = KittingStationHistoryDetailsButtons.
   
   KittingStationHistoryDetailsForm:endForm(). 
   
   KittingStationHistoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + KittingStationHistoryDetailsForm:getErrors().  */
  
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
   
   ASSIGN chrKittingStationID = get-value("KittingStationID")
          intSelectedKittingStation = INTEGER(chrKittingStationID)
          chrScrollToKittingStationRow = STRING(INTEGER(get-value("kittingstation_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrKittingStationID <> "" THEN
      chrKittingStationRow = 'selectKittingStationRow(document.getElementById("kittingstation_browse_row_' + chrKittingStationID + '"),"' 
                                                          + chrKittingStationID +  '");'.
                                                          
   IF get-value('popup_kittingstationhistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("kittingstationhistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
      chrBodyLoad = chrBodyLoad + 'document.getElementById("kittingstation_browse").scrollTop=' + chrScrollToKittingStationRow 
                                                           + chrKittingStationRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Kitting Station Admin".
   ThisPage:FrameTitle = "Kitting Station Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("kittingstation.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pKittingStationBrowse.
   
   /******* Popup Browsers and Forms ********/
   IF intSelectedKittingStation <> 0 THEN
      FIND FIRST KittingStation NO-LOCK 
         WHERE KittingStation.KittingStationID = intSelectedKittingStation NO-ERROR.
         
   RUN pKittingStationDetails.
   
   RUN pKittingStationHistoryBrowse.
   
   RUN pKittingStationHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT KittingStationBrowseFrame           NO-ERROR.
   DELETE OBJECT KittingStationBrowse                NO-ERROR.
   DELETE OBJECT KittingStationBrowseButtons         NO-ERROR.
   DELETE OBJECT KittingStationDetailsForm           NO-ERROR.
   DELETE OBJECT KittingStationDetailsButtons        NO-ERROR.
   DELETE OBJECT KittingStationHistoryBrowseForm     NO-ERROR.
   DELETE OBJECT KittingStationHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT KittingStationHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT KittingStationHistoryBrowse         NO-ERROR.
   DELETE OBJECT KittingStationHistoryBrowseButtons  NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationBrowse Procedure 
PROCEDURE pKittingStationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "kittingstation_details_form"}
   
   KittingStationBrowse = NEW browseTable("kittingstation_browse").
   KittingStationBrowse:BrowseWidth  = 965.
   KittingStationBrowse:BrowseHeight = 455.
   KittingStationBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   KittingStationBrowse:insertColumn(fTL("Station ID"),      120, "INTEGER", "LEFT", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i KittingStation}
   
   KittingStationBrowse:insertColumn(fTL("Sequence"),        100, "INTEGER", "LEFT", FALSE).
   KittingStationBrowse:insertColumn(fTL("StationCode"),     120, "CHARACTER", "LEFT", FALSE).
   KittingStationBrowse:insertColumn(fTL("StationName"),     130, "CHARACTER", "LEFT", FALSE).
   KittingStationBrowse:insertColumn(fTL("Location"),        100, "CHARACTER", "LEFT", FALSE).
   KittingStationBrowse:insertColumn(fTL("Kitting Line"),    120, "CHARACTER", "LEFT", FALSE).
   KittingStationBrowse:insertColumn(fTL("Active"),           90, "LOGICAL", FALSE).
   
   /*Body*/
   KittingStationBrowse:startBody().
   
   FOR EACH KittingStation NO-LOCK /*idx=ActiveListingSequence*/
      BY KittingStation.StationName:
      
      FIND FIRST Location OF KittingStation NO-LOCK NO-ERROR. /*idx=KittingLineID*/
      
      FIND FIRST KittingLine NO-LOCK
         WHERE KittingLine.KittingLineID = KittingStation.KittingLineID. /*idx=KittingLineID*/
      
/*      FIND FIRST kittingOnHoldLocation NO-LOCK                                             */
/*         WHERE kittingOnHoldLocation.LocationID = KittingStation.OnHoldLocationID NO-ERROR.*/
      
/*      FIND FIRST postKitLocation NO-LOCK                                                  */
/*         WHERE postKitLocation.LocationID = KittingStation.PostKittingLocationID NO-ERROR.*/
      
      KittingStationBrowse:startRow(KittingStation.KittingStationID, "selectKittingStationRow(this," + '"' + STRING(KittingStation.KittingStationID) + '"' + ");", "").
      KittingStationBrowse:insertData(KittingStation.KittingStationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i KittingStation}
      
      KittingStationBrowse:insertData(KittingStation.StationSequence, "LEFT").
      KittingStationBrowse:insertData((IF AVAILABLE KittingStation THEN KittingStation.StationCode ELSE ""), "LEFT").
      KittingStationBrowse:insertData((IF AVAILABLE KittingStation THEN KittingStation.StationName ELSE ""), "LEFT").
      KittingStationBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "LEFT").
      KittingStationBrowse:insertData((IF AVAILABLE KittingLine THEN KittingLine.LineName ELSE ""),"LEFT").
      KittingStationBrowse:insertData((IF AVAILABLE KittingStation THEN STRING(KittingStation.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      KittingStationBrowse:insertHiddenData("KittingStationVersionID",KittingStation.VersionID).
      
      KittingStationBrowse:endRow().
      
   END. /*FOR EACH KittingStation NO-LOCK */
   
   KittingStationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + KittingStationBrowse:getErrors().
   
   /* Create a new frame */
   KittingStationBrowseFrame = NEW pageFrame().
   KittingStationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   KittingStationBrowseFrame:FormAction="dbKittingStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   KittingStationBrowseFrame:formOpen("kittingstation_browse_form").
   
   /* Start the Frame Header */
   KittingStationBrowseFrame:insertSpacer(5).
   KittingStationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   KittingStationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   KittingStationBrowseFrame:frameClose().
   KittingStationBrowseFrame:insertSpacer(10).
   
   KittingStationBrowseFrame:insertHiddenField("kittingstation_browse_scroll","").
   KittingStationBrowseFrame:insertHiddenField("KittingStationID","").
   KittingStationBrowseFrame:insertHiddenField("KittingStationVersionID","").
   KittingStationBrowseFrame:insertHiddenField("form_name","kittingstation_browse_form").
   KittingStationBrowseFrame:insertHiddenField("popup_kittingstationhistory_browse","").
   KittingStationBrowseFrame:insertHiddenField("prog_name","adKittingStationAdmin.p").
   KittingStationBrowseFrame:insertHiddenField("popup_kittingstationhistory_details","").
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingStationBrowseFrame}
   
   KittingStationBrowseFrame:formClose().
   
   /* Create Button Bar */
   KittingStationBrowseButtons = NEW buttonBar().
   KittingStationBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   KittingStationBrowseButtons:addButton("kittingstation_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewKittingStationDetails('kittingstation_details_form');",
                                         (IF intSelectedKittingStation > 0 THEN "" ELSE "Disabled")).
   
   KittingStationBrowseButtons:addButton("kittingstation_browse_form_btn_create",
                                         fTL("Create"),
                                         "createKittingStation('kittingstation_details_form');",
                                         "").
   
   KittingStationBrowseButtons:addButton("kittingstation_browse_form_btn_history",
                                         fTL("History"),
                                         "viewKittingStationHistory('kittingstation_browse_form');",
                                         (IF intSelectedKittingStation > 0 THEN "" ELSE "Disabled")).
   
   /*
   KittingStationBrowseButtons:addButton("kittingstation_browse_form_btn_delete",
                                         fTL("Delete"),
                                         "confirmDeleteKittingStation();",
                                         (IF intSelectedKittingStation > 0 THEN "" ELSE "Disabled")).
   */
   
   KittingStationBrowseButtons:closeBar().  
   KittingStationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetails Procedure 
PROCEDURE pKittingStationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "kittingstation_details_form"}
   
   ASSIGN chrDisplayFieldList  = "KittingStationID,StationCode,StationName,StationSequence,KittingLineID,"
                                        + "LocationID,PostKittingLocationID,OnHoldLocationID,IsFinishedGoodStation,Active"
          chrEditFieldList     = "StationName,StationSequence,KittingLineID,LocationID,"
                                        + "PostKittingLocationID,OnHoldLocationID,IsFinishedGoodStation,Active"
          chrNewFieldList      = "StationCode,StationName,StationSequence,KittingLineID,"
                                        + "PostKittingLocationID,OnHoldLocationID,LocationID,IsFinishedGoodStation,Active"
          chrRequiredFieldList = "StationCode,StationName,StationSequence,LocationID,IsFinishedGoodStation"
          chrExtraFieldList    = ""
          chrValidateFieldList = "StationSequence:INTEGER".
   
   KittingStationDetailsForm = NEW dataForm("kittingstation_details_form").
   KittingStationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   KittingStationDetailsForm:FormAction = "dbKittingStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   KittingStationDetailsForm:FormWidth   = 460.
   KittingStationDetailsForm:FormHeight  = 300.
   KittingStationDetailsForm:FormTitle   = fTL("KittingStation Details").
   KittingStationDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   KittingStationDetailsForm:insertPaddingColumn(30).
   KittingStationDetailsForm:insertColumn(130).
   KittingStationDetailsForm:insertColumn(120).
   KittingStationDetailsForm:insertColumn(20).
   KittingStationDetailsForm:insertColumn(4).
   KittingStationDetailsForm:insertColumn(100).
   
   /* Fields */
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("Station ID").
   KittingStationDetailsForm:insertTextField("KittingStationID", "", 110, TRUE). 
   
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("StationCode").
   KittingStationDetailsForm:insertTextField("StationCode", "", 110, TRUE).  
   
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("StationName").
   KittingStationDetailsForm:insertTextField("StationName", "", 110, TRUE).  

   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("Sequence").
   KittingStationDetailsForm:insertTextField("StationSequence", "", 110, TRUE).
   
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("Location").
   KittingStationDetailsForm:insertComboField("LocationID", "", 110, TRUE).  

   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "Kitting"
      AND   LocationType.Active NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: Kitting".
   FOR EACH Location OF LocationType NO-LOCK /*idx=LocationRef*/
      WHERE Location.Active
      BY    Location.LocationRef:

      KittingStationDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("Post Kit Location").
   KittingStationDetailsForm:insertComboField("PostKittingLocationID", "", 110, TRUE).  

   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PostKittingLocations"
      AND   LocationType.Active NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      chrPageBuildError = chrPageBuildError + "No  is available for Code: PostKittingLocations".
   FOR EACH Location OF LocationType NO-LOCK /*idx=LocationRef*/
      WHERE Location.Active
      BY    Location.LocationRef:

      KittingStationDetailsForm:insertComboPairs("PostKittingLocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("OnHold Location").
   KittingStationDetailsForm:insertComboField("OnHoldLocationID", "", 110, TRUE).  

   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "KittingOnHold"
      AND   LocationType.Active NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: KittingOnHold".
   FOR EACH Location OF LocationType NO-LOCK /*idx=LocationRef*/
      WHERE Location.Active
      BY    Location.LocationRef:

      KittingStationDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingStationDetailsForm:StartRow().
   KittingStationDetailsForm:insertLabel("Kitting Line").
   KittingStationDetailsForm:insertComboField("KittingLineID", "", 110, TRUE).
   
   FOR EACH KittingLine NO-LOCK /*idx:KittingLineID*/ 
      WHERE KittingLine.Active
      BY    KittingLine.KittingLineID:
            
      KittingStationDetailsForm:insertComboPairs("KittingLineID", STRING(KittingLine.KittingLineID), KittingLine.LineName). 
   END.     
                                                                                                                      
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel("IsFinishedGoodStation").
   KittingStationDetailsForm:insertComboField("IsFinishedGoodStation", "", 180, TRUE).  
   KittingStationDetailsForm:insertComboPairs("IsFinishedGoodStation", "yes", "FinishedGoodStation").
   KittingStationDetailsForm:insertComboPairs("IsFinishedGoodStation", "no",  "NotFinishedGoodStation").      
   
   KittingStationDetailsForm:startRow().
   KittingStationDetailsForm:insertLabel(fTL("Active")). 
   KittingStationDetailsForm:insertComboField("Active", "", 110, TRUE).  
   KittingStationDetailsForm:insertComboPairs("Active", "yes", "Active").
   KittingStationDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pKittingStationDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   KittingStationDetailsForm:insertHiddenField("kittingstation_browse_scroll", "").
   KittingStationDetailsForm:insertHiddenField("form_name", "kittingstation_details_form").
   KittingStationDetailsForm:insertHiddenField("prog_name", "adKittingStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingStationDetailsForm}
   
   /* Create Button Bar */
   KittingStationDetailsButtons = NEW buttonBar().
   KittingStationDetailsButtons:addButton("kittingstation_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateKittingStation('kittingstation_details_form');").
   KittingStationDetailsButtons:addButton("kittingstation_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('kittingstation_details_form_popup');").
   KittingStationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingStationDetailsForm:FormButtons = KittingStationDetailsButtons.
   
   KittingStationDetailsForm:endForm(). 
   
   KittingStationDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + KittingStationDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetailsFields Procedure 
PROCEDURE pKittingStationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      KittingStationDetailsForm:startRow().
      KittingStationDetailsForm:insertLabel(fTL("Field Label")).
      KittingStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
    {adKittingStation_kittingstation_details_form.i}
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationHistoryBrowse Procedure 

PROCEDURE pKittingStationHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "kittingstation_details_form"}

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   KittingStationHistoryBrowseForm           = NEW dataForm("kittingstationhistory_browse_form").
   KittingStationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   KittingStationHistoryBrowseForm:FormWidth  = 860.
   KittingStationHistoryBrowseForm:FormHeight = 530.
   KittingStationHistoryBrowseForm:FormTitle  = fTL("Kitting Station History") + (IF AVAILABLE KittingStation THEN fTL("for KittingStationID: ")
                                                + STRING(KittingStation.KittingStationID) ELSE "").
   KittingStationHistoryBrowseForm:FormType   = "xxl_large".
   KittingStationHistoryBrowse                = NEW browseTable("kittingstationhistory_browse").
   KittingStationHistoryBrowse:BrowseWidth    = 840.
   KittingStationHistoryBrowse:BrowseHeight   = 490.
   
   KittingStationHistoryBrowse:insertColumn(fTL("HistoryID"),           60, "INTEGER", FALSE).
   KittingStationHistoryBrowse:insertColumn(fTL("StationCode"),      90, "CHARACTER", "LEFT", FALSE).
   KittingStationHistoryBrowse:insertColumn(fTL("StationName"),      90, "CHARACTER", "LEFT", FALSE).
   KittingStationHistoryBrowse:insertColumn(fTL("Location"),        100, "CHARACTER", "LEFT", FALSE).
   KittingStationHistoryBrowse:insertColumn(fTL("KittingLine"),     110, "CHARACTER", "LEFT", FALSE).
   KittingStationHistoryBrowse:insertColumn(fTL("User"),            120, "CHARACTER", "LEFT", FALSE).
   KittingStationHistoryBrowse:insertColumn(fTL("Created"),         130, "CHARACTER", "LEFT", FALSE).
   KittingStationHistoryBrowse:insertColumn(fTL("Active"),           80, "LOGICAL", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i KittingStationHistory}
   
   KittingStationHistoryBrowse:StartBody().
   
   FOR EACH KittingStationHistory NO-LOCK /*idx=KittingStationID*/
      WHERE KittingStationHistory.KittingStationID = intSelectedKittingStation
      BY KittingStationHistory.KittingStationHistoryID DESC:
          
      FIND FIRST Location      OF KittingStationHistory NO-LOCK NO-ERROR.
      
      FIND FIRST KittingLine NO-LOCK   /*idx=KittingLineID*/
         WHERE KittingStationHistory.KittingLineID = KittingLine.KittingLineID.
      
      FIND FIRST OperationType OF KittingStationHistory NO-LOCK NO-ERROR.
      FIND FIRST GateUser      OF KittingStationHistory NO-LOCK NO-ERROR.

      KittingStationHistoryBrowse:startRow(KittingStationHistory.KittingStationHistoryID, "selectKittingStationHistoryRow(this," + '"' + STRING(KittingStationHistory.KittingStationHistoryID) + '"' + ");", "").
                                                                                 

      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i KittingStationHistory}
      
      KittingStationHistoryBrowse:insertData(KittingStationHistory.KittingStationHistoryID, "LEFT").
      KittingStationHistoryBrowse:insertData(KittingStationHistory.StationCode, "LEFT").
      KittingStationHistoryBrowse:insertData(KittingStationHistory.StationName, "LEFT").
      KittingStationHistoryBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "LEFT").
      KittingStationHistoryBrowse:insertData((IF AVAILABLE KittingLine THEN KittingLine.LineName ELSE ""), "LEFT").
      KittingStationHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      KittingStationHistoryBrowse:insertData(fDisplayDate&Time(KittingStationHistory.Created,"y/m/d H:M:S"), "Left").
      KittingStationHistoryBrowse:insertData(STRING(KittingStationHistory.Active,"Yes/No")).
      
      KittingStationHistoryBrowse:endRow().
   END. /* FOR EACH KittingStationHistory */
   
   KittingStationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + KittingStationHistoryBrowse:getErrors().
   
   KittingStationHistoryBrowseForm:insertHiddenField("popup_kittingstationhistory_browse","").
   KittingStationHistoryBrowseForm:insertHiddenField("KittingStationHistoryID","").
   KittingStationHistoryBrowseForm:insertHiddenField("display_field_list","").
   KittingStationHistoryBrowseForm:insertHiddenField("KittingStationHistoryVersionID","").
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingStationHistoryBrowseForm}
   
   /* Create Button Bar */
   KittingStationHistoryBrowseButtons = NEW buttonBar().

   KittingStationHistoryBrowseButtons:addButton("kittingstationhistory_browse_form_btn_view",
                                             fTL("Details"),
                                             "viewKittingStationHistoryDetails('kittingstationhistory_details_form');",
                                             "Disabled").
   

   KittingStationHistoryBrowseButtons:addButton("kittingstationhistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('kittingstationhistory_browse_form_popup');").
   
   KittingStationHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingStationHistoryBrowseForm:FormBrowse  = KittingStationHistoryBrowse.
   KittingStationHistoryBrowseForm:FormButtons = KittingStationHistoryBrowseButtons.
   KittingStationHistoryBrowseForm:endForm(). 
   
   KittingStationHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


