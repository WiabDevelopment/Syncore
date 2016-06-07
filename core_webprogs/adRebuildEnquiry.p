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

  Author: 

  Created: 
     
  Changes: 02/11/2015  ML  Adjusted logic for filtered action point.

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
{fncLoggingFunctions.i}
{fncStatusTypeFunctions.i}

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedRebuild             AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectRebuildRow            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToRebuildRow          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrRebuildID                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredRebuildRef          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredOrderRef         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredLocationRef      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredStatus           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredRecords          AS CHARACTER NO-UNDO INITIAL 500.
DEFINE VARIABLE logFiltering                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrSelectedDate             AS CHARACTER NO-UNDO.
DEFINE VARIABLE intDisplayCount             AS INTEGER   NO-UNDO.
DEFINE VARIABLE intAvailableStatus          AS INTEGER   NO-UNDO.
DEFINE VARIABLE intFilteredRecords          AS INTEGER   NO-UNDO.

/* Definitions for System Options for Receiving */
{getReceivingOptions.i}

/* Objects */
/*DEFINE BUFFER bufRebuild FOR Rebuild.*/

/* Main Browse */
DEFINE VARIABLE RebuildBrowseFrame             AS pageFrame.
DEFINE VARIABLE RebuildBrowse                  AS browseTable.
DEFINE VARIABLE RebuildBrowseButtons           AS buttonBar.
DEFINE VARIABLE RebuildDetailsForm             AS dataForm.
DEFINE VARIABLE RebuildDetailsButtons          AS buttonBar.

/* Filter */
DEFINE VARIABLE RebuildFilterForm              AS dataForm.
DEFINE VARIABLE RebuildFilterButtons           AS buttonBar.

DEFINE STREAM strLog.


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
         HEIGHT             = 19.43
         WIDTH              = 67.4.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-pInboundBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundBrowse Procedure 
PROCEDURE pRebuildBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "rebuild_browse_form"}
   
   RebuildBrowse = NEW browseTable("rebuild_browse").
   RebuildBrowse:BrowseWidth  = 965.
   RebuildBrowse:BrowseHeight = 455.
   RebuildBrowse:ExcelExport  = TRUE.
   RebuildBrowse:SessionID    = intGblSessionID.
   RebuildBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   RebuildBrowse:insertColumn(fTL("ID"), 50, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
/*   {webGetOptionalBrowseHeaders.i rebuild}*/
   
   RebuildBrowse:insertColumn(fTL("KittingLine"),  150, "CHARACTER", "left", FALSE).
   RebuildBrowse:insertColumn(fTL("HourOfDay"),     120, "CHARACTER", "left", FALSE).
   RebuildBrowse:insertColumn(fTL("QualityStation"),  150, "CHARACTER", "left", FALSE).
   RebuildBrowse:insertColumn(fTL("StockPackage"),    125, "CHARACTER", "left", FALSE).
   RebuildBrowse:insertColumn(fTL("Units Inspected"), 100, "INTEGER", "left", FALSE).
   RebuildBrowse:insertColumn(fTL("User"),    125, "CHARACTER", "left", FALSE).
   RebuildBrowse:insertColumn(fTL("Completed"),    125, "DATE", "left", FALSE).
   /*Body*/
   RebuildBrowse:startBody().
   
/*   intAvailableStatus = fGetStatusID("Rebuild","Available").                                                                 */
/*   ASSIGN intFilteredRecords = INTEGER(chrFilteredRecords) NO-ERROR.                                                        */
/*   IF ERROR-STATUS:ERROR THEN                                                                                               */
/*   DO:                                                                                                                      */
/*      intFilteredRecords = 0.                                                                                               */
/*   END.                                                                                                                     */
/*   intDisplayCount = 0.                                                                                                     */
/*   IF logFiltering THEN                                                                                                     */
/*   DO:                                                                                                                      */
/*      FilterLoop:                                                                                                           */
/*      FOR EACH Rebuild NO-LOCK:                                                                                              */
/*         IF intDisplayCount >= intFilteredRecords THEN LEAVE FilterLoop.                                                    */
/*         FIND FIRST RebuildLineJourney NO-LOCK                                                                               */
/*            WHERE RebuildLineJourney.RebuildID = Rebuild.RebuildID AND RebuildLineJourney.Completed = "" NO-ERROR.               */
/*         FIND FIRST TaskLineWorkRebuildLink WHERE TaskLineWorkRebuildLink.RebuildID =  Rebuild.RebuildID                         */
/*                                           AND TaskLineWorkRebuildLink.Completed = "" NO-LOCK NO-ERROR.                      */
/*         FIND FIRST TaskLineWork WHERE TaskLineWork.TaskLineWorkID = TaskLineWorkRebuildLink.TaskLineWorkID NO-LOCK NO-ERROR.*/
/*         FIND FIRST Shiporder WHERE ShipOrder.ShipOrderID = TaskLineWork.ShipOrderID NO-LOCK NO-ERROR.                      */
/*         FIND FIRST Location   WHERE Location.LocationID = Rebuild.LocationID NO-LOCK NO-ERROR.                              */
/*         FIND FIRST RebuildStatus WHERE RebuildStatus.RebuildStatusID = Rebuild.RebuildStatusID NO-LOCK NO-ERROR.                */
/*                                                                                                                            */
/*         IF chrFilteredRebuildRef <> "" AND Rebuild.RebuildRef <> chrFilteredRebuildRef THEN                                    */
/*            NEXT FilterLoop.                                                                                                */
/*                                                                                                                            */
/*         IF AVAILABLE ShipOrder THEN                                                                                        */
/*         DO:                                                                                                                */
/*            IF chrFilteredOrderRef <> "" AND ShipOrder.OrderRef <> chrFilteredOrderRef THEN                                 */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*         ELSE DO:                                                                                                           */
/*            IF chrFilteredOrderRef <> "" THEN                                                                               */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*                                                                                                                            */
/*         IF AVAILABLE Location THEN                                                                                         */
/*         DO:                                                                                                                */
/*            IF chrFilteredLocationRef <> "" AND Location.LocationRef <> chrFilteredLocationRef THEN                         */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*         ELSE DO:                                                                                                           */
/*            IF chrFilteredLocationRef <> "" THEN                                                                            */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*                                                                                                                            */
/*         IF AVAILABLE RebuildStatus THEN                                                                                     */
/*         DO:                                                                                                                */
/*            IF chrFilteredStatus <> "" AND RebuildStatus.StatusName <> chrFilteredStatus THEN                                */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*         ELSE DO:                                                                                                           */
/*            IF chrFilteredStatus <> "" THEN                                                                                 */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*                                                                                                                            */
/*         RUN pSetRebuildRow.                                                                                                 */
/*         intDisplayCount = intDisplayCount + 1.                                                                             */
/*      END.                                                                                                                  */
/*   END. /*IF logFiltering THEN*/                                                                                            */
/*   ELSE                                                                                                                     */
/*   DO:                                                                                                                      */
/*      UnfilteredLoop:                                                                                                       */
/*      FOR EACH Rebuild WHERE Rebuild.RebuildStatusID <> intAvailableStatus NO-LOCK:                                            */
/*         IF intDisplayCount >= intFilteredRecords THEN LEAVE UnfilteredLoop.                                                */
/*         FIND FIRST RebuildType WHERE RebuildType.RebuildTypeID = Rebuild.RebuildTypeID NO-LOCK NO-ERROR.                        */
/*         IF RebuildType.ReUsable = YES THEN                                                                                  */
/*         DO:                                                                                                                */
/*            RUN pSetRebuildRow.                                                                                              */
/*            intDisplayCount = intDisplayCount + 1.                                                                          */
/*         END.                                                                                                               */
/*      END.                                                                                                                  */
/*   END.*/
   
   RebuildBrowse:endTable().
   
   /* Create a new frame */
   RebuildBrowseFrame = NEW pageFrame().
   RebuildBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   RebuildBrowseFrame:formOpen("rebuild_browse_form").
   
   /* Start the Frame Header */
   RebuildBrowseFrame:insertSpacer(5).
   RebuildBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   RebuildBrowse:displayBrowse().  
   
   /* This creates an Excel File */
   /* No point in adding this as there's no room for a button to display it */
   /*InboundBrowse:generateExcel(intGblSessionID, "inbound_browse").  */

   /* End the Frame Header */
   RebuildBrowseFrame:frameClose().
   RebuildBrowseFrame:insertSpacer(10).
   
   RebuildBrowseFrame:insertHiddenField("rebuild_browse_scroll","").
   RebuildBrowseFrame:insertHiddenField("RebuildID","").
   RebuildBrowseFrame:insertHiddenField("FilteredRebuildRef", chrFilteredRebuildRef).
   RebuildBrowseFrame:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   RebuildBrowseFrame:insertHiddenField("FilteredLocationRef", chrFilteredLocationRef).
   RebuildBrowseFrame:insertHiddenField("FilteredStatus", chrFilteredStatus).
   RebuildBrowseFrame:insertHiddenField("filtering", STRING(logFiltering)).
  
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i RebuildBrowseFrame}
   
   RebuildBrowseFrame:formClose().
   
   /* Create Button Bar */
   RebuildBrowseButtons = NEW buttonBar().
   RebuildBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   RebuildBrowseButtons:addButton("rebuild_browse_form_btn_filter",
                                  fTL("Filter"),
                                  "viewRebuildFilter('rebuild_filter_form');").
                                  
   RebuildBrowseButtons:addButton("rebuild_browse_form_btn_excel",
                                  fTL("Excel Export"),
                                  "excelExport('" + STRING(intGblSessionID) + "_rebuild_browse.xml')").

   RebuildBrowseButtons:closeBar().  
   RebuildBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-pInboundDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundDetailsFields Procedure 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundFilter Procedure 
PROCEDURE pRebuildFilter :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   RebuildFilterForm = NEW dataForm("rebuild_filter_form").
   RebuildFilterForm:WebStream  = STREAM WebStream:HANDLE.
   RebuildFilterForm:FormAction = "adRebuildEnquiry.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   RebuildFilterForm:FormWidth   = 350.
   RebuildFilterForm:FormHeight  = 200.
   RebuildFilterForm:FormTitle   = "Rebuild Filter".
   RebuildFilterForm:FormType    = "small_wide".

   /* Column Layout */
   RebuildFilterForm:insertPaddingColumn(10).
   RebuildFilterForm:insertColumn(100).
   RebuildFilterForm:insertColumn(169).
   RebuildFilterForm:insertColumn(20).
   RebuildFilterForm:insertColumn(2).

   /* Fields */
   RebuildFilterForm:startRow().
   RebuildFilterForm:insertLabel(fTL("Sample Matrix")).
   RebuildFilterForm:insertComboField("SampleMatrix", chrFilteredRebuildRef, 200, TRUE).
   
   RebuildFilterForm:startRow().
   RebuildFilterForm:insertLabel(fTL("Stock Package")).
   RebuildFilterForm:insertTextField("FilteredOrderRef", chrFilteredOrderRef, 200, TRUE).
   
   RebuildFilterForm:startRow().
   RebuildFilterForm:insertLabel(fTL("Kitting Line")).
   RebuildFilterForm:insertComboField("KittingLine", chrFilteredRebuildRef, 200, TRUE).
   
   RebuildFilterForm:startRow().
   RebuildFilterForm:insertLabel(fTL("User")).
   RebuildFilterForm:insertComboField("GateUser", chrFilteredRebuildRef, 200, TRUE).
   
   RebuildFilterForm:startRow().
   RebuildFilterForm:insertLabel(fTL("Date")).
   RebuildFilterForm:insertDateField("FilteredRecords", chrFilteredRecords, 200, TRUE).

   /* Add Hidden Fields*/
   RebuildFilterForm:insertHiddenField("form_name", "rebuild_filter_form").
   RebuildFilterForm:insertHiddenField("prog_name", "adRebuildEnquiry.p").
   RebuildFilterForm:insertHiddenField("FilteredRebuildRef", "").
   RebuildFilterForm:insertHiddenField("FilteredOrderRef", "").
   RebuildFilterForm:insertHiddenField("FilteredLocationRef", "").
   RebuildFilterForm:insertHiddenField("FilteredStatus", "").
   RebuildFilterForm:insertHiddenField("FilteredRecords", "").
   RebuildFilterForm:insertHiddenField("filtering", "yes").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i RebuildFilterForm}

   /* Create Button Bar */
   RebuildFilterButtons = NEW buttonBar().

   RebuildFilterButtons:addButton("rebuild_filter_form_btn_search",
                                   fTL("Filter"),
                                   "filterRebuild('rebuild_filter_form');").

   RebuildFilterButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   RebuildFilterForm:FormButtons = RebuildFilterButtons.

   RebuildFilterForm:endForm().
   RebuildFilterForm:displayForm().

   /*   chrPageBuildError = chrPageBuildError + InboundFilterForm:getErrors().  */

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
   
   /* Process URL values */
   ASSIGN chrRebuildID              = get-value("RebuildID")
          intSelectedRebuild        = INT(chrRebuildID)
          chrFilteredRebuildRef     = get-value("FilteredRebuildRef")
          chrFilteredOrderRef    = get-value("FilteredOrderRef")
          chrFilteredLocationRef = get-value("FilteredLocationRef")
          chrFilteredStatus      = get-value("FilteredStatus")
          chrFilteredRecords     = (IF get-value("FilteredRecords") <> "" THEN get-value("FilteredRecords") ELSE chrFilteredRecords)
          logFiltering           = (get-value("filtering") = "yes")
          chrScrollToRebuildRow     = STRING(INT(get-value("rebuild_browse_scroll"))) + ";" NO-ERROR.

   IF chrFilteredRebuildRef     = "" AND
      chrFilteredOrderRef    = "" AND
      chrFilteredLocationRef = "" AND
      chrFilteredStatus      = "" THEN
      ASSIGN logFiltering    = FALSE.

   IF intSelectedRebuild > 0 THEN
      chrSelectRebuildRow = 'selectRebuildRow(document.getElementById("rebuild_browse_row_' + chrRebuildID + '"),"'
                              + chrRebuildID + '","adRebuildEnquiry.p' + '");'.
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("rebuild_browse").scrollTop=' + chrScrollToRebuildRow + chrSelectRebuildRow.
     
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Rebuild Enquiry".
   ThisPage:FrameTitle = "Rebuild Enquiry".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("rebuildenquiry.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
/*   IF logGblDebugging THEN                                                                                                    */
/*      fLog("GblSessionID:" + STRING(intGblSessionID) + " GblUserID:" + STRING(intGblUserID) + " GblUserName:" + chrGblUserName*/
/*                           + " GblLanguageID:" + STRING(intGblLanguageID) + " GblEnvironment:" + chrGblEnvironment).          */
   
   /******* Main Browser ********************/
   RUN pRebuildBrowse.
   
   /******* Popup Browsers and Forms ********/    
/*   FIND FIRST Inbound WHERE Inbound.InboundID = intSelectedInbound NO-LOCK NO-ERROR.*/
   
   RUN pRebuildFilter.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Errors from an Update program are most important - get and show these first */
   chrFromURLError = get-value("error").
   
   /* This will display errors that came in from an Update program via the URL */
   IF chrFromURLError <> "" THEN
      {&OUT} '<script>systemAlert("message_alert","' + chrFromURLError + '","Error")</script>' SKIP.
   
   /* This will display errors that were generated when building the page */
   IF chrPageBuildError <> "" THEN
      {&OUT} '<script>systemAlert("message_alert","' + chrPageBuildError + '","Error")</script>' SKIP.
   
   IF chrFromURLError = "" THEN
      {&OUT} '<script>footerMessage("' + get-value("return-message") + '");</script>' SKIP.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetRebuildRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetRebuildRow Procedure
PROCEDURE pSetRebuildRow:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*         FIND FIRST Location         NO-LOCK WHERE Location.LocationID = Rebuild.LocationID NO-ERROR.                                            */
/*         FIND FIRST RebuildStatus       NO-LOCK WHERE RebuildStatus.RebuildStatusID = Rebuild.RebuildStatusID NO-ERROR.                              */
/*         FIND FIRST RebuildLineJourney  NO-LOCK WHERE RebuildLineJourney.RebuildID = Rebuild.RebuildID AND RebuildLineJourney.Completed = "" NO-ERROR.*/
/*         FIND FIRST RebuildLineStop     NO-LOCK WHERE RebuildLineStop.RebuildLineJourneyID = RebuildLineJourney.RebuildLineJourneyID AND             */
/*                                                   RebuildLineStop.RebuildID = Rebuild.RebuildID AND                                                */
/*                                                   RebuildLineStop.RebuildLineStopArrival = "" NO-ERROR.                                          */
/*         FIND FIRST PickPackStation  NO-LOCK WHERE PickPackStation.PickPackStationID = RebuildLineStop.PickPackStationID NO-ERROR.               */
/*         FIND FIRST TaskLineWorkRebuildLink WHERE TaskLineWorkRebuildLink.RebuildID =  Rebuild.RebuildID                                             */
/*                                           AND TaskLineWorkRebuildLink.Completed = "" NO-LOCK NO-ERROR.                                          */
/*         FIND FIRST TaskLineWork WHERE TaskLineWork.TaskLineWorkID = TaskLineWorkRebuildLink.TaskLineWorkID NO-LOCK NO-ERROR.                    */
/*         FIND FIRST Shiporder WHERE ShipOrder.ShipOrderID = TaskLineWork.ShipOrderID NO-LOCK NO-ERROR.                                          */
/*                                                                                                                                                */
/*         RebuildBrowse:startRow(Rebuild.RebuildID, "selectRebuildRow(this," + '"' + STRING(Rebuild.RebuildID) + '"' + ");", "").                      */
/*         RebuildBrowse:insertData(Rebuild.RebuildID, "left").                                                                                      */
/*                                                                                                                                                */
/*                                                                                                                                                */
/*         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */                           */
/*/*         {webGetOptionalBrowseFields.i rebuild}*/                                                                                              */
/*                                                                                                                                                */
/*         RebuildBrowse:insertData(Rebuild.RebuildRef, "left").                                                                                     */
/*         RebuildBrowse:insertData((IF AVAILABLE ShipOrder       THEN ShipOrder.OrderRef          ELSE ""), "left").                              */
/*         RebuildBrowse:insertData((IF AVAILABLE Location        THEN Location.LocationRef        ELSE ""), "left").                              */
/*         RebuildBrowse:insertData((IF AVAILABLE RebuildStatus      THEN RebuildStatus.StatusName       ELSE ""), "left").                          */
/*         RebuildBrowse:insertData((IF AVAILABLE PickPackStation THEN PickPackStation.StationName ELSE ""), "left").                              */
         
         RebuildBrowse:endRow().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF
