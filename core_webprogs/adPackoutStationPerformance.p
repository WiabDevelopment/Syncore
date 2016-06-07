&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adPackoutStationPerformance.p


  Description: Allows user to filter station based on name and time interval. User is able to view details of records belonging to Table 
               pickpackstationunitsprocessed and also export the filtered result on to an excel sheet.
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Kitty Jose

  Created: 21/09/2015

------------------------------------------------------------------------*/
/*           This .W file was created with the Progress AppBuilder.     */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     clean up will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Custom Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{defWebDefinitions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedPickPackStationID                AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrPickPackStationUnitsProcessedID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedPickPackStUnitsProcessedID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickPackRow                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFilters                             AS CHARACTER NO-UNDO.
DEFINE VARIABLE logFilter                                   AS LOGICAL   NO-UNDO.

/* Filter Variables */
DEFINE VARIABLE chrPickPackStationID                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPickPackStationUnitsProcessedSelectedRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE dateSelectedStation                         AS DATE      NO-UNDO.

/* Definitions for System Options for Admin */
{getAdminOptions.i}

/* Objects */

DEFINE VARIABLE PickPackStationUnitsProcessedBrowseFrame    AS pageFrame.
DEFINE VARIABLE PickPackStationUnitsProcessedDetailsForm    AS dataForm.
DEFINE VARIABLE PickPackStationUnitsProcessedFilterForm     AS dataForm.
DEFINE VARIABLE PickPackStationUnitsProcessedBrowse         AS browseTable.
DEFINE VARIABLE PickPackStationUnitsProcessedBrowseButton   AS buttonBar.
DEFINE VARIABLE PickPackStationUnitsProcessedDetailsButton  AS buttonBar.
DEFINE VARIABLE PickPackStationUnitsProcessedFilterButtons  AS buttonBar.

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
   
   DEFINE OUTPUT PARAMETER chrError AS CHARACTER NO-UNDO.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPickPackDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickPackDetails Procedure 
PROCEDURE pPickPackStationUnitsProcessedDetails :
/*------------------------------------------------------------------------------
 Purpose: to create Details form when user click on a particular field of the browse table to view more data.
 Notes:
------------------------------------------------------------------------------*/
   {webGetWebForm.i "packoutstationperformance_details_form"}

   ASSIGN chrDisplayFieldList  = "PickPackStationUnitsProcessedID,PickPackStationID,TimeInterval,"
                                  + "TargetUph,CurrentUph,OrdersToProcess".
                    
   PickPackStationUnitsProcessedDetailsForm = NEW dataForm("packoutstationperformance_details_form").
   PickPackStationUnitsProcessedDetailsForm:WebStream = STREAM WebStream:HANDLE.   
 
   PickPackStationUnitsProcessedDetailsForm:FormWidth  = 460.
   PickPackStationUnitsProcessedDetailsForm:FormHeight = 300.
   PickPackStationUnitsProcessedDetailsForm:FormTitle  = "Station Performance Details".
   PickPackStationUnitsProcessedDetailsForm:FormType   = "medium".

   PickPackStationUnitsProcessedDetailsForm:insertPaddingColumn(50).
   PickPackStationUnitsProcessedDetailsForm:insertColumn(90).
   PickPackStationUnitsProcessedDetailsForm:insertColumn(110).
   PickPackStationUnitsProcessedDetailsForm:insertColumn(90).
   PickPackStationUnitsProcessedDetailsForm:insertColumn(80).
   PickPackStationUnitsProcessedDetailsForm:insertColumn(70).
   PickPackStationUnitsProcessedDetailsForm:insertColumn(110).
   PickPackStationUnitsProcessedDetailsForm:insertColumn(90).

   PickPackStationUnitsProcessedDetailsForm:startRow().
   PickPackStationUnitsProcessedDetailsForm:insertLabel("ID").
   PickPackStationUnitsProcessedDetailsForm:insertTextField("PickPackStationUnitsProcessedID", "", 110, TRUE).

   PickPackStationUnitsProcessedDetailsForm:startRow().
   PickPackStationUnitsProcessedDetailsForm:insertLabel("PickPackStationID").
   PickPackStationUnitsProcessedDetailsForm:insertTextField("PickPackStationID", "", 110, TRUE).
   
   PickPackStationUnitsProcessedDetailsForm:startRow().
   PickPackStationUnitsProcessedDetailsForm:insertLabel("StationName").

   /* Inserting StationName field from pickpackstation Table */
   IF intSelectedPickPackStationID > 0 THEN
   DO:
      FIND FIRST PickPackStation WHERE PickPackStation.PickPackStationID = intSelectedPickPackStationID.
      IF AVAILABLE PickPackStation THEN 
         PickPackStationUnitsProcessedDetailsForm:insertTextField("StationName", PickPackStation.StationName, 150, TRUE).
   END.
   
   PickPackStationUnitsProcessedDetailsForm:startRow().
   PickPackStationUnitsProcessedDetailsForm:insertLabel("TimeInterval").
   PickPackStationUnitsProcessedDetailsForm:insertTextField("TimeInterval", "", 150, TRUE).
   
   PickPackStationUnitsProcessedDetailsForm:startRow().
   PickPackStationUnitsProcessedDetailsForm:insertLabel("TargetUph").
   PickPackStationUnitsProcessedDetailsForm:insertTextField("TargetUph", "", 110, TRUE).

   PickPackStationUnitsProcessedDetailsForm:startRow().
   PickPackStationUnitsProcessedDetailsForm:insertLabel("CurrentUph").
   PickPackStationUnitsProcessedDetailsForm:insertTextField("CurrentUph", "", 110, TRUE).

   PickPackStationUnitsProcessedDetailsForm:startRow().
   PickPackStationUnitsProcessedDetailsForm:insertLabel("OrdersToProcess").
   PickPackStationUnitsProcessedDetailsForm:insertTextField("OrdersToProcess", "", 110, TRUE).
   
   /* Add hidden fields */
   PickPackStationUnitsProcessedDetailsForm:insertHiddenField("pickpackstationunitsprocessed_browse_scroll", "").
   PickPackStationUnitsProcessedDetailsForm:insertHiddenField("form_name", "packoutstationperformance_details_form").
   PickPackStationUnitsProcessedDetailsForm:insertHiddenField("prog_name", "adPackoutStationPerformance.p").
   PickPackStationUnitsProcessedDetailsForm:insertHiddenField("filtering", "no").
     
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackStationUnitsProcessedDetailsForm}
    
   /* Add buttons */
   PickPackStationUnitsProcessedDetailsButton = NEW buttonBar().
   
   PickPackStationUnitsProcessedDetailsButton:addButton("packoutstationperformance_details_form_btn_cancel", 
                                                        fTL("Cancel"), 
                                                        "disablePopup('packoutstationperformance_details_form_popup');").
   
   PickPackStationUnitsProcessedDetailsButton:closeBar().  
   PickPackStationUnitsProcessedDetailsForm:FormButtons = PickPackStationUnitsProcessedDetailsButton.
   
   PickPackStationUnitsProcessedDetailsForm:endForm().
   PickPackStationUnitsProcessedDetailsForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-pPickPackFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickPackFilter Procedure
PROCEDURE pPickPackStationUnitsProcessedFilter:
/*------------------------------------------------------------------------------
 Purpose: To produce a pop up when user clicks on Fliter button which has a combo box from which user can select station ID specific to dates.
          
 Notes: The filter pop up is automatic when page reloads. If no Station Name is selected from the combo box an error is displayed. User 
        will have to proceed by selecting a Station of which detail has ot be viewed. Selection of date is not mandatory.
------------------------------------------------------------------------------*/   
    
   PickPackStationUnitsProcessedFilterForm = NEW dataForm("pickpackstationunitsprocessed_filter_form").
   PickPackStationUnitsProcessedFilterForm :WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   PickPackStationUnitsProcessedFilterForm :formWidth  = 350.
   PickPackStationUnitsProcessedFilterForm :formHeight = 200.
   PickPackStationUnitsProcessedFilterForm :formTitle  = "Select Station".
   PickPackStationUnitsProcessedFilterForm :formType   = "small_wide".

   /* Colummn style */
   PickPackStationUnitsProcessedFilterForm:insertPaddingColumn(30).
   PickPackStationUnitsProcessedFilterForm:insertColumn(120).
   PickPackStationUnitsProcessedFilterForm:insertColumn(120).
   PickPackStationUnitsProcessedFilterForm:insertColumn(20).
   PickPackStationUnitsProcessedFilterForm:insertColumn(4).
   PickPackStationUnitsProcessedFilterForm:insertColumn(80).
       
   /* Fields */  
   PickPackStationUnitsProcessedFilterForm:startRow().
   PickPackStationUnitsProcessedFilterForm:insertLabel(fTL("Station Name")).
   PickPackStationUnitsProcessedFilterForm:insertComboField("PickPackStationID", "", 120, TRUE).
   PickPackStationUnitsProcessedFilterForm:insertComboPairs("PickPackStationID", "0", "None Selected").
   
   /* Populating stationName combo */
   FOR EACH PickPackStation NO-LOCK 
      WHERE PickPackStation.Active = YES 
      BY PickPackStation.PickPackStationID:
          
      PickPackStationUnitsProcessedFilterForm:insertComboPairs("PickPackStationID", 
                                                               STRING(PickPackStation.PickPackStationID), 
                                                               PickPackStation.StationName).
   END.      

   PickPackStationUnitsProcessedFilterForm:startRow().
   PickPackStationUnitsProcessedFilterForm:insertLabel(fTL("TimeInterval")).
   PickPackStationUnitsProcessedFilterForm:insertDateField("TimeInterval", "", 120, TRUE).    

   /* Add hidden fields */
   PickPackStationUnitsProcessedFilterForm:insertHiddenField("form_name","pickpackstationunitsprocessed_filter_form").
   PickPackStationUnitsProcessedFilterForm:insertHiddenField("prog_name","adPackoutStationPerformance.p").  
   PickPackStationUnitsProcessedFilterForm:insertHiddenField("PickPackStationID", "").
   PickPackStationUnitsProcessedFilterForm:insertHiddenField("filtering", "no").
   
   {webGetHiddenFormFields.i PickPackStationUnitsProcessedFilterForm}
   
   /* Create buttons */
   PickPackStationUnitsProcessedFilterButtons = NEW buttonBar().

   PickPackStationUnitsProcessedFilterButtons:addButton("pickpackstationunitsprocessed_filter_form_btn_filter", 
                                                        fTL("Filter"), 
                                                        "filterPickPackStationUnitsProcessed();").
                                 
   PickPackStationUnitsProcessedFilterButtons:closeBar().
   PickPackStationUnitsProcessedFilterForm:formButtons = PickPackStationUnitsProcessedFilterButtons.
   
   /* Display form */
   PickPackStationUnitsProcessedFilterForm:endForm().
   PickPackStationUnitsProcessedFilterForm:displayForm().
   
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
                 
   ASSIGN chrPickPackStationID = get-value("PickPackStationID")
          intSelectedPickPackStationID = INTEGER(chrPickPackStationID)   
          chrPickPackStationUnitsProcessedID = get-value("PickPackStationUnitsProcessedID")
          intSelectedPickPackStUnitsProcessedID = INTEGER(chrPickPackStationUnitsProcessedID)
          chrScrollToPickPackRow = STRING(INTEGER(get-value("pickpackstationunitsprocessed_browse_scroll"))) + ";"          
          dateSelectedStation = DATE(get-value("TimeInterval"))
          logFilter = (get-value('filtering') <> "no").
    
    /* Process URL values */     
    IF logFilter THEN
       chrPopupFilters = 'viewPickPackFilter("pickpackstationunitsprocessed_filter_form");'.    
     
    IF  chrPickPackStationUnitsProcessedID <> "" THEN 
       chrPickPackStationUnitsProcessedSelectedRow = 'selectPickPackRow(document.getElementById("pickpackstationunitsprocessed_browse_row'+ 
                                   chrPickPackStationUnitsProcessedID + '"),"' + chrPickPackStationUnitsProcessedID +  '");'.
      
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("pickpackstationunitsprocessed_browse").scrollTop=' 
                             + chrScrollToPickPackRow 
                             + chrPickPackStationUnitsProcessedSelectedRow
                             + chrPopupFilters.                       
                               
   /* Mandatory include to set opening HTML tags and default files */
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Pick and Pack Performance".
   ThisPage:FrameTitle    = "Pick and Pack Station Performance".
   ThisPage:OnBodyLoad    = chrBodyLoad. 
         
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("pickpackstationperformance.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
 
   /******* Main Browser ********************/

   RUN pPickPackStationUnitsProcessedBrowse.
     
   RUN pPickPackStationUnitsProcessedDetails.
   
   RUN pPickPackStationUnitsProcessedFilter.
       
   /******* Pop-up Browsers and Forms ********/    

   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   
   DELETE OBJECT PickPackStationUnitsProcessedBrowseFrame   NO-ERROR.
   DELETE OBJECT PickPackStationUnitsProcessedDetailsForm   NO-ERROR.
   DELETE OBJECT PickPackStationUnitsProcessedBrowse        NO-ERROR.
   DELETE OBJECT PickPackStationUnitsProcessedBrowseButton  NO-ERROR.
   DELETE OBJECT PickPackStationUnitsProcessedDetailsButton NO-ERROR.
   
   /* Delete Filter objects */
   DELETE OBJECT PickPackStationUnitsProcessedFilterForm    NO-ERROR.
   DELETE OBJECT PickPackStationUnitsProcessedFilterButtons NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-process-web-request) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-web-request Procedure 
PROCEDURE pPickPackStationUnitsProcessedBrowse:
/*------------------------------------------------------------------------------
  Purpose:     To create browse form and display details from pickpackunitsprocessed table.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {webGetWebForm.i "packoutstationperformance_details_form"}

   /* Create browse table */  
   PickPackStationUnitsProcessedBrowse = NEW browseTable("pickpackstationunitsprocessed_browse").

   PickPackStationUnitsProcessedBrowse:BrowseWidth  = 965.
   PickPackStationUnitsProcessedBrowse:BrowseHeight = 460.
   PickPackStationUnitsProcessedBrowse:ExcelExport  = TRUE.
   PickPackStationUnitsProcessedBrowse:SessionID    = intGblSessionID.
   PickPackStationUnitsProcessedBrowse:WebStream    = STREAM WebStream:HANDLE.

   PickPackStationUnitsProcessedBrowse:insertColumn(fTL("ID"),              85,  "INTEGER",   "LEFT", FALSE).
   PickPackStationUnitsProcessedBrowse:insertColumn(fTL("StationName"),     120, "CHARACTER", "LEFT", FALSE).
   PickPackStationUnitsProcessedBrowse:insertColumn(fTL("TimeInterval"),    150, "CHARACTER", "LEFT", FALSE).
   PickPackStationUnitsProcessedBrowse:insertColumn(fTL("OrdersToProcess"), 130, "INTEGER",   "LEFT", FALSE).
   PickPackStationUnitsProcessedBrowse:insertColumn(fTL("TargetUph"),       100, "INTEGER",   "LEFT", FALSE).
   PickPackStationUnitsProcessedBrowse:insertColumn(fTL("CurrentUph"),      110, "INTEGER",   "LEFT", FALSE).     
     
   /* Body */
   PickPackStationUnitsProcessedBrowse:startBody().  
   
   PickPackStationUnitsProcessedLoop: 
   FOR EACH PickPackStationUnitsProcessed NO-LOCK 
      WHERE PickPackStationUnitsProcessed.PickPackStationID = intSelectedPickPackStationID 
      BY PickPackStationUnitsProcessed.TimeInterval DESC:
         
      FIND FIRST PickPackStation OF PickPackStationUnitsProcessed
         WHERE PickPackStation.Active = YES NO-LOCK NO-ERROR.   
      
      IF STRING(dateSelectedStation) <> ? THEN     
      DO:
        IF  fGetDate(PickPackStationUnitsProcessed.TimeInterval) <> dateSelectedStation THEN 
           NEXT PickPackStationUnitsProcessedLoop.
      END.
      
      PickPackStationUnitsProcessedBrowse:startRow(PickPackStationUnitsProcessed.PickPackStationUnitsProcessedID, "selectPickPackRow(this," + '"' + STRING(PickPackStationUnitsProcessed.PickPackStationUnitsProcessedID) + '"' + ");", "").     
      PickPackStationUnitsProcessedBrowse:insertData(PickPackStationUnitsProcessed.PickPackStationID,"LEFT").  
      PickPackStationUnitsProcessedBrowse:insertData(IF AVAILABLE PickPackStation THEN PickPackStation.StationName ELSE "","LEFT").  
      PickPackStationUnitsProcessedBrowse:insertData(fDisplayDate&Time(PickPackStationUnitsProcessed.TimeInterval,"d/m/y H:M:SS"),"LEFT").  
      PickPackStationUnitsProcessedBrowse:insertData(PickPackStationUnitsProcessed.OrdersToProcess,"LEFT").    
      PickPackStationUnitsProcessedBrowse:insertData(PickPackStationUnitsProcessed.TargetUph,"LEFT").     
      PickPackStationUnitsProcessedBrowse:insertData(PickPackStationUnitsProcessed.CurrentUph,"LEFT").         
      /* Add hidden fields */
      PickPackStationUnitsProcessedBrowse:insertHiddenData("PickPackVersionID",PickPackStationUnitsProcessed.VersionID).
      PickPackStationUnitsProcessedBrowse:endRow().
   END. /* FOR EACH PickPackStationUnitsProcessedLoop */
      
   PickPackStationUnitsProcessedBrowse:endTable().

   /* Create new frame */
   PickPackStationUnitsProcessedBrowseFrame = NEW pageFrame().
   PickPackStationUnitsProcessedBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PickPackStationUnitsProcessedBrowseFrame:formOpen("pickpackstationunitsprocessed_browse_form").

   /* Frame header */
   PickPackStationUnitsProcessedBrowseFrame:insertSpacer(5).
   PickPackStationUnitsProcessedBrowseFrame:frameOpen(985,500,"").

   PickPackStationUnitsProcessedBrowse:displayBrowse().

   /* Close frame */
   PickPackStationUnitsProcessedBrowseFrame:frameClose().
   PickPackStationUnitsProcessedBrowseFrame:insertSpacer(5).

   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("pickpackstationunitsprocessed_browse_scroll","").
   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("PickPackStationID",STRING(intSelectedPickPackStationID)).
   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("PickPackStationUnitsProcessedID","").
   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("TimeInterval",IF dateSelectedStation <> ? THEN STRING(dateSelectedStation) ELSE "").
   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("PickPackVersionID","").
   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("form_name", "pickpackstationunitsprocessed_browse_form").
   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("prog_name", "adPackoutStationPerformance.p").
   PickPackStationUnitsProcessedBrowseFrame:insertHiddenField("filtering", "no").

   PickPackStationUnitsProcessedBrowseFrame:formClose().
   
   /* Create buttons */
   PickPackStationUnitsProcessedBrowseButton = NEW buttonBar().
   PickPackStationUnitsProcessedBrowseButton:WebStream = STREAM WebStream:HANDLE.
   
   PickPackStationUnitsProcessedBrowseButton:addButton("pickpackstationunitsprocessed_browse_form_btn_filter", 
                                                       fTL("Filter"), 
                                                       "viewPickPackFilter('pickpackstationunitsprocessed_filter_form');","").
   PickPackStationUnitsProcessedBrowseButton:addButton("pickpackstationunitsprocessed_browse_form_btn_details", 
                                                       fTL("Details"), 
                                                       "viewPickPackDetails('packoutstationperformance_details_form');", 
                                                       (IF intSelectedPickPackStUnitsProcessedID > 0 THEN "" ELSE "disabled")).
   PickPackStationUnitsProcessedBrowseButton:addButton("pickpackstationunitsprocessed_browse_form_btn_excel",
                                                       fTL("Excel Export"),
                                                       "excelExport('" + STRING(intGblSessionID) + "_pickpackstationunitsprocessed_browse.xml')").
                                   
   PickPackStationUnitsProcessedBrowseButton:closeBar().
   PickPackStationUnitsProcessedBrowseButton:displayButtonBar().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

