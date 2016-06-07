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

DEFINE VARIABLE intSelectedNetworkAction             AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectNetworkActionRow            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkActionRow          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrNetworkActionID                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intFilteredActionPoint               AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrFilteredToteRef                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredShipOrderRef              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrStartTimestamp                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEndTimestamp                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE datStartDate                         AS DATE      NO-UNDO.
DEFINE VARIABLE chrStartHour                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrStartMins                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE datEndDate                           AS DATE      NO-UNDO.
DEFINE VARIABLE chrEndHour                           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEndMins                           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrActionPoint                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE logFiltering                         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrSelectedDate                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intDisplayCount                      AS INTEGER   NO-UNDO.

/* Definitions for System Options for Receiving */
{getReceivingOptions.i}

/* Objects */
DEFINE VARIABLE NetworkActionBrowseFrame           AS pageFrame.
DEFINE VARIABLE NetworkActionBrowse                AS browseTable.
DEFINE VARIABLE NetworkActionBrowseButtons         AS buttonBar.
DEFINE VARIABLE NetworkActionDetailsForm           AS dataForm.
DEFINE VARIABLE NetworkActionDetailsButtons        AS buttonBar.
DEFINE VARIABLE NetworkActionFilterForm            AS dataForm.
DEFINE VARIABLE NetworkActionFilterButtons         AS buttonBar.

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
PROCEDURE pNetworkActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "network_action_form"}
   
   NetworkActionBrowse = NEW browseTable("networkaction_browse").
   NetworkActionBrowse:BrowseWidth  = 965.
   NetworkActionBrowse:BrowseHeight = 455.
   NetworkActionBrowse:ExcelExport  = TRUE.
   NetworkActionBrowse:SessionID    = intGblSessionID.
   NetworkActionBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkActionBrowse:insertColumn(fTL("ActionID"), 100, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
/*   {webGetOptionalBrowseHeaders.i network_action}*/
   
   NetworkActionBrowse:insertColumn(fTL("Action Point"), 200, "CHARACTER", "left", FALSE).
   NetworkActionBrowse:insertColumn(fTL("Action Value"), 100, "CHARACTER", "left", FALSE).
   NetworkActionBrowse:insertColumn(fTL("Tote"),         200, "CHARACTER",         FALSE).
   NetworkActionBrowse:insertColumn(fTL("ShipOrder"),    200, "CHARACTER",         FALSE).
   NetworkActionBrowse:insertColumn(fTL("Created"),      140, "DATE",              FALSE).
   
   /*Body*/
   NetworkActionBrowse:startBody().
   
   IF logFiltering THEN
   FilterBlock:
   DO:
      IF datStartDate <> ? THEN
      DO:
         chrStartTimeStamp = fDisplayDateWithFormat(datStartDate,"YYYYMMDD", "") + chrStartHour + chrStartMins.
         IF datEndDate <> ? THEN
         DO:
            chrEndTimeStamp = fDisplayDateWithFormat(datEndDate,"YYYYMMDD", "") + chrEndHour + chrEndMins.
            
            DblDateLoop:
            FOR EACH NetworkAction NO-LOCK WHERE NetworkAction.Created >= chrStartTimestamp AND
                                                 NetworkAction.Created <= chrEndTimestamp
                                                 BY NetworkAction.Created DESCENDING:

               FIND FIRST NetworkActionPoint WHERE NetworkActionPoint.NetworkActionPointID = NetworkAction.NetworkActionPointID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkAction.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkAction.ShipOrderID NO-LOCK NO-ERROR.
               
               IF AVAILABLE Tote THEN
               DO:
                  IF chrFilteredToteRef <> "" AND Tote.ToteRef <> chrFilteredToteRef THEN
                     NEXT DblDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredToteRef <> "" THEN NEXT DblDateLoop.
               END.
               
               IF AVAILABLE ShipOrder THEN
               DO:
                  IF chrFilteredShipOrderRef <> "" AND ShipOrder.OrderRef <> chrFilteredShipOrderRef THEN
                     NEXT DblDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredShipOrderRef <> "" THEN NEXT DblDateLoop.
               END.
               
               IF AVAILABLE NetworkActionPoint THEN
               DO:      
                  IF intFilteredActionPoint <> 0 AND NetworkActionPoint.NetworkActionPointID <> intFilteredActionPoint THEN
                     NEXT DblDateLoop.
               END.
               ELSE
               DO:
                  IF intFilteredActionPoint <> 0 THEN NEXT DblDateLoop.
               END. 
               
               RUN pSetNetworkActionRow.
               
            END. /* FOR EACH NetworkRead ... */
         END. /* IF datEndDate <> ? THEN */
         ELSE DO:
            StartDateLoop:
            FOR EACH NetworkAction NO-LOCK WHERE NetworkAction.Created >= chrStartTimestamp
                                                 BY NetworkAction.Created DESCENDING:

               FIND FIRST NetworkActionPoint WHERE NetworkActionPoint.NetworkActionPointID = NetworkAction.NetworkActionPointID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkAction.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkAction.ShipOrderID NO-LOCK NO-ERROR.
                              
               IF AVAILABLE Tote THEN
               DO:
                  IF chrFilteredToteRef <> "" AND Tote.ToteRef <> chrFilteredToteRef THEN
                     NEXT StartDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredToteRef <> "" THEN NEXT StartDateLoop.
               END.
               
               IF AVAILABLE ShipOrder THEN
               DO:
                  IF chrFilteredShipOrderRef <> "" AND ShipOrder.OrderRef <> chrFilteredShipOrderRef THEN
                     NEXT StartDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredShipOrderRef <> "" THEN NEXT StartDateLoop.
               END.
               
               IF AVAILABLE NetworkActionPoint THEN
               DO:      
                  IF intFilteredActionPoint <> 0 AND NetworkActionPoint.NetworkActionPointID <> intFilteredActionPoint THEN
                     NEXT StartDateLoop.
               END.
               ELSE
               DO:
                  IF intFilteredActionPoint <> 0 THEN NEXT StartDateLoop.
               END. 
               
               RUN pSetNetworkActionRow.
               
            END. /* FOR EACH NetworkRead */
         END. /* ELSE (datEndDate <> ? THEN) */
      END. /* IF datStartDate <> ? THEN */
      ELSE DO: 
         IF datEndDate <> ? THEN
         DO:
            chrEndTimeStamp = fDisplayDateWithFormat(datEndDate,"YYYYMMDD", "") + chrEndHour + chrEndMins.

            EndDateLoop:
            FOR EACH NetworkAction NO-LOCK WHERE NetworkAction.Created <= chrEndTimestamp
                                                 BY NetworkAction.Created DESCENDING:
   
               FIND FIRST NetworkActionPoint WHERE NetworkActionPoint.NetworkActionPointID = NetworkAction.NetworkActionPointID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkAction.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkAction.ShipOrderID NO-LOCK NO-ERROR.
                              
               IF AVAILABLE Tote THEN
               DO:
                  IF chrFilteredToteRef <> "" AND Tote.ToteRef <> chrFilteredToteRef THEN
                     NEXT EndDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredToteRef <> "" THEN NEXT EndDateLoop.
               END.
               
               IF AVAILABLE ShipOrder THEN
               DO:
                  IF chrFilteredShipOrderRef <> "" AND ShipOrder.OrderRef <> chrFilteredShipOrderRef THEN
                     NEXT EndDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredShipOrderRef <> "" THEN NEXT EndDateLoop.
               END.
               
               IF AVAILABLE NetworkActionPoint THEN
               DO:      
                  IF intFilteredActionPoint <> 0 AND NetworkActionPoint.NetworkActionPointID <> intFilteredActionPoint THEN
                     NEXT EndDateLoop.
               END.
               ELSE
               DO:
                  IF intFilteredActionPoint <> 0 THEN NEXT EndDateLoop.
               END. 
               
               RUN pSetNetworkActionRow.
               
            END. /* FOR EACH NetworkRead ... */
         END. /* IF datEndDate <> ? THEN */
      
         ELSE DO:
            NoDateLoop:
            FOR EACH NetworkAction NO-LOCK BY NetworkAction.Created DESCENDING:
   
               FIND FIRST NetworkActionPoint WHERE NetworkActionPoint.NetworkActionPointID = NetworkAction.NetworkActionPointID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkAction.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkAction.ShipOrderID NO-LOCK NO-ERROR.
                              
               IF AVAILABLE Tote THEN
               DO:
                  IF chrFilteredToteRef <> "" AND Tote.ToteRef <> chrFilteredToteRef THEN
                     NEXT NoDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredToteRef <> "" THEN NEXT NoDateLoop.
               END.
               
               IF AVAILABLE ShipOrder THEN
               DO:
                  IF chrFilteredShipOrderRef <> "" AND ShipOrder.OrderRef <> chrFilteredShipOrderRef THEN
                     NEXT NoDateLoop.
               END.
               ELSE
               DO:
                  IF chrFilteredShipOrderRef <> "" THEN NEXT NoDateLoop.
               END.
               
               IF AVAILABLE NetworkActionPoint THEN
               DO:      
                  IF intFilteredActionPoint <> 0 AND NetworkActionPoint.NetworkActionPointID <> intFilteredActionPoint THEN
                     NEXT NoDateLoop.
               END.
               ELSE
               DO:
                  IF intFilteredActionPoint <> 0 THEN NEXT NoDateLoop.
               END. 
               
               RUN pSetNetworkActionRow.
               
            END. /* FOR EACH NetworkRead ... */
         END. /* ELSE (IF datEndDate <> ? THEN) */
      END. /* ELSE DO (IF datStartDate <> ? THEN) */
         
   END. /*IF logFiltering THEN*/
   ELSE
   DO:
      intDisplayCount = 0.
      MesssageLoop:
      FOR EACH NetworkAction NO-LOCK BY NetworkAction.Created DESCENDING:
         IF intDisplayCount >= 500 THEN LEAVE MesssageLoop.
         RUN pSetNetworkActionRow.      
         intDisplayCount = intDisplayCount + 1.             
      END. /*FOR EACH Inbound NO-LOCK */
   END.
   
   NetworkActionBrowse:endTable().
   
   /* Create a new frame */
   NetworkActionBrowseFrame = NEW pageFrame().
   NetworkActionBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkActionBrowseFrame:formOpen("network_action_browse_form").
   
   /* Start the Frame Header */
   NetworkActionBrowseFrame:insertSpacer(5).
   NetworkActionBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkActionBrowse:displayBrowse().  
   
   /* This creates an Excel File */
   /* No point in adding this as there's no room for a button to display it */
   /*InboundBrowse:generateExcel(intGblSessionID, "inbound_browse").  */

   /* End the Frame Header */
   NetworkActionBrowseFrame:frameClose().
   NetworkActionBrowseFrame:insertSpacer(10).
   
   NetworkActionBrowseFrame:insertHiddenField("network_action_browse_scroll","").
   NetworkActionBrowseFrame:insertHiddenField("ActionID","").
   NetworkActionBrowseFrame:insertHiddenField("FilteredActionPoint", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionBrowseFrame}
   
   NetworkActionBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkActionBrowseButtons = NEW buttonBar().
   NetworkActionBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   NetworkActionBrowseButtons:addButton("network_action_browse_form_btn_filter",
                                  fTL("Filter"),
                                  "viewNetworkActionFilter('network_action_filter_form');").
   
   NetworkActionBrowseButtons:addButton("network_action_browse_form_btn_details", 
                                  fTL("Details"), 
                                  "viewNetworkActionDetails('network_action_details_form');",
                                  "Disabled").
                                  
   NetworkActionBrowseButtons:addButton("network_action_browse_form_btn_excel",
                                  fTL("Excel Export"),
                                  "excelExport('" + STRING(intGblSessionID) + "_networkaction_browse.xml')").

   NetworkActionBrowseButtons:closeBar().  
   NetworkActionBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundDetails Procedure 
PROCEDURE pNetworkActionDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "network_action_details_form"}

   ASSIGN chrDisplayFieldList  = "NetworkActionID,NetworkActionPointID,ActionValue,ReturnMessage,"
                                    + "CreatedDate,CreatedHour,CreatedMins,CreatedSecs,CreatedMs,"
                                    + "CompletedDate,CompletedHour,CompletedMins,CompletedSecs,CompletedMs,"
                                    + "ToteID,ShipOrderID,ToteLineJourneyID"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkActionDetailsForm = NEW dataForm("network_action_details_form").
   NetworkActionDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkActionDetailsForm:FormAction  = "".
   
   /* Setup */
   NetworkActionDetailsForm:FormWidth   = 460.
   NetworkActionDetailsForm:FormHeight  = 300.
   NetworkActionDetailsForm:FormTitle   = "Network Action Details".
   NetworkActionDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   NetworkActionDetailsForm:insertPaddingColumn(20).
   NetworkActionDetailsForm:insertColumn(130).
   NetworkActionDetailsForm:insertColumn(128).
   NetworkActionDetailsForm:insertColumn(20).
   NetworkActionDetailsForm:insertColumn(4).
   NetworkActionDetailsForm:insertColumn(4).
   NetworkActionDetailsForm:insertColumn(4).
   NetworkActionDetailsForm:insertColumn(4).
   NetworkActionDetailsForm:insertColumn(4).
   
   /* Fields */
   NetworkActionDetailsForm:startRow().
   NetworkActionDetailsForm:insertLabel("Action ID").
   NetworkActionDetailsForm:insertTextField("NetworkActionID", "", 250, TRUE).  
   
   
   NetworkActionDetailsForm:startRow().  
   NetworkActionDetailsForm:insertLabel("Action Point").
   NetworkActionDetailsForm:insertComboField("NetworkActionPointID", "", 250, TRUE).
   FOR EACH NetworkActionPoint NO-LOCK: 
      NetworkActionDetailsForm:insertComboPairs("NetworkActionPointID", STRING(NetworkActionPoint.NetworkActionPointID) , 
                                                NetworkActionPoint.NetworkActionPointName).
   END.
   
   NetworkActionDetailsForm:startRow().  
   NetworkActionDetailsForm:insertLabel("Action").
   NetworkActionDetailsForm:insertTextField("ActionValue", "", 250, TRUE). 
   
   NetworkActionDetailsForm:startRow().  
   NetworkActionDetailsForm:insertLabel("Tote Ref").
   NetworkActionDetailsForm:insertComboField("ToteID", "", 250, TRUE).
   FOR EACH Tote NO-LOCK: 
      NetworkActionDetailsForm:insertComboPairs("ToteID", STRING(Tote.ToteID), Tote.ToteRef).
   END.
   
   NetworkActionDetailsForm:startRow().  
   NetworkActionDetailsForm:insertLabel("ShipOrder Ref").
   NetworkActionDetailsForm:insertComboField("ShipOrderID", "", 250, TRUE).
   FOR EACH ShipOrder NO-LOCK: 
      NetworkActionDetailsForm:insertComboPairs("ShipOrderID", STRING(ShipOrder.ShipOrderID), ShipOrder.OrderRef).
   END.
   
   NetworkActionDetailsForm:startRow().  
   NetworkActionDetailsForm:insertLabel("Journey ID").
   NetworkActionDetailsForm:insertTextField("ToteLineJourneyID", "", 250, TRUE).
   
   NetworkActionDetailsForm:startRow().
   NetworkActionDetailsForm:insertLabel("Return Message").
   NetworkActionDetailsForm:insertTextAreaField("ReturnMessage", "", 250, 3, TRUE). 
   
   
   NetworkActionDetailsForm:startRow().
   NetworkActionDetailsForm:insertLabel("Created").
   NetworkActionDetailsForm:insertDateField("CreatedDate", "", 120, TRUE).  
   /* Time fields have no label */
   NetworkActionDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkActionDetailsForm:insertLabel(":").
   NetworkActionDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   NetworkActionDetailsForm:insertLabel(":").
   NetworkActionDetailsForm:insertTextField("CreatedSecs", "00", 18, TRUE).
   NetworkActionDetailsForm:insertLabel(":").
   NetworkActionDetailsForm:insertTextField("CreatedMs", "00", 24, TRUE).   
   
   NetworkActionDetailsForm:startRow().
   NetworkActionDetailsForm:insertLabel("Completed").
   NetworkActionDetailsForm:insertDateField("CompletedDate", "", 120, TRUE).  
   /* Time fields have no label */
   NetworkActionDetailsForm:insertTextField("CompletedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkActionDetailsForm:insertLabel(":").
   NetworkActionDetailsForm:insertTextField("CompletedMins", "00", 18, TRUE).
   NetworkActionDetailsForm:insertLabel(":").
   NetworkActionDetailsForm:insertTextField("CompletedSecs", "00", 18, TRUE).
   NetworkActionDetailsForm:insertLabel(":").
   NetworkActionDetailsForm:insertTextField("CompletedMs", "00", 24, TRUE).
   
   /* Add Hidden Fields*/
   NetworkActionDetailsForm:insertHiddenField("network_action_browse_scroll","").
   NetworkActionDetailsForm:insertHiddenField("form_name","network_action_details_form").
   NetworkActionDetailsForm:insertHiddenField("prog_name","adNetworkActionEnquiry.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionDetailsForm}
   
   /* Create Button Bar */
   NetworkActionBrowseButtons = NEW buttonBar().
   
   NetworkActionBrowseButtons:addButton("network_action_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('network_action_details_form_popup');").
   
   NetworkActionBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionDetailsForm:FormButtons = NetworkActionBrowseButtons.
   NetworkActionDetailsForm:endForm(). 
   
   NetworkActionDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkActionDetailsForm:getErrors().  */
   
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
PROCEDURE pNetworkActionFilter :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   NetworkActionFilterForm = NEW dataForm("network_action_filter_form").
   NetworkActionFilterForm:WebStream  = STREAM WebStream:HANDLE.
   NetworkActionFilterForm:FormAction = "adNetworkActionEnquiry.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   NetworkActionFilterForm:FormWidth   = 350.
   NetworkActionFilterForm:FormHeight  = 200.
   NetworkActionFilterForm:FormTitle   = "Network Action Filter".
   NetworkActionFilterForm:FormType    = "small_wide".

   /* Column Layout */
   NetworkActionFilterForm:insertPaddingColumn(10).
   NetworkActionFilterForm:insertColumn(80).
   NetworkActionFilterForm:insertColumn(169).
   NetworkActionFilterForm:insertColumn(20).
   NetworkActionFilterForm:insertColumn(2).

   /* Fields */
   NetworkActionFilterForm:startRow().
   NetworkActionFilterForm:insertLabel(fTL("Action Point")).
/*   NetworkActionFilterForm:insertTextField("FilteredActionPoint", intFilteredActionPoint, 210, TRUE).*/
   NetworkActionFilterForm:insertComboField("FilteredActionPoint", "", 180, TRUE).
   NetworkActionFilterForm:insertComboPairs("FilteredActionPoint", "0" , "None Selected").  
   FOR EACH NetworkActionPoint NO-LOCK: /*idx=NetworkActionPointID*/
      NetworkActionFilterForm:insertComboPairs("FilteredActionPoint", STRING(NetworkActionPoint.NetworkActionPointID) , NetworkActionPoint.NetworkActionPointName).
   END.
   
   NetworkActionFilterForm:startRow().
   NetworkActionFilterForm:insertLabel(fTL("Tote Ref")).
   NetworkActionFilterForm:insertTextField("FilteredToteRef", "", 210, TRUE).

   NetworkActionFilterForm:startRow().
   NetworkActionFilterForm:insertLabel(fTL("Order Ref")).
   NetworkActionFilterForm:insertTextField("FilteredShipOrderRef", "", 210, TRUE).

   NetworkActionFilterForm:startRow().
   NetworkActionFilterForm:insertLabel("Start Date").
   NetworkActionFilterForm:insertDateField("StartDate", "", 160, TRUE).  
   /* Time fields have no label */
   NetworkActionFilterForm:insertTextField("StartHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkActionFilterForm:insertLabel(":").
   NetworkActionFilterForm:insertTextField("StartMins", "00", 18, TRUE).  
   
   NetworkActionFilterForm:startRow().
   NetworkActionFilterForm:insertLabel("End Date").
   NetworkActionFilterForm:insertDateField("EndDate", "", 160, TRUE).  
   /* Time fields have no label */
   NetworkActionFilterForm:insertTextField("EndHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkActionFilterForm:insertLabel(":").
   NetworkActionFilterForm:insertTextField("EndMins", "00", 18, TRUE).  


   /* Add Hidden Fields*/
   NetworkActionFilterForm:insertHiddenField("form_name", "network_action_filter_form").
   NetworkActionFilterForm:insertHiddenField("prog_name", "adNetworkActionEnquiry.p").
   NetworkActionFilterForm:insertHiddenField("filtering", "yes").
/*   NetworkActionFilterForm:insertHiddenField("FilteredActionPoint", intFilteredActionPoint).*/

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionFilterForm}

   /* Create Button Bar */
   NetworkActionFilterButtons = NEW buttonBar().

   NetworkActionFilterButtons:addButton("network_action_filter_form_btn_search",
                                   fTL("Filter"),
                                   "filterNetworkAction('network_action_filter_form');").

   NetworkActionFilterButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   NetworkActionFilterForm:FormButtons = NetworkActionFilterButtons.

   NetworkActionFilterForm:endForm().
   NetworkActionFilterForm:displayForm().

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
   ASSIGN chrNetworkActionID          = get-value("ActionID")
          intSelectedNetworkAction    = INT(chrNetworkActionID)
          intFilteredActionPoint      = INT(get-value("FilteredActionPoint"))
          chrFilteredToteRef          = get-value("FilteredToteRef")
          chrFilteredShipOrderRef     = get-value("FilteredShipOrderRef")
          logFiltering                = (get-value("filtering") = "yes")
          datStartDate                = DATE(get-value('StartDate'))
          datEndDate                  = DATE(get-value('EndDate'))
          chrStartHour                = get-value("StartHour")
          chrStartMins                = get-value("StartMins")
          chrEndHour                  = get-value("EndHour")
          chrEndMins                  = get-value("EndMins")
          chrScrollToNetworkActionRow = STRING(INT(get-value("network_action_browse_scroll"))) + ";" NO-ERROR.
  
   IF intFilteredActionPoint  = 0  AND
      chrFilteredToteRef      = "" AND
      chrFilteredShipOrderRef = "" AND
      datStartDate            = ?  AND
      datEndDate              = ?  THEN
      ASSIGN logFiltering     = FALSE.

   IF intSelectedNetworkAction > 0 THEN
      chrSelectNetworkActionRow = 'selectNetworkActionRow(document.getElementById("network_action_browse_row_' + chrNetworkActionID + '"),"'
                              + chrNetworkActionID + '","adNetworkActionEnquiry.p' + '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("network_action_browse").scrollTop=' + chrScrollToNetworkActionRow 
                             + chrSelectNetworkActionRow.
     
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Network Action Enquiry".
   ThisPage:FrameTitle = "Network Action Enquiry".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("networkactionenquiry.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
/*   IF logGblDebugging THEN                                                                                                    */
/*      fLog("GblSessionID:" + STRING(intGblSessionID) + " GblUserID:" + STRING(intGblUserID) + " GblUserName:" + chrGblUserName*/
/*                           + " GblLanguageID:" + STRING(intGblLanguageID) + " GblEnvironment:" + chrGblEnvironment).          */
   
   /******* Main Browser ********************/
   RUN pNetworkActionBrowse.
   
   /******* Popup Browsers and Forms ********/    
/*   FIND FIRST Inbound WHERE Inbound.InboundID = intSelectedInbound NO-LOCK NO-ERROR.*/
   
   RUN pNetworkActionDetails.
   RUN pNetworkActionFilter.
   
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

&IF DEFINED(EXCLUDE-pSetNetworkActionRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetNetworkActionRow Procedure
PROCEDURE pSetNetworkActionRow:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
         FIND FIRST NetworkActionPoint WHERE NetworkActionPoint.NetworkActionPointID = NetworkAction.NetworkActionPointID NO-LOCK NO-ERROR.
         FIND FIRST Tote WHERE Tote.ToteID =  NetworkAction.ToteID NO-LOCK NO-ERROR.
         FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkAction.ShipOrderID NO-LOCK NO-ERROR.
                        
         NetworkActionBrowse:startRow(NetworkAction.NetworkActionID, "selectNetworkActionRow(this," + '"' + STRING(NetworkAction.NetworkActionID) + '"' + ");", "").
         NetworkActionBrowse:insertData(NetworkAction.NetworkActionID, "left").
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
/*         {webGetOptionalBrowseFields.i network_action}*/
         
         NetworkActionBrowse:insertData((IF AVAILABLE NetworkActionPoint THEN NetworkActionPoint.NetworkActionPointName ELSE ""), "left").
         NetworkActionBrowse:insertData(NetworkAction.ActionValue, "left").
         NetworkActionBrowse:insertData((IF AVAILABLE Tote THEN Tote.ToteRef ELSE ""), "left").
         NetworkActionBrowse:insertData((IF AVAILABLE ShipOrder THEN ShipOrder.OrderRef ELSE ""), "left").
         NetworkActionBrowse:insertData(fDisplayDate&Time(NetworkAction.Created,"d/m/y H:M:S")).
         
         NetworkActionBrowse:endRow().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

