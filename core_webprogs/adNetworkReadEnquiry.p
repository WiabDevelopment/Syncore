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
     
  Changes: 02/11/2015  ML  Changed adNetworkReadEnquiry.p to adNetworkReadEnquiry.p 

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

DEFINE VARIABLE intSelectedNetworkRead           AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectNetworkReadRow          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkReadRow        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrNetworkReadID                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredNetworkRead           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredToteRef               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredShipOrderRef          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrStartTimestamp                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEndTimestamp                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE datStartDate                     AS DATE      NO-UNDO.
DEFINE VARIABLE chrStartHour                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrStartMins                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE datEndDate                       AS DATE      NO-UNDO.
DEFINE VARIABLE chrEndHour                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrEndMins                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrToteRef                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrJourneyID                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE intNetworkReaderID               AS INTEGER   NO-UNDO.
DEFINE VARIABLE logFiltering                     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrSelectedDate                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE intDisplayCount                  AS INTEGER   NO-UNDO.

/* Definitions for System Options for Receiving */
{getReceivingOptions.i}

/* Objects */
DEFINE VARIABLE NetworkReadBrowseFrame           AS pageFrame.
DEFINE VARIABLE NetworkReadBrowse                AS browseTable.
DEFINE VARIABLE NetworkReadBrowseButtons         AS buttonBar.
DEFINE VARIABLE NetworkReadDetailsForm           AS dataForm.
DEFINE VARIABLE NetworkReadDetailsButtons        AS buttonBar.
DEFINE VARIABLE NetworkReadFilterForm            AS dataForm.
DEFINE VARIABLE NetworkReadFilterButtons         AS buttonBar.

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
PROCEDURE pNetworkReadBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "network_read_form"}
   
   NetworkReadBrowse = NEW browseTable("networkread_browse").
   NetworkReadBrowse:BrowseWidth  = 965.
   NetworkReadBrowse:BrowseHeight = 455.
   NetworkReadBrowse:ExcelExport  = TRUE.
   NetworkReadBrowse:SessionID    = intGblSessionID.
   NetworkReadBrowse:WebStream = STREAM WebStream:HANDLE.
   
   
   
   /* Add in the ID as first Column */
   NetworkReadBrowse:insertColumn(fTL("Read ID"), 75, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
/*   {webGetOptionalBrowseHeaders.i Device_Messaging}*/
   
   NetworkReadBrowse:insertColumn(fTL("Reader"),       60, "CHARACTER", "left", FALSE).
   NetworkReadBrowse:insertColumn(fTL("Reader Type"), 130, "CHARACTER", "left", FALSE).
   NetworkReadBrowse:insertColumn(fTL("Tote"),        100, "CHARACTER",         FALSE).
   NetworkReadBrowse:insertColumn(fTL("ShipOrder"),   150, "CHARACTER",         FALSE).
   NetworkReadBrowse:insertColumn(fTL("Sent"),        200, "CHARACTER",         FALSE).
   NetworkReadBrowse:insertColumn(fTL("Received"),    100, "CHARACTER",         FALSE).
   NetworkReadBrowse:insertColumn(fTL("Created"),     130, "DATE",              FALSE).
   
   /*Body*/
   NetworkReadBrowse:startBody().
   
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
            FOR EACH NetworkRead NO-LOCK WHERE NetworkRead.Created >= chrStartTimestamp AND
                                               NetworkRead.Created <= chrEndTimestamp
                                               BY NetworkRead.Created DESCENDING:

               FIND FIRST NetworkReader WHERE NetworkReader.NetworkReaderID = NetworkRead.NetworkReaderID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkRead.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkRead.ShipOrderID NO-LOCK NO-ERROR.
               
               
               
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
               
               IF AVAILABLE NetworkReader THEN
               DO:      
                  IF intNetworkReaderID <> 0 AND NetworkReader.NetworkReaderID <> intNetworkReaderID THEN
                     NEXT DblDateLoop.
               END.
               ELSE
               DO:
                  IF intNetworkReaderID <> 0 THEN NEXT DblDateLoop.
               END. 
                  
               IF NetworkRead.ToteLineJourneyID <> INTEGER(chrJourneyID) AND chrJourneyID <> "" THEN
                  NEXT DblDateLoop. 
               
               RUN pSetNetworkReadRow.
               
            END. /* FOR EACH NetworkRead ... */
         END. /* IF datEndDate <> ? THEN */
         ELSE DO:
            StartDateLoop:
            FOR EACH NetworkRead NO-LOCK WHERE NetworkRead.Created >= chrStartTimestamp
                                               BY NetworkRead.Created DESCENDING:

               FIND FIRST NetworkReader WHERE NetworkReader.NetworkReaderID = NetworkRead.NetworkReaderID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkRead.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkRead.ShipOrderID NO-LOCK NO-ERROR.
               
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
               
               IF AVAILABLE NetworkReader THEN
               DO:      
                  IF intNetworkReaderID <> 0 AND NetworkReader.NetworkReaderID <> intNetworkReaderID THEN
                     NEXT StartDateLoop.
               END.
               ELSE
               DO:
                  IF intNetworkReaderID <> 0 THEN NEXT StartDateLoop.
               END.  
                  
               IF NetworkRead.ToteLineJourneyID <> INTEGER(chrJourneyID) AND chrJourneyID <> "" THEN
                  NEXT StartDateLoop.
               
               RUN pSetNetworkReadRow.
               
            END. /* FOR EACH NetworkRead */
         END. /* ELSE (datEndDate <> ? THEN) */
      END. /* IF datStartDate <> ? THEN */
      ELSE DO: 
         IF datEndDate <> ? THEN
         DO:
            chrEndTimeStamp = fDisplayDateWithFormat(datEndDate,"YYYYMMDD", "") + chrEndHour + chrEndMins.

            EndDateLoop:
            FOR EACH NetworkRead NO-LOCK WHERE NetworkRead.Created <= chrEndTimestamp
                                               BY NetworkRead.Created DESCENDING:
   
               FIND FIRST NetworkReader WHERE NetworkReader.NetworkReaderID = NetworkRead.NetworkReaderID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkRead.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkRead.ShipOrderID NO-LOCK NO-ERROR.
               
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
               
               IF AVAILABLE NetworkReader THEN
               DO:      
                  IF intNetworkReaderID <> 0 AND NetworkReader.NetworkReaderID <> intNetworkReaderID THEN
                     NEXT EndDateLoop.
               END.
               ELSE
               DO:
                  IF intNetworkReaderID <> 0 THEN NEXT EndDateLoop.
               END.
                  
               IF NetworkRead.ToteLineJourneyID <> INTEGER(chrJourneyID) AND chrJourneyID <> "" THEN
                  NEXT EndDateLoop.
               
               RUN pSetNetworkReadRow.
               
            END. /* FOR EACH NetworkRead ... */
         END. /* IF datEndDate <> ? THEN */
      
         ELSE DO:
            NoDateLoop:
            FOR EACH NetworkRead NO-LOCK BY NetworkRead.Created DESCENDING:
   
               FIND FIRST NetworkReader WHERE NetworkReader.NetworkReaderID = NetworkRead.NetworkReaderID NO-LOCK NO-ERROR.
               FIND FIRST Tote WHERE Tote.ToteID =  NetworkRead.ToteID NO-LOCK NO-ERROR.
               FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = NetworkRead.ShipOrderID NO-LOCK NO-ERROR.
               
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
               
               IF AVAILABLE NetworkReader THEN
               DO:      
                  IF intNetworkReaderID <> 0 AND NetworkReader.NetworkReaderID <> intNetworkReaderID THEN
                     NEXT NoDateLoop.
               END.
               ELSE
               DO:
                  IF intNetworkReaderID <> 0 THEN NEXT NoDateLoop.
               END.
                  
               IF NetworkRead.ToteLineJourneyID <> INTEGER(chrJourneyID) AND chrJourneyID <> "" THEN
                  NEXT NoDateLoop.
               
               RUN pSetNetworkReadRow.
               
            END. /* FOR EACH NetworkRead ... */
         END. /* ELSE (IF datEndDate <> ? THEN) */
      END. /* ELSE DO (IF datStartDate <> ? THEN) */
         
   END. /*IF logFiltering THEN*/
   ELSE
   DO:
      MesssageLoop:
      FOR EACH NetworkRead NO-LOCK BY NetworkRead.Created DESCENDING:
         IF intDisplayCount >= 500 THEN LEAVE MesssageLoop.
         RUN pSetNetworkReadRow.     
         intDisplayCount = intDisplayCount + 1.             
      END. /*FOR EACH Inbound NO-LOCK */
   END.

   NetworkReadBrowse:endTable().
   
   /* Create a new frame */
   NetworkReadBrowseFrame = NEW pageFrame().
   NetworkReadBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   
   /* Insert a form */
   NetworkReadBrowseFrame:formOpen("network_read_browse_form").
   
   /* Start the Frame Header */
   NetworkReadBrowseFrame:insertSpacer(5).
   NetworkReadBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkReadBrowse:displayBrowse().  
   
   /* This creates an Excel File */
   /* No point in adding this as there's no room for a button to display it */
   /*InboundBrowse:generateExcel(intGblSessionID, "inbound_browse").  */

   /* End the Frame Header */
   NetworkReadBrowseFrame:frameClose().
   NetworkReadBrowseFrame:insertSpacer(10).
   
   NetworkReadBrowseFrame:insertHiddenField("network_read_browse_scroll","").
   NetworkReadBrowseFrame:insertHiddenField("MessageID","").
   NetworkReadBrowseFrame:insertHiddenField("FilteredNetworkRead", chrFilteredNetworkRead).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReadBrowseFrame}
   
   NetworkReadBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkReadBrowseButtons = NEW buttonBar().
   NetworkReadBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   NetworkReadBrowseButtons:addButton("network_read_browse_form_btn_filter",
                                  fTL("Filter"),
                                  "viewNetworkReadFilter('network_read_filter_form');").
   
   NetworkReadBrowseButtons:addButton("network_read_browse_form_btn_details", 
                                  fTL("Details"), 
                                  "viewNetworkReadDetails('network_read_details_form');",
                                  "Disabled").
                                  
   NetworkReadBrowseButtons:addButton("network_read_browse_form_btn_excel",
                                  fTL("Excel Export"),
                                  "excelExport('" + STRING(intGblSessionID) + "_networkread_browse.xml')").
   
   NetworkReadBrowseButtons:closeBar().  
   NetworkReadBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundDetails Procedure 
PROCEDURE pNetworkReadDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "network_read_details_form"}

   ASSIGN chrDisplayFieldList  = "NetworkReadID,NetworkReaderID,ToteLineJourneyID,ToteID,MessageSent,MessageReceived,"
                                    + "CreatedDate,CreatedHour,CreatedMins,CreatedSecs,CreatedMs,ShipOrderID,ToteID"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkReadDetailsForm = NEW dataForm("network_read_details_form").
   NetworkReadDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkReadDetailsForm:FormAction  = "".
   
   /* Setup */
   NetworkReadDetailsForm:FormWidth   = 460.
   NetworkReadDetailsForm:FormHeight  = 300.
   NetworkReadDetailsForm:FormTitle   = "Network Read Details".
   NetworkReadDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   NetworkReadDetailsForm:insertPaddingColumn(20).
   NetworkReadDetailsForm:insertColumn(130).
   NetworkReadDetailsForm:insertColumn(128).
   NetworkReadDetailsForm:insertColumn(20).
   NetworkReadDetailsForm:insertColumn(4).
   NetworkReadDetailsForm:insertColumn(4).
   NetworkReadDetailsForm:insertColumn(4).
   NetworkReadDetailsForm:insertColumn(4).
   NetworkReadDetailsForm:insertColumn(4).
   
   
   /* Fields */
   NetworkReadDetailsForm:startRow().
   NetworkReadDetailsForm:insertLabel("Network Read ID").
   NetworkReadDetailsForm:insertTextField("NetworkReadID", "", 250, TRUE).  
   
   NetworkReadDetailsForm:startRow().  
   NetworkReadDetailsForm:insertLabel("Network Reader").
   NetworkReadDetailsForm:insertComboField("NetworkReaderID", "0", 250, TRUE).

   FOR EACH NetworkReader NO-LOCK: 
      NetworkReadDetailsForm:insertComboPairs("NetworkReaderID", STRING(NetworkReader.NetworkReaderID) , NetworkReader.NetworkReaderName).
   END.
   
   NetworkReadDetailsForm:startRow().  
   NetworkReadDetailsForm:insertLabel("Journey ID").
   NetworkReadDetailsForm:insertTextField("ToteLineJourneyID", "", 250, TRUE). 
   
   NetworkReadDetailsForm:startRow().  
   NetworkReadDetailsForm:insertLabel("Tote Ref").
   NetworkReadDetailsForm:insertComboField("ToteID", "", 250, TRUE).
   FOR EACH Tote NO-LOCK: 
      NetworkReadDetailsForm:insertComboPairs("ToteID", STRING(Tote.ToteID), Tote.ToteRef).
   END.
   
   NetworkReadDetailsForm:startRow().  
   NetworkReadDetailsForm:insertLabel("ShipOrder Ref").
   NetworkReadDetailsForm:insertComboField("ShipOrderID", "", 250, TRUE).
   FOR EACH ShipOrder NO-LOCK: 
      NetworkReadDetailsForm:insertComboPairs("ShipOrderID", STRING(ShipOrder.ShipOrderID), ShipOrder.OrderRef).
   END.
   
   NetworkReadDetailsForm:startRow().
   NetworkReadDetailsForm:insertLabel("MessageSent").
   NetworkReadDetailsForm:insertTextAreaField("MessageSent", "", 250, 3, TRUE). 
      
   NetworkReadDetailsForm:startRow().
   NetworkReadDetailsForm:insertLabel("MessageReceived").
   NetworkReadDetailsForm:insertTextAreaField("MessageReceived", "", 250, 3, TRUE). 
   
   
   NetworkReadDetailsForm:startRow().
   NetworkReadDetailsForm:insertLabel("Created").
   NetworkReadDetailsForm:insertDateField("CreatedDate", "", 120, TRUE).  
   /* Time fields have no label */
   NetworkReadDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkReadDetailsForm:insertLabel(":").
   NetworkReadDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).  
   NetworkReadDetailsForm:insertLabel(":").
   NetworkReadDetailsForm:insertTextField("CreatedSecs", "00", 18, TRUE).
   NetworkReadDetailsForm:insertLabel(":").
   NetworkReadDetailsForm:insertTextField("CreatedMs", "00", 24, TRUE).
   
   
   /* Add Hidden Fields*/
   NetworkReadDetailsForm:insertHiddenField("network_read_browse_scroll","").
   NetworkReadDetailsForm:insertHiddenField("form_name","network_read_details_form").
   NetworkReadDetailsForm:insertHiddenField("prog_name","adNetworkReadEnquiry.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReadDetailsForm}
   
   /* Create Button Bar */
   NetworkReadBrowseButtons = NEW buttonBar().
   
   NetworkReadBrowseButtons:addButton("network_read_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('network_read_details_form_popup');").
   
   NetworkReadBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkReadDetailsForm:FormButtons = NetworkReadBrowseButtons.
   NetworkReadDetailsForm:endForm(). 
   
   NetworkReadDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkReadDetailsForm:getErrors().  */
   
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
PROCEDURE pNetworkReadFilter :
/*------------------------------------------------------------------------------
  Purpose:  
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   NetworkReadFilterForm = NEW dataForm("network_read_filter_form").
   NetworkReadFilterForm:WebStream  = STREAM WebStream:HANDLE.
   NetworkReadFilterForm:FormAction = "adNetworkReadEnquiry.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   NetworkReadFilterForm:FormWidth   = 350.
   NetworkReadFilterForm:FormHeight  = 200.
   NetworkReadFilterForm:FormTitle   = "Network Read Filter".
   NetworkReadFilterForm:FormType    = "small_wide".

   /* Column Layout */
   NetworkReadFilterForm:insertPaddingColumn(10).
   NetworkReadFilterForm:insertColumn(80).
   NetworkReadFilterForm:insertColumn(169).
   NetworkReadFilterForm:insertColumn(20).
   NetworkReadFilterForm:insertColumn(2).

   /* Fields */
   NetworkReadFilterForm:startRow().
   NetworkReadFilterForm:insertLabel(fTL("Reader Ref")).
/*   NetworkReadFilterForm:insertTextField("NetworkReaderRef", chrFilteredNetworkRead, 210, TRUE).*/
   NetworkReadFilterForm:insertComboField("NetworkReaderRef", "", 180, TRUE).
   NetworkReadFilterForm:insertComboPairs("NetworkReaderRef", "0" , "None Selected").  
   FOR EACH NetworkReader NO-LOCK: /*idx=NetworkReaderID*/
      NetworkReadFilterForm:insertComboPairs("NetworkReaderRef", STRING(NetworkReader.NetworkReaderID) , NetworkReader.NetworkReaderName).
   END.

/*   NetworkReadFilterForm:startRow().                                      */
/*   NetworkReadFilterForm:insertLabel(fTL("Tote Ref")).                    */
/*   NetworkReadFilterForm:insertTextField("ToteID", chrToteRef, 210, TRUE).*/
   
   NetworkReadFilterForm:startRow().
   NetworkReadFilterForm:insertLabel(fTL("Journey ID")).
   NetworkReadFilterForm:insertTextField("JourneyID", "", 210, TRUE).
   
   NetworkReadFilterForm:startRow().
   NetworkReadFilterForm:insertLabel(fTL("Tote Ref")).
   NetworkReadFilterForm:insertTextField("FilteredToteRef", "", 210, TRUE).

   NetworkReadFilterForm:startRow().
   NetworkReadFilterForm:insertLabel(fTL("Order Ref")).
   NetworkReadFilterForm:insertTextField("FilteredShipOrderRef", "", 210, TRUE).
   
   NetworkReadFilterForm:startRow().
   NetworkReadFilterForm:insertLabel("Start Date").
   NetworkReadFilterForm:insertDateField("StartDate", "", 160, TRUE).  
   /* Time fields have no label */
   NetworkReadFilterForm:insertTextField("StartHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkReadFilterForm:insertLabel(":").
   NetworkReadFilterForm:insertTextField("StartMins", "00", 18, TRUE).  
   
   NetworkReadFilterForm:startRow().
   NetworkReadFilterForm:insertLabel("End Date").
   NetworkReadFilterForm:insertDateField("EndDate", "", 160, TRUE).  
   /* Time fields have no label */
   NetworkReadFilterForm:insertTextField("EndHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkReadFilterForm:insertLabel(":").
   NetworkReadFilterForm:insertTextField("EndMins", "00", 18, TRUE).  


   /* Add Hidden Fields*/
   NetworkReadFilterForm:insertHiddenField("form_name", "network_read_filter_form").
   NetworkReadFilterForm:insertHiddenField("prog_name", "adNetworkReadEnquiry.p").
   NetworkReadFilterForm:insertHiddenField("filtering", "yes").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReadFilterForm}

   /* Create Button Bar */
   NetworkReadFilterButtons = NEW buttonBar().

   NetworkReadFilterButtons:addButton("network_read_filter_form_btn_search",
                                   fTL("Filter"),
                                   "filterNetworkRead('network_read_filter_form');").

   NetworkReadFilterButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   NetworkReadFilterForm:FormButtons = NetworkReadFilterButtons.

   NetworkReadFilterForm:endForm().
   NetworkReadFilterForm:displayForm().

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
   ASSIGN chrNetworkReadID          = get-value("NetworkReadID")
          intSelectedNetworkRead    = INT(chrNetworkReadID)
          chrFilteredNetworkRead    = get-value("FilteredNetworkRead")
          chrFilteredToteRef        = get-value("FilteredToteRef")
          chrFilteredShipOrderRef   = get-value("FilteredShipOrderRef")
          intNetworkReaderID        = INT(get-value("NetworkReaderRef"))
          logFiltering              = (get-value("filtering") = "yes")
          datStartDate              = DATE(get-value('StartDate'))
          datEndDate                = DATE(get-value('EndDate'))
          chrStartHour              = get-value("StartHour")
          chrStartMins              = get-value("StartMins")
          chrEndHour                = get-value("EndHour")
          chrEndMins                = get-value("EndMins")
          chrJourneyID              = get-value("JourneyID")
          chrScrollToNetworkReadRow = STRING(INT(get-value("network_read_browse_scroll"))) + ";" NO-ERROR.

   IF intNetworkReaderID      = 0 AND
      chrJourneyID            = "" AND
      chrFilteredToteRef      = "" AND
      chrFilteredShipOrderRef = "" AND
      datStartDate            = ?  AND
      datEndDate              = ?  THEN
      ASSIGN logFiltering = FALSE.

   IF intSelectedNetworkRead > 0 THEN
      chrSelectNetworkReadRow = 'selectNetworkReadRow(document.getElementById("network_read_browse_row_' + chrNetworkReadID + '"),"'
                              + chrNetworkReadID + '","adNetworkReadEnquiry.p' + '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("network_read_browse").scrollTop=' + chrScrollToNetworkReadRow 
                             + chrSelectNetworkReadRow.
     
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Network Read Enquiry".
   ThisPage:FrameTitle = "Network Read Enquiry".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("networkreadenquiry.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
/*   IF logGblDebugging THEN                                                                                                    */
/*      fLog("GblSessionID:" + STRING(intGblSessionID) + " GblUserID:" + STRING(intGblUserID) + " GblUserName:" + chrGblUserName*/
/*                           + " GblLanguageID:" + STRING(intGblLanguageID) + " GblEnvironment:" + chrGblEnvironment).          */
   
   /******* Main Browser ********************/
   RUN pNetworkReadBrowse.
   
   /******* Popup Browsers and Forms ********/    
/*   FIND FIRST Inbound WHERE Inbound.InboundID = intSelectedInbound NO-LOCK NO-ERROR.*/
   
   RUN pNetworkReadDetails.
   RUN pNetworkReadFilter.
   
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

&IF DEFINED(EXCLUDE-pSetNetworkReadRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetNetworkReadRow Procedure
PROCEDURE pSetNetworkReadRow:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
         FIND FIRST NetworkReader     WHERE NetworkReader.NetworkReaderID = NetworkRead.NetworkReaderID NO-LOCK NO-ERROR.
         FIND FIRST NetworkReaderType WHERE NetworkReaderType.NetworkReaderTypeID = NetworkReader.NetworkReaderTypeID NO-LOCK NO-ERROR.
         FIND FIRST Tote              WHERE Tote.ToteID = NetworkRead.ToteID NO-LOCK NO-ERROR.
         FIND FIRST ShipOrder         WHERE ShipOrder.ShipOrderID = NetworkRead.ShipOrderID NO-LOCK NO-ERROR.

         NetworkReadBrowse:startRow(NetworkRead.NetworkReadID, "selectNetworkReadRow(this," + '"' + STRING(NetworkRead.NetworkReadID) + '"' + ");", "").
         NetworkReadBrowse:insertData(NetworkRead.NetworkReadID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
/*         {webGetOptionalBrowseFields.i Device_Messaging}*/
         
         NetworkReadBrowse:insertData((IF AVAILABLE NetworkReader THEN NetworkReader.NetworkReaderName ELSE ""), "left").
         NetworkReadBrowse:insertData((IF AVAILABLE NetworkReaderType THEN NetworkReaderType.TypeName ELSE ""), "left").
         NetworkReadBrowse:insertData((IF AVAILABLE Tote THEN Tote.ToteRef ELSE ""), "left").
         NetworkReadBrowse:insertData((IF AVAILABLE ShipOrder THEN ShipOrder.OrderRef ELSE ""), "left").
         NetworkReadBrowse:insertData(NetworkRead.MessageSent, "left").
         NetworkReadBrowse:insertData(NetworkRead.MessageReceived, "left").
         NetworkReadBrowse:insertData(fDisplayDate&Time(NetworkRead.Created,"d/m/y H:M:S")).
         
         NetworkReadBrowse:endRow().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

