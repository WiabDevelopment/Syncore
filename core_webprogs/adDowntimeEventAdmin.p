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
{getReceivingOptions.i}


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedDowntimeEvent           AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedDowntimeEventHistory    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectDowntimeEventRow          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectedDate                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToDowntimeEventRow        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDowntimeEventID                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupHistory                    AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE DowntimeEventBrowseFrame           AS pageFrame.
DEFINE VARIABLE DowntimeEventBrowse                AS browseTable.
DEFINE VARIABLE DowntimeEventBrowseButtons         AS buttonBar.
DEFINE VARIABLE DowntimeEventDetailsForm           AS dataForm.
DEFINE VARIABLE DowntimeEventDetailsButtons        AS buttonBar.

DEFINE VARIABLE DowntimeEventHistoryBrowseForm     AS dataForm.
DEFINE VARIABLE DowntimeEventHistoryBrowseFrame    AS pageFrame.
DEFINE VARIABLE DowntimeEventHistoryBrowse         AS browseTable.
DEFINE VARIABLE DowntimeEventHistoryButtons        AS buttonBar.
DEFINE VARIABLE DowntimeEventHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE DowntimeEventHistoryDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-adDowntimeEventHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adDowntimeEventHistoryDetails Procedure
PROCEDURE pDowntimeEventHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimeeventhistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "DowntimeEventHistoryID,DowntimeEventID,DowntimeReasonID,UserName,EventDescr,EventDate,"
                                 + "Duration,BeginningHour,CreatedDate,CreatedHour,CreatedMins,GateUserID,OperationTypeID,"
                                 + "KittingLineID,KittingStationID,PickPackStationID,ToteLineID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   DowntimeEventHistoryDetailsForm = NEW dataForm("downtimeeventhistory_details_form").
   DowntimeEventHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   DowntimeEventHistoryDetailsForm:FormAction = "dbDowntimeEventUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DowntimeEventHistoryDetailsForm:FormWidth   = 580.
   DowntimeEventHistoryDetailsForm:FormHeight  = 430.
   DowntimeEventHistoryDetailsForm:FormTitle   = "Downtime Event History Details".
   DowntimeEventHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   DowntimeEventHistoryDetailsForm:insertPaddingColumn(30).
   DowntimeEventHistoryDetailsForm:insertColumn(150).
   DowntimeEventHistoryDetailsForm:insertColumn(125).
   DowntimeEventHistoryDetailsForm:insertColumn(20).
   DowntimeEventHistoryDetailsForm:insertColumn(4).
   DowntimeEventHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("History ID")).
   DowntimeEventHistoryDetailsForm:insertTextField("DowntimeEventHistoryID", "", 100, TRUE).
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Downtime Event ID")).
   DowntimeEventHistoryDetailsForm:insertTextField("DowntimeEventID", "", 100, TRUE).
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Downtime Reason")).
   DowntimeEventHistoryDetailsForm:insertComboField("DowntimeReasonID", "", 168, TRUE).
   DowntimeEventHistoryDetailsForm:insertComboPairs("DowntimeReasonID", "0", "None").
   FOR EACH DowntimeReason NO-LOCK /*idx=DowntimeReasonID*/
      WHERE DowntimeReason.Active:
         
      DowntimeEventHistoryDetailsForm:insertComboPairs("DowntimeReasonID",STRING(DowntimeReason.DowntimeReasonID),DowntimeReason.ReasonCode).               
   END. /* FOR EACH DowntimeReason */
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Kitting Line")).
   DowntimeEventHistoryDetailsForm:insertComboField("KittingLineID", "", 168, TRUE).
   DowntimeEventHistoryDetailsForm:insertComboPairs("KittingLineID", "0", "None").
   FOR EACH KittingLine NO-LOCK /*idx=KittingLineID*/
      WHERE KittingLine.Active:
         
      DowntimeEventHistoryDetailsForm:insertComboPairs("KittingLineID",STRING(KittingLine.KittingLineID),KittingLine.LineName).               
   END. /* FOR EACH KittingLine*/
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Kitting Station")).
   DowntimeEventHistoryDetailsForm:insertComboField("KittingStationID", "", 168, TRUE).
   DowntimeEventHistoryDetailsForm:insertComboPairs("KittingStationID", "0", "None").
   FOR EACH KittingStation NO-LOCK /*idx=KittingStationID*/
      WHERE KittingStation.Active:
         
      DowntimeEventHistoryDetailsForm:insertComboPairs("KittingStationID",STRING(KittingStation.KittingStationID),KittingStation.StationName).               
   END. /* FOR EACH KittingStation */
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("PickPackStation")).
   DowntimeEventHistoryDetailsForm:insertComboField("PickPackStationID", "", 168, TRUE).
   DowntimeEventHistoryDetailsForm:insertComboPairs("PickPackStationID", "0", "None").
   FOR EACH PickPackStation NO-LOCK /*idx=PickPackStationID*/
      WHERE PickPackStation.Active:
         
      DowntimeEventHistoryDetailsForm:insertComboPairs("PickPackStationID",STRING(PickPackStation.PickPackStationID),PickPackStation.StationName).               
   END. /* FOR EACH PickPackStation */
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Tote Line")).
   DowntimeEventHistoryDetailsForm:insertComboField("ToteLineID", "", 168, TRUE).
   DowntimeEventHistoryDetailsForm:insertComboPairs("ToteLineID", "0", "None").
   FOR EACH ToteLine NO-LOCK /*idx=ToteLineID*/
      WHERE ToteLine.Active:
         
      DowntimeEventHistoryDetailsForm:insertComboPairs("ToteLineID",STRING(ToteLine.ToteLineID),ToteLine.LineName).               
   END. /* FOR EACH ToteLine */
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("User Name")).
   DowntimeEventHistoryDetailsForm:insertComboField("UserName", "", 168, TRUE).
   DowntimeEventHistoryDetailsForm:insertComboPairs("UserName", "0", "None").
   FOR EACH GateUser NO-LOCK /*idx=GateUserID*/
      WHERE GateUser.Active = TRUE
      BY    GateUser.FullName:
         
      DowntimeEventHistoryDetailsForm:insertComboPairs("UserName",STRING(GateUser.GateUserID),GateUser.FullName).               
   END. /* FOR EACH GateUser */
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Event Descr")).
   DowntimeEventHistoryDetailsForm:insertTextField("EventDescr", "", 100, TRUE).
      
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Event Date")).
   DowntimeEventHistoryDetailsForm:insertDateField("EventDate", "", 100, TRUE).
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Duration")).
   DowntimeEventHistoryDetailsForm:insertTextField("Duration", "", 100, TRUE).
   
   DowntimeEventHistoryDetailsForm:startRow().
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Beginning Hour")).
   DowntimeEventHistoryDetailsForm:insertTextField("BeginningHour", "", 100, TRUE).

   DowntimeEventHistoryDetailsForm:startRow().                                                                                           
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   DowntimeEventHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   DowntimeEventHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   DowntimeEventHistoryDetailsForm:insertLabel(":").                                                                                     
   DowntimeEventHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                                         
   DowntimeEventHistoryDetailsForm:startRow().                                                                                           
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   DowntimeEventHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/                                                                                                      
      BY OperationType.OperationTypeID:                                                                                                  
      DowntimeEventHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                          OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   DowntimeEventHistoryDetailsForm:startRow().                                                                                           
   DowntimeEventHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   DowntimeEventHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*idx=GateUserID*/                                                                                                        
      BY GateUser.FullName:                                                                                                              
      DowntimeEventHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pDowntimeEventHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   DowntimeEventHistoryDetailsForm:insertHiddenField("downtimeeventhistory_browse_scroll", "").
   DowntimeEventHistoryDetailsForm:insertHiddenField("form_name", "downtimeeventhistory_details_form").
   DowntimeEventHistoryDetailsForm:insertHiddenField("prog_name", "adDowntimeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeEventHistoryDetailsForm}
   
   /* Create Button Bar */
   DowntimeEventHistoryDetailsButtons = NEW buttonBar().

   DowntimeEventHistoryDetailsButtons:addButton("downtimeeventhistory_details_form_btn_cancel", 
                                                fTL("Cancel"), 
                                                "disablePopup('downtimeeventhistory_details_form_popup');").
   DowntimeEventHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeEventHistoryDetailsForm:FormButtons = DowntimeEventHistoryDetailsButtons.
   
   DowntimeEventHistoryDetailsForm:endForm(). 
   
   DowntimeEventHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


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

&IF DEFINED(EXCLUDE-pDowntimeEventHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeEventHistory Procedure
PROCEDURE pDowntimeEventHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   DowntimeEventHistoryBrowseForm           = NEW dataForm("downtimeeventhistory_browse_form").
   DowntimeEventHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   DowntimeEventHistoryBrowseForm:FormWidth  = 860.
   DowntimeEventHistoryBrowseForm:FormHeight = 540.
   DowntimeEventHistoryBrowseForm:FormTitle  = fTL("Downtime Event History") + (IF intSelectedDowntimeEvent > 0 THEN " : " 
                                                                                + STRING(intSelectedDowntimeEvent) ELSE "").
   DowntimeEventHistoryBrowseForm:FormType   = "xxl_large".
   DowntimeEventHistoryBrowse                = NEW browseTable("downtimeeventhistory_browse").
   DowntimeEventHistoryBrowse:BrowseWidth    = 840.
   DowntimeEventHistoryBrowse:BrowseHeight   = 500.
   
   DowntimeEventHistoryBrowse:insertColumn(fTL("History ID"), 60, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeEventHistory}

   DowntimeEventHistoryBrowse:insertColumn(fTL("Reason Code"), 150, "CHARACTER", "LEFT",   FALSE).
   DowntimeEventHistoryBrowse:insertColumn(fTL("User Name"),   120, "CHARACTER", "LEFT",   FALSE).
   DowntimeEventHistoryBrowse:insertColumn(fTL("Event Descr"), 120, "CHARACTER", "LEFT",   FALSE).
   DowntimeEventHistoryBrowse:insertColumn(fTL("Event Date"),  80,  "DATE",      "LEFT",   FALSE).
   DowntimeEventHistoryBrowse:insertColumn(fTL("Duration"),    80,  "INTEGER",   "CENTER", FALSE).
   DowntimeEventHistoryBrowse:insertColumn(fTL("Start Hour"),  70,  "INTEGER",   "CENTER", FALSE).
   DowntimeEventHistoryBrowse:insertColumn(fTL("Created"),     140, "LOGICAL",   "LEFT",   FALSE).
   
   DowntimeEventHistoryBrowse:StartBody().
   
   FOR EACH DowntimeEventHistory NO-LOCK /*idx=SDowntimeEventID and DowntimeEventHistoryID*/
      WHERE DowntimeEventHistory.DowntimeEventID = intSelectedDowntimeEvent
      BY    DowntimeEventHistory.DowntimeEventHistoryID DESC:
      
      FIND FIRST DowntimeReason NO-LOCK /*idx=DowntimeReasonID*/
         WHERE DowntimeReason.DowntimeReasonID = DowntimeEventHistory.DowntimeReasonID NO-ERROR.
         
      FIND FIRST GateUser NO-LOCK /*idx=GateUsrID*/
         WHERE STRING(GateUser.GateUserID) = DowntimeEventHistory.UserName NO-ERROR.
         
      DowntimeEventHistoryBrowse:startRow (DowntimeEventHistory.DowntimeEventHistoryID,
         "selectDowntimeEventHistoryRow(this," + '"' + STRING(DowntimeEventHistory.DowntimeEventHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i DowntimeEventHistory}
      
      chrSelectedDate = "".
      chrSelectedDate = STRING(DAY(DATE(DowntimeEventHistory.EventDate)),   "99") + "/" +
                        STRING(MONTH(DATE(DowntimeEventHistory.EventDate)), "99") + "/" +
                        STRING(YEAR(DATE(DowntimeEventHistory.EventDate)),  "9999").
      
      DowntimeEventHistoryBrowse:insertData(STRING(DowntimeEventHistory.DowntimeEventHistoryID), "LEFT").
      DowntimeEventHistoryBrowse:insertData(IF AVAILABLE DowntimeReason THEN DowntimeReason.ReasonCode ELSE "", "LEFT").
      DowntimeEventHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "LEFT").
      DowntimeEventHistoryBrowse:insertData(STRING(DowntimeEventHistory.EventDescr),    "LEFT").
      DowntimeEventHistoryBrowse:insertData(chrSelectedDate,                            "LEFT").
      DowntimeEventHistoryBrowse:insertData(STRING(DowntimeEventHistory.Duration),      "CENTER").
      DowntimeEventHistoryBrowse:insertData(STRING(DowntimeEventHistory.BeginningHour), "CENTER").
      DowntimeEventHistoryBrowse:insertData(fDisplayDate&Time(DowntimeEventHistory.Created, "y/m/d H:M:S"), "LEFT").

      DowntimeEventHistoryBrowse:endRow().
   END. /* FOR EACH DowntimeEventHistory */
   
   DowntimeEventHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + DowntimeEventHistoryBrowse:getErrors().
   
   DowntimeEventHistoryBrowseForm:insertHiddenField("popup_downtimeeventhistory_browse","").
   DowntimeEventHistoryBrowseForm:insertHiddenField("DowntimeEventHistoryID","").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeEventHistoryBrowseForm}
   
   /* Create Button Bar */
   DowntimeEventHistoryButtons = NEW buttonBar().
   
   DowntimeEventHistoryButtons:addButton("downtimeeventhistory_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewDowntimeEventHistoryDetails('downtimeeventhistory_details_form');",
                                         (IF intSelectedDowntimeEventHistory > 0 THEN "" ELSE "Disabled")).
                                         
   DowntimeEventHistoryButtons:addButton("downtimeeventhistory_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('downtimeeventhistory_browse_form_popup');").
   DowntimeEventHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeEventHistoryBrowseForm:FormBrowse  = DowntimeEventHistoryBrowse.
   DowntimeEventHistoryBrowseForm:FormButtons = DowntimeEventHistoryButtons.
   DowntimeEventHistoryBrowseForm:endForm(). 
   
   DowntimeEventHistoryBrowseForm:displayForm().   

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
   
   ASSIGN chrDowntimeEventID              = get-value("DowntimeEventID")
          intSelectedDowntimeEvent        = INTEGER(chrDowntimeEventID)
          intSelectedDowntimeEventHistory = INTEGER(get-value("DowntimeEventHistoryID"))
          chrScrollToDowntimeEventRow     = STRING(INTEGER(get-value("downtimeevent_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrDowntimeEventID <> "" THEN
     chrSelectDowntimeEventRow = 'selectDowntimeEventRow(document.getElementById("downtimeevent_browse_row_' + chrDowntimeEventID + '"),"' + 
                                                         chrDowntimeEventID +  '");'.
   
   IF get-value('popup_downtimeeventhistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("downtimeeventhistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("downtimeevent_browse").scrollTop=' + chrScrollToDowntimeEventRow + 
                 chrSelectDowntimeEventRow + chrPopupHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Downtime Event Admin".
   ThisPage:FrameTitle    = "Downtime Event Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("downtimeevent.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pDowntimeEventBrowse.
   
   FIND FIRST DowntimeEvent NO-LOCK /*idx=DowntimeEvenetID*/
      WHERE DowntimeEvent.DowntimeEventID = intSelectedDowntimeEvent NO-ERROR.
   
   /******* Popup Browsers and Forms ********/    
   RUN pDowntimeEventDetails.
   RUN pDowntimeEventHistory.
   RUN pDowntimeEventHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT DowntimeEventBrowseFrame        NO-ERROR.
   DELETE OBJECT DowntimeEventBrowse             NO-ERROR.
   DELETE OBJECT DowntimeEventBrowseButtons      NO-ERROR.
   DELETE OBJECT DowntimeEventDetailsForm        NO-ERROR.
   DELETE OBJECT DowntimeEventDetailsButtons     NO-ERROR.
   
   DELETE OBJECT DowntimeEventHistoryBrowseFrame NO-ERROR.
   DELETE OBJECT DowntimeEventHistoryBrowse      NO-ERROR.
   DELETE OBJECT DowntimeEventHistoryButtons     NO-ERROR.
   DELETE OBJECT DowntimeEventHistoryBrowseForm  NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDowntimeEventBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeEventBrowse Procedure 
PROCEDURE pDowntimeEventBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "downtimeevent_details_form"}
   
   DowntimeEventBrowse = NEW browseTable("downtimeevent_browse").
   DowntimeEventBrowse:BrowseWidth  = 965.
   DowntimeEventBrowse:BrowseHeight = 455.
   DowntimeEventBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   DowntimeEventBrowse:insertColumn(fTL("Event ID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeEvent}
   
   DowntimeEventBrowse:insertColumn(fTL("Downtime Reason"), 180, "CHARACTER", "LEFT",   FALSE).
   DowntimeEventBrowse:insertColumn(fTL("Kitting Line"),    180, "CHARACTER", "LEFT",   FALSE).
   DowntimeEventBrowse:insertColumn(fTL("Event Descr"),     180, "CHARACTER", "LEFT",   FALSE).
   DowntimeEventBrowse:insertColumn(fTL("Event Date"),      100, "DATE",      "LEFT",   FALSE).
   DowntimeEventBrowse:insertColumn(fTL("Duration"),        100, "INTEGER",   "CENTER", FALSE).
   DowntimeEventBrowse:insertColumn(fTL("Beginning Hour"),  130, "INTEGER",   "CENTER", FALSE).
   
   /*Body*/
   DowntimeEventBrowse:startBody().
   
   FOR EACH DowntimeEvent NO-LOCK /*idx=DowntimeEventID*/
      BY DowntimeEvent.DowntimeEventID:
         
      FIND FIRST DowntimeReason NO-LOCK /*idx=DowntimeReasonID*/
         WHERE DowntimeReason.DowntimeReasonID = DowntimeEvent.DowntimeReasonID NO-ERROR.
         
      FIND FIRST GateUser NO-LOCK /*idx=GateUserID*/
         WHERE STRING(GateUser.GateUserID) = DowntimeEvent.UserName NO-ERROR.
         
      FIND FIRST KittingLine NO-LOCK /*idx=KittingLineID*/
         WHERE KittingLine.KittingLineID = DowntimeEvent.KittingLineID NO-ERROR. 

      DowntimeEventBrowse:startRow(DowntimeEvent.DowntimeEventID, "selectDowntimeEventRow(this," + '"' + STRING(DowntimeEvent.DowntimeEventID) + '"' + ");", "").
      DowntimeEventBrowse:insertData(DowntimeEvent.DowntimeEventID).

      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i DowntimeEvent}
      
      chrSelectedDate = "".
      chrSelectedDate = STRING(DAY(DATE(DowntimeEvent.EventDate)),   "99") + "/" +
                        STRING(MONTH(DATE(DowntimeEvent.EventDate)), "99") + "/" +
                        STRING(YEAR(DATE(DowntimeEvent.EventDate)),  "9999").

      DowntimeEventBrowse:insertData(IF AVAILABLE DowntimeReason THEN DowntimeReason.ReasonCode ELSE "", "LEFT").
      DowntimeEventBrowse:insertData(IF AVAILABLE KittingLine THEN KittingLine.LineName ELSE "", "LEFT").
      DowntimeEventBrowse:insertData(DowntimeEvent.EventDescr,    "LEFT").
      DowntimeEventBrowse:insertData(chrSelectedDate,             "LEFT").
      DowntimeEventBrowse:insertData(DowntimeEvent.Duration,      "CENTER").
      DowntimeEventBrowse:insertData(DowntimeEvent.BeginningHour, "CENTER").

      /* Add hidden fields */
      DowntimeEventBrowse:insertHiddenData("DowntimeEventVersionID",DowntimeEvent.VersionID).

      DowntimeEventBrowse:endRow().
      
   END. /*FOR EACH DowntimeEvent NO-LOCK */
   
   DowntimeEventBrowse:endTable().
   
   /* Create a new frame */
   DowntimeEventBrowseFrame = NEW pageFrame().
   DowntimeEventBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   DowntimeEventBrowseFrame:FormAction="dbDowntimeEventUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   DowntimeEventBrowseFrame:formOpen("downtimeevent_browse_form").
   
   /* Start the Frame Header */
   DowntimeEventBrowseFrame:insertSpacer(5).
   DowntimeEventBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   DowntimeEventBrowse:displayBrowse().  
   
   /* End the Frame Header */
   DowntimeEventBrowseFrame:frameClose().
   DowntimeEventBrowseFrame:insertSpacer(10).
   
   DowntimeEventBrowseFrame:insertHiddenField("downtimeevent_browse_scroll","").
   DowntimeEventBrowseFrame:insertHiddenField("DowntimeEventID","").
   DowntimeEventBrowseFrame:insertHiddenField("DowntimeEventVersionID","").
   DowntimeEventBrowseFrame:insertHiddenField("form_name", "downtimeevent_browse_form").
   DowntimeEventBrowseFrame:insertHiddenField("prog_name", "adDowntimeEventAdmin.p").
   DowntimeEventBrowseFrame:insertHiddenField("popup_downtimeeventhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeEventBrowseFrame}
   
   DowntimeEventBrowseFrame:formClose().
   
   /* Create Button Bar */
   DowntimeEventBrowseButtons = NEW buttonBar().
   DowntimeEventBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   DowntimeEventBrowseButtons:addButton("downtimeevent_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewDowntimeEventDetails('downtimeevent_details_form');",
                                        (IF intSelectedDowntimeEvent > 0 THEN "" ELSE "Disabled")).
   
   DowntimeEventBrowseButtons:addButton("downtimeevent_browse_form_btn_create",
                                        fTL("Create"),
                                        "createDowntimeEvent('downtimeevent_details_form');",
                                        "").
   
   DowntimeEventBrowseButtons:addButton("downtimeevent_browse_form_btn_delete",
                                        fTL("Delete"),
                                        "confirmDeleteDowntimeEvent();",
                                        (IF intSelectedDowntimeEvent > 0 THEN "" ELSE "Disabled")).
                                        
   DowntimeEventBrowseButtons:addButton("downtimeevent_browse_form_btn_history",
                                        fTL("History"),
                                        "viewDowntimeEventHistory();",
                                        (IF intSelectedDowntimeEvent > 0 THEN "" ELSE "Disabled")).
   
   DowntimeEventBrowseButtons:closeBar().  
   DowntimeEventBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDowntimeEventDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeEventDetails Procedure 
PROCEDURE pDowntimeEventDetails:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimeevent_details_form"}
   
   ASSIGN chrDisplayFieldList  = "DowntimeEventID,DowntimeReasonID,UserName,EventDescr,EventDate,Duration,BeginningHour," +
                                 "KittingLineID,KittingStationID,PickPackStationID,ToteLineID"
          chrEditFieldList     = "DowntimeReasonID,UserName,EventDescr,EventDate,Duration,BeginningHour," +
                                 "KittingLineID,KittingStationID,PickPackStationID,ToteLineID"
          chrNewFieldList      = "DowntimeReasonID,UserName,EventDescr,EventDate,Duration,BeginningHour," +
                                 "KittingLineID,KittingStationID,PickPackStationID,ToteLineID"
          chrRequiredFieldList = "DowntimeReasonID,UserName,EventDescr,EventDate,Duration,BeginningHour," +
                                 "KittingLineID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "Duration:INTEGER,BeginningHour:INTEGER".
   
   DowntimeEventDetailsForm = NEW dataForm("downtimeevent_details_form").
   DowntimeEventDetailsForm:WebStream  = STREAM WebStream:HANDLE.
   
   DowntimeEventDetailsForm:FormAction = "dbDowntimeEventUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DowntimeEventDetailsForm:FormWidth  = 450.
   DowntimeEventDetailsForm:FormHeight = 300.
   DowntimeEventDetailsForm:FormTitle  = "DowntimeEvent Details".
   DowntimeEventDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   DowntimeEventDetailsForm:insertPaddingColumn(50).
   DowntimeEventDetailsForm:insertColumn(90).
   DowntimeEventDetailsForm:insertColumn(120).
   DowntimeEventDetailsForm:insertColumn(20).
   DowntimeEventDetailsForm:insertColumn(4).
   DowntimeEventDetailsForm:insertColumn(110).
   
   /* Fields */
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel("Event ID").
   DowntimeEventDetailsForm:insertTextField("DowntimeEventID", "", 110, TRUE).
    
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel(fTL("Reason")).
   DowntimeEventDetailsForm:insertComboField("DowntimeReasonID", "", 168, TRUE).
   DowntimeEventDetailsForm:insertComboPairs("DowntimeReasonID", "", "None").
   FOR EACH DowntimeReason NO-LOCK /*idx=DowntimeReasonID*/
      WHERE DowntimeReason.Active:
         
      DowntimeEventDetailsForm:insertComboPairs("DowntimeReasonID",STRING(DowntimeReason.DowntimeReasonID),DowntimeReason.ReasonCode).               
   END. /* FOR EACH DowntimeReason */
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel(fTL("Kitting Line")).
   DowntimeEventDetailsForm:insertComboField("KittingLineID", "", 168, TRUE).
   DowntimeEventDetailsForm:insertComboPairs("KittingLineID", "0", "None").
   FOR EACH KittingLine NO-LOCK /*idx=KittingLineID*/
      WHERE KittingLine.Active:
         
      DowntimeEventDetailsForm:insertComboPairs("KittingLineID",STRING(KittingLine.KittingLineID),KittingLine.LineName).               
   END. /* FOR EACH KittingLine*/
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel(fTL("Kitting Station")).
   DowntimeEventDetailsForm:insertComboField("KittingStationID", "", 168, TRUE).
   DowntimeEventDetailsForm:insertComboPairs("KittingStationID", "0", "None").
/*   FOR EACH KittingStation NO-LOCK /*idx=KittingStationID*/                                                                            */
/*      WHERE KittingStation.Active:                                                                                                     */
/*                                                                                                                                       */
/*      DowntimeEventDetailsForm:insertComboPairs("KittingStationID",STRING(KittingStation.KittingStationID),KittingStation.StationName).*/
/*   END. /* FOR EACH KittingStation */                                                                                                  */
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel(fTL("PickPackStation")).
   DowntimeEventDetailsForm:insertComboField("PickPackStationID", "", 168, TRUE).
   DowntimeEventDetailsForm:insertComboPairs("PickPackStationID", "0", "None").
/*   FOR EACH PickPackStation NO-LOCK /*idx=PickPackStationID*/                                                                              */
/*      WHERE PickPackStation.Active:                                                                                                        */
/*                                                                                                                                           */
/*      DowntimeEventDetailsForm:insertComboPairs("PickPackStationID",STRING(PickPackStation.PickPackStationID),PickPackStation.StationName).*/
/*   END. /* FOR EACH PickPackStation */                                                                                                     */
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel(fTL("Tote Line")).
   DowntimeEventDetailsForm:insertComboField("ToteLineID", "", 168, TRUE).
   DowntimeEventDetailsForm:insertComboPairs("ToteLineID", "0", "None").
/*   FOR EACH ToteLine NO-LOCK /*idx=ToteLineID*/                                                             */
/*      WHERE ToteLine.Active:                                                                                */
/*                                                                                                            */
/*      DowntimeEventDetailsForm:insertComboPairs("ToteLineID",STRING(ToteLine.ToteLineID),ToteLine.LineName).*/
/*   END. /* FOR EACH ToteLine */                                                                             */
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel(fTL("User Name")).
   DowntimeEventDetailsForm:insertComboField("UserName", "", 168, TRUE).
   DowntimeEventDetailsForm:insertComboPairs("UserName", "0", "None").
   GateUserloop:
   FOR EACH GateUser NO-LOCK /*idx=GateuserID*/
      WHERE GateUser.Active 
      BY GateUser.FullName:
         
      FOR EACH AccessUserLink OF GateUser NO-LOCK
         WHERE AccessUserLink.Active:
      
         /*Basically says this User has access to this Application via this accessgroup*/
         IF CAN-FIND(FIRST AccessAppLink NO-LOCK
            WHERE AccessAppLink.AccessGroupID = AccessUserLink.AccessGroupID
            AND AccessAppLink.ApplicationID = intGblApplicationID
            AND AccessAppLink.Active) THEN
         DO:
            /*Insert Combo*/
            DowntimeEventDetailsForm:insertComboPairs("UserName",STRING(GateUser.GateUserID), GateUser.FullName). 
            NEXT GateUserLoop.
         END. 
      END.
   END. /* FOR EACH GateUser */.  
      
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel("Event Descr").
   DowntimeEventDetailsForm:insertTextField("EventDescr", "", 110, TRUE). 
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel("Event Date").
   DowntimeEventDetailsForm:insertDateField("EventDate", "", 110, TRUE). 
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel("Duration").
   DowntimeEventDetailsForm:insertTextField("Duration", "", 110, TRUE).
   
   DowntimeEventDetailsForm:startRow().
   DowntimeEventDetailsForm:insertLabel("Beginning Hour").
   DowntimeEventDetailsForm:insertTextField("BeginningHour", "", 110, TRUE).
   
   {webGetOptionalFormFields.i pDowntimeEventDetailsFields}
   
   /* Add Hidden Fields*/
   DowntimeEventDetailsForm:insertHiddenField("ListingSequence", "").
   DowntimeEventDetailsForm:insertHiddenField("downtimeevent_browse_scroll", "").
   DowntimeEventDetailsForm:insertHiddenField("form_name", "downtimeevent_details_form").
   DowntimeEventDetailsForm:insertHiddenField("prog_name", "adDowntimeEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeEventDetailsForm}
   
   /* Create Button Bar */
   DowntimeEventDetailsButtons = NEW buttonBar().
   
   DowntimeEventDetailsButtons:addButton("downtimeevent_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateDowntimeEvent('downtimeevent_details_form');").
   
   DowntimeEventDetailsButtons:addButton("downtimeevent_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "cancelUpdate('UserCancelled','process_mode'); disablePopup('downtimeevent_details_form_popup');").
   
   DowntimeEventDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeEventDetailsForm:FormButtons = DowntimeEventDetailsButtons.
   
   DowntimeEventDetailsForm:endForm(). 
   
   DowntimeEventDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + DowntimeEventDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOwnerDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeEventDetailsFields Procedure 
PROCEDURE pDowntimeEventDetailsFields:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         DowntimeEventDetailsForm:startRow().
         DowntimeEventDetailsForm:insertLabel(fTL("Field Label")).
         DowntimeEventDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      /*{adDowntimeEventAdmin_downtimeevent_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

