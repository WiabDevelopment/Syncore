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

  Author: Lily Tran

  Created: 25/10/2013

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

/* Standard Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{defWebDefinitions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* CronEvent Local Variables */
DEFINE VARIABLE chrCronEventID                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCronEventRow              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronEventRow                AS CHARACTER NO-UNDO.  
DEFINE VARIABLE intSelectedCronEvent                 AS INTEGER   NO-UNDO.

/* CronEventParameter Local Variables */
DEFINE VARIABLE chrCronEventParameterID              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCronEventParameter           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronEventParameterRow       AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCronEventParameter        AS INTEGER   NO-UNDO. 

/* CronEventRunResultType Local Variables */
DEFINE VARIABLE chrCronEventRunResultTypeID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCronEventRunResultType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronEventRunResultTypeRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCronEventRunResultType    AS INTEGER   NO-UNDO.                                                                       

/* CronEventRun Local Variables */
DEFINE VARIABLE chrCronEventRunID                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCronEventRun                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronEventRunRow             AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCronEventRun              AS INTEGER   NO-UNDO.
DEFINE VARIABLE intRecordCounter                     AS INTEGER   NO-UNDO.

/* CronEventHistory Local Variables */
DEFINE VARIABLE chrCronEventHistoryID                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCronEventHistory             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronEventHistoryRow         AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCronEventHistory          AS INTEGER   NO-UNDO.

/* CronConfig Local Variables */
DEFINE VARIABLE chrCronConfigID                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCronConfig                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronConfigRow               AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCronConfig                AS INTEGER   NO-UNDO.
DEFINE VARIABLE logRecordAvailable                   AS LOGICAL   NO-UNDO.

/* CronEventHistory Local Variables */
DEFINE VARIABLE chrCronConfigHistoryID               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCronConfigHistory            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronConfigHistoryRow        AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCronConfigHistory         AS INTEGER   NO-UNDO.

/* CronEventEnvironmentLink Local Variables */
DEFINE VARIABLE chrCronEventEnvironmentLinkID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCronEventEnvironmentLink     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCronEventEnvironmentLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCronEventEnvironmentLink  AS INTEGER   NO-UNDO.

/* CronEvent Objects */
DEFINE VARIABLE CronEventBrowse                        AS browseTable.
DEFINE VARIABLE CronEventBrowseButtons                 AS buttonBar.
DEFINE VARIABLE CronEventBrowseMoreButtons                 AS buttonBar.
DEFINE VARIABLE CronEventBrowseFrame                   AS pageFrame.
DEFINE VARIABLE CronEventDetailsButtons                AS buttonBar.
DEFINE VARIABLE CronEventDetailsForm                   AS dataForm.

/* CronEventParameter Objects*/
DEFINE VARIABLE CronEventParameterBrowse               AS browseTable.
DEFINE VARIABLE CronEventParameterBrowseButtons        AS buttonBar.
DEFINE VARIABLE CronEventParameterBrowseForm           AS dataForm.
DEFINE VARIABLE CronEventParameterDetailsButtons       AS buttonBar.
DEFINE VARIABLE CronEventParameterDetailsForm          AS dataForm.

/* CronEventRunResultType Objects */
DEFINE VARIABLE CronEventRunResultTypeBrowse           AS browseTable.
DEFINE VARIABLE CronEventRunResultTypeBrowseButtons    AS buttonBar.
DEFINE VARIABLE CronEventRunResultTypeBrowseForm       AS dataForm.
DEFINE VARIABLE CronEventRunResultTypeDetailsButtons   AS buttonBar.
DEFINE VARIABLE CronEventRunResultTypeDetailsForm      AS dataForm.

/* CronEventRun Objects */
DEFINE VARIABLE CronEventRunBrowse                     AS browseTable.
DEFINE VARIABLE CronEventRunBrowseButtons              AS buttonBar.
DEFINE VARIABLE CronEventRunBrowseForm                 AS dataForm.
DEFINE VARIABLE CronEventRunDetailsButtons             AS buttonBar.
DEFINE VARIABLE CronEventRunDetailsForm                AS dataForm.

/* CronEventHistory Objects */
DEFINE VARIABLE CronEventHistoryBrowse                 AS browseTable.
DEFINE VARIABLE CronEventHistoryBrowseButtons          AS buttonBar.
DEFINE VARIABLE CronEventHistoryBrowseForm             AS dataForm.

/* CronConfig Objects */
DEFINE VARIABLE CronConfigBrowse                       AS browseTable.
DEFINE VARIABLE CronConfigBrowseButtons                AS buttonBar.
DEFINE VARIABLE CronConfigBrowseForm                   AS dataForm.
DEFINE VARIABLE CronConfigDetailsButtons               AS buttonBar.
DEFINE VARIABLE CronConfigDetailsForm                  AS dataForm.

/* CronConfigHistory Objects */
DEFINE VARIABLE CronConfigHistoryBrowse                AS browseTable.
DEFINE VARIABLE CronConfigHistoryBrowseButtons         AS buttonBar.
DEFINE VARIABLE CronConfigHistoryBrowseForm            AS dataForm.

/* CronEventEnvironmentLink */
DEFINE VARIABLE CronEventEnvironmentLinkBrowse         AS browseTable.
DEFINE VARIABLE CronEventEnvironmentLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE CronEventEnvironmentLinkBrowseForm     AS dataForm.
DEFINE VARIABLE CronEventEnvironmentLinkDetailsButtons AS buttonBar.
DEFINE VARIABLE CronEventEnvironmentLinkDetailsForm    AS dataForm.

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
         HEIGHT             = 20.81
         WIDTH              = 69.6.
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

&IF DEFINED(EXCLUDE-pCronConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronConfigBrowse Procedure 
PROCEDURE pCronConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF CAN-FIND(FIRST CronConfig NO-LOCK) THEN
      logRecordAvailable = YES.

   {webGetWebForm.i "cronconfig_details_form"}
   
   CronConfigBrowseForm = NEW dataForm("cronconfig_browse_form").
   CronConfigBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CronConfigBrowseForm:FormAction  = "dbCronConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronConfigBrowseForm:FormWidth   = 580.
   CronConfigBrowseForm:FormHeight  = 420.
   CronConfigBrowseForm:FormTitle   = fTL("CronConfig").
                                               
   CronConfigBrowseForm:FormType    = "large".
   
   CronConfigBrowse = NEW browseTable("cronconfig_browse").
   CronConfigBrowse:BrowseWidth  = 560.
   CronConfigBrowse:BrowseHeight = 375.
   
   CronConfigBrowse:insertColumn(fTL("ID"), 80, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronConfig}
   
   CronConfigBrowse:insertColumn(fTL("Interval"),      85, "INTEGER",   "left").
   CronConfigBrowse:insertColumn(fTL("Previous Run"), 100, "CHARACTER", "left").
   CronConfigBrowse:insertColumn(fTL("Is Running"),    90, "LOGICAL",   "left").
   CronConfigBrowse:insertColumn(fTL("Active"),        90, "LOGICAL",   "left").
   
   CronConfigBrowse:StartBody().
   
   /* List the CronConfigs for the CronEvent */
   FOR EACH CronConfig NO-LOCK: /* idx=CronConfigID */
      
      CronConfigBrowse:startRow(CronConfig.CronConfigID, "selectCronConfigRow(this," + '"' + STRING(CronConfig.CronConfigID)
                                 + '","adCronEventAdmin.p","cronconfig_browse_form"' + ");", ""). 
                                            
      CronConfigBrowse:insertData(CronConfig.CronConfigID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CronConfig}
           
      CronConfigBrowse:insertData(STRING(CronConfig.CronInterval), "right").
      CronConfigBrowse:insertData(fDisplayDate&Time(CronConfig.PreviousRun, "d/m/y H:M"), "left").
      CronConfigBrowse:insertData(STRING(CronConfig.CronIsRunning, "Yes/No"), "left").
      CronConfigBrowse:insertData(STRING(CronConfig.Active, "Yes/No"), "left").
      
      /* Add hidden fields */
      CronConfigBrowse:insertHiddenData("CronConfigVersionID",CronConfig.VersionID).

      CronConfigBrowse:endRow().
   
   END. /* FOR EACH CronConfig OF CronEvent NO-LOCK */

   
   CronConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CronConfigBrowse:getErrors().
   
   CronConfigBrowseForm:insertHiddenField("CronConfigID",chrCronConfigID).
   CronConfigBrowseForm:insertHiddenField("CronConfigVersionID","").
   CronConfigBrowseForm:insertHiddenField("cronconfig_browse_scroll","").   
   CronConfigBrowseForm:insertHiddenField("popup_cronconfig_browse","").
   CronConfigBrowseForm:insertHiddenField("popup_cronconfighistory_browse","").
   CronConfigBrowseForm:insertHiddenField("form_name","cronconfig_browse_form").
   CronConfigBrowseForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronConfigBrowseForm}
   
   /* Create Button Bar */
   CronConfigBrowseButtons = NEW buttonBar().
   
   CronConfigBrowseButtons:addButton("cronconfig_browse_form_btn_create",
                                     fTL("Create"),
                                     "createCronConfig('cronconfig_details_form');",
                                     IF logRecordAvailable THEN "Disabled" ELSE ""). 

   CronConfigBrowseButtons:addButton("cronconfig_browse_form_btn_view",
                                     fTL("Details"),
                                     "viewCronConfigDetails('cronconfig_details_form');",
                                     "Disabled").
        
   CronConfigBrowseButtons:addButton("cronconfig_browse_form_btn_history",
                                     fTL("History"),
                                     "viewCronConfigHistoryBrowse();",
                                     "Disabled").

   CronConfigBrowseButtons:addButton("cronconfig_browse_form_btn_cancel",
                                     fTL("Cancel"),
                                     "disablePopup('cronconfig_browse_form_popup');").
   
   CronConfigBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronConfigBrowseForm:FormBrowse  = CronConfigBrowse.
   CronConfigBrowseForm:FormButtons = CronConfigBrowseButtons.
   CronConfigBrowseForm:endForm(). 
   
   CronConfigBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronConfigDetails Procedure 
PROCEDURE pCronConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "cronconfig_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CronConfigID,CronInterval,PreviousRunDate,PreviousRunHour,PreviousRunMins,CronIsRunning,Active"
          chrEditFieldList     = "CronInterval,Active"
          chrNewFieldList      = "CronInterval,Active"
          chrRequiredFieldList = "CronInterval,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "CronInterval:INTEGER>0".

   IF chrGblEnvironment <> "Live" THEN
      chrEditFieldList = "CronInterval,PreviousRunDate,PreviousRunHour,PreviousRunMins,Active".
   
   CronConfigDetailsForm = NEW dataForm("cronconfig_details_form").
   CronConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CronConfigDetailsForm:FormAction  = "dbCronConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronConfigDetailsForm:FormWidth   = 460.
   CronConfigDetailsForm:FormHeight  = 300.
   CronConfigDetailsForm:FormTitle   = "CronConfig".
   CronConfigDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CronConfigDetailsForm:insertPaddingColumn(10).
   CronConfigDetailsForm:insertColumn(140).
   CronConfigDetailsForm:insertColumn(120).
   CronConfigDetailsForm:insertColumn(20).
   CronConfigDetailsForm:insertColumn(4).
   CronConfigDetailsForm:insertColumn(40).
   
   /* Fields */
   CronConfigDetailsForm:startRow().
   CronConfigDetailsForm:insertLabel(fTL("CronConfig ID")).
   CronConfigDetailsForm:insertTextField("CronConfigID", "", 90, TRUE).    
   
   CronConfigDetailsForm:startRow().
   CronConfigDetailsForm:insertLabel(fTL("Cron Interval")).
   CronConfigDetailsForm:insertTextField("CronInterval", "", 110, TRUE).  

   CronConfigDetailsForm:startRow().
   CronConfigDetailsForm:insertLabel(fTL("PreviousRun")).
   CronConfigDetailsForm:insertDateField("PreviousRunDate", "", 110, TRUE).
   CronConfigDetailsForm:insertTextField("PreviousRunHour", "", 18, TRUE).
   CronConfigDetailsForm:insertLabel(":").
   CronConfigDetailsForm:insertTextField("PreviousRunMins", "", 18, TRUE).
      
   CronConfigDetailsForm:startRow().
   CronConfigDetailsForm:insertLabel(fTL("Cron Is Running")). 
   CronConfigDetailsForm:insertComboField("CronIsRunning", "", 160, TRUE).  
   CronConfigDetailsForm:insertComboPairs("CronIsRunning", "yes", "Is Running Now").
   CronConfigDetailsForm:insertComboPairs("CronIsRunning", "no",  "Is Not Running Now").

   CronConfigDetailsForm:startRow().
   CronConfigDetailsForm:insertLabel(fTL("Active")). 
   CronConfigDetailsForm:insertComboField("Active", "", 160, TRUE).  
   CronConfigDetailsForm:insertComboPairs("Active", "yes", "Is Active").
   CronConfigDetailsForm:insertComboPairs("Active", "no",  "Is Not Active").
      
   /* Add Hidden Fields*/
   CronConfigDetailsForm:insertHiddenField("cronevent_browse_scroll","").
   CronConfigDetailsForm:insertHiddenField("popup_cronconfig_browse", "").
   CronConfigDetailsForm:insertHiddenField("CronEventID", IF intSelectedCronEvent = 0 THEN "" ELSE STRING(intSelectedCronEvent)).
   CronConfigDetailsForm:insertHiddenField("CronConfigID",STRING(intSelectedCronConfig)).
   CronConfigDetailsForm:insertHiddenField("form_name","cronconfig_details_form").
   CronConfigDetailsForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronConfigDetailsForm}
   
   /* Create Button Bar */
   CronConfigDetailsButtons = NEW buttonBar().
   
   CronConfigDetailsButtons:addButton("cronconfig_details_form_btn_save", 
                                      fTL("Save"), 
                                      "updateCronConfig('cronconfig_details_form');").
   
   CronConfigDetailsButtons:addButton("cronconfig_details_form_btn_cancel", 
                                      fTL("Cancel"), 
                                      "cancelUpdate('UserCancelled','process_mode'); " + "disablePopup('cronconfig_details_form_popup');").
                                    
   CronConfigDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CronConfigDetailsForm:FormButtons = CronConfigDetailsButtons.
   
   CronConfigDetailsForm:endForm(). 
   CronConfigDetailsForm:displayForm(). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronConfigHistoryBrowse Procedure 
PROCEDURE pCronConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CronConfigHistoryBrowseForm = NEW dataForm("cronconfighistory_browse_form").
   CronConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   CronConfigHistoryBrowseForm:FormWidth  = 700.
   CronConfigHistoryBrowseForm:FormHeight = 490.
   CronConfigHistoryBrowseForm:FormTitle  = fTL("CronConfig History") + (IF AVAILABLE CronConfig THEN ": " + STRING(CronConfig.CronConfigID) 
                                                                                                 ELSE "").                                           
   CronConfigHistoryBrowseForm:FormType   = "xl_large".  
   CronConfigHistoryBrowse = NEW browseTable("cronconfighistory_browse").
   CronConfigHistoryBrowse:BrowseWidth  = 680.
   CronConfigHistoryBrowse:BrowseHeight = 432.
   
   CronConfigHistoryBrowse:insertColumn(fTL("History ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronConfigHistory}

   CronConfigHistoryBrowse:insertColumn(fTL("Cron Interval"),  80, "INTEGER",   "left").
   CronConfigHistoryBrowse:insertColumn(fTL("Previous Run"),  100, "CHARACTER", "left").
   CronConfigHistoryBrowse:insertColumn(fTL("Is Running"),     70, "LOGICAL"  , "left").
   CronConfigHistoryBrowse:insertColumn(fTl("Active"),         70, "LOGICAL",   "left").
   CronConfigHistoryBrowse:insertColumn(fTL("User"),          110, "CHARACTER", "left").
   CronConfigHistoryBrowse:insertColumn(fTL("Created"),       100, "CHARACTER", "left").

   CronConfigHistoryBrowse:StartBody().

   IF AVAILABLE CronConfig THEN
   DO:
      /* List the CronConfigHistorys for the CronConfig */
      FOR EACH CronConfigHistory NO-LOCK /* idx=CronConfigIDCreated */
         WHERE CronConfigHistory.CronConfigID = CronConfig.CronConfigID 
         BY CronConfigHistory.CronConfigID
         BY CronConfigHistory.Created DESCENDING:

         FIND FIRST GateUser OF CronConfigHistory NO-LOCK NO-ERROR.

         CronConfigHistoryBrowse:startRow(CronConfigHistory.CronConfigHistoryID, "selectCronConfigHistoryRow(this," + '"' 
                                    + STRING(CronConfigHistory.CronConfigHistoryID)
                                    + '","adCronConfigAdmin.p","cronconfighistory_browse_form"' + ");", "").

         CronConfigHistoryBrowse:insertData(CronConfigHistory.CronConfigHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CronConfigHistory}

         CronConfigHistoryBrowse:insertData(STRING(CronConfigHistory.CronInterval), "right").
         CronConfigHistoryBrowse:insertData(fDisplayDate&Time(CronConfigHistory.PreviousRun,"y/m/d H:M:S"), "left").
         CronConfigHistoryBrowse:insertData(STRING(CronConfigHistory.CronIsRunning, "Yes/No"), "left"). 
         CronConfigHistoryBrowse:insertData(STRING(CronConfigHistory.Active, "Yes/No"), "left").
         CronConfigHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         CronConfigHistoryBrowse:insertData(fDisplayDate&Time(CronConfigHistory.Created,"y/m/d H:M:S"), "left").                  
                  
         CronConfigHistoryBrowse:endRow().
      
      END. /* FOR EACH CronConfigHistory OF CronConfig NO-LOCK */
   END. /*IF AVAILABLE CronConfig THEN*/
   
   CronConfigHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + CronConfigHistoryBrowse:getErrors().

   CronConfigHistoryBrowseForm:insertHiddenField("popup_cronconfighistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   CronConfigHistoryBrowseButtons = NEW buttonBar().
          
   CronConfigHistoryBrowseButtons:addButton("cronconfighistory_browse_form_btn_cancel",
                                            fTL("Cancel"),
                                            "disablePopup('cronconfighistory_browse_form_popup');").
   
   CronConfigHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronConfigHistoryBrowseForm:FormBrowse  = CronConfigHistoryBrowse.
   CronConfigHistoryBrowseForm:FormButtons = CronConfigHistoryBrowseButtons.
   CronConfigHistoryBrowseForm:endForm(). 
   
   CronConfigHistoryBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventBrowse Procedure 
PROCEDURE pCronEventBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "cronevent_details_form"}
   
   CronEventBrowse              = NEW browseTable("cronevent_browse").
   CronEventBrowse:BrowseWidth  = 965.
   CronEventBrowse:BrowseHeight = 425.
   CronEventBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CronEventBrowse:insertColumn(fTL("ID"), 60, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronEvent}
   
   CronEventBrowse:insertColumn(fTL("Prty"),               40, "INTEGER",   "LEFT").
   CronEventBrowse:insertColumn(fTL("Active"),             45, "LOGICAL",   "LEFT").
   CronEventBrowse:insertColumn(fTL("Cron Description"),  150, "CHARACTER", "LEFT").
   CronEventBrowse:insertColumn(fTL("Program Name"),      145, "CHARACTER", "LEFT").
   CronEventBrowse:insertColumn(fTL("NIRS"),               40, "CHARACTER", "LEFT").
   CronEventBrowse:insertColumn(fTL("CP"),                 45, "CHARACTER", "LEFT").
   CronEventBrowse:insertColumn(fTL("Minutes"),            65, "CHARACTER", "LEFT").
   CronEventBrowse:insertColumn(fTL("Hours"),              50, "CHARACTER", "LEFT").
   CronEventBrowse:insertColumn(fTL("Avg Run"),            50, "INTEGER",  "RIGHT").
   CronEventBrowse:insertColumn(fTL("Exec Delay"),         90, "CHARACTER", "LEFT").
   CronEventBrowse:insertColumn(fTL("Log"),                60, "LOGICAL",   "LEFT").
/*   CronEventBrowse:insertColumn("",                        35, "", FALSE).*/
   
   /*Body*/
   CronEventBrowse:startBody().
   
   FIND FIRST Environment NO-LOCK
      WHERE Environment.EnvironmentCode = chrGblEnvironment NO-ERROR.

   IF NOT AVAILABLE Environment THEN
      chrPageBuildError = chrPageBuildError + "Environment record for EnvironmentCode: " + chrGblEnvironment
                             + " not found. ".
   
   /* Find all Cron Events then sort by Priority */
   CronEventLoop:
   FOR EACH CronEvent NO-LOCK /* idx=ActivePriority */
      BY CronEvent.Active DESCENDING
      BY CronEvent.Priority
      BY cronEvent.EventDescr:

      IF NOT fCanViewBusinessUnit(intGblSessionID,CronEvent.BusinessUnitID) THEN 
         NEXT CronEventLoop.

      /* CronEvent Active flag is determined by the link record and current environment */
      IF AVAILABLE Environment THEN
         FIND FIRST CronEventEnvironmentLink NO-LOCK /* idx=CronEventIDEnvironmentLinkID */
            WHERE CronEventEnvironmentLink.CronEventID = CronEvent.CronEventID
            AND CronEventEnvironmentLink.EnvironmentID = Environment.EnvironmentID NO-ERROR.
      
      IF NOT AVAILABLE CronEventEnvironmentLink THEN
         chrPageBuildError = chrPageBuildError + "CronEventEnvironmentLink record for EnvironmentCode: " + chrGblEnvironment
                                + " and CronEventID: " + STRING(CronEvent.CronEventID) + " not found.".
      
      FIND FIRST CodePage OF CronEvent NO-LOCK NO-ERROR.

      FIND FIRST ProcessProgram OF CronEvent NO-LOCK NO-ERROR.
      
      CronEventBrowse:startRow(CronEvent.CronEventID, "selectCronEventRow(this," + '"' + STRING(CronEvent.CronEventID) + '"' + ");", "").
      CronEventBrowse:insertData(CronEvent.CronEventID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CronEvent}
      
      CronEventBrowse:insertData(STRING(CronEvent.Priority)).
      CronEventBrowse:insertData(IF AVAILABLE CronEventEnvironmentLink THEN STRING(CronEventEnvironmentLink.Active, "Yes/No") 
                                                                       ELSE "No", "left").
      CronEventBrowse:insertData(CronEvent.EventDescr, "left").
      CronEventBrowse:insertData(IF AVAILABLE ProcessProgram THEN ProcessProgram.ProgramName ELSE "", "left").
      CronEventBrowse:insertData(CronEvent.DirectoryToRunFrom, "left").
      CronEventBrowse:insertData(IF AVAILABLE CodePage THEN CodePage.CodePageCode ELSE "", "left").
      CronEventBrowse:insertData(CronEvent.MinuteList, "left").
      CronEventBrowse:insertData(CronEvent.HourList, "left").
      CronEventBrowse:insertData(CronEvent.AverageRunDuration, "right").
      CronEventBrowse:insertData(CronEvent.SecondsToPauseBeforeExecuting).
      CronEventBrowse:insertData(STRING(CronEvent.LoggingIsOn,"Yes/No"), "left").
/*      CronEventBrowse:insertImage("view", "View Change History", 'viewCronEventHistoryBrowse("'*/
/*                                     + STRING(CronEvent.CronEventID) + '", this.parentNode);').*/
            
      /* Add hidden fields */
      CronEventBrowse:insertHiddenData("CronEventVersionID",CronEvent.VersionID).
      CronEventBrowse:insertHiddenData("CronEventDescr" , CronEvent.EventDescr).

      IF AVAILABLE CronEventEnvironmentLink THEN
      DO:
         CronEventBrowse:insertHiddenData("LinkActive", CronEventEnvironmentLink.Active).
         CronEventBrowse:insertHiddenData("CronEventEnvironmentLinkID",CronEventEnvironmentLink.CronEventEnvironmentLinkID).
         CronEventBrowse:insertHiddenData("CronEventEnvironmentLinkVersionID",CronEventEnvironmentLink.VersionID).
      END.
      ELSE 
      DO:
         CronEventBrowse:insertHiddenData("LinkActive","no").
         CronEventBrowse:insertHiddenData("CronEventEnvironmentLinkID","").
         CronEventBrowse:insertHiddenData("CronEventEnvironmentLinkVersionID","").    
      END.

      CronEventBrowse:endRow().
      
   END. /* FOR EACH CronEvent NO-LOCK */
   
   CronEventBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CronEventBrowse:getErrors().
   
   /* Create a new frame */
   CronEventBrowseFrame           = NEW pageFrame().
   CronEventBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CronEventBrowseFrame:formOpen("cronevent_browse_form").
   
   /* Start the Frame Header */
   CronEventBrowseFrame:insertSpacer(5).
   CronEventBrowseFrame:frameOpen(985, 465, "").  
   
   /* This outputs the Browse Table */  
   CronEventBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CronEventBrowseFrame:frameClose().
   CronEventBrowseFrame:insertSpacer(10).
   
   CronEventBrowseFrame:insertHiddenField("cronevent_browse_scroll","").
   CronEventBrowseFrame:insertHiddenField("CronEventID","").
   CronEventBrowseFrame:insertHiddenField("CronEventVersionID","").
   CronEventBrowseFrame:insertHiddenField("CronEventDescr","").
   CronEventBrowseFrame:insertHiddenField("CronEventEnvironmentID","").
   CronEventBrowseFrame:insertHiddenField("CronEventEnvironmentVersionID","").
   CronEventBrowseFrame:insertHiddenField("CronConfigID","").
   CronEventBrowseFrame:insertHiddenField("LinkActive","").
   CronEventBrowseFrame:insertHiddenField("CronEventParameterID","").
   CronEventBrowseFrame:insertHiddenField("ActiveCronConfig", IF AVAILABLE CronConfig THEN "yes" ELSE "no").
   CronEventBrowseFrame:insertHiddenField("form_name","cronevent_browse_form").
   CronEventBrowseFrame:insertHiddenField("prog_name","adCronEventAdmin.p").  
   CronEventBrowseFrame:insertHiddenField("popup_croneventparameter_browse","").
   CronEventBrowseFrame:insertHiddenField("popup_croneventenvironmentlink_browse","").
   CronEventBrowseFrame:insertHiddenField("popup_croneventrunresulttype_browse","").
   CronEventBrowseFrame:insertHiddenField("popup_croneventrun_browse","").
   CronEventBrowseFrame:insertHiddenField("popup_croneventhistory_browse","").
   CronEventBrowseFrame:inserthiddenField("popup_cronconfig_browse","").
   CronEventBrowseFrame:inserthiddenField("popup_cronconfighistory_browse","").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventBrowseFrame}
   
   CronEventBrowseFrame:formClose().
   
   /* Create Button Bar */
   CronEventBrowseButtons           = NEW buttonBar().
   CronEventBrowseButtons:WebStream = STREAM WebStream:HANDLE.
           
   CronEventBrowseButtons:addButton("cronevent_browse_form_btn_config",
                                    fTL("Config"),
                                    "viewCronConfigBrowse();"). 
   
   CronEventBrowseButtons:addButton("cronevent_browse_form_btn_details",
                                    fTL("Create") + " / " + fTL("Edit"),
                                    "viewCronEventDetails('cronevent_details_form');").
   
   CronEventBrowseButtons:addButton("cronevent_browse_form_btn_croneventparameters",
                                    fTL("Parameters"),
                                    "viewCronEventParameterBrowse();",
                                    "Disabled").

   CronEventBrowseButtons:addButton("cronevent_browse_form_btn_environment",
                                    fTL("Environments"),
                                    "viewCronEventEnvironmentLinkBrowse();",
                                    "Disabled").

   CronEventBrowseButtons:addButton("cronevent_browse_form_btn_resulttype",
                                    fTL("Result Type"),
                                    "viewCronEventRunResultTypeBrowse();").

   CronEventBrowseButtons:addButton("cronevent_browse_form_btn_run",
                                    fTL("Run Cron Event"),
                                    "runCronEventConfirm('cronevent_browse_form');",
                                    "Disabled").

   CronEventBrowseButtons:addButton("cronevent_browse_form_btn_previousrun",
                                    fTL("Run History"),
                                    "viewCronEventRunBrowse();",
                                    "Disabled").
      
   CronEventBrowseButtons:closeBar().  
   CronEventBrowseButtons:displayButtonBar().  
   
   /* Create the second Button Bar */   
   CronEventBrowseFrame:insertSpacer(10).
   CronEventBrowseMoreButtons = NEW buttonBar().
   CronEventBrowseMoreButtons:WebStream = STREAM WebStream:HANDLE.                               
   
   CronEventBrowseMoreButtons:addButton("cronevent_browse_form_btn_eventhistory",
                                   fTL("Event History"),
                                   "viewCronEventHistoryBrowse();",
                                   "Disabled").  
                                   
   
   CronEventBrowseMoreButtons:closeBar().  
   CronEventBrowseMoreButtons:displayButtonBar().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventDetails Procedure 
PROCEDURE pCronEventDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "cronevent_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CronEventID,BusinessUnitID,EventDescr,MinuteList,HourList,DaysOfWeekList,DaysOfMonthList,"
                                    + "MonthList,BeginningDate,BeginningHour,BeginningMins,EndingDate,EndingHour,EndingMins,"
                                    + "BeginPauseDate,BeginPauseHour,BeginPauseMins,EndPauseDate,EndPauseHour,"
                                    + "EndPauseMins,Priority,LoggingIsOn,ProcessProgramID,CodePageID,DirectoryToRunFrom,Active,"
                                    + "MinimumRunDuration,MaximumRunDuration,AverageRunDuration,SecondsToPauseBeforeExecuting"
          chrEditFieldList     = "Priority,EventDescr,MinuteList,HourList,DaysOfWeekList,DaysOfMonthList,MonthList,"
                                    + "BeginningDate,BeginningHour,BeginningMins,EndingDate,EndingHour,EndingMins,"
                                    + "BeginPauseDate,BeginPauseHour,BeginPauseMins,EndPauseDate,EndPauseHour,EndPauseMins,BusinessUnitID,"
                                    + "LoggingIsOn,ProcessProgramID,CodePageID,DirectoryToRunFrom,Active,SecondsToPauseBeforeExecuting"
          chrNewFieldList      = "Priority,EventDescr,MinuteList,HourList,DaysOfWeekList,DaysOfMonthList,MonthList,"
                                    + "BeginningDate,BeginningHour,BeginningMins,EndingDate,EndingHour,EndingMins,"
                                    + "BeginPauseDate,BeginPauseHour,BeginPauseMins,EndPauseDate,EndPauseHour,EndPauseMins,BusinessUnitID,"
                                    + "LoggingIsOn,ProcessProgramID,CodePageID,DirectoryToRunFrom,Active,SecondsToPauseBeforeExecuting"
          chrRequiredFieldList = "CodePageID,ProcessProgramID,EventDescr,MinuteList"
          chrExtraFieldList    = ""
          chrValidateFieldList = "Priority:INTEGER>0".
   
   CronEventDetailsForm           = NEW dataForm("cronevent_details_form").
   CronEventDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CronEventDetailsForm:FormAction = "dbCronEventUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronEventDetailsForm:FormWidth  = 860.
   CronEventDetailsForm:FormHeight = 550.
   CronEventDetailsForm:FormTitle  = "CronEvent Details".
   CronEventDetailsForm:FormType   = "xxl_large".
   
   /* Column Layout */
   CronEventDetailsForm:insertPaddingColumn(10).
   CronEventDetailsForm:insertColumn(140).
   CronEventDetailsForm:insertColumn(120).
   CronEventDetailsForm:insertColumn(20).
   CronEventDetailsForm:insertColumn(4).
   CronEventDetailsForm:insertColumn(40).
   
   /* Fields */

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Cron Event ID")).
   CronEventDetailsForm:insertTextField("CronEventID", "", 220, TRUE).  
   
   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Priority")).
   CronEventDetailsForm:insertTextField("Priority", "", 70, TRUE).  
   
   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("CodePage")).
   CronEventDetailsForm:insertComboField("CodePageID", "", 110, TRUE).
   
   FOR EACH CodePage NO-LOCK /* idx=ActiveListingSequence */
      WHERE CodePage.Active
      BY CodePage.ListingSequence:

      CronEventDetailsForm:insertComboPairs("CodePageID", STRING(CodePage.CodePageID), CodePage.CodePageCode).
   END.

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Program Name")).
   CronEventDetailsForm:insertComboField("ProcessProgramID", "", 320, TRUE).
   CronEventDetailsForm:insertComboPairs("ProcessProgramID", "0", "Please select a program...").
   
   FOR EACH ProcessProgram NO-LOCK /* idx=ListingSequence */
      BY ProcessProgram.ProgramName:

      CronEventDetailsForm:insertComboPairs("ProcessProgramID",STRING(ProcessProgram.ProcessProgramID), ProcessProgram.ProgramName).
   END.

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Change Request No")).
   CronEventDetailsForm:insertTextField("DirectoryToRunFrom", "", 495, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Description")).
   CronEventDetailsForm:insertTextField("EventDescr", "", 495, TRUE).
      
   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Minutes")).
   CronEventDetailsForm:insertTextField("MinuteList", "", 495, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Hours")).
   CronEventDetailsForm:insertTextField("HourList", "", 495, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Dates")).
   CronEventDetailsForm:insertTextField("DaysOfMonthList", "", 495, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Months")).
   CronEventDetailsForm:insertTextField("MonthList", "", 220, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("WeekDays")).
   CronEventDetailsForm:insertTextField("DaysOfWeekList", "", 220, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Begins")).
   CronEventDetailsForm:insertDateField("BeginningDate", "", 110, TRUE).
   CronEventDetailsForm:insertTextField("BeginningHour", "", 18, TRUE).
   CronEventDetailsForm:insertLabel(":").
   CronEventDetailsForm:insertTextField("BeginningMins", "",18,TRUE).
   
   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Ends")).
   CronEventDetailsForm:insertDateField("EndingDate", "", 110, TRUE).
   CronEventDetailsForm:insertTextField("EndingHour", "", 18, TRUE).
   CronEventDetailsForm:insertLabel(":").
   CronEventDetailsForm:insertTextField("EndingMins", "", 18, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Start Pause")).
   CronEventDetailsForm:insertDateField("BeginPauseDate", "", 110, TRUE).
   CronEventDetailsForm:insertTextField("BeginPauseHour", "", 18, TRUE).
   CronEventDetailsForm:insertLabel(":").
   CronEventDetailsForm:insertTextField("BeginPauseMins", "", 18, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("End Pause")).
   CronEventDetailsForm:insertDateField("EndPauseDate", "", 110, TRUE).
   CronEventDetailsForm:insertTextField("EndPauseHour", "", 18, TRUE).
   CronEventDetailsForm:insertLabel(":").
   CronEventDetailsForm:insertTextField("EndPauseMins", "", 18, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Logging On")).
   CronEventDetailsForm:insertComboField("LoggingIsOn", "", 110, TRUE).
   CronEventDetailsForm:insertComboPairs("LoggingIsOn", "yes", "Logging On").
   CronEventDetailsForm:insertComboPairs("LoggingIsOn", "no", "Logging Off").

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Business Unit")). 
   CronEventDetailsForm:insertComboField("BusinessUnitID", "0", 280, TRUE).  
   
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK /* idx=ActiveListingSequence */
      WHERE BusinessUnit.Active
      BY BusinessUnit.ListingSequence:

      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.

      CronEventDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID), BusinessUnit.UnitName).
   END.

   /* CronEventEnvironmentLink.Active */
   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("EnvironmentLinkActive")).
   CronEventDetailsForm:insertComboField("Active", "", 110, TRUE).
   CronEventDetailsForm:insertComboPairs("Active", "yes", "Active").
   CronEventDetailsForm:insertComboPairs("Active", "no", "Not Active").

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Min Run Duration (in S)")).
   CronEventDetailsForm:insertTextField("MinimumRunDuration", "", 70, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Max Run Duration [(in S)]")).
   CronEventDetailsForm:insertTextField("MaximumRunDuration", "", 70, TRUE).

   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Avg Run Duration [(in S)]")).
   CronEventDetailsForm:insertTextField("AverageRunDuration", "", 70, TRUE).
   
   CronEventDetailsForm:startRow().
   CronEventDetailsForm:insertLabel(fTL("Execution Delay [(in S)]")).
   CronEventDetailsForm:insertTextField("SecondsToPauseBeforeExecuting", "", 70, TRUE).
      
   /* Add Hidden Fields*/
   CronEventDetailsForm:insertHiddenField("cronevent_browse_scroll", "").
   CronEventDetailsForm:insertHiddenField("form_name", "cronevent_details_form").
   CronEventDetailsForm:insertHiddenField("prog_name", "adCronEventAdmin.p").
   CronEventDetailsForm:insertHiddenField("CronEventEnvironmentLinkID","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventDetailsForm}
   
   /* Create Button Bar */
   CronEventDetailsButtons = NEW buttonBar().

   CronEventDetailsButtons:addButton("cronevent_details_form_btn_create",
                                     fTL("Create"),
                                     "createCronEvent('cronevent_details_form');").
   
   CronEventDetailsButtons:addButton("cronevent_details_form_btn_save", 
                                     fTL("Save"), 
                                     "updateCronEvent('cronevent_details_form');").
   
   CronEventDetailsButtons:addButton("cronevent_details_form_btn_cancel", 
                                     fTL("Cancel"), 
                                     "cancelUpdate('UserCancelled','process_mode'); disablePopup('cronevent_details_form_popup');").
   
   CronEventDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventDetailsForm:FormButtons = CronEventDetailsButtons.
   
   CronEventDetailsForm:endForm(). 
   
   CronEventDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventEnvironmentLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventEnvironmentLinkBrowse Procedure 
PROCEDURE pCronEventEnvironmentLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {webGetWebForm.i "croneventenvironmentlink_details_form"}
   
   CronEventEnvironmentLinkBrowseForm = NEW dataForm("croneventenvironmentlink_browse_form").
   CronEventEnvironmentLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CronEventEnvironmentLinkBrowseForm:FormAction  = "dbCronEventEnvironmentLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronEventEnvironmentLinkBrowseForm:FormWidth   = 580.
   CronEventEnvironmentLinkBrowseForm:FormHeight  = 420.
   CronEventEnvironmentLinkBrowseForm:FormTitle   = fTL("CronEventEnvironmentLink") + (IF AVAILABLE CronEvent THEN " : " 
                                                       + CronEvent.EventDescr ELSE "").
                                               
   CronEventEnvironmentLinkBrowseForm:FormType    = "large".
   
   CronEventEnvironmentLinkBrowse = NEW browseTable("croneventenvironmentlink_browse").
   CronEventEnvironmentLinkBrowse:BrowseWidth  = 560.
   CronEventEnvironmentLinkBrowse:BrowseHeight = 375.
   
   CronEventEnvironmentLinkBrowse:insertColumn(fTL("ID"), 100, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronEventEnvironmentLink}
   
   CronEventEnvironmentLinkBrowse:insertColumn(fTL("Environment"), 200, "CHARACTER", "left").
   CronEventEnvironmentLinkBrowse:insertColumn(fTL("Active"),       90, "LOGICAL",   "left").

   CronEventEnvironmentLinkBrowse:StartBody().
   
   /* List the CronEventEnvironmentLinks for the CronEvent */
   FOR EACH CronEventEnvironmentLink NO-LOCK /* idx=CronEventIDActive */
      WHERE CronEventEnvironmentLink.CronEventID = intSelectedCronEvent:

      FIND FIRST Environment OF CronEventEnvironmentLink NO-LOCK NO-ERROR.
               
      CronEventEnvironmentLinkBrowse:startRow(CronEventEnvironmentLink.CronEventEnvironmentLinkID, "selectCronEventEnvironmentLinkRow(this," 
                                        + '"' + STRING(CronEventEnvironmentLink.CronEventEnvironmentLinkID)
                                        + '","adCronEventAdmin.p","croneventenvironmentlink_browse_form"' + ");", ""). 
                                            
      CronEventEnvironmentLinkBrowse:insertData(CronEventEnvironmentLink.CronEventEnvironmentLinkID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CronEventEnvironmentLink}
           
      CronEventEnvironmentLinkBrowse:insertData(IF AVAILABLE Environment THEN Environment.EnvironmentCode ELSE "","left").
      CronEventEnvironmentLinkBrowse:insertData(STRING(CronEventEnvironmentLink.Active, "Yes/No"), "left").
      
      /* Add hidden fields */
      CronEventEnvironmentLinkBrowse:insertHiddenData("CronEventEnvironmentLinkVersionID",CronEventEnvironmentLink.VersionID).

      CronEventEnvironmentLinkBrowse:endRow().
   
   END. /* FOR EACH CronEventEnvironmentLink OF CronEvent NO-LOCK */
   
   CronEventEnvironmentLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CronEventEnvironmentLinkBrowse:getErrors().
   
   CronEventEnvironmentLinkBrowseForm:insertHiddenField("CronEventEnvironmentLinkID",chrCronEventEnvironmentLinkID).
   CronEventEnvironmentLinkBrowseForm:insertHiddenField("CronEventEnvironmentLinkVersionID","").
   CronEventEnvironmentLinkBrowseForm:insertHiddenField("cronevent_browse_scroll","").   
   CronEventEnvironmentLinkBrowseForm:insertHiddenField("popup_croneventenvironmentlink_browse","").
   CronEventEnvironmentLinkBrowseForm:insertHiddenField("form_name","croneventenvironmentlink_browse_form").
   CronEventEnvironmentLinkBrowseForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventEnvironmentLinkBrowseForm}
   
   /* Create Button Bar */
   CronEventEnvironmentLinkBrowseButtons = NEW buttonBar().
   
   CronEventEnvironmentLinkBrowseButtons:addButton("croneventenvironmentlink_browse_form_btn_create",
                                                   fTL("Create"),
                                                   "createCronEventEnvironmentLink('croneventenvironmentlink_details_form');"). 

   CronEventEnvironmentLinkBrowseButtons:addButton("croneventenvironmentlink_browse_form_btn_view",
                                                   fTL("Details"),
                                                   "viewCronEventEnvironmentLinkDetails('croneventenvironmentlink_details_form');",
                                                   "Disabled").
        
   CronEventEnvironmentLinkBrowseButtons:addButton("croneventenvironmentlink_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('croneventenvironmentlink_browse_form_popup');").
   
   CronEventEnvironmentLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventEnvironmentLinkBrowseForm:FormBrowse  = CronEventEnvironmentLinkBrowse.
   CronEventEnvironmentLinkBrowseForm:FormButtons = CronEventEnvironmentLinkBrowseButtons.
   CronEventEnvironmentLinkBrowseForm:endForm(). 
   
   CronEventEnvironmentLinkBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventEnvironmentLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventEnvironmentLinkDetails Procedure 
PROCEDURE pCronEventEnvironmentLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "croneventenvironmentlink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CronEventEnvironmentLinkID,EnvironmentID,Active"
          chrEditFieldList     = "Active"
          chrNewFieldList      = "EnvironmentID,Active"
          chrRequiredFieldList = "EnvironmentID,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CronEventEnvironmentLinkDetailsForm = NEW dataForm("croneventenvironmentlink_details_form").
   CronEventEnvironmentLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CronEventEnvironmentLinkDetailsForm:FormAction  = "dbCronEventEnvironmentLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronEventEnvironmentLinkDetailsForm:FormWidth   = 460.
   CronEventEnvironmentLinkDetailsForm:FormHeight  = 300.
   CronEventEnvironmentLinkDetailsForm:FormTitle   = "CronEventEnvironmentLink".
   CronEventEnvironmentLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CronEventEnvironmentLinkDetailsForm:insertPaddingColumn(10).
   CronEventEnvironmentLinkDetailsForm:insertColumn(180).
   CronEventEnvironmentLinkDetailsForm:insertColumn(120).
   CronEventEnvironmentLinkDetailsForm:insertColumn(20).
   CronEventEnvironmentLinkDetailsForm:insertColumn(4).
   CronEventEnvironmentLinkDetailsForm:insertColumn(40).
   
   /* Fields */
   CronEventEnvironmentLinkDetailsForm:startRow().
   CronEventEnvironmentLinkDetailsForm:insertLabel(fTL("CronEventEnvironmentLink ID")).
   CronEventEnvironmentLinkDetailsForm:insertTextField("CronEventEnvironmentLinkID", "", 90, TRUE).    
   
   CronEventEnvironmentLinkDetailsForm:startRow().
   CronEventEnvironmentLinkDetailsForm:insertLabel(fTL("Environment")).
   CronEventEnvironmentLinkDetailsForm:insertComboField("EnvironmentID", "", 260, TRUE).  

   FOR EACH Environment NO-LOCK /* idx=ActiveListingSequence */
      WHERE Environment.Active
      BY Environment.ListingSequence:

      CronEventEnvironmentLinkDetailsForm:insertComboPairs("EnvironmentID", STRING(Environment.EnvironmentID),Environment.EnvironmentCode).
   END.
   
   CronEventEnvironmentLinkDetailsForm:startRow().
   CronEventEnvironmentLinkDetailsForm:insertLabel(fTL("Active")).
   CronEventEnvironmentLinkDetailsForm:insertComboField("Active", "", 110, TRUE).
   CronEventEnvironmentLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   CronEventEnvironmentLinkDetailsform:insertComboPairs("Active", "no", "Not Active").
   
   /* Add Hidden Fields*/
   CronEventEnvironmentLinkDetailsForm:insertHiddenField("cronevent_browse_scroll","").
   CronEventEnvironmentLinkDetailsForm:insertHiddenField("popup_croneventenvironmentlink_browse", "").
   CronEventEnvironmentLinkDetailsForm:insertHiddenField("CronEventID", IF intSelectedCronEvent = 0 THEN "" 
                                                                                                    ELSE STRING(intSelectedCronEvent)).
   CronEventEnvironmentLinkDetailsForm:insertHiddenField("CronEventEnvironmentLinkID",STRING(intSelectedCronEventEnvironmentLink)).
   CronEventEnvironmentLinkDetailsForm:insertHiddenField("form_name","croneventenvironmentlink_details_form").
   CronEventEnvironmentLinkDetailsForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventEnvironmentLinkDetailsForm}
   
   /* Create Button Bar */
   CronEventEnvironmentLinkDetailsButtons = NEW buttonBar().
   
   CronEventEnvironmentLinkDetailsButtons:addButton("croneventenvironmentlink_details_form_btn_save", 
                                                    fTL("Save"), 
                                                    "updateCronEventEnvironmentLink('croneventenvironmentlink_details_form');").
   
   CronEventEnvironmentLinkDetailsButtons:addButton("croneventenvironmentlink_details_form_btn_cancel", 
                                                    fTL("Cancel"), 
                                                    "cancelUpdate('UserCancelled','process_mode'); " 
                                                       + "disablePopup('croneventenvironmentlink_details_form_popup');").
                                    
   CronEventEnvironmentLinkDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventEnvironmentLinkDetailsForm:FormButtons = CronEventEnvironmentLinkDetailsButtons.
   
   CronEventEnvironmentLinkDetailsForm:endForm(). 
   CronEventEnvironmentLinkDetailsForm:displayForm(). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventHistoryBrowse Procedure 
PROCEDURE pCronEventHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcesEvent which is linked to the form associated with this browse */     
   
   CronEventHistoryBrowseForm = NEW dataForm("croneventhistory_browse_form").
   CronEventHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   CronEventHistoryBrowseForm:FormWidth  = 860.
   CronEventHistoryBrowseForm:FormHeight = 530.
   CronEventHistoryBrowseForm:FormTitle  = fTL("Cron Event History") + (IF AVAILABLE CronEvent THEN ": " + CronEvent.EventDescr ELSE "").
   CronEventHistoryBrowseForm:FormType   = "xxl_large".
   
   CronEventHistoryBrowse = NEW browseTable("croneventhistory_browse").
   CronEventHistoryBrowse:BrowseWidth  = 840.
   CronEventHistoryBrowse:BrowseHeight = 490.
   
   CronEventHistoryBrowse:insertColumn(fTL("ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronEventHistory}
   
   CronEventHistoryBrowse:insertColumn(fTL("Gate User"), 100, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("Created"),    70, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("CP"),         55, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("Program"),   110, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("Path"),       45, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("Minutes"),    60, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("Hours"),      50, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("WeekDays"),   70, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("Days"),       50, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("Months"),     60, "CHARACTER", "left").
   CronEventHistoryBrowse:insertColumn(fTL("ExecDelay"),  70, "CHARACTER", "left").
   
   CronEventHistoryBrowse:StartBody().
   
   IF AVAILABLE CronEvent THEN
   DO:
      /* List the CronEventHistorys for the CronEvent */
      FOR EACH CronEventHistory NO-LOCK /* idx=CronEventID */
         WHERE CronEventHistory.CronEventID = intSelectedCronEvent 
         BY CronEventHistory.CronEventID
         BY CronEventHistory.Created DESCENDING:

         FIND FIRST GateUser OF CronEventHistory NO-LOCK NO-ERROR.
         
         FIND FIRST CodePage OF CronEventHistory NO-LOCK NO-ERROR.

         FIND FIRST ProcessProgram OF CronEventHistory NO-LOCK NO-ERROR.
         
         CronEventHistoryBrowse:startRow(CronEventHistory.CronEventHistoryID, "selectCronEventHistoryRow(this," 
                                            + '"' + STRING(CronEventHistory.CronEventHistoryID)
                                            + '","adCronEventAdmin.p","cronevent_browse_form"' + ");", "").

         CronEventHistoryBrowse:insertData(CronEventHistory.CronEventHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CronEventHistory}
         
         CronEventHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         CronEventHistoryBrowse:insertData(fDisplayDate&Time(CronEventHistory.Created, "d/m/y H:M")).
         CronEventHistoryBrowse:insertData(IF AVAILABLE CodePage THEN CodePage.CodePageCode ELSE "", "left").
         CronEventHistoryBrowse:insertData(IF AVAILABLE ProcessProgram THEN ProcessProgram.ProgramName ELSE "", "left").
         CronEventHistoryBrowse:insertData(CronEventHistory.DirectoryToRunFrom).
         CronEventHistoryBrowse:insertData(CronEventHistory.MinuteList, "left").
         CronEventHistoryBrowse:insertData(CronEventHistory.HourList, "left").
         CronEventHistoryBrowse:insertData(CronEventHistory.DaysOfWeekList, "left").
         CronEventHistoryBrowse:insertData(CronEventHistory.DaysOfMonthList, "left").
         CronEventHistoryBrowse:insertData(CronEventHistory.MonthList, "left").
         CronEventHistoryBrowse:insertData(CronEventHistory.SecondsToPauseBeforeExecuting).
          
          
          /* Add hidden fields */
         CronEventHistoryBrowse:insertHiddenData("CronEventID", CronEvent.CronEventID).
         CronEventHistoryBrowse:insertHiddenData("CronEventVersionID", CronEvent.VersionID).
         
         CronEventHistoryBrowse:endRow().
      
      END. /* FOR EACH CronEventHistory OF CronEvent NO-LOCK */
   END. /*IF AVAILABLE CronEvent THEN*/
   
   CronEventHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + CronEventHistoryBrowse:getErrors().

   CronEventHistoryBrowseForm:insertHiddenField("popup_croneventhistory_browse","").
   CronEventHistoryBrowseForm:insertHiddenField("CronEventID", "").
   CronEventHistoryBrowseForm:insertHiddenField("CronEventVersionID", ""). 
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventHistoryBrowseForm}
   
   /* Create Button Bar */
   CronEventHistoryBrowseButtons = NEW buttonBar().
          
   CronEventHistoryBrowseButtons:addButton("croneventhistory_browse_form_btn_cancel",
                                           fTL("Cancel"),
                                           "disablePopup('croneventhistory_browse_form_popup');").
   
   CronEventHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventHistoryBrowseForm:FormBrowse  = CronEventHistoryBrowse.
   CronEventHistoryBrowseForm:FormButtons = CronEventHistoryBrowseButtons.
   CronEventHistoryBrowseForm:endForm(). 
   
   CronEventHistoryBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventParameterBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventParameterBrowse Procedure 
PROCEDURE pCronEventParameterBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcesEvent which is linked to the form associated with this browse */

   {webGetWebForm.i "croneventparameter_details_form"}
   
   CronEventParameterBrowseForm = NEW dataForm("croneventparameter_browse_form").
   CronEventParameterBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CronEventParameterBrowseForm:FormAction  = "dbCronEventParameterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronEventParameterBrowseForm:FormWidth   = 700.
   CronEventParameterBrowseForm:FormHeight  = 490.
   CronEventParameterBrowseForm:FormTitle   = fTL("Cron Event Parameters") + (IF AVAILABLE CronEvent THEN " : " 
                                                 + CronEvent.EventDescr ELSE "").
   CronEventParameterBrowseForm:FormType    = "xl_large".
   
   CronEventParameterBrowse = NEW browseTable("croneventparameter_browse").
   CronEventParameterBrowse:BrowseWidth  = 680.
   CronEventParameterBrowse:BrowseHeight = 432.
   
   CronEventParameterBrowse:insertColumn(fTL("ParamID"), 80, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronEventParameter}
   
   CronEventParameterBrowse:insertColumn(fTL("Name"),        150, "CHARACTER", "left").
   CronEventParameterBrowse:insertColumn(fTL("Value"),       175, "CHARACTER", "left").
   CronEventParameterBrowse:insertColumn(fTL("Description"), 200, "CHARACTER", "left").
   
   CronEventParameterBrowse:StartBody().
   
   IF AVAILABLE CronEvent THEN
   DO:
      /* List the CronEventParameters for the CronEvent */
      FOR EACH CronEventParameter OF CronEvent NO-LOCK: /* idx=CronEventIDParameterName */
         
         CronEventParameterBrowse:startRow(CronEventParameter.CronEventParameterID, "selectCronEventParameterRow(this," 
                                             + '"' + STRING(CronEventParameter.CronEventParameterID)
                                             + '","adCronEventAdmin.p","cronevent_browse_form"' + ");", "").
         CronEventParameterBrowse:insertData(CronEventParameter.CronEventParameterID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CronEventParameter}
         
         CronEventParameterBrowse:insertData(CronEventParameter.ParameterName, "left").
         CronEventParameterBrowse:insertData(CronEventParameter.ParameterValue, "left").
         CronEventParameterBrowse:insertData(CronEventParameter.ParameterDescr, "left").
         
         /* Add hidden fields */
         CronEventParameterBrowse:insertHiddenData("CronEventParameterVersionID",CronEventParameter.VersionID).
         CronEventParameterBrowse:insertHiddenData("CronEventID", CronEventParameter.CronEventID).
         CronEventParameterBrowse:insertHiddenData("CronEventVersionID", CronEvent.VersionID). 

         CronEventParameterBrowse:endRow().
      
      END. /* FOR EACH CronEventParameter OF CronEvent NO-LOCK */
   END. /*IF AVAILABLE CronEvent THEN*/
   
   CronEventParameterBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CronEventParameterBrowse:getErrors().
   
   CronEventParameterBrowseForm:insertHiddenField("CronEventParameterID","").
   CronEventParameterBrowseForm:insertHiddenField("CronEventParameterVersionID","").
   CronEventParameterBrowseForm:insertHiddenField("CronEventID",       (IF AVAILABLE CronEvent THEN STRING(CronEvent.CronEventID) ELSE "")).
   CronEventParameterBrowseForm:insertHiddenField("CronEventVersionID",(IF AVAILABLE CronEvent THEN STRING(CronEvent.VersionID) ELSE "")).
   CronEventParameterBrowseForm:insertHiddenField("cronevent_browse_scroll","").
   CronEventParameterBrowseForm:insertHiddenField("popup_croneventparameter_browse","").

   CronEventParameterBrowseForm:insertHiddenField("form_name","croneventparameter_browse_form").
   CronEventParameterBrowseForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventParameterBrowseForm}
   
   /* Create Button Bar */
   CronEventParameterBrowseButtons = NEW buttonBar().
   
   CronEventParameterBrowseButtons:addButton("croneventparameter_browse_form_btn_create",
                                             fTL("Create"),
                                             "createCronEventParameter('croneventparameter_details_form');"). 

   CronEventParameterBrowseButtons:addButton("croneventparameter_browse_form_btn_view",
                                             fTL("Details"),
                                             "viewCronEventParameterDetails('croneventparameter_details_form');",
                                             "Disabled").
     
   CronEventParameterBrowseButtons:addButton("croneventparameter_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteCronEventParameter();", 
                                             "Disabled").
   
   CronEventParameterBrowseButtons:addButton("croneventparameter_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('croneventparameter_browse_form_popup');").
   
   CronEventParameterBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventParameterBrowseForm:FormBrowse  = CronEventParameterBrowse.
   CronEventParameterBrowseForm:FormButtons = CronEventParameterBrowseButtons.
   CronEventParameterBrowseForm:endForm(). 
   
   CronEventParameterBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventParameterDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventParameterDetails Procedure 
PROCEDURE pCronEventParameterDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "croneventparameter_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CronEventParameterID,ParameterName,ParameterValue,ParameterDescr,Mandatory,Dynamic"
          chrEditFieldList     = "ParameterName,ParameterValue,ParameterDescr,Mandatory,Dynamic"
          chrNewFieldList      = chrEditFieldList
          chrRequiredFieldList = "ParameterName,ParameterValue"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CronEventParameterDetailsForm = NEW dataForm("croneventparameter_details_form").
   CronEventParameterDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CronEventParameterDetailsForm:FormAction  = "dbCronEventParameterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronEventParameterDetailsForm:FormWidth   = 460.
   CronEventParameterDetailsForm:FormHeight  = 300.
   CronEventParameterDetailsForm:FormTitle   = "Cron Event Parameter Details".
   CronEventParameterDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CronEventParameterDetailsForm:insertPaddingColumn(70).
   CronEventParameterDetailsForm:insertColumn(120).
   CronEventParameterDetailsForm:insertColumn(120).
   CronEventParameterDetailsForm:insertColumn(30).
   CronEventParameterDetailsForm:insertColumn(120).  
   
   /* Fields */
   CronEventParameterDetailsForm:startRow().
   CronEventParameterDetailsForm:insertLabel(fTL("Param ID")).
   CronEventParameterDetailsForm:insertTextField("CronEventParameterID", "", 90, TRUE).    
   
   CronEventParameterDetailsForm:startRow().
   CronEventParameterDetailsForm:insertLabel(fTL("Parameter Name")).
   CronEventParameterDetailsForm:insertTextField("ParameterName", "", 160, TRUE).   
   
   CronEventParameterDetailsForm:startRow().
   CronEventParameterDetailsForm:insertLabel(fTL("Parameter Value")).
   CronEventParameterDetailsForm:insertTextField("ParameterValue", "", 160, TRUE).  
   
   CronEventParameterDetailsForm:startRow().
   CronEventParameterDetailsForm:insertLabel(fTL("Parameter Descr")).
   CronEventParameterDetailsForm:insertTextField("ParameterDescr", "", 160, TRUE).  
   
   /* Add Hidden Fields*/
   CronEventParameterDetailsForm:insertHiddenField("cronevent_browse_scroll","").
   CronEventParameterDetailsForm:insertHiddenField("popup_croneventparameter_browse", "").
   CronEventParameterDetailsForm:insertHiddenField("CronEventID",STRING(intSelectedCronEvent)).
   CronEventParameterDetailsForm:insertHiddenField("CronEventParameterID",STRING(intSelectedCronEventParameter)).
   CronEventParameterDetailsForm:insertHiddenField("form_name","croneventparameter_details_form").
   CronEventParameterDetailsForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventParameterDetailsForm}
   
   /* Create Button Bar */
   CronEventParameterDetailsButtons = NEW buttonBar().
   
   CronEventParameterDetailsButtons:addButton("croneventparameter_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateCronEventParameter('croneventparameter_details_form');").
   
   CronEventParameterDetailsButtons:addButton("croneventparameter_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); " 
                                                 + "disablePopup('croneventparameter_details_form_popup');").
   CronEventParameterDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventParameterDetailsForm:FormButtons = CronEventParameterDetailsButtons.
   
   CronEventParameterDetailsForm:endForm(). 
   CronEventParameterDetailsForm:displayForm(). 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventRunBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventRunBrowse Procedure 
PROCEDURE pCronEventRunBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcesEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "croneventrun_details_form"}
   
   CronEventRunBrowseForm = NEW dataForm("croneventrun_browse_form").
   CronEventRunBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CronEventRunBrowseForm:FormWidth   = 700.
   CronEventRunBrowseForm:FormHeight  = 490.
   CronEventRunBrowseForm:FormTitle   = fTL("Cron Event Run") + (IF AVAILABLE CronEvent THEN " : " + CronEvent.EventDescr ELSE "").
   CronEventRunBrowseForm:FormType    = "xl_large".
   
   CronEventRunBrowse = NEW browseTable("croneventrun_browse").
   CronEventRunBrowse:BrowseWidth  = 680.
   CronEventRunBrowse:BrowseHeight = 432.
   
   CronEventRunBrowse:insertColumn(fTL("RunID"), 90, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronEventRun}
   
   CronEventRunBrowse:insertColumn(fTL("Result"),     140, "CHARACTER", "left").
   CronEventRunBrowse:insertColumn(fTL("Gate User"),  130, "CHARACTER", "left").
   CronEventRunBrowse:insertColumn(fTL("Started"),    120, "CHARACTER", "left").
   CronEventRunBrowse:insertColumn(fTL("Time Taken"),  80, "INTEGER", "left").
   CronEventRunBrowse:insertColumn(fTL("Unix PID"),    70, "INTEGER", "left").
   
   CronEventRunBrowse:StartBody().

   intRecordCounter = 0.
   
   IF AVAILABLE CronEvent THEN
   DO:
      /* List the CronEventRuns for the CronEvent */
      FOR EACH CronEventRun NO-LOCK /* idx=CronEventIDStarted */
         WHERE CronEventRun.CronEventID = CronEvent.CronEventID
         BY CronEventRun.CronEventID
         BY CronEventRun.Started DESCENDING:

         intRecordCounter = intRecordCounter + 1.

         FIND FIRST CronEventRunResultType OF CronEventRun NO-LOCK NO-ERROR.

         FIND FIRST GateUser OF CronEventRun NO-LOCK NO-ERROR.
         
         CronEventRunBrowse:startRow(CronEventRun.CronEventRunID, "selectCronEventRunRow(this," 
                                        + '"' + STRING(CronEventRun.CronEventRunID)
                                        + '","adCronEventAdmin.p","cronevent_browse_form"' + ");", "").
         CronEventRunBrowse:insertData(CronEventRun.CronEventRunID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CronEventRun}
         
         CronEventRunBrowse:insertData(IF AVAILABLE CronEventRunResultType THEN CronEventRunResultType.TypeName ELSE "", "left").
         CronEventRunBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         CronEventRunBrowse:insertData(fDisplayDate&Time(CronEventRun.Started, "d/m/y H:M"), "left").
         CronEventRunBrowse:insertData(IF CronEventRun.Completed <> '' THEN STRING(fInterval(CronEventRun.Completed,
                                                                                             CronEventRun.Started,
                                                                                             "Seconds"))
                                                                       ELSE "", "right").
         CronEventRunBrowse:insertData(STRING(UnixPID), "right").
         
         /* Add hidden fields */
         CronEventRunBrowse:insertHiddenData("CronEventID", CronEventRun.CronEventID).
         CronEventRunBrowse:insertHiddenData("CronEventVersionID", CronEvent.VersionID). 

         CronEventRunBrowse:endRow().
      
         /* Limit number record return to 100 */
         IF intRecordCounter > 100 THEN
            LEAVE.
      END. /* FOR EACH CronEventRun OF CronEvent NO-LOCK */
   END. /*IF AVAILABLE CronEvent THEN*/
   
   CronEventRunBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CronEventRunBrowse:getErrors().
   
   CronEventRunBrowseForm:insertHiddenField("CronEventRunID","").
   CronEventRunBrowseForm:insertHiddenField("CronEventID",        (IF AVAILABLE CronEvent THEN STRING(CronEvent.CronEventID) ELSE "")).
   CronEventRunBrowseForm:insertHiddenField("CronEventVersionID", (IF AVAILABLE CronEvent THEN STRING(CronEvent.VersionID) ELSE "")).
   CronEventRunBrowseForm:insertHiddenField("cronevent_browse_scroll","").
   CronEventRunBrowseForm:insertHiddenField("popup_croneventrun_browse","").

   CronEventRunBrowseForm:insertHiddenField("form_name","croneventrun_browse_form").
   CronEventRunBrowseForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventRunBrowseForm}
   
   /* Create Button Bar */
   CronEventRunBrowseButtons = NEW buttonBar().
   
   CronEventRunBrowseButtons:addButton("croneventrun_browse_form_btn_view",
                                       fTL("Details"),
                                       "viewCronEventRunDetails('croneventrun_details_form');",
                                       "Disabled").
        
   CronEventRunBrowseButtons:addButton("croneventrun_browse_form_btn_cancel",
                                       fTL("Cancel"),
                                       "disablePopup('croneventrun_browse_form_popup');").
   
   CronEventRunBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventRunBrowseForm:FormBrowse  = CronEventRunBrowse.
   CronEventRunBrowseForm:FormButtons = CronEventRunBrowseButtons.
   CronEventRunBrowseForm:endForm(). 
   
   CronEventRunBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventRunDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventRunDetails Procedure 
PROCEDURE pCronEventRunDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "croneventrun_details_form"}
   
   chrDisplayFieldList  = "CronEventRunID,CronEventRunResultTypeID,GateUserID,StartedDate,StartedHour,StartedMins,CompletedDate,"
                             + "CompletedHour,CompletedMins,FinishedOk,LogFile,ErrorLogFile,UnixPID".
   
   CronEventRunDetailsForm = NEW dataForm("croneventrun_details_form").
   CronEventRunDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   CronEventRunDetailsForm:FormWidth   = 460.
   CronEventRunDetailsForm:FormHeight  = 300.
   CronEventRunDetailsForm:FormTitle   = "Cron Event Run Details".
   CronEventRunDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CronEventRunDetailsForm:insertPaddingColumn(40).
   CronEventRunDetailsForm:insertColumn(110).
   CronEventRunDetailsForm:insertColumn(120).
   CronEventRunDetailsForm:insertColumn(20).
   CronEventRunDetailsForm:insertColumn(4).
   CronEventRunDetailsForm:insertColumn(40).  
   
   /* Fields */
   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Run ID")).
   CronEventRunDetailsForm:insertTextField("CronEventRunID", "", 90, TRUE).    
   
   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Result")).
   CronEventRunDetailsForm:insertComboField("CronEventRunResultTypeID", "", 160, TRUE).
   
   FOR EACH CronEventRunResultType NO-LOCK /* idx=ActiveListingSequence */
      WHERE CronEventRunResultType.Active
      BY CronEventRunResultType.ListingSequence:

      CronEventRunDetailsForm:insertComboPairs("CronEventRunResultTypeID",STRING(CronEventRunResultTypeID),CronEventRunResultType.TypeName).
   END.
   
   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Gate User")).
   CronEventRunDetailsForm:insertComboField("GateUserID", "", 160, TRUE).
     
   FOR EACH GateUser NO-LOCK: /* idx=GateUserID */

      CronEventRunDetailsForm:insertComboPairs("GateUserID", STRING(GateUserID),GateUser.FullName).
   END.
   
   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Started")).
   CronEventRunDetailsForm:insertDateField("StartedDate", "",  110, TRUE).
   CronEventRunDetailsForm:insertTextField("StartedHour", "00", 18, TRUE).
   CronEventRunDetailsForm:insertLabel(":").
   CronEventRunDetailsForm:insertTextField("StartedMins", "00", 18, TRUE).

   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Completed")). 
   CronEventRunDetailsForm:insertDateField("CompletedDate", "",  110, TRUE).
   CronEventRunDetailsForm:insertTextField("CompletedHour", "", 18, TRUE).
   CronEventRunDetailsForm:insertLabel(":").
   CronEventRunDetailsForm:insertTextField("CompletedMins", "", 18, TRUE).
   
   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Finished Ok")). 
   CronEventRunDetailsForm:insertComboField("FinishedOk", "", 160, TRUE).  
   CronEventRunDetailsForm:insertComboPairs("FinishedOk", "yes", "Finished Ok").
   CronEventRunDetailsForm:insertComboPairs("FinishedOk", "no",  "Not Finished Ok").

   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Unix PID")).
   CronEventRunDetailsForm:insertTextField("UnixPID", "", 90, TRUE).

   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Log File")).
   CronEventRunDetailsForm:insertTextField("LogFile", "", 200, TRUE).

   CronEventRunDetailsForm:startRow().
   CronEventRunDetailsForm:insertLabel(fTL("Error Log File")).
   CronEventRunDetailsForm:insertTextField("ErrorLogFile", "", 200, TRUE).
   
   /* Add Hidden Fields*/
   CronEventRunDetailsForm:insertHiddenField("cronevent_browse_scroll","").
   CronEventRunDetailsForm:insertHiddenField("popup_croneventrun_browse", "").
   CronEventRunDetailsForm:insertHiddenField("CronEventID",STRING(intSelectedCronEvent)).
   CronEventRunDetailsForm:insertHiddenField("CronEventRunID",STRING(intSelectedCronEventRun)).
   CronEventRunDetailsForm:insertHiddenField("form_name","croneventrun_details_form").
   CronEventRunDetailsForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventRunDetailsForm}
   
   /* Create Button Bar */
   CronEventRunDetailsButtons = NEW buttonBar().
   
   CronEventRunDetailsButtons:addButton("croneventrun_details_form_btn_cancel", 
                                        fTL("Cancel"), 
                                        "cancelUpdate('UserCancelled','process_mode'); " 
                                           + "disablePopup('croneventrun_details_form_popup');").
   CronEventRunDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventRunDetailsForm:FormButtons = CronEventRunDetailsButtons.
   
   CronEventRunDetailsForm:endForm(). 
   CronEventRunDetailsForm:displayForm(). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventRunResultTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventRunResultTypeBrowse Procedure 
PROCEDURE pCronEventRunResultTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {webGetWebForm.i "croneventrunresulttype_details_form"}
   
   CronEventRunResultTypeBrowseForm = NEW dataForm("croneventrunresulttype_browse_form").
   CronEventRunResultTypeBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CronEventRunResultTypeBrowseForm:FormAction  = "dbCronEventRunResultTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronEventRunResultTypeBrowseForm:FormWidth   = 700.
   CronEventRunResultTypeBrowseForm:FormHeight  = 490.
   CronEventRunResultTypeBrowseForm:FormTitle   = fTL("Cron Event Run Result Type").
                                               
   CronEventRunResultTypeBrowseForm:FormType    = "xl_large".
   
   CronEventRunResultTypeBrowse = NEW browseTable("croneventrunresulttype_browse").
   CronEventRunResultTypeBrowse:BrowseWidth  = 680.
   CronEventRunResultTypeBrowse:BrowseHeight = 432.
   
   CronEventRunResultTypeBrowse:insertColumn(fTL("Type ID"), 80, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CronEventRunResultType}
   
   CronEventRunResultTypeBrowse:insertColumn(fTL("Type Code"),   110, "CHARACTER", "left").
   CronEventRunResultTypeBrowse:insertColumn(fTL("Type Name"),   110, "CHARACTER", "left").
   CronEventRunResultTypeBrowse:insertColumn(fTL("Description"), 120, "CHARACTER", "left").
   CronEventRunResultTypeBrowse:insertColumn(fTL("List Seq"),     60, "INTEGER",   "left").
   CronEventRunResultTypeBrowse:insertColumn(fTL("IsError"),      60, "LOGICAL",   "left").
   CronEventRunResultTypeBrowse:insertColumn(fTL("Active"),       60, "LOGICAL",   "left").
   
   CronEventRunResultTypeBrowse:StartBody().
   
   /* List the CronEventRunResultTypes for the CronEvent */
   FOR EACH CronEventRunResultType NO-LOCK /* idx=ActiveListingSequence */
      BY CronEventRunResultType.Active
      BY CronEventRunResultType.ListingSequence:
      
      CronEventRunResultTypeBrowse:startRow(CronEventRunResultType.CronEventRunResultTypeID, "selectCronEventRunResultTypeRow(this," 
                                               + '"' + STRING(CronEventRunResultType.CronEventRunResultTypeID)
                                               + '","adCronEventAdmin.p","croneventrunresulttype_browse_form"' + ");", "").   
      CronEventRunResultTypeBrowse:insertData(CronEventRunResultType.CronEventRunResultTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CronEventRunResultType}
           
      CronEventRunResultTypeBrowse:insertData(CronEventRunResultType.TypeCode, "left").
      CronEventRunResultTypeBrowse:insertData(CronEventRunResultType.TypeName, "left").
      CronEventRunResultTypeBrowse:insertData(CronEventRunResultType.TypeDescr, "left").
      CronEventRunResultTypeBrowse:insertData(STRING(CronEventRunResultType.ListingSequence), "right").
      CronEventRunResultTypeBrowse:insertData(STRING(CronEventRunResultType.IsError, "Yes/No"), "left").
      CronEventrunResultTypeBrowse:insertData(STRING(CronEventRunResultType.Active, "Yes/No"), "left").
      
      /* Add hidden fields */
      CronEventRunResultTypeBrowse:insertHiddenData("CronEventRunResultTypeVersionID",CronEventRunResultType.VersionID).

      CronEventRunResultTypeBrowse:endRow().
   
   END. /* FOR EACH CronEventRunResultType OF CronEvent NO-LOCK */

   
   CronEventRunResultTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CronEventRunResultTypeBrowse:getErrors().
   
   CronEventRunResultTypeBrowseForm:insertHiddenField("CronEventRunResultTypeID","").
   CronEventRunResultTypeBrowseForm:insertHiddenField("CronEventRunResultTypeVersionID","").
   CronEventRunResultTypeBrowseForm:insertHiddenField("CronEventID",       (IF AVAIL CronEvent THEN STRING(CronEvent.CronEventID) ELSE "")).
   CronEventRunResultTypeBrowseForm:insertHiddenField("CronEventVersionID",(IF AVAIL CronEvent THEN STRING(CronEvent.VersionID) ELSE "")).
   CronEventRunResultTypeBrowseForm:insertHiddenField("cronevent_browse_scroll","").   
   CronEventRunResultTypeBrowseForm:insertHiddenField("popup_croneventrunresulttype_browse","").
   CronEventRunResultTypeBrowseForm:insertHiddenField("form_name","croneventrunresulttype_browse_form").
   CronEventRunResultTypeBrowseForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventRunResultTypeBrowseForm}
   
   /* Create Button Bar */
   CronEventRunResultTypeBrowseButtons = NEW buttonBar().
   
   CronEventRunResultTypeBrowseButtons:addButton("croneventrunresulttype_browse_form_btn_create",
                                                 fTL("Create"),
                                                 "createCronEventRunResultType('croneventrunresulttype_details_form');"). 

   CronEventRunResultTypeBrowseButtons:addButton("croneventrunresulttype_browse_form_btn_view",
                                                 fTL("Details"),
                                                 "viewCronEventRunResultTypeDetails('croneventrunresulttype_details_form');",
                                                 "Disabled").
     
   CronEventRunResultTypeBrowseButtons:addButton("croneventrunresulttype_browse_form_btn_delete",
                                                 fTL("Delete"),
                                                 "confirmDeleteCronEventRunResultType();", 
                                                 "Disabled").
   
   CronEventRunResultTypeBrowseButtons:addButton("croneventrunresulttype_browse_form_btn_cancel",
                                                 fTL("Cancel"),
                                                 "disablePopup('croneventrunresulttype_browse_form_popup');").
   
   CronEventRunResultTypeBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventRunResultTypeBrowseForm:FormBrowse  = CronEventRunResultTypeBrowse.
   CronEventRunResultTypeBrowseForm:FormButtons = CronEventRunResultTypeBrowseButtons.
   CronEventRunResultTypeBrowseForm:endForm(). 
   
   CronEventRunResultTypeBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCronEventRunResultTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCronEventRunResultTypeDetails Procedure 
PROCEDURE pCronEventRunResultTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "croneventrunresulttype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CronEventRunResultTypeID,ListingSequence,TypeCode,TypeName,TypeDescr,IsError,Active"
          chrEditFieldList     = "ListingSequence,TypeCode,TypeName,TypeDescr,IsError,Active"
          chrNewFieldList      = chrEditFieldList
          chrRequiredFieldList = "TypeCode,TypeName,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   CronEventRunResultTypeDetailsForm = NEW dataForm("croneventrunresulttype_details_form").
   CronEventRunResultTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CronEventRunResultTypeDetailsForm:FormAction  = "dbCronEventRunResultTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CronEventRunResultTypeDetailsForm:FormWidth   = 460.
   CronEventRunResultTypeDetailsForm:FormHeight  = 300.
   CronEventRunResultTypeDetailsForm:FormTitle   = "Cron Event Run Result Type".
   CronEventRunResultTypeDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CronEventRunResultTypeDetailsForm:insertPaddingColumn(70).
   CronEventRunResultTypeDetailsForm:insertColumn(120).
   CronEventRunResultTypeDetailsForm:insertColumn(120).
   CronEventRunResultTypeDetailsForm:insertColumn(30).
   CronEventRunResultTypeDetailsForm:insertColumn(120).  
   
   /* Fields */
   CronEventRunResultTypeDetailsForm:startRow().
   CronEventRunResultTypeDetailsForm:insertLabel(fTL("Type ID")).
   CronEventRunResultTypeDetailsForm:insertTextField("CronEventRunResultTypeID", "", 90, TRUE).    
   
   CronEventRunResultTypeDetailsForm:startRow().
   CronEventRunResultTypeDetailsForm:insertLabel(fTL("Listing Seq")).
   CronEventRunResultTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  

   CronEventRunResultTypeDetailsForm:startRow().
   CronEventRunResultTypeDetailsForm:insertLabel(fTL("Type Code")).
   CronEventRunResultTypeDetailsForm:insertTextField("TypeCode", "", 160, TRUE). 
   
   CronEventRunResultTypeDetailsForm:startRow().
   CronEventRunResultTypeDetailsForm:insertLabel(fTL("Type Name")).
   CronEventRunResultTypeDetailsForm:insertTextField("TypeName", "", 160, TRUE).  
   
   CronEventRunResultTypeDetailsForm:startRow().
   CronEventRunResultTypeDetailsForm:insertLabel(fTL("Type Descr")).
   CronEventRunResultTypeDetailsForm:insertTextField("TypeDescr", "", 160, TRUE). 

   CronEventRunResultTypeDetailsForm:startRow().
   CronEventRunResultTypeDetailsForm:insertLabel(fTL("Is Error")). 
   CronEventRunResultTypeDetailsForm:insertComboField("IsError", "", 160, TRUE).  
   CronEventRunResultTypeDetailsForm:insertComboPairs("IsError", "yes", "Error").
   CronEventRunResultTypeDetailsForm:insertComboPairs("IsError", "no",  "Not Error").

   CronEventRunResultTypeDetailsForm:startRow().
   CronEventRunResultTypeDetailsForm:insertLabel(fTL("Active")). 
   CronEventRunResultTypeDetailsForm:insertComboField("Active", "", 160, TRUE).  
   CronEventRunResultTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   CronEventRunResultTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
     
   /* Add Hidden Fields*/
   CronEventRunResultTypeDetailsForm:insertHiddenField("cronevent_browse_scroll","").
   CronEventRunResultTypeDetailsForm:insertHiddenField("popup_croneventrunresulttype_browse", "").
   CronEventRunResultTypeDetailsForm:insertHiddenField("CronEventID",IF intSelectedCronEvent = 0 THEN "" ELSE STRING(intSelectedCronEvent)).
   CronEventRunResultTypeDetailsForm:insertHiddenField("CronEventRunResultTypeID",STRING(intSelectedCronEventRunResultType)).
   CronEventRunResultTypeDetailsForm:insertHiddenField("form_name","croneventrunresulttype_details_form").
   CronEventRunResultTypeDetailsForm:insertHiddenField("prog_name","adCronEventAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CronEventRunResultTypeDetailsForm}
   
   /* Create Button Bar */
   CronEventRunResultTypeDetailsButtons = NEW buttonBar().
   
   CronEventRunResultTypeDetailsButtons:addButton("croneventrunresulttype_details_form_btn_save", 
                                                  fTL("Save"), 
                                                  "updateCronEventRunResultType('croneventrunresulttype_details_form');").
   
   CronEventRunResultTypeDetailsButtons:addButton("croneventrunresulttype_details_form_btn_cancel", 
                                                  fTL("Cancel"), 
                                                  "cancelUpdate('UserCancelled','process_mode'); " 
                                                     + "disablePopup('croneventrunresulttype_details_form_popup');").
   CronEventRunResultTypeDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CronEventRunResultTypeDetailsForm:FormButtons = CronEventRunResultTypeDetailsButtons.
   
   CronEventRunResultTypeDetailsForm:endForm(). 
   CronEventRunResultTypeDetailsForm:displayForm(). 

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
   
   ASSIGN chrCronEventID                      = get-value("CronEventID")
          chrCronEventParameterID             = get-value("CronEventParameterID")
          chrCronEventEnvironmentLinkID       = get-value("CronEventEnvironmentID")    
          chrCronEventRunResultTypeID         = get-value("CronEventRunResultTypeID")
          chrCronEventRunID                   = get-value("CronEventRunID")
          chrCronEventHistoryID               = get-value("CronEventHistoryID")
          chrCronConfigID                     = get-value("CronConfigID")          
          intSelectedCronEvent                = INTEGER(chrCronEventID)
          intSelectedCronEventParameter       = INTEGER(chrCronEventParameterID)
          intSelectedCronEventEnvironmentLink = INTEGER(chrCronEventEnvironmentLinkID)   
          intSelectedCronEventRunResultType   = INTEGER(chrCronEventRunResultTypeID)
          intSelectedCronEventRun             = INTEGER(chrCronEventRunID)
          intSelectedCronEventHistory         = INTEGER(chrCronEventHistoryID)
          intSelectedCronConfig               = INTEGER(chrCronConfigID)          
          chrScrollToCronEventRow             = STRING(INTEGER(get-value("cronevent_browse_scroll"))) + ";".

   /* Process URL values */
   IF chrCronEventID <> "" THEN
      chrSelectCronEventRow = 'selectCronEventRow(document.getElementById("cronevent_browse_row_' + chrCronEventID + '"),"' 
                                 + chrCronEventID + '");'.
   
   IF chrCronEventParameterID <> "" THEN
      chrSelectCronEventParameterRow = 'selectCronEventParameterRow(document.getElementById("croneventparameter_browse_row_' 
                                          + chrCronEventParameterID + '"),"' + chrCronEventParameterID +  '");'.

   IF chrCronEventEnvironmentLinkID <> "" THEN
      chrSelectCronEventEnvironmentLinkRow = 'selectCronEventEnvironmentLinkRow(document.getElementById("croneventenvironmentlink_browse_row_'
                                                + chrCronEventEnvironmentLinkID + '"),"' + chrCronEventEnvironmentLinkID +  '");'.

   IF chrCronEventRunResultTypeID <> "" THEN
      chrSelectCronEventRunResultTypeRow = 'selectCronEventRunResultTypeRow(document.getElementById("croneventrunresulttype_browse_row_' 
                                              + chrCronEventRunResultTypeID + '"),"' + chrCronEventRunResultTypeID +  '");'.

   IF chrCronEventRunID <> "" THEN
      chrSelectCronEventRunRow = 'selectCronEventRunRow(document.getElementById("croneventrun_browse_row_' 
                                    + chrCronEventRunID + '"),"' + chrCronEventRunID +  '");'.

   IF chrCronEventHistoryID <> "" THEN
      chrSelectCronEventHistoryRow = 'selectCronEventHistoryRow(document.getElementById("croneventhistory_browse_row_' 
                                        + chrCronEventHistoryID + '"),"' + chrCronEventHistoryID +  '");'.

   IF chrCronConfigID <> "" THEN
      chrSelectCronConfigRow = 'selectCronConfigRow(document.getElementById("cronconfig_browse_row_' + chrCronConfigID + '"),"' 
                                + chrCronConfigID +  '");'.

   IF get-value('popup_croneventhistory_browse') = "yes" THEN
      chrPopupCronEventHistory = 'enablePopup("croneventhistory_browse_form_popup");'.

   IF get-value('popup_cronconfig_browse') = "yes" THEN
      chrPopupCronConfig = 'enablePopup("cronconfig_browse_form_popup");'.

   IF get-value('popup_croneventparameter_browse') = "yes" THEN
      chrPopupCronEventParameter = 'enablePopup("croneventparameter_browse_form_popup");'.

   IF get-value('popup_croneventenvironmentlink_browse') = "yes"  THEN
      chrPopupCronEventEnvironmentLink = 'enablePopup("croneventenvironmentlink_browse_form_popup");'.

   IF get-value('popup_croneventrunresulttype_browse') = "yes" THEN
      chrPopupCronEventRunResultType = 'enablePopup("croneventrunresulttype_browse_form_popup");'.

   IF get-value('popup_croneventrun_browse') = "yes" THEN
      chrPopupCronEventRun = 'enablePopup("croneventrun_browse_form_popup");'.

   IF get-value('popup_cronconfighistory_browse') = "yes" THEN
   chrPopupCronConfigHistory = 'enablePopup("cronconfighistory_browse_form_popup");'.

   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad 
                   + 'document.getElementById("cronevent_browse").scrollTop=' 
                   + chrScrollToCronEventRow + chrSelectCronEventRow                  
                   + chrSelectCronEventParameterRow + chrPopupCronEventParameter 
                   + chrSelectCronEventEnvironmentLinkRow + chrPopupCronEventEnvironmentLink
                   + chrSelectCronEventRunResultTypeRow + chrPopupCronEventRunResultType
                   + chrSelectCronEventRunRow + chrPopupCronEventRun
                   + chrSelectCronEventHistoryRow + chrPopupCronEventHistory
                   + chrSelectCronConfigRow + chrPopupCronConfig
                   + chrSelectCronConfigHistoryRow + chrPopupCronConfigHistory.
   
   FIND FIRST CronConfig WHERE CronConfig.Active NO-LOCK NO-ERROR.

   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Cron Event Admin".
   ThisPage:FrameTitle    = IF AVAILABLE CronConfig THEN "Cron Event Admin" ELSE "**Cron is currently InActive**".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("cronevent.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pCronEventBrowse.

   /******* Pop-up Browsers and Forms ********/
   IF intSelectedCronEvent <> 0 THEN
   DO:

      FIND FIRST CronEvent NO-LOCK 
         WHERE CronEvent.CronEventID = intSelectedCronEvent NO-ERROR.

      FIND FIRST Environment NO-LOCK
         WHERE Environment.EnvironmentCode = chrGblEnvironment NO-ERROR.

      IF AVAILABLE Environment AND AVAILABLE CronEvent THEN
         FIND FIRST CronEventEnvironmentLink NO-LOCK
            WHERE CronEventEnvironmentLink.CronEventID = CronEvent.CronEventID NO-ERROR.
   END.

   RUN pCronEventDetails.
   RUN pCronEventParameterBrowse.
   RUN pCronEventParameterDetails.
   RUN pCronEventEnvironmentLinkBrowse.
   RUN pCronEventEnvironmentLinkDetails.
   RUN pCronEventRunResultTypeBrowse.
   RUN pCronEventRunResultTypeDetails.
   RUN pCronEventRunBrowse.
   RUN pCronEventRunDetails.
   RUN pCronEventHistoryBrowse.
   RUN pCronConfigBrowse.
   RUN pCronConfigDetails.
   
   IF intSelectedCronConfig <> 0 THEN
      FIND FIRST CronConfig NO-LOCK
         WHERE CronConfig.CronConfigID = intSelectedCronConfig NO-ERROR.
      
   RUN pCronConfigHistoryBrowse.

   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   /* CronEvent Objects */
   DELETE OBJECT CronEventBrowse                        NO-ERROR.
   DELETE OBJECT CronEventBrowseButtons                 NO-ERROR.   
   DELETE OBJECT CronEventBrowseFrame                   NO-ERROR.
   DELETE OBJECT CronEventDetailsButtons                NO-ERROR.
   DELETE OBJECT CronEventDetailsForm                   NO-ERROR.
   
   /* CronEventParameter Objects */
   DELETE OBJECT CronEventParameterBrowse               NO-ERROR.
   DELETE OBJECT CronEventParameterBrowseButtons        NO-ERROR.
   DELETE OBJECT CronEventParameterBrowseForm           NO-ERROR.   
   DELETE OBJECT CronEventParameterDetailsButtons       NO-ERROR.
   DELETE OBJECT CronEventParameterDetailsForm          NO-ERROR.
      
   /* CronEventRunResultType Objects */
   DELETE OBJECT CronEventRunResultTypeBrowse           NO-ERROR.
   DELETE OBJECT CronEventRunResultTypeBrowseButtons    NO-ERROR.
   DELETE OBJECT CronEventRunResultTypeBrowseForm       NO-ERROR.
   DELETE OBJECT CronEventRunResultTypeDetailsButtons   NO-ERROR.
   DELETE OBJECT CronEventRunResultTypeDetailsForm      NO-ERROR.

   /* CronEventRun Objects */
   DELETE OBJECT CronEventRunBrowse                     NO-ERROR.
   DELETE OBJECT CronEventRunBrowseButtons              NO-ERROR.
   DELETE OBJECT CronEventRunBrowseForm                 NO-ERROR.
   DELETE OBJECT CronEventRunDetailsButtons             NO-ERROR.
   DELETE OBJECT CronEventRunDetailsForm                NO-ERROR.

   /* CronEventHistory Objects */
   DELETE OBJECT CronEventHistoryBrowse                 NO-ERROR.
   DELETE OBJECT CronEventHistoryBrowseButtons          NO-ERROR.
   DELETE OBJECT CronEventHistoryBrowseForm             NO-ERROR.

   /* CronConfig Objects */
   DELETE OBJECT CronConfigBrowse                       NO-ERROR.
   DELETE OBJECT CronConfigBrowseButtons                NO-ERROR.
   DELETE OBJECT CronConfigBrowseForm                   NO-ERROR.
   DELETE OBJECT CronConfigDetailsButtons               NO-ERROR.
   DELETE OBJECT CronConfigDetailsForm                  NO-ERROR.

   /* CronConfigHistory Objects */
   DELETE OBJECT CronConfigHistoryBrowse                NO-ERROR.
   DELETE OBJECT CronConfigHistoryBrowseButtons         NO-ERROR.
   DELETE OBJECT CronConfigHistoryBrowseForm            NO-ERROR.

   /* CronEventEnvironmentLink Objects */
   DELETE OBJECT CronEventEnvironmentLinkBrowse         NO-ERROR.
   DELETE OBJECT CronEventEnvironmentLinkBrowseButtons  NO-ERROR.
   DELETE OBJECT CronEventEnvironmentLinkBrowseForm     NO-ERROR.
   DELETE OBJECT CronEventEnvironmentLinkDetailsButtons NO-ERROR.
   DELETE OBJECT CronEventEnvironmentLinkDetailsForm    NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

