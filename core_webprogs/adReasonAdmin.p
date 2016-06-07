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
     clean up will occur on deletion of the procedure. */
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


/* Definitions for System Options for Admin */
{getAdminOptions.i}

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttReason 
   FIELD ReasonID        AS INTEGER
   FIELD ListingSequence AS INTEGER
   FIELD RuleID          AS INTEGER
   FIELD RuleDesc        AS CHARACTER
   FIELD FromStatusID    AS INTEGER
   FIELD ToStatusID      AS INTEGER
   FIELD ReasonName      AS CHARACTER
   FIELD ReasonDetails   AS CHARACTER
   FIELD ACTIVE          AS LOGICAL
   FIELD VersionID       AS INTEGER
   FIELD ReasonTableName AS CHARACTER
   INDEX ReasonIdIndex ReasonTableName RuleDesc ReasonID
   INDEX ListingSequenceIndex ReasonTableName RuleDesc ListingSequence ACTIVE.

DEFINE VARIABLE chrSelectedReasonTable  AS CHARACTER NO-UNDO INITIAL "0".
DEFINE VARIABLE chrSelectedRuleTable    AS CHARACTER NO-UNDO.

DEFINE VARIABLE intSelectedReason       AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectReasonRow      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToReasonRow    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReasonID             AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdlTempReasonQuery      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdlTempReasonBuffer     AS HANDLE    NO-UNDO.
DEFINE VARIABLE logReasonQueryPrepareOK AS LOGICAL   NO-UNDO.

DEFINE VARIABLE hdlTempRuleBuffer       AS HANDLE    NO-UNDO.
DEFINE VARIABLE logRuleQueryPrepareOK   AS LOGICAL   NO-UNDO.

/* Objects */
DEFINE VARIABLE ReasonSelectionFrame AS pageFrame.
DEFINE VARIABLE ReasonSelectionForm  AS dataForm.
DEFINE VARIABLE ReasonBrowseFrame    AS pageFrame.
DEFINE VARIABLE ReasonBrowse         AS browseTable.
DEFINE VARIABLE ReasonBrowseButtons  AS buttonBar.
DEFINE VARIABLE ReasonDetailsForm    AS dataForm.
DEFINE VARIABLE ReasonDetailsButtons AS buttonBar.

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
         HEIGHT             = 13.71
         WIDTH              = 61.6.
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

&IF DEFINED(EXCLUDE-pReasonBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReasonBrowse Procedure 
PROCEDURE pReasonBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   EMPTY TEMP-TABLE ttReason.

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "reason_details_form"} 
   
   /* Browser on the Reason table */
   ReasonBrowse = NEW browseTable("reason_browse").
   ReasonBrowse:BrowseWidth  = 965.
   ReasonBrowse:BrowseHeight = 395.
   ReasonBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Build the corresponding Rule table name for the Reason table */
   chrSelectedRuleTable = REPLACE(chrSelectedReasonTable, "Reason", "Rule").
   
   /* Find the corresponding Rule table for the Reason table */
   FIND core._file NO-LOCK WHERE core._file._file-Name = chrSelectedRuleTable NO-ERROR.
   IF AVAILABLE core._file THEN
   DO:
      /* If there is a corresponding Rule table, then build RuleID and RuleDesc into the browse */
      ReasonBrowse:insertColumn(fTL("RuleID"),       50, "INTEGER", FALSE).
      ReasonBrowse:insertColumn(fTL("RuleDesc"),    180, "CHARACTER", "left", FALSE).
      ReasonBrowse:insertColumn(fTL("ReasonID"),     50, "INTEGER", FALSE).
      ReasonBrowse:insertColumn(fTL("Listing"),      70, "INTEGER", FALSE).
      ReasonBrowse:insertColumn(fTL("ReasonName"),  140, "CHARACTER", "left", FALSE).
      ReasonBrowse:insertColumn(fTL("Description"), 200, "CHARACTER", "left", FALSE).
      ReasonBrowse:insertColumn(fTL("ACTIVE"),       80, "LOGICAL", FALSE).
   END.
   ELSE
   DO:
      ReasonBrowse:insertColumn(fTL("ReasonID"),     80, "INTEGER", FALSE).
      ReasonBrowse:insertColumn(fTL("Listing"),     120, "INTEGER", FALSE).
      ReasonBrowse:insertColumn(fTL("ReasonName"),  180, "CHARACTER", "left", FALSE).
      ReasonBrowse:insertColumn(fTL("Description"), 200, "CHARACTER", "left", FALSE).
      ReasonBrowse:insertColumn(fTL("ACTIVE"),       80, "LOGICAL", FALSE).

      /* If there is no corresponding Rule table, then blank the chrSelectedRuleTable */
      chrSelectedRuleTable = "".
   END.
   
   /* Check if the selected Reason Table exist; if so, built reason temp table */
   FIND core._file NO-LOCK WHERE core._file._file-Name = chrSelectedReasonTable NO-ERROR.
   IF AVAILABLE core._file THEN
   DO:
      CREATE BUFFER hdlTempReasonBuffer FOR TABLE chrSelectedReasonTable NO-ERROR.
      CREATE QUERY hdlTempReasonQuery NO-ERROR.
      
      hdlTempReasonQuery:ADD-BUFFER(hdlTempReasonBuffer).
      logReasonQueryPrepareOK = hdlTempReasonQuery:QUERY-PREPARE("FOR EACH " + chrSelectedReasonTable) NO-ERROR.
      
      IF logReasonQueryPrepareOK THEN
      DO:
         
         hdlTempReasonQuery:QUERY-OPEN().
         hdlTempReasonQuery:GET-FIRST(NO-LOCK).
         
         REPEAT WHILE NOT hdlTempReasonQuery:QUERY-OFF-END:
            
            CREATE ttReason.
            ASSIGN ttReason.ReasonID        = INTEGER(STRING(hdlTempReasonBuffer:BUFFER-FIELD(chrSelectedReasonTable + "ID"):BUFFER-VALUE))
                   ttReason.ListingSequence = INTEGER(hdlTempReasonBuffer:BUFFER-FIELD("ListingSequence"):BUFFER-VALUE)
                   ttReason.ReasonName      = hdlTempReasonBuffer:BUFFER-FIELD("ReasonName"):BUFFER-VALUE
                   ttReason.ReasonDetails   = hdlTempReasonBuffer:BUFFER-FIELD("ReasonDetails"):BUFFER-VALUE
                   ttReason.ACTIVE          = LOGICAL(hdlTempReasonBuffer:BUFFER-FIELD("Active"):BUFFER-VALUE)
                   ttReason.VersionID       = INTEGER(hdlTempReasonBuffer:BUFFER-FIELD("VersionID"):BUFFER-VALUE)
                   ttReason.ReasonTableName = chrSelectedReasonTable.
            
            /* If there is a corresponding Rule table, then try find the RuleID of the Reason record */
            IF chrSelectedRuleTable <> "" THEN
            Find_Rule_Block:
            DO:
               ttReason.RuleID = INTEGER(hdlTempReasonBuffer:BUFFER-FIELD(chrSelectedRuleTable + "ID"):BUFFER-VALUE) NO-ERROR.
               
               IF ttReason.RuleID = ? THEN 
                  ttReason.RuleID = 0.
               
               /* If the RuleID is zero then there is no point in trying to build the rule description, so leave current block */
               IF ttReason.RuleID = 0 THEN
                  LEAVE Find_Rule_Block.
               
               CREATE BUFFER hdlTempRuleBuffer FOR TABLE chrSelectedRuleTable NO-ERROR.
               
               logRuleQueryPrepareOK = hdlTempRuleBuffer:FIND-FIRST("WHERE " + chrSelectedRuleTable + "." + chrSelectedRuleTable + "ID = " 
                                         + STRING(ttReason.RuleID),NO-LOCK) NO-ERROR.
               
               IF logRuleQueryPrepareOK THEN
               DO:
                  ASSIGN ttReason.FromStatusID = INTEGER(hdlTempRuleBuffer:BUFFER-FIELD("FromStockStatusID"):BUFFER-VALUE)
                         ttReason.ToStatusID   = INTEGER(hdlTempRuleBuffer:BUFFER-FIELD("ToStockStatusID"):BUFFER-VALUE).
                  
                  /* Buld rule description using the StatusCodes */
                  FIND FIRST StockStatus NO-LOCK
                     WHERE StockStatus.StockStatusID = ttReason.FromStatusID NO-ERROR.
                  IF AVAILABLE StockStatus THEN
                  DO:
                     ttReason.RuleDesc = "From " + StockStatus.StatusCode.
                  END.
                  
                  FIND FIRST StockStatus NO-LOCK
                     WHERE StockStatus.StockStatusID = ttReason.ToStatusID NO-ERROR.
                  IF AVAILABLE StockStatus THEN
                  DO:
                     ttReason.RuleDesc = ttReason.RuleDesc + " to " + StockStatus.StatusCode.
                  END.
               END. /* IF logRuleQueryPrepareOK THEN */
               ELSE 
               DO:
                  ASSIGN
                     ttReason.FromStatusID = 0
                     ttReason.ToStatusID   = 0
                     ttReason.RuleDesc     = "".
               END. /* logRuleQueryPrepareOK = NO */
            END. /* IF chrSelectedRuleTable <> "" THEN */
            ELSE
            DO:
               ASSIGN
                  ttReason.FromStatusID = 0
                  ttReason.ToStatusID   = 0
                  ttReason.RuleDesc     = "".
            END. /* chrSelectedRuleTable = "" */
            
            hdlTempReasonQuery:GET-NEXT(NO-LOCK).
         END.  /* REPEAT */
         
         hdlTempReasonQuery:QUERY-CLOSE().
         
      END.  /* IF logReasonQueryPrepareOK THEN */
      ELSE 
      DO:
         chrPageBuildError = chrPageBuildError + "Bad query: FOR EACH " + chrSelectedReasonTable + ".".
      END. /* logReasonQueryPrepareOK = NO */
   END.  /* IF AVAILABLE core._file THEN */
   

   /* Clean-up */
   DELETE OBJECT hdlTempReasonBuffer NO-ERROR.
   DELETE OBJECT hdlTempReasonQuery  NO-ERROR.
   DELETE OBJECT hdlTempRuleBuffer   NO-ERROR.

   /* build body of browse */
   ReasonBrowse:startBody().
   
   FOR EACH ttReason:
      
      ReasonBrowse:startRow(ttReason.ReasonID, 
                            "selectReasonRow(this," + '"' + STRING(ttReason.ReasonID) + '","' + chrSelectedReasonTable + '"' + ");", 
                            "").
      /* Only insert the RuleId and RuleDescription if there is a corresponding Rule table for the Reason table */
      IF chrSelectedRuleTable <> "" THEN
      DO:
         ReasonBrowse:insertData(ttReason.RuleID).
         ReasonBrowse:insertData(ttReason.RuleDesc, "left").
      END.

      ReasonBrowse:insertData(ttReason.ReasonID).
      ReasonBrowse:insertData(STRING(ttReason.ListingSequence)).
      ReasonBrowse:insertData(ttReason.ReasonName, "left").
      ReasonBrowse:insertData(ttReason.ReasonDetails, "left").
      ReasonBrowse:insertData(STRING(ttReason.ACTIVE,"Yes/No")).
      
      /* Add hidden fields */
      ReasonBrowse:insertHiddenData(chrSelectedReasonTable + "ID",ttReason.ReasonID).
      ReasonBrowse:insertHiddenData(chrSelectedReasonTable + "VersionID",ttReason.VersionID).
      ReasonBrowse:insertHiddenData("ReasonTableName",chrSelectedReasonTable).
      
      ReasonBrowse:endRow().
   END.
   
   ReasonBrowse:endTable().
   
   /* --------------Create Reason Browse Button Bar --------------*/
   ReasonBrowseButtons           = NEW buttonBar().
   ReasonBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   ReasonBrowseButtons:addButton("reason_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewReasonDetails('reason_details_form','" + chrSelectedReasonTable + "');",
                                 (IF intSelectedReason > 0 THEN "" ELSE "Disabled")).
   
   ReasonBrowseButtons:addButton("reason_browse_form_btn_create",
                                 fTL("Create"),
                                 "createReason('reason_details_form','" + chrSelectedReasonTable + "');",
                                 (IF chrSelectedReasonTable > "0" THEN "" ELSE "Disabled")).
   
   ReasonBrowseButtons:addButton("reason_browse_form_btn_delete",
                                 fTL("Delete"),
                                 "confirmDeleteReason('" + chrSelectedReasonTable + "');",
                                 "Disabled").
   
   ReasonBrowseButtons:closeBar().
   
   
   /* Building page for adding pieces to main frame */
   ReasonSelectionFrame = NEW pageFrame().
   ReasonSelectionFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* creating a new frame */
   ReasonSelectionFrame:frameOpen(985, 70, "").
   
   ReasonSelectionForm:displayForm().
   
   ReasonSelectionFrame:frameClose().
   
   /* Create a new frame */
   ReasonBrowseFrame = NEW pageFrame().
   ReasonBrowseFrame:Webstream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   ReasonBrowseFrame:FormAction="dbReasonUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   ReasonBrowseFrame:formOpen("reason_browse_form").
   
   ReasonBrowseFrame:frameOpen(985, 435, "").
   
   ReasonBrowse:displayBrowse().
   
   ReasonBrowseFrame:frameClose().
   ReasonBrowseFrame:insertSpacer(10).
   
   /* insert hidden fields */
   ReasonBrowseFrame:insertHiddenField("ReasonTableName", chrSelectedReasonTable).
   ReasonBrowseFrame:insertHiddenField(chrSelectedReasonTable + "ID", "").
   ReasonBrowseFrame:insertHiddenField(chrSelectedReasonTable + "VersionID","").
   ReasonBrowseFrame:insertHiddenField("reason_browse_scroll","").
   ReasonBrowseFrame:insertHiddenField("form_name", "reason_browse_form").
   ReasonBrowseFrame:insertHiddenField("prog_name", "adReasonAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ReasonBrowseFrame}
   
   ReasonBrowseFrame:formClose().
   
   ReasonBrowseButtons:displayButtonBar().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReasonDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReasonDetails Procedure 
PROCEDURE pReasonDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "reason_details_form"}
   
   ASSIGN chrDisplayFieldList  = chrSelectedReasonTable + "ID,ReasonName,ReasonDetails,ListingSequence,Active"
          chrEditFieldList     = "ReasonName,ReasonDetails,ListingSequence,Active"
          chrNewFieldList      = "ReasonName,ReasonDetails,ListingSequence,Active"
          chrRequiredFieldList = "ReasonName,ReasonDetails"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER>0".
   
   ReasonDetailsForm            = NEW dataForm("reason_details_form").
   ReasonDetailsForm:WebStream  = STREAM WebStream:HANDLE.
   ReasonDetailsForm:FormAction = "dbReasonUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ReasonDetailsForm:FormWidth  = 460.
   ReasonDetailsForm:FormHeight = 300.
   ReasonDetailsForm:FormTitle  = chrSelectedReasonTable + " Details".
   ReasonDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   ReasonDetailsForm:insertPaddingColumn(60).
   
   /* Fields */
   ReasonDetailsForm:startRow().
   ReasonDetailsForm:insertLabel("Reason ID").
   ReasonDetailsForm:insertTextField(chrSelectedReasonTable + "ID", "", 60, TRUE).  
   
   ReasonDetailsForm:startRow().
   ReasonDetailsForm:insertLabel("Reason Name").
   ReasonDetailsForm:insertTextField("ReasonName", "", 200, TRUE).  
   
   ReasonDetailsForm:startRow().
   ReasonDetailsForm:insertLabel("Reason Details").
   ReasonDetailsForm:insertTextAreaField("ReasonDetails", "", 200, TRUE).  
   
   ReasonDetailsForm:startRow().
   ReasonDetailsForm:insertLabel("Listing Sequence").
   ReasonDetailsForm:insertTextField("ListingSequence", "", 60, TRUE).  
   
   ReasonDetailsForm:startRow().
   ReasonDetailsForm:insertLabel(fTL("Active")). 
   ReasonDetailsForm:insertComboField("Active", "", 100, TRUE).  
   ReasonDetailsForm:insertComboPairs("Active", "yes", "Active").
   ReasonDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pReasonDetailsFields}
   
   /* Add Hidden Fields*/
   ReasonDetailsForm:insertHiddenField("reason_browse_scroll", "").
   ReasonDetailsForm:insertHiddenField("form_name", "reason_details_form").
   ReasonDetailsForm:insertHiddenField("prog_name", "adReasonAdmin.p").
   ReasonDetailsForm:insertHiddenField("ReasonTableName", chrSelectedReasonTable).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ReasonDetailsForm}
   
   /* Create Button Bar */
   ReasonDetailsButtons = NEW buttonBar().
   ReasonDetailsButtons:addButton("reason_details_form_btn_save", 
                                  fTL("Save"), 
                                  "updateReason('reason_details_form','" + chrSelectedReasonTable + "');").
   
   ReasonDetailsButtons:addButton("reason_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('reason_details_form_popup');").
   ReasonDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   ReasonDetailsForm:FormButtons = ReasonDetailsButtons.
   
   ReasonDetailsForm:endForm(). 
   
   ReasonDetailsForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReasonSelectionDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReasonSelectionDetails Procedure 
PROCEDURE pReasonSelectionDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   ReasonSelectionForm             = NEW dataForm("reason_selection_form").
   ReasonSelectionForm:WebStream   = STREAM WebStream:HANDLE.
   ReasonSelectionForm:ShowAsPopup = FALSE.
   ReasonSelectionForm:FloatForm   = "left".
   ReasonSelectionForm:FormAction  = "adReasonAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ReasonSelectionForm:FormWidth  = 964.
   ReasonSelectionForm:FormHeight =  50.
   ReasonSelectionForm:FormTitle  = "Select Reason Table".
   ReasonSelectionForm:FormType   = "medium".
   
   /* Column Layout */
   ReasonSelectionForm:insertPaddingColumn(10).
   ReasonSelectionForm:insertColumn(100).
   ReasonSelectionForm:insertColumn(200).
   
   /* Fields design */
   ReasonSelectionForm:startRow().
   ReasonSelectionForm:insertLabel(fTL("Reason Table")).
   ReasonSelectionForm:insertComboField("ReasonTableName", chrSelectedReasonTable, 200, TRUE, 'changeReason(this.value)').
   
   ReasonSelectionForm:insertComboPairs("ReasonTableName", "0", "Choose a table...").
   
   /* Build a list with all the Reason tables in the core DB */
   FOR EACH core._file NO-LOCK 
      WHERE core._file._file-Name MATCHES "*" + "Reason" + "*":
      
      ReasonSelectionForm:insertComboPairs("ReasonTableName", core._file._file-Name, core._file._file-Name).
      
   END.
   
   ReasonSelectionForm:endForm().
   
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
   
   /* Get URL Values */
   chrSelectedReasonTable = get-value("ReasonTableName").
   chrReasonID            = get-value(chrSelectedReasonTable + "ID").
   intSelectedReason      = INTEGER(chrReasonID).
   chrScrollToReasonRow   = STRING(INTEGER(get-value("reason_browse_scroll"))) + ";".
   
   IF chrSelectedReasonTable = "" THEN chrSelectedReasonTable = "0".
   
   /* Process URL values */
   IF chrReasonID <> "" THEN
      chrSelectReasonRow = 'selectReasonRow(document.getElementById("reason_browse_row_' + chrReasonID + '"),"' 
                                            + chrReasonID + '","' + chrSelectedReasonTable + '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("reason_browse").scrollTop=' + chrScrollToReasonRow 
                             + chrSelectReasonRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Reason Admin".
   ThisPage:FrameTitle    = "Reason Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("reason.js"). /* Append your file here */
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pReasonSelectionDetails.
   RUN pReasonBrowse.

   /******* Pop-up Browsers and Forms ********/    
   RUN pReasonDetails.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT ReasonSelectionFrame NO-ERROR.
   DELETE OBJECT ReasonSelectionForm  NO-ERROR.
   DELETE OBJECT ReasonBrowseFrame    NO-ERROR.
   DELETE OBJECT ReasonBrowse         NO-ERROR.
   DELETE OBJECT ReasonBrowseButtons  NO-ERROR.
   DELETE OBJECT ReasonDetailsForm    NO-ERROR.
   DELETE OBJECT ReasonDetailsButtons NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

