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

  Author: Anthony Ferrari

  Created: 10/03/2016

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

DEFINE VARIABLE intSelectedReplenLocationTypeRule           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectReplenLocationTypeRuleRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToReplenLocationTypeRuleRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrReplenLocationTypeRuleID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupReplenLocationTypeRuleHistory       AS CHARACTER   NO-UNDO.

/* Buffers */
DEFINE BUFFER targetLocationType FOR LocationType.

/* Objects */
DEFINE VARIABLE ReplenLocationTypeRuleBrowseFrame           AS pageFrame.
DEFINE VARIABLE ReplenLocationTypeRuleBrowse                AS browseTable.
DEFINE VARIABLE ReplenLocationTypeRuleBrowseButtons         AS buttonBar.
DEFINE VARIABLE ReplenLocationTypeRuleDetailsForm           AS dataForm.
DEFINE VARIABLE ReplenLocationTypeRuleDetailsButtons        AS buttonBar.

DEFINE VARIABLE ReplenLocationTypeRuleHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE ReplenLocationTypeRuleHistoryBrowse         AS browseTable.
DEFINE VARIABLE ReplenLocationTypeRuleHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE ReplenLocationTypeRuleHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE ReplenLocationTypeRuleHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrReplenLocationTypeRuleID          = get-value("ReplenLocationTypeRuleID")
          intSelectedReplenLocationTypeRule    = INTEGER(chrReplenLocationTypeRuleID)
          chrScrollToReplenLocationTypeRuleRow = STRING(INTEGER(get-value("replenlocationtyperule_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrReplenLocationTypeRuleID <> "" THEN
     chrSelectReplenLocationTypeRuleRow = 'selectReplenLocationTypeRuleRow(document.getElementById("replenlocationtyperule_browse_row_' + chrReplenLocationTypeRuleID + '"),"' 
                                                         + chrReplenLocationTypeRuleID +  '");'.
   
/*   IF get-value('popup_replenlocationtyperulehistory_browse') = "yes" THEN                                       */
/*      chrPopupReplenLocationTypeRuleHistory  = 'enablePopup("replenlocationtyperulehistory_browse_form_popup");'.*/
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("replenlocationtyperule_browse").scrollTop=' + chrScrollToReplenLocationTypeRuleRow 
                                                          + chrSelectReplenLocationTypeRuleRow. 
/*                                                          + chrPopupReplenLocationTypeRuleHistory.*/
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "ReplenLocationTypeRule Admin".
   ThisPage:FrameTitle    = "ReplenLocationTypeRule Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("replenlocationtyperule.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pReplenLocationTypeRuleBrowse.
   
   FIND FIRST ReplenLocationTypeRule NO-LOCK
      WHERE ReplenLocationTypeRule.ReplenLocationTypeRuleID = intSelectedReplenLocationTypeRule NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pReplenLocationTypeRuleDetails.
/*   RUN pReplenLocationTypeRuleHistory.       */
/*   RUN pReplenLocationTypeRuleHistoryDetails.*/
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT ReplenLocationTypeRuleBrowseFrame              NO-ERROR.
   DELETE OBJECT ReplenLocationTypeRuleBrowse                   NO-ERROR.
   DELETE OBJECT ReplenLocationTypeRuleBrowseButtons            NO-ERROR.
   DELETE OBJECT ReplenLocationTypeRuleDetailsForm              NO-ERROR.
   DELETE OBJECT ReplenLocationTypeRuleDetailsButtons           NO-ERROR.
   
   DELETE OBJECT ReplenLocationTypeRuleHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT ReplenLocationTypeRuleHistoryBrowse            NO-ERROR.
   DELETE OBJECT ReplenLocationTypeRuleHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT ReplenLocationTypeRuleHistoryDetailsForm       NO-ERROR.
   DELETE OBJECT ReplenLocationTypeRuleHistoryDetailsButtons    NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReplenLocationTypeRuleBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenLocationTypeRuleBrowse Procedure 
PROCEDURE pReplenLocationTypeRuleBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "replenlocationtyperule_details_form"}
   
   ReplenLocationTypeRuleBrowse = NEW browseTable("replenlocationtyperule_browse").
   ReplenLocationTypeRuleBrowse:BrowseWidth  = 965.
   ReplenLocationTypeRuleBrowse:BrowseHeight = 455.
   ReplenLocationTypeRuleBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   ReplenLocationTypeRuleBrowse:insertColumn(fTL("RLTR ID"), 80, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ReplenLocationTypeRule}
   
   ReplenLocationTypeRuleBrowse:insertColumn(fTL("Listing Seq"),            80, "INTEGER",           FALSE).
   ReplenLocationTypeRuleBrowse:insertColumn(fTL("Target Location Type"),  150, "CHARACTER", "left", FALSE).
   ReplenLocationTypeRuleBrowse:insertColumn(fTL("Source Location Type"),  150, "CHARACTER", "left", FALSE).
   ReplenLocationTypeRuleBrowse:insertColumn(fTL("Priority"),               80, "INTEGER",           FALSE).
   ReplenLocationTypeRuleBrowse:insertColumn(fTL("Active"),                 70, "LOGICAL",           FALSE).
   
   /*Body*/
   ReplenLocationTypeRuleBrowse:startBody().
   
   FOR EACH ReplenLocationTypeRule NO-LOCK /*idx=ActiveListingSequence*/
      BY    ReplenLocationTypeRule.ListingSequence
      BY    ReplenLocationTypeRule.Active DESC:
         
      FIND FIRST LocationType NO-LOCK /* idx=LocationTypeID */
         WHERE LocationType.LocationTypeID = ReplenLocationTypeRule.SourceLocationTypeID NO-ERROR.
         
      FIND FIRST targetLocationType NO-LOCK /* idx=LocationTypeID */
         WHERE targetLocationType.LocationTypeID = ReplenLocationTypeRule.TargetLocationTypeID NO-ERROR.       
      
      ReplenLocationTypeRuleBrowse:startRow(ReplenLocationTypeRule.ReplenLocationTypeRuleID, "selectReplenLocationTypeRuleRow(this," + '"' + STRING(ReplenLocationTypeRule.ReplenLocationTypeRuleID) 
                                                                  + '"' + ");", "").
      ReplenLocationTypeRuleBrowse:insertData(ReplenLocationTypeRule.ReplenLocationTypeRuleID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i ReplenLocationTypeRule}
      
      ReplenLocationTypeRuleBrowse:insertData(STRING(ReplenLocationTypeRule.ListingSequence)).
      ReplenLocationTypeRuleBrowse:insertData((IF AVAILABLE LocationType THEN LocationType.TypeName ELSE "") ,"left").
      ReplenLocationTypeRuleBrowse:insertData((IF AVAILABLE targetLocationType THEN targetLocationType.TypeName ELSE "") ,"left").
      ReplenLocationTypeRuleBrowse:insertData(STRING(ReplenLocationTypeRule.Priority)).
      ReplenLocationTypeRuleBrowse:insertData(STRING(ReplenLocationTypeRule.Active,"Yes/No")).
      
      /* Add hidden fields */
      ReplenLocationTypeRuleBrowse:insertHiddenData("ReplenLocationTypeRuleVersionID",ReplenLocationTypeRule.VersionID).
      
      ReplenLocationTypeRuleBrowse:endRow().
      
   END. /*FOR EACH ReplenLocationTypeRule NO-LOCK */
   
   ReplenLocationTypeRuleBrowse:endTable().
   chrPageBuildError = chrPageBuildError + ReplenLocationTypeRuleBrowse:getErrors().
   
   /* Create a new frame */
   ReplenLocationTypeRuleBrowseFrame = NEW pageFrame().
   ReplenLocationTypeRuleBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   ReplenLocationTypeRuleBrowseFrame:FormAction="dbReplenLocationTypeRuleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   ReplenLocationTypeRuleBrowseFrame:formOpen("replenlocationtyperule_browse_form").
   
   /* Start the Frame Header */
   ReplenLocationTypeRuleBrowseFrame:insertSpacer(5).
   ReplenLocationTypeRuleBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   ReplenLocationTypeRuleBrowse:displayBrowse().  
   
   /* End the Frame Header */
   ReplenLocationTypeRuleBrowseFrame:frameClose().
   ReplenLocationTypeRuleBrowseFrame:insertSpacer(10).
   
   ReplenLocationTypeRuleBrowseFrame:insertHiddenField("replenlocationtyperule_browse_scroll","").
   ReplenLocationTypeRuleBrowseFrame:insertHiddenField("ReplenLocationTypeRuleID","").
   ReplenLocationTypeRuleBrowseFrame:insertHiddenField("ReplenLocationTypeRuleVersionID","").
   ReplenLocationTypeRuleBrowseFrame:insertHiddenField("popup_replenlocationtyperulehistory_browse","").
   ReplenLocationTypeRuleBrowseFrame:insertHiddenField("form_name","replenlocationtyperule_browse_form").
   ReplenLocationTypeRuleBrowseFrame:insertHiddenField("prog_name","adReplenLocationTypeRule.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ReplenLocationTypeRuleBrowseFrame}
   
   ReplenLocationTypeRuleBrowseFrame:formClose().
   
   /* Create Button Bar */
   ReplenLocationTypeRuleBrowseButtons = NEW buttonBar().
   ReplenLocationTypeRuleBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   ReplenLocationTypeRuleBrowseButtons:addButton("replenlocationtyperule_browse_form_btn_create",
                                             fTL("Create"),
                                             "createReplenLocationTypeRule('replenlocationtyperule_details_form');",
                                             "").
   
   ReplenLocationTypeRuleBrowseButtons:addButton("replenlocationtyperule_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewReplenLocationTypeRuleDetails('replenlocationtyperule_details_form');",
                                             "Disabled").
   

/*   ReplenLocationTypeRuleBrowseButtons:addButton("replenlocationtyperule_browse_form_btn_history",                  */
/*                                             fTL("History"),                                                        */
/*                                             "viewHistory();",                                                      */
/*                                             "Disabled").                                                           */
                                             
   /*
   ReplenLocationTypeRuleBrowseButtons:addButton("replenlocationtyperule_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteReplenLocationTypeRule();",
                                             (IF intSelectedReplenLocationTypeRule > 0 THEN "" ELSE "Disabled")).
   */
   
   ReplenLocationTypeRuleBrowseButtons:closeBar().  
   ReplenLocationTypeRuleBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReplenLocationTypeRuleDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenLocationTypeRuleDetails Procedure 
PROCEDURE pReplenLocationTypeRuleDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "replenlocationtyperule_details_form"}
   
   ASSIGN chrDisplayFieldList  = "ReplenLocationTypeRuleID,ListingSequence,SourceLocationTypeID,TargetLocationTypeID" 
                               + ",Priority,Active"
          chrEditFieldList     = "ListingSequence,SourceLocationTypeID,TargetLocationTypeID,Priority,Active" 
          chrNewFieldList      = "ListingSequence,SourceLocationTypeID,TargetLocationTypeID,Priority,Active" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER,Priority:INTEGER".
   
   ReplenLocationTypeRuleDetailsForm = NEW dataForm("replenlocationtyperule_details_form").
   ReplenLocationTypeRuleDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   ReplenLocationTypeRuleDetailsForm:FormAction = "dbReplenLocationTypeRuleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ReplenLocationTypeRuleDetailsForm:FormWidth   = 460.
   ReplenLocationTypeRuleDetailsForm:FormHeight  = 200.
   ReplenLocationTypeRuleDetailsForm:FormTitle   = "ReplenLocationTypeRule Details".
   ReplenLocationTypeRuleDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   ReplenLocationTypeRuleDetailsForm:insertPaddingColumn(30).
   ReplenLocationTypeRuleDetailsForm:insertColumn(170).
   ReplenLocationTypeRuleDetailsForm:insertColumn(160).
   ReplenLocationTypeRuleDetailsForm:insertColumn(20).
   ReplenLocationTypeRuleDetailsForm:insertColumn(4).
   ReplenLocationTypeRuleDetailsForm:insertColumn(110).
   
   /* Fields */
   ReplenLocationTypeRuleDetailsForm:startRow().
   ReplenLocationTypeRuleDetailsForm:insertLabel("ReplenLocationTypeRuleID").
   ReplenLocationTypeRuleDetailsForm:insertTextField("ReplenLocationTypeRuleID", "", 200, TRUE).  
   
   ReplenLocationTypeRuleDetailsForm:startRow().
   ReplenLocationTypeRuleDetailsForm:insertLabel("Listing Sequence").
   ReplenLocationTypeRuleDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).  
   
   ReplenLocationTypeRuleDetailsForm:startRow().
   ReplenLocationTypeRuleDetailsForm:insertLabel("Source Location Type").
   ReplenLocationTypeRuleDetailsForm:insertComboField("SourceLocationTypeID", "", 200, TRUE).
   FOR EACH LocationType NO-LOCK /*idx=LocationTypeActive*/
/*      WHERE LocationType.LocationTypeID = SourceLocationTypeID.LocationTypeID*/
      WHERE   LocationType.Active:
      
      ReplenLocationTypeRuleDetailsForm:insertComboPairs("SourceLocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName). 
   END.   
   
   ReplenLocationTypeRuleDetailsForm:startRow().
   ReplenLocationTypeRuleDetailsForm:insertLabel("Target Location Type").
   ReplenLocationTypeRuleDetailsForm:insertComboField("TargetLocationTypeID", "", 200, TRUE).
   FOR EACH LocationType NO-LOCK /*idx=LocationTypeActive*/
/*      WHERE LocationType.LocationTypeID = TargetLocationTypeID.LocationTypeID*/
      WHERE   LocationType.Active:
      
      ReplenLocationTypeRuleDetailsForm:insertComboPairs("TargetLocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName). 
   END. 
   
   ReplenLocationTypeRuleDetailsForm:startRow().
   ReplenLocationTypeRuleDetailsForm:insertLabel("Priority").
   ReplenLocationTypeRuleDetailsForm:insertTextField("Priority", "", 200, TRUE).  
   
   ReplenLocationTypeRuleDetailsForm:startRow().
   ReplenLocationTypeRuleDetailsForm:insertLabel(fTL("Active")). 
   ReplenLocationTypeRuleDetailsForm:insertComboField("Active", "", 200, TRUE).  
   ReplenLocationTypeRuleDetailsForm:insertComboPairs("Active", "yes", "Active").
   ReplenLocationTypeRuleDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pReplenLocationTypeRuleDetailsFields}
   
   /* Add Hidden Fields*/
   ReplenLocationTypeRuleDetailsForm:insertHiddenField("replenlocationtyperule_browse_scroll", "").
   ReplenLocationTypeRuleDetailsForm:insertHiddenField("form_name", "replenlocationtyperule_details_form").
   ReplenLocationTypeRuleDetailsForm:insertHiddenField("prog_name", "adReplenLocationTypeRule.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ReplenLocationTypeRuleDetailsForm}
   
   /* Create Button Bar */
   ReplenLocationTypeRuleDetailsButtons = NEW buttonBar().
   
   ReplenLocationTypeRuleDetailsButtons:addButton("replenlocationtyperule_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateReplenLocationTypeRule('replenlocationtyperule_details_form');").
   
   ReplenLocationTypeRuleDetailsButtons:addButton("replenlocationtyperule_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('replenlocationtyperule_details_form_popup');").
   
   ReplenLocationTypeRuleDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ReplenLocationTypeRuleDetailsForm:FormButtons = ReplenLocationTypeRuleDetailsButtons.
   
   ReplenLocationTypeRuleDetailsForm:endForm(). 
   
   ReplenLocationTypeRuleDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + ReplenLocationTypeRuleDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReplenLocationTypeRuleDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenLocationTypeRuleDetailsFields Procedure 
PROCEDURE pReplenLocationTypeRuleDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         ReplenLocationTypeRuleDetailsForm:startRow().
         ReplenLocationTypeRuleDetailsForm:insertLabel(fTL("Field Label")).
         ReplenLocationTypeRuleDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReplenLocationTypeRuleHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenLocationTypeRuleHistory Procedure
PROCEDURE pReplenLocationTypeRuleHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
/*   {webGetWebForm.i "replenlocationtyperulehistory_details_form"}                                                                                                                                                          */
/*                                                                                                                                                                                                                           */
/*   FIND FIRST ReplenLocationTypeRule WHERE ReplenLocationTypeRule.ReplenLocationTypeRuleID = intSelectedReplenLocationTypeRule NO-LOCK NO-ERROR.                                                                           */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowseForm = NEW dataForm("replenlocationtyperulehistory_browse_form").                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.                                                                                                                                            */
/*                                                                                                                                                                                                                           */
/*   /* Setup */                                                                                                                                                                                                             */
/*   ReplenLocationTypeRuleHistoryBrowseForm:FormWidth   = 850.                                                                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowseForm:FormHeight  = 540.                                                                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE ReplenLocationTypeRule THEN " for ReplenLocationTypeRuleID: "                                                                      */
/*                                                                  + STRING(ReplenLocationTypeRule.ReplenLocationTypeRuleID) ELSE "").                                                                                      */
/*   ReplenLocationTypeRuleHistoryBrowseForm:FormType    = "xxl_large".                                                                                                                                                      */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowse = NEW browseTable("replenlocationtyperulehistory_browse").                                                                                                                          */
/*   ReplenLocationTypeRuleHistoryBrowse:BrowseWidth  = 830.                                                                                                                                                                 */
/*   ReplenLocationTypeRuleHistoryBrowse:BrowseHeight = 500.                                                                                                                                                                 */
/*   ReplenLocationTypeRuleHistoryBrowse:ExcelExport  = TRUE.                                                                                                                                                                */
/*   ReplenLocationTypeRuleHistoryBrowse:SessionID    = intGblSessionID.                                                                                                                                                     */
/*                                                                                                                                                                                                                           */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("History ID"),       70, "INTEGER", FALSE).                                                                                                                        */
/*                                                                                                                                                                                                                           */
/*   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */                                                                                                    */
/*   {webGetOptionalBrowseHeaders.i ReplenLocationTypeRuleHistory}                                                                                                                                                           */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("Listing Seq"),      70, "INTEGER",           FALSE).                                                                                                              */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("Source Location Type"), 100, "CHARACTER", "left", FALSE).                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("Target Location Type"), 100, "CHARACTER", "left", FALSE).                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("Priority"),         80, "INTEGER",           FALSE).                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("Active"),           80, "LOGICAL",           FALSE).                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("User"),            120, "CHARACTER", "left", FALSE).                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowse:insertColumn(fTL("Created"),         120, "CHARACTER", "left", FALSE).                                                                                                              */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowse:StartBody().                                                                                                                                                                        */
/*                                                                                                                                                                                                                           */
/*   IF AVAILABLE ReplenLocationTypeRule THEN                                                                                                                                                                                */
/*   DO:                                                                                                                                                                                                                     */
/*      List the ReplenLocationTypeRuleHistory*/
/*      FOR EACH ReplenLocationTypeRuleHistory NO-LOCK                                                              */
/*         WHERE  ReplenLocationTypeRuleHistory.ReplenLocationTypeRuleID = intSelectedReplenLocationTypeRule        */
/*         BY ReplenLocationTypeRuleHistory.ReplenLocationTypeRuleHistoryID:                                        */
/*                                                                                                                  */
/*         FIND FIRST LocationType NO-LOCK /* idx=LocationTypeID */                                                 */
/*            WHERE LocationType.LocationTypeID = ReplenLocationTypeRuleHistory.SourceLocationTypeID NO-ERROR.      */
/*                                                                                                                  */
/*         FIND FIRST targetLocationType NO-LOCK /* idx=LocationTypeID */                                           */
/*            WHERE targetLocationType.LocationTypeID = ReplenLocationTypeRuleHistory.TargetLocationTypeID NO-ERROR.*/
/*                                                                                                                  */
/*         FIND FIRST GateUser NO-LOCK                                                                              */
/*            WHERE GateUser.GateUserID =  ReplenLocationTypeRuleHistory.GateUserID NO-ERROR.                       */
/*                                                                                                                                                                                                                           */
/*         ReplenLocationTypeRuleHistoryBrowse:startRow(ReplenLocationTypeRuleHistory.ReplenLocationTypeRuleHistoryID, "selectHistoryRow(this," + '"' + STRING(ReplenLocationTypeRuleHistory.ReplenLocationTypeRuleHistoryID)*/
/*                                                                     + '","replenLocationTypeRuleHistory"' + ");", "").                                                                                                    */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData(ReplenLocationTypeRuleHistory.ReplenLocationTypeRuleHistoryID).                                                                                                    */
/*                                                                                                                                                                                                                           */
/*         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */                                                                                                      */
/*         {webGetOptionalBrowseFields.i ReplenLocationTypeRuleHistory}                                                                                                                                                      */
/*                                                                                                                                                                                                                           */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData(STRING(ReplenLocationTypeRuleHistory.ListingSequence)).                                                                                                            */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData((IF AVAILABLE LocationType THEN LocationType.TypeName ELSE "") ,"left").                                                                                                */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData((IF AVAILABLE targetLocationType THEN targetLocationType.TypeName ELSE "") ,"left").                                                                            */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData(STRING(ReplenLocationTypeRuleHistory.Priority)).                                                                                                                   */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData(STRING(ReplenLocationTypeRuleHistory.Active, "Yes/No")).                                                                                                           */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").                                                                                                   */
/*         ReplenLocationTypeRuleHistoryBrowse:insertData(fDisplayDate&Time(ReplenLocationTypeRuleHistory.Created,"y/m/d H:M:S"), "right").                                                                                  */
/*                                                                                                                                                                                                                           */
/*                                                                                                                                                                                                                           */
/*         /* Add hidden fields */                                                                                                                                                                                           */
/*         ReplenLocationTypeRuleHistoryBrowse:insertHiddendata("ReplenLocationTypeRuleHistoryID",ReplenLocationTypeRuleHistory.ReplenLocationTypeRuleHistoryID).                                                            */
/*                                                                                                                                                                                                                           */
/*         ReplenLocationTypeRuleHistoryBrowse:endRow().                                                                                                                                                                     */
/*                                                                                                                                                                                                                           */
/*      END. /* FOR EACH ReplenLocationTypeRuleHistory NO-LOCK, */                                                                                                                                                           */
/*   END. /*IF AVAILABLE SerialMaskType THEN*/                                                                                                                                                                               */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowse:endTable().                                                                                                                                                                         */
/*   chrPageBuildError = chrPageBuildError + ReplenLocationTypeRuleHistoryBrowse:getErrors().                                                                                                                                */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowseForm:insertHiddenField("ReplenLocationTypeRuleHistoryID","").                                                                                                                        */
/*   ReplenLocationTypeRuleHistoryBrowseForm:insertHiddenField("popup_replenlocationtyperulehistory_browse","").                                                                                                             */
/*                                                                                                                                                                                                                           */
/*   /* This adds all of the standard form field lists to the form */                                                                                                                                                        */
/*   {webGetHiddenFormFields.i ReplenLocationTypeRuleHistoryBrowseForm}                                                                                                                                                      */
/*                                                                                                                                                                                                                           */
/*   /* Create Button Bar */                                                                                                                                                                                                 */
/*   ReplenLocationTypeRuleHistoryBrowseButtons = NEW buttonBar().                                                                                                                                                           */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowseButtons:addButton("replenlocationtyperulehistory_browse_form_btn_details",                                                                                                           */
/*                                                fTL("Details"),                                                                                                                                                            */
/*                                                "viewReplenLocationTypeRuleHistoryDetails('replenlocationtyperulehistory_details_form');",                                                                                 */
/*                                                "Disabled").                                                                                                                                                               */
/*                                                                                                                                                                                                                           */
/*     /*Button for later if needed*/                                                                                                                                                                                        */
/*/*   ReplenLocationTypeRuleHistoryBrowseButtons:addButton("replenlocationtyperulehistory_browse_form_btn_excel",               */                                                                                                            */
/*/*                                                fTL("Excel Export"),                                                     */                                                                                              */
/*/*                                                "excelExport('" + STRING(intGblSessionID) + "_replenlocationtyperulehistory_browse.xml')").*/                                                                                              */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowseButtons:addButton("replenlocationtyperulehistory_browse_form_btn_cancel",                                                                                                            */
/*                                                fTL("Cancel"),                                                                                                                                                             */
/*                                                "disablePopup('replenlocationtyperulehistory_browse_form_popup');").                                                                                                       */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowseButtons:closeBar().                                                                                                                                                                  */
/*                                                                                                                                                                                                                           */
/*   /* Assign the Button Bar Object to the Form Object */                                                                                                                                                                   */
/*   ReplenLocationTypeRuleHistoryBrowseForm:FormBrowse  = ReplenLocationTypeRuleHistoryBrowse.                                                                                                                              */
/*   ReplenLocationTypeRuleHistoryBrowseForm:FormButtons = ReplenLocationTypeRuleHistoryBrowseButtons.                                                                                                                       */
/*   ReplenLocationTypeRuleHistoryBrowseForm:endForm().                                                                                                                                                                      */
/*                                                                                                                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryBrowseForm:displayForm().                                                                                                                                                                  */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pReplenLocationTypeRuleHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenLocationTypeRuleHistoryDetails Procedure
PROCEDURE pReplenLocationTypeRuleHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
/*   {webGetWebForm.i "replenlocationtyperulehistory_details_form"}                                                                                   */
/*                                                                                                                                                    */
/*   chrDisplayFieldList  = "ReplenLocationTypeRuleHistoryID,ReplenLocationTypeRuleID,SourceLocationTypeID,TargetLocationTypeID"                      */
/*                        + ",Priority,ListingSequence,Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".                                        */
/*                                                                                                                                                    */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm = NEW dataForm("replenlocationtyperulehistory_details_form").                                           */
/*   ReplenLocationTypeRuleHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.                                                                    */
/*                                                                                                                                                    */
/*   /* Setup */                                                                                                                                      */
/*   ReplenLocationTypeRuleHistoryDetailsForm:FormWidth   = 545.                                                                                      */
/*   ReplenLocationTypeRuleHistoryDetailsForm:FormHeight  = 440.                                                                                      */
/*   ReplenLocationTypeRuleHistoryDetailsForm:FormTitle   = "ReplenLocationTypeRule History Details".                                                 */
/*   ReplenLocationTypeRuleHistoryDetailsForm:FormType    = "large".                                                                                  */
/*                                                                                                                                                    */
/*   /* Column Layout */                                                                                                                              */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertPaddingColumn(40).                                                                                */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertColumn(110).                                                                                      */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertColumn(120).                                                                                      */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertColumn(20).                                                                                       */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertColumn(4).                                                                                        */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertColumn(40).                                                                                       */
/*                                                                                                                                                    */
/*   /* Fields */                                                                                                                                     */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel(fTL("History ID")).                                                                         */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertTextField("ReplenLocationTypeRuleHistoryID", "", 90, TRUE).                                       */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel(fTL("ReplenLocationTypeRuleID")).                                                           */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertTextField("ReplenLocationTypeRuleID", "", 90, TRUE).                                              */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel(fTL("ListingSequence")).                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertTextField("ListingSequence", "", 90, TRUE).                                                       */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel("Source Location Type").                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertComboField("SourceLocationTypeID", "", 200, TRUE).                                                */
/*   FOR EACH LocationType NO-LOCK /*idx=LocationTypeActive*/                                                                                         */
/*/*      WHERE LocationType.LocationTypeID = SourceLocationTypeID.LocationTypeID*/                                                                   */
/*      WHERE   LocationType.Active:                                                                                                                  */
/*                                                                                                                                                    */
/*      ReplenLocationTypeRuleHistoryDetailsForm:insertComboPairs("SourceLocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).*/
/*   END.                                                                                                                                             */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleDetailsHistoryForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleDetailsHistoryForm:insertLabel("Target Location Type").                                                                    */
/*   ReplenLocationTypeRuleDetailsHistoryForm:insertComboField("TargetLocationTypeID", "", 200, TRUE).                                                */
/*   FOR EACH LocationType NO-LOCK /*idx=LocationTypeActive*/                                                                                         */
/*/*      WHERE LocationType.LocationTypeID = TargetLocationTypeID.LocationTypeID*/                                                                   */
/*      WHERE   LocationType.Active:                                                                                                                  */
/*                                                                                                                                                    */
/*      ReplenLocationTypeRuleHistoryDetailsForm:insertComboPairs("TargetLocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).*/
/*   END.                                                                                                                                             */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel("Priority").                                                                                */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertTextField("Priority", "", 200, TRUE).                                                             */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel(fTL("Active")).                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertComboField("Active", "", 110, TRUE).                                                              */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").                                                            */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").                                                        */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel("User").                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertComboField("GateUserID", "", 110, TRUE).                                                          */
/*   /* Insert the Status Codes */                                                                                                                    */
/*   FOR EACH GateUser NO-LOCK                                                                                                                        */
/*      BY GateUser.FullName:                                                                                                                         */
/*      ReplenLocationTypeRuleHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                      */
/*   END.                                                                                                                                             */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:startRow().                                                                                             */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel("Created").                                                                                 */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).                                                          */
/*   /* Time fields have no label */                                                                                                                  */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                         */
/*   /* This has a label to separate the time */                                                                                                      */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertLabel(":").                                                                                       */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                         */
/*                                                                                                                                                    */
/*   /* Add Hidden Fields*/                                                                                                                           */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertHiddenField("replenlocationtyperule_browse_scroll","").                                           */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertHiddenField("popup_replenlocationtyperulehistory_browse", "").                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertHiddenField("ReplenLocationTypeRuleHistoryID","").                                                */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertHiddenField("form_name","replenlocationtyperulehistory_details_form").                            */
/*   ReplenLocationTypeRuleHistoryDetailsForm:insertHiddenField("prog_name","adReplenLocationTypeRule.p").                                            */
/*                                                                                                                                                    */
/*   /* This adds all of the standard form field lists to the form */                                                                                 */
/*   {webGetHiddenFormFields.i ReplenLocationTypeRuleHistoryDetailsForm}                                                                              */
/*                                                                                                                                                    */
/*   /* Create Button Bar */                                                                                                                          */
/*   ReplenLocationTypeRuleHistoryDetailsButtons = NEW buttonBar().                                                                                   */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsButtons:addButton("replenlocationtyperulehistory_details_form_btn_cancel",                                   */
/*                                                  fTL("Cancel"),                                                                                    */
/*                                                  "disablePopup('replenlocationtyperulehistory_details_form_popup');").                             */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsButtons:closeBar().                                                                                          */
/*                                                                                                                                                    */
/*   /* Assign the Button Bar Object to the Form Object */                                                                                            */
/*   ReplenLocationTypeRuleHistoryDetailsForm:FormButtons = ReplenLocationTypeRuleHistoryDetailsButtons.                                              */
/*                                                                                                                                                    */
/*   ReplenLocationTypeRuleHistoryDetailsForm:endForm().                                                                                              */
/*   ReplenLocationTypeRuleHistoryDetailsForm:displayForm().                                                                                          */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

