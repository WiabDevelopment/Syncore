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
{defWebDefinitions.i}
{defDataMigrationVariables.i}

/* Definitions for System Options for Admin */
{getAdminOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedInboundStatusGroup        AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectInboundStatusGroupRow       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToInboundStatusGroupRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrInboundStatusGroupID              AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedInboundStatusGroupLink    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectInboundStatusGroupLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToInboundStatusGroupLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrInboundStatusGroupLinkID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupInboundStatusGroupLinks      AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE InboundStatusGroupBrowseFrame        AS pageFrame.
DEFINE VARIABLE InboundStatusGroupBrowse             AS browseTable.
DEFINE VARIABLE InboundStatusGroupBrowseButtons      AS buttonBar.
DEFINE VARIABLE InboundStatusGroupDetailsForm        AS dataForm.
DEFINE VARIABLE InboundStatusGroupDetailsButtons     AS buttonBar.

DEFINE VARIABLE InboundStatusGroupLinkBrowseFrame    AS pageFrame.
DEFINE VARIABLE InboundStatusGroupLinkBrowse         AS browseTable.
DEFINE VARIABLE InboundStatusGroupLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE InboundStatusGroupLinkDetailsForm    AS dataForm.
DEFINE VARIABLE InboundStatusGroupLinkDetailsButtons AS buttonBar.
DEFINE VARIABLE InboundStatusGroupLinkBrowseForm     AS dataForm.

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
         WIDTH              = 67.
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
  {prcDataMigrationProcedures.i}
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
   
   ASSIGN chrInboundStatusGroupID              = get-value("InboundStatusGroupID")
          intSelectedInboundStatusGroup        = INTEGER(chrInboundStatusGroupID)
          chrScrollToInboundStatusGroupRow     = STRING(INTEGER(get-value("inboundstatusgroup_browse_scroll"))) + ";"
          chrInboundStatusGroupLinkID          = get-value("InboundStatusGroupLinkID")
          intSelectedInboundStatusGroupLink    = INTEGER(chrInboundStatusGroupLinkID)
          chrScrollToInboundStatusGroupLinkRow = STRING(INTEGER(get-value("inboundstatusgrouplink_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrInboundStatusGroupID <> "" THEN
      chrSelectInboundStatusGroupRow = 'selectInboundStatusGroupRow(document.getElementById("inboundstatusgroup_browse_row_' + chrInboundStatusGroupID + '"),"' 
                                                         + chrInboundStatusGroupID +  '");'.
   
   IF chrInboundStatusGroupLinkID <> "" THEN
      chrSelectInboundStatusGroupLinkRow = 'selectInboundStatusGroupLinkRow(document.getElementById("inboundstatusgrouplink_browse_row_' 
                                           + chrInboundStatusGroupLinkID + '"),"' + chrInboundStatusGroupLinkID +  '");'.

   IF get-value('popup_inboundstatusgrouplink_browse') = "yes" THEN 
      chrPopupInboundStatusGroupLinks = 'enablePopup("inboundstatusgrouplink_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("inboundstatusgroup_browse").scrollTop=' + chrScrollToInboundStatusGroupRow 
                             + chrSelectInboundStatusGroupRow + chrSelectInboundStatusGroupLinkRow + chrPopupInboundStatusGroupLinks.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}  
      
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "InboundStatusGroup Admin".
   ThisPage:FrameTitle    = "InboundStatusGroup Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("inboundstatusgroup.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pInboundStatusGroupBrowse.
   
   FIND FIRST InboundStatusGroup NO-LOCK
      WHERE InboundStatusGroup.InboundStatusGroupID = intSelectedInboundStatusGroup NO-ERROR.

   /******* Pop-up Browsers and Forms ********/    
   RUN pInboundStatusGroupDetails.

   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
   
   RUN pInboundStatusGroupLinkBrowse.

   FIND FIRST InboundStatusGroupLink NO-LOCK
      WHERE InboundStatusGroupLink.InboundStatusGroupLinkID = intSelectedInboundStatusGroupLink NO-ERROR.

   RUN pInboundStatusGroupLinkDetails.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT InboundStatusGroupBrowseFrame        NO-ERROR.
   DELETE OBJECT InboundStatusGroupBrowse             NO-ERROR.
   DELETE OBJECT InboundStatusGroupBrowseButtons      NO-ERROR.
   DELETE OBJECT InboundStatusGroupDetailsForm        NO-ERROR.
   DELETE OBJECT InboundStatusGroupDetailsButtons     NO-ERROR.
   DELETE OBJECT InboundStatusGroupLinkBrowseFrame    NO-ERROR.
   DELETE OBJECT InboundStatusGroupLinkBrowse         NO-ERROR.
   DELETE OBJECT InboundStatusGroupLinkBrowseButtons  NO-ERROR.
   DELETE OBJECT InboundStatusGroupLinkDetailsForm    NO-ERROR.
   DELETE OBJECT InboundStatusGroupLinkDetailsButtons NO-ERROR.
   DELETE OBJECT InboundStatusGroupLinkBrowseForm     NO-ERROR.

   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusGroupBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusGroupBrowse Procedure 
PROCEDURE pInboundStatusGroupBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "inboundstatusgroup_details_form"}
   
   InboundStatusGroupBrowse              = NEW browseTable("inboundstatusgroup_browse").
   InboundStatusGroupBrowse:BrowseWidth  = 965.
   InboundStatusGroupBrowse:BrowseHeight = 455.
   InboundStatusGroupBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   InboundStatusGroupBrowse:insertColumn(fTL("Group ID"),           60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i InboundStatusGroup}
   
   InboundStatusGroupBrowse:insertColumn(fTL("List Seq"),           60, "INTEGER", FALSE).
   InboundStatusGroupBrowse:insertColumn(fTL("Group Code"),        150, "CHARACTER", "left", FALSE).
   InboundStatusGroupBrowse:insertColumn(fTL("Group Name"),        150, "CHARACTER", "left", FALSE).
   InboundStatusGroupBrowse:insertColumn(fTL("Group Description"), 220, "CHARACTER", "left", FALSE).
   InboundStatusGroupBrowse:insertColumn(fTL("Active"),             50, "LOGICAL", FALSE).
   
   /*Body*/
   InboundStatusGroupBrowse:startBody().
   
   /* Find all InboundStatusGroups then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH InboundStatusGroup NO-LOCK
      BY    InboundStatusGroup.Active DESCENDING
      BY    InboundStatusGroup.ListingSequence 
      BY    InboundStatusGroup.InboundStatusGroupID:
      
      InboundStatusGroupBrowse:startRow(InboundStatusGroup.InboundStatusGroupID, "selectInboundStatusGroupRow(this," + '"' 
                                                                     + STRING(InboundStatusGroup.InboundStatusGroupID) + '"' + ");", "").
      InboundStatusGroupBrowse:insertData(InboundStatusGroup.InboundStatusGroupID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i InboundStatusGroup}
      
      InboundStatusGroupBrowse:insertData(STRING(InboundStatusGroup.ListingSequence)).
      InboundStatusGroupBrowse:insertData(InboundStatusGroup.GroupCode, "left").
      InboundStatusGroupBrowse:insertData(InboundStatusGroup.GroupName, "left").
      InboundStatusGroupBrowse:insertData(InboundStatusGroup.GroupDescr, "left").
      InboundStatusGroupBrowse:insertData(STRING(InboundStatusGroup.Active, "Yes/No")).
      
      /* Add hidden fields */
      InboundStatusGroupBrowse:insertHiddenData("InboundStatusGroupVersionID",InboundStatusGroup.VersionID).
      
      InboundStatusGroupBrowse:endRow().
      
   END. /* FOR EACH InboundStatusGroup NO-LOCK */
   
   InboundStatusGroupBrowse:endTable().
   chrPageBuildError = chrPageBuildError + InboundStatusGroupBrowse:getErrors().
   
   /* Create a new frame */
   InboundStatusGroupBrowseFrame           = NEW pageFrame().
   InboundStatusGroupBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   InboundStatusGroupBrowseFrame:FormAction="dbInboundStatusGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   InboundStatusGroupBrowseFrame:formOpen("inboundstatusgroup_browse_form").
   
   /* Start the Frame Header */
   InboundStatusGroupBrowseFrame:insertSpacer(5).
   InboundStatusGroupBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   InboundStatusGroupBrowse:displayBrowse().  
   
   /* End the Frame Header */
   InboundStatusGroupBrowseFrame:frameClose().
   InboundStatusGroupBrowseFrame:insertSpacer(10).
   
   InboundStatusGroupBrowseFrame:insertHiddenField("inboundstatusgroup_browse_scroll","").
   InboundStatusGroupBrowseFrame:insertHiddenField("InboundStatusGroupID","").
   InboundStatusGroupBrowseFrame:insertHiddenField("InboundStatusGroupVersionID","").
   InboundStatusGroupBrowseFrame:insertHiddenField("inboundstatusgrouplink_browse_scroll","").
   InboundStatusGroupBrowseFrame:insertHiddenField("popup_inboundstatusgrouplink_browse","").
   InboundStatusGroupBrowseFrame:insertHiddenField("InboundStatusGroupLinkID","").
   InboundStatusGroupBrowseFrame:insertHiddenField("form_name","inboundstatusgroup_browse_form").
   InboundStatusGroupBrowseFrame:insertHiddenField("prog_name","adInboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundStatusGroupBrowseFrame}
   
   InboundStatusGroupBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}   
      
   /* Create Button Bar */
   InboundStatusGroupBrowseButtons           = NEW buttonBar().
   InboundStatusGroupBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
      
   InboundStatusGroupBrowseButtons:addButton("inboundstatusgroup_browse_form_btn_details",
                                          fTL("Details"),
                                          "viewInboundStatusGroupDetails('inboundstatusgroup_details_form');",
                                          (IF intSelectedInboundStatusGroup > 0 THEN "" ELSE "Disabled")).
   
   InboundStatusGroupBrowseButtons:addButton("inboundstatusgroup_browse_form_btn_inboundstatus",
                                          fTL("InboundStatus"),
                                          "viewInboundStatusGroupLinks('inboundstatus_details_form');",
                                          (IF intSelectedInboundStatusGroup > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO: 
      InboundStatusGroupBrowseButtons:addButton("inboundstatusgroup_browse_form_btn_create",
                                             fTL("Create"),
                                             "createInboundStatusGroup('inboundstatusgroup_details_form');",
                                             "").

      InboundStatusGroupBrowseButtons:addButton("inboundstatusgroup_browse_form_btn_applinks",
                                             fTL("App Links"),
                                             "viewAppLinkBrowse('inboundstatusgroup_browse_form','InboundStatusGroup');",
                                             "Disabled").
                                     
   END. 
   
   InboundStatusGroupBrowseButtons:closeBar().  
   InboundStatusGroupBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusGroupDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusGroupDetails Procedure 
PROCEDURE pInboundStatusGroupDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "inboundstatusgroup_details_form"}
   
   ASSIGN chrDisplayFieldList  = "InboundStatusGroupID,ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrEditFieldList     = "ListingSequence,GroupName,GroupDescr,Active"
          chrNewFieldList      = "ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrRequiredFieldList = "ListingSequence,GroupCode,GroupName,GroupDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   InboundStatusGroupDetailsForm           = NEW dataForm("inboundstatusgroup_details_form").
   InboundStatusGroupDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   InboundStatusGroupDetailsForm:FormAction = "dbInboundStatusGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   InboundStatusGroupDetailsForm:FormWidth  = 460.
   InboundStatusGroupDetailsForm:FormHeight = 300.
   InboundStatusGroupDetailsForm:FormTitle  = "InboundStatusGroup Details".
   InboundStatusGroupDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   InboundStatusGroupDetailsForm:insertPaddingColumn(10).
   InboundStatusGroupDetailsForm:insertColumn(100).
   InboundStatusGroupDetailsForm:insertColumn(120).
   InboundStatusGroupDetailsForm:insertColumn(20).
   InboundStatusGroupDetailsForm:insertColumn(4).
   InboundStatusGroupDetailsForm:insertColumn(110).
   
   /* Fields */
   InboundStatusGroupDetailsForm:startRow().
   InboundStatusGroupDetailsForm:insertLabel("Group ID").
   InboundStatusGroupDetailsForm:insertTextField("InboundStatusGroupID", "", 110, TRUE).  
   
   InboundStatusGroupDetailsForm:startRow().
   InboundStatusGroupDetailsForm:insertLabel("Listing Seq").
   InboundStatusGroupDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   InboundStatusGroupDetailsForm:startRow().
   InboundStatusGroupDetailsForm:insertLabel("Group Code").
   InboundStatusGroupDetailsForm:insertTextField("GroupCode", "", 150, TRUE).  
   
   InboundStatusGroupDetailsForm:startRow().
   InboundStatusGroupDetailsForm:insertLabel("Group Name").
   InboundStatusGroupDetailsForm:insertTextField("GroupName", "", 150, TRUE).
   
   InboundStatusGroupDetailsForm:startRow().
   InboundStatusGroupDetailsForm:insertLabel("Group Descr").
   InboundStatusGroupDetailsForm:insertTextField("GroupDescr", "", 300, TRUE).

   InboundStatusGroupDetailsForm:startRow().
   InboundStatusGroupDetailsForm:insertLabel(fTL("Active")). 
   InboundStatusGroupDetailsForm:insertComboField("Active", "", 110, TRUE).  
   InboundStatusGroupDetailsForm:insertComboPairs("Active", "yes", "Active").
   InboundStatusGroupDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pInboundStatusGroupDetailsFields}
   
   /* Add Hidden Fields*/
   InboundStatusGroupDetailsForm:insertHiddenField("inboundstatusgroup_browse_scroll", "").
   InboundStatusGroupDetailsForm:insertHiddenField("form_name", "inboundstatusgroup_details_form").
   InboundStatusGroupDetailsForm:insertHiddenField("prog_name", "adInboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundStatusGroupDetailsForm}
   
   /* Create Button Bar */
   InboundStatusGroupDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:   
      InboundStatusGroupDetailsButtons:addButton("inboundstatusgroup_details_form_btn_save", 
                                               fTL("Save"), 
                                               "updateInboundStatusGroup('inboundstatusgroup_details_form');").
   END.
      
   InboundStatusGroupDetailsButtons:addButton("inboundstatusgroup_details_form_btn_cancel", 
                                            fTL("Cancel"), 
                                            "cancelUpdate('UserCancelled','process_mode'); disablePopup('inboundstatusgroup_details_form_popup');").
   
   InboundStatusGroupDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   InboundStatusGroupDetailsForm:FormButtons = InboundStatusGroupDetailsButtons.
   
   InboundStatusGroupDetailsForm:endForm(). 
   
   InboundStatusGroupDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusGroupDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusGroupDetailsFields Procedure 
PROCEDURE pInboundStatusGroupDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         InboundStatusGroupDetailsForm:startRow().
         InboundStatusGroupDetailsForm:insertLabel(fTL("Field Label")).
         InboundStatusGroupDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adInboundStatusGroupAdmin_inboundstatusgroup_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusGroupLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusGroupLinkBrowse Procedure 
PROCEDURE pInboundStatusGroupLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   
   {webGetWebForm.i "inboundstatusgrouplink_details_form"}
   
   InboundStatusGroupLinkBrowseForm = NEW dataForm("inboundstatusgrouplink_browse_form").
   InboundStatusGroupLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   InboundStatusGroupLinkBrowseForm:FormAction  = "dbInboundStatusGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   InboundStatusGroupLinkBrowseForm:FormWidth   = 700.
   InboundStatusGroupLinkBrowseForm:FormHeight  = 490.
   InboundStatusGroupLinkBrowseForm:FormTitle   = fTL("InboundStatuses for InboundStatusGroup") + 
                                               (IF AVAILABLE InboundStatusGroup THEN " : " + InboundStatusGroup.GroupName ELSE "").
   InboundStatusGroupLinkBrowseForm:FormType    = "xl_large".
   
   InboundStatusGroupLinkBrowse = NEW browseTable("inboundstatusgrouplink_browse").
   InboundStatusGroupLinkBrowse:BrowseWidth  = 680.
   InboundStatusGroupLinkBrowse:BrowseHeight = 432.
   
   InboundStatusGroupLinkBrowse:insertColumn(fTL("LinkID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i InboundStatusGroupLink}
   
   InboundStatusGroupLinkBrowse:insertColumn(fTL("InboundStatus Code"), 120, "CHARACTER", "left", FALSE).
   InboundStatusGroupLinkBrowse:insertColumn(fTL("InboundStatus Description"), 140, "CHARACTER", "left", FALSE).
   InboundStatusGroupLinkBrowse:insertColumn(fTL("Active"), 80, "CHARACTER", FALSE).
   
   InboundStatusGroupLinkBrowse:StartBody().
   
   IF AVAILABLE InboundStatusGroup THEN
   DO:
      /* List the InboundStatusGroupLinks for the InboundStatusGroup */
      FOR EACH InboundStatusGroupLink OF InboundStatusGroup NO-LOCK
         BY InboundStatusGroupLink.InboundStatusGroupID:
         
         FIND FIRST InboundStatus OF InboundStatusGroupLink NO-LOCK NO-ERROR.

         InboundStatusGroupLinkBrowse:startRow(InboundStatusGroupLink.InboundStatusGroupLinkID, 
                                            "selectInboundStatusGroupLinkRow(this," + '"' + 
                                            STRING(InboundStatusGroupLink.InboundStatusGroupLinkID) + 
                                            '","adInboundStatusGroupAdmin.p","inboundstatusgrouplink_browse_form"' + ");", "").
         InboundStatusGroupLinkBrowse:insertData(InboundStatusGroupLink.InboundStatusGroupLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i InboundStatusGroupLink}
         
         InboundStatusGroupLinkBrowse:insertData(IF AVAILABLE InboundStatus THEN InboundStatus.StatusCode ELSE "", "left").
         InboundStatusGroupLinkBrowse:insertData(IF AVAILABLE InboundStatus THEN InboundStatus.StatusDesc ELSE "", "left").
         InboundStatusGroupLinkBrowse:insertData(STRING(InboundStatusGroupLink.Active,"Yes/No")).
         
         /* Add hidden fields */
         InboundStatusGroupLinkBrowse:insertHiddenData("InboundStatusGroupID",InboundStatusGroupLink.InboundStatusGroupID).
         InboundStatusGroupLinkBrowse:insertHiddenData("InboundStatusID",(IF AVAILABLE InboundStatus THEN STRING(InboundStatus.InboundStatusID) ELSE "")).
         InboundStatusGroupLinkBrowse:insertHiddenData("StatusCode",(IF AVAILABLE InboundStatus THEN InboundStatus.StatusCode ELSE "")).
         InboundStatusGroupLinkBrowse:insertHiddenData("InboundStatusVersionID",(IF AVAILABLE InboundStatus THEN STRING(InboundStatus.VersionID) ELSE "")).
         InboundStatusGroupLinkBrowse:insertHiddenData("InboundStatusGroupLinkID",InboundStatusGroupLink.InboundStatusGroupLinkID).
         InboundStatusGroupLinkBrowse:insertHiddenData("InboundStatusGroupLinkVersionID",InboundStatusGroupLink.VersionID).
         InboundStatusGroupLinkBrowse:endRow().
      
      END. /* FOR EACH InboundStatusGroupLink OF InboundStatusGroup NO-LOCK */
   END. /*IF AVAILABLE InboundStatusGroup THEN*/
   
   InboundStatusGroupLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + InboundStatusGroupLinkBrowse:getErrors().
   
   InboundStatusGroupLinkBrowseForm:insertHiddenField("InboundStatusGroupID","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("GroupName","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("InboundStatusID","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("InboundStatusVersionID","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("InboundStatusCode","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("InboundStatusGroupLinkID","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("InboundStatusGroupLinkVersionID","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("inboundstatusgroup_browse_scroll","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("inboundstatusgrouplink_browse_scroll","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("popup_inboundstatusgrouplink_browse","").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("form_name","inboundstatusgrouplink_browse_form").
   InboundStatusGroupLinkBrowseForm:insertHiddenField("prog_name","adInboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundStatusGroupLinkBrowseForm}
   
   /* Create Button Bar */
   InboundStatusGroupLinkBrowseButtons = NEW buttonBar().
   
   InboundStatusGroupLinkBrowseButtons:addButton("inboundstatusgrouplink_browse_form_btn_create",
                                              fTL("Create"),
                                              "createInboundStatusGroupLink('inboundstatusgrouplink_details_form');"). 
   
   InboundStatusGroupLinkBrowseButtons:addButton("inboundstatusgrouplink_browse_form_btn_view",
                                              fTL("Details"),
                                              "viewInboundStatusGroupLinkDetails('inboundstatusgrouplink_details_form');",
                                              (IF intSelectedInboundStatusGroupLink > 0 THEN "" ELSE "Disabled")).
   
   InboundStatusGroupLinkBrowseButtons:addButton("inboundstatusgrouplink_browse_form_btn_delete",
                                              fTL("Delete"),
                                              "confirmDeleteInboundStatusGroupLink('inboundstatusgrouplink_browse_form');", 
                                              (IF intSelectedInboundStatusGroupLink > 0 THEN "" ELSE "Disabled")).
   
   InboundStatusGroupLinkBrowseButtons:addButton("inboundstatusgrouplink_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('inboundstatusgrouplink_browse_form_popup');").
   
   InboundStatusGroupLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   InboundStatusGroupLinkBrowseForm:FormBrowse  = InboundStatusGroupLinkBrowse.
   InboundStatusGroupLinkBrowseForm:FormButtons = InboundStatusGroupLinkBrowseButtons.
   InboundStatusGroupLinkBrowseForm:endForm(). 
   
   InboundStatusGroupLinkBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusGroupLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusGroupLinkDetails Procedure 
PROCEDURE pInboundStatusGroupLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "inboundstatusgrouplink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "InboundStatusGroupLinkID,InboundStatusID,Active"
          chrEditFieldList     = "Active"
          chrNewFieldList      = "InboundStatusID,Active"
          chrRequiredFieldList = "InboundStatusID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   InboundStatusGroupLinkDetailsForm = NEW dataForm("inboundstatusgrouplink_details_form").
   InboundStatusGroupLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   InboundStatusGroupLinkDetailsForm:FormAction  = "dbInboundStatusGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   InboundStatusGroupLinkDetailsForm:FormWidth   = 460.
   InboundStatusGroupLinkDetailsForm:FormHeight  = 300.
   InboundStatusGroupLinkDetailsForm:FormTitle   = "Inbound Status Group Link Details".
   InboundStatusGroupLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   InboundStatusGroupLinkDetailsForm:insertPaddingColumn(70).
   InboundStatusGroupLinkDetailsForm:insertColumn(120).
   InboundStatusGroupLinkDetailsForm:insertColumn(120).
   InboundStatusGroupLinkDetailsForm:insertColumn(30).
   InboundStatusGroupLinkDetailsForm:insertColumn(120).  
   
   /* Fields */
   InboundStatusGroupLinkDetailsForm:startRow().
   InboundStatusGroupLinkDetailsForm:insertLabel(fTL("Link ID")).
   InboundStatusGroupLinkDetailsForm:insertTextField("InboundStatusGroupLinkID", "", 110, TRUE).    
   
   InboundStatusGroupLinkDetailsForm:startRow().
   InboundStatusGroupLinkDetailsForm:insertLabel(fTL("Inbound Status")).
   InboundStatusGroupLinkDetailsForm:insertComboField("InboundStatusID", "", 200, TRUE). 
   FOR EACH InboundStatus NO-LOCK /*idx=ActiveListingSequence*/
      BY InboundStatus.Active DESC
      BY InboundStatus.StatusCode:
      
      InboundStatusGroupLinkDetailsForm:insertComboPairs("InboundStatusID", STRING(InboundStatus.InboundStatusID), InboundStatus.StatusCode).
   END.   
   
   InboundStatusGroupLinkDetailsForm:startRow().
   InboundStatusGroupLinkDetailsForm:insertLabel(fTL("Active")). 
   InboundStatusGroupLinkDetailsForm:insertComboField("Active", "", 110, TRUE).  
   InboundStatusGroupLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   InboundStatusGroupLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pInboundStatusGroupLinkDetailsFields}
   
   /* Add Hidden Fields*/
   InboundStatusGroupLinkDetailsForm:insertHiddenField("inboundstatusgroup_browse_scroll",    "").
   InboundStatusGroupLinkDetailsForm:insertHiddenField("popup_inboundstatusgrouplink_browse", "").
   InboundStatusGroupLinkDetailsForm:insertHiddenField("InboundStatusGroupID",                STRING(intSelectedInboundStatusGroup)).
   InboundStatusGroupLinkDetailsForm:insertHiddenField("InboundStatusGroupLinkID",            STRING(intSelectedInboundStatusGroupLink)).
   InboundStatusGroupLinkDetailsForm:insertHiddenField("InboundStatusID",                        "").
   InboundStatusGroupLinkDetailsForm:insertHiddenField("form_name",                         "inboundstatusgrouplink_details_form").
   InboundStatusGroupLinkDetailsForm:insertHiddenField("prog_name",                         "adInboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i InboundStatusGroupLinkDetailsForm}
   
   /* Create Button Bar */
   InboundStatusGroupLinkDetailsButtons = NEW buttonBar().
   
   InboundStatusGroupLinkDetailsButtons:addButton("inboundstatusgrouplink_details_form_btn_save", 
                                                fTL("Save"), 
                                                "updateInboundStatusGroupLink('inboundstatusgrouplink_details_form');").
   
   InboundStatusGroupLinkDetailsButtons:addButton("inboundstatusgrouplink_details_form_btn_cancel", 
                                                fTL("Cancel"), 
                                                "cancelUpdate('UserCancelled','process_mode'); " 
                                                + "disablePopup('inboundstatusgrouplink_details_form_popup');").
   InboundStatusGroupLinkDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   InboundStatusGroupLinkDetailsForm:FormButtons = InboundStatusGroupLinkDetailsButtons.
   
   InboundStatusGroupLinkDetailsForm:endForm(). 
   InboundStatusGroupLinkDetailsForm:displayForm(). 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pInboundStatusGroupLinkDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundStatusGroupLinkDetailsFields Procedure 
PROCEDURE pInboundStatusGroupLinkDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adInboundStatusGroupAdmin_inboundstatusgrouplink_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

