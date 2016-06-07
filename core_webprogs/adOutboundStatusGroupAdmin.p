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


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedOutboundStatusGroup        AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectOutboundStatusGroupRow       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToOutboundStatusGroupRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOutboundStatusGroupID              AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedOutboundStatusGroupLink    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectOutboundStatusGroupLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToOutboundStatusGroupLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOutboundStatusGroupLinkID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupOutboundStatusGroupLinks      AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE OutboundStatusGroupBrowseFrame        AS pageFrame.
DEFINE VARIABLE OutboundStatusGroupBrowse             AS browseTable.
DEFINE VARIABLE OutboundStatusGroupBrowseButtons      AS buttonBar.
DEFINE VARIABLE OutboundStatusGroupDetailsForm        AS dataForm.
DEFINE VARIABLE OutboundStatusGroupDetailsButtons     AS buttonBar.

DEFINE VARIABLE OutboundStatusGroupLinkBrowseFrame    AS pageFrame.
DEFINE VARIABLE OutboundStatusGroupLinkBrowse         AS browseTable.
DEFINE VARIABLE OutboundStatusGroupLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE OutboundStatusGroupLinkDetailsForm    AS dataForm.
DEFINE VARIABLE OutboundStatusGroupLinkDetailsButtons AS buttonBar.
DEFINE VARIABLE OutboundStatusGroupLinkBrowseForm     AS dataForm.

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

&IF DEFINED(EXCLUDE-pOutboundStatusGroupBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusGroupBrowse Procedure 
PROCEDURE pOutboundStatusGroupBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "outboundstatusgroup_details_form"}
   
   OutboundStatusGroupBrowse              = NEW browseTable("outboundstatusgroup_browse").
   OutboundStatusGroupBrowse:BrowseWidth  = 965.
   OutboundStatusGroupBrowse:BrowseHeight = 455.
   OutboundStatusGroupBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   OutboundStatusGroupBrowse:insertColumn(fTL("Group ID"),           60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i OutboundStatusGroup}
   
   OutboundStatusGroupBrowse:insertColumn(fTL("List Seq"),           60, "INTEGER", FALSE).
   OutboundStatusGroupBrowse:insertColumn(fTL("Group Code"),        150, "CHARACTER", "left", FALSE).
   OutboundStatusGroupBrowse:insertColumn(fTL("Group Name"),        150, "CHARACTER", "left", FALSE).
   OutboundStatusGroupBrowse:insertColumn(fTL("Group Description"), 220, "CHARACTER", "left", FALSE).
   OutboundStatusGroupBrowse:insertColumn(fTL("Active"),             50, "LOGICAL", FALSE).
   
   /*Body*/
   OutboundStatusGroupBrowse:startBody().
   
   /* Find all OutboundStatusGroups then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH OutboundStatusGroup NO-LOCK
      BY    OutboundStatusGroup.Active DESCENDING
      BY    OutboundStatusGroup.ListingSequence 
      BY    OutboundStatusGroup.OutboundStatusGroupID:
      
      OutboundStatusGroupBrowse:startRow(OutboundStatusGroup.OutboundStatusGroupID, "selectOutboundStatusGroupRow(this," + '"' 
                                                                     + STRING(OutboundStatusGroup.OutboundStatusGroupID) + '"' + ");", "").
      OutboundStatusGroupBrowse:insertData(OutboundStatusGroup.OutboundStatusGroupID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i OutboundStatusGroup}
      
      OutboundStatusGroupBrowse:insertData(STRING(OutboundStatusGroup.ListingSequence)).
      OutboundStatusGroupBrowse:insertData(OutboundStatusGroup.GroupCode, "left").
      OutboundStatusGroupBrowse:insertData(OutboundStatusGroup.GroupName, "left").
      OutboundStatusGroupBrowse:insertData(OutboundStatusGroup.GroupDescr, "left").
      OutboundStatusGroupBrowse:insertData(STRING(OutboundStatusGroup.Active, "Yes/No")).
      
      /* Add hidden fields */
      OutboundStatusGroupBrowse:insertHiddenData("OutboundStatusGroupVersionID",OutboundStatusGroup.VersionID).
      
      OutboundStatusGroupBrowse:endRow().
      
   END. /* FOR EACH OutboundStatusGroup NO-LOCK */
   
   OutboundStatusGroupBrowse:endTable().
   chrPageBuildError = chrPageBuildError + OutboundStatusGroupBrowse:getErrors().
   
   /* Create a new frame */
   OutboundStatusGroupBrowseFrame           = NEW pageFrame().
   OutboundStatusGroupBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   OutboundStatusGroupBrowseFrame:FormAction="dbOutboundStatusGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   OutboundStatusGroupBrowseFrame:formOpen("outboundstatusgroup_browse_form").
   
   /* Start the Frame Header */
   OutboundStatusGroupBrowseFrame:insertSpacer(5).
   OutboundStatusGroupBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   OutboundStatusGroupBrowse:displayBrowse().  
   
   /* End the Frame Header */
   OutboundStatusGroupBrowseFrame:frameClose().
   OutboundStatusGroupBrowseFrame:insertSpacer(10).
   
   OutboundStatusGroupBrowseFrame:insertHiddenField("outboundstatusgroup_browse_scroll","").
   OutboundStatusGroupBrowseFrame:insertHiddenField("OutboundStatusGroupID","").
   OutboundStatusGroupBrowseFrame:insertHiddenField("OutboundStatusGroupVersionID","").
   OutboundStatusGroupBrowseFrame:insertHiddenField("outboundstatusgrouplink_browse_scroll","").
   OutboundStatusGroupBrowseFrame:insertHiddenField("popup_outboundstatusgrouplink_browse","").
   OutboundStatusGroupBrowseFrame:insertHiddenField("OutboundStatusGroupLinkID","").
   OutboundStatusGroupBrowseFrame:insertHiddenField("form_name","outboundstatusgroup_browse_form").
   OutboundStatusGroupBrowseFrame:insertHiddenField("prog_name","adOutboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OutboundStatusGroupBrowseFrame}
   
   OutboundStatusGroupBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   OutboundStatusGroupBrowseButtons           = NEW buttonBar().
   OutboundStatusGroupBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   OutboundStatusGroupBrowseButtons:addButton("outboundstatusgroup_browse_form_btn_details",
                                              fTL("Details"),
                                              "viewOutboundStatusGroupDetails('outboundstatusgroup_details_form');",
                                              (IF intSelectedOutboundStatusGroup > 0 THEN "" ELSE "Disabled")).
   
   OutboundStatusGroupBrowseButtons:addButton("outboundstatusgroup_browse_form_btn_outboundstatus",
                                              fTL("OutboundStatus"),
                                              "viewOutboundStatusGroupLinks('outboundstatus_details_form');",
                                              (IF intSelectedOutboundStatusGroup > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
      OutboundStatusGroupBrowseButtons:addButton("outboundstatusgroup_browse_form_btn_create",
                                                 fTL("Create"),
                                                 "createOutboundStatusGroup('outboundstatusgroup_details_form');",
                                                 "").
                                           
      OutboundStatusGroupBrowseButtons:addButton("outboundstatusgroup_browse_form_btn_applinks",
                                                 fTL("App Links"),
                                                 "viewAppLinkBrowse('outboundstatusgroup_browse_form','OutboundStatusGroup');",
                                                 "Disabled").                                                                                      
   END.                                        
   
   OutboundStatusGroupBrowseButtons:closeBar().  
   OutboundStatusGroupBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusGroupDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusGroupDetails Procedure 
PROCEDURE pOutboundStatusGroupDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "outboundstatusgroup_details_form"}
   
   ASSIGN chrDisplayFieldList  = "OutboundStatusGroupID,ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrEditFieldList     = "ListingSequence,GroupName,GroupDescr,Active"
          chrNewFieldList      = "ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrRequiredFieldList = "ListingSequence,GroupCode,GroupName,GroupDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   OutboundStatusGroupDetailsForm           = NEW dataForm("outboundstatusgroup_details_form").
   OutboundStatusGroupDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   OutboundStatusGroupDetailsForm:FormAction = "dbOutboundStatusGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   OutboundStatusGroupDetailsForm:FormWidth  = 460.
   OutboundStatusGroupDetailsForm:FormHeight = 300.
   OutboundStatusGroupDetailsForm:FormTitle  = "OutboundStatusGroup Details".
   OutboundStatusGroupDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   OutboundStatusGroupDetailsForm:insertPaddingColumn(10).
   OutboundStatusGroupDetailsForm:insertColumn(100).
   OutboundStatusGroupDetailsForm:insertColumn(120).
   OutboundStatusGroupDetailsForm:insertColumn(20).
   OutboundStatusGroupDetailsForm:insertColumn(4).
   OutboundStatusGroupDetailsForm:insertColumn(110).
   
   /* Fields */
   OutboundStatusGroupDetailsForm:startRow().
   OutboundStatusGroupDetailsForm:insertLabel("Group ID").
   OutboundStatusGroupDetailsForm:insertTextField("OutboundStatusGroupID", "", 110, TRUE).  
   
   OutboundStatusGroupDetailsForm:startRow().
   OutboundStatusGroupDetailsForm:insertLabel("Listing Seq").
   OutboundStatusGroupDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   OutboundStatusGroupDetailsForm:startRow().
   OutboundStatusGroupDetailsForm:insertLabel("Group Code").
   OutboundStatusGroupDetailsForm:insertTextField("GroupCode", "", 150, TRUE).  
   
   OutboundStatusGroupDetailsForm:startRow().
   OutboundStatusGroupDetailsForm:insertLabel("Group Name").
   OutboundStatusGroupDetailsForm:insertTextField("GroupName", "", 150, TRUE).
   
   OutboundStatusGroupDetailsForm:startRow().
   OutboundStatusGroupDetailsForm:insertLabel("Group Descr").
   OutboundStatusGroupDetailsForm:insertTextField("GroupDescr", "", 300, TRUE).

   OutboundStatusGroupDetailsForm:startRow().
   OutboundStatusGroupDetailsForm:insertLabel(fTL("Active")). 
   OutboundStatusGroupDetailsForm:insertComboField("Active", "", 110, TRUE).  
   OutboundStatusGroupDetailsForm:insertComboPairs("Active", "yes", "Active").
   OutboundStatusGroupDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pOutboundStatusGroupDetailsFields}
   
   /* Add Hidden Fields*/
   OutboundStatusGroupDetailsForm:insertHiddenField("outboundstatusgroup_browse_scroll", "").
   OutboundStatusGroupDetailsForm:insertHiddenField("form_name", "outboundstatusgroup_details_form").
   OutboundStatusGroupDetailsForm:insertHiddenField("prog_name", "adOutboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OutboundStatusGroupDetailsForm}
   
   /* Create Button Bar */
   OutboundStatusGroupDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      OutboundStatusGroupDetailsButtons:addButton("outboundstatusgroup_details_form_btn_save", 
                                                  fTL("Save"), 
                                                  "updateOutboundStatusGroup('outboundstatusgroup_details_form');").
   
   OutboundStatusGroupDetailsButtons:addButton("outboundstatusgroup_details_form_btn_cancel", 
                                               fTL("Cancel"), 
                                               "cancelUpdate('UserCancelled','process_mode'); disablePopup('outboundstatusgroup_details_form_popup');").
   
   OutboundStatusGroupDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   OutboundStatusGroupDetailsForm:FormButtons = OutboundStatusGroupDetailsButtons.
   
   OutboundStatusGroupDetailsForm:endForm(). 
   
   OutboundStatusGroupDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusGroupDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusGroupDetailsFields Procedure 
PROCEDURE pOutboundStatusGroupDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         OutboundStatusGroupDetailsForm:startRow().
         OutboundStatusGroupDetailsForm:insertLabel(fTL("Field Label")).
         OutboundStatusGroupDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adOutboundStatusGroupAdmin_outboundstatusgroup_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusGroupLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusGroupLinkBrowse Procedure 
PROCEDURE pOutboundStatusGroupLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   
   {webGetWebForm.i "outboundstatusgrouplink_details_form"}
   
   OutboundStatusGroupLinkBrowseForm = NEW dataForm("outboundstatusgrouplink_browse_form").
   OutboundStatusGroupLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   OutboundStatusGroupLinkBrowseForm:FormAction  = "dbOutboundStatusGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   OutboundStatusGroupLinkBrowseForm:FormWidth   = 700.
   OutboundStatusGroupLinkBrowseForm:FormHeight  = 490.
   OutboundStatusGroupLinkBrowseForm:FormTitle   = fTL("OutboundStatuses for OutboundStatusGroup") + 
                                               (IF AVAILABLE OutboundStatusGroup THEN " : " + OutboundStatusGroup.GroupName ELSE "").
   OutboundStatusGroupLinkBrowseForm:FormType    = "xl_large".
   
   OutboundStatusGroupLinkBrowse = NEW browseTable("outboundstatusgrouplink_browse").
   OutboundStatusGroupLinkBrowse:BrowseWidth  = 680.
   OutboundStatusGroupLinkBrowse:BrowseHeight = 432.
   
   OutboundStatusGroupLinkBrowse:insertColumn(fTL("LinkID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i OutboundStatusGroupLink}
   
   OutboundStatusGroupLinkBrowse:insertColumn(fTL("OutboundStatus Code"), 140, "CHARACTER", "left", FALSE).
   OutboundStatusGroupLinkBrowse:insertColumn(fTL("OutboundStatus Description"), 160, "CHARACTER", "left", FALSE).
   OutboundStatusGroupLinkBrowse:insertColumn(fTL("Active"), 80, "CHARACTER", FALSE).
   
   OutboundStatusGroupLinkBrowse:StartBody().
   
   IF AVAILABLE OutboundStatusGroup THEN
   DO:
      /* List the OutboundStatusGroupLinks for the OutboundStatusGroup */
      FOR EACH OutboundStatusGroupLink OF OutboundStatusGroup NO-LOCK
         BY OutboundStatusGroupLink.OutboundStatusGroupID:
         
         FIND FIRST OutboundStatus OF OutboundStatusGroupLink NO-LOCK NO-ERROR.

         OutboundStatusGroupLinkBrowse:startRow(OutboundStatusGroupLink.OutboundStatusGroupLinkID, 
                                            "selectOutboundStatusGroupLinkRow(this," + '"' + 
                                            STRING(OutboundStatusGroupLink.OutboundStatusGroupLinkID) + 
                                            '","adOutboundStatusGroupAdmin.p","outboundstatusgrouplink_browse_form"' + ");", "").
         OutboundStatusGroupLinkBrowse:insertData(OutboundStatusGroupLink.OutboundStatusGroupLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i OutboundStatusGroupLink}
         
         OutboundStatusGroupLinkBrowse:insertData(IF AVAILABLE OutboundStatus THEN OutboundStatus.StatusCode ELSE "", "left").
         OutboundStatusGroupLinkBrowse:insertData(IF AVAILABLE OutboundStatus THEN OutboundStatus.StatusDesc ELSE "", "left").
         OutboundStatusGroupLinkBrowse:insertData(STRING(OutboundStatusGroupLink.Active,"Yes/No")).
         
         /* Add hidden fields */
         OutboundStatusGroupLinkBrowse:insertHiddenData("OutboundStatusGroupID",OutboundStatusGroupLink.OutboundStatusGroupID).
         OutboundStatusGroupLinkBrowse:insertHiddenData("OutboundStatusID",(IF AVAILABLE OutboundStatus THEN STRING(OutboundStatus.OutboundStatusID) ELSE "")).
         OutboundStatusGroupLinkBrowse:insertHiddenData("StatusCode",(IF AVAILABLE OutboundStatus THEN OutboundStatus.StatusCode ELSE "")).
         OutboundStatusGroupLinkBrowse:insertHiddenData("OutboundStatusVersionID",(IF AVAILABLE OutboundStatus THEN STRING(OutboundStatus.VersionID) ELSE "")).
         OutboundStatusGroupLinkBrowse:insertHiddenData("OutboundStatusGroupLinkID",OutboundStatusGroupLink.OutboundStatusGroupLinkID).
         OutboundStatusGroupLinkBrowse:insertHiddenData("OutboundStatusGroupLinkVersionID",OutboundStatusGroupLink.VersionID).
         OutboundStatusGroupLinkBrowse:endRow().
      
      END. /* FOR EACH OutboundStatusGroupLink OF OutboundStatusGroup NO-LOCK */
   END. /*IF AVAILABLE OutboundStatusGroup THEN*/
   
   OutboundStatusGroupLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + OutboundStatusGroupLinkBrowse:getErrors().
   
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("OutboundStatusGroupID","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("GroupName","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("OutboundStatusID","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("OutboundStatusVersionID","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("OutboundStatusCode","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("OutboundStatusGroupLinkID","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("OutboundStatusGroupLinkVersionID","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("outboundstatusgroup_browse_scroll","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("outboundstatusgrouplink_browse_scroll","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("popup_outboundstatusgrouplink_browse","").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("form_name","outboundstatusgrouplink_browse_form").
   OutboundStatusGroupLinkBrowseForm:insertHiddenField("prog_name","adOutboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OutboundStatusGroupLinkBrowseForm}
   
   /* Create Button Bar */
   OutboundStatusGroupLinkBrowseButtons = NEW buttonBar().
   
   OutboundStatusGroupLinkBrowseButtons:addButton("outboundstatusgrouplink_browse_form_btn_create",
                                                  fTL("Create"),
                                                  "createOutboundStatusGroupLink('outboundstatusgrouplink_details_form');"). 
   
   OutboundStatusGroupLinkBrowseButtons:addButton("outboundstatusgrouplink_browse_form_btn_view",
                                                  fTL("Details"),
                                                  "viewOutboundStatusGroupLinkDetails('outboundstatusgrouplink_details_form');",
                                                  (IF intSelectedOutboundStatusGroupLink > 0 THEN "" ELSE "Disabled")).
   
   OutboundStatusGroupLinkBrowseButtons:addButton("outboundstatusgrouplink_browse_form_btn_delete",
                                                  fTL("Delete"),
                                                  "confirmDeleteOutboundStatusGroupLink('outboundstatusgrouplink_browse_form');", 
                                                  (IF intSelectedOutboundStatusGroupLink > 0 THEN "" ELSE "Disabled")).
   
   OutboundStatusGroupLinkBrowseButtons:addButton("outboundstatusgrouplink_browse_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('outboundstatusgrouplink_browse_form_popup');").
   
   OutboundStatusGroupLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   OutboundStatusGroupLinkBrowseForm:FormBrowse  = OutboundStatusGroupLinkBrowse.
   OutboundStatusGroupLinkBrowseForm:FormButtons = OutboundStatusGroupLinkBrowseButtons.
   OutboundStatusGroupLinkBrowseForm:endForm(). 
   
   OutboundStatusGroupLinkBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusGroupLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusGroupLinkDetails Procedure 
PROCEDURE pOutboundStatusGroupLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "outboundstatusgrouplink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "OutboundStatusGroupLinkID,OutboundStatusID,Active"
          chrEditFieldList     = "Active"
          chrNewFieldList      = "OutboundStatusID,Active"
          chrRequiredFieldList = "OutboundStatusID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   OutboundStatusGroupLinkDetailsForm = NEW dataForm("outboundstatusgrouplink_details_form").
   OutboundStatusGroupLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   OutboundStatusGroupLinkDetailsForm:FormAction  = "dbOutboundStatusGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   OutboundStatusGroupLinkDetailsForm:FormWidth   = 460.
   OutboundStatusGroupLinkDetailsForm:FormHeight  = 300.
   OutboundStatusGroupLinkDetailsForm:FormTitle   = "Outbound Status Group Link Details".
   OutboundStatusGroupLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   OutboundStatusGroupLinkDetailsForm:insertPaddingColumn(70).
   OutboundStatusGroupLinkDetailsForm:insertColumn(120).
   OutboundStatusGroupLinkDetailsForm:insertColumn(120).
   OutboundStatusGroupLinkDetailsForm:insertColumn(30).
   OutboundStatusGroupLinkDetailsForm:insertColumn(120).  
   
   /* Fields */
   OutboundStatusGroupLinkDetailsForm:startRow().
   OutboundStatusGroupLinkDetailsForm:insertLabel(fTL("Link ID")).
   OutboundStatusGroupLinkDetailsForm:insertTextField("OutboundStatusGroupLinkID", "", 110, TRUE).    
   
   OutboundStatusGroupLinkDetailsForm:startRow().
   OutboundStatusGroupLinkDetailsForm:insertLabel(fTL("Outbound Status")).
   OutboundStatusGroupLinkDetailsForm:insertComboField("OutboundStatusID", "", 200, TRUE). 
   FOR EACH OutboundStatus NO-LOCK /*idx=ActiveListingSequence*/
      BY OutboundStatus.Active DESC
      BY OutboundStatus.StatusCode:
      
      OutboundStatusGroupLinkDetailsForm:insertComboPairs("OutboundStatusID", STRING(OutboundStatus.OutboundStatusID), OutboundStatus.StatusCode).
   END.   
   
   OutboundStatusGroupLinkDetailsForm:startRow().
   OutboundStatusGroupLinkDetailsForm:insertLabel(fTL("Active")). 
   OutboundStatusGroupLinkDetailsForm:insertComboField("Active", "", 110, TRUE).  
   OutboundStatusGroupLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   OutboundStatusGroupLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pOutboundStatusGroupLinkDetailsFields}
   
   /* Add Hidden Fields*/
   OutboundStatusGroupLinkDetailsForm:insertHiddenField("outboundstatusgroup_browse_scroll",    "").
   OutboundStatusGroupLinkDetailsForm:insertHiddenField("popup_outboundstatusgrouplink_browse", "").
   OutboundStatusGroupLinkDetailsForm:insertHiddenField("OutboundStatusGroupID",                STRING(intSelectedOutboundStatusGroup)).
   OutboundStatusGroupLinkDetailsForm:insertHiddenField("OutboundStatusGroupLinkID",            STRING(intSelectedOutboundStatusGroupLink)).
   OutboundStatusGroupLinkDetailsForm:insertHiddenField("OutboundStatusID",                        "").
   OutboundStatusGroupLinkDetailsForm:insertHiddenField("form_name",                         "outboundstatusgrouplink_details_form").
   OutboundStatusGroupLinkDetailsForm:insertHiddenField("prog_name",                         "adOutboundStatusGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OutboundStatusGroupLinkDetailsForm}
   
   /* Create Button Bar */
   OutboundStatusGroupLinkDetailsButtons = NEW buttonBar().
   
   OutboundStatusGroupLinkDetailsButtons:addButton("outboundstatusgrouplink_details_form_btn_save", 
                                                fTL("Save"), 
                                                "updateOutboundStatusGroupLink('outboundstatusgrouplink_details_form');").
   
   OutboundStatusGroupLinkDetailsButtons:addButton("outboundstatusgrouplink_details_form_btn_cancel", 
                                                fTL("Cancel"), 
                                                "cancelUpdate('UserCancelled','process_mode'); " 
                                                + "disablePopup('outboundstatusgrouplink_details_form_popup');").
   OutboundStatusGroupLinkDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   OutboundStatusGroupLinkDetailsForm:FormButtons = OutboundStatusGroupLinkDetailsButtons.
   
   OutboundStatusGroupLinkDetailsForm:endForm(). 
   OutboundStatusGroupLinkDetailsForm:displayForm(). 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOutboundStatusGroupLinkDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOutboundStatusGroupLinkDetailsFields Procedure 
PROCEDURE pOutboundStatusGroupLinkDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adOutboundStatusGroupAdmin_outboundstatusgrouplink_details_form.i}
      
   END CASE. /*chrOption:*/
   
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
   
   ASSIGN chrOutboundStatusGroupID              = get-value("OutboundStatusGroupID")
          intSelectedOutboundStatusGroup        = INTEGER(chrOutboundStatusGroupID)
          chrScrollToOutboundStatusGroupRow     = STRING(INTEGER(get-value("outboundstatusgroup_browse_scroll"))) + ";"
          chrOutboundStatusGroupLinkID          = get-value("OutboundStatusGroupLinkID")
          intSelectedOutboundStatusGroupLink    = INTEGER(chrOutboundStatusGroupLinkID)
          chrScrollToOutboundStatusGroupLinkRow = STRING(INTEGER(get-value("outboundstatusgrouplink_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrOutboundStatusGroupID <> "" THEN
      chrSelectOutboundStatusGroupRow = 'selectOutboundStatusGroupRow(document.getElementById("outboundstatusgroup_browse_row_' + chrOutboundStatusGroupID + '"),"' 
                                                         + chrOutboundStatusGroupID +  '");'.
   
   IF chrOutboundStatusGroupLinkID <> "" THEN
      chrSelectOutboundStatusGroupLinkRow = 'selectOutboundStatusGroupLinkRow(document.getElementById("outboundstatusgrouplink_browse_row_' 
                                           + chrOutboundStatusGroupLinkID + '"),"' + chrOutboundStatusGroupLinkID +  '");'.

   IF get-value('popup_outboundstatusgrouplink_browse') = "yes" THEN 
      chrPopupOutboundStatusGroupLinks = 'enablePopup("outboundstatusgrouplink_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("outboundstatusgroup_browse").scrollTop=' + chrScrollToOutboundStatusGroupRow 
                             + chrSelectOutboundStatusGroupRow + chrSelectOutboundStatusGroupLinkRow + chrPopupOutboundStatusGroupLinks.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "OutboundStatusGroup Admin".
   ThisPage:FrameTitle    = "OutboundStatusGroup Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("outboundstatusgroup.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pOutboundStatusGroupBrowse.
   
   FIND FIRST OutboundStatusGroup NO-LOCK
      WHERE OutboundStatusGroup.OutboundStatusGroupID = intSelectedOutboundStatusGroup NO-ERROR.

   /******* Pop-up Browsers and Forms ********/    
   RUN pOutboundStatusGroupDetails.
   
   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
   
   RUN pOutboundStatusGroupLinkBrowse.

   FIND FIRST OutboundStatusGroupLink NO-LOCK
      WHERE OutboundStatusGroupLink.OutboundStatusGroupLinkID = intSelectedOutboundStatusGroupLink NO-ERROR.

   RUN pOutboundStatusGroupLinkDetails.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT OutboundStatusGroupBrowseFrame        NO-ERROR.
   DELETE OBJECT OutboundStatusGroupBrowse             NO-ERROR.
   DELETE OBJECT OutboundStatusGroupBrowseButtons      NO-ERROR.
   DELETE OBJECT OutboundStatusGroupDetailsForm        NO-ERROR.
   DELETE OBJECT OutboundStatusGroupDetailsButtons     NO-ERROR.
   DELETE OBJECT OutboundStatusGroupLinkBrowseFrame    NO-ERROR.
   DELETE OBJECT OutboundStatusGroupLinkBrowse         NO-ERROR.
   DELETE OBJECT OutboundStatusGroupLinkBrowseButtons  NO-ERROR.
   DELETE OBJECT OutboundStatusGroupLinkDetailsForm    NO-ERROR.
   DELETE OBJECT OutboundStatusGroupLinkDetailsButtons NO-ERROR.
   DELETE OBJECT OutboundStatusGroupLinkBrowseForm     NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

