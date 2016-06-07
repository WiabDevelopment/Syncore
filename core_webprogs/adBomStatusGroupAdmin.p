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
DEFINE VARIABLE chrBomStatusGroupID              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrBomStatusGroupLinkID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupBomStatusGroupLinks      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToBomStatusGroupRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToBomStatusGroupLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectBomStatusGroupRow       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectBomStatusGroupLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedBomStatusGroup        AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedBomStatusGroupLink    AS INTEGER   NO-UNDO.

/* Objects */
DEFINE VARIABLE BomStatusGroupBrowseFrame        AS pageFrame.
DEFINE VARIABLE BomStatusGroupBrowse             AS browseTable.
DEFINE VARIABLE BomStatusGroupBrowseButtons      AS buttonBar.
DEFINE VARIABLE BomStatusGroupDetailsForm        AS dataForm.
DEFINE VARIABLE BomStatusGroupDetailsButtons     AS buttonBar.

DEFINE VARIABLE BomStatusGroupLinkBrowseForm     AS dataForm.
DEFINE VARIABLE BomStatusGroupLinkBrowse         AS browseTable.
DEFINE VARIABLE BomStatusGroupLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE BomStatusGroupLinkDetailsForm    AS dataForm.
DEFINE VARIABLE BomStatusGroupLinkDetailsButtons AS buttonBar.


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

&IF DEFINED(EXCLUDE-pBomStatusGroupBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusGroupBrowse Procedure 
PROCEDURE pBomStatusGroupBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "bomstatusgroup_details_form"}
   
   BomStatusGroupBrowse              = NEW browseTable("bomstatusgroup_browse").
   BomStatusGroupBrowse:BrowseWidth  = 965.
   BomStatusGroupBrowse:BrowseHeight = 455.
   BomStatusGroupBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   BomStatusGroupBrowse:insertColumn(fTL("Group ID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i BomStatusGroup}
   
   BomStatusGroupBrowse:insertColumn(fTL("List Seq"),           60, "INTEGER", FALSE).
   BomStatusGroupBrowse:insertColumn(fTL("Group Code"),        150, "CHARACTER", "left", FALSE).
   BomStatusGroupBrowse:insertColumn(fTL("Group Name"),        150, "CHARACTER", "left", FALSE).
   BomStatusGroupBrowse:insertColumn(fTL("Group Description"), 220, "CHARACTER", "left", FALSE).
   BomStatusGroupBrowse:insertColumn(fTL("Active"),             50, "LOGICAL", FALSE).
   
   /*Body*/
   BomStatusGroupBrowse:startBody().
   
   /* Find all BomStatusGroups then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH BomStatusGroup NO-LOCK
      BY    BomStatusGroup.Active DESCENDING
      BY    BomStatusGroup.ListingSequence 
      BY    BomStatusGroup.BomStatusGroupID:
      
      BomStatusGroupBrowse:startRow(BomStatusGroup.BomStatusGroupID,
                                   "selectBomStatusGroupRow(this," + '"' + STRING(BomStatusGroup.BomStatusGroupID) + '"' + ");", 
                                   "").
      BomStatusGroupBrowse:insertData(BomStatusGroup.BomStatusGroupID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i BomStatusGroup}
      
      BomStatusGroupBrowse:insertData(STRING(BomStatusGroup.ListingSequence)).
      BomStatusGroupBrowse:insertData(BomStatusGroup.GroupCode, "left").
      BomStatusGroupBrowse:insertData(BomStatusGroup.GroupName, "left").
      BomStatusGroupBrowse:insertData(BomStatusGroup.GroupDescr, "left").
      BomStatusGroupBrowse:insertData(STRING(BomStatusGroup.Active, "Yes/No")).
      
      /* Add hidden fields */
      BomStatusGroupBrowse:insertHiddenData("BomStatusGroupVersionID",BomStatusGroup.VersionID).
      
      BomStatusGroupBrowse:endRow().
      
   END. /* FOR EACH BomStatusGroup NO-LOCK */
   
   BomStatusGroupBrowse:endTable().
   chrPageBuildError = chrPageBuildError + BomStatusGroupBrowse:getErrors().
   
   /* Create a new frame */
   BomStatusGroupBrowseFrame           = NEW pageFrame().
   BomStatusGroupBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   BomStatusGroupBrowseFrame:FormAction = "dbBomStatusGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   BomStatusGroupBrowseFrame:formOpen("bomstatusgroup_browse_form").
   
   /* Start the Frame Header */
   BomStatusGroupBrowseFrame:insertSpacer(5).
   BomStatusGroupBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   BomStatusGroupBrowse:displayBrowse().  
   
   /* End the Frame Header */
   BomStatusGroupBrowseFrame:frameClose().
   BomStatusGroupBrowseFrame:insertSpacer(10).

   BomStatusGroupBrowseFrame:insertHiddenField("form_name", "bomstatusgroup_browse_form").
   BomStatusGroupBrowseFrame:insertHiddenField("prog_name", "adBomStatusGroupAdmin.p").   
   BomStatusGroupBrowseFrame:insertHiddenField("bomstatusgroup_browse_scroll", "").
   BomStatusGroupBrowseFrame:insertHiddenField("bomstatusgrouplink_browse_scroll", "").
   BomStatusGroupBrowseFrame:insertHiddenField("popup_bomstatusgrouplink_browse", "").
   BomStatusGroupBrowseFrame:insertHiddenField("BomStatusGroupID", "").
   BomStatusGroupBrowseFrame:insertHiddenField("BomStatusGroupVersionID", "").
   BomStatusGroupBrowseFrame:insertHiddenField("BomStatusGroupLinkID", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomStatusGroupBrowseFrame}
   
   BomStatusGroupBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   BomStatusGroupBrowseButtons           = NEW buttonBar().
   BomStatusGroupBrowseButtons:WebStream = STREAM WebStream:HANDLE.
     
   BomStatusGroupBrowseButtons:addButton("bomstatusgroup_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewBomStatusGroupDetails('bomstatusgroup_details_form');",
                                         "Disabled").
   
   BomStatusGroupBrowseButtons:addButton("bomstatusgroup_browse_form_btn_bomstatus",
                                         fTL("Bom Status"),
                                         "viewBomStatusGroupLinks('bomstatus_details_form');",
                                         "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO:
      BomStatusGroupBrowseButtons:addButton("bomstatusgroup_browse_form_btn_create",
                                            fTL("Create"),
                                            "createBomStatusGroup('bomstatusgroup_details_form');").
                                           
      BomStatusGroupBrowseButtons:addButton("bomstatusgroup_browse_form_btn_applinks",
                                            fTL("App Links"),
                                            "viewAppLinkBrowse('bomstatusgroup_browse_form','BomStatusGroup');",
                                            "Disabled").                                                                                      
   END.                                        
   
   BomStatusGroupBrowseButtons:closeBar().  
   BomStatusGroupBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusGroupDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusGroupDetails Procedure 
PROCEDURE pBomStatusGroupDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "bomstatusgroup_details_form"}
   
   ASSIGN chrDisplayFieldList  = "BomStatusGroupID,ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrEditFieldList     = "ListingSequence,GroupName,GroupDescr,Active"
          chrNewFieldList      = "ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrRequiredFieldList = "ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   BomStatusGroupDetailsForm           = NEW dataForm("bomstatusgroup_details_form").
   BomStatusGroupDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   BomStatusGroupDetailsForm:FormAction = "dbBomStatusGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BomStatusGroupDetailsForm:FormWidth  = 460.
   BomStatusGroupDetailsForm:FormHeight = 300.
   BomStatusGroupDetailsForm:FormTitle  = "Bom Status Group Details".
   BomStatusGroupDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   BomStatusGroupDetailsForm:insertPaddingColumn(10).
   BomStatusGroupDetailsForm:insertColumn(100).
   BomStatusGroupDetailsForm:insertColumn(120).
   BomStatusGroupDetailsForm:insertColumn(20).
   BomStatusGroupDetailsForm:insertColumn(4).
   BomStatusGroupDetailsForm:insertColumn(110).
   
   /* Fields */
   BomStatusGroupDetailsForm:startRow().
   BomStatusGroupDetailsForm:insertLabel("Group ID").
   BomStatusGroupDetailsForm:insertTextField("BomStatusGroupID", "", 110, TRUE).  
   
   BomStatusGroupDetailsForm:startRow().
   BomStatusGroupDetailsForm:insertLabel("Listing Seq").
   BomStatusGroupDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   BomStatusGroupDetailsForm:startRow().
   BomStatusGroupDetailsForm:insertLabel("Group Code").
   BomStatusGroupDetailsForm:insertTextField("GroupCode", "", 150, TRUE).  
   
   BomStatusGroupDetailsForm:startRow().
   BomStatusGroupDetailsForm:insertLabel("Group Name").
   BomStatusGroupDetailsForm:insertTextField("GroupName", "", 150, TRUE).
   
   BomStatusGroupDetailsForm:startRow().
   BomStatusGroupDetailsForm:insertLabel("Group Descr").
   BomStatusGroupDetailsForm:insertTextField("GroupDescr", "", 300, TRUE).

   BomStatusGroupDetailsForm:startRow().
   BomStatusGroupDetailsForm:insertLabel(fTL("Active")). 
   BomStatusGroupDetailsForm:insertComboField("Active", "", 110, TRUE).  
   BomStatusGroupDetailsForm:insertComboPairs("Active", "yes", "Active").
   BomStatusGroupDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pBomStatusGroupDetailsFields}
   
   /* Add Hidden Fields*/
   BomStatusGroupDetailsForm:insertHiddenField("form_name", "bomstatusgroup_details_form").
   BomStatusGroupDetailsForm:insertHiddenField("prog_name", "adBomStatusGroupAdmin.p").
   BomStatusGroupDetailsForm:insertHiddenField("bomstatusgroup_browse_scroll", "").   
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomStatusGroupDetailsForm}
   
   /* Create Button Bar */
   BomStatusGroupDetailsButtons = NEW buttonBar().  
   
   IF NOT logPreventDataUpdates THEN
      BomStatusGroupDetailsButtons:addButton("bomstatusgroup_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateBomStatusGroup('bomstatusgroup_details_form');").
   
   BomStatusGroupDetailsButtons:addButton("bomstatusgroup_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode');" 
                                             + "disablePopup('bomstatusgroup_details_form_popup');").
   
   BomStatusGroupDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   BomStatusGroupDetailsForm:FormButtons = BomStatusGroupDetailsButtons.
   
   BomStatusGroupDetailsForm:endForm(). 
   
   BomStatusGroupDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusGroupDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusGroupDetailsFields Procedure 
PROCEDURE pBomStatusGroupDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         BomStatusGroupDetailsForm:startRow().
         BomStatusGroupDetailsForm:insertLabel(fTL("Field Label")).
         BomStatusGroupDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adBomStatusGroupAdmin_bomstatusgroup_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusGroupLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusGroupLinkBrowse Procedure 
PROCEDURE pBomStatusGroupLinkBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   
   {webGetWebForm.i "bomstatusgrouplink_details_form"}
   
   BomStatusGroupLinkBrowseForm = NEW dataForm("bomstatusgrouplink_browse_form").
   BomStatusGroupLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   BomStatusGroupLinkBrowseForm:FormAction  = "dbBomStatusGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BomStatusGroupLinkBrowseForm:FormWidth   = 700.
   BomStatusGroupLinkBrowseForm:FormHeight  = 490.
   BomStatusGroupLinkBrowseForm:FormTitle   = fTL("Bom Statuses for Bom Status Group") 
                                                 + (IF AVAILABLE BomStatusGroup THEN " : " + BomStatusGroup.GroupName ELSE "").
   BomStatusGroupLinkBrowseForm:FormType    = "xl_large".
   
   BomStatusGroupLinkBrowse = NEW browseTable("bomstatusgrouplink_browse").
   BomStatusGroupLinkBrowse:BrowseWidth  = 680.
   BomStatusGroupLinkBrowse:BrowseHeight = 432.
   
   BomStatusGroupLinkBrowse:insertColumn(fTL("LinkID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i BomStatusGroupLink}
   
   BomStatusGroupLinkBrowse:insertColumn(fTL("Bom Status Code"),        140, "CHARACTER", "left", FALSE).
   BomStatusGroupLinkBrowse:insertColumn(fTL("Bom Status Description"), 160, "CHARACTER", "left", FALSE).
   BomStatusGroupLinkBrowse:insertColumn(fTL("Active"),                  80, "CHARACTER", FALSE).
   
   BomStatusGroupLinkBrowse:StartBody().
   
   IF AVAILABLE BomStatusGroup THEN
   DO:
      /* List the BomStatusGroupLinks for the BomStatusGroup */
      FOR EACH BomStatusGroupLink OF BomStatusGroup NO-LOCK
         BY BomStatusGroupLink.BomStatusGroupID:
         
         FIND FIRST BomStatus OF BomStatusGroupLink NO-LOCK NO-ERROR.

         BomStatusGroupLinkBrowse:startRow(BomStatusGroupLink.BomStatusGroupLinkID,
                                           'selectBomStatusGroupLinkRow(this, "' + STRING(BomStatusGroupLink.BomStatusGroupLinkID) + '");',
                                           "").
         BomStatusGroupLinkBrowse:insertData(BomStatusGroupLink.BomStatusGroupLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i BomStatusGroupLink}
         
         BomStatusGroupLinkBrowse:insertData(IF AVAILABLE BomStatus THEN BomStatus.StatusCode ELSE "", "left").
         BomStatusGroupLinkBrowse:insertData(IF AVAILABLE BomStatus THEN BomStatus.StatusDesc ELSE "", "left").
         BomStatusGroupLinkBrowse:insertData(STRING(BomStatusGroupLink.Active, "Yes/No")).
         
         /* Add hidden fields */
         BomStatusGroupLinkBrowse:insertHiddenData("BomStatusGroupID", BomStatusGroupLink.BomStatusGroupID).
         BomStatusGroupLinkBrowse:insertHiddenData("BomStatusID", (IF AVAILABLE BomStatus THEN STRING(BomStatus.BomStatusID) ELSE "")).
         BomStatusGroupLinkBrowse:insertHiddenData("StatusCode", (IF AVAILABLE BomStatus THEN BomStatus.StatusCode ELSE "")).
         BomStatusGroupLinkBrowse:insertHiddenData("BomStatusVersionID", (IF AVAILABLE BomStatus THEN STRING(BomStatus.VersionID) ELSE "")).
         BomStatusGroupLinkBrowse:insertHiddenData("BomStatusGroupLinkID", BomStatusGroupLink.BomStatusGroupLinkID).
         BomStatusGroupLinkBrowse:insertHiddenData("BomStatusGroupLinkVersionID", BomStatusGroupLink.VersionID).
         BomStatusGroupLinkBrowse:endRow().
      
      END. /* FOR EACH BomStatusGroupLink OF BomStatusGroup NO-LOCK */
   END. /*IF AVAILABLE BomStatusGroup THEN*/
   
   BomStatusGroupLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + BomStatusGroupLinkBrowse:getErrors().
   
   BomStatusGroupLinkBrowseForm:insertHiddenField("form_name", "bomstatusgrouplink_browse_form").
   BomStatusGroupLinkBrowseForm:insertHiddenField("prog_name", "adBomStatusGroupAdmin.p").
   BomStatusGroupLinkBrowseForm:insertHiddenField("popup_bomstatusgrouplink_browse", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("bomstatusgroup_browse_scroll", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("bomstatusgrouplink_browse_scroll", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("BomStatusGroupID", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("GroupName", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("BomStatusID", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("BomStatusVersionID", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("BomStatusCode", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("BomStatusGroupLinkID", "").
   BomStatusGroupLinkBrowseForm:insertHiddenField("BomStatusGroupLinkVersionID", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomStatusGroupLinkBrowseForm}
   
   /* Create Button Bar */
   BomStatusGroupLinkBrowseButtons = NEW buttonBar().
   
   BomStatusGroupLinkBrowseButtons:addButton("bomstatusgrouplink_browse_form_btn_create",
                                             fTL("Create"),
                                             "createBomStatusGroupLink('bomstatusgrouplink_details_form');"). 
   
   BomStatusGroupLinkBrowseButtons:addButton("bomstatusgrouplink_browse_form_btn_view",
                                             fTL("Details"),
                                             "viewBomStatusGroupLinkDetails('bomstatusgrouplink_details_form');",
                                             "Disabled").
   
   BomStatusGroupLinkBrowseButtons:addButton("bomstatusgrouplink_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteBomStatusGroupLink('bomstatusgrouplink_browse_form');", 
                                             "Disabled").
   
   BomStatusGroupLinkBrowseButtons:addButton("bomstatusgrouplink_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('bomstatusgrouplink_browse_form_popup');").
   
   BomStatusGroupLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   BomStatusGroupLinkBrowseForm:FormBrowse  = BomStatusGroupLinkBrowse.
   BomStatusGroupLinkBrowseForm:FormButtons = BomStatusGroupLinkBrowseButtons.
   BomStatusGroupLinkBrowseForm:endForm(). 
   
   BomStatusGroupLinkBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusGroupLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusGroupLinkDetails Procedure 
PROCEDURE pBomStatusGroupLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "bomstatusgrouplink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "BomStatusGroupLinkID,BomStatusID,Active"
          chrEditFieldList     = "Active"
          chrNewFieldList      = "BomStatusID,Active"
          chrRequiredFieldList = "BomStatusID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   BomStatusGroupLinkDetailsForm = NEW dataForm("bomstatusgrouplink_details_form").
   BomStatusGroupLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   BomStatusGroupLinkDetailsForm:FormAction  = "dbBomStatusGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BomStatusGroupLinkDetailsForm:FormWidth   = 460.
   BomStatusGroupLinkDetailsForm:FormHeight  = 300.
   BomStatusGroupLinkDetailsForm:FormTitle   = "Bom Status Group Link Details".
   BomStatusGroupLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   BomStatusGroupLinkDetailsForm:insertPaddingColumn(70).
   BomStatusGroupLinkDetailsForm:insertColumn(120).
   BomStatusGroupLinkDetailsForm:insertColumn(120).
   BomStatusGroupLinkDetailsForm:insertColumn(30).
   BomStatusGroupLinkDetailsForm:insertColumn(120).  
   
   /* Fields */
   BomStatusGroupLinkDetailsForm:startRow().
   BomStatusGroupLinkDetailsForm:insertLabel(fTL("Link ID")).
   BomStatusGroupLinkDetailsForm:insertTextField("BomStatusGroupLinkID", "", 110, TRUE).    
   
   BomStatusGroupLinkDetailsForm:startRow().
   BomStatusGroupLinkDetailsForm:insertLabel(fTL("Bom Status")).
   BomStatusGroupLinkDetailsForm:insertComboField("BomStatusID", "", 200, TRUE). 
   FOR EACH BomStatus NO-LOCK /*idx=ActiveListingSequence*/
      BY BomStatus.Active DESC
      BY BomStatus.StatusCode:
      
      BomStatusGroupLinkDetailsForm:insertComboPairs("BomStatusID", 
                                                     STRING(BomStatus.BomStatusID), 
                                                     BomStatus.StatusCode).
   END.   
   
   BomStatusGroupLinkDetailsForm:startRow().
   BomStatusGroupLinkDetailsForm:insertLabel(fTL("Active")). 
   BomStatusGroupLinkDetailsForm:insertComboField("Active", "", 110, TRUE).  
   BomStatusGroupLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   BomStatusGroupLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pBomStatusGroupLinkDetailsFields}
   
   /* Add Hidden Fields*/
   BomStatusGroupLinkDetailsForm:insertHiddenField("form_name", "bomstatusgrouplink_details_form").
   BomStatusGroupLinkDetailsForm:insertHiddenField("prog_name", "adBomStatusGroupAdmin.p").
   BomStatusGroupLinkDetailsForm:insertHiddenField("bomstatusgroup_browse_scroll", "").
   BomStatusGroupLinkDetailsForm:insertHiddenField("popup_bomstatusgrouplink_browse", "").
   BomStatusGroupLinkDetailsForm:insertHiddenField("BomStatusGroupID", chrBomStatusGroupID).
   BomStatusGroupLinkDetailsForm:insertHiddenField("BomStatusGroupLinkID", chrBomStatusGroupLinkID).
   BomStatusGroupLinkDetailsForm:insertHiddenField("BomStatusID", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomStatusGroupLinkDetailsForm}
   
   /* Create Button Bar */
   BomStatusGroupLinkDetailsButtons = NEW buttonBar().
   
   BomStatusGroupLinkDetailsButtons:addButton("bomstatusgrouplink_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateBomStatusGroupLink('bomstatusgrouplink_details_form');").
   
   BomStatusGroupLinkDetailsButtons:addButton("bomstatusgrouplink_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); " 
                                                + "disablePopup('bomstatusgrouplink_details_form_popup');").
   BomStatusGroupLinkDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   BomStatusGroupLinkDetailsForm:FormButtons = BomStatusGroupLinkDetailsButtons.
   
   BomStatusGroupLinkDetailsForm:endForm(). 
   BomStatusGroupLinkDetailsForm:displayForm(). 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomStatusGroupLinkDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomStatusGroupLinkDetailsFields Procedure 
PROCEDURE pBomStatusGroupLinkDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adBomStatusGroupAdmin_bomstatusgrouplink_details_form.i}
      
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
   
   ASSIGN chrBomStatusGroupID              = get-value("BomStatusGroupID")
          chrBomStatusGroupLinkID          = get-value("BomStatusGroupLinkID")
          intSelectedBomStatusGroup        = INTEGER(chrBomStatusGroupID)
          intSelectedBomStatusGroupLink    = INTEGER(chrBomStatusGroupLinkID)
          chrScrollToBomStatusGroupRow     = STRING(INTEGER(get-value("bomstatusgroup_browse_scroll"))) + ";"                    
          chrScrollToBomStatusGroupLinkRow = STRING(INTEGER(get-value("bomstatusgrouplink_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrBomStatusGroupID <> "" THEN
      chrSelectBomStatusGroupRow = 'selectBomStatusGroupRow(document.getElementById("bomstatusgroup_browse_row_'
                                      + chrBomStatusGroupID + '"),"' + chrBomStatusGroupID + '");'.
   
   IF chrBomStatusGroupLinkID <> "" THEN
      chrSelectBomStatusGroupLinkRow = 'selectBomStatusGroupLinkRow(document.getElementById("bomstatusgrouplink_browse_row_' 
                                          + chrBomStatusGroupLinkID + '"),"' + chrBomStatusGroupLinkID +  '");'.

   IF get-value('popup_bomstatusgrouplink_browse') = "yes" THEN 
      chrPopupBomStatusGroupLinks = 'enablePopup("bomstatusgrouplink_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                    + 'document.getElementById("bomstatusgroup_browse").scrollTop=' + chrScrollToBomStatusGroupRow 
                    + chrSelectBomStatusGroupRow + chrSelectBomStatusGroupLinkRow + chrPopupBomStatusGroupLinks.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Bom Status Group Admin".
   ThisPage:FrameTitle    = "Bom Status Group Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("bomstatusgroup.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pBomStatusGroupBrowse.
   
   FIND FIRST BomStatusGroup NO-LOCK
      WHERE BomStatusGroup.BomStatusGroupID = intSelectedBomStatusGroup NO-ERROR.

   /******* Pop-up Browsers and Forms ********/    
   RUN pBomStatusGroupDetails.
   
   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
   
   RUN pBomStatusGroupLinkBrowse.

   FIND FIRST BomStatusGroupLink NO-LOCK
      WHERE BomStatusGroupLink.BomStatusGroupLinkID = intSelectedBomStatusGroupLink NO-ERROR.

   RUN pBomStatusGroupLinkDetails.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT BomStatusGroupBrowseFrame        NO-ERROR.
   DELETE OBJECT BomStatusGroupBrowse             NO-ERROR.
   DELETE OBJECT BomStatusGroupBrowseButtons      NO-ERROR.
   DELETE OBJECT BomStatusGroupDetailsForm        NO-ERROR.
   DELETE OBJECT BomStatusGroupDetailsButtons     NO-ERROR.

   DELETE OBJECT BomStatusGroupLinkBrowseForm     NO-ERROR.   
   DELETE OBJECT BomStatusGroupLinkBrowse         NO-ERROR.
   DELETE OBJECT BomStatusGroupLinkBrowseButtons  NO-ERROR.
   DELETE OBJECT BomStatusGroupLinkDetailsForm    NO-ERROR.
   DELETE OBJECT BomStatusGroupLinkDetailsButtons NO-ERROR.
  
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

