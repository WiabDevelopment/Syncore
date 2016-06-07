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
{defDataMigrationVariables.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedEmailGroup                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedEmailGroupUserLink         AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectEmailGroupRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToEmailGroupRow              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupEmailGroupUserLinks           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectEmailGroupUserLinkRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToEmailGroupUserLinkRow      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrEmailGroupID                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrEmailGroupUserLinkID               AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE EmailGroupBrowseFrame                 AS pageFrame.
DEFINE VARIABLE EmailGroupBrowse                      AS browseTable.
DEFINE VARIABLE EmailGroupBrowseButtons               AS buttonBar.
DEFINE VARIABLE EmailGroupDetailsForm                 AS dataForm.
DEFINE VARIABLE EmailGroupDetailsButtons              AS buttonBar.
DEFINE VARIABLE EmailGroupUserLinkBrowseForm          AS dataForm.
DEFINE VARIABLE EmailGroupUserLinkBrowse              AS browseTable.
DEFINE VARIABLE EmailGroupUserLinkBrowseButtons       AS buttonBar.
DEFINE VARIABLE EmailGroupUserLinkDetailsForm         AS dataForm.
DEFINE VARIABLE EmailGroupUserLinkDetailsButtons      AS buttonBar.

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
         HEIGHT             = 18.38
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

&IF DEFINED(EXCLUDE-pEmailGroupBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEmailGroupBrowse Procedure 
PROCEDURE pEmailGroupBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "emailgroup_details_form"}
   
   EmailGroupBrowse = NEW browseTable("emailgroup_browse").
   EmailGroupBrowse:BrowseWidth  = 965.                                        
   EmailGroupBrowse:BrowseHeight = 455.
   EmailGroupBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   EmailGroupBrowse:insertColumn(fTL("GroupID"), 85, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i EmailGroup}
   
   EmailGroupBrowse:insertColumn(fTL("Listing"), 60, "INTEGER", FALSE).
   EmailGroupBrowse:insertColumn(fTL("GroupCode"), 190, "CHARACTER", "left", FALSE).
   EmailGroupBrowse:insertColumn(fTL("GroupName"), 190, "CHARACTER", "left", FALSE).
   EmailGroupBrowse:insertColumn(fTL("GroupDescr"), 250, "CHARACTER", "left", FALSE).
   EmailGroupBrowse:insertColumn(fTL("Type"), 90, "CHARACTER", FALSE).
   EmailGroupBrowse:insertColumn(fTL("Active"), 70, "LOGICAL", FALSE).
   
   /*Body*/
   EmailGroupBrowse:startBody().
   
   EmailGroupLoop:
   FOR EACH EmailGroup NO-LOCK /*idx = ActiveListingSequence*/
      BY    EmailGroup.ListingSequence
      BY    EmailGroup.Active:
         
      IF NOT fCanViewBusinessUnit(intGblSessionID,EmailGroup.BusinessUnitID) THEN 
        NEXT EmailGroupLoop.
      
      EmailGroupBrowse:startRow(EmailGroup.EmailGroupID, "selectEmailGroupRow(this," + '"' + STRING(EmailGroup.EmailGroupID) + '"' + ");", "").
      EmailGroupBrowse:insertData(EmailGroup.EmailGroupID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i EmailGroup}
      
      EmailGroupBrowse:insertData(STRING(EmailGroup.ListingSequence)).
      EmailGroupBrowse:insertData(STRING(EmailGroup.GroupCode), "left").
      EmailGroupBrowse:insertData(STRING(EmailGroup.GroupName), "left").
      EmailGroupBrowse:insertData(STRING(EmailGroup.GroupDescr), "left").
      /*EmailGroupBrowse:insertData(fDisplayDate&Time(EmailGroup.Created,"d/m/y H:M")).*/
      EmailGroupBrowse:insertData(EmailGroup.GroupType).
      EmailGroupBrowse:insertData(STRING(EmailGroup.Active,"Yes/No")).
      
      /* Add hidden fields */
      EmailGroupBrowse:insertHiddenData("EmailGroupVersionID",EmailGroup.VersionID).
      
      EmailGroupBrowse:endRow().
      
   END. /*FOR EACH EmailGroup NO-LOCK */
   
   EmailGroupBrowse:endTable().
   chrPageBuildError = chrPageBuildError + EmailGroupBrowse:getErrors().
   
   /* Create a new frame */
   EmailGroupBrowseFrame = NEW pageFrame().
   EmailGroupBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   EmailGroupBrowseFrame:formOpen("emailgroup_browse_form").
   
   /* Start the Frame Header */
   EmailGroupBrowseFrame:insertSpacer(5).
   EmailGroupBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   EmailGroupBrowse:displayBrowse().  
   
   /* End the Frame Header */
   EmailGroupBrowseFrame:frameClose().
   EmailGroupBrowseFrame:insertSpacer(10).
   
   EmailGroupBrowseFrame:insertHiddenField("emailgroup_browse_scroll","").
   EmailGroupBrowseFrame:insertHiddenField("popup_emailgroupuserlink_browse","").
   /* These are set with every row selection */
   EmailGroupBrowseFrame:insertHiddenField("EmailGroupID","").
   EmailGroupBrowseFrame:insertHiddenField("EmailGroupVersionID","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i EmailGroupBrowseFrame}
   
   EmailGroupBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   EmailGroupBrowseButtons = NEW buttonBar().
   EmailGroupBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   IF NOT logPreventDataCreates THEN
   DO:
      EmailGroupBrowseButtons:addButton("emailgroup_browse_form_btn_details", 
                                        fTL("Create"), 
                                        "createEmailGroup('emailgroup_details_form');").
                                     
      EmailGroupBrowseButtons:addButton("emailgroup_browse_form_btn_applinks",
                                        fTL("App Links"),
                                        "viewAppLinkBrowse('emailgroup_browse_form','EmailGroup');",
                                        "Disabled").                                                                          
   END.                                  
   
   EmailGroupBrowseButtons:addButton("emailgroup_browse_form_btn_details", 
                                     fTL("View"), 
                                     "viewEmailGroupDetails('emailgroup_details_form');").
   
   EmailGroupBrowseButtons:addButton("emailgroup_browse_form_btn_emailgroupuserlinks",
                                     fTL("Users Linked"),
                                     "viewEmailGroupUserLinkBrowse('adEmailGroupAdmin.p');",
                                     "Disabled").
   
   EmailGroupBrowseButtons:addButton("emailgroup_browse_form_btn_delete",
                                     fTL("Delete"),
                                     "alert('Delete is not done yet!');",
                                     "Disabled").
   
   EmailGroupBrowseButtons:addButton("emailgroup_browse_form_btn_testmail",
                                     fTL("Test Mail"),
                                     "alert('Test Mail is not done yet!');",
                                     "Disabled").
   
   EmailGroupBrowseButtons:closeBar().  
   EmailGroupBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pEmailGroupDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEmailGroupDetails Procedure 
PROCEDURE pEmailGroupDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "emailgroup_details_form"}
   
   ASSIGN chrDisplayFieldList  = "EmailGroupID,ListingSequence,GroupCode,GroupName,GroupDescr,GroupType,Active,BusinessUnitID"
          chrEditFieldList     = "Active,ListingSequence,GroupName,GroupType,GroupDescr,BusinessUnitID"
          chrNewFieldList      = "ListingSequence,GroupCode,GroupName,GroupDescr,GroupType,Active,BusinessUnitID"
          chrRequiredFieldList = "ListingSequence,GroupCode,GroupName,GroupDescr,GroupType"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   EmailGroupDetailsForm = NEW dataForm("emailgroup_details_form").
   EmailGroupDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   EmailGroupDetailsForm:FormAction  = "dbEmailGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   EmailGroupDetailsForm:FormWidth   = 460.
   EmailGroupDetailsForm:FormHeight  = 300.
   EmailGroupDetailsForm:FormTitle   = "Email Group Details".
   EmailGroupDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   EmailGroupDetailsForm:insertPaddingColumn(10).
   EmailGroupDetailsForm:insertColumn(90).
   EmailGroupDetailsForm:insertColumn(220).
   EmailGroupDetailsForm:insertColumn(20).
   EmailGroupDetailsForm:insertColumn(4).
   EmailGroupDetailsForm:insertColumn(80).
   
   /* Fields */
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("EmailGroup ID").
   EmailGroupDetailsForm:insertTextField("EmailGroupID", "", 110, TRUE).  
   
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("Listing Seq").
   EmailGroupDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("Group Code").
   EmailGroupDetailsForm:insertTextField("GroupCode", "", 220, TRUE).  
   
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("Group Name").
   EmailGroupDetailsForm:insertTextField("GroupName", "", 220, TRUE).  
   
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("Group Descr").
   EmailGroupDetailsForm:insertTextField("GroupDescr", "", 320, TRUE).  
   
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("Type").
   EmailGroupDetailsForm:insertComboField("GroupType", "", 110, TRUE).
   EmailGroupDetailsForm:insertComboPairs("GroupType", "Internal", "Internal").  
   EmailGroupDetailsForm:insertComboPairs("GroupType", "External", "External").  
   EmailGroupDetailsForm:insertComboPairs("GroupType", "Both", "Both").  
   
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("BusinessUnit").
   EmailGroupDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).  
   
   EmailGroupLoop:
   FOR EACH BusinessUnit NO-LOCK
      WHERE BusinessUnit.Active
      BY    BusinessUnit.ListingSequence:
            
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN NEXT EmailGroupLoop.
      EmailGroupDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID), BusinessUnit.UnitName).
   END.
   
   EmailGroupDetailsForm:startRow().
   EmailGroupDetailsForm:insertLabel("Active").
   EmailGroupDetailsForm:insertComboField("Active", "", 110, TRUE).
   EmailGroupDetailsForm:insertComboPairs("Active", "yes", "Yes").
   EmailGroupDetailsForm:insertComboPairs("Active", "no", "No").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pEmailGroupDetailsFields}
   
   /* Add Hidden Fields*/
   EmailGroupDetailsForm:insertHiddenField("emailgroup_browse_scroll","").
   EmailGroupDetailsForm:insertHiddenField("form_name","emailgroup_details_form").
   EmailGroupDetailsForm:insertHiddenField("prog_name","adEmailGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i EmailGroupDetailsForm}
   
   /* Create Button Bar */
   EmailGroupDetailsButtons = NEW buttonBar().
   
   /*   EmailGroupDetailsButtons:addButton("emailgroup_details_form_btn_create",          */
   /*                                    fTL("Create"),                       */
   /*                                    "createEmailGroup('emailgroup_details_form');"). */
   
   IF NOT logPreventDataUpdates THEN
      EmailGroupDetailsButtons:addButton("emailgroup_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateEmailGroup('emailgroup_details_form');").
   
   EmailGroupDetailsButtons:addButton("emailgroup_details_form_btn_cancel", 
                                       fTL("Cancel"), 
                                       "cancelUpdate('UserCancelled','process_mode'); disablePopup('emailgroup_details_form_popup');").
   EmailGroupDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   EmailGroupDetailsForm:FormButtons = EmailGroupDetailsButtons.
   
   EmailGroupDetailsForm:endForm(). 
   
   EmailGroupDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + EmailGroupDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pEmailGroupDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEmailGroupDetailsFields Procedure 
PROCEDURE pEmailGroupDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adEmailGroupAdmin_emailgroup_details_form.i}
      
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pEmailGroupUserLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEmailGroupUserLinkBrowse Procedure 
PROCEDURE pEmailGroupUserLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "emailgroupuserlink_details_form"}
   
   EmailGroupUserLinkBrowseForm = NEW dataForm("emailgroupuserlink_browse_form").
   EmailGroupUserLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   EmailGroupUserLinkBrowseForm:FormAction  = "dbEmailGroupUserLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   EmailGroupUserLinkBrowseForm:FormWidth  = 460.
   EmailGroupUserLinkBrowseForm:FormHeight = 300.
   EmailGroupUserLinkBrowseForm:FormTitle  = fTL("Users of EmailGroup") + (IF AVAILABLE EmailGroup THEN ":" + EmailGroup.GroupName ELSE "").
   EmailGroupUserLinkBrowseForm:FormType   = "medium".
   
   EmailGroupUserLinkBrowse = NEW browseTable("emailgroupuserlink_browse").
   EmailGroupUserLinkBrowse:BrowseWidth  = 440.
   EmailGroupUserLinkBrowse:BrowseHeight = 252.
   
   EmailGroupUserLinkBrowse:insertColumn(fTL("LinkID"), 120, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i EmailGroupUserLink}
   
   EmailGroupUserLinkBrowse:insertColumn(fTL("User"), 140, "CHARACTER", "left", FALSE).
   EmailGroupUserLinkBrowse:insertColumn(fTL("Active"), 90, "CHARACTER", "left", FALSE).
   
   /*Body*/
   EmailGroupUserLinkBrowse:startBody().
   
   IF AVAILABLE EmailGroup THEN
   DO:
      /*List the EmailGroups*/
      FOR EACH EmailGroupUserLink OF EmailGroup NO-LOCK:
         
         FIND GateUser       OF EmailGroupUserLink NO-LOCK NO-ERROR.
         
         EmailGroupUserLinkBrowse:startRow(EmailGroupUserLink.EmailGroupUserLinkID, "selectEmailGroupUserLinkRow(this," + '"' 
                                     + STRING(EmailGroupUserLink.EmailGroupUserLinkID) + '","adEmailGroupAdmin.p"' + ");", "").
         
         EmailGroupUserLinkBrowse:insertData(EmailGroupUserLink.EmailGroupUserLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i EmailGroupUserLink}
         
         EmailGroupUserLinkBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         EmailGroupUserLinkBrowse:insertData(STRING(EmailGroupUserLink.Active,"Yes/No"), "left").
         
         /* Add hidden fields */
         EmailGroupUserLinkBrowse:insertHiddenData("EmailGroupUserLinkVersionID",STRING(EmailGroupUserLink.VersionID)).
         EmailGroupUserLinkBrowse:insertHiddenData("EmailGroupID",EmailGroupUserLink.EmailGroupID).
         EmailGroupUserLinkBrowse:insertHiddenData("EmailGroupVersionID",EmailGroup.VersionID).
         EmailGroupUserLinkBrowse:insertHiddenData("GateUserID",IF AVAILABLE GateUser THEN STRING(GateUser.GateUserID) ELSE "").
         /*EmailGroupUserLinkBrowse:insertHiddenData("GateUserVersionID",IF AVAILABLE GateUser THEN STRING(GateUser.VersionID) ELSE "").*/
         
         EmailGroupUserLinkBrowse:endRow().
         
      END. /*FOR EACH EmailGroupUserLink NO-LOCK */
     
   END. /*IF AVAILABLE EmailGroup THEN*/
   
   EmailGroupUserLinkBrowse:endTable().
   
   /*EmailGroupUserLinkBrowseForm:insertHiddenField("EmailGroupUserLinkID","").*/
   EmailGroupUserLinkBrowseForm:insertHiddenField("EmailGroupUserLinkID","").
   EmailGroupUserLinkBrowseForm:insertHiddenField("EmailGroupUserLinkVersionID","").
   EmailGroupUserLinkBrowseForm:insertHiddenField("EmailGroupID", IF AVAILABLE EmailGroup THEN STRING(EmailGroup.EmailGroupID) ELSE "").
   EmailGroupUserLinkBrowseForm:insertHiddenField("EmailGroupVersionID", IF AVAILABLE EmailGroup THEN STRING(EmailGroup.VersionID) ELSE "").
   EmailGroupUserLinkBrowseForm:insertHiddenField("GateUserID", IF AVAILABLE GateUser THEN STRING(GateUser.GateUserID) ELSE "").
   EmailGroupUserLinkBrowseForm:insertHiddenField("emailgroup_browse_scroll","").
   EmailGroupUserLinkBrowseForm:insertHiddenField("popup_emailgroupuserlink_browse","").
   EmailGroupUserLinkBrowseForm:insertHiddenField("form_name","emailgroupuserlink_browse_form").
   EmailGroupUserLinkBrowseForm:insertHiddenField("prog_name","adEmailGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i EmailGroupUserLinkBrowseForm}
   
   /* Create Button Bar */
   EmailGroupUserLinkBrowseButtons = NEW buttonBar().
   
   EmailGroupUserLinkBrowseButtons:addButton("emailgroupuserlink_browse_form_btn_view",
                                             fTL("View"),
                                             "viewEmailGroupUserLinkDetails('emailgroupuserlink_details_form');",
                                             "Disabled").
   
   IF AVAILABLE EmailGroup AND EmailGroup.Active THEN
      EmailGroupUserLinkBrowseButtons:addButton("emailgroupuserlink_browse_form_btn_create",
                                                fTL("Create"),
                                                "createEmailGroupUserLink('emailgroupuserlink_details_form');").
   
   EmailGroupUserLinkBrowseButtons:addButton("emailgroupuserlink_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "hideEmailGroupUserLinkBrowse();").
   
   EmailGroupUserLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   EmailGroupUserLinkBrowseForm:FormBrowse  = EmailGroupUserLinkBrowse.
   EmailGroupUserLinkBrowseForm:FormButtons = EmailGroupUserLinkBrowseButtons.
   
   EmailGroupUserLinkBrowseForm:endForm(). 
   
   EmailGroupUserLinkBrowseForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pEmailGroupUserLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEmailGroupUserLinkDetails Procedure 
PROCEDURE pEmailGroupUserLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "emailgroupuserlink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "EmailGroupUserLinkID,GateUserID,Active"
          chrEditFieldList     = "Active"
          chrNewFieldList      = "GateUserID,Active"
          chrRequiredFieldList = "GateUserID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   EmailGroupUserLinkDetailsForm = NEW dataForm("emailgroupuserlink_details_form").
   EmailGroupUserLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   EmailGroupUserLinkDetailsForm:FormAction  = "dbEmailGroupUserLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   EmailGroupUserLinkDetailsForm:FormWidth   = 460.
   EmailGroupUserLinkDetailsForm:FormHeight  = 300.
   EmailGroupUserLinkDetailsForm:FormTitle   = "EmailGroup-User Link Details".
   EmailGroupUserLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   EmailGroupUserLinkDetailsForm:insertPaddingColumn(20).
   EmailGroupUserLinkDetailsForm:insertColumn(90).
   EmailGroupUserLinkDetailsForm:insertColumn(120).
   EmailGroupUserLinkDetailsForm:insertColumn(30).
   EmailGroupUserLinkDetailsForm:insertColumn(120).  
   
   /* Fields */
   EmailGroupUserLinkDetailsForm:startRow().
   EmailGroupUserLinkDetailsForm:insertLabel(fTL("Link ID")).
   EmailGroupUserLinkDetailsForm:insertTextField("EmailGroupUserLinkID", "", 110, TRUE).    
   
   EmailGroupUserLinkDetailsForm:startRow().
   EmailGroupUserLinkDetailsForm:insertLabel("User").
   EmailGroupUserLinkDetailsForm:insertComboField("GateUserID", "", 250, TRUE).
   /* Insert Users */
   FOR EACH GateUser NO-LOCK
      /* WHERE GateUser.Active */
      BY    GateUser.FullName:
      
      EmailGroupUserLinkDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).  
   END.
   
   EmailGroupUserLinkDetailsForm:startRow().
   EmailGroupUserLinkDetailsForm:insertLabel("Active").
   EmailGroupUserLinkDetailsForm:insertComboField("Active", "", 110, TRUE).
   EmailGroupUserLinkDetailsForm:insertComboPairs("Active", "yes", "Yes").  
   EmailGroupUserLinkDetailsForm:insertComboPairs("Active", "no", "No").  
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pEmailGroupUserLinkDetailsFields}
   
   /* Add Hidden Fields*/
   EmailGroupUserLinkDetailsForm:insertHiddenField("emailgroup_browse_scroll",    "").
   EmailGroupUserLinkDetailsForm:insertHiddenField("popup_emailgroupuserlink_browse", "").
   EmailGroupUserLinkDetailsForm:insertHiddenField("EmailGroupID", STRING(intSelectedEmailGroup)).
   EmailGroupUserLinkDetailsForm:insertHiddenField("form_name", "emailgroupuserlink_details_form").
   EmailGroupUserLinkDetailsForm:insertHiddenField("prog_name", "adEmailGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i EmailGroupUserLinkDetailsForm}
   
   /* Create Button Bar */
   EmailGroupUserLinkDetailsButtons = NEW buttonBar().
   
   EmailGroupUserLinkDetailsButtons:addButton("emailgroupuserlink_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateEmailGroupUserLink('adEmailGroupAdmin.p','emailgroupuserlink_details_form');").
   
   EmailGroupUserLinkDetailsButtons:addButton("emailgroupuserlink_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode');" 
                                                 + " disablePopup('emailgroupuserlink_details_form_popup');").
   
   EmailGroupUserLinkDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   EmailGroupUserLinkDetailsForm:FormButtons = EmailGroupUserLinkDetailsButtons.
   
   EmailGroupUserLinkDetailsForm:endForm(). 
   
   EmailGroupUserLinkDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + EmailGroupUserLinkDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pEmailGroupUserLinkDetailsOptions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEmailGroupUserLinkDetailsOptions Procedure 
PROCEDURE pEmailGroupUserLinkDetailsOptions :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adEmailGroupAdmin_emailgroupuserlink_details_form.i}
      
   END CASE. /*chrOption:*/
   
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
   
   /* Process URL values */
   ASSIGN chrEmailGroupID                = get-value("EmailGroupID")
          chrEmailGroupUserLinkID        = get-value("EmailGroupUserLinkID")
          intSelectedEmailGroup          = INTEGER(chrEmailGroupID)
          intSelectedEmailGroupUserLink  = INTEGER(chrEmailGroupUserLinkID)
          chrScrollToEmailGroupRow       = STRING(INTEGER(get-value("emailgroup_browse_scroll"))) + ";".
   
   IF intSelectedEmailGroup > 0 THEN
     chrSelectEmailGroupRow = 'selectEmailGroupRow(document.getElementById("emailgroup_browse_row_' + chrEmailGroupID + '"),"' 
                              + chrEmailGroupID + '","adEmailGroupAdmin.p' + '");'.
   
   IF intSelectedEmailGroupUserLink > 0 THEN 
     chrSelectEmailGroupUserLinkRow = 'selectEmailGroupUserLinkRow(document.getElementById("emailgroupuserlink_browse_row_' 
                                         + chrEmailGroupUserLinkID + '"),"' + chrEmailGroupUserLinkID + '","adEmailGroupAdmin.p' + '");'.
   
   IF get-value('popup_emailgroupuserlink_browse') = "yes" THEN 
     chrPopupEmailGroupUserLinks = 'enablePopup("emailgroupuserlink_browse_form_popup");'.
  
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("emailgroup_browse").scrollTop=' + chrScrollToEmailGroupRow 
                             + chrSelectEmailGroupRow + chrPopupEmailGroupUserLinks + chrSelectEmailGroupUserLinkRow.
                             
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                                
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:PageTitle = "EmailGroup Admin".
   ThisPage:FrameTitle = "EmailGroup Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
  
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Local JS for this program */
   ThisPage:addJavaScript("emailgroup.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pEmailGroupBrowse.
   
   /******* Popup Browsers and Forms ********/    
   FIND EmailGroup WHERE EmailGroup.EmailGroupID = intSelectedEmailGroup NO-LOCK NO-ERROR.
   
   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
   
   RUN pEmailGroupDetails.
   RUN pEmailGroupUserLinkBrowse.  
   RUN pEmailGroupUserLinkDetails.     
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT EmailGroupBrowseFrame                 NO-ERROR.
   DELETE OBJECT EmailGroupBrowse                      NO-ERROR.
   DELETE OBJECT EmailGroupBrowseButtons               NO-ERROR.
   DELETE OBJECT EmailGroupDetailsForm                 NO-ERROR.
   DELETE OBJECT EmailGroupDetailsButtons              NO-ERROR.
   DELETE OBJECT EmailGroupUserLinkBrowseForm          NO-ERROR.
   DELETE OBJECT EmailGroupUserLinkBrowse              NO-ERROR.
   DELETE OBJECT EmailGroupUserLinkBrowseButtons       NO-ERROR.
   DELETE OBJECT EmailGroupUserLinkDetailsForm         NO-ERROR.
   DELETE OBJECT EmailGroupUserLinkDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

