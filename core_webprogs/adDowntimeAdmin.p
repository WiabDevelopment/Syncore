&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adDowntimeAdmin.p 

  Description: ad file for the Downtime Owner Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Alex Litas

  Created: 17/12/2015

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
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedDowntimeOwner            AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedDowntimeOwnerHistory     AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedDowntimeGroup            AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedDowntimeGroupHistory     AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedDowntimeReason           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedDowntimeReasonHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrDowntimeOwnerHistoryID           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDowntimeOwnerRow                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDowntimeGroupRow                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDowntimeReasonRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToDowntimeOwnerRow         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToDowntimeOwnerHistoryRow  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToDowntimeGroupRow         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDowntimeOwnerID                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDowntimeGroupID                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDowntimeReasonID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupGroupHistory                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupGroup                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupReason                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupReasonHistory               AS CHARACTER   NO-UNDO.

/* Buffers */
/* Objects */
DEFINE VARIABLE DowntimeOwnerBrowseFrame            AS pageFrame.
DEFINE VARIABLE DowntimeOwnerBrowse                 AS browseTable.
DEFINE VARIABLE DowntimeOwnerBrowseButtons          AS buttonBar.
DEFINE VARIABLE DowntimeOwnerDetailsForm            AS dataForm.
DEFINE VARIABLE DowntimeOwnerDetailsButtons         AS buttonBar.

DEFINE VARIABLE DowntimeOwnerHistoryBrowseForm      AS dataForm.  
DEFINE VARIABLE DowntimeOwnerHistoryBrowse          AS browseTable.
DEFINE VARIABLE DowntimeOwnerHistoryDetailsForm     AS dataForm.
DEFINE VARIABLE DowntimeOwnerHistoryDetailsButtons  AS buttonBar.
DEFINE VARIABLE DowntimeOwnerHistoryButtons         AS buttonBar.

DEFINE VARIABLE DowntimeGroupButtons                AS buttonBar.
DEFINE VARIABLE DowntimeGroupBrowseForm             AS dataForm.  
DEFINE VARIABLE DowntimeGroupBrowse                 AS browseTable.
DEFINE VARIABLE DowntimeGroupDetailsForm            AS dataForm.
DEFINE VARIABLE DowntimeGroupDetailsButtons         AS buttonBar.

DEFINE VARIABLE DowntimeGroupHistoryBrowseForm      AS dataForm.  
DEFINE VARIABLE DowntimeGroupHistoryBrowse          AS browseTable.
DEFINE VARIABLE DowntimeGroupHistoryDetailsForm     AS dataForm.
DEFINE VARIABLE DowntimeGroupHistoryDetailsButtons  AS buttonBar.
DEFINE VARIABLE DowntimeGroupHistoryButtons         AS buttonBar.

DEFINE VARIABLE DowntimeReasonBrowseForm            AS dataForm.  
DEFINE VARIABLE DowntimeReasonBrowse                AS browseTable.
DEFINE VARIABLE DowntimeReasonDetailsForm           AS dataForm.
DEFINE VARIABLE DowntimeReasonDetailsButtons        AS buttonBar.
DEFINE VARIABLE DowntimeReasonButtons               AS buttonBar.

DEFINE VARIABLE DowntimeReasonHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE DowntimeReasonHistoryBrowse         AS browseTable.
DEFINE VARIABLE DowntimeReasonHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE DowntimeReasonHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE DowntimeReasonHistoryButtons        AS buttonBar.

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

&IF DEFINED(EXCLUDE-pDowntimeGroupBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeGroupBrowse Procedure
PROCEDURE pDowntimeGroupBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   DowntimeGroupBrowseForm           = NEW dataForm("downtimegroup_browse_form").
   DowntimeGroupBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   IF intSelectedDowntimeOwner > 0 THEN
      FIND FIRST DowntimeOwner NO-LOCK /*idx=DowntimeOwnerID*/
         WHERE DowntimeOwner.DowntimeOwnerID = intSelectedDowntimeOwner NO-ERROR.
         
   /* Setup */
   DowntimeGroupBrowseForm:FormWidth  = 860.
   DowntimeGroupBrowseForm:FormHeight = 530.
   DowntimeGroupBrowseForm:FormTitle  = fTL("Downtime Group ") + (IF intSelectedDowntimeOwner > 0 THEN " : " 
                                                                    + DowntimeOwner.OwnerCode ELSE "").
   DowntimeGroupBrowseForm:FormType   = "xxl_large".
   DowntimeGroupBrowse                = NEW browseTable("downtimegroup_browse").
   DowntimeGroupBrowse:BrowseWidth    = 840.
   DowntimeGroupBrowse:BrowseHeight   = 490.
   
   DowntimeGroupBrowse:insertColumn(fTL("Downtime Group ID"), 150, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeGroup}

   DowntimeGroupBrowse:insertColumn(fTL("Group Code"),       200, "CHARACTER", "LEFT",   FALSE).
   DowntimeGroupBrowse:insertColumn(fTL("Group Name"),       200, "CHARACTER", "LEFT",   FALSE).
   DowntimeGroupBrowse:insertColumn(fTL("Listing Sequence"), 150, "INTEGER",   "CENTER", FALSE).
   DowntimeGroupBrowse:insertColumn(fTL("Active"),           100, "LOGICAL",   "CENTER", FALSE).
   
   DowntimeGroupBrowse:StartBody().
   
   FOR EACH DowntimeGroup NO-LOCK /*idx=DowntimeOwnerID*/
      WHERE DowntimeGroup.DowntimeOwnerID = intSelectedDowntimeOwner:
          
      DowntimeGroupBrowse:startRow (DowntimeGroup.DowntimeGroupID,
                                    "selectDowntimeGroupRow(this," + '"' + STRING(DowntimeGroup.DowntimeGroupID) + '"' + ");", "").
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i DowntimeGroup}
      
      DowntimeGroupBrowse:insertData(STRING(DowntimeGroup.DowntimeGroupID),  "CENTER").
      DowntimeGroupBrowse:insertData(STRING(DowntimeGroup.GroupCode),        "LEFT").
      DowntimeGroupBrowse:insertData(STRING(DowntimeGroup.GroupName),        "LEFT").
      DowntimeGroupBrowse:insertData(STRING(DowntimeGroup.ListingSequence),  "CENTER").
      DowntimeGroupBrowse:insertData(STRING(DowntimeGroup.Active, "Yes/No"), "CENTER").
      
      /* Add hidden fields */
      DowntimeGroupBrowse:insertHiddenData("DowntimeOwnerID",DowntimeGroup.DowntimeOwnerID).
      DowntimeGroupBrowse:insertHiddenData("DowntimeGroupID",DowntimeGroup.DowntimeGroupID).
      DowntimeGroupBrowse:insertHiddenData("DowntimeGroupVersionID",DowntimeGroup.VersionID).

      DowntimeGroupBrowse:endRow().
   END. /* FOR EACH DowntimeGroup */
   
   DowntimeGroupBrowse:endTable().
   chrPageBuildError = chrPageBuildError + DowntimeGroupBrowse:getErrors().
   
   DowntimeGroupBrowseForm:insertHiddenField("downtimegroup_browse_scroll","").
   DowntimeGroupBrowseForm:insertHiddenField("form_name","downtimegroup_browse_form").
   DowntimeGroupBrowseForm:insertHiddenField("prog_name","adDowntimeAdmin.p").
   DowntimeGroupBrowseForm:insertHiddenField("DowntimeGroupVersionID","").
   DowntimeGroupBrowseForm:insertHiddenField("popup_downtimegroup_browse","").
   DowntimeGroupBrowseForm:insertHiddenField("popup_downtimegrouphistory_browse","").
   DowntimeGroupBrowseForm:insertHiddenField("popup_downtimereason_browse","").
   DowntimeGroupBrowseForm:insertHiddenField("popup_downtimereasonhistory_browse","").
   DowntimeGroupBrowseForm:insertHiddenField("DowntimeGroupID","").
   DowntimeGroupBrowseForm:insertHiddenField("DowntimeOwnerID",STRING(intSelectedDowntimeOwner)).   
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeGroupBrowseForm}
   
   /* Create Button Bar */
   DowntimeGroupButtons = NEW buttonBar().
   
   DowntimeGroupButtons:addButton("downtimegroup_browse_form_btn_reason",
                                  fTL("Reason"),
                                  "viewDowntimeReason('downtimereason_browse_form');",
                                  (IF intSelectedDowntimeGroup > 0 THEN "" ELSE "Disabled")).
   
   DowntimeGroupButtons:addButton("downtimegroup_browse_form_btn_details",
                                  fTL("Details"),
                                  "viewDowntimeGroupDetails('downtimegroup_details_form');",
                                  (IF intSelectedDowntimeGroup > 0 THEN "" ELSE "Disabled")).
                                  
   IF NOT logPreventDataCreates THEN
   DO:
      DowntimeGroupButtons:addButton("downtimegroup_browse_form_btn_create",
                                     fTL("Create"),
                                     "createDowntimeOwner('downtimegroup_details_form');",
                                     "").
   END.
   
   DowntimeGroupButtons:addButton("downtimegroup_browse_form_btn_history",
                                  fTL("History"),
                                  "viewDowntimeGroupHistory('downtimegrouphistory_browse_form');",
                                  (IF intSelectedDowntimeGroup > 0 THEN "" ELSE "Disabled")).    
                                         
   DowntimeGroupButtons:addButton("downtimegroup_browse_form_btn_cancel",
                                  fTL("Cancel"),
                                  "disablePopup('downtimegroup_browse_form_popup');").
   DowntimeGroupButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeGroupBrowseForm:FormBrowse  = DowntimeGroupBrowse.
   DowntimeGroupBrowseForm:FormButtons = DowntimeGroupButtons.
   DowntimeGroupBrowseForm:endForm(). 
   
   DowntimeGroupBrowseForm:displayForm().   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDowntimeGroupDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeGroupDetails Procedure
PROCEDURE pDowntimeGroupDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimegroup_details_form"}
   ASSIGN chrDisplayFieldList  = "DowntimeGroupID,DowntimeOwnerID,GroupCode,GroupName,"
                                 + "Active,ListingSequence" 
          chrEditFieldList     = "GroupCode,GroupName,Active,ListingSequence"
          chrNewFieldList      = "GroupCode,GroupName,Active,ListingSequence"
          chrRequiredFieldList = "GroupCode,GroupName,Active,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   DowntimeGroupDetailsForm = NEW dataForm("downtimegroup_details_form").
   DowntimeGroupDetailsForm:WebStream  = STREAM WebStream:HANDLE.
   
   DowntimeGroupDetailsForm:FormAction = "dbDowntimeGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DowntimeGroupDetailsForm:FormWidth  = 350.
   DowntimeGroupDetailsForm:FormHeight = 200.
   DowntimeGroupDetailsForm:FormTitle  = "Downtime Group Details".
   DowntimeGroupDetailsForm:FormType   = "small_wide".
   
   /* Column Layout */
   DowntimeGroupDetailsForm:insertPaddingColumn(30).
   DowntimeGroupDetailsForm:insertColumn(150).
   DowntimeGroupDetailsForm:insertColumn(125).
   DowntimeGroupDetailsForm:insertColumn(20).
   DowntimeGroupDetailsForm:insertColumn(4).
   DowntimeGroupDetailsForm:insertColumn(20).
   
   /* Fields */
   DowntimeGroupDetailsForm:startRow().
   DowntimeGroupDetailsForm:insertLabel(fTL("Downtime Group ID")).
   DowntimeGroupDetailsForm:insertTextField("DowntimeGroupID", "", 100, TRUE).
   
   DowntimeGroupDetailsForm:startRow().
   DowntimeGroupDetailsForm:insertLabel(fTL("Downtime Owner Code")).
   DowntimeGroupDetailsForm:insertComboField("DowntimeOwnerID", STRING(intSelectedDowntimeOwner), 100, TRUE).                                                   
   FOR EACH DowntimeOwner NO-LOCK  /*idx=DowntimeOwnerID*/                                                                                                      
      BY DowntimeOwner.OwnerCode:                                                                                                  
      DowntimeGroupDetailsForm:insertComboPairs("DowntimeOwnerID", STRING(DowntimeOwner.DowntimeOwnerID),
                                                                          DowntimeOwner.OwnerCode).
   END. /*FOR EACH DowntimeOwner NO-LOCK*/           
   
   DowntimeGroupDetailsForm:startRow().
   DowntimeGroupDetailsForm:insertLabel(fTL("Group Code")).
   DowntimeGroupDetailsForm:insertTextField("GroupCode", "", 100, TRUE).
   
   DowntimeGroupDetailsForm:startRow().
   DowntimeGroupDetailsForm:insertLabel(fTL("Group Name")).
   DowntimeGroupDetailsForm:insertTextField("GroupName", "", 100, TRUE).
      
   DowntimeGroupDetailsForm:startRow().
   DowntimeGroupDetailsForm:insertLabel(fTL("Listing Sequence")).
   DowntimeGroupDetailsForm:insertTextField("ListingSequence", "", 100, TRUE).

   DowntimeGroupDetailsForm:startRow().
   DowntimeGroupDetailsForm:insertLabel(fTL("Active")). 
   DowntimeGroupDetailsForm:insertComboField("Active", "", 100, TRUE).  
   DowntimeGroupDetailsForm:insertComboPairs("Active", "yes", "Active").
   DowntimeGroupDetailsForm:insertComboPairs("Active", "no",  "Not Active").

   {webGetOptionalFormFields.i pDowntimeGroupDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   DowntimeGroupDetailsForm:insertHiddenField("downtimeowner_browse_scroll", "").
   DowntimeGroupDetailsForm:insertHiddenField("popup_downtimegroup_browse", "").
   DowntimeGroupDetailsForm:insertHiddenField("popup_downtimegroup_details_form", "").
   DowntimeGroupDetailsForm:insertHiddenField("DowntimeOwnerID", STRING(intSelectedDowntimeOwner)).
   DowntimeGroupDetailsForm:insertHiddenField("DowntimeGroupID", STRING(intSelectedDowntimeGroup)).
   DowntimeGroupDetailsForm:insertHiddenField("downtimegroup_browse_scroll", "").
   DowntimeGroupDetailsForm:insertHiddenField("form_name", "downtimegroup_details_form").
   DowntimeGroupDetailsForm:insertHiddenField("prog_name", "adDowntimeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeGroupDetailsForm}
   
   /* Create Button Bar */
   DowntimeGroupDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:
   DowntimeGroupDetailsButtons:addButton("downtimegroup_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateDowntimeGroup('downtimegroup_details_form');").
   END.

   DowntimeGroupDetailsButtons:addButton("downtimegroup_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "disablePopup('downtimegroup_details_form_popup');").
   DowntimeGroupDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeGroupDetailsForm:FormButtons = DowntimeGroupDetailsButtons.
   
   DowntimeGroupDetailsForm:endForm(). 
   
   DowntimeGroupDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDowntimeGroupHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeGroupHistoryBrowse Procedure
PROCEDURE pDowntimeGroupHistoryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   DowntimeGroupHistoryBrowseForm           = NEW dataForm("downtimegrouphistory_browse_form").
   DowntimeGroupHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   IF intSelectedDowntimeGroup > 0 THEN
      FIND FIRST DowntimeGroup NO-LOCK /*idx=DowntimeGroupID*/
         WHERE DowntimeGroup.DowntimeGroupID = intSelectedDowntimeGroup NO-ERROR.
   
   /* Setup */
   DowntimeGroupHistoryBrowseForm:FormWidth  = 700.
   DowntimeGroupHistoryBrowseForm:FormHeight = 500.
   DowntimeGroupHistoryBrowseForm:FormTitle  = fTL("Downtime Group History") + (IF intSelectedDowntimeGroup > 0 THEN " : " 
                                                                                + DowntimeGroup.GroupCode ELSE "").
   DowntimeGroupHistoryBrowseForm:FormType   = "xl_large".
   DowntimeGroupHistoryBrowse                = NEW browseTable("downtimegrouphistory_browse").
   DowntimeGroupHistoryBrowse:BrowseWidth    = 680.
   DowntimeGroupHistoryBrowse:BrowseHeight   = 460.
   
   DowntimeGroupHistoryBrowse:insertColumn(fTL("History ID"), 60, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeGroupHistory}

   DowntimeGroupHistoryBrowse:insertColumn(fTL("Group Code"),       100, "CHARACTER", "LEFT",   FALSE).
   DowntimeGroupHistoryBrowse:insertColumn(fTL("Group Name"),       110, "CHARACTER", "LEFT",   FALSE).
   DowntimeGroupHistoryBrowse:insertColumn(fTL("Listing Sequence"), 110, "INTEGER",   "CENTER", FALSE).
   DowntimeGroupHistoryBrowse:insertColumn(fTL("Active"),           50,  "LOGICAL",   "CENTER", FALSE).
   DowntimeGroupHistoryBrowse:insertColumn(fTL("Gate User"),        100, "CHARACTER", "LEFT",   FALSE).
   DowntimeGroupHistoryBrowse:insertColumn(fTL("Created"),          130, "CENTER",    "LEFT",   FALSE).
   
   DowntimeGroupHistoryBrowse:StartBody().
   
   FOR EACH DowntimeGroupHistory NO-LOCK /*idx=DowntimeGroupHistoryID*/
      WHERE DowntimeGroupHistory.DowntimeGroupID = intSelectedDowntimeGroup
         BY DowntimeGroupHistory.DowntimeGroupHistoryID DESC:
          
      FIND FIRST OperationType OF DowntimeGroupHistory NO-LOCK NO-ERROR. /*idx=OperationTypeID*/
      FIND FIRST GateUser      OF DowntimeGroupHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/

      DowntimeGroupHistoryBrowse:startRow (DowntimeGroupHistory.DowntimeGroupHistoryID,
         "selectDowntimeGroupHistoryRow(this," + '"' + STRING(DowntimeGroupHistory.DowntimeGroupHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i DowntimeGroupHistory}
      
      DowntimeGroupHistoryBrowse:insertData(STRING(DowntimeGroupHistory.DowntimeGroupHistoryID),  "LEFT").
      DowntimeGroupHistoryBrowse:insertData(STRING(DowntimeGroupHistory.GroupCode),               "LEFT").
      DowntimeGroupHistoryBrowse:insertData(STRING(DowntimeGroupHistory.GroupName),               "LEFT").
      DowntimeGroupHistoryBrowse:insertData(STRING(DowntimeGroupHistory.ListingSequence),         "CENTER").
      DowntimeGroupHistoryBrowse:insertData(STRING(DowntimeGroupHistory.Active, "Yes/No"),        "CENTER").
      DowntimeGroupHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "LEFT").
      DowntimeGroupHistoryBrowse:insertData(fDisplayDate&Time(DowntimeGroupHistory.Created, "y/m/d H:M:S"), "LEFT").

      DowntimeGroupHistoryBrowse:endRow().
   END. /* FOR EACH DowntimeGroupHistory */
   
   DowntimeGroupHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + DowntimeGroupHistoryBrowse:getErrors().
   
   DowntimeGroupHistoryBrowseForm:insertHiddenField("popup_downtimegroup_browse","").
   DowntimeGroupHistoryBrowseForm:insertHiddenField("popup_downtimegrouphistory_browse","").
   DowntimeGroupHistoryBrowseForm:insertHiddenField("DowntimeGroupHistoryID",STRING(intSelectedDowntimeGroupHistory)).
      
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeGroupHistoryBrowseForm}
   
   /* Create Button Bar */
   DowntimeGroupHistoryButtons = NEW buttonBar().
   
   DowntimeGroupHistoryButtons:addButton("downtimegrouphistory_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewDowntimeGroupHistoryDetails('downtimegrouphistory_details_form');",
                                         (IF intSelectedDowntimeGroupHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   DowntimeGroupHistoryButtons:addButton("downtimegrouphistory_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('downtimegrouphistory_browse_form_popup');").
   DowntimeGroupHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeGroupHistoryBrowseForm:FormBrowse  = DowntimeGroupHistoryBrowse.
   DowntimeGroupHistoryBrowseForm:FormButtons = DowntimeGroupHistoryButtons.
   DowntimeGroupHistoryBrowseForm:endForm(). 
   
   DowntimeGroupHistoryBrowseForm:displayForm().   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDowntimeGroupHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeGroupHistoryDetails Procedure
PROCEDURE pDowntimeGroupHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimegrouphistory_details_form"}
   ASSIGN chrDisplayFieldList  = "DowntimeGroupHistoryID,DowntimeGroupID,GroupCode,GroupName,"
                                 + "Active,ListingSequence,CreatedDate,CreatedHour,CreatedMins,GateUserID,OperationTypeID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   DowntimeGroupHistoryDetailsForm = NEW dataForm("downtimegrouphistory_details_form").
   DowntimeGroupHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   DowntimeGroupHistoryDetailsForm:FormWidth  = 350.
   DowntimeGroupHistoryDetailsForm:FormHeight = 200.
   DowntimeGroupHistoryDetailsForm:FormTitle  = "Downtime Group History Details".
   DowntimeGroupHistoryDetailsForm:FormType   = "small_wide".
   
   /* Column Layout */
   DowntimeGroupHistoryDetailsForm:insertPaddingColumn(30).
   DowntimeGroupHistoryDetailsForm:insertColumn(150).
   DowntimeGroupHistoryDetailsForm:insertColumn(125).
   DowntimeGroupHistoryDetailsForm:insertColumn(20).
   DowntimeGroupHistoryDetailsForm:insertColumn(4).
   DowntimeGroupHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   DowntimeGroupHistoryDetailsForm:startRow().
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("History ID")).
   DowntimeGroupHistoryDetailsForm:insertTextField("DowntimeGroupHistoryID", "", 100, TRUE).
   
   DowntimeGroupHistoryDetailsForm:startRow().
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("Downtime Group ID")).
   DowntimeGroupHistoryDetailsForm:insertTextField("DowntimeGroupID", "", 100, TRUE).
   
   DowntimeGroupHistoryDetailsForm:startRow().
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("Group Code")).
   DowntimeGroupHistoryDetailsForm:insertTextField("GroupCode", "", 100, TRUE).
   
   DowntimeGroupHistoryDetailsForm:startRow().
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("Group Name")).
   DowntimeGroupHistoryDetailsForm:insertTextField("GroupName", "", 100, TRUE).
      
   DowntimeGroupHistoryDetailsForm:startRow().
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("Listing Sequence")).
   DowntimeGroupHistoryDetailsForm:insertTextField("ListingSequence", "", 100, TRUE).

   DowntimeGroupHistoryDetailsForm:startRow().                                                                                           
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   DowntimeGroupHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   DowntimeGroupHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   DowntimeGroupHistoryDetailsForm:insertLabel(":").                                                                                     
   DowntimeGroupHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                                         
   DowntimeGroupHistoryDetailsForm:startRow().                                                                                           
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   DowntimeGroupHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/                                                                                                      
      BY OperationType.OperationTypeID:                                                                                                  
      DowntimeGroupHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                        OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   DowntimeGroupHistoryDetailsForm:startRow().                                                                                           
   DowntimeGroupHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   DowntimeGroupHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*GateUserID*/                                                                                                        
      BY GateUser.FullName:                                                                                                              
      DowntimeGroupHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pDowntimeGroupHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   DowntimeGroupHistoryDetailsForm:insertHiddenField("downtimegrouphistory_browse_scroll", "").
   DowntimeGroupHistoryDetailsForm:insertHiddenField("form_name", "downtimegrouphistory_details_form").
   DowntimeGroupHistoryDetailsForm:insertHiddenField("prog_name", "adDowntimeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeGroupHistoryDetailsForm}
   
   /* Create Button Bar */
   DowntimeGroupHistoryDetailsButtons = NEW buttonBar().

   DowntimeGroupHistoryDetailsButtons:addButton("downtimegrouphistory_details_form_btn_cancel", 
                                                fTL("Cancel"), 
                                                "disablePopup('downtimegrouphistory_details_form_popup');").
   DowntimeGroupHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeGroupHistoryDetailsForm:FormButtons = DowntimeGroupHistoryDetailsButtons.
   
   DowntimeGroupHistoryDetailsForm:endForm(). 
   
   DowntimeGroupHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDowntimeReasonBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeReasonBrowse Procedure
PROCEDURE pDowntimeReasonBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   DowntimeReasonBrowseForm            = NEW dataForm("downtimereason_browse_form").
   DowntimeReasonBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   
   IF intSelectedDowntimeGroup > 0 THEN
      FIND FIRST DowntimeGroup NO-LOCK /*idx=DowntimeGroupID*/
         WHERE DowntimeGroup.DowntimeGroupID = intSelectedDowntimeGroup NO-ERROR.
      
   /* Setup */
   DowntimeReasonBrowseForm:FormWidth  = 700.
   DowntimeReasonBrowseForm:FormHeight = 500.
   DowntimeReasonBrowseForm:FormTitle  = fTL("Downtime Reason ") + (IF intSelectedDowntimeGroup > 0 THEN " : " 
                                                                 + DowntimeGroup.GroupCode ELSE "").
   DowntimeReasonBrowseForm:FormType   = "xl_large".
   DowntimeReasonBrowse                = NEW browseTable("downtimereason_browse").
   DowntimeReasonBrowse:BrowseWidth    = 680.
   DowntimeReasonBrowse:BrowseHeight   = 460.
   
   DowntimeReasonBrowse:insertColumn(fTL("Downtime Reason ID"), 140, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeReason}

   DowntimeReasonBrowse:insertColumn(fTL("Reason Code"),      160, "CHARACTER", "LEFT",   FALSE).
   DowntimeReasonBrowse:insertColumn(fTL("Reason Descr"),     160, "CHARACTER", "LEFT",   FALSE).
   DowntimeReasonBrowse:insertColumn(fTL("Listing Sequence"), 120, "INTEGER",   "CENTER", FALSE).
   DowntimeReasonBrowse:insertColumn(fTL("Active"),           70,  "LOGICAL",   "CENTER", FALSE).
   
   DowntimeReasonBrowse:StartBody().
   
   FOR EACH DowntimeReason NO-LOCK /*idx=DowntimeReasonID*/
      WHERE DowntimeReason.DowntimeGroupID = intSelectedDowntimeGroup:
         
      DowntimeReasonBrowse:startRow (DowntimeReason.DowntimeReasonID,
                                    "selectDowntimeReasonRow(this," + '"' + STRING(DowntimeReason.DowntimeReasonID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i DowntimeReason}
      
      DowntimeReasonBrowse:insertData(STRING(DowntimeReason.DowntimeReasonID), "CENTER").
      DowntimeReasonBrowse:insertData(STRING(DowntimeReason.ReasonCode),       "LEFT").
      DowntimeReasonBrowse:insertData(STRING(DowntimeReason.ReasonDescr),      "LEFT").
      DowntimeReasonBrowse:insertData(STRING(DowntimeReason.ListingSequence),  "CENTER").
      DowntimeReasonBrowse:insertData(STRING(DowntimeReason.Active, "Yes/No"), "CENTER").
      
      /* Add hidden fields */
      DowntimeReasonBrowse:insertHiddenData("DowntimeGroupID",DowntimeReason.DowntimeGroupID).
      DowntimeReasonBrowse:insertHiddenData("DowntimeReasonID",DowntimeReason.DowntimeReasonID).
      DowntimeReasonBrowse:insertHiddenData("DowntimeReasonVersionID",DowntimeReason.VersionID).

      DowntimeReasonBrowse:endRow().
   END. /* FOR EACH DowntimeReason */
   
   DowntimeReasonBrowse:endTable().
   chrPageBuildError = chrPageBuildError + DowntimeReasonBrowse:getErrors().
   
   DowntimeReasonBrowseForm:insertHiddenField("downtimereason_browse_scroll","").
   DowntimeReasonBrowseForm:insertHiddenField("form_name","downtimereason_browse_form").
   DowntimeReasonBrowseForm:insertHiddenField("prog_name","adDowntimeAdmin.p").
   DowntimeReasonBrowseForm:insertHiddenField("DowntimeReasonVersionID","").
   DowntimeReasonBrowseForm:insertHiddenField("popup_downtimereason_browse","").
   DowntimeReasonBrowseForm:insertHiddenField("popup_downtimereasonhistory_browse","").
   DowntimeReasonBrowseForm:insertHiddenField("popup_downtimegroup_browse","").
   DowntimeReasonBrowseForm:insertHiddenField("DowntimeReasonID", STRING(intSelectedDowntimeReason)).
   DowntimeReasonBrowseForm:insertHiddenField("DowntimeGroupID",  STRING(intSelectedDowntimeGroup)).
   DowntimeReasonBrowseForm:insertHiddenField("DowntimeOwnerID",  STRING(intSelectedDowntimeOwner)).
     
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeReasonBrowseForm}
   
   /* Create Button Bar */
   DowntimeReasonButtons = NEW buttonBar().
      
   DowntimeReasonButtons:addButton("downtimereason_browse_form_btn_details",
                                   fTL("Details"),
                                   "viewDowntimeReasonDetails('downtimereason_details_form');",
                                   "Disabled").
                                  
   IF NOT logPreventDataCreates THEN
   DO:
      DowntimeReasonButtons:addButton("downtimereason_browse_form_btn_create",
                                      fTL("Create"),
                                      "createDowntimeReason('downtimereason_details_form');",
                                      "").
   END.
   
   DowntimeReasonButtons:addButton("downtimereason_browse_form_btn_history",
                                   fTL("History"),
                                   "viewDowntimeReasonHistory('downtimereasonhistory_browse_form');",
                                   "Disabled").    
                                         
   DowntimeReasonButtons:addButton("downtimereason_browse_form_btn_cancel",
                                   fTL("Cancel"),
                                   "disablePopup('downtimereason_browse_form_popup');").
   DowntimeReasonButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeReasonBrowseForm:FormBrowse  = DowntimeReasonBrowse.
   DowntimeReasonBrowseForm:FormButtons = DowntimeReasonButtons.
   DowntimeReasonBrowseForm:endForm(). 
   
   DowntimeReasonBrowseForm:displayForm().   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDowntimeReasonDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeReasonDetails Procedure
PROCEDURE pDowntimeReasonDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimereason_details_form"}
   ASSIGN chrDisplayFieldList  = "DowntimeReasonID,DowntimeGroupID,ReasonCode,ReasonDescr,ListingSequence,Active"
          chrEditFieldList     = "ReasonCode,ReasonDescr,ListingSequence,Active"
          chrNewFieldList      = "ReasonCode,ReasonDescr,ListingSequence,Active"
          chrRequiredFieldList = "ReasonCode,ReasonDescr,ListingSequence,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   DowntimeReasonDetailsForm = NEW dataForm("downtimereason_details_form").
   DowntimeReasonDetailsForm:WebStream  = STREAM WebStream:HANDLE.
   
   DowntimeReasonDetailsForm:FormAction = "dbDowntimeReasonUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DowntimeReasonDetailsForm:FormWidth  = 350.
   DowntimeReasonDetailsForm:FormHeight = 200.
   DowntimeReasonDetailsForm:FormTitle  = "Downtime Reason Details".
   DowntimeReasonDetailsForm:FormType   = "small_wide".
   
   /* Column Layout */
   DowntimeReasonDetailsForm:insertPaddingColumn(10).
   DowntimeReasonDetailsForm:insertColumn(150).
   DowntimeReasonDetailsForm:insertColumn(120).
   DowntimeReasonDetailsForm:insertColumn(20).
   DowntimeReasonDetailsForm:insertColumn(4).
   DowntimeReasonDetailsForm:insertColumn(110).  
   
    /* Fields */
   DowntimeReasonDetailsForm:startRow().
   DowntimeReasonDetailsForm:insertLabel(fTL("Downtime Reason ID")).
   DowntimeReasonDetailsForm:insertTextField("DowntimeReasonID", "", 110, TRUE).
   
   DowntimeReasonDetailsForm:startRow().
   DowntimeReasonDetailsForm:insertLabel(fTL("Downtime Group Code")).
   DowntimeReasonDetailsForm:insertComboField("DowntimeGroupID", STRING(intSelectedDowntimeGroup), 168, TRUE).                                                   
   FOR EACH DowntimeGroup NO-LOCK  /*DowntimeGroupID*/                                                                                                      
      BY DowntimeGroup.GroupCode:                                                                                                  
      DowntimeReasonDetailsForm:insertComboPairs("DowntimeGroupID", STRING(DowntimeGroup.DowntimeGroupID),
                                                                           DowntimeGroup.GroupCode).
   END. /*FOR EACH DowntimeGroup NO-LOCK*/   
   
   DowntimeReasonDetailsForm:startRow().
   DowntimeReasonDetailsForm:insertLabel(fTL("Reason Code")).
   DowntimeReasonDetailsForm:insertTextField("ReasonCode", "", 168, TRUE).
   
   DowntimeReasonDetailsForm:startRow().
   DowntimeReasonDetailsForm:insertLabel(fTL("Reason Descr ")).
   DowntimeReasonDetailsForm:insertTextField("ReasonDescr", "", 168, TRUE).
      
   DowntimeReasonDetailsForm:startRow().
   DowntimeReasonDetailsForm:insertLabel(fTL("Listing Sequence")).
   DowntimeReasonDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).
   
   DowntimeReasonDetailsForm:startRow().
   DowntimeReasonDetailsForm:insertLabel(fTL("Active")). 
   DowntimeReasonDetailsForm:insertComboField("Active", "", 112, TRUE).  
   DowntimeReasonDetailsForm:insertComboPairs("Active", "yes", "Active").
   DowntimeReasonDetailsForm:insertComboPairs("Active", "no",  "Not Active").

   {webGetOptionalFormFields.i pDowntimeReasonDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   DowntimeReasonDetailsForm:insertHiddenField("downtimereason_browse_scroll", "").
   DowntimeReasonDetailsForm:insertHiddenField("form_name", "downtimereason_details_form").
   DowntimeReasonDetailsForm:insertHiddenField("prog_name", "adDowntimeAdmin.p").
   
   DowntimeReasonDetailsForm:insertHiddenField("popup_downtimereason_browse", "").
   DowntimeReasonDetailsForm:insertHiddenField("popup_downtimereason_details_form", "").
   DowntimeReasonDetailsForm:insertHiddenField("DowntimeReasonID", STRING(intSelectedDowntimeReason)).
   DowntimeReasonDetailsForm:insertHiddenField("DowntimeGroupID",  STRING(intSelectedDowntimeGroup)).
   DowntimeReasonDetailsForm:insertHiddenField("DowntimeOwnerID",  STRING(intSelectedDowntimeOwner)).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeReasonDetailsForm}
   
   /* Create Button Bar */
   DowntimeReasonDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataUpdates THEN
   DO:
   DowntimeReasonDetailsButtons:addButton("downtimereason_details_form_btn_save", 
                                          fTL("Save"), 
                                          "updateDowntimeReason('downtimereason_details_form');").
   END.
   DowntimeReasonDetailsButtons:addButton("downtimereason_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode');" + 
                                          "disablePopup('downtimereason_details_form_popup');").
   DowntimeReasonDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeReasonDetailsForm:FormButtons = DowntimeReasonDetailsButtons.
   
   DowntimeReasonDetailsForm:endForm(). 
   
   DowntimeReasonDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDowntimeReasonHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeReasonHistoryBrowse Procedure
PROCEDURE pDowntimeReasonHistoryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   DowntimeReasonHistoryBrowseForm           = NEW dataForm("downtimereasonhistory_browse_form").
   DowntimeReasonHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   IF intSelectedDowntimeReason > 0 THEN
      FIND FIRST DowntimeReason NO-LOCK 
         WHERE DowntimeReason.DowntimeReasonID = intSelectedDowntimeReason NO-ERROR.

   /* Setup */
   DowntimeReasonHistoryBrowseForm:FormWidth  = 700.
   DowntimeReasonHistoryBrowseForm:FormHeight = 500.
   DowntimeReasonHistoryBrowseForm:FormTitle  = fTL("Downtime Reason History") + (IF intSelectedDowntimeReason > 0 THEN " : "
                                                                               +  DowntimeReason.ReasonCode ELSE "").
   DowntimeReasonHistoryBrowseForm:FormType   = "xl_large".
   DowntimeReasonHistoryBrowse                = NEW browseTable("downtimereasonhistory_browse").
   DowntimeReasonHistoryBrowse:BrowseWidth    = 680.
   DowntimeReasonHistoryBrowse:BrowseHeight   = 460.

   DowntimeReasonHistoryBrowse:insertColumn(fTL("History ID"), 60, "INTEGER").

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeReasonHistory}

   DowntimeReasonHistoryBrowse:insertColumn(fTL("Reason Code"),      150, "CHARACTER", "LEFT",   FALSE).
   DowntimeReasonHistoryBrowse:insertColumn(fTL("Reason Descr"),     160, "CHARACTER", "LEFT",   FALSE).
   DowntimeReasonHistoryBrowse:insertColumn(fTL("Listing Sequence"), 110, "INTEGER",   "CENTER", FALSE).
   DowntimeReasonHistoryBrowse:insertColumn(fTL("Active"),           50,  "LOGICAL",   "CENTER", FALSE).
   DowntimeReasonHistoryBrowse:insertColumn(fTL("Created"),          130, "CHARACTER", "LEFT",   FALSE).

   DowntimeReasonHistoryBrowse:StartBody().

   FOR EACH DowntimeReasonHistory NO-LOCK /*idx=DowntimeReasonID and DowntimeReasonHistoryID*/
      WHERE DowntimeReasonHistory.DowntimeReasonID = intSelectedDowntimeReason
         BY DowntimeReasonHistory.DowntimeReasonHistoryID DESC:

      FIND FIRST OperationType OF DowntimeReasonHistory NO-LOCK NO-ERROR. /*idx=OperationTypeID*/

      DowntimeReasonHistoryBrowse:startRow (DowntimeReasonHistory.DowntimeReasonHistoryID,
         "selectDowntimeReasonHistoryRow(this," + '"' + STRING(DowntimeReasonHistory.DowntimeReasonHistoryID) + '"' + ");", "").

      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i DowntimeReasonHistory}

      DowntimeReasonHistoryBrowse:insertData(STRING(DowntimeReasonHistory.DowntimeReasonHistoryID), "LEFT").
      DowntimeReasonHistoryBrowse:insertData(STRING(DowntimeReasonHistory.ReasonCode),              "LEFT").
      DowntimeReasonHistoryBrowse:insertData(STRING(DowntimeReasonHistory.ReasonDescr),             "LEFT").
      DowntimeReasonHistoryBrowse:insertData(STRING(DowntimeReasonHistory.ListingSequence),         "CENTER").
      DowntimeReasonHistoryBrowse:insertData(STRING(DowntimeReasonHistory.Active, "Yes/No"),        "CENTER").
      DowntimeReasonHistoryBrowse:insertData(fDisplayDate&Time(DowntimeReasonHistory.Created, "y/m/d H:M:S"), "LEFT").

      DowntimeReasonHistoryBrowse:endRow().
   END. /* FOR EACH DowntimeReasonHistory */

   DowntimeReasonHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + DowntimeReasonHistoryBrowse:getErrors().

   DowntimeReasonHistoryBrowseForm:insertHiddenField("popup_downtimereason_browse","").
   DowntimeReasonHistoryBrowseForm:insertHiddenField("popup_downtimereasonhistory_browse","").
   DowntimeReasonHistoryBrowseForm:insertHiddenField("DowntimeReasonHistoryID","").
   DowntimeReasonHistoryBrowseForm:insertHiddenField("DowntimeReasonID",STRING(intSelectedDowntimeReason)).
   DowntimeReasonHistoryBrowseForm:insertHiddenField("DowntimeOwnerID","").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeReasonHistoryBrowseForm}

   /* Create Button Bar */
   DowntimeReasonHistoryButtons = NEW buttonBar().

   DowntimeReasonHistoryButtons:addButton("downtimereasonhistory_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewDowntimeReasonHistoryDetails('downtimereasonhistory_details_form');",
                                         (IF intSelectedDowntimeReasonHistory > 0 THEN "" ELSE "Disabled")).

   DowntimeReasonHistoryButtons:addButton("downtimereasonhistory_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('downtimereasonhistory_browse_form_popup');").
   DowntimeReasonHistoryButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   DowntimeReasonHistoryBrowseForm:FormBrowse  = DowntimeReasonHistoryBrowse.
   DowntimeReasonHistoryBrowseForm:FormButtons = DowntimeReasonHistoryButtons.
   DowntimeReasonHistoryBrowseForm:endForm().

   DowntimeReasonHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDowntimeReasonHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDowntimeReasonHistoryDetails Procedure
PROCEDURE pDowntimeReasonHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimereasonhistory_details_form"}
   ASSIGN chrDisplayFieldList  = "DowntimeReasonHistoryID,DowntimeReasonID,ReasonCode,ReasonDescr,"
                                 + "Active,ListingSequence,CreatedDate,CreatedHour,CreatedMins,GateUserID,OperationTypeID"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   DowntimeReasonHistoryDetailsForm = NEW dataForm("downtimereasonhistory_details_form").
   DowntimeReasonHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   DowntimeReasonHistoryDetailsForm:FormWidth   = 350.
   DowntimeReasonHistoryDetailsForm:FormHeight  = 200.
   DowntimeReasonHistoryDetailsForm:FormTitle   = "Downtime Reason History Details".
   DowntimeReasonHistoryDetailsForm:FormType    = "small_wide".

   /* Column Layout */
   DowntimeReasonHistoryDetailsForm:insertPaddingColumn(30).
   DowntimeReasonHistoryDetailsForm:insertColumn(150).
   DowntimeReasonHistoryDetailsForm:insertColumn(125).
   DowntimeReasonHistoryDetailsForm:insertColumn(20).
   DowntimeReasonHistoryDetailsForm:insertColumn(4).
   DowntimeReasonHistoryDetailsForm:insertColumn(20).

   /* Fields */
   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("History ID")).
   DowntimeReasonHistoryDetailsForm:insertTextField("DowntimeReasonHistoryID", "", 100, TRUE).

   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("Downtime Reason ID")).
   DowntimeReasonHistoryDetailsForm:insertTextField("DowntimeReasonID", "", 100, TRUE).

   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("Reason Code")).
   DowntimeReasonHistoryDetailsForm:insertTextField("ReasonCode", "", 168, TRUE).

   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("Reason Descr")).
   DowntimeReasonHistoryDetailsForm:insertTextField("ReasonDescr", "", 168, TRUE).

   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("Listing Sequence")).
   DowntimeReasonHistoryDetailsForm:insertTextField("ListingSequence", "", 100, TRUE).

   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("Created")).
   DowntimeReasonHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).
   /* Time fields have no label*/
   DowntimeReasonHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).
   /* This has a label to separate the time */
   DowntimeReasonHistoryDetailsForm:insertLabel(":").
   DowntimeReasonHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).

   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).
   DowntimeReasonHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/
      BY OperationType.OperationTypeID:
      DowntimeReasonHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                          OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/

   DowntimeReasonHistoryDetailsForm:startRow().
   DowntimeReasonHistoryDetailsForm:insertLabel(fTL("Gate User ID")).
   DowntimeReasonHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).
   FOR EACH GateUser NO-LOCK  /*GateUserID*/
      BY GateUser.FullName:
      DowntimeReasonHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.  /*FOR EACH GateUser NO-LOCK*/

   {webGetOptionalFormFields.i pDowntimeReasonHistoryDetailsFields}

   chrExtraFieldList = TRIM(chrExtraFieldList,",").

   /* Add Hidden Fields*/
   DowntimeReasonHistoryDetailsForm:insertHiddenField("downtimereasonhistory_browse_scroll", "").
   DowntimeReasonHistoryDetailsForm:insertHiddenField("form_name", "downtimereasonhistory_details_form").
   DowntimeReasonHistoryDetailsForm:insertHiddenField("prog_name", "adDowntimeAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeReasonHistoryDetailsForm}

   /* Create Button Bar */
   DowntimeReasonHistoryDetailsButtons = NEW buttonBar().

   DowntimeReasonHistoryDetailsButtons:addButton("downtimereasonhistory_details_form_btn_cancel",
                                                 fTL("Cancel"),
                                                 "disablePopup('downtimereasonhistory_details_form_popup');").
   DowntimeReasonHistoryDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   DowntimeReasonHistoryDetailsForm:FormButtons = DowntimeReasonHistoryDetailsButtons.

   DowntimeReasonHistoryDetailsForm:endForm().

   DowntimeReasonHistoryDetailsForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END*/
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
   RUN pGetSystemOptions.  
   
   ASSIGN chrDowntimeOwnerID                 = get-value("DowntimeOwnerID")
          chrDowntimeGroupID                 = get-value("DowntimeGroupID")
          chrDowntimeReasonID                = get-value("DowntimeReasonID")
          intSelectedDowntimeOwner           = INTEGER(chrDowntimeOwnerID)
          intSelectedDowntimeGroup           = INTEGER(chrDowntimeGroupID)
          intSelectedDowntimeGroupHistory    = INTEGER(get-value("DowntimeGroupHistoryID"))
          intSelectedDowntimeReasonHistory   = INTEGER(get-value("DowntimeReasonHistoryID"))
          intSelectedDowntimeReason          = INTEGER(chrDowntimeReasonID)
          chrScrollToDowntimeOwnerRow        = STRING(INTEGER(get-value("downtimeowner_browse_scroll"))) + ";"
          chrScrollToDowntimeGroupRow        = STRING(INTEGER(get-value("downtimegroup_browse_scroll"))) + ";"
          chrDowntimeOwnerHistoryID          = get-value("DowntimeOwnerHistoryID")
          intSelectedDowntimeOwnerHistory    = INTEGER(chrDowntimeOwnerHistoryID)
          chrScrollToDowntimeOwnerHistoryRow = STRING(INTEGER(get-value("downtimeownerhistory_browse_scroll"))) + ";".

   /* Process URL values */
   IF chrDowntimeOwnerID <> "" THEN
      chrDowntimeOwnerRow = 'selectDowntimeOwnerRow(document.getElementById("downtimeowner_browse_row_'
                               + chrDowntimeOwnerID + '"),"' + chrDowntimeOwnerID +  '");'.
                               
   IF chrDowntimeGroupID <> "" THEN
      chrDowntimeGroupRow = 'selectDowntimeGroupRow(document.getElementById("downtimegroup_browse_row_'
                               + chrDowntimeGroupID + '"),"' + chrDowntimeGroupID +  '");'.
                               
   IF chrDowntimeReasonID <> "" THEN
      chrDowntimeReasonRow = 'selectDowntimeReasonRow(document.getElementById("downtimereason_browse_row_'
                               + chrDowntimeReasonID + '"),"' + chrDowntimeReasonID +  '");'.
                                                                                      
   IF get-value('popup_downtimeownerhistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("downtimeownerhistory_browse_form_popup");'.
      
   IF get-value('popup_downtimegrouphistory_browse') = "Yes" THEN
      chrPopupGroupHistory = 'enablePopup("downtimegrouphistory_browse_form_popup");'.
      
   IF get-value('popup_downtimegroup_browse') = "Yes" THEN
      chrPopupGroup = 'enablePopup("downtimegroup_browse_form_popup");'.
      
   IF get-value('popup_downtimereason_browse') = "Yes" THEN
      chrPopupReason = 'enablePopup("downtimereason_browse_form_popup");'.
      
   IF get-value('popup_downtimereasonhistory_browse') = "Yes" THEN
      chrPopupReasonHistory = 'enablePopup("downtimereasonhistory_browse_form_popup");'.
      
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("downtimeowner_browse").scrollTop=' + chrScrollToDowntimeOwnerRow 
                             + chrDowntimeOwnerRow + chrDowntimeGroupRow + chrPopUpHistory + chrPopupGroup + chrPopupGroupHistory 
                             + chrPopupReason + chrPopupReasonHistory + chrDowntimeReasonRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Downtime Owner Admin".
   ThisPage:FrameTitle    = "Downtime Owner Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("downtime.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pDowntimeOwnerBrowse.
   RUN pDowntimeOwnerDetails.
   
   /******* Popup Browsers and Forms ********/    
   RUN pDowntimeOwnerHistoryBrowse.
   RUN pDowntimeOwnerHistoryDetails.
   
   RUN pDowntimeGroupBrowse.
   RUN pDowntimeGroupDetails.
   
   RUN pDowntimeGroupHistoryBrowse.
   RUN pDowntimeGroupHistoryDetails.
   
   RUN pDowntimeReasonBrowse.
   RUN pDowntimeReasonDetails.
   
   RUN pDowntimeReasonHistoryBrowse.
   RUN pDowntimeReasonHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT DowntimeOwnerBrowseFrame           NO-ERROR.
   DELETE OBJECT DowntimeOwnerBrowse                NO-ERROR.
   DELETE OBJECT DowntimeOwnerBrowseButtons         NO-ERROR.
   DELETE OBJECT DowntimeOwnerDetailsForm           NO-ERROR.
   DELETE OBJECT DowntimeOwnerDetailsButtons        NO-ERROR.
   
   DELETE OBJECT DowntimeOwnerHistoryBrowseForm     NO-ERROR.  
   DELETE OBJECT DowntimeOwnerHistoryBrowse         NO-ERROR.
   DELETE OBJECT DowntimeOwnerHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT DowntimeOwnerHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT DowntimeOwnerHistoryButtons        NO-ERROR.
   
   DELETE OBJECT DowntimeGroupButtons               NO-ERROR.
   DELETE OBJECT DowntimeGroupBrowseForm            NO-ERROR.  
   DELETE OBJECT DowntimeGroupBrowse                NO-ERROR.
   DELETE OBJECT DowntimeGroupDetailsForm           NO-ERROR.
   DELETE OBJECT DowntimeGroupDetailsButtons        NO-ERROR.
   
   DELETE OBJECT DowntimeGroupHistoryBrowseForm     NO-ERROR.  
   DELETE OBJECT DowntimeGroupHistoryBrowse         NO-ERROR.
   DELETE OBJECT DowntimeGroupHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT DowntimeGroupHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT DowntimeGroupHistoryButtons        NO-ERROR.
   
   DELETE OBJECT DowntimeReasonButtons              NO-ERROR.
   DELETE OBJECT DowntimeReasonBrowseForm           NO-ERROR.  
   DELETE OBJECT DowntimeReasonBrowse               NO-ERROR.
   DELETE OBJECT DowntimeReasonDetailsForm          NO-ERROR.
   DELETE OBJECT DowntimeReasonDetailsButtons       NO-ERROR.
   
   DELETE OBJECT DowntimeReasonHistoryBrowseForm     NO-ERROR.  
   DELETE OBJECT DowntimeReasonHistoryBrowse         NO-ERROR.
   DELETE OBJECT DowntimeReasonHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT DowntimeReasonHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT DowntimeReasonHistoryButtons        NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pDowntimeOwnerBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "downtimeowner_details_form"}
   
   DowntimeOwnerBrowse = NEW browseTable("downtimeowner_browse").
   DowntimeOwnerBrowse:BrowseWidth  = 965.
   DowntimeOwnerBrowse:BrowseHeight = 455.
   DowntimeOwnerBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   DowntimeOwnerBrowse:insertColumn(fTL(" Downtime Owner ID"), 200, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeOwner}
   
   DowntimeOwnerBrowse:insertColumn(fTL("Owner Code"),       200, "CHARACTER", "LEFT",   FALSE).
   DowntimeOwnerBrowse:insertColumn(fTL("Owner Name"),       200, "CHARACTER", "LEFT",   FALSE).
   DowntimeOwnerBrowse:insertColumn(fTL("Listing Sequence"), 200, "INTEGER",   "CENTER", FALSE).
   DowntimeOwnerBrowse:insertColumn(fTL("Active"),           145, "LOGICAL",   "CENTER", FALSE).
   
   /*Body*/
   DowntimeOwnerBrowse:startBody().
   
   FOR EACH DowntimeOwner NO-LOCK: /*idx=DowntimeOwnerID*/
   
      DowntimeOwnerBrowse:startRow(DowntimeOwner.DowntimeOwnerID, "selectDowntimeOwnerRow(this," + '"'
                                       + STRING(DowntimeOwner.DowntimeOwnerID) + '"' + ");", "").

      DowntimeOwnerBrowse:insertData(DowntimeOwner.DowntimeOwnerID).
  
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i DowntimeOwner}

      DowntimeOwnerBrowse:insertData(DowntimeOwner.OwnerCode,                "LEFT").
      DowntimeOwnerBrowse:insertData(DowntimeOwner.OwnerName,                "LEFT").
      DowntimeOwnerBrowse:insertData(DowntimeOwner.ListingSequence,          "CENTER").
      DowntimeOwnerBrowse:insertData(STRING(DowntimeOwner.Active, "Yes/No"), "CENTER").

      /* Add hidden fields */
      DowntimeOwnerBrowse:insertHiddenData("DowntimeOwnerVersionID",DowntimeOwner.VersionID).

      DowntimeOwnerBrowse:endRow().
      
   END. /*FOR EACH DowntimeOwnerAdmin NO-LOCK */
   
   DowntimeOwnerBrowse:endTable().
   chrPageBuildError = chrPageBuildError + DowntimeOwnerBrowse:getErrors().
   
   /* Create a new frame */
   DowntimeOwnerBrowseFrame = NEW pageFrame().
   DowntimeOwnerBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   DowntimeOwnerBrowseFrame:FormAction="dbDowntimeOwnerUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   DowntimeOwnerBrowseFrame:formOpen("downtimeowner_browse_form").
   
   /* Start the Frame Header */
   DowntimeOwnerBrowseFrame:insertSpacer(5).
   DowntimeOwnerBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   DowntimeOwnerBrowse:displayBrowse().  
   
   /* End the Frame Header */
   DowntimeOwnerBrowseFrame:frameClose().
   DowntimeOwnerBrowseFrame:insertSpacer(10).
   
   DowntimeOwnerBrowseFrame:insertHiddenField("downtimeowner_browse_scroll","").
   DowntimeOwnerBrowseFrame:insertHiddenField("DowntimeOwnerID","").
   DowntimeOwnerBrowseFrame:insertHiddenField("DowntimeGroupID","").
   DowntimeOwnerBrowseFrame:insertHiddenField("DowntimeReasonID","").
   DowntimeOwnerBrowseFrame:insertHiddenField("DowntimeOwnerVersionID","").
   DowntimeOwnerBrowseFrame:insertHiddenField("form_name","downtimeowner_browse_form").
   DowntimeOwnerBrowseFrame:insertHiddenField("popup_downtimeownerhistory_browse","").
   DowntimeOwnerBrowseFrame:insertHiddenField("popup_downtimegroup_browse","").
   DowntimeOwnerBrowseFrame:insertHiddenField("popup_downtimegrouphistory_browse","").
   DowntimeOwnerBrowseFrame:insertHiddenField("popup_downtimereason_browse","").
   DowntimeOwnerBrowseFrame:insertHiddenField("popup_downtimereasonhistory_browse","").
   DowntimeOwnerBrowseFrame:insertHiddenField("prog_name","adDowntimeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeOwnerBrowseFrame}
   
   DowntimeOwnerBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   DowntimeOwnerBrowseButtons = NEW buttonBar().
   DowntimeOwnerBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   DowntimeOwnerBrowseButtons:addButton("downtimeowner_browse_form_btn_groups",
                                        fTL("Groups"),
                                        "viewDowntimeGroup('downtimegroup_browse_form');",
                                        (IF intSelectedDowntimeOwner > 0 THEN "" ELSE "Disabled")).
                                         
   DowntimeOwnerBrowseButtons:addButton("downtimeowner_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewDowntimeOwnerDetails('downtimeowner_details_form');",
                                        (IF intSelectedDowntimeOwner > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
   DowntimeOwnerBrowseButtons:addButton("downtimeowner_browse_form_btn_create",
                                        fTL("Create"),
                                        "createDowntimeOwner('downtimeowner_details_form');",
                                        "").
   END.
   
   DowntimeOwnerBrowseButtons:addButton("downtimeowner_browse_form_btn_history",
                                        fTL("History"),
                                        "viewDowntimeOwnerHistory('downtimeownerhistory_browse_form');",
                                        (IF intSelectedDowntimeOwner > 0 THEN "" ELSE "Disabled")).
   
   DowntimeOwnerBrowseButtons:closeBar().  
   DowntimeOwnerBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pDowntimeOwnerDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimeowner_details_form"}
   ASSIGN chrDisplayFieldList  = "DowntimeOwnerID,OwnerCode,OwnerName,ListingSequence,Active"
          chrEditFieldList     = "OwnerCode,OwnerName,ListingSequence,Active"
          chrNewFieldList      = "OwnerCode,OwnerName,ListingSequence,Active"
          chrRequiredFieldList = "OwnerCode,OwnerName,ListingSequence,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   DowntimeOwnerDetailsForm = NEW dataForm("downtimeowner_details_form").
   DowntimeOwnerDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   DowntimeOwnerDetailsForm:FormAction = "dbDowntimeOwnerUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DowntimeOwnerDetailsForm:FormWidth  = 350.
   DowntimeOwnerDetailsForm:FormHeight = 200.
   DowntimeOwnerDetailsForm:FormTitle  = "Downtime Owner Details".
   DowntimeOwnerDetailsForm:FormType   = "small_wide".
   
   /* Column Layout */
   DowntimeOwnerDetailsForm:insertPaddingColumn(10).
   DowntimeOwnerDetailsForm:insertColumn(150).
   DowntimeOwnerDetailsForm:insertColumn(120).
   DowntimeOwnerDetailsForm:insertColumn(20).
   DowntimeOwnerDetailsForm:insertColumn(4).
   DowntimeOwnerDetailsForm:insertColumn(110).  
   
    /* Fields */
   DowntimeOwnerDetailsForm:startRow().
   DowntimeOwnerDetailsForm:insertLabel(fTL("Downtime Owner ID")).
   DowntimeOwnerDetailsForm:insertTextField("DowntimeOwnerID", "", 110, TRUE).
   
   DowntimeOwnerDetailsForm:startRow().
   DowntimeOwnerDetailsForm:insertLabel(fTL("Owner Code")).
   DowntimeOwnerDetailsForm:insertTextField("OwnerCode", "", 110, TRUE).
   
   DowntimeOwnerDetailsForm:startRow().
   DowntimeOwnerDetailsForm:insertLabel(fTL("Owner Name ")).
   DowntimeOwnerDetailsForm:insertTextField("OwnerName", "", 110, TRUE).
      
   DowntimeOwnerDetailsForm:startRow().
   DowntimeOwnerDetailsForm:insertLabel(fTL("Listing Sequence")).
   DowntimeOwnerDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).
   
   DowntimeOwnerDetailsForm:startRow().
   DowntimeOwnerDetailsForm:insertLabel(fTL("Active")). 
   DowntimeOwnerDetailsForm:insertComboField("Active", "", 110, TRUE).  
   DowntimeOwnerDetailsForm:insertComboPairs("Active", "yes", "Active").
   DowntimeOwnerDetailsForm:insertComboPairs("Active", "no",  "Not Active").


   {webGetOptionalFormFields.i pDowntimeOwnerDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   DowntimeOwnerDetailsForm:insertHiddenField("downtimeowner_browse_scroll", "").
   DowntimeOwnerDetailsForm:insertHiddenField("form_name", "downtimeowner_details_form").
   DowntimeOwnerDetailsForm:insertHiddenField("prog_name", "adDowntimeAdmin.p").
   
   DowntimeOwnerDetailsForm:insertHiddenField("popup_downtimeowner_browse", "").
   DowntimeOwnerDetailsForm:insertHiddenField("popup_downtimeowner_details_form", "").
   DowntimeOwnerDetailsForm:insertHiddenField("DowntimeOwnerID", STRING(intSelectedDowntimeOwner)).
   DowntimeOwnerDetailsForm:insertHiddenField("DowntimeGroupID", STRING(intSelectedDowntimeGroup)).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeOwnerDetailsForm}
   
   /* Create Button Bar */
   DowntimeOwnerDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataUpdates THEN
   DO:
   DowntimeOwnerDetailsButtons:addButton("downtimeowner_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateDowntimeOwner('downtimeowner_details_form');").
   END.
   DowntimeOwnerDetailsButtons:addButton("downtimeowner_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "cancelUpdate('UserCancelled','process_mode');" + 
                                         "disablePopup('downtimeowner_details_form_popup');").
   DowntimeOwnerDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeOwnerDetailsForm:FormButtons = DowntimeOwnerDetailsButtons.
   
   DowntimeOwnerDetailsForm:endForm(). 
   
   DowntimeOwnerDetailsForm:displayForm(). 
  
END PROCEDURE.

PROCEDURE pDowntimeOwnerDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      DowntimeOwnerDetailsForm:startRow().
      DowntimeOwnerDetailsForm:insertLabel(fTL("Field Label")).
      DowntimeOwnerDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pDowntimeOwnerHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "downtimeownerhistory_details_form"}
   ASSIGN chrDisplayFieldList  = "DowntimeOwnerHistoryID,DowntimeOwnerID,OwnerCode,OwnerName,"
                                 + "Active,ListingSequence,CreatedDate,CreatedHour,CreatedMins,GateUserID,OperationTypeID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   DowntimeOwnerHistoryDetailsForm = NEW dataForm("downtimeownerhistory_details_form").
   DowntimeOwnerHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   DowntimeOwnerHistoryDetailsForm:FormAction = "dbDowntimeOwnerUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DowntimeOwnerHistoryDetailsForm:FormWidth   = 350.
   DowntimeOwnerHistoryDetailsForm:FormHeight  = 200.
   DowntimeOwnerHistoryDetailsForm:FormTitle   = "Downtime Owner History Details".
   DowntimeOwnerHistoryDetailsForm:FormType    = "small_wide".
   
   /* Column Layout */
   DowntimeOwnerHistoryDetailsForm:insertPaddingColumn(30).
   DowntimeOwnerHistoryDetailsForm:insertColumn(150).
   DowntimeOwnerHistoryDetailsForm:insertColumn(125).
   DowntimeOwnerHistoryDetailsForm:insertColumn(20).
   DowntimeOwnerHistoryDetailsForm:insertColumn(4).
   DowntimeOwnerHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   DowntimeOwnerHistoryDetailsForm:startRow().
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("History ID")).
   DowntimeOwnerHistoryDetailsForm:insertTextField("DowntimeOwnerHistoryID", "", 100, TRUE).
   
   DowntimeOwnerHistoryDetailsForm:startRow().
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("Downtime Owner ID")).
   DowntimeOwnerHistoryDetailsForm:insertTextField("DowntimeOwnerID", "", 100, TRUE).
   
   DowntimeOwnerHistoryDetailsForm:startRow().
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("Owner Code")).
   DowntimeOwnerHistoryDetailsForm:insertTextField("OwnerCode", "", 100, TRUE).
   
   DowntimeOwnerHistoryDetailsForm:startRow().
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("Owner Name")).
   DowntimeOwnerHistoryDetailsForm:insertTextField("OwnerName", "", 100, TRUE).
      
   DowntimeOwnerHistoryDetailsForm:startRow().
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("Listing Sequence")).
   DowntimeOwnerHistoryDetailsForm:insertTextField("ListingSequence", "", 100, TRUE).

   DowntimeOwnerHistoryDetailsForm:startRow().                                                                                           
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   DowntimeOwnerHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   DowntimeOwnerHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   DowntimeOwnerHistoryDetailsForm:insertLabel(":").                                                                                     
   DowntimeOwnerHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                                         
   DowntimeOwnerHistoryDetailsForm:startRow().                                                                                           
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   DowntimeOwnerHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/                                                                                                      
      BY OperationType.OperationTypeID:                                                                                                  
      DowntimeOwnerHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                          OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   DowntimeOwnerHistoryDetailsForm:startRow().                                                                                           
   DowntimeOwnerHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   DowntimeOwnerHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*GateUserID*/                                                                                                        
      BY GateUser.FullName:                                                                                                              
      DowntimeOwnerHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pDowntimeOwnerHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   DowntimeOwnerHistoryDetailsForm:insertHiddenField("downtimeownerhistory_browse_scroll", "").
   DowntimeOwnerHistoryDetailsForm:insertHiddenField("form_name", "downtimeownerhistory_details_form").
   DowntimeOwnerHistoryDetailsForm:insertHiddenField("prog_name", "adDowntimeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeOwnerHistoryDetailsForm}
   
   /* Create Button Bar */
   DowntimeOwnerHistoryDetailsButtons = NEW buttonBar().

   DowntimeOwnerHistoryDetailsButtons:addButton("downtimeownerhistory_details_form_btn_cancel", 
                                                fTL("Cancel"), 
                                                "disablePopup('downtimeownerhistory_details_form_popup');").
   DowntimeOwnerHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeOwnerHistoryDetailsForm:FormButtons = DowntimeOwnerHistoryDetailsButtons.
   
   DowntimeOwnerHistoryDetailsForm:endForm(). 
   
   DowntimeOwnerHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pDowntimeOwnerHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      DowntimeOwnerDetailsForm:startRow().
      DowntimeOwnerDetailsForm:insertLabel(fTL("Field Label")).
      DowntimeOwnerDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adDowntimeOwnerAdmin_downtimeowner_details_form.i}*/ COMEBACK START COMEBACK END*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pDowntimeOwnerHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   DowntimeOwnerHistoryBrowseForm           = NEW dataForm("downtimeownerhistory_browse_form").
   DowntimeOwnerHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   DowntimeOwnerHistoryBrowseForm:FormWidth  = 860.
   DowntimeOwnerHistoryBrowseForm:FormHeight = 530.
   DowntimeOwnerHistoryBrowseForm:FormTitle  = fTL("Downtime Owner History") + (IF intSelectedDowntimeOwner > 0 THEN " : " 
                                                                                + STRING(intSelectedDowntimeOwner) ELSE "").
   DowntimeOwnerHistoryBrowseForm:FormType   = "xxl_large".
   DowntimeOwnerHistoryBrowse                = NEW browseTable("downtimeownerhistory_browse").
   DowntimeOwnerHistoryBrowse:BrowseWidth    = 840.
   DowntimeOwnerHistoryBrowse:BrowseHeight   = 490.
   
   DowntimeOwnerHistoryBrowse:insertColumn(fTL("History ID"), 60, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DowntimeOwnerHistory}

   DowntimeOwnerHistoryBrowse:insertColumn(fTL("Owner Code"),       150, "CHARACTER", "LEFT",   FALSE).
   DowntimeOwnerHistoryBrowse:insertColumn(fTL("Owner Name"),       150, "CHARACTER", "LEFT",   FALSE).
   DowntimeOwnerHistoryBrowse:insertColumn(fTL("Listing Sequence"), 150, "INTEGER",   "CENTER", FALSE).
   DowntimeOwnerHistoryBrowse:insertColumn(fTL("Active"),            50, "LOGICAL",   "CENTER", FALSE).
   DowntimeOwnerHistoryBrowse:insertColumn(fTL("Gate User"),        100, "LOGICAL",   "LEFT",   FALSE).
   DowntimeOwnerHistoryBrowse:insertColumn(fTL("Created"),          160, "LOGICAL",   "LEFT",   FALSE).
   
   DowntimeOwnerHistoryBrowse:StartBody().
   
   FOR EACH DowntimeOwnerHistory NO-LOCK /*idx=SDowntimeOwnerID and DowntimeOwnerHistoryID*/
      WHERE DowntimeOwnerHistory.DowntimeOwnerID = intSelectedDowntimeOwner
         BY DowntimeOwnerHistory.DowntimeOwnerHistoryID DESC:
          
      FIND FIRST OperationType OF DowntimeOwnerHistory NO-LOCK NO-ERROR. /*idx=OperationTypeID*/
      FIND FIRST GateUser      OF DowntimeOwnerHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/

      DowntimeOwnerHistoryBrowse:startRow (DowntimeOwnerHistory.DowntimeOwnerHistoryID,
         "selectDowntimeOwnerHistoryRow(this," + '"' + STRING(DowntimeOwnerHistory.DowntimeOwnerHistoryID) + '"' + ");", "").
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i DowntimeOwnerHistory}
      
      DowntimeOwnerHistoryBrowse:insertData(STRING(DowntimeOwnerHistory.DowntimeOwnerHistoryID),  "LEFT").
      DowntimeOwnerHistoryBrowse:insertData(STRING(DowntimeOwnerHistory.OwnerCode),               "LEFT").
      DowntimeOwnerHistoryBrowse:insertData(STRING(DowntimeOwnerHistory.OwnerName),               "LEFT").
      DowntimeOwnerHistoryBrowse:insertData(STRING(DowntimeOwnerHistory.ListingSequence),         "CENTER").
      DowntimeOwnerHistoryBrowse:insertData(STRING(DowntimeOwnerHistory.Active, "Yes/No"),        "CENTER").
      DowntimeOwnerHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "LEFT").
      DowntimeOwnerHistoryBrowse:insertData(fDisplayDate&Time(DowntimeOwnerHistory.Created, "y/m/d H:M:S"), "LEFT").

      DowntimeOwnerHistoryBrowse:endRow().
   END. /* FOR EACH DowntimeOwnerHistory */
   
   DowntimeOwnerHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + DowntimeOwnerHistoryBrowse:getErrors().
   
   DowntimeOwnerHistoryBrowseForm:insertHiddenField("popup_downtimeownerhistory_browse","").
   DowntimeOwnerHistoryBrowseForm:insertHiddenField("DowntimeOwnerHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DowntimeOwnerHistoryBrowseForm}
   
   /* Create Button Bar */
   DowntimeOwnerHistoryButtons = NEW buttonBar().
   
   DowntimeOwnerHistoryButtons:addButton("downtimeownerhistory_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewDowntimeOwnerHistoryDetails('downtimeownerhistory_details_form');",
                                         (IF intSelectedDowntimeOwnerHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   DowntimeOwnerHistoryButtons:addButton("downtimeownerhistory_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('downtimeownerhistory_browse_form_popup');").
   DowntimeOwnerHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DowntimeOwnerHistoryBrowseForm:FormBrowse  = DowntimeOwnerHistoryBrowse.
   DowntimeOwnerHistoryBrowseForm:FormButtons = DowntimeOwnerHistoryButtons.
   DowntimeOwnerHistoryBrowseForm:endForm(). 
   
   DowntimeOwnerHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


