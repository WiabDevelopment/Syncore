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
DEFINE VARIABLE intSelectedLocationTypeGroup    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectLocationTypeGroupRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToLocationTypeGroupRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLocationTypeGroupID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedLocationTypeGroupLink    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectLocationTypeGroupLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToLocationTypeGroupLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLocationTypeGroupLinkID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupLocationTypeGroupLinks      AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE LocationTypeGroupBrowseFrame    AS pageFrame.
DEFINE VARIABLE LocationTypeGroupBrowse         AS browseTable.
DEFINE VARIABLE LocationTypeGroupBrowseButtons  AS buttonBar.
DEFINE VARIABLE LocationTypeGroupDetailsForm    AS dataForm.
DEFINE VARIABLE LocationTypeGroupDetailsButtons AS buttonBar.

DEFINE VARIABLE LocationTypeGroupLinkBrowseFrame    AS pageFrame.
DEFINE VARIABLE LocationTypeGroupLinkBrowse         AS browseTable.
DEFINE VARIABLE LocationTypeGroupLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE LocationTypeGroupLinkDetailsForm    AS dataForm.
DEFINE VARIABLE LocationTypeGroupLinkDetailsButtons AS buttonBar.
DEFINE VARIABLE LocationTypeGroupLinkBrowseForm     AS dataForm.

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

&IF DEFINED(EXCLUDE-pLocationTypeGroupBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeGroupBrowse Procedure 
PROCEDURE pLocationTypeGroupBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "locationtypegroup_details_form"}
   
   LocationTypeGroupBrowse              = NEW browseTable("locationtypegroup_browse").
   LocationTypeGroupBrowse:BrowseWidth  = 965.
   LocationTypeGroupBrowse:BrowseHeight = 455.
   LocationTypeGroupBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   LocationTypeGroupBrowse:insertColumn(fTL("GroupID"),           60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LocationTypeGroup}
   
   LocationTypeGroupBrowse:insertColumn(fTL("List Seq"),           60, "INTEGER", FALSE).
   LocationTypeGroupBrowse:insertColumn(fTL("Group Code"),        150, "CHARACTER", "left", FALSE).
   LocationTypeGroupBrowse:insertColumn(fTL("Group Name"),        150, "CHARACTER", "left", FALSE).
   LocationTypeGroupBrowse:insertColumn(fTL("Group Description"), 220, "CHARACTER", "left", FALSE).
   LocationTypeGroupBrowse:insertColumn(fTL("Active"),             50, "LOGICAL", FALSE).
   
   /*Body*/
   LocationTypeGroupBrowse:startBody().
   
   /* Find all Location Type Groups then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH LocationTypeGroup NO-LOCK
      BY    LocationTypeGroup.Active DESCENDING
      BY    LocationTypeGroup.ListingSequence 
      BY    LocationTypeGroup.LocationTypeGroupID:
      
      LocationTypeGroupBrowse:startRow(LocationTypeGroup.LocationTypeGroupID, "selectLocationTypeGroupRow(this," + '"' 
                                                                     + STRING(LocationTypeGroup.LocationTypeGroupID) + '"' + ");", "").
      LocationTypeGroupBrowse:insertData(LocationTypeGroup.LocationTypeGroupID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i LocationTypeGroup}
      
      LocationTypeGroupBrowse:insertData(STRING(LocationTypeGroup.ListingSequence)).
      LocationTypeGroupBrowse:insertData(LocationTypeGroup.GroupCode, "left").
      LocationTypeGroupBrowse:insertData(LocationTypeGroup.GroupName, "left").
      LocationTypeGroupBrowse:insertData(LocationTypeGroup.GroupDescr, "left").
      LocationTypeGroupBrowse:insertData(STRING(LocationTypeGroup.Active, "Yes/No")).
      
      /* Add hidden fields */
      LocationTypeGroupBrowse:insertHiddenData("LocationTypeGroupVersionID",LocationTypeGroup.VersionID).
      
      LocationTypeGroupBrowse:endRow().
      
   END. /* FOR EACH LocationTypeGroup NO-LOCK */
   
   LocationTypeGroupBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationTypeGroupBrowse:getErrors().
   
   /* Create a new frame */
   LocationTypeGroupBrowseFrame           = NEW pageFrame().
   LocationTypeGroupBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LocationTypeGroupBrowseFrame:FormAction="dbLocationTypeGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LocationTypeGroupBrowseFrame:formOpen("locationtypegroup_browse_form").
   
   /* Start the Frame Header */
   LocationTypeGroupBrowseFrame:insertSpacer(5).
   LocationTypeGroupBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LocationTypeGroupBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LocationTypeGroupBrowseFrame:frameClose().
   LocationTypeGroupBrowseFrame:insertSpacer(10).
   
   LocationTypeGroupBrowseFrame:insertHiddenField("locationtypegroup_browse_scroll","").
   LocationTypeGroupBrowseFrame:insertHiddenField("LocationTypeGroupID","").
   LocationTypeGroupBrowseFrame:insertHiddenField("LocationTypeGroupVersionID","").
   LocationTypeGroupBrowseFrame:insertHiddenField("locationtypegrouplink_browse_scroll","").
   LocationTypeGroupBrowseFrame:insertHiddenField("popup_locationtypegrouplink_browse","").
   LocationTypeGroupBrowseFrame:insertHiddenField("LocationTypeGroupLinkID","").
   LocationTypeGroupBrowseFrame:insertHiddenField("form_name","locationtypegroup_browse_form").
   LocationTypeGroupBrowseFrame:insertHiddenField("prog_name","adLocationTypeGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationTypeGroupBrowseFrame}
   
   LocationTypeGroupBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   LocationTypeGroupBrowseButtons           = NEW buttonBar().
   LocationTypeGroupBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   LocationTypeGroupBrowseButtons:addButton("locationtypegroup_browse_form_btn_details",
                                            fTL("Details"),
                                            "viewLocationTypeGroupDetails('locationtypegroup_details_form');",
                                            (IF intSelectedLocationTypeGroup > 0 THEN "" ELSE "Disabled")).
   
   LocationTypeGroupBrowseButtons:addButton("locationtypegroup_browse_form_btn_locationtype",
                                            fTL("LocationType"),
                                            "viewLocationTypeGroupLinks('locationtype_details_form');",
                                            (IF intSelectedLocationTypeGroup > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO: 
      LocationTypeGroupBrowseButtons:addButton("locationtypegroup_browse_form_btn_create",
                                               fTL("Create"),
                                               "createLocationTypeGroup('locationtypegroup_details_form');",
                                               "").
                                               
      LocationTypeGroupBrowseButtons:addButton("locationtypegroup_browse_form_btn_applinks",
                                               fTL("App Links"),
                                               "viewAppLinkBrowse('locationtypegroup_browse_form','LocationTypeGroup');",
                                               "Disabled").
                                               
   END.                                            
   
   LocationTypeGroupBrowseButtons:closeBar().  
   LocationTypeGroupBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationTypeGroupDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeGroupDetails Procedure 
PROCEDURE pLocationTypeGroupDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "locationtypegroup_details_form"}
   
   ASSIGN chrDisplayFieldList  = "LocationTypeGroupID,ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrEditFieldList     = "ListingSequence,GroupName,GroupDescr,Active"
          chrNewFieldList      = "ListingSequence,GroupCode,GroupName,GroupDescr,Active"
          chrRequiredFieldList = "ListingSequence,GroupCode,GroupName,GroupDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   LocationTypeGroupDetailsForm           = NEW dataForm("locationtypegroup_details_form").
   LocationTypeGroupDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationTypeGroupDetailsForm:FormAction = "dbLocationTypeGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationTypeGroupDetailsForm:FormWidth  = 460.
   LocationTypeGroupDetailsForm:FormHeight = 300.
   LocationTypeGroupDetailsForm:FormTitle  = "LocationTypeGroup Details".
   LocationTypeGroupDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   LocationTypeGroupDetailsForm:insertPaddingColumn(10).
   LocationTypeGroupDetailsForm:insertColumn(100).
   LocationTypeGroupDetailsForm:insertColumn(120).
   LocationTypeGroupDetailsForm:insertColumn(20).
   LocationTypeGroupDetailsForm:insertColumn(4).
   LocationTypeGroupDetailsForm:insertColumn(110).
   
   /* Fields */
   LocationTypeGroupDetailsForm:startRow().
   LocationTypeGroupDetailsForm:insertLabel("Group ID").
   LocationTypeGroupDetailsForm:insertTextField("LocationTypeGroupID", "", 110, TRUE).  
   
   LocationTypeGroupDetailsForm:startRow().
   LocationTypeGroupDetailsForm:insertLabel("Listing Seq").
   LocationTypeGroupDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   LocationTypeGroupDetailsForm:startRow().
   LocationTypeGroupDetailsForm:insertLabel("Group Code").
   LocationTypeGroupDetailsForm:insertTextField("GroupCode", "", 150, TRUE).  
   
   LocationTypeGroupDetailsForm:startRow().
   LocationTypeGroupDetailsForm:insertLabel("Group Name").
   LocationTypeGroupDetailsForm:insertTextField("GroupName", "", 150, TRUE).
   
   LocationTypeGroupDetailsForm:startRow().
   LocationTypeGroupDetailsForm:insertLabel("Group Descr").
   LocationTypeGroupDetailsForm:insertTextField("GroupDescr", "", 300, TRUE).

   LocationTypeGroupDetailsForm:startRow().
   LocationTypeGroupDetailsForm:insertLabel(fTL("Active")). 
   LocationTypeGroupDetailsForm:insertComboField("Active", "", 110, TRUE).  
   LocationTypeGroupDetailsForm:insertComboPairs("Active", "yes", "Active").
   LocationTypeGroupDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pLocationTypeGroupDetailsFields}
   
   /* Add Hidden Fields*/
   LocationTypeGroupDetailsForm:insertHiddenField("locationtypegroup_browse_scroll", "").
   LocationTypeGroupDetailsForm:insertHiddenField("form_name", "locationtypegroup_details_form").
   LocationTypeGroupDetailsForm:insertHiddenField("prog_name", "adLocationTypeGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationTypeGroupDetailsForm}
   
   /* Create Button Bar */
   LocationTypeGroupDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      LocationTypeGroupDetailsButtons:addButton("locationtypegroup_details_form_btn_save", 
                                                fTL("Save"), 
                                                "updateLocationTypeGroup('locationtypegroup_details_form');").
   
   LocationTypeGroupDetailsButtons:addButton("locationtypegroup_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); disablePopup('locationtypegroup_details_form_popup');").
   
   LocationTypeGroupDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationTypeGroupDetailsForm:FormButtons = LocationTypeGroupDetailsButtons.
   
   LocationTypeGroupDetailsForm:endForm(). 
   
   LocationTypeGroupDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationTypeGroupDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeGroupDetailsFields Procedure 
PROCEDURE pLocationTypeGroupDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         LocationTypeGroupDetailsForm:startRow().
         LocationTypeGroupDetailsForm:insertLabel(fTL("Field Label")).
         LocationTypeGroupDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adLocationTypeGroupAdmin_locationtypegroup_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationTypeGroupLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeGroupLinkBrowse Procedure 
PROCEDURE pLocationTypeGroupLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   
   {webGetWebForm.i "locationtypegrouplink_details_form"}
   
   LocationTypeGroupLinkBrowseForm = NEW dataForm("locationtypegrouplink_browse_form").
   LocationTypeGroupLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationTypeGroupLinkBrowseForm:FormAction  = "dbLocationTypeGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationTypeGroupLinkBrowseForm:FormWidth   = 700.
   LocationTypeGroupLinkBrowseForm:FormHeight  = 490.
   LocationTypeGroupLinkBrowseForm:FormTitle   = fTL("LocationTypes for LocationTypeGroup") + 
                                               (IF AVAILABLE LocationTypeGroup THEN " : " + LocationTypeGroup.GroupName ELSE "").
   LocationTypeGroupLinkBrowseForm:FormType    = "xl_large".
   
   LocationTypeGroupLinkBrowse = NEW browseTable("locationtypegrouplink_browse").
   LocationTypeGroupLinkBrowse:BrowseWidth  = 680.
   LocationTypeGroupLinkBrowse:BrowseHeight = 432.
   
   LocationTypeGroupLinkBrowse:insertColumn(fTL("LinkID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LocationTypeGroupLink}
   
   LocationTypeGroupLinkBrowse:insertColumn(fTL("LocationType Code"), 120, "CHARACTER", "left", FALSE).
   LocationTypeGroupLinkBrowse:insertColumn(fTL("LocationType Description"), 140, "CHARACTER", "left", FALSE).
   LocationTypeGroupLinkBrowse:insertColumn(fTL("Active"), 80, "CHARACTER", FALSE).
   
   LocationTypeGroupLinkBrowse:StartBody().
   
   IF AVAILABLE LocationTypeGroup THEN
   DO:
      /* List the LocationTypeGroupLinks for the LocationTypeGroup */
      FOR EACH LocationTypeGroupLink OF LocationTypeGroup NO-LOCK
         BY LocationTypeGroupLink.LocationTypeGroupID:
         
         FIND FIRST LocationType OF LocationTypeGroupLink NO-LOCK NO-ERROR.

         LocationTypeGroupLinkBrowse:startRow(LocationTypeGroupLink.LocationTypeGroupLinkID, 
                                              "selectLocationTypeGroupLinkRow(this," + '"' + 
                                              STRING(LocationTypeGroupLink.LocationTypeGroupLinkID) + 
                                              '","adLocationTypeGroupAdmin.p","locationtypegrouplink_browse_form"' + ");", "").
         LocationTypeGroupLinkBrowse:insertData(LocationTypeGroupLink.LocationTypeGroupLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i LocationTypeGroupLink}
         
         LocationTypeGroupLinkBrowse:insertData(IF AVAILABLE LocationType THEN LocationType.TypeCode ELSE "", "left").
         LocationTypeGroupLinkBrowse:insertData(IF AVAILABLE LocationType THEN LocationType.TypeDesc ELSE "", "left").
         LocationTypeGroupLinkBrowse:insertData(STRING(LocationTypeGroupLink.Active,"Yes/No")).
         
         /* Add hidden fields */
         LocationTypeGroupLinkBrowse:insertHiddenData("LocationTypeGroupID",LocationTypeGroupLink.LocationTypeGroupID).
         LocationTypeGroupLinkBrowse:insertHiddenData("LocationTypeID",(IF AVAILABLE LocationType THEN STRING(LocationType.LocationTypeID) ELSE "")).
         LocationTypeGroupLinkBrowse:insertHiddenData("TypeCode",(IF AVAILABLE LocationType THEN LocationType.TypeCode ELSE "")).
         LocationTypeGroupLinkBrowse:insertHiddenData("LocationTypeVersionID",(IF AVAILABLE LocationType THEN STRING(LocationType.VersionID) ELSE "")).
         LocationTypeGroupLinkBrowse:insertHiddenData("LocationTypeGroupLinkID",LocationTypeGroupLink.LocationTypeGroupLinkID).
         LocationTypeGroupLinkBrowse:insertHiddenData("LocationTypeGroupLinkVersionID",LocationTypeGroupLink.VersionID).
         LocationTypeGroupLinkBrowse:endRow().
      
      END. /* FOR EACH LocationTypeGroupLink OF LocationTypeGroup NO-LOCK */
   END. /*IF AVAILABLE LocationTypeGroup THEN*/
   
   LocationTypeGroupLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationTypeGroupLinkBrowse:getErrors().
   
   LocationTypeGroupLinkBrowseForm:insertHiddenField("LocationTypeGroupID","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("GroupName","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("LocationTypeID","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("LocationTypeVersionID","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("LocationTypeCode","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("LocationTypeGroupLinkID","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("LocationTypeGroupLinkVersionID","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("locationtypegroup_browse_scroll","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("locationtypegrouplink_browse_scroll","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("popup_locationtypegrouplink_browse","").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("form_name","locationtypegrouplink_browse_form").
   LocationTypeGroupLinkBrowseForm:insertHiddenField("prog_name","adLocationTypeGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationTypeGroupLinkBrowseForm}
   
   /* Create Button Bar */
   LocationTypeGroupLinkBrowseButtons = NEW buttonBar().
   
   LocationTypeGroupLinkBrowseButtons:addButton("locationtypegrouplink_browse_form_btn_create",
                                                fTL("Create"),
                                                "createLocationTypeGroupLink('locationtypegrouplink_details_form');"). 
   
   LocationTypeGroupLinkBrowseButtons:addButton("locationtypegrouplink_browse_form_btn_view",
                                                fTL("Details"),
                                                "viewLocationTypeGroupLinkDetails('locationtypegrouplink_details_form');",
                                                (IF intSelectedLocationTypeGroupLink > 0 THEN "" ELSE "Disabled")).
   
   LocationTypeGroupLinkBrowseButtons:addButton("locationtypegrouplink_browse_form_btn_delete",
                                                fTL("Delete"),
                                                "confirmDeleteLocationTypeGroupLink('locationtypegrouplink_browse_form');", 
                                                (IF intSelectedLocationTypeGroupLink > 0 THEN "" ELSE "Disabled")).
   
   LocationTypeGroupLinkBrowseButtons:addButton("locationtypegrouplink_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('locationtypegrouplink_browse_form_popup');").
   
   LocationTypeGroupLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationTypeGroupLinkBrowseForm:FormBrowse  = LocationTypeGroupLinkBrowse.
   LocationTypeGroupLinkBrowseForm:FormButtons = LocationTypeGroupLinkBrowseButtons.
   LocationTypeGroupLinkBrowseForm:endForm(). 
   
   LocationTypeGroupLinkBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationTypeGroupLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeGroupLinkDetails Procedure 
PROCEDURE pLocationTypeGroupLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "locationtypegrouplink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "LocationTypeGroupLinkID,LocationTypeID,Active"
          chrEditFieldList     = "Active"
          chrNewFieldList      = "LocationTypeID,Active"
          chrRequiredFieldList = "LocationTypeID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   LocationTypeGroupLinkDetailsForm = NEW dataForm("locationtypegrouplink_details_form").
   LocationTypeGroupLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationTypeGroupLinkDetailsForm:FormAction  = "dbLocationTypeGroupLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationTypeGroupLinkDetailsForm:FormWidth   = 460.
   LocationTypeGroupLinkDetailsForm:FormHeight  = 300.
   LocationTypeGroupLinkDetailsForm:FormTitle   = "Location Type Group Link Details".
   LocationTypeGroupLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   LocationTypeGroupLinkDetailsForm:insertPaddingColumn(70).
   LocationTypeGroupLinkDetailsForm:insertColumn(120).
   LocationTypeGroupLinkDetailsForm:insertColumn(120).
   LocationTypeGroupLinkDetailsForm:insertColumn(30).
   LocationTypeGroupLinkDetailsForm:insertColumn(120).  
   
   /* Fields */
   LocationTypeGroupLinkDetailsForm:startRow().
   LocationTypeGroupLinkDetailsForm:insertLabel(fTL("Link ID")).
   LocationTypeGroupLinkDetailsForm:insertTextField("LocationTypeGroupLinkID", "", 110, TRUE).    
   
   LocationTypeGroupLinkDetailsForm:startRow().
   LocationTypeGroupLinkDetailsForm:insertLabel(fTL("Location Type")).
   LocationTypeGroupLinkDetailsForm:insertComboField("LocationTypeID", "", 200, TRUE). 
   FOR EACH LocationType NO-LOCK /*idx=ActiveListingSequence*/
      BY LocationType.Active DESC
      BY LocationType.TypeCode:
      
      LocationTypeGroupLinkDetailsForm:insertComboPairs("LocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeCode).
   END.   
   
   LocationTypeGroupLinkDetailsForm:startRow().
   LocationTypeGroupLinkDetailsForm:insertLabel(fTL("Active")). 
   LocationTypeGroupLinkDetailsForm:insertComboField("Active", "", 110, TRUE).  
   LocationTypeGroupLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   LocationTypeGroupLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pLocationTypeGroupLinkDetailsFields}
   
   /* Add Hidden Fields*/
   LocationTypeGroupLinkDetailsForm:insertHiddenField("locationtypegroup_browse_scroll",    "").
   LocationTypeGroupLinkDetailsForm:insertHiddenField("popup_locationtypegrouplink_browse", "").
   LocationTypeGroupLinkDetailsForm:insertHiddenField("LocationTypeGroupID",                STRING(intSelectedLocationTypeGroup)).
   LocationTypeGroupLinkDetailsForm:insertHiddenField("LocationTypeGroupLinkID",            STRING(intSelectedLocationTypeGroupLink)).
   LocationTypeGroupLinkDetailsForm:insertHiddenField("LocationTypeID",                     "").
   LocationTypeGroupLinkDetailsForm:insertHiddenField("form_name",                          "locationtypegrouplink_details_form").
   LocationTypeGroupLinkDetailsForm:insertHiddenField("prog_name",                          "adLocationTypeGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationTypeGroupLinkDetailsForm}
   
   /* Create Button Bar */
   LocationTypeGroupLinkDetailsButtons = NEW buttonBar().
   
   LocationTypeGroupLinkDetailsButtons:addButton("locationtypegrouplink_details_form_btn_save", 
                                                 fTL("Save"), 
                                                 "updateLocationTypeGroupLink('locationtypegrouplink_details_form');").
   
   LocationTypeGroupLinkDetailsButtons:addButton("locationtypegrouplink_details_form_btn_cancel", 
                                                 fTL("Cancel"), 
                                                 "cancelUpdate('UserCancelled','process_mode'); " 
                                                 + "disablePopup('locationtypegrouplink_details_form_popup');").
   LocationTypeGroupLinkDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   LocationTypeGroupLinkDetailsForm:FormButtons = LocationTypeGroupLinkDetailsButtons.
   
   LocationTypeGroupLinkDetailsForm:endForm(). 
   LocationTypeGroupLinkDetailsForm:displayForm(). 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationTypeGroupLinkDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeGroupLinkDetailsFields Procedure 
PROCEDURE pLocationTypeGroupLinkDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adLocationTypeGroupAdmin_locationtypegrouplink_details_form.i}
      
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
   
   ASSIGN chrLocationTypeGroupID              = get-value("LocationTypeGroupID")
          intSelectedLocationTypeGroup        = INTEGER(chrLocationTypeGroupID)
          chrScrollToLocationTypeGroupRow     = STRING(INTEGER(get-value("locationtypegroup_browse_scroll"))) + ";"
          chrLocationTypeGroupLinkID          = get-value("LocationTypeGroupLinkID")
          intSelectedLocationTypeGroupLink    = INTEGER(chrLocationTypeGroupLinkID)
          chrScrollToLocationTypeGroupLinkRow = STRING(INTEGER(get-value("locationtypegrouplink_browse_scroll"))) + ";".

   /* Process URL values */
   IF chrLocationTypeGroupID <> "" THEN
      chrSelectLocationTypeGroupRow = 'selectLocationTypeGroupRow(document.getElementById("locationtypegroup_browse_row_' + chrLocationTypeGroupID + '"),"' 
                                                         + chrLocationTypeGroupID +  '");'.
   
   IF chrLocationTypeGroupLinkID <> "" THEN
      chrSelectLocationTypeGroupLinkRow = 'selectLocationTypeGroupLinkRow(document.getElementById("locationtypegrouplink_browse_row_' 
                                           + chrLocationTypeGroupLinkID + '"),"' + chrLocationTypeGroupLinkID +  '");'.

   IF get-value('popup_locationtypegrouplink_browse') = "yes" THEN 
      chrPopupLocationTypeGroupLinks = 'enablePopup("locationtypegrouplink_browse_form_popup");'.

   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("locationtypegroup_browse").scrollTop=' + chrScrollToLocationTypeGroupRow 
                             + chrSelectLocationTypeGroupRow + chrSelectLocationTypeGroupLinkRow + chrPopupLocationTypeGroupLinks.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
      
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "LocationTypeGroup Admin".
   ThisPage:FrameTitle    = "LocationTypeGroup Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("locationtypegroup.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pLocationTypeGroupBrowse.
   
   FIND FIRST LocationTypeGroup NO-LOCK
      WHERE LocationTypeGroup.LocationTypeGroupID = intSelectedLocationTypeGroup NO-ERROR.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pLocationTypeGroupDetails.

   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
      
   RUN pLocationTypeGroupLinkBrowse.

   FIND FIRST LocationTypeGroupLink NO-LOCK
      WHERE LocationTypeGroupLink.LocationTypeGroupLinkID = intSelectedLocationTypeGroupLink NO-ERROR.

   RUN pLocationTypeGroupLinkDetails.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT LocationTypeGroupBrowseFrame        NO-ERROR.
   DELETE OBJECT LocationTypeGroupBrowse             NO-ERROR.
   DELETE OBJECT LocationTypeGroupBrowseButtons      NO-ERROR.
   DELETE OBJECT LocationTypeGroupDetailsForm        NO-ERROR.
   DELETE OBJECT LocationTypeGroupDetailsButtons     NO-ERROR.
   DELETE OBJECT LocationTypeGroupLinkBrowseFrame    NO-ERROR.
   DELETE OBJECT LocationTypeGroupLinkBrowse         NO-ERROR.
   DELETE OBJECT LocationTypeGroupLinkBrowseButtons  NO-ERROR.
   DELETE OBJECT LocationTypeGroupLinkDetailsForm    NO-ERROR.
   DELETE OBJECT LocationTypeGroupLinkDetailsButtons NO-ERROR.
   DELETE OBJECT LocationTypeGroupLinkBrowseForm     NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

