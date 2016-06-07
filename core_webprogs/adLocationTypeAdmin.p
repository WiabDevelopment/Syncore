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
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{defDataMigrationVariables.i}


/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedLocationType        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectLocationTypeRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLocationTypeRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLocationTypeID              AS CHARACTER   NO-UNDO.

/* Definitions for System Options for Receiving */
{getLocationOptions.i}

/* Objects */
DEFINE VARIABLE LocationTypeBrowseFrame        AS pageFrame.
DEFINE VARIABLE LocationTypeBrowse             AS browseTable.
DEFINE VARIABLE LocationTypeBrowseButtons      AS buttonBar.
DEFINE VARIABLE LocationTypeDetailsForm        AS dataForm.
DEFINE VARIABLE LocationTypeDetailsButtons     AS buttonBar.

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

&IF DEFINED(EXCLUDE-pLocationTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeBrowse Procedure 
PROCEDURE pLocationTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "locationtype_details_form"}
   
   LocationTypeBrowse = NEW browseTable("locationtype_browse").
   LocationTypeBrowse:BrowseWidth  = 965.
   LocationTypeBrowse:BrowseHeight = 455.
   LocationTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   LocationTypeBrowse:insertColumn(fTL("LocType"), 75, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LocationType}
   
   LocationTypeBrowse:insertColumn(fTL("List Seq"),       60, "INTEGER", FALSE).
   LocationTypeBrowse:insertColumn(fTL("LocType Code"),  120, "CHARACTER", "left", FALSE).
   LocationTypeBrowse:insertColumn(fTL("LocType Name"),  150, "CHARACTER", "left", FALSE).
   LocationTypeBrowse:insertColumn(fTL("LocType Descr"), 220, "CHARACTER", "left", FALSE).
   LocationTypeBrowse:insertColumn(fTL("Picking Seq"),    80, "INTEGER", FALSE).
   LocationTypeBrowse:insertColumn(fTL("Putaway Seq"),    80, "INTEGER", FALSE).
   LocationTypeBrowse:insertColumn(fTL("Active"),         50, "LOGICAL", FALSE).
   
   /*Body*/
   LocationTypeBrowse:startBody().
   
   FOR EACH LocationType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE LocationType.Active
      BY    LocationType.ListingSequence:
      
      LocationTypeBrowse:startRow(LocationType.LocationTypeID, "selectLocationTypeRow(this," + '"' + STRING(LocationType.LocationTypeID) 
                                                                  + '"' + ");", "").
      LocationTypeBrowse:insertData(LocationType.LocationTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i LocationType}
      
      LocationTypeBrowse:insertData(STRING(LocationType.ListingSequence)).
      LocationTypeBrowse:insertData(LocationType.TypeCode, "left").
      LocationTypeBrowse:insertData(LocationType.TypeName, "left").
      LocationTypeBrowse:insertData(LocationType.TypeDescr, "left").
      LocationTypeBrowse:insertData(STRING(LocationType.PickingSequence)).
      LocationTypeBrowse:insertData(STRING(LocationType.PutawaySequence)).
      LocationTypeBrowse:insertData(STRING(LocationType.Active,"Yes/No")).
      
      /* Add hidden fields */
      LocationTypeBrowse:insertHiddenData("LocationTypeVersionID",LocationType.VersionID).
      
      LocationTypeBrowse:endRow().
      
   END. /*FOR EACH LocationType NO-LOCK */
   
   LocationTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationTypeBrowse:getErrors().
   
   /* Create a new frame */
   LocationTypeBrowseFrame = NEW pageFrame().
   LocationTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LocationTypeBrowseFrame:FormAction="dbLocationTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LocationTypeBrowseFrame:formOpen("locationtype_browse_form").
   
   /* Start the Frame Header */
   LocationTypeBrowseFrame:insertSpacer(5).
   LocationTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LocationTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LocationTypeBrowseFrame:frameClose().
   LocationTypeBrowseFrame:insertSpacer(10).
   
   LocationTypeBrowseFrame:insertHiddenField("locationtype_browse_scroll","").
   LocationTypeBrowseFrame:insertHiddenField("LocationTypeID","").
   LocationTypeBrowseFrame:insertHiddenField("LocationTypeVersionID","").
   LocationTypeBrowseFrame:insertHiddenField("form_name","locationtype_browse_form").
   LocationTypeBrowseFrame:insertHiddenField("prog_name","adLocationTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationTypeBrowseFrame}
   
   LocationTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}   
   
   /* Create Button Bar */
   LocationTypeBrowseButtons = NEW buttonBar().
   LocationTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   LocationTypeBrowseButtons:addButton("locationtype_browse_form_btn_details",
                                       fTL("Details"),
                                       "viewLocationTypeDetails('locationtype_details_form');",
                                       (IF intSelectedLocationType > 0 THEN "" ELSE "Disabled")).
   
   
   IF NOT logPreventDataCreates THEN
   DO: 
      LocationTypeBrowseButtons:addButton("locationtype_browse_form_btn_create",
                                          fTL("Create"),
                                          "createLocationType('locationtype_details_form');",
                                          "").

      LocationTypeBrowseButtons:addButton("locationtype_browse_form_btn_applinks",
                                          fTL("App Links"),
                                          "viewAppLinkBrowse('locationtype_browse_form','LocationType');",
                                          "Disabled").
                                     
   END.   
   
   /**
   LocationTypeBrowseButtons:addButton("locationtype_browse_form_btn_delete",
                                       fTL("Delete"),
                                       "confirmDeleteLocationType();",
                                       (IF intSelectedLocationType > 0 THEN "" ELSE "Disabled")).
   **/
   
   LocationTypeBrowseButtons:closeBar().  
   LocationTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeDetails Procedure 
PROCEDURE pLocationTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "locationtype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "LocationTypeID,ListingSequence,TypeCode,TypeName,TypeDescr,PickingSequence,PutawaySequence" 
                                    + ",AutoReplenishLocType,AutoPutawayLocType,LooseUnitStorage,Active,StoreMultiPart"
                                    + ",StoreMultiStockStatus"
          chrEditFieldList     = "ListingSequence,TypeName,TypeDescr,PickingSequence,PutawaySequence,AutoReplenishLocType" 
                                    + ",AutoPutawayLocType,Active,StoreMultiPart,StoreMultiStockStatus"
          chrNewFieldList      = "TypeCode,ListingSequence,TypeName,TypeDescr,PickingSequence,PutawaySequence,AutoReplenishLocType" 
                                    + ",AutoPutawayLocType,LooseUnitStorage,Active,StoreMultiPart,StoreMultiStockStatus"
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   LocationTypeDetailsForm = NEW dataForm("locationtype_details_form").
   LocationTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationTypeDetailsForm:FormAction = "dbLocationTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationTypeDetailsForm:FormWidth   = 580.
   LocationTypeDetailsForm:FormHeight  = 420.
   LocationTypeDetailsForm:FormTitle   = "LocationType Details".
   LocationTypeDetailsForm:FormType    = "large".
   
   /* Column Layout */
   LocationTypeDetailsForm:insertPaddingColumn(30).
   LocationTypeDetailsForm:insertColumn(150).
   LocationTypeDetailsForm:insertColumn(160).
   LocationTypeDetailsForm:insertColumn(20).
   LocationTypeDetailsForm:insertColumn(4).
   LocationTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel("LocType ID").
   LocationTypeDetailsForm:insertTextField("LocationTypeID", "", 110, TRUE).  
   
   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel("Listing Seq").
   LocationTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel("LocType Code").
   LocationTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel("LocType Name").
   LocationTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).
   
   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel("LocType Descr").
   LocationTypeDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).

   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel("Picking Seq").
   LocationTypeDetailsForm:insertTextField("PickingSequence", "", 110, TRUE).

   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel("Putaway Seq").
   LocationTypeDetailsForm:insertTextField("PutawaySequence", "", 110, TRUE).
   
   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel(fTL("Auto Replen")). 
   LocationTypeDetailsForm:insertComboField("AutoReplenishLocType", "", 150, TRUE).  
   LocationTypeDetailsForm:insertComboPairs("AutoReplenishLocType", "yes", "Auto Replen").
   LocationTypeDetailsForm:insertComboPairs("AutoReplenishLocType", "no",  "Not Auto Replen").
   
   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel(fTL("Auto Putaway")). 
   LocationTypeDetailsForm:insertComboField("AutoPutawayLocType", "", 150, TRUE).  
   LocationTypeDetailsForm:insertComboPairs("AutoPutawayLocType", "yes", "Auto Putaway").
   LocationTypeDetailsForm:insertComboPairs("AutoPutawayLocType", "no",  "Not Auto Putaway").

   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel(fTL("Loose Unit Storage")). 
   LocationTypeDetailsForm:insertComboField("LooseUnitStorage", "", 110, TRUE).  
   LocationTypeDetailsForm:insertComboPairs("LooseUnitStorage", "no",  "Not Loose Unit Storage").
   LocationTypeDetailsForm:insertComboPairs("LooseUnitStorage", "yes", "Loose Unit Storage").

   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel(fTL("Store Multi Part")).
   LocationTypeDetailsForm:insertComboField("StoreMultiPart", "", 200, TRUE).
   LocationTypeDetailsForm:insertComboPairs("StoreMultiPart", "yes", "Allow Multi Part").
   LocationTypeDetailsForm:insertComboPairs("StoreMultiPart", "no",  "Do Not Allow Multi Part").

   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel(fTL("Store Multi Stock Status")).
   LocationTypeDetailsForm:insertComboField("StoreMultiStockStatus", "", 200, TRUE).
   LocationTypeDetailsForm:insertComboPairs("StoreMultiStockStatus", "yes", "Allow Multi Stock Status").
   LocationTypeDetailsForm:insertComboPairs("StoreMultiStockStatus", "no",  "Do Not Allow Multi Stock Status").

   LocationTypeDetailsForm:startRow().
   LocationTypeDetailsForm:insertLabel(fTL("Active")). 
   LocationTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   LocationTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   LocationTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pLocationTypeDetailsFields}
   
   /* Add Hidden Fields*/
   LocationTypeDetailsForm:insertHiddenField("locationtype_browse_scroll", "").
   LocationTypeDetailsForm:insertHiddenField("form_name", "locationtype_details_form").
   LocationTypeDetailsForm:insertHiddenField("prog_name", "adLocationTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationTypeDetailsForm}
   
   /* Create Button Bar */
   LocationTypeDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:
      LocationTypeDetailsButtons:addButton("locationtype_details_form_btn_save", 
                                           fTL("Save"), 
                                           "updateLocationType('locationtype_details_form');").
   END.    
   
   LocationTypeDetailsButtons:addButton("locationtype_details_form_btn_cancel", 
                                        fTL("Cancel"), 
                                        "cancelUpdate('UserCancelled','process_mode'); disablePopup('locationtype_details_form_popup');").
   
   LocationTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationTypeDetailsForm:FormButtons = LocationTypeDetailsButtons.
   
   LocationTypeDetailsForm:endForm(). 
   
   LocationTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + LocationTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationTypeDetailsFields Procedure 
PROCEDURE pLocationTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         LocationTypeDetailsForm:startRow().
         LocationTypeDetailsForm:insertLabel(fTL("Field Label")).
         LocationTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adLocationTypeAdmin_locationtype_details_form.i}
      
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
   
   ASSIGN chrLocationTypeID = get-value("LocationTypeID")
          intSelectedLocationType = INTEGER(chrLocationTypeID)
          chrScrollToLocationTypeRow = STRING(INTEGER(get-value("locationtype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrLocationTypeID <> "" THEN
     chrSelectLocationTypeRow = 'selectLocationTypeRow(document.getElementById("locationtype_browse_row_' + chrLocationTypeID + '"),"' 
                                                         + chrLocationTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("locationtype_browse").scrollTop=' + chrScrollToLocationTypeRow 
                                                          + chrSelectLocationTypeRow.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}      
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "LocationType Admin".
   ThisPage:FrameTitle    = "LocationType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pLocationTypeBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pLocationTypeDetails.
   
   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT LocationTypeBrowseFrame         NO-ERROR.
   DELETE OBJECT LocationTypeBrowse              NO-ERROR.
   DELETE OBJECT LocationTypeBrowseButtons       NO-ERROR.
   DELETE OBJECT LocationTypeDetailsForm         NO-ERROR.
   DELETE OBJECT LocationTypeDetailsButtons      NO-ERROR.

   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

