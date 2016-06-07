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
  Revisions:
     02/12/2014 - twierzch - screen moved out of syncore controlled. Show edit/save buttons always and remove AppLink.

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
{defDataMigrationVariables.i}

/* Includes */
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Definitions for System Options for Receiving */
{getPartOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedPartCategory        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectPartCategoryRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPartCategoryRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartCategoryID              AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE PartCategoryBrowseFrame        AS pageFrame.
DEFINE VARIABLE PartCategoryBrowse             AS browseTable.
DEFINE VARIABLE PartCategoryBrowseButtons      AS buttonBar.
DEFINE VARIABLE PartCategoryDetailsForm        AS dataForm.
DEFINE VARIABLE PartCategoryDetailsButtons     AS buttonBar.

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
   Category: Procedure
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
   * (in minutes) before running outputContentCategory.  If you supply a timeout 
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
   * Output additional cookie information here before running outputContentCategory.
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

&IF DEFINED(EXCLUDE-pPartCategoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartCategoryBrowse Procedure 
PROCEDURE pPartCategoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "partcategory_details_form"}
   
   PartCategoryBrowse = NEW browseTable("partcategory_browse").
   PartCategoryBrowse:BrowseWidth  = 965.
   PartCategoryBrowse:BrowseHeight = 455.
   PartCategoryBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Part Ref as first Column */
   PartCategoryBrowse:insertColumn(fTL("CategoryID"), 105, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartCategory}
   
   PartCategoryBrowse:insertColumn(fTL("Category Code"), 160, "CHARACTER", "left", FALSE).
   PartCategoryBrowse:insertColumn(fTL("Category Name"), 180, "CHARACTER", "left", FALSE).
   PartCategoryBrowse:insertColumn(fTL("Descr"), 230, "CHARACTER", "left", FALSE).
   PartCategoryBrowse:insertColumn(fTL("List Seq"), 90, "INTEGER", FALSE).
   /*PartCategoryBrowse:insertColumn(fTL("Inventory"), 90, "LOGICAL", FALSE).*/
   PartCategoryBrowse:insertColumn(fTL("Active"), 90, "LOGICAL", FALSE).
   
   /*Body*/
   PartCategoryBrowse:startBody().
   
   FOR EACH PartCategory NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PartCategory.Active
      BY    PartCategory.ListingSequence:
      
      PartCategoryBrowse:startRow(PartCategory.PartCategoryID, "selectPartCategoryRow(this," + '"' + STRING(PartCategory.PartCategoryID) + '"' + ");", "").
      PartCategoryBrowse:insertData(PartCategory.PartCategoryID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PartCategory}
      
      PartCategoryBrowse:insertData((IF AVAILABLE PartCategory THEN PartCategory.CategoryCode ELSE ""), "left").
      PartCategoryBrowse:insertData((IF AVAILABLE PartCategory THEN PartCategory.CategoryName ELSE ""), "left").
      PartCategoryBrowse:insertData((IF AVAILABLE PartCategory THEN PartCategory.CategoryDescr ELSE ""), "left").
      PartCategoryBrowse:insertData((IF AVAILABLE PartCategory THEN STRING(PartCategory.ListingSequence) ELSE "")).
      /*PartCategoryBrowse:insertData((IF AVAILABLE PartCategory THEN STRING(PartCategory.InventoryPart,"Yes/No") ELSE "")).*/
      PartCategoryBrowse:insertData((IF AVAILABLE PartCategory THEN STRING(PartCategory.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      PartCategoryBrowse:insertHiddenData("PartCategoryVersionID",PartCategory.VersionID).
      
      intRecordCount = intRecordCount + 1.
      
      PartCategoryBrowse:endRow().
      
   END. /*FOR EACH PartCategory NO-LOCK */
   
   PartCategoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PartCategoryBrowse:getErrors().
   
   /* Create a new frame */
   PartCategoryBrowseFrame = NEW pageFrame().
   PartCategoryBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PartCategoryBrowseFrame:FormAction="dbPartCategoryUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PartCategoryBrowseFrame:formOpen("partcategory_browse_form").
   
   /* Start the Frame Header */
   PartCategoryBrowseFrame:insertSpacer(5).
   PartCategoryBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PartCategoryBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PartCategoryBrowseFrame:frameClose().
   PartCategoryBrowseFrame:insertSpacer(10).
   
   PartCategoryBrowseFrame:insertHiddenField("partcategory_browse_scroll","").
   PartCategoryBrowseFrame:insertHiddenField("PartCategoryID","").
   PartCategoryBrowseFrame:insertHiddenField("PartCategoryVersionID","").
   
   PartCategoryBrowseFrame:insertHiddenField("form_name","partcategory_browse_form").
   PartCategoryBrowseFrame:insertHiddenField("prog_name","adPartCategoryAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartCategoryBrowseFrame}
   
   PartCategoryBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   PartCategoryBrowseButtons = NEW buttonBar().
   PartCategoryBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   PartCategoryBrowseButtons:addButton("partcategory_browse_form_btn_details",
                                       fTL("Details"),
                                       "viewPartCategoryDetails('partcategory_details_form');",
                                       "Disabled").

   PartCategoryBrowseButtons:addButton("partcategory_browse_form_btn_create",
                                       fTL("Create"),
                                       "createPartCategory('partcategory_details_form');",
                                       "").
   
   PartCategoryBrowseButtons:closeBar().  
   PartCategoryBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartCategoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartCategoryDetails Procedure 
PROCEDURE pPartCategoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "partcategory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PartCategoryID,CategoryCode,CategoryName,CategoryDescr,ListingSequence,Active"
          chrEditFieldList     = "CategoryName,CategoryDescr,ListingSequence,Active"
          chrNewFieldList      = "CategoryCode,CategoryName,CategoryDescr,ListingSequence,InventoryPart,Active"
          chrRequiredFieldList = "CategoryCode,CategoryName,CategoryDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PartCategoryDetailsForm = NEW dataForm("partcategory_details_form").
   PartCategoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PartCategoryDetailsForm:FormAction = "dbPartCategoryUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PartCategoryDetailsForm:FormWidth   = 460.
   PartCategoryDetailsForm:FormHeight  = 300.
   PartCategoryDetailsForm:FormTitle   = "PartCategory Details".
   PartCategoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PartCategoryDetailsForm:insertPaddingColumn(50).
   PartCategoryDetailsForm:insertColumn(100).
   PartCategoryDetailsForm:insertColumn(120).
   PartCategoryDetailsForm:insertColumn(20).
   PartCategoryDetailsForm:insertColumn(4).
   PartCategoryDetailsForm:insertColumn(110).
   
   /* Fields */
   PartCategoryDetailsForm:startRow().
   PartCategoryDetailsForm:insertLabel("PartCategory ID").
   PartCategoryDetailsForm:insertTextField("PartCategoryID", "", 110, TRUE).  
   
   PartCategoryDetailsForm:startRow().
   PartCategoryDetailsForm:insertLabel("CategoryCode").
   PartCategoryDetailsForm:insertTextField("CategoryCode", "", 110, TRUE).  
   
   PartCategoryDetailsForm:startRow().
   PartCategoryDetailsForm:insertLabel("CategoryName").
   PartCategoryDetailsForm:insertTextField("CategoryName", "", 110, TRUE).  
   
   PartCategoryDetailsForm:startRow().
   PartCategoryDetailsForm:insertLabel("Category Descr").
   PartCategoryDetailsForm:insertTextField("CategoryDescr", "", 220, TRUE).  
   
   PartCategoryDetailsForm:startRow().
   PartCategoryDetailsForm:insertLabel("Listing Sequence").
   PartCategoryDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   /*    PartCategoryDetailsForm:startRow().                                                     */
   /*    PartCategoryDetailsForm:insertLabel(fTL("Inventory Part")).                             */
   /*    PartCategoryDetailsForm:insertComboField("InventoryPart", "", 110, TRUE).               */
   /*    PartCategoryDetailsForm:insertComboPairs("InventoryPart", "yes", "Inventory Part").     */
   /*    PartCategoryDetailsForm:insertComboPairs("InventoryPart", "no",  "Not Inventory Part"). */
   
   PartCategoryDetailsForm:startRow().
   PartCategoryDetailsForm:insertLabel(fTL("Active")). 
   PartCategoryDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PartCategoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartCategoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPartCategoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PartCategoryDetailsForm:insertHiddenField("partcategory_browse_scroll", "").
   PartCategoryDetailsForm:insertHiddenField("form_name", "partcategory_details_form").
   PartCategoryDetailsForm:insertHiddenField("prog_name", "adPartCategoryAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartCategoryDetailsForm}
   
   /* Create Button Bar */
   PartCategoryDetailsButtons = NEW buttonBar().
   
   PartCategoryDetailsButtons:addButton("partcategory_details_form_btn_save", 
                                        fTL("Save"), 
                                        "updatePartCategory('partcategory_details_form');").
   
   PartCategoryDetailsButtons:addButton("partcategory_details_form_btn_cancel", 
                                        fTL("Cancel"), 
                                        "cancelUpdate('UserCancelled','process_mode'); disablePopup('partcategory_details_form_popup');").
   
   PartCategoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PartCategoryDetailsForm:FormButtons = PartCategoryDetailsButtons.
   
   PartCategoryDetailsForm:endForm(). 
   
   PartCategoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + PartCategoryDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartCategoryDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartCategoryDetailsFields Procedure 
PROCEDURE pPartCategoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         PartCategoryDetailsForm:startRow().
         PartCategoryDetailsForm:insertLabel(fTL("Field Label")).
         PartCategoryDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adPartCategoryAdmin_partcategory_details_form.i}
      
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
   
   ASSIGN chrPartCategoryID = get-value("PartCategoryID")
          intSelectedPartCategory = INTEGER(chrPartCategoryID)
          chrScrollToPartCategoryRow = STRING(INTEGER(get-value("partcategory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPartCategoryID <> "" THEN
      chrSelectPartCategoryRow = 'selectPartCategoryRow(document.getElementById("partcategory_browse_row_' + chrPartCategoryID + '"),"' 
                                                          + chrPartCategoryID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                   + 'document.getElementById("partcategory_browse").scrollTop=' + chrScrollToPartCategoryRow 
                   + chrSelectPartCategoryRow.
                   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                      
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "PartCategory Admin".
   ThisPage:FrameTitle = "PartCategory Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("partcategory.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPartCategoryBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pPartCategoryDetails.
   
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
   DELETE OBJECT PartCategoryBrowseFrame             NO-ERROR.
   DELETE OBJECT PartCategoryBrowse                  NO-ERROR.
   DELETE OBJECT PartCategoryBrowseButtons           NO-ERROR.
   DELETE OBJECT PartCategoryDetailsForm             NO-ERROR.
   DELETE OBJECT PartCategoryDetailsButtons          NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

