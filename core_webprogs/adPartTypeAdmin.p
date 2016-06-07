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
DEFINE VARIABLE intSelectedPartType        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectPartTypeRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPartTypeRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartTypeID              AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE PartTypeBrowseFrame        AS pageFrame.
DEFINE VARIABLE PartTypeBrowse             AS browseTable.
DEFINE VARIABLE PartTypeBrowseButtons      AS buttonBar.
DEFINE VARIABLE PartTypeDetailsForm        AS dataForm.
DEFINE VARIABLE PartTypeDetailsButtons     AS buttonBar.

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

&IF DEFINED(EXCLUDE-pPartTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartTypeBrowse Procedure 
PROCEDURE pPartTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "parttype_details_form"}
   
   PartTypeBrowse = NEW browseTable("parttype_browse").
   PartTypeBrowse:BrowseWidth  = 965.
   PartTypeBrowse:BrowseHeight = 455.
   PartTypeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Part Ref as first Column */
   PartTypeBrowse:insertColumn(fTL("PartType ID"), 105, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartType}
   
   PartTypeBrowse:insertColumn(fTL("Type Code"),   160, "CHARACTER", "left", FALSE).
   PartTypeBrowse:insertColumn(fTL("Type Name"),   180, "CHARACTER", "left", FALSE).
   PartTypeBrowse:insertColumn(fTL("Descr"),       230, "CHARACTER", "left", FALSE).
   PartTypeBrowse:insertColumn(fTL("Listing Seq"),  90, "INTEGER", FALSE).
   PartTypeBrowse:insertColumn(fTL("Inventory"),    90, "LOGICAL", FALSE).
   PartTypeBrowse:insertColumn(fTL("Active"),       90, "LOGICAL", FALSE).
   
   /*Body*/
   PartTypeBrowse:startBody().
   
   FOR EACH PartType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PartType.Active
      BY    PartType.ListingSequence:
      
      PartTypeBrowse:startRow(PartType.PartTypeID, "selectPartTypeRow(this," + '"' + STRING(PartType.PartTypeID) + '"' + ");", "").
      PartTypeBrowse:insertData(PartType.PartTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PartType}
      
      PartTypeBrowse:insertData((IF AVAILABLE PartType THEN PartType.TypeCode ELSE ""), "left").
      PartTypeBrowse:insertData((IF AVAILABLE PartType THEN PartType.TypeName ELSE ""), "left").
      PartTypeBrowse:insertData((IF AVAILABLE PartType THEN PartType.TypeDescr ELSE ""), "left").
      PartTypeBrowse:insertData((IF AVAILABLE PartType THEN STRING(PartType.ListingSequence) ELSE "")).
      PartTypeBrowse:insertData((IF AVAILABLE PartType THEN STRING(PartType.InventoryPart,"Yes/No") ELSE "")).
      PartTypeBrowse:insertData((IF AVAILABLE PartType THEN STRING(PartType.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      PartTypeBrowse:insertHiddenData("PartTypeVersionID",PartType.VersionID).
      
      intRecordCount = intRecordCount + 1.
      
      PartTypeBrowse:endRow().
      
   END. /*FOR EACH PartType NO-LOCK */
   
   PartTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PartTypeBrowse:getErrors().
   
   /* Create a new frame */
   PartTypeBrowseFrame = NEW pageFrame().
   PartTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PartTypeBrowseFrame:FormAction="dbPartTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PartTypeBrowseFrame:formOpen("parttype_browse_form").
   
   /* Start the Frame Header */
   PartTypeBrowseFrame:insertSpacer(5).
   PartTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PartTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PartTypeBrowseFrame:frameClose().
   PartTypeBrowseFrame:insertSpacer(10).
   
   PartTypeBrowseFrame:insertHiddenField("parttype_browse_scroll","").
   PartTypeBrowseFrame:insertHiddenField("PartTypeID","").
   PartTypeBrowseFrame:insertHiddenField("PartTypeVersionID","").
   
   PartTypeBrowseFrame:insertHiddenField("form_name","parttype_browse_form").
   PartTypeBrowseFrame:insertHiddenField("prog_name","adPartTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartTypeBrowseFrame}
   
   PartTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
      
   /* Create Button Bar */
   PartTypeBrowseButtons = NEW buttonBar().
   PartTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   PartTypeBrowseButtons:addButton("parttype_browse_form_btn_details",
                                   fTL("Details"),
                                   "viewPartTypeDetails('parttype_details_form');",
                                   "Disabled").
                                   
   PartTypeBrowseButtons:addButton("parttype_browse_form_btn_create",
                                      fTL("Create"),
                                      "createPartType('parttype_details_form');",
                                      "").
                                      
   PartTypeBrowseButtons:closeBar().  
   PartTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartTypeDetails Procedure 
PROCEDURE pPartTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "parttype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PartTypeID,TypeCode,TypeName,TypeDescr,ListingSequence,InventoryPart,CaptureBatchRef,Active"
          chrEditFieldList     = "TypeName,TypeDescr,ListingSequence,CaptureBatchRef,Active"
          chrNewFieldList      = "TypeCode,TypeName,TypeDescr,ListingSequence,InventoryPart,CaptureBatchRef,Active"
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PartTypeDetailsForm = NEW dataForm("parttype_details_form").
   PartTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PartTypeDetailsForm:FormAction = "dbPartTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PartTypeDetailsForm:FormWidth   = 460.
   PartTypeDetailsForm:FormHeight  = 300.
   PartTypeDetailsForm:FormTitle   = "PartType Details".
   PartTypeDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PartTypeDetailsForm:insertPaddingColumn(50).
   PartTypeDetailsForm:insertColumn(100).
   PartTypeDetailsForm:insertColumn(120).
   PartTypeDetailsForm:insertColumn(20).
   PartTypeDetailsForm:insertColumn(4).
   PartTypeDetailsForm:insertColumn(110).
   
   /* Fields */
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel("PartType ID").
   PartTypeDetailsForm:insertTextField("PartTypeID", "", 110, TRUE).  
   
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel("TypeCode").
   PartTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel("TypeName").
   PartTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).  
   
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel("PartType Descr").
   PartTypeDetailsForm:insertTextField("TypeDescr", "", 220, TRUE).  
   
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel("Listing Sequence").
   PartTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel(fTL("Inventory Part")). 
   PartTypeDetailsForm:insertComboField("InventoryPart", "", 110, TRUE).  
   PartTypeDetailsForm:insertComboPairs("InventoryPart", "yes", "Inventory Part").
   PartTypeDetailsForm:insertComboPairs("InventoryPart", "no",  "Not Inventory Part").
   
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel(fTL("Capture BatchRef")). 
   PartTypeDetailsForm:insertComboField("CaptureBatchRef", "", 110, TRUE).  
   PartTypeDetailsForm:insertComboPairs("CaptureBatchRef", "yes", "Yes").
   PartTypeDetailsForm:insertComboPairs("CaptureBatchRef", "no",  "No").
   
   PartTypeDetailsForm:startRow().
   PartTypeDetailsForm:insertLabel(fTL("Active")). 
   PartTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PartTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPartTypeDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PartTypeDetailsForm:insertHiddenField("parttype_browse_scroll", "").
   PartTypeDetailsForm:insertHiddenField("form_name", "parttype_details_form").
   PartTypeDetailsForm:insertHiddenField("prog_name", "adPartTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartTypeDetailsForm}
   
   /* Create Button Bar */
   PartTypeDetailsButtons = NEW buttonBar().
   
   PartTypeDetailsButtons:addButton("parttype_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updatePartType('parttype_details_form');").
   
   PartTypeDetailsButtons:addButton("parttype_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('parttype_details_form_popup');").
   
   PartTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PartTypeDetailsForm:FormButtons = PartTypeDetailsButtons.
   
   PartTypeDetailsForm:endForm(). 
   
   PartTypeDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + PartTypeDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartTypeDetailsFields Procedure 
PROCEDURE pPartTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         PartTypeDetailsForm:startRow().
         PartTypeDetailsForm:insertLabel(fTL("Field Label")).
         PartTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adPartTypeAdmin_parttype_details_form.i}
      
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
   
   ASSIGN chrPartTypeID          = get-value("PartTypeID")
          intSelectedPartType    = INTEGER(chrPartTypeID)
          chrScrollToPartTypeRow = STRING(INTEGER(get-value("parttype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPartTypeID <> "" THEN
      chrSelectPartTypeRow = 'selectPartTypeRow(document.getElementById("parttype_browse_row_' + chrPartTypeID + '"),"' 
                                + chrPartTypeID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                    + 'document.getElementById("parttype_browse").scrollTop=' + chrScrollToPartTypeRow 
                    + chrSelectPartTypeRow.
                    
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                       
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "PartType Admin".
   ThisPage:FrameTitle    = "PartType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("part.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPartTypeBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pPartTypeDetails.
   
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
   DELETE OBJECT PartTypeBrowseFrame             NO-ERROR.
   DELETE OBJECT PartTypeBrowse                  NO-ERROR.
   DELETE OBJECT PartTypeBrowseButtons           NO-ERROR.
   DELETE OBJECT PartTypeDetailsForm             NO-ERROR.
   DELETE OBJECT PartTypeDetailsButtons          NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

