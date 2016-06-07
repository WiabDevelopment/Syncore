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

/* Definitions for System Options for Receiving */
{getReceivingOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedBusinessUnit      AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectBusinessUnitRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToBusinessUnitRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrBusinessUnitID            AS CHARACTER NO-UNDO.

/* Objects for Rule Browse */
DEFINE VARIABLE BusinessUnitBrowseFrame      AS pageFrame.
DEFINE VARIABLE BusinessUnitBrowse           AS browseTable.
DEFINE VARIABLE BusinessUnitBrowseButtons    AS buttonBar.
DEFINE VARIABLE BusinessUnitDetailsForm      AS dataForm.
DEFINE VARIABLE BusinessUnitDetailsButtons   AS buttonBar.

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
         HEIGHT             = 12.79
         WIDTH              = 76.8.
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

&IF DEFINED(EXCLUDE-pBusinessUnitBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBusinessUnitBrowse Procedure 
PROCEDURE pBusinessUnitBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "businessunit_details_form"}
   
   BusinessUnitBrowse              = NEW browseTable("businessunit_browse").
   BusinessUnitBrowse:BrowseWidth  = 965.
   BusinessUnitBrowse:BrowseHeight = 455.
   BusinessUnitBrowse:WebStream    = STREAM WebStream:HANDLE.

   /* Add in the ID as first Column */
   BusinessUnitBrowse:insertColumn(fTL("BusinessUnit ID"), 120, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i BusinessUnit}
   
   BusinessUnitBrowse:insertColumn(fTL("ListingSequence") , 120, "INTEGER", FALSE).
   BusinessUnitBrowse:insertColumn(fTL("Unit Name")       , 100, "CHARACTER", "left", FALSE).
   BusinessUnitBrowse:insertColumn(fTL("Unit Description"), 250, "CHARACTER", "left", FALSE).
   BusinessUnitBrowse:insertColumn(fTL("Active")          , 110, "LOGICAL", FALSE).
   
   /*Body*/
   BusinessUnitBrowse:startBody().
   
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK 
      BY    BusinessUnit.ListingSequence:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.
         
      BusinessUnitBrowse:startRow(BusinessUnit.BusinessUnitID, "selectBusinessUnitRow(this," + '"' 
                                    + STRING(BusinessUnit.BusinessUnitID) + '"' + ");", "").
      
      BusinessUnitBrowse:insertData(BusinessUnit.BusinessUnitID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i BusinessUnit}
      BusinessUnitBrowse:insertData(BusinessUnit.ListingSequence, "left").
      BusinessUnitBrowse:insertData(BusinessUnit.UnitName, "left").
      BusinessUnitBrowse:insertData(BusinessUnit.UnitDescr, "left").

      BusinessUnitBrowse:insertData(STRING(BusinessUnit.Active,"Yes/No")).
      
      /* Add hidden fields */
      BusinessUnitBrowse:insertHiddenData("BusinessUnitID",BusinessUnit.BusinessUnitID).
      BusinessUnitBrowse:insertHiddenData("BusinessUnitVersionID",BusinessUnit.VersionID).
      
      BusinessUnitBrowse:endRow().
      
   END. /*FOR EACH BusinessUnit NO-LOCK */
   
   BusinessUnitBrowse:endTable().
   
   /* Create a new frame */
   BusinessUnitBrowseFrame           = NEW pageFrame().
   BusinessUnitBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   BusinessUnitBrowseFrame:FormAction="dbBusinessUnitUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   BusinessUnitBrowseFrame:formOpen("businessunit_browse_form").
   
   /* Start the Frame Header */
   BusinessUnitBrowseFrame:insertSpacer(5).
   BusinessUnitBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   BusinessUnitBrowse:displayBrowse().  
   
   /* End the Frame Header */
   BusinessUnitBrowseFrame:frameClose().
   BusinessUnitBrowseFrame:insertSpacer(10).
   
   BusinessUnitBrowseFrame:insertHiddenField("businessunit_browse_scroll","").
   BusinessUnitBrowseFrame:insertHiddenField("BusinessUnitID","").
   BusinessUnitBrowseFrame:insertHiddenField("BusinessUnitVersionID","").
   BusinessUnitBrowseFrame:insertHiddenField("form_name", "businessunit_browse_form").
   BusinessUnitBrowseFrame:insertHiddenField("prog_name", "adBusinessUnitAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BusinessUnitBrowseFrame}
   
   BusinessUnitBrowseFrame:formClose().
   
   /* Create Button Bar */
   BusinessUnitBrowseButtons = NEW buttonBar().
   BusinessUnitBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   BusinessUnitBrowseButtons:addButton("businessunit_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewBusinessUnitDetails('businessunit_details_form');",
                                 (IF intSelectedBusinessUnit > 0 THEN "" ELSE "Disabled")).
   
   BusinessUnitBrowseButtons:addButton("businessunit_browse_form_btn_create",
                                 fTL("Create"),
                                 "createBusinessUnit('businessunit_details_form');",
                                 "").
   
   BusinessUnitBrowseButtons:addButton("businessunit_browse_form_btn_delete",
                                 fTL("Delete"),
                                 "confirmDeleteBusinessUnit();",
                                 "Disabled").
                                 /* (IF intSelectedBusinessUnit > 0 THEN "" ELSE "Disabled")). */
   
   BusinessUnitBrowseButtons:closeBar().  
   BusinessUnitBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBusinessUnitDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBusinessUnitDetails Procedure 
PROCEDURE pBusinessUnitDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "businessunit_details_form"}
   
   ASSIGN chrDisplayFieldList  = "BusinessUnitID,ListingSequence,UnitName,UnitDescr,Active"
          chrEditFieldList     = "ListingSequence,UnitName,UnitDescr,Active"
          chrNewFieldList      = "ListingSequence,UnitName,UnitDescr,Active"
          chrRequiredFieldList = "ListingSequence,UnitName,UnitDescr,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER>0".
   
   BusinessUnitDetailsForm            = NEW dataForm("businessunit_details_form").
   BusinessUnitDetailsForm:WebStream  = STREAM WebStream:HANDLE.
   BusinessUnitDetailsForm:FormAction = "dbBusinessUnitUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BusinessUnitDetailsForm:FormWidth  = 460.
   BusinessUnitDetailsForm:FormHeight = 300.
   BusinessUnitDetailsForm:FormTitle  = "Business Unit Details".
   BusinessUnitDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   BusinessUnitDetailsForm:insertPaddingColumn(50).
   BusinessUnitDetailsForm:insertColumn(100).
   
   /* Fields */
   BusinessUnitDetailsForm:startRow().
   BusinessUnitDetailsForm:insertLabel("").
   
   BusinessUnitDetailsForm:startRow().
   BusinessUnitDetailsForm:insertLabel("Unit ID").
   BusinessUnitDetailsForm:insertTextField("BusinessUnitID", "", 76, TRUE).
   
   BusinessUnitDetailsForm:startRow().
   BusinessUnitDetailsForm:insertLabel("Unit Name").
   BusinessUnitDetailsForm:insertTextField("UnitName", "", 110, TRUE).
   
   BusinessUnitDetailsForm:startRow().
   BusinessUnitDetailsForm:insertLabel("Description").
   BusinessUnitDetailsForm:insertTextField("UnitDescr", "", 110, TRUE).
   
   BusinessUnitDetailsForm:startRow().
   BusinessUnitDetailsForm:insertLabel("ListingSequence").
   BusinessUnitDetailsForm:insertTextField("ListingSequence", "", 76, TRUE).
   
   BusinessUnitDetailsForm:startRow().
   BusinessUnitDetailsForm:insertLabel(fTL("Active")).
   BusinessUnitDetailsForm:insertComboField("Active", "", 76, TRUE).
   BusinessUnitDetailsForm:insertComboPairs("Active", "yes", "Active").
   BusinessUnitDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pBusinessUnitDetailsFields}
   
   /* Add Hidden Fields*/
   BusinessUnitDetailsForm:insertHiddenField("businessunit_browse_scroll", "").
   BusinessUnitDetailsForm:insertHiddenField("form_name", "businessunit_details_form").
   BusinessUnitDetailsForm:insertHiddenField("prog_name", "adBusinessUnitAdmin.p").
   BusinessUnitDetailsForm:insertHiddenField("popup_stockstatuschangereason_browse","").  
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BusinessUnitDetailsForm}
   
   /* Create Button Bar */
   BusinessUnitDetailsButtons = NEW buttonBar().
   
   BusinessUnitDetailsButtons:addButton("businessunit_details_form_btn_save", 
                                  fTL("Save"), 
                                  "updateBusinessUnit('businessunit_details_form');").
   
   BusinessUnitDetailsButtons:addButton("businessunit_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('businessunit_details_form_popup');").
   
   BusinessUnitDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   BusinessUnitDetailsForm:FormButtons = BusinessUnitDetailsButtons.
   BusinessUnitDetailsForm:endForm().
   BusinessUnitDetailsForm:displayForm().
   
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
   
   /* Get URL values */
   ASSIGN chrBusinessUnitID          = get-value("BusinessUnitID")
          intSelectedBusinessUnit    = INTEGER(chrBusinessUnitID)
          chrScrollToBusinessUnitRow = STRING(INTEGER(get-value("businessunit_browse_scroll"))) + ";"
          .
   
   /* Process URL values */
   /* Build the Select Rule Row command if needed */
   IF chrBusinessUnitID <> "" THEN
     chrSelectBusinessUnitRow = 'selectBusinessUnitRow(document.getElementById("businessunit_browse_row_' 
                                  + chrBusinessUnitID + '"),"' + chrBusinessUnitID +  '");'.
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("businessunit_browse").scrollTop=' 
                             + chrScrollToBusinessUnitRow + chrSelectBusinessUnitRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Business Unit Admin".
   ThisPage:FrameTitle = ThisPage:PageTitle.
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   ThisPage:addJavaScript("businessunit.js").   
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pBusinessUnitBrowse.
   
   FIND BusinessUnit NO-LOCK
      WHERE BusinessUnit.BusinessUnitID = intSelectedBusinessUnit NO-ERROR.
   
   /******* Popup Browsers and Forms ********/    
   RUN pBusinessUnitDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT BusinessUnitBrowseFrame      NO-ERROR.
   DELETE OBJECT BusinessUnitBrowse           NO-ERROR.
   DELETE OBJECT BusinessUnitBrowseButtons    NO-ERROR.
   DELETE OBJECT BusinessUnitDetailsForm      NO-ERROR.
   DELETE OBJECT BusinessUnitDetailsButtons   NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

