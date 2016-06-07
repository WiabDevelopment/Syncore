&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adPartPickMethodology.p

  Purpose: Admin screen for PartPickMethodology. I have future proofed a bit so that if we add a history all you have to do is uncomment
           some things and add a webform. 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Anthony Ferrari

  Created: 24/03/2016

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



/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedPartPickMethodology           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectPartPickMethodologyRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPartPickMethodologyRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartPickMethodologyID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPartPickMethodologyHistory       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE PartPickMethodologyBrowseFrame           AS pageFrame.
DEFINE VARIABLE PartPickMethodologyBrowse                AS browseTable.
DEFINE VARIABLE PartPickMethodologyBrowseButtons         AS buttonBar.
DEFINE VARIABLE PartPickMethodologyDetailsForm           AS dataForm.
DEFINE VARIABLE PartPickMethodologyDetailsButtons        AS buttonBar.

DEFINE VARIABLE PartPickMethodologyHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE PartPickMethodologyHistoryBrowse         AS browseTable.
DEFINE VARIABLE PartPickMethodologyHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE PartPickMethodologyHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE PartPickMethodologyHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrPartPickMethodologyID          = get-value("PartPickMethodologyID")
          intSelectedPartPickMethodology    = INTEGER(chrPartPickMethodologyID)
          chrScrollToPartPickMethodologyRow = STRING(INTEGER(get-value("partpickmethodology_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPartPickMethodologyID <> "" THEN
     chrSelectPartPickMethodologyRow = 'selectPartPickMethodologyRow(document.getElementById("partpickmethodology_browse_row_' + chrPartPickMethodologyID + '"),"' 
                                                         + chrPartPickMethodologyID +  '");'.
   
/*   IF get-value('popup_partpickmethodologyhistory_browse') = "yes" THEN                                    */
/*      chrPopupPartPickMethodologyHistory  = 'enablePopup("partpickmethodologyhistory_browse_form_popup");'.*/
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("partpickmethodology_browse").scrollTop=' + chrScrollToPartPickMethodologyRow 
                                                          + chrSelectPartPickMethodologyRow + chrPopupPartPickMethodologyHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "PartPickMethodology Admin".
   ThisPage:FrameTitle    = "PartPickMethodology Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("partpickmethodology.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPartPickMethodologyBrowse.
   
   FIND FIRST PartPickMethodology NO-LOCK /* idx=PartPickMethodologyID */
      WHERE PartPickMethodology.PartPickMethodologyID = intSelectedPartPickMethodology NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pPartPickMethodologyDetails.
/*   RUN pPartPickMethodologyHistory.       */
/*   RUN pPartPickMethodologyHistoryDetails.*/
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT PartPickMethodologyBrowseFrame           NO-ERROR.
   DELETE OBJECT PartPickMethodologyBrowse                NO-ERROR.
   DELETE OBJECT PartPickMethodologyBrowseButtons         NO-ERROR.
   DELETE OBJECT PartPickMethodologyDetailsForm           NO-ERROR.
   DELETE OBJECT PartPickMethodologyDetailsButtons        NO-ERROR.
   
   DELETE OBJECT PartPickMethodologyHistoryBrowseForm     NO-ERROR.   
   DELETE OBJECT PartPickMethodologyHistoryBrowse         NO-ERROR.
   DELETE OBJECT PartPickMethodologyHistoryBrowseButtons  NO-ERROR.
   
   DELETE OBJECT PartPickMethodologyHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT PartPickMethodologyHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartPickMethodologyBrowse Procedure 
PROCEDURE pPartPickMethodologyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "partpickmethodology_details_form"}
   
   PartPickMethodologyBrowse = NEW browseTable("partpickmethodology_browse").
   PartPickMethodologyBrowse:BrowseWidth  = 965.
   PartPickMethodologyBrowse:BrowseHeight = 455.
   PartPickMethodologyBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   PartPickMethodologyBrowse:insertColumn(fTL("PartPickMethodologyID"), 160, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartPickMethodology}
   
   PartPickMethodologyBrowse:insertColumn(fTL("List Seq"),               60, "INTEGER",           FALSE).
   PartPickMethodologyBrowse:insertColumn(fTL("Type Code"),             120, "CHARACTER", "left", FALSE).
   PartPickMethodologyBrowse:insertColumn(fTL("Type Name"),             120, "CHARACTER", "left", FALSE).
   PartPickMethodologyBrowse:insertColumn(fTL("Type Descr"),            200, "CHARACTER", "left", FALSE).
   PartPickMethodologyBrowse:insertColumn(fTL("Active"),                 50, "LOGICAL",           FALSE).
   
   /*Body*/
   PartPickMethodologyBrowse:startBody().
   
   FOR EACH PartPickMethodology NO-LOCK /*idx=ActiveListingSequence*/
      BY PartPickMethodology.Active DESC
      BY PartPickMethodology.ListingSequence:
      
      PartPickMethodologyBrowse:startRow(PartPickMethodology.PartPickMethodologyID, "selectPartPickMethodologyRow(this," + '"' + STRING(PartPickMethodology.PartPickMethodologyID) 
                                                                  + '"' + ");", "").
      PartPickMethodologyBrowse:insertData(PartPickMethodology.PartPickMethodologyID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PartPickMethodology}
      
      PartPickMethodologyBrowse:insertData(STRING(PartPickMethodology.ListingSequence)).
      PartPickMethodologyBrowse:insertData(PartPickMethodology.TypeCode, "left").
      PartPickMethodologyBrowse:insertData(PartPickMethodology.TypeName, "left").
      PartPickMethodologyBrowse:insertData(PartPickMethodology.TypeDescr, "left").
      PartPickMethodologyBrowse:insertData(STRING(PartPickMethodology.Active,"Yes/No")).
      
      /* Add hidden fields */
      PartPickMethodologyBrowse:insertHiddenData("PartPickMethodologyVersionID",PartPickMethodology.VersionID).
      
      PartPickMethodologyBrowse:endRow().
      
   END. /*FOR EACH PartPickMethodology NO-LOCK */
   
   PartPickMethodologyBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PartPickMethodologyBrowse:getErrors().
   
   /* Create a new frame */
   PartPickMethodologyBrowseFrame = NEW pageFrame().
   PartPickMethodologyBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PartPickMethodologyBrowseFrame:FormAction="dbPartPickMethodologyUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PartPickMethodologyBrowseFrame:formOpen("partpickmethodology_browse_form").
   
   /* Start the Frame Header */
   PartPickMethodologyBrowseFrame:insertSpacer(5).
   PartPickMethodologyBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PartPickMethodologyBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PartPickMethodologyBrowseFrame:frameClose().
   PartPickMethodologyBrowseFrame:insertSpacer(10).
   
   PartPickMethodologyBrowseFrame:insertHiddenField("partpickmethodology_browse_scroll","").
   PartPickMethodologyBrowseFrame:insertHiddenField("PartPickMethodologyID","").
   PartPickMethodologyBrowseFrame:insertHiddenField("PartPickMethodologyVersionID","").
/*   PartPickMethodologyBrowseFrame:insertHiddenField("popup_partpickmethodologyhistory_browse","").*/
   PartPickMethodologyBrowseFrame:insertHiddenField("form_name","partpickmethodology_browse_form").
   PartPickMethodologyBrowseFrame:insertHiddenField("prog_name","adPartPickMethodology.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartPickMethodologyBrowseFrame}
   
   PartPickMethodologyBrowseFrame:formClose().
   
   /* Create Button Bar */
   PartPickMethodologyBrowseButtons = NEW buttonBar().
   PartPickMethodologyBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   PartPickMethodologyBrowseButtons:addButton("partpickmethodology_browse_form_btn_create",
                                             fTL("Create"),
                                             "createPartPickMethodology('partpickmethodology_details_form');",
                                             "").
   
   PartPickMethodologyBrowseButtons:addButton("partpickmethodology_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewPartPickMethodologyDetails('partpickmethodology_details_form');",
                                             "Disabled").
   
/*   PartPickMethodologyBrowseButtons:addButton("partpickmethodology_browse_form_btn_history",*/
/*                                             fTL("History"),                                */
/*                                             "viewHistory();",                              */
/*                                             "Disabled").                                   */
   
   PartPickMethodologyBrowseButtons:closeBar().  
   PartPickMethodologyBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartPickMethodologyDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartPickMethodologyDetails Procedure 
PROCEDURE pPartPickMethodologyDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "partpickmethodology_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PartPickMethodologyID,ListingSequence,TypeCode,TypeName,TypeDescr,Active" 
          chrEditFieldList     = "ListingSequence,TypeName,TypeDescr,Active" 
          chrNewFieldList      = "TypeCode,ListingSequence,TypeName,TypeDescr,Active" 
          chrRequiredFieldList = "TypeCode,TypeName,TypeDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PartPickMethodologyDetailsForm = NEW dataForm("partpickmethodology_details_form").
   PartPickMethodologyDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PartPickMethodologyDetailsForm:FormAction = "dbPartPickMethodologyUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PartPickMethodologyDetailsForm:FormWidth   = 460.
   PartPickMethodologyDetailsForm:FormHeight  = 200.
   PartPickMethodologyDetailsForm:FormTitle   = "PartPickMethodology Details".
   PartPickMethodologyDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   PartPickMethodologyDetailsForm:insertPaddingColumn(30).
   PartPickMethodologyDetailsForm:insertColumn(150).
   PartPickMethodologyDetailsForm:insertColumn(160).
   PartPickMethodologyDetailsForm:insertColumn(20).
   PartPickMethodologyDetailsForm:insertColumn(4).
   PartPickMethodologyDetailsForm:insertColumn(110).
   
   /* Fields */
   PartPickMethodologyDetailsForm:startRow().
   PartPickMethodologyDetailsForm:insertLabel("PartPickMethodologyID").
   PartPickMethodologyDetailsForm:insertTextField("PartPickMethodologyID", "", 200, TRUE).  
   
   PartPickMethodologyDetailsForm:startRow().
   PartPickMethodologyDetailsForm:insertLabel("Listing Seq").
   PartPickMethodologyDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).  
   
   PartPickMethodologyDetailsForm:startRow().
   PartPickMethodologyDetailsForm:insertLabel("Type Code").
   PartPickMethodologyDetailsForm:insertTextField("TypeCode", "", 200, TRUE).  
   
   PartPickMethodologyDetailsForm:startRow().
   PartPickMethodologyDetailsForm:insertLabel("Type Name").
   PartPickMethodologyDetailsForm:insertTextField("TypeName", "", 200, TRUE).
   
   PartPickMethodologyDetailsForm:startRow().
   PartPickMethodologyDetailsForm:insertLabel("Type Descr").
   PartPickMethodologyDetailsForm:insertTextField("TypeDescr", "", 200, TRUE).  
   
   PartPickMethodologyDetailsForm:startRow().
   PartPickMethodologyDetailsForm:insertLabel(fTL("Active")). 
   PartPickMethodologyDetailsForm:insertComboField("Active", "", 200, TRUE).  
   PartPickMethodologyDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartPickMethodologyDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPartPickMethodologyDetailsFields}
   
   /* Add Hidden Fields*/
   PartPickMethodologyDetailsForm:insertHiddenField("partpickmethodology_browse_scroll", "").
   PartPickMethodologyDetailsForm:insertHiddenField("form_name", "partpickmethodology_details_form").
   PartPickMethodologyDetailsForm:insertHiddenField("prog_name", "adPartPickMethodology.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartPickMethodologyDetailsForm}
   
   /* Create Button Bar */
   PartPickMethodologyDetailsButtons = NEW buttonBar().
   
   PartPickMethodologyDetailsButtons:addButton("partpickmethodology_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updatePartPickMethodology('partpickmethodology_details_form');").
   
   PartPickMethodologyDetailsButtons:addButton("partpickmethodology_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('partpickmethodology_details_form_popup');").
   
   PartPickMethodologyDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PartPickMethodologyDetailsForm:FormButtons = PartPickMethodologyDetailsButtons.
   
   PartPickMethodologyDetailsForm:endForm(). 
   
   PartPickMethodologyDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + PartPickMethodologyDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartPickMethodologyDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartPickMethodologyDetailsFields Procedure 
PROCEDURE pPartPickMethodologyDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         PartPickMethodologyDetailsForm:startRow().
         PartPickMethodologyDetailsForm:insertLabel(fTL("Field Label")).
         PartPickMethodologyDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartPickMethodologyHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartPickMethodologyHistory Procedure
PROCEDURE pPartPickMethodologyHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*/* This finds the ProcessEvent which is linked to the form associated with this browse */                                                                                                                   */
/*   {webGetWebForm.i "partpickmethodologyhistory"}                                                                                                                                                   */
/*                                                                                                                                                                                                            */
/*   FIND FIRST PartPickMethodology NO-LOCK /* idx=PartPickMethodologyID */                       */
/*      WHERE PartPickMethodology.PartPickMethodologyID = intSelectedPartPickMethodology NO-ERROR.*/
/**/
/*   PartPickMethodologyHistoryBrowseForm = NEW dataForm("partpickmethodologyhistory_browse_form").                                                                                                           */
/*   PartPickMethodologyHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.                                                                                                                                */
/*                                                                                                                                                                                                            */
/*   /* Setup */                                                                                                                                                                                              */
/*   PartPickMethodologyHistoryBrowseForm:FormWidth   = 850.                                                                                                                                                  */
/*   PartPickMethodologyHistoryBrowseForm:FormHeight  = 540.                                                                                                                                                  */
/*   PartPickMethodologyHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE PartPickMethodology THEN " for PartPickMethodology: "                                                                  */
/*                                                                  + STRING(PartPickMethodology.TypeCode) ELSE "").                                                                                          */
/*   PartPickMethodologyHistoryBrowseForm:FormType    = "xxl_large".                                                                                                                                          */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowse = NEW browseTable("partpickmethodologyhistory_browse").                                                                                                                 */
/*   PartPickMethodologyHistoryBrowse:BrowseWidth  = 830.                                                                                                                                                     */
/*   PartPickMethodologyHistoryBrowse:BrowseHeight = 500.                                                                                                                                                     */
/*   PartPickMethodologyHistoryBrowse:ExcelExport  = TRUE.                                                                                                                                                    */
/*   PartPickMethodologyHistoryBrowse:SessionID    = intGblSessionID.                                                                                                                                         */
/*                                                                                                                                                                                                            */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).                                                                                                                   */
/*                                                                                                                                                                                                            */
/*   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */                                                                                     */
/*   {webGetOptionalBrowseHeaders.i PartPickMethodologyHistory}                                                                                                                                               */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("Listing Seq"),     70, "INTEGER",           FALSE).                                                                                                   */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("Type Code"),       80, "CHARACTER", "left", FALSE).                                                                                                   */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("Type Name"),       80, "CHARACTER", "left", FALSE).                                                                                                   */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("Type Descr"),     140, "CHARACTER", "left", FALSE).                                                                                                   */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("Active"),          50, "LOGICAL",           FALSE).                                                                                                   */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("User"),           120, "CHARACTER", "left", FALSE).                                                                                                   */
/*   PartPickMethodologyHistoryBrowse:insertColumn(fTL("Created"),        135, "CHARACTER", "left", FALSE).                                                                                                   */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowse:StartBody().                                                                                                                                                            */
/*                                                                                                                                                                                                            */
/*   IF AVAILABLE PartPickMethodology THEN                                                                                                                                                                    */
/*   DO:                                                                                                                                                                                                      */
/*      /*List the PartPickMethodologyHistory*/                                                                                                                                                               */
/*      FOR EACH PartPickMethodologyHistory NO-LOCK                                                                                                                                                           */
/*         WHERE  PartPickMethodologyHistory.PartPickMethodologyID = intSelectedPartPickMethodology                                                                                                           */
/*         BY PartPickMethodologyHistory.PartPickMethodologyHistoryID:                                                                                                                                        */
/*                                                                                                                                                                                                            */
/*         FIND FIRST GateUser NO-LOCK  /* idx=GateUserID */                                                                                                                                                  */
/*         WHERE GateUser.GateUserID = PartPickMethodologyHistory.GateUserID NO-ERROR.                                                                                                                        */
/*                                                                                                                                                                                                            */
/*         PartPickMethodologyHistoryBrowse:startRow(PartPickMethodologyHistory.PartPickMethodologyHistoryID, "selectHistoryRow(this," + '"' + STRING(PartPickMethodologyHistory.PartPickMethodologyHistoryID)*/
/*                                                                     + '","partPickMethodologyHistory"' + ");", "").                                                                                        */
/*         PartPickMethodologyHistoryBrowse:insertData(PartPickMethodologyHistory.PartPickMethodologyHistoryID).                                                                                              */
/*                                                                                                                                                                                                            */
/*         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */                                                                                       */
/*         {webGetOptionalBrowseFields.i PartPickMethodologyHistory}                                                                                                                                          */
/*                                                                                                                                                                                                            */
/*         PartPickMethodologyHistoryBrowse:insertData(PartPickMethodologyHistory.ListingSequence, "").                                                                                                       */
/*         PartPickMethodologyHistoryBrowse:insertData(PartPickMethodologyHistory.TypeCode, "left").                                                                                                          */
/*         PartPickMethodologyHistoryBrowse:insertData(PartPickMethodologyHistory.TypeName, "left").                                                                                                          */
/*         PartPickMethodologyHistoryBrowse:insertData(PartPickMethodologyHistory.TypeDescr, "left").                                                                                                         */
/*         PartPickMethodologyHistoryBrowse:insertData(STRING(PartPickMethodologyHistory.Active, "Yes/No")).                                                                                                  */
/*         PartPickMethodologyHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").                                                                                       */
/*         PartPickMethodologyHistoryBrowse:insertData(fDisplayDate&Time(PartPickMethodologyHistory.Created,"y/m/d H:M:S"), "left").                                                                          */
/*                                                                                                                                                                                                            */
/*                                                                                                                                                                                                            */
/*         /* Add hidden fields */                                                                                                                                                                            */
/*         PartPickMethodologyHistoryBrowse:insertHiddendata("PartPickMethodologyHistoryID",PartPickMethodologyHistory.PartPickMethodologyHistoryID).                                                         */
/*                                                                                                                                                                                                            */
/*         PartPickMethodologyHistoryBrowse:endRow().                                                                                                                                                         */
/*                                                                                                                                                                                                            */
/*      END. /* FOR EACH PartPickMethodologyHistory OF SerialMaskType NO-LOCK, */                                                                                                                             */
/*   END. /*IF AVAILABLE SerialMaskType THEN*/                                                                                                                                                                */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowse:endTable().                                                                                                                                                             */
/*   chrPageBuildError = chrPageBuildError + PartPickMethodologyHistoryBrowse:getErrors().                                                                                                                    */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowseForm:insertHiddenField("PartPickMethodologyHistoryID","").                                                                                                               */
/*   PartPickMethodologyHistoryBrowseForm:insertHiddenField("popup_partpickmethodologyhistory_browse","").                                                                                                    */
/*                                                                                                                                                                                                            */
/*   /* This adds all of the standard form field lists to the form */                                                                                                                                         */
/*   {webGetHiddenFormFields.i PartPickMethodologyHistoryBrowseForm}                                                                                                                                          */
/*                                                                                                                                                                                                            */
/*   /* Create Button Bar */                                                                                                                                                                                  */
/*   PartPickMethodologyHistoryBrowseButtons = NEW buttonBar().                                                                                                                                               */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowseButtons:addButton("partpickmethodologyhistory_browse_form_btn_details",                                                                                                  */
/*                                                fTL("Details"),                                                                                                                                             */
/*                                                "viewPartPickMethodologyHistoryDetails('partpickmethodologyhistory_details_form');",                                                                        */
/*                                                "Disabled").                                                                                                                                                */
/*                                                                                                                                                                                                            */
/*     /*Button for later if needed*/                                                                                                                                                                         */
/*/*   PartPickMethodologyHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */                                                                                                */
/*/*                                                fTL("Excel Export"),                                                     */                                                                               */
/*/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/                                                                               */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowseButtons:addButton("partpickmethodologyhistory_browse_form_btn_cancel",                                                                                                   */
/*                                                fTL("Cancel"),                                                                                                                                              */
/*                                                "disablePopup('partpickmethodologyhistory_browse_form_popup');").                                                                                           */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowseButtons:closeBar().                                                                                                                                                      */
/*                                                                                                                                                                                                            */
/*   /* Assign the Button Bar Object to the Form Object */                                                                                                                                                    */
/*   PartPickMethodologyHistoryBrowseForm:FormBrowse  = PartPickMethodologyHistoryBrowse.                                                                                                                     */
/*   PartPickMethodologyHistoryBrowseForm:FormButtons = PartPickMethodologyHistoryBrowseButtons.                                                                                                              */
/*   PartPickMethodologyHistoryBrowseForm:endForm().                                                                                                                                                          */
/*                                                                                                                                                                                                            */
/*   PartPickMethodologyHistoryBrowseForm:displayForm().                                                                                                                                                      */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pPartPickMethodologyHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartPickMethodologyHistoryDetails Procedure
PROCEDURE pPartPickMethodologyHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*/* This finds the ProcessEvent which is linked to this form */                                                             */
/*   {webGetWebForm.i "partpickmethodologyhistory"}                                                                  */
/*                                                                                                                           */
/*   chrDisplayFieldList  = "PartPickMethodologyHistoryID,PartPickMethodologyID,TypeCode,TypeName,TypeDescr,"                */
/*                             + "ListingSequence,Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".                    */
/*                                                                                                                           */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm = NEW dataForm("partpickmethodologyhistory_details_form").                        */
/*   PartPickMethodologyHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.                                              */
/*                                                                                                                           */
/*   /* Setup */                                                                                                             */
/*   PartPickMethodologyHistoryDetailsForm:FormWidth   = 545.                                                                */
/*   PartPickMethodologyHistoryDetailsForm:FormHeight  = 440.                                                                */
/*   PartPickMethodologyHistoryDetailsForm:FormTitle   = "Part Pick Methodology History Details".                            */
/*   PartPickMethodologyHistoryDetailsForm:FormType    = "large".                                                            */
/*                                                                                                                           */
/*   /* Column Layout */                                                                                                     */
/*   PartPickMethodologyHistoryDetailsForm:insertPaddingColumn(40).                                                          */
/*   PartPickMethodologyHistoryDetailsForm:insertColumn(150).                                                                */
/*   PartPickMethodologyHistoryDetailsForm:insertColumn(120).                                                                */
/*   PartPickMethodologyHistoryDetailsForm:insertColumn(20).                                                                 */
/*   PartPickMethodologyHistoryDetailsForm:insertColumn(4).                                                                  */
/*   PartPickMethodologyHistoryDetailsForm:insertColumn(40).                                                                 */
/*                                                                                                                           */
/*   /* Fields */                                                                                                            */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(fTL("History ID")).                                                   */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("PartPickMethodologyHistoryID", "", 200, TRUE).                   */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(fTL("PartPickMethodologyID")).                                        */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("PartPickMethodologyID", "", 200, TRUE).                          */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(fTL("TypeCode")).                                                     */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("TypeCode", "", 200, TRUE).                                       */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(fTL("TypeName")).                                                     */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("TypeName", "", 200, TRUE).                                       */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(fTL("TypeDescr")).                                                    */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("TypeDescr", "", 200, TRUE).                                      */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(fTL("ListingSequence")).                                              */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("ListingSequence", "", 200, TRUE).                                */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(fTL("Active")).                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).                                        */
/*   PartPickMethodologyHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").                                      */
/*   PartPickMethodologyHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").                                  */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel("User").                                                              */
/*   PartPickMethodologyHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).                                    */
/*   /* Insert the Status Codes */                                                                                           */
/*   FOR EACH GateUser NO-LOCK                                                                                               */
/*      BY GateUser.FullName:                                                                                                */
/*      PartPickMethodologyHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).*/
/*   END.                                                                                                                    */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:startRow().                                                                       */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel("Created").                                                           */
/*   PartPickMethodologyHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).                                    */
/*   /* Time fields have no label */                                                                                         */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                   */
/*   /* This has a label to separate the time */                                                                             */
/*   PartPickMethodologyHistoryDetailsForm:insertLabel(":").                                                                 */
/*   PartPickMethodologyHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                   */
/*                                                                                                                           */
/*   /* Add Hidden Fields*/                                                                                                  */
/*   PartPickMethodologyHistoryDetailsForm:insertHiddenField("partpickmethodology_browse_scroll","").                        */
/*   PartPickMethodologyHistoryDetailsForm:insertHiddenField("popup_partpickmethodologyhistory_browse", "").                 */
/*   PartPickMethodologyHistoryDetailsForm:insertHiddenField("PartPickMethodologyHistoryID","").                             */
/*   PartPickMethodologyHistoryDetailsForm:insertHiddenField("form_name","partpickmethodologyhistory_details_form").         */
/*   PartPickMethodologyHistoryDetailsForm:insertHiddenField("prog_name","adPartPickMethodology.p").                         */
/*                                                                                                                           */
/*   /* This adds all of the standard form field lists to the form */                                                        */
/*   {webGetHiddenFormFields.i PartPickMethodologyHistoryDetailsForm}                                                        */
/*                                                                                                                           */
/*   /* Create Button Bar */                                                                                                 */
/*   PartPickMethodologyHistoryDetailsButtons = NEW buttonBar().                                                             */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsButtons:addButton("partpickmethodologyhistory_details_form_btn_cancel",                */
/*                                                  fTL("Cancel"),                                                           */
/*                                                  "disablePopup('partpickmethodologyhistory_details_form_popup');").       */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsButtons:closeBar().                                                                    */
/*                                                                                                                           */
/*   /* Assign the Button Bar Object to the Form Object */                                                                   */
/*   PartPickMethodologyHistoryDetailsForm:FormButtons = PartPickMethodologyHistoryDetailsButtons.                           */
/*                                                                                                                           */
/*   PartPickMethodologyHistoryDetailsForm:endForm().                                                                        */
/*   PartPickMethodologyHistoryDetailsForm:displayForm().                                                                    */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

