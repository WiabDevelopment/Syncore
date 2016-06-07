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

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedPartStatus     AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectPartStatusRow    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToPartStatusRow  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPartStatusID           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupPartStatusHistory AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectedFaiField       AS CHARACTER NO-UNDO.

/**
/* Definitions for System Options for Admin */
{getAdminOptions.i}
**/

/* Objects */
DEFINE VARIABLE PartStatusBrowseFrame    AS pageFrame.
DEFINE VARIABLE PartStatusBrowse         AS browseTable.
DEFINE VARIABLE PartStatusBrowseButtons  AS buttonBar.
DEFINE VARIABLE PartStatusDetailsForm    AS dataForm.
DEFINE VARIABLE PartStatusDetailsButtons AS buttonBar.

DEFINE VARIABLE PartStatusHistoryBrowse     AS browseTable.
DEFINE VARIABLE PartStatusHistoryButtons    AS buttonBar.
DEFINE VARIABLE PartStatusHistoryBrowseForm AS dataForm.

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

&IF DEFINED(EXCLUDE-pPartStatusBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartStatusBrowse Procedure 
PROCEDURE pPartStatusBrowse :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form that is passed in */
   {webGetWebForm.i "partstatus_details_form"}

   PartStatusBrowse              = NEW browseTable("partstatus_browse").
   PartStatusBrowse:BrowseWidth  = 965.
   PartStatusBrowse:BrowseHeight = 455.
   PartStatusBrowse:WebStream    = STREAM WebStream:HANDLE.

   /* Add in the ID as first Column */
   PartStatusBrowse:insertColumn(fTL("PartStatus ID"),        80, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartStatus}

   PartStatusBrowse:insertColumn(fTL("Listing Seq"),         90, "INTEGER", FALSE).
   PartStatusBrowse:insertColumn(fTL("Code"),               100, "INTEGER",   "left", FALSE).
   PartStatusBrowse:insertColumn(fTL("Name"),                90, "CHARACTER", "left", FALSE).
   PartStatusBrowse:insertColumn(fTL("Status Description"), 250, "CHARACTER", "left", FALSE).
   PartStatusBrowse:insertColumn(fTL("Active"),             120, "LOGICAL", FALSE).

   /*Body*/
   PartStatusBrowse:startBody().

   /* Find all PartStatus then sort by ID */
   FOR EACH PartStatus NO-LOCK
      BY    PartStatus.PartStatusID:
      PartStatusBrowse:startRow(PartStatus.PartStatusID, "selectPartStatusRow(this," + '"'
                                                     + STRING(PartStatus.PartStatusID) + '"' + ");", "").
                                                     
      PartStatusBrowse:insertData(PartStatus.PartStatusID).

      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PartStatus}

      PartStatusBrowse:insertData(PartStatus.ListingSequence).
      PartStatusBrowse:insertData(PartStatus.StatusCode, "left").
      PartStatusBrowse:insertData(PartStatus.StatusName, "left").
      PartStatusBrowse:insertData(PartStatus.StatusDescr, "left").
      PartStatusBrowse:insertData(STRING(PartStatus.Active, "Yes/No")).

      /* Add hidden fields */
      PartStatusBrowse:insertHiddenData("PartStatusVersionID",PartStatus.VersionID).

      PartStatusBrowse:endRow().

   END. /* FOR EACH PartStatus NO-LOCK */

   PartStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PartStatusBrowse:getErrors().

   /* Create a new frame */
   PartStatusBrowseFrame           = NEW pageFrame().
   PartStatusBrowseFrame:WebStream = STREAM WebStream:HANDLE.

   /* Insert a form */
   PartStatusBrowseFrame:FormAction="dbPartStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PartStatusBrowseFrame:formOpen("partstatus_browse_form").

   /* Start the Frame Header */
   PartStatusBrowseFrame:insertSpacer(5).
   PartStatusBrowseFrame:frameOpen(985, 500, "").

   /* This outputs the Browse Table */
   PartStatusBrowse:displayBrowse().

   /* End the Frame Header */
   PartStatusBrowseFrame:frameClose().
   PartStatusBrowseFrame:insertSpacer(10).

   PartStatusBrowseFrame:insertHiddenField("partstatus_browse_scroll","").
   PartStatusBrowseFrame:insertHiddenField("PartStatusID","").
   PartStatusBrowseFrame:insertHiddenField("PartStatusVersionID","").
   PartStatusBrowseFrame:insertHiddenField("form_name","partstatus_browse_form").
   PartStatusBrowseFrame:insertHiddenField("prog_name","adPartStatus.p").
   PartStatusBrowseFrame:insertHiddenField("popup_partstatushistory_browse", "").


   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartStatusBrowseFrame}

   PartStatusBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   PartStatusBrowseButtons           = NEW buttonBar().
   PartStatusBrowseButtons:WebStream = STREAM WebStream:HANDLE.


   PartStatusBrowseButtons:addButton("partstatus_browse_form_btn_details",
                                     fTL("Details"),
                                     "viewPartStatusDetails('partstatus_details_form');",
                                     (IF intSelectedPartStatus > 0 THEN "" ELSE "Disabled")).

   IF NOT logPreventDataCreates THEN
   DO:   
      PartStatusBrowseButtons:addButton("partstatus_browse_form_btn_create",
                                        fTL("Create"),
                                        "createPartStatus('partstatus_details_form');",
                                        "").
                                        
      PartStatusBrowseButtons:addButton("partstatus_browse_form_btn_applinks",
                                        fTL("App Links"),
                                        "viewAppLinkBrowse('partstatus_browse_form','PartStatus');",
                                        "Disabled").
                                                                                                                        
   END.                                     

   PartStatusBrowseButtons:addButton("partstatus_browse_form_btn_history",
                                     fTL("History"),
                                     "viewPartStatusHistory('partstatushistory_browse_form');",
                                     (IF intSelectedPartStatus > 0 THEN "" ELSE "Disabled")).


   PartStatusBrowseButtons:closeBar().
   PartStatusBrowseButtons:displayButtonBar().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartStatusDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartStatusDetails Procedure 
PROCEDURE pPartStatusDetails :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form that is passed in for use below */
   {webGetWebForm.i  "partstatus_details_form"}

   ASSIGN chrDisplayFieldList  = "PartStatusID,StatusCode,StatusName,StatusDescr,Active,ListingSequence"
          chrEditFieldList     = "StatusCode,StatusName,StatusDescr,Active,ListingSequence"
          chrNewFieldList      = "StatusCode,StatusName,StatusDescr,Active,ListingSequence"
          chrRequiredFieldList = "StatusCode,StatusName,StatusDescr,Active,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".


   PartStatusDetailsForm           = NEW dataForm("partstatus_details_form").
   PartStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.

   PartStatusDetailsForm:FormAction = "dbPartStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartStatusDetailsForm:FormWidth  = 460.
   PartStatusDetailsForm:FormHeight = 300.
   PartStatusDetailsForm:FormTitle  = "Part Status Details".
   PartStatusDetailsForm:FormType   = "medium".

   /* Column Layout */
   PartStatusDetailsForm:insertPaddingColumn(10).
   PartStatusDetailsForm:insertColumn(120).
   PartStatusDetailsForm:insertColumn(120).
   PartStatusDetailsForm:insertColumn(10).

   /* Fields */
   PartStatusDetailsForm:startRow().
   PartStatusDetailsForm:insertLabel("PartStatus ID").
   PartStatusDetailsForm:insertTextField("PartStatusID", "", 90, TRUE).

   PartStatusDetailsForm:startRow().
   PartStatusDetailsForm:insertLabel("Listing Seq").
   PartStatusDetailsForm:insertTextField("ListingSequence", "", 90, TRUE).

   PartStatusDetailsForm:startRow().
   PartStatusDetailsForm:insertLabel("Status Code").
   PartStatusDetailsForm:insertTextField("StatusCode", "", 90, TRUE).

   PartStatusDetailsForm:startRow().
   PartStatusDetailsForm:insertLabel("StatusName").
   PartStatusDetailsForm:insertTextField("StatusName", "", 90, TRUE).

   PartStatusDetailsForm:startRow().
   PartStatusDetailsForm:insertLabel("StatusDescr").
   PartStatusDetailsForm:insertTextField("StatusDescr", "", 300, TRUE).

   PartStatusDetailsForm:startRow().
   PartStatusDetailsForm:insertLabel(fTL("Active")).
   PartStatusDetailsForm:insertComboField("Active",  "", 210, TRUE).
   PartStatusDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartStatusDetailsForm:insertComboPairs("Active",  "no", "Not Active").

   {webGetOptionalFormFields.i pPartStatusDetailsFields}

   /* Add Hidden Fields*/
   PartStatusDetailsForm:insertHiddenField("partstatus_browse_scroll", "").
   PartStatusDetailsForm:insertHiddenField("form_name", "partstatus_details_form").
   PartStatusDetailsForm:insertHiddenField("prog_name", "adPartStatusAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartStatusDetailsForm}

   /* Create Button Bar */
   PartStatusDetailsButtons = NEW buttonBar().

   IF NOT logPreventDataUpdates THEN
      PartStatusDetailsButtons:addButton("partstatus_details_form_btn_save",
                                         fTL("Save"),
                                         "updatePartStatus('partstatus_details_form');").

   PartStatusDetailsButtons:addButton("partstatus_details_form_btn_cancel",
                                      fTL("Cancel"),
                                      "cancelUpdate('UserCancelled','process_mode'); disablePopup('partstatus_details_form_popup');").

   PartStatusDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartStatusDetailsForm:FormButtons = PartStatusDetailsButtons.

   PartStatusDetailsForm:endForm().

   PartStatusDetailsForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartStatusDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartStatusDetailsFields Procedure
PROCEDURE pPartStatusDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         PartStatusDetailsForm:startRow().
         PartStatusDetailsForm:insertLabel(fTL("Field Label")).
         PartStatusDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adPartStatusAdmin_partstatus_details_form.i}
      
   END CASE. /*chrOption:*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pPartStatusHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartStatusHistory Procedure 
PROCEDURE pPartStatusHistory :
/*------------------------------------------------------------------------------
   Purpose:
   Parameters:  <none>
   Notes:
 -----------------------------------------------------------------------------*/

    PartStatusHistoryBrowseForm           = NEW dataForm("partstatushistory_browse_form").
    PartStatusHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.

    /* Setup */
    PartStatusHistoryBrowseForm:FormWidth   = 900.
    PartStatusHistoryBrowseForm:FormHeight  = 540.
    PartStatusHistoryBrowseForm:FormTitle   = fTL("Part Status History").
    PartStatusHistoryBrowseForm:FormType    = "xxl_large".

    PartStatusHistoryBrowse= NEW browseTable("partstatushistory_browse").
    PartStatusHistoryBrowse:BrowseWidth  = 875.
    PartStatusHistoryBrowse:BrowseHeight = 480.

    PartStatusHistoryBrowse:insertColumn(fTL("ID"), 30, "INTEGER", FALSE).

    /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
    {webGetOptionalBrowseHeaders.i PartStatusHistory}

    PartStatusHistoryBrowse:insertColumn(fTL("StatusID"),        60, "CHARACTER", "left", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("Code"),            70, "CHARACTER", "left", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("Name"),            70, "CHARACTER", "left", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("Description"),    120, "CHARACTER", "left", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("Seq"),             50, "CHARACTER", "left", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("Active"),          60, "CHARACTER", "left", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("Created"),        125, "CHARACTER", "left", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("Transaction"),     80, "CHARACTER", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("OperationType"),   90, "CHARACTER", FALSE).
    PartStatusHistoryBrowse:insertColumn(fTL("User"),           110, "CHARACTER", "left", FALSE).


    PartStatusHistoryBrowse:StartBody().

    /* Add PartStatusHistory Query and build Browse */
    IF AVAILABLE PartStatus THEN
    DO:

       FOR EACH PartStatusHistory  NO-LOCK
          WHERE PartStatusHistory.PartStatusID = PartStatus.PartStatusID.

          FIND FIRST GateUser NO-LOCK
             WHERE GateUser.GateUserID = PartStatusHistory.GateUserID NO-ERROR.

          PartStatusHistoryBrowse:startRow(PartStatusHistory.PartStatusHistoryID,"","").

          PartStatusHistoryBrowse:insertData(PartStatusHistory.PartStatusHistoryID).

          /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
          {webGetOptionalBrowseFields.i PartStatusHistory}

          PartStatusHistoryBrowse:insertData(PartStatusHistory.PartStatusID, "Left").
          PartStatusHistoryBrowse:insertData(PartStatusHistory.StatusCode, "Left").
          PartStatusHistoryBrowse:insertData(PartStatusHistory.StatusName, "Left").
          PartStatusHistoryBrowse:insertData(PartStatusHistory.StatusDescr, "Left").
          PartStatusHistoryBrowse:insertData(PartStatusHistory.ListingSequence, "Left").
          PartStatusHistoryBrowse:insertData(PartStatusHistory.Active).
          PartStatusHistoryBrowse:insertData(fDisplayDate&Time(PartStatusHistory.Created,"y/m/d H:M:S"), "Left").
          PartStatusHistoryBrowse:insertData(PartStatusHistory.TransactionID).
          PartStatusHistoryBrowse:insertData(PartStatusHistory.OperationTypeID).
          PartStatusHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "Left").

          /* Add hidden fields */
          PartStatusHistoryBrowse:insertHiddenData("PartStatusHistoryID",PartStatusHistory.PartStatusHistoryID).

          PartStatusHistoryBrowse:endRow().
       END.
    END.

    PartStatusHistoryBrowse:endTable().

    PartStatusHistoryBrowseForm:insertHiddenField("popup_partstatushistory_browse","").
    PartStatusHistoryBrowseForm:insertHiddenField("PartID","").


    /* This adds all of the standard form field lists to the form */
    {webGetHiddenFormFields.i PartStatusHistoryBrowseForm}

    /* Create Button Bar */
    PartStatusHistoryButtons= NEW buttonBar().

    PartStatusHistoryButtons:addButton("partstatushistory_browse_form_btn_cancel",
                                       fTL("Cancel"),
                                       "disablePopup('partstatushistory_browse_form_popup');").
    PartStatusHistoryButtons:closeBar().

    /* Assign the Button Bar Object to the Form Object */
    PartStatusHistoryBrowseForm:FormBrowse  = PartStatusHistoryBrowse.
    PartStatusHistoryBrowseForm:FormButtons = PartStatusHistoryButtons.
    PartStatusHistoryBrowseForm:endForm().

    PartStatusHistoryBrowseForm:displayForm().

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

   ASSIGN chrPartStatusID          = get-value("PartStatusID")
          intSelectedPartStatus  = INTEGER(chrPartStatusID)
          chrScrollToPartStatusRow = STRING(INTEGER(get-value("partstatus_browse_scroll"))) + ";".

   /* Process URL values */
   IF chrPartStatusID <> "" THEN
      chrSelectPartStatusRow = 'selectPartStatusRow(document.getElementById("partstatus_browse_row_' + chrPartStatusID + '"),"'
                                                         + chrPartStatusID +  '");'.

   IF get-value('popup_partstatushistory_browse') = "yes" THEN
      chrPopupPartStatusHistory = 'enablePopup("partstatushistory_browse_form_popup");'.


   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("partstatus_browse").scrollTop=' + chrScrollToPartStatusRow
                             + chrSelectPartStatusRow + chrPopupPartStatusHistory.
                             
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                             
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Part Status Admin".
   ThisPage:FrameTitle    = "Part Status Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.

   /* Include CSS Files */
   {webCssFiles.i}

   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("partstatus.js").

   ThisPage:PageHeader().

   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pPartStatusBrowse.

   FIND FIRST PartStatus NO-LOCK
      WHERE PartStatus.PartStatusID = intSelectedPartStatus NO-ERROR.

   /******* Pop-up Browsers and Forms ********/
   RUN pPartStatusDetails.

   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}

   RUN pPartStatusHistory.

   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}

   ThisPage:PageFooter().

   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}

   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}

   /* Delete objects defined locally */
   DELETE OBJECT PartStatusBrowseFrame    NO-ERROR.
   DELETE OBJECT PartStatusBrowse         NO-ERROR.
   DELETE OBJECT PartStatusBrowseButtons  NO-ERROR.
   DELETE OBJECT PartStatusDetailsForm    NO-ERROR.
   DELETE OBJECT PartStatusDetailsButtons NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

