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


/* ShipOrderStatus Local Variables */
DEFINE VARIABLE intSelectedPrinterType    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectPrinterTypeRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToPrinterTypeRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPrinterTypeID          AS CHARACTER NO-UNDO.

/* ShipOrderStatus Objects */
DEFINE VARIABLE PrinterTypeBrowseFrame    AS pageFrame.
DEFINE VARIABLE PrinterTypeBrowse         AS browseTable.
DEFINE VARIABLE PrinterTypeBrowseButtons  AS buttonBar.
DEFINE VARIABLE PrinterTypeDetailsForm    AS dataForm.
DEFINE VARIABLE PrinterTypeDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pPrinterTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterTypeBrowse Procedure 
PROCEDURE pPrinterTypeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "printertype_details_form"}
   
   PrinterTypeBrowse              = NEW browseTable("printertype_browse").
   PrinterTypeBrowse:BrowseWidth  = 965.
   PrinterTypeBrowse:BrowseHeight = 455.
   PrinterTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   PrinterTypeBrowse:insertColumn(fTL("ID"),70, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PrinterType}
   
   PrinterTypeBrowse:insertColumn(fTL("Type Code"),   100, "CHARACTER", FALSE).
   PrinterTypeBrowse:insertColumn(fTL("Type Name"),   100, "CHARACTER", "left", FALSE).
   PrinterTypeBrowse:insertColumn(fTL("Description"), 120, "CHARACTER", "left", FALSE).
   PrinterTypeBrowse:insertColumn(fTL("Listing Seq"),  70, "INTEGER", "right", FALSE).
   PrinterTypeBrowse:insertColumn(fTL("Active"),       70, "LOGICAL", "left", FALSE).
   
   /*Body*/
   PrinterTypeBrowse:startBody().
   
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH PrinterType NO-LOCK
      BY    PrinterType.Active DESCENDING 
      BY    PrinterType.ListingSequence 
      BY    PrinterType.PrinterTypeID:
      
      PrinterTypeBrowse:startRow(PrinterType.PrinterTypeID, "selectPrinterTypeRow(this," + '"' + STRING(PrinterType.PrinterTypeID) + '"' + ");", "").
      PrinterTypeBrowse:insertData(PrinterType.PrinterTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PrinterType}

      PrinterTypeBrowse:insertData(PrinterType.TypeCode, "left").
      PrinterTypeBrowse:insertData(PrinterType.TypeName, "left").
      PrinterTypeBrowse:insertData(PrinterType.TypeDescr, "left").
      PrinterTypeBrowse:insertData(STRING(PrinterType.ListingSequence), "right").
      PrinterTypeBrowse:insertData(STRING(PrinterType.Active, "Yes/No")).
      
      /* Add hidden fields */
      PrinterTypeBrowse:insertHiddenData("PrinterTypeVersionID", PrinterType.VersionID).
      
      PrinterTypeBrowse:endRow().
      
   END. /* FOR EACH PrinterType NO-LOCK */
   
   PrinterTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PrinterTypeBrowse:getErrors().
   
   /* Create a new frame */
   PrinterTypeBrowseFrame           = NEW pageFrame().
   PrinterTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PrinterTypeBrowseFrame:FormAction="dbPrinterTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PrinterTypeBrowseFrame:formOpen("printertype_browse_form").
   
   /* Start the Frame Header */
   PrinterTypeBrowseFrame:insertSpacer(5).
   PrinterTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PrinterTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PrinterTypeBrowseFrame:frameClose().
   PrinterTypeBrowseFrame:insertSpacer(10).
   
   PrinterTypeBrowseFrame:insertHiddenField("printertype_browse_scroll", "").
   PrinterTypeBrowseFrame:insertHiddenField("PrinterTypeID", "").
   PrinterTypeBrowseFrame:insertHiddenField("PrinterTypeVersionID", "").
   PrinterTypeBrowseFrame:insertHiddenField("form_name", "printertype_browse_form").
   PrinterTypeBrowseFrame:insertHiddenField("prog_name", "adPrinterTypeAdmin.p").

   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PrinterTypeBrowseFrame}
   
   PrinterTypeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   PrinterTypeBrowseButtons           = NEW buttonBar().
   PrinterTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   PrinterTypeBrowseButtons:addButton("printertype_browse_form_btn_details",
                                      fTL("Details"),
                                      "viewPrinterTypeDetails('printertype_details_form');",
                                      "Disabled").
   
   IF NOT logPreventDataCreates THEN
   DO: 
      PrinterTypeBrowseButtons:addButton("printertype_browse_form_btn_create",
                                         fTL("Create"),
                                         "createPrinterType('printertype_details_form');",
                                         "").
      
      PrinterTypeBrowseButtons:addButton("printertype_browse_form_btn_applinks",
                                         fTL("App Links"),
                                         "viewAppLinkBrowse('printertype_browse_form','PrinterType');",
                                         "Disabled").
                                               
   END.
   
   
   PrinterTypeBrowseButtons:closeBar().  
   PrinterTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPrinterTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterTypeDetails Procedure 
PROCEDURE pPrinterTypeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "printertype_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PrinterTypeID,TypeName,TypeCode,TypeDescr,ListingSequence,Active"
          chrEditFieldList     = "TypeName,TypeDescr,ListingSequence,Active"
          chrNewFieldList      = "TypeName,TypeCode,TypeDescr,ListingSequence,Active"
          chrRequiredFieldList = "TypeName,TypeCode,TypeDescr,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   PrinterTypeDetailsForm           = NEW dataForm("printertype_details_form").
   PrinterTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PrinterTypeDetailsForm:FormAction = "dbPrinterTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PrinterTypeDetailsForm:FormWidth  = 460.
   PrinterTypeDetailsForm:FormHeight = 300.
   PrinterTypeDetailsForm:FormTitle  = "Printer Type Details".
   PrinterTypeDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   PrinterTypeDetailsForm:insertPaddingColumn(20).
   PrinterTypeDetailsForm:insertColumn(90).
   PrinterTypeDetailsForm:insertColumn(200).
   PrinterTypeDetailsForm:insertColumn(20).
   PrinterTypeDetailsForm:insertColumn(4).
   PrinterTypeDetailsForm:insertColumn(50).
   
   /* Fields */
   PrinterTypeDetailsForm:startRow().
   PrinterTypeDetailsForm:insertLabel("PrinterType ID").
   PrinterTypeDetailsForm:insertTextField("PrinterTypeID", "", 110, TRUE).  

   PrinterTypeDetailsForm:startRow().
   PrinterTypeDetailsForm:insertLabel("Type Name").
   PrinterTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).
      
   PrinterTypeDetailsForm:startRow().
   PrinterTypeDetailsForm:insertLabel("Type Code").
   PrinterTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).  
   
   PrinterTypeDetailsForm:startRow().
   PrinterTypeDetailsForm:insertLabel("Description").
   PrinterTypeDetailsForm:insertTextField("TypeDescr", "", 300, TRUE).
   
   PrinterTypeDetailsForm:startRow().
   PrinterTypeDetailsForm:insertLabel("Listing Seq").
   PrinterTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   PrinterTypeDetailsForm:startRow().
   PrinterTypeDetailsForm:insertLabel(fTL("Active")). 
   PrinterTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PrinterTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   PrinterTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPrinterDetailsFields}
   
   /* Add Hidden Fields*/
   PrinterTypeDetailsForm:insertHiddenField("printertype_browse_scroll", "").
   PrinterTypeDetailsForm:insertHiddenField("form_name", "printertype_details_form").
   PrinterTypeDetailsForm:insertHiddenField("prog_name", "adPrinterTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PrinterTypeDetailsForm}
   
   /* Create Button Bar */
   PrinterTypeDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
   DO:
      PrinterTypeDetailsButtons:addButton("printertype_details_form_btn_save", 
                                          fTL("Save"), 
                                          "updatePrinterType('printertype_details_form');").
   END.     
   
   PrinterTypeDetailsButtons:addButton("printertype_details_form_btn_cancel", 
                                       fTL("Cancel"), 
                                       "cancelUpdate('UserCancelled','process_mode'); disablePopup('printertype_details_form_popup');").
   
   PrinterTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PrinterTypeDetailsForm:FormButtons = PrinterTypeDetailsButtons.
   
   PrinterTypeDetailsForm:endForm(). 
   
   PrinterTypeDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPrinterTypeDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterTypeDetailsFields Procedure 
PROCEDURE pPrinterTypeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         PrinterTypeDetailsForm:startRow().
         PrinterTypeDetailsForm:insertLabel(fTL("Field Label")).
         PrinterTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adPrinterTypeAdmin_printertype_details_form.i}
      
      
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
   
   ASSIGN chrPrinterTypeID          = get-value("PrinterTypeID")
          intSelectedPrinterType    = INTEGER(chrPrinterTypeID)
          chrScrollToPrinterTypeRow = STRING(INTEGER(get-value("printertype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPrinterTypeID <> "" THEN
      chrSelectPrinterTypeRow = 'selectPrinterTypeRow(document.getElementById("printertype_browse_row_' 
                                   + chrPrinterTypeID + '"),"' + chrPrinterTypeID + '");'.
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad 
                   + 'document.getElementById("printertype_browse").scrollTop=' + chrScrollToPrinterTypeRow
                   + chrSelectPrinterTypeRow.
   
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Printer Type Admin".
   ThisPage:FrameTitle    = "Printer Type Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("printertype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
      
   /******* Main Browser ********************/
   RUN pPrinterTypeBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pPrinterTypeDetails.
   
   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT PrinterTypeBrowseFrame         NO-ERROR.
   DELETE OBJECT PrinterTypeBrowse              NO-ERROR.
   DELETE OBJECT PrinterTypeBrowseButtons       NO-ERROR.
   DELETE OBJECT PrinterTypeDetailsForm         NO-ERROR.
   DELETE OBJECT PrinterTypeDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

