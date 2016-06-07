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
DEFINE VARIABLE intSelectedDangerousGoods        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectDangerousGoodsRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToDangerousGoodsRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDangerousGoodsID              AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE DangerousGoodsBrowseFrame        AS pageFrame.
DEFINE VARIABLE DangerousGoodsBrowse             AS browseTable.
DEFINE VARIABLE DangerousGoodsBrowseButtons      AS buttonBar.
DEFINE VARIABLE DangerousGoodsDetailsForm        AS dataForm.
DEFINE VARIABLE DangerousGoodsDetailsButtons     AS buttonBar.

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
   
   ASSIGN chrDangerousGoodsID = get-value("DangerousGoodsID")
          intSelectedDangerousGoods = INTEGER(chrDangerousGoodsID)
          chrScrollToDangerousGoodsRow = STRING(INTEGER(get-value("dangerousgoods_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrDangerousGoodsID <> "" THEN
     chrSelectDangerousGoodsRow = 'selectDangerousGoodsRow(document.getElementById("dangerousgoods_browse_row_' + chrDangerousGoodsID + '"),"' + chrDangerousGoodsID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("dangerousgoods_browse").scrollTop=' + chrScrollToDangerousGoodsRow + chrSelectDangerousGoodsRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Dangerous Goods Admin".
   ThisPage:FrameTitle = "Dangerous Goods Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pDangerousGoodsBrowse.
   
   FIND DangerousGoods NO-LOCK 
      WHERE DangerousGoods.DangerousGoodsID = intSelectedDangerousGoods NO-ERROR.
   
   /******* Popup Browsers and Forms ********/    
   RUN pDangerousGoodsDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT DangerousGoodsBrowseFrame      NO-ERROR.
   DELETE OBJECT DangerousGoodsBrowse           NO-ERROR.
   DELETE OBJECT DangerousGoodsBrowseButtons    NO-ERROR.
   DELETE OBJECT DangerousGoodsDetailsForm      NO-ERROR.
   DELETE OBJECT DangerousGoodsDetailsButtons   NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDangerousGoodsBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsBrowse Procedure 
PROCEDURE pDangerousGoodsBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "DangerousGoods_details_form"}
   
   DangerousGoodsBrowse = NEW browseTable("DangerousGoods_browse").
   DangerousGoodsBrowse:BrowseWidth  = 965.
   DangerousGoodsBrowse:BrowseHeight = 455.
   DangerousGoodsBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   DangerousGoodsBrowse:insertColumn(fTL("DangerousGoods ID"), 85, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DangerousGoods}
   
   DangerousGoodsBrowse:insertColumn(fTL("DangerousGoodsRef"),           200, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("DangerousGoodsDescr"),         240, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("DangerousGoodsSubDivisionID"), 100, "INTEGER",   "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("UNCode"),                      100, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("DangerValue"),                 100, "INTEGER",   "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("Active"),                      110, "LOGICAL", FALSE).
   
   /*Body*/
   DangerousGoodsBrowse:startBody().
   
   FOR EACH DangerousGoods NO-LOCK /*idx=ActiveDangerousGoodsName*/
      WHERE DangerousGoods.Active
      BY    DangerousGoodsDescr:
      
      DangerousGoodsBrowse:startRow(DangerousGoods.DangerousGoodsID, "selectDangerousGoodsRow(this," + '"' + STRING(DangerousGoods.DangerousGoodsID) + '"' + ");", "").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerousGoodsID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i DangerousGoods}
      
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerousGoodsRef, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerousGoodsDescr, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerousGoodsSubDivisionID, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.UNCode, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerValue, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.Active).
      
      /* Add hidden fields */
      DangerousGoodsBrowse:insertHiddenData("DangerousGoodsVersionID",DangerousGoods.VersionID).
      
      DangerousGoodsBrowse:endRow().
      
   END. /*FOR EACH DangerousGoods NO-LOCK */
   
   DangerousGoodsBrowse:endTable().
   
   /* Create a new frame */
   DangerousGoodsBrowseFrame = NEW pageFrame().
   DangerousGoodsBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   DangerousGoodsBrowseFrame:FormAction="dbDangerousGoodsUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   DangerousGoodsBrowseFrame:formOpen("dangerousgoods_browse_form").
   
   /* Start the Frame Header */
   DangerousGoodsBrowseFrame:insertSpacer(5).
   DangerousGoodsBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   DangerousGoodsBrowse:displayBrowse().  
   
   /* End the Frame Header */
   DangerousGoodsBrowseFrame:frameClose().
   DangerousGoodsBrowseFrame:insertSpacer(10).
   
   DangerousGoodsBrowseFrame:insertHiddenField("dangerousgoods_browse_scroll","").
   DangerousGoodsBrowseFrame:insertHiddenField("DangerousGoodsID","").
   DangerousGoodsBrowseFrame:insertHiddenField("DangerousGoodsVersionID","").
   DangerousGoodsBrowseFrame:insertHiddenField("form_name", "dangerousgoods_browse_form").
   DangerousGoodsBrowseFrame:insertHiddenField("prog_name", "adDangerousGoods.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsBrowseFrame}
   
   DangerousGoodsBrowseFrame:formClose().
   
   /* Create Button Bar */
   DangerousGoodsBrowseButtons = NEW buttonBar().
   DangerousGoodsBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewDangerousGoodsDetails('dangerousgoods_details_form');",
                                 (IF intSelectedDangerousGoods > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_create",
                                 fTL("Create"),
                                 "createDangerousGoods('dangerousgoods_details_form');",
                                 "").
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_delete",
                                 fTL("Delete"),
                                 "confirmDeleteDangerousGoods();",
                                 (IF intSelectedDangerousGoods > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsBrowseButtons:closeBar().  
   DangerousGoodsBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDangerousGoodsDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsDetails Procedure 
PROCEDURE pDangerousGoodsDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "DangerousGoods_details_form"}
   
   ASSIGN chrDisplayFieldList  = "DangerousGoodsID,DangerousGoodsCode,DangerousGoodsName,Active"
          chrEditFieldList     = "DangerousGoodsCode,DangerousGoodsName,Active"
          chrNewFieldList      = "DangerousGoodsCode,DangerousGoodsName,Active"
          chrRequiredFieldList = "DangerousGoodsCode,DangerousGoodsName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   
   
DangerousGoodsID
DangerousGoodsRef
DangerousGoodsSubDivisionID
DangerousGoodsDescr
UNCode
DangerValue
Active
   
   
   DangerousGoodsDetailsForm = NEW dataForm("DangerousGoods_details_form").
   DangerousGoodsDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsDetailsForm:FormAction  = "dbDangerousGoodsUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DangerousGoodsDetailsForm:FormWidth   = 460.
   DangerousGoodsDetailsForm:FormHeight  = 300.
   DangerousGoodsDetailsForm:FormTitle   = "DangerousGoods Details".
   DangerousGoodsDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   DangerousGoodsDetailsForm:insertPaddingColumn(50).
   DangerousGoodsDetailsForm:insertColumn(90).
   DangerousGoodsDetailsForm:insertColumn(120).
   DangerousGoodsDetailsForm:insertColumn(20).
   DangerousGoodsDetailsForm:insertColumn(4).
   DangerousGoodsDetailsForm:insertColumn(110).
   
   /* Fields */
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("DangerousGoods ID").
   DangerousGoodsDetailsForm:insertTextField("DangerousGoodsID", "", 110, TRUE).  
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("DangerousGoods Code").
   DangerousGoodsDetailsForm:insertTextField("DangerousGoodsCode", "", 220, TRUE).  
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("DangerousGoods Name").
   DangerousGoodsDetailsForm:insertTextField("DangerousGoodsName", "", 220, TRUE).  
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel(fTL("Active")). 
   DangerousGoodsDetailsForm:insertComboField("Active", "", 110, TRUE).  
   DangerousGoodsDetailsForm:insertComboPairs("Active", "yes", "Active").
   DangerousGoodsDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pDangerousGoodsDetailsFields}
   
   /* Add Hidden Fields*/
   DangerousGoodsDetailsForm:insertHiddenField("dangerousgoods_browse_scroll", "").
   DangerousGoodsDetailsForm:insertHiddenField("form_name", "dangerousgoods_details_form").
   DangerousGoodsDetailsForm:insertHiddenField("prog_name", "adDangerousGoodsAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsDetailsForm}
   
   /* Create Button Bar */
   DangerousGoodsDetailsButtons = NEW buttonBar().
   
   DangerousGoodsDetailsButtons:addButton("dangerousgoods_details_form_btn_save", 
                                  fTL("Save"), 
                                  "updateDangerousGoods('dangerousgoods_details_form');").
   
   DangerousGoodsDetailsButtons:addButton("dangerousgoods_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('dangerousgoods_details_form_popup');").
   
   DangerousGoodsDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DangerousGoodsDetailsForm:FormButtons = DangerousGoodsDetailsButtons.
   
   DangerousGoodsDetailsForm:endForm(). 
   
   DangerousGoodsDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + DangerousGoodsDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDangerousGoodsDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsDetailsFields Procedure 
PROCEDURE pDangerousGoodsDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         DangerousGoodsDetailsForm:startRow().
         DangerousGoodsDetailsForm:insertLabel(fTL("Field Label")).
         DangerousGoodsDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adDangerousGoodsAdmin_dangerousgoods_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

