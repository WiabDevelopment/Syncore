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

  Author: Anthony Ferrari

  Created: 07/03/2016

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

DEFINE VARIABLE intSelectedAisle        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectAisleRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToAisleRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrAisleID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupAisleHistory    AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE AisleBrowseFrame        AS pageFrame.
DEFINE VARIABLE AisleBrowse             AS browseTable.
DEFINE VARIABLE AisleBrowseButtons      AS buttonBar.
DEFINE VARIABLE AisleDetailsForm        AS dataForm.
DEFINE VARIABLE AisleDetailsButtons     AS buttonBar.

DEFINE VARIABLE AisleHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE AisleHistoryBrowse         AS browseTable.
DEFINE VARIABLE AisleHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE AisleHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE AisleHistoryDetailsButtons AS buttonBar.



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
   
   ASSIGN chrAisleID          = get-value("AisleID")
          intSelectedAisle    = INTEGER(chrAisleID)
          chrScrollToAisleRow = STRING(INTEGER(get-value("aisle_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrAisleID <> "" THEN
     chrSelectAisleRow = 'selectAisleRow(document.getElementById("aisle_browse_row_' + chrAisleID + '"),"' 
                                                         + chrAisleID +  '");'.
   
   IF get-value('popup_aislehistory_browse') = "yes" THEN
      chrPopupAisleHistory  = 'enablePopup("aislehistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("aisle_browse").scrollTop=' + chrScrollToAisleRow 
                                                          + chrSelectAisleRow + chrPopupAisleHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Aisle Admin".
   ThisPage:FrameTitle    = "Aisle Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("aisle.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pAisleBrowse.
   
   FIND FIRST Aisle NO-LOCK
      WHERE Aisle.AisleID = intSelectedAisle NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pAisleDetails.
   RUN pAisleHistory.
   RUN pAisleHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT AisleBrowseFrame           NO-ERROR.
   DELETE OBJECT AisleBrowse                NO-ERROR.
   DELETE OBJECT AisleBrowseButtons         NO-ERROR.
   DELETE OBJECT AisleDetailsForm           NO-ERROR.
   DELETE OBJECT AisleDetailsButtons        NO-ERROR.
   
   DELETE OBJECT AisleHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT AisleHistoryBrowse            NO-ERROR.
   DELETE OBJECT AisleHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT AisleHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT AisleHistoryDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAisleBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAisleBrowse Procedure 
PROCEDURE pAisleBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "aisle_details_form"}
   
   AisleBrowse = NEW browseTable("aisle_browse").
   AisleBrowse:BrowseWidth  = 965.
   AisleBrowse:BrowseHeight = 455.
   AisleBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   AisleBrowse:insertColumn(fTL("AisleID"),       80, "INTEGER",   "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Aisle}
   
   AisleBrowse:insertColumn(fTL("Aisle Code"),   120, "CHARACTER", "left", FALSE).
   AisleBrowse:insertColumn(fTL("Aisle Name"),   120, "CHARACTER", "left", FALSE).
   AisleBrowse:insertColumn(fTL("WorkZone"),     150, "CHARACTER", "left", FALSE).
   AisleBrowse:insertColumn(fTL("Double Sided"),  90, "LOGICAL",           FALSE).
   AisleBrowse:insertColumn(fTL("Active"),        50, "LOGICAL",           FALSE).
   
   /*Body*/
   AisleBrowse:startBody().
   
   FOR EACH Aisle NO-LOCK /*idx=ActiveListingSequence*/
      BY Aisle.Active DESC:
         
      FIND FIRST WorkZone NO-LOCK /* idx=WorkZoneID */
            WHERE WorkZone.WorkZoneID = Aisle.WorkZoneID NO-ERROR.    
      
      AisleBrowse:startRow(Aisle.AisleID, "selectAisleRow(this," + '"' + STRING(Aisle.AisleID) 
                                                                  + '"' + ");", "").
      AisleBrowse:insertData(Aisle.AisleID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Aisle}
      
      AisleBrowse:insertData(Aisle.AisleCode, "left").
      AisleBrowse:insertData(Aisle.AisleName, "left").
      AisleBrowse:insertData((IF AVAILABLE WorkZone THEN WorkZone.WorkZoneName ELSE ""), "left").  
      AisleBrowse:insertData(STRING(Aisle.DoubleSided,"Yes/No")).
      AisleBrowse:insertData(STRING(Aisle.Active,"Yes/No")).
      
      /* Add hidden fields */
      AisleBrowse:insertHiddenData("AisleVersionID",Aisle.VersionID).
      
      AisleBrowse:endRow().
      
   END. /*FOR EACH Aisle NO-LOCK */
   
   AisleBrowse:endTable().
   chrPageBuildError = chrPageBuildError + AisleBrowse:getErrors().
   
   /* Create a new frame */
   AisleBrowseFrame = NEW pageFrame().
   AisleBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   AisleBrowseFrame:FormAction="dbAisleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   AisleBrowseFrame:formOpen("aisle_browse_form").
   
   /* Start the Frame Header */
   AisleBrowseFrame:insertSpacer(5).
   AisleBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   AisleBrowse:displayBrowse().  
   
   /* End the Frame Header */
   AisleBrowseFrame:frameClose().
   AisleBrowseFrame:insertSpacer(10).
   
   AisleBrowseFrame:insertHiddenField("aisle_browse_scroll","").
   AisleBrowseFrame:insertHiddenField("AisleID","").
   AisleBrowseFrame:insertHiddenField("AisleVersionID","").
   AisleBrowseFrame:insertHiddenField("popup_aislehistory_browse","").
   AisleBrowseFrame:insertHiddenField("form_name","aisle_browse_form").
   AisleBrowseFrame:insertHiddenField("prog_name","adAisle.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AisleBrowseFrame}
   
   AisleBrowseFrame:formClose().
   
   /* Create Button Bar */
   AisleBrowseButtons = NEW buttonBar().
   AisleBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   AisleBrowseButtons:addButton("aisle_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewAisleDetails('aisle_details_form');",
                                             "Disabled").
   
   AisleBrowseButtons:addButton("aisle_browse_form_btn_create",
                                             fTL("Create"),
                                             "createAisle('aisle_details_form');",
                                             "").
                                             
   AisleBrowseButtons:addButton("aisle_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   /*
   AisleBrowseButtons:addButton("aisle_browse_form_btn_delete",
                                             fTL("Delete"),
                                             "confirmDeleteAisle();",
                                             (IF intSelectedAisle > 0 THEN "" ELSE "Disabled")).
   */
   
   AisleBrowseButtons:closeBar().  
   AisleBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAisleDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAisleDetails Procedure 
PROCEDURE pAisleDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "aisle_details_form"}
   
   ASSIGN chrDisplayFieldList  = "AisleID,AisleCode,AisleName,WorkZoneID,DoubleSided,Active" 
          chrEditFieldList     = "DoubleSided,Active" 
          chrNewFieldList      = "AisleCode,AisleName,WorkZoneID,DoubleSided,Active" 
          chrRequiredFieldList = "AisleCode,AisleName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   AisleDetailsForm = NEW dataForm("aisle_details_form").
   AisleDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   AisleDetailsForm:FormAction = "dbAisleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   AisleDetailsForm:FormWidth   = 460.
   AisleDetailsForm:FormHeight  = 200.
   AisleDetailsForm:FormTitle   = "Aisle Details".
   AisleDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   AisleDetailsForm:insertPaddingColumn(30).
   AisleDetailsForm:insertColumn(150).
   AisleDetailsForm:insertColumn(160).
   AisleDetailsForm:insertColumn(20).
   AisleDetailsForm:insertColumn(4).
   AisleDetailsForm:insertColumn(110).
   
   /* Fields */
   AisleDetailsForm:startRow().
   AisleDetailsForm:insertLabel("Aisle ID").
   AisleDetailsForm:insertTextField("AisleID", "", 200, TRUE).  
   
   AisleDetailsForm:startRow().
   AisleDetailsForm:insertLabel("Aisle Code").
   AisleDetailsForm:insertTextField("AisleCode", "", 200, TRUE).  
   
   AisleDetailsForm:startRow().
   AisleDetailsForm:insertLabel("Aisle Name").
   AisleDetailsForm:insertTextField("AisleName", "", 200, TRUE).
   
   AisleDetailsForm:startRow().
   AisleDetailsForm:insertLabel("Work Zone").
   AisleDetailsForm:insertComboField("WorkZoneID", "", 200, TRUE).  
    /* WorkZone DropDown Options */
   FOR EACH WorkZone NO-LOCK /*idx=WorkZoneID*/
      WHERE WorkZone.Active = TRUE:
               
      AisleDetailsForm:insertComboPairs("WorkZoneID", STRING(WorkZone.WorkZoneID), WorkZone.WorkZoneName).    
   END.
   
   AisleDetailsForm:startRow().
   AisleDetailsForm:insertLabel(fTL("DoubleSided")). 
   AisleDetailsForm:insertComboField("DoubleSided", "", 200, TRUE).  
   AisleDetailsForm:insertComboPairs("DoubleSided", "yes", "Yes").
   AisleDetailsForm:insertComboPairs("DoubleSided", "no",  "No").

   AisleDetailsForm:startRow().
   AisleDetailsForm:insertLabel(fTL("Active")). 
   AisleDetailsForm:insertComboField("Active", "", 200, TRUE).  
   AisleDetailsForm:insertComboPairs("Active", "yes", "Active").
   AisleDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pAisleDetailsFields}
   
   /* Add Hidden Fields*/
   AisleDetailsForm:insertHiddenField("aisle_browse_scroll", "").
   AisleDetailsForm:insertHiddenField("form_name", "aisle_details_form").
   AisleDetailsForm:insertHiddenField("prog_name", "adAisle.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AisleDetailsForm}
   
   /* Create Button Bar */
   AisleDetailsButtons = NEW buttonBar().
   
   AisleDetailsButtons:addButton("aisle_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateAisle('aisle_details_form');").
   
   AisleDetailsButtons:addButton("aisle_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('aisle_details_form_popup');").
   
   AisleDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AisleDetailsForm:FormButtons = AisleDetailsButtons.
   
   AisleDetailsForm:endForm(). 
   
   AisleDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAisleDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAisleDetailsFields Procedure 
PROCEDURE pAisleDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         AisleDetailsForm:startRow().
         AisleDetailsForm:insertLabel(fTL("Field Label")).
         AisleDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAisleHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAisleHistory Procedure
PROCEDURE pAisleHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "aislehistory_details_form"}
   
   FIND FIRST Aisle WHERE Aisle.AisleID = intSelectedAisle NO-LOCK NO-ERROR.
   
   AisleHistoryBrowseForm = NEW dataForm("aislehistory_browse_form").
   AisleHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   AisleHistoryBrowseForm:FormWidth   = 850.
   AisleHistoryBrowseForm:FormHeight  = 540.
   AisleHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE Aisle THEN " for Aisle: " 
                                                                  + STRING(Aisle.AisleName) ELSE "").
   AisleHistoryBrowseForm:FormType    = "xxl_large".
   
   AisleHistoryBrowse = NEW browseTable("aislehistory_browse").
   AisleHistoryBrowse:BrowseWidth  = 830.
   AisleHistoryBrowse:BrowseHeight = 500.
   AisleHistoryBrowse:ExcelExport  = TRUE.
   AisleHistoryBrowse:SessionID    = intGblSessionID.
   
   
   AisleHistoryBrowse:insertColumn(fTL("History ID"),      80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i AisleHistory}

   
   AisleHistoryBrowse:insertColumn(fTL("Aisle Code"),     120, "CHARACTER", "left", FALSE).
   AisleHistoryBrowse:insertColumn(fTL("Aisle Name"),     120, "CHARACTER", "left", FALSE).
   AisleHistoryBrowse:insertColumn(fTL("Work Zone"),      120, "CHARACTER", "left", FALSE).
   AisleHistoryBrowse:insertColumn(fTL("Double Sided"),    90, "LOGICAL",   "left", FALSE).
   AisleHistoryBrowse:insertColumn(fTL("Active"),          70, "LOGICAL",           FALSE).
   AisleHistoryBrowse:insertColumn(fTL("User"),           100, "CHARACTER", "left", FALSE).
   AisleHistoryBrowse:insertColumn(fTL("Created"),        100, "CHARACTER", "left", FALSE).
   
   AisleHistoryBrowse:StartBody().
   
   IF AVAILABLE Aisle THEN
   DO:
      /*List the AisleHistory*/
      FOR EACH AisleHistory NO-LOCK 
         WHERE  AisleHistory.AisleID = intSelectedAisle
         BY AisleHistory.AisleHistoryID:
         
         FIND FIRST GateUser NO-LOCK /* idx=GateUserID */
            WHERE GateUser.GateUserID = AisleHistory.GateUserID NO-ERROR.
            
         FIND FIRST WorkZone NO-LOCK /* idx=WorkZoneID */
            WHERE WorkZone.WorkZoneID = AisleHistory.WorkZoneID NO-ERROR.   
       
         AisleHistoryBrowse:startRow(AisleHistory.AisleHistoryID, "selectHistoryRow(this," + '"' + STRING(AisleHistory.AisleHistoryID) 
                                                                     + '","aisleHistory"' + ");", "").
         AisleHistoryBrowse:insertData(AisleHistory.AisleHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i AisleHistory}
         
         AisleHistoryBrowse:insertData(AisleHistory.AisleCode, "left").
         AisleHistoryBrowse:insertData(AisleHistory.AisleName, "left").
         AisleHistoryBrowse:insertData((IF AVAILABLE WorkZone THEN WorkZone.WorkZoneName ELSE ""), "left").   
         AisleHistoryBrowse:insertData(STRING(AisleHistory.DoubleSided, "Yes/No")).                        
         AisleHistoryBrowse:insertData(STRING(AisleHistory.Active, "Yes/No")).
         AisleHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         AisleHistoryBrowse:insertData(fDisplayDate&Time(AisleHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */         
         AisleHistoryBrowse:insertHiddendata("AisleHistoryID",AisleHistory.AisleHistoryID).
         
         AisleHistoryBrowse:endRow().
      
      END. /* FOR EACH AisleHistory NO-LOCK, */
   END. /*IF AVAILABLE Aisle THEN*/
   
   AisleHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + AisleHistoryBrowse:getErrors().
   
   AisleHistoryBrowseForm:insertHiddenField("AisleHistoryID","").
   AisleHistoryBrowseForm:insertHiddenField("popup_aislehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AisleHistoryBrowseForm}
   
   /* Create Button Bar */
   AisleHistoryBrowseButtons = NEW buttonBar().                                                 
   
   AisleHistoryBrowseButtons:addButton("aislehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewAisleHistoryDetails('aislehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   AisleHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   AisleHistoryBrowseButtons:addButton("aislehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('aislehistory_browse_form_popup');").
   
   AisleHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AisleHistoryBrowseForm:FormBrowse  = AisleHistoryBrowse.
   AisleHistoryBrowseForm:FormButtons = AisleHistoryBrowseButtons.
   AisleHistoryBrowseForm:endForm(). 
   
   AisleHistoryBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pAisleHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAisleHistoryDetails Procedure
PROCEDURE pAisleHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "aislehistory_details_form"}
   
   chrDisplayFieldList  = "AisleHistoryID,AisleID,AisleCode,AisleName,WorkZoneID,DoubleSided"
                        + ",Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   AisleHistoryDetailsForm = NEW dataForm("aislehistory_details_form").
   AisleHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   AisleHistoryDetailsForm:FormWidth   = 460.
   AisleHistoryDetailsForm:FormHeight  = 300.
   AisleHistoryDetailsForm:FormTitle   = "Aisle History Details".
   AisleHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   AisleHistoryDetailsForm:insertPaddingColumn(40).
   AisleHistoryDetailsForm:insertColumn(110).
   AisleHistoryDetailsForm:insertColumn(120).
   AisleHistoryDetailsForm:insertColumn(20).
   AisleHistoryDetailsForm:insertColumn(4).
   AisleHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel(fTL("History ID")).
   AisleHistoryDetailsForm:insertTextField("AisleHistoryID", "", 200, TRUE).    
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel("Aisle ID").
   AisleHistoryDetailsForm:insertTextField("AisleID", "", 200, TRUE).  
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel("Aisle Code").
   AisleHistoryDetailsForm:insertTextField("AisleCode", "", 200, TRUE).  
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel("Aisle Name").
   AisleHistoryDetailsForm:insertTextField("AisleName", "", 200, TRUE).
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel("Work Zone").
   AisleHistoryDetailsForm:insertComboField("WorkZoneID", "", 200, TRUE).  
    /* WorkZone DropDown Options */
   FOR EACH WorkZone NO-LOCK /*idx=WorkZoneID*/
      WHERE WorkZone.Active = TRUE:
               
      AisleHistoryDetailsForm:insertComboPairs("WorkZoneID", STRING(WorkZone.WorkZoneID), WorkZone.WorkZoneName).    
   END.
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel(fTL("DoubleSided")). 
   AisleHistoryDetailsForm:insertComboField("DoubleSided", "", 200, TRUE).  
   AisleHistoryDetailsForm:insertComboPairs("DoubleSided", "yes", "Yes").
   AisleHistoryDetailsForm:insertComboPairs("DoubleSided", "no",  "No").
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel(fTL("Active")).
   AisleHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   AisleHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   AisleHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel("User").
   AisleHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      AisleHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   AisleHistoryDetailsForm:startRow().
   AisleHistoryDetailsForm:insertLabel("Created").
   AisleHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   AisleHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   AisleHistoryDetailsForm:insertLabel(":").
   AisleHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   AisleHistoryDetailsForm:insertHiddenField("aisle_browse_scroll","").
   AisleHistoryDetailsForm:insertHiddenField("popup_aislehistory_browse", "").
   AisleHistoryDetailsForm:insertHiddenField("AisleHistoryID","").
   AisleHistoryDetailsForm:insertHiddenField("form_name","aislehistory_details_form").
   AisleHistoryDetailsForm:insertHiddenField("prog_name","adAisle.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AisleHistoryDetailsForm}
   
   /* Create Button Bar */
   AisleHistoryDetailsButtons = NEW buttonBar().
   
   AisleHistoryDetailsButtons:addButton("aislehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('aislehistory_details_form_popup');").
                                        
   AisleHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   AisleHistoryDetailsForm:FormButtons = AisleHistoryDetailsButtons.
   
   AisleHistoryDetailsForm:endForm(). 
   AisleHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

