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

DEFINE VARIABLE intSelectedCustDbTableLink          AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCustDbTableLinkRow         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCustDbTableLinkRow       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCustDbTableLinkID                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupHistory                     AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE CustDbTableLinkBrowseFrame          AS pageFrame.
DEFINE VARIABLE CustDbTableLinkBrowse               AS browseTable.
DEFINE VARIABLE CustDbTableLinkBrowseButtons        AS buttonBar.
DEFINE VARIABLE CustDbTableLinkDetailsForm          AS dataForm.
DEFINE VARIABLE CustDbTableLinkDetailsButtons       AS buttonBar.

DEFINE VARIABLE CustDbTableLinkHistBrowseForm    AS dataform. 
DEFINE VARIABLE CustDbTableLinkHistBrowse        AS browseTable.
DEFINE VARIABLE CustDbTableLinkHistBrowseButtons AS buttonBar.




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

&IF DEFINED(EXCLUDE-pCustDbTableLinkHistBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustDbTableLinkHistBrowse Procedure
PROCEDURE pCustDbTableLinkHistBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "custdbtablelink_details_form"}

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   CustDbTableLinkHistBrowseForm           = NEW dataForm("custdbtablelinkhistory_browse_form").
   CustDbTableLinkHistBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CustDbTableLinkHistBrowseForm:FormWidth  = 860.
   CustDbTableLinkHistBrowseForm:FormHeight = 530.
   CustDbTableLinkHistBrowseForm:FormTitle  = fTL("CustDb Table Link History") + (IF AVAILABLE CustDbTableLink THEN fTL("for CustDbTableLinkID: ")
                                                + STRING(CustDbTableLink.CustDbTableLinkID) ELSE "").
   CustDbTableLinkHistBrowseForm:FormType   = "xxl_large".
   CustDbTableLinkHistBrowse                = NEW browseTable("custdbtablelinkhistory_browse").
   CustDbTableLinkHistBrowse:BrowseWidth    = 840.
   CustDbTableLinkHistBrowse:BrowseHeight   = 490.
   
   CustDbTableLinkHistBrowse:insertColumn(fTL("History ID"),         60, "INTEGER",           FALSE).
   CustDbTableLinkHistBrowse:insertColumn(fTL("User"),              130, "CHARACTER", "LEFT", FALSE).
   CustDbTableLinkHistBrowse:insertColumn(fTL("Core Table Name"),   140, "CHARACTER", "LEFT", FALSE).
   CustDbTableLinkHistBrowse:insertColumn(fTL("Cust Table Name"),   140, "CHARACTER", "LEFT", FALSE).
   CustDbTableLinkHistBrowse:insertColumn(fTL("CreateCust Rec"),    110, "LOGICAL",           FALSE).
   CustDbTableLinkHistBrowse:insertColumn(fTL("Created"),           130, "CHARACTER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CustDbTableLinkHist}
   
   CustDbTableLinkHistBrowse:StartBody().
   
   FOR EACH CustDbTableLinkHist NO-LOCK
      WHERE CustDbTableLinkHist.CustDbTableLinkID = intSelectedCustDbTableLink
         BY CustDbTableLinkHist.CustDbTableLinkHistID DESCENDING:
            
      FIND FIRST GateUser OF CustDbTableLinkHist NO-LOCK NO-ERROR.      

      CustDbTableLinkHistBrowse:startRow(CustDbTableLinkHist.CustDbTableLinkHistID, "selectCustDbTableLinkHistoryRow(this," + '"' + STRING(CustDbTableLinkHist.CustDbTableLinkHistID) + '"' + ");", "").
                                                                                 
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i CustDbTableLinkHist}
      
      CustDbTableLinkHistBrowse:insertData(CustDbTableLinkHist.CustDbTableLinkHistID, "LEFT").
      CustDbTableLinkHistBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      CustDbTableLinkHistBrowse:insertData(CustDbTableLinkHist.CoreTableName, "LEFT").
      CustDbTableLinkHistBrowse:insertData(CustDbTableLinkHist.CustTableName, "LEFT").
      CustDbTableLinkHistBrowse:insertData(CustDbTableLinkHist.CreateCustRecords, "").
      CustDbTableLinkHistBrowse:insertData(fDisplayDate&Time(CustDbTableLinkHist.Created,"y/m/d H:M:S"), "").
      
      CustDbTableLinkHistBrowse:endRow().
   END. /* FOR EACH CustDbTableLinkHist */
   
   CustDbTableLinkHistBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CustDbTableLinkHistBrowse:getErrors().
   
   CustDbTableLinkHistBrowseForm:insertHiddenField("popup_custdbtablelinkhistory_browse","").
   CustDbTableLinkHistBrowseForm:insertHiddenField("CustDbTableLinkHistID","").
   CustDbTableLinkHistBrowseForm:insertHiddenField("display_field_list","").
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CustDbTableLinkHistBrowseForm}
   
   /* Create Button Bar */
   CustDbTableLinkHistBrowseButtons = NEW buttonBar().

/*   CustDbTableLinkHistBrowseButtons:addButton("custdbtablelinkhistory_browse_form_btn_view",                              */
/*                                              fTL("Details"),                                                             */
/*                                              "viewCustDbTableLinkHistoryDetails('custdbtablelinkhistory_details_form');",*/
/*                                              "Disabled").                                                                */
   

   CustDbTableLinkHistBrowseButtons:addButton("custdbtablelinkhistory_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('custdbtablelinkhistory_browse_form_popup');").
   
   CustDbTableLinkHistBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CustDbTableLinkHistBrowseForm:FormBrowse  = CustDbTableLinkHistBrowse.
   CustDbTableLinkHistBrowseForm:FormButtons = CustDbTableLinkHistBrowseButtons.
   CustDbTableLinkHistBrowseForm:endForm(). 
   
   CustDbTableLinkHistBrowseForm:displayForm().   

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
   
   ASSIGN chrCustDbTableLinkID          = get-value("CustDbTableLinkID")
          intSelectedCustDbTableLink    = INTEGER(chrCustDbTableLinkID)
          chrScrollToCustDbTableLinkRow = STRING(INTEGER(get-value("custdbtablelink_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrCustDbTableLinkID <> "" THEN
      chrSelectCustDbTableLinkRow = 'selectCustDbTableLinkRow(document.getElementById("custdbtablelink_browse_row_' + chrCustDbTableLinkID + '"),"' 
                                + chrCustDbTableLinkID +  '");'.
                                
   IF get-value('popup_custdbtablelinkhistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("custdbtablelinkhistory_browse_form_popup");'.                              
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("custdbtablelink_browse").scrollTop=' + chrScrollToCustDbTableLinkRow 
                             + chrSelectCustDbTableLinkRow + chrPopupHistory.
                             
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   
                                
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "CustDbTableLink Admin".
   ThisPage:FrameTitle    = "CustDbTableLink Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("custdbtablelink.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pCustDbTableLinkBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pCustDbTableLinkDetails.
   RUN pCustDbTableLinkHistBrowse.

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
   DELETE OBJECT CustDbTableLinkBrowseFrame         NO-ERROR.
   DELETE OBJECT CustDbTableLinkBrowse              NO-ERROR.
   DELETE OBJECT CustDbTableLinkBrowseButtons       NO-ERROR.
   DELETE OBJECT CustDbTableLinkDetailsForm         NO-ERROR.
   DELETE OBJECT CustDbTableLinkDetailsButtons      NO-ERROR.
   
   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCustDbTableLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustDbTableLinkBrowse Procedure 
PROCEDURE pCustDbTableLinkBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "custdbtablelink_details_form"}
   
   CustDbTableLinkBrowse              = NEW browseTable("custdbtablelink_browse").
   CustDbTableLinkBrowse:BrowseWidth  = 965.
   CustDbTableLinkBrowse:BrowseHeight = 455.
   CustDbTableLinkBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CustDbTableLinkBrowse:insertColumn(fTL("LinkID"),             90, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CustDbTableLink}
   
   CustDbTableLinkBrowse:insertColumn(fTL("Core Table Name"),   230, "CHARACTER", "left", FALSE).
   CustDbTableLinkBrowse:insertColumn(fTL("Cust Table Name"),   240, "CHARACTER", "left", FALSE).
   CustDbTableLinkBrowse:insertColumn(fTL("CreateCustRecords"), 120, "LOGICAL", FALSE).

   
   /*Body*/
   CustDbTableLinkBrowse:startBody().
   
   FOR EACH CustDbTableLink NO-LOCK 
      BY    CustDbTableLink.CustDbTableLinkID:
      
      CustDbTableLinkBrowse:startRow(CustDbTableLink.CustDbTableLinkID, 
                             "selectCustDbTableLinkRow(this," + '"' + STRING(CustDbTableLink.CustDbTableLinkID) + '"' + ");", 
                             "").
      CustDbTableLinkBrowse:insertData(CustDbTableLink.CustDbTableLinkID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CustDbTableLink}
      
      CustDbTableLinkBrowse:insertData(CustDbTableLink.CoreTableName, "left").
      CustDbTableLinkBrowse:insertData(CustDbTableLink.CustTableName, "left").
      CustDbTableLinkBrowse:insertData(STRING(CustDbTableLink.CreateCustRecords, "Yes/No")).

      
      /* Add hidden fields */
      CustDbTableLinkBrowse:insertHiddenData("CustDbTableLinkVersionID",CustDbTableLink.VersionID).
      
      CustDbTableLinkBrowse:endRow().
      
   END. /* FOR EACH CustDbTableLink NO-LOCK */
   
   CustDbTableLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CustDbTableLinkBrowse:getErrors().
   
   /* Create a new frame */
   CustDbTableLinkBrowseFrame           = NEW pageFrame().
   CustDbTableLinkBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CustDbTableLinkBrowseFrame:FormAction="dbCustDbTableLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CustDbTableLinkBrowseFrame:formOpen("custdbtablelink_browse_form").
   
   /* Start the Frame Header */
   CustDbTableLinkBrowseFrame:insertSpacer(5).
   CustDbTableLinkBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CustDbTableLinkBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CustDbTableLinkBrowseFrame:frameClose().
   CustDbTableLinkBrowseFrame:insertSpacer(10).
   
   CustDbTableLinkBrowseFrame:insertHiddenField("custdbtablelink_browse_scroll","").
   CustDbTableLinkBrowseFrame:insertHiddenField("CustDbTableLinkID","").
   CustDbTableLinkBrowseFrame:insertHiddenField("CustDbTableLinkVersionID","").
   CustDbTableLinkBrowseFrame:insertHiddenField("popup_custdbtablelinkhistory_browse", "").
   CustDbTableLinkBrowseFrame:insertHiddenField("form_name","custdbtablelink_browse_form").
   CustDbTableLinkBrowseFrame:insertHiddenField("prog_name","adCustDbTableLinkAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CustDbTableLinkBrowseFrame}
   
   CustDbTableLinkBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   CustDbTableLinkBrowseButtons           = NEW buttonBar().
   CustDbTableLinkBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   CustDbTableLinkBrowseButtons:addButton("custdbtablelink_browse_form_btn_details",
                                          fTL("Details"),
                                          "viewCustDbTableLinkDetails('custdbtablelink_details_form');",
                                          "Disabled").
   
   CustDbTableLinkBrowseButtons:addButton("custdbtablelink_browse_form_btn_create",
                                          fTL("Create"),
                                          "createCustDbTableLink('custdbtablelink_details_form');").
                                      
   CustDbTableLinkBrowseButtons:addButton("custdbtablelink_browse_form_btn_history",
                                          fTL("History"),
                                          "viewHistory();",
                                          "Disabled").                                
   
   CustDbTableLinkBrowseButtons:closeBar().  
   CustDbTableLinkBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCustDbTableLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustDbTableLinkDetails Procedure 
PROCEDURE pCustDbTableLinkDetails:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "custdbtablelink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CustDbTableLinkID,CoreTableName,CustTableName,CreateCustRecords,Created"
          chrEditFieldList     = "CoreTableName,CustTableName,CreateCustRecords"
          chrNewFieldList      = "CoreTableName,CustTableName,CreateCustRecords"
          chrRequiredFieldList = "CoreTableName,CustTableName,CreateCustRecords"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CustDbTableLinkDetailsForm           = NEW dataForm("custdbtablelink_details_form").
   CustDbTableLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CustDbTableLinkDetailsForm:FormAction = "dbCustDbTableLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CustDbTableLinkDetailsForm:FormWidth  = 460.
   CustDbTableLinkDetailsForm:FormHeight = 300.
   CustDbTableLinkDetailsForm:FormTitle  = "CustDbTableLink Details".
   CustDbTableLinkDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CustDbTableLinkDetailsForm:insertPaddingColumn(50).
   CustDbTableLinkDetailsForm:insertColumn(100).
   CustDbTableLinkDetailsForm:insertColumn(120).
   CustDbTableLinkDetailsForm:insertColumn(20).
   CustDbTableLinkDetailsForm:insertColumn(4).
  
   /* Fields */
   CustDbTableLinkDetailsForm:startRow().
   CustDbTableLinkDetailsForm:insertLabel("Table Link ID").
   CustDbTableLinkDetailsForm:insertTextField("CustDbTableLinkID", "", 110, TRUE).  
      
   CustDbTableLinkDetailsForm:startRow().
   CustDbTableLinkDetailsForm:insertLabel(fTL("Core Table Name")).
   CustDbTableLinkDetailsForm:insertTextField("CoreTableName", "", 110, TRUE).  
   
   CustDbTableLinkDetailsForm:startRow().
   CustDbTableLinkDetailsForm:insertLabel(fTL("Cust Table Name")).
   CustDbTableLinkDetailsForm:insertTextField("CustTableName", "", 110, TRUE).
   
   CustDbTableLinkDetailsForm:startRow().
   CustDbTableLinkDetailsForm:insertLabel(fTL("CreateCustRecords")). 
   CustDbTableLinkDetailsForm:insertComboField("CreateCustRecords", "", 110, TRUE).  
   CustDbTableLinkDetailsForm:insertComboPairs("CreateCustRecords", "yes", "Yes").
   CustDbTableLinkDetailsForm:insertComboPairs("CreateCustRecords", "no",  "No").
   
   {webGetOptionalFormFields.i pCustDbTableLinkDetailsFields}
   
   /* Add Hidden Fields*/
   CustDbTableLinkDetailsForm:insertHiddenField("custdbtablelink_browse_scroll", "").
   CustDbTableLinkDetailsForm:insertHiddenField("form_name", "custdbtablelink_details_form").
   CustDbTableLinkDetailsForm:insertHiddenField("prog_name", "adCustDbTableLinkAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CustDbTableLinkDetailsForm}
   
   /* Create Button Bar */
   CustDbTableLinkDetailsButtons = NEW buttonBar().
   
   CustDbTableLinkDetailsButtons:addButton("custdbtablelink_details_form_btn_save", 
                                           fTL("Save"), 
                                           "updateCustDbTableLink('custdbtablelink_details_form');").
   
   CustDbTableLinkDetailsButtons:addButton("custdbtablelink_details_form_btn_cancel", 
                                           fTL("Cancel"), 
                                           "cancelUpdate('UserCancelled','process_mode'); disablePopup('custdbtablelink_details_form_popup');").
   
   CustDbTableLinkDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CustDbTableLinkDetailsForm:FormButtons = CustDbTableLinkDetailsButtons.
   
   CustDbTableLinkDetailsForm:endForm(). 
   
   CustDbTableLinkDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCustDbTableLinkDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustDbTableLinkDetailsFields Procedure 
PROCEDURE pCustDbTableLinkDetailsFields:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         CustDbTableLinkDetailsForm:startRow().
         CustDbTableLinkDetailsForm:insertLabel(fTL("Field Label")).
         CustDbTableLinkDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
/*      {adCustDbTableLinkAdmin_custdbtablelink_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

