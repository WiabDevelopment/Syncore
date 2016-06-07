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

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE chrPopupReplenishConfigHistory      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrReplenishConfigID                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToReplenishConfigRow       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectReplenishConfigRow         AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedReplenishConfig          AS INTEGER   NO-UNDO.
DEFINE VARIABLE logRecordAvailable                  AS LOGICAL   NO-UNDO.

/* Objects */
DEFINE VARIABLE ReplenishConfigBrowseFrame          AS pageFrame.
DEFINE VARIABLE ReplenishConfigBrowse               AS browseTable.
DEFINE VARIABLE ReplenishConfigBrowseButtons        AS buttonBar.
DEFINE VARIABLE ReplenishConfigDetailsForm          AS dataForm.
DEFINE VARIABLE ReplenishConfigDetailsButtons       AS buttonBar.

/* Objects for History Popup */
DEFINE VARIABLE ReplenishConfigHistoryBrowse        AS browseTable.
DEFINE VARIABLE ReplenishConfigHistoryBrowseButtons AS buttonBar.
DEFINE VARIABLE ReplenishConfigHistoryBrowseForm    AS dataForm.

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
         WIDTH              = 49.4.
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

&IF DEFINED(EXCLUDE-pReplenishConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenishConfigBrowse Procedure 
PROCEDURE pReplenishConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* If a record is available then disable the create */
   IF CAN-FIND(FIRST ReplenishConfig) THEN
      logRecordAvailable = YES.

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "replenishconfig_details_form"}
   
   ReplenishConfigBrowse              = NEW browseTable("replenishconfig_browse").
   ReplenishConfigBrowse:BrowseWidth  = 965.
   ReplenishConfigBrowse:BrowseHeight = 455.
   ReplenishConfigBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   ReplenishConfigBrowse:insertColumn(fTL("Config ID"), 70, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ReplenishConfig}
   
   ReplenishConfigBrowse:insertColumn(fTL("Num Parts To Auto Replenish"), 150, "INTEGER", FALSE).
   ReplenishConfigBrowse:insertColumn(fTL("Max Locations/Part"),          150, "INTEGER", FALSE).  
   ReplenishConfigBrowse:insertColumn(fTL("Num Days For Ranking"),        150, "INTEGER", FALSE).
   ReplenishConfigBrowse:insertColumn(fTL("Num Days To Replen"),          150, "INTEGER", FALSE).
   ReplenishConfigBrowse:insertColumn(fTL("Free Space Buffer %"),         150, "DECIMAL", FALSE).
   ReplenishConfigBrowse:insertColumn(fTL("Priority"),                     70, "INTEGER", FALSE).
   
   /*Body*/
   ReplenishConfigBrowse:startBody().
      
   /* Find all ReplenishConfigs then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH ReplenishConfig NO-LOCK:
      
      ReplenishConfigBrowse:startRow(ReplenishConfig.ReplenishConfigID, 
                                        "selectReplenishConfigRow(this," + '"' + STRING(ReplenishConfig.ReplenishConfigID) + '"' + ");", "").
      ReplenishConfigBrowse:insertData(ReplenishConfig.ReplenishConfigID).
      
      /* Add in Optional & Customer Specific fields according to the ReplenishConfigFields associated with this ProcessEvent */

      {webGetOptionalBrowseFields.i ReplenishConfig}

      ReplenishConfigBrowse:insertData(STRING(ReplenishConfig.NumPartsToAutoReplenish), "right").
      ReplenishConfigBrowse:insertData(STRING(ReplenishConfig.MaxNumLocationsPerPart), "right"). 
      ReplenishConfigBrowse:insertData(STRING(ReplenishConfig.NumDaysForRankingCalculations), "right").
      ReplenishConfigBrowse:insertData(STRING(ReplenishConfig.NumDaysToReplenFor), "right").
      ReplenishConfigBrowse:insertData(STRING(ReplenishConfig.DefaultFreeSpaceBufferPercentage,">>9.99"), "right"). 
      ReplenishConfigBrowse:insertData(STRING(ReplenishConfig.DefaultReplenPriority), "right").

      /* Add hidden fields */
      ReplenishConfigBrowse:insertHiddenData("ReplenishConfigVersionID", ReplenishConfig.VersionID).
      
      ReplenishConfigBrowse:endRow().
      
   END. /* FOR EACH ReplenishConfig NO-LOCK */
   
   ReplenishConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + ReplenishConfigBrowse:getErrors().
   
   /* Create a new frame */
   ReplenishConfigBrowseFrame           = NEW pageFrame().
   ReplenishConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   ReplenishConfigBrowseFrame:FormAction="dbReplenishConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   ReplenishConfigBrowseFrame:formOpen("replenishconfig_browse_form").
   
   /* Start the Frame Header */
   ReplenishConfigBrowseFrame:insertSpacer(5).
   ReplenishConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   ReplenishConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   ReplenishConfigBrowseFrame:frameClose().
   ReplenishConfigBrowseFrame:insertSpacer(10).
   
   ReplenishConfigBrowseFrame:insertHiddenField("replenishconfig_browse_scroll", "").
   ReplenishConfigBrowseFrame:insertHiddenField("ReplenishConfigID", "").
   ReplenishConfigBrowseFrame:insertHiddenField("ReplenishConfigVersionID", "").
   ReplenishConfigBrowseFrame:insertHiddenField("form_name", "replenishconfig_browse_form").
   ReplenishConfigBrowseFrame:insertHiddenField("prog_name", "adReplenishConfigAdmin.p").
   ReplenishConfigBrowseFrame:insertHiddenField("popup_replenishconfighistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ReplenishConfigBrowseFrame}
   
   ReplenishConfigBrowseFrame:formClose().
   
   /* Create Button Bar */
   ReplenishConfigBrowseButtons           = NEW buttonBar().
   ReplenishConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   ReplenishConfigBrowseButtons:addButton("replenishconfig_browse_form_btn_details",
                                          fTL("Details"),
                                          "viewReplenishConfigDetails('replenishconfig_details_form');",
                                          "Disabled").

   ReplenishConfigBrowseButtons:addButton("replenishconfig_browse_form_btn_history",
                                          fTL("History"),
                                          "viewReplenishConfigHistoryBrowse();",
                                          "Disabled").
   
   ReplenishConfigBrowseButtons:addButton("replenishconfig_browse_form_btn_create",
                                          fTL("Create"),
                                          "createReplenishConfig('replenishconfig_details_form');",
                                          IF logRecordAvailable THEN "Disabled" ELSE "" ).
   
   ReplenishConfigBrowseButtons:closeBar().  
   ReplenishConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReplenishConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenishConfigDetails Procedure 
PROCEDURE pReplenishConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "replenishconfig_details_form"}
   
   ASSIGN chrDisplayFieldList  = "ReplenishConfigID,NumPartsToAutoReplenish,MaxNumLocationsPerPart,NumDaysForRankingCalculations,"
                                    + "NumDaysToReplenFor,DefaultFreeSpaceBufferPercentage,DefaultReplenPriority"
          chrEditFieldList     = "NumPartsToAutoReplenish,MaxNumLocationsPerPart,NumDaysForRankingCalculations,NumDaysToReplenFor,"
                                    + "DefaultFreeSpaceBufferPercentage,DefaultReplenPriority"
          chrNewFieldList      = "NumPartsToAutoReplenish,MaxNumLocationsPerPart,NumDaysForRankingCalculations,NumDaysToReplenFor,"
                                    + "DefaultFreeSpaceBufferPercentage,DefaultReplenPriority"
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "NumPartsToAutoReplenish:INTEGER,MaxNumLocationsPerPart:INTEGER,NumDaysForRankingCalculations:INTEGER,"
                                    + "NumDaysToReplenFor:INTEGER,DefaultFreeSpaceBufferPercentage:DECIMAL,DefaultReplenPriority:INTEGER".
   
   ReplenishConfigDetailsForm           = NEW dataForm("replenishconfig_details_form").
   ReplenishConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   ReplenishConfigDetailsForm:FormAction = "dbReplenishConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ReplenishConfigDetailsForm:FormWidth  = 460.
   ReplenishConfigDetailsForm:FormHeight = 300.
   ReplenishConfigDetailsForm:FormTitle  = "ReplenishConfig Details".
   ReplenishConfigDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   ReplenishConfigDetailsForm:insertPaddingColumn(70).
   ReplenishConfigDetailsForm:insertColumn(220).
   ReplenishConfigDetailsForm:insertColumn(70).
   ReplenishConfigDetailsForm:insertColumn(120).
   ReplenishConfigDetailsForm:insertColumn(80).
   ReplenishConfigDetailsForm:insertColumn(70).  
   
   /* Fields */
   ReplenishConfigDetailsForm:startRow().
   ReplenishConfigDetailsForm:insertLabel(fTL("Config ID")).
   ReplenishConfigDetailsForm:insertTextField("ReplenishConfigID", "", 50, TRUE).  
   
   ReplenishConfigDetailsForm:startRow().
   ReplenishConfigDetailsForm:insertLabel(fTL("Num Parts To Auto Replenish")).
   ReplenishConfigDetailsForm:insertTextField("NumPartsToAutoReplenish", "", 50, TRUE).  
   
   ReplenishConfigDetailsForm:startRow().
   ReplenishConfigDetailsForm:insertLabel(fTL("Max Num Locations Per Part")).
   ReplenishConfigDetailsForm:insertTextField("MaxNumLocationsPerPart", "", 50, TRUE).  
   
   ReplenishConfigDetailsForm:startRow().
   ReplenishConfigDetailsForm:insertLabel(fTL("Num Days For Ranking Calculations")).
   ReplenishConfigDetailsForm:insertTextField("NumDaysForRankingCalculations", "", 50, TRUE).
   
   ReplenishConfigDetailsForm:startRow().
   ReplenishConfigDetailsForm:insertLabel(fTL("Num Days To Replen For")).
   ReplenishConfigDetailsForm:insertTextField("NumDaysToReplenFor", "", 50, TRUE).

   ReplenishConfigDetailsForm:startRow().
   ReplenishConfigDetailsForm:insertLabel(fTL("Free Space Buffer Percentage")).
   ReplenishConfigDetailsForm:insertTextField("DefaultFreeSpaceBufferPercentage", "", 50, TRUE).

   ReplenishConfigDetailsForm:startRow().
   ReplenishConfigDetailsForm:insertLabel(fTL("Replen Priority")).
   ReplenishConfigDetailsForm:insertTextField("DefaultReplenPriority", "", 50, TRUE).
   
   {webGetOptionalFormFields.i pReplenishConfigDetailsFields}
   
   /* Add Hidden Fields*/
   ReplenishConfigDetailsForm:insertHiddenField("replenishconfig_browse_scroll", "").
   ReplenishConfigDetailsForm:insertHiddenField("form_name", "replenishconfig_details_form").
   ReplenishConfigDetailsForm:insertHiddenField("prog_name", "adReplenishConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ReplenishConfigDetailsForm}
   
   /* Create Button Bar */
   ReplenishConfigDetailsButtons = NEW buttonBar().
   
   ReplenishConfigDetailsButtons:addButton("replenishconfig_details_form_btn_save", 
                                           fTL("Save"), 
                                           "updateReplenishConfig('replenishconfig_details_form');").
   
   ReplenishConfigDetailsButtons:addButton("replenishconfig_details_form_btn_cancel", 
                                           fTL("Cancel"), 
                                           "cancelUpdate('UserCancelled','process_mode'); disablePopup('replenishconfig_details_form_popup');").
   
   ReplenishConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ReplenishConfigDetailsForm:FormButtons = ReplenishConfigDetailsButtons.
   
   ReplenishConfigDetailsForm:endForm(). 
   
   ReplenishConfigDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReplenishConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenishConfigDetailsFields Procedure 
PROCEDURE pReplenishConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         ReplenishConfigDetailsForm:startRow().
         ReplenishConfigDetailsForm:insertLabel(fTL("Field Label")).
         ReplenishConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adReplenishConfigAdmin_replenishconfig_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pReplenishConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReplenishConfigHistoryBrowse Procedure 
PROCEDURE pReplenishConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ReplenishConfigHistoryBrowseForm = NEW dataForm("replenishconfighistory_browse_form").
   ReplenishConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   ReplenishConfigHistoryBrowseForm:FormWidth  = 860.
   ReplenishConfigHistoryBrowseForm:FormHeight = 530.
   ReplenishConfigHistoryBrowseForm:FormTitle  = fTL("Replenish Config History") + (IF AVAILABLE ReplenishConfig THEN " : " 
                                                                                      + STRING(ReplenishConfig.ReplenishConfigID) ELSE "").
   ReplenishConfigHistoryBrowseForm:FormType   = "xxl_large".
   
   ReplenishConfigHistoryBrowse = NEW browseTable("replenishconfighistory_browse").
   ReplenishConfigHistoryBrowse:BrowseWidth  = 840.
   ReplenishConfigHistoryBrowse:BrowseHeight = 490.
   
   ReplenishConfigHistoryBrowse:insertColumn(fTL("History ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ReplenishConfigHistory}

   ReplenishConfigHistoryBrowse:insertColumn(fTL("# Parts Auto Replen"), 115, "INTEGER", FALSE).
   ReplenishConfigHistoryBrowse:insertColumn(fTL("Locations/Part"),      100, "INTEGER", FALSE).  
   ReplenishConfigHistoryBrowse:insertColumn(fTL("# Days-Ranking"),      100, "INTEGER", FALSE).
   ReplenishConfigHistoryBrowse:insertColumn(fTL("# Days-Replen"),        90, "INTEGER", FALSE).
   ReplenishConfigHistoryBrowse:insertColumn(fTL("FreeSpace Buffer %"),  115, "DECIMAL", FALSE).
   ReplenishConfigHistoryBrowse:insertColumn(fTL("Priority"),             60, "INTEGER", FALSE).
   ReplenishConfigHistoryBrowse:insertColumn(fTL("User"),                 60, "CHARACTER", FALSE).
   ReplenishConfigHistoryBrowse:insertColumn(fTL("Created"),              90, "CHARACTER", FALSE).
   
   ReplenishConfigHistoryBrowse:StartBody().
   
   IF AVAILABLE ReplenishConfig THEN
   DO:
      /* List the ReplenishConfigHistorys for the ReplenishConfig */
      FOR EACH ReplenishConfigHistory NO-LOCK
         WHERE ReplenishConfigHistory.ReplenishConfigID = ReplenishConfig.ReplenishConfigID 
         BY ReplenishConfigHistory.ReplenishConfigID
         BY ReplenishConfigHistory.Created DESCENDING:
         
         FIND FIRST GateUser NO-LOCK 
            WHERE GateUser.GateUserID = ReplenishConfigHistory.GateUserID NO-ERROR.

         ReplenishConfigHistoryBrowse:startRow(ReplenishConfigHistory.ReplenishConfigHistoryID, "selectReplenishConfigHistoryRow(this," 
                                            + '"' + STRING(ReplenishConfigHistory.ReplenishConfigHistoryID)
                                            + '","adReplenishConfigAdmin.p","replenishconfig_browse_form"' + ");", "").

         ReplenishConfigHistoryBrowse:insertData(ReplenishConfigHistory.ReplenishConfigHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i ReplenishConfigHistory}

         ReplenishConfigHistoryBrowse:insertData(STRING(ReplenishConfigHistory.NumPartsToAutoReplenish), "right").
         ReplenishConfigHistoryBrowse:insertData(STRING(ReplenishConfigHistory.MaxNumLocationsPerPart), "right"). 
         ReplenishConfigHistoryBrowse:insertData(STRING(ReplenishConfigHistory.NumDaysForRankingCalculations), "right").
         ReplenishConfigHistoryBrowse:insertData(STRING(ReplenishConfigHistory.NumDaysToReplenFor), "right").
         ReplenishConfigHistoryBrowse:insertData(STRING(ReplenishConfigHistory.DefaultFreeSpaceBufferPercentage,">>9.99"), "right"). 
         ReplenishConfigHistoryBrowse:insertData(STRING(ReplenishConfigHistory.DefaultReplenPriority), "right").
         ReplenishConfigHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "").
         ReplenishConfigHistoryBrowse:insertData(fDisplayDate&Time(ReplenishConfigHistory.Created,"y/m/d H:M:S"), "right").                  

         ReplenishConfigHistoryBrowse:endRow().
      
      END. /* FOR EACH ReplenishConfigHistory OF ReplenishConfig NO-LOCK */
   END. /*IF AVAILABLE ReplenishConfig THEN*/
   
   ReplenishConfigHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + ReplenishConfigHistoryBrowse:getErrors().

   ReplenishConfigHistoryBrowseForm:insertHiddenField("popup_replenishconfighistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ReplenishConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   ReplenishConfigHistoryBrowseButtons = NEW buttonBar().
          
   ReplenishConfigHistoryBrowseButtons:addButton("replenishconfighistory_browse_form_btn_cancel",
                                                 fTL("Cancel"),
                                                 "disablePopup('replenishconfighistory_browse_form_popup');").
   
   ReplenishConfigHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ReplenishConfigHistoryBrowseForm:FormBrowse  = ReplenishConfigHistoryBrowse.
   ReplenishConfigHistoryBrowseForm:FormButtons = ReplenishConfigHistoryBrowseButtons.
   ReplenishConfigHistoryBrowseForm:endForm(). 
   
   ReplenishConfigHistoryBrowseForm:displayForm().
   
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
   
   ASSIGN chrReplenishConfigID          = get-value("ReplenishConfigID")
          intSelectedReplenishConfig    = INTEGER(chrReplenishConfigID)
          chrScrollToReplenishConfigRow = STRING(INTEGER(get-value("replenishconfig_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrReplenishConfigID <> "" THEN
      chrSelectReplenishConfigRow = 'selectReplenishConfigRow(document.getElementById("replenishconfig_browse_row_' + chrReplenishConfigID + '"),"' 
                                                         + chrReplenishConfigID +  '");'.

   IF get-value('popup_replenishconfighistory_browse') = "yes" THEN
      chrPopupReplenishConfigHistory = 'enablePopup("replenishconfighistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("replenishconfig_browse").scrollTop=' + chrScrollToReplenishConfigRow 
                             + chrSelectReplenishConfigRow + chrPopupReplenishConfigHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "ReplenishConfig Admin".
   ThisPage:FrameTitle    = "ReplenishConfig Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("replenishconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pReplenishConfigBrowse.
   
   IF intSelectedReplenishConfig <> 0 THEN
      FIND FIRST ReplenishConfig NO-LOCK 
         WHERE ReplenishConfig.ReplenishConfigID = intSelectedReplenishConfig NO-ERROR.

   /******* Pop-up Browsers and Forms ********/    
   RUN pReplenishConfigDetails.
   RUN pReplenishConfigHistoryBrowse.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT ReplenishConfigBrowseFrame          NO-ERROR.
   DELETE OBJECT ReplenishConfigBrowse               NO-ERROR.
   DELETE OBJECT ReplenishConfigBrowseButtons        NO-ERROR.
   DELETE OBJECT ReplenishConfigDetailsForm          NO-ERROR.
   DELETE OBJECT ReplenishConfigDetailsButtons       NO-ERROR.
   DELETE OBJECT ReplenishConfigHistoryBrowse        NO-ERROR.
   DELETE OBJECT ReplenishConfigHistoryBrowseButtons NO-ERROR.
   DELETE OBJECT ReplenishConfigHistoryBrowseForm    NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

