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

  Author:  Lily Tran 

  Created: 09/12/2013

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
DEFINE VARIABLE chrConfigID                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupConfigHistory       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToConfigRow        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectConfigRow          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedConfig           AS INTEGER   NO-UNDO.

/* Objects */
DEFINE VARIABLE ConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE ConfigBrowse                AS browseTable.
DEFINE VARIABLE ConfigBrowseButtons         AS buttonBar.
DEFINE VARIABLE ConfigDetailsForm           AS dataForm.
DEFINE VARIABLE ConfigDetailsButtons        AS buttonBar.

/* ConfigHisotry Objects */
DEFINE VARIABLE ConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE ConfigHistoryBrowseButtons  AS buttonBar.
DEFINE VARIABLE ConfigHistoryBrowseForm     AS dataForm.
DEFINE VARIABLE ConfigHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE ConfigHistoryDetailsForm    AS dataForm.
/* Buffers */
DEFINE BUFFER   extApplication              FOR Application.
DEFINE BUFFER   readConfig                  FOR Config.

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

&IF DEFINED(EXCLUDE-pConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConfigBrowse Procedure 
PROCEDURE pConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "config_details_form"}
   
   ConfigBrowse              = NEW browseTable("config_browse").
   ConfigBrowse:BrowseWidth  = 965.
   ConfigBrowse:BrowseHeight = 455.
   ConfigBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   ConfigBrowse:insertColumn(fTL("Config ID"), 70, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Config}
   
   ConfigBrowse:insertColumn("App ID",            60, "INTEGER",   "LEFT").  
   ConfigBrowse:insertColumn(fTL("System Name"), 150, "CHARACTER", "LEFT").
   ConfigBrowse:insertColumn(FTL("Client Name"), 100, "CHARACTER", "LEFT").
   ConfigBrowse:insertColumn("DB Table Area",    100, "CHARACTER", "LEFT").
   ConfigBrowse:insertColumn("DB Index Area",    100, "CHARACTER", "LEFT").
   ConfigBrowse:insertColumn("Char Timeout",      90, "INTEGER",   "LEFT").
   ConfigBrowse:insertColumn("Web Timeout",       90, "INTEGER",   "LEFT").
   ConfigBrowse:insertColumn("Maint",             60, "LOGICAL",   "LEFT").
   ConfigBrowse:insertColumn("Simulate",          80, "CHARACTER", "LEFT").
   
   /*Body*/
   ConfigBrowse:startBody().
     
   /* Find all Configs then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH readConfig NO-LOCK:
      
      FIND FIRST Environment NO-LOCK
         WHERE Environment.EnvironmentID = readConfig.SimulationEnvironmentID NO-ERROR.
      
      ConfigBrowse:startRow(readConfig.ConfigID, "selectConfigRow(this," + '"' + STRING(readConfig.ConfigID) + '"' + ");", "").
      ConfigBrowse:insertData(readConfig.ConfigID).
      
      /* Add in Optional & Customer Specific fields according to the ConfigFields associated with this ProcessEvent */      
      {webGetOptionalBrowseFields.i Config}
      
      ConfigBrowse:insertData(STRING(readConfig.ApplicationID), "right"). 
      ConfigBrowse:insertData(readConfig.SystemName, "left").
      ConfigBrowse:insertData(readConfig.ClientName, "left").
      ConfigBrowse:insertData(readConfig.DBAreaForTable, "left").
      ConfigBrowse:insertData(readConfig.DBAreaForIndex, "left").
      ConfigBrowse:insertData(STRING(readConfig.CharSessionTimeoutMins), "right"). 
      ConfigBrowse:insertData(STRING(readConfig.WebSessionTimeoutMins), "right").
      ConfigBrowse:insertData(STRING(readConfig.Maintenance, "Yes/No"), "left").
      ConfigBrowse:insertData(IF AVAILABLE Environment THEN Environment.EnvironmentCode ELSE "", "left").

      /* Add hidden fields */
      ConfigBrowse:insertHiddenData("ConfigVersionID", readConfig.VersionID).
      
      ConfigBrowse:endRow().
      
   END. /* FOR EACH Config NO-LOCK */
   
   ConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + ConfigBrowse:getErrors().
   
   /* Create a new frame */
   ConfigBrowseFrame           = NEW pageFrame().
   ConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   ConfigBrowseFrame:FormAction="dbConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   ConfigBrowseFrame:formOpen("config_browse_form").
   
   /* Start the Frame Header */
   ConfigBrowseFrame:insertSpacer(5).
   ConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   ConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   ConfigBrowseFrame:frameClose().
   ConfigBrowseFrame:insertSpacer(10).
   
   ConfigBrowseFrame:insertHiddenField("config_browse_scroll", "").
   ConfigBrowseFrame:insertHiddenField("ConfigID", "").
   ConfigBrowseFrame:insertHiddenField("ConfigVersionID", "").
   ConfigBrowseFrame:insertHiddenField("form_name", "config_browse_form").
   ConfigBrowseFrame:insertHiddenField("prog_name", "adConfigAdmin.p").
   ConfigBrowseFrame:insertHiddenField("popup_confighistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ConfigBrowseFrame}
   
   ConfigBrowseFrame:formClose().
   
   /* Create Button Bar */
   ConfigBrowseButtons           = NEW buttonBar().
   ConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.
      
   ConfigBrowseButtons:addButton("config_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewConfigDetails('config_details_form');",
                                 "Disabled").

   ConfigBrowseButtons:addButton("config_browse_form_btn_history",
                                 fTL("History"),
                                 "viewConfigHistoryBrowse();",
                                 "Disabled").
     
   ConfigBrowseButtons:closeBar().  
   ConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConfigDetails Procedure 
PROCEDURE pConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "config_details_form"}

   /* ApplicationID and SystemName have a dependency so should not allow update of SystemName to avoid ApplicationID and 
      SystemName to become out of sync */
          
   ASSIGN chrDisplayFieldList  = "ConfigID,SystemName,PackageName,ClientName,ApplicationID,ExternalApplicationID,CreateTranslations,DbsUsingClasses,"
                                    + "DbAreaForTables,DbAreaForIndexes,ClassLockType,KillUncommittedUpdates,DaysBeforeArchive,"
                                    + "SyngateConnect,CurrentlyArchiving,Maintenance,DefaultWriteOnStockStatusID,"
                                    + "DefaultBusinessUnitID,WebSessionTimeoutMins,CharSessionTimeoutMins,SimulationEnvironmentID,"
                                    + "CurrentPurchaseOrderNo"
          chrEditFieldList     = "PackageName,ClientName,CreateTranslations,ExternalApplicationID,DbsUsingClasses,DbAreaForTables,DbAreaForIndexes,"
                                    + "ClassLockType,KillUncommittedUpdates,DaysBeforeArchive,SyngateConnect,CurrentlyArchiving,Maintenance,"
                                    + "DefaultWriteOnStockStatusID,DefaultBusinessUnitID,WebSessionTimeoutMins,CharSessionTimeoutMins,SimulationEnvironmentID,"
                                    + "CurrentPurchaseOrderNo"
          chrRequiredFieldList = "PackageName,ApplicationID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "DaysBeforeArchive:INTEGER>0,DefaultWriteOnStockStatusID:INTEGER,CharSessionTimeoutMins:INTEGER,"
                                    + "WebSessionTimeoutMins:INTEGER".
   
   ConfigDetailsForm           = NEW dataForm("config_details_form").
   ConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   ConfigDetailsForm:FormAction = "dbConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ConfigDetailsForm:FormWidth  = 860.
   ConfigDetailsForm:FormHeight = 530.
   ConfigDetailsForm:FormTitle  = "Config Details".
   ConfigDetailsForm:FormType   = "xxl_large".
   
   /* Column Layout */
   ConfigDetailsForm:insertPaddingColumn(60).
   ConfigDetailsForm:insertColumn(200).
   ConfigDetailsForm:insertColumn(250).
   ConfigDetailsForm:insertColumn(20).
   ConfigDetailsForm:insertColumn(4).
   ConfigDetailsForm:insertColumn(10).  
   
   /* Fields */
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel(fTL("Config ID")).
   ConfigDetailsForm:insertTextField("ConfigID", "", 50, TRUE).  
   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Application").
   ConfigDetailsForm:insertComboField("ApplicationID", "0", 250, TRUE).

   FOR EACH Application NO-LOCK /* idx=ApplicationID */
      WHERE Application.ACTIVE:

      ConfigDetailsForm:insertComboPairs("ApplicationID", STRING(Application.ApplicationID) , Application.AppName).
   END.
   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("External Application").
   ConfigDetailsForm:insertComboField("ExternalApplicationID", "0", 250, TRUE).
   ConfigDetailsForm:insertComboPairs("ExternalApplicationID","0", "None").
  
   FOR EACH extApplication NO-LOCK /* idx=ApplicationID */
      WHERE extApplication.Active
      AND   extApplication.External:

      ConfigDetailsForm:insertComboPairs("ExternalApplicationID", STRING(extApplication.ApplicationID) , extApplication.AppName).
   END.
   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel(fTL("System Name")).
   ConfigDetailsForm:insertTextField("SystemName", "", 250, TRUE).
   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Package Name").
   ConfigDetailsForm:insertTextField("PackageName", "", 250, TRUE).  
   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel(fTL("Client Name")).
   ConfigDetailsForm:insertTextField("ClientName", "", 250, TRUE).
      
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Create Translations").
   ConfigDetailsForm:insertComboField("CreateTranslations", "", 250, TRUE).
   ConfigDetailsForm:insertComboPairs("CreateTranslations", "yes", "Create Translations").
   ConfigDetailsForm:insertComboPairs("CreateTranslations", "no", "Don't Create Translations").

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Dbs Using Classes").
   ConfigDetailsForm:insertTextField("DbsUsingClasses", "", 250, TRUE).

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Db Area For Tables").
   ConfigDetailsForm:insertTextField("DbAreaForTables", "", 250, TRUE).

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Db Area For Indexes").
   ConfigDetailsForm:insertTextField("DbAreaForIndexes", "", 250, TRUE).

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Class Lock Type").
   ConfigDetailsForm:insertTextField("ClassLockType", "", 250, TRUE).

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Kill Uncommitted Updates").
   ConfigDetailsForm:insertComboField("KillUncommittedUpdates", "", 250, TRUE).
   ConfigDetailsForm:insertComboPairs("KillUncommittedUpdates", "yes", "Kill Uncommitted Updates").
   ConfigDetailsForm:insertComboPairs("KillUncommittedUpdates", "no", "Don't Kill Uncommitted updates").

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Days Before Archive").
   ConfigDetailsForm:insertTextField("DaysBeforeArchive", "", 250, TRUE).

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Syngate Connect").
   ConfigDetailsForm:insertTextField("SyngateConnect", "", 250, TRUE).

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Currently Archiving").
   ConfigDetailsForm:insertComboField("CurrentlyArchiving", "", 250, TRUE).
   ConfigDetailsForm:insertComboPairs("CurrentlyArchiving", "yes", "System Is Archiving Right Now").
   ConfigDetailsForm:insertComboPairs("CurrentlyArchiving", "no", "System Is Not Archiving Right Now").

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel(fTL("Maintenance")).
   ConfigDetailsForm:insertComboField("Maintenance", "", 250, TRUE).
   ConfigDetailsForm:insertComboPairs("Maintenance", "yes", "System Is Under Maintenance").
   ConfigDetailsForm:insertComboPairs("Maintenance", "no", "System Is Not Under Maintenance").

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Simulate Environment").
   ConfigDetailsForm:insertComboField("SimulationEnvironmentID", "0", 250, TRUE).
   ConfigDetailsForm:insertComboPairs("SimulationEnvironmentID", "0", "None").      

   FOR EACH Environment NO-LOCK /* idx=ActiveListingSequence */
      WHERE Environment.Active
      BY Environment.ListingSequence:

      ConfigDetailsForm:insertComboPairs("SimulationEnvironmentID", STRING(Environment.EnvironmentID) , Environment.EnvironmentCode).
   END.

   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Default WriteOn Stock Status ID").
   ConfigDetailsForm:insertTextField("DefaultWriteOnStockStatusID", "", 150, TRUE).
   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Default BusinessUnit ID").
   ConfigDetailsForm:insertTextField("DefaultBusinessUnitID", "", 150, TRUE).
   
   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Char Session Timout Mins").
   ConfigDetailsForm:insertTextField("CharSessionTimeoutMins", "", 150, TRUE).

   ConfigDetailsForm:startRow().
   ConfigDetailsForm:insertLabel("Web Session Timeout Mins").
   ConfigDetailsForm:insertTextField("WebSessionTimeoutMins", "", 150, TRUE).

   {webGetOptionalFormFields.i pConfigDetailsFields}
   
   /* Add Hidden Fields*/
   ConfigDetailsForm:insertHiddenField("config_browse_scroll", "").
   ConfigDetailsForm:insertHiddenField("form_name", "config_details_form").
   ConfigDetailsForm:insertHiddenField("prog_name", "adConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ConfigDetailsForm}
   
   /* Create Button Bar */
   ConfigDetailsButtons = NEW buttonBar().
   
   ConfigDetailsButtons:addButton("config_details_form_btn_save", 
                                  fTL("Save"), 
                                  "updateConfig('config_details_form');").
   
   ConfigDetailsButtons:addButton("config_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('config_details_form_popup');").
   
   ConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ConfigDetailsForm:FormButtons = ConfigDetailsButtons.
   
   ConfigDetailsForm:endForm(). 
   
   ConfigDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConfigDetailsFields Procedure 
PROCEDURE pConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         ConfigDetailsForm:startRow().
         ConfigDetailsForm:insertLabel(fTL("Field Label")).
         ConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      WHEN "CurrentPurchaseOrderNo" THEN
      DO:
         ConfigDetailsForm:startRow().
         ConfigDetailsForm:insertLabel(fTL("Current Purchase Order #")).
         ConfigDetailsForm:insertTextField("CurrentPurchaseOrderNo", "", 250, TRUE).  
      END. /*WHEN "CurrentPurchaseOrderNo" THEN*/
      
      /* This will be held in customer specific code repository */
      {adConfigAdmin_config_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConfigHistoryBrowse Procedure 
PROCEDURE pConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ConfigHistoryBrowseForm = NEW dataForm("confighistory_browse_form").
   ConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   ConfigHistoryBrowseForm:FormWidth  = 860.
   ConfigHistoryBrowseForm:FormHeight = 530.
   ConfigHistoryBrowseForm:FormTitle  = fTL("Config History") + (IF AVAILABLE Config THEN " : " + STRING(Config.ConfigID)
                                           + " ApplicationID: " + String(Config.ApplicationID) + " SystemName: " + Config.SystemName ELSE "").
   ConfigHistoryBrowseForm:FormType   = "xxl_large".
   
   ConfigHistoryBrowse = NEW browseTable("confighistory_browse").
   ConfigHistoryBrowse:BrowseWidth  = 840.
   ConfigHistoryBrowse:BrowseHeight = 490.
   
   ConfigHistoryBrowse:insertColumn(fTL("History ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ConfigHistory}

   ConfigHistoryBrowse:insertColumn(fTL("Package"),   65, "CHARACTER", "LEFT").
   ConfigHistoryBrowse:insertColumn("Syngate",        55, "CHARACTER", "LEFT").
   ConfigHistoryBrowse:insertColumn("StatusID",       65, "CHARACTER", "LEFT").
   ConfigHistoryBrowse:insertColumn("Char",           40, "INTEGER", "LEFT").
   ConfigHistoryBrowse:insertColumn("Web",            40, "INTEGER", "LEFT").
   ConfigHistoryBrowse:insertColumn("Days",           50, "INTEGER", "LEFT").
   ConfigHistoryBrowse:insertColumn("Archive",        55, "LOGICAL", "LEFT").
   ConfigHistoryBrowse:insertColumn("Maint.",         50, "LOGICAL", "LEFT").
   ConfigHistoryBrowse:insertColumn("ExtApp",         50, "CHARACTER","LEFT").
   ConfigHistoryBrowse:insertColumn("Translation",    70, "LOGICAL", "LEFT").
   ConfigHistoryBrowse:insertColumn("User",           70, "CHARACTER", "LEFT").
   ConfigHistoryBrowse:insertColumn("Created",        70, "CHARACTER", "LEFT").
   ConfigHistoryBrowse:insertColumn("Simulate",       70, "CHARACTER", "LEFT").

   ConfigHistoryBrowse:StartBody().
   
   IF AVAILABLE Config THEN
   DO:
      /* List the ConfigHistorys for the Config */
      FOR EACH ConfigHistory NO-LOCK /* idx= ConfigID */
         WHERE ConfigHistory.ConfigID = Config.ConfigID 
         BY ConfigHistory.ConfigID
         BY ConfigHistory.Created DESCENDING:

         FIND FIRST GateUser OF ConfigHistory NO-LOCK NO-ERROR.

         FIND FIRST Environment NO-LOCK
            WHERE Environment.EnvironmentID = ConfigHistory.SimulationEnvironmentID NO-ERROR.

         FIND FIRST Application NO-LOCK
            WHERE Application.ApplicationID = ConfigHistory.ExternalApplicationID NO-ERROR.
         
         ConfigHistoryBrowse:startRow(ConfigHistory.ConfigHistoryID, "selectConfigHistoryRow(this," + '"' + STRING(ConfigHistory.ConfigHistoryID)
                                         + '","adConfigAdmin.p","confighistory_browse_form"' + ");", "").

         ConfigHistoryBrowse:insertData(ConfigHistory.ConfigHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i ConfigHistory}

         ConfigHistoryBrowse:insertData(ConfigHistory.PackageName, "left").
         ConfigHistoryBrowse:insertData(ConfigHistory.SyngateConnect, "left").
         ConfigHistoryBrowse:insertData(ConfigHistory.DefaultWriteOnStockStatusID, "Right").
         ConfigHistoryBrowse:insertData(STRING(ConfigHistory.CharSessionTimeoutMins), "right"). 
         ConfigHistoryBrowse:insertData(STRING(ConfigHistory.WebSessionTimeoutMins), "right").
         ConfigHistoryBrowse:insertData(STRING(ConfigHistory.DaysBeforeArchive), "right").
         ConfigHistoryBrowse:insertData(STRING(ConfigHistory.CurrentlyArchiving, "Yes/No"), "left").
         ConfigHistoryBrowse:insertData(STRING(ConfigHistory.Maintenance, "Yes/No"), "left").
         ConfigHistoryBrowse:insertData(IF AVAILABLE Application THEN Application.AppName ELSE "").
         ConfigHistoryBrowse:insertData(STRING(ConfigHistory.CreateTranslations, "Yes/No"), "left").
         ConfigHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "").
         ConfigHistoryBrowse:insertData(fDisplayDate&Time(ConfigHistory.Created,"y/m/d H:M:S"), "right").
         ConfigHistoryBrowse:insertData(IF AVAILABLE Environment THEN Environment.EnvironmentCode ELSE "").
                  
         ConfigHistoryBrowse:endRow().
      
      END. /* FOR EACH ConfigHistory OF Config NO-LOCK */
   END. /*IF AVAILABLE Config THEN*/
   
   ConfigHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + ConfigHistoryBrowse:getErrors().
   
   ConfigHistoryBrowseForm:insertHiddenField("ConfigHistoryID", "").
   ConfigHistoryBrowseForm:insertHiddenField("popup_confighistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   ConfigHistoryBrowseButtons = NEW buttonBar().

   ConfigHistoryBrowseButtons:addButton("confighistory_browse_form_btn_cancel",
                                        fTL("Details"),
                                        "viewConfigHistoryDetails('confighistory_details_form');").
          
   ConfigHistoryBrowseButtons:addButton("confighistory_browse_form_btn_cancel",
                                        fTL("Cancel"),
                                        "disablePopup('confighistory_browse_form_popup');").
   
   ConfigHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ConfigHistoryBrowseForm:FormBrowse  = ConfigHistoryBrowse.
   ConfigHistoryBrowseForm:FormButtons = ConfigHistoryBrowseButtons.
   ConfigHistoryBrowseForm:endForm(). 
   
   ConfigHistoryBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pConfigHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConfigHistoryDetails Procedure
PROCEDURE pConfigHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "confighistory_details_form"}

   /* ApplicationID and SystemName have a dependency so should not allow update of SystemName to avoid ApplicationID and 
      SystemName to become out of sync */
          
   ASSIGN chrDisplayFieldList  = "ConfigHistoryID,ConfigID,SystemName,PackageName,ClientName,ApplicationID,ExternalApplicationID,CreateTranslations,DbsUsingClasses,"
                                    + "DbAreaForTables,DbAreaForIndexes,ClassLockType,KillUncommittedUpdates,DaysBeforeArchive,"
                                    + "SyngateConnect,CurrentlyArchiving,Maintenance,DefaultWriteOnStockStatusID,"
                                    + "DefaultBusinessUnitID,WebSessionTimeoutMins,CharSessionTimeoutMins,SimulationEnvironmentID"
                                    + ",CurrentPurchaseOrderNo"
          chrEditFieldList     = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
                                    
   
   ConfigHistoryDetailsForm           = NEW dataForm("confighistory_details_form").
   ConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   
   /* Setup */
   ConfigHistoryDetailsForm:FormWidth  = 860.
   ConfigHistoryDetailsForm:FormHeight = 530.
   ConfigHistoryDetailsForm:FormTitle  = "Config Details".
   ConfigHistoryDetailsForm:FormType   = "xxl_large".
   
   /* Column Layout */
   ConfigHistoryDetailsForm:insertPaddingColumn(60).
   ConfigHistoryDetailsForm:insertColumn(200).
   ConfigHistoryDetailsForm:insertColumn(250).
   ConfigHistoryDetailsForm:insertColumn(20).
   ConfigHistoryDetailsForm:insertColumn(4).
   ConfigHistoryDetailsForm:insertColumn(10).  
   
   /* Fields */  
      
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel(fTL("Config ID")).
   ConfigHistoryDetailsForm:insertTextField("ConfigID", "", 50, TRUE).  
   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Application").
   ConfigHistoryDetailsForm:insertComboField("ApplicationID", "", 250, TRUE).

   FOR EACH Application NO-LOCK /* idx=ApplicationID */
      WHERE Application.ACTIVE:

      ConfigHistoryDetailsForm:insertComboPairs("ApplicationID", STRING(Application.ApplicationID) , Application.AppName).
   END.
   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("External Application").
   ConfigHistoryDetailsForm:insertComboField("ExternalApplicationID", "", 250, TRUE).
   ConfigHistoryDetailsForm:insertComboPairs("ExternalApplicationID","0", "None").
  
   FOR EACH extApplication NO-LOCK /* idx=ApplicationID */
      WHERE extApplication.Active
      AND   extApplication.External:

      ConfigHistoryDetailsForm:insertComboPairs("ExternalApplicationID", STRING(extApplication.ApplicationID) , extApplication.AppName).
   END.
   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel(fTL("System Name")).
   ConfigHistoryDetailsForm:insertTextField("SystemName", "", 250, TRUE).
   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Package Name").
   ConfigHistoryDetailsForm:insertTextField("PackageName", "", 250, TRUE).  
   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel(fTL("Client Name")).
   ConfigHistoryDetailsForm:insertTextField("ClientName", "", 250, TRUE).
      
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Create Translations").
   ConfigHistoryDetailsForm:insertComboField("CreateTranslations", "", 250, TRUE).
   ConfigHistoryDetailsForm:insertComboPairs("CreateTranslations", "yes", "Create Translations").
   ConfigHistoryDetailsForm:insertComboPairs("CreateTranslations", "no", "Don't Create Translations").

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Dbs Using Classes").
   ConfigHistoryDetailsForm:insertTextField("DbsUsingClasses", "", 250, TRUE).

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Db Area For Tables").
   ConfigHistoryDetailsForm:insertTextField("DbAreaForTables", "", 250, TRUE).

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Db Area For Indexes").
   ConfigHistoryDetailsForm:insertTextField("DbAreaForIndexes", "", 250, TRUE).

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Class Lock Type").
   ConfigHistoryDetailsForm:insertTextField("ClassLockType", "", 250, TRUE).

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Kill Uncommitted Updates").
   ConfigHistoryDetailsForm:insertComboField("KillUncommittedUpdates", "", 250, TRUE).
   ConfigHistoryDetailsForm:insertComboPairs("KillUncommittedUpdates", "yes", "Kill Uncommitted Updates").
   ConfigHistoryDetailsForm:insertComboPairs("KillUncommittedUpdates", "no", "Don't Kill Uncommitted updates").

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Days Before Archive").
   ConfigHistoryDetailsForm:insertTextField("DaysBeforeArchive", "", 250, TRUE).

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Syngate Connect").
   ConfigHistoryDetailsForm:insertTextField("SyngateConnect", "", 250, TRUE).

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Currently Archiving").
   ConfigHistoryDetailsForm:insertComboField("CurrentlyArchiving", "", 250, TRUE).
   ConfigHistoryDetailsForm:insertComboPairs("CurrentlyArchiving", "yes", "System Is Archiving Right Now").
   ConfigHistoryDetailsForm:insertComboPairs("CurrentlyArchiving", "no", "System Is Not Archiving Right Now").

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel(fTL("Maintenance")).
   ConfigHistoryDetailsForm:insertComboField("Maintenance", "", 250, TRUE).
   ConfigHistoryDetailsForm:insertComboPairs("Maintenance", "yes", "System Is Under Maintenance").
   ConfigHistoryDetailsForm:insertComboPairs("Maintenance", "no", "System Is Not Under Maintenance").

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Simulate Environment").
   ConfigHistoryDetailsForm:insertComboField("SimulationEnvironmentID", "", 250, TRUE).
   ConfigHistoryDetailsForm:insertComboPairs("SimulationEnvironmentID", "0", "None").      

   FOR EACH Environment NO-LOCK /* idx=ActiveListingSequence */
      WHERE Environment.Active
      BY Environment.ListingSequence:

      ConfigHistoryDetailsForm:insertComboPairs("SimulationEnvironmentID", STRING(Environment.EnvironmentID) , Environment.EnvironmentCode).
   END.

   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Default WriteOn Stock Status ID").
   ConfigHistoryDetailsForm:insertTextField("DefaultWriteOnStockStatusID", "", 150, TRUE).
   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Default BusinessUnit ID").
   ConfigHistoryDetailsForm:insertTextField("DefaultBusinessUnitID", "", 150, TRUE).
   
   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Char Session Timout Mins").
   ConfigHistoryDetailsForm:insertTextField("CharSessionTimeoutMins", "", 150, TRUE).

   ConfigHistoryDetailsForm:startRow().
   ConfigHistoryDetailsForm:insertLabel("Web Session Timeout Mins").
   ConfigHistoryDetailsForm:insertTextField("WebSessionTimeoutMins", "", 150, TRUE).
 
   {webGetOptionalFormFields.i pConfigHistoryDetailsFields}
   
   /* Add Hidden Fields*/
   ConfigHistoryDetailsForm:insertHiddenField("config_browse_scroll", "").
   ConfigHistoryDetailsForm:insertHiddenField("form_name", "confighistory_details_form").
   ConfigHistoryDetailsForm:insertHiddenField("form_name", "config_details_form").
   ConfigHistoryDetailsForm:insertHiddenField("prog_name", "adConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   ConfigHistoryDetailsButtons = NEW buttonBar().
   
   ConfigHistoryDetailsButtons:addButton("confighistory_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('confighistory_details_form_popup');").
   
   ConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ConfigHistoryDetailsForm:FormButtons = ConfigHistoryDetailsButtons.
   
   ConfigHistoryDetailsForm:endForm(). 
   
   ConfigHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pConfigHistoryDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pConfigHistoryDetailsFields Procedure
PROCEDURE pConfigHistoryDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
       
       CASE chrOption:
         
          
          WHEN "CurrentPurchaseOrderNo" THEN
          DO:
             ConfigHistoryDetailsForm:startRow().
             ConfigHistoryDetailsForm:insertLabel(fTL("Current Purchase Order #")).
             ConfigHistoryDetailsForm:insertTextField("CurrentPurchaseOrderNo", "", 250, TRUE).  
          END. /*WHEN "CurrentPurchaseOrderNo" THEN*/
          
          /* This will be held in customer specific code repository */
          {adConfigAdmin_config_details_form.i}
          
       END CASE. /*chrOption:*/

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
   
   ASSIGN chrConfigID          = get-value("ConfigID")
          intSelectedConfig    = INTEGER(chrConfigID)
          chrScrollToConfigRow = STRING(INTEGER(get-value("config_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrConfigID <> "" THEN
      chrSelectConfigRow = 'selectConfigRow(document.getElementById("config_browse_row_' + chrConfigID + '"),"' + chrConfigID +  '");'.

   IF get-value('popup_confighistory_browse') = "yes" THEN
      chrPopupConfigHistory = 'enablePopup("confighistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("config_browse").scrollTop=' + chrScrollToConfigRow + chrSelectConfigRow
                             + chrPopupConfigHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Config Admin".
   ThisPage:FrameTitle    = "Config Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("config.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pConfigBrowse.

   IF intSelectedConfig <> 0 THEN
      FIND FIRST Config NO-LOCK
         WHERE Config.ConfigID = intSelectedConfig NO-ERROR.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pConfigDetails.
   RUN pConfigHistoryBrowse.
   RUN pConfigHistoryDetails.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT ConfigBrowseFrame           NO-ERROR.
   DELETE OBJECT ConfigBrowse                NO-ERROR.
   DELETE OBJECT ConfigBrowseButtons         NO-ERROR.
   DELETE OBJECT ConfigDetailsForm           NO-ERROR.
   DELETE OBJECT ConfigDetailsButtons        NO-ERROR.
   DELETE OBJECT ConfigHistoryBrowse         NO-ERROR.
   DELETE OBJECT ConfigHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT ConfigHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT ConfigHistoryBrowseButtons  NO-ERROR.
   DELETE OBJECT ConfigHistoryBrowseForm     NO-ERROR.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

