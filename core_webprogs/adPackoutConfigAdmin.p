&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adPackoutConfigAdmin.p

  Description: 

  Author: Ashwin Baliga

  Created: 06/24/2014
  
  Revisions:
     18/03/2015 - aferrari - add history details poup and columns for new fields

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
DEFINE VARIABLE chrPopupPackoutConfigHistory       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPackoutConfigID                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPackoutConfigHistoryID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToPackoutConfigRow        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToPackoutConfigHistoryRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectPackoutConfigRow          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedPackoutConfig           AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedPackoutConfigHistory    AS INTEGER   NO-UNDO.
DEFINE VARIABLE logRecordAvailable                 AS LOGICAL   NO-UNDO.

/* Objects */
DEFINE VARIABLE PackoutConfigBrowseFrame           AS pageFrame.
DEFINE VARIABLE PackoutConfigBrowse                AS browseTable.
DEFINE VARIABLE PackoutConfigBrowseButtons         AS buttonBar.
DEFINE VARIABLE PackoutConfigDetailsForm           AS dataForm.
DEFINE VARIABLE PackoutConfigDetailsButtons        AS buttonBar.

/* Objects for History Popup */
DEFINE VARIABLE PackoutConfigHistoryBrowse         AS browseTable.
DEFINE VARIABLE PackoutConfigHistoryBrowseButtons  AS buttonBar.
DEFINE VARIABLE PackoutConfigHistoryBrowseForm     AS dataForm.
DEFINE VARIABLE PackoutConfigHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE PackoutConfigHistoryDetailsButtons AS buttonBar.

/* Buffers */
DEFINE BUFFER PostPackoutShipOrderStatus FOR ShipOrderStatus.
DEFINE BUFFER PostPackoutShipStatus      FOR ShipStatus.
DEFINE BUFFER ConfirmShipOrderStatus     FOR ShipOrderStatus.
DEFINE BUFFER ConfirmShipStatus          FOR ShipStatus.



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

&IF DEFINED(EXCLUDE-pPackoutConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutConfigBrowse Procedure 
PROCEDURE pPackoutConfigBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* If a record is available then disable the create */
   IF CAN-FIND(FIRST PackoutConfig) THEN
      logRecordAvailable = YES.

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "packoutconfig_details_form"}
   
   PackoutConfigBrowse              = NEW browseTable("packoutconfig_browse").
   PackoutConfigBrowse:BrowseWidth  = 965.
   PackoutConfigBrowse:BrowseHeight = 455.
   PackoutConfigBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   PackoutConfigBrowse:insertColumn(fTL("Config ID"), 70, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PackoutConfig}
   
   PackoutConfigBrowse:insertColumn(fTL("PostPackShipOrderStatus"),  175, "CHARACTER","left", FALSE).
   PackoutConfigBrowse:insertColumn(fTL("PostPackShipStatus"),       175, "CHARACTER","left", FALSE).
   PackoutConfigBrowse:insertColumn(fTL("Confirm ShipOrder Status"), 175, "CHARACTER","left",FALSE).
   PackoutConfigBrowse:insertColumn(fTL("Confirm Ship Status"),      175, "CHARACTER","left",FALSE).
   PackoutConfigBrowse:insertColumn(fTL("PrintPackSlipAtBegin"),     150, "LOGICAL","left",FALSE).   
   
   /*Body*/
   PackoutConfigBrowse:startBody().
      
   /* Find all PackoutConfigs then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   FOR EACH PackoutConfig NO-LOCK:
      
      PackoutConfigBrowse:startRow(PackoutConfig.PackoutConfigID, 
                                        "selectPackoutConfigRow(this," + '"' + STRING(PackoutConfig.PackoutConfigID) + '"' + ");", "").
      PackoutConfigBrowse:insertData(PackoutConfig.PackoutConfigID).
      
      /* Add in Optional & Customer Specific fields according to the PackoutConfigFields associated with this ProcessEvent */

      {webGetOptionalBrowseFields.i PackoutConfig}
      
      FIND FIRST PostPackoutShipOrderStatus NO-LOCK 
         WHERE PostPackoutShipOrderStatus.ShipOrderStatusID = PackoutConfig.PostPackoutShipOrderStatusID NO-ERROR.
      
      FIND FIRST PostPackoutShipStatus NO-LOCK 
         WHERE PostPackoutShipStatus.ShipStatusID = PackoutConfig.PostPackoutShipStatusID NO-ERROR.
         
      FIND FIRST ConfirmShipOrderStatus NO-LOCK 
         WHERE ConfirmShipOrderStatus.ShipOrderStatusID = PackoutConfig.PostPickConfirmShipOrderStatusID NO-ERROR.
      
      FIND FIRST ConfirmShipStatus NO-LOCK 
         WHERE ConfirmShipStatus.ShipStatusID = PackoutConfig.PostPickConfirmShipStatusID NO-ERROR.

      PackoutConfigBrowse:insertData(IF AVAILABLE PostPackoutShipOrderStatus THEN PostPackoutShipOrderStatus.StatusCode ELSE "","left").
      PackoutConfigBrowse:insertData(IF AVAILABLE PostPackoutShipStatus THEN PostPackoutShipStatus.StatusCode ELSE "","left").
      PackoutConfigBrowse:insertData(IF AVAILABLE ConfirmShipOrderStatus THEN ConfirmShipOrderStatus.StatusCode ELSE "","left").
      PackoutConfigBrowse:insertData(IF AVAILABLE ConfirmShipStatus THEN ConfirmShipStatus.StatusCode ELSE "","left").
      PackoutConfigBrowse:insertData(STRING(PackoutConfig.PrintPackSlipAtBegin,"Yes/No"), "left").

      /* Add hidden fields */
      PackoutConfigBrowse:insertHiddenData("PackoutConfigVersionID", PackoutConfig.VersionID).
      
      PackoutConfigBrowse:endRow().
      
   END. /* FOR EACH PackoutConfig NO-LOCK */
   
   PackoutConfigBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PackoutConfigBrowse:getErrors().
   
   /* Create a new frame */
   PackoutConfigBrowseFrame           = NEW pageFrame().
   PackoutConfigBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PackoutConfigBrowseFrame:FormAction="dbPackoutConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PackoutConfigBrowseFrame:formOpen("packoutconfig_browse_form").
   
   /* Start the Frame Header */
   PackoutConfigBrowseFrame:insertSpacer(5).
   PackoutConfigBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PackoutConfigBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PackoutConfigBrowseFrame:frameClose().
   PackoutConfigBrowseFrame:insertSpacer(10).
   
   PackoutConfigBrowseFrame:insertHiddenField("packoutconfig_browse_scroll", "").
   PackoutConfigBrowseFrame:insertHiddenField("PackoutConfigID", "").
   PackoutConfigBrowseFrame:insertHiddenField("PackoutConfigVersionID", "").
   PackoutConfigBrowseFrame:insertHiddenField("form_name", "packoutconfig_browse_form").
   PackoutConfigBrowseFrame:insertHiddenField("prog_name", "adPackoutConfigAdmin.p").
   PackoutConfigBrowseFrame:insertHiddenField("popup_packoutconfighistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PackoutConfigBrowseFrame}
   
   PackoutConfigBrowseFrame:formClose().
   
   /* Create Button Bar */
   PackoutConfigBrowseButtons           = NEW buttonBar().
   PackoutConfigBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   
   PackoutConfigBrowseButtons:addButton("packoutconfig_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewPackoutConfigDetails('packoutconfig_details_form');",
                                        "Disabled").

   PackoutConfigBrowseButtons:addButton("packoutconfig_browse_form_btn_history",
                                        fTL("History"),
                                        "viewPackoutConfigHistoryBrowse();",
                                        "Disabled").
   
   PackoutConfigBrowseButtons:addButton("packoutconfig_browse_form_btn_create",
                                        fTL("Create"),
                                        "createPackoutConfig('packoutconfig_details_form');",
                                        IF logRecordAvailable THEN "Disabled" ELSE "" ).
   
   PackoutConfigBrowseButtons:closeBar().  
   PackoutConfigBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPackoutConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutConfigDetails Procedure 
PROCEDURE pPackoutConfigDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "packoutconfig_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PackoutConfigID,MaxWeightPercentageVariation,PostPackoutShipOrderStatusID,PostPackoutShipStatusID,"
                                + "PostPickConfirmShipOrderStatusID,PostPickConfirmShipStatusID,PrintPackSlipAtBegin"
          chrEditFieldList     = "MaxWeightPercentageVariation,PostPackoutShipOrderStatusID,PostPackoutShipStatusID,"
                                 + "PostPickConfirmShipOrderStatusID,PostPickConfirmShipStatusID,PrintPackSlipAtBegin"
          chrNewFieldList      = "MaxWeightPercentageVariation"
          chrRequiredFieldList = "MaxWeightPercentageVariation"
          chrExtraFieldList    = ""
          chrValidateFieldList = "MaxWeightPercentageVariation:DECIMAL".
   
   PackoutConfigDetailsForm           = NEW dataForm("packoutconfig_details_form").
   PackoutConfigDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PackoutConfigDetailsForm:FormAction = "dbPackoutConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PackoutConfigDetailsForm:FormWidth  = 460.
   PackoutConfigDetailsForm:FormHeight = 300.
   PackoutConfigDetailsForm:FormTitle  = "PackoutConfig Details".
   PackoutConfigDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   PackoutConfigDetailsForm:insertPaddingColumn(20).
   PackoutConfigDetailsForm:insertColumn(220).
   PackoutConfigDetailsForm:insertColumn(70).
   PackoutConfigDetailsForm:insertColumn(120).
   PackoutConfigDetailsForm:insertColumn(80).
   PackoutConfigDetailsForm:insertColumn(70).  
   
   /* Fields */
   PackoutConfigDetailsForm:startRow().
   PackoutConfigDetailsForm:insertLabel(fTL("Config ID")).
   PackoutConfigDetailsForm:insertTextField("PackoutConfigID", "", 50, TRUE).  

   PackoutConfigDetailsForm:startRow().  
   PackoutConfigDetailsForm:insertLabel("Post Packout ShipOrder Status").
   PackoutConfigDetailsForm:insertComboField("PostPackoutShipOrderStatusID", "0", 110, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active 
      BY    ShipOrderStatus.ListingSequence:
      
      PackoutConfigDetailsForm:insertComboPairs("PostPackoutShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID) , ShipOrderStatus.StatusCode).
   END.

   PackoutConfigDetailsForm:startRow().  
   PackoutConfigDetailsForm:insertLabel("Post Packout Ship Status").
   PackoutConfigDetailsForm:insertComboField("PostPackoutShipStatusID", "0", 110, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active 
      BY    ShipStatus.ListingSequence:
      
      PackoutConfigDetailsForm:insertComboPairs("PostPackoutShipStatusID", STRING(ShipStatus.ShipStatusID) , ShipStatus.StatusCode).
   END.

   PackoutConfigDetailsForm:startRow().
   PackoutConfigDetailsForm:insertLabel(fTL("Confirm Ship Order StatusID")).
   PackoutConfigDetailsForm:insertComboField("PostPickConfirmShipOrderStatusID", "", 110, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active 
      BY    ShipOrderStatus.ListingSequence:
      
      PackoutConfigDetailsForm:insertComboPairs("PostPickConfirmShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID) , ShipOrderStatus.StatusCode).
   END.
   
   PackoutConfigDetailsForm:startRow().
   PackoutConfigDetailsForm:insertLabel(fTL("Confirm Ship StatusID")).
   PackoutConfigDetailsForm:insertComboField("PostPickConfirmShipStatusID", "", 110, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active 
      BY    ShipStatus.ListingSequence:
      
      PackoutConfigDetailsForm:insertComboPairs("PostPickConfirmShipStatusID", STRING(ShipStatus.ShipStatusID) , ShipStatus.StatusCode).
   END.
   
   PackoutConfigDetailsForm:startRow().
   PackoutConfigDetailsForm:insertLabel(fTL("Print Pack Slip At Begin")).
   PackoutConfigDetailsForm:insertComboField("PrintPackSlipAtBegin", "", 70, TRUE).
   PackoutConfigDetailsForm:insertComboPairs("PrintPackSlipAtBegin", "yes", "Yes").
   PackoutConfigDetailsForm:insertComboPairs("PrintPackSlipAtBegin", "no", "No").
   
   PackoutConfigDetailsForm:startRow().
   PackoutConfigDetailsForm:insertLabel(fTL("Max Weight Percentage Variation")).
   PackoutConfigDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 50, TRUE).
   

   {webGetOptionalFormFields.i pPackoutConfigDetailsFields}
   
   /* Add Hidden Fields*/
   PackoutConfigDetailsForm:insertHiddenField("packoutconfig_browse_scroll", "").
   PackoutConfigDetailsForm:insertHiddenField("form_name", "packoutconfig_details_form").
   PackoutConfigDetailsForm:insertHiddenField("prog_name", "adPackoutConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PackoutConfigDetailsForm}
   
   /* Create Button Bar */
   PackoutConfigDetailsButtons = NEW buttonBar().
   
   PackoutConfigDetailsButtons:addButton("packoutconfig_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updatePackoutConfig('packoutconfig_details_form');").
   
   PackoutConfigDetailsButtons:addButton("packoutconfig_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "cancelUpdate('UserCancelled','process_mode'); disablePopup('packoutconfig_details_form_popup');").
   
   PackoutConfigDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PackoutConfigDetailsForm:FormButtons = PackoutConfigDetailsButtons.
   
   PackoutConfigDetailsForm:endForm(). 
   
   PackoutConfigDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPackoutConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutConfigDetailsFields Procedure 
PROCEDURE pPackoutConfigDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         PackoutConfigDetailsForm:startRow().
         PackoutConfigDetailsForm:insertLabel(fTL("Field Label")).
         PackoutConfigDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adPackoutConfigAdmin_packoutconfig_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPackoutConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutConfigHistoryBrowse Procedure 
PROCEDURE pPackoutConfigHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   PackoutConfigHistoryBrowseForm = NEW dataForm("packoutconfighistory_browse_form").
   PackoutConfigHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   PackoutConfigHistoryBrowseForm:FormWidth  = 860.
   PackoutConfigHistoryBrowseForm:FormHeight = 530.
   PackoutConfigHistoryBrowseForm:FormTitle  = fTL("Packout Config History") + (IF AVAILABLE PackoutConfig THEN " : " 
                                                                                      + STRING(PackoutConfig.PackoutConfigID) ELSE "").
   PackoutConfigHistoryBrowseForm:FormType   = "xxl_large".
   
   PackoutConfigHistoryBrowse = NEW browseTable("packoutconfighistory_browse").
   PackoutConfigHistoryBrowse:BrowseWidth  = 840.
   PackoutConfigHistoryBrowse:BrowseHeight = 490.
   
   PackoutConfigHistoryBrowse:insertColumn(fTL("History ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PackoutConfigHistory}
   
   PackoutConfigHistoryBrowse:insertColumn(fTL("PostPackShipOrderStatus"), 165, "DECIMAL", "left",   FALSE).
   PackoutConfigHistoryBrowse:insertColumn(fTL("PostPackShipStatus"),      140, "DECIMAL", "left",   FALSE).
   PackoutConfigHistoryBrowse:insertColumn(fTL("ConfirmShipOrderStatus"),  165, "DECIMAL", "left",   FALSE).
   PackoutConfigHistoryBrowse:insertColumn(fTL("ConfirmShipStatus"),       140, "DECIMAL", "left",   FALSE).
   PackoutConfigHistoryBrowse:insertColumn(fTL("Created"),                 100, "CHARACTER", "left", FALSE).
   
   PackoutConfigHistoryBrowse:StartBody().
   
   IF AVAILABLE PackoutConfig THEN
   DO:
      /* List the PackoutConfigHistory for the PackoutConfig */
      FOR EACH PackoutConfigHistory NO-LOCK
         WHERE PackoutConfigHistory.PackoutConfigID = PackoutConfig.PackoutConfigID 
         BY PackoutConfigHistory.PackoutConfigID
         BY PackoutConfigHistory.Created DESCENDING:
         
         FIND FIRST PostPackoutShipOrderStatus NO-LOCK 
         WHERE PostPackoutShipOrderStatus.ShipOrderStatusID = PackoutConfigHistory.PostPackoutShipOrderStatusID NO-ERROR.
      
         FIND FIRST PostPackoutShipStatus NO-LOCK 
            WHERE PostPackoutShipStatus.ShipStatusID = PackoutConfigHistory.PostPackoutShipStatusID NO-ERROR.
         
         FIND FIRST ConfirmShipOrderStatus NO-LOCK 
            WHERE ConfirmShipOrderStatus.ShipOrderStatusID = PackoutConfigHistory.PostPickConfirmShipOrderStatusID NO-ERROR.
         
         FIND FIRST ConfirmShipStatus NO-LOCK 
            WHERE ConfirmShipStatus.ShipStatusID = PackoutConfigHistory.PostPickConfirmShipStatusID NO-ERROR.
            
         PackoutConfigHistoryBrowse:startRow(PackoutConfigHistory.PackoutConfigHistoryID, "selectPackoutConfigHistoryRow(this," 
                                            + '"' + STRING(PackoutConfigHistory.PackoutConfigHistoryID)
                                            + '","adPackoutConfigAdmin.p","packoutconfig_browse_form"' + ");", "").

         PackoutConfigHistoryBrowse:insertData(PackoutConfigHistory.PackoutConfigHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PackoutConfigHistory}

         PackoutConfigHistoryBrowse:insertData(IF AVAILABLE PostPackoutShipOrderStatus THEN PostPackoutShipOrderStatus.StatusCode ELSE "","left").
         PackoutConfigHistoryBrowse:insertData(IF AVAILABLE PostPackoutShipStatus THEN PostPackoutShipStatus.StatusCode ELSE "","left").
         PackoutConfigHistoryBrowse:insertData(IF AVAILABLE ConfirmShipOrderStatus THEN ConfirmShipOrderStatus.StatusCode ELSE "","left").
         PackoutConfigHistoryBrowse:insertData(IF AVAILABLE ConfirmShipStatus THEN ConfirmShipStatus.StatusCode ELSE "","left").
         PackoutConfigHistoryBrowse:insertData(fDisplayDate&Time(PackoutConfigHistory.Created,"y/m/d H:M:S"), "left").                  

         PackoutConfigHistoryBrowse:endRow().
      
      END. /* FOR EACH PackoutConfigHistory OF PackoutConfig NO-LOCK */
   END. /*IF AVAILABLE PackoutConfig THEN*/
   
   PackoutConfigHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PackoutConfigHistoryBrowse:getErrors().

   PackoutConfigHistoryBrowseForm:insertHiddenField("PackoutConfigHistoryID", "").
   PackoutConfigHistoryBrowseForm:insertHiddenField("popup_packoutconfighistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PackoutConfigHistoryBrowseForm}
   
   /* Create Button Bar */
   PackoutConfigHistoryBrowseButtons = NEW buttonBar().
   
   PackoutConfigHistoryBrowseButtons:addButton("packoutconfighistory_browse_form_btn_details",
                                              fTL("Details"),
                                              "viewPackoutConfigHistoryDetails('packoutconfighistory_details_form');",
                                              (IF intSelectedPackoutConfigHistory > 0 THEN "" ELSE "Disabled")).
/*                                               (IF intSelectedPackotConfigHistory > 0 THEN "" ELSE "Disabled")).*/

          
   PackoutConfigHistoryBrowseButtons:addButton("packoutconfighistory_browse_form_btn_cancel",
                                               fTL("Cancel"),
                                               "disablePopup('packoutconfighistory_browse_form_popup');").
   
   PackoutConfigHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PackoutConfigHistoryBrowseForm:FormBrowse  = PackoutConfigHistoryBrowse.
   PackoutConfigHistoryBrowseForm:FormButtons = PackoutConfigHistoryBrowseButtons.
   PackoutConfigHistoryBrowseForm:endForm(). 
   
   PackoutConfigHistoryBrowseForm:displayForm().
   
END PROCEDURE.

PROCEDURE pPackoutConfigHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "packoutconfighistory_details_form"}
   ASSIGN chrDisplayFieldList  = "PackoutConfigHistoryID,PackoutConfigID,MaxWeightPercentageVariation,PostPackoutShipOrderStatusID," +
                                 "PostPackoutShipStatusID,PostPickConfirmShipOrderStatusID,PostPickConfirmShipStatusID," +
                                 "PrintPackSlipAtBegin,CreatedDate,CreatedHour,CreatedMins,OperationTypeID,GateUserID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PackoutConfigHistoryDetailsForm = NEW dataForm("packoutconfighistory_details_form").
   PackoutConfigHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PackoutConfigHistoryDetailsForm:FormAction = "dbPackoutConfigUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PackoutConfigHistoryDetailsForm:FormWidth   = 460.
   PackoutConfigHistoryDetailsForm:FormHeight  = 300.
   PackoutConfigHistoryDetailsForm:FormTitle   = "Packout Config History Details".
   PackoutConfigHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PackoutConfigHistoryDetailsForm:insertPaddingColumn(30).
   PackoutConfigHistoryDetailsForm:insertColumn(150).
   PackoutConfigHistoryDetailsForm:insertColumn(125).
   PackoutConfigHistoryDetailsForm:insertColumn(20).
   PackoutConfigHistoryDetailsForm:insertColumn(4).
   PackoutConfigHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("History ID")).
   PackoutConfigHistoryDetailsForm:insertTextField("PackoutConfigHistoryID", "", 100, TRUE).
   
   PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Packout Config ID")).
   PackoutConfigHistoryDetailsForm:insertTextField("PackoutConfigID", "", 100, TRUE).
   
   PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Post Ship Order StatusID")).
   PackoutConfigHistoryDetailsForm:insertComboField("PostPackoutShipOrderStatusID", "", 100, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active 
      BY    ShipOrderStatus.ListingSequence:
      
      PackoutConfigHistoryDetailsForm:insertComboPairs("PostPackoutShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID) , ShipOrderStatus.StatusCode).
   END.
   
   PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Post Ship StatusID")).
   PackoutConfigHistoryDetailsForm:insertComboField("PostPackoutShipStatusID", "", 100, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active 
      BY    ShipStatus.ListingSequence:
      
      PackoutConfigHistoryDetailsForm:insertComboPairs("PostPackoutShipStatusID", STRING(ShipStatus.ShipStatusID) , ShipStatus.StatusCode).
   END.
   
   PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Confirm ShpOrd StatusID")).
   PackoutConfigHistoryDetailsForm:insertComboField("PostPickConfirmShipOrderStatusID", "", 100, TRUE).
    FOR EACH ShipOrderStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipOrderStatus.Active 
      BY    ShipOrderStatus.ListingSequence:
      
      PackoutConfigHistoryDetailsForm:insertComboPairs("PostPickConfirmShipOrderStatusID", STRING(ShipOrderStatus.ShipOrderStatusID) , ShipOrderStatus.StatusCode).
   END.
   
   PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Confirm Ship StatusID")).
   PackoutConfigHistoryDetailsForm:insertComboField("PostPickConfirmShipStatusID", "", 100, TRUE).
   FOR EACH ShipStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipStatus.Active 
      BY    ShipStatus.ListingSequence:
      
      PackoutConfigHistoryDetailsForm:insertComboPairs("PostPickConfirmShipStatusID", STRING(ShipStatus.ShipStatusID) , ShipStatus.StatusCode).
   END.
   
   PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Max Weight Variation %")).
   PackoutConfigHistoryDetailsForm:insertTextField("MaxWeightPercentageVariation", "", 100, TRUE).
   
    PackoutConfigHistoryDetailsForm:startRow().
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Print Pack Slip At Begin")).
   PackoutConfigHistoryDetailsForm:insertTextField("PrintPackSlipAtBegin", "", 100, TRUE).

   PackoutConfigHistoryDetailsForm:startRow().                                                                                           
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   PackoutConfigHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   PackoutConfigHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   PackoutConfigHistoryDetailsForm:insertLabel(":").                                                                                     
   PackoutConfigHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                                         
   PackoutConfigHistoryDetailsForm:startRow().                                                                                           
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   PackoutConfigHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/                                                                                                      
      BY OperationType.OperationTypeID:                                                                                                  
      PackoutConfigHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   PackoutConfigHistoryDetailsForm:startRow().                                                                                           
   PackoutConfigHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   PackoutConfigHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*GateUserID*/                                                                                                        
      BY GateUser.FullName:                                                                                                              
      PackoutConfigHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pPackoutConfigHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PackoutConfigHistoryDetailsForm:insertHiddenField("packoutconfighistory_browse_scroll", "").
   PackoutConfigHistoryDetailsForm:insertHiddenField("form_name", "packoutconfighistory_details_form").
   PackoutConfigHistoryDetailsForm:insertHiddenField("prog_name", "adPackoutConfigAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PackoutConfigHistoryDetailsForm}
   
   /* Create Button Bar */
   PackoutConfigHistoryDetailsButtons = NEW buttonBar().

   PackoutConfigHistoryDetailsButtons:addButton("packoutconfighistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "disablePopup('packoutconfighistory_details_form_popup');").
   PackoutConfigHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PackoutConfigHistoryDetailsForm:FormButtons = PackoutConfigHistoryDetailsButtons.
   
   PackoutConfigHistoryDetailsForm:endForm(). 
   
   PackoutConfigHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pPackoutConfigHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      PackoutConfigHistoryDetailsForm:startRow().
      PackoutConfigHistoryDetailsForm:insertLabel(fTL("Field Label")).
      PackoutConfigHistoryDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adPackoutConfigAdmin_packoutconfig_details_form.i}*/ COMEBACK START COMEBACK END*/
    
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
   
   ASSIGN chrPackoutConfigID                 = get-value("PackoutConfigID")
          intSelectedPackoutConfig           = INTEGER(chrPackoutConfigID)
          chrScrollToPackoutConfigRow        = STRING(INTEGER(get-value("packoutconfig_browse_scroll"))) + ";"
          /*History details button*/
          chrPackoutConfigHistoryID          = get-value("PackoutConfigHistoryID")
          intSelectedPackoutConfigHistory    = INTEGER(chrPackoutConfigHistoryID)
          chrScrollToPackoutConfigHistoryRow = STRING(INTEGER(get-value("packoutconfighistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPackoutConfigID <> "" THEN
      chrSelectPackoutConfigRow = 'selectPackoutConfigRow(document.getElementById("packoutconfig_browse_row_' + chrPackoutConfigID + '"),"' 
                                                         + chrPackoutConfigID +  '");'.

   IF get-value('popup_packoutconfighistory_browse') = "yes" THEN
      chrPopupPackoutConfigHistory = 'enablePopup("packoutconfighistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("packoutconfig_browse").scrollTop=' + chrScrollToPackoutConfigRow 
                             + chrSelectPackoutConfigRow + chrPopupPackoutConfigHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "PackoutConfig Admin".
   ThisPage:FrameTitle    = "PackoutConfig Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("packoutconfig.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pPackoutConfigBrowse.
   
   IF intSelectedPackoutConfig <> 0 THEN
      FIND FIRST PackoutConfig NO-LOCK 
         WHERE PackoutConfig.PackoutConfigID = intSelectedPackoutConfig NO-ERROR.

   /******* Pop-up Browsers and Forms ********/    
   RUN pPackoutConfigDetails.
   RUN pPackoutConfigHistoryBrowse.
   RUN pPackoutConfigHistoryDetails.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT PackoutConfigBrowseFrame           NO-ERROR.
   DELETE OBJECT PackoutConfigBrowse                NO-ERROR.
   DELETE OBJECT PackoutConfigBrowseButtons         NO-ERROR.
   DELETE OBJECT PackoutConfigDetailsForm           NO-ERROR.
   DELETE OBJECT PackoutConfigDetailsButtons        NO-ERROR.
   
   DELETE OBJECT PackoutConfigHistoryBrowse         NO-ERROR.
   DELETE OBJECT PackoutConfigHistoryBrowseButtons  NO-ERROR.
   DELETE OBJECT PackoutConfigHistoryBrowseForm     NO-ERROR.
   DELETE OBJECT PackoutConfigHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT PackoutConfigHistoryDetailsButtons NO-ERROR.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

