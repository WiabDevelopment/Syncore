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

  Author:  Anthony Ferrari

  Created: 17/09/2015
  
  Revisions:
     27/10/2015 - twierzch - add Command button and browse to create NetworkActionCommandLinks

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


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intNetworkActionPoint                       AS INTEGER     NO-UNDO.
DEFINE VARIABLE intNetworkActionPointHistory                AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrNetworkActionPointList                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectNetworkActionPointRow              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkActionPointRow            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkActionPointID                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkActionPointHistoryID              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedNetworkActionPointHistory        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkActionPointHistoryRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logPreventDataCreates                       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE chrPopupHistory                             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedNetworkActionCommandLink         AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkActionCommandLinkID         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectNetworkActionCommandLinkID         AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkActionCommandLinkRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCommandBrowse                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedNetworkActionCommandLinkHistory  AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPopupCommandLinkHistoryBrowse            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intNetworkActionCommandLink                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPopupStopServer                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrStopProcessRan                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupStartServer                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrStartProcessRan                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupSendData                            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSendProcessRan                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrActiveServers                            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrInActiveServers                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDataWasSent                              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrResponseFromDb                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupResponseFromDb                      AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE NetworkActionPointBrowseFrame               AS pageFrame.
DEFINE VARIABLE NetworkActionPointBrowse                    AS browseTable.
DEFINE VARIABLE NetworkActionPointBrowseButtons             AS buttonBar.
DEFINE VARIABLE NetworkActionPointDetailsForm               AS dataForm.
DEFINE VARIABLE NetworkActionPointDetailsButtons            AS buttonBar.

DEFINE VARIABLE NetworkActionPointHistoryBrowseForm         AS dataForm.
DEFINE VARIABLE NetworkActionPointHistoryBrowse             AS browseTable.
DEFINE VARIABLE NetworkActionPointHistoryBrowseButtons      AS buttonBar.
DEFINE VARIABLE NetworkActionPointHistoryDetailsForm        AS dataForm.
DEFINE VARIABLE NetworkActionPointHistoryDetailsButtons     AS buttonBar.

DEFINE VARIABLE NetworkActionCommandLinkBrowseForm          AS dataForm.
DEFINE VARIABLE NetworkActionCommandLinkBrowse              AS browseTable.
DEFINE VARIABLE NetworkActionCommandLinkBrowseButtons       AS buttonBar.
DEFINE VARIABLE NetworkActionCommandLinkDetailsForm         AS dataForm.
DEFINE VARIABLE NetworkActionCommandLinkDetailsButtons      AS buttonBar.

DEFINE VARIABLE NetworkActionCommandLinkHistBrowseForm      AS dataForm.
DEFINE VARIABLE NetworkActionCommandLinkHistBrowse          AS browseTable.
DEFINE VARIABLE NetworkActionCommandLinkHistBrowseButtons   AS buttonBar.
DEFINE VARIABLE NetworkActionCommandLinkHistDetailsForm     AS dataForm.
DEFINE VARIABLE NetworkActionCommandLinkHistDetailsButtons  AS buttonBar.

DEFINE VARIABLE NetworkActionSendDataForm                   AS dataForm.
DEFINE VARIABLE NetworkActionSendDataFormButtons            AS buttonBar.

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

&IF DEFINED(EXCLUDE-pNetworkActionCommandLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionCommandLinkBrowse Procedure 
PROCEDURE pNetworkActionCommandLinkBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   NetworkActionCommandLinkBrowseForm           = NEW dataForm("networkactioncommandlink_browse_form").
   NetworkActionCommandLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkActionCommandLinkBrowseForm:FormWidth  = 580.
   NetworkActionCommandLinkBrowseForm:FormHeight = 420.
   NetworkActionCommandLinkBrowseForm:FormTitle  = fTL("Network Action Commands").
   NetworkActionCommandLinkBrowseForm:FormType   = "large".
   NetworkActionCommandLinkBrowse                = NEW browseTable("networkactioncommandlink_browse").
   NetworkActionCommandLinkBrowse:BrowseWidth    = 560.
   NetworkActionCommandLinkBrowse:BrowseHeight   = 380.
   
   NetworkActionCommandLinkBrowse:insertColumn(fTL("LinkID"),       80, "INTEGER",           FALSE).
   NetworkActionCommandLinkBrowse:insertColumn(fTL("CommandCode"), 200, "CHARACTER", "LEFT", FALSE).
   NetworkActionCommandLinkBrowse:insertColumn(fTL("ActionValue"), 201, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionCommandLink}
   
   NetworkActionCommandLinkBrowse:StartBody().
   
   FOR EACH NetworkActionCommandLink NO-LOCK /*idx=ActionPointIDCommandID*/
      WHERE NetworkActionCommandLink.NetworkActionPointID = intNetworkActionPoint:
          
      FIND FIRST NetworkActionCommand OF NetworkActionCommandLink NO-LOCK NO-ERROR. /*idx=NetworkActionCommandID*/

      NetworkActionCommandLinkBrowse:startRow (NetworkActionCommandLink.NetworkActionCommandLinkID, 
         "selectNetworkActionCommandLinkRow(this," + '"' + STRING(NetworkActionCommandLink.NetworkActionCommandLinkID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i NetworkActionCommandLink}
      
      NetworkActionCommandLinkBrowse:insertData(NetworkActionCommandLink.NetworkActionCommandLinkID).
      NetworkActionCommandLinkBrowse:insertData(NetworkActionCommand.CommandCode,"LEFT").
      NetworkActionCommandLinkBrowse:insertData(NetworkActionCommandLink.ActionValue,"LEFT").
      
      NetworkActionCommandLinkBrowse:insertHiddenData("NetworkActionCommandLinkVersionID",NetworkActionCommandLink.VersionID).
      
      NetworkActionCommandLinkBrowse:endRow().
   END. /* FOR EACH NetworkActionCommandLink */
   
   NetworkActionCommandLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkActionCommandLinkBrowse:getErrors().
   
   NetworkActionCommandLinkBrowseForm:insertHiddenField("popup_networkactioncommandlink_browse","").
   NetworkActionCommandLinkBrowseForm:insertHiddenField("popup_networkactioncommandlinkhistory_browse","").
   NetworkActionCommandLinkBrowseForm:insertHiddenField("NetworkActionCommandLinkID","").
   NetworkActionCommandLinkBrowseForm:insertHiddenField("NetworkActionCommandLinkVersionID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionCommandLinkBrowseForm}
   
   /* Create Button Bar */
   NetworkActionCommandLinkBrowseButtons = NEW buttonBar().
   
   NetworkActionCommandLinkBrowseButtons:addButton("networkactioncommandlink_browse_form_btn_add",
                                                   fTL("Add"),
                                                   "addNetworkActionCommandLinkDetails('networkactioncommandlink_details_form');").
                                                                                            
   NetworkActionCommandLinkBrowseButtons:addButton("networkactioncommandlink_browse_form_btn_details",
                                                   fTL("Details"),
                                                   "viewNetworkActionCommandLinkDetails('networkactioncommandlink_details_form');",
                                                   (IF intSelectedNetworkActionCommandLink > 0 THEN "" ELSE "Disabled")).    
                                         
   NetworkActionCommandLinkBrowseButtons:addButton("networkactioncommandlink_browse_form_btn_history",
                                                   fTL("History"),
                                                   "viewNetworkActionCommandLinkHistory();",
                                                   (IF intSelectedNetworkActionCommandLink > 0 THEN "" ELSE "Disabled")).    
                                         
   NetworkActionCommandLinkBrowseButtons:addButton("networkactioncommandlink_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('networkactioncommandlink_browse_form_popup');").
                                                   
   NetworkActionCommandLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionCommandLinkBrowseForm:FormBrowse  = NetworkActionCommandLinkBrowse.
   NetworkActionCommandLinkBrowseForm:FormButtons = NetworkActionCommandLinkBrowseButtons.
   NetworkActionCommandLinkBrowseForm:endForm(). 
   
   NetworkActionCommandLinkBrowseForm:displayForm().  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionCommandLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionCommandLinkDetails Procedure 
PROCEDURE pNetworkActionCommandLinkDetails :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkactionpointhistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkActionCommandLinkID,NetworkActionPointID,NetworkActionCommandID,ActionValue"
          chrEditFieldList     = "ActionValue"
          chrNewFieldList      = "NetworkActionCommandID,ActionValue"
          chrRequiredFieldList = "ActionValue"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkActionCommandLinkDetailsForm = NEW dataForm("networkactioncommandlink_details_form").
   NetworkActionCommandLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkActionCommandLinkDetailsForm:FormAction = "dbNetworkActionCommandLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkActionCommandLinkDetailsForm:FormWidth   = 460.
   NetworkActionCommandLinkDetailsForm:FormHeight  = 300.
   NetworkActionCommandLinkDetailsForm:FormTitle   = "ActionPoint History Details".
   NetworkActionCommandLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   NetworkActionCommandLinkDetailsForm:insertPaddingColumn(50).
   NetworkActionCommandLinkDetailsForm:insertColumn(120).
   NetworkActionCommandLinkDetailsForm:insertColumn(120).
   NetworkActionCommandLinkDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkActionCommandLinkDetailsForm:startRow().
   NetworkActionCommandLinkDetailsForm:insertLabel(fTL("History ID")).
   NetworkActionCommandLinkDetailsForm:insertTextField("NetworkActionCommandLinkID", "", 200, TRUE).  
   
   NetworkActionCommandLinkDetailsForm:startRow().
   NetworkActionCommandLinkDetailsForm:insertLabel(fTL("NetworkActionPointID")).
   NetworkActionCommandLinkDetailsForm:insertComboField("NetworkActionPointID", "", 200, TRUE).
   FOR EACH NetworkActionPoint NO-LOCK /*idx=NetworkActionPointID*/
      WHERE NetworkActionPoint.Active:
            
      NetworkActionCommandLinkDetailsForm:insertComboPairs("NetworkActionPointID",
                                                           STRING(NetworkActionPoint.NetworkActionPointID),
                                                           NetworkActionPoint.NetworkActionPointCode). 
   END. 
   
   NetworkActionCommandLinkDetailsForm:startRow().
   NetworkActionCommandLinkDetailsForm:insertLabel(fTL("NetworkActionCommandID")).
   NetworkActionCommandLinkDetailsForm:insertComboField("NetworkActionCommandID", "", 200, TRUE).    
   FOR EACH NetworkActionCommand NO-LOCK: /*idx=LocationTypeActive*/
      
      NetworkActionCommandLinkDetailsForm:insertComboPairs("NetworkActionCommandID",
                                                           STRING(NetworkActionCommand.NetworkActionCommandID),
                                                           NetworkActionCommand.CommandCode). 
   END. 
   
   NetworkActionCommandLinkDetailsForm:startRow().
   NetworkActionCommandLinkDetailsForm:insertLabel(fTL("ActionValue")).
   NetworkActionCommandLinkDetailsForm:insertTextField("ActionValue", "", 200, TRUE).
   
   {webGetOptionalFormFields.i pNetworkActionCommandLinkDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   NetworkActionCommandLinkDetailsForm:insertHiddenField("NetworkActionCommandLink_browse_scroll", "").
   NetworkActionCommandLinkDetailsForm:insertHiddenField("form_name", "networkactioncommandlink_details_form").
   NetworkActionCommandLinkDetailsForm:insertHiddenField("prog_name", "adNetworkActionPointAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionCommandLinkDetailsForm}
   
   /* Create Button Bar */
   NetworkActionCommandLinkDetailsButtons = NEW buttonBar().

   NetworkActionCommandLinkDetailsButtons:addButton("networkactioncommandlink_details_form_btn_save", 
                                                    fTL("Edit"), 
                                                    "updateNetworkActionCommandLink('networkactioncommandlink_details_form');").
                                                     
   NetworkActionCommandLinkDetailsButtons:addButton("networkactioncommandlink_details_form_btn_cancel", 
                                                    fTL("Cancel"), 
                                                    "disablePopup('networkactioncommandlink_details_form_popup');").
                                                     
   NetworkActionCommandLinkDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionCommandLinkDetailsForm:FormButtons =  NetworkActionCommandLinkDetailsButtons.
   
   NetworkActionCommandLinkDetailsForm:endForm(). 
   
   NetworkActionCommandLinkDetailsForm:displayForm(). 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionCommandLinkHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionCommandLinkHistory Procedure 
PROCEDURE pNetworkActionCommandLinkHistory :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   NetworkActionCommandLinkHistBrowseForm           = NEW dataForm("networkactioncommandlinkhistory_browse_form").
   NetworkActionCommandLinkHistBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkActionCommandLinkHistBrowseForm:FormWidth  = 580.
   NetworkActionCommandLinkHistBrowseForm:FormHeight = 420.
   NetworkActionCommandLinkHistBrowseForm:FormTitle  = fTL("Network Action Command History").
   NetworkActionCommandLinkHistBrowseForm:FormType   = "large".
   NetworkActionCommandLinkHistBrowse                = NEW browseTable("networkactioncommandlinkhistory_browse").
   NetworkActionCommandLinkHistBrowse:BrowseWidth    = 560.
   NetworkActionCommandLinkHistBrowse:BrowseHeight   = 380.
   
   NetworkActionCommandLinkHistBrowse:insertColumn(fTL("HistoryID"),     80, "INTEGER",           FALSE).
   NetworkActionCommandLinkHistBrowse:insertColumn(fTL("Command Code"), 200, "CHARACTER", "LEFT", FALSE).
   NetworkActionCommandLinkHistBrowse:insertColumn(fTL("Action Value"), 200, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionCommandLinkHist}
   
   NetworkActionCommandLinkHistBrowse:StartBody().
   
   FOR EACH NetworkActionCommandLinkHist NO-LOCK /*idx=ActionPointIDCommandID*/
      WHERE NetworkActionCommandLinkHist.NetworkActionCommandLinkID = NetworkActionCommandLink.NetworkActionCommandLinkID:
          
      FIND FIRST NetworkActionCommand OF NetworkActionCommandLinkHist NO-LOCK NO-ERROR. /*idx=NetworkActionCommandID*/

      NetworkActionCommandLinkHistBrowse:startRow (NetworkActionCommandLinkHist.NetworkActionCommandLinkHistID, 
         "selectNetworkActionCommandLinkHistoryRow(this," + '"' + STRING(NetworkActionCommandLinkHist.NetworkActionCommandLinkHistID) 
         + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i NetworkActionCommandLinkHist}
      
      NetworkActionCommandLinkHistBrowse:insertData(NetworkActionCommandLinkHist.NetworkActionCommandLinkHistID).
      NetworkActionCommandLinkHistBrowse:insertData(NetworkActionCommandHist.CommandCode,"LEFT").
      NetworkActionCommandLinkHistBrowse:insertData(NetworkActionCommandLinkHist.ActionValue,"LEFT").
      
      NetworkActionCommandLinkHistBrowse:endRow().
   END. /* FOR EACH PickPackStationHistory */
   
   NetworkActionCommandLinkHistBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkActionCommandLinkHistBrowse:getErrors().
   
   NetworkActionCommandLinkHistBrowseForm:insertHiddenField("popup_networkactioncommandlink_browse","").
   NetworkActionCommandLinkHistBrowseForm:insertHiddenField("popup_networkactioncommandlinkhistory_browse","").
   NetworkActionCommandLinkHistBrowseForm:insertHiddenField("NetworkActionCommandLinkID","").
   NetworkActionCommandLinkHistBrowseForm:insertHiddenField("NetworkActionCommandLinkVersionID","").
   NetworkActionCommandLinkHistBrowseForm:insertHiddenField("NetworkActionCommandLinkHistID","").
        
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionCommandLinkHistBrowseForm}
   
   /* Create Button Bar */
   NetworkActionCommandLinkHistBrowseButtons = NEW buttonBar().
   
   NetworkActionCommandLinkHistBrowseButtons:addButton("networkactioncommandlinkhistory_browse_form_btn_details",
                                                       fTL("Details"),
                                                       "viewNetworkActionCommandLinkHistoryDetails('networkactioncommandlinkhistory_details_form');",
                                                       "Disabled").                                                                                                  
                                         
   NetworkActionCommandLinkHistBrowseButtons:addButton("networkactioncommandlinkhistory_browse_form_btn_cancel",
                                                       fTL("Cancel"),
                                                       "disablePopup('networkactioncommandlinkhistory_browse_form_popup');").
                                                   
   NetworkActionCommandLinkHistBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionCommandLinkHistBrowseForm:FormBrowse  = NetworkActionCommandLinkHistBrowse.
   NetworkActionCommandLinkHistBrowseForm:FormButtons = NetworkActionCommandLinkHistBrowseButtons.
   NetworkActionCommandLinkHistBrowseForm:endForm(). 
   
   NetworkActionCommandLinkHistBrowseForm:displayForm().  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointBrowse Procedure 
PROCEDURE pNetworkActionPointBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkactionpoint_details_form"}

   NetworkActionPointBrowse = NEW browseTable("networkactionpoint_browse").
   NetworkActionPointBrowse:BrowseWidth  = 965.
   NetworkActionPointBrowse:BrowseHeight = 455.
   NetworkActionPointBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkActionPointBrowse:insertColumn(fTL("ID"), 85, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionPoint}
   
   NetworkActionPointBrowse:insertColumn(fTL("ActionPoint Code"), 150, "CHARACTER", "LEFT", FALSE).
   NetworkActionPointBrowse:insertColumn(fTL("ActionPoint Type"), 150, "INTEGER",   "LEFT", FALSE).
   NetworkActionPointBrowse:insertColumn(fTL("ToteLine"),         150, "INTEGER",   "LEFT", FALSE).
   NetworkActionPointBrowse:insertColumn(fTL("Register"),          75, "INTEGER",   "LEFT", FALSE).
   NetworkActionPointBrowse:insertColumn(fTL("IsOperating"),       75, "INTEGER",   "LEFT", FALSE).
   NetworkActionPointBrowse:insertColumn(fTL("Active"),            75, "LOGICAL",           FALSE).
   
   /*Body*/
   NetworkActionPointBrowse:startBody().

   FOR EACH NetworkActionPoint NO-LOCK /*idx=ActiveVendorName*/
      BY    NetworkActionPoint.Active DESCENDING 
      BY   NetworkActionPoint.NetworkActionPointID:
      
      FIND FIRST ToteLine OF NetworkActionPoint NO-LOCK NO-ERROR.
      FIND FIRST NetworkActionPointType OF NetworkActionPoint NO-LOCK NO-ERROR.
                                       
      NetworkActionPointBrowse:startRow(NetworkActionPoint.NetworkActionPointID, 
         'multiSelectNetworkActionPointRow("' + STRING(NetworkActionPoint.NetworkActionPointID) + '")', "").
      NetworkActionPointBrowse:insertData(NetworkActionPoint.NetworkActionPointID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkActionPoint}
      
      NetworkActionPointBrowse:insertData((IF AVAILABLE NetworkActionPoint THEN NetworkActionPoint.NetworkActionPointCode ELSE ""), "LEFT").
      NetworkActionPointBrowse:insertData((IF AVAILABLE NetworkActionPointType THEN NetworkActionPointType.TypeName ELSE ""), "LEFT").
      NetworkActionPointBrowse:insertData((IF AVAILABLE ToteLine THEN ToteLine.LineName ELSE ""), "LEFT").
      NetworkActionPointBrowse:insertData(STRING(NetworkActionPoint.Register), "LEFT").
      NetworkActionPointBrowse:insertData((IF AVAILABLE NetworkActionPoint THEN STRING(NetworkActionPoint.IsOperating,"Yes/No") ELSE "")).
      NetworkActionPointBrowse:insertData((IF AVAILABLE NetworkActionPoint THEN STRING(NetworkActionPoint.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      NetworkActionPointBrowse:insertHiddenData("NetworkActionPointVersionID",NetworkActionPoint.VersionID).
      
      NetworkActionPointBrowse:endRow().
      
   END. /*FOR EACH NetworkActionPoint NO-LOCK */
   
   NetworkActionPointBrowse:endTable().
   
   /* Create a new frame */
   NetworkActionPointBrowseFrame = NEW pageFrame().
   NetworkActionPointBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   NetworkActionPointBrowseFrame:FormAction="dbNetworkActionPointUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   NetworkActionPointBrowseFrame:formOpen("networkactionpoint_browse_form").

   /* Start the Frame Header */
   NetworkActionPointBrowseFrame:insertSpacer(5).
   NetworkActionPointBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkActionPointBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkActionPointBrowseFrame:frameClose().
   NetworkActionPointBrowseFrame:insertSpacer(10).
   
   NetworkActionPointBrowseFrame:insertHiddenField("networkactionpoint_browse_scroll","").
   NetworkActionPointBrowseFrame:insertHiddenField("NetworkActionPointID", chrNetworkActionPointID).
   NetworkActionPointBrowseFrame:insertHiddenField("NetworkActionPointVersionID","").
   NetworkActionPointBrowseFrame:insertHiddenField("NetworkActionPointList","").

   NetworkActionPointBrowseFrame:insertHiddenField("popup_networkactionpointhistory_browse","").
   NetworkActionPointBrowseFrame:insertHiddenField("popup_networkactioncommandlink_browse","").
   NetworkActionPointBrowseFrame:insertHiddenField("popup_networkactioncommandlinkhistory_browse","").
   
   NetworkActionPointBrowseFrame:insertHiddenField("form_name", "networkactionpoint_browse_form").
   NetworkActionPointBrowseFrame:insertHiddenField("prog_name", "adNetworkActionPointAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointBrowseFrame}
   
   NetworkActionPointBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkActionPointBrowseButtons = NEW buttonBar().
   NetworkActionPointBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   IF NOT logPreventDataCreates THEN
   DO:
      NetworkActionPointBrowseButtons:addButton("networkactionpoint_browse_form_btn_create",
                                                fTL("Create"),
                                                "createNetworkActionPoint('networkactionpoint_details_form');",
                                                "").
   END.
   
   NetworkActionPointBrowseButtons:addButton("networkactionpoint_browse_form_btn_details",
                                              fTL("Details"),
                                              "viewNetworkActionPointDetails('networkactionpoint_details_form');",
                                              (IF intNetworkActionPoint > 0 THEN "" ELSE "Disabled")).
                                 
   NetworkActionPointBrowseButtons:addButton("networkactionpoint_browse_form_btn_command",
                                              fTL("Commands"),
                                              "viewNetworkActionCommandLinkBrowse('networkactionpoint_command_browse_form');",
                                              "Disabled").    
                                 
   NetworkActionPointBrowseButtons:addButton("networkactionpoint_browse_form_btn_history",
                                              fTL("History"),
                                              "viewNetworkActionPointHistory();",
                                              (IF intNetworkActionPoint  > 0 THEN "" ELSE "Disabled")).                                                                    
                                 
   NetworkActionPointBrowseButtons:addButton("networkactionpoint_browse_form_btn_send",  
                                              fTL("Send Data"),
                                              "viewSendDataForm('networkactionsenddata_browse_form');",
                                              (IF intNetworkActionPoint > 0 THEN "" ELSE "Disabled")). 
                                              
   NetworkActionPointBrowseButtons:addButton("networkactionpoint_browse_form_btn_startServer",
                                              fTL("Start Server"),
                                              "sendStartProcessToServer('networkactionpoint_browse_form');",
                                              (IF intNetworkActionPoint > 0 THEN "" ELSE "Disabled")).            
                                 
   NetworkActionPointBrowseButtons:addButton("networkactionpoint_browse_form_btn_stopServer",
                                              fTL("Stop Server"),
                                              "sendStopProcessToServer('networkactionpoint_browse_form');",
                                              (IF intNetworkActionPoint > 0 THEN "" ELSE "Disabled")).       
   
   NetworkActionPointBrowseButtons:closeBar().  
   NetworkActionPointBrowseButtons:displayButtonBar().  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointDetails Procedure 
PROCEDURE pNetworkActionPointDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkactionpoint_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkActionPointID,NetworkActionPointCode,NetworkActionPointName,NetworkActionPointTypeID"
                               + ",IPAddress,PortNumber,ServerIPAddress,ServerPortNumber,Register,ToteLineID,PrinterID,Active"
                               + ",IsOperating,StartOperating,ProcessProgramID,CanConnect,TriggerInterval,DirectoryToRunFrom"
          chrEditFieldList     = ",NetworkActionPointName,NetworkActionPointTypeID,IPAddress,PortNumber,ToteLineID,PrinterID"
                               + ",ServerIPAddress,ServerPortNumber,Active,Register,ProcessProgramID,CanConnect"
                               + ",TriggerInterval,DirectoryToRunFrom"
          chrNewFieldList      = "NetworkActionPointCode,NetworkActionPointName,NetworkActionPointTypeID,IPAddress,PortNumber,"
                               + "ServerIPAddress,ServerPortNumber,Register,ToteLineID,PrinterID,Active,IsOperating,StartOperating"
                               + ",CanConnect,TriggerInterval,DirectoryToRunFrom"
          chrRequiredFieldList = "NetworkActionPointCode,NetworkActionPointName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkActionPointDetailsForm = NEW dataForm("networkactionpoint_details_form").
   NetworkActionPointDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkActionPointDetailsForm:FormAction  = "dbNetworkActionPointUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkActionPointDetailsForm:FormWidth   = 700.
   NetworkActionPointDetailsForm:FormHeight  = 490.
   NetworkActionPointDetailsForm:FormTitle   = "Network Action Point Details".
   NetworkActionPointDetailsForm:FormType    = "xl_large".
   
   /* Column Layout */
   NetworkActionPointDetailsForm:insertPaddingColumn(50).
   NetworkActionPointDetailsForm:insertColumn(130).
   NetworkActionPointDetailsForm:insertColumn(120).
   NetworkActionPointDetailsForm:insertColumn(20).
   NetworkActionPointDetailsForm:insertColumn(4).
   NetworkActionPointDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("ActionPoint ID").
   NetworkActionPointDetailsForm:insertTextField("NetworkActionPointID", "", 200, TRUE).  
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("ActionPoint Code").
   NetworkActionPointDetailsForm:insertTextField("NetworkActionPointCode", "", 200, TRUE).  
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("ActionPoint Name").
   NetworkActionPointDetailsForm:insertTextField("NetworkActionPointName", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("ActionPoint Type").
   NetworkActionPointDetailsForm:insertComboField("NetworkActionPointTypeID", "", 200, TRUE).
   NetworkActionPointDetailsForm:insertComboPairs("NetworkActionPointTypeID", "0", "None Selected..."). 
   FIND FIRST NetworkActionPointType OF NetworkActionPoint NO-LOCK NO-ERROR.
   
   FOR EACH NetworkActionPointType NO-LOCK /*idx=NetworkActionPointTypeID*/
      WHERE NetworkActionPointType.Active:
      
      NetworkActionPointDetailsForm:insertComboPairs("NetworkActionPointTypeID", STRING(NetworkActionPointType.NetworkActionPointTypeID), NetworkActionPointType.TypeName). 
   
   END.   
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("IP Address").
   NetworkActionPointDetailsForm:insertTextField("IPAddress", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Port Number").
   NetworkActionPointDetailsForm:insertTextField("PortNumber", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Server Address").
   NetworkActionPointDetailsForm:insertTextField("ServerIPAddress", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Server Port").
   NetworkActionPointDetailsForm:insertTextField("ServerPortNumber", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Register").
   NetworkActionPointDetailsForm:insertTextField("Register", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Process Program").
   NetworkActionPointDetailsForm:insertComboField("ProcessProgramID", "", 200, TRUE).
   NetworkActionPointDetailsForm:insertComboPairs("ProcessProgramID", "0", "None Selected...").
   FIND FIRST ProcessProgram OF NetworkActionPoint NO-LOCK NO-ERROR.
   
   FOR EACH ProcessProgram NO-LOCK
      BY ProcessProgram.ProgramName: /*idx=ToteLineID*/
      
      NetworkActionPointDetailsForm:insertComboPairs("ProcessProgramID", STRING(ProcessProgram.ProcessProgramID), ProcessProgram.ProgramName). 
   END.
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Tote Line").
   NetworkActionPointDetailsForm:insertComboField("ToteLineID", "", 200, TRUE).
   NetworkActionPointDetailsForm:insertComboPairs("ToteLineID", "0", "None Selected...").
   FIND FIRST ToteLine OF NetworkActionPoint NO-LOCK NO-ERROR.
   
   FOR EACH ToteLine NO-LOCK /*idx=ToteLineID*/
      WHERE   ToteLine.Active:
      
      NetworkActionPointDetailsForm:insertComboPairs("ToteLineID", STRING(ToteLine.ToteLineID), ToteLine.LineName). 
   END. 
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Printer").
   NetworkActionPointDetailsForm:insertComboField("PrinterID", "", 200, TRUE).
   NetworkActionPointDetailsForm:insertComboPairs("PrinterID", "0", "None Selected...").
   FOR EACH Printer NO-LOCK /*idx=PrinterID*/
      WHERE Printer.Active:
      
      NetworkActionPointDetailsForm:insertComboPairs("PrinterID", STRING(Printer.PrinterID), Printer.PrinterName). 
   END.                                                                                                                               
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel(fTL("Start Operating")). 
   NetworkActionPointDetailsForm:insertComboField("StartOperating", "", 200, TRUE).  
   NetworkActionPointDetailsForm:insertComboPairs("StartOperating", "yes", "Yes").
   NetworkActionPointDetailsForm:insertComboPairs("StartOperating", "no",  "No").
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel(fTL("Is Operating")). 
   NetworkActionPointDetailsForm:insertComboField("IsOperating", "", 200, TRUE).  
   NetworkActionPointDetailsForm:insertComboPairs("IsOperating", "yes", "Yes").
   NetworkActionPointDetailsForm:insertComboPairs("IsOperating", "no",  "No").
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel(fTL("Can Connect")). 
   NetworkActionPointDetailsForm:insertComboField("CanConnect", "", 200, TRUE).  
   NetworkActionPointDetailsForm:insertComboPairs("CanConnect", "yes", "Yes").
   NetworkActionPointDetailsForm:insertComboPairs("CanConnect", "no",  "No").
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Trigger Interval").
   NetworkActionPointDetailsForm:insertTextField("TriggerInterval", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel("Directory To Run From").
   NetworkActionPointDetailsForm:insertTextField("DirectoryToRunFrom", "", 200, TRUE).
   
   NetworkActionPointDetailsForm:startRow().
   NetworkActionPointDetailsForm:insertLabel(fTL("Active")). 
   NetworkActionPointDetailsForm:insertComboField("Active", "", 200, TRUE).  
   NetworkActionPointDetailsForm:insertComboPairs("Active", "yes", "Active").
   NetworkActionPointDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pNetworkActionPointDetailsFields}
   
   /* Add Hidden Fields*/
   NetworkActionPointDetailsForm:insertHiddenField("networkactionpoint_browse_scroll", "").
   NetworkActionPointDetailsForm:insertHiddenField("form_name", "networkactionpoint_details_form").
   NetworkActionPointDetailsForm:insertHiddenField("prog_name", "adNetworkActionPointAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointDetailsForm}
   
   /* Create Button Bar */
   NetworkActionPointDetailsButtons = NEW buttonBar().
   
   NetworkActionPointDetailsButtons:addButton("networkactionpoint_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateNetworkActionPoint('networkactionpoint_details_form');").
   
   NetworkActionPointDetailsButtons:addButton("networkactionpoint_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('networkactionpoint_details_form_popup');").
   
   NetworkActionPointDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionPointDetailsForm:FormButtons = NetworkActionPointDetailsButtons.
   
   NetworkActionPointDetailsForm:endForm(). 
   
   NetworkActionPointDetailsForm:displayForm().       
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointDetailsFields Procedure 
PROCEDURE pNetworkActionPointDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         NetworkActionPointDetailsForm:startRow().
         NetworkActionPointDetailsForm:insertLabel(fTL("Field Label")).
         NetworkActionPointDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointHistory Procedure 
PROCEDURE pNetworkActionPointHistory :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   NetworkActionPointHistoryBrowseForm           = NEW dataForm("networkactionpointhistory_browse_form").
   NetworkActionPointHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkActionPointHistoryBrowseForm:FormWidth  = 860.
   NetworkActionPointHistoryBrowseForm:FormHeight = 530.
   NetworkActionPointHistoryBrowseForm:FormTitle  = fTL("Network Action Point History").
   NetworkActionPointHistoryBrowseForm:FormType   = "xxl_large".
   NetworkActionPointHistoryBrowse                = NEW browseTable("networkactionpointhistory_browse").
   NetworkActionPointHistoryBrowse:BrowseWidth    = 840.
   NetworkActionPointHistoryBrowse:BrowseHeight   = 490.
   
   NetworkActionPointHistoryBrowse:insertColumn(fTL("History ID"),       80, "INTEGER",           FALSE).
   NetworkActionPointHistoryBrowse:insertColumn(fTL("ActionPoint Code"), 120, "CHARACTER", "LEFT", FALSE).
   NetworkActionPointHistoryBrowse:insertColumn(fTL("ActionPoint Name"), 120, "CHARACTER", "LEFT", FALSE).
   NetworkActionPointHistoryBrowse:insertColumn(fTL("ActionPoint Type"), 120, "CHARACTER", "LEFT", FALSE).
   NetworkActionPointHistoryBrowse:insertColumn(fTL("Active"),            70, "LOGICAL",           FALSE).
   NetworkActionPointHistoryBrowse:insertColumn(fTL("User"),             150, "CHARACTER", "LEFT", FALSE).
   NetworkActionPointHistoryBrowse:insertColumn(fTL("Created"),          150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkActionPointHistory}
   
   NetworkActionPointHistoryBrowse:StartBody().
   
   FOR EACH NetworkActionPointHistory NO-LOCK /*idx=NetworkActionPointID*/
      WHERE NetworkActionPointHistory.NetworkActionPointID = intNetworkActionPoint
      BY    NetworkActionPointHistory.Created DESC:
          
      FIND FIRST GateUser OF NetworkActionPointHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/
      FIND FIRST ToteLine OF NetworkActionPointHistory NO-LOCK NO-ERROR. /*idx=ToteLineID*/
      FIND FIRST NetworkActionPointType OF NetworkActionPointHistory NO-LOCK NO-ERROR. /*idx=NetworkActionPointTypeID*/

      NetworkActionPointHistoryBrowse:startRow (NetworkActionPointHistory.NetworkActionPointHistoryID, 
         "selectNetworkActionPointHistoryRow(this," + '"' + STRING(NetworkActionPointHistory.NetworkActionPointHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i NetworkActionPointHistory}
      
      NetworkActionPointHistoryBrowse:insertData(NetworkActionPointHistory.NetworkActionPointHistoryID).
      NetworkActionPointHistoryBrowse:insertData(NetworkActionPointHistory.NetworkActionPointCode,"LEFT").
      NetworkActionPointHistoryBrowse:insertData(NetworkActionPointHistory.NetworkActionPointName,"LEFT").
      NetworkActionPointHistoryBrowse:insertData((IF AVAILABLE NetworkActionPointType THEN NetworkActionPointType.TypeName ELSE ""),"LEFT").
      NetworkActionPointHistoryBrowse:insertData((IF AVAILABLE NetworkActionPoint THEN STRING(NetworkActionPoint.Active,"Yes/No") ELSE "")).
      NetworkActionPointHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      NetworkActionPointHistoryBrowse:insertData(fDisplayDate&Time(NetworkActionPointHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      NetworkActionPointHistoryBrowse:endRow().
   END. /* FOR EACH PickPackStationHistory */
   
   NetworkActionPointHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkActionPointHistoryBrowse:getErrors().
   
   NetworkActionPointHistoryBrowseForm:insertHiddenField("popup_networkactionpointhistory_browse","").
   NetworkActionPointHistoryBrowseForm:insertHiddenField("NetworkActionPointHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointHistoryBrowseForm}
   
   /* Create Button Bar */
   NetworkActionPointHistoryBrowseButtons = NEW buttonBar().
   
   NetworkActionPointHistoryBrowseButtons:addButton("networkactionpointhistory_browse_form_btn_details",
                                                    fTL("Details"),
                                                    "viewNetworkActionPointHistoryDetails('networkactionpointhistory_details_form');",
                                                    (IF intSelectedNetworkActionPointHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   NetworkActionPointHistoryBrowseButtons:addButton("networkactionpointhistory_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('networkactionpointhistory_browse_form_popup');").
                                                    
   NetworkActionPointHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionPointHistoryBrowseForm:FormBrowse  = NetworkActionPointHistoryBrowse.
   NetworkActionPointHistoryBrowseForm:FormButtons = NetworkActionPointHistoryBrowseButtons.
   
   NetworkActionPointHistoryBrowseForm:endForm(). 
   
   NetworkActionPointHistoryBrowseForm:displayForm().  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkActionPointHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkActionPointHistoryDetails Procedure 
PROCEDURE pNetworkActionPointHistoryDetails :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

 /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkactionpointhistory_details_form"}
   ASSIGN chrDisplayFieldList  = "NetworkActionPointHistoryID,NetworkActionPointCode,NetworkActionPointName"
                               + ",NetworkActionPointTypeID,IPAddress,PortNumber,ToteLineID,PrinterID,Active,GateUserID"
                               + ",CreatedDate,CreatedHour,CreatedMins,IsOperating,StartOperating,ServerIPAddress"
                               + ",ServerPortNumber,Register,IsOperating,StartOperating,ProcessProgramID,CanConnect"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkActionPointHistoryDetailsForm = NEW dataForm("networkactionpointhistory_details_form").
   NetworkActionPointHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkActionPointHistoryDetailsForm:FormAction = "dbNetworkActionPointUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkActionPointHistoryDetailsForm:FormWidth   = 580.
   NetworkActionPointHistoryDetailsForm:FormHeight  = 420.
   NetworkActionPointHistoryDetailsForm:FormTitle   = "Network Action Point History Details".
   NetworkActionPointHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   NetworkActionPointHistoryDetailsForm:insertPaddingColumn(50).
   NetworkActionPointHistoryDetailsForm:insertColumn(110).
   NetworkActionPointHistoryDetailsForm:insertColumn(120).
   NetworkActionPointHistoryDetailsForm:insertColumn(20).
   NetworkActionPointHistoryDetailsForm:insertColumn(4).
   NetworkActionPointHistoryDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("History ID")).
   NetworkActionPointHistoryDetailsForm:insertTextField("NetworkActionPointHistoryID", "", 200, TRUE).  
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("ActionPoint Code")).
   NetworkActionPointHistoryDetailsForm:insertTextField("NetworkActionPointCode", "", 200, TRUE).
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("ActionPoint Name")).
   NetworkActionPointHistoryDetailsForm:insertTextField("NetworkActionPointName", "", 200, TRUE).
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("ActionPoint Type")).
   NetworkActionPointHistoryDetailsForm:insertComboField("NetworkActionPointTypeID", "", 200, TRUE).
   NetworkActionPointHistoryDetailsForm:insertComboPairs("NetworkActionPointTypeID", "0", "None Selected..."). 
   FOR EACH NetworkActionPointType NO-LOCK /*idx=LocationTypeActive*/
      WHERE NetworkActionPointType.Active:
      
      NetworkActionPointHistoryDetailsForm:insertComboPairs("NetworkActionPointTypeID", STRING(NetworkActionPointType.NetworkActionPointTypeID), NetworkActionPointType.TypeName). 
   END. 
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Tote Line")).
   NetworkActionPointHistoryDetailsForm:insertComboField("ToteLineID", "", 200, TRUE).   
   NetworkActionPointHistoryDetailsForm:insertComboPairs("ToteLineID", "0", "None Selected...").  
   FOR EACH ToteLine NO-LOCK /*idx=LocationTypeActive*/
      WHERE ToteLine.Active:
      
      NetworkActionPointHistoryDetailsForm:insertComboPairs("ToteLineID", STRING(ToteLine.ToteLineID), ToteLine.LineName). 
      
   END. 
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("IP Address")).
   NetworkActionPointHistoryDetailsForm:insertTextField("IPAddress", "", 200, TRUE).
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel("Server IP Address").
   NetworkActionPointHistoryDetailsForm:insertTextField("ServerIPAddress", "", 200, TRUE).
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel("Server Port Num").
   NetworkActionPointHistoryDetailsForm:insertTextField("ServerPortNumber", "", 200, TRUE).
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel("Register").
   NetworkActionPointHistoryDetailsForm:insertTextField("Register", "", 200, TRUE).
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Port Number")).
   NetworkActionPointHistoryDetailsForm:insertTextField("PortNumber", "", 200, TRUE).  
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel("Process Program").
   NetworkActionPointHistoryDetailsForm:insertComboField("ProcessProgramID", "", 200, TRUE).
   NetworkActionPointHistoryDetailsForm:insertComboPairs("ProcessProgramID", "0", "None Selected...").
   FIND FIRST ProcessProgram OF NetworkActionPoint NO-LOCK NO-ERROR.
   
   FOR EACH ProcessProgram NO-LOCK
      BY ProcessProgram.ProgramName: /*idx=ToteLineID*/
      
      NetworkActionPointHistoryDetailsForm:insertComboPairs("ProcessProgramID", STRING(ProcessProgram.ProcessProgramID), ProcessProgram.ProgramName). 
   
   END.

   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Printer")).
   NetworkActionPointHistoryDetailsForm:insertComboField("PrinterID", "", 200, TRUE).  
   NetworkActionPointHistoryDetailsForm:insertComboPairs("PrinterID", "0", "None Selected...").  
   FOR EACH Printer NO-LOCK /*idx=LocationTypeActive*/
      WHERE Printer.Active:
                                                                                                         
      NetworkActionPointHistoryDetailsForm:insertComboPairs("PrinterID", STRING(Printer.PrinterID), Printer.PrinterName).
  
   END. /*FOR EACH PickPackType NO-LOCK*/
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Start Operating")). 
   NetworkActionPointHistoryDetailsForm:insertComboField("StartOperating", "", 200, TRUE).  
   NetworkActionPointHistoryDetailsForm:insertComboPairs("StartOperating", "yes", "Yes").
   NetworkActionPointHistoryDetailsForm:insertComboPairs("StartOperating", "no",  "No").
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Is Operating")). 
   NetworkActionPointHistoryDetailsForm:insertComboField("IsOperating", "", 200, TRUE).  
   NetworkActionPointHistoryDetailsForm:insertComboPairs("IsOperating", "yes", "Yes").
   NetworkActionPointHistoryDetailsForm:insertComboPairs("IsOperating", "no",  "No").
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Can Connect")). 
   NetworkActionPointHistoryDetailsForm:insertComboField("CanConnect", "", 200, TRUE).  
   NetworkActionPointHistoryDetailsForm:insertComboPairs("CanConnect", "yes", "Yes").
   NetworkActionPointHistoryDetailsForm:insertComboPairs("CanConnect", "no",  "No").
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Active")).
   NetworkActionPointHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   NetworkActionPointHistoryDetailsForm:insertComboPairs("Active", "yes", "Yes").
   NetworkActionPointHistoryDetailsForm:insertComboPairs("Active", "no", "No").
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Gate User ID")).
   NetworkActionPointHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE). 
   FOR EACH GateUser NO-LOCK /*idx=GateUserID*/
      BY GateUser.FullName:
         
      NetworkActionPointHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
      
   END.
   
   NetworkActionPointHistoryDetailsForm:startRow().
   NetworkActionPointHistoryDetailsForm:insertLabel(fTL("Created")).
   NetworkActionPointHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).  
   /* Time fields have no label */
   NetworkActionPointHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkActionPointHistoryDetailsForm:insertLabel(":").
   NetworkActionPointHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE). 

   {webGetOptionalFormFields.i pNetworkActionPointHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   NetworkActionPointHistoryDetailsForm:insertHiddenField("networkactionpointhistory_browse_scroll", "").
   NetworkActionPointHistoryDetailsForm:insertHiddenField("form_name", "networkactionpointhistory_details_form").
   NetworkActionPointHistoryDetailsForm:insertHiddenField("prog_name", "adNetworkActionPointAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionPointHistoryDetailsForm}
   
   /* Create Button Bar */
   NetworkActionPointHistoryDetailsButtons = NEW buttonBar().

   NetworkActionPointHistoryDetailsButtons:addButton("networkactionpointhistory_details_form_btn_cancel", 
                                                     fTL("Cancel"), 
                                                     "disablePopup('networkactionpointhistory_details_form_popup');").
   NetworkActionPointHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionPointHistoryDetailsForm:FormButtons =  NetworkActionPointHistoryDetailsButtons.
   
   NetworkActionPointHistoryDetailsForm:endForm(). 
   
   NetworkActionPointHistoryDetailsForm:displayForm(). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkSendData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkSendData Procedure 
PROCEDURE pNetworkSendData :
/*------------------------------------------------------------------------------
 Purpose: send plc value to selected network
 Notes: 
------------------------------------------------------------------------------*/                              
   NetworkActionSendDataForm           = NEW dataForm("networkactionsenddata_browse_form").
   NetworkActionSendDataForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */   
   NetworkActionSendDataForm:FormWidth  = 350.
   NetworkActionSendDataForm:FormHeight = 200.
   NetworkActionSendDataForm:FormTitle  = fTL("Data to be send").
   NetworkActionSendDataForm:FormType   = "small_wide".
     
   /* Colummn style */
   NetworkActionSendDataForm:insertPaddingColumn(30).
   NetworkActionSendDataForm:insertColumn(80).
   NetworkActionSendDataForm:insertColumn(80).
   NetworkActionSendDataForm:insertColumn(80).
       
   /* Fields */  
   NetworkActionSendDataForm:startRow().
   NetworkActionSendDataForm:insertLabel(fTL("Value")).
   NetworkActionSendDataForm:insertTextField("NetworkValue", "", 120, TRUE).
   
   NetworkActionSendDataForm:insertHiddenField("form_name","networkactionsenddata_browse_form").
   NetworkActionSendDataForm:insertHiddenField("prog_name","adNetworkActionPointAdmin.p").  
   NetworkActionSendDataForm:insertHiddenField("NetworkActionPointID", chrNetworkActionPointID).
   NetworkActionSendDataForm:insertHiddenField("NetworkActionPointIDList", chrNetworkActionPointList).
   NetworkActionSendDataForm:insertHiddenField("NetworkValue", "").
   NetworkActionSendDataForm:insertHiddenField("process_mode", "send").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkActionSendDataForm}   
   /* Create Button Bar */
   NetworkActionSendDataFormButtons = NEW buttonBar().
   
   NetworkActionSendDataFormButtons:addButton("networkactionsenddata_browse_form_btn_send",
                                              fTL("Send"),
                                              "sendData('networkactionsenddata_browse_form');").    
                                         
   NetworkActionSendDataFormButtons:addButton("networkactionsenddata_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('networkactionsenddata_browse_form_popup');").
                                              
   NetworkActionSendDataFormButtons:closeBar(). 
     
   /* Assign the Button Bar Object to the Form Object */
   NetworkActionSendDataForm:FormButtons = NetworkActionSendDataFormButtons.

   NetworkActionSendDataForm:FormAction="dbNetworkActionPointUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   NetworkActionSendDataForm:endForm(). 
   
   NetworkActionSendDataForm:displayForm().  

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

   ASSIGN chrNetworkActionPointID                 = get-value("NetworkActionPointID")          
          chrNetworkActionPointList               = get-value("NetworkActionPointList")
          intNetworkActionPoint                   = INTEGER(chrNetworkActionPointList)
          chrActiveServers                        = get-value("ActiveServer")
          chrInActiveServers                      = get-value("InActiveServer")
          chrStopProcessRan                       = get-value("StopWasRun")
          chrStartProcessRan                      = get-value("StartWasRun")
          chrSendProcessRan                       = get-value("SendWasRun")
          chrDataWasSent                          = get-value("DataWasSent")
          chrResponseFromDb                       = get-value("ProcessResponse")
          chrScrollToNetworkActionPointRow        = STRING(INTEGER(get-value("networkactionpoint_browse_scroll"))) + ";"
          chrNetworkActionPointHistoryID          = get-value("NetworkActionPointHistoryID")
          intSelectedNetworkActionPointHistory    = INTEGER(chrNetworkActionPointHistoryID)
          chrScrollToNetworkActionPointHistoryRow = STRING(INTEGER(get-value("networkactionpointhistory_browse_scroll"))) + ";"
          chrSelectNetworkActionCommandLinkID     = get-value("NetworkActionCommandLinkID")
          intSelectNetworkActionCommandLinkID     = INTEGER(chrSelectNetworkActionCommandLinkID).         

   IF NUM-ENTRIES(chrNetworkActionPointList) = 1 THEN
   DO:
      chrNetworkActionPointID = chrNetworkActionPointList.
      intNetworkActionPoint = INTEGER(chrNetworkActionPointList).
   END.
   
   /* Process URL values */
   ProcessURLloop:
   DO:
      IF chrNetworkActionPointID <> "" AND chrStopProcessRan <> "TRUE" AND chrStartProcessRan <> "TRUE" THEN
         chrSelectNetworkActionPointRow = 'selectMultipleNetworkActionPointRows("' + chrNetworkActionPointID + '");'.

      IF chrNetworkActionPointList <> "" AND chrStopProcessRan <> "TRUE" AND chrStartProcessRan <> "TRUE" THEN
         chrSelectNetworkActionPointRow = 'selectMultipleNetworkActionPointRows("' + chrNetworkActionPointList + '");'.   
      
      IF get-value('popup_networkactionpointhistory_browse') = "Yes" THEN
      DO:
         chrPopupHistory = 'enablePopup("networkactionpointhistory_browse_form_popup");'.
         LEAVE ProcessURLloop.  /* form pop ups do not intefere with url processing popups */
      END.
       
      IF get-value('popup_networkactioncommandlink_browse') = "Yes" THEN
      DO:
         chrPopupCommandBrowse = 'enablePopup("networkactioncommandlink_browse_form_popup");'.
         LEAVE ProcessURLloop.
      END.
      
      IF get-value('popup_networkactioncommandlinkhistory_browse') = "Yes" THEN
      DO:
         chrPopupCommandLinkHistoryBrowse = 'enablePopup("networkactioncommandlinkhistory_browse_form_popup");'. 
         LEAVE ProcessURLloop.
      END.
      
      IF chrStopProcessRan = "True" THEN
      DO:
         chrPopupStopServer = 'checkServerStatusAfterStop("' + chrActiveServers + '","' + chrInActiveServers + '","' + chrResponseFromDb + '");'.     
         intNetworkActionPoint = 0.
      END.
      
      IF chrStartProcessRan = "True" THEN
      DO:
         chrPopupStartServer = 'checkServerStatusAfterStart("' + chrActiveServers + '","' + chrInActiveServers + '","' + chrResponseFromDb + '");'.
         intNetworkActionPoint = 0.
      END.
      IF chrSendProcessRan = "True" THEN
      DO:
         FIND NetworkActionPoint NO-LOCK
            WHERE NetworkActionPoint.NetworkActionPointID = INTEGER(chrNetworkActionPointID) NO-ERROR.
         IF AVAILABLE NetworkActionPoint THEN 
            chrPopupSendData = 'sendDataAlert("' + NetworkActionPoint.NetworkActionPointCode + '","' + chrDataWasSent + '");'.  
      END.
                                  
      IF chrSelectNetworkActionCommandLinkID <> "" THEN
        chrSelectNetworkActionCommandLinkRow = 'selectNetworkActionCommandLinkRow(document.getElementById("networkactioncommandlink_browse_row_' 
                                                + chrSelectNetworkActionCommandLinkID + '"),"' + chrSelectNetworkActionCommandLinkID +  '");'.
                                                                                     
   END. /* Process URL values */                                              
 
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkactionpoint_browse").scrollTop=' + chrScrollToNetworkActionPointRow 
                             + chrSelectNetworkActionPointRow + chrPopupHistory
                             + chrPopupCommandBrowse + chrSelectNetworkActionCommandLinkRow
                             + chrPopupCommandLinkHistoryBrowse + chrPopupStartServer  + chrPopupStopServer
                             + chrPopupSendData .

   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Network Device Action Point Admin".
   ThisPage:FrameTitle = "Network Device Action Point Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
 
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File specifically for Network ActionPoints */
   ThisPage:addJavaScript("networkdeviceactionpoints.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkActionPointBrowse.

   /******* Popup Browsers and Forms ********/    
   RUN pNetworkActionPointDetails.
   RUN pNetworkActionCommandLinkBrowse.
   RUN pNetworkActionCommandLinkDetails.
   RUN pNetworkActionCommandLinkHistory.
   RUN pNetworkActionPointHistory.
   RUN pNetworkActionPointHistoryDetails.
   RUN pNetworkSendData.

   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT NetworkActionPointBrowseFrame               NO-ERROR.
   DELETE OBJECT NetworkActionPointBrowse                    NO-ERROR.
   DELETE OBJECT NetworkActionPointBrowseButtons             NO-ERROR.
   DELETE OBJECT NetworkActionPointDetailsForm               NO-ERROR.
   DELETE OBJECT NetworkActionPointDetailsButtons            NO-ERROR.
   DELETE OBJECT NetworkActionSendDataForm                   NO-ERROR.
   DELETE OBJECT NetworkActionSendDataFormButtons            NO-ERROR.
   DELETE OBJECT NetworkActionPointHistoryBrowseForm         NO-ERROR.
   DELETE OBJECT NetworkActionPointHistoryBrowse             NO-ERROR.
   DELETE OBJECT NetworkActionPointHistoryBrowseButtons      NO-ERROR.
   DELETE OBJECT NetworkActionPointHistoryDetailsForm        NO-ERROR.
   DELETE OBJECT NetworkActionPointHistoryDetailsButtons     NO-ERROR.   
   DELETE OBJECT NetworkActionCommandLinkBrowseForm          NO-ERROR.
   DELETE OBJECT NetworkActionCommandLinkBrowse              NO-ERROR.
   DELETE OBJECT NetworkActionCommandLinkBrowseButtons       NO-ERROR.
   DELETE OBJECT NetworkActionCommandLinkDetailsForm         NO-ERROR.
   DELETE OBJECT NetworkActionCommandLinkDetailsButtons      NO-ERROR.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

