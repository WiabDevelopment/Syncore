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
     30/10/2015 - twierzch - Sort History browse descending, add IsListening to Reader Browse and history

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
DEFINE VARIABLE intNetworkReader                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intNetworkReaderHistory            AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectNetworkReaderRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkReaderRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkReaderID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkReaderList               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrNetworkReaderHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedNetworkReaderHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrNetworkReaderCode               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToNetworkReaderHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logPreventDataCreates              AS LOGICAL     NO-UNDO.
DEFINE VARIABLE chrPopupHistory                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupStopServer                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrStopProcessRan                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupStartServer                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrStartProcessRan                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupSendData                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSendProcessRan                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrActiveServers                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrInActiveServers                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrDataWasSent                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrResponseFromDb                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupResponseFromDb             AS CHARACTER   NO-UNDO.


/* Objects */
DEFINE VARIABLE NetworkReaderBrowseFrame           AS pageFrame.
DEFINE VARIABLE NetworkReaderBrowse                AS browseTable.
DEFINE VARIABLE NetworkReaderBrowseButtons         AS buttonBar.
DEFINE VARIABLE NetworkReaderDetailsForm           AS dataForm.
DEFINE VARIABLE NetworkReaderDetailsButtons        AS buttonBar.

DEFINE VARIABLE NetworkReaderHistoryBrowseForm     AS dataForm.
DEFINE VARIABLE NetworkReaderHistoryBrowse         AS browseTable.
DEFINE VARIABLE NetworkReaderHistoryBrowseButtons  AS buttonBar.
DEFINE VARIABLE NetworkReaderHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE NetworkReaderHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE NetworkReaderSendDataForm          AS dataForm.
DEFINE VARIABLE NetworkReaderSendDataFormButtons   AS buttonBar.

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

&IF DEFINED(EXCLUDE-pNetworkReaderBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderBrowse Procedure 
PROCEDURE pNetworkReaderBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "networkreader_details_form"}
   
   NetworkReaderBrowse = NEW browseTable("networkreader_browse").
   NetworkReaderBrowse:BrowseWidth  = 965.
   NetworkReaderBrowse:BrowseHeight = 455.
   NetworkReaderBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   NetworkReaderBrowse:insertColumn(fTL("ID"), 85, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkReader}
   
   NetworkReaderBrowse:insertColumn(fTL("Reader Code"), 150, "CHARACTER", "LEFT", FALSE).
   NetworkReaderBrowse:insertColumn(fTL("Reader Name"), 150, "CHARACTER", "LEFT", FALSE).
   NetworkReaderBrowse:insertColumn(fTL("Reader Type"), 150, "INTEGER",   "LEFT", FALSE).
   NetworkReaderBrowse:insertColumn(fTL("ToteLine"),    150, "INTEGER",   "LEFT", FALSE).
   NetworkReaderBrowse:insertColumn(fTL("IsListening"), 100, "LOGICAL",           FALSE).
   NetworkReaderBrowse:insertColumn(fTL("Active"),       70, "LOGICAL",           FALSE).
   
   /* Body */
   NetworkReaderBrowse:startBody().
   
   FOR EACH NetworkReader NO-LOCK /*idx=ActiveVendorName*/
      BY    NetworkReader.Active DESCENDING
      BY    NetworkReader.NetworkReaderID:
      
      FIND FIRST NetworkReaderType OF NetworkReader NO-LOCK NO-ERROR. /* idx=NetworkReaderTypeID */
      FIND FIRST PickPackStation OF NetworkReader NO-LOCK NO-ERROR. /* idx=PickPackStationID */
      FIND FIRST ToteLine OF NetworkReader NO-LOCK NO-ERROR. /* idx=ToteLineID */
      FIND FIRST ToteLineCell OF NetworkReader NO-LOCK NO-ERROR. /* idx=ToteLineCellID */
      FIND FIRST NetworkActionPoint NO-LOCK /* idx=NetworkActionPointID */
         WHERE NetworkActionPoint.NetworkActionPointID = NetworkReader.TargetActionPointID NO-ERROR.         

      NetworkReaderBrowse:startRow(NetworkReader.NetworkReaderID, 
         'multiSelectNetworkReaderRow("' + STRING(NetworkReader.NetworkReaderID) + '","' + STRING(NetworkReader.VersionID) + '")', "").
         
      NetworkReaderBrowse:insertData(NetworkReader.NetworkReaderID , "LEFT").
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i NetworkReader}
      
      NetworkReaderBrowse:insertData((IF AVAILABLE NetworkReader THEN NetworkReader.NetworkReaderCode ELSE ""), "LEFT").
      NetworkReaderBrowse:insertData((IF AVAILABLE NetworkReader THEN NetworkReader.NetworkReaderName ELSE ""), "LEFT").
      NetworkReaderBrowse:insertData((IF AVAILABLE NetworkReaderType THEN NetworkReaderType.TypeName ELSE ""), "LEFT").
      NetworkReaderBrowse:insertData((IF AVAILABLE ToteLine THEN ToteLine.LineName ELSE ""), "LEFT").
      NetworkReaderBrowse:insertData((IF AVAILABLE NetworkReader THEN STRING(NetworkReader.IsListening,"Yes/No") ELSE "")).
      NetworkReaderBrowse:insertData((IF AVAILABLE NetworkReader THEN STRING(NetworkReader.Active,"Yes/No") ELSE "")).
      /* Add hidden fields */
      NetworkReaderBrowse:insertHiddenData("NetworkReaderVersionID",NetworkReader.VersionID).
      
      NetworkReaderBrowse:endRow().
      
   END. /*FOR EACH NetworkReader NO-LOCK */

   NetworkReaderBrowse:endTable().
   
   /* Create a new frame */
   NetworkReaderBrowseFrame = NEW pageFrame().
   NetworkReaderBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */

   NetworkReaderBrowseFrame:FormAction="dbNetworkReaderUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   NetworkReaderBrowseFrame:formOpen("networkreader_browse_form").
   
   /* Start the Frame Header */
   NetworkReaderBrowseFrame:insertSpacer(5).
   NetworkReaderBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   NetworkReaderBrowse:displayBrowse().  
   
   /* End the Frame Header */
   NetworkReaderBrowseFrame:frameClose().
   NetworkReaderBrowseFrame:insertSpacer(10).   

   NetworkReaderBrowseFrame:insertHiddenField("networkreader_browse_scroll","").
   NetworkReaderBrowseFrame:insertHiddenField("NetworkReaderID", chrNetworkReaderID).
   NetworkReaderBrowseFrame:insertHiddenField("NetworkReaderVersionID","").    
   NetworkReaderBrowseFrame:insertHiddenField("NetworkReaderList","").
     
   NetworkReaderBrowseFrame:insertHiddenField("popup_networkreaderhistory_browse","").

   NetworkReaderBrowseFrame:insertHiddenField("form_name", "networkreader_browse_form").
   NetworkReaderBrowseFrame:insertHiddenField("prog_name", "adNetworkReaderAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderBrowseFrame}
   
   NetworkReaderBrowseFrame:formClose().
   
   /* Create Button Bar */
   NetworkReaderBrowseButtons = NEW buttonBar().
   NetworkReaderBrowseButtons:WebStream = STREAM WebStream:HANDLE.

   IF NOT logPreventDataCreates THEN
   DO:
      NetworkReaderBrowseButtons:addButton("networkreader_browse_form_btn_create",
                                            fTL("Create"),
                                            "createNetworkReader('networkreader_details_form');",
                                            "").
   END.
   
   NetworkReaderBrowseButtons:addButton("networkreader_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewNetworkReaderDetails('networkreader_details_form');",
                                         (IF intNetworkReader > 0 THEN "" ELSE "Disabled")).
                                 
   NetworkReaderBrowseButtons:addButton("networkreader_browse_form_btn_history",
                                         fTL("History"),
                                         "viewNetworkReaderHistory('networkreader_browse_form');",
                                         (IF intNetworkReader > 0 THEN "" ELSE "Disabled")).   
                                    
   NetworkReaderBrowseButtons:addButton("networkreader_browse_form_btn_sendData",
                                         fTL("Send Data"),
                                         "viewSendDataForm('networkreadersenddata_browse_form');",
                                         (IF intNetworkReader > 0 THEN "" ELSE "Disabled")).
                                         
   NetworkReaderBrowseButtons:addButton("networkreader_browse_form_btn_startServer",
                                         fTL("Start Server"),
                                         "sendStartProcessToServer('networkreader_browse_form');",
                                         (IF intNetworkReader > 0 THEN "" ELSE "Disabled")).                                                              
                                         
   NetworkReaderBrowseButtons:addButton("networkreader_browse_form_btn_stopServer",
                                         fTL("Stop Server"),
                                         "sendStopProcessToServer('networkreader_browse_form');",
                                         (IF intNetworkReader > 0 THEN "" ELSE "Disabled")).                                                                          
   
   NetworkReaderBrowseButtons:closeBar().  
   NetworkReaderBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderDetails Procedure 
PROCEDURE pNetworkReaderDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkreader_details_form"}
   
   ASSIGN chrDisplayFieldList  = "NetworkReaderID,NetworkReaderCode,NetworkReaderName,NetworkReaderTypeID"
                               + ",IPAddress,PortNumber,ServerIPAddress,ServerPortNumber,ToteLineID,PrinterID,Active"
                               + ",ProcessProgramID,ToteLineCellID,TargetActionPointID,PickPackStationID"
                               + ",IsListening,StartListening,CanConnect,DirectoryToRunFrom"
          chrEditFieldList     = "NetworkReaderName,NetworkReaderTypeID,IPAddress,PortNumber,ToteLineID,PrinterID"
                               + ",ServerIPAddress,ServerPortNumber,Active,ProcessProgramID,ToteLineCellID,TargetActionPointID"
                               + ",PickPackStationID,DirectoryToRunFrom"
          chrNewFieldList      = "NetworkReaderCode,NetworkReaderName,NetworkReaderTypeID,IPAddress,PortNumber"
                               + ",ServerIPAddress,ServerPortNumber,ToteLineID,PrinterID,Active,ProcessProgramID"
                               + ",ToteLineCellID,TargetActionPointID,PickPackStationID,IsListening,StartListening,CanConnect"
                               + ",DirectoryToRunFrom"
          chrRequiredFieldList = "NetworkReaderName,NetworkReaderCode"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkReaderDetailsForm = NEW dataForm("networkreader_details_form").
   NetworkReaderDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkReaderDetailsForm:FormAction  = "dbNetworkReaderUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkReaderDetailsForm:FormWidth   = 700.
   NetworkReaderDetailsForm:FormHeight  = 490.
   NetworkReaderDetailsForm:FormTitle   = "Network Reader Details".
   NetworkReaderDetailsForm:FormType    = "xl_large".
   
   /* Column Layout */
   NetworkReaderDetailsForm:insertPaddingColumn(50).
   NetworkReaderDetailsForm:insertColumn(130).
   NetworkReaderDetailsForm:insertColumn(120).
   NetworkReaderDetailsForm:insertColumn(20).
   NetworkReaderDetailsForm:insertColumn(4).
   NetworkReaderDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Reader ID").
   NetworkReaderDetailsForm:insertTextField("NetworkReaderID", "", 110, TRUE).  
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Reader Code").
   NetworkReaderDetailsForm:insertTextField("NetworkReaderCode", "", 200, TRUE).  
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Reader Name").
   NetworkReaderDetailsForm:insertTextField("NetworkReaderName", "", 200, TRUE).
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Reader Type").
   NetworkReaderDetailsForm:insertComboField("NetworkReaderTypeID", "", 200, TRUE).
   
   FIND FIRST NetworkReaderType OF NetworkReader NO-LOCK NO-ERROR.
   
   FOR EACH NetworkReaderType NO-LOCK /*idx=NetworkReaderTypeID*/
      WHERE NetworkReaderType.Active:
      
      NetworkReaderDetailsForm:insertComboPairs("NetworkReaderTypeID", STRING(NetworkReaderType.NetworkReaderTypeID), NetworkReaderType.TypeName). 
   END.   
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("IP Address").
   NetworkReaderDetailsForm:insertTextField("IPAddress", "", 200, TRUE).
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Port Number").
   NetworkReaderDetailsForm:insertTextField("PortNumber", "", 200, TRUE).
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Server Address").
   NetworkReaderDetailsForm:insertTextField("ServerIPAddress", "", 200, TRUE).
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Server Port").
   NetworkReaderDetailsForm:insertTextField("ServerPortNumber", "", 200, TRUE).
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Directory To Run From").
   NetworkReaderDetailsForm:insertTextField("DirectoryToRunFrom", "", 200, TRUE).
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Tote Line").
   NetworkReaderDetailsForm:insertComboField("ToteLineID", "", 200, TRUE).
   NetworkReaderDetailsForm:insertComboPairs("ToteLineID", "0", "None Selected...").
   
   FIND FIRST ToteLine OF NetworkReader NO-LOCK NO-ERROR.
   
   FOR EACH ToteLine NO-LOCK /*idx=ToteLineID*/
      WHERE ToteLine.Active:
      
      NetworkReaderDetailsForm:insertComboPairs("ToteLineID", STRING(ToteLine.ToteLineID), ToteLine.LineName). 
   END. 
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("ToteLine Cell").
   NetworkReaderDetailsForm:insertComboField("ToteLineCellID", "", 200, TRUE).
   NetworkReaderDetailsForm:insertComboPairs("ToteLineCellID", "0", "None Selected...").
   
   FOR EACH ToteLineCell NO-LOCK /*idx=ToteLineCellID*/
      WHERE ToteLineCell.Active:
      
      NetworkReaderDetailsForm:insertComboPairs("ToteLineCellID", STRING(ToteLineCell.ToteLineCellID), ToteLineCell.ToteLineCellName).
   END.
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("PickPackStation").
   NetworkReaderDetailsForm:insertComboField("PickPackStationID", "", 200, TRUE).
   NetworkReaderDetailsForm:insertComboPairs("PickPackStationID", "0", "None Selected...").
   
   FOR EACH PickPackStation NO-LOCK /*idx=PickPackStationID*/
      WHERE PickPackStation.Active:
      
      NetworkReaderDetailsForm:insertComboPairs("PickPackStationID", STRING(PickPackStation.PickPackStationID), PickPackStation.StationName).
   END. 
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Target ActionPoint").
   NetworkReaderDetailsForm:insertComboField("TargetActionPointID", "", 200, TRUE).
   NetworkReaderDetailsForm:insertComboPairs("TargetActionPointID", "0", "None Selected...").
   
   FIND FIRST NetworkActionPoint NO-LOCK
      WHERE NetworkActionPoint.NetworkActionPointID = NetworkReader.TargetActionPointID NO-ERROR.
   
   FOR EACH NetworkActionPoint NO-LOCK /*idx=PrinterID*/
      WHERE NetworkActionPoint.Active:
      
      NetworkReaderDetailsForm:insertComboPairs("TargetActionPointID", STRING(NetworkActionPoint.NetworkActionPointID), NetworkActionPoint.NetworkActionPointName).
   END.
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel("Process Program").
   NetworkReaderDetailsForm:insertComboField("ProcessProgramID", "", 200, TRUE).
   NetworkReaderDetailsForm:insertComboPairs("ProcessProgramID", "0", "None Selected...").
   
   FIND FIRST ToteLine OF NetworkReader NO-LOCK NO-ERROR.
   
   FOR EACH ProcessProgram NO-LOCK
      BY ProcessProgram.ProgramName: /*idx=ProcessProgramID*/
      
      NetworkReaderDetailsForm:insertComboPairs("ProcessProgramID", STRING(ProcessProgram.ProcessProgramID), ProcessProgram.ProgramName). 
   END.                                  
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel(fTL("StartListening")). 
   NetworkReaderDetailsForm:insertComboField("StartListening", "", 200, TRUE).  
   NetworkReaderDetailsForm:insertComboPairs("StartListening", "yes", "Yes").
   NetworkReaderDetailsForm:insertComboPairs("StartListening", "no",  "No").
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel(fTL("IsListening")). 
   NetworkReaderDetailsForm:insertComboField("IsListening", "", 200, TRUE).  
   NetworkReaderDetailsForm:insertComboPairs("IsListening", "yes", "Yes").
   NetworkReaderDetailsForm:insertComboPairs("IsListening", "no",  "No").
   
   NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel(fTL("Can Connect")). 
   NetworkReaderDetailsForm:insertComboField("CanConnect", "", 200, TRUE).  
   NetworkReaderDetailsForm:insertComboPairs("CanConnect", "yes", "Yes").
   NetworkReaderDetailsForm:insertComboPairs("CanConnect", "no",  "No").
   
    NetworkReaderDetailsForm:startRow().
   NetworkReaderDetailsForm:insertLabel(fTL("Active")). 
   NetworkReaderDetailsForm:insertComboField("Active", "", 200, TRUE).  
   NetworkReaderDetailsForm:insertComboPairs("Active", "yes", "Active").
   NetworkReaderDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pNetworkReaderDetailsFields}

   /* Add Hidden Fields*/
   NetworkReaderDetailsForm:insertHiddenField("networkreader_browse_scroll", "").
   NetworkReaderDetailsForm:insertHiddenField("NetworkReaderCode", chrNetworkReaderCode).
   NetworkReaderDetailsForm:insertHiddenField("form_name", "networkreader_details_form").
   NetworkReaderDetailsForm:insertHiddenField("prog_name", "adNetworkReaderAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderDetailsForm}   
   
   /* Create Button Bar */
   NetworkReaderDetailsButtons = NEW buttonBar().
   
   NetworkReaderDetailsButtons:addButton("networkreader_details_form_btn_save", 
                                  fTL("Save"), 
                                  "updateNetworkReader('networkreader_details_form');").
   
   NetworkReaderDetailsButtons:addButton("networkreader_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('networkreader_details_form_popup');").
   
   NetworkReaderDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkReaderDetailsForm:FormButtons = NetworkReaderDetailsButtons.
   
   NetworkReaderDetailsForm:endForm(). 
   
   NetworkReaderDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + NetworkReaderDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderDetailsFields Procedure 
PROCEDURE pNetworkReaderDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         NetworkReaderDetailsForm:startRow().
         NetworkReaderDetailsForm:insertLabel(fTL("Field Label")).
         NetworkReaderDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderHistory Procedure 
PROCEDURE pNetworkReaderHistory :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   NetworkReaderHistoryBrowseForm           = NEW dataForm("networkreaderhistory_browse_form").
   NetworkReaderHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   NetworkReaderHistoryBrowseForm:FormWidth  = 860.
   NetworkReaderHistoryBrowseForm:FormHeight = 530.
   NetworkReaderHistoryBrowseForm:FormTitle  = fTL("Network Reader History").
   NetworkReaderHistoryBrowseForm:FormType   = "xxl_large".
   NetworkReaderHistoryBrowse                = NEW browseTable("networkreaderhistory_browse").
   NetworkReaderHistoryBrowse:BrowseWidth    = 840.
   NetworkReaderHistoryBrowse:BrowseHeight   = 490.
   
   NetworkReaderHistoryBrowse:insertColumn(fTL("Hist ID"),      60, "INTEGER",           FALSE).
   NetworkReaderHistoryBrowse:insertColumn(fTL("Reader Code"), 100, "CHARACTER", "LEFT", FALSE).
   NetworkReaderHistoryBrowse:insertColumn(fTL("Reader Name"), 100, "CHARACTER", "LEFT", FALSE).
   NetworkReaderHistoryBrowse:insertColumn(fTL("Reader Type"), 120, "CHARACTER", "LEFT", FALSE).
   NetworkReaderHistoryBrowse:insertColumn(fTL("IsListening"),  70, "LOGICAL",           FALSE).
   NetworkReaderHistoryBrowse:insertColumn(fTL("Active"),       70, "LOGICAL",           FALSE).
   NetworkReaderHistoryBrowse:insertColumn(fTL("User"),        150, "CHARACTER", "LEFT", FALSE).
   NetworkReaderHistoryBrowse:insertColumn(fTL("Created"),     150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i NetworkReaderHistory}
   
   NetworkReaderHistoryBrowse:StartBody().
   
   FOR EACH NetworkReaderHistory NO-LOCK /*idx=NetworkReaderID*/
      WHERE NetworkReaderHistory.NetworkReaderID = intNetworkReader
      BY    NetworkReaderHistory.NetworkReaderHistoryID DESCENDING:
          
      FIND FIRST GateUser OF NetworkReaderHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/
      FIND FIRST ToteLine OF NetworkReaderHistory NO-LOCK NO-ERROR. /*idx=ToteLineID*/
      FIND FIRST NetworkReaderType OF NetworkReaderHistory NO-LOCK NO-ERROR. /*idx=NetworkReaderTypeID*/

      NetworkReaderHistoryBrowse:startRow (NetworkReaderHistory.NetworkReaderHistoryID, 
         "selectNetworkReaderHistoryRow(this," + '"' + STRING(NetworkReaderHistory.NetworkReaderHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i NetworkReaderHistory}
      
      NetworkReaderHistoryBrowse:insertData(NetworkReaderHistory.NetworkReaderHistoryID).
      NetworkReaderHistoryBrowse:insertData(NetworkReaderHistory.NetworkReaderCode,"LEFT").
      NetworkReaderHistoryBrowse:insertData(NetworkReaderHistory.NetworkReaderName,"LEFT").
      NetworkReaderHistoryBrowse:insertData((IF AVAILABLE NetworkReaderType THEN NetworkReaderType.TypeName ELSE ""),"LEFT").
      NetworkReaderHistoryBrowse:insertData(NetworkReaderHistory.IsListening,"Yes/No").
      NetworkReaderHistoryBrowse:insertData(NetworkReaderHistory.Active,"Yes/No").
      NetworkReaderHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      NetworkReaderHistoryBrowse:insertData(fDisplayDate&Time(NetworkReaderHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      NetworkReaderHistoryBrowse:endRow().
   END. /* FOR EACH PickPackStationHistory */
   
   NetworkReaderHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + NetworkReaderHistoryBrowse:getErrors().
   
   NetworkReaderHistoryBrowseForm:insertHiddenField("popup_networkreader_browse","").
   NetworkReaderHistoryBrowseForm:insertHiddenField("popup_networkreaderhistory_browse","").
   NetworkReaderHistoryBrowseForm:insertHiddenField("NetworkReaderHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderHistoryBrowseForm}
   
   /* Create Button Bar */
   NetworkReaderHistoryBrowseButtons = NEW buttonBar().
   
   NetworkReaderHistoryBrowseButtons:addButton("networkreaderhistory_browse_form_btn_details",
                                               fTL("Details"),
                                               "viewNetworkReaderHistoryDetails('networkreaderhistory_details_form');",
                                              (IF intSelectedNetworkReaderHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   NetworkReaderHistoryBrowseButtons:addButton("networkreaderhistory_browse_form_btn_cancel",
                                               fTL("Cancel"),
                                               "disablePopup('networkreaderhistory_browse_form_popup');").
   NetworkReaderHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkReaderHistoryBrowseForm:FormBrowse  = NetworkReaderHistoryBrowse.
   NetworkReaderHistoryBrowseForm:FormButtons = NetworkReaderHistoryBrowseButtons.
   NetworkReaderHistoryBrowseForm:endForm(). 
   
   NetworkReaderHistoryBrowseForm:displayForm().  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pNetworkReaderHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pNetworkReaderHistoryDetails Procedure 
PROCEDURE pNetworkReaderHistoryDetails :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
 /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "networkreaderhistory_details_form"}
   ASSIGN chrDisplayFieldList  = "NetworkReaderHistoryID,NetworkReaderCode,NetworkReaderName,IsListening,StartListening"
                               + ",ServerIPAddress,ServerPortNumber,NetworkReaderTypeID,IPAddress,PortNumber,ToteLineID,ToteLineCellID,Active,GateUserID"
                               + ",CreatedDate,CreatedHour,CreatedMins,PickPackStationID,TargetActionPointID,ProcessProgramID"
                               + ",DirectoryToRunFrom,CanConnect"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   NetworkReaderHistoryDetailsForm = NEW dataForm("networkreaderhistory_details_form").
   NetworkReaderHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   NetworkReaderHistoryDetailsForm:FormAction = "dbNetworkReaderUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   NetworkReaderHistoryDetailsForm:FormWidth   = 700.
   NetworkReaderHistoryDetailsForm:FormHeight  = 490.
   NetworkReaderHistoryDetailsForm:FormTitle   = "Network Reader History Details".
   NetworkReaderHistoryDetailsForm:FormType    = "xl_large".
   
   /* Column Layout */
   NetworkReaderHistoryDetailsForm:insertPaddingColumn(50).
   NetworkReaderHistoryDetailsForm:insertColumn(130).
   NetworkReaderHistoryDetailsForm:insertColumn(120).
   NetworkReaderHistoryDetailsForm:insertColumn(20).
   NetworkReaderHistoryDetailsForm:insertColumn(4).
   NetworkReaderHistoryDetailsForm:insertColumn(110).
   
   /* Fields */
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("History ID")).
   NetworkReaderHistoryDetailsForm:insertTextField("NetworkReaderHistoryID", "", 200, TRUE).  
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Reader Code")).
   NetworkReaderHistoryDetailsForm:insertTextField("NetworkReaderCode", "", 200, TRUE).
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Reader Name")).
   NetworkReaderHistoryDetailsForm:insertTextField("NetworkReaderName", "", 200, TRUE).
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Reader Type")).
   NetworkReaderHistoryDetailsForm:insertComboField("NetworkReaderTypeID", "", 200, TRUE).
   NetworkReaderHistoryDetailsForm:insertComboPairs("NetworkReaderTypeID", "0", "None Selected...").   
   
   FOR EACH NetworkReaderType NO-LOCK /*idx=NetworkReaderTypeID*/
      WHERE NetworkReaderType.Active:
      
      NetworkReaderHistoryDetailsForm:insertComboPairs("NetworkReaderTypeID", STRING(NetworkReaderType.NetworkReaderTypeID), NetworkReaderType.TypeName). 
   END.      
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("IP Address")).
   NetworkReaderHistoryDetailsForm:insertTextField("IPAddress", "", 200, TRUE).
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Port Number")).
   NetworkReaderHistoryDetailsForm:insertTextField("PortNumber", "", 200, TRUE).  
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Server Address")).
   NetworkReaderHistoryDetailsForm:insertTextField("ServerIPAddress", "", 200, TRUE).
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Server Port")).
   NetworkReaderHistoryDetailsForm:insertTextField("ServerPortNumber", "", 200, TRUE).  
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel("Directory To Run From").
   NetworkReaderHistoryDetailsForm:insertTextField("DirectoryToRunFrom", "", 200, TRUE).
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Tote Line")).
   NetworkReaderHistoryDetailsForm:insertComboField("ToteLineID", "", 200, TRUE).   
   NetworkReaderHistoryDetailsForm:insertComboPairs("ToteLineID", "0", "None Selected...").   
   
   FOR EACH ToteLine NO-LOCK /*idx=ToteLineID*/
      WHERE ToteLine.Active:
      
      NetworkReaderHistoryDetailsForm:insertComboPairs("ToteLineID", STRING(ToteLine.ToteLineID), ToteLine.LineName). 
   END. 

   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel("ToteLine Cell").
   NetworkReaderHistoryDetailsForm:insertComboField("ToteLineCellID", "", 200, TRUE).
   NetworkReaderHistoryDetailsForm:insertComboPairs("ToteLineCellID", "0", "None Selected..."). 
   
   FOR EACH ToteLineCell NO-LOCK /*idx=ToteLineCellID*/
      WHERE ToteLineCell.Active:
      
      NetworkReaderHistoryDetailsForm:insertComboPairs("ToteLineCellID", STRING(ToteLineCell.ToteLineCellID), ToteLineCell.ToteLineCellName).
   END.
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel("PickPackStation").
   NetworkReaderHistoryDetailsForm:insertComboField("PickPackStationID", "", 200, TRUE).
   NetworkReaderHistoryDetailsForm:insertComboPairs("PickPackStationID", "0", "None Selected..."). 
   
   FOR EACH PickPackStation NO-LOCK /*idx=PickPackStationID*/
      WHERE PickPackStation.Active:
      
      NetworkReaderHistoryDetailsForm:insertComboPairs("PickPackStationID", STRING(PickPackStation.PickPackStationID), PickPackStation.StationName).
   END. 
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel("Target ActionPoint").
   NetworkReaderHistoryDetailsForm:insertComboField("TargetActionPointID", "", 200, TRUE).
   NetworkReaderHistoryDetailsForm:insertComboPairs("TargetActionPointID", "0", "None Selected..."). 
   
   FIND FIRST NetworkActionPoint NO-LOCK
      WHERE NetworkActionPoint.NetworkActionPointID = NetworkReader.TargetActionPointID NO-ERROR.
   
   FOR EACH NetworkActionPoint NO-LOCK /*idx=PrinterID*/
      WHERE NetworkActionPoint.Active:
      
      NetworkReaderHistoryDetailsForm:insertComboPairs("TargetActionPointID", STRING(NetworkActionPoint.NetworkActionPointID), NetworkActionPoint.NetworkActionPointName).
   END.
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel("Process Program").
   NetworkReaderHistoryDetailsForm:insertComboField("ProcessProgramID", "", 200, TRUE).
   NetworkReaderHistoryDetailsForm:insertComboPairs("ProcessProgramID", "0", "None Selected..."). 
   
   FOR EACH ProcessProgram NO-LOCK: /*idx=ProcessProgramID*/
      
      NetworkReaderHistoryDetailsForm:insertComboPairs("ProcessProgramID", STRING(ProcessProgram.ProcessProgramID), ProcessProgram.ProgramName). 
   END.
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("StartListening")). 
   NetworkReaderHistoryDetailsForm:insertComboField("StartListening", "", 200, TRUE).  
   NetworkReaderHistoryDetailsForm:insertComboPairs("StartListening", "yes", "Yes").
   NetworkReaderHistoryDetailsForm:insertComboPairs("StartListening", "no",  "No").
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("IsListening")). 
   NetworkReaderHistoryDetailsForm:insertComboField("IsListening", "", 200, TRUE).  
   NetworkReaderHistoryDetailsForm:insertComboPairs("IsListening", "yes", "Yes").
   NetworkReaderHistoryDetailsForm:insertComboPairs("IsListening", "no",  "No").
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Can Connect")).
   NetworkReaderHistoryDetailsForm:insertComboField("CanConnect", "", 200, TRUE).
   NetworkReaderHistoryDetailsForm:insertComboPairs("CanConnect", "yes", "Yes").
   NetworkReaderHistoryDetailsForm:insertComboPairs("CanConnect", "no", "No").
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Active")).
   NetworkReaderHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   NetworkReaderHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   NetworkReaderHistoryDetailsForm:insertComboPairs("Active", "no", "Not Active").
   
   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Created")).
   NetworkReaderHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).  
   /* Time fields have no label */
   NetworkReaderHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   NetworkReaderHistoryDetailsForm:insertLabel(":").
   NetworkReaderHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE). 

   NetworkReaderHistoryDetailsForm:startRow().
   NetworkReaderHistoryDetailsForm:insertLabel(fTL("Gate User ID")).
   NetworkReaderHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE). 
   FOR EACH GateUser NO-LOCK /*idx=GateUserID*/
      BY GateUser.FullName:
         
      NetworkReaderHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.

   {webGetOptionalFormFields.i pNetworkReaderHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   NetworkReaderHistoryDetailsForm:insertHiddenField("networkreaderhistory_browse_scroll", "").
   NetworkReaderHistoryDetailsForm:insertHiddenField("form_name", "networkreaderhistory_details_form").
   NetworkReaderHistoryDetailsForm:insertHiddenField("prog_name", "adNetworkReaderAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderHistoryDetailsForm}
   
   /* Create Button Bar */
    NetworkReaderHistoryDetailsButtons = NEW buttonBar().

    NetworkReaderHistoryDetailsButtons:addButton("networkreaderhistory_details_form_btn_cancel", 
                                                     fTL("Cancel"), 
                                                     "disablePopup('networkreaderhistory_details_form_popup');").
    NetworkReaderHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   NetworkReaderHistoryDetailsForm:FormButtons =  NetworkReaderHistoryDetailsButtons.
   
   NetworkReaderHistoryDetailsForm:endForm(). 
   
   NetworkReaderHistoryDetailsForm:displayForm(). 

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
   NetworkReaderSendDataForm           = NEW dataForm("networkreadersenddata_browse_form").
   NetworkReaderSendDataForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */   
   NetworkReaderSendDataForm:FormWidth  = 350.
   NetworkReaderSendDataForm:FormHeight = 200.
   NetworkReaderSendDataForm:FormTitle  = fTL("Data to be send").
   NetworkReaderSendDataForm:FormType   = "small_wide".
   
   
   /* Colummn style */
   NetworkReaderSendDataForm:insertPaddingColumn(30).
   NetworkReaderSendDataForm:insertColumn(80).
   NetworkReaderSendDataForm:insertColumn(80).
   NetworkReaderSendDataForm:insertColumn(80).
       
   /* Fields */  
   
   NetworkReaderSendDataForm:startRow().
   NetworkReaderSendDataForm:insertLabel(fTL("Value")).
   NetworkReaderSendDataForm:insertTextField("NetworkValue", "", 120, TRUE).
   
   NetworkReaderSendDataForm:insertHiddenField("form_name","networkreadersenddata_browse_form").
   NetworkReaderSendDataForm:insertHiddenField("prog_name","adNetworkReaderAdmin.p").  
   NetworkReaderSendDataForm:insertHiddenField("NetworkReaderID", chrNetworkReaderID).
   NetworkReaderSendDataForm:insertHiddenField("NetworkReaderList", chrNetworkReaderList).
   NetworkReaderSendDataForm:insertHiddenField("NetworkValue", "").
   NetworkReaderSendDataForm:insertHiddenField("process_mode", "send").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i NetworkReaderSendDataForm}

   /* Create Button Bar */
   NetworkReaderSendDataFormButtons = NEW buttonBar().
   
   NetworkReaderSendDataFormButtons:addButton("networkreadersenddata_browse_form_btn_send",
                                              fTL("Send"),
                                              "sendData('networkreadersenddata_browse_form');").    
                                         
   NetworkReaderSendDataFormButtons:addButton("networkreadersenddata_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('networkreadersenddata_browse_form_popup');").
   NetworkReaderSendDataFormButtons:closeBar(). 
     
   /* Assign the Button Bar Object to the Form Object */
   NetworkReaderSendDataForm:FormButtons = NetworkReaderSendDataFormButtons.
   
   NetworkReaderSendDataForm:FormAction="dbNetworkReaderUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   NetworkReaderSendDataForm:endForm(). 
   
   NetworkReaderSendDataForm:displayForm().  

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
   
   ASSIGN chrNetworkReaderID                 = get-value("NetworkReaderID")
          chrNetworkReaderList               = get-value("NetworkReaderList")
          intNetworkReader                   = INTEGER(chrNetworkReaderList)
          chrActiveServers                   = get-value("ActiveServer")
          chrInActiveServers                 = get-value("InActiveServer")
          chrStopProcessRan                  = get-value("StopWasRun")
          chrStartProcessRan                 = get-value("StartWasRun")
          chrSendProcessRan                  = get-value("SendWasRun")
          chrDataWasSent                     = get-value("DataWasSent")
          chrResponseFromDb                  = get-value("ProcessResponse")
          chrScrollToNetworkReaderRow        = STRING(INTEGER(get-value("networkreader_browse_scroll"))) + ";"
          chrNetworkReaderHistoryID          = get-value("NetworkReaderHistoryID")
          intSelectedNetworkReaderHistory    = INTEGER(chrNetworkReaderHistoryID)
          chrNetworkReaderCode               = get-value("NetworkReaderCode")
          chrScrollToNetworkReaderHistoryRow = STRING(INTEGER(get-value("networkreaderhistory_browse_scroll"))) + ";".
   
   IF NUM-ENTRIES(chrNetworkReaderList) = 1 THEN
   DO:
      chrNetworkReaderID = chrNetworkReaderList.
      intNetworkReader = INTEGER(chrNetworkReaderList).
   END.
          
   /* Process URL values */ 
   ProcessURLloop:
   DO:
                                      
      IF chrNetworkReaderID <> "" AND chrStopProcessRan <> "TRUE" AND chrStartProcessRan <> "TRUE" THEN
         chrSelectNetworkReaderRow = 'selectMultipleNetworkReaderRows("' + chrNetworkReaderID + '");'.
      
      IF chrNetworkReaderList <> "" AND chrStopProcessRan <> "TRUE" AND chrStartProcessRan <> "TRUE"  THEN
         chrSelectNetworkReaderRow = 'selectMultipleNetworkReaderRows("' + chrNetworkReaderList + '");'.     
      
      IF get-value('popup_networkreaderhistory_browse') = "Yes" THEN
      DO:
         chrPopupHistory = 'enablePopup("networkreaderhistory_browse_form_popup");'.
         LEAVE ProcessURLloop. /* form pop ups do not intefere with url processing popups */
      END.
                                                             
      IF chrStopProcessRan = "True" THEN
      DO:
         chrPopupStopServer = 'checkServerStatusAfterStop("' + chrActiveServers + '","' + chrInActiveServers + '","' + chrResponseFromDb + '");'.          
         intNetworkReader = 0.
      END.
      
      IF chrStartProcessRan = "True" THEN
      DO:
         chrPopupStartServer = 'checkServerStatusAfterStart("' + chrActiveServers + '","' + chrInActiveServers + '","' + chrResponseFromDb + '");'.                  
         intNetworkReader = 0.
      END.
           
      IF chrSendProcessRan = "True" THEN
      DO:
         FIND NetworkReader NO-LOCK
            WHERE NetworkReader.NetworkReaderID = INTEGER(chrNetworkReaderID) NO-ERROR.
         IF AVAILABLE NetworkReader THEN 
            chrPopupSendData = 'sendDataAlert("' + NetworkReader.NetworkReaderCode + '","' + chrDataWasSent + '");'. 
      END.
         
   END. /* Process URL values */
                           
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("networkreader_browse").scrollTop=' + chrScrollToNetworkReaderRow 
                             + chrSelectNetworkReaderRow + chrPopupHistory + chrPopupStartServer  + chrPopupStopServer
                             + chrPopupSendData.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Network Device Reader Admin".
   ThisPage:FrameTitle = "Network Device Readers".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File specifically for Network Readers */
   ThisPage:addJavaScript("networkdevicereaders.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pNetworkReaderBrowse.   
   
   /******* Popup Browsers and Forms ********/    
   RUN pNetworkReaderDetails.
   RUN pNetworkReaderHistory.
   RUN pNetworkReaderHistoryDetails.
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
   DELETE OBJECT NetworkReaderBrowseFrame               NO-ERROR.
   DELETE OBJECT NetworkReaderBrowse                    NO-ERROR.
   DELETE OBJECT NetworkReaderBrowseButtons             NO-ERROR.
   DELETE OBJECT NetworkReaderDetailsForm               NO-ERROR.
   DELETE OBJECT NetworkReaderDetailsButtons            NO-ERROR.
   DELETE OBJECT NetworkReaderSendDataForm              NO-ERROR.
   DELETE OBJECT NetworkReaderSendDataFormButtons       NO-ERROR.
   DELETE OBJECT NetworkReaderHistoryBrowseForm         NO-ERROR.
   DELETE OBJECT NetworkReaderHistoryBrowse             NO-ERROR.
   DELETE OBJECT NetworkReaderHistoryBrowseButtons      NO-ERROR.
   DELETE OBJECT NetworkReaderHistoryDetailsForm        NO-ERROR.
   DELETE OBJECT NetworkReaderHistoryDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

