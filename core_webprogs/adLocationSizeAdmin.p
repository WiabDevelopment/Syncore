&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adLocationSizeAdmin.p 

  Description: ad file for the Location Size Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Anthony Ferrari

  Created: 20/03/2015

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
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedLocationSize           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedLocationSizeHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrLocationSizeHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLocationSizeRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLocationSizeRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLocationSizeHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLocationSizeID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                   AS CHARACTER   NO-UNDO.

/* Buffers */
/* Objects */
DEFINE VARIABLE LocationSizeBrowseFrame           AS pageFrame.
DEFINE VARIABLE LocationSizeBrowse                AS browseTable.
DEFINE VARIABLE LocationSizeBrowseButtons         AS buttonBar.
DEFINE VARIABLE LocationSizeDetailsForm           AS dataForm.
DEFINE VARIABLE LocationSizeDetailsButtons        AS buttonBar.
DEFINE VARIABLE LocationSizeHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE LocationSizeHistoryBrowse         AS browseTable.
DEFINE VARIABLE LocationSizeHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE LocationSizeHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE LocationSizeHistoryButtons        AS buttonBar.


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
   RUN pGetSystemOptions.  
   
   ASSIGN chrLocationSizeID = get-value("LocationSizeID")
          intSelectedLocationSize = INTEGER(chrLocationSizeID)
          chrScrollToLocationSizeRow = STRING(INTEGER(get-value("locationsize_browse_scroll"))) + ";"
          /*History details button*/
          chrLocationSizeHistoryID = get-value("LocationSizeHistoryID")
          intSelectedLocationSizeHistory = INTEGER(chrLocationSizeHistoryID)
          chrScrollToLocationSizeHistoryRow = STRING(INTEGER(get-value("locationsizehistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrLocationSizeID <> "" THEN
      chrLocationSizeRow = 'selectLocationSizeRow(document.getElementById("locationsize_browse_row_'
                               + chrLocationSizeID + '"),"' + chrLocationSizeID +  '");'.
                                                          
   IF get-value('popup_locationsizehistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("locationsizehistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("locationsize_browse").scrollTop=' + chrScrollToLocationSizeRow 
                             + chrLocationSizeRow + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Location Size Admin".
   ThisPage:FrameTitle = "Location Size Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("locationsizeadmin.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pLocationSizeBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pLocationSizeDetails.
   
   RUN pLocationSizeHistoryBrowse.
   
   RUN pLocationSizeHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT LocationSizeBrowseFrame        NO-ERROR.
   DELETE OBJECT LocationSizeBrowse             NO-ERROR.
   DELETE OBJECT LocationSizeBrowseButtons      NO-ERROR.
   DELETE OBJECT LocationSizeDetailsForm        NO-ERROR.
   DELETE OBJECT LocationSizeDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pLocationSizeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "locationsize_details_form"}
   
   LocationSizeBrowse = NEW browseTable("locationsize_browse").
   LocationSizeBrowse:BrowseWidth  = 965.
   LocationSizeBrowse:BrowseHeight = 455.
   LocationSizeBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   LocationSizeBrowse:insertColumn(fTL(" Location Size ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LocationSize}
   
   LocationSizeBrowse:insertColumn(fTL("Size Code"), 80, "CHARACTER", "LEFT", FALSE).
   LocationSizeBrowse:insertColumn(fTL("Size Name"), 150, "CHARACTER", "LEFT", FALSE).
   LocationSizeBrowse:insertColumn(fTL("Size Description"), 200, "CHARACTER", "LEFT", FALSE).   
   LocationSizeBrowse:insertColumn(fTL("Height"),    70, "DECIMAL", "LEFT", FALSE). 
   LocationSizeBrowse:insertColumn(fTL("Width"),     70, "DECIMAL", "LEFT", FALSE).
   LocationSizeBrowse:insertColumn(fTL("Depth"),     70, "DECIMAL", "LEFT", FALSE).
   LocationSizeBrowse:insertColumn(fTL("Volume"),    70, "DECIMAL", "LEFT", FALSE).
   LocationSizeBrowse:insertColumn(fTL("Active"),    50, "LOGICAL", "LEFT", FALSE).
   
   
   /*Body*/
   LocationSizeBrowse:startBody().
   
   FOR EACH LocationSize NO-LOCK: /*idx=LocationSizeID*/
   
       FIND FIRST LocationType OF LocationSize NO-LOCK NO-ERROR. /*idx=LocationTypeID*/
            
      LocationSizeBrowse:startRow(LocationSize.LocationSizeID, "selectLocationSizeRow(this," + '"' 
                                       + STRING(LocationSize.LocationSizeID) + '"' + ");", "").
                                     
      LocationSizeBrowse:insertData(LocationSize.LocationSizeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i LocationSize}      
 
      LocationSizeBrowse:insertData(LocationSize.SizeCode,                      "LEFT").
      LocationSizeBrowse:insertData(LocationSize.SizeName,                      "LEFT").
      LocationSizeBrowse:insertData(LocationSize.SizeDescr,                     "LEFT"). 
      LocationSizeBrowse:insertData(STRING(LocationSize.Height, ">>>>9.999"),   "LEFT").
      LocationSizeBrowse:insertData(STRING(LocationSize.Width, ">>>>9.999"),    "LEFT").
      LocationSizeBrowse:insertData(STRING(LocationSize.Depth, ">>>>9.999"),    "LEFT").
      LocationSizeBrowse:insertData(STRING(LocationSize.Volume, ">>>>9.99999"), "LEFT").
      LocationSizeBrowse:insertData(STRING(LocationSize.Active , "Yes/No"),     "LEFT").
      
            
      /* Add hidden fields */
      LocationSizeBrowse:insertHiddenData("LocationSizeVersionID",LocationSize.VersionID).
      
      LocationSizeBrowse:endRow().
      
   END. /*FOR EACH LocationSizeAdmin NO-LOCK */
   
   LocationSizeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationSizeBrowse:getErrors().
   
   /* Create a new frame */
   LocationSizeBrowseFrame = NEW pageFrame().
   LocationSizeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LocationSizeBrowseFrame:FormAction="dbLocationSizeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LocationSizeBrowseFrame:formOpen("locationsize_browse_form").
   
   /* Start the Frame Header */
   LocationSizeBrowseFrame:insertSpacer(5).
   LocationSizeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LocationSizeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LocationSizeBrowseFrame:frameClose().
   LocationSizeBrowseFrame:insertSpacer(10).
   
   LocationSizeBrowseFrame:insertHiddenField("locationsize_browse_scroll","").
   LocationSizeBrowseFrame:insertHiddenField("LocationSizeID","").
   LocationSizeBrowseFrame:insertHiddenField("LocationSizeVersionID","").
   LocationSizeBrowseFrame:insertHiddenField("form_name","locationsize_browse_form").
   LocationSizeBrowseFrame:insertHiddenField("popup_locationsizehistory_browse","").
   LocationSizeBrowseFrame:insertHiddenField("prog_name","adLocationSizeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationSizeBrowseFrame}
   
   LocationSizeBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   LocationSizeBrowseButtons = NEW buttonBar().
   LocationSizeBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   LocationSizeBrowseButtons:addButton("locationsize_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewLocationSizeDetails('locationsize_details_form');",
                                         (IF intSelectedLocationSize > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
   LocationSizeBrowseButtons:addButton("locationsize_browse_form_btn_create",
                                         fTL("Create"),
                                         "createLocationSize('locationsize_details_form');",
                                         "").
   END.
   
   LocationSizeBrowseButtons:addButton("locationsize_browse_form_btn_history",
                                         fTL("History"),
                                         "viewLocationSizeHistory('locationsizehistory_browse_form');",
                                         (IF intSelectedLocationSize > 0 THEN "" ELSE "Disabled")).
   
   LocationSizeBrowseButtons:closeBar().  
   LocationSizeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pLocationSizeDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "locationsize_details_form"}
   ASSIGN chrDisplayFieldList  = "LocationSizeID,LocationTypeID,SizeCode,SizeName,SizeDescr,Height,Width,Depth,Volume,Active,"
                                 + "ListingSequence"
          chrEditFieldList     = "SizeName,SizeDescr,Height,Width,Depth,Volume,Active,LocationTypeID,ListingSequence"
          chrNewFieldList      = "SizeCode,SizeName,SizeDescr,Height,Width,Depth,Volume,Active,LocationTypeID,ListingSequence"
          chrRequiredFieldList = "SizeCode,SizeName,SizeDescr,Height,Width,Depth,Volume,Active,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "Height:DECIMAL,Width:DECIMAL,Depth:DECIMAL,Volume:DECIMAL,ListingSequence:INTEGER".

   
   LocationSizeDetailsForm = NEW dataForm("locationsize_details_form").
   LocationSizeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationSizeDetailsForm:FormAction = "dbLocationSizeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationSizeDetailsForm:FormWidth   = 460.
   LocationSizeDetailsForm:FormHeight  = 300.
   LocationSizeDetailsForm:FormTitle   = "Location Size Admin Details".
   LocationSizeDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   LocationSizeDetailsForm:insertPaddingColumn(30).
   LocationSizeDetailsForm:insertColumn(185).
   
   /* Fields */
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Location Size ID")).
   LocationSizeDetailsForm:insertTextField("LocationSizeID", "", 190, TRUE).
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Location Type ID")).
   LocationSizeDetailsForm:insertComboField("LocationTypeID", "", 190, TRUE).
   FOR EACH LocationType NO-LOCK  /*idx=LocationTypeID*/                                                                                                      
      BY LocationType.LocationTypeID:                                                                                                  
      LocationSizeDetailsForm:insertComboPairs("LocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).
   END. /*FOR EACH LocationType NO-LOCK*/
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Size Code")).
   LocationSizeDetailsForm:insertTextField("SizeCode", "", 190, TRUE).  
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Size Name")).
   LocationSizeDetailsForm:insertTextField("SizeName", "", 190, TRUE).  
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Size Description")).
   LocationSizeDetailsForm:insertTextField("SizeDescr", "", 190, TRUE).
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Height")).
   LocationSizeDetailsForm:insertTextField("Height", "", 190, TRUE).
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Width")).
   LocationSizeDetailsForm:insertTextField("Width", "", 190, TRUE).
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Depth")).
   LocationSizeDetailsForm:insertTextField("Depth", "", 190, TRUE).
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Volume")).
   LocationSizeDetailsForm:insertTextField("Volume", "", 190, TRUE).
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Listing Sequence")).
   LocationSizeDetailsForm:insertTextfield("ListingSequence", "", 190, TRUE).
   
   LocationSizeDetailsForm:startRow().
   LocationSizeDetailsForm:insertLabel(fTL("Active")).
   LocationSizeDetailsForm:insertComboField("Active", "", 190, TRUE).
   LocationSizeDetailsForm:insertComboPairs("Active", "yes", "Yes").    
   LocationSizeDetailsForm:insertComboPairs("Active", "no", "No"). 


   {webGetOptionalFormFields.i pLocationSizeDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LocationSizeDetailsForm:insertHiddenField("locationsize_browse_scroll", "").
   LocationSizeDetailsForm:insertHiddenField("form_name", "locationsize_details_form").
   LocationSizeDetailsForm:insertHiddenField("prog_name", "adLocationSizeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationSizeDetailsForm}
   
   /* Create Button Bar */
   LocationSizeDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataUpdates THEN
   DO:
   LocationSizeDetailsButtons:addButton("locationsize_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateLocationSize('locationsize_details_form');").
   END.
   LocationSizeDetailsButtons:addButton("locationsize_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode');" + 
                                    "disablePopup('locationsize_details_form_popup');").
   LocationSizeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationSizeDetailsForm:FormButtons = LocationSizeDetailsButtons.
   
   LocationSizeDetailsForm:endForm(). 
   
   LocationSizeDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pLocationSizeDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      LocationSizeDetailsForm:startRow().
      LocationSizeDetailsForm:insertLabel(fTL("Field Label")).
      LocationSizeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pLocationSizeHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "locationsizehistory_details_form"}
   ASSIGN chrDisplayFieldList  = "LocationSizeHistoryID,LocationSizeID,LocationTypeID,SizeCode,SizeName,SizeDescr,Height,Width,"
                                 + "Depth,Volume,Active,ListingSequence,Created,GateUserID,TransactionID,OperationTypeID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   LocationSizeHistoryDetailsForm = NEW dataForm("locationsizehistory_details_form").
   LocationSizeHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationSizeHistoryDetailsForm:FormAction = "dbLocationSizeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationSizeHistoryDetailsForm:FormWidth   = 580.
   LocationSizeHistoryDetailsForm:FormHeight  = 420.
   LocationSizeHistoryDetailsForm:FormTitle   = "Location Size Admin History Details".
   LocationSizeHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   LocationSizeHistoryDetailsForm:insertPaddingColumn(30).
   LocationSizeHistoryDetailsForm:insertColumn(150).
   LocationSizeHistoryDetailsForm:insertColumn(125).
   LocationSizeHistoryDetailsForm:insertColumn(20).
   LocationSizeHistoryDetailsForm:insertColumn(4).
   LocationSizeHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Loction Size History ID")).
   LocationSizeHistoryDetailsForm:insertTextField("LocationSizeHistoryID", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Location Size ID")).
   LocationSizeHistoryDetailsForm:insertTextField("LocationSizeID", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Location Type ID")).
   LocationSizeHistoryDetailsForm:insertComboField("LocationTypeID", "", 168, TRUE).
   FOR EACH LocationType NO-LOCK  /*idx=LocationTypeID*/                                                                                                      
      BY LocationType.LocationTypeID:                                                                                                  
      LocationSizeHistoryDetailsForm:insertComboPairs("LocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).
   END. /*FOR EACH LocationType NO-LOCK*/
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Size Name")).
   LocationSizeHistoryDetailsForm:insertTextField("SizeName", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Size Description")).
   LocationSizeHistoryDetailsForm:insertTextField("SizeDescr", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Height")).
   LocationSizeHistoryDetailsForm:insertTextField("Height", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Width")).
   LocationSizeHistoryDetailsForm:insertTextField("Width", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Depth")).
   LocationSizeHistoryDetailsForm:insertTextField("Depth", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Volume")).
   LocationSizeHistoryDetailsForm:insertTextField("Volume", "", 100, TRUE).
   
   LocationSizeHistoryDetailsForm:startRow().
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Listing Sequence")).
   LocationSizeHistoryDetailsForm:insertTextField("ListingSequence", "", 100, TRUE).

   LocationSizeHistoryDetailsForm:startRow().                                                                                           
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   LocationSizeHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   LocationSizeHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   LocationSizeHistoryDetailsForm:insertLabel(":").                                                                                     
   LocationSizeHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                                         
   LocationSizeHistoryDetailsForm:startRow().                                                                                           
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   LocationSizeHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*OperationTypeID*/                                                                                                      
      BY OperationType.OperationTypeID:                                                                                                  
      LocationSizeHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                          OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   LocationSizeHistoryDetailsForm:startRow().                                                                                           
   LocationSizeHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   LocationSizeHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*GateUserID*/                                                                                                        
      BY GateUser.FullName:                                                                                                              
      LocationSizeHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pLocationSizeHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LocationSizeHistoryDetailsForm:insertHiddenField("locationsizehistory_browse_scroll", "").
   LocationSizeHistoryDetailsForm:insertHiddenField("form_name", "locationsizehistory_details_form").
   LocationSizeHistoryDetailsForm:insertHiddenField("prog_name", "adLocationSizeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationSizeHistoryDetailsForm}
   
   /* Create Button Bar */
   LocationSizeHistoryDetailsButtons = NEW buttonBar().

   LocationSizeHistoryDetailsButtons:addButton("locationsizehistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "disablePopup('locationsizehistory_details_form_popup');").
   LocationSizeHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationSizeHistoryDetailsForm:FormButtons = LocationSizeHistoryDetailsButtons.
   
   LocationSizeHistoryDetailsForm:endForm(). 
   
   LocationSizeHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pLocationSizeHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      LocationSizeDetailsForm:startRow().
      LocationSizeDetailsForm:insertLabel(fTL("Field Label")).
      LocationSizeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adLocationSizeAdmin_locationsize_details_form.i}*/ COMEBACK START COMEBACK END*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pLocationSizeHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   LocationSizeHistoryBrowseForm           = NEW dataForm("locationsizehistory_browse_form").
   LocationSizeHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   LocationSizeHistoryBrowseForm:FormWidth  = 860.
   LocationSizeHistoryBrowseForm:FormHeight = 530.
   LocationSizeHistoryBrowseForm:FormTitle  = fTL("Location Size Admin History").
   LocationSizeHistoryBrowseForm:FormType   = "xxl_large".
   LocationSizeHistoryBrowse                = NEW browseTable("locationsizehistory_browse").
   LocationSizeHistoryBrowse:BrowseWidth    = 840.
   LocationSizeHistoryBrowse:BrowseHeight   = 490.
   
   LocationSizeHistoryBrowse:insertColumn(fTL("History ID"), 75, "INTEGER", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Size ID"),    50, "INTEGER", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Size Name"),  125, "CHARACTER", "LEFT", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Height"),     60, "DECIMAL", "LEFT", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Width"),      60, "DECIMAL", "LEFT", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Depth"),      60, "DECIMAL", "LEFT", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Volume"),     60, "DECIMAL", "LEFT", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Active"),     60, "LOGICAL", "LEFT", FALSE). 
   LocationSizeHistoryBrowse:insertColumn(fTL("User"),       125, "CHARACTER", "LEFT", FALSE).
   LocationSizeHistoryBrowse:insertColumn(fTL("Created"),    130, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LocationSizeHistory}
   
   LocationSizeHistoryBrowse:StartBody().
   
   FOR EACH LocationSizeHistory NO-LOCK /*idx=SLocationSizeID and LocationSizeHistoryID*/
      WHERE LocationSizeHistory.LocationSizeID = intSelectedLocationSize
         BY LocationSizeHistory.LocationSizeHistoryID:
          
      FIND FIRST OperationType OF LocationSizeHistory NO-LOCK NO-ERROR. /*idx=OperationTypeID*/
      FIND FIRST GateUser      OF LocationSizeHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/

      LocationSizeHistoryBrowse:startRow (LocationSizeHistory.LocationSizeHistoryID, 
         "selectLocationSizeHistoryRow(this," + '"' + STRING(LocationSizeHistory.LocationSizeHistoryID) + '"' + ");", "").    
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i LocationSizeHistory}
      
      LocationSizeHistoryBrowse:insertData(LocationSizeHistory.LocationSizeHistoryID,                        "").
      LocationSizeHistoryBrowse:insertData(LocationSizeHistory.LocationSizeID,                               "").
      LocationSizeHistoryBrowse:insertData(LocationSizeHistory.SizeName,                                 "LEFT").
      LocationSizeHistoryBrowse:insertData(STRING(LocationSizeHistory.Height, ">>>>9.999"),              "LEFT").
      LocationSizeHistoryBrowse:insertData(STRING(LocationSizeHistory.Width, ">>>>9.999"),               "LEFT").
      LocationSizeHistoryBrowse:insertData(STRING(LocationSizeHistory.Depth, ">>>>9.999"),               "LEFT").
      LocationSizeHistoryBrowse:insertData(STRING(LocationSizeHistory.Volume, ">>>>9.99999"),            "LEFT").
      LocationSizeHistoryBrowse:insertData(STRING(LocationSizeHistory.Active, "Yes/No"),                 "LEFT").
      LocationSizeHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""),       "LEFT").
      LocationSizeHistoryBrowse:insertData(fDisplayDate&Time(LocationSizeHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      LocationSizeHistoryBrowse:endRow().
   END. /* FOR EACH LocationSizeHistory */
   
   LocationSizeHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationSizeHistoryBrowse:getErrors().
   
   LocationSizeHistoryBrowseForm:insertHiddenField("popup_locationsizehistory_browse","").
   LocationSizeHistoryBrowseForm:insertHiddenField("LocationSizeHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationSizeHistoryBrowseForm}
   
   /* Create Button Bar */
   LocationSizeHistoryButtons = NEW buttonBar().
   
   LocationSizeHistoryButtons:addButton("locationsizehistory_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewLocationSizeHistoryDetails('locationsizehistory_details_form');",
                                        (IF intSelectedLocationSizeHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   LocationSizeHistoryButtons:addButton("locationsizehistory_browse_form_btn_cancel",
                                        fTL("Cancel"),
                                        "disablePopup('locationsizehistory_browse_form_popup');").
   LocationSizeHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationSizeHistoryBrowseForm:FormBrowse  = LocationSizeHistoryBrowse.
   LocationSizeHistoryBrowseForm:FormButtons = LocationSizeHistoryButtons.
   LocationSizeHistoryBrowseForm:endForm(). 
   
   LocationSizeHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


