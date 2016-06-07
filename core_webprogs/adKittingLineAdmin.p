&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adKittingLine.p 
  Description: admin file for the Kitting Line screen
  Author: Nick Diessner
  Created: 20/02/2015
  Revisions:
  
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
DEFINE VARIABLE intSelectedKittingLine                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrKittingLineRow                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToKittingLineRow               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrKittingLineID                        AS CHARACTER   NO-UNDO.

DEFINE VARIABLE intSelectedPostPick                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPostPickRow                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPostPickID                           AS CHARACTER   NO-UNDO.

DEFINE VARIABLE intSelectedKittingLineHistory           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrKittingLineHistoryRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrKittingLineHistoryID                 AS CHARACTER   NO-UNDO.

DEFINE VARIABLE intSelectedPostPickHistory              AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPostPickHistoryRow                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPostPickHistoryID                    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chrPopupDetails                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistoryBrowse                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistoryDetails                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupLocationBrowse                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPostPickDetails                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPostPickHistoryBrowse           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPostPickHistoryDetails          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupLineStationLinkBrowse           AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chrPopupAddKittingLinePostPickLinks     AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedKittingLinePostPickLinks     AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectKittingLinePostPickLinkRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToKittingLinePostPickLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrKittingLinePostPickLinksID           AS CHARACTER NO-UNDO.

DEFINE VARIABLE chrLocations                            AS CHARACTER NO-UNDO.
DEFINE VARIABLE intLocationCnt                          AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrPopupLinePostPickLinkBrowse          AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE KittingLineBrowseFrame              AS pageFrame.
DEFINE VARIABLE KittingLineBrowse                   AS browseTable.
DEFINE VARIABLE KittingLineBrowseButtons            AS buttonBar.
DEFINE VARIABLE KittingLineDetailsForm              AS dataForm.
DEFINE VARIABLE KittingLineDetailsButtons           AS buttonBar.

DEFINE VARIABLE KittingLineHistoryBrowseForm        AS dataform. 
DEFINE VARIABLE KittingLineHistoryBrowse            AS browseTable.
DEFINE VARIABLE KittingLineHistoryBrowseButtons     AS buttonBar.
DEFINE VARIABLE KittingLineHistoryDetailsForm       AS dataForm.
DEFINE VARIABLE KittingLineHistoryDetailsButtons    AS buttonBar.

DEFINE VARIABLE LocationBrowseForm                 AS dataform. 
DEFINE VARIABLE LocationBrowse                     AS browseTable.
DEFINE VARIABLE LocationBrowseButtons              AS buttonBar.
DEFINE VARIABLE PostPickDetailsForm                AS dataForm.
DEFINE VARIABLE PostPickDetailsButtons             AS buttonBar.

DEFINE VARIABLE PostPickHistoryBrowseForm          AS dataform. 
DEFINE VARIABLE PostPickHistoryBrowse              AS browseTable.
DEFINE VARIABLE PostPickHistoryBrowseButtons       AS buttonBar.
DEFINE VARIABLE PostPickHistoryDetailsForm         AS dataForm.
DEFINE VARIABLE PostPickHistoryDetailsButtons      AS buttonBar.

DEFINE VARIABLE KittingLinePostPickLinkBrowseForm    AS dataform. 
DEFINE VARIABLE KittingLinePostPickLinkBrowse        AS browseTable.
DEFINE VARIABLE KittingLinePostPickLinkBrowseButtons AS buttonBar.

DEFINE VARIABLE AddKittingLinePostPickLinkBrowseFrame   AS pageFrame.
DEFINE VARIABLE AddKittingLinePostPickLinkBrowseForm    AS dataForm.
DEFINE VARIABLE AddKittingLinePostPickLinkBrowse        AS browseTable.
DEFINE VARIABLE AddKittingLinePostPickLinkBrowseButtons AS buttonBar.

DEFINE VARIABLE KittingLineStationLinkBrowseFrame   AS pageFrame.
DEFINE VARIABLE KittingLineStationLinkBrowseForm    AS dataForm.
DEFINE VARIABLE KittingLineStationLinkBrowse        AS browseTable.
DEFINE VARIABLE KittingLineStationLinkBrowseButtons AS buttonBar.



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

&IF DEFINED(EXCLUDE-pAddShipOrderTypeStreamLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddShipOrderTypeStreamLinkBrowse Procedure
PROCEDURE pAddKittingLinePostPickLinkBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   AddKittingLinePostPickLinkBrowseForm            = NEW dataForm("addkittinglinepostpicklink_browse_form").
   AddKittingLinePostPickLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   AddKittingLinePostPickLinkBrowseForm:FormAction = "dbCreateKittingLinePostPickLinks.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   AddKittingLinePostPickLinkBrowseForm:FormWidth  = 700.
   AddKittingLinePostPickLinkBrowseForm:FormHeight = 490.
   AddKittingLinePostPickLinkBrowseForm:FormTitle  = fTL("Available WorkOrderPostPick Locations for Kitting Line ") +
                                                        (IF AVAILABLE KittingLine THEN " : " + KittingLine.LineName ELSE "").
   AddKittingLinePostPickLinkBrowseForm:FormType   = "xl_large".
   
   /* Form Data */
   AddKittingLinePostPickLinkBrowseForm:insertPaddingColumn(180).
   AddKittingLinePostPickLinkBrowseForm:insertColumn(110).
   AddKittingLinePostPickLinkBrowseForm:insertColumn(130).
   AddKittingLinePostPickLinkBrowseForm:insertColumn(40).
   AddKittingLinePostPickLinkBrowseForm:insertColumn(85).
   AddKittingLinePostPickLinkBrowseForm:insertColumn(130).
   
   AddKittingLinePostPickLinkBrowseForm:startRow().
   AddKittingLinePostPickLinkBrowseForm:insertBlankRow(10). 

   AddKittingLinePostPickLinkBrowse              = NEW browseTable("addkittinglinepostpicklink_browse").
   AddKittingLinePostPickLinkBrowse:BrowseWidth  = 680.
   AddKittingLinePostPickLinkBrowse:BrowseHeight = 442.
   
   AddKittingLinePostPickLinkBrowse:insertColumn(fTL("Location ID"),                80, "INTEGER", FALSE).
   AddKittingLinePostPickLinkBrowse:insertColumn(fTL("WorkOrderPostpick Location"), 120, "CHARACTER", "left", FALSE).
   AddKittingLinePostPickLinkBrowse:insertColumn(fTL("Active"),                     80, "CHARACTER", FALSE).
   
   AddKittingLinePostPickLinkBrowse:StartBody().
   FIND FIRST LocationType NO-LOCK
         WHERE LocationType.TypeCode = "WorkOrderPostPick".
   PostPickKittingLineLinkLoop:
  
      FOR EACH Location NO-LOCK /* idx=ShipOrderTypeID */
         WHERE Location.LocationTypeID = LocationType.LocationTypeID
         BY Location.LocationID:

         IF CAN-FIND(FIRST PostpickKittingLineLink OF Location NO-LOCK
                        WHERE PostpickKittingLineLink.LocationID = Location.LocationID /* idx ShipOrderTypeID  */
                          AND PostpickKittingLineLink.KittingLineID = INTEGER(chrKittingLineID) ) THEN /* idx ShipOrderStreamID*/
                               NEXT PostpickKittingLineLinkLoop.

         RUN pSetKittingLineRows.

      END.

   IF intLocationCnt <= 2000 THEN
     chrLocations  = TRIM(chrLocations, ",").
   ELSE
     chrLocations = "".

   AddKittingLinePostPickLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + AddKittingLinePostPickLinkBrowse:getErrors().
   
   AddKittingLinePostPickLinkBrowseForm:insertHiddenField("LocationList","").
   AddKittingLinePostPickLinkBrowseForm:insertHiddenField("KittingLineID",chrKittingLineID).
   AddKittingLinePostPickLinkBrowseForm:insertHiddenField("KittingLinePostPickLinkID",chrKittingLinePostPickLinksID).
   AddKittingLinePostPickLinkBrowseForm:insertHiddenField("KittingLinePostPickLinkVersionID","").
   AddKittingLinePostPickLinkBrowseForm:insertHiddenField("popup_kittingline_postpick_browse_form","").
   AddKittingLinePostPickLinkBrowseForm:insertHiddenField("popup_addkittinglinepostpicklink_browse","").
   AddKittingLinePostPickLinkBrowseForm:insertHiddenField("form_name","addkittinglinepostpicklink_browse_form").
   AddKittingLinePostpickLinkBrowseForm:insertHiddenField("prog_name","adKittingLineAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddKittingLinePostPickLinkBrowseForm}
   
   /* Create Button Bar */
   AddKittingLinePostPickLinkBrowseButtons = NEW buttonBar().
   
   AddKittingLinePostPickLinkBrowseButtons:addButton("addkittinglinepostpicklink_browse_form_btn_selectall",
                                                    fTL("Select All"),
                                                    "selectAllLocations('" + STRING(chrLocations) + "','" + STRING(intLocationCnt) + "')").

   AddKittingLinePostPickLinkBrowseButtons:addButton("addkittinglinepostpicklink_browse_form_btn_create",
                                                    fTL("Confirm Links"),
                                                    "createKittingLinePostPickLinks();"). 
   
   AddKittingLinePostPickLinkBrowseButtons:addButton("addkittinglinepostpicklink_browse_form_btn_delete",
                                                    fTL("Clear Selection"),
                                                    "deselectAllLocationRows()").
   
   AddKittingLinePostPickLinkBrowseButtons:addButton("addkittinglinepostpicklink_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('addkittinglinepostpicklink_browse_form_popup');").
   
   AddKittingLinePostPickLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddKittingLinePostPickLinkBrowseForm:FormBrowse  = AddKittingLinePostPickLinkBrowse.
   AddKittingLinePostPickLinkBrowseForm:FormButtons = AddKittingLinePostPickLinkBrowseButtons.
   AddKittingLinePostPickLinkBrowseForm:endForm(). 
   
   AddKittingLinePostPickLinkBrowseForm:displayForm().
   


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
   
   ASSIGN chrPostPickID                        = get-value("PostPickID")
          intSelectedPostPick                  = INTEGER(chrPostPickID)
          chrKittingLineID                     = get-value("KittingLineID")
          intSelectedKittingLine               = INTEGER(chrKittingLineID)
          chrPostPickHistoryID                 = get-value("PostPickHistoryID")
          intSelectedPostPickHistory           = INTEGER(chrPostPickHistoryID)
          chrKittingLineHistoryID              = get-value("KittingLineHistoryID")
          intSelectedKittingLineHistory        = INTEGER(chrKittingLineHistoryID)
          chrKittingLinePostPickLinksID        = get-value("KittingLinePostPickLinkID")
          intSelectedKittingLinePostPickLinks  = INTEGER(chrKittingLinePostPickLinksID)
          chrScrollToKittingLineRow            = STRING(INTEGER(get-value("kittingline_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrKittingLineID <> "" THEN
      chrKittingLineRow = 'selectKittingLineRow(document.getElementById("kittingline_browse_row_' + chrKittingLineID + '"),"' 
                                                          + chrKittingLineID +  '");'.
   IF chrPostPickID <> "" THEN
      chrPostPickRow = 'selectPostPickRow(document.getElementById("kittingline_postpick_browse_row_' + chrPostPickID + '"),"' 
                                                          + chrPostPickID +  '");'.
                                                          
   IF chrKittingLinePostPickLinksID <> "" THEN
      chrSelectKittingLinePostPickLinkRow = 'selectKittingLinePostPickLinkRow(document.getElementById("countgrouplocationlink_browse_row_' + chrKittingLinePostPickLinksID + '"),"'
                                                              + chrKittingLinePostPickLinksID + '");'.                                                       
                                                          
   IF get-value('popup_addkittinglinepostpicklink_browse') = "yes" THEN
      chrPopupAddKittingLinePostPickLinks = 'enablePopup("addkittinglinepostpicklink_browse_form_popup");'.                                                       
                                                          
   IF get-value('popup_kittingline_details_form') = "yes" THEN
      chrPopupDetails = 'enablePopup("kittingline_details_form_popup");'. 

   IF get-value('popup_kittingline_history_browse_form') = "yes" THEN
      chrPopupHistoryBrowse = 'enablePopup("kittingline_history_browse_form_popup");'. 

   IF get-value('popup_kittingline_history_details_form') = "yes" THEN
      chrPopupHistoryDetails = 'enablePopup("kittingline_history_details_form_popup");'. 
   
   IF get-value('popup_kittingline_postpick_browse_form') = "yes" THEN
      chrPopupLocationBrowse = 'enablePopup("kittingline_postpick_browse_form_popup");'. 
       
/*   IF get-value('popup_kittingline_postpick_details_form') = "yes" THEN                   */
/*      chrPopupPostPickDetails = 'enablePopup("kittingline_postpick_details_form_popup");'.*/
/*                                                                                          */
/*   IF get-value('popup_kittingline_postpick_history_browse_form') = "yes" THEN                         */
/*      chrPopupPostPickHistoryBrowse = 'enablePopup("kittingline_postpick_history_browse_form_popup");'.*/
       
/*   IF get-value('popup_kittingline_postpick_history_details_form') = "yes" THEN                          */
/*      chrPopupPostPickHistoryDetails = 'enablePopup("kittingline_postpick_history_details_form_popup");'.*/
      
   IF get-value('popup_kittingline_station_browse_form') = "yes" THEN
      chrPopupLineStationLinkBrowse = 'enablePopup("kittingline_station_browse_form_popup");'.   
                                                        
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("kittingline_browse").scrollTop=' + chrScrollToKittingLineRow 
                                                           + chrPopupDetails + chrKittingLineRow + chrPostPickRow
                                                           + chrPopupHistoryBrowse + chrPopupHistoryDetails 
                                                           + chrPopupLocationBrowse + chrPopupAddKittingLinePostPickLinks
                                                           + chrPopupLineStationLinkBrowse. 
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Kitting Line Admin".
   ThisPage:FrameTitle = "Kitting Line Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Line Admin */
   ThisPage:addJavaScript("kittingline.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pKittingLineBrowse.

   FIND FIRST KittingLine NO-LOCK
      WHERE KittingLine.KittingLineID = intSelectedKittingLine NO-ERROR.
   
   /******* Popup Browsers and Forms ********/    
   RUN pKittingLineDetails.
   
   RUN pKittingLineHistoryBrowse.
   RUN pKittingLineHistoryDetails.
   
   RUN pLocationBrowse.
   RUN pPostPickDetails.
   
   RUN pPostPickHistoryBrowse.
   RUN pPostPickHistoryDetails.
   
   RUN pAddKittingLinePostPickLinkBrowse.
   
   FIND FIRST KittingLine NO-LOCK
      WHERE KittingLine.KittingLineID = intSelectedKittingLine NO-ERROR.
   
   RUN pKittingLineStationLinkBrowse.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT KittingLineBrowseFrame              NO-ERROR.
   DELETE OBJECT KittingLineBrowse                   NO-ERROR.
   DELETE OBJECT KittingLineBrowseButtons            NO-ERROR.
   DELETE OBJECT KittingLineDetailsForm              NO-ERROR.
   DELETE OBJECT KittingLineDetailsButtons           NO-ERROR.
   
   DELETE OBJECT KittingLineHistoryBrowseForm        NO-ERROR. 
   DELETE OBJECT KittingLineHistoryBrowse            NO-ERROR.
   DELETE OBJECT KittingLineHistoryBrowseButtons     NO-ERROR.
   DELETE OBJECT KittingLineHistoryDetailsForm       NO-ERROR.
   DELETE OBJECT KittingLineHistoryDetailsButtons    NO-ERROR.
   
   DELETE OBJECT LocationBrowseForm                 NO-ERROR. 
   DELETE OBJECT LocationBrowse                     NO-ERROR.
   DELETE OBJECT LocationBrowseButtons              NO-ERROR.
   DELETE OBJECT PostPickDetailsForm                NO-ERROR.
   DELETE OBJECT PostPickDetailsButtons             NO-ERROR.
   
   DELETE OBJECT AddKittingLinePostPickLinkBrowseFrame   NO-ERROR.
   DELETE OBJECT AddKittingLinePostPickLinkBrowseForm    NO-ERROR.
   DELETE OBJECT AddKittingLinePostPickLinkBrowse        NO-ERROR.
   DELETE OBJECT AddKittingLinePostPickLinkBrowseButtons NO-ERROR.
   
   DELETE OBJECT PostPickHistoryBrowseForm          NO-ERROR. 
   DELETE OBJECT PostPickHistoryBrowse              NO-ERROR.
   DELETE OBJECT PostPickHistoryBrowseButtons       NO-ERROR.
   DELETE OBJECT PostPickHistoryDetailsForm         NO-ERROR.
   DELETE OBJECT PostPickHistoryDetailsButtons      NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetShipOrderStreamRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetShipOrderStreamRows Procedure
PROCEDURE pSetKittingLineRows:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   AddKittingLinePostPickLinkBrowse:startRow(Location.LocationID,
                                  'multiSelectStreamRow("' +
                                  STRING(Location.LocationID) + '")', "").

   AddKittingLinePostPickLinkBrowse:insertData(STRING(Location.LocationID), "left").
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalBrowseFields.i KittingLine}

   AddKittingLinePostPickLinkBrowse:insertData(Location.LocationRef,"left").
   AddKittingLinePostPickLinkBrowse:insertData(STRING(Location.Active,"Yes/No")).

   /* Add hidden fields */
   AddKittingLinePostPickLinkBrowse:insertHiddenData("PostPickLocationID",Location.LocationID).
   AddKittingLinePostPickLinkBrowse:insertHiddenData("PostPickLocationVersionID",Location.VersionID).

   AddKittingLinePostPickLinkBrowse:endRow().

   /* Count the Locations */
   intLocationCnt = intLocationCnt + 1.

   IF intLocationCnt <= 2000 THEN
      chrLocations = chrLocations + STRING(Location.LocationID) + ",".

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderStreamPackOutLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderStreamPackOutLinkBrowse Procedure
PROCEDURE pKittingLineStationLinkBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "kittingline_station_browse_form"}
   
   KittingLineStationLinkBrowseForm           = NEW dataForm("kittingline_station_browse_form").
   KittingLineStationLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   KittingLineStationLinkBrowseForm:FormWidth  = 860.
   KittingLineStationLinkBrowseForm:FormHeight = 530.
   KittingLineStationLinkBrowseForm:FormTitle  = fTL("Kitting Line Stations")+
                                                        (IF AVAILABLE KittingLine THEN " : " + KittingLine.LineName ELSE "").
   KittingLineStationLinkBrowseForm:FormType   = "xxl_large".
   KittingLineStationLinkBrowse                = NEW browseTable("kittingline_station_browse").
   KittingLineStationLinkBrowse:BrowseWidth    = 840.
   KittingLineStationLinkBrowse:BrowseHeight   = 490.
   KittingLineStationLinkBrowse:ExcelExport    = TRUE.
   KittingLineStationLinkBrowse:SessionID      = intGblSessionID.
   KittingLineStationLinkBrowse:WebStream      = STREAM WebStream:HANDLE.
   
   KittingLineStationLinkBrowse:insertColumn(fTL("Station ID"),           90, "INTEGER",           FALSE).
   KittingLineStationLinkBrowse:insertColumn(fTL("Station Name"),        150, "INTEGER",           FALSE).
   KittingLineStationLinkBrowse:insertColumn(fTL("Location"),            200, "CHARACTER", "LEFT", FALSE).   
   KittingLineStationLinkBrowse:insertColumn(fTL("Active"),              100, "LOGICAL",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i KittingLineStationLink}
   
   
   KittingLineStationLinkBrowse:StartBody().
   
   FOR EACH KittingStation NO-LOCK
      WHERE KittingStation.KittingLineID = intSelectedKittingLine:
         
         
      FIND FIRST Location OF KittingStation NO-LOCK NO-ERROR.

      KittingLineStationLinkBrowse:startRow(KittingStation.KittingStationID, "selectKittingLineStationRow(this," + '"' + STRING(KittingStation.KittingStationID) + '"' + ");", "").

      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i KittingLine}

      KittingLineStationLinkBrowse:insertData(KittingStation.KittingStationID).
      KittingLineStationLinkBrowse:insertData(KittingStation.StationName).
      KittingLineStationLinkBrowse:insertData(IF AVAILABLE Location THEN Location.LocationRef ELSE "", "LEFT").
      KittingLineStationLinkBrowse:insertData(KittingStation.Active,"Yes/No").

      /* Hidden row fields */
      KittingLineStationLinkBrowse:insertHiddenData("KittingStationID",KittingStation.KittingStationID).

      KittingLineStationLinkBrowse:endRow().
      END. /* FOR EACH ShipOrderStreamPackoutLink */

   KittingLineStationLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + KittingLineStationLinkBrowse:getErrors().
   
   /* Hidden Fields */
   KittingLineStationLinkBrowseForm:insertHiddenField("kittingline_browse_scroll","").
   KittingLineStationLinkBrowseForm:insertHiddenField("KittingLineStationLinkID","").

   KittingLineStationLinkBrowseForm:insertHiddenField("popup_kittingline_station_browse_form","").
      
   KittingLineStationLinkBrowseForm:insertHiddenField("form_name","kittingline_station_browse_form").
   KittingLineStationLinkBrowseForm:insertHiddenField("prog_name","adKittingLineAdmin.p").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingLineStationLinkBrowseForm}
   
   /* Create Button Bar */
   KittingLineStationLinkBrowseButtons = NEW buttonBar().
   
   KittingLineStationLinkBrowseButtons:addButton("kittingline_station_browse_form_btn_cancel",
                                                     fTL("Cancel"),
                                                     "disablePopup('kittingline_station_browse_form_popup');").
   
   KittingLineStationLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingLineStationLinkBrowseForm:FormBrowse  = KittingLineStationLinkBrowse.
   KittingLineStationLinkBrowseForm:FormButtons = KittingLineStationLinkBrowseButtons.
   KittingLineStationLinkBrowseForm:endForm(). 
   
   KittingLineStationLinkBrowseForm:displayForm().  

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderTypeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderTypeBrowse Procedure
PROCEDURE pLocationBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "kittingline_postpick_browse_form"}
   
   LocationBrowseForm           = NEW dataForm("kittingline_postpick_browse_form").
   LocationBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   LocationBrowseForm:FormWidth  = 860.
   LocationBrowseForm:FormHeight = 530.
   LocationBrowseForm:FormTitle  = fTL("Work Order Post Pick Locations Linked to Kitting Line") +
                                           (IF AVAILABLE KittingLine THEN " : " + KittingLine.LineName ELSE ""). 
                                          
   LocationBrowseForm:FormType   = "xxl_large".
   LocationBrowseForm:FormAction = "dbSetKittingLinePostPickLinkActive.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LocationBrowse                 = NEW browseTable("kittingline_postpick_browse").
   LocationBrowse:BrowseWidth    = 840.
   LocationBrowse:BrowseHeight   = 490.
   LocationBrowse:ExcelExport    = TRUE.
   LocationBrowse:SessionID      = intGblSessionID.
   LocationBrowse:WebStream      = STREAM WebStream:HANDLE.
   
   LocationBrowse:insertColumn(fTL("Link ID"),                    80, "INTEGER", FALSE).
   LocationBrowse:insertColumn(fTL("WorkOrderPostPick Location"), 600, "CHARACTER","LEFT", FALSE).
   LocationBrowse:insertColumn(fTL("Active"),                     90, "LOGICAL", FALSE).
   
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Location}/*Change to Location and PostPickBrowse to LocationBrowse*/
   
   LocationBrowse:StartBody().

   FOR EACH PostPickKittingLineLink NO-LOCK  /*idx=KittingLineID*/
      WHERE PostPickKittingLineLink.KittingLineID = KittingLine.KittingLineID,
         EACH  Location OF PostPickKittingLineLink NO-LOCK: /*idx=PostPickLocationID*/

      LocationBrowse:startRow(PostPickKittingLineLink.PostPickKittingLineLinkID, "selectKittingLinePostPickLinkRow(this," + '"'
                                                            + STRING(PostPickKittingLineLink.PostPickKittingLineLinkID) + '"' + ");", "").
      LocationBrowse:insertData(PostPickKittingLineLink.PostPickKittingLineLinkID).

      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i Location}/*SAME*/

      LocationBrowse:insertData(Location.LocationRef, "LEFT").
      LocationBrowse:insertData(STRING(PostPickKittingLineLink.Active, "Yes/No")).

      LocationBrowse:insertHiddenData("KittingLineVersionID",KittingLine.VersionID).
      LocationBrowse:insertHiddenData("KittingLinePostPickID",PostPickKittingLineLink.PostPickKittingLineLinkID).
      LocationBrowse:insertHiddenData("KittingLinePostPickLinkVersionID",PostPickKittingLineLink.VersionID).
      LocationBrowse:insertHiddenData("PostPickLocationID",Location.LocationID).
      LocationBrowse:insertHiddenData("PostPickLocationVersionID",Location.VersionID).
      LocationBrowse:insertHiddenData("LinkActive",PostPickKittingLineLink.Active).

      LocationBrowse:endRow().
   END. /* FOR EACH PostPick */

   LocationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationBrowse:getErrors().
   
   /* Hidden Fields */
   LocationBrowseForm:insertHiddenField("kittingline_browse_scroll","").
   LocationBrowseForm:insertHiddenField("KittingLineID",chrKittingLineID).
   LocationBrowseForm:insertHiddenField("KittingLineVersionID","").
   LocationBrowseForm:insertHiddenField("KittingLinePostPickLinkID",chrKittingLinePostPickLinksID).
   LocationBrowseForm:insertHiddenField("KittingLinePostPickLinkVersionID","").
   LocationBrowseForm:insertHiddenField("PostPickLocationID","").
   LocationBrowseForm:insertHiddenField("PostPickLocationVersionID","").
   LocationBrowseForm:insertHiddenField("KittingLinePostPickLinkActive","").
   
   LocationBrowseForm:insertHiddenField("popup_kittingline_postpick_browse_form","yes").
   LocationBrowseForm:insertHiddenField("popup_kittingline_postpick_details_form","").
   LocationBrowseForm:insertHiddenField("popup_addkittinglinepostpicklink_browse","").
   LocationBrowseForm:insertHiddenField("popup_kittingline_postpick_history_browse_form","").
   LocationBrowseForm:insertHiddenField("popup_kittingline_postpick_history_details_form","").
      
   LocationBrowseForm:insertHiddenField("form_name","kittingline_postpick_browse_form").
   LocationBrowseForm:insertHiddenField("prog_name","adKittingLineAdmin.p").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationBrowseForm}
   
   /* Create Button Bar */
   LocationBrowseButtons = NEW buttonBar().
   
   LocationBrowseButtons:addButton("kittingline_postpick_browse_form_btn_add",
                                        fTL("Add Links"),
                                        "addKittingLinePostPickLink();").

   LocationBrowseButtons:addButton("kittingline_postpick_browse_form_btn_deactivatelink",
                                        fTL("Deactivate Link"),
                                        "setKittingLinePostPickLinkActiveFlag('kittingline_postpick_browse_form');",
                                        (IF intSelectedPostPick > 0 THEN "" ELSE "Disabled")).
                                         
/*   ShipOrderTypeBrowseButtons:addButton("shiporderstream_type_browse_form_btn_history",                 */
/*                                        fTL("History"),                                                  */
/*                                        "viewShipOrderStreamPackOut('shiporderstream_type_browse_form')",*/
/*                                        (IF intSelectedShipOrderType > 0 THEN "" ELSE "Disabled")).    */
   
   LocationBrowseButtons:addButton("kittingline_postpick_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('kittingline_postpick_browse_form_popup');",
                                         "").
                         
   LocationBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationBrowseForm:FormBrowse  = LocationBrowse.
   LocationBrowseForm:FormButtons = LocationBrowseButtons.
   LocationBrowseForm:endForm(). 
   
   LocationBrowseForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderTypeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderTypeDetails Procedure
PROCEDURE pPostPickDetails:
/*/*------------------------------------------------------------------------------                                                                   */
/* Purpose:                                                                                                                                          */
/* Notes:                                                                                                                                            */
/*------------------------------------------------------------------------------*/                                                                   */
/*   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */                  */
/*   {webGetWebForm.i "kittingline_postpick_details_form"}                                                                                           */
/*                                                                                                                                                   */
/*   ASSIGN chrDisplayFieldList  = ""                                                                                                                */
/*          chrEditFieldList     = ""                                                                                                                */
/*          chrNewFieldList      = ""                                                                                                                */
/*          chrRequiredFieldList = ""                                                                                                                */
/*          chrExtraFieldList    = ""                                                                                                                */
/*          chrValidateFieldList = "".                                                                                                               */
/*                                                                                                                                                   */
/*   PostPickDetailsForm = NEW dataForm("kittingline_postpick_details_form").                                                                        */
/*   PostPickDetailsForm:WebStream = STREAM WebStream:HANDLE.                                                                                        */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:FormAction = "dbPostPickLocationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").                                              */
/*                                                                                                                                                   */
/*   /* Setup */                                                                                                                                     */
/*   PostPickDetailsForm:FormWidth  = 460.                                                                                                           */
/*   PostPickDetailsForm:FormHeight = 300.                                                                                                           */
/*   PostPickDetailsForm:FormTitle  = "PostPick Location Details".                                                                                   */
/*   PostPickDetailsForm:FormType   = "medium".                                                                                                      */
/*                                                                                                                                                   */
/*   /* Column Layout */                                                                                                                             */
/*   PostPickDetailsForm:insertPaddingColumn(30).                                                                                                    */
/*   PostPickDetailsForm:insertColumn(150).                                                                                                          */
/*   PostPickDetailsForm:insertColumn(200).                                                                                                          */
/*                                                                                                                                                   */
/*   /* Fields */                                                                                                                                    */
/*   PostPickDetailsForm:startRow().                                                                                                                 */
/*   PostPickDetailsForm:insertLabel(fTL("PostPick Location ID")).                                                                                   */
/*   PostPickDetailsForm:insertTextField("LocationID", "", 100, TRUE).                                                                               */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:startRow().                                                                                                                 */
/*   PostPickDetailsForm:insertLabel(fTL("PostPick Code")).                                                                                          */
/*   PostPickDetailsForm:insertTextField("LocationCode", "", 190, TRUE).                                                                             */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:startRow().                                                                                                                 */
/*   PostPickDetailsForm:insertLabel(fTL("PostPick Name")).                                                                                          */
/*   PostPickDetailsForm:insertTextField("TypeName", "", 190, TRUE).                                                                                 */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:startRow().                                                                                                                 */
/*   PostPickDetailsForm:insertLabel(fTL("PostPick Descr")).                                                                                         */
/*   PostPickDetailsForm:insertTextAreaField("TypeDescr", "", 190, TRUE).                                                                            */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:startRow().                                                                                                                 */
/*   PostPickDetailsForm:insertLabel(fTL("Active")).                                                                                                 */
/*   PostPickDetailsForm:insertComboField("Active", "", 100, TRUE).                                                                                  */
/*   PostPickDetailsForm:insertComboPairs("Active", "yes", "Active").                                                                                */
/*   PostPickDetailsForm:insertComboPairs("Active", "no",  "Not Active").                                                                            */
/*                                                                                                                                                   */
/*/*   ShipOrderTypeDetailsForm:startRow().                                                                                                     */   */
/*/*   ShipOrderTypeDetailsForm:insertLabel(fTL("Listing Seq")).                                                                                */   */
/*/*   ShipOrderTypeDetailsForm:insertTextField("ListingSequence", "", 190, TRUE).                                                              */   */
/*/*                                                                                                                                            */   */
/*/*   ShipOrderTypeDetailsForm:startRow().                                                                                                     */   */
/*/*   ShipOrderTypeDetailsForm:insertLabel(fTL("ShipOrder Stream")).                                                                           */   */
/*/*   ShipOrderTypeDetailsForm:insertComboField("ShipOrderStreamID", "", 100, TRUE).                                                           */   */
/*/*   FOR EACH ShipOrderStream NO-LOCK                                                                                                         */   */
/*/*      BY ShipOrderStream.StreamName:                                                                                                        */   */
/*/*      ShipOrderTypeDetailsForm:insertComboPairs("ShipOrderStreamID", STRING(ShipOrderStream.ShipOrderStreamID), ShipOrderStream.StreamName).*/   */
/*/*   END.                                                                                                                                     */   */
/*/*                                                                                                                                            */   */
/*   {webGetOptionalFormFields.i pPostPickDetailsFields}                                                                                             */
/*                                                                                                                                                   */
/*   chrExtraFieldList = TRIM(chrExtraFieldList,",").                                                                                                */
/*                                                                                                                                                   */
/*   /* Add Hidden Fields*/                                                                                                                          */
/*   PostPickDetailsForm:insertHiddenField("kittingline_browse_scroll", "").                                                                         */
/*   PostPickDetailsForm:insertHiddenField("KittingLineID","").                                                                                      */
/*   PostPickDetailsForm:insertHiddenField("KittingLineVersionID","").                                                                               */
/*   PostPickDetailsForm:insertHiddenField("PostPickLocationID","").                                                                                 */
/*   PostPickDetailsForm:insertHiddenField("PostPickLocationVersionID","").                                                                          */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:insertHiddenField("popup_kittingline_postpick_browse_form","yes").                                                          */
/*   PostPickDetailsForm:insertHiddenField("popup_kittingline_postpick_details_form","").                                                            */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:insertHiddenField("form_name", "kittingline_postpick_details_form").                                                        */
/*   PostPickDetailsForm:insertHiddenField("prog_name", "adKittingLineAdmin.p").                                                                     */
/*                                                                                                                                                   */
/*   /* This adds all of the standard form field lists to the form */                                                                                */
/*   {webGetHiddenFormFields.i PostPickDetailsForm}                                                                                                  */
/*                                                                                                                                                   */
/*   /* Create Button Bar */                                                                                                                         */
/*   PostPickDetailsButtons = NEW buttonBar().                                                                                                       */
/*   PostPickDetailsButtons:addButton("kittingline_postpick_details_form_btn_save",                                                                  */
/*                                         fTL("Save"),                                                                                              */
/*                                         "updateKittingLine('kittingline_postpick_details_form')").                                                */
/*                                                                                                                                                   */
/*   PostPickDetailsButtons:addButton("kittingline_postpick_details_form_btn_cancel",                                                                */
/*                                         fTL("Cancel"),                                                                                            */
/*                                         "cancelUpdate('UserCancelled','process_mode'); disablePopup('kittingline_postpick_details_form_popup');").*/
/*                                                                                                                                                   */
/*   PostPickDetailsButtons:closeBar().                                                                                                              */
/*                                                                                                                                                   */
/*   /* Assign the Button Bar Object to the Form Object */                                                                                           */
/*   PostPickDetailsForm:FormButtons = PostPickDetailsButtons.                                                                                       */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:endForm().                                                                                                                  */
/*                                                                                                                                                   */
/*   PostPickDetailsForm:displayForm().                                                                                                              */
/*                                                                                                                                                   */
/*   /*   chrPageBuildError = chrPageBuildError + ShipOrderTypeDetailsForm:getErrors().  */                                                          */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderTypeHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderTypeHistoryBrowse Procedure
PROCEDURE pPostPickHistoryBrowse:
/*/*------------------------------------------------------------------------------                                                                                                */
/* Purpose:                                                                                                                                                                       */
/* Notes:                                                                                                                                                                         */
/*------------------------------------------------------------------------------*/                                                                                                */
/*   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */                                                             */
/*   {webGetWebForm.i "kittingline_postpick_history_browse_form"}                                                                                                                 */
/*                                                                                                                                                                                */
/*   PostPickHistoryDetailsForm           = NEW dataForm("kittingline_postpick_history_browse_form").                                                                             */
/*   PostPickHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.                                                                                                              */
/*                                                                                                                                                                                */
/*   /* Setup */                                                                                                                                                                  */
/*   PostPickHistoryDetailsForm:FormWidth  = 700.                                                                                                                                 */
/*   PostPickHistoryDetailsForm:FormHeight = 490.                                                                                                                                 */
/*   PostPickHistoryDetailsForm:FormTitle  = fTL("PostPick Location History").                                                                                                    */
/*   PostPickHistoryDetailsForm:FormType   = "xl_large".                                                                                                                          */
/*   PostPickHistoryBrowse                 = NEW browseTable("kittingline_postpick_history_browse").                                                                              */
/*   PostPickHistoryBrowse:BrowseWidth     = 680.                                                                                                                                 */
/*   PostPickHistoryBrowse:BrowseHeight    = 450.                                                                                                                                 */
/*   PostPickHistoryBrowse:ExcelExport     = TRUE.                                                                                                                                */
/*   PostPickHistoryBrowse:SessionID       = intGblSessionID.                                                                                                                     */
/*   PostPickHistoryBrowse:WebStream       = STREAM WebStream:HANDLE.                                                                                                             */
/*                                                                                                                                                                                */
/*   PostPickHistoryBrowse:insertColumn(fTL("Hist ID"),     90, "INTEGER",           FALSE).                                                                                      */
/*   PostPickHistoryBrowse:insertColumn(fTL("Location Name"),  120, "CHARACTER", "LEFT", FALSE).                                                                                  */
/*   PostPickHistoryBrowse:insertColumn(fTL("Location Descr"), 180, "CHARACTER", "LEFT", FALSE).                                                                                  */
/*   PostPickHistoryBrowse:insertColumn(fTL("Created"),    120, "CHARACTER", "LEFT", FALSE).                                                                                      */
/*   PostPickHistoryBrowse:insertColumn(fTL("Active"),     100, "LOGICAL",           FALSE).                                                                                      */
/*                                                                                                                                                                                */
/*   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */                                                         */
/*   {webGetOptionalBrowseHeaders.i PostPickHistory}                                                                                                                              */
/*                                                                                                                                                                                */
/*                                                                                                                                                                                */
/*   PostPickHistoryBrowse:StartBody().                                                                                                                                           */
/*                                                                                                                                                                                */
/*/*   FOR EACH ShipOrderTypeStreamLinkHist NO-LOCK  /*idx=ShipOrderTypeStreamLinkHistID*/                                                                                      */*/
/*/*      WHERE ShipOrderTypeStreamLinkHist.ShipOrderTypeStreamLinkHistID = intSelectedShipOrderTypeStreamLink                                                                  */*/
/*/*      BY    ShipOrderTypeStreamLinkHist.ShipOrderTypeStreamLinkHistID DESCENDING:                                                                                           */*/
/*/*                                                                                                                                                                            */*/
/*/*      FIND FIRST GateUser OF ShipOrderTypeStreamLinkHist NO-LOCK NO-ERROR.                                                                                                  */*/
/*/*                                                                                                                                                                            */*/
/*/*      ShipOrderTypeHistoryBrowse:startRow(ShipOrderTypeStreamLinkHist.ShipOrderTypeStreamLinkHistID,                                                                        */*/
/*/*                                        "selectShipOrderStreamHistoryRow(this," + '"' + STRING(ShipOrderTypeStreamLinkHist.ShipOrderTypeStreamLinkHistID) + '"' + ");", "").*/*/
/*/*                                                                                                                                                                            */*/
/*/*      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */                                               */*/
/*/*      {webGetOptionalBrowseFields.i ShipOrderTypeHistory}                                                                                                                   */*/
/*/*                                                                                                                                                                            */*/
/*/*      ShipOrderTypeHistoryBrowse:insertData(ShipOrderTypeStreamLinkHist.ShipOrderTypeStreamLinkHistID).                                                                     */*/
/*/*      ShipOrderTypeHistoryBrowse:insertData(ShipOrderTypeStreamLinkHist.ListingSequence, "LEFT").                                                                           */*/
/*/*      ShipOrderTypeHistoryBrowse:insertData(ShipOrderStreamHistory.StreamDescr,"LEFT").                                                                                     */*/
/*/*      ShipOrderTypeHistoryBrowse:insertData(fDisplayDate&Time(ShipOrderTypeStreamLinkHist.Created,"y/m/d H:M:S"), "LEFT").                                                  */*/
/*/*      ShipOrderTypeHistoryBrowse:insertData(ShipOrderTypeStreamLinkHist.Active,"Yes/No").                                                                                   */*/
/*/*                                                                                                                                                                            */*/
/*/*      /* Hidden row fields */                                                                                                                                               */*/
/*/*      ShipOrderTypeHistoryBrowse:insertHiddenData("ShipOrderTypeHistoryID",ShipOrderTypeHistory.ShipOrderTypeHistoryID).                                                    */*/
/*/*                                                                                                                                                                            */*/
/*/*      ShipOrderTypeHistoryBrowse:endRow().                                                                                                                                  */*/
/*/*      END. /* FOR EACH ShipOrderTypeHistory */                                                                                                                              */*/
/*                                                                                                                                                                                */
/*   PostPickHistoryBrowse:endTable().                                                                                                                                            */
/*   chrPageBuildError = chrPageBuildError + PostPickHistoryBrowse:getErrors().                                                                                                   */
/*                                                                                                                                                                                */
/*   /* Hidden Fields */                                                                                                                                                          */
/*   PostPickHistoryDetailsForm:insertHiddenField("kittingline_browse_scroll","").                                                                                                */
/*   PostPickHistoryDetailsForm:insertHiddenField("KittingLineID","").                                                                                                            */
/*   PostPickHistoryDetailsForm:insertHiddenField("KittingLineVersionID","").                                                                                                     */
/*   PostPickHistoryDetailsForm:insertHiddenField("PostPickLocationID","").                                                                                                       */
/*   PostPickHistoryDetailsForm:insertHiddenField("PostPickLocationVersionID","").                                                                                                */
/*   PostPickHistoryDetailsForm:insertHiddenField("PostPickLocationHistoryID","").                                                                                                */
/*                                                                                                                                                                                */
/*   PostPickHistoryDetailsForm:insertHiddenField("popup_kittingline_postpick_browse_form","").                                                                                   */
/*   PostPickHistoryDetailsForm:insertHiddenField("popup_kittingline_postpick_history_browse_form","").                                                                           */
/*   PostPickHistoryDetailsForm:insertHiddenField("popup_kittingline_postpick_history_details_form","").                                                                          */
/*                                                                                                                                                                                */
/*   PostPickHistoryDetailsForm:insertHiddenField("form_name","kittingline_postpick_history_browse_form").                                                                        */
/*   PostPickHistoryDetailsForm:insertHiddenField("prog_name","adKittingLineAdmin.p").                                                                                            */
/*                                                                                                                                                                                */
/*   /* This adds all of the standard form field lists to the form */                                                                                                             */
/*   {webGetHiddenFormFields.i PostPickHistoryDetailsForm}                                                                                                                        */
/*                                                                                                                                                                                */
/*   /* Create Button Bar */                                                                                                                                                      */
/*   PostPickHistoryBrowseButtons = NEW buttonBar().                                                                                                                              */
/*                                                                                                                                                                                */
/*   PostPickHistoryBrowseButtons:addButton("kittingline_postpick_history_browse_form_btn_details",                                                                               */
/*                                             fTL("Details"),                                                                                                                    */
/*                                             "viewKittingLineHistoryDetails('kittingline_postpick_history_details_form');",                                                     */
/*                                             (IF intSelectedPostPickHistory > 0 THEN "" ELSE "Disabled")).                                                                      */
/*                                                                                                                                                                                */
/*/*   ShipOrderTypeHistoryBrowseButtons:addButton("shiporderstream_type_history_browse_form_btn_excel",                                   */                                     */
/*/*                                             fTL("Excel Export"),                                                               */                                            */
/*/*                                             "excelExport('" + STRING(intGblSessionID) + "_shiporderstream_type_history_browse.xml')").*/                                     */
/*                                                                                                                                                                                */
/*   PostPickHistoryBrowseButtons:addButton("kittingline_postpick_history_browse_form_btn_cancel",                                                                                */
/*                                             fTL("Cancel"),                                                                                                                     */
/*                                             "disablePopup('kittingline_postpick_history_browse_form_popup');").                                                                */
/*                                                                                                                                                                                */
/*   PostPickHistoryBrowseButtons:closeBar().                                                                                                                                     */
/*                                                                                                                                                                                */
/*   /* Assign the Button Bar Object to the Form Object */                                                                                                                        */
/*   PostPickHistoryDetailsForm:FormBrowse  = PostPickHistoryBrowse.                                                                                                              */
/*   PostPickHistoryDetailsForm:FormButtons = PostPickHistoryBrowseButtons.                                                                                                       */
/*   PostPickHistoryDetailsForm:endForm().                                                                                                                                        */
/*                                                                                                                                                                                */
/*   PostPickHistoryDetailsForm:displayForm().                                                                                                                                    */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderTypeHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderTypeHistoryDetails Procedure
PROCEDURE pPostPickHistoryDetails:
/*/*------------------------------------------------------------------------------                                                             */
/* Purpose:                                                                                                                                    */
/* Notes:                                                                                                                                      */
/*------------------------------------------------------------------------------*/                                                             */
/*   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */                          */
/*   {webGetWebForm.i "kittingline_postpick_history_details_form"}                                                                             */
/*                                                                                                                                             */
/*   ASSIGN chrDisplayFieldList  = ""                                                                                                          */
/*          chrEditFieldList     = ""                                                                                                          */
/*          chrNewFieldList      = ""                                                                                                          */
/*          chrRequiredFieldList = ""                                                                                                          */
/*          chrExtraFieldList    = ""                                                                                                          */
/*          chrValidateFieldList = "".                                                                                                         */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm           = NEW dataForm("kittingline_postpick_history_details_form").                                         */
/*   PostPickHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.                                                                           */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:FormAction = "".                                                                                               */
/*                                                                                                                                             */
/*   /* Setup */                                                                                                                               */
/*   PostPickHistoryDetailsForm:FormWidth  = 580.                                                                                              */
/*   PostPickHistoryDetailsForm:FormHeight = 420.                                                                                              */
/*   PostPickHistoryDetailsForm:FormTitle  = "PostPick Location History Details".                                                              */
/*   PostPickHistoryDetailsForm:FormType   = "large".                                                                                          */
/*                                                                                                                                             */
/*   /* Column Layout */                                                                                                                       */
/*   PostPickHistoryDetailsForm:insertPaddingColumn(30).                                                                                       */
/*   PostPickHistoryDetailsForm:insertColumn(150).                                                                                             */
/*   PostPickHistoryDetailsForm:insertColumn(150).                                                                                             */
/*   PostPickHistoryDetailsForm:insertColumn(20).                                                                                              */
/*   PostPickHistoryDetailsForm:insertColumn(4).                                                                                               */
/*   PostPickHistoryDetailsForm:insertColumn(20).                                                                                              */
/*                                                                                                                                             */
/*   /* Fields */                                                                                                                              */
/*   PostPickHistoryDetailsForm:startRow().                                                                                                    */
/*   PostPickHistoryDetailsForm:insertLabel(fTL("History ID")).                                                                                */
/*   PostPickHistoryDetailsForm:insertTextField("PostPickLocationHistoryID", "", 100, TRUE).                                                   */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:startRow().                                                                                                    */
/*   PostPickHistoryDetailsForm:insertLabel(fTL("Kitting Line ID")).                                                                           */
/*   PostPickHistoryDetailsForm:insertTextField("PostPickLocationID", "", 100, TRUE).                                                          */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:startRow().                                                                                                    */
/*   PostPickHistoryDetailsForm:insertLabel(fTL("Location Code")).                                                                             */
/*   PostPickHistoryDetailsForm:insertTextField("LocationCode", "", 190, TRUE).                                                                */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:startRow().                                                                                                    */
/*   PostPickHistoryDetailsForm:insertLabel(fTL("Location Name")).                                                                             */
/*   PostPickHistoryDetailsForm:insertTextField("LocationName", "", 190, TRUE).                                                                */
/*                                                                                                                                             */
/*/*   ShipOrderTypeHistoryDetailsForm:startRow().                                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertLabel(fTL("Type Descr")).                                                                       */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertTextAreaField("TypeDescr", "", 190, TRUE).                                                      */*/
/*/*                                                                                                                                         */*/
/*/*   ShipOrderTypeHistoryDetailsForm:startRow().                                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertLabel(fTL("Active")).                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertComboField("Active", "", 100, TRUE).                                                            */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").                                                          */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").                                                      */*/
/*/*                                                                                                                                         */*/
/*/*   ShipOrderTypeHistoryDetailsForm:startRow().                                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertLabel(fTL("Listing Seq")).                                                                      */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertTextField("ListingSequence", "", 190, TRUE).                                                    */*/
/*/*                                                                                                                                         */*/
/*/*   ShipOrderTypeHistoryDetailsForm:startRow().                                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertLabel(fTL("User")).                                                                             */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertComboField("GateUserID", "", 195, TRUE).                                                        */*/
/*/*   FOR EACH GateUser NO-LOCK                                                                                                             */*/
/*/*      BY GateUser.FullName:                                                                                                              */*/
/*/*      ShipOrderTypeHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    */*/
/*/*   END.                                                                                                                                  */*/
/*/*                                                                                                                                         */*/
/*/*   ShipOrderTypeHistoryDetailsForm:startRow().                                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertLabel(fTL("OperationType")).                                                                    */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertComboField("OperationTypeID", "", 190, TRUE).                                                   */*/
/*/*   FOR EACH OperationType NO-LOCK                                                                                                        */*/
/*/*      BY OperationType.TypeName:                                                                                                         */*/
/*/*      ShipOrderTypeHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).*/*/
/*/*   END.                                                                                                                                  */*/
/*/*                                                                                                                                         */*/
/*/*   ShipOrderTypeHistoryDetailsForm:startRow().                                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertDateField("CreatedDate", "", 135, TRUE).                                                        */*/
/*/*   /* Time fields have no label */                                                                                                       */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       */*/
/*/*   /* This has a label to separate the time */                                                                                           */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertLabel(":").                                                                                     */*/
/*/*   ShipOrderTypeHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       */*/
/*                                                                                                                                             */
/*   {webGetOptionalFormFields.i pPostPickDetailsFields}                                                                                       */
/*                                                                                                                                             */
/*   chrExtraFieldList = TRIM(chrExtraFieldList,",").                                                                                          */
/*                                                                                                                                             */
/*   /* Add Hidden Fields*/                                                                                                                    */
/*   PostPickHistoryDetailsForm:insertHiddenField("kittingline_browse_scroll","").                                                             */
/*   PostPickHistoryDetailsForm:insertHiddenField("PostPickID","").                                                                            */
/*   PostPickHistoryDetailsForm:insertHiddenField("PostPickVersionID","").                                                                     */
/*   PostPickHistoryDetailsForm:insertHiddenField("PostPickHistoryID","").                                                                     */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:insertHiddenField("popup_kittingline_postpick_browse_form","").                                                */
/*   PostPickHistoryDetailsForm:insertHiddenField("popup_kittingline_postpick_history_browse_form","").                                        */
/*   PostPickHistoryDetailsForm:insertHiddenField("popup_kittingline_postpick_history_details_form","").                                       */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:insertHiddenField("form_name", "kittingline_postpick_history_details_form").                                   */
/*   PostPickHistoryDetailsForm:insertHiddenField("prog_name", "adKittingLineAdmin.p").                                                        */
/*                                                                                                                                             */
/*   /* This adds all of the standard form field lists to the form */                                                                          */
/*   {webGetHiddenFormFields.i PostPickHistoryDetailsForm}                                                                                     */
/*                                                                                                                                             */
/*   /* Create Button Bar */                                                                                                                   */
/*   PostPickDetailsButtons = NEW buttonBar().                                                                                                 */
/*                                                                                                                                             */
/*   PostPickDetailsButtons:addButton("kittingline_postpick_history_details_form_btn_cancel",                                                  */
/*                                         fTL("Cancel"),                                                                                      */
/*                                         "disablePopup('kittingline_postpick_history_details_form_popup');").                                */
/*                                                                                                                                             */
/*   PostPickDetailsButtons:closeBar().                                                                                                        */
/*                                                                                                                                             */
/*   /* Assign the Button Bar Object to the Form Object */                                                                                     */
/*   PostPickHistoryDetailsForm:FormButtons = PostPickDetailsButtons.                                                                          */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:endForm().                                                                                                     */
/*                                                                                                                                             */
/*   PostPickHistoryDetailsForm:displayForm().                                                                                                 */
/*                                                                                                                                             */
/*   /*   chrPageBuildError = chrPageBuildError + ShipOrderStreamHistoryDetailsForm:getErrors().  */                                           */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderStreamBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderStreamBrowse Procedure 
PROCEDURE pKittingLineBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "kittingline_browse_form"}
   
   KittingLineBrowse              = NEW browseTable("kittingline_browse").
   KittingLineBrowse:BrowseWidth  = 965.
   KittingLineBrowse:BrowseHeight = 455.
   KittingLineBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   KittingLineBrowse:insertColumn(fTL("KittingLine ID"), 120, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i KittingLine}
   
   KittingLineBrowse:insertColumn(fTL("Line Name"),               125, "CHARACTER", "LEFT",  FALSE).
   KittingLineBrowse:insertColumn(fTL("Line Location"),           100, "CHARACTER", "LEFT",  FALSE).
   KittingLineBrowse:insertColumn(fTL("WorkOrder Type"),          100, "INTEGER",            FALSE).
   KittingLineBrowse:insertColumn(fTL("Listing"),                 100, "INTEGER",            FALSE).
   KittingLineBrowse:insertColumn(fTL("Active"),                   80, "LOGICAL",            FALSE).
   
   /*Body*/
   KittingLineBrowse:startBody().
   
   FOR EACH KittingLine NO-LOCK
      BY KittingLine.KittingLineID: /*idx=ActiveListingSequence*/
      
      KittingLineBrowse:startRow(KittingLine.KittingLineID, "selectKittingLineRow(this," + '"' + STRING(KittingLine.KittingLineID) + '"' + ");", "").
      
      KittingLineBrowse:insertData(KittingLine.KittingLineID).
      
      FIND FIRST Location OF KittingLine NO-LOCK NO-ERROR.
      
      FIND FIRST WorkOrderType OF KittingLine NO-LOCK NO-ERROR.
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i KittingLine}
      
      KittingLineBrowse:insertData(KittingLine.LineName,"LEFT").
      KittingLineBrowse:insertData(IF AVAILABLE Location THEN Location.LocationRef ELSE  "","LEFT").
      KittingLineBrowse:insertData(IF AVAILABLE WorkOrderType THEN WorkOrderType.TypeName ELSE  "", "LEFT").
      KittingLineBrowse:insertData(KittingLine.ListingSequence).
      KittingLineBrowse:insertData(STRING(KittingLine.Active, "Yes/No")).
      
      /* Add hidden fields */
      KittingLineBrowse:insertHiddenData("KittingLineVersionID",KittingLine.VersionID).
      
      KittingLineBrowse:endRow().
      
   END. /*FOR EACH KittingLine NO-LOCK */
   
   KittingLineBrowse:endTable().
   chrPageBuildError = chrPageBuildError + KittingLineBrowse:getErrors().
   
   /* Create a new frame */
   KittingLineBrowseFrame = NEW pageFrame().
   KittingLineBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   KittingLineBrowseFrame:FormAction="dbKittingLineUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   KittingLineBrowseFrame:formOpen("kittingline_browse_form").
   
   /* Start the Frame Header */
   KittingLineBrowseFrame:insertSpacer(5).
   KittingLineBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   KittingLineBrowse:displayBrowse().  
   
   /* End the Frame Header */
   KittingLineBrowseFrame:frameClose().
   KittingLineBrowseFrame:insertSpacer(10).
   
   /* Hidden Fields */
   KittingLineBrowseFrame:insertHiddenField("kittingline_browse_scroll","").
   KittingLineBrowseFrame:insertHiddenField("KittingLineID","").
   KittingLineBrowseFrame:insertHiddenField("KittingLineVersionID","").
   KittingLineBrowseFrame:insertHiddenField("PostPickLocationID","").
   KittingLineBrowseFrame:insertHiddenField("PostPickLocationVersionID","").
   KittingLineBrowseFrame:insertHiddenField("KittingLineHistoryID","").

   KittingLineBrowseFrame:insertHiddenField("popup_kittingline_details_form","").
   KittingLineBrowseFrame:insertHiddenField("popup_kittingline_history_browse_form","").
   KittingLineBrowseFrame:insertHiddenField("popup_kittingline_station_browse_form","").
   KittingLineBrowseFrame:insertHiddenField("popup_kittingline_history_details_form","").
   KittingLineBrowseFrame:insertHiddenField("popup_kittingline_postpick_browse_form","").
   KittingLineBrowseFrame:insertHiddenField("popup_addkittinglinepostpicklink_browse","").
   KittingLineBrowseFrame:insertHiddenField("popup_kittingline_postpick_history_browse_form","").
   KittingLineBrowseFrame:insertHiddenField("popup_kittingline_postpick_history_details_form","").
   
   KittingLineBrowseFrame:insertHiddenField("form_name","kittingline_browse_form").
   KittingLineBrowseFrame:insertHiddenField("prog_name","adKittingLineAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingLineBrowseFrame}
   
   KittingLineBrowseFrame:formClose().
   
   /* Create Button Bar */
   KittingLineBrowseButtons = NEW buttonBar().
   KittingLineBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   KittingLineBrowseButtons:addButton("kittingline_browse_form_btn_details",
                                          fTL("Details"),
                                          "viewKittingLineDetails('kittingline_details_form');",
                                          (IF intSelectedKittingLine > 0 THEN "" ELSE "Disabled")).
  
   KittingLineBrowseButtons:addButton("kittingline_browse_form_btn_create",
                                          fTL("Create"),
                                          "createKittingLine('kittingline_details_form');",
                                          "").
   
   KittingLineBrowseButtons:addButton("kittingline_browse_form_btn_stationlink",
                                         fTL("Station Link"),
                                         "viewKittingLineStation('kittingline_browse_form');",
                                         (IF intSelectedKittingLine > 0 THEN "" ELSE "Disabled")).
   
   KittingLineBrowseButtons:addButton("kittingline_browse_form_btn_postpick",
                                         fTL("Post Pick"),
                                         "viewKittingLine('kittingline_browse_form');",
                                         (IF intSelectedKittingLine > 0 THEN "" ELSE "Disabled")).
   
   KittingLineBrowseButtons:addButton("kittingline_browse_form_btn_history",
                                          fTL("History"),
                                          "viewKittingLineHistory('kittingline_browse_form');",
                                          (IF intSelectedKittingLine > 0 THEN "" ELSE "Disabled")).
   
   KittingLineBrowseButtons:closeBar().  
   KittingLineBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderStreamDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderStreamDetails Procedure 
PROCEDURE pKittingLineDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "kittingline_details_form"}
         
   ASSIGN chrDisplayFieldList  = "KittingLineID,LineName,LocationID,WorkOrderTypeID,ListingSequence,Active"
          chrEditFieldList     = "LineName,LocationID,WorkOrderTypeID,ListingSequence,Active"
          chrNewFieldList      = "LineName,LocationID,WorkOrderTypeID,ListingSequence,Active"                                       
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   KittingLineDetailsForm = NEW dataForm("kittingline_details_form").
   KittingLineDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   KittingLineDetailsForm:FormAction = "dbKittingLineUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   KittingLineDetailsForm:FormWidth   = 460.
   KittingLineDetailsForm:FormHeight  = 300.
   KittingLineDetailsForm:FormTitle   = "Kitting Line Details".
   KittingLineDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   KittingLineDetailsForm:insertPaddingColumn(30).
   KittingLineDetailsForm:insertColumn(175).
   KittingLineDetailsForm:insertColumn(200).
   
   /* Fields */
   KittingLineDetailsForm:startRow().
   KittingLineDetailsForm:insertLabel(fTL("KittingLine ID")).
   KittingLineDetailsForm:insertTextField("KittingLineID", "", 100, TRUE).   
   
   KittingLineDetailsForm:startRow().
   KittingLineDetailsForm:insertLabel(fTL("Line Name")).
   KittingLineDetailsForm:insertTextField("LineName", "", 100, TRUE). 

   KittingLineDetailsForm:startRow().
   KittingLineDetailsForm:insertLabel(fTL("Location")).
   KittingLineDetailsForm:insertComboField("LocationID", "", 100, TRUE).
   FIND FIRST LocationType NO-LOCK
          WHERE  LocationType.TypeCode = "KittingLine".
   LocationLoop:
      FOR EACH  Location NO-LOCK
          WHERE Location.Active
          AND   Location.LocationTypeID = LocationType.LocationTypeID
          BY    Location.LocationID:
             
            KittingLineDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   KittingLineDetailsForm:startRow().
   KittingLineDetailsForm:insertLabel(fTL("WorkOrder Type")).
   KittingLineDetailsForm:insertComboField("WorkOrderTypeID", "", 100, TRUE).
   WorkOrderLoop:
      FOR EACH WorkOrderType NO-LOCK
         WHERE WorkOrderType.Active:
         
         KittingLineDetailsForm:insertComboPairs("WorkOrderTypeID", STRING(WorkOrderType.WorkOrderTypeID), WorkOrderType.TypeName). 
      END.
   
   KittingLineDetailsForm:startRow().
   KittingLineDetailsForm:insertLabel(fTL("Listing Seq")).
   KittingLineDetailsForm:insertTextField("ListingSequence", "", 100, TRUE).

   KittingLineDetailsForm:startRow().
   KittingLineDetailsForm:insertLabel(fTL("Active")).
   KittingLineDetailsForm:insertComboField("Active", "", 100, TRUE).
   KittingLineDetailsForm:insertComboPairs("Active", "yes", "Active").
   KittingLineDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pKittingLineDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   KittingLineDetailsForm:insertHiddenField("kittingline_browse_scroll", "").
   KittingLineDetailsForm:insertHiddenField("KittingLineID","").
   KittingLineDetailsForm:insertHiddenField("KittingLineVersionID","").
      
   
   KittingLineDetailsForm:insertHiddenField("form_name", "kittingline_details_form").
   KittingLineDetailsForm:insertHiddenField("prog_name", "adKittingLineAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingLineDetailsForm}
   
   /* Create Button Bar */
   KittingLineDetailsButtons = NEW buttonBar().
   
   KittingLineDetailsButtons:addButton("kittingline_details_form_btn_save", 
                                           fTL("Save"), 
                                           "updateKittingLine('kittingline_details_form');").
                                    
   KittingLineDetailsButtons:addButton("kittingline_details_form_btn_cancel", 
                                           fTL("Cancel"), 
                                           "cancelUpdate('UserCancelled','process_mode'); disablePopup('kittingline_details_form_popup');").
   
   KittingLineDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingLineDetailsForm:FormButtons = KittingLineDetailsButtons.
   
   KittingLineDetailsForm:endForm(). 
   
   KittingLineDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + KittingLineDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderStreamDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderStreamDetailsFields Procedure 
PROCEDURE pKittingLineDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      KittingLineDetailsForm:startRow().
      KittingLineDetailsForm:insertLabel(fTL("Field Label")).
      KittingLineDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*    {adShipOrderType_shipordertype_details_form.i}*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderStreamHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderStreamHistoryBrowse Procedure 
PROCEDURE pKittingLineHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "kittingline_history_browse_form"}
   
   KittingLineHistoryBrowseForm           = NEW dataForm("kittingline_history_browse_form").
   KittingLineHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   KittingLineHistoryBrowseForm:FormWidth  = 860.
   KittingLineHistoryBrowseForm:FormHeight = 530.
   KittingLineHistoryBrowseForm:FormTitle  = fTL("Kitting Line History").
   KittingLineHistoryBrowseForm:FormType   = "xxl_large".
   KittingLineHistoryBrowse                = NEW browseTable("kittingline_history_browse").
   KittingLineHistoryBrowse:BrowseWidth    = 840.
   KittingLineHistoryBrowse:BrowseHeight   = 490.
   KittingLineHistoryBrowse:ExcelExport    = TRUE.
   KittingLineHistoryBrowse:SessionID      = intGblSessionID.
   KittingLineHistoryBrowse:WebStream      = STREAM WebStream:HANDLE.
   
   KittingLineHistoryBrowse:insertColumn(fTL("Hist ID"),       90, "INTEGER"  ,         FALSE).
   KittingLineHistoryBrowse:insertColumn(fTL("Line Name"),    270, "CHARACTER", "LEFT", FALSE).
   KittingLineHistoryBrowse:insertColumn(fTL("Gate User"),    200, "CHARACTER", "LEFT", FALSE).
   KittingLineHistoryBrowse:insertColumn(fTL("Created"),      120, "CHARACTER", "LEFT", FALSE).
   KittingLineHistoryBrowse:insertColumn(fTL("Active"),       100, "LOGICAL"  ,         FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i KittingLineHistory}
   
   
   KittingLineHistoryBrowse:StartBody().
   
   FOR EACH KittingLineHistory NO-LOCK  /*idx=KittingLineHistoryID*/
      WHERE KittingLineHistory.KittingLineID = intSelectedKittingLine
      BY    KittingLineHistory.KittingLineHistoryID DESCENDING:

      FIND FIRST GateUser OF KittingLineHistory NO-LOCK NO-ERROR.

      KittingLineHistoryBrowse:startRow(KittingLineHistory.KittingLineHistoryID,
                                        "selectKittingLineHistoryRow(this," + '"' + STRING(KittingLineHistory.KittingLineHistoryID) + '"' + ");", "").

      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i KittingLineHistory}

      KittingLineHistoryBrowse:insertData(KittingLineHistory.KittingLineHistoryID).
      KittingLineHistoryBrowse:insertData(KittingLineHistory.LineName, "LEFT").
      KittingLineHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      KittingLineHistoryBrowse:insertData(fDisplayDate&Time(KittingLineHistory.Created,"y/m/d H:M:S"), "LEFT").
      KittingLineHistoryBrowse:insertData(STRING(KittingLineHistory.Active,"Yes/No")).

      /* Hidden row fields */
      KittingLineHistoryBrowse:insertHiddenData("KittingLineHistoryID",KittingLineHistory.KittingLineHistoryID).

      KittingLineHistoryBrowse:endRow().
      END. /* FOR EACH ShipOrderStreamHistory */

   KittingLineHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + KittingLineHistoryBrowse:getErrors().
   
   /* Hidden Fields */
   KittingLineHistoryBrowseForm:insertHiddenField("kittingline_browse_scroll","").
   KittingLineHistoryBrowseForm:insertHiddenField("KittingLineID","").
   KittingLineHistoryBrowseForm:insertHiddenField("KittingLineVersionID","").
   KittingLineHistoryBrowseForm:insertHiddenField("KittingLineHistoryID","").

   KittingLineHistoryBrowseForm:insertHiddenField("popup_kittingline_history_browse_form","").
   KittingLineHistoryBrowseForm:insertHiddenField("popup_kittingline_history_details_form","").
      
   KittingLineHistoryBrowseForm:insertHiddenField("form_name","kittingline_history_browse_form").
   KittingLineHistoryBrowseForm:insertHiddenField("prog_name","adKittingLineAdmin.p").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingLineHistoryBrowseForm}
   
   /* Create Button Bar */
   KittingLineHistoryBrowseButtons = NEW buttonBar().
   
   KittingLineHistoryBrowseButtons:addButton("kittingline_history_browse_form_btn_details",
                                                 fTL("Details"),
                                                 "viewKittingLineHistoryDetails('kittingline_history_details_form');",
                                                 (IF intSelectedKittingLineHistory > 0 THEN "" ELSE "Disabled")).
   
/*   ShipOrderStreamHistoryBrowseButtons:addButton("shiporderstream_history_browse_form_btn_excel",                                   */
/*                                             fTL("Excel Export"),                                                               */
/*                                             "excelExport('" + STRING(intGblSessionID) + "_shiporderstream_history_browse.xml')").*/
   
   KittingLineHistoryBrowseButtons:addButton("kittingline_history_browse_form_btn_cancel",
                                                 fTL("Cancel"),
                                                 "disablePopup('kittingline_history_browse_form_popup');").
   
   KittingLineHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingLineHistoryBrowseForm:FormBrowse  = KittingLineHistoryBrowse.
   KittingLineHistoryBrowseForm:FormButtons = KittingLineHistoryBrowseButtons.
   KittingLineHistoryBrowseForm:endForm(). 
   
   KittingLineHistoryBrowseForm:displayForm().  
    
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderStreamHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderStreamHistoryDetails Procedure
PROCEDURE pKittingLineHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "kittingline_history_details_form"}
   
   ASSIGN chrDisplayFieldList  = "KittingLineID,KittingLineHistoryID,LineName,LocationID,WorkOrderTypeID,ListingSequence,Active,"
                                 + "GateUserID,LineCode,OperationTypeID,CreatedDate,CreatedHour,CreatedMins"
          chrEditFieldList     = ""
          chrNewFieldList      = "KittingLineID,KittingLineHistoryID,LineName,LocationID,WorkOrderTypeID,ListingSequence,Active,"
                                 + "GateUserID,LineCode,OperationTypeID,CreatedDate,CreatedHour,CreatedMins"     
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   KittingLineHistoryDetailsForm = NEW dataForm("kittingline_history_details_form").
   KittingLineHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   KittingLineHistoryDetailsForm:FormAction = "".
   
   /* Setup */
   KittingLineHistoryDetailsForm:FormWidth   = 580.
   KittingLineHistoryDetailsForm:FormHeight  = 420.
   KittingLineHistoryDetailsForm:FormTitle   = "Kitting Line History Details".
   KittingLineHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   KittingLineHistoryDetailsForm:insertPaddingColumn(30).
   KittingLineHistoryDetailsForm:insertColumn(160).
   KittingLineHistoryDetailsForm:insertColumn(150).
   KittingLineHistoryDetailsForm:insertColumn(20).
   KittingLineHistoryDetailsForm:insertColumn(4).
   KittingLineHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("Kitting Line ID")).
   KittingLineHistoryDetailsForm:insertTextField("KittingLineID", "", 100, TRUE).  
   
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("History ID")).
   KittingLineHistoryDetailsForm:insertTextField("KittingLineHistoryID", "", 100, TRUE).  
   
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("Line Code")).
   KittingLineHistoryDetailsForm:insertTextField("LineCode", "", 190, TRUE).  
   
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("LineName")).
   KittingLineHistoryDetailsForm:insertTextField("LineName", "", 190, TRUE).  
   
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("Active")). 
   KittingLineHistoryDetailsForm:insertComboField("Active", "", 100, TRUE).  
   KittingLineHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   KittingLineHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("Listing Seq")).
   KittingLineHistoryDetailsForm:insertTextField("ListingSequence", "", 100, TRUE).  
   
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("Gate User")).
   KittingLineHistoryDetailsForm:insertComboField("GateUserID", "", 190, TRUE).
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      KittingLineHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
     
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("OperationType")).
   KittingLineHistoryDetailsForm:insertComboField("OperationTypeID", "", 190, TRUE).
   FOR EACH OperationType NO-LOCK
      BY OperationType.TypeName:
      KittingLineHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END.  
   
   KittingLineHistoryDetailsForm:startRow().
   KittingLineHistoryDetailsForm:insertLabel(fTL("Created")).
   KittingLineHistoryDetailsForm:insertDateField("CreatedDate", "", 135, TRUE).  
   /* Time fields have no label */
   KittingLineHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   KittingLineHistoryDetailsForm:insertLabel(":").
   KittingLineHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE). 
   
   {webGetOptionalFormFields.i pKittingLineDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   KittingLineHistoryDetailsForm:insertHiddenField("kittingline_browse_scroll","").
   KittingLineHistoryDetailsForm:insertHiddenField("KittingLineID","").
   KittingLineHistoryDetailsForm:insertHiddenField("KittingLineVersionID","").
   KittingLineHistoryDetailsForm:insertHiddenField("KittingLineHistoryID","").

   KittingLineHistoryDetailsForm:insertHiddenField("popup_kittingline_history_browse_form","").
   KittingLineHistoryDetailsForm:insertHiddenField("popup_kittingline_history_details_form","").
   
   KittingLineHistoryDetailsForm:insertHiddenField("form_name", "kittingline_history_details_form").
   KittingLineHistoryDetailsForm:insertHiddenField("prog_name", "adKittingLineAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i KittingLineHistoryDetailsForm}
   
   /* Create Button Bar */
   KittingLineDetailsButtons = NEW buttonBar().
   KittingLineDetailsButtons:addButton("kittingline_history_details_form_btn_cancel", 
                                           fTL("Cancel"), 
                                           "disablePopup('kittingline_history_details_form_popup');").
                                    
   KittingLineDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   KittingLineHistoryDetailsForm:FormButtons = KittingLineDetailsButtons.
   
   KittingLineHistoryDetailsForm:endForm(). 
   
   KittingLineHistoryDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + KittingLineHistoryDetailsForm:getErrors().  */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

