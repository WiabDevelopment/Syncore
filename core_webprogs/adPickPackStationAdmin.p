&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adPickPackStationAdmin.p 

  Description: ad file for the PickPack Station Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Nick Diessner

  Created: 27/03/2015
  
  Revisions:
     20/11/2015 - twierzch - add Remove button to clear QueueEntries from PickPackStation

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
{fncStatusTypeFunctions.i}

/* Definitions for System Options for Receiving */
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
{defDataMigrationVariables.i}



/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedPickPackStation                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedPickPackStationHistory           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPickPackStationHistoryID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPickPackStationRow                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickPackStationRow               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickPackStationHistoryRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPickPackStationID                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSpaces                                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrShipOrderStreamID                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intStationLocationCnt                       AS INTEGER     NO-UNDO.
DEFINE VARIABLE intStreamLocationCnt                        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrStationLocations                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrStreamLocations                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedShipOrderStream                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE calcMismatch                                AS INTEGER     NO-UNDO.
DEFINE VARIABLE intPickPackStationID                        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectedPickedPackStationQueueEntry      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedPickedPackStationQueueEntry      AS INTEGER     NO-UNDO.

/*Locations Button*/
DEFINE VARIABLE intSelectedPickPackLocEaseOfAccessLink      AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedPickPackLocEaseOfAccessLnkHst    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPickPackLocEaseOfAccessLnkHstID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPickPackLocEaseOfAccessLinkRow           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickPackLocEaseOfAccessLinkRow   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPickPackLocEaseOfAccessLnkHstRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPickPackLocEaseOfAccessLinkID            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPickPackLocEaseOfAccessLnkHst       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPickPackLocEaseOfAccessLink         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupMismatches                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPickPackStationQueueEntry           AS CHARACTER   NO-UNDO.

/* variables for file import */
DEFINE VARIABLE intFileLineSequence                         AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrImportedLine                             AS CHARACTER NO-UNDO.
DEFINE VARIABLE mptFile                                     AS MEMPTR    NO-UNDO.
DEFINE VARIABLE chrFileName                                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDestinationFile                          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrColumnValue                              AS CHARACTER NO-UNDO.
DEFINE VARIABLE intColumnsCount                             AS INTEGER   NO-UNDO.
DEFINE VARIABLE logIsLogFile                                AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE intPickPackStationIDCol                     AS INTEGER   NO-UNDO.
DEFINE VARIABLE intLocationIDCol                            AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCurrentColumnsWidth                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE intDisplayedColumnsNo                       AS INTEGER   NO-UNDO.
DEFINE VARIABLE intViewSkusFileID                           AS INTEGER   NO-UNDO.

/*Upload Variables*/
DEFINE VARIABLE intSelectedFile                             AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectFileRow                            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileID                                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFile                                AS CHARACTER NO-UNDO.
DEFINE VARIABLE intLocationUploadMasterID                   AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrValidFile                                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFileSelection                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupLocationUpload                      AS CHARACTER NO-UNDO.

/* Buffers */

/* Objects */
DEFINE VARIABLE PickPackStationBrowseFrame                  AS pageFrame.
DEFINE VARIABLE PickPackStationBrowse                       AS browseTable.
DEFINE VARIABLE PickPackStationBrowseButtons                AS buttonBar.
DEFINE VARIABLE PickPackStationBrowseMoreButtons            AS buttonBar.
DEFINE VARIABLE PickPackStationDetailsForm                  AS dataForm.
DEFINE VARIABLE PickPackStationDetailsButtons               AS buttonBar.
DEFINE VARIABLE PickPackStationHistoryBrowseForm            AS dataForm.  
DEFINE VARIABLE PickPackStationHistoryBrowse                AS browseTable.
DEFINE VARIABLE PickPackStationHistoryDetailsForm           AS dataForm.
DEFINE VARIABLE PickPackStationHistoryDetailsButtons        AS buttonBar.
DEFINE VARIABLE PickPackStationHistoryButtons               AS buttonBar.

DEFINE VARIABLE ShipOrderStreamPickPackLinkBrowseForm       AS dataForm.
DEFINE VARIABLE ShipOrderStreamPickPackLinkBrowse           AS browseTable.
DEFINE VARIABLE ShipOrderStreamPickPackLinkBrowseButtons    AS buttonBar.

DEFINE VARIABLE AddShipOrderStreamPickPackLinkBrowseForm    AS dataForm.
DEFINE VARIABLE AddShipOrderStreamPickPackLinkBrowse        AS browseTable.
DEFINE VARIABLE AddShipOrderStreamPickPackLinkBrowseButtons AS buttonBar.
DEFINE VARIABLE chrAddShipOrderStreamPickPackLink           AS CHARACTER NO-UNDO.

DEFINE VARIABLE chrShipOrderStreamPickPackLinkID            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupShipOrderStreamPickPackLink         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectShipOrderStreamPickPackLinkRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedShipOrderStreamPickPackLink      AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrShipOrderStreamPickPackLinkRow           AS CHARACTER NO-UNDO.

DEFINE VARIABLE ZoneLinkBrowseForm                          AS dataForm.
DEFINE VARIABLE ZoneLinkBrowse                              AS browseTable.
DEFINE VARIABLE ZoneLinkBrowseButtons                       AS buttonBar.
DEFINE VARIABLE chrPopupZoneLink                            AS CHARACTER NO-UNDO.

DEFINE VARIABLE AddZoneLinkBrowseForm                       AS dataForm.
DEFINE VARIABLE AddZoneLinkBrowse                           AS browseTable.
DEFINE VARIABLE AddZoneLinkBrowseButtons                    AS buttonBar.
DEFINE VARIABLE chrPopupAddZoneLink                         AS CHARACTER NO-UNDO.

/*PickPackLocEaseOfAccessLink Browse*/
DEFINE VARIABLE PickPackLocEaseOfAccessLinkBrowseFrame      AS pageFrame.
DEFINE VARIABLE PickPackLocEaseOfAccessLinkBrowseForm       AS dataForm.
DEFINE VARIABLE PickPackLocEaseOfAccessLinkBrowse           AS browseTable.
DEFINE VARIABLE PickPackLocEaseOfAccessLinkButtons          AS buttonBar.
DEFINE VARIABLE PickPackLocEaseOfAccessLinkDetailsForm      AS dataForm.
DEFINE VARIABLE PickPackLocEaseOfAccessLinkDetailsButtons   AS buttonBar.

/*PickPackLocEaseOfAccessLnkHst Browse*/
DEFINE VARIABLE PickPackLocEaseOfAccessLnkHstBrowseFrame    AS pageFrame.
DEFINE VARIABLE PickPackLocEaseOfAccessLnkHstBrowseForm     AS dataForm.
DEFINE VARIABLE PickPackLocEaseOfAccessLnkHstBrowse         AS browseTable.
DEFINE VARIABLE PickPackLocEaseOfAccessLnkHstButtons        AS buttonBar.
DEFINE VARIABLE PickPackLocEaseOfAccessLnkHstDetailsForm    AS dataForm.
DEFINE VARIABLE PickPackLocEaseOfAccessLnkHstDetailsButtons AS buttonBar.

/*PartProfileOrderStreamRplLock Browse*/
DEFINE VARIABLE PartProfileOrderStreamRplLocBrowseFrame      AS pageFrame.
DEFINE VARIABLE PartProfileOrderStreamRplLocBrowseForm       AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamRplLocBrowse           AS browseTable.
DEFINE VARIABLE PartProfileOrderStreamRplLocButtons          AS buttonBar.
DEFINE VARIABLE PartProfileOrderStreamRplLocDetailsForm      AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamRplLocDetailsButtons   AS buttonBar.

/*PickPackStationQueueEntry Browse*/
DEFINE VARIABLE PickPackStationQueueEntryBrowseFrame      AS pageFrame.
DEFINE VARIABLE PickPackStationQueueEntryBrowseForm       AS dataForm.
DEFINE VARIABLE PickPackStationQueueEntryBrowse           AS browseTable.
DEFINE VARIABLE PickPackStationQueueEntryButtons          AS buttonBar.
DEFINE VARIABLE PickPackStationQueueEntryDetailsForm      AS dataForm.
DEFINE VARIABLE PickPackStationQueueEntryDetailsButtons   AS buttonBar.

DEFINE VARIABLE intPartLocationSequence AS INTEGER.

DEFINE TEMP-TABLE ttPartLocation 
   FIELD PartLocationID      AS INTEGER 
   FIELD LocationID          AS INTEGER
   FIELD PartId              AS INTEGER
   FIELD PreferredLocationID AS INTEGER
   FIELD Mismatch            AS INTEGER
   FIELD PreferredEoA        AS INTEGER
   FIELD ActualEoA           AS INTEGER.


DEFINE BUFFER chkttPartLocation FOR ttPartLocation.

/*Upload Browse*/
DEFINE STREAM sUploadFile.

DEFINE TEMP-TABLE ttFileLine
   FIELD FileLineNo AS INTEGER 
   FIELD LineString AS CHARACTER 
   INDEX iFileLineNo FileLineNo.

/* Objects for File Browse */
DEFINE VARIABLE FileSelectionForm                           AS dataForm.
DEFINE VARIABLE FileBrowseForm                              AS dataForm.
DEFINE VARIABLE FileBrowse                                  AS browseTable.
DEFINE VARIABLE FileBrowseButtons                           AS buttonBar.
DEFINE VARIABLE FileDetailsButtons                          AS buttonBar.

/* Objects for UploadLocations Browse */
DEFINE VARIABLE UploadLocationBrowse                        AS browseTable.
DEFINE VARIABLE UploadLocationBrowseButtons                 AS buttonBar.
DEFINE VARIABLE UploadLocationBrowseForm                    AS dataForm.


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

&IF DEFINED(EXCLUDE-pAddShipOrderStreamPickPackLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddShipOrderStreamPickPackLinkBrowse Procedure
PROCEDURE pAddShipOrderStreamPickPackLinkBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   AddShipOrderStreamPickPackLinkBrowseForm            = NEW dataForm("addshiporderstreampickpacklink_browse_form").
   AddShipOrderStreamPickPackLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   AddShipOrderStreamPickPackLinkBrowseForm:FormAction = "dbCreateShipOrderStreamPickPackLinks.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   AddShipOrderStreamPickPackLinkBrowseForm:FormWidth  = 700.
   AddShipOrderStreamPickPackLinkBrowseForm:FormHeight = 490.
   AddShipOrderStreamPickPackLinkBrowseForm:FormTitle  = fTL("Available Streams to Link to PickPack Station: " + chrPickPackStationID).
   AddShipOrderStreamPickPackLinkBrowseForm:FormType   = "xl_large".
   
   /* Form Data */
   AddShipOrderStreamPickPackLinkBrowseForm:insertPaddingColumn(180).
   AddShipOrderStreamPickPackLinkBrowseForm:insertColumn(110).
   AddShipOrderStreamPickPackLinkBrowseForm:insertColumn(130).
   AddShipOrderStreamPickPackLinkBrowseForm:insertColumn(40).
   AddShipOrderStreamPickPackLinkBrowseForm:insertColumn(85).
   AddShipOrderStreamPickPackLinkBrowseForm:insertColumn(130).
   
   AddShipOrderStreamPickPackLinkBrowseForm:startRow().
   AddShipOrderStreamPickPackLinkBrowseForm:insertBlankRow(10). 

   AddShipOrderStreamPickPackLinkBrowse              = NEW browseTable("addshiporderstreampickpacklink_browse").
   AddShipOrderStreamPickPackLinkBrowse:BrowseWidth  = 680.
   AddShipOrderStreamPickPackLinkBrowse:BrowseHeight = 442.
   
   AddShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Stream ID"),       80, "INTEGER",           FALSE).
   AddShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Stream Name"),   110, "CHARACTER", "left", FALSE).
   AddShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Active"),          80, "LOGICAL",           FALSE).
   
   AddShipOrderStreamPickPackLinkBrowse:StartBody().
   
   ShipOrderStreamLinkLoop:
   FOR EACH ShipOrderStream NO-LOCK /* idx=ShipOrderStreamID */
      BY ShipOrderStream.ShipOrderStreamID:

      IF CAN-FIND(FIRST ShipOrderStreamPickPackLink OF ShipOrderStream NO-LOCK
                     WHERE ShipOrderStreamPickPackLink.ShipOrderStreamID = ShipOrderStream.ShipOrderStreamID /* idx ShipOrderStreamID  */
                       AND ShipOrderStreamPickPackLink.PickPackStationID = INTEGER(chrPickPackStationID) ) THEN /* idx ShipOrderStreamID*/
                            NEXT ShipOrderStreamLinkLoop.
                            
      AddShipOrderStreamPickPackLinkBrowse:startRow(ShipOrderStream.ShipOrderStreamID, 
                               'multiSelectStreamRow("' + STRING(ShipOrderStream.ShipOrderStreamID) + '")', "").

      AddShipOrderStreamPickPackLinkBrowse:insertData(STRING(ShipOrderStream.ShipOrderStreamID), "left").
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      /* {webGetOptionalBrowseFields.i ShipOrderStreamPickPackLink}*/
   
      AddShipOrderStreamPickPackLinkBrowse:insertData(ShipOrderStream.StreamName,"left").
      AddShipOrderStreamPickPackLinkBrowse:insertData(STRING(ShipOrderStream.Active,"Yes/No")).
   
      /* Add hidden fields */
      AddShipOrderStreamPickPackLinkBrowse:insertHiddenData("ShipOrderStreamID",ShipOrderStream.ShipOrderStreamID).
      AddShipOrderStreamPickPackLinkBrowse:insertHiddenData("ShipOrderStreamVersionID",ShipOrderStream.VersionID).
   
      AddShipOrderStreamPickPackLinkBrowse:endRow().
   
      /* Count the Locations */
      intStationLocationCnt = intStationLocationCnt + 1.
   
      IF intStreamLocationCnt <= 2000 THEN
         chrStreamLocations = chrStreamLocations + STRING(ShipOrderStream.ShipOrderStreamID) + ",".
   
   END. /* FOR EACH PickPackStation NO-LOCK */
   
   IF intStreamLocationCnt <= 2000 THEN
     chrStreamLocations  = TRIM(chrStreamLocations, ",").
   ELSE
     chrStreamLocations = "".

   AddShipOrderStreamPickPackLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + AddShipOrderStreamPickPackLinkBrowse:getErrors().

   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("LocationList","").
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("pickpackstation_browse_scroll","").
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamID","").
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("PickPackStationID",chrPickPackStationID).
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamPickPackLinkID",chrShipOrderStreamPickPackLinkID).
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamPickPackLinkVersionID","").
   
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("popup_shiporderstreampickpacklink_browse_form","").
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("popup_addshiporderstreampickpacklink_browse","").
   
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("form_name","addshiporderstreampickpacklink_browse_form").
   AddShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("prog_name","adPickPackStationAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddShipOrderStreamPickPackLinkBrowseForm}
   
   /* Create Button Bar */
   AddShipOrderStreamPickPackLinkBrowseButtons = NEW buttonBar().
   
   AddShipOrderStreamPickPackLinkBrowseButtons:addButton("addshipordertypestreamlink_browse_form_btn_selectall",
                                                    fTL("Select All"),
                                                    "selectAllStreams('" + STRING(chrStreamLocations) + "','" + STRING(intStreamLocationCnt) + "')").

   AddShipOrderStreamPickPackLinkBrowseButtons:addButton("addshiporderstreampickpacklink_browse_form_btn_create",
                                                    fTL("Confirm Links"),
                                                    "createShipOrderStreamPickPackLinks('');"). 
   
   AddShipOrderStreamPickPackLinkBrowseButtons:addButton("addshiporderstreampickpacklink_browse_form_btn_delete",
                                                    fTL("Clear Selection"),
                                                    "deselectAllStreamRows()").
   
   AddShipOrderStreamPickPackLinkBrowseButtons:addButton("addshiporderstreampickpacklink_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('addshiporderstreampickpacklink_browse_form_popup');").
   
   AddShipOrderStreamPickPackLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddShipOrderStreamPickPackLinkBrowseForm:FormBrowse  = AddShipOrderStreamPickPackLinkBrowse.
   AddShipOrderStreamPickPackLinkBrowseForm:FormButtons = AddShipOrderStreamPickPackLinkBrowseButtons.
   AddShipOrderStreamPickPackLinkBrowseForm:endForm(). 
   
   AddShipOrderStreamPickPackLinkBrowseForm:displayForm().
   
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pAddZoneLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddZoneLinkBrowse Procedure
PROCEDURE pAddZoneLinkBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
   IF AVAILABLE PickPackStation THEN
      chrFormTitle = fTL("Available Work Zones for  Pick and Pack: " + PickPackStation.StationName + chrSpaces + "ID: " + STRING(PickPackStation.PickPackStationID)).
   ELSE
      chrFormTitle = fTL("Available Work Zones").      

   /* Form Setup */
   AddZoneLinkBrowseForm            = NEW dataForm("addzonelink_browse_form").
   AddZoneLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   AddZoneLinkBrowseForm:FormWidth  = 460.
   AddZoneLinkBrowseForm:FormHeight = 300.
   AddZoneLinkBrowseForm:FormTitle  = chrFormTitle.
   AddZoneLinkBrowseForm:FormType   = "medium".
   AddZoneLinkBrowseForm:FormAction = "dbPickPackWorkZoneLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues, "&").
   
   /* Browse Setup */
   AddZoneLinkBrowse              = NEW browseTable("addzonelink_browse").
   AddZoneLinkBrowse:BrowseWidth  = 440.
   AddZoneLinkBrowse:BrowseHeight = 260.
   AddZoneLinkBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Column Headers */
   AddZoneLinkBrowse:insertColumn(fTL("ZoneID"),     90, "INTEGER", FALSE).
   AddZoneLinkBrowse:insertColumn(fTL("Zone Name"), 100, "CHARACTER", "LEFT", FALSE).
   AddZoneLinkBrowse:insertColumn(fTL("Zone"),      100, "CHARACTER", "LEFT", FALSE).
   
   AddZoneLinkBrowse:StartBody().

   IF AVAILABLE PickPackStation THEN
   DO:

      ZoneLoop:
      FOR EACH WorkZone NO-LOCK /*idx=WorkZoneID*/
         WHERE WorkZone.Active:

         FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStation*/
            WHERE PickPackStation.PickPackStationID = intSelectedPickPackStation NO-ERROR.
         IF NOT AVAILABLE PickPackStation THEN
            NEXT ZoneLoop.

         IF CAN-FIND(FIRST PickPackWorkZoneLink  NO-LOCK
                     WHERE PickPackWorkZoneLink.WorkZoneID = WorkZone.WorkZoneID
                     AND   PickPackWorkZoneLink.PickPackStationID   = PickPackStation.PickPackStationID) THEN
            NEXT ZoneLoop.

/*         IF WorkZone.WorkOrderTypeID <> PickPackStation.WorkOrderTypeID THEN*/
/*            NEXT ZoneLoop.                                                      */

         FIND FIRST Warehouse OF WorkZone NO-LOCK NO-ERROR.

         AddZoneLinkBrowse:startRow(WorkZone.WorkZoneID,
                                    'multiSelectWorkZone("' + STRING(WorkZone.WorkZoneID) + '")',
                                    "").
         AddZoneLinkBrowse:insertData(WorkZone.WorkZoneID).
         AddZoneLinkBrowse:insertData(WorkZone.WorkZoneName, "left").
         AddZoneLinkBrowse:insertData(IF AVAILABLE Warehouse THEN Warehouse.WarehouseName ELSE STRING(WorkZone.WorkZoneName), "left").

         AddZoneLinkBrowse:endRow().

      END. /* FOR EACH WorkZone */
   END. /* IF AVAILABLE PickPackStation */

   AddZoneLinkBrowse:endTable().
   
   /* Hidden Data */
   AddZoneLinkBrowseForm:insertHiddenField("form_name", "addzonelink_browse_form").
   AddZoneLinkBrowseForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   AddZoneLinkBrowseForm:insertHiddenField("pickpackstation_browse_scroll", "").
   AddZoneLinkBrowseForm:insertHiddenField("popup_zonelink_browse_form", "").
   AddZoneLinkBrowseForm:insertHiddenField("popup_addzonelink_browse_form", "").
   AddZoneLinkBrowseForm:insertHiddenField("PickPackStationID", chrPickPackStationID).
   AddZoneLinkBrowseForm:insertHiddenField("PickPackWorkZoneLinkID", ""). 
   AddZoneLinkBrowseForm:insertHiddenField("WorkZoneList", "").  
         
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddZoneLinkBrowseForm}
   
   /* Create Button Bar */
   AddZoneLinkBrowseButtons = NEW buttonBar().
   
   AddZoneLinkBrowseButtons:addButton("addzonelink_browse_form_btn_create",
                                      fTL("Confirm Links"),
                                      "createWorkZoneLinks('addzonelink_browse_form');"). 
   
   AddZoneLinkBrowseButtons:addButton("addzonelink_browse_form_btn_clear",
                                      fTL("Clear Selection"),
                                      "clearSelectedWorkZone()").
   
   AddZoneLinkBrowseButtons:addButton("addzonelink_browse_form_btn_cancel",
                                      fTL("Cancel"),
                                      "disablePopup('addzonelink_browse_form_popup');").
      
   AddZoneLinkBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   AddZoneLinkBrowseForm:FormBrowse  = AddZoneLinkBrowse.
   AddZoneLinkBrowseForm:FormButtons = AddZoneLinkBrowseButtons.
   AddZoneLinkBrowseForm:endForm(). 
   
   AddZoneLinkBrowseForm:displayForm().

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

&IF DEFINED(EXCLUDE-pMismatches) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMismatches Procedure
PROCEDURE pMismatches:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   PartProfileOrderStreamRplLocBrowseForm           = NEW dataForm("partprofileorderstreamrplloc_browse_form").
   PartProfileOrderStreamRplLocBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PartProfileOrderStreamRplLocBrowseForm:FormWidth  = 860.
   PartProfileOrderStreamRplLocBrowseForm:FormHeight = 530.
   PartProfileOrderStreamRplLocBrowseForm:FormTitle  = fTL("Top Mismatches for PickPack Station: " + chrPickPackStationID).
                                                  
                                                                                                                         
   PartProfileOrderStreamRplLocBrowseForm:FormType   = "xxl_large".
   
   PartProfileOrderStreamRplLocBrowse                = NEW browseTable("partprofileorderstreamrplloc_browse").
   PartProfileOrderStreamRplLocBrowse:BrowseWidth    = 840.
   PartProfileOrderStreamRplLocBrowse:BrowseHeight   = 490.
   PartProfileOrderStreamRplLocBrowse:ExcelExport    = TRUE.
   PartProfileOrderStreamRplLocBrowse:SessionID      = intGblSessionID.
   PartProfileOrderStreamRplLocBrowse:WebStream      = STREAM WebStream:HANDLE.
   
   PartProfileOrderStreamRplLocBrowse:insertColumn(fTL("Part"),          125, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamRplLocBrowse:insertColumn(fTL("Location"),      125, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamRplLocBrowse:insertColumn(fTL("Preferred EoA"), 125, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamRplLocBrowse:insertColumn(fTL("Actual EoA"),    125, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamRplLocBrowse:insertColumn(fTL("Mismatch"),      125, "INTEGER", "LEFT", FALSE).
/*   PartProfileOrderStreamRplLocBrowse:insertColumn(fTL("Closest Empty Loc."), 150, "CHARACTER", "LEFT", FALSE).*/
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartProfileOrderStreamRplLoc}
   
   PartProfileOrderStreamRplLocBrowse:StartBody().

/*   FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStationID*/                     */
/*      WHERE PickPackStation.PickPackStationID = intSelectedPickPackStation NO-ERROR.*/

   FOR EACH PickPackLocEaseOfAccessLink NO-LOCK 
      WHERE PickPackLocEaseOfAccessLink.PickPackStationID = intSelectedPickPackStation:
      
      FIND FIRST PartProfileOrderStreamRplLoc NO-LOCK  /*idx=LocationID*/
         WHERE   PartProfileOrderStreamRplLoc.LocationID = PickPackLocEaseOfAccessLink.LocationID
         AND     PartProfileOrderStreamRplLoc.Completed  = "" NO-ERROR.

      IF AVAILABLE PartProfileOrderStreamRplLoc THEN
      DO:
         CREATE ttPartLocation.
         ASSIGN intPartLocationSequence            = intPartLocationSequence + 1
                ttPartLocation.PartLocationID      = intPartLocationSequence
/*                ttPartLocation.PreferredLocationID = PartProfileOrderStreamRplLoc.LocationID*/
                ttPartLocation.PreferredEoA        = PickPackLocEaseOfAccessLink.EaseOfAccessRanking
                ttPartLocation.PartID              = PartProfileOrderStreamRplLoc.PartID
                ttPartLocation.LocationID          = PartProfileOrderStreamRplLoc.LocationID.     
      END.

   END. /*FOR EACH PickPackLocEaseOfAccessLink NO-LOCK */
   
   FOR EACH ttPartLocation:
      
      FIND FIRST Part NO-LOCK /*idx=PartId*/
         WHERE Part.PartId = ttPartLocation.PartID NO-ERROR.
 
      FIND FIRST Location NO-LOCK /*idx=LocationID*/
         WHERE Location.LocationID = ttPartLocation.LocationID NO-ERROR.  
          
      StockCheckLoop:
      FOR EACH chkttPartLocation NO-LOCK:

         IF chkttPartLocation.PartLocationID = ttPartLocation.PartLocationID THEN
            NEXT StockCheckLoop. 

         FIND FIRST StockPackage NO-LOCK /*idx=LocationID*/
            WHERE StockPackage.LocationID = chkttPartLocation.LocationID NO-ERROR.

         IF AVAILABLE StockPackage THEN
         DO:
            ttPartLocation.LocationID = StockPackage.LocationID.
            
            FIND FIRST PickPackLocEaseOfAccessLink NO-LOCK  /*idx=LocationID*/
               WHERE PickPackLocEaseOfAccessLink.LocationID = StockPackage.LocationID NO-ERROR.
            IF AVAILABLE PickPackLocEaseOfAccessLink THEN
               ASSIGN ttPartLocation.ActualEoA = PickPackLocEaseOfAccessLink.EaseOfAccessRanking.
                      
            LEAVE StockCheckLoop.
         END. 
      END.
      
      ttPartLocation.Mismatch  = ABSOLUTE(ttPartLocation.PreferredEoA - ttPartLocation.ActualEoA).
      
   END. /* FOR EACH ttPartLocation */
   
/*   IF ttPartLocation.ActualEoA = 0 THEN*/
/*      DELETE ttPartLocation.           */
   
   FOR EACH ttPartLocation NO-LOCK 
      BY ttPartLocation.Mismatch DESCENDING:
      
      FIND FIRST Part NO-LOCK /*idx=PartID*/
         WHERE Part.PartID = ttPartLocation.PartID NO-ERROR.
            
      FIND FIRST Location NO-LOCK /*idx=LocationID*/
         WHERE Location.LocationID = ttPartLocation.LocationID NO-ERROR.   
      
      PartProfileOrderStreamRplLocBrowse:startRow(ttPartLocation.LocationID,
          "selectPartProfileOrderStreamRplLocRow(this," + '"' + STRING(ttPartLocation.LocationID)
           + '"' + ");", "").    
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PartProfileOrderStreamRplLoc}
      
      PartProfileOrderStreamRplLocBrowse:insertData((IF AVAILABLE Part THEN Part.PartRef ELSE ""), "LEFT").
      PartProfileOrderStreamRplLocBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "LEFT").
      PartProfileOrderStreamRplLocBrowse:insertData((IF AVAILABLE ttPartLocation THEN STRING(ttPartLocation.PreferredEoA) ELSE "")).
      PartProfileOrderStreamRplLocBrowse:insertData((IF AVAILABLE ttPartLocation THEN STRING(ttPartLocation.ActualEoA) ELSE "")).
      PartProfileOrderStreamRplLocBrowse:insertData(ttPartLocation.Mismatch).
/*      PartProfileOrderStreamRplLocBrowse:insertData((IF AVAILABLE ttPartLocation THEN STRING(ttPartLocation.PreferredLocationID) ELSE ""), "LEFT").*/
      
      PartProfileOrderStreamRplLocBrowse:endRow().
   END. /* FOR EACH PartProfileOrderStreamRplLoc */
   
   PartProfileOrderStreamRplLocBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PartProfileOrderStreamRplLocBrowse:getErrors().
   
   /*Hidden Fields*/
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("form_name", "partprofileorderstreamrplloc_browse_form").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("popup_pickpackloceaseofaccesslink_browse", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("pickpackloceaseofaccesslink_browse_scroll", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("popup_pickpackloceaseofaccesslnkhst_browse", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("pickpackloceaseofaccesslnkhst_browse_scroll", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("popup_partprofileorderstreamrplloc_browse", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("partprofileorderstreamrplloc_browse_scroll", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("popup_workzone_browse", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("pickpackstation_browse_scroll", "").
   
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLinkID", chrPickPackLocEaseOfAccessLinkID).
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLinkVersionID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLnkHstID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLnkHstVersionID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PartProfileOrderStreamRplLocID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PartProfileOrderStreamRplLocVersionID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("WorkZoneID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("WorkZoneVersionID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PickPackStationID", "").
   PartProfileOrderStreamRplLocBrowseForm:insertHiddenField("PickPackStationVersionID", "").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartProfileOrderStreamRplLocBrowseForm}
   
   /* Create Button Bar */
   PartProfileOrderStreamRplLocButtons = NEW buttonBar().
   
   PartProfileOrderStreamRplLocButtons:addButton("partprofileorderstreamrplloc_browse_form_btn_excel",
                               fTL("Excel Export"),
                               "excelExport('" + STRING(intGblSessionID) + "_partprofileorderstreamrplloc_browse.xml')").
                                         
   PartProfileOrderStreamRplLocButtons:addButton("partprofileorderstreamrplloc_browse_form_btn_cancel",
                                        fTL("Cancel"),
                                        "disablePopup('partprofileorderstreamrplloc_browse_form_popup');").
                                        
                                                                         
   PartProfileOrderStreamRplLocButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PartProfileOrderStreamRplLocBrowseForm:FormBrowse  = PartProfileOrderStreamRplLocBrowse.
   PartProfileOrderStreamRplLocBrowseForm:FormButtons = PartProfileOrderStreamRplLocButtons.
   PartProfileOrderStreamRplLocBrowseForm:endForm(). 
   
   PartProfileOrderStreamRplLocBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pPickPackStationQueueEntryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickPackStationQueueEntryBrowse Procedure
PROCEDURE pPickPackStationQueueEntryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   PickPackStationQueueEntryBrowseForm           = NEW dataForm("pickpackstationqueueentry_browse_form").
   PickPackStationQueueEntryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PickPackStationQueueEntryBrowseForm:FormWidth  = 860.
   PickPackStationQueueEntryBrowseForm:FormHeight = 530.
   PickPackStationQueueEntryBrowseForm:FormTitle  = fTL("Station Queue Entry").
   PickPackStationQueueEntryBrowseForm:FormType   = "xxl_large".
   PickPackStationQueueEntryBrowseForm:FormAction = "dbPickPackStationQueueEntryUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   PickPackStationQueueEntryBrowse                = NEW browseTable("pickpackstationqueueentry_browse").
   PickPackStationQueueEntryBrowse:BrowseWidth    = 840.
   PickPackStationQueueEntryBrowse:BrowseHeight   = 490.
   
   PickPackStationQueueEntryBrowse:insertColumn(fTL("ID"),                 60, "INTEGER", FALSE).
   PickPackStationQueueEntryBrowse:insertColumn(fTL("Station"),           125, "INTEGER", "LEFT", FALSE).
   PickPackStationQueueEntryBrowse:insertColumn(fTL("ToteLineJourneyID"), 125, "INTEGER", "LEFT", FALSE).
   PickPackStationQueueEntryBrowse:insertColumn(fTL("Tote Ref"),          125, "INTEGER", "LEFT", FALSE).
   PickPackStationQueueEntryBrowse:insertColumn(fTL("Order Ref"),         125, "CHARACTER", "LEFT", FALSE). 
   PickPackStationQueueEntryBrowse:insertColumn(fTL("Created"),           125, "CHARACTER", "LEFT", FALSE).
   PickPackStationQueueEntryBrowse:insertColumn(fTL("Arrived"),           125, "CHARACTER", "LEFT", FALSE). 
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickPackStationQueueEntry}
   
   PickPackStationQueueEntryBrowse:StartBody().

   FOR EACH PickPackStationQueueEntry NO-LOCK
      WHERE PickPackStationQueueEntry.PickPackStationID = intSelectedPickPackStation
      AND   PickPackStationQueueEntry.Completed = ""
      BY    PickPackStationQueueEntry.PickPackStationID
      BY    PickPackStationQueueEntry.Created DESC: /*idx=PickPackStationQueueEntryID*/   

      FIND FIRST ToteLineJourney OF PickPackStationQueueEntry NO-LOCK NO-ERROR. /*idx-ToteLineJourneyID*/
      FIND FIRST ShipOrder OF ToteLineJourney NO-LOCK NO-ERROR. /*idx-ShipOrderID*/  
      FIND FIRST Tote OF ToteLineJourney NO-LOCK NO-ERROR. /*idx-ToteID*/  
      
      FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStationID*/
            WHERE PickPackStation.PickPackStationID = PickPackStationQueueEntry.PickPackStationID NO-ERROR.                                                                                         
          
      PickPackStationQueueEntryBrowse:startRow(PickPackStationQueueEntry.PickPackStationQueueEntryID,
          "selectPickPackStationQueueEntryRow(this," + '"' + STRING(PickPackStationQueueEntry.PickPackStationQueueEntryID)
           + '"' + ");", "").    
          
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PickPackStationQueueEntry}
      
      PickPackStationQueueEntryBrowse:insertData(PickPackStationQueueEntry.PickPackStationQueueEntryID,                   "").
      PickPackStationQueueEntryBrowse:insertData((IF AVAILABLE PickPackStation THEN PickPackStation.StationName ELSE ""), "LEFT").
      PickPackStationQueueEntryBrowse:insertData(PickPackStationQueueEntry.ToteLineJourneyID,                             "LEFT").
      PickPackStationQueueEntryBrowse:insertData((IF AVAILABLE ShipOrder THEN Tote.ToteRef ELSE ""),                      "LEFT").
      PickPackStationQueueEntryBrowse:insertData((IF AVAILABLE Tote THEN ShipOrder.OrderRef ELSE ""),                     "LEFT").
      PickPackStationQueueEntryBrowse:insertData(fDisplayDate&Time(PickPackStationQueueEntry.Created,"y/m/d H:M:S"),      "LEFT").
      PickPackStationQueueEntryBrowse:insertData(fDisplayDate&Time(PickPackStationQueueEntry.Arrived,"y/m/d H:M:S"),      "LEFT").
      
      
      PickPackStationQueueEntryBrowse:endRow().
   END. /* FOR EACH PickPackLocEaseOfAccessLink */
   
   PickPackStationQueueEntryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PickPackStationQueueEntryBrowse:getErrors().
   
   /*Hidden Fields*/
   PickPackStationQueueEntryBrowseForm:insertHiddenField("form_name", "pickpackstationqueueentry_browse_form").
   PickPackStationQueueEntryBrowseForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   PickPackStationQueueEntryBrowseForm:insertHiddenField("popup_pickpackstationqueueentry_browse", "").
   PickPackStationQueueEntryBrowseForm:insertHiddenField("pickpackstationqueueentry_browse_scroll", "").
   PickPackStationQueueEntryBrowseForm:insertHiddenField("popup_pickpackstation_browse", "").
   PickPackStationQueueEntryBrowseForm:insertHiddenField("pickpackstation_browse_scroll", "").
   
   PickPackStationQueueEntryBrowseForm:insertHiddenField("PickPackStationQueueEntryID", ""). /* chrPickPackStationQueueEntryID */
   PickPackStationQueueEntryBrowseForm:insertHiddenField("PickPackStationQueueEntryVersionID", "").
   PickPackStationQueueEntryBrowseForm:insertHiddenField("PickPackStationID", chrPickPackStationID).
   PickPackStationQueueEntryBrowseForm:insertHiddenField("PickPackStationVersionID", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackStationQueueEntryBrowseForm}
   
   /* Create Button Bar */
   PickPackStationQueueEntryButtons = NEW buttonBar().
   
/*   PickPackStationQueueEntryButtons:addButton("pickpackstationqueueentry_browse_form_btn_details",             */
/*                                        fTL("Details"),                                                            */
/*                                        "viewWorkZoneLocationDetails('pickpackstationqueueentry_details_form');",*/
/*                                        (IF intSelectedPickPackLocEaseOfAccessLink > 0 THEN "" ELSE "Disabled")).  */
                                         
   PickPackStationQueueEntryButtons:addButton("pickpackstationqueueentry_browse_form_btn_remove",
                                              fTL("Remove"),
                                              "removeQueueEntry();",
                                              (IF intSelectedPickedPackStationQueueEntry > 0 THEN "" ELSE "Disabled")).

   PickPackStationQueueEntryButtons:addButton("pickpackstationqueueentry_browse_form_btn_cancel",
                                        fTL("Cancel"),
                                        "disablePopup('pickpackstationqueueentry_browse_form_popup');").
                                                                         
   PickPackStationQueueEntryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickPackStationQueueEntryBrowseForm:FormBrowse  = PickPackStationQueueEntryBrowse.
   PickPackStationQueueEntryBrowseForm:FormButtons = PickPackStationQueueEntryButtons.
   PickPackStationQueueEntryBrowseForm:endForm(). 
   
   PickPackStationQueueEntryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pReadFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReadFile Procedure
PROCEDURE pReadFile:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE ttFileLine.
   
   chrFileName        = get-value("UploadFile").
   chrDestinationFile = get-value("DestinationFile").
      
   /* This piece of code will only execute if the Destination File was posted */
   IF chrDestinationFile = "" THEN RETURN.

   logIsLogFile = ENTRY(2,chrDestinationFile,".") = "log".
   
   /* Import the file content into temp-table ttFileLine */
   intFileLineSequence = 0.
   
   INPUT STREAM sUploadFile FROM VALUE(chrDestinationFile).
   
   REPEAT:
      
      chrImportedLine = "".
      
      IMPORT STREAM sUploadFile UNFORMATTED chrImportedLine NO-ERROR.

      IF chrImportedLine <> "" THEN 
      DO:
         CREATE ttFileLine.
         ASSIGN intFileLineSequence   = intFileLineSequence + 1
                ttFileLine.FileLineNo = intFileLineSequence
                ttFileLine.LineString = chrimportedLine.
      END.
   END.
   
   INPUT STREAM sUploadFile CLOSE.
   
   /* Delete the first row of the file as it only represents column names */
   IF NOT logIsLogFile THEN 
   FOR FIRST ttFileLine WHERE ttFileLine.FileLineNo = 1:
      DELETE ttFileLine.
   END.

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
   
   intGblOperationTypeID = fGetTypeID("Operation","WebAdmin").
   
   FIND FIRST FileMaster NO-LOCK
      WHERE FileMaster.MasterName = "LocationUpload" NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrPageBuildError = "FileMaster LocationUpload does not exist.".
   END.
   ELSE intLocationUploadMasterID = FileMaster.FileMasterID.
   
   ASSIGN chrPickPackStationID                     = get-value("PickPackStationID")
          intSelectedPickPackStation               = INTEGER(chrPickPackStationID)
          chrScrollToPickPackStationRow            = STRING(INTEGER(get-value("pickpackstation_browse_scroll"))) + ";"
          chrShipOrderStreamPickPackLinkID         = get-value("ShipOrderStreamPickPackLinkID")
          intSelectedShipOrderStreamPickPackLink   = INTEGER(chrShipOrderStreamPickPackLinkID)
          chrPickPackStationHistoryID              = get-value("PickPackStationHistoryID")
          intSelectedPickPackStationHistory        = INTEGER(chrPickPackStationHistoryID)
          chrScrollToPickPackStationHistoryRow     = STRING(INTEGER(get-value("pickpackstationhistory_browse_scroll"))) + ";"
          chrSelectedPickedPackStationQueueEntry   = get-value("PickPackStationQueueEntryID")
          intSelectedPickedPackStationQueueEntry   = INTEGER(chrSelectedPickedPackStationQueueEntry)
          /*Upload Browes*/
          chrValidFile                             = get-value("ValidFile")
          chrFileID                                = get-value("FileID").

             
   /* Process URL values */
   IF chrPickPackStationID <> "" THEN
      chrPickPackStationRow = 'selectPickPackStationRow(document.getElementById("pickpackstation_browse_row_' + chrPickPackStationID + '"),"' 
                                                          + chrPickPackStationID +  '");'.
   IF chrShipOrderStreamPickPackLinkID <> "" THEN
      chrShipOrderStreamPickPackLinkRow = 'selectShipOrderStreamPickPackLinkRow(document.getElementById("shiporderstreampickpacklink_browse_row_' + chrShipOrderStreamPickPackLinkID + '"),"' 
                                                          + chrShipOrderStreamPickPackLinkID +  '");'.
                                                          
   IF get-value('popup_pickpackstationhistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("pickpackstationhistory_browse_form_popup");'.
  
   IF get-value('popup_shiporderstreampickpacklink_browse_form') = "yes" THEN
      chrPopupShipOrderStreamPickPackLink = 'enablePopup("shiporderstreampickpacklink_browse_form_popup");'.
      
   IF get-value('popup_zonelink_browse') = "Yes" THEN
      chrPopupZoneLink = 'enablePopup("zonelink_browse_form_popup");'.    
       
   IF get-value('popup_addzonelink_browse') = "Yes" THEN
      chrPopupAddZoneLink = 'enablePopup("addzonelink_browse_form_popup");'.
      
   IF get-value('popup_addshiporderstreampickpacklink_browse') = "Yes" THEN
      chrAddShipOrderStreamPickPackLink = 'enablePopup("addshiporderstreampickpacklink_browse_form_popup");'. 
      
   IF get-value('popup_pickpackloceaseofaccesslink_browse') = "Yes" THEN
      chrPopupPickPackLocEaseOfAccessLink = 'enablePopup("pickpackloceaseofaccesslink_browse_form_popup");'. 
      
   IF chrPickPackLocEaseOfAccessLinkID <> "" THEN
      chrPickPackLocEaseOfAccessLinkRow = 'selectPickPackLocEaseOfAccessLinkRow(document.getElementById("pickpackloceaseofaccesslink_browse_row_'
                               + chrPickPackLocEaseOfAccessLinkID + '"),"' + chrPickPackLocEaseOfAccessLinkID +  '");'.   
                               
   IF get-value('popup_pickpackstationqueueentry_browse') = "Yes" THEN
      chrPopupPickPackStationQueueEntry = 'enablePopup("pickpackstationqueueentry_browse_form_popup");'.                               
   
      /*PickPackLocEaseOfAccessLnkHst Browse*/  
   IF get-value('popup_pickpackloceaseofaccesslnkhst_browse') = "Yes" THEN
      chrPopupPickPackLocEaseOfAccessLnkHst = 'enablePopup("pickpackloceaseofaccesslnkhst_browse_form_popup");'.
      
      /* Build the Part Popup File Selection command if requested */
   IF get-value('popup_pickpackloceaseofaccesslink_selection_form') = "yes" THEN 
      chrPopupFileSelection = 'enablePopup("pickpackloceaseofaccesslink_selection_form_popup");'.
      
   /* Build the Popup uploadlocation browse command if requested */
   IF get-value('popup_uploadlocation_browse_form') = "yes" THEN
      chrPopupLocationUpload = 'enablePopup("uploadlocation_browse_form_popup");'.         
      
   /*Mismatches*/
   IF get-value('popup_partprofileorderstreamrplloc_browse') = "Yes" THEN
      chrPopupMismatches = 'enablePopup("partprofileorderstreamrplloc_browse_form_popup");'. 
   
    
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + "setDebugging();"
                             + 'document.getElementById("pickpackstation_browse").scrollTop=' + chrScrollToPickPackStationRow 
                             + chrPickPackStationRow + chrPopUpHistory 
                             + chrPopupShipOrderStreamPickPackLink + chrPopupZoneLink 
                             + chrPopupAddZoneLink + chrAddShipOrderStreamPickPackLink 
                             + chrPopupPickPackLocEaseOfAccessLink + chrPopupPickPackLocEaseOfAccessLnkHst 
                             + chrPopupFileSelection + chrPopupLocationUpload + chrPopupMismatches
                             + chrPopupPickPackStationQueueEntry.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "PickPack Station Admin".
   ThisPage:FrameTitle = "PickPack Station Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("pickpackstation.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPickPackStationBrowse.
   
   FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStationID*/
      WHERE PickPackLocEaseOfAccessLink.PickPackStationID = PickPackStation.PickPackStationID NO-ERROR.
            
   FIND FIRST PickPackLocEaseOfAccessLink NO-LOCK /*idx=LocationID*/
      WHERE PickPackLocEaseOfAccessLink.LocationID = Location.LocationID NO-ERROR.
      
   FIND FIRST ShipOrderStream NO-LOCK /*idx=LocationID*/
      WHERE PartProfileOrderStreamRplLoc.ShipOrderStreamID = ShipOrderStream.ShipOrderStreamID NO-ERROR.
            
   FIND FIRST Location NO-LOCK /*idx=LocationID*/
      WHERE PartProfileOrderStreamRplLoc.LocationID = Location.LocationID NO-ERROR.
   
   FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStationID*/
      WHERE PickPackStation.PickPackStationID = intSelectedPickPackStation NO-ERROR.
      
    FIND FIRST ShipOrderStream NO-LOCK /*idx=ShipOrderStreamID*/
      WHERE ShipOrderStream.ShipOrderStreamID = intSelectedShipOrderStream NO-ERROR.
      
    FIND FIRST PartProfile NO-LOCK /*idx=PartProfile*/
      WHERE PartProfileOrderStreamRplLoc.PartProfileID = PartProfile.PartProfileID NO-ERROR.  
   
   /******* Popup Browsers and Forms ********/    
   RUN pPickPackStationDetails.
   RUN pPickPackStationHistoryBrowse.
   RUN pPickPackStationHistoryDetails.
   RUN pShipOrderStreamPickPackLinkBrowse.
   RUN pZoneLinkBrowse.
   RUN pAddZoneLinkBrowse.
   RUN pAddShipOrderStreamPickPackLinkBrowse.
   RUN pWorkZoneLocations.
   RUN pWorkZoneLocationDetails.
   RUN pWorkZoneLocationsHistory.
   RUN pWorkZoneLocationUpload.
   RUN pReadFile.
   RUN pUploadLocationBrowse.
   RUN pMismatches.
   RUN pPickPackStationQueueEntryBrowse.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT PickPackStationBrowseFrame                      NO-ERROR.
   DELETE OBJECT PickPackStationBrowse                           NO-ERROR.
   DELETE OBJECT PickPackStationBrowseButtons                    NO-ERROR.
   DELETE OBJECT PickPackStationDetailsForm                      NO-ERROR.
   DELETE OBJECT PickPackStationDetailsButtons                   NO-ERROR.
   
   DELETE OBJECT PickPackLocEaseOfAccessLinkBrowseFrame          NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLinkBrowse               NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLinkButtons              NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLinkDetailsForm          NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLinkDetailsButtons       NO-ERROR.
   
   DELETE OBJECT PickPackLocEaseOfAccessLnkHstBrowseFrame        NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLnkHstBrowse             NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLnkHstButtons            NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLnkHstDetailsForm        NO-ERROR.
   DELETE OBJECT PickPackLocEaseOfAccessLnkHstDetailsButtons     NO-ERROR.
   
   DELETE OBJECT FileSelectionForm                               NO-ERROR.
   DELETE OBJECT FileBrowseForm                                  NO-ERROR.
   DELETE OBJECT FileBrowse                                      NO-ERROR.
   DELETE OBJECT FileBrowseButtons                               NO-ERROR.
   DELETE OBJECT FileDetailsButtons                              NO-ERROR.

   DELETE OBJECT UploadLocationBrowse                            NO-ERROR.
   DELETE OBJECT UploadLocationBrowseButtons                     NO-ERROR.
   DELETE OBJECT UploadLocationBrowseForm                        NO-ERROR.
   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pPickPackStationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "pickpackstation_details_form"}
   
   PickPackStationBrowse = NEW browseTable("pickpackstation_browse").
   PickPackStationBrowse:BrowseWidth  = 965.
   PickPackStationBrowse:BrowseHeight = 425.
   PickPackStationBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   PickPackStationBrowse:insertColumn(fTL("Station ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickPackStation}
   
   PickPackStationBrowse:insertColumn(fTL("Station Name"), 200, "CHARACTER", "LEFT", FALSE). 
   PickPackStationBrowse:insertColumn(fTL("Location"),     235, "CHARACTER", "LEFT", FALSE).   
   PickPackStationBrowse:insertColumn(fTL("Active"),       100, "LOGICAL",   "LEFT", FALSE). 
   PickPackStationbrowse:insertColumn(fTL("Using Jiffy"),  125, "LOGICAL",   "LEFT", FALSE).
   
   
   /*Body*/
   PickPackStationBrowse:startBody().
   
   FOR EACH PickPackStation NO-LOCK /*idx=ActiveStationName*/
      BY    PickPackStation.PickPackStationID: 
         
      FIND FIRST Location OF PickPackStation NO-LOCK NO-ERROR. /*idx=LocationID*/
            
      PickPackStationBrowse:startRow(PickPackStation.PickPackStationID, "selectPickPackStationRow(this," + '"' 
                                          + STRING(PickPackStation.PickPackStationID) + '"' + ");", "").
                                          
      PickPackStationBrowse:insertData(PickPackStation.PickPackStationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PickPackStation}      

      PickPackStationBrowse:insertData(PickPackStation.StationName,"LEFT"). 
      PickPackStationBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE "") ,"LEFT"). 
      PickPackStationBrowse:insertData(STRING(PickPackStation.Active, "Yes/No"),"LEFT").
      PickPackStationBrowse:insertData(STRING(PickPackStation.UsingJiffyMachine, "Yes/No"), "LEFT").
            
      /* Add hidden fields */
      PickPackStationBrowse:insertHiddenData("PickPackStationID",PickPackStation.PickPackStationID).      
      PickPackStationBrowse:insertHiddenData("PickPackStationVersionID",PickPackStation.VersionID).
      
      PickPackStationBrowse:endRow().
      
   END. /*FOR EACH PickPackStation NO-LOCK */
   
   PickPackStationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PickPackStationBrowse:getErrors().
   
   /* Create a new frame */
   PickPackStationBrowseFrame = NEW pageFrame().
   PickPackStationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PickPackStationBrowseFrame:FormAction="dbPickPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PickPackStationBrowseFrame:formOpen("pickpackstation_browse_form").
   
   /* Start the Frame Header */
   PickPackStationBrowseFrame:insertSpacer(5).
   PickPackStationBrowseFrame:frameOpen(985, 465, "").  
   
   /* This outputs the Browse Table */  
   PickPackStationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PickPackStationBrowseFrame:frameClose().
   PickPackStationBrowseFrame:insertSpacer(10).
   
   
   PickPackStationBrowseFrame:insertHiddenField("DebuggingIsOn",STRING(logGblDebugging)).
   PickPackStationBrowseFrame:insertHiddenField("OperationTypeID",STRING(intGblOperationTypeID)).
   PickPackStationBrowseFrame:insertHiddenField("pickpackstation_browse_scroll","").
   PickPackStationBrowseFrame:insertHiddenField("PickPackStationID", chrPickPackStationID).
   PickPackStationBrowseFrame:insertHiddenField("PickPackStationVersionID","").
   PickPackStationBrowseFrame:insertHiddenField("PickPackWorkZoneLinkID", "").
   PickPackStationBrowseFrame:insertHiddenField("PickPackWorkZoneActive", "").
   PickPackStationBrowseFrame:insertHiddenField("PickPackWorkZoneVersionID","").
   
   PickPackStationBrowseFrame:insertHiddenField("popup_pickpackstationhistory_browse","").
   PickPackStationBrowseFrame:insertHiddenField("popup_shiporderstreampickpacklink_browse_form","").
   PickPackStationBrowseFrame:insertHiddenField("popup_addshiporderstreampickpacklink_browse_form","").
   PickPackStationBrowseFrame:insertHiddenField("popup_zonelink_browse","").
   PickPackStationBrowseFrame:insertHiddenField("popup_addzonelink_browse","").
   PickPackStationBrowseFrame:insertHiddenField("popup_pickpackloceaseofaccesslink_browse", "").
   PickPackStationBrowseFrame:insertHiddenField("popup_pickpackloceaseofaccesslnkhst_browse", "").
   PickPackStationBrowseFrame:insertHiddenField("popup_uploadlocation_browse_form","").
   PickPackStationBrowseFrame:insertHiddenField("popup_pickpackloceaseofaccesslink_selection_form","").
   PickPackStationBrowseFrame:insertHiddenField("popup_partprofileorderstreamrplloc_browse","").
   PickPackStationBrowseFrame:insertHiddenField("popup_pickpackstationqueueentry_browse","").
   
   
   PickPackStationBrowseFrame:insertHiddenField("form_name","pickpackstation_browse_form").
   PickPackStationBrowseFrame:insertHiddenField("prog_name","adPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackStationBrowseFrame}
   
   PickPackStationBrowseFrame:formClose().
   
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   PickPackStationBrowseButtons = NEW buttonBar().
   PickPackStationBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   PickPackStationBrowseButtons:addButton("pickpackstation_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewPickPackStationDetails('pickpackstation_details_form');",
                                             (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).
                                             
   PickPackStationBrowseButtons:addButton("pickpackstation_browse_form_btn_zonelink",
                                             fTL("Zone Links"),                                           
                                             "viewZoneLinkBrowse('zonelink_browse_form');",
                                             (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).
                                    
   PickPackStationBrowseButtons:addButton("pickpackstation_browse_form_btn_shiporderstreampickpacklink",
                                             fTL("ShipOrder Streams"),
                                             "viewShipOrderStreamPickPackLink('pickpackstation_browse_form');",
                                             (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).
  
   IF NOT logPreventDataCreates THEN
   DO:  
   PickPackStationBrowseButtons:addButton("pickpackstation_browse_form_btn_create",
                                             fTL("Create"),
                                             "createPickPackStation('pickpackstation_details_form');",
                                             "").
   END.                                         
   
   PickPackStationBrowseButtons:addButton("pickpackstation_browse_form_btn_history",
                                             fTL("History"),
                                             "viewPickPackStationHistory('pickpackstation_browse_form');",
                                             (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).
                                             
   PickPackStationBrowseButtons:addButton("pickpackstation_browse_form_btn_locations",
                                            fTL("Ease Of Access"),
                                            "viewWorkZoneLocations('pickpackloceaseofaccesslink_browse_form');",
                                            (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).
                                         
   PickPackStationBrowseButtons:addButton("pickpackstation_browse_form_btn_mismatches",
                                            fTL("Mismatches"),
                                            "viewMismatches('pickpackloceaseofaccesslink_browse_form');",
                                            (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).                           
   
   PickPackStationBrowseButtons:closeBar().  
   PickPackStationBrowseButtons:displayButtonBar().  
   
   /* Create Button Bar */
   
      PickPackStationBrowseFrame:insertSpacer(10).
      PickPackStationBrowseMoreButtons = NEW buttonBar().
      PickPackStationBrowseMoreButtons:WebStream = STREAM WebStream:HANDLE.  
      
      IF chrGblEnvironment <> "LIVE" THEN
      DO:
         PickPackStationBrowseMoreButtons:addButton("pickpackstation_browse_form_btn_unlink",
                                                      fTL("Remove Links"),
                                                      "viewUnlinkConfirmation();",
                                                      (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).
                                                   
      END.                                             
                                                   
      PickPackStationBrowseMoreButtons:addButton("pickpackstation_browse_form_btn_queueentry",
                                                   fTL("Queue Entry"),
                                                   "viewQueueEntry('pickpackstationqueueentry_browse_form');",
                                                   (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).                                             
      
      PickPackStationBrowseMoreButtons:closeBar().
      PickPackStationBrowseMoreButtons:displayButtonBar().
     
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pPickPackStationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "pickpackstation_details_form"}
   ASSIGN chrDisplayFieldList  = "PickPackStationID,LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active"
                                 + ",PickPackTypeID,DefaultTargetUph,UsingJiffyMachine,ResolutionLocationID"
                                 + ",CellSequence,UsableToteSpacesInQueue,TotalToteSpacesInQueue,TargetActionPointID"
                                 + ",ToteLineCellID"
          chrEditFieldList     = "LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active,PickPackTypeID"
                                 + ",DefaultTargetUph,UsingJiffyMachine,ResolutionLocationID,CellSequence"
                                 + ",UsableToteSpacesInQueue,TotalToteSpacesInQueue,TargetActionPointID,ToteLineCellID"
          chrNewFieldList      = "LocationID,StationName,OnHoldLocationID,PostPackoutLocationID,Active,PickPackTypeID"
                                 + ",DefaultTargetUph,UsingJiffyMachine,ResolutionLocationID,CellSequence"
                                 + ",UsableToteSpacesInQueue,TotalToteSpacesInQueue,TargetActionPointID,ToteLineCellID"
          chrRequiredFieldList = "StationName,LocationID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   
   PickPackStationDetailsForm = NEW dataForm("pickpackstation_details_form").
   PickPackStationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PickPackStationDetailsForm:FormAction = "dbPickPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PickPackStationDetailsForm:FormWidth   = 580.
   PickPackStationDetailsForm:FormHeight  = 420.
   PickPackStationDetailsForm:FormTitle   = "PickPack Station details".
   PickPackStationDetailsForm:FormType    = "large".
   
   /* Column Layout */
   PickPackStationDetailsForm:insertPaddingColumn(30).
   PickPackStationDetailsForm:insertColumn(180).
   
   /* Fields */
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Station ID")).
   PickPackStationDetailsForm:insertTextField("PickPackStationID", "", 190, TRUE).  
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Station Name")).
   PickPackStationDetailsForm:insertTextField("StationName", "", 190, TRUE).
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Default Target Uph")).
   PickPackStationDetailsForm:insertTextField("DefaultTargetUph", "", 190, TRUE).  
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Location")).
   PickPackStationDetailsForm:insertComboField("LocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PickPackStation" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END. 
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("OnHold Location")).
   PickPackStationDetailsForm:insertComboField("OnHoldLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PickPackOnHold" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef).  
   END.
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("PickPack TypeID")).
   PickPackStationDetailsForm:insertComboField("PickPackTypeID", "", 190, TRUE).  
   
     FIND FIRST PickPackType OF PickPackStation NO-LOCK NO-ERROR. /*idx=PickPackType*/
     
     FOR EACH PickPackType NO-LOCK  /*idx=PickPackTypeID*/                                                                                                      
      BY PickPackType.PickPackTypeID:  
                                                                                                         
       PickPackStationDetailsForm:insertComboPairs("PickPackTypeID", STRING(PickPackType.PickPackTypeID), PickPackType.TypeName).
      END. /*FOR EACH PickPackType NO-LOCK*/
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("PostPackout Location")).
   PickPackStationDetailsForm:insertComboField("PostPackoutLocationID", "", 190, TRUE).
   PickPackStationDetailsForm:insertComboPairs("PostPackoutLocationID", "0", "None Selected...").  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PickPackPostPack" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationDetailsForm:insertComboPairs("PostPackoutLocationID", STRING(Location.LocationID), Location.LocationRef).
   END. 
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("ToteLine Cell")).
   PickPackStationDetailsForm:insertComboField("ToteLineCellID", "", 190, TRUE).
   PickPackStationDetailsForm:insertComboPairs("ToteLineCellID", "0", "None Selected...").

   FIND FIRST ToteLineCell NO-LOCK
      WHERE ToteLineCell.ToteLineCellID = PickPackStation.ToteLineCellID NO-ERROR.

   FOR EACH ToteLineCell NO-LOCK /*idx=ToteLineID*/
      WHERE ToteLineCell.Active:

      PickPackStationDetailsForm:insertComboPairs("ToteLineCellID", STRING(ToteLineCell.ToteLineCellID), ToteLineCell.ToteLineCellName).
   END.
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Cell Sequence")).
   PickPackStationDetailsForm:insertTextField("CellSequence", "", 190, TRUE).
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Usable Tote Spaces In Queue")).
   PickPackStationDetailsForm:insertTextField("UsableToteSpacesInQueue", "", 190, TRUE).
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Total ToteS paces In Queue")).
   PickPackStationDetailsForm:insertTextField("TotalToteSpacesInQueue", "", 190, TRUE).
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Target Action Point")).
   PickPackStationDetailsForm:insertComboField("TargetActionPointID", "", 190, TRUE).
   PickPackStationDetailsForm:insertComboPairs("TargetActionPointID", "0", "None Selected...").

   FIND FIRST NetworkActionPoint NO-LOCK /*idx=NetworkActionPointID*/
      WHERE NetworkActionPoint.NetworkActionPointID = PickPackStation.TargetActionPointID NO-ERROR.

   FOR EACH NetworkActionPoint NO-LOCK /*idx=NetworkActionPointID*/
      WHERE NetworkActionPoint.Active:

      PickPackStationDetailsForm:insertComboPairs("TargetActionPointID", STRING(NetworkActionPoint.NetworkActionPointID), NetworkActionPoint.NetworkActionPointName).
   END.
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Resolution Location")).
   PickPackStationDetailsForm:insertComboField("ResolutionLocationID", "", 190, TRUE).  
   PickPackStationDetailsForm:insertComboPairs("ResolutionLocationID", "0", "None Selected...").
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "ToteLineResolution" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationDetailsForm:insertComboPairs("ResolutionLocationID", STRING(Location.LocationID), Location.LocationRef).
   END. 
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("UsingJiffyMachine")).
   PickPackStationDetailsForm:insertComboField("UsingJiffyMachine", "", 190, TRUE).
   PickPackStationDetailsForm:insertComboPairs("UsingJiffyMachine", "yes", "Yes").
   PickPackStationDetailsForm:insertComboPairs("UsingJiffyMachine", "no", "No").
   
   PickPackStationDetailsForm:startRow().
   PickPackStationDetailsForm:insertLabel(fTL("Active")).
   PickPackStationDetailsForm:insertComboField("Active", "", 190, TRUE).
   PickPackStationDetailsForm:insertComboPairs("Active", "yes", "Yes").
   PickPackStationDetailsForm:insertComboPairs("Active", "no", "No").


   {webGetOptionalFormFields.i pPickPackStationDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PickPackStationDetailsForm:insertHiddenField("pickpackstation_browse_scroll", "").
   PickPackStationDetailsForm:insertHiddenField("form_name", "pickpackstation_details_form").
   PickPackStationDetailsForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackStationDetailsForm}
   
   /* Create Button Bar */
   PickPackStationDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataCreates THEN 
      PickPackStationDetailsButtons:addButton("pickpackstation_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updatePickPackStation('pickpackstation_details_form');").
                                                   
   PickPackStationDetailsButtons:addButton("pickpackstation_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('pickpackstation_details_form_popup');").
   PickPackStationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickPackStationDetailsForm:FormButtons = PickPackStationDetailsButtons.
   
   PickPackStationDetailsForm:endForm(). 
   
   PickPackStationDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPickPackStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickPackStationDetailsFields Procedure 
PROCEDURE pPickPackStationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      PickPackStationDetailsForm:startRow().
      PickPackStationDetailsForm:insertLabel(fTL("Field Label")).
      PickPackStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pPickPackStationHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "pickpackstationhistory_details_form"}
   ASSIGN chrDisplayFieldList  = "PickPackStationHistoryID,LocationID,StationName,OnHoldLocationID,PostPackoutLocationID"
                               + ",PickPackTypeID,Active,GateUserID,CreatedDate,CreatedHour,CreatedMins,DefaultTargetUph,UsingJiffyMachine"
                               + ",ResolutionLocationID,ToteLineCellID,CellSequence,UsableToteSpacesInQueue,TotalToteSpacesInQueue"  
                               + ",TargetActionPointID"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PickPackStationHistoryDetailsForm = NEW dataForm("pickpackstationhistory_details_form").
   PickPackStationHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PickPackStationHistoryDetailsForm:FormAction = "dbPickPackStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PickPackStationHistoryDetailsForm:FormWidth   = 700.
   PickPackStationHistoryDetailsForm:FormHeight  = 490.
   PickPackStationHistoryDetailsForm:FormTitle   = "PickPack Station History Details".
   PickPackStationHistoryDetailsForm:FormType    = "xl_large".
   
   /* Column Layout */
   PickPackStationHistoryDetailsForm:insertPaddingColumn(30).
   PickPackStationHistoryDetailsForm:insertColumn(200).
   PickPackStationHistoryDetailsForm:insertColumn(125).
   PickPackStationHistoryDetailsForm:insertColumn(20).
   PickPackStationHistoryDetailsForm:insertColumn(4).
   PickPackStationHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Station ID")).
   PickPackStationHistoryDetailsForm:insertTextField("PickPackStationHistoryID", "", 190, TRUE).  
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Station Name")).
   PickPackStationHistoryDetailsForm:insertTextField("StationName", "", 190, TRUE).
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Default Target Uph")).
   PickPackStationHistoryDetailsForm:insertTextField("DefaultTargetUph", "", 190, TRUE).  
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Location")).
   PickPackStationHistoryDetailsForm:insertComboField("LocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PickPackStation" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationHistoryDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef). 
   END. 

   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("PickPack TypeID")).
   PickPackStationHistoryDetailsForm:insertComboField("PickPackTypeID", "", 190, TRUE).  
   
   FIND FIRST PickPackType OF PickPackStation NO-LOCK NO-ERROR. /*idx=PickPackType*/
     
   FOR EACH PickPackType NO-LOCK  /*idx=PickPackTypeID*/                                                                                                      
      BY PickPackType.PickPackTypeID:  
                                                                                                         
      PickPackStationHistoryDetailsForm:insertComboPairs("PickPackTypeID", STRING(PickPackType.PickPackTypeID), PickPackType.TypeName).
  END. /*FOR EACH PickPackType NO-LOCK*/
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("OnHold Location")).
   PickPackStationHistoryDetailsForm:insertComboField("OnHoldLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PickPackOnHold" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationHistoryDetailsForm:insertComboPairs("OnHoldLocationID", STRING(Location.LocationID), Location.LocationRef).  
   END.
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("PostPackout Location")).
   PickPackStationHistoryDetailsForm:insertComboField("PostPackoutLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "PickPackPostPack" NO-ERROR.
                                   
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationHistoryDetailsForm:insertComboPairs("PostPackoutLocationID", STRING(Location.LocationID), Location.LocationRef).
   END. 
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("ToteLine Cell")).
   PickPackStationHistoryDetailsForm:insertComboField("ToteLineCellID", "", 190, TRUE).

   FIND FIRST ToteLineCell NO-LOCK
      WHERE ToteLineCell.ToteLineCellID = PickPackStation.ToteLineCellID NO-ERROR.

   FOR EACH ToteLineCell NO-LOCK /*idx=ToteLineID*/
      WHERE ToteLineCell.Active:

      PickPackStationHistoryDetailsForm:insertComboPairs("ToteLineCellID", STRING(ToteLineCell.ToteLineCellID), ToteLineCell.ToteLineCellName).
   END.
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Cell Sequence")).
   PickPackStationHistoryDetailsForm:insertTextField("CellSequence", "", 190, TRUE).
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Usable Tote Spaces In Queue")).
   PickPackStationHistoryDetailsForm:insertTextField("UsableToteSpacesInQueue", "", 190, TRUE).
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Total ToteS paces In Queue")).
   PickPackStationHistoryDetailsForm:insertTextField("TotalToteSpacesInQueue", "", 190, TRUE).
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Target Action Point")).
   PickPackStationHistoryDetailsForm:insertComboField("TargetActionPointID", "", 190, TRUE).

   FIND FIRST NetworkActionPoint NO-LOCK /*idx=NetworkActionPointID*/
      WHERE NetworkActionPoint.NetworkActionPointID = PickPackStation.TargetActionPointID NO-ERROR.

   FOR EACH NetworkActionPoint NO-LOCK /*idx=NetworkActionPointID*/
      WHERE NetworkActionPoint.Active:

      PickPackStationHistoryDetailsForm:insertComboPairs("TargetActionPointID", STRING(NetworkActionPoint.NetworkActionPointID), NetworkActionPoint.NetworkActionPointName).
   END.
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Resolution Location")).
   PickPackStationHistoryDetailsForm:insertComboField("ResolutionLocationID", "", 190, TRUE).  
   
   FIND FIRST LocationType NO-LOCK /*idx=TypeCode*/
      WHERE LocationType.TypeCode = "ToteLineResolution" NO-ERROR.
      
   FOR EACH Location NO-LOCK /*idx=LocationTypeActive*/
      WHERE Location.LocationTypeID = LocationType.LocationTypeID
      AND   Location.Active:
      
      PickPackStationHistoryDetailsForm:insertComboPairs("ResolutionLocationID", STRING(Location.LocationID), Location.LocationRef).
   END. 
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("UsingJiffyMachine")).
   PickPackStationHistoryDetailsForm:insertComboField("UsingJiffyMachine", "", 190, TRUE).
   PickPackStationHistoryDetailsForm:insertComboPairs("UsingJiffyMachine", "yes", "Yes").
   PickPackStationHistoryDetailsForm:insertComboPairs("UsingJiffyMachine", "no", "No").
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Active")).
   PickPackStationHistoryDetailsForm:insertComboField("Active", "", 190, TRUE).
   PickPackStationHistoryDetailsForm:insertComboPairs("Active", "yes", "Yes").
   PickPackStationHistoryDetailsForm:insertComboPairs("Active", "no", "No").
   
   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Created")).
   PickPackStationHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).  
   /* Time fields have no label */
   PickPackStationHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   PickPackStationHistoryDetailsForm:insertLabel(":").
   PickPackStationHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE). 

   PickPackStationHistoryDetailsForm:startRow().
   PickPackStationHistoryDetailsForm:insertLabel(fTL("Gate User ID")).
   PickPackStationHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE). 
     
   FOR EACH GateUser NO-LOCK /*idx=GateUserID*/
      BY GateUser.FullName:
         
      PickPackStationHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.

   {webGetOptionalFormFields.i pPickPackStationHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PickPackStationHistoryDetailsForm:insertHiddenField("pickpackstationhistory_browse_scroll", "").
   PickPackStationHistoryDetailsForm:insertHiddenField("form_name", "pickpackstationhistory_details_form").
   PickPackStationHistoryDetailsForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackStationHistoryDetailsForm}
   
   /* Create Button Bar */
   PickPackStationHistoryDetailsButtons = NEW buttonBar().

   PickPackStationHistoryDetailsButtons:addButton("pickpackstationhistory_details_form_btn_cancel", 
                                                     fTL("Cancel"), 
                                                     "disablePopup('pickpackstationhistory_details_form_popup');").
   PickPackStationHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickPackStationHistoryDetailsForm:FormButtons = PickPackStationHistoryDetailsButtons.
   
   PickPackStationHistoryDetailsForm:endForm(). 
   
   PickPackStationHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pPickPackStationHistoryDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      PickPackStationHistoryDetailsForm:startRow().
      PickPackStationHistoryDetailsForm:insertLabel(fTL("Field Label")).
      PickPackStationHistoryDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
/*/*    {adPickPackStation_pickpackstation_details_form.i}*/ COMEBACK START COMEBACK END*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPickPackStationHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPickPackStationHistoryBrowse Procedure

PROCEDURE pPickPackStationHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   PickPackStationHistoryBrowseForm           = NEW dataForm("pickpackstationhistory_browse_form").
   PickPackStationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PickPackStationHistoryBrowseForm:FormWidth  = 860.
   PickPackStationHistoryBrowseForm:FormHeight = 530.
   PickPackStationHistoryBrowseForm:FormTitle  = fTL("PickPack Station History").
   PickPackStationHistoryBrowseForm:FormType   = "xxl_large".
   PickPackStationHistoryBrowse                = NEW browseTable("pickpackstationhistory_browse").
   PickPackStationHistoryBrowse:BrowseWidth    = 840.
   PickPackStationHistoryBrowse:BrowseHeight   = 490.
   
   PickPackStationHistoryBrowse:insertColumn(fTL("History ID"),    100, "INTEGER",           FALSE).
   PickPackStationHistoryBrowse:insertColumn(fTL("Station Name"),  150, "CHARACTER", "LEFT", FALSE).
   PickPackStationHistoryBrowse:insertColumn(fTL("Location"),      175, "CHARACTER", "LEFT", FALSE).
   PickPackStationHistoryBrowse:insertColumn(fTL("User"),          200, "CHARACTER", "LEFT", FALSE).
   PickPackStationHistoryBrowse:insertColumn(fTL("Created"),       150, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickPackStationHistory}
   
   PickPackStationHistoryBrowse:StartBody().
   
   FOR EACH PickPackStationHistory NO-LOCK /*idx=PickPackStationID*/
      WHERE PickPackStationHistory.PickPackStationID = intSelectedPickPackStation
      BY    PickPackStationHistory.PickPackStationHistoryID:
          
      FIND FIRST GateUser OF PickPackStationHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/
      FIND FIRST Location OF PickPackStationHistory NO-LOCK NO-ERROR. /*idx=LocationID*/

      PickPackStationHistoryBrowse:startRow (PickPackStationHistory.PickPackStationHistoryID, 
         "selectPickPackStationHistoryRow(this," + '"' + STRING(PickPackStationHistory.PickPackStationHistoryID) + '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PickPackStationHistory}
      
      PickPackStationHistoryBrowse:insertData(PickPackStationHistory.PickPackStationHistoryID).
      PickPackStationHistoryBrowse:insertData(PickPackStationHistory.StationName,"LEFT").
      PickPackStationHistoryBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""),"LEFT").
      PickPackStationHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      PickPackStationHistoryBrowse:insertData(fDisplayDate&Time(PickPackStationHistory.Created,"y/m/d H:M:S"), "LEFT").
      
      PickPackStationHistoryBrowse:endRow().
   END. /* FOR EACH PickPackStationHistory */
   
   PickPackStationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PickPackStationHistoryBrowse:getErrors().
   
   PickPackStationHistoryBrowseForm:insertHiddenField("popup_pickpackstationhistory_browse","").
   PickPackStationHistoryBrowseForm:insertHiddenField("PickPackStationHistoryID","").
      
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackStationHistoryBrowseForm}
   
   /* Create Button Bar */
   PickPackStationHistoryButtons = NEW buttonBar().
   
      PickPackStationHistoryButtons:addButton("pickpackstationhistory_browse_form_btn_details",
                                              fTL("Details"),
                                              "viewPickPackStationHistoryDetails('pickpackstationhistory_details_form');",
                                              (IF intSelectedPickPackStationHistory > 0 THEN "" ELSE "Disabled")).    
                                         
   PickPackStationHistoryButtons:addButton("pickpackstationhistory_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('pickpackstationhistory_browse_form_popup');").
   PickPackStationHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickPackStationHistoryBrowseForm:FormBrowse  = PickPackStationHistoryBrowse.
   PickPackStationHistoryBrowseForm:FormButtons = PickPackStationHistoryButtons.
   PickPackStationHistoryBrowseForm:endForm(). 
   
   PickPackStationHistoryBrowseForm:displayForm().  
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-pWorkZoneBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWorkZoneBrowse Procedure
PROCEDURE pShipOrderStreamPickPackLinkBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   ShipOrderStreamPickPackLinkBrowseForm           = NEW dataForm("shiporderstreampickpacklink_browse_form").
   ShipOrderStreamPickPackLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   ShipOrderStreamPickPackLinkBrowseForm:FormWidth  = 860.
   ShipOrderStreamPickPackLinkBrowseForm:FormHeight = 530.
   ShipOrderStreamPickPackLinkBrowseForm:FormTitle  = fTL("ShipOrder Streams Linked to PickPack Station: " + chrPickPackStationID).                                           
                                          
   ShipOrderStreamPickPackLinkBrowseForm:FormType   = "xxl_large".
   ShipOrderStreamPickPackLinkBrowseForm:FormAction = "dbSetShipOrderStreamPickPackLinkActive.p?" + TRIM(chrGblDefaultUrlValues,"&").
   ShipOrderStreamPickPackLinkBrowse                = NEW browseTable("shiporderstreampickpacklink_browse").
   ShipOrderStreamPickPackLinkBrowse:BrowseWidth    = 840.
   ShipOrderStreamPickPackLinkBrowse:BrowseHeight   = 490.
   ShipOrderStreamPickPackLinkBrowse:ExcelExport    = TRUE.
   ShipOrderStreamPickPackLinkBrowse:SessionID      = intGblSessionID.
   ShipOrderStreamPickPackLinkBrowse:WebStream      = STREAM WebStream:HANDLE.
   
   ShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Link ID"),       100, "INTEGER"           , FALSE).
   ShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Stream ID"),     100, "INTEGER"           , FALSE).
   ShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Stream Name"),   150, "CHARACTER", "LEFT" , FALSE).
   ShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Description"),   151, "CHARACTER", "LEFT" , FALSE).
   ShipOrderStreamPickPackLinkBrowse:insertColumn(fTL("Active"),         90, "LOGICAL"           , FALSE).
   
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ShipOrderStreamPickPackLink}
   
   ShipOrderStreamPickPackLinkBrowse:StartBody().

   FOR EACH ShipOrderStreamPickPackLink NO-LOCK  /*idx=ShipOrderStreamID*/
      WHERE ShipOrderStreamPickPackLink.PickPackStationID = PickPackStation.PickPackStationID,
         EACH  ShipOrderStream OF ShipOrderStreamPickPackLink NO-LOCK: /*idx=PickPackStationID*/
                  
         ShipOrderStreamPickPackLinkBrowse:startRow(ShipOrderStreamPickPackLink.ShipOrderStreamPickPackLinkID, "selectShipOrderStreamPickPackLinkRow(this," + '"'
                                                               + STRING(ShipOrderStreamPickPackLink.ShipOrderStreamPickPackLinkID) + '"' + ");", "").
                                                               
         ShipOrderStreamPickPackLinkBrowse:insertData(ShipOrderStreamPickPackLink.ShipOrderStreamPickPackLinkID).
   
         /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
         {webGetOptionalBrowseFields.i ShipOrderStreamPickPackLink}
   
         ShipOrderStreamPickPackLinkBrowse:insertData(ShipOrderStream.ShipOrderStreamID).
         ShipOrderStreamPickPackLinkBrowse:insertData(ShipOrderStream.StreamName, "LEFT").
         ShipOrderStreamPickPackLinkBrowse:insertData(ShipOrderStream.StreamDescr, "LEFT").
         ShipOrderStreamPickPackLinkBrowse:insertData(STRING(ShipOrderStreamPickPackLink.Active, "Yes/No")).
   
         ShipOrderStreamPickPackLinkBrowse:insertHiddenData("ShipOrderStreamVersionID",ShipOrderStream.VersionID).
         ShipOrderStreamPickPackLinkBrowse:insertHiddenData("ShipOrderStreamPickPackLinkID",ShipOrderStreamPickPackLink.ShipOrderStreamPickPackLinkID).
         ShipOrderStreamPickPackLinkBrowse:insertHiddenData("ShipOrderStreamPickPackLinkVersionID",ShipOrderStreamPickPackLink.VersionID).
         ShipOrderStreamPickPackLinkBrowse:insertHiddenData("PickPackStationID",PickPackStation.PickPackStationID).
         ShipOrderStreamPickPackLinkBrowse:insertHiddenData("PickPackStationVersionID",PickPackStation.VersionID).
         ShipOrderStreamPickPackLinkBrowse:insertHiddenData("LinkActive",ShipOrderStreamPickPackLink.Active).
   
         ShipOrderStreamPickPackLinkBrowse:endRow().
   END. /* FOR EACH PickPackStation */

   ShipOrderStreamPickPackLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + ShipOrderStreamPickPackLinkBrowse:getErrors().
   
   /* Hidden Fields */
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("pickpackstation_browse_scroll","").
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamID","").
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamVersionID","").
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamPickPackLinkID",chrShipOrderStreamPickPackLinkID).
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamPickPackLinkVersionID","").
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("PickPackStationID",chrPickPackStationID).
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("PickPackStationVersionID","").
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("ShipOrderStreamPickPackLinkActive","").

   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("popup_shiporderstreampickpacklink_browse_form","").
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("popup_addshiporderstreampickpacklink_browse","").

   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("form_name","shiporderstreampickpacklink_browse_form").
   ShipOrderStreamPickPackLinkBrowseForm:insertHiddenField("prog_name","adPickPackStationAdmin.p").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderStreamPickPackLinkBrowseForm}
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   ShipOrderStreamPickPackLinkBrowseButtons = NEW buttonBar().
   
   
   
   ShipOrderStreamPickPackLinkBrowseButtons:addButton("shiporderstreampickpacklink_browse_form_btn_create",
                                        fTL("Add Links"),
                                        "addShipOrderStreamPickPackLink();").

   ShipOrderStreamPickPackLinkBrowseButtons:addButton("shiporderstreampickpacklink_browse_form_btn_deactivatelink",
                                        fTL("Deactivate Link"),
                                        "setShipOrderStreamPickPackLinkActiveFlag('shiporderstreampickpacklink_browse_form');",
                                        (IF intSelectedShipOrderStreamPickPackLink > 0 THEN "" ELSE "Disabled")).

/*   PickPackStationBrowseButtons:addButton("shiporderstream_type_browse_form_btn_history",                 */
/*                                        fTL("History"),                                                  */
/*                                        "viewShipOrderStreamPackOut('shiporderstream_type_browse_form')",*/
/*                                        (IF intSelectedPickPackStation > 0 THEN "" ELSE "Disabled")).    */
   
   ShipOrderStreamPickPackLinkBrowseButtons:addButton("shiporderstreampickpacklink_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('shiporderstreampickpacklink_browse_form_popup');",
                                         "").
                         
   ShipOrderStreamPickPackLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ShipOrderStreamPickPackLinkBrowseForm:FormBrowse  = ShipOrderStreamPickPackLinkBrowse.
   ShipOrderStreamPickPackLinkBrowseForm:FormButtons = ShipOrderStreamPickPackLinkBrowseButtons.
   ShipOrderStreamPickPackLinkBrowseForm:endForm(). 
   
   ShipOrderStreamPickPackLinkBrowseForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pZoneLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pZoneLinkBrowse Procedure
PROCEDURE pZoneLinkBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   IF AVAILABLE WorkZone THEN
      chrFormTitle = fTL("Work Zone for  Pick and Pack: " + WorkZone.WorkZoneName + chrSpaces + "ID: " + STRING(WorkZone.WorkZoneID)).
   ELSE
      chrFormTitle = fTL("Work Zone for  Pick and Pack").      

   /* Form Setup */
   ZoneLinkBrowseForm            = NEW dataForm("zonelink_browse_form").
   ZoneLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   ZoneLinkBrowseForm:FormWidth  = 580.
   ZoneLinkBrowseForm:FormHeight = 420.
   ZoneLinkBrowseForm:FormTitle  = chrFormTitle.
   ZoneLinkBrowseForm:FormType   = "large".
   ZoneLinkBrowseForm:FormAction = "dbPickPackWorkZoneLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues, "&").
   
   /* Browse Setup */
   ZoneLinkBrowse              = NEW browseTable("zonelink_browse").
   ZoneLinkBrowse:BrowseWidth  = 560.
   ZoneLinkBrowse:BrowseHeight = 375.
   ZoneLinkBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Column Headers */
   ZoneLinkBrowse:insertColumn(fTL("LinkID"),    90, "INTEGER", FALSE).
   ZoneLinkBrowse:insertColumn(fTL("Work Zone"), 100, "CHARACTER", FALSE).
   ZoneLinkBrowse:insertColumn(fTL("Active"),    80, "CHARACTER", FALSE).
   
   ZoneLinkBrowse:StartBody().
  
   IF AVAILABLE PickPackStation THEN
   DO:
      FOR EACH PickPackWorkZoneLink NO-LOCK /* idx=WorkZoneID */
         WHERE PickPackWorkZoneLink.PickPackStationID = PickPackStation.PickPackStationID:
              
         FIND FIRST WorkZone OF PickPackWorkZoneLink NO-LOCK NO-ERROR. /* idx=WorkZoneID */
         
         ZoneLinkBrowse:startRow(PickPackWorkZoneLink.PickPackWorkZoneLinkID,
                                 'selectZoneLinkRow(this,"' + STRING(PickPackWorkZoneLink.PickPackWorkZoneLinkID) + '");',
                                 "").  
         ZoneLinkBrowse:insertData(PickPackWorkZoneLink.PickPackWorkZoneLinkID).
         ZoneLinkBrowse:insertData(IF AVAILABLE WorkZone THEN WorkZone.WorkZoneName ELSE STRING(PickPackWorkZoneLink.WorkZoneID)).                         
         ZoneLinkBrowse:insertData(STRING(PickPackWorkZoneLink.Active, "Yes/No")).
         
         /* Hidden Field */
         ZoneLinkBrowse:insertHiddenData("PickPackWorkZoneLinkActive", STRING(PickPackWorkZoneLink.Active, "yes/no")).
         ZoneLinkBrowse:insertHiddenData("WorkZoneID","").
         ZoneLinkBrowse:insertHiddenData("PickPackStationID",chrPickPackStationID).
         ZoneLinkBrowse:insertHiddenData("PickPackWorkZoneLinkID","").
         
         
         ZoneLinkBrowse:endRow().
         
      END. /* FOR EACH PickPackWorkZoneLink */                  
   END. /* IF AVAILABLE WorkZone */
   
   ZoneLinkBrowse:endTable().
    
   /* Hidden Fields */ 
   ZoneLinkBrowseForm:insertHiddenField("form_name", "zonelink_browse_form").
   ZoneLinkBrowseForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   ZoneLinkBrowseForm:insertHiddenField("workzone_browse_scroll", "").   
   ZoneLinkBrowseForm:insertHiddenField("popup_zonelink_browse", "").  
   ZoneLinkBrowseForm:insertHiddenField("popup_addzonelink_browse", "").    
   ZoneLinkBrowseForm:insertHiddenField("ShipOrderStreamPickPackLinkID", chrShipOrderStreamPickPackLinkID).
   ZoneLinkBrowseForm:insertHiddenField("PickPackStationID", chrPickPackStationID).
   ZoneLinkBrowseForm:insertHiddenField("PickPackWorkZoneLinkID", "").
   ZoneLinkBrowseForm:insertHiddenField("PickPackWorkZoneLinkActive", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ZoneLinkBrowseForm}
   
   /* Create Button Bar */
   ZoneLinkBrowseButtons = NEW buttonBar().
   
   ZoneLinkBrowseButtons:addButton("zonelink_browse_form_btn_addlink",
                                   fTL("Add Zone Link"),
                                   "viewAddZoneLinkBrowse();").

   ZoneLinkBrowseButtons:addButton("zonelink_browse_form_btn_activate",
                                   fTL("Activate"),
                                   "confirmZoneLinkUpdate('zonelink_browse_form');",
                                   "Disabled").

   ZoneLinkBrowseButtons:addButton("zonelink_browse_form_btn_close",
                                   fTL("Cancel"),
                                   "disablePopup('zonelink_browse_form_popup');").
                                                                                                         
   ZoneLinkBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   ZoneLinkBrowseForm:FormBrowse  = ZoneLinkBrowse.
   ZoneLinkBrowseForm:FormButtons = ZoneLinkBrowseButtons.
   ZoneLinkBrowseForm:endForm(). 
   
   ZoneLinkBrowseForm:displayForm().  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(EXCLUDE-pUploadLocationBrowse) = 0 &THEN

&ENDIF

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUploadLocationBrowse Procedure

PROCEDURE pUploadLocationBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   UploadLocationBrowseForm            = NEW dataForm("uploadlocation_browse_form").
   UploadLocationBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   UploadLocationBrowseForm:FormAction = "dbPickPackLocEaseOfAccessLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   UploadLocationBrowseForm:FormWidth  = 700.
   UploadLocationBrowseForm:FormHeight = 490.
   UploadLocationBrowseForm:FormTitle  = fTL("Confirm Location Upload").
   UploadLocationBrowseForm:FormType   = "xl_large".
   
   UploadLocationBrowse              = NEW browseTable("uploadlocation_browse").
   UploadLocationBrowse:BrowseWidth  = 680.
   UploadLocationBrowse:BrowseHeight = 450.

   /* design the columns of the browse */
   ASSIGN intDisplayedColumnsNo  = 0
          intCurrentColumnsWidth = 0.
                                                                                                            
   UploadLocationBrowse:insertColumn(fTL("Station"),               150, "INTEGER", "left",  FALSE).
   UploadLocationBrowse:insertColumn(fTL("Location"),              150, "INTEGER", "left",  FALSE).
   UploadLocationBrowse:insertColumn(fTL("Ease Of AccessRanking"), 150, "INTEGER", "left",  FALSE).
   UploadLocationBrowse:insertColumn(fTL("Active"),                 60, "LOGICAL", "left",  FALSE).
   
   
    UploadLocationBrowse:startBody().
    
   /*Body*/
   FOR EACH ttFileLine NO-LOCK:
      
      FIND FIRST Location OF PickPackLocEaseOfAccessLink NO-LOCK NO-ERROR. /*idx-LocationID*/  
      
      FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStationID*/
            WHERE PickPackLocEaseOfAccessLink.PickPackStationID = PickPackStation.PickPackStationID NO-ERROR.
      
      UploadLocationBrowse:startRow(ttFileLine.FileLineNo, "", "").
      
      ASSIGN intColumnsCount = 0
             intPickPackStationIDCol   = 0 NO-ERROR.   
      FOR EACH FileMasterField NO-LOCK
         WHERE FileMasterField.FileMasterID = intLocationUploadMasterID
         BREAK BY FileMasterField.PositionInFile:

         /* Add data into each column for the current row */
         chrColumnValue = ENTRY(FileMasterField.PositionInFile,ttFileLine.LineString, ",").
         UploadLocationBrowse:insertData(chrColumnValue, "left").
         
         intColumnsCount = intColumnsCount + 1.
         
         IF intColumnsCount = intDisplayedColumnsNo THEN LEAVE.
      END.

      FIND FIRST PickPackStation NO-LOCK 
         WHERE PickPackStation.StationName = ENTRY(intPickPackStationIDCol,ttFileLine.LineString,",") NO-ERROR.
      
      /* Add hidden fields */
      UploadLocationBrowse:insertHiddenData("FileLineNo",ttFileLine.FileLineNo).
      
      UploadLocationBrowse:endRow().
      
   END. /* FOR EACH ttFileLine NO-LOCK */
   
   UploadLocationBrowse:endTable().
   
   UploadLocationBrowseForm:insertHiddenField("uploadlocation_browse_scroll","").
   UploadLocationBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLinkID",chrPickPackLocEaseOfAccessLinkID).
   UploadLocationBrowseForm:insertHiddenField("uploadlocationID","").
   UploadLocationBrowseForm:insertHiddenField("uploadlocationVersionID","").
   UploadLocationBrowseForm:insertHiddenField("UploadedFileName",chrFileName).
   UploadLocationBrowseForm:insertHiddenField("popup_uploadlocation_browse_form","").
   UploadLocationBrowseForm:insertHiddenField("popup_pickpackloceaseofaccesslink_selection_form","").
   UploadLocationBrowseForm:insertHiddenField("form_name", "uploadlocation_browse_form").
   UploadLocationBrowseForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i UploadLocationBrowseForm}
   
   /* Create Button Bar */
   UploadLocationBrowseButtons = NEW buttonBar().
   
   UploadLocationBrowseButtons:addButton("uploadlocation_browse_form_btn_save",
                                     fTL("Confirm"),
                                     "saveUploadLocation('uploadlocation_browse_form');",
                                     (IF chrValidFile = "YES" THEN "" ELSE "DISABLED")).
   
   UploadLocationBrowseButtons:addButton("uploadlocation_browse_form_btn_cancel",
                                     fTL("Cancel"),
                                     "cancelUpdate('UserCancelled','process_mode'); 
                                     disablePopup('uploadlocation_browse_form_popup');").
   
   UploadLocationBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   UploadLocationBrowseForm:FormBrowse  = UploadLocationBrowse.
   UploadLocationBrowseForm:FormButtons = UploadLocationBrowseButtons.
   UploadLocationBrowseForm:endForm().
   UploadLocationBrowseForm:displayForm().

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(EXCLUDE-pWorkZoneLocationDetails) = 0 &THEN

&ENDIF

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWorkZoneLocationDetails Procedure

PROCEDURE pWorkZoneLocationDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "pickpackloceaseofaccesslink_details_form"}
   ASSIGN chrDisplayFieldList  = "PickPackLocEaseOfAccessLinkID,PickPackStationID,LocationID,EaseOfAccessRanking,Active"
          chrEditFieldList     = "PickPackStationID,Active,LocationID,EaseOfAccessRanking"
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   
   PickPackLocEaseOfAccessLinkDetailsForm = NEW dataForm("pickpackloceaseofaccesslink_details_form").
   PickPackLocEaseOfAccessLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PickPackLocEaseOfAccessLinkDetailsForm:FormAction = "dbPickPackLocEaseOfAccessLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PickPackLocEaseOfAccessLinkDetailsForm:FormWidth   = 460.
   PickPackLocEaseOfAccessLinkDetailsForm:FormHeight  = 300.
   PickPackLocEaseOfAccessLinkDetailsForm:FormTitle   = "Location Details".
   PickPackLocEaseOfAccessLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PickPackLocEaseOfAccessLinkDetailsForm:insertPaddingColumn(30).
   PickPackLocEaseOfAccessLinkDetailsForm:insertColumn(185).
   
   /* Fields */
   PickPackLocEaseOfAccessLinkDetailsForm:startRow().
   PickPackLocEaseOfAccessLinkDetailsForm:insertLabel(fTL("ID")).
   PickPackLocEaseOfAccessLinkDetailsForm:insertTextField("PickPackLocEaseOfAccessLinkID", "", 190, TRUE).
   
   PickPackLocEaseOfAccessLinkDetailsForm:startRow().
   PickPackLocEaseOfAccessLinkDetailsForm:insertLabel(fTL("PickPackStation")).
   PickPackLocEaseOfAccessLinkDetailsForm:insertComboField("PickPackStationID", "", 190, TRUE).
   FOR EACH PickPackStation NO-LOCK  /*idx=WarehouseID*/                                                                                                      
      BY PickPackStation.PickPackStationID:                                                                                                  
      PickPackLocEaseOfAccessLinkDetailsForm:insertComboPairs("PickPackStationID", STRING(PickPackStation.PickPackStationID), PickPackStation.StationName).
   END. /*FOR EACH PickPackStation NO-LOCK*/
   
   PickPackLocEaseOfAccessLinkDetailsForm:startRow().
   PickPackLocEaseOfAccessLinkDetailsForm:insertLabel(fTL("Location")).
   PickPackLocEaseOfAccessLinkDetailsForm:insertComboField("LocationID", "", 190, TRUE).  
   FOR EACH Location NO-LOCK  /*idx=WarehouseID*/                                                                                                      
      BY Location.LocationID:                                                                                                  
      PickPackLocEaseOfAccessLinkDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef).
   END. /*FOR EACH PickPackStation NO-LOCK*/
   
   PickPackLocEaseOfAccessLinkDetailsForm:startRow().
   PickPackLocEaseOfAccessLinkDetailsForm:insertLabel(fTL("Ease Of Access Ranking")).
   PickPackLocEaseOfAccessLinkDetailsForm:insertTextField("EaseOfAccessRanking", "", 190, TRUE).  
   
   PickPackLocEaseOfAccessLinkDetailsForm:startRow().
   PickPackLocEaseOfAccessLinkDetailsForm:insertLabel(fTL("Active")).
   PickPackLocEaseOfAccessLinkDetailsForm:insertComboField("Active", "", 190, TRUE).
   PickPackLocEaseOfAccessLinkDetailsForm:insertComboPairs("Active", "yes", "Yes").    
   PickPackLocEaseOfAccessLinkDetailsForm:insertComboPairs("Active", "no", "No"). 


   {webGetOptionalFormFields.i pWorkZoneLocationDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("form_name", "pickpackloceaseofaccesslink_details_form").
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("popup_pickpackloceaseofaccesslink_browse", "").
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("pickpackloceaseofaccesslink_browse_scroll", "").
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("popup_workzone_browse", "").
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("pickpackstation_browse_scroll", "").
   
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("PickPackLocEaseOfAccessLinkID", chrPickPackLocEaseOfAccessLinkID).
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("PickPackLocEaseOfAccessLinkVersionID", "").
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("WorkZoneID", "").
   PickPackLocEaseOfAccessLinkDetailsForm:insertHiddenField("WorkZoneVersionID", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackLocEaseOfAccessLinkDetailsForm}
   
   /* Create Button Bar */
   PickPackLocEaseOfAccessLinkDetailsButtons = NEW buttonBar().
   
/*   PickPackLocEaseOfAccessLinkDetailsButtons:addButton("pickpackloceaseofaccesslink_details_form_btn_save",*/
/*                                    fTL("Save"),                                                           */
/*                                    "updateLocations('pickpackloceaseofaccesslink_details_form');").       */

   PickPackLocEaseOfAccessLinkDetailsButtons:addButton("pickpackloceaseofaccesslink_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode');" + 
                                    "disablePopup('pickpackloceaseofaccesslink_details_form_popup');").
   PickPackLocEaseOfAccessLinkDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickPackLocEaseOfAccessLinkDetailsForm:FormButtons = PickPackLocEaseOfAccessLinkDetailsButtons.
   
   PickPackLocEaseOfAccessLinkDetailsForm:endForm(). 
   
   PickPackLocEaseOfAccessLinkDetailsForm:displayForm().

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pWorkZoneLocationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWorkZoneLocationDetailsFields Procedure

PROCEDURE pWorkZoneLocationDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      PickPackLocEaseOfAccessLinkDetailsForm:startRow().
      PickPackLocEaseOfAccessLinkDetailsForm:insertLabel(fTL("Field Label")).
      PickPackLocEaseOfAccessLinkDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pWorkZoneLocations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWorkZoneLocations Procedure

PROCEDURE pWorkZoneLocations:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/* This finds the ProcessEvent which is linked to the form associated with this browse */
   PickPackLocEaseOfAccessLinkBrowseForm           = NEW dataForm("pickpackloceaseofaccesslink_browse_form").
   PickPackLocEaseOfAccessLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PickPackLocEaseOfAccessLinkBrowseForm:FormWidth  = 860.
   PickPackLocEaseOfAccessLinkBrowseForm:FormHeight = 530.
   PickPackLocEaseOfAccessLinkBrowseForm:FormTitle  = fTL("Work Zone Ease of Access").
   PickPackLocEaseOfAccessLinkBrowseForm:FormType   = "xxl_large".
   
   PickPackLocEaseOfAccessLinkBrowse                = NEW browseTable("pickpackloceaseofaccesslink_browse").
   PickPackLocEaseOfAccessLinkBrowse:BrowseWidth    = 840.
   PickPackLocEaseOfAccessLinkBrowse:BrowseHeight   = 490.
   
   PickPackLocEaseOfAccessLinkBrowse:insertColumn(fTL("ID"),                      60, "INTEGER", FALSE).
   PickPackLocEaseOfAccessLinkBrowse:insertColumn(fTL("Station"),                125, "INTEGER", "LEFT", FALSE).
   PickPackLocEaseOfAccessLinkBrowse:insertColumn(fTL("Location"),               125, "INTEGER", "LEFT", FALSE).
   PickPackLocEaseOfAccessLinkBrowse:insertColumn(fTL("Ease Of Access Ranking"), 130, "INTEGER", "LEFT", FALSE).
   PickPackLocEaseOfAccessLinkBrowse:insertColumn(fTL("Active"),                  60, "LOGICAL", "LEFT", FALSE). 
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickPackLocEaseOfAccessLink}
   
   PickPackLocEaseOfAccessLinkBrowse:StartBody().

   PickPackLocEaseOfAccessLinkBlk:
   FOR EACH PickPackLocEaseOfAccessLink NO-LOCK
      WHERE PickPackLocEaseOfAccessLink.PickPackStationID = intSelectedPickPackStation
      BY    PickPackLocEaseOfAccessLink.PickPackStationID
      BY    PickPackLocEaseOfAccessLink.EaseOfAccessRanking: /*idx=PickPackLocEaseOfAccessLinkID*/   

      IF PickPackLocEaseOfAccessLink.Active = FALSE THEN 
         NEXT PickPackLocEaseOfAccessLinkBlk. 

      FIND FIRST Location OF PickPackLocEaseOfAccessLink NO-LOCK NO-ERROR. /*idx-LocationID*/  
      
      FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStationID*/
            WHERE PickPackLocEaseOfAccessLink.PickPackStationID = PickPackStation.PickPackStationID NO-ERROR.                                                                                         
          
      PickPackLocEaseOfAccessLinkBrowse:startRow(PickPackLocEaseOfAccessLink.PickPackLocEaseOfAccessLinkID,
          "selectPickPackLocEaseOfAccessLinkRow(this," + '"' + STRING(PickPackLocEaseOfAccessLink.PickPackLocEaseOfAccessLinkID)
           + '"' + ");", "").    
          
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PickPackLocEaseOfAccessLink}
      
      PickPackLocEaseOfAccessLinkBrowse:insertData(PickPackLocEaseOfAccessLink.PickPackLocEaseOfAccessLinkID,                   "").
      PickPackLocEaseOfAccessLinkBrowse:insertData((IF AVAILABLE PickPackStation THEN PickPackStation.StationName ELSE ""), "LEFT").
      PickPackLocEaseOfAccessLinkBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""),               "LEFT").
      PickPackLocEaseOfAccessLinkBrowse:insertData(PickPackLocEaseOfAccessLink.EaseOfAccessRanking,                         "LEFT").
      PickPackLocEaseOfAccessLinkBrowse:insertData(STRING(PickPackLocEaseOfAccessLink.Active, "Yes/No"),                    "LEFT").
      
      PickPackLocEaseOfAccessLinkBrowse:endRow().
   END. /* FOR EACH PickPackLocEaseOfAccessLink */
   
   PickPackLocEaseOfAccessLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PickPackLocEaseOfAccessLinkBrowse:getErrors().
   
   /*Hidden Fields*/
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("form_name", "pickpackloceaseofaccesslink_browse_form").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("popup_pickpackloceaseofaccesslink_browse", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("pickpackloceaseofaccesslink_browse_scroll", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("popup_pickpackloceaseofaccesslnkhst_browse", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("pickpackloceaseofaccesslnkhst_browse_scroll", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("popup_workzone_browse", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("pickpackstation_browse_scroll", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("popup_uploadlocation_browse_form","").
   
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLinkID", chrPickPackLocEaseOfAccessLinkID).
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLinkVersionID", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLnkHstID", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLnkHstVersionID", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("PickPackStationID", chrPickPackStationID).
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("PickPackStationVersionID", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("WorkZoneID", "").
   PickPackLocEaseOfAccessLinkBrowseForm:insertHiddenField("WorkZoneVersionID", "").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackLocEaseOfAccessLinkBrowseForm}
   
   /* Create Button Bar */
   PickPackLocEaseOfAccessLinkButtons = NEW buttonBar().
   
   PickPackLocEaseOfAccessLinkButtons:addButton("pickpackloceaseofaccesslink_browse_form_btn_details",
                                        fTL("Details"),
                                        "viewWorkZoneLocationDetails('pickpackloceaseofaccesslink_details_form');",
                                        (IF intSelectedPickPackLocEaseOfAccessLink > 0 THEN "" ELSE "Disabled")).
                                        
/*   PickPackLocEaseOfAccessLinkButtons:addButton("pickpackloceaseofaccesslink_browse_form_btn_history",               */
/*                                        fTL("History"),                                                              */
/*                                        "viewWorkZoneLocationsHistory('pickpackloceaseofaccesslnkhst_browse_form');",*/
/*                                        (IF intSelectedPickPackLocEaseOfAccessLink > 0 THEN "" ELSE "Disabled")).    */
                                         
   PickPackLocEaseOfAccessLinkButtons:addButton("pickpackloceaseofaccesslink_browse_form_btn_cancel",
                                        fTL("Cancel"),
                                        "disablePopup('pickpackloceaseofaccesslink_browse_form_popup');").
                                        
    PickPackLocEaseOfAccessLinkButtons:addButton("pickpackloceaseofaccesslink_browse_form_btn_locationupload",
                                     fTL("Upload"),
                                     "viewWorkZoneLocationUpload('pickpackloceaseofaccesslink_selection_form');").
                                                                         
   PickPackLocEaseOfAccessLinkButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickPackLocEaseOfAccessLinkBrowseForm:FormBrowse  = PickPackLocEaseOfAccessLinkBrowse.
   PickPackLocEaseOfAccessLinkBrowseForm:FormButtons = PickPackLocEaseOfAccessLinkButtons.
   PickPackLocEaseOfAccessLinkBrowseForm:endForm(). 
   
   PickPackLocEaseOfAccessLinkBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pWorkZoneLocationsHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWorkZoneLocationsHistory Procedure

PROCEDURE pWorkZoneLocationsHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/* This finds the ProcessEvent which is linked to the form associated with this browse */
   PickPackLocEaseOfAccessLnkHstBrowseForm           = NEW dataForm("pickpackloceaseofaccesslnkhst_browse_form").
   PickPackLocEaseOfAccessLnkHstBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PickPackLocEaseOfAccessLnkHstBrowseForm:FormWidth  = 860.
   PickPackLocEaseOfAccessLnkHstBrowseForm:FormHeight = 530.
   PickPackLocEaseOfAccessLnkHstBrowseForm:FormTitle  = fTL("Location History").
   PickPackLocEaseOfAccessLnkHstBrowseForm:FormType   = "xxl_large".
   
   PickPackLocEaseOfAccessLnkHstBrowse                = NEW browseTable("pickpackloceaseofaccesslnkhst_browse").
   PickPackLocEaseOfAccessLnkHstBrowse:BrowseWidth    = 840.
   PickPackLocEaseOfAccessLnkHstBrowse:BrowseHeight   = 490.
   
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("History ID"),              70, "INTEGER", FALSE).
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("ID"),                      90, "INTEGER", FALSE).
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("Station"),                100, "INTEGER", FALSE).
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("Location"),               100, "INTEGER", "LEFT", FALSE).
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("Ease Of Access Ranking"), 150, "INTEGER", "LEFT", FALSE).
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("Active"),                  60, "LOGICAL", "LEFT", FALSE). 
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("User"),                   110, "CHARACTER", "LEFT", FALSE).
   PickPackLocEaseOfAccessLnkHstBrowse:insertColumn(fTL("Created"),                140, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PickPackLocEaseOfAccessLnkHst}
   
   PickPackLocEaseOfAccessLnkHstBrowse:StartBody().
   
   FOR EACH PickPackLocEaseOfAccessLnkHst NO-LOCK /*idx=WorkZoneID and WorkZoneHistoryID*/
      WHERE PickPackLocEaseOfAccessLnkHst.PickPackLocEaseOfAccessLinkID = intSelectedPickPackLocEaseOfAccessLink
         BY PickPackLocEaseOfAccessLnkHst.PickPackLocEaseOfAccessLnkHstID:
            
      FIND FIRST Location OF PickPackLocEaseOfAccessLnkHst NO-LOCK NO-ERROR. /*idx-LocationID*/  
      
      FIND FIRST PickPackStation NO-LOCK /*idx=PickPackStationID*/
            WHERE PickPackLocEaseOfAccessLnkHst.PickPackStationID = PickPackStation.PickPackStationID NO-ERROR.  
          
/*      FIND FIRST Warehouse OF WorkZoneHistory NO-LOCK NO-ERROR. /*idx-WarehouseID*/        */
      FIND FIRST OperationType OF PickPackLocEaseOfAccessLnkHst NO-LOCK NO-ERROR. /*idx=OperationTypeID*/
      FIND FIRST GateUser      OF PickPackLocEaseOfAccessLnkHst NO-LOCK NO-ERROR. /*idx=GateUserID*/

      PickPackLocEaseOfAccessLnkHstBrowse:startRow (PickPackLocEaseOfAccessLnkHst.PickPackLocEaseOfAccessLnkHstID, 
         "selectPickPackLocEaseOfAccessLnkHstRow(this," + '"' + STRING(PickPackLocEaseOfAccessLnkHst.PickPackLocEaseOfAccessLnkHstID)
          + '"' + ");", "").    
         
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PickPackLocEaseOfAccessLnkHst}
      
      PickPackLocEaseOfAccessLnkHstBrowse:insertData(PickPackLocEaseOfAccessLnkHst.PickPackLocEaseOfAccessLnkHstID,               "").
      PickPackLocEaseOfAccessLnkHstBrowse:insertData(PickPackLocEaseOfAccessLnkHst.PickPackLocEaseOfAccessLinkID,                 "").
      PickPackLocEaseOfAccessLnkHstBrowse:insertData((IF AVAILABLE PickPackStation THEN PickPackStation.StationName ELSE ""), "LEFT").
      PickPackLocEaseOfAccessLnkHstBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""),               "LEFT").
      PickPackLocEaseOfAccessLnkHstBrowse:insertData(PickPackLocEaseOfAccessLnkHst.EaseOfAccessRanking,                       "LEFT").
      PickPackLocEaseOfAccessLnkHstBrowse:insertData(STRING(PickPackLocEaseOfAccessLnkHst.Active, "Yes/No"),                  "LEFT").
      PickPackLocEaseOfAccessLnkHstBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""),                  "LEFT").
      PickPackLocEaseOfAccessLnkHstBrowse:insertData(fDisplayDate&Time(PickPackLocEaseOfAccessLnkHst.Created,"y/m/d H:M:S"),  "LEFT").
      
      PickPackLocEaseOfAccessLnkHstBrowse:endRow().
   END. /* FOR EACH PickPackLocEaseOfAccessLnkHst */
   
   PickPackLocEaseOfAccessLnkHstBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PickPackLocEaseOfAccessLnkHstBrowse:getErrors().
   
   /*Hidden Fields*/
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("form_name", "pickpackloceaseofaccesslnkhst_browse_form").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("popup_pickpackloceaseofaccesslink_browse", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("pickpackloceaseofaccesslink_browse_scroll", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("popup_pickpackloceaseofaccesslnkhst_browse", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("pickpackloceaseofaccesslnkhst_browse_scroll", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("popup_workzone_browse", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("pickpackstation_browse_scroll", "").
   
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLinkID", chrPickPackLocEaseOfAccessLinkID).
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLinkVersionID", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLnkHstID", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("PickPackLocEaseOfAccessLnkHstVersionID", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("WorkZoneID", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("WorkZoneVersionID", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("PickPackStationID", chrPickPackStationID).
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("PickPackStationVersionID", "").
   PickPackLocEaseOfAccessLnkHstBrowseForm:insertHiddenField("process_mode", "").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PickPackLocEaseOfAccessLnkHstBrowseForm}
   
   /* Create Button Bar */
   PickPackLocEaseOfAccessLnkHstButtons = NEW buttonBar().
                                           
                                         
   PickPackLocEaseOfAccessLnkHstButtons:addButton("pickpackloceaseofaccesslnkhst_browse_form_btn_cancel",
                                        fTL("Cancel"),
                                        "cancelUpdate('UserCancelled','process_mode');" + 
                                        "disablePopup('pickpackloceaseofaccesslnkhst_browse_form_popup');").                   
   PickPackLocEaseOfAccessLnkHstButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PickPackLocEaseOfAccessLnkHstBrowseForm:FormBrowse  = PickPackLocEaseOfAccessLnkHstBrowse.
   PickPackLocEaseOfAccessLnkHstBrowseForm:FormButtons = PickPackLocEaseOfAccessLnkHstButtons.
   PickPackLocEaseOfAccessLnkHstBrowseForm:endForm(). 
   
   PickPackLocEaseOfAccessLnkHstBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pWorkZoneLocationUpload) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWorkZoneLocationUpload Procedure

PROCEDURE pWorkZoneLocationUpload:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "pickpackloceaseofaccesslinkfile_details_form"}

   FileSelectionForm            = NEW dataForm("pickpackloceaseofaccesslink_selection_form").
   FileSelectionForm:WebStream  = STREAM WebStream:HANDLE.
   FileSelectionForm:FormAction = "dbPickPackLocEaseOfAccessLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileSelectionForm:FormWidth  = 420.
   FileSelectionForm:FormHeight = 70.
   FileSelectionForm:FormTitle  = "Select Pick Pack Location Ease Of Access Link File".
   FileSelectionForm:FormType   = "xxsmall_wide".
   
   /* Column Layout */
   FileSelectionForm:insertPaddingColumn(15).
   
   /* Fields */
   {webGetOptionalFormFields.i pLocationUploadFileDetails}
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel("").
   FileSelectionForm:insertFileInput("UploadFile",380,TRUE).
   
   /* Add Hidden Fields*/
   FileSelectionForm:insertHiddenField("pickpackloceaseofaccesslink_browse_scroll", "").
   FileSelectionForm:insertHiddenField("process_mode","").
   FileSelectionForm:insertHiddenField("popup_pickpackloceaseofaccesslink_selection_form","").
   FileSelectionForm:insertHiddenField("popup_uploadlocation_browse_form","").
   FileSelectionForm:insertHiddenField("pickpackloceaseofaccesslinkID","").
   FileSelectionForm:insertHiddenField("form_name", "pickpackloceaseofaccesslink_selection_form").
   FileSelectionForm:insertHiddenField("prog_name", "adPickPackStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileSelectionForm}
   
   /* Create Button Bar */
   FileDetailsButtons = NEW buttonBar().
   FileDetailsButtons:addButton("pickpackloceaseofaccesslink_selection_form_btn_save", 
                                fTL("Upload"), 
                                "viewUploadLocationBrowse('pickpackloceaseofaccesslink_selection_form');").
   
   FileDetailsButtons:addButton("pickpackloceaseofaccesslink_selection_form_btn_cancel", 
                                fTL("Cancel"), 
                                "cancelUpdate('UserCancelled','process_mode'); 
                                disablePopup('pickpackloceaseofaccesslink_selection_form_popup');").
   
   FileDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   FileSelectionForm:FormButtons = FileDetailsButtons.
   FileSelectionForm:endForm(). 
   FileSelectionForm:displayForm().

END PROCEDURE.


	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


