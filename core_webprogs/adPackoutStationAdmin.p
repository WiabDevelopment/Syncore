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
     
  Revisions:
     17/12/2014 - twierzch - add support for PostPickPackoutLink and ShipOrderStreamPackoutLink tables

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
DEFINE VARIABLE intSelectedPackoutStation             AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedPostpickPackoutLink        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPackoutStationRow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPackoutStationRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPackoutStationID                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectedPostpickPackoutLink        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPostPicks                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupAddPostPicks                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupShipOrderStream               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupAddShipOrderStream            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intLocationTypePostpickID             AS INTEGER     NO-UNDO.
DEFINE VARIABLE intLocationCnt                        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrLocations                          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intStreamCnt                          AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrStreams                            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPostpickPackoutLinkRow             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrShipOrderStreamPackoutLinkRow      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedShipOrderStreamPackoutLink AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectedShipOrderStreamPackoutLink AS CHARACTER   NO-UNDO.

/* Buffers */
DEFINE BUFFER onHoldLocationType   FOR LocationType.
DEFINE BUFFER postPackLocationType FOR LocationType.
DEFINE BUFFER onHoldLocation       FOR Location.
DEFINE BUFFER postPackLocation     FOR Location.

/* Objects */
DEFINE VARIABLE PackoutStationBrowseFrame                   AS pageFrame.
DEFINE VARIABLE PackoutStationBrowse                        AS browseTable.
DEFINE VARIABLE PackoutStationBrowseButtons                 AS buttonBar.
DEFINE VARIABLE PackoutStationDetailsForm                   AS dataForm.
DEFINE VARIABLE PackoutStationDetailsButtons                AS buttonBar.
DEFINE VARIABLE PackoutStationHistoryBrowseForm             AS dataform. 
DEFINE VARIABLE PackoutStationHistoryBrowse                 AS browseTable.
DEFINE VARIABLE PackoutStationHistoryBrowseButtons          AS buttonBar.
DEFINE VARIABLE PostpickPackoutLinkBrowseForm               AS dataform. 
DEFINE VARIABLE PostpickPackoutLinkBrowse                   AS browseTable.
DEFINE VARIABLE PostpickPackoutLinkBrowseButtons            AS buttonBar.
DEFINE VARIABLE AddPostpickPackoutLinksBrowseForm           AS dataform. 
DEFINE VARIABLE AddPostpickPackoutLinksBrowse               AS browseTable.
DEFINE VARIABLE AddPostpickPackoutLinksBrowseButtons        AS buttonBar.
DEFINE VARIABLE ShipOrderStreamPackoutLinkBrowseForm        AS dataform. 
DEFINE VARIABLE ShipOrderStreamPackoutLinkBrowse            AS browseTable.
DEFINE VARIABLE ShipOrderStreamPackoutLinkBrowseButtons     AS buttonBar.
DEFINE VARIABLE AddShipOrderStreamPackoutLinkBrowseForm     AS dataform. 
DEFINE VARIABLE AddShipOrderStreamPackoutLinkBrowse         AS browseTable.
DEFINE VARIABLE AddShipOrderStreamPackoutLinkBrowseButtons  AS buttonBar.


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

&IF DEFINED(EXCLUDE-pAddPostpickPackoutLinks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddPostpickPackoutLinks Procedure
PROCEDURE pAddPostpickPackoutLinks:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   AddPostpickPackoutLinksBrowseForm            = NEW dataForm("add_postpick_packout_links_browse_form").
   AddPostpickPackoutLinksBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   AddPostpickPackoutLinksBrowseForm:FormAction = "dbCreatePostpickPackoutLinks.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   AddPostpickPackoutLinksBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   AddPostpickPackoutLinksBrowseForm:FormWidth  = 700.
   AddPostpickPackoutLinksBrowseForm:FormHeight = 490.
   AddPostpickPackoutLinksBrowseForm:FormTitle  = fTL("Available PostPicks to Link to Packout Station") + 
                                                        (IF AVAILABLE PackoutStation THEN " : " + PackoutStation.StationName ELSE "").
   AddPostpickPackoutLinksBrowseForm:FormType   = "xl_large".
   
   /* Form Data */
   AddPostpickPackoutLinksBrowseForm:insertPaddingColumn(180).
   AddPostpickPackoutLinksBrowseForm:insertColumn(110).
   AddPostpickPackoutLinksBrowseForm:insertColumn(130).
   AddPostpickPackoutLinksBrowseForm:insertColumn(40).
   AddPostpickPackoutLinksBrowseForm:insertColumn(85).
   AddPostpickPackoutLinksBrowseForm:insertColumn(130).
   
   AddPostpickPackoutLinksBrowseForm:startRow().
   AddPostpickPackoutLinksBrowseForm:insertBlankRow(10). 

   AddPostpickPackoutLinksBrowse              = NEW browseTable("add_postpick_packout_links_browse").
   AddPostpickPackoutLinksBrowse:BrowseWidth  = 680.
   AddPostpickPackoutLinksBrowse:BrowseHeight = 420.
   
   AddPostpickPackoutLinksBrowse:insertColumn(fTL("Location ID"),       100, "INTEGER", FALSE).
   AddPostpickPackoutLinksBrowse:insertColumn(fTL("Postpick Location"), 120, "CHARACTER", "left", FALSE).
   AddPostpickPackoutLinksBrowse:insertColumn(fTL("Active"),            100, "CHARACTER", FALSE).
   
   AddPostpickPackoutLinksBrowse:StartBody().
   AddPostpickPackoutLinkLoop:
   FOR EACH Location NO-LOCK
      WHERE Location.LocationTypeID = intLocationTypePostpickID:
      IF CAN-FIND(PostpickPackoutLink WHERE PostpickPackoutLink.PackoutStationID = PackoutStation.PackoutStationID
                                        AND PostpickPackoutLink.LocationID = Location.LocationID) THEN 
      NEXT AddPostpickPackoutLinkLoop.
      
      AddPostpickPackoutLinksBrowse:startRow(Location.LocationID,
                                     'multiSelectLocationRow("' +
                                     STRING(Location.LocationID) + '")', "").
   
      AddPostpickPackoutLinksBrowse:insertData(STRING(Location.LocationID), "left").
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   
      AddPostpickPackoutLinksBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "left").
      AddPostpickPackoutLinksBrowse:insertData(STRING(Location.Active,"Yes/No")).
   
      /* Add hidden fields */
      AddPostpickPackoutLinksBrowse:insertHiddenData("LocationID",Location.LocationID).
      AddPostpickPackoutLinksBrowse:insertHiddenData("LocationVersionID",Location.VersionID).

      /* Count the Locations */
      intLocationCnt = intLocationCnt + 1.
      chrLocations = chrLocations + STRING(Location.LocationID) + ",".
   
      AddPostpickPackoutLinksBrowse:endRow().
      
   END.
  
   chrLocations  = TRIM(chrLocations, ",").
   
   AddPostpickPackoutLinksBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + AddPostpickPackoutLinksBrowse:getErrors().
   
   AddPostpickPackoutLinksBrowseForm:insertHiddenField("LocationList","").
   AddPostpickPackoutLinksBrowseForm:insertHiddenField("add_postpick_packout_links_browse_scroll","").
   AddPostpickPackoutLinksBrowseForm:insertHiddenField("popup_postpick_packout_links_browse","yes").
   AddPostpickPackoutLinksBrowseForm:insertHiddenField("popup_add_postpick_packout_links_browse","yes").
   AddPostpickPackoutLinksBrowseForm:insertHiddenField("form_name","add_postpick_packout_links_browse_form").
   AddPostpickPackoutLinksBrowseForm:insertHiddenField("prog_name","adPackoutStationAdmin.p").
   AddPostpickPackoutLinksBrowseForm:insertHiddenField("PackoutStationID",chrPackoutStationID).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddPostpickPackoutLinksBrowseForm}
   
   /* Create Button Bar */
   AddPostpickPackoutLinksBrowseButtons = NEW buttonBar().
   
   AddPostpickPackoutLinksBrowseButtons:addButton("postpick_packout_links_browse_form_btn_selectall",
                                                   fTL("Select All"),
                                                   "selectAllLocations('" + STRING(chrLocations) + "','" + STRING(intLocationCnt) + "')").

   AddPostpickPackoutLinksBrowseButtons:addButton("postpick_packout_links_browse_form_btn_add",
                                                   fTL("Confirm Links"),
                                                   "createPostpickPackoutLinks();"). 
   
   AddPostpickPackoutLinksBrowseButtons:addButton("postpick_packout_links_browse_form_btn_clear",
                                                   fTL("Clear Selection"),
                                                   "deselectAllLocationRows()").

   AddPostpickPackoutLinksBrowseButtons:addButton("postpick_packout_links_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('add_postpick_packout_links_browse_form_popup');").
   
   AddPostpickPackoutLinksBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddPostpickPackoutLinksBrowseForm:FormBrowse  = AddPostpickPackoutLinksBrowse.
   AddPostpickPackoutLinksBrowseForm:FormButtons = AddPostpickPackoutLinksBrowseButtons.
   AddPostpickPackoutLinksBrowseForm:endForm(). 
   
   AddPostpickPackoutLinksBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pAddShipOrderStreamPackoutLinks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddShipOrderStreamPackoutLinks Procedure
PROCEDURE pAddShipOrderStreamPackoutLinks:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   AddShipOrderStreamPackoutLinkBrowseForm            = NEW dataForm("add_shiporderstream_packout_links_browse_form").
   AddShipOrderStreamPackoutLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   AddShipOrderStreamPackoutLinkBrowseForm:FormAction = "dbCreateShipOrderStreamPackoutLinks.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   AddShipOrderStreamPackoutLinkBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   AddShipOrderStreamPackoutLinkBrowseForm:FormWidth  = 700.
   AddShipOrderStreamPackoutLinkBrowseForm:FormHeight = 490.
   AddShipOrderStreamPackoutLinkBrowseForm:FormTitle  = fTL("Available ShipOrder Streams to Link to Packout Station") + 
                                                        (IF AVAILABLE PackoutStation THEN " : " + PackoutStation.StationName ELSE "").
   AddShipOrderStreamPackoutLinkBrowseForm:FormType   = "xl_large".
   
   /* Form Data */
   AddShipOrderStreamPackoutLinkBrowseForm:insertPaddingColumn(180).
   AddShipOrderStreamPackoutLinkBrowseForm:insertColumn(110).
   AddShipOrderStreamPackoutLinkBrowseForm:insertColumn(130).
   AddShipOrderStreamPackoutLinkBrowseForm:insertColumn(40).
   AddShipOrderStreamPackoutLinkBrowseForm:insertColumn(85).
   AddShipOrderStreamPackoutLinkBrowseForm:insertColumn(130).
   
   AddShipOrderStreamPackoutLinkBrowseForm:startRow().
   AddShipOrderStreamPackoutLinkBrowseForm:insertBlankRow(10). 

   AddShipOrderStreamPackoutLinkBrowse              = NEW browseTable("add_shiporderstream_packout_links_browse").
   AddShipOrderStreamPackoutLinkBrowse:BrowseWidth  = 680.
   AddShipOrderStreamPackoutLinkBrowse:BrowseHeight = 420.
   
   AddShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Stream ID"),    100, "INTEGER", FALSE).
   AddShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Stream Name"),  140, "CHARACTER", "left", FALSE).
   AddShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Stream Descr"), 150, "CHARACTER", "left", FALSE).
   AddShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Active"),       100, "CHARACTER", "left", FALSE).
   
   AddShipOrderStreamPackoutLinkBrowse:StartBody().
   AddShipOrderStreamPackoutLinkLoop:
   FOR EACH ShipOrderStream NO-LOCK:
      IF CAN-FIND(ShipOrderStreamPackoutLink 
            WHERE ShipOrderStreamPackoutLink.PackoutStationID  = PackoutStation.PackoutStationID
            AND   ShipOrderStreamPackoutLink.ShipOrderStreamID = ShipOrderStream.ShipOrderStreamID) THEN
         NEXT AddShipOrderStreamPackoutLinkLoop.

      AddShipOrderStreamPackoutLinkBrowse:startRow(ShipOrderStream.ShipOrderStreamID,
                                                   'multiSelectShipOrderStreamRow("' +
                                                   STRING(ShipOrderStream.ShipOrderStreamID) + '")', "").

      AddShipOrderStreamPackoutLinkBrowse:insertData(STRING(ShipOrderStream.ShipOrderStreamID), "left").
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */

      AddShipOrderStreamPackoutLinkBrowse:insertData(ShipOrderStream.StreamName, "left").
      AddShipOrderStreamPackoutLinkBrowse:insertData(ShipOrderStream.StreamDescr, "left").
      
      AddShipOrderStreamPackoutLinkBrowse:insertData(STRING(ShipOrderStream.Active,"Yes/No")).

      /* Add hidden fields */
      AddShipOrderStreamPackoutLinkBrowse:insertHiddenData("ShipOrderStreamID",ShipOrderStream.ShipOrderStreamID).
      AddShipOrderStreamPackoutLinkBrowse:insertHiddenData("ShipOrderStreamVersionID",ShipOrderStream.VersionID).

      /* Count the Locations */
      intStreamCnt = intStreamCnt + 1.
      chrStreams = chrStreams + STRING(ShipOrderStream.ShipOrderStreamID) + ",".

      AddShipOrderStreamPackoutLinkBrowse:endRow().

   END.
  
   chrStreams  = TRIM(chrStreams, ",").
   
   AddShipOrderStreamPackoutLinkBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + AddShipOrderStreamPackoutLinkBrowse:getErrors().
   
   AddShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("StreamList","").
   AddShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("add_shiporderstream_packout_links_browse_scroll","").
   AddShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("popup_shiporderstream_packout_links_browse","yes").
   AddShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("popup_add_shiporderstream_packout_links_browse","yes").
   AddShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("form_name","add_shiporderstream_packout_links_browse_form").
   AddShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("prog_name","adPackoutStationAdmin.p").
   AddShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("PackoutStationID",chrPackoutStationID).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddShipOrderStreamPackoutLinkBrowseForm}
   
   /* Create Button Bar */
   AddShipOrderStreamPackoutLinkBrowseButtons = NEW buttonBar().
   
   AddShipOrderStreamPackoutLinkBrowseButtons:addButton("shiporderstream_packout_links_browse_form_btn_selectall",
                                                    fTL("Select All"),
                                                    "selectAllStreams('" + STRING(chrStreams) + "','" + STRING(intStreamCnt) + "')").

   AddShipOrderStreamPackoutLinkBrowseButtons:addButton("shiporderstream_packout_links_browse_form_btn_add",
                                                    fTL("Confirm Links"),
                                                    "createShipOrderStreamPackoutLinks();"). 
   
   AddShipOrderStreamPackoutLinkBrowseButtons:addButton("shiporderstream_packout_links_browse_form_btn_clear",
                                                    fTL("Clear Selection"),
                                                    "deselectAllStreamRows()").

   AddShipOrderStreamPackoutLinkBrowseButtons:addButton("shiporderstream_packout_links_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('add_shiporderstream_packout_links_browse_form_popup');").
   
   AddShipOrderStreamPackoutLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddShipOrderStreamPackoutLinkBrowseForm:FormBrowse  = AddShipOrderStreamPackoutLinkBrowse.
   AddShipOrderStreamPackoutLinkBrowseForm:FormButtons = AddShipOrderStreamPackoutLinkBrowseButtons.
   AddShipOrderStreamPackoutLinkBrowseForm:endForm(). 
   
   AddShipOrderStreamPackoutLinkBrowseForm:displayForm().

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

&IF DEFINED(EXCLUDE-pPostpickPackoutLinks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPostpickPackoutLinks Procedure
PROCEDURE pPostpickPackoutLinks:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   PostpickPackoutLinkBrowseForm            = NEW dataForm("postpick_packout_links_browse_form").
   PostpickPackoutLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   PostpickPackoutLinkBrowseForm:FormAction = "dbSetPostpickPackoutLinkActive.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   PostpickPackoutLinkBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   PostpickPackoutLinkBrowseForm:FormWidth  = 860.
   PostpickPackoutLinkBrowseForm:FormHeight = 530.
   PostpickPackoutLinkBrowseForm:FormTitle  = fTL("Current PostPicks Linked to Packout Station") + 
                                                        (IF AVAILABLE PackoutStation THEN " : " + PackoutStation.StationName ELSE "").
   PostpickPackoutLinkBrowseForm:FormType   = "xxl_large".
   
   /* Form Data */
   PostpickPackoutLinkBrowseForm:insertPaddingColumn(180).
   PostpickPackoutLinkBrowseForm:insertColumn(110).
   PostpickPackoutLinkBrowseForm:insertColumn(130).
   PostpickPackoutLinkBrowseForm:insertColumn(40).
   PostpickPackoutLinkBrowseForm:insertColumn(85).
   PostpickPackoutLinkBrowseForm:insertColumn(130).
   
   PostpickPackoutLinkBrowseForm:startRow().
   PostpickPackoutLinkBrowseForm:insertBlankRow(10). 

   PostpickPackoutLinkBrowse              = NEW browseTable("postpick_packout_links_browse").
   PostpickPackoutLinkBrowse:BrowseWidth  = 840.
   PostpickPackoutLinkBrowse:BrowseHeight = 460.
   
   PostpickPackoutLinkBrowse:insertColumn(fTL("Link ID"),           100, "INTEGER", FALSE).
   PostpickPackoutLinkBrowse:insertColumn(fTL("Postpick Location"), 120, "CHARACTER", "left", FALSE).
   PostpickPackoutLinkBrowse:insertColumn(fTL("Active"),            100, "CHARACTER", FALSE).
   
   PostpickPackoutLinkBrowse:StartBody().
   PostpickPackoutLinkLoop:
   FOR EACH PostpickPackoutLink NO-LOCK
      WHERE PostpickPackoutLink.PackoutStationID = PackoutStation.PackoutStationID,
      EACH  Location OF PostpickPackoutLink NO-LOCK
      BY    PostpickPackoutLink.Active DESCENDING 
      BY    Location.LocationRef:
         
      PostpickPackoutLinkBrowse:startRow(PostpickPackoutLink.PostpickPackoutLinkID,
                                  "selectPostpickPackoutLinkRow(this," + '"' + 
                                  STRING(PostpickPackoutLink.PostpickPackoutLinkID) + '");', "").

      PostpickPackoutLinkBrowse:insertData(STRING(PostpickPackoutLink.PostpickPackoutLinkID), "left").
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PostpickPackoutLink}
   
      PostpickPackoutLinkBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "left").
      PostpickPackoutLinkBrowse:insertData(STRING(PostpickPackoutLink.Active,"Yes/No")).
   
      /* Add hidden fields */
      PostpickPackoutLinkBrowse:insertHiddenData("LocationID",Location.LocationID).
      PostpickPackoutLinkBrowse:insertHiddenData("LocationVersionID",Location.VersionID).
      PostpickPackoutLinkBrowse:insertHiddenData("PostpickPackoutLinkVersionID",PostpickPackoutLink.VersionID).
      PostpickPackoutLinkBrowse:insertHiddenData("LinkActive",STRING(PostpickPackoutLink.Active)).

      PostpickPackoutLinkBrowse:endRow().
   END. /* FOR EACH PostpickPackoutLink WHERE PostpickPackoutLink.PackoutStationID = PackoutStation.PackoutStationID ...  */
   
   PostpickPackoutLinkBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PostpickPackoutLinkBrowse:getErrors().
   
   PostpickPackoutLinkBrowseForm:insertHiddenField("PackoutStationID", IF AVAILABLE PackoutStation THEN STRING(PackoutStation.PackoutStationID) ELSE "").
   PostpickPackoutLinkBrowseForm:insertHiddenField("PostpickPackoutLinkID", "").
   PostpickPackoutLinkBrowseForm:insertHiddenField("PostpickPackoutLinkVersionID", "").
   PostpickPackoutLinkBrowseForm:insertHiddenField("PostpickPackoutLinkActive", "").
   PostpickPackoutLinkBrowseForm:insertHiddenField("packoutstation_browse_scroll", "").
   PostpickPackoutLinkBrowseForm:insertHiddenField("postpick_packout_link_browse_scroll", "").
   PostpickPackoutLinkBrowseForm:insertHiddenField("popup_postpick_packout_links_browse","yes").
   PostpickPackoutLinkBrowseForm:insertHiddenField("popup_add_postpick_packout_links_browse","").
   PostpickPackoutLinkBrowseForm:insertHiddenField("form_name","postpick_packout_links_browse_form").
   PostpickPackoutLinkBrowseForm:insertHiddenField("prog_name","adPackoutStationAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PostpickPackoutLinkBrowseForm}
   
   /* Create Button Bar */
   PostpickPackoutLinkBrowseButtons = NEW buttonBar().
   
   PostpickPackoutLinkBrowseButtons:addButton("postpick_packout_links_browse_form_btn_add",
                                              fTL("Add Links"),
                                              "addPackoutPostPickLinks();"). 
   
   PostpickPackoutLinkBrowseButtons:addButton("postpick_packout_links_browse_form_btn_deactivatelink",
                                               fTL("Deactivate Link"),
                                               "setPostpickPackoutLinkActiveFlag('postpick_packout_links_browse_form');",
                                               (IF intSelectedPostpickPackoutLink > 0 THEN "" ELSE "Disabled")).
   
   PostpickPackoutLinkBrowseButtons:addButton("postpick_packout_links_browse_form_btn_cancel",
                                               fTL("Cancel"),
                                               "disablePopup('postpick_packout_links_browse_form_popup');").
   
   PostpickPackoutLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PostpickPackoutLinkBrowseForm:FormBrowse  = PostpickPackoutLinkBrowse.
   PostpickPackoutLinkBrowseForm:FormButtons = PostpickPackoutLinkBrowseButtons.
   PostpickPackoutLinkBrowseForm:endForm(). 
   
   PostpickPackoutLinkBrowseForm:displayForm().

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
   
   ASSIGN chrPackoutStationID                   = get-value("PackoutStationID")
          intSelectedPackoutStation             = INTEGER(chrPackoutStationID)
          chrScrollToPackoutStationRow          = STRING(INTEGER(get-value("packoutstation_browse_scroll"))) + ";"
          chrSelectedPostpickPackoutLink        = get-value("PostpickPackoutLinkID")
          intSelectedPostpickPackoutLink        = INTEGER(chrSelectedPostpickPackoutLink)
          chrSelectedShipOrderStreamPackoutLink = get-value("ShipOrderStreamPackoutLinkID")
          intSelectedShipOrderStreamPackoutLink = INTEGER(chrSelectedShipOrderStreamPackoutLink).
          
   /* Process URL values */
   IF chrPackoutStationID <> "" THEN
      chrPackoutStationRow = 'selectPackoutStationRow(document.getElementById("packoutstation_browse_row_' + chrPackoutStationID + '"),"' 
                                                          + chrPackoutStationID +  '");'.
   IF chrSelectedPostpickPackoutLink <> "" THEN
      chrPostpickPackoutLinkRow = 'selectPostpickPackoutLinkRow(document.getElementById("postpick_packout_links_browse_row_' + chrSelectedPostpickPackoutLink + '"),"' 
                                                          + chrSelectedPostpickPackoutLink +  '");'.
                                                          
   IF chrSelectedShipOrderStreamPackoutLink <> "" THEN
      chrShipOrderStreamPackoutLinkRow = 'selectShipOrderStreamPackoutLinkRow(document.getElementById("shiporderstream_packout_links_browse_row_' + chrSelectedShipOrderStreamPackoutLink + '"),"' 
                                                          + chrSelectedShipOrderStreamPackoutLink +  '");'.
                                                          
   IF get-value('popup_packoutstationhistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("packoutstationhistory_browse_form_popup");'.
      
   IF get-value('popup_postpick_packout_links_browse') = "yes" THEN
      chrPopupPostPicks = 'enablePopup("postpick_packout_links_browse_form_popup");'.
      
   IF get-value('popup_add_postpick_packout_links_browse') = "yes" THEN
      chrPopupAddPostPicks = 'enablePopup("add_postpick_packout_links_browse_form_popup");'.
      
   IF get-value('popup_shiporderstream_packout_links_browse') = "yes" THEN
      chrPopupShipOrderStream = 'enablePopup("shiporderstream_packout_links_browse_form_popup");'.
      
   IF get-value('popup_add_shiporderstream_packout_links_browse') = "yes" THEN
      chrPopupAddShipOrderStream = 'enablePopup("add_shiporderstream_packout_links_browse_form_popup");'.

   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("packoutstation_browse").scrollTop=' + chrScrollToPackoutStationRow 
                                                           + chrPackoutStationRow
                                                           + chrPopUpHistory 
                                                           + chrPopupPostPicks + chrPostpickPackoutLinkRow
                                                           + chrPopupAddPostPicks
                                                           + chrPopupShipOrderStream + chrShipOrderStreamPackoutLinkRow
                                                           + chrPopupAddShipOrderStream.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Packout Station Admin".
   ThisPage:FrameTitle = "Packout Station Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Packout Station */
   ThisPage:addJavaScript("packoutstation.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pPackoutStationBrowse.
   
   FIND FIRST PackoutStation NO-LOCK 
      WHERE PackoutStation.PackoutStationID = intSelectedPackoutStation NO-ERROR. 
   
   FIND FIRST LocationType NO-LOCK 
      WHERE LocationType.TypeCode = "PostPick" NO-ERROR.
   intLocationTypePostpickID = LocationType.LocationTypeID.    
   
   /******* Popup Browsers and Forms ********/    
   RUN pPackoutStationDetails.
   
   RUN pPackoutStationHistoryBrowse.
   
   RUN pPostpickPackoutLinks.
   
   RUN pAddPostpickPackoutLinks.
   
   RUN pShipOrderStreamPackoutLinks.
   
   RUN pAddShipOrderStreamPackoutLinks.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT PackoutStationBrowseFrame                  NO-ERROR.
   DELETE OBJECT PackoutStationBrowse                       NO-ERROR.
   DELETE OBJECT PackoutStationBrowseButtons                NO-ERROR.
   DELETE OBJECT PackoutStationDetailsForm                  NO-ERROR.
   DELETE OBJECT PackoutStationDetailsButtons               NO-ERROR.
   DELETE OBJECT PostpickPackoutLinkBrowseForm              NO-ERROR.
   DELETE OBJECT PostpickPackoutLinkBrowse                  NO-ERROR.
   DELETE OBJECT PostpickPackoutLinkBrowseButtons           NO-ERROR.
   DELETE OBJECT AddPostpickPackoutLinksBrowseForm          NO-ERROR.
   DELETE OBJECT AddPostpickPackoutLinksBrowse              NO-ERROR.
   DELETE OBJECT AddPostpickPackoutLinksBrowseButtons       NO-ERROR.
   DELETE OBJECT ShipOrderStreamPackoutLinkBrowseForm       NO-ERROR.
   DELETE OBJECT ShipOrderStreamPackoutLinkBrowse           NO-ERROR.
   DELETE OBJECT ShipOrderStreamPackoutLinkBrowseButtons    NO-ERROR.
   DELETE OBJECT AddShipOrderStreamPackoutLinkBrowseForm    NO-ERROR.
   DELETE OBJECT AddShipOrderStreamPackoutLinkBrowse        NO-ERROR.
   DELETE OBJECT AddShipOrderStreamPackoutLinkBrowseButtons NO-ERROR.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPackoutStationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutStationBrowse Procedure 
PROCEDURE pPackoutStationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "packoutstation_details_form"}
   
   PackoutStationBrowse = NEW browseTable("packoutstation_browse").
   PackoutStationBrowse:BrowseWidth  = 965.
   PackoutStationBrowse:BrowseHeight = 455.
   PackoutStationBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   PackoutStationBrowse:insertColumn(fTL("PackoutStation ID"), 140, "INTEGER", "LEFT", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PackoutStation}
   
   PackoutStationBrowse:insertColumn(fTL("Station Name"),     150, "CHARACTER", "LEFT", FALSE).
   PackoutStationBrowse:insertColumn(fTL("Location"),         140, "CHARACTER", "LEFT", FALSE).
   PackoutStationBrowse:insertColumn(fTL("OnHold Location"),  140, "CHARACTER", "LEFT", FALSE).
   PackoutStationBrowse:insertColumn(fTL("PostPack Location"),140, "CHARACTER", "LEFT", FALSE).
   PackoutStationBrowse:insertColumn(fTL("Active"),            90, "LOGICAL", FALSE).
   
   /*Body*/
   PackoutStationBrowse:startBody().
   
   PackoutStationLoop:
   FOR EACH PackoutStation NO-LOCK /*idx=ActiveListingSequence*/
      BY PackoutStation.StationName:
      
      FIND FIRST Location OF PackoutStation NO-LOCK NO-ERROR.

      IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
         NEXT PackoutStationLoop.

      FIND FIRST onHoldLocation NO-LOCK 
         WHERE onHoldLocation.LocationID = PackoutStation.OnHoldLocationID NO-ERROR.

      IF NOT fCanViewBusinessUnit(intGblSessionID,onHoldLocation.BusinessUnitID) THEN 
         NEXT PackoutStationLoop.
      
      FIND FIRST postPackLocation NO-LOCK 
         WHERE postPackLocation.LocationID = PackoutStation.PostPackoutLocationID NO-ERROR.

      IF NOT fCanViewBusinessUnit(intGblSessionID,postPackLocation.BusinessUnitID) THEN 
         NEXT PackoutStationLoop.      

      PackoutStationBrowse:startRow(PackoutStation.PackoutStationID , "selectPackoutStationRow(this," + '"' + STRING(PackoutStation.PackoutStationID) + '"' + ");", "").
      PackoutStationBrowse:insertData(PackoutStation.PackoutStationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PackoutStation}
      
      PackoutStationBrowse:insertData((IF AVAILABLE PackoutStation THEN PackoutStation.StationName ELSE ""), "LEFT").
      PackoutStationBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "LEFT").
      PackoutStationBrowse:insertData((IF AVAILABLE onHoldLocation THEN onHoldLocation.LocationRef ELSE ""),"LEFT").
      PackoutStationBrowse:insertData((IF AVAILABLE postPackLocation THEN postPackLocation.LocationRef ELSE ""),"LEFT").
      PackoutStationBrowse:insertData((IF AVAILABLE PackoutStation THEN STRING(PackoutStation.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      PackoutStationBrowse:insertHiddenData("PackoutStationVersionID",PackoutStation.VersionID).
      
      PackoutStationBrowse:endRow().
      
   END. /*FOR EACH PackoutStation NO-LOCK */
   
   PackoutStationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PackoutStationBrowse:getErrors().
   
   /* Create a new frame */
   PackoutStationBrowseFrame = NEW pageFrame().
   PackoutStationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PackoutStationBrowseFrame:FormAction="dbPackoutStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PackoutStationBrowseFrame:formOpen("packoutstation_browse_form").
   
   /* Start the Frame Header */
   PackoutStationBrowseFrame:insertSpacer(5).
   PackoutStationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PackoutStationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PackoutStationBrowseFrame:frameClose().
   PackoutStationBrowseFrame:insertSpacer(10).
   
   PackoutStationBrowseFrame:insertHiddenField("packoutstation_browse_scroll","").
   PackoutStationBrowseFrame:insertHiddenField("PackoutStationID","").
   PackoutStationBrowseFrame:insertHiddenField("PackoutStationVersionID","").
   PackoutStationBrowseFrame:insertHiddenField("PostpickPackoutLinkID","").
   PackoutStationBrowseFrame:insertHiddenField("ShipOrderStreamPackoutLinkID","").
   PackoutStationBrowseFrame:insertHiddenField("form_name","packoutstation_browse_form").
   PackoutStationBrowseFrame:insertHiddenField("popup_packoutstationhistory_browse","").
   PackoutStationBrowseFrame:insertHiddenField("popup_postpick_packout_links_browse","").
   PackoutStationBrowseFrame:insertHiddenField("popup_add_postpick_packout_links_browse","").
   PackoutStationBrowseFrame:insertHiddenField("popup_shiporderstream_packout_links_browse","").
   PackoutStationBrowseFrame:insertHiddenField("popup_add_shiporderstream_packout_links_browse","").
   PackoutStationBrowseFrame:insertHiddenField("prog_name","adPackoutStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PackoutStationBrowseFrame}
   
   PackoutStationBrowseFrame:formClose().
   
   /* Create Button Bar */
   PackoutStationBrowseButtons = NEW buttonBar().
   PackoutStationBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   PackoutStationBrowseButtons:addButton("packoutstation_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewPackoutStationDetails('packoutstation_details_form');",
                                         (IF intSelectedPackoutStation > 0 THEN "" ELSE "Disabled")).
   
   PackoutStationBrowseButtons:addButton("packoutstation_browse_form_btn_create",
                                         fTL("Create"),
                                         "createPackoutStation('packoutstation_details_form');",
                                         "").
                                         
   PackoutStationBrowseButtons:addButton("packoutstation_browse_form_btn_postpicks",
                                         fTL("PostPicks"),
                                         "showPostPickPackoutLinks('postpick_packout_links_browse_form');",
                                         (IF intSelectedPackoutStation > 0 THEN "" ELSE "Disabled")).
                                         
   PackoutStationBrowseButtons:addButton("packoutstation_browse_form_btn_shiporderstreams",
                                         fTL("ShipOrder Streams"),
                                         "showShipOrderStreamPackoutLinks('shiporder_stream_packout_links_browse_form');",
                                         (IF intSelectedPackoutStation > 0 THEN "" ELSE "Disabled")).

   PackoutStationBrowseButtons:addButton("packoutstation_browse_form_btn_history",
                                         fTL("History"),
                                         "viewPackoutStationHistory('packoutstation_browse_form');",
                                         (IF intSelectedPackoutStation > 0 THEN "" ELSE "Disabled")).
   
   /*
   PackoutStationBrowseButtons:addButton("packoutstation_browse_form_btn_delete",
                                         fTL("Delete"),
                                         "confirmDeletePackoutStation();",
                                         (IF intSelectedPackoutStation > 0 THEN "" ELSE "Disabled")).
   */
   
   PackoutStationBrowseButtons:closeBar().  
   PackoutStationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPackoutStationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutStationDetails Procedure 
PROCEDURE pPackoutStationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "packoutstation_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PackoutStationID,StationName,LocationID,OnHoldLocationID,PostPackoutLocationID,Active"
          chrEditFieldList     = "StationName,LocationID,OnHoldLocationID,PostPackoutLocationID,Active"
          chrNewFieldList      = "StationName,LocationID,OnHoldLocationID,PostPackoutLocationID,Active"
          chrRequiredFieldList = "StationName,LocationID,OnHoldLocationID,PostPackoutLocationID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PackoutStationDetailsForm = NEW dataForm("packoutstation_details_form").
   PackoutStationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PackoutStationDetailsForm:FormAction = "dbPackoutStationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PackoutStationDetailsForm:FormWidth   = 460.
   PackoutStationDetailsForm:FormHeight  = 300.
   PackoutStationDetailsForm:FormTitle   = "PackoutStation Details".
   PackoutStationDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PackoutStationDetailsForm:insertPaddingColumn(30).
   PackoutStationDetailsForm:insertColumn(130).
   PackoutStationDetailsForm:insertColumn(120).
   PackoutStationDetailsForm:insertColumn(20).
   PackoutStationDetailsForm:insertColumn(4).
   PackoutStationDetailsForm:insertColumn(100).
   
   /* Fields */
   PackoutStationDetailsForm:startRow().
   PackoutStationDetailsForm:insertLabel("Station ID").
   PackoutStationDetailsForm:insertTextField("PackoutStationID", "", 110, TRUE).  
   
   PackoutStationDetailsForm:startRow().
   PackoutStationDetailsForm:insertLabel("StationName").
   PackoutStationDetailsForm:insertTextField("StationName", "", 110, TRUE).  
   
   PackoutStationDetailsForm:startRow().
   PackoutStationDetailsForm:insertLabel("Location").
   PackoutStationDetailsForm:insertComboField("LocationID", "", 110, TRUE).  
   
   FIND FIRST LocationType NO-LOCK
      WHERE LocationType.TypeCode = "Packout"
      AND   LocationType.Active NO-ERROR.
   IF NOT AVAILABLE LocationType THEN
      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: Packout".
   LocationLoop:
   FOR EACH Location OF LocationType NO-LOCK
      WHERE Location.Active
      BY    Location.LocationRef:
      IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
         NEXT LocationLoop.
      
      PackoutStationDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   
   PackoutStationDetailsForm:startRow().
   PackoutStationDetailsForm:insertLabel("OnHold Location").
   PackoutStationDetailsForm:insertComboField("OnHoldLocationID", "", 110, TRUE).  

   FIND FIRST onHoldLocationType NO-LOCK 
      WHERE onHoldLocationType.TypeCode = "PackoutOnHold" 
      AND   onHoldLocationType.Active NO-ERROR.
   IF NOT AVAILABLE onHoldLocationType THEN 
      chrPageBuildError = chrPageBuildError + "No LocationType is available for Code: PackoutOnHold".
   LocationLoop:
   FOR EACH onHoldLocation OF onHoldLocationType NO-LOCK
      WHERE onHoldLocation.Active 
      BY    onHoldLocation.LocationRef:
      IF NOT fCanViewBusinessUnit(intGblSessionID,onHoldLocation.BusinessUnitID) THEN 
         NEXT LocationLoop.
      
      PackoutStationDetailsForm:insertComboPairs("OnHoldLocationID", STRING(onHoldLocation.LocationID), onHoldLocation.LocationRef).
   END.
      
   PackoutStationDetailsForm:startRow().
   PackoutStationDetailsForm:insertLabel("PostPackout Location").
   PackoutStationDetailsForm:insertComboField("PostPackoutLocationID", "", 110, TRUE).  
  
   FIND FIRST LocationTypeGroup NO-LOCK 
      WHERE LocationTypeGroup.GroupCode = "PostPackoutLocations"
      AND   LocationTypeGroup.Active NO-ERROR.
   IF NOT AVAILABLE LocationTypeGroup THEN 
      chrPageBuildError = chrPageBuildError + "No LocationTypeGroup is available for Code: PostPackoutLocations".
   FOR EACH LocationTypeGroupLink OF LocationTypeGroup NO-LOCK
      WHERE LocationTypeGroupLink.Active,
         EACH postPackLocationType OF LocationTypeGroupLink NO-LOCK
            WHERE postPackLocationType.Active,
               EACH postPackLocation OF postPackLocationType NO-LOCK 
                  WHERE postPackLocationType.Active
                  BY    postPackLocation.LocationRef:
      
                  PackoutStationDetailsForm:insertComboPairs("PostPackoutLocationID", STRING(postPackLocation.LocationID), postPackLocation.LocationRef).
   END.
      
   PackoutStationDetailsForm:startRow().
   PackoutStationDetailsForm:insertLabel(fTL("Active")). 
   PackoutStationDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PackoutStationDetailsForm:insertComboPairs("Active", "yes", "Active").
   PackoutStationDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPackoutStationDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   PackoutStationDetailsForm:insertHiddenField("packoutstation_browse_scroll", "").
   PackoutStationDetailsForm:insertHiddenField("form_name", "packoutstation_details_form").
   PackoutStationDetailsForm:insertHiddenField("prog_name", "adPackoutStationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PackoutStationDetailsForm}
   
   /* Create Button Bar */
   PackoutStationDetailsButtons = NEW buttonBar().
   PackoutStationDetailsButtons:addButton("packoutstation_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updatePackoutStation('packoutstation_details_form');").
   PackoutStationDetailsButtons:addButton("packoutstation_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('packoutstation_details_form_popup');").
   PackoutStationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PackoutStationDetailsForm:FormButtons = PackoutStationDetailsButtons.
   
   PackoutStationDetailsForm:endForm(). 
   
   PackoutStationDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + PackoutStationDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPackoutStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutStationDetailsFields Procedure 
PROCEDURE pPackoutStationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      PackoutStationDetailsForm:startRow().
      PackoutStationDetailsForm:insertLabel(fTL("Field Label")).
      PackoutStationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
    /* This will be held in customer specific code repository */
    {adPackoutStation_packoutstation_details_form.i}
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPackoutStationHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPackoutStationHistoryBrowse Procedure 

PROCEDURE pPackoutStationHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   PackoutStationHistoryBrowseForm           = NEW dataForm("packoutstationhistory_browse_form").
   PackoutStationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   PackoutStationHistoryBrowseForm:FormWidth  = 860.
   PackoutStationHistoryBrowseForm:FormHeight = 530.
   PackoutStationHistoryBrowseForm:FormTitle  = fTL("Packout Station History").
   PackoutStationHistoryBrowseForm:FormType   = "xxl_large".
   PackoutStationHistoryBrowse                = NEW browseTable("packoutstationhistory_browse").
   PackoutStationHistoryBrowse:BrowseWidth    = 840.
   PackoutStationHistoryBrowse:BrowseHeight   = 490.
   
   PackoutStationHistoryBrowse:insertColumn(fTL("HistID"),           60, "INTEGER", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("Station Name"),     90, "CHARACTER", "LEFT", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("Location"),         90, "CHARACTER", "LEFT", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("OnHold Loc"),       90, "CHARACTER", "LEFT", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("PostPack Loc"),     90, "CHARACTER", "LEFT", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("Active"),           70, "LOGICAL", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("User"),            100, "CHARACTER", "LEFT", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("Operation"),        80, "CHARACTER", "LEFT", FALSE).
   PackoutStationHistoryBrowse:insertColumn(fTL("Created"),         100, "CHARACTER", "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PackoutStationHistory}
   
   PackoutStationHistoryBrowse:StartBody().
   
   FOR EACH PackoutStationHistory NO-LOCK
      WHERE PackoutStationHistory.PackoutStationID = intSelectedPackoutStation:
          
      FIND FIRST Location      OF PackoutStationHistory NO-LOCK NO-ERROR.
      FIND FIRST onHoldLocation NO-LOCK 
         WHERE onHoldLocation.LocationID = PackoutStationHistory.OnHoldLocationID NO-ERROR.
      FIND FIRST postPackLocation NO-LOCK 
         WHERE postPackLocation.LocationID = PackoutStationHistory.PostPackoutLocationID NO-ERROR.
      FIND FIRST OperationType OF PackoutStationHistory NO-LOCK NO-ERROR.
      FIND FIRST GateUser      OF PackoutStationHistory NO-LOCK NO-ERROR.

      PackoutStationHistoryBrowse:startRow(PackoutStationHistory.PackoutStationHistoryID, 
                                        "selectBrowseRow(this," + '"packoutstationhistory_browse"' + ");","").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i PackoutStationHistory}
      
      PackoutStationHistoryBrowse:insertData(PackoutStationHistory.PackoutStationHistoryID, "LEFT").
      PackoutStationHistoryBrowse:insertData(PackoutStationHistory.StationName, "LEFT").
      PackoutStationHistoryBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "LEFT").
      PackoutStationHistoryBrowse:insertData((IF AVAILABLE onHoldLocation THEN onHoldLocation.LocationRef ELSE ""), "LEFT").
      PackoutStationHistoryBrowse:insertData((IF AVAILABLE postPackLocation THEN postPackLocation.LocationRef ELSE ""), "LEFT").
      PackoutStationHistoryBrowse:insertData(STRING(PackoutStationHistory.Active,"Yes/No")).
      PackoutStationHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      PackoutStationHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "LEFT").
      PackoutStationHistoryBrowse:insertData(fDisplayDate&Time(PackoutStationHistory.Created,"y/m/d H:M:S"), "RIGHT").
      
      PackoutStationHistoryBrowse:endRow().
   END. /* FOR EACH PackoutStationHistory */
   
   PackoutStationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PackoutStationHistoryBrowse:getErrors().
   
   PackoutStationHistoryBrowseForm:insertHiddenField("popup_packoutstationhistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PackoutStationHistoryBrowseForm}
   
   /* Create Button Bar */
   PackoutStationHistoryBrowseButtons = NEW buttonBar().
   
   PackoutStationHistoryBrowseButtons:addButton("packoutstationhistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('packoutstationhistory_browse_form_popup');").
   
   PackoutStationHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PackoutStationHistoryBrowseForm:FormBrowse  = PackoutStationHistoryBrowse.
   PackoutStationHistoryBrowseForm:FormButtons = PackoutStationHistoryBrowseButtons.
   PackoutStationHistoryBrowseForm:endForm(). 
   
   PackoutStationHistoryBrowseForm:displayForm().   
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderStreamPackoutLink) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderStreamPackoutLink Procedure
PROCEDURE pShipOrderStreamPackoutLinks:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   ShipOrderStreamPackoutLinkBrowseForm            = NEW dataForm("shiporderstream_packout_links_browse_form").
   ShipOrderStreamPackoutLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   ShipOrderStreamPackoutLinkBrowseForm:FormAction = "dbSetShipOrderStreamPackoutLinkActive.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   ShipOrderStreamPackoutLinkBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   ShipOrderStreamPackoutLinkBrowseForm:FormWidth  = 860.
   ShipOrderStreamPackoutLinkBrowseForm:FormHeight = 530.
   ShipOrderStreamPackoutLinkBrowseForm:FormTitle  = fTL("Current ShipOrder Streams Linked to Packout Station") + 
                                                        (IF AVAILABLE PackoutStation THEN " : " + PackoutStation.StationName ELSE "").
   ShipOrderStreamPackoutLinkBrowseForm:FormType   = "xxl_large".
   
   /* Form Data */
   ShipOrderStreamPackoutLinkBrowseForm:insertPaddingColumn(180).
   ShipOrderStreamPackoutLinkBrowseForm:insertColumn(110).
   ShipOrderStreamPackoutLinkBrowseForm:insertColumn(130).
   ShipOrderStreamPackoutLinkBrowseForm:insertColumn(40).
   ShipOrderStreamPackoutLinkBrowseForm:insertColumn(85).
   ShipOrderStreamPackoutLinkBrowseForm:insertColumn(130).
   
   ShipOrderStreamPackoutLinkBrowseForm:startRow().
   ShipOrderStreamPackoutLinkBrowseForm:insertBlankRow(10). 

   ShipOrderStreamPackoutLinkBrowse              = NEW browseTable("shiporderstream_packout_links_browse").
   ShipOrderStreamPackoutLinkBrowse:BrowseWidth  = 840.
   ShipOrderStreamPackoutLinkBrowse:BrowseHeight = 460.
   
   ShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Link ID"),      100, "INTEGER",   FALSE).
   ShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Stream ID"),    100, "INTEGER",   FALSE).
   ShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Stream"),       150, "CHARACTER", "LEFT", FALSE).
   ShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Stream Descr"), 160, "CHARACTER", "LEFT", FALSE).
   ShipOrderStreamPackoutLinkBrowse:insertColumn(fTL("Link Active"),  100, "CHARACTER", "LEFT", FALSE).
   
   ShipOrderStreamPackoutLinkBrowse:StartBody().
   ShipOrderStreamPackoutLinkLoop:
   FOR EACH ShipOrderStreamPackoutLink NO-LOCK
      WHERE ShipOrderStreamPackoutLink.PackoutStationID = PackoutStation.PackoutStationID,
       EACH ShipOrderStream OF ShipOrderStreamPackoutLink NO-LOCK
       BY   ShipOrderStreamPackoutLink.Active DESCENDING
       BY   ShipOrderStream.StreamName:

      ShipOrderStreamPackoutLinkBrowse:startRow(ShipOrderStreamPackoutLink.ShipOrderStreamPackoutLinkID,
                                  "selectShipOrderStreamPackoutLinkRow(this," + '"' +
                                  STRING(ShipOrderStreamPackoutLink.ShipOrderStreamPackoutLinkID) + '");', "").

      ShipOrderStreamPackoutLinkBrowse:insertData(STRING(ShipOrderStreamPackoutLink.ShipOrderStreamPackoutLinkID)).
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i ShipOrderStreamPackoutLink}
      ShipOrderStreamPackoutLinkBrowse:insertData(STRING(ShipOrderStreamPackoutLink.ShipOrderStreamID)).
      ShipOrderStreamPackoutLinkBrowse:insertData((IF AVAILABLE ShipOrderStream THEN ShipOrderStream.StreamName ELSE ""), "left").
      ShipOrderStreamPackoutLinkBrowse:insertData((IF AVAILABLE ShipOrderStream THEN ShipOrderStream.StreamDescr ELSE ""), "left").
      ShipOrderStreamPackoutLinkBrowse:insertData(STRING(ShipOrderStreamPackoutLink.Active,"Yes/No")).

      /* Add hidden fields */
      ShipOrderStreamPackoutLinkBrowse:insertHiddenData("ShipOrderStreamID", ShipOrderStream.ShipOrderStreamID).
      ShipOrderStreamPackoutLinkBrowse:insertHiddenData("ShipOrderStreamVersionID", ShipOrderStream.VersionID).
      ShipOrderStreamPackoutLinkBrowse:insertHiddenData("ShipOrderStreamPackoutLinkVersionID",ShipOrderStreamPackoutLink.VersionID).
      ShipOrderStreamPackoutLinkBrowse:insertHiddenData("LinkActive",STRING(ShipOrderStreamPackoutLink.Active)).

      ShipOrderStreamPackoutLinkBrowse:endRow().
   END. /* FOR EACH ShipOrderStreamPackoutLink WHERE ShipOrderStreamPackoutLink.PackoutStationID = PackoutStation.PackoutStationID ...  */
   
   ShipOrderStreamPackoutLinkBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + ShipOrderStreamPackoutLinkBrowse:getErrors().
   
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("PackoutStationID", IF AVAILABLE PackoutStation THEN STRING(PackoutStation.PackoutStationID) ELSE "").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("ShipOrderStreamPackoutLinkID", "").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("ShipOrderStreamPackoutLinkVersionID", "").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("ShipOrderStreamPackoutLinkActive", "").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("packoutstation_browse_scroll", "").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("shiporderstream_packout_link_browse_scroll", "").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("popup_shiporderstream_packout_links_browse","yes").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("popup_add_shiporderstream_packout_links_browse","").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("form_name","shiporderstream_packout_links_browse_form").
   ShipOrderStreamPackoutLinkBrowseForm:insertHiddenField("prog_name","adPackoutStationAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderStreamPackoutLinkBrowseForm}
   
   /* Create Button Bar */
   ShipOrderStreamPackoutLinkBrowseButtons = NEW buttonBar().
   
   ShipOrderStreamPackoutLinkBrowseButtons:addButton("shiporderstream_packout_links_browse_form_btn_add",
                                                    fTL("Add Links"),
                                                    "addShipOrderStreamPackoutLinks('');"). 
   
   ShipOrderStreamPackoutLinkBrowseButtons:addButton("shiporderstream_packout_links_browse_form_btn_deactivatelink",
                                                    fTL("Deactivate Link"),
                                                    "setShipOrderStreamPackoutLinkActiveFlag('shiporderstream_packout_links_browse_form');",
                                                    (IF intSelectedShipOrderStreamPackoutLink > 0 THEN "" ELSE "Disabled")).
   
   ShipOrderStreamPackoutLinkBrowseButtons:addButton("shiporderstream_packout_links_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('shiporderstream_packout_links_browse_form_popup');").
   
   ShipOrderStreamPackoutLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ShipOrderStreamPackoutLinkBrowseForm:FormBrowse  = ShipOrderStreamPackoutLinkBrowse.
   ShipOrderStreamPackoutLinkBrowseForm:FormButtons = ShipOrderStreamPackoutLinkBrowseButtons.
   ShipOrderStreamPackoutLinkBrowseForm:endForm(). 
   
   ShipOrderStreamPackoutLinkBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

