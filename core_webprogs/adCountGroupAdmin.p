
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
{fncUserAccessFunctions.i}
/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

/* Count Group Browse */
DEFINE VARIABLE intSelectedCountGroup    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountGroupRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountGroupRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountGroupID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupHistory          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intNumLocations          AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrLocations             AS CHARACTER NO-UNDO.
DEFINE VARIABLE intLocationCnt           AS INTEGER   NO-UNDO.
DEFINE VARIABLE intLoop                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrUserIDList            AS CHARACTER NO-UNDO.

DEFINE VARIABLE CountGroupBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountGroupBrowse         AS browseTable.
DEFINE VARIABLE CountGroupBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountGroupDetailsForm    AS dataForm.
DEFINE VARIABLE CountGroupDetailsButtons AS buttonBar.

DEFINE VARIABLE CountGroupUserBrowse     AS browseTable.

/* Generate tasks */
DEFINE VARIABLE CountGroupGenerateTasksForm    AS dataForm.
DEFINE VARIABLE CountGroupGenerateTasksButtons AS buttonBar.

/* CountGroupLocationLink Browse */
DEFINE VARIABLE chrPopupCountGroupLocationLinks      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCountGroupLocationLink    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountGroupLocationLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountGroupLocationLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountGroupLocationLinkID          AS CHARACTER NO-UNDO.

DEFINE VARIABLE CountGroupLocationLinkBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountGroupLocationLinkBrowseForm     AS dataForm.
DEFINE VARIABLE CountGroupLocationLinkBrowse         AS browseTable.
DEFINE VARIABLE CountGroupLocationLinkBrowseButtons  AS buttonBar.

/* AddCountGroupLocationLink Browse */
DEFINE VARIABLE chrPopupAddCountGroupLocationLinks AS CHARACTER NO-UNDO.

DEFINE VARIABLE AddCountGroupLocationLinkBrowseFrame   AS pageFrame.
DEFINE VARIABLE AddCountGroupLocationLinkBrowseForm    AS dataForm.
DEFINE VARIABLE AddCountGroupLocationLinkBrowse        AS browseTable.
DEFINE VARIABLE AddCountGroupLocationLinkBrowseButtons AS buttonBar.

/* CountGroupRecount Browse */
DEFINE VARIABLE chrPopupCountGroupRecounts      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCountGroupRecount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountGroupRecountRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountGroupRecountRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountGroupRecountID          AS CHARACTER NO-UNDO.

DEFINE VARIABLE CountGroupRecountBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountGroupRecountBrowseForm     AS dataForm.
DEFINE VARIABLE CountGroupRecountBrowse         AS browseTable.
DEFINE VARIABLE CountGroupRecountBrowseButtons  AS buttonBar.

DEFINE VARIABLE CountGroupRecountDetailsForm    AS dataForm.
DEFINE VARIABLE CountGroupRecountDetailsButtons AS buttonBar.

/* CountGroup History Browse */
DEFINE VARIABLE CountGroupHistoryBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountGroupHistoryBrowseForm     AS dataForm.
DEFINE VARIABLE CountGroupHistoryBrowse         AS browseTable.
DEFINE VARIABLE CountGroupHistoryBrowseButtons  AS buttonBar.

/* Location Filter */
DEFINE VARIABLE logFilterIsPoppedUp       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logAllUnused              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrPopupFilters           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectedLocationRef    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLocationRefBegins      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedLocationTypeID AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedAisleID        AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedWorkZoneID     AS INTEGER   NO-UNDO.

DEFINE VARIABLE LocationBrowseFilterForm    AS dataForm.
DEFINE VARIABLE LocationBrowseFilterButtons AS buttonBar.

/*  Buffers*/
DEFINE BUFFER displayCountGroup       FOR CountGroup.
DEFINE BUFFER groundLocationTypeGroup FOR LocationTypeGroup.
DEFINE BUFFER upperLocationTypeGroup  FOR LocationTypeGroup.

/* Temp-tables */
DEFINE TEMP-TABLE ttFile NO-UNDO
   FIELD FileID AS INTEGER
   INDEX FileID
     FileId.

/* File Upload Vars */
DEFINE TEMP-TABLE ttUploadLocation LIKE Location.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedFile    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectFileRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToFileRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFile       AS CHARACTER NO-UNDO.

DEFINE VARIABLE intSelectedLocationHistory    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectLocationHistoryRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToLocationHistoryRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrLocationHistoryID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupLocationDifferences   AS CHARACTER NO-UNDO.

DEFINE VARIABLE intLocationFileMasterID   AS INTEGER   NO-UNDO.
DEFINE VARIABLE intPartFileMasterID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrValidFile              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFileSelection     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFileLocationLinks AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrMessageNumberOfUpdates AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrTaskGenerateMessage    AS CHARACTER NO-UNDO.

DEFINE VARIABLE chrPopupErrors                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupUploadCountGroupLocationLinks AS CHARACTER NO-UNDO.

/* variables for file import */
DEFINE VARIABLE intFileLineSequence              AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrImportedLine                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mptFile                          AS MEMPTR    NO-UNDO.
DEFINE VARIABLE chrFileName                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDestinationFile               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrColumnValue                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intColumnsCount                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE logIsLogFile                     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE intLocationRefCol                AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCurrentColumnsWidth           AS INTEGER   NO-UNDO.
DEFINE VARIABLE intDisplayedColumnsNo            AS INTEGER   NO-UNDO.
DEFINE VARIABLE intAddedLocationLinksCount       AS INTEGER   NO-UNDO.
DEFINE VARIABLE intDeactivatedLocationLinksCount AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrIgnoredLocations              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSuccessTaskGenerate           AS CHARACTER NO-UNDO.

/* Streams */
DEFINE STREAM sUploadFile.

/* Temp-tables */
DEFINE TEMP-TABLE ttFileLine
   FIELD FileLineNo AS INTEGER 
   FIELD LineString AS CHARACTER 
   INDEX iFileLineNo FileLineNo.
   
/* Objects */

/* Objects for File Browse */
DEFINE VARIABLE FileSelectionFrame   AS pageFrame.
DEFINE VARIABLE FileSelectionForm    AS dataForm.
DEFINE VARIABLE FileBrowseFrame      AS pageFrame.
DEFINE VARIABLE FileBrowseForm       AS dataForm.
DEFINE VARIABLE FileBrowse           AS browseTable.
DEFINE VARIABLE FileBrowseButtons    AS buttonBar.
DEFINE VARIABLE FileDetailsForm      AS dataForm.
DEFINE VARIABLE FileDetailsButtons   AS buttonBar.

/* Objects for UploadLocations Browse */
DEFINE VARIABLE UploadCountGroupLocationLinksBrowseFrame   AS pageFrame.
DEFINE VARIABLE UploadCountGroupLocationLinksBrowse        AS browseTable.
DEFINE VARIABLE UploadCountGroupLocationLinksBrowseButtons AS buttonBar.
DEFINE VARIABLE UploadCountGroupLocationLinksBrowseForm    AS dataForm.

/* Objects for UploadPartLocations Browse */
DEFINE VARIABLE UploadCountGroupPartLocationLinksBrowseFrame   AS pageFrame.
DEFINE VARIABLE UploadCountGroupPartLocationLinksBrowse        AS browseTable.
DEFINE VARIABLE UploadCountGroupPartLocationLinksBrowseButtons AS buttonBar.
DEFINE VARIABLE UploadCountGroupPartLocationLinksBrowseForm    AS dataForm.

/* Objects for Errors Browse */
DEFINE VARIABLE ErrorsBrowseFrame   AS pageFrame.
DEFINE VARIABLE ErrorsBrowseForm    AS dataForm.
DEFINE VARIABLE ErrorsBrowse        AS browseTable.
DEFINE VARIABLE ErrorsBrowseButtons AS buttonBar.

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
         HEIGHT             = 22.67
         WIDTH              = 62.2.
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

&IF DEFINED(EXCLUDE-pAddCountGroupLocationLinksBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddCountGroupLocationLinksBrowse Procedure 
PROCEDURE pAddCountGroupLocationLinksBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   AddCountGroupLocationLinkBrowseForm            = NEW dataForm("addcountgrouplocationlink_browse_form").
   AddCountGroupLocationLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   AddCountGroupLocationLinkBrowseForm:FormAction = "dbCreateCountGroupLocationLinks.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   AddCountGroupLocationLinkBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   AddCountGroupLocationLinkBrowseForm:FormWidth  = 700.
   AddCountGroupLocationLinkBrowseForm:FormHeight = 490.
   AddCountGroupLocationLinkBrowseForm:FormTitle  = fTL("Available Locations to Link to Count Group ") + 
                                                        (IF AVAILABLE CountGroup THEN " : " + CountGroup.CountGroupName ELSE "").
   AddCountGroupLocationLinkBrowseForm:FormType   = "xl_large".
   
   /* Form Data */
   AddCountGroupLocationLinkBrowseForm:insertPaddingColumn(180).
   AddCountGroupLocationLinkBrowseForm:insertColumn(110).
   AddCountGroupLocationLinkBrowseForm:insertColumn(130).
   AddCountGroupLocationLinkBrowseForm:insertColumn(40).
   AddCountGroupLocationLinkBrowseForm:insertColumn(85).
   AddCountGroupLocationLinkBrowseForm:insertColumn(130).
   
   AddCountGroupLocationLinkBrowseForm:startRow().
   AddCountGroupLocationLinkBrowseForm:insertBlankRow(10). 

   AddCountGroupLocationLinkBrowse              = NEW browseTable("addcountgrouplocationlink_browse").
   AddCountGroupLocationLinkBrowse:BrowseWidth  = 680.
   AddCountGroupLocationLinkBrowse:BrowseHeight = 442.
   
   AddCountGroupLocationLinkBrowse:insertColumn(fTL("LocationID"),    80, "INTEGER", FALSE).
   AddCountGroupLocationLinkBrowse:insertColumn(fTL("LocationType"), 120, "CHARACTER", "left", FALSE).
   AddCountGroupLocationLinkBrowse:insertColumn(fTL("Aisle"),        110, "CHARACTER", "left", FALSE).
   AddCountGroupLocationLinkBrowse:insertColumn(fTL("WorkZone"),     110, "CHARACTER", "left", FALSE).
   AddCountGroupLocationLinkBrowse:insertColumn(fTL("Location Ref"), 160, "CHARACTER", "left", FALSE).
   AddCountGroupLocationLinkBrowse:insertColumn(fTL("Active"),        80, "CHARACTER", FALSE).
   
   AddCountGroupLocationLinkBrowse:StartBody().
   
   IF AVAILABLE CountGroup THEN
   DO:
      IF logAllUnused THEN
      DO:
         LocationLoop:
         FOR EACH Location NO-LOCK
            BY Location.ACTIVE DESC
            BY Location.CountSequence
            BY Location.LocationID:

            IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
               NEXT LocationLoop.

            /* Skip if used by any count group */
            IF CAN-FIND(FIRST CountGroupLocationLink OF Location NO-LOCK) THEN
               NEXT. 
            
            FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
            
            FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.
            
            IF AVAILABLE Aisle THEN
               FIND FIRST WorkZone OF Aisle NO-LOCK NO-ERROR.
            
            RUN pSetLocationRows.

         END. /* FOR EACH Location NO-LOCK */

      END. /* IF logAllUnused THEN */
      ELSE
      DO:
         IF chrSelectedLocationRef <> "" THEN
         DO:
            FIND FIRST Location NO-LOCK 
               WHERE Location.LocationRef = chrSelectedLocationRef NO-ERROR.
            
            IF AVAILABLE Location AND fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN
            DO:
               FIND FIRST CountGroupLocationLink OF Location NO-LOCK 
               WHERE CountGroupLocationLink.CountGroupID = CountGroup.CountGroupID NO-ERROR.
               
               /* Show location only if not linked to this countgroup already */
               IF NOT AVAILABLE CountGroupLocationLink THEN
               DO:
                  FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
                  FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.
                  IF AVAILABLE Aisle THEN
                     FIND FIRST WorkZone OF Aisle NO-LOCK NO-ERROR.
                  RUN pSetLocationRows.
               END.
            END.
         END. /* IF chrSelectedLocationRef <> "" THEN */
         ELSE IF chrLocationRefBegins <> "" THEN
         DO:
            LocationBeginsLoop:
            FOR EACH Location NO-LOCK
               WHERE Location.LocationRef BEGINS chrLocationRefBegins
               BY Location.ACTIVE DESC
               BY Location.CountSequence
               BY Location.LocationID:
   
               IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
                  NEXT LocationBeginsLoop.

               /* If linked, then skip */
               IF CAN-FIND(FIRST CountGroupLocationLink OF Location NO-LOCK
                           WHERE CountGroupLocationLink.CountGroupID = CountGroup.CountGroupID) THEN
                  NEXT LocationBeginsLoop.
               
               FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
               
               IF intSelectedLocationTypeID <> 0 AND Location.LocationTypeID <> intSelectedLocationTypeID THEN
                  NEXT LocationBeginsLoop.
               
               FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.

               IF intSelectedAisleID <> 0 AND Location.AisleID <> intSelectedAisleID THEN
                  NEXT LocationBeginsLoop.

               IF AVAILABLE Aisle THEN
               DO:
                  FIND FIRST WorkZone OF Aisle NO-LOCK NO-ERROR.

                  IF intSelectedWorkZoneID <> 0 AND WorkZone.WorkZoneID <> intSelectedWorkZoneID THEN
                     NEXT LocationBeginsLoop.
               END.
                  
               RUN pSetLocationRows.
            END. /* FOR EACH Location NO-LOCK */
         END. /* IF chrLocationRefBegins <> "" THEN */
         ELSE IF intSelectedLocationTypeID <> 0 THEN     
         DO:
            LocationTypeLoop:
            FOR EACH Location NO-LOCK
               WHERE Location.LocationTypeID = intSelectedLocationTypeID
               BY Location.ACTIVE DESC
               BY Location.CountSequence
               BY Location.LocationID:
               
               IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
                  NEXT LocationTypeLoop.

               /* If linked already, then skip */
               IF CAN-FIND(FIRST CountGroupLocationLink OF Location NO-LOCK
                           WHERE CountGroupLocationLink.CountGroupID = CountGroup.CountGroupID) THEN
                  NEXT LocationTypeLoop.
               
               FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
               
               FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.

               IF intSelectedAisleID <> 0 AND Location.AisleID <> intSelectedAisleID THEN
                  NEXT LocationTypeLoop.

               IF AVAILABLE Aisle THEN
               DO:
                  FIND FIRST WorkZone OF Aisle NO-LOCK NO-ERROR.

                  IF intSelectedWorkZoneID <> 0 AND WorkZone.WorkZoneID <> intSelectedWorkZoneID THEN
                     NEXT LocationTypeLoop.
               END.
                  
               RUN pSetLocationRows.
            END. /* FOR EACH Location NO-LOCK */
         END. /* IF intSelectedLocationTypeID <> 0 */
         ELSE IF intSelectedAisleID <> 0 THEN
         DO:
            
            AisleLoop:
            FOR EACH Location NO-LOCK
               WHERE Location.AisleID = intSelectedAisleID
               BY Location.ACTIVE DESC
               BY Location.CountSequence
               BY Location.LocationID:
               
               IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
                  NEXT AisleLoop.

               /* If linked already, then skip */
               IF CAN-FIND(FIRST CountGroupLocationLink OF Location NO-LOCK
                           WHERE CountGroupLocationLink.CountGroupID = CountGroup.CountGroupID) THEN
                  NEXT AisleLoop.
               
               FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
               
               IF intSelectedLocationTypeID <> 0 AND Location.LocationTypeID <> intSelectedLocationTypeID THEN
                  NEXT AisleLoop.
               
               FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.

               IF AVAILABLE Aisle THEN
               DO:
                  FIND FIRST WorkZone OF Aisle NO-LOCK NO-ERROR.

                  IF intSelectedWorkZoneID <> 0 AND WorkZone.WorkZoneID <> intSelectedWorkZoneID THEN
                     NEXT AisleLoop.
               END.
                  
               RUN pSetLocationRows.
            END. /* FOR EACH Location NO-LOCK */
         
         END.
         ELSE IF intSelectedWorkZoneID <> 0 THEN
         DO:
            WorkZoneLoop:
            FOR EACH WorkZone NO-LOCK
               WHERE WorkZone.WorkZoneID = intSelectedWorkZoneID,
               EACH Aisle OF WorkZone NO-LOCK,
               EACH Location OF Aisle NO-LOCK
                  BY Location.ACTIVE DESC
                  BY Location.CountSequence
                  BY Location.LocationID:

               IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
                  NEXT WorkZoneLoop.

               /* If linked already, then skip */
               IF CAN-FIND(FIRST CountGroupLocationLink OF Location NO-LOCK
                           WHERE CountGroupLocationLink.CountGroupID = CountGroup.CountGroupID) THEN
                  NEXT WorkZoneLoop.
               
               FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
               
               IF intSelectedLocationTypeID <> 0 AND Location.LocationTypeID <> intSelectedLocationTypeID THEN
                  NEXT WorkZoneLoop.
               
               IF intSelectedAisleID <> 0 AND Location.AisleID <> intSelectedAisleID THEN
                  NEXT WorkZoneLoop.

               RUN pSetLocationRows.
            END.


         END.
      END.
   END. /*IF AVAILABLE CountGroup THEN */
   
   IF intLocationCnt <= 2000 THEN
     chrLocations  = TRIM(chrLocations, ",").
   ELSE
     chrLocations = "".

   AddCountGroupLocationLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + AddCountGroupLocationLinkBrowse:getErrors().
   
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("LocationList","").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("CountGroupID",chrCountGroupID).
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("CountGroupLocationLinkID",chrCountGroupLocationLinkID).
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("CountGroupLocationLinkVersionID","").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("countgroup_browse_scroll","").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("countgrouplocationlink_browse_scroll","").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("popup_countgrouplocationlink_browse","").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("popup_addcountgrouplocationlink_browse","").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("filtering", "no").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("form_name","addcountgrouplocationlink_browse_form").
   AddCountGroupLocationLinkBrowseForm:insertHiddenField("prog_name","adCountGroupAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddCountGroupLocationLinkBrowseForm}
   
   /* Create Button Bar */
   AddCountGroupLocationLinkBrowseButtons = NEW buttonBar().
   
   AddCountGroupLocationLinkBrowseButtons:addButton("addcountgrouplocationlink_browse_form_btn_selectall",
                                                    fTL("Select All"),
                                                    "selectAllLocations('" + STRING(chrLocations) + "','" + STRING(intLocationCnt) + "')").

   AddCountGroupLocationLinkBrowseButtons:addButton("addcountgrouplocationlink_browse_form_btn_filter",
                                                    fTL("Filter"),
                                                    "viewLocationFilter('location_filter_form');"). 

   AddCountGroupLocationLinkBrowseButtons:addButton("addcountgrouplocationlink_browse_form_btn_create",
                                                    fTL("Confirm Links"),
                                                    "createCountGroupLocationLinks('');"). 
   
   AddCountGroupLocationLinkBrowseButtons:addButton("addcountgrouplocationlink_browse_form_btn_delete",
                                                    fTL("Clear Selection"),
                                                    "deselectAllLocationRows()").
   
   AddCountGroupLocationLinkBrowseButtons:addButton("addcountgrouplocationlink_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('addcountgrouplocationlink_browse_form_popup');").
   
   AddCountGroupLocationLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddCountGroupLocationLinkBrowseForm:FormBrowse  = AddCountGroupLocationLinkBrowse.
   AddCountGroupLocationLinkBrowseForm:FormButtons = AddCountGroupLocationLinkBrowseButtons.
   AddCountGroupLocationLinkBrowseForm:endForm(). 
   
   AddCountGroupLocationLinkBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupBrowse Procedure 
PROCEDURE pCountGroupBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "countgroup_details_form"}
   
   CountGroupBrowse              = NEW browseTable("countgroup_browse").
   CountGroupBrowse:BrowseWidth  = 965.
   CountGroupBrowse:BrowseHeight = 455.
   CountGroupBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountGroupBrowse:insertColumn(fTL("GroupID"),       60, "CHARACTER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountGroup}
   
   CountGroupBrowse:insertColumn(fTL("Group Code"),   100, "CHARACTER", "left", FALSE).
   CountGroupBrowse:insertColumn(fTL("Group Name"),   110, "CHARACTER", "left", FALSE).
   CountGroupBrowse:insertColumn(fTL("Group Type"),   100, "CHARACTER", "left", FALSE).
   CountGroupBrowse:insertColumn(fTL("Task Type"),    100, "CHARACTER", "left", FALSE).
   CountGroupBrowse:insertColumn(fTL("Active"),        50, "LOGICAL", FALSE).
   CountGroupBrowse:insertColumn(fTL("ListingSeq"),    80, "INTEGER", FALSE).
   CountGroupBrowse:insertColumn(fTL("Frequency"),     80, "INTEGER", FALSE).
   CountGroupBrowse:insertColumn(fTL("BusinessUnit"), 100, "CHARACTER", "left", FALSE).
   CountGroupBrowse:insertColumn(fTL("Num Locs"),      80, "INTEGER",   "left", FALSE).
   CountGroupBrowse:insertColumn("",                   35, "", FALSE).
   
   /*Body*/
   CountGroupBrowse:startBody().
   
   /* Find all Count Groups then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   CountGroupLoop:
   FOR EACH CountGroup NO-LOCK
      BY    CountGroup.Active DESC
      BY    CountGroup.CountGroupID:

      IF NOT fCanViewBusinessUnit(intGblSessionID,CountGroup.BusinessUnitID) THEN 
         NEXT CountGroupLoop.

      FIND FIRST BusinessUnit OF CountGroup NO-LOCK NO-ERROR.
      FIND FIRST CountGroupType OF CountGroup NO-LOCK NO-ERROR.
      FIND FIRST CountTaskType OF CountGroup NO-LOCK NO-ERROR.

      CountGroupBrowse:startRow(CountGroup.CountGroupID, "selectCountGroupRow(this," + '"' 
                                                                            + STRING(CountGroup.CountGroupID) + '"' + ");", "").
      CountGroupBrowse:insertData(CountGroup.CountGroupID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountGroup}
      
      intNumLocations = 0.
      FOR EACH CountGroupLocationLink OF CountGroup
         WHERE CountGroupLocationLink.Active = YES NO-LOCK:
         intNumLocations = intNumLocations + 1.
      END.
      
      CountGroupBrowse:insertData(CountGroup.CountGroupCode, "left").
      CountGroupBrowse:insertData(CountGroup.CountGroupName, "left").
      CountGroupBrowse:insertData((IF AVAILABLE CountGroupType THEN CountGroupType.TypeCode ELSE ""),"left").
      CountGroupBrowse:insertData((IF AVAILABLE CountTaskType THEN CountTaskType.TypeCode ELSE ""),"left").
      CountGroupBrowse:insertData(STRING(CountGroup.Active, "Yes/No")).
      CountGroupBrowse:insertData(CountGroup.ListingSequence).
      CountGroupBrowse:insertData(CountGroup.FREQUENCY).
      CountGroupBrowse:insertData((IF AVAILABLE BusinessUnit THEN BusinessUnit.UnitName ELSE ""), "left").
      CountGroupBrowse:insertData(STRING(intNumLocations),"").
      CountGroupBrowse:insertImage("view", "View Change History", 'viewCountGroupHistoryBrowse("' 
                                     + STRING(CountGroup.CountGroupID) + '", this.parentNode);').
      /* Add hidden fields */
      CountGroupBrowse:insertHiddenData("CountGroupVersionID",CountGroup.VersionID).
      CountGroupBrowse:insertHiddenData("Active",CountGroup.Active).
      CountGroupBrowse:insertHiddenData("GroupType",(IF AVAILABLE CountGroupType THEN CountGroupType.TypeCode ELSE "")).
      
      IF CAN-FIND(FIRST CountGroupLocationLink OF CountGroup WHERE CountGroupLocationLink.Active) THEN
         CountGroupBrowse:insertHiddenData("GenerateTasks","yes").
      ELSE
         CountGroupBrowse:insertHiddenData("GenerateTasks","no").
      
      /* Check if there are uncompleted tasks for the current group; if yes then can't generate other tasks */
      FIND FIRST CountTask OF CountGroup NO-LOCK 
         WHERE CountTask.Complete = "" NO-ERROR.
      
      IF AVAILABLE CountTask THEN 
         CountGroupBrowse:insertHiddenData("CanGenerateTasks","no").
      ELSE 
         CountGroupBrowse:insertHiddenData("CanGenerateTasks","yes").      
      
      CountGroupBrowse:endRow().
      
   END. /* FOR EACH CountGroup NO-LOCK */
   
   CountGroupBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountGroupBrowse:getErrors().
   
   /* Create a new frame */
   CountGroupBrowseFrame           = NEW pageFrame().
   CountGroupBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CountGroupBrowseFrame:FormAction="dbCountGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CountGroupBrowseFrame:formOpen("countgroup_browse_form").
   
   /* Start the Frame Header */
   CountGroupBrowseFrame:insertSpacer(5).
   CountGroupBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CountGroupBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CountGroupBrowseFrame:frameClose().
   CountGroupBrowseFrame:insertSpacer(10).
   
   CountGroupBrowseFrame:insertHiddenField("countgroup_browse_scroll","").
   CountGroupBrowseFrame:insertHiddenField("countgrouplocationlink_browse_scroll","").
   CountGroupBrowseFrame:insertHiddenField("CountGroupID","").
   CountGroupBrowseFrame:insertHiddenField("CountGroupLocationLinkID",chrCountGroupLocationLinkID).
   CountGroupBrowseFrame:insertHiddenField("CountGroupRecountID",chrCountGroupRecountID).
   CountGroupBrowseFrame:insertHiddenField("popup_countgrouplocationlink_browse","").
   CountGroupBrowseFrame:insertHiddenField("popup_uploadcountgrouplocationlinks_form","").
   CountGroupBrowseFrame:insertHiddenField("popup_errors_browse_form","").
   CountGroupBrowseFrame:insertHiddenField("popup_addcountgrouplocationlink_browse","").
   CountGroupBrowseFrame:insertHiddenField("popup_countgrouprecount_browse","").
   CountGroupBrowseFrame:insertHiddenField("popup_countgrouphistory_browse","").
   CountGroupBrowseFrame:insertHiddenField("popup_file_browse","").
   CountGroupBrowseFrame:insertHiddenField("CountGroupVersionID","").
   CountGroupBrowseFrame:insertHiddenField("GenerateTasks","").
   CountGroupBrowseFrame:insertHiddenField("added_locationlinks_count","").
   CountGroupBrowseFrame:insertHiddenField("deactivated_locationlinks_count","").
   CountGroupBrowseFrame:insertHiddenField("LocationRef",chrSelectedLocationRef).
   CountGroupBrowseFrame:insertHiddenField("LocationRefBegins",chrLocationRefBegins).
   CountGroupBrowseFrame:insertHiddenField("LocationTypeID",STRING(intSelectedLocationTypeID)).
   CountGroupBrowseFrame:insertHiddenField("AisleID",STRING(intSelectedAisleID)).
   CountGroupBrowseFrame:insertHiddenField("WorkZoneID",STRING(intSelectedWorkZoneID)).
   CountGroupBrowseFrame:insertHiddenField("CanGenerateTasks","").
   CountGroupBrowseFrame:insertHiddenField("form_name","countgroup_browse_form").
   CountGroupBrowseFrame:insertHiddenField("prog_name","adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupBrowseFrame}
   
   CountGroupBrowseFrame:formClose().
   
   /* Create Button Bar */
   CountGroupBrowseButtons           = NEW buttonBar().
   CountGroupBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_locupload",
                                     fTL("Location Upload"),
                                     "viewLocationUploadDetails('locationfile_selection_form');",
                                     (IF intSelectedCountGroup > 0 THEN "" ELSE "Disabled")).
   
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_partupload",
                                     fTL("Part Upload"),
                                     "viewPartUploadDetails('partfile_selection_form');",
                                     (IF intSelectedCountGroup > 0 THEN "" ELSE "Disabled")).
   
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_create",
                                     fTL("Create"),
                                     "createCountGroup('countgroup_details_form');").
   
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_details",
                                     fTL("Details"),
                                     "viewCountGroupDetails('countgroup_details_form');",
                                     (IF intSelectedCountGroup > 0 THEN "" ELSE "Disabled")).
   
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_locations",
                                     fTL("Locations"),
                                     "viewCountGroupLocationLinks('countgroup_browse_form');",
                                     (IF intSelectedCountGroup > 0 THEN "" ELSE "Disabled")).
   
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_recounts",
                                     fTL("Recounts"),
                                     "viewCountGroupRecounts('countgroup_browse_form');",
                                     (IF intSelectedCountGroup > 0 THEN "" ELSE "Disabled")).
   
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_generatetasks",
                                     fTL("Generate Tasks"),
                                     "generateTasksOptions('countgroup_generatetasks_form'); ",
                                     "Disabled").
   /*
   CountGroupBrowseButtons:addButton("countgroup_browse_form_btn_history",
                                     fTL("Upload History"),
                                     "viewUploadHistory('countgroup_details_form');",
                                     (IF intSelectedCountGroup > 0 THEN "" ELSE "Disabled")).
   */
   CountGroupBrowseButtons:closeBar().  
   CountGroupBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupDetails Procedure 
PROCEDURE pCountGroupDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "countgroup_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountGroupID,CountGroupCode,CountGroupName,Active,ListingSequence,BusinessUnitID,Frequency,CountGroupTypeID,CountTaskTypeID"
          chrEditFieldList     = "CountGroupCode,CountGroupName,Active,ListingSequence,BusinessUnitID,Frequency,CountTaskTypeID"
          chrNewFieldList      = "CountGroupCode,CountGroupName,Active,ListingSequence,BusinessUnitID,Frequency,CountGroupTypeID,CountTaskTypeID"
          chrRequiredFieldList = "CountGroupCode,CountGroupName,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER".
   
   CountGroupDetailsForm           = NEW dataForm("countgroup_details_form").
   CountGroupDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountGroupDetailsForm:FormAction = "dbCountGroupUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountGroupDetailsForm:FormWidth  = 460.
   CountGroupDetailsForm:FormHeight = 300.
   CountGroupDetailsForm:FormTitle  = " Count Group Details".
   CountGroupDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountGroupDetailsForm:insertPaddingColumn(50).
   CountGroupDetailsForm:insertColumn(120).
   CountGroupDetailsForm:insertColumn(120).
   CountGroupDetailsForm:insertColumn(20).
   CountGroupDetailsForm:insertColumn(4).
   CountGroupDetailsForm:insertColumn(110).
   
   /* Fields */
   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Group ID").
   CountGroupDetailsForm:insertTextField("CountGroupID", "", 110, TRUE).  
   
   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Group Code").
   CountGroupDetailsForm:insertTextField("CountGroupCode", "", 110, TRUE).
   
   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Group Name").
   CountGroupDetailsForm:insertTextField("CountGroupName", "", 110, TRUE).
   
   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Business Unit").
   CountGroupDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).  
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK /* idx=ActiveListingSequence*/
      WHERE BusinessUnit.Active
      BY BusinessUnit.ListingSequence:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.
      
      CountGroupDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID), BusinessUnit.UnitName).
   END.

   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Count Group Type").
   CountGroupDetailsForm:insertComboField("CountGroupTypeID", "", 110, TRUE, "groupTypeValueChange(this.value)").
   
   FOR EACH CountGroupType NO-LOCK /* idx=ActiveListingSequence*/
      WHERE CountGroupType.ACTIVE
      BY CountGroupType.ListingSequence:
      
      CountGroupDetailsForm:insertComboPairs("CountGroupTypeID", STRING(CountGroupType.CountGroupTypeID), CountGroupType.TypeName).
   END.

   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Count Task Type").
   CountGroupDetailsForm:insertComboField("CountTaskTypeID", "", 110, TRUE).  
   FOR EACH CountTaskType NO-LOCK /* idx=ActiveListingSequence*/
      WHERE CountTaskType.ACTIVE
      BY CountTaskType.ListingSequence:
      
      CountGroupDetailsForm:insertComboPairs("CountTaskTypeID", STRING(CountTaskType.CountTaskTypeID), CountTaskType.TypeName).
   END.

   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Frequency").
   CountGroupDetailsForm:insertTextField("Frequency", "", 110, TRUE).  

   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel("Listing Seq").
   CountGroupDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   CountGroupDetailsForm:startRow().
   CountGroupDetailsForm:insertLabel(fTL("Active")). 
   CountGroupDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CountGroupDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountGroupDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCountGroupDetailsFields}
   
   /* Add Hidden Fields*/
   CountGroupDetailsForm:insertHiddenField("countgroup_browse_scroll", "").
   CountGroupDetailsForm:insertHiddenField("form_name", "countgroup_details_form").
   CountGroupDetailsForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupDetailsForm}
   
   /* Create Button Bar */
   CountGroupDetailsButtons = NEW buttonBar().
   
   CountGroupDetailsButtons:addButton("countgroup_details_form_btn_save", 
                                      fTL("Save"), 
                                      "updateCountGroup('countgroup_details_form');").
   
   CountGroupDetailsButtons:addButton("countgroup_details_form_btn_cancel", 
                                      fTL("Cancel"), 
                                      "cancelUpdate('UserCancelled','process_mode'); disablePopup('countgroup_details_form_popup');").
   
   CountGroupDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountGroupDetailsForm:FormButtons = CountGroupDetailsButtons.
   
   CountGroupDetailsForm:endForm(). 
   
   CountGroupDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupHistoryBrowse Procedure 
PROCEDURE pCountGroupHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   CountGroupHistoryBrowseForm           = NEW dataForm("countgrouphistory_browse_form").
   CountGroupHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CountGroupHistoryBrowseForm:FormWidth  = 860.
   CountGroupHistoryBrowseForm:FormHeight = 530.
   CountGroupHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE CountGroup THEN " for CountGroup: " 
                                          + STRING(CountGroup.CountGroupName) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ID:" 
                                          + STRING(CountGroup.CountGroupID) ELSE "").
   CountGroupHistoryBrowseForm:FormType   = "xxl_large".
   CountGroupHistoryBrowse                = NEW browseTable("countgrouphistory_browse").
   CountGroupHistoryBrowse:BrowseWidth    = 840.
   CountGroupHistoryBrowse:BrowseHeight   = 490.
   
   CountGroupHistoryBrowse:insertColumn(fTL("HistoryID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountGroupHistory}
   
   CountGroupHistoryBrowse:insertColumn(fTL("Group Code"),    80, "CHARACTER", "left", FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("Group Name"),   100, "CHARACTER", "left", FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("Group Type"),   100, "CHARACTER", "left", FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("Business Unit"),100, "CHARACTER", "left", FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("Active"),        50, "CHARACTER", "left", FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("ListSeq"),       50, "INTEGER",FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("User"),          80, "CHARACTER", "left", FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("Operation"),     80, "CHARACTER", "left", FALSE).
   CountGroupHistoryBrowse:insertColumn(fTL("Created"),      100, "CHARACTER", "left", FALSE).
   
   CountGroupHistoryBrowse:StartBody().
   
   IF AVAILABLE CountGroup THEN
   DO:
      FOR EACH CountGroupHistory NO-LOCK /* idx=CountGroupIDCreated */
         WHERE CountGroupHistory.CountGroupID = CountGroup.CountGroupID
         BY CountGroupHistory.Created DESCENDING
         BY CountGroupHistory.CountGroupID:
         
         FIND FIRST CountGroupType OF CountGroupHistory NO-LOCK NO-ERROR.
         FIND FIRST BusinessUnit   OF CountGroupHistory NO-LOCK NO-ERROR.
         FIND FIRST OperationType  OF CountGroupHistory NO-LOCK NO-ERROR.
         FIND FIRST GateUser       OF CountGroupHistory NO-LOCK NO-ERROR.
         
         CountGroupHistoryBrowse:startRow(CountGroupHistory.CountGroupHistoryID, 
                                           "selectBrowseRow(this," + '"countgrouphistory_browse"' + ");","").
         CountGroupHistoryBrowse:insertData(CountGroupHistory.CountGroupHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
         {webGetOptionalBrowseFields.i CountGroupHistory}
         
         CountGroupHistoryBrowse:insertData(CountGroupHistory.CountGroupCode, "left").
         CountGroupHistoryBrowse:insertData(CountGroupHistory.CountGroupName, "left").
         CountGroupHistoryBrowse:insertData((IF AVAILABLE CountGroupType THEN CountGroupType.TypeName ELSE ""), "left").
         CountGroupHistoryBrowse:insertData((IF AVAILABLE BusinessUnit THEN BusinessUnit.UnitName ELSE ""), "left").
         CountGroupHistoryBrowse:insertData(CountGroupHistory.Active, "left").
         CountGroupHistoryBrowse:insertData(STRING(CountGroupHistory.ListingSequence)).
         CountGroupHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         CountGroupHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
         CountGroupHistoryBrowse:insertData(fDisplayDate&Time(CountGroupHistory.Created,"y/m/d H:M:S"), "right").
         
         CountGroupHistoryBrowse:endRow().
      
      END. /* FOR EACH CountGroupHistory NO-LOCK */
   END. /*IF AVAILABLE CountGroup THEN*/
   
   CountGroupHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountGroupHistoryBrowse:getErrors().
   
   CountGroupHistoryBrowseForm:insertHiddenField("popup_countgrouphistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupHistoryBrowseForm}
   
   /* Create Button Bar */
   CountGroupHistoryBrowseButtons = NEW buttonBar().
   
   CountGroupHistoryBrowseButtons:addButton("countgrouphistory_browse_form_btn_cancel",
                                             fTL("Cancel"),
                                             "disablePopup('countgrouphistory_browse_form_popup');").
   
   CountGroupHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountGroupHistoryBrowseForm:FormBrowse  = CountGroupHistoryBrowse.
   CountGroupHistoryBrowseForm:FormButtons = CountGroupHistoryBrowseButtons.
   CountGroupHistoryBrowseForm:endForm(). 
   
   CountGroupHistoryBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupLocationLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupLocationLinkBrowse Procedure 
PROCEDURE pCountGroupLocationLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "countgrouplocationlink_details_form"}
   
   CountGroupLocationLinkBrowseForm            = NEW dataForm("countgrouplocationlink_browse_form").
   CountGroupLocationLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   CountGroupLocationLinkBrowseForm:FormAction = "dbSetCountGroupLocationLinkActive.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   CountGroupLocationLinkBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   CountGroupLocationLinkBrowseForm:FormWidth  = 860. 
   CountGroupLocationLinkBrowseForm:FormHeight = 530. 
   CountGroupLocationLinkBrowseForm:FormTitle  = fTL("Locations Linked to Count Group") + 
                                                      (IF AVAILABLE CountGroup THEN " : " + CountGroup.CountGroupName ELSE "").
   CountGroupLocationLinkBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountGroupLocationLinkBrowse              = NEW browseTable("countgrouplocationlink_browse").
   CountGroupLocationLinkBrowse:BrowseWidth  = 840.
   CountGroupLocationLinkBrowse:BrowseHeight = 490.
   
   /* Add in the ID as first Column */
   CountGroupLocationLinkBrowse:insertColumn(fTL("Link ID"),    120, "CHARACTER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountGroupLocationLink}
   
   CountGroupLocationLinkBrowse:insertColumn(fTL("LocationID"),     80, "INTEGER", FALSE).
   CountGroupLocationLinkBrowse:insertColumn(fTL("Location"),      150, "CHARACTER", "left", FALSE).
   CountGroupLocationLinkBrowse:insertColumn(fTL("Location Type"), 160, "CHARACTER", "left", FALSE).
   CountGroupLocationLinkBrowse:insertColumn(fTL("Count Seq"),     100, "INTEGER",   "left", FALSE).
   CountGroupLocationLinkBrowse:insertColumn(fTL("Last Counted"),  100, "CHARACTER", "left", FALSE).
   CountGroupLocationLinkBrowse:insertColumn(fTL("Active"),         80, "LOGICAL", FALSE).
   
   /*Body*/
   CountGroupLocationLinkBrowse:StartBody().
   CountGroupLocationLinkLoop:
   FOR EACH CountGroupLocationLink NO-LOCK
      WHERE CountGroupLocationLink.CountGroupID = CountGroup.CountGroupID,
      EACH Location OF CountGroupLocationLink NO-LOCK,
      EACH LocationType OF Location NO-LOCK:

      IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
         NEXT CountGroupLocationLinkLoop.
      
      FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.
      
      IF AVAILABLE Aisle THEN
         FIND FIRST WorkZone OF Aisle NO-LOCK NO-ERROR.

      CountGroupLocationLinkBrowse:startRow(CountGroupLocationLink.CountGroupLocationLinkID, "selectCountGroupLocationLinkRow(this," + '"' 
                                                                     + STRING(CountGroupLocationLink.CountGroupLocationLinkID) + '"' + ");", "").
      CountGroupLocationLinkBrowse:insertData(CountGroupLocationLink.CountGroupLocationLinkID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountGroupLocationLink}
      
      CountGroupLocationLinkBrowse:insertData(Location.LocationID).
      CountGroupLocationLinkBrowse:insertData(STRING(Location.LocationRef),"left").
      CountGroupLocationLinkBrowse:insertData(STRING(LocationType.TypeDescr),"left").
      CountGroupLocationLinkBrowse:insertData(STRING(Location.CountSequence)).
      CountGroupLocationLinkBrowse:insertData("").
      CountGroupLocationLinkBrowse:insertData(STRING(CountGroupLocationLink.Active, "Yes/No")).
      
      /* Add hidden fields */
      CountGroupLocationLinkBrowse:insertHiddenData("CountGroupVersionID",CountGroup.VersionID).
      CountGroupLocationLinkBrowse:insertHiddenData("CountGroupLocationLinkVersionID",CountGroupLocationLink.VersionID).
      CountGroupLocationLinkBrowse:insertHiddenData("CountGroupLocationLinkID",STRING(CountGroupLocationLink.CountGroupLocationLinkID)).
      CountGroupLocationLinkBrowse:insertHiddenData("LocationID",STRING(Location.LocationID)).
      CountGroupLocationLinkBrowse:insertHiddenData("LocationVersionID",STRING(Location.VersionID)).
      CountGroupLocationLinkBrowse:insertHiddenData("LinkActive",STRING(CountGroupLocationLink.ACTIVE)).

      CountGroupLocationLinkBrowse:endRow().
      
   END. /* FOR EACH CountGroup NO-LOCK */
   
   CountGroupLocationLinkBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + CountGroupLocationLinkBrowse:getErrors().
   
   /* Hidden Fields */
   
   CountGroupLocationLinkBrowseForm:insertHiddenField("CountGroupID",chrCountGroupID).
   CountGroupLocationLinkBrowseForm:insertHiddenField("CountGroupLocationLinkID","").
   CountGroupLocationLinkBrowseForm:insertHiddenField("CountGroupLocationLinkVersionID","").
   CountGroupLocationLinkBrowseForm:insertHiddenField("countgroup_browse_scroll","").
   CountGroupLocationLinkBrowseForm:insertHiddenField("countgrouplocationlink_browse_scroll","").
   CountGroupLocationLinkBrowseForm:insertHiddenField("popup_countgrouplocationlink_browse","").
   CountGroupLocationLinkBrowseForm:insertHiddenField("popup_addcountgrouplocationlink_browse","").
   CountGroupLocationLinkBrowseForm:insertHiddenField("filtering","yes").
   CountGroupLocationLinkBrowseForm:insertHiddenField("LocationRef",chrSelectedLocationRef).
   CountGroupLocationLinkBrowseForm:insertHiddenField("LocationRefBegins",chrLocationRefBegins).
   CountGroupLocationLinkBrowseForm:insertHiddenField("LocationTypeID",STRING(intSelectedLocationTypeID)).
   CountGroupLocationLinkBrowseForm:insertHiddenField("AisleID",STRING(intSelectedAisleID)).
   CountGroupLocationLinkBrowseForm:insertHiddenField("WorkZoneID",STRING(intSelectedWorkZoneID)).
   CountGroupLocationLinkBrowseForm:insertHiddenField("CountGroupLocationLinkActive","").
   CountGroupLocationLinkBrowseForm:insertHiddenField("form_name","countgrouplocationlink_browse_form").
   CountGroupLocationLinkBrowseForm:insertHiddenField("prog_name","adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupLocationLinkBrowseForm}
   
   /* Create Button Bar */
   CountGroupLocationLinkBrowseButtons = NEW buttonBar().
   
   CountGroupLocationLinkBrowseButtons:addButton("countgrouplocationlink_browse_form_btn_create",
                                                 fTL("Add Location Links"),
                                                 "addCountGroupLocationLink('countgrouplocationlink_details_form');").
   
   /**
   CountGroupLocationLinkBrowseButtons:addButton("countgrouplocationlink_browse_form_btn_remove",
                                                 fTL("Remove"),
                                                 "removeCountGroup('countgroup_details_form');",
                                                 (IF intSelectedCountGroupLocationLink > 0 THEN "" ELSE "Disabled")).
   **/

   CountGroupLocationLinkBrowseButtons:addButton("countgrouplocationlink_browse_form_btn_deactivatelink",
                                                 fTL("Deactivate Link"),
                                                 "setCountGroupLocationLinkActiveFlag('countgrouplocationlink_browse_form');",
                                                 (IF intSelectedCountGroupLocationLink > 0 THEN "" ELSE "Disabled")).
   
   CountGroupLocationLinkBrowseButtons:addButton("countgrouplocationlink_browse_form_btn_cancel",
                                                 fTL("Cancel"),
                                                 "disablePopup('countgrouplocationlink_browse_form_popup');").

   CountGroupLocationLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountGroupLocationLinkBrowseForm:FormBrowse  = CountGroupLocationLinkBrowse.
   CountGroupLocationLinkBrowseForm:FormButtons = CountGroupLocationLinkBrowseButtons.
   CountGroupLocationLinkBrowseForm:endForm(). 
   
   CountGroupLocationLinkBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupRecountBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupRecountBrowse Procedure 
PROCEDURE pCountGroupRecountBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "countgrouprecount_details_form"}
   
   CountGroupRecountBrowseForm            = NEW dataForm("countgrouprecount_browse_form").
   CountGroupRecountBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   CountGroupRecountBrowseForm:FormAction = "dbCountGroupRecountUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   CountGroupRecountBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   CountGroupRecountBrowseForm:FormWidth  = 860. 
   CountGroupRecountBrowseForm:FormHeight = 530. 
   CountGroupRecountBrowseForm:FormTitle  = fTL("Count Group Recounts Linked to Count Group") + 
                                                      (IF AVAILABLE CountGroup THEN " : " + CountGroup.CountGroupName ELSE "").
   CountGroupRecountBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountGroupRecountBrowse              = NEW browseTable("countgrouprecount_browse").
   CountGroupRecountBrowse:BrowseWidth  = 840.
   CountGroupRecountBrowse:BrowseHeight = 490.
   
   /* Add in the ID as first Column */
   CountGroupRecountBrowse:insertColumn(fTL("Recount ID"),                        90, "CHARACTER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountGroupRecount}

   CountGroupRecountBrowse:insertColumn(fTL("Recount Seq"),                       90, "INTEGER", "left", FALSE).
   CountGroupRecountBrowse:insertColumn(fTL("Group Name"),                       140, "CHARACTER", "left", FALSE).
   CountGroupRecountBrowse:insertColumn(fTL("Task Type"),                        120, "CHARACTER", "left", FALSE).
   CountGroupRecountBrowse:insertColumn(fTL("Priority"),                          80, "INTEGER", "left", FALSE).
   CountGroupRecountBrowse:insertColumn(fTL("Active"),                            80, "LOGICAL", FALSE).
   CountGroupRecountBrowse:insertColumn(fTL("Allow Recount By Original Counter"),150, "LOGICAL", FALSE).

   /*Body*/
   CountGroupRecountBrowse:StartBody().
   
   IF AVAILABLE CountGroup THEN
   FOR EACH CountGroupRecount NO-LOCK
      WHERE CountGroupRecount.CountGroupID = CountGroup.CountGroupID,
      EACH CountTaskType OF CountGroupRecount
      BY CountGroupRecount.Active DESCENDING
      BY CountGroupRecount.CountSequence:
      
      CountGroupRecountBrowse:startRow(CountGroupRecount.CountGroupRecountID, "selectCountGroupRecountRow(this," + '"' 
                                                                     + STRING(CountGroupRecount.CountGroupRecountID) + '"' + ");", "").
      CountGroupRecountBrowse:insertData(CountGroupRecount.CountGroupRecountID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountGroupRecount}
      
      CountGroupRecountBrowse:insertData(STRING(CountGroupRecount.CountSequence)).
      CountGroupRecountBrowse:insertData(STRING(CountGroup.CountGroupName),"left").
      CountGroupRecountBrowse:insertData(STRING(CountTaskType.TypeName),"left").
      CountGroupRecountBrowse:insertData(STRING(CountGroupRecount.Priority)).
      CountGroupRecountBrowse:insertData(STRING(CountGroupRecount.Active, "Yes/No")).
      CountGroupRecountBrowse:insertData(STRING(CountGroupRecount.AllowRecountByOriginalCounter, "Yes/No")).

      /* Add hidden fields */
      CountGroupRecountBrowse:insertHiddenData("CountGroupVersionID",CountGroup.VersionID).
      CountGroupRecountBrowse:insertHiddenData("CountGroupRecountVersionID",CountGroupRecount.VersionID).
      
      CountGroupRecountBrowse:endRow().
      
   END. /* FOR EACH CountGroup NO-LOCK */
   
   CountGroupRecountBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountGroupRecountBrowse:getErrors().
   
   /* Hidden Fields */
   
   CountGroupRecountBrowseForm:insertHiddenField("CountGroupID",chrCountGroupID).
   CountGroupRecountBrowseForm:insertHiddenField("CountGroupRecountID","").
   CountGroupRecountBrowseForm:insertHiddenField("CountGroupRecountVersionID","").
   CountGroupRecountBrowseForm:insertHiddenField("countgroup_browse_scroll","").
   CountGroupRecountBrowseForm:insertHiddenField("countgrouprecount_browse_scroll","").
   CountGroupRecountBrowseForm:insertHiddenField("popup_countgrouprecount_browse","").
   CountGroupRecountBrowseForm:insertHiddenField("popup_addcountgrouprecount_browse","").
   CountGroupRecountBrowseForm:insertHiddenField("form_name","countgroup_browse_form").
   CountGroupRecountBrowseForm:insertHiddenField("prog_name","adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupRecountBrowseForm}
   
   /* Create Button Bar */
   CountGroupRecountBrowseButtons = NEW buttonBar().
   
   CountGroupRecountBrowseButtons:addButton("countgrouprecount_browse_form_btn_create",
                                            fTL("Create"),
                                            "createCountGroupRecount('countgrouprecount_details_form');").

   CountGroupRecountBrowseButtons:addButton("countgrouprecount_browse_form_btn_details",
                                            fTL("Details"),
                                            "viewCountGroupRecountDetails('countgrouprecount_details_form');",
                                            (IF intSelectedCountGroupRecount > 0 THEN "" ELSE "Disabled")).

/*    CountGroupRecountBrowseButtons:addButton("countgrouprecount_browse_form_btn_delete",                          */
/*                                             fTL("Delete"),                                                       */
/*                                             "confirmDeleteCountGroupRecount('countgrouprecount_details_form');", */
/*                                             (IF intSelectedCountGroupRecount > 0 THEN "" ELSE "Disabled")).      */
   
   CountGroupRecountBrowseButtons:addButton("countgrouprecount_browse_form_btn_cancel",
                                            fTL("Cancel"),
                                            "disablePopup('countgrouprecount_browse_form_popup');").

   CountGroupRecountBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountGroupRecountBrowseForm:FormBrowse  = CountGroupRecountBrowse.
   CountGroupRecountBrowseForm:FormButtons = CountGroupRecountBrowseButtons.
   CountGroupRecountBrowseForm:endForm(). 
   
   CountGroupRecountBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountGroupRecountDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountGroupRecountDetails Procedure 
PROCEDURE pCountGroupRecountDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "countgrouprecount_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountGroupRecountID,CountGroupID,CountTaskTypeID,CountSequence,Priority,Active,AllowRecountByOriginalCounter"
          chrEditFieldList     = "CountSequence,Active,CountTaskTypeID,Priority,AllowRecountByOriginalCounter"
          chrNewFieldList      = "CountSequence,Active,CountTaskTypeID,Priority,AllowRecountByOriginalCounter"
          chrRequiredFieldList = "CountSequence,Priority"
          chrExtraFieldList    = ""
          chrValidateFieldList = "CountSequence:INTEGER".
   
   CountGroupRecountDetailsForm           = NEW dataForm("countgrouprecount_details_form").
   CountGroupRecountDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountGroupRecountDetailsForm:FormAction  = "dbCountGroupRecountUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountGroupRecountDetailsForm:FormWidth  = 460.
   CountGroupRecountDetailsForm:FormHeight = 300.
   CountGroupRecountDetailsForm:FormTitle  = "Count Group Recount Details".
   CountGroupRecountDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountGroupRecountDetailsForm:insertPaddingColumn(20).
   CountGroupRecountDetailsForm:insertColumn(200).
   CountGroupRecountDetailsForm:insertColumn(120).
   CountGroupRecountDetailsForm:insertColumn(30).
   CountGroupRecountDetailsForm:insertColumn(120).  
   
   /* Fields */
   CountGroupRecountDetailsForm:startRow().
   CountGroupRecountDetailsForm:insertLabel(fTL("CountGroupRecount ID")).
   CountGroupRecountDetailsForm:insertTextField("CountGroupRecountID", "", 110, TRUE).  

   CountGroupRecountDetailsForm:startRow().
   CountGroupRecountDetailsForm:insertLabel(fTL("Count Sequence")).
   CountGroupRecountDetailsForm:insertTextField("CountSequence", "", 110, TRUE).  
   
   CountGroupRecountDetailsForm:startRow().
   CountGroupRecountDetailsForm:insertLabel(fTL("CountGroup")).
   CountGroupRecountDetailsForm:insertComboField("CountGroupID",IF AVAILABLE CountGroup THEN STRING(CountGroup.CountGroupID) ELSE "", 200, TRUE).  
   DisplayCountGroupLoop:
   FOR EACH displayCountGroup NO-LOCK 
      BY displayCountGroup.Active DESC
      BY displayCountGroup.CountGroupName:

      IF NOT fCanViewBusinessUnit(intGblSessionID,displayCountGroup.BusinessUnitID) THEN 
         NEXT DisplayCountGroupLoop.
      
      CountGroupRecountDetailsForm:insertComboPairs("CountGroupID", STRING(displayCountGroup.CountGroupID), displayCountGroup.CountGroupName).
   END.   
   
   CountGroupRecountDetailsForm:startRow().
   CountGroupRecountDetailsForm:insertLabel(fTL("CountTaskType")).
   CountGroupRecountDetailsForm:insertComboField("CountTaskTypeID", IF AVAILABLE CountGroup THEN STRING(CountGroup.CountTaskTypeID) ELSE "", 200, TRUE).  
   FOR EACH CountTaskType NO-LOCK 
      BY CountTaskType.Active DESC
      BY CountTaskType.TypeName:
      
      CountGroupRecountDetailsForm:insertComboPairs("CountTaskTypeID", STRING(CountTaskType.CountTaskTypeID), CountTaskType.TypeName).
   END.  

   CountGroupRecountDetailsForm:startRow().
   CountGroupRecountDetailsForm:insertLabel(fTL("Priority")).
   CountGroupRecountDetailsForm:insertTextField("Priority", "", 110, TRUE).  
   
   CountGroupRecountDetailsForm:startRow().
   CountGroupRecountDetailsForm:insertLabel(fTL("Active")). 
   CountGroupRecountDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CountGroupRecountDetailsForm:insertComboPairs("Active", "yes", "Active").
   CountGroupRecountDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   CountGroupRecountDetailsForm:startRow().
   CountGroupRecountDetailsForm:insertLabel(fTL("Allow Recount by Original Counter")). 
   CountGroupRecountDetailsForm:insertComboField("AllowRecountByOriginalCounter", "", 110, TRUE).  
   CountGroupRecountDetailsForm:insertComboPairs("AllowRecountByOriginalCounter", "yes", "Allow").
   CountGroupRecountDetailsForm:insertComboPairs("AllowRecountByOriginalCounter", "no",  "Do not Allow").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pCountGroupRecountDetailsFields}
   
   /* Add Hidden Fields*/
   CountGroupRecountDetailsForm:insertHiddenField("countgroup_browse_scroll","").
   CountGroupRecountDetailsForm:insertHiddenField("popup_countgrouprecount_browse","").
   CountGroupRecountDetailsForm:insertHiddenField("CountGroupID",STRING(intSelectedCountGroup)).
   CountGroupRecountDetailsForm:insertHiddenField("CountGroupRecountID",STRING(intSelectedCountGroupRecount)).
   CountGroupRecountDetailsForm:insertHiddenField("form_name","countgrouprecount_details_form").
   CountGroupRecountDetailsForm:insertHiddenField("prog_name","adCountGroupAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupRecountDetailsForm}
   
   /* Create Button Bar */
   CountGroupRecountDetailsButtons = NEW buttonBar().
   
   CountGroupRecountDetailsButtons:addButton("countgrouprecount_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateCountGroupRecount('countgrouprecount_details_form');").
   
   CountGroupRecountDetailsButtons:addButton("countgrouprecount_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); " 
                                             + "disablePopup('countgrouprecount_details_form_popup');").
   CountGroupRecountDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CountGroupRecountDetailsForm:FormButtons = CountGroupRecountDetailsButtons.
   
   CountGroupRecountDetailsForm:endForm(). 
   CountGroupRecountDetailsForm:displayForm(). 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pErrorBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pErrorBrowse Procedure 
PROCEDURE pErrorBrowse :
/*------------------------------------------------------------------------------
  Purpose:   Build a browse to show the validation errors form the upload file 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "uploadcountgrouplocationlinks_details_form"}
   
   ErrorsBrowseForm             = NEW dataForm("errors_browse_form").
   ErrorsBrowseForm:WebStream   = STREAM WebStream:HANDLE.
   ErrorsBrowseForm:FormAction  = "adCountGroupAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ErrorsBrowseForm:FormWidth  = 700.
   ErrorsBrowseForm:FormHeight = 490.
   ErrorsBrowseForm:FormTitle  = fTL("CountGroupLocationLink File Upload Errors").
   ErrorsBrowseForm:FormType   = "xl_large".
   
   ErrorsBrowse              = NEW browseTable("errors_browse").
   ErrorsBrowse:BrowseWidth  = 680.
   ErrorsBrowse:BrowseHeight = 450.
   
   ErrorsBrowse:insertColumn("Line No", 90, "CHARACTER", "left", FALSE).
   ErrorsBrowse:insertColumn("Column",  90, "CHARACTER", "left", FALSE).
   ErrorsBrowse:insertColumn("Error",  100, "CHARACTER", "left", FALSE).
   
   /*Body*/
   ErrorsBrowse:startBody().
   
   IF chrValidFile = "NO" THEN
   FOR EACH ttFileLine NO-LOCK:
      
      ErrorsBrowse:startRow(ttFileLine.FileLineNo, "", "").
      
      chrColumnValue = ENTRY(1,ttFileLine.LineString, ",").
      ErrorsBrowse:insertData(chrColumnValue, "left").
         
      chrColumnValue = ENTRY(2,ttFileLine.LineString, ",").
      ErrorsBrowse:insertData(chrColumnValue, "left").
      
      chrColumnValue = ENTRY(3,ttFileLine.LineString, ",").
      ErrorsBrowse:insertData(chrColumnValue, "left").
      
      /* Add hidden fields */
      ErrorsBrowse:insertHiddenData("FileLineNo",ttFileLine.FileLineNo).
      
      ErrorsBrowse:endRow().
      
   END. /* FOR EACH ttFileLine NO-LOCK */
   
   ErrorsBrowse:endTable().
   
   ErrorsBrowseForm:insertHiddenField("errors_browse_scroll","").
   ErrorsBrowseForm:insertHiddenField("ErrorsID","").
   ErrorsBrowseForm:insertHiddenField("ErrorsVersionID","").
   ErrorsBrowseForm:insertHiddenField("UploadFile",chrFileName).
   ErrorsBrowseForm:insertHiddenField("popup_errors_browse_form","").
   ErrorsBrowseForm:insertHiddenField("form_name", "errors_browse_form").
   ErrorsBrowseForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ErrorsBrowseForm}
   
   /* Create Button Bar */
   ErrorsBrowseButtons = NEW buttonBar().
   
   ErrorsBrowseButtons:addButton("errors_browse_form_btn_cancel",
                                 fTL("Close"),
                                 "cancelUpdate('UserCancelled','process_mode'); disablePopup('errors_browse_form_popup');").
   
   ErrorsBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   ErrorsBrowseForm:FormBrowse  = ErrorsBrowse.
   ErrorsBrowseForm:FormButtons = ErrorsBrowseButtons.
   ErrorsBrowseForm:endForm().
   ErrorsBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileBrowse Procedure 
PROCEDURE pFileBrowse :
/*------------------------------------------------------------------------------
  Purpose:   Build a browse to show the File for BulkFileUpload 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* build a temp table with all the Uploaded File Ids that pertain to the CountGroup selected */
   FOR EACH CountGroupLocationLink NO-LOCK
      WHERE CountGroupLocationLink.CountGroupID = intSelectedCountGroup
      AND CountGroupLocationLink.FileID <> 0:
      
      FIND FIRST ttFile NO-LOCK
         WHERE ttFile.FileID = CountGroupLocationLink.FileID NO-ERROR.
      IF NOT AVAILABLE ttFile THEN
      DO:
         CREATE ttFile.
         ASSIGN ttFile.FileID = CountGroupLocationLink.FileID.
      END.
   END. /* FOR EACH CountGroupLocationLink */
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   FileBrowseForm           = NEW dataForm("file_browse_form").
   FileBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   FileBrowseForm:FormWidth  = 860.
   FileBrowseForm:FormHeight = 530.
   FileBrowseForm:FormTitle  = fTL("Upload History") + (IF AVAILABLE CountGroup THEN " for CountGroup: " 
                                          + STRING(CountGroup.CountGroupName) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ID:" 
                                          + STRING(CountGroup.CountGroupID) ELSE "").
   FileBrowseForm:FormType   = "xxl_large".
   FileBrowse                = NEW browseTable("file_browse").
   FileBrowse:BrowseWidth    = 840.
   FileBrowse:BrowseHeight   = 490.
   
   
   /* Add in the ID as first Column */
   FileBrowse:insertColumn(fTL("File ID"),      70, "INTEGER",   "left", FALSE).
   FileBrowse:insertColumn(fTL("File Name"),   230, "CHARACTER", "left", FALSE).
   FileBrowse:insertColumn(fTL("Status"),      100, "CHARACTER", "left", FALSE).
   FileBrowse:insertColumn(fTL("Uploaded By"), 140, "CHARACTER", "left", FALSE).
   FileBrowse:insertColumn(fTL("Uploaded"),    150, "CHARACTER", "left", FALSE).
   FileBrowse:insertColumn(fTL("View File"),    80, "CHARACTER", "left", FALSE).
   
   /*Body*/
   FileBrowse:startBody().
   
   FOR EACH File NO-LOCK 
      WHERE File.FileMasterID = intLocationFileMasterID 
      OR    File.FileMasterID = intPartFileMasterID
      BY    File.FileID: 
      
      FIND FIRST ttFile NO-LOCK 
         WHERE ttFile.FileID = File.FileID NO-ERROR.
      
      IF NOT AVAILABLE ttFile THEN NEXT.
      
      FileBrowse:startRow(File.FileID, "selectFileRow(this," + '"' + STRING(File.FileID) + '"' + ");", "").
      
      FileBrowse:insertData(File.FileID, "left").
      FileBrowse:insertData(File.FileName, "left").
      
      FIND FIRST FileStatus NO-LOCK
         WHERE FileStatus.FileStatusID = File.FileStatusID NO-ERROR.
      IF AVAILABLE FileStatus THEN
         FileBrowse:insertData(FileStatus.StatusCode, "left").
      ELSE 
         FileBrowse:insertData(File.FileStatusID, "left").
      
      FIND FIRST GateUser NO-LOCK
         WHERE GateUser.GateUserID = File.GateUserID NO-ERROR.
      
      IF AVAILABLE GateUser THEN 
         FileBrowse:insertData(GateUser.FullName, "left").
      ELSE 
         FileBrowse:insertData(File.GateUserID, "left").
      
      FileBrowse:insertData(fDisplayDate&Time(File.Completed,"d/m/y H:M"), "left").
      
      FileBrowse:insertLink("download", 
                            "downloadLocationFile(" + '"' + REPLACE(get-config("fileUploadDirectory"), "/uploads/", "") + '/csv/intray/' + File.FileName + '"' + ")").
      
      /* Add hidden fields */
      FileBrowse:insertHiddenData("FileID",File.FileID).
      FileBrowse:insertHiddenData("FileVersionID",File.VersionID).
      
      FileBrowse:endRow().
      
   END. /*FOR EACH File NO-LOCK */
   
   FileBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + FileBrowse:getErrors().
   
   FileBrowseForm:insertHiddenField("file_browse_scroll","").
   FileBrowseForm:insertHiddenField("FileID","").
   FileBrowseForm:insertHiddenField("FileVersionID","").
   FileBrowseForm:insertHiddenField("form_name", "file_browse_form").
   FileBrowseForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileBrowseForm}
   
   /* Create Button Bar */
   FileBrowseButtons = NEW buttonBar().
   FileBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   FileBrowseButtons:addButton("file_browse_form_btn_details",
                               fTL("Details"),
                               "viewFileDetails('file_details_form');",
                               (IF intSelectedFile > 0 THEN "" ELSE "Disabled")).
   
   FileBrowseButtons:addButton("file_browse_form_btn_cancel",
                               fTL("Cancel"),
                               "disablePopup('file_browse_form_popup');").
   FileBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   FileBrowseForm:FormBrowse  = FileBrowse.
   FileBrowseForm:FormButtons = FileBrowseButtons.
   FileBrowseForm:endForm().
   
   FileBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileDetails Procedure 
PROCEDURE pFileDetails :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "file_details_form"}
   
   ASSIGN chrDisplayFieldList  = "FileID,Completed,Created,CustomerRef,FileMasterID,FileName,FilePath,BusinessUnitID,FileStatusID,FileTypeID,GateUserID" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   FileDetailsForm = NEW dataForm("file_details_form").
   FileDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileDetailsForm:FormAction  = "adCountGroupAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileDetailsForm:FormWidth   = 460.
   FileDetailsForm:FormHeight  = 300.
   FileDetailsForm:FormTitle   = "File Details".
   FileDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   FileDetailsForm:insertPaddingColumn(30).
   FileDetailsForm:insertColumn(140).
   FileDetailsForm:insertColumn(140).
   
   /* Fields */
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("FileID").
   FileDetailsForm:insertTextField("FileID", "", 150, TRUE).  
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("Completed").
   FileDetailsForm:insertDateField("Completed", "", 150, TRUE).  
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("Created").
   FileDetailsForm:insertDateField("Created", "", 150, TRUE).  
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("Customer Ref").
   FileDetailsForm:insertTextField("CustomerRef", "", 150, TRUE).  
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("FileMasterID").
   FileDetailsForm:insertComboField("FileMasterID", "", 150, TRUE).  
   
   FOR EACH FileMaster NO-LOCK:      
      FileDetailsForm:insertComboPairs("FileMasterID", STRING(FileMaster.FileMasterID), FileMaster.MasterName).
   END.
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("FileName").
   FileDetailsForm:insertTextField("FileName", "", 300, TRUE).  
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("FilePath").
   FileDetailsForm:insertTextField("FilePath", "", 300, TRUE).  
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("BusinessUnitID").
   FileDetailsForm:insertComboField("BusinessUnitID", "", 150, TRUE).  
   
   FOR EACH BusinessUnit NO-LOCK:      
      FileDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID), BusinessUnit.UnitName).
   END.
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("File Status").
   FileDetailsForm:insertComboField("FileStatusID", "", 150, TRUE).  
   
   FOR EACH FileStatus NO-LOCK:      
      FileDetailsForm:insertComboPairs("FileStatusID", STRING(FileStatus.FileStatusID), FileStatus.StatusCode).
   END.
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("FileTypeID").
   FileDetailsForm:insertComboField("FileTypeID", "", 150, TRUE).  
   
   FOR EACH FileType NO-LOCK:      
      FileDetailsForm:insertComboPairs("FileTypeID", STRING(FileType.FileTypeID), FileType.TypeCode).
   END.
   
   FileDetailsForm:startRow().
   FileDetailsForm:insertLabel("GateUserID").
   FileDetailsForm:insertComboField("GateUserID", "", 150, TRUE).  
   
   FOR EACH GateUser NO-LOCK:      
      FileDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pFileDetailsFields}
   
   /* Add Hidden Fields*/
   /* FileDetailsForm:insertHiddenField("file_browse_scroll", ""). */
   FileDetailsForm:insertHiddenField("form_name", "file_details_form").
   FileDetailsForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileDetailsForm}
   
   /* Create Button Bar */
   FileDetailsButtons = NEW buttonBar().
   
   FileDetailsButtons:addButton("file_details_form_btn_cancel", 
                                fTL("OK"), 
                                "cancelUpdate('UserCancelled','process_mode'); disablePopup('file_details_form_popup');").
   
   FileDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileDetailsForm:FormButtons = FileDetailsButtons.
   
   FileDetailsForm:endForm(). 
   
   FileDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGenerateCountTasks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateCountTasks Procedure 
PROCEDURE pGenerateCountTasks :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "countgroup_generatetasks_form"}
      
   ASSIGN
      chrDisplayFieldList  = "BlindCount,Priority"
      chrRequiredFieldList = "BlindCount,Priority"
      chrValidateFieldList = "Priority:INTEGER".
   
   CountGroupGenerateTasksForm           = NEW dataForm("countgroup_generatetasks_form").
   CountGroupGenerateTasksForm:WebStream = STREAM WebStream:HANDLE.
   
   CountGroupGenerateTasksForm:FormAction = "dbCountGroupGenerateTasks.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountGroupGenerateTasksForm:FormWidth  = 460.
   CountGroupGenerateTasksForm:FormHeight = 300.
   CountGroupGenerateTasksForm:FormTitle  = "Count Group Generate Tasks".
   CountGroupGenerateTasksForm:FormType   = "medium".
   
   /* Column Layout */
   CountGroupGenerateTasksForm:insertPaddingColumn(20).
   CountGroupGenerateTasksForm:insertColumn(120).
   CountGroupGenerateTasksForm:insertColumn(120).
   
   /* Fields */
   CountGroupGenerateTasksForm:startRow().
   CountGroupGenerateTasksForm:insertLabel(fTL("Blind Count")). 
   CountGroupGenerateTasksForm:insertComboField("BlindCount", "", 110, TRUE).  
   CountGroupGenerateTasksForm:insertComboPairs("BlindCount", "no",  "No").
   CountGroupGenerateTasksForm:insertComboPairs("BlindCount", "yes", "Yes").

   CountGroupGenerateTasksForm:startRow().
   CountGroupGenerateTasksForm:insertLabel("Priority").
   CountGroupGenerateTasksForm:insertTextField("Priority", "0", 110, TRUE).  
   
   CountGroupGenerateTasksForm:startRow().
   CountGroupGenerateTasksForm:insertLabel("Assigned To").
     
   CountGroupUserBrowse              = NEW browseTable("countgroupuser_browse").
   CountGroupUserBrowse:BrowseWidth  = 435.
   CountGroupUserBrowse:BrowseHeight = 190.
   CountGroupUserBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountGroupUserBrowse:insertColumn(fTL("UserID"),       60, "CHARACTER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountGroup}
   
   CountGroupUserBrowse:insertColumn(fTL("User Name"),   150, "CHARACTER", "left", FALSE).
   
   /*Body*/
   CountGroupUserBrowse:startBody().

   FIND FIRST Environment NO-LOCK
      WHERE Environment.EnvironmentCode = chrGblEnvironment NO-ERROR.
   IF AVAILABLE Environment THEN
   DO:

      chrUserIDList = fGetUsersForApplication(UserSession.ApplicationID,Environment.EnvironmentID).

      DO intLoop = 1 TO NUM-ENTRIES(chrUserIDList):
         FIND FIRST GateUser NO-LOCK 
            WHERE GateUser.GateUserID = INTEGER(ENTRY(intLoop,chrUserIDList)) NO-ERROR.
         IF AVAILABLE GateUser THEN
         DO:
            CountGroupUserBrowse:startRow(GateUser.GateUserID, "multiSelectCountGroupUserRow(" + '"' + STRING(GateUser.GateUserID) + '"' + ");", "").
            
            CountGroupUserBrowse:insertData(GateUser.GateUserID).
            CountGroupUserBrowse:insertData(GateUser.FullName, "left").

            CountGroupUserBrowse:endRow().
         END.
      END.

   END. /* AVAILABLE Environment */

   CountGroupUserBrowse:endTable().
    
   chrPageBuildError = chrPageBuildError + CountGroupUserBrowse:getErrors().

   CountGroupGenerateTasksForm:startRow().
     
   {webGetOptionalFormFields.i pCountGroupGenerateTasksFields}
   
   /* Add Hidden Fields*/
   CountGroupGenerateTasksForm:insertHiddenField("countgroup_browse_scroll", "").
   CountGroupGenerateTasksForm:insertHiddenField("count_browse_scroll","").
   CountGroupGenerateTasksForm:insertHiddenField("countgrouplocationlink_browse_scroll","").
   CountGroupGenerateTasksForm:insertHiddenField("CountGroupID",chrCountGroupID).
   CountGroupGenerateTasksForm:insertHiddenField("CountGroupLocationLinkID",chrCountGroupLocationLinkID).
   CountGroupGenerateTasksForm:insertHiddenField("popup_countgrouplocationlink_browse","").
   CountGroupGenerateTasksForm:insertHiddenField("popup_addcountgrouplocationlink_browse","").
   CountGroupGenerateTasksForm:insertHiddenField("CountGroupVersionID","").
   CountGroupGenerateTasksForm:insertHiddenField("LocationRef",chrSelectedLocationRef).
   CountGroupGenerateTasksForm:insertHiddenField("LocationRefBegins",chrLocationRefBegins).
   CountGroupGenerateTasksForm:insertHiddenField("LocationTypeID",STRING(intSelectedLocationTypeID)).
   CountGroupGenerateTasksForm:insertHiddenField("AisleID",STRING(intSelectedAisleID)).
   CountGroupGenerateTasksForm:insertHiddenField("WorkZoneID",STRING(intSelectedWorkZoneID)).
   CountGroupGenerateTasksForm:insertHiddenField("AssignedTo", "").
   CountGroupGenerateTasksForm:insertHiddenField("form_name", "countgroup_generatetasks_form").
   CountGroupGenerateTasksForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountGroupGenerateTasksForm}

   /* User Multiselection Browse here */
   
   /* Create Button Bar */
   CountGroupGenerateTasksButtons = NEW buttonBar().
   
   CountGroupGenerateTasksButtons:addButton("countgroup_generatetasks_form_btn_generatetasks", 
                                            fTL("Generate Tasks"), 
                                            "saveGenerateTasks('countgroup_generatetasks_form'); ").
   
   CountGroupGenerateTasksButtons:addButton("countgroup_generatetasks_form_btn_clear", 
                                            fTL("Clear Selection"), 
                                            "deselectCountGroupUserSelection();").

   CountGroupGenerateTasksButtons:addButton("countgroup_generatetasks_form_btn_cancel", 
                                            fTL("Cancel"), 
                                            "deselectCountGroupUserSelection(); disablePopup('countgroup_generatetasks_form_popup');").
   
   CountGroupGenerateTasksButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   CountGroupGenerateTasksForm:FormBrowse  = CountGroupUserBrowse.
   CountGroupGenerateTasksForm:FormButtons = CountGroupGenerateTasksButtons.
   
   CountGroupGenerateTasksForm:endForm(). 
   
   CountGroupGenerateTasksForm:displayForm(). 
   
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

&IF DEFINED(EXCLUDE-pLocationFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationFilter Procedure 
PROCEDURE pLocationFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   
   LocationBrowseFilterForm = NEW dataForm("location_filter_form").
   LocationBrowseFilterForm:WebStream  = STREAM WebStream:HANDLE.
   LocationBrowseFilterForm:FormAction = "adCountGroupAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationBrowseFilterForm:FormWidth   = 350.
   LocationBrowseFilterForm:FormHeight  = 200.
   LocationBrowseFilterForm:FormTitle   = "Location Filter".
   LocationBrowseFilterForm:FormType    = "small_wide".
   
   /* Column Layout */   
   LocationBrowseFilterForm:insertPaddingColumn(10).
   LocationBrowseFilterForm:insertColumn(140).
   LocationBrowseFilterForm:insertColumn(100).
   LocationBrowseFilterForm:insertColumn(90).
   LocationBrowseFilterForm:insertColumn(90).  
   
   /* Fields */
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Location Ref")).
   LocationBrowseFilterForm:insertTextField("LocationRef", chrSelectedLocationRef, 160, TRUE).  
   
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Location Ref Begins")).
   LocationBrowseFilterForm:insertTextField("LocationRefBegins", chrLocationRefBegins, 160, TRUE). 

   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Location Type")).   
   LocationBrowseFilterForm:insertComboField("LocationTypeID", STRING(intSelectedLocationTypeID), 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("LocationTypeID", "0", "All Types...").      
   /* Populate the LocationTypes */
   FOR EACH LocationType NO-LOCK
      BY LocationType.ACTIVE DESC
      BY LocationType.ListingSequence:      
      
      LocationBrowseFilterForm:insertComboPairs("LocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).
   END.
   
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Aisle")).   
   LocationBrowseFilterForm:insertComboField("AisleID", STRING(intSelectedAisleID), 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("AisleID", "0", "All Aisles...").      
   /* Populate the Aisles */
   FOR EACH Aisle NO-LOCK
      BY Aisle.ACTIVE DESC:
      
      LocationBrowseFilterForm:insertComboPairs("AisleID", STRING(Aisle.AisleID), Aisle.AisleName).
   END.

   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Work Zone")).   
   LocationBrowseFilterForm:insertComboField("WorkZoneID", STRING(intSelectedWorkZoneID), 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("WorkZoneID", "0", "All WorkZones...").      
   /* Populate the WorkZones */
   FOR EACH WorkZone NO-LOCK
      BY WorkZone.ACTIVE DESC:
      
      LocationBrowseFilterForm:insertComboPairs("WorkZoneID", STRING(WorkZone.WorkZoneID), WorkZone.WorkZoneName).
   END.

   /* Add Hidden Fields*/

   LocationBrowseFilterForm:insertHiddenField("LocationList","").
   LocationBrowseFilterForm:insertHiddenField("CountGroupID",chrCountGroupID).
   LocationBrowseFilterForm:insertHiddenField("CountGroupLocationLinkID",chrCountGroupLocationLinkID).
   LocationBrowseFilterForm:insertHiddenField("CountGroupLocationLinkVersionID","").
   LocationBrowseFilterForm:insertHiddenField("countgroup_browse_scroll","").
   LocationBrowseFilterForm:insertHiddenField("countgrouplocationlink_browse_scroll","").
   LocationBrowseFilterForm:insertHiddenField("popup_countgrouplocationlink_browse","").
   LocationBrowseFilterForm:insertHiddenField("popup_addcountgrouplocationlink_browse","").
   LocationBrowseFilterForm:insertHiddenField("allunused","").
   LocationBrowseFilterForm:insertHiddenField("filtering","").
   LocationBrowseFilterForm:insertHiddenField("form_name","location_filter_form").
   LocationBrowseFilterForm:insertHiddenField("prog_name","adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationBrowseFilterForm}
   
   /* Create Button Bar */
   LocationBrowseFilterButtons = NEW buttonBar().
   
   LocationBrowseFilterButtons:addButton("location_filter_form_btn_search", 
                                         fTL("Filter"), 
                                         "filterLocations()").
   
   LocationBrowseFilterButtons:addButton("location_filter_form_btn_all_unused", 
                                         fTL("All Un-Used"), 
                                         "allUnusedLocations()").
   
   LocationBrowseFilterButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   LocationBrowseFilterForm:FormButtons = LocationBrowseFilterButtons.
   
   LocationBrowseFilterForm:endForm(). 
   LocationBrowseFilterForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartUploadFileDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartUploadFileDetails Procedure
PROCEDURE pPartUploadFileDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "file_details_form"}

   FileSelectionForm            = NEW dataForm("partfile_selection_form").
   FileSelectionForm:WebStream  = STREAM WebStream:HANDLE.
   FileSelectionForm:FormAction = "dbCountGroupPartLocationLinkUpload.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileSelectionForm:FormWidth  = 420.
   FileSelectionForm:FormHeight = 70.
   FileSelectionForm:FormTitle  = "Select CountGroupPartLink Upload File".
   FileSelectionForm:FormType   = "xxsmall_wide".
   
   /* Column Layout */
   FileSelectionForm:insertPaddingColumn(15).
   
   /* Fields */
   {webGetOptionalFormFields.i pLocationUploadFileDetails}
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel("").
   FileSelectionForm:insertFileInput("UploadFile",380,TRUE).
   
   /* Add Hidden Fields*/
   FileSelectionForm:insertHiddenField("file_browse_scroll", "").
   FileSelectionForm:insertHiddenField("process_mode","").
   FileSelectionForm:insertHiddenField("popup_partfile_selection_form","").
   FileSelectionForm:insertHiddenField("CountGroupID",chrCountGroupID).
   FileSelectionForm:insertHiddenField("form_name", "partfile_selection_form").
   FileSelectionForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileSelectionForm}
   
   /* Create Button Bar */
   FileDetailsButtons = NEW buttonBar().
   FileDetailsButtons:addButton("partfile_selection_form_btn_save", 
                                fTL("Upload"), 
                                "uploadPartFile('partfile_selection_form');").
   
   FileDetailsButtons:addButton("partfile_selection_form_btn_cancel", 
                                fTL("Cancel"), 
                                "cancelUpdate('UserCancelled','process_mode'); disablePopup('partfile_selection_form_popup');").
   
   FileDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   FileSelectionForm:FormButtons = FileDetailsButtons.
   FileSelectionForm:endForm(). 
   FileSelectionForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pReadFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReadFile Procedure 
PROCEDURE pReadFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   EMPTY TEMP-TABLE ttFileLine.
   
   chrFileName        = get-value("UploadFile").
   chrDestinationFile = get-value("DestinationFile").
   
   /* This piece of code will only execute if the Destination File was posted */
   IF chrDestinationFile = "" THEN RETURN.

   logIsLogFile = entry(2,chrDestinationFile,".") = "log".
   
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
   FOR FIRST ttFileLine where ttFileLine.FileLineNo = 1:
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
   RUN pGetSystemOptions(OUTPUT chrPageBuildError).  

   /* Validate the FileMaster records exist */
   FIND FIRST FileMaster NO-LOCK
      WHERE FileMaster.MasterName = "CountGroupLocationLinkUpload" NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrPageBuildError = "FileMaster CountGroupLocationLinkUpload does not exist.".
   END.
   ELSE intLocationFileMasterID = FileMaster.FileMasterID.

   FIND FIRST FileMaster NO-LOCK
      WHERE FileMaster.MasterName = "CountGroupPartLocationLinkUpload" NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrPageBuildError = "FileMaster CountGroupPartLocationLinkUpload does not exist.".
   END.
   ELSE intPartFileMasterID = FileMaster.FileMasterID.

   
   ASSIGN chrCountGroupID                      = get-value("CountGroupID")
          intSelectedCountGroup                = INTEGER(chrCountGroupID)
          chrScrollToCountGroupRow             = STRING(INTEGER(get-value("countgroup_browse_scroll"))) + ";"
          chrCountGroupLocationLinkID          = get-value("CountGroupLocationLinkID")
          intSelectedCountGroupLocationLink    = INTEGER(chrCountGroupLocationLinkID)
          chrScrollToCountGroupLocationLinkRow = STRING(INTEGER(get-value("countgrouplocationlink_browse_scroll"))) + ";"
          chrCountGroupRecountID               = get-value("CountGroupRecountID")
          intSelectedCountGroupRecount         = INTEGER(chrCountGroupRecountID)
          chrScrollToCountGroupRecountRow      = STRING(INTEGER(get-value("countgrouprecount_browse_scroll"))) + ";"
          logFilterIsPoppedUp                  = (get-value('filtering') = "yes")
          logAllUnused                         = (get-value('allunused') = "yes")
          chrSelectedLocationRef               = get-value('LocationRef')
          chrLocationRefBegins                 = get-value('LocationRefBegins')
          intSelectedLocationTypeID            = INTEGER(get-value('LocationTypeID'))
          intSelectedAisleID                   = INTEGER(get-value('AisleID'))
          intSelectedWorkZoneID                = INTEGER(get-value('WorkZoneID'))
          chrValidFile                         = get-value("ValidFile")
          chrFileID                            = get-value("FileID")
          intSelectedFile                      = INTEGER(chrFileID)
          chrScrollToFileRow                   = STRING(INTEGER(get-value("file_browse_scroll"))) + ";"
          chrLocationHistoryID                 = get-value("LocationHistoryID")
          intSelectedLocationHistory           = INTEGER(chrLocationHistoryID)
          intAddedLocationLinksCount           = INTEGER(get-value("added_locationlinks_count"))
          intDeactivatedLocationLinksCount     = INTEGER(get-value("deactivated_locationlinks_count"))
          chrIgnoredLocations                  = get-value("IgnoredLocations")
          chrSuccessTaskGenerate               = get-value("SuccessTaskGenerate").
   
   IF logFilterIsPoppedUp THEN
     chrPopupFilters = 'viewLocationFilter("location_filter_form");'.   
   
   /* Process URL values */
   IF chrCountGroupID <> "" THEN
      chrSelectCountGroupRow = 'selectCountGroupRow(document.getElementById("countgroup_browse_row_' + chrCountGroupID + '"),"' 
                                                         + chrCountGroupID +  '");'.
   IF chrCountGroupLocationLinkID <> "" THEN
      chrSelectCountGroupLocationLinkRow = 'selectCountGroupLocationLinkRow(document.getElementById("countgrouplocationlink_browse_row_' + chrCountGroupLocationLinkID + '"),"'
                                                              + chrCountGroupLocationLinkID + '");'.
    
   IF chrCountGroupRecountID <> "" THEN
      chrSelectCountGroupRecountRow = 'selectCountGroupRecountRow(document.getElementById("countgrouprecount_browse_row_' + chrCountGroupRecountID + '"),"'
                                                              + chrCountGroupRecountID + '");'.

   IF chrFileID <> "" THEN
      chrSelectFileRow = 'selectFileRow(document.getElementById("file_browse_row_' + chrFileID + '"),"' + chrFileID + '");'.
   
   IF get-value('popup_countgrouphistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("countgrouphistory_browse_form_popup");'.

   /* Popups */
   IF get-value('popup_countgrouplocationlink_browse') = "yes" THEN
      chrPopupCountGroupLocationLinks = 'enablePopup("countgrouplocationlink_browse_form_popup");'.

   IF get-value('popup_addcountgrouplocationlink_browse') = "yes" THEN
      chrPopupAddCountGroupLocationLinks = 'enablePopup("addcountgrouplocationlink_browse_form_popup");'.

   IF get-value('popup_countgrouprecount_browse') = "yes" 
   AND chrPopupFilters = "" AND chrPopupHistory = "" 
   AND chrPopupCountGroupLocationLinks = "" AND chrPopupAddCountGroupLocationLinks = ""
   THEN
   DO:
      chrPopupCountGroupRecounts = 'enablePopup("countgrouprecount_browse_form_popup");'.
   END.
   
   IF get-value('popup_file_browse') = "yes" THEN
      chrPopupFile = 'enablePopup("file_browse_form_popup");'.
   
   /* Build the Location Popup File Selection command if requested */
   IF get-value('popup_locationfile_selection_form') = "yes" THEN 
      chrPopupFileSelection = 'enablePopup("locationfile_selection_form_popup");'.   
   
   /* Build the Part Popup File Selection command if requested */
   IF get-value('popup_partfile_selection_form') = "yes" THEN 
      chrPopupFileSelection = 'enablePopup("partfile_selection_form_popup");'.   
   
   /* Build the Popup uploadcountgrouplocationlinks browse command if requested */
   IF get-value('popup_uploadcountgrouplocationlinks_browse_form') = "yes"
   AND chrPopupFilters = "" AND chrPopupHistory = "" AND chrPopupCountGroupLocationLinks = "" 
   AND chrPopupAddCountGroupLocationLinks = "" AND chrPopupCountGroupRecounts = "" AND chrPopupFile = "" 
   AND chrPopupFileSelection = "" AND chrPopupErrors = "" AND chrPopupFileLocationLinks = "" 
   AND chrPopupLocationDifferences = "" THEN
   DO:
      chrPopupUploadCountGroupLocationLinks = 'enablePopup("uploadcountgrouplocationlinks_browse_form_popup");'.
   END.
   
   /* Build the Popup uploadcountgrouplocationlinks browse command if requested */
   IF get-value('popup_uploadcountgrouppartlocationlinks_browse_form') = "yes" 
   AND chrPopupFilters = "" AND chrPopupHistory = "" AND chrPopupCountGroupLocationLinks = "" 
   AND chrPopupAddCountGroupLocationLinks = "" AND chrPopupCountGroupRecounts = "" AND chrPopupFile = "" 
   AND chrPopupFileSelection = "" AND chrPopupErrors = "" AND chrPopupFileLocationLinks = "" 
   AND chrPopupLocationDifferences = "" THEN
   DO:
      chrPopupUploadCountGroupLocationLinks = 'enablePopup("uploadcountgrouppartlocationlinks_browse_form_popup");'.
   END.      
   
   /* Build the Popup Errors command if requested */
   IF get-value('popup_errors_browse_form') = "yes" 
   AND chrPopupFilters = "" AND chrPopupHistory = "" AND chrPopupCountGroupLocationLinks = "" 
   AND chrPopupAddCountGroupLocationLinks = "" AND chrPopupCountGroupRecounts = "" AND chrPopupFile = "" 
   AND chrPopupFileSelection = "" AND chrPopupUploadCountGroupLocationLinks = "" AND chrPopupFileLocationLinks = "" 
   AND chrPopupLocationDifferences = "" THEN
   DO: 
      chrPopupErrors = 'enablePopup("errors_browse_form_popup");'.
   END.
   
   /* Build the Popup FileLocationLink command if requested */
   IF get-value('popup_filelocationlink_browse_form') = "yes" THEN 
      chrPopupFileLocationLinks = 'enablePopup("filelocationlink_browse_form_popup");'.
   
   IF chrLocationHistoryID <> "" AND chrLocationHistoryID <> "0" THEN
      chrSelectLocationHistoryRow = 'selectFileLocationLinkRow(document.getElementById("filelocationlink_browse_row_' + chrLocationHistoryID + '"),"' + chrLocationHistoryID + '");'.
   
   /* Build the Popup LocationDifferences command if requested */
   IF get-value('popup_locationdifferences_browse_form') = "yes" THEN 
      chrPopupLocationDifferences = 'enablePopup("locationdifferences_browse_form_popup");'.
   
   IF (intAddedLocationLinksCount > 0 OR intDeactivatedLocationLinksCount > 0) 
   AND chrPopupAddCountGroupLocationLinks = "" AND chrPopupFilters = "" THEN 
   DO:
      chrMessageNumberOfUpdates = 'systemAlert("message_alert","' 
                                    /* + "File has successfully uploaded.  " */
                                    + "Ignored Location(s): " + chrIgnoredLocations + ".   "
                                    + "Deactivated Location Link(s): " + STRING(intDeactivatedLocationLinksCount) + ".   "
                                    + "Created Location Link(s): " + STRING(intAddedLocationLinksCount) + "."
                                    + '","File has successfully uploaded")'.
   END.
   
   /* Build the Generate Tasks success message */
   IF chrSuccessTaskGenerate <> "" 
   AND chrPopupFilters = "" AND chrPopupHistory = "" AND chrPopupCountGroupLocationLinks = "" 
   AND chrPopupAddCountGroupLocationLinks = "" AND chrPopupCountGroupRecounts = "" AND chrPopupFile = "" 
   AND chrPopupFileSelection = "" AND chrPopupUploadCountGroupLocationLinks = "" AND chrPopupFileLocationLinks = "" 
   AND chrPopupLocationDifferences = "" AND chrPopupErrors = "" THEN
   DO:
      chrTaskGenerateMessage = 'systemAlert("message_alert","' + chrSuccessTaskGenerate + '","Tasks successfully generated")'.
   END.

   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("countgroup_browse").scrollTop=' + chrScrollToCountGroupRow + chrSelectCountGroupRow
                             + chrPopupCountGroupLocationLinks
                             + 'document.getElementById("countgrouplocationlink_browse").scrollTop=' + chrScrollToCountGroupLocationLinkRow + chrSelectCountGroupLocationLinkRow
                             + chrPopupCountGroupRecounts
                             + 'document.getElementById("countgrouprecount_browse").scrollTop=' + chrScrollToCountGroupRecountRow + chrSelectCountGroupRecountRow
                             + chrPopupAddCountGroupLocationLinks
                             + chrPopupFilters
                             + chrPopupHistory
                             + chrPopupFile
                             + chrPopupFileSelection 
                             + chrPopupUploadCountGroupLocationLinks 
                             + chrPopupErrors + chrSelectLocationHistoryRow
                             + chrPopupFileLocationLinks + chrPopupLocationDifferences
                             + chrMessageNumberOfUpdates
                             + chrTaskGenerateMessage.

   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Count Group Admin".
   ThisPage:FrameTitle    = "Count Group Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   /* ThisPage:addJavaScript("/syngate/" + LOWER(Config.PackageName) + "/js/countgroup.js"). */
   ThisPage:addJavaScript("countgroup.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pCountGroupBrowse.
   
   FIND FIRST CountGroup NO-LOCK
      WHERE CountGroup.CountGroupID = intSelectedCountGroup NO-ERROR.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pCountGroupDetails.

   RUN pCountGroupHistoryBrowse.

   RUN pCountGroupLocationLinkBrowse.
   
   FIND FIRST CountGroupLocationLink NO-LOCK
      WHERE CountGroupLocationLink.CountGroupLocationLinkID = intSelectedCountGroupLocationLink NO-ERROR.

   RUN pAddCountGroupLocationLinksBrowse.
   
   RUN pLocationFilter.
   
   RUN pGenerateCountTasks.

   RUN pCountGroupRecountBrowse.

   FIND FIRST CountGroupRecount NO-LOCK
      WHERE CountGroupRecount.CountGroupRecountID = intSelectedCountGroupRecount NO-ERROR.

   RUN pCountGroupRecountDetails.
   
   RUN pReadFile.
   
   RUN pFileBrowse.
   
   IF intSelectedFile <> 0 THEN
   DO:
      FIND FIRST File NO-LOCK 
         WHERE File.FileId = intSelectedFile NO-ERROR.
   END.
   
   IF AVAILABLE CountGroup THEN 
   DO:
      FIND FIRST CountGroupType OF CountGroup NO-LOCK NO-ERROR.
   END.
   
   RUN pFileDetails.
   
   RUN pLocationUploadFileDetails.
   
   RUN pPartUploadFileDetails.
   
   RUN pUploadCountGroupLocationLinkBrowse.
   RUN pUploadCountGroupPartLocationLinkBrowse.
   RUN pErrorBrowse.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT CountGroupBrowseFrame                  NO-ERROR.
   DELETE OBJECT CountGroupBrowse                       NO-ERROR.
   DELETE OBJECT CountGroupBrowseButtons                NO-ERROR.
   DELETE OBJECT CountGroupDetailsForm                  NO-ERROR.
   DELETE OBJECT CountGroupDetailsButtons               NO-ERROR.
      
   DELETE OBJECT CountGroupLocationLinkBrowseFrame      NO-ERROR.
   DELETE OBJECT CountGroupLocationLinkBrowseForm       NO-ERROR.
   DELETE OBJECT CountGroupLocationLinkBrowse           NO-ERROR.
   DELETE OBJECT CountGroupLocationLinkBrowseButtons    NO-ERROR.

   DELETE OBJECT AddCountGroupLocationLinkBrowseFrame   NO-ERROR.
   DELETE OBJECT AddCountGroupLocationLinkBrowseForm    NO-ERROR.
   DELETE OBJECT AddCountGroupLocationLinkBrowse        NO-ERROR.
   DELETE OBJECT AddCountGroupLocationLinkBrowseButtons NO-ERROR.

   DELETE OBJECT CountGroupRecountBrowseFrame           NO-ERROR.
   DELETE OBJECT CountGroupRecountBrowseForm            NO-ERROR.
   DELETE OBJECT CountGroupRecountBrowse                NO-ERROR.
   DELETE OBJECT CountGroupRecountBrowseButtons         NO-ERROR.

   DELETE OBJECT LocationBrowseFilterForm               NO-ERROR.
   DELETE OBJECT LocationBrowseFilterButtons            NO-ERROR.

   DELETE OBJECT CountGroupGenerateTasksForm            NO-ERROR.
   DELETE OBJECT CountGroupGenerateTasksButtons         NO-ERROR.

   DELETE OBJECT CountGroupRecountDetailsForm           NO-ERROR.
   DELETE OBJECT CountGroupRecountDetailsButtons        NO-ERROR.

   DELETE OBJECT CountGroupHistoryBrowseFrame           NO-ERROR.
   DELETE OBJECT CountGroupHistoryBrowseForm            NO-ERROR.
   DELETE OBJECT CountGroupHistoryBrowse                NO-ERROR.
   DELETE OBJECT CountGroupHistoryBrowseButtons         NO-ERROR.

   DELETE OBJECT CountGroupUserBrowse                   NO-ERROR.

   DELETE OBJECT FileSelectionFrame                         NO-ERROR.
   DELETE OBJECT FileSelectionForm                          NO-ERROR.
   DELETE OBJECT FileBrowseFrame                            NO-ERROR.
   DELETE OBJECT FileBrowse                                 NO-ERROR.
   DELETE OBJECT FileBrowseButtons                          NO-ERROR.
   DELETE OBJECT FileDetailsForm                            NO-ERROR.
   DELETE OBJECT FileDetailsButtons                         NO-ERROR.
   DELETE OBJECT UploadCountGroupLocationLinksBrowseFrame   NO-ERROR.
   DELETE OBJECT UploadCountGroupLocationLinksBrowse        NO-ERROR.
   DELETE OBJECT UploadCountGroupLocationLinksBrowseButtons NO-ERROR.
   DELETE OBJECT UploadCountGroupLocationLinksBrowseForm    NO-ERROR.
   DELETE OBJECT ErrorsBrowseFrame                          NO-ERROR.
   DELETE OBJECT ErrorsBrowseForm                           NO-ERROR.
   DELETE OBJECT ErrorsBrowse                               NO-ERROR.
   DELETE OBJECT ErrorsBrowseButtons                        NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetLocationRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetLocationRows Procedure 
PROCEDURE pSetLocationRows :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   AddCountGroupLocationLinkBrowse:startRow(Location.LocationID, 
                                  'multiSelectLocationRow("' + 
                                  STRING(Location.LocationID) + '")', "").
   
   AddCountGroupLocationLinkBrowse:insertData(STRING(Location.LocationID), "left").
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalBrowseFields.i CountGroupLocationLink}

   AddCountGroupLocationLinkBrowse:insertData((IF AVAILABLE LocationType THEN LocationType.TypeName ELSE ""), "left").
   AddCountGroupLocationLinkBrowse:insertData((IF AVAILABLE Aisle THEN Aisle.AisleName ELSE ""),"left").
   AddCountGroupLocationLinkBrowse:insertData((IF AVAILABLE Aisle AND AVAILABLE WorkZone THEN WorkZone.WorkZoneName ELSE ""),"left").
   AddCountGroupLocationLinkBrowse:insertData(Location.LocationRef,"left").
   AddCountGroupLocationLinkBrowse:insertData(STRING(Location.Active,"Yes/No")).

   /* Add hidden fields */
   AddCountGroupLocationLinkBrowse:insertHiddenData("LocationID",Location.LocationID).
   AddCountGroupLocationLinkBrowse:insertHiddenData("LocationVersionID",Location.VersionID).

   AddCountGroupLocationLinkBrowse:endRow().

   /* Count the Locations */
   intLocationCnt = intLocationCnt + 1.

   IF intLocationCnt <= 2000 THEN
      chrLocations = chrLocations + STRING(Location.LocationID) + ",".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pUploadCountGroupLocationLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUploadCountGroupLocationLinkBrowse Procedure 
PROCEDURE pUploadCountGroupLocationLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:   Build a browse to show the locations from the upload file 
             before uploading them into the system  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "uploadcountgrouplocationlinks_details_form"}
   
   UploadCountGroupLocationLinksBrowseForm = NEW dataForm("uploadcountgrouplocationlinks_browse_form").
   UploadCountGroupLocationLinksBrowseForm:WebStream = STREAM WebStream:HANDLE.
   UploadCountGroupLocationLinksBrowseForm:FormAction  = "dbCountGroupLocationLinkUpload.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   UploadCountGroupLocationLinksBrowseForm:FormWidth  = 700.
   UploadCountGroupLocationLinksBrowseForm:FormHeight = 490.
   UploadCountGroupLocationLinksBrowseForm:FormTitle  = fTL("Confirm CountGroupLocationLink Upload").
   UploadCountGroupLocationLinksBrowseForm:FormType   = "xl_large".
   
   UploadCountGroupLocationLinksBrowse              = NEW browseTable("uploadcountgrouplocationlinks_browse").
   UploadCountGroupLocationLinksBrowse:BrowseWidth  = 680.
   UploadCountGroupLocationLinksBrowse:BrowseHeight = 450.
   
   /*
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i StockStatusChangeReason}
   */
   
   /* the ground and upper LocationTypeGroups; check for availability when used */
   FIND FIRST groundLocationTypeGroup NO-LOCK 
      WHERE groundLocationTypeGroup.GroupCode = "GroundLocations" 
      AND groundLocationTypeGroup.Active NO-ERROR.
      
   FIND FIRST upperLocationTypeGroup NO-LOCK 
      WHERE upperLocationTypeGroup.GroupCode = "UpperLocations" 
      AND upperLocationTypeGroup.Active NO-ERROR.
      
      
   /* design the columns of the browse */
   UploadCountGroupLocationLinksBrowse:insertColumn("LocationRef" , 150, "CHARACTER", "left", FALSE). 
   UploadCountGroupLocationLinksBrowse:insertColumn("Details" ,     100, "CHARACTER", "left", FALSE).
   
   /*Body*/
   UploadCountGroupLocationLinksBrowse:startBody().
   
   FOR EACH ttFileLine NO-LOCK:
      
      UploadCountGroupLocationLinksBrowse:startRow(ttFileLine.FileLineNo, "", "").
      
      chrColumnValue = ENTRY(1,ttFileLine.LineString, ",").
      UploadCountGroupLocationLinksBrowse:insertData(chrColumnValue, "left").
      
      FIND FIRST Location NO-LOCK
         WHERE Location.LocationRef = chrColumnValue NO-ERROR.
   
      /* Start Validate if Location belongs to Ground or Upper LocationTypeGroup - if not, stamp as ignored */
      FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
      
      /* Check to see if CountGroupLocationLink is active and belongs to one of the LocationTypeGroups below */
      IF CAN-FIND(FIRST LocationTypeGroupLink OF LocationType NO-LOCK
                      WHERE LocationTypeGroupLink.LocationTypeGroupID = groundLocationTypeGroup.LocationTypeGroupID
                      AND LocationTypeGroupLink.Active) OR 
         CAN-FIND(FIRST LocationTypeGroupLink OF LocationType NO-LOCK
                      WHERE LocationTypeGroupLink.LocationTypeGroupID = upperLocationTypeGroup.LocationTypeGroupID
                      AND LocationTypeGroupLink.Active) THEN 
      DO:
         /* a new link will always be created for each location uploaded */
         UploadCountGroupLocationLinksBrowse:insertData("New", "left").
      END. /* Finish Validate if Location belongs to Ground or Upper LocationTypeGroup */
      ELSE 
      DO:
         /* Signal this location will be ignored */
         UploadCountGroupLocationLinksBrowse:insertData("Ignored", "left").
      END.
      
      /* Add hidden fields */
      UploadCountGroupLocationLinksBrowse:insertHiddenData("FileLineNo",ttFileLine.FileLineNo).

      UploadCountGroupLocationLinksBrowse:endRow().
      
   END. /* FOR EACH ttFileLine NO-LOCK */
   
   UploadCountGroupLocationLinksBrowse:endTable().
   
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("uploadcountgrouplocationlinks_browse_scroll","").
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("CountGroupID",chrCountGroupID).
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("uploadcountgrouplocationlinksID","").
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("uploadcountgrouplocationlinksVersionID","").
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("UploadedFileName",chrFileName).
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("popup_uploadcountgrouplocationlinks_browse_form","").  
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("form_name", "uploadcountgrouplocationlinks_browse_form").
   UploadCountGroupLocationLinksBrowseForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i UploadCountGroupLocationLinksBrowseForm}
   
   /* Create Button Bar */
   UploadCountGroupLocationLinksBrowseButtons = NEW buttonBar().
   
   UploadCountGroupLocationLinksBrowseButtons:addButton("uploadcountgrouplocationlinks_browse_form_btn_save",
                                                        fTL("Confirm"),
                                                        "saveUploadCountGroupLocationLinks('uploadcountgrouplocationlinks_browse_form');",
                                                        (IF chrValidFile = "YES" THEN "" ELSE "DISABLED")).
   
   UploadCountGroupLocationLinksBrowseButtons:addButton("uploadcountgrouplocationlinks_browse_form_btn_cancel",
                                                        fTL("Cancel"),
                                                        "cancelUpdate('UserCancelled','process_mode'); disablePopup('uploadcountgrouplocationlinks_browse_form_popup');").
   
   UploadCountGroupLocationLinksBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   UploadCountGroupLocationLinksBrowseForm:FormBrowse  = UploadCountGroupLocationLinksBrowse.
   UploadCountGroupLocationLinksBrowseForm:FormButtons = UploadCountGroupLocationLinksBrowseButtons.
   UploadCountGroupLocationLinksBrowseForm:endForm().
   UploadCountGroupLocationLinksBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationUploadFileDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationUploadFileDetails Procedure 
PROCEDURE pLocationUploadFileDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "file_details_form"}

   FileSelectionForm            = NEW dataForm("locationfile_selection_form").
   FileSelectionForm:WebStream  = STREAM WebStream:HANDLE.
   FileSelectionForm:FormAction = "dbCountGroupLocationLinkUpload.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileSelectionForm:FormWidth  = 420.
   FileSelectionForm:FormHeight = 70.
   FileSelectionForm:FormTitle  = "Select CountGroupLocationLink Upload File".
   FileSelectionForm:FormType   = "xxsmall_wide".
   
   /* Column Layout */
   FileSelectionForm:insertPaddingColumn(15).
   
   /* Fields */
   {webGetOptionalFormFields.i pLocationUploadFileDetails}
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel("").
   FileSelectionForm:insertFileInput("UploadFile",380,TRUE).
   
   /* Add Hidden Fields*/
   FileSelectionForm:insertHiddenField("file_browse_scroll", "").
   FileSelectionForm:insertHiddenField("process_mode","").
   FileSelectionForm:insertHiddenField("popup_locationfile_selection_form","").
   FileSelectionForm:insertHiddenField("CountGroupID",chrCountGroupID).
   FileSelectionForm:insertHiddenField("form_name", "locationfile_selection_form").
   FileSelectionForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileSelectionForm}
   
   /* Create Button Bar */
   FileDetailsButtons = NEW buttonBar().
   FileDetailsButtons:addButton("locationfile_selection_form_btn_save", 
                                fTL("Upload"), 
                                "uploadLocationFile('locationfile_selection_form');").
   
   FileDetailsButtons:addButton("locationfile_selection_form_btn_cancel", 
                                fTL("Cancel"), 
                                "cancelUpdate('UserCancelled','process_mode'); disablePopup('locationfile_selection_form_popup');").
   
   FileDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   FileSelectionForm:FormButtons = FileDetailsButtons.
   FileSelectionForm:endForm(). 
   FileSelectionForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pUploadCountGroupPartLocationLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUploadCountGroupPartLocationLinkBrowse Procedure
PROCEDURE pUploadCountGroupPartLocationLinkBrowse:
/*------------------------------------------------------------------------------
  Purpose:   Build a browse to show the parts from the upload file 
             before uploading them into the system  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "uploadcountgrouplocationlinks_details_form"}
   
   UploadCountGroupPartLocationLinksBrowseForm = NEW dataForm("uploadcountgrouppartlocationlinks_browse_form").
   UploadCountGroupPartLocationLinksBrowseForm:WebStream = STREAM WebStream:HANDLE.
   UploadCountGroupPartLocationLinksBrowseForm:FormAction  = "dbCountGroupPartLocationLinkUpload.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   UploadCountGroupPartLocationLinksBrowseForm:FormWidth  = 700.
   UploadCountGroupPartLocationLinksBrowseForm:FormHeight = 490.
   UploadCountGroupPartLocationLinksBrowseForm:FormTitle  = fTL("Confirm CountGroupPartLocationLink Upload").
   UploadCountGroupPartLocationLinksBrowseForm:FormType   = "xl_large".
   
   UploadCountGroupPartLocationLinksBrowse              = NEW browseTable("uploadcountgrouppartlocationlinks_browse").
   UploadCountGroupPartLocationLinksBrowse:BrowseWidth  = 680.
   UploadCountGroupPartLocationLinksBrowse:BrowseHeight = 450.

   /* the ground and upper LocationTypeGroups; check for availability when used */
   FIND FIRST groundLocationTypeGroup NO-LOCK 
      WHERE groundLocationTypeGroup.GroupCode = "GroundLocations" 
      AND groundLocationTypeGroup.Active NO-ERROR.
      
   FIND FIRST upperLocationTypeGroup NO-LOCK 
      WHERE upperLocationTypeGroup.GroupCode = "UpperLocations" 
      AND upperLocationTypeGroup.Active NO-ERROR.
      
   /* design the columns of the browse */
   UploadCountGroupPartLocationLinksBrowse:insertColumn("Part" ,     200, "CHARACTER", "left", FALSE). 
   UploadCountGroupPartLocationLinksBrowse:insertColumn("Location" , 210, "CHARACTER", "left", FALSE).
   UploadCountGroupPartLocationLinksBrowse:insertColumn("Details" ,  100, "CHARACTER", "left", FALSE).
   
   /*Body*/
   UploadCountGroupPartLocationLinksBrowse:startBody().
   
   FOR EACH ttFileLine NO-LOCK:
      
      FIND FIRST Part NO-LOCK
         WHERE Part.PartRef = ENTRY(1,ttFileLine.LineString, ",") NO-ERROR.
      
      FOR EACH StockPackage NO-LOCK 
         WHERE StockPackage.PartID = Part.PartID
         BREAK BY StockPackage.LocationID:
         
         IF StockPackage.PackageQty = 0 THEN NEXT.
         
         FIND FIRST Location OF StockPackage NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Location THEN NEXT. 
         
         IF FIRST-OF(StockPackage.LocationID) THEN 
         DO:
            /* Start Row */
            UploadCountGroupPartLocationLinksBrowse:startRow(ttFileLine.FileLineNo, "", "").
            
            UploadCountGroupPartLocationLinksBrowse:insertData(Part.PartRef,         "left").
            UploadCountGroupPartLocationLinksBrowse:insertData(Location.LocationRef, "left").
            
            /* Start Validate if Location belongs to Ground or Upper LocationTypeGroup - if not, stampe as ignored */
            FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
            
            /* Check to see if CountGroupLocationLink is active and belongs to one of the LocationTypeGroups below */
            IF CAN-FIND(FIRST LocationTypeGroupLink OF LocationType NO-LOCK
                            WHERE LocationTypeGroupLink.LocationTypeGroupID = groundLocationTypeGroup.LocationTypeGroupID
                            AND LocationTypeGroupLink.Active) OR 
               CAN-FIND(FIRST LocationTypeGroupLink OF LocationType NO-LOCK
                            WHERE LocationTypeGroupLink.LocationTypeGroupID = upperLocationTypeGroup.LocationTypeGroupID
                            AND LocationTypeGroupLink.Active) THEN 
            DO:
               /* a new link will always be created for each location uploaded */
               UploadCountGroupPartLocationLinksBrowse:insertData("New", "left").
            END. /* Finish Validate if Location belongs to Ground or Upper LocationTypeGroup */
            ELSE 
            DO:
               /* Signal this location will be ignored */
               UploadCountGroupPartLocationLinksBrowse:insertData("Ignored", "left").
            END.
            
            /* Add hidden fields */
            UploadCountGroupPartLocationLinksBrowse:insertHiddenData("FileLineNo",ttFileLine.FileLineNo).
            
            UploadCountGroupPartLocationLinksBrowse:endRow().
            /* End Row */
         END. /* IF FIRST-OF(StockPackage.LocationID)  */
      END. /* Each StockPackage of Part */   
   END. /* FOR EACH ttFileLine NO-LOCK */
   
   UploadCountGroupPartLocationLinksBrowse:endTable().
   
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("uploadcountgrouppartlocationlinks_browse_scroll","").
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("CountGroupID",chrCountGroupID).
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("uploadcountgrouppartlocationlinksID","").
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("uploadcountgrouppartlocationlinksVersionID","").
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("UploadedFileName",chrFileName).
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("popup_uploadcountgrouppartlocationlinks_browse_form","").  
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("form_name", "uploadcountgrouppartlocationlinks_browse_form").
   UploadCountGroupPartLocationLinksBrowseForm:insertHiddenField("prog_name", "adCountGroupAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i UploadCountGroupPartLocationLinksBrowseForm}
   
   /* Create Button Bar */
   UploadCountGroupPartLocationLinksBrowseButtons = NEW buttonBar().
   
   UploadCountGroupPartLocationLinksBrowseButtons:addButton("uploadcountgrouppartlocationlinks_browse_form_btn_save",
                                                        fTL("Confirm"),
                                                        "saveUploadCountGroupPartLocationLinks('uploadcountgrouppartlocationlinks_browse_form');",
                                                        (IF chrValidFile = "YES" THEN "" ELSE "DISABLED")).
   
   UploadCountGroupPartLocationLinksBrowseButtons:addButton("uploadcountgrouppartlocationlinks_browse_form_btn_cancel",
                                                        fTL("Cancel"),
                                                        "cancelUpdate('UserCancelled','process_mode'); disablePopup('uploadcountgrouppartlocationlinks_browse_form_popup');").
   
   UploadCountGroupPartLocationLinksBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   UploadCountGroupPartLocationLinksBrowseForm:FormBrowse  = UploadCountGroupPartLocationLinksBrowse.
   UploadCountGroupPartLocationLinksBrowseForm:FormButtons = UploadCountGroupPartLocationLinksBrowseButtons.
   UploadCountGroupPartLocationLinksBrowseForm:endForm().
   UploadCountGroupPartLocationLinksBrowseForm:displayForm().
   
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

