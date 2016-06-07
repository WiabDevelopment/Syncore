&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: 

  Location:
   
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
     cleanup will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Custom Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{fncStatusTypeFunctions.i}

{defWebDefinitions.i}

/* Get System Options */
{getLocationOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedLocation               AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectLocationRow              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedPrinterLocationLink    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrScrollToLocationRow            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLocationID                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectedLocationRef            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLocationRefBegins              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPrinterLocationLinkID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectPrinterLocationLinkRow   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPrinterLocationLinkRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedLocationTypeID         AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedAisleID                AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedWorkZoneID             AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectedEmpty                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intFilteredLocation               AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedBusinessUnitID         AS INTEGER     NO-UNDO.
DEFINE VARIABLE logFilterIsPoppedUp               AS LOGICAL     NO-UNDO.
DEFINE VARIABLE chrPopupPrinterLocationLinks      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupFilters                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedTaskLineWork           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPopupTaskLineWorks             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectTaskLineWorkRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedLocationHistoryID      AS INTEGER     NO-UNDO.

/* Objects */
DEFINE VARIABLE LocationBrowseFrame               AS pageFrame.
DEFINE VARIABLE LocationBrowse                    AS browseTable.
DEFINE VARIABLE LocationBrowseButtons             AS buttonBar.
DEFINE VARIABLE LocationDetailsForm               AS dataForm.
DEFINE VARIABLE LocationDetailsButtons            AS buttonBar.
DEFINE VARIABLE LocationBrowseFilterForm          AS dataForm.
DEFINE VARIABLE LocationBrowseFilterButtons       AS buttonBar.
DEFINE VARIABLE PrinterLocationLinkBrowseFrame    AS pageFrame.
DEFINE VARIABLE PrinterLocationLinkBrowse         AS browseTable.
DEFINE VARIABLE PrinterLocationLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE PrinterLocationLinkDetailsForm    AS dataForm.
DEFINE VARIABLE PrinterLocationLinkDetailsButtons AS buttonBar.
DEFINE VARIABLE PrinterLocationLinkBrowseForm     AS dataForm.
DEFINE VARIABLE TaskLineWorkBrowseForm            AS dataForm.
DEFINE VARIABLE TaskLineWorkBrowse                AS browseTable.
DEFINE VARIABLE TaskLineWorkBrowseButtons         AS buttonBar.
DEFINE VARIABLE TaskLineWorkDetailsForm           AS dataForm.
DEFINE VARIABLE TaskLineWorkDetailsButtons        AS buttonBar.
DEFINE VARIABLE LocationHistoryBrowseFrame        AS pageFrame.
DEFINE VARIABLE LocationHistoryBrowseForm         AS dataForm.
DEFINE VARIABLE LocationHistoryBrowse             AS browseTable.
DEFINE VARIABLE LocationHistoryBrowseButtons      AS buttonBar.
DEFINE VARIABLE LocationHistoryDetailsForm        AS dataForm.
DEFINE VARIABLE LocationHistoryDetailsButtons     AS buttonBar.

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
         HEIGHT             = 21.05
         WIDTH              = 57.
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

&IF DEFINED(EXCLUDE-pLocationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationBrowse Procedure 
PROCEDURE pLocationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "location_details_form"}
   
   LocationBrowse              = NEW browseTable("location_browse").
   LocationBrowse:BrowseWidth  = 965.
   LocationBrowse:BrowseHeight = 455.
   LocationBrowse:ExcelExport  = TRUE.
   LocationBrowse:SessionID    = intGblSessionID.
   LocationBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   LocationBrowse:insertColumn(fTL("Location ID"), 90, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Location}
   
   LocationBrowse:insertColumn(fTL("Location Ref"),     120, "CHARACTER", "left", FALSE).
   LocationBrowse:insertColumn(fTL("Location Type"),    115, "CHARACTER", "left", FALSE).
   LocationBrowse:insertColumn(fTL("Aisle"),             60, "CHARACTER", "left", FALSE).
   LocationBrowse:insertColumn(fTL("WorkZone"),         120, "CHARACTER", "left", FALSE).
   LocationBrowse:insertColumn(fTL("EOA"),               60, "INTEGER", FALSE).
   LocationBrowse:insertColumn(fTL("Walk Seq"),          70, "INTEGER", FALSE).
   LocationBrowse:insertColumn(fTL("Count Seq"),         70, "INTEGER", FALSE).
   LocationBrowse:insertColumn(fTL("Max Parts"),         60, "INTEGER", FALSE).
   LocationBrowse:insertColumn(fTL("Counting"),          60, "INTEGER", FALSE).
   LocationBrowse:insertColumn(fTL("Empty"),             60, "LOGICAL", FALSE).
   LocationBrowse:insertColumn(fTL("Active"),            60, "LOGICAL", FALSE).

   /*Body*/
   LocationBrowse:startBody().
   
   IF NOT logFilterIsPoppedUp THEN
   DO:
      IF intFilteredLocation <> 0 THEN
      DO:
         FIND FIRST Location NO-LOCK
            WHERE LocationID = intFilteredLocation NO-ERROR.
         IF AVAILABLE Location THEN
         DO:
            FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
            
            FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.
            
            FIND FIRST WorkZone OF Location NO-LOCK NO-ERROR.
            
            RUN pSetLocationRows.
         END. /* IF AVAILABLE Location THEN */
      END. /* ELSE */
      ELSE IF intSelectedLocationTypeID <> 0 OR
              intSelectedAisleID <> 0 OR
              intSelectedBusinessUnitID <> 0 OR 
              intSelectedWorkZoneID <> 0 THEN
      DO:
         LocationLoop:
         FOR EACH Location NO-LOCK /* ActiveLocationRef*/
            BY Location.Active DESC
            BY Location.LocationRef:
               
            IF Location.BusinessUnitID <> 0 AND NOT fCanViewBusinessUnit(intGblSessionID, Location.BusinessUnitID) THEN 
               NEXT LocationLoop.
            
            FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.

            IF chrSelectedEmpty = "ShowEmptyLocations" AND CAN-FIND(FIRST StockPackage OF Location) THEN
               NEXT LocationLoop.
            
            IF chrSelectedEmpty = "ShowOccupiedLocations" AND NOT CAN-FIND(FIRST StockPackage OF Location) THEN
               NEXT LocationLoop.
            
            IF intSelectedLocationTypeID <> 0 AND Location.LocationTypeID <> intSelectedLocationTypeID THEN 
               NEXT LocationLoop.
            
            FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.
            
            FIND FIRST WorkZone OF Location NO-LOCK NO-ERROR.

            IF intSelectedAisleID <> 0 AND Location.AisleID <> intSelectedAisleID THEN
               NEXT LocationLoop.
            
            IF intSelectedWorkZoneID <> 0 AND Location.WorkZoneID <> intSelectedWorkZoneID THEN
               NEXT LocationLoop.   
               
            IF intSelectedBusinessUnitID <> 0 AND Location.BusinessUnitID <> intSelectedBusinessUnitID THEN
               NEXT LocationLoop.
            
            IF chrLocationRefBegins <> "" AND NOT(Location.LocationRef BEGINS chrLocationRefBegins) THEN 
               NEXT LocationLoop.
                        
            RUN pSetLocationRows.

         END. /*FOR EACH Location NO-LOCK */
      END. /* IF intSelectedLocationTypeID <> 0 THEN*/
      ELSE IF chrLocationRefBegins <> "" THEN
      DO:
         LocationBeginsLoop:
         FOR EACH Location NO-LOCK
            WHERE Location.LocationRef BEGINS chrLocationRefBegins
            BY    Location.ACTIVE DESC
            BY    Location.LocationRef:
            
            IF Location.BusinessUnitID <> 0 AND NOT fCanViewBusinessUnit(intGblSessionID, Location.BusinessUnitID) THEN 
               NEXT LocationBeginsLoop.
            
            FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
            
            IF intSelectedLocationTypeID <> 0 AND Location.LocationTypeID <> intSelectedLocationTypeID THEN 
               NEXT LocationBeginsLoop.
           
            FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.
            
            FIND FIRST WorkZone OF Location NO-LOCK NO-ERROR.
            
            IF intSelectedAisleID <> 0 AND Location.AisleID <> intSelectedAisleID THEN
               NEXT LocationBeginsLoop.
               
            IF intSelectedWorkZoneID <> 0 AND Location.WorkZoneID <> intSelectedWorkZoneID THEN
               NEXT LocationBeginsLoop.   
            
            IF intSelectedBusinessUnitID <> 0 AND Location.BusinessUnitID <> intSelectedBusinessUnitID THEN
               NEXT LocationBeginsLoop. 
            
            RUN pSetLocationRows.
            
         END. /* FOR EACH Location NO-lOCK */
      END. /* ELSE IF chrSelectedLocationRefBegins <> "" THEN */
      ELSE /* All Locations */
      DO:
         LocationLoop:
         FOR EACH Location NO-LOCK /* ActiveLocationRef*/
            BY Location.ACTIVE DESC
            BY Location.LocationRef:
            
            IF Location.BusinessUnitID <> 0 AND NOT fCanViewBusinessUnit(intGblSessionID, Location.BusinessUnitID) THEN 
               NEXT LocationLoop.
            
            FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
            
            FIND FIRST Aisle OF Location NO-LOCK NO-ERROR.
            
            FIND FIRST WorkZone OF Location NO-LOCK NO-ERROR.
            
            RUN pSetLocationRows.

         END. /*FOR EACH Location NO-LOCK */
      END.
   END. /* IF NOT logFilterIsPoppedUp THEN */

   LocationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationBrowse:getErrors().
   
   /* Create a new frame */
   LocationBrowseFrame = NEW pageFrame().
   LocationBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LocationBrowseFrame:FormAction = "dbLocationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LocationBrowseFrame:formOpen("location_browse_form").
   
   /* Start the Frame Header */
   LocationBrowseFrame:insertSpacer(5).
   LocationBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LocationBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LocationBrowseFrame:frameClose().
   LocationBrowseFrame:insertSpacer(10).
   
   LocationBrowseFrame:insertHiddenField("location_browse_scroll","").
   LocationBrowseFrame:insertHiddenField("LocationID","").
   LocationBrowseFrame:insertHiddenField("LocationVersionID","").
   LocationBrowseFrame:insertHiddenField("form_name","location_browse_form").
   LocationBrowseFrame:insertHiddenField("prog_name","adLocationAdmin.p").
   LocationBrowseFrame:insertHiddenField("printerlocationlink_browse_scroll","").
   LocationBrowseFrame:insertHiddenField("popup_printerlocationlink_browse","").
   LocationBrowseFrame:insertHiddenField("popup_tasklinework_browse","").
   LocationBrowseFrame:insertHiddenField("popup_locationhistory_browse","").
   LocationBrowseFrame:insertHiddenField("PrinterLocationLinkID","").
   LocationBrowseFrame:insertHiddenField("LocationRef", chrSelectedLocationRef).
   LocationBrowseFrame:insertHiddenField("LocationRefBegins",chrLocationRefBegins).
   LocationBrowseFrame:insertHiddenField("LocationTypeID", STRING(intSelectedLocationTypeID)).
   LocationBrowseFrame:insertHiddenField("AisleID",STRING(intSelectedAisleID)).
   LocationBrowseFrame:insertHiddenField("WorkZoneID",STRING(intSelectedWorkZoneID)).
   LocationBrowseFrame:insertHiddenField("EmptyID", chrSelectedEmpty).
   LocationBrowseFrame:insertHiddenField("filtering", "no").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationBrowseFrame}
   
   LocationBrowseFrame:formClose().
   
   /* Create Button Bar */
   LocationBrowseButtons = NEW buttonBar().
   LocationBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   


   LocationBrowseButtons:addButton("location_browse_form_btn_filter",
                                   fTL("Filter"),
                                   "viewLocationFilter('location_filter_form');",
                                   "").

   LocationBrowseButtons:addButton("location_browse_form_btn_details", 
                                   fTL("Create") + " / " + fTL("Edit"), 
                                   "viewLocationDetails('location_details_form');").

   LocationBrowseButtons:addButton("location_browse_form_btn_printers",
                                   fTL("Printers"),
                                   "viewPrinterLocationLinks('location_details_form');",
                                   (IF intSelectedLocation > 0 THEN "" ELSE "Disabled")).

   LocationBrowseButtons:addButton("location_browse_form_btn_tasks",
                                   fTL("Tasks"),
                                   "viewTaskLineWorkBrowse('location_details_form');",
                                   (IF intSelectedLocation > 0 THEN "" ELSE "Disabled")).

   LocationBrowseButtons:addButton("location_browse_form_btn_history",
                                   fTL("History"),
                                   "viewLocationHistoryBrowse('location_details_form');",
                                   (IF intSelectedLocation > 0 THEN "" ELSE "Disabled")).
   
   LocationBrowseButtons:addButton("location_browse_form_btn_delete",
                                   fTL("Delete"),
                                   "confirmDeleteLocation();",
                                   (IF intSelectedLocation > 0 THEN "" ELSE "Disabled")).
   
   LocationBrowseButtons:addButton("location_browse_form_btn_excel",
                                   fTL("Excel Export"),
                                   "excelExport('" + STRING(intGblSessionID) + "_location_browse.xml')").
   
   LocationBrowseButtons:closeBar().  
   LocationBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationDetails Procedure 
PROCEDURE pLocationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "location_details_form"}
   
   ASSIGN chrDisplayFieldList  = "LocationID,LocationRef,LocationTypeID,AisleID,EaseOfAccessRanking,CountSequence,WalkSequence" 
                                    + ",Active,MaxNumDifferentParts,CountInProcess,BusinessUnitID,WorkZoneID"
          chrEditFieldList     = "LocationTypeID,AisleID,BusinessUnitID,MaxNumDifferentParts,EaseOfAccessRanking,CountSequence,WalkSequence"
                                    + ",Active,CountInProcess,WorkZoneID"
          chrNewFieldList      = "LocationRef,LocationTypeID,AisleID,MaxNumDifferentParts,EaseOfAccessRanking,CountSequence,WalkSequence,Active,BusinessUnitID"
                                 + ",WorkZoneID"
          chrRequiredFieldList = "LocationRef,LocationTypeID,AisleID,MaxNumDifferentParts"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   LocationDetailsForm = NEW dataForm("location_details_form").
   LocationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationDetailsForm:FormAction  = "dbLocationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LocationDetailsForm:FormWidth   = 580.
   LocationDetailsForm:FormHeight  = 420.
   LocationDetailsForm:FormTitle   = "Location Details".
   LocationDetailsForm:FormType    = "large".
   
   /* Column Layout */
   LocationDetailsForm:insertPaddingColumn(30).
   LocationDetailsForm:insertColumn(140).
   LocationDetailsForm:insertColumn(140).
   LocationDetailsForm:insertColumn(20).
   LocationDetailsForm:insertColumn(4).
   LocationDetailsForm:insertColumn(160).
   
   /* Fields */
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Location ID").
   LocationDetailsForm:insertTextField("LocationID", "", 110, TRUE).  
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Location Ref").
   LocationDetailsForm:insertTextField("LocationRef", "", 150, TRUE).  
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Location Type").
   LocationDetailsForm:insertComboField("LocationTypeID", "", 110, TRUE).  
   FOR EACH LocationType NO-LOCK /* idx=ActiveListingSequence*/
      BY LocationType.Active DESC
      BY LocationType.ListingSequence:
      
      LocationDetailsForm:insertComboPairs("LocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).
   END.
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Aisle").
   LocationDetailsForm:insertComboField("AisleID", "", 110, TRUE).
   LocationDetailsForm:insertComboPairs("AisleID","0","None").
   FOR EACH Aisle NO-LOCK /* idx=ActiveListingSequence*/
      BY Aisle.Active DESC:
      
      LocationDetailsForm:insertComboPairs("AisleID", STRING(Aisle.AisleID), Aisle.AisleName).
   END.
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("BusinessUnit").
   LocationDetailsForm:insertComboField("BusinessUnitID", "", 150, TRUE).
   
   /* Populate the BusinessUnit*/
   LocationDetailsForm:insertComboPairs("BusinessUnitID", "0", "All Business Units").
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK
      BY BusinessUnit.ACTIVE DESC:
         
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
        NEXT BusinessUnitLoop.
      
      LocationDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID), BusinessUnit.UnitName).
   END.
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel(fTL("Active")). 
   LocationDetailsForm:insertComboField("Active", "", 110, TRUE).  
   LocationDetailsForm:insertComboPairs("Active", "yes", "Active").
   LocationDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("EaseOfAccess Rank").
   LocationDetailsForm:insertTextField("EaseOfAccessRanking", "", 110, TRUE).  
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Walk Sequence").
   LocationDetailsForm:insertTextField("WalkSequence", "", 110, TRUE).  
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Count Sequence").
   LocationDetailsForm:insertTextField("CountSequence", "", 110, TRUE).  
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Max Num Parts").
   LocationDetailsForm:insertTextField("MaxNumDifferentParts", "", 110, TRUE).  
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel(fTL("Count In Process")). 
   LocationDetailsForm:insertComboField("CountInProcess", "", 160, TRUE).  
   LocationDetailsForm:insertComboPairs("CountInProcess", "no", "Count is NOT In Process").
   LocationDetailsForm:insertComboPairs("CountInProcess", "yes",  "Count is In Process").
   
   LocationDetailsForm:startRow().
   LocationDetailsForm:insertLabel("Work Zone").
   LocationDetailsForm:insertComboField("WorkZoneID", "", 150, TRUE).  
   LocationDetailsForm:insertComboPairs("WorkZoneID", "0", "None Selected").
   
   FOR EACH WorkZone NO-LOCK /* idx=ActiveListingSequence*/
      WHERE WorkZone.Active:
      
      LocationDetailsForm:insertComboPairs("WorkZoneID", STRING(WorkZone.WorkZoneID), WorkZone.WorkZoneName).
   END. /*FOR EACH WorkZone NO-LOCK*/

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pLocationDetailsFields}
   
   /* Add Hidden Fields*/
   LocationDetailsForm:insertHiddenField("location_browse_scroll", "").
   LocationDetailsForm:insertHiddenField("form_name", "location_details_form").
   LocationDetailsForm:insertHiddenField("prog_name", "adLocationAdmin.p").
   LocationDetailsForm:insertHiddenField("LocationRef", chrSelectedLocationRef).
   LocationDetailsForm:insertHiddenField("LocationRefBegins",chrLocationRefBegins).
   LocationDetailsForm:insertHiddenField("LocationTypeID", STRING(intSelectedLocationTypeID)).
   LocationDetailsForm:insertHiddenField("AisleID", STRING(intSelectedAisleID)).
   LocationDetailsForm:insertHiddenField("WorkZoneID", STRING(intSelectedWorkZoneID)).
   LocationDetailsForm:insertHiddenField("EmptyID", STRING(chrSelectedEmpty)).
   LocationDetailsForm:insertHiddenField("filtering", "no").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationDetailsForm}
   
   /* Create Button Bar */
   LocationDetailsButtons = NEW buttonBar().
   
   LocationDetailsButtons:addButton("location_details_form_btn_create", 
                                    fTL("Create"), 
                                    "createLocation('location_details_form');").

   LocationDetailsButtons:addButton("location_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateLocation('location_details_form');").
   
   LocationDetailsButtons:addButton("location_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('location_details_form_popup');").
   
   LocationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationDetailsForm:FormButtons = LocationDetailsButtons.
   
   LocationDetailsForm:endForm(). 
   
   LocationDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationDetailsFields Procedure 
PROCEDURE pLocationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         LocationDetailsForm:startRow().
         LocationDetailsForm:insertLabel(fTL("Field Label")).
         LocationDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adLocationAdmin_location_details_form.i}
      
   END CASE. /*chrOption:*/
   
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
   /*{webGetWebForm.i "location_filter_form"}*/
   
   LocationBrowseFilterForm = NEW dataForm("location_filter_form").
   LocationBrowseFilterForm:WebStream  = STREAM WebStream:HANDLE.
   LocationBrowseFilterForm:FormAction = "adLocationAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
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
   LocationBrowseFilterForm:insertTextField("LocationRef", "", 160, TRUE).  
   
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Location Ref Begins")).
   LocationBrowseFilterForm:insertTextField("LocationRefBegins", "", 160, TRUE). 

   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Location Type")).   
   LocationBrowseFilterForm:insertComboField("LocationTypeID", "", 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("LocationTypeID", "0", "All Types...").      
   /* Populate the LocationTypes */
   FOR EACH LocationType NO-LOCK
      BY LocationType.ACTIVE DESC
      BY LocationType.ListingSequence:      
      
      LocationBrowseFilterForm:insertComboPairs("LocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).
   END.
   
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Aisle")).   
   LocationBrowseFilterForm:insertComboField("AisleID", "", 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("AisleID", "0", "All Aisles...").      
    
   /* Populate the Aisles */
   FOR EACH Aisle NO-LOCK
      BY Aisle.ACTIVE DESC:
      
      LocationBrowseFilterForm:insertComboPairs("AisleID", STRING(Aisle.AisleID), Aisle.AisleName).
   END.
   
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("WorkZone")).   
   LocationBrowseFilterForm:insertComboField("WorkZoneID", "", 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("WorkZoneID", "0", "All WorkZones...").      
    
   /* Populate the WorkZones */
   FOR EACH WorkZone NO-LOCK
      WHERE WorkZone.Active:
             
      LocationBrowseFilterForm:insertComboPairs("WorkZoneID", STRING(WorkZone.WorkZoneID), WorkZone.WorkZoneName).
   END.
   
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("Empty")).
   LocationBrowseFilterForm:insertComboField("EmptyID", "", 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("EmptyID", "ShowAllLocations", "Show All Locations").
   LocationBrowseFilterForm:insertComboPairs("EmptyID", "ShowEmptyLocations", "Show Empty Locations Only").
   LocationBrowseFilterForm:insertComboPairs("EmptyID", "ShowOccupiedLocations", "Show Occupied Locations Only").
  
   LocationBrowseFilterForm:startRow().
   LocationBrowseFilterForm:insertLabel(fTL("BusinessUnit")).   
   LocationBrowseFilterForm:insertComboField("BusinessUnitID", "", 140, TRUE).
   LocationBrowseFilterForm:insertComboPairs("BusinessUnitID", "0", "All BusinessUnits...").      
   
   /* Populate the BusinessUnit*/
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK
      BY BusinessUnit.ACTIVE DESC:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
        NEXT BusinessUnitLoop.
      
      LocationBrowseFilterForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID), BusinessUnit.UnitName).
   END.

   /* Add Hidden Fields*/
   LocationBrowseFilterForm:insertHiddenField("form_name", "location_filter_form").
   LocationBrowseFilterForm:insertHiddenField("prog_name", "adLocationAdmin.p").
   LocationBrowseFilterForm:insertHiddenField("filtering", "no").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationBrowseFilterForm}
   
   /* Create Button Bar */
   LocationBrowseFilterButtons = NEW buttonBar().
   
   LocationBrowseFilterButtons:addButton("location_filter_form_btn_search", 
                                         fTL("Filter"), 
                                         "filterLocations()").
   
   LocationBrowseFilterButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   LocationBrowseFilterForm:FormButtons = LocationBrowseFilterButtons.
   
   LocationBrowseFilterForm:endForm(). 
   LocationBrowseFilterForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationHistoryBrowse Procedure 
PROCEDURE pLocationHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   LocationHistoryBrowseForm           = NEW dataForm("locationhistory_browse_form").
   LocationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   LocationHistoryBrowseForm:FormWidth  = 860.
   LocationHistoryBrowseForm:FormHeight = 530.
   LocationHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE Location THEN " for Location: " 
                                          + STRING(Location.LocationRef) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ID:" 
                                          + STRING(Location.LocationID) ELSE "").
   LocationHistoryBrowseForm:FormType   = "xxl_large".
   LocationHistoryBrowse                = NEW browseTable("locationhistory_browse").
   LocationHistoryBrowse:BrowseWidth    = 840.
   LocationHistoryBrowse:BrowseHeight   = 490.
   
   LocationHistoryBrowse:insertColumn(fTL("HistoryID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LocationHistory}
   
   LocationHistoryBrowse:insertColumn(fTL("Location Ref"),  80, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("Location Type"),100, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("EaseOfAccess"), 100, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("WalkSeq"),       70, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("CountSeq"),      70, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("Active"),        50, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("User"),          80, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("Operation"),     80, "CHARACTER", "left", FALSE).
   LocationHistoryBrowse:insertColumn(fTL("Created"),      100, "CHARACTER", "left", FALSE).
   
   LocationHistoryBrowse:StartBody().
   
   IF AVAILABLE Location THEN
   DO:
      /*List the LocationHistorys for the Location*/
      FOR EACH LocationHistory NO-LOCK
         WHERE LocationHistory.LocationID = Location.LocationID
         BY LocationHistory.LocationHistoryID: /*idxLocationHistoryID*/
         
         FIND LocationType  OF LocationHistory NO-LOCK NO-ERROR.
         FIND OperationType OF LocationHistory NO-LOCK NO-ERROR.
         FIND GateUser      OF LocationHistory NO-LOCK NO-ERROR.
         
         LocationHistoryBrowse:startRow(LocationHistory.LocationHistoryID, "selectHistoryRow(this," + '"' + STRING(LocationHistory.LocationHistoryID) 
                                                                     + '","locationHistory"' + ");", "").
                                                                     
         LocationHistoryBrowse:insertData(LocationHistory.LocationHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i LocationHistory}
         
         LocationHistoryBrowse:insertData(LocationHistory.LocationRef, "left").
         LocationHistoryBrowse:insertData((IF AVAILABLE LocationType THEN LocationType.TypeName ELSE ""), "left").
         LocationHistoryBrowse:insertData(LocationHistory.EaseOfAccessRanking, "left").
         LocationHistoryBrowse:insertData(LocationHistory.WalkSequence, "left").
         LocationHistoryBrowse:insertData(LocationHistory.CountSequence, "left").
         LocationHistoryBrowse:insertData(LocationHistory.ACTIVE, "left").
         LocationHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         LocationHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
         LocationHistoryBrowse:insertData(fDisplayDate&Time(LocationHistory.Created,"y/m/d H:M:S"), "right").
         
         LocationHistoryBrowse:insertHiddendata("LocationHistoryID", LocationHistory.LocationHistoryID).
         
         LocationHistoryBrowse:endRow().
      
      END. /* FOR EACH LocationHistory OF Location NO-LOCK */
   END. /*IF AVAILABLE Location THEN*/
   
   LocationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LocationHistoryBrowse:getErrors().
   
   LocationHistoryBrowseForm:insertHiddenField("LocationHistoryID","").
   LocationHistoryBrowseForm:insertHiddenField("popup_locationhistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationHistoryBrowseForm}
   
   /* Create Button Bar */
   LocationHistoryBrowseButtons = NEW buttonBar().
   
   LocationHistoryBrowseButtons:addButton("locationhistory_browse_form_btn_cancel",
                                          fTL("Details"),
                                          "viewLocationHistoryDetails('locationhistory_details_form');").
   
   LocationHistoryBrowseButtons:addButton("locationhistory_browse_form_btn_cancel",
                                          fTL("Cancel"),
                                          "disablePopup('locationhistory_browse_form_popup');").
   
   LocationHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationHistoryBrowseForm:FormBrowse  = LocationHistoryBrowse.
   LocationHistoryBrowseForm:FormButtons = LocationHistoryBrowseButtons.
   LocationHistoryBrowseForm:endForm(). 
   
   LocationHistoryBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocationHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationHistoryDetails Procedure
PROCEDURE pLocationHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "locationhistory_details_form"}
   
   chrDisplayFieldList  = "LocationHistoryID,LocationID,BusinessUnitID,LocationRef,LocationTypeID,AisleID,"
                             + "CreatedDate,CreatedHour,CreatedMins,Revision,TransactionID,OperationTypeID,"
                             + "ScannableRef,RackLevel,WalkSequence,CountSequence,EaseOfAccessRanking,GateUserID,"
                             + "CountInProcess,Active,FileID".                        
   
   LocationHistoryDetailsForm = NEW dataForm("locationhistory_details_form").
   LocationHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   LocationHistoryDetailsForm:FormWidth   = 545.
   LocationHistoryDetailsForm:FormHeight  = 440.
   LocationHistoryDetailsForm:FormTitle   = "Location History Details".
   LocationHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   LocationHistoryDetailsForm:insertPaddingColumn(40).
   LocationHistoryDetailsForm:insertColumn(110).
   LocationHistoryDetailsForm:insertColumn(120).
   LocationHistoryDetailsForm:insertColumn(20).
   LocationHistoryDetailsForm:insertColumn(4).
   LocationHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("LocationHistoryID")).
   LocationHistoryDetailsForm:insertTextField("LocationHistoryID", "", 90, TRUE).    
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("LocationID")).
   LocationHistoryDetailsForm:insertTextField("LocationID", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel("User").
   LocationHistoryDetailsForm:insertComboField("GateUserID", "", 110, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      LocationHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel("BusinessUnitID").
   LocationHistoryDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).
   LocationHistoryDetailsForm:insertComboPairs("BusinessUnitID", "0", "None Selected").
   /* Insert the Status Codes */
   FOR EACH BusinessUnit NO-LOCK
      BY Business.BusinessUnitID:
      LocationHistoryDetailsForm:insertComboPairs("BusinessUnitID", STRING(Business.BusinessUnitID), BusinessUnit.UnitName).
   END.
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("LocationRef")).
   LocationHistoryDetailsForm:insertTextField("LocationRef", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel("LocationType").
   LocationHistoryDetailsForm:insertComboField("LocationTypeID", "", 130, TRUE).
   /* Insert the Status Codes */
   FOR EACH LocationType NO-LOCK
      BY LocationType.TypeCode:
      LocationHistoryDetailsForm:insertComboPairs("LocationTypeID", STRING(LocationType.LocationTypeID), LocationType.TypeName).
   END.
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel("Aisle").
   LocationHistoryDetailsForm:insertComboField("AisleID", "", 110, TRUE).
   LocationHistoryDetailsForm:insertComboPairs("AisleID", "0", "None Selected").
   /* Insert the Status Codes */
   FOR EACH Aisle NO-LOCK
      BY Aisle.AisleName:
      LocationHistoryDetailsForm:insertComboPairs("AisleID", STRING(Aisle.AisleID), Aisle.AisleName).
   END.
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("Revision")).
   LocationHistoryDetailsForm:insertTextField("Revision", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel("OperationType").
   LocationHistoryDetailsForm:insertComboField("OperationTypeID", "", 110, TRUE).
   /* Insert the Status Codes */
   FOR EACH OperationType NO-LOCK
      BY OperationType.TypeName:
      LocationHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END.
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("ScannableRef")).
   LocationHistoryDetailsForm:insertTextField("ScannableRef", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("RackLevel")).
   LocationHistoryDetailsForm:insertTextField("RackLevel", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("WalkSequence")).
   LocationHistoryDetailsForm:insertTextField("WalkSequence", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("CountSequence")).
   LocationHistoryDetailsForm:insertTextField("CountSequence", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("EaseOfAccessRanking")).
   LocationHistoryDetailsForm:insertTextField("EaseOfAccessRanking", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("CountInProcess")).
   LocationHistoryDetailsForm:insertTextField("CountInProcess", "", 90, TRUE).
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("Active")). 
   LocationHistoryDetailsForm:insertComboField("Active", "", 110, TRUE).  
   LocationHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   LocationHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").  
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel("Created").
   LocationHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   LocationHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   LocationHistoryDetailsForm:insertLabel(":").
   LocationHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).   
   
   LocationHistoryDetailsForm:startRow().
   LocationHistoryDetailsForm:insertLabel(fTL("FileID")).
   LocationHistoryDetailsForm:insertTextField("FIleID", "", 90, TRUE).
   
   /* Add Hidden Fields*/
   LocationHistoryDetailsForm:insertHiddenField("location_browse_scroll","").
   LocationHistoryDetailsForm:insertHiddenField("popup_locationhistory_browse", "").
   LocationHistoryDetailsForm:insertHiddenField("LocationHistoryID","").
   LocationHistoryDetailsForm:insertHiddenField("form_name","locationhistory_details_form").
   LocationHistoryDetailsForm:insertHiddenField("prog_name","adLocationAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LocationHistoryDetailsForm}
   
   /* Create Button Bar */
   LocationHistoryDetailsButtons = NEW buttonBar().
   
   LocationHistoryDetailsButtons:addButton("locationhistory_details_form_btn_cancel",
                                           fTL("Cancel"),
                                           "disablePopup('locationhistory_details_form_popup');").
                                        
   LocationHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   LocationHistoryDetailsForm:FormButtons = LocationHistoryDetailsButtons.
   
   LocationHistoryDetailsForm:endForm(). 
   LocationHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pPrinterLocationLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterLocationLinkBrowse Procedure 
PROCEDURE pPrinterLocationLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   
   {webGetWebForm.i "printerlocationlink_details_form"}
   
   PrinterLocationLinkBrowseForm = NEW dataForm("printerlocationlink_browse_form").
   PrinterLocationLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   PrinterLocationLinkBrowseForm:FormAction  = "dbLocationPrinterLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PrinterLocationLinkBrowseForm:FormWidth   = 860.
   PrinterLocationLinkBrowseForm:FormHeight  = 530.
   PrinterLocationLinkBrowseForm:FormTitle   = fTL("Printers for Location") + 
                                               (IF AVAILABLE Location THEN " : " + Location.LocationRef ELSE "").
   PrinterLocationLinkBrowseForm:FormType    = "xxl_large".
   
   PrinterLocationLinkBrowse = NEW browseTable("printerlocationlink_browse").
   PrinterLocationLinkBrowse:BrowseWidth  = 840.
   PrinterLocationLinkBrowse:BrowseHeight = 490.
   
   PrinterLocationLinkBrowse:insertColumn(fTL("LinkID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PrinterLocationLink}
   
   PrinterLocationLinkBrowse:insertColumn(fTL("Printer Name"),         150, "CHARACTER", "left", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("OSNetworkPrinterName"), 160, "CHARACTER", "left", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("Active"),                80, "CHARACTER", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("Printer Type"),         100, "CHARACTER", "left", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("List Seq"),              60, "INTEGER", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("Link Active"),           80, "CHARACTER", FALSE).
   
   PrinterLocationLinkBrowse:StartBody().
   
   IF AVAILABLE Location THEN
   DO:
      /* List the PrinterLocationLinks for the Printer */
      FOR EACH PrinterLocationLink OF Location NO-LOCK
         BY PrinterLocationLink.ListingSequence
         BY PrinterLocationLink.PrinterID:
         
         FIND FIRST Printer OF PrinterLocationLink NO-LOCK NO-ERROR.

         IF AVAILABLE Printer THEN
            FIND FIRST PrinterType OF Printer NO-LOCK NO-ERROR.

         PrinterLocationLinkBrowse:startRow(PrinterLocationLink.PrinterLocationLinkID, 
                                            "selectPrinterLocationLinkRow(this," + '"' + 
                                            STRING(PrinterLocationLink.PrinterLocationLinkID) + 
                                            '","adLocationAdmin.p","printerlocationlink_browse_form"' + ");", "").
         PrinterLocationLinkBrowse:insertData(PrinterLocationLink.PrinterLocationLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PrinterLocationLink}
         
         PrinterLocationLinkBrowse:insertData(IF AVAILABLE Printer THEN Printer.PrinterName ELSE "No Printer Assigned", "left").
         PrinterLocationLinkBrowse:insertData(IF AVAILABLE Printer THEN Printer.OSNetworkPrinterName ELSE "", "left").
         PrinterLocationLinkBrowse:insertData(IF AVAILABLE Printer THEN STRING(Printer.Active,"Yes/No") ELSE "").
         PrinterLocationLinkBrowse:insertData(IF AVAILABLE PrinterType THEN 
                                                 PrinterType.TypeCode 
                                              ELSE 
                                                 "No PrinterType Assigned", "left").
         PrinterLocationLinkBrowse:insertData(STRING(PrinterLocationLink.ListingSequence), "right").
         PrinterLocationLinkBrowse:insertData(STRING(PrinterLocationLink.Active,"Yes/No")).
         
         /* Add hidden fields */
         PrinterLocationLinkBrowse:insertHiddenData("LocationID",PrinterLocationLink.LocationID).
         PrinterLocationLinkBrowse:insertHiddenData("PrinterID",(IF AVAILABLE Printer THEN STRING(Printer.PrinterID) ELSE "")).
         PrinterLocationLinkBrowse:insertHiddenData("PrinterName",(IF AVAILABLE Printer THEN Printer.PrinterName ELSE "")).
         PrinterLocationLinkBrowse:insertHiddenData("PrinterVersionID",(IF AVAILABLE Printer THEN STRING(Printer.VersionID) ELSE "")).
         PrinterLocationLinkBrowse:insertHiddenData("PrinterLocationLinkID",PrinterLocationLink.PrinterLocationLinkID).
         PrinterLocationLinkBrowse:insertHiddenData("PrinterLocationLinkVersionID",PrinterLocationLink.VersionID).
         PrinterLocationLinkBrowse:endRow().
      
      END. /* FOR EACH PrinterLocationLink OF Location NO-LOCK */
   END. /*IF AVAILABLE Location THEN*/
   
   PrinterLocationLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PrinterLocationLinkBrowse:getErrors().
   
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationRef","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterVersionID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterName","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterLocationLinkID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterLocationLinkVersionID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("location_browse_scroll","").
   PrinterLocationLinkBrowseForm:insertHiddenField("printerlocationlink_browse_scroll","").
   PrinterLocationLinkBrowseForm:insertHiddenField("popup_printerlocationlink_browse","").
   PrinterLocationLinkBrowseForm:insertHiddenField("form_name","printerlocationlink_browse_form").
   PrinterLocationLinkBrowseForm:insertHiddenField("prog_name","adLocationAdmin.p").
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationRef",chrSelectedLocationRef).
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationRefBegins",chrLocationRefBegins).
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationTypeID",STRING(intSelectedLocationTypeID)).
   PrinterLocationLinkBrowseForm:insertHiddenField("filtering","no").
   
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PrinterLocationLinkBrowseForm}
   
   /* Create Button Bar */
   PrinterLocationLinkBrowseButtons = NEW buttonBar().
   
   PrinterLocationLinkBrowseButtons:addButton("printerlocationlink_browse_form_btn_create",
                                              fTL("Create"),
                                              "createPrinterLocationLink('printerlocationlink_details_form');"). 
   
   PrinterLocationLinkBrowseButtons:addButton("printerlocationlink_browse_form_btn_view",
                                              fTL("Details"),
                                              "viewPrinterLocationLinkDetails('printerlocationlink_details_form');",
                                              (IF intSelectedPrinterLocationLink > 0 THEN "" ELSE "Disabled")).
   
   PrinterLocationLinkBrowseButtons:addButton("printerlocationlink_browse_form_btn_delete",
                                              fTL("Delete"),
                                              "confirmDeletePrinterLocationLink('printerlocationlink_browse_form');", 
                                              (IF intSelectedPrinterLocationLink > 0 THEN "" ELSE "Disabled")).
   
   PrinterLocationLinkBrowseButtons:addButton("printerlocationlink_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('printerlocationlink_browse_form_popup');").
   
   PrinterLocationLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PrinterLocationLinkBrowseForm:FormBrowse  = PrinterLocationLinkBrowse.
   PrinterLocationLinkBrowseForm:FormButtons = PrinterLocationLinkBrowseButtons.
   PrinterLocationLinkBrowseForm:endForm(). 
   
   PrinterLocationLinkBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPrinterLocationLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterLocationLinkDetails Procedure 
PROCEDURE pPrinterLocationLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "printerlocationlink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PrinterLocationLinkID,PrinterID,ListingSequence,Active"
          chrEditFieldList     = "ListingSequence,Active"
          chrNewFieldList      = "PrinterID,ListingSequence,Active"
          chrRequiredFieldList = "PrinterID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PrinterLocationLinkDetailsForm = NEW dataForm("printerlocationlink_details_form").
   PrinterLocationLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PrinterLocationLinkDetailsForm:FormAction  = "dbLocationPrinterLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PrinterLocationLinkDetailsForm:FormWidth   = 460.
   PrinterLocationLinkDetailsForm:FormHeight  = 300.
   PrinterLocationLinkDetailsForm:FormTitle   = "Printer Location Link Details".
   PrinterLocationLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PrinterLocationLinkDetailsForm:insertPaddingColumn(20).
   PrinterLocationLinkDetailsForm:insertColumn(120).
   PrinterLocationLinkDetailsForm:insertColumn(120).
   PrinterLocationLinkDetailsForm:insertColumn(30).
   PrinterLocationLinkDetailsForm:insertColumn(120).  
   
   /* Fields */
   PrinterLocationLinkDetailsForm:startRow().
   PrinterLocationLinkDetailsForm:insertLabel(fTL("Link ID")).
   PrinterLocationLinkDetailsForm:insertTextField("PrinterLocationLinkID", "", 110, TRUE).    
   
   PrinterLocationLinkDetailsForm:startRow().
   PrinterLocationLinkDetailsForm:insertLabel(fTL("Printer")).
   PrinterLocationLinkDetailsForm:insertComboField("PrinterID", "", 200, TRUE).  
   FOR EACH Printer NO-LOCK /*idx=ActiveListingSequence*/
      BY Printer.Active DESC
      BY Printer.PrinterName:
      
      PrinterLocationLinkDetailsForm:insertComboPairs("PrinterID", STRING(Printer.PrinterID), Printer.PrinterName).
   END.   
   
   PrinterLocationLinkDetailsForm:startRow().
   PrinterLocationLinkDetailsForm:insertLabel(fTL("Listing Sequence")).
   PrinterLocationLinkDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).  
   
   PrinterLocationLinkDetailsForm:startRow().
   PrinterLocationLinkDetailsForm:insertLabel(fTL("Active")). 
   PrinterLocationLinkDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PrinterLocationLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   PrinterLocationLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pPrinterLocationLinkDetailsFields}
   
   /* Add Hidden Fields*/
   PrinterLocationLinkDetailsForm:insertHiddenField("location_browse_scroll",           "").
   PrinterLocationLinkDetailsForm:insertHiddenField("popup_printerlocationlink_browse", "").
   PrinterLocationLinkDetailsForm:insertHiddenField("LocationID",                       STRING(intSelectedLocation)).
   PrinterLocationLinkDetailsForm:insertHiddenField("PrinterLocationLinkID",            STRING(intSelectedPrinterLocationLink)).
   PrinterLocationLinkDetailsForm:insertHiddenField("PrinterID",                        "").
   PrinterLocationLinkDetailsForm:insertHiddenField("form_name",                        "printerlocationlink_details_form").
   PrinterLocationLinkDetailsForm:insertHiddenField("prog_name",                        "adLocationAdmin.p").
   PrinterLocationLinkDetailsForm:insertHiddenField("LocationRef",                      chrSelectedLocationRef).
   PrinterLocationLinkDetailsForm:insertHiddenField("LocationRefBegins",                chrLocationRefBegins).
   PrinterLocationLinkDetailsForm:insertHiddenField("LocationTypeID",                   STRING(intSelectedLocationTypeID)).
   PrinterLocationLinkDetailsForm:insertHiddenField("filtering",                        "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PrinterLocationLinkDetailsForm}
   
   /* Create Button Bar */
   PrinterLocationLinkDetailsButtons = NEW buttonBar().
   
   PrinterLocationLinkDetailsButtons:addButton("printerlocationlink_details_form_btn_save", 
                                               fTL("Save"), 
                                               "updatePrinterLocationLink('printerlocationlink_details_form');").
   
   PrinterLocationLinkDetailsButtons:addButton("printerlocationlink_details_form_btn_cancel", 
                                               fTL("Cancel"), 
                                               "cancelUpdate('UserCancelled','process_mode'); " 
                                               + "disablePopup('printerlocationlink_details_form_popup');").
   PrinterLocationLinkDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   PrinterLocationLinkDetailsForm:FormButtons = PrinterLocationLinkDetailsButtons.
   
   PrinterLocationLinkDetailsForm:endForm(). 
   PrinterLocationLinkDetailsForm:displayForm(). 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPrinterLocationLinkDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterLocationLinkDetailsFields Procedure 
PROCEDURE pPrinterLocationLinkDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adLocationAdmin_printerlocationlink_details_form.i}
      
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
   
   ASSIGN chrLocationID                     = get-value("LocationID")
          intSelectedLocation               = INTEGER(chrLocationID)
          chrScrollToLocationRow            = STRING(INTEGER(get-value("location_browse_scroll"))) + ";"
          chrPrinterLocationLinkID          = get-value("PrinterLocationLinkID")
          intSelectedPrinterLocationLink    = INTEGER(chrPrinterLocationLinkID)
          chrScrollToPrinterLocationLinkRow = STRING(INTEGER(get-value("printerlocationlink_browse_scroll"))) + ";"
          chrSelectedLocationRef            = get-value('LocationRef')
          chrLocationRefBegins              = get-value('LocationRefBegins')
          intSelectedLocationTypeID         = INTEGER(get-value('LocationTypeID'))
          intSelectedAisleID                = INTEGER(get-value('AisleID'))
          intSelectedWorkZoneID             = INTEGER(get-value('WorkZoneID'))
          intSelectedBusinessUnitID         = INTEGER(get-value('BusinessUnitID'))
          intSelectedLocationHistoryID      = INTEGER(get-value('LocationHistoryID'))
          logFilterIsPoppedUp               = (get-value('filtering') <> "no").
    
   /* Process URL values */
   IF chrSelectedLocationRef <> "" THEN
   DO:
      FIND FIRST Location NO-LOCK
         WHERE Location.LocationRef = chrSelectedLocationRef NO-ERROR.
      IF AVAILABLE Location THEN
         ASSIGN intFilteredLocation = Location.LocationID
                intSelectedLocation = Location.LocationID
                chrLocationID       = STRING(Location.LocationID).
   END. /* IF chrSelectedLocationRef  THEN */


   IF logFilterIsPoppedUp THEN
     chrPopupFilters = 'viewLocationFilter("location_filter_form");'.

   IF chrLocationID <> "" THEN
      chrSelectLocationRow = 'selectLocationRow(document.getElementById("location_browse_row_' + chrLocationID + '"),"' + 
                                                chrLocationID + '");'.
   
   IF chrPrinterLocationLinkID <> "" THEN
      chrSelectPrinterLocationLinkRow = 'selectPrinterLocationLinkRow(document.getElementById("printerlocationlink_browse_row_' 
                                           + chrPrinterLocationLinkID + '"),"' + chrPrinterLocationLinkID +  '");'.
   
   IF get-value('popup_printerlocationlink_browse') = "yes" THEN 
      chrPopupPrinterLocationLinks = 'enablePopup("printerlocationlink_browse_form_popup");'.

   IF get-value('popup_tasklinework_browse') = "yes" THEN 
     chrPopupTaskLineWorks = 'enablePopup("tasklinework_browse_form_popup");'.

   IF get-value('popup_locationhistory_browse') = "yes" THEN
      chrPopupHistory   = 'enablePopup("locationhistory_browse_form_popup");'.

   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("location_browse").scrollTop=' + chrScrollToLocationRow + chrSelectLocationRow 
                             + chrSelectPrinterLocationLinkRow + chrPopupPrinterLocationLinks + chrPopupFilters
                             + chrPopupTaskLineWorks + chrSelectTaskLineWorkRow + chrPopupHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Location Admin".
   ThisPage:FrameTitle    = "Location Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("location.js").
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Program Output/Logic Start ********/
   RUN pLocationBrowse.
   
   IF intSelectedLocation <> 0 THEN
      FIND FIRST Location NO-LOCK 
         WHERE Location.LocationID = intSelectedLocation NO-ERROR.

   /******* Procedures to build Popups ********/
   RUN pLocationDetails.

   RUN pPrinterLocationLinkBrowse.
   FIND FIRST PrinterLocationLink NO-LOCK
      WHERE PrinterLocationLink.PrinterLocationLInkID = intSelectedPrinterLocationLink NO-ERROR.

   RUN pPrinterLocationLinkDetails.
                                   
   RUN pTaskLineWorkBrowse.  
   
   RUN pTaskLineWorkDetails.
   
   RUN pLocationHistoryBrowse.
   
   FIND FIRST LocationHistory NO-LOCK 
      WHERE LocationHistory.LocationHistoryID = intSelectedLocationHistoryID NO-ERROR.
   
   RUN pLocationHistoryDetails.

   RUN pLocationFilter.
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   IF chrUserInfoMessage = "" THEN
   DO:
      {&OUT} '<script>footerMessage("' + TRIM(STRING(intRecordCount)) + " Records Displayed" + '");</script>' SKIP.   
   END.

   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT LocationBrowseFrame               NO-ERROR.
   DELETE OBJECT LocationBrowse                    NO-ERROR.
   DELETE OBJECT LocationBrowseButtons             NO-ERROR.
   DELETE OBJECT LocationDetailsForm               NO-ERROR.
   DELETE OBJECT LocationDetailsButtons            NO-ERROR.
   DELETE OBJECT LocationBrowseFilterForm          NO-ERROR.
   DELETE OBJECT LocationBrowseFilterButtons       NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowseFrame    NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowse         NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowseButtons  NO-ERROR.
   DELETE OBJECT PrinterLocationLinkDetailsForm    NO-ERROR.
   DELETE OBJECT PrinterLocationLinkDetailsButtons NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowseForm     NO-ERROR.
   DELETE OBJECT TaskLineWorkBrowseForm            NO-ERROR.
   DELETE OBJECT TaskLineWorkBrowse                NO-ERROR.
   DELETE OBJECT TaskLineWorkBrowseButtons         NO-ERROR.
   DELETE OBJECT TaskLineWorkDetailsForm           NO-ERROR.
   DELETE OBJECT TaskLineWorkDetailsButtons        NO-ERROR.
   DELETE OBJECT LocationHistoryBrowseFrame        NO-ERROR.
   DELETE OBJECT LocationHistoryBrowseForm         NO-ERROR.
   DELETE OBJECT LocationHistoryBrowse             NO-ERROR.
   DELETE OBJECT LocationHistoryBrowseButtons      NO-ERROR.
   DELETE OBJECT LocationHistoryDetailsForm        NO-ERROR.
   DELETE OBJECT LocationHistoryDetailsButtons     NO-ERROR.

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
   
   LocationBrowse:startRow(Location.LocationID, "selectLocationRow(this," + '"' + STRING(Location.LocationID) + '"' + ");", "").
   LocationBrowse:insertData(Location.LocationID).

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalBrowseFields.i Location}

   LocationBrowse:insertData(Location.LocationRef, "left").
   LocationBrowse:insertData((IF AVAILABLE LocationType THEN LocationType.TypeName ELSE ""), "left").
   LocationBrowse:insertData((IF AVAILABLE Aisle THEN Aisle.AisleName ELSE ""),"left").
   LocationBrowse:insertData((IF AVAILABLE WorkZone THEN WorkZone.WorkZoneName ELSE ""),"left").
   LocationBrowse:insertData(STRING(Location.EaseOfAccessRanking)).      
   LocationBrowse:insertData(STRING(Location.WalkSequence)).      
   LocationBrowse:insertData(STRING(Location.CountSequence)).      
   LocationBrowse:insertData(STRING(Location.MaxNumDifferentParts)).  
   LocationBrowse:insertData(STRING(Location.CountInProcess,"Yes/No")).
   LocationBrowse:insertData(STRING(NOT CAN-FIND(FIRST StockPackage OF Location),"Yes/No")).
   LocationBrowse:insertData(STRING(Location.Active,"Yes/No")).
   
   /* Add hidden fields */
   LocationBrowse:insertHiddenData("LocationVersionID",Location.VersionID).

   intRecordCount = intRecordCount + 1.

   LocationBrowse:endRow().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pTaskLineWorkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTaskLineWorkBrowse Procedure 
PROCEDURE pTaskLineWorkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE VARIABLE chrUserList  AS CHARACTER NO-UNDO.
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "tasklinework_details_form"}
   
   TaskLineWorkBrowseForm = NEW dataForm("tasklinework_browse_form").
   TaskLineWorkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   TaskLineWorkBrowseForm:FormAction  = "dbTaskLineWorkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   TaskLineWorkBrowseForm:FormWidth   = 860.
   TaskLineWorkBrowseForm:FormHeight  = 530.
   TaskLineWorkBrowseForm:FormTitle   = fTL("Task Work of Location") + (IF AVAILABLE Location THEN " : " + STRING(Location.LocationRef) ELSE "").
   TaskLineWorkBrowseForm:FormType    = "xxl_large".
   
   TaskLineWorkBrowse = NEW browseTable("tasklinework_browse").
   TaskLineWorkBrowse:BrowseWidth  = 840.
   TaskLineWorkBrowse:BrowseHeight = 490.
   
   TaskLineWorkBrowse:insertColumn(fTL("TaskWork"), 90, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i TaskLineWork}
   
   TaskLineWorkBrowse:insertColumn(fTL("Part"), 100, "CHARACTER", "left", FALSE).
   TaskLineWorkBrowse:insertColumn(fTL("TaskID"), 90, "CHARACTER", "left", FALSE).
   TaskLineWorkBrowse:insertColumn(fTL("QtyAssigned"), 90, "INTEGER", FALSE).
   TaskLineWorkBrowse:insertColumn(fTL("QtyCompleted"), 90, "INTEGER", FALSE).
   TaskLineWorkBrowse:insertColumn(fTL("User Assigned"), 100, "CHARACTER", "left", FALSE).
   TaskLineWorkBrowse:insertColumn(fTL("Work Status"), 90, "CHARACTER", "left", FALSE).
   TaskLineWorkBrowse:insertColumn(fTL("Source/Target"),90, "CHARACTER", "left", FALSE).
   TaskLineWorkBrowse:StartBody().
   
   IF AVAILABLE Location THEN
   DO:
      /*List the TaskLineWork for the Location*/
      FOR EACH TaskLineWork NO-LOCK /*idxSourceLocationID*/
         WHERE (TaskLineWork.LocationID = Location.LocationID OR
                TaskLineWork.TargetLocationID = Location.LocationID)
         AND TaskLineWork.Completed = "":
         
         chrUserList = "".
         FOR EACH TaskLineWorkUserLink OF TaskLineWork NO-LOCK 
            WHERE TaskLineWorkUserLink.Completed = "", 
            EACH GateUser OF TaskLineWorkUserLink NO-LOCK:
            
            chrUserList = chrUserList + GateUser.FullName + ",".
         END.
         
         chrUserList = TRIM(chrUserList,",").
         
         IF chrUserList = "" THEN
            chrUserList = "Not Assigned".

         FIND FIRST TaskLine   OF TaskLineWork NO-LOCK NO-ERROR.
         FIND FIRST Task       OF TaskLine     NO-LOCK NO-ERROR.
         FIND FIRST Part       OF TaskLine     NO-LOCK NO-ERROR.
         FIND FIRST TaskStatus OF TaskLineWork NO-LOCK NO-ERROR.
         
         TaskLineWorkBrowse:startRow(TaskLineWork.TaskLineWorkID, 
                                     "selectTaskLineWorkRow(this," + '"' + 
                                     STRING(TaskLineWork.TaskLineWorkID) + 
                                     '","adLocationAdmin.p","tasklinework_browse_form"' + ");", "").
         TaskLineWorkBrowse:insertData(TaskLineWork.TaskLineWorkID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i TaskLineWork}
         
         TaskLineWorkBrowse:insertData(IF AVAILABLE Part THEN Part.PartRef ELSE "No Part", "left").
         TaskLineWorkBrowse:insertData((IF AVAILABLE Task THEN STRING(Task.TaskID) ELSE "No Task"), "left").
         TaskLineWorkBrowse:insertData(TaskLineWork.QtyAssigned, "right").
         TaskLineWorkBrowse:insertData(TaskLineWork.QtyCompleted, "right").
         TaskLineWorkBrowse:insertData(chrUserList, "left").
         TaskLineWorkBrowse:insertData((IF AVAILABLE TaskStatus THEN TaskStatus.StatusName ELSE ""), "left").
         TaskLineWorkBrowse:insertData((IF TaskLineWork.LocationID <> 0 THEN "Source" ELSE "Target"),"left").
         
         /* Add hidden fields */
         TaskLineWorkBrowse:insertHiddenData("LocationID",Location.LocationID).
         TaskLineWorkBrowse:insertHiddenData("LocationVersionID",Location.VersionID).
         TaskLineWorkBrowse:insertHiddenData("TaskLineWorkID",TaskLineWork.TaskLineWorkID).
         TaskLineWorkBrowse:insertHiddenData("TaskLineWorkVersionID",TaskLineWork.VersionID).
         TaskLineWorkBrowse:insertHiddenData("PartID",(IF AVAILABLE Part THEN STRING(Part.PartID) ELSE "")).
         TaskLineWorkBrowse:insertHiddenData("PartRef",(IF AVAILABLE Part THEN Part.PartRef ELSE "")).
         TaskLineWorkBrowse:insertHiddenData("PartVersionID",(IF AVAILABLE Part THEN STRING(Part.VersionID) ELSE "")).
         TaskLineWorkBrowse:endRow().
      END. /* FOR EACH TaskLine OF Task NO-LOCK, EACH TaskLineWork OF TaskLine NO-LOCK */
   END. /*IF AVAILABLE Task THEN*/
   
   TaskLineWorkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + TaskLineWorkBrowse:getErrors().
   
   TaskLineWorkBrowseForm:insertHiddenField("LocationID","").
   TaskLineWorkBrowseForm:insertHiddenField("LocationVersionID","").
   TaskLineWorkBrowseForm:insertHiddenField("TaskLineWorkID","").
   TaskLineWorkBrowseForm:insertHiddenField("TaskLineWorkVersionID","").
   TaskLineWorkBrowseForm:insertHiddenField("PartID","").
   TaskLineWorkBrowseForm:insertHiddenField("PartVersionID","").
   TaskLineWorkBrowseForm:insertHiddenField("PartRef","").
   TaskLineWorkBrowseForm:insertHiddenField("TaskStatusID","").
   TaskLineWorkBrowseForm:insertHiddenField("StatusDescr","").
   TaskLineWorkBrowseForm:insertHiddenField("location_browse_scroll","").
   TaskLineWorkBrowseForm:insertHiddenField("popup_tasklinework_browse","").
   TaskLineWorkBrowseForm:insertHiddenField("form_name","tasklinework_browse_form").
   TaskLineWorkBrowseForm:insertHiddenField("prog_name","AdLocationAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i TaskLineWorkBrowseForm}
   
   /* Create Button Bar */
   TaskLineWorkBrowseButtons = NEW buttonBar().
   
   
   TaskLineWorkBrowseButtons:addButton("tasklinework_browse_form_btn_view",
                                       fTL("View"),
                                       "viewTaskLineWorkDetails('tasklinework_details_form');",
                                       (IF intSelectedTaskLineWork > 0 THEN "" ELSE "Disabled")).
   
   TaskLineWorkBrowseButtons:addButton("tasklinework_browse_form_btn_cancel",
                                       fTL("Cancel"),
                                       "disablePopup('tasklinework_browse_form_popup');").
   
   TaskLineWorkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   TaskLineWorkBrowseForm:FormBrowse  = TaskLineWorkBrowse.
   TaskLineWorkBrowseForm:FormButtons = TaskLineWorkBrowseButtons.
   TaskLineWorkBrowseForm:endForm(). 
   
   TaskLineWorkBrowseForm:displayForm().   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pTaskLineWorkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTaskLineWorkDetails Procedure 
PROCEDURE pTaskLineWorkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "tasklinework_details_form"}
   
   ASSIGN chrDisplayFieldList  = "TaskLineWorkID,PartRef,PartDescr,QtyAssigned,QtyCompleted,GateUserID,TaskStatusID"
          chrEditFieldList     = "GateUserID,TaskStatusID"
          chrNewFieldList      = ""
          chrRequiredFieldList = "TaskLineWorkID,TaskStatusID"
          chrExtraFieldList    = "".
   
   TaskLineWorkDetailsForm = NEW dataForm("tasklinework_details_form").
   TaskLineWorkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   TaskLineWorkDetailsForm:FormAction  = "dbTaskLineWorkUpdate.p" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   TaskLineWorkDetailsForm:FormWidth   = 460.
   TaskLineWorkDetailsForm:FormHeight  = 300.
   TaskLineWorkDetailsForm:FormTitle   = "Task Part Details".
   TaskLineWorkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   TaskLineWorkDetailsForm:insertColumn(20).
   TaskLineWorkDetailsForm:insertColumn(90).
   TaskLineWorkDetailsForm:insertColumn(120).
   TaskLineWorkDetailsForm:insertColumn(30).
   TaskLineWorkDetailsForm:insertColumn(120).  
   
   /* Fields */
   TaskLineWorkDetailsForm:startRow().
   TaskLineWorkDetailsForm:insertLabel("").
   TaskLineWorkDetailsForm:insertLabel(fTL("TaskWork ID")).
   TaskLineWorkDetailsForm:insertTextField("TaskLineWorkID", "", 110, TRUE).    
   
   TaskLineWorkDetailsForm:startRow().
   TaskLineWorkDetailsForm:insertLabel("").
   TaskLineWorkDetailsForm:insertLabel(fTL("Part Ref")).
   TaskLineWorkDetailsForm:insertTextField("PartRef", "", 110, TRUE).  
   
   TaskLineWorkDetailsForm:startRow().
   TaskLineWorkDetailsForm:insertLabel("").
   TaskLineWorkDetailsForm:insertLabel(fTL("Part Descr")).
   TaskLineWorkDetailsForm:insertTextField("PartDescr", "0", 280, TRUE). 
   
   TaskLineWorkDetailsForm:startRow().
   TaskLineWorkDetailsForm:insertLabel("").
   TaskLineWorkDetailsForm:insertLabel(fTL("Qty Assigned")).
   TaskLineWorkDetailsForm:insertTextField("QtyAssigned", "0", 110, TRUE).  
   
   TaskLineWorkDetailsForm:startRow().
   TaskLineWorkDetailsForm:insertLabel("").
   TaskLineWorkDetailsForm:insertLabel(fTL("Qty Completed")).
   TaskLineWorkDetailsForm:insertTextField("QtyCompleted", "0", 110, TRUE).  
   
   TaskLineWorkDetailsForm:startRow().
   TaskLineWorkDetailsForm:insertLabel("").
   TaskLineWorkDetailsForm:insertLabel("Status").
   TaskLineWorkDetailsForm:insertComboField("TaskStatusID", "", 110, TRUE).
   /* Insert the Status Codes */
   FOR EACH TaskStatus NO-LOCK
      BY TaskStatus.Active DESC
      BY TaskStatus.ListingSequence:      
      
      TaskLineWorkDetailsForm:insertComboPairs("TaskStatusID", STRING(TaskStatus.TaskStatusID), TaskStatus.StatusName).  
   END.
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pTaskLineWorkDetailsFields}
   
   /* Add Hidden Fields*/
   TaskLineWorkDetailsForm:insertHiddenField("location_browse_scroll",    "").
   TaskLineWorkDetailsForm:insertHiddenField("popup_tasklinework_browse", "").
   TaskLineWorkDetailsForm:insertHiddenField("LocationID",                STRING(intSelectedLocation)).
   TaskLineWorkDetailsForm:insertHiddenField("PartID",                    "").
   TaskLineWorkDetailsForm:insertHiddenField("form_name",                 "tasklinework_details_form").
   TaskLineWorkDetailsForm:insertHiddenField("prog_name",                 "adLocationAdmin.p").
   TaskLineWorkDetailsForm:insertHiddenField("LocationRef",               chrSelectedLocationRef).
   TaskLineWorkDetailsForm:insertHiddenField("LocationRefBegins",         chrLocationRefBegins).
   TaskLineWorkDetailsForm:insertHiddenField("LocationTypeID",            STRING(intSelectedLocationTypeID)).
   TaskLineWorkDetailsForm:insertHiddenField("filtering",                 "no").


   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i TaskLineWorkDetailsForm}
   
   /* Create Button Bar */
   TaskLineWorkDetailsButtons = NEW buttonBar().
   
   TaskLineWorkDetailsButtons:addButton("tasklinework_details_form_btn_cancel", 
                                        fTL("Cancel"), 
                                        "cancelUpdate('UserCancelled','process_mode'); disablePopup('tasklinework_details_form_popup');").
   TaskLineWorkDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   TaskLineWorkDetailsForm:FormButtons = TaskLineWorkDetailsButtons.
   
   TaskLineWorkDetailsForm:endForm(). 
   
   TaskLineWorkDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pTaskLineWorkDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pTaskLineWorkDetailsFields Procedure 
PROCEDURE pTaskLineWorkDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adLocationAdmin_tasklinework_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

