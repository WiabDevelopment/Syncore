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
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Custom Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

/* Local Variables for Part Browse */
DEFINE VARIABLE chrPartID                                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToPartRow                               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectPartRow                                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intFilteredPart                                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedPart                                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectedSerialisedPart                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectedFinishedGoodPart                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectedComponentPart                         AS CHARACTER   NO-UNDO.

/* Local Variables for PartHistory Browse */
DEFINE VARIABLE chrPopupPartHistory                              AS CHARACTER   NO-UNDO.

/* Local Variables for Part Filter */
DEFINE VARIABLE chrPopupFilters                                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectedPartRef                               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectedTopNumParts                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedBusinessUnitID                        AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedPartTypeID                            AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedVendorID                              AS INTEGER     NO-UNDO.
DEFINE VARIABLE logFilterIsPoppedUp                              AS LOGICAL     NO-UNDO.

/* Local Variables for PartStockEntityLink Browse */
DEFINE VARIABLE chrPartStockEntityLinkID                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPartStockEntityLink                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectPartStockEntityLinkRow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedPartStockEntityLink                   AS INTEGER     NO-UNDO.

/* Local Variables for RelocatePartStockEntityRule Browse */
DEFINE VARIABLE chrFormTitle                                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrRelocatePartStockEntityRuleID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupRelocatePartStockEntityRule              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectRelocatePartStockEntityRuleRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedRelocatePartStockEntityRule           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrPopupPartStockEntityLinkHistory               AS CHARACTER   NO-UNDO.

/* Local Variables for PartDangerousGoodsLink Browse */
DEFINE VARIABLE chrPartDangerousGoodsLinkID                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupPartDangerousGoodsLink                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectPartDangerousGoodsLinkRow               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedPartDangerousGoodsLink                AS INTEGER     NO-UNDO.

/* Local Variable for PartLocationSizeFit Browse */
DEFINE VARIABLE chrPopupPartLocationSizeFit                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartLocationSizeFitID                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectPartLocationSizeFitRow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedPartLocationSizeFitID                 AS INTEGER     NO-UNDO.

/* Local Variable for PartProfileOrderStreamLink Browse */
DEFINE VARIABLE chrPopupPartProfileOrderStreamLink               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartProfileOrderStreamLinkID                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectPartProfileOrderStreamLinkRow           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedPartProfileOrderStreamLink            AS INTEGER     NO-UNDO.

DEFINE VARIABLE chrPopupPartProfileOrderStreamLinkLocation       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartProfileOrderStreamLinkLocationID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartProfileOrderStreamLinkLocationRow         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intPartProfileOrderStreamLinkLocationID          AS INTEGER     NO-UNDO.

DEFINE VARIABLE chrPopupPartProfileOrderStreamLinkHistory        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPartProfileOrderStreamLinkHistoryID           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectPartProfileOrderStreamLinkHistoryRow    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedPartProfileOrderStreamLinkHistoryID   AS INTEGER     NO-UNDO.

/* Local Variables for PartLocationSizeFitHistory Browse */
DEFINE VARIABLE chrPopupPartLocationSizeFitHistory               AS CHARACTER   NO-UNDO.

/* System options */
{getPartOptions.i}

/* Objects for Main Browse */
DEFINE VARIABLE PartBrowseFrame                                  AS pageFrame.
DEFINE VARIABLE PartBrowse                                       AS browseTable.
DEFINE VARIABLE PartBrowseButtons                                AS buttonBar.
DEFINE VARIABLE PartBrowseMoreButtons                            AS buttonBar.
DEFINE VARIABLE PartDetailsForm                                  AS dataForm.
DEFINE VARIABLE PartDetailsButtons                               AS buttonBar.
DEFINE VARIABLE PartProfileForm                                  AS dataForm.
DEFINE VARIABLE PartProfileButtons                               AS buttonBar.

/* Objects for PartLocationSizeFit Browse */
DEFINE VARIABLE PartLocationSizeFitBrowse                        AS browseTable.
DEFINE VARIABLE PartLocationSizeFitBrowseForm                    AS dataForm.
DEFINE VARIABLE PartLocationSizeFitBrowseButtons                 AS buttonBar.
DEFINE VARIABLE PartLocationSizeFitDetailsForm                   AS dataForm.
DEFINE VARIABLE PartLocationSizeFitDetailsButtons                AS buttonBar.

/* Objects for PartProfileOrderStreamLink Browse */
DEFINE VARIABLE PartProfileOrderStreamLinkBrowse                 AS browseTable.
DEFINE VARIABLE PartProfileOrderStreamLinkBrowseForm             AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamLinkBrowseButtons          AS buttonBar.
DEFINE VARIABLE PartProfileOrderStreamLinkDetailsForm            AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamLinkDetailsButtons         AS buttonBar.


/* Objects for PartProfileOrderStreamLinkHistory Browse */
DEFINE VARIABLE PartProfileOrderStreamLinkHistoryBrowse          AS browseTable.
DEFINE VARIABLE PartProfileOrderStreamLinkHistoryBrowseForm      AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamLinkHistoryBrowseButtons   AS buttonBar.
DEFINE VARIABLE PartProfileOrderStreamLinkHistoryDetailsForm     AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamLinkHistoryDetailsButtons  AS buttonBar.

/* Objects for PartLocationSizeFit History */
DEFINE VARIABLE PartLocationSizeFitHistoryBrowse                 AS browseTable.
DEFINE VARIABLE PartLocationSizeFitHistoryBrowseButtons          AS buttonBar.
DEFINE VARIABLE PartLocationSizeFitHistoryBrowseForm             AS dataForm.

/* Objects for Filter */
DEFINE VARIABLE PartBrowseFilterForm                             AS dataForm.
DEFINE VARIABLE PartBrowseFilterButtons                          AS buttonBar.

/* Objects for Part History */
DEFINE VARIABLE PartHistoryBrowse                                AS browseTable.
DEFINE VARIABLE PartHistoryBrowseButtons                         AS buttonBar.
DEFINE VARIABLE PartHistoryBrowseForm                            AS dataForm.
DEFINE VARIABLE PartHistoryDetailsButtons                        AS buttonBar.
DEFINE VARIABLE PartHistoryDetailsForm                           AS dataForm. 

/* Objects for PartStockEntityLink */
DEFINE VARIABLE PartStockEntityLinkBrowse                        AS browseTable.
DEFINE VARIABLE PartStockEntityLinkBrowseButtons                 AS buttonBar.
DEFINE VARIABLE PartStockEntityLinkBrowseForm                    AS dataForm.
DEFINE VARIABLE PartStockEntityLinkDetailsButtons                AS buttonBar.
DEFINE VARIABLE PartStockEntityLinkDetailsForm                   AS dataForm.

/* Objects for RelocatePartStockEntityRule */
DEFINE VARIABLE RelocatePartStockEntityRuleBrowse                AS browseTable.
DEFINE VARIABLE RelocatePartStockEntityRuleBrowseButtons         AS buttonBar.
DEFINE VARIABLE RelocatePartStockEntityRuleBrowseForm            AS dataForm.
DEFINE VARIABLE RelocatePartStockEntityRuleDetailsButtons        AS buttonBar.
DEFINE VARIABLE RelocatePartStockEntityRuleDetailsForm           AS dataForm.

/* PartStockEntityLinkHistory Objects */
DEFINE VARIABLE PartStockEntityLinkHistoryBrowse                 AS browseTable.
DEFINE VARIABLE PartStockEntityLinkHistoryBrowseButtons          AS buttonBar.
DEFINE VARIABLE PartStockEntityLinkHistoryBrowseForm             AS dataForm.

/* Objects for PartDangerousGoodsLink */
DEFINE VARIABLE PartDangerousGoodsLinkBrowse                     AS browseTable.
DEFINE VARIABLE PartDangerousGoodsLinkBrowseButtons              AS buttonBar.
DEFINE VARIABLE PartDangerousGoodsLinkBrowseForm                 AS dataForm.
DEFINE VARIABLE PartDangerousGoodsLinkDetailsButtons             AS buttonBar.
DEFINE VARIABLE PartDangerousGoodsLinkDetailsForm                AS dataForm.

/* Objects for PartProfileOrderStreamLinkHistory Browse */
DEFINE VARIABLE PartProfileOrderStreamLinkLocationBrowse         AS browseTable.
DEFINE VARIABLE PartProfileOrderStreamLinkLocationBrowseForm     AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamLinkLocationBrowseButtons  AS buttonBar.
DEFINE VARIABLE PartProfileOrderStreamLinkLocationDetailsForm    AS dataForm.
DEFINE VARIABLE PartProfileOrderStreamLinkLocationDetailsButtons AS buttonBar.


DEFINE BUFFER parentStockEntity FOR StockEntity.

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
         HEIGHT             = 16.67
         WIDTH              = 62.
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

&IF DEFINED(EXCLUDE-pPartBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartBrowse Procedure 
PROCEDURE pPartBrowse :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "part_details_form"}

   PartBrowse              = NEW browseTable("part_browse").
   PartBrowse:BrowseWidth  = 965.
   PartBrowse:BrowseHeight = 435.
   PartBrowse:ExcelExport  = TRUE.
   PartBrowse:SessionID    = intGblSessionID.
   PartBrowse:WebStream    = STREAM WebStream:HANDLE.

   /* Add in the Part Ref as first Column */
   PartBrowse:insertColumn(fTL("Part Ref"),   140, "CHARACTER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Part}
   
   PartBrowse:insertColumn(fTL("Vendor"),     100, "CHARACTER", "left", FALSE).
   PartBrowse:insertColumn(fTL("Part Type"),  100, "CHARACTER", "left", FALSE).
   PartBrowse:insertColumn(fTL("Part Descr"), 141, "CHARACTER", "left", FALSE).
   PartBrowse:insertColumn(fTL("Rank"),        40, "DECIMAL",           FALSE).
   PartBrowse:insertColumn(fTL("Serialised"),  60, "LOGICAL",           FALSE).
   PartBrowse:insertColumn(fTL("Active"),      50, "LOGICAL",           FALSE).
   
   /*Body*/
   PartBrowse:startBody().

   IF NOT logFilterIsPoppedUp THEN
   DO:
      IF intFilteredPart <> 0 THEN
      DO:
         FIND FIRST Part NO-LOCK /*idx=PartID*/
            WHERE PartID = intFilteredPart NO-ERROR.
         IF AVAILABLE Part THEN
         DO:
            FIND FIRST PartType OF Part NO-LOCK NO-ERROR. /*idx=PartID*/
            FIND FIRST Vendor   OF Part NO-LOCK NO-ERROR. /*idx=PartID*/
            FIND FIRST PartProfile OF Part NO-LOCK /*idx=PartIDCompletedHighRunnerRanking*/
               WHERE PartProfile.Completed = "" NO-ERROR.

            RUN pSetPartRows.

         END. /* IF AVAILABLE Part */
      END. /* IF intFilteredPart <> 0 */
      ELSE /* IF intFilteredPart = 0 */
      DO:
         PartLoop:
         FOR EACH Part NO-LOCK /* idx=PartRef */
            BY    Part.PartRef:
              
            IF NOT fCanViewBusinessUnit(intGblSessionID,Part.BusinessUnitID) THEN 
               NEXT PartLoop.
            
            IF intSelectedBusinessUnitID <> 0 AND
               Part.BusinessUnitID       <> intSelectedBusinessUnitID THEN
               NEXT PartLoop.

            IF intSelectedPartTypeID <> 0 AND
               Part.PartTypeID       <> intSelectedPartTypeID THEN
               NEXT PartLoop.

            IF intSelectedVendorID <> 0 AND
               Part.VendorID       <> intSelectedVendorID THEN
               NEXT PartLoop.
            
            IF chrSelectedSerialisedPart = "SerialisedParts" AND Part.SerialisedPart = NO THEN
               NEXT PartLoop.

            IF chrSelectedSerialisedPart = "NonSerialisedParts" AND Part.SerialisedPart = YES THEN
               NEXT PartLoop.

            IF chrSelectedFinishedGoodPart = "AllFinishedGoodParts" AND Part.FinishedGoodPart = NO THEN
               NEXT PartLoop.

            IF chrSelectedFinishedGoodPart = "NonFinishedGoodParts" AND Part.FinishedGoodPart = YES THEN
               NEXT PartLoop.
               
            IF chrSelectedComponentPart = "AllComponentParts" AND Part.ComponentPart = NO THEN
               NEXT PartLoop.

            IF chrSelectedComponentPart = "NonComponentParts" AND Part.ComponentPart = YES THEN
               NEXT PartLoop.   

            FIND FIRST PartProfile OF Part NO-LOCK /*idx=PartIDCompletedHighRunnerRanking*/
               WHERE PartProfile.Completed = "" NO-ERROR.
            
            IF NOT AVAILABLE PartProfile AND
               INTEGER(chrSelectedTopNumParts) <> 0 THEN
               NEXT PartLoop.
            
            IF AVAILABLE PartProfile AND
                         INTEGER(chrSelectedTopNumParts) <> 0 AND
                         PartProfile.HighRunnerRanking > INTEGER(chrSelectedTopNumParts) THEN
               NEXT PartLoop.

            FIND FIRST PartType OF Part NO-LOCK NO-ERROR. /*idx=PartID*/
            FIND FIRST Vendor   OF Part NO-LOCK NO-ERROR. /*idx=PartID*/

            RUN pSetPartRows.

         END. /* FOR EACH Part */
      END. /* IF intFilteredPart = 0 */
   END. /* IF NOT logFilterIsPoppedUp */

   PartBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PartBrowse:getErrors().

   /* Create a new frame */
   PartBrowseFrame           = NEW pageFrame().
   PartBrowseFrame:WebStream = STREAM WebStream:HANDLE.

   /* Insert a form */
   PartBrowseFrame:FormAction = "dbPartUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PartBrowseFrame:formOpen("part_browse_form").

   /* Start the Frame Header */
   PartBrowseFrame:insertSpacer(5).
   PartBrowseFrame:frameOpen(985, 480, "").

   /* This outputs the Browse Table */
   PartBrowse:displayBrowse().

   /* End the Frame Header */
   PartBrowseFrame:frameClose().
   PartBrowseFrame:insertSpacer(10).

   /* Hidden Variables for Filter */
   PartBrowseFrame:insertHiddenField("PartRef",                chrSelectedPartRef).
   PartBrowseFrame:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   PartBrowseFrame:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   PartBrowseFrame:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   PartBrowseFrame:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   PartBrowseFrame:insertHiddenField("filtering",              "no").

   /* Hidden Variables for Popup and Browse attributes */
   PartBrowseFrame:insertHiddenField("popup_partstockentitylink_browse",         "").
   PartBrowseFrame:insertHiddenField("popup_relocatepartstockentityrule_browse", "").
   PartBrowseFrame:insertHiddenField("popup_partdangerousgoodslink_browse",      "").
   PartBrowseFrame:insertHiddenField("popup_parthistory_browse",                 "").
   PartBrowseFrame:insertHiddenField("popup_partstockentitylinkhistory_browse",  "").
   PartBrowseFrame:insertHiddenField("popup_partlocationsizefit_browse",         "").
   PartBrowseFrame:insertHiddenField("popup_partlocationsizefithistory_browse",  "").
   PartBrowseFrame:insertHiddenField("popup_partprofileorderstreamlink_browse",  "").
   PartBrowseFrame:insertHiddenField("popup_partprofileorderstreamlinkhistory_browse",  "").
   PartBrowseFrame:insertHiddenField("popup_partprofileorderstreamlinklocation_browse",  "").
   
   
   
   PartBrowseFrame:insertHiddenField("form_name", "part_browse_form").
   PartBrowseFrame:insertHiddenField("prog_name", "adPartAdmin.p").
   PartBrowseFrame:insertHiddenField("part_browse_scroll", "").

   /* Hidden Variables for Key values */
   PartBrowseFrame:insertHiddenField("PartID",                        chrPartID).
   PartBrowseFrame:insertHiddenField("PartVersionID",                 "").
   PartBrowseFrame:insertHiddenField("PartStockEntityLinkID",         chrPartStockEntityLinkID).
   PartBrowseFrame:insertHiddenField("RelocatePartStockEntityRuleID", chrRelocatePartStockEntityRuleID).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartBrowseFrame}

   PartBrowseFrame:formClose().

   /* Create Button Bar */
   PartBrowseButtons           = NEW buttonBar().
   PartBrowseButtons:WebStream = STREAM WebStream:HANDLE.

   PartBrowseButtons:addButton("part_browse_form_btn_filter",
                               fTL("Filter"),
                               "viewPartFilter('part_filter_form');",
                               "").

   PartBrowseButtons:addButton("part_browse_form_btn_details",
                               fTL("Create/Edit"),
                               "viewPartDetails('part_details_form');").

   PartBrowseButtons:addButton("part_browse_form_btn_stock",
                               fTL("Stock Entities"),
                               "viewPartStockEntityLinkBrowse();",
                               "Disabled").

   PartBrowseButtons:addButton("part_browse_form_btn_danger",
                               fTL("Dangerous Goods"),
                               "viewPartDangerousGoodsLinkBrowse();",
                               "Disabled").

   PartBrowseButtons:addButton("part_browse_form_btn_history",
                               fTL("History"),
                               "viewPartHistoryBrowse();",
                               "Disabled").

   PartBrowseButtons:addButton("part_browse_form_btn_profile",
                               fTL("Profile"),
                               "viewPartProfile('part_profile_form');",
                               "Disabled").

   PartBrowseButtons:addButton("part_browse_form_btn_excel",
                               fTL("Excel Export"),
                               "excelExport('" + STRING(intGblSessionID) + "_part_browse.xml')").

   PartBrowseButtons:closeBar().
   
   PartBrowseButtons:displayButtonBar().
   
   /* Create the second Button Bar */   
   PartBrowseFrame:insertSpacer(10).
   PartBrowseMoreButtons = NEW buttonBar().
   PartBrowseMoreButtons:WebStream = STREAM WebStream:HANDLE.                               
   
   PartBrowseMoreButtons:addButton("part_browse_form_btn_locationsizefit",
                                   fTL("Location Size Fit"),
                                   "viewLocationSizeFit();",
                                   "Disabled").  
                                   
  PartBrowseMoreButtons:addButton("part_browse_form_btn_streamprofiles",
                                   fTL("Stream Profiles"),
                                   "viewStreamProfiles();",
                                   "Disabled").
   
   PartBrowseMoreButtons:closeBar().  
   PartBrowseMoreButtons:displayButtonBar().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartDangerousGoodsLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartDangerousGoodsLinkBrowse Procedure 
PROCEDURE pPartDangerousGoodsLinkBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "partdangerousgoodslink_details_form"}

   PartDangerousGoodsLinkBrowseForm            = NEW dataForm("partdangerousgoodslink_browse_form").
   PartDangerousGoodsLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   PartDangerousGoodsLinkBrowseForm:FormAction = "dbPartDangerousGoodsLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartDangerousGoodsLinkBrowseForm:FormWidth  = 580.
   PartDangerousGoodsLinkBrowseForm:FormHeight = 420.
   PartDangerousGoodsLinkBrowseForm:FormTitle  = fTL("Dangerous Goods assigned to Part: ")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;ID: " + string(Part.PartID))
                                                                       ELSE "").
   PartDangerousGoodsLinkBrowseForm:FormType   = "large".

   PartDangerousGoodsLinkBrowse              = NEW browseTable("partdangerousgoodslink_browse").
   PartDangerousGoodsLinkBrowse:BrowseWidth  = 560.
   PartDangerousGoodsLinkBrowse:BrowseHeight = 375.

   PartDangerousGoodsLinkBrowse:insertColumn(fTL("LinkID"),        100, "INTEGER"                  ).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartDangerousGoodsLink}

   PartDangerousGoodsLinkBrowse:insertColumn(fTL("UN Code"),       100, "CHARACTER",  "LEFT", FALSE).
   PartDangerousGoodsLinkBrowse:insertColumn(fTL("Factor Per KG"), 110, "INTEGER",   "RIGHT", FALSE).
   PartDangerousGoodsLinkBrowse:insertColumn(fTL("Part Weight %"), 110, "INTEGER",   "RIGHT", FALSE).
    
   PartDangerousGoodsLinkBrowse:StartBody().
   
   IF AVAILABLE Part THEN
   DO:      
      FOR EACH PartDangerousGoodsLink OF Part NO-LOCK: /* idx=PartID */
      
         FIND FIRST DangerousGoods OF PartDangerousGoodsLink NO-LOCK NO-ERROR. /*idx=PartDangerousGoodsLinkID*/

         PartDangerousGoodsLinkBrowse:startRow(PartDangerousGoodsLink.PartDangerousGoodsLinkID, "selectPartDangerousGoodsLinkRow(this," + '"'
                                               + STRING(PartDangerousGoodsLink.PartDangerousGoodsLinkID) + '"' + ");", "").

         PartDangerousGoodsLinkBrowse:insertData(PartDangerousGoodsLink.PartDangerousGoodsLinkID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartDangerousGoodsLink}

         PartDangerousGoodsLinkBrowse:insertData(IF AVAILABLE DangerousGoods THEN DangerousGoods.UNCode ELSE "", "right").
         PartDangerousGoodsLinkBrowse:insertData(STRING(PartDangerousGoodsLink.FactorPerKG), "right").
         PartDangerousGoodsLinkBrowse:insertData(STRING(PartDangerousGoodsLink.PartWeightPercentage), "right").

         /* Add hidden fields */
         PartDangerousGoodsLinkBrowse:insertHiddenData("PartDangerousGoodsLinkVersionID", PartDangerousGoodsLink.VersionID).

         PartDangerousGoodsLinkBrowse:endRow().

      END. /*  FOR EACH PartDangerousGoodsLink OF Part NO-LOCK */
   END. /*IF AVAILABLE Part THEN*/

   PartDangerousGoodsLinkBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PartDangerousGoodsLinkBrowse:getErrors().

   /* Hidden Fields for Filter */
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("filtering",              "no").

   /* Hidden Variables for Popup and Browse attributes */
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("form_name", "partdangerousgoodslink_browse_form").
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("partdangerousgoodslink_browse_scroll", "").
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("popup_partdangerousgoodslink_browse",  "").

   /* Hidden Variables for Key Values */
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("PartID",                          chrPartID).
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("PartVersionID",                   IF AVAILABLE Part THEN 
                                                                                            STRING(Part.VersionID) ELSE "").
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("PartDangerousGoodsLinkID",        chrPartDangerousGoodsLinkID).
   PartDangerousGoodsLinkBrowseForm:insertHiddenField("PartDangerousGoodsLinkVersionID", "").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartDangerousGoodsLinkBrowseForm}

   /* Create Button Bar */
   PartDangerousGoodsLinkBrowseButtons = NEW buttonBar().

   PartDangerousGoodsLinkBrowseButtons:addButton("partdangerousgoodslink_browse_form_btn_create",
                                                 fTL("Create"),
                                                 "createPartDangerousGoodsLink('partdangerousgoodslink_details_form');").

   PartDangerousGoodsLinkBrowseButtons:addButton("partdangerousgoodslink_browse_form_btn_view",
                                                 fTL("Details"),
                                                 "viewPartDangerousGoodsLink('partdangerousgoodslink_details_form');",
                                                 "Disabled").

   PartDangerousGoodsLinkBrowseButtons:addButton("partdangerousgoodslink_browse_form_btn_cancel",
                                                 fTL("Cancel"),
                                                 "disablePopup('partdangerousgoodslink_browse_form_popup');").

   PartDangerousGoodsLinkBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartDangerousGoodsLinkBrowseForm:FormBrowse  = PartDangerousGoodsLinkBrowse.
   PartDangerousGoodsLinkBrowseForm:FormButtons = PartDangerousGoodsLinkBrowseButtons.

   PartDangerousGoodsLinkBrowseForm:endForm().

   PartDangerousGoodsLinkBrowseForm:displayForm().
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartDangerousGoodsLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartDangerousGoodsLinkDetails Procedure 
PROCEDURE pPartDangerousGoodsLinkDetails :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "partdangerousgoodslink_details_form"}

   ASSIGN chrDisplayFieldList  = "PartDangerousGoodsLinkID,PartID,DangerousGoodsID,FactorPerKG,"
                                    + "PartWeightPercentage"
          chrEditFieldList     = "FactorPerKG,PartWeightPercentage"
          chrNewFieldList      = "DangerousGoodsID,FactorPerKG,PartWeightPercentage"
          chrRequiredFieldList = "PartID,DangerousGoodsID,FactorPerKG"
          chrExtraFieldList    = ""
          chrValidateFieldList = "FactorPerKG:DECIMAL,PartWeightPercentage:INTEGER".

   PartDangerousGoodsLinkDetailsForm           = NEW dataForm("partdangerousgoodslink_details_form").
   PartDangerousGoodsLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.

   PartDangerousGoodsLinkDetailsForm:FormAction = "dbPartDangerousGoodsLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartDangerousGoodsLinkDetailsForm:FormWidth  = 460.
   PartDangerousGoodsLinkDetailsForm:FormHeight = 300.
   PartDangerousGoodsLinkDetailsForm:FormTitle  = "Part Dangerous Goods Details".
   PartDangerousGoodsLinkDetailsForm:FormType   = "medium".

   /* Column Layout */
   PartDangerousGoodsLinkDetailsForm:insertPaddingColumn(40).
   PartDangerousGoodsLinkDetailsForm:insertColumn(120).
   PartDangerousGoodsLinkDetailsForm:insertColumn(120).
   PartDangerousGoodsLinkDetailsForm:insertColumn(30).
   PartDangerousGoodsLinkDetailsForm:insertColumn(90).

   /* Fields */
   PartDangerousGoodsLinkDetailsForm:startRow().
   PartDangerousGoodsLinkDetailsForm:insertLabel(fTL("Link ID")).
   PartDangerousGoodsLinkDetailsForm:insertTextField("PartDangerousGoodsLinkID", "", 110, TRUE).

   PartDangerousGoodsLinkDetailsForm:startRow().
   PartDangerousGoodsLinkDetailsForm:insertLabel(fTL("Part ID")).
   PartDangerousGoodsLinkDetailsForm:insertTextField("PartID", "0", 110, TRUE).

   PartDangerousGoodsLinkDetailsForm:startRow().
   PartDangerousGoodsLinkDetailsForm:insertLabel(fTL("DangerousGoods")).
   PartDangerousGoodsLinkDetailsForm:insertComboField("DangerousGoodsID", "", 110, TRUE).
   /* Insert DangerousGoods */
   FOR EACH DangerousGoods NO-LOCK: 

      PartDangerousGoodsLinkDetailsForm:insertComboPairs("DangerousGoodsID", 
                                                         STRING(DangerousGoods.DangerousGoodsID), 
                                                         DangerousGoods.UNCode).
   END.

   PartDangerousGoodsLinkDetailsForm:startRow().
   PartDangerousGoodsLinkDetailsForm:insertLabel(fTL("Factor Per KG")).
   PartDangerousGoodsLinkDetailsForm:insertTextField("FactorPerKG", "0", 110, TRUE).

   PartDangerousGoodsLinkDetailsForm:startRow().
   PartDangerousGoodsLinkDetailsForm:insertLabel(fTL("Part Weight %")).
   PartDangerousGoodsLinkDetailsForm:insertTextField("PartWeightPercentage", "0", 110, TRUE).

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pPartDangerousGoodsLinkDetailsFields}

   /* Hidden Fields for Filter */
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("filtering",              "no").

   /* Hidden Variables for Popup and Browse attributes */
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("popup_partdangerousgoodslink_browse", "").
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("form_name", "partdangerousgoodslink_details_form").
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("partdangerousgoodslink_browse_scroll", "").

   /* Hidden Variables for Key Values */
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("PartID",                   chrPartID).
   PartDangerousGoodsLinkDetailsForm:insertHiddenField("PartDangerousGoodsLinkID", chrPartDangerousGoodsLinkID).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartDangerousGoodsLinkDetailsForm}

   /* Create Button Bar */
   PartDangerousGoodsLinkDetailsButtons = NEW buttonBar().

   PartDangerousGoodsLinkDetailsButtons:addButton("partdangerousgoodslink_details_form_btn_save",
                                                  fTL("Save"),
                                                  "updatePartDangerousGoodsLink('partdangerousgoodslink_details_form');").

   PartDangerousGoodsLinkDetailsButtons:addButton("partdangerousgoodslink_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "cancelUpdate('UserCancelled','process_mode');
                                                  disablePopup('partdangerousgoodslink_details_form_popup');").

   PartDangerousGoodsLinkDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartDangerousGoodsLinkDetailsForm:FormButtons = PartDangerousGoodsLinkDetailsButtons.

   PartDangerousGoodsLinkDetailsForm:endForm().

   PartDangerousGoodsLinkDetailsForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartDetails Procedure 
PROCEDURE pPartDetails :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   ASSIGN chrDisplayFieldList  = "PartID,PartRef,UpcCode,PartTypeID,VendorID,PartDescr,UnitWeight,UnitValue,SerialisedPart"
                                    + ",MinimumStockLevel,ShelfLife,DefaultStockEntityID,HighRunnerRanking,Active,BusinessUnitID"
                                    + ",PartStatusID,InventoryPart,FinishedGoodPart,ComponentPart,VendorPartRef,CommitSerialsWithoutScanning"
                                    + ",WeightVolumetric,Depth,Height,Width,PartCategoryID"
          chrEditFieldList     = "PartTypeID,PartDescr,UpcCode,UnitWeight,DefaultStockEntityID,UnitValue,SerialisedPart"
                                    + ",MinimumStockLevel,ShelfLife,HighRunnerRanking,Active,InventoryPart,FinishedGoodPart"
                                    + ",ComponentPart,VendorPartRef,CommitSerialsWithoutScanning"
                                    + ",WeightVolumetric,Depth,Height,Width,PartCategoryID"
          chrNewFieldList      = "PartRef,PartTypeID,UpcCode,VendorID,PartDescr,UnitWeight,UnitValue,SerialisedPart"
                                    + ",MinimumStockLevel,ShelfLife,DefaultStockEntityID,HighRunnerRanking,Active,BusinessUnitID"
                                    + ",InventoryPart,FinishedGoodPart,ComponentPart,VendorPartRef,CommitSerialsWithoutScanning"
                                    + ",WeightVolumetric,Depth,Height,Width,PartCategoryID"
          chrRequiredFieldList = "PartRef,PartTypeID,VendorID,UnitValue,PartDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ShelfLife:INTEGER,MinimumStockLevel:INTEGER,WeightVolumetric:DECIMAL,Depth:DECIMAL"
                                 + ",Height:DECIMAL,Width:DECIMAL".
                                 
   PartDetailsForm           = NEW dataForm("part_details_form").
   PartDetailsForm:WebStream = STREAM WebStream:HANDLE.

   PartDetailsForm:FormAction = "dbPartUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartDetailsForm:FormWidth  = 860.
   PartDetailsForm:FormHeight = 530.
   PartDetailsForm:FormTitle  = fTL("Part Details").
   PartDetailsForm:FormType   = "xxl_large".

   /* Column Layout */
   PartDetailsForm:insertPaddingColumn(40).
   PartDetailsForm:insertColumn(170).
   PartDetailsForm:insertColumn(200).
   PartDetailsForm:insertColumn(80).
   PartDetailsForm:insertColumn(200).

   /* Fields */
   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Part ID").
   PartDetailsForm:insertTextField("PartID", "", 150, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Part Ref").
   PartDetailsForm:insertTextField("PartRef", "", 150, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Vendor").
   PartDetailsForm:insertComboField("VendorID", "", 150, TRUE).
   /* Insert Vendor */
   VendorLoop:
   FOR EACH Vendor NO-LOCK /*idx=ActiveVendorName*/
      WHERE Vendor.Active
      BY    Vendor.VendorName:
         
      IF NOT fCanViewBusinessUnit(intGblSessionID,Vendor.BusinessUnitID) THEN
         NEXT VendorLoop.
      
      PartDetailsForm:insertComboPairs("VendorID", 
                                       STRING(Vendor.VendorID), 
                                       Vendor.VendorName).
   END.

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("BusinessUnit").
   PartDetailsForm:insertComboField("BusinessUnitID", "", 150, TRUE).
   /* Insert BusinessUnit */
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK /*idx=ActiveListingSequence*/
      WHERE BusinessUnit.Active
      BY    BusinessUnit.UnitName:
         
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.
      
      PartDetailsForm:insertComboPairs("BusinessUnitID", 
                                       STRING(BusinessUnit.BusinessUnitID), 
                                       BusinessUnit.UnitName).
   END.

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Part Type").
   PartDetailsForm:insertComboField("PartTypeID", "", 150, TRUE).
   /* Insert PartType */
   FOR EACH PartType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PartType.Active
      BY    PartType.ListingSequence:

      PartDetailsForm:insertComboPairs("PartTypeID", 
                                       STRING(PartType.PartTypeID), 
                                       PartType.TypeDescr).
   END.

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Part Description").
   PartDetailsForm:insertTextField("PartDescr", "", 350, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Unit Weight").
   PartDetailsForm:insertTextField("UnitWeight", "", 150, TRUE).
   
   /*   PartDetailsForm:startRow().*/
   PartDetailsForm:insertLabel("Width").
   PartDetailsForm:insertTextField("Width", "", 150, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Unit Value").
   PartDetailsForm:insertTextField("UnitValue", "", 150, TRUE).
   
   /*   PartDetailsForm:startRow().*/
   PartDetailsForm:insertLabel("Height").
   PartDetailsForm:insertTextField("Height", "", 150, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Minimum Stock Level").
   PartDetailsForm:insertTextField("MinimumStockLevel", "", 150, TRUE).
   
   /*   PartDetailsForm:startRow().*/
   PartDetailsForm:insertLabel("Depth").
   PartDetailsForm:insertTextField("Depth", "", 150, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel(fTL("Serialised Part")).
   PartDetailsForm:insertComboField("SerialisedPart", "", 150, TRUE).
   PartDetailsForm:insertComboPairs("SerialisedPart", "yes", "Serialised").
   PartDetailsForm:insertComboPairs("SerialisedPart", "no",  "Not Serialised").
   
   /*   PartDetailsForm:startRow().*/
   PartDetailsForm:insertLabel("Volume").
   PartDetailsForm:insertTextField("WeightVolumetric", "", 150, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Shelf Life").
   PartDetailsForm:insertTextField("ShelfLife", "", 150, TRUE).

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Default StockEntity").
   PartDetailsForm:insertComboField("DefaultStockEntityID", "", 150, TRUE).
   PartDetailsForm:insertComboPairs("DefaultStockEntityID", "0", "None Selected...").
   /* Insert StockEntity */
   StockEntityLoop:
   FOR EACH StockEntity NO-LOCK /*idx=ActiveListingSequence*/
      WHERE StockEntity.Active
      AND   StockEntity.VirtualEntity = FALSE
      BY    StockEntity.ListingSequence:

      PartDetailsForm:insertComboPairs("DefaultStockEntityID", 
                                       STRING(StockEntity.StockEntityID), 
                                       StockEntity.EntityName).
   END.

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("High Runner Rank").
   PartDetailsForm:insertTextField("HighRunnerRanking", "", 150, TRUE).
   
   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel(fTL("Finished Good Part")).
   PartDetailsForm:insertComboField("FinishedGoodPart", "", 150, TRUE).
   PartDetailsForm:insertComboPairs("FinishedGoodPart", "yes", "Yes").
   PartDetailsForm:insertComboPairs("FinishedGoodPart", "no",  "No").
   
   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel(fTL("Component Part")).
   PartDetailsForm:insertComboField("ComponentPart", "", 150, TRUE).
   PartDetailsForm:insertComboPairs("ComponentPart", "yes", "Yes").
   PartDetailsForm:insertComboPairs("ComponentPart", "no",  "No").
   
   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel(fTL("Inventory Part")).
   PartDetailsForm:insertComboField("InventoryPart", "", 150, TRUE).
   PartDetailsForm:insertComboPairs("InventoryPart", "yes", "Inventory Part").
   PartDetailsForm:insertComboPairs("InventoryPart", "no",  "Not Inventory Part").
   
   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel(fTL("Active")).
   PartDetailsForm:insertComboField("Active", "", 150, TRUE).
   PartDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel(fTL("Commit Serials w/o Scanning")).
   PartDetailsForm:insertComboField("CommitSerialsWithoutScanning", "", 150, TRUE).
   PartDetailsForm:insertComboPairs("CommitSerialsWithoutScanning", "yes", "Yes").
   PartDetailsForm:insertComboPairs("CommitSerialsWithoutScanning", "no",  "No").

   PartDetailsForm:startRow().
   PartDetailsForm:insertLabel("Part Status").
   PartDetailsForm:insertComboField("PartStatusID", "", 150, TRUE).
   /* Insert PartStatus */
   FOR EACH PartStatus NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PartStatus.Active:

      PartDetailsForm:insertComboPairs("PartStatusID", 
                                       STRING(PartStatus.PartStatusID), 
                                       PartStatus.StatusCode).
   END.
   
   PartDetailsForm:insertLabel("Part Category").
   PartDetailsForm:insertComboField("PartCategoryID", "", 150, TRUE).
   FIND FIRST PartCategory OF Part NO-LOCK NO-ERROR. /*idx=PartID*/
   FOR EACH PartCategory NO-LOCK /*idx=PartCategoryID*/
      BY PartCategory.PartCategoryID:
      PartDetailsForm:insertComboPairs("PartCategoryID", STRING(PartCategory.PartCategoryID), PartCategory.CategoryName).
   END.

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pPartDetailsFields}

   /* Add Hidden Fields*/
   PartDetailsForm:insertHiddenField("part_browse_scroll", "").
   PartDetailsForm:insertHiddenField("form_name", "part_details_form").
   PartDetailsForm:insertHiddenField("prog_name", "adPartAdmin.p").

   /* Hidden Variables for Filter */
   PartDetailsForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   PartDetailsForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   PartDetailsForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   PartDetailsForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   PartDetailsForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   PartDetailsForm:insertHiddenField("filtering",              "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartDetailsForm}

   /* Create Button Bar */
   PartDetailsButtons = NEW buttonBar().

   PartDetailsButtons:addButton("part_browse_form_btn_fai",
                                fTL("FAI Part"),
                                "confirmFaiPart('part_browse_form');",
                                "Disabled").   

   PartDetailsButtons:addButton("part_details_form_btn_create",
                                fTL("Create"),
                                "createPart('part_details_form');").   

   PartDetailsButtons:addButton("part_details_form_btn_save",
                                fTL("Save"),
                                "updatePart('part_details_form');").

   PartDetailsButtons:addButton("part_details_form_btn_cancel",
                                fTL("Cancel"),
                                "cancelUpdate('UserCancelled','process_mode'); 
                                disablePopup('part_details_form_popup');").

   PartDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartDetailsForm:FormButtons = PartDetailsButtons.

   PartDetailsForm:endForm().

   PartDetailsForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartDetailsFields Procedure 
PROCEDURE pPartDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.

   CASE chrOption:

      WHEN "UpcCode" THEN
      DO:
         PartDetailsForm:startRow().
         PartDetailsForm:insertLabel(fTL("UPC Code")).
         PartDetailsForm:insertTextField("UpcCode", "", 200, TRUE).
      END. /*WHEN "FieldName" THEN*/

      WHEN "EanCode" THEN
      DO:
         PartDetailsForm:startRow().
         PartDetailsForm:insertLabel(fTL("EAN Code")).
         PartDetailsForm:insertTextField("EanCode", "", 150, TRUE).
      END. /*WHEN "FieldName" THEN*/

      WHEN "ItfCode" THEN
      DO:
         PartDetailsForm:startRow().
         PartDetailsForm:insertLabel(fTL("ITF Code")).
         PartDetailsForm:insertTextField("ItfCode", "", 200, TRUE).
      END. /*WHEN "FieldName" THEN*/

      WHEN "SerialPrefix" THEN
      DO:
         PartDetailsForm:startRow().
         PartDetailsForm:insertLabel(fTL("Serial Prefix")).
         PartDetailsForm:insertTextField("SerialPrefix", "", 200, TRUE).
      END. /*WHEN "FieldName" THEN*/

      WHEN "VendorPartRef" THEN
      DO:
         PartDetailsForm:startRow().
         PartDetailsForm:insertLabel(fTL("Vendor PartRef")).
         PartDetailsForm:insertTextField("VendorPartRef", "", 150, TRUE).
      END. /*WHEN "VendorPartRef" THEN*/

      /* This will be held in customer specific code repository */
      {adPartAdmin_part_details_form.i}

   END CASE. /*chrOption:*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartFai) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartFai Procedure 
PROCEDURE pPartFai :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartFilter Procedure 
PROCEDURE pPartFilter :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   PartBrowseFilterForm            = NEW dataForm("part_filter_form").
   PartBrowseFilterForm:WebStream  = STREAM WebStream:HANDLE.
   PartBrowseFilterForm:FormAction = "adPartAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartBrowseFilterForm:FormWidth  = 460.
   PartBrowseFilterForm:FormHeight = 300.
   PartBrowseFilterForm:FormTitle  = "Part Filter".
   PartBrowseFilterForm:FormType   = "medium".

   /* Column Layout */
   PartBrowseFilterForm:insertPaddingColumn(10).
   PartBrowseFilterForm:insertColumn(100).
   PartBrowseFilterForm:insertColumn(140).
   PartBrowseFilterForm:insertColumn(90).
   PartBrowseFilterForm:insertColumn(140).

   /* Fields */
   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Part Ref")).
   PartBrowseFilterForm:insertTextField("PartRef", chrSelectedPartRef, 200, TRUE).

   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Top Ranked")).
   PartBrowseFilterForm:insertTextField("TopNumParts", chrSelectedTopNumParts, 110, TRUE).

   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Vendor")).
   PartBrowseFilterForm:insertComboField("FilteredVendorID", STRING(intSelectedVendorID), 140, TRUE).
   PartBrowseFilterForm:insertComboPairs("FilteredVendorID", "0", "All Vendors...").
   /* Insert Vendor */
   VendorLoop:
   FOR EACH Vendor NO-LOCK /*idx=ActiveListingSequence*/
      WHERE Vendor.Active
      BY    Vendor.ListingSequence:

      IF NOT fCanViewBusinessUnit(intGblSessionID,Vendor.BusinessUnitID) THEN 
         NEXT VendorLoop.
      
      PartBrowseFilterForm:insertComboPairs("FilteredVendorID", 
                                            STRING(Vendor.VendorID), 
                                            Vendor.VendorName).
   END.

   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Part Type")).
   PartBrowseFilterForm:insertComboField("FilteredPartTypeID", STRING(intSelectedPartTypeID), 140, TRUE).
   PartBrowseFilterForm:insertComboPairs("FilteredPartTypeID", "0", "All Types...").
   /* Insert PartType */
   FOR EACH PartType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PartType.Active
      BY    PartType.ListingSequence:
      
      PartBrowseFilterForm:insertComboPairs("FilteredPartTypeID", 
                                            STRING(PartType.PartTypeID), 
                                            PartType.TypeName).
   END.

   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Business Unit")).
   PartBrowseFilterForm:insertComboField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID), 140, TRUE).
   PartBrowseFilterForm:insertComboPairs("FilteredBusinessUnitID", "0", "All Business Units...").
   /* Insert BusinessUnit */
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK /*idx=ActiveListingSequence*/
      WHERE BusinessUnit.Active
      BY    BusinessUnit.ListingSequence:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.
      
      PartBrowseFilterForm:insertComboPairs("FilteredBusinessUnitID", 
                                            STRING(BusinessUnit.BusinessUnitID), 
                                            BusinessUnit.UnitName).
   END.

   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Show Parts")).
   PartBrowseFilterForm:insertComboField("SerialisedID", chrSelectedSerialisedPart, 140, TRUE).
   PartBrowseFilterForm:insertComboPairs("SerialisedID", "AllParts", "All Parts").
   PartBrowseFilterForm:insertComboPairs("SerialisedID", "SerialisedParts", "All Serialised Parts").
   PartBrowseFilterForm:insertComboPairs("SerialisedID", "NonSerialisedParts", "All Non-Serialised Parts").

   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Fin. Good Part")).
   PartBrowseFilterForm:insertComboField("FinishedGoodPartID", chrSelectedFinishedGoodPart, 140, TRUE).
   PartBrowseFilterForm:insertComboPairs("FinishedGoodPartID", "AllParts", "All Parts").
   PartBrowseFilterForm:insertComboPairs("FinishedGoodPartID", "AllFinishedGoodParts", "All Finished Good Parts").
   PartBrowseFilterForm:insertComboPairs("FinishedGoodPartID", "NonFinishedGoodParts", "All Non-Finished Good Parts").
   
   PartBrowseFilterForm:startRow().
   PartBrowseFilterForm:insertLabel(fTL("Component Part")).
   PartBrowseFilterForm:insertComboField("ComponentPartID", chrSelectedComponentPart, 140, TRUE).
   PartBrowseFilterForm:insertComboPairs("ComponentPartID", "AllParts", "All Parts").
   PartBrowseFilterForm:insertComboPairs("ComponentPartID", "AllComponentParts", "All Component Parts").
   PartBrowseFilterForm:insertComboPairs("ComponentPartID", "NonComponentParts", "All Non-Component Parts").
   
   /* Add Hidden Fields*/
   PartBrowseFilterForm:insertHiddenField("form_name", "part_filter_form").
   PartBrowseFilterForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartBrowseFilterForm:insertHiddenField("filtering", "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartBrowseFilterForm}

   /* Create Button Bar */
   PartBrowseFilterButtons = NEW buttonBar().

   PartBrowseFilterButtons:addButton("part_filter_form_btn_search",
                                     fTL("Filter"),
                                     "filterParts()").

   PartBrowseFilterButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartBrowseFilterForm:FormButtons = PartBrowseFilterButtons.

   PartBrowseFilterForm:endForm().
   
   PartBrowseFilterForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartHistoryBrowse Procedure 
PROCEDURE pPartHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "parthistory_details_form"}

   PartHistoryBrowseForm           = NEW dataForm("parthistory_browse_form").
   PartHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   PartHistoryBrowseForm:FormWidth  = 860.
   PartHistoryBrowseForm:FormHeight = 530.
   PartHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE Part THEN " for Part: "
                                           + STRING(Part.PartRef) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ID:"
                                           + STRING(Part.PartID) ELSE "").
   PartHistoryBrowseForm:FormType   = "xxl_large".

   PartHistoryBrowse              = NEW browseTable("parthistory_browse").
   PartHistoryBrowse:BrowseWidth  = 840.
   PartHistoryBrowse:BrowseHeight = 490.

   PartHistoryBrowse:insertColumn(fTL("HistoryID"),   80, "INTEGER",            FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartHistory}

   PartHistoryBrowse:insertColumn(fTL("User"),        80, "CHARACTER", "left",  FALSE).
   PartHistoryBrowse:insertColumn(fTL("Descr"),      140, "CHARACTER", "left",  FALSE).
   PartHistoryBrowse:insertColumn(fTL("Shelf Life"),  80, "INTEGER",   "right", FALSE).
   PartHistoryBrowse:insertColumn(fTL("Weight"),      70, "CHARACTER", "right", FALSE).
   PartHistoryBrowse:insertColumn(fTL("Value"),       70, "CHARACTER", "right", FALSE).
   PartHistoryBrowse:insertColumn(fTL("Operation"),   70, "CHARACTER", "left",  FALSE).
   PartHistoryBrowse:insertColumn(fTL("Created"),    130, "CHARACTER", "left",  FALSE).
   PartHistoryBrowse:insertColumn(fTL("Active"),      50, "CHARACTER",          FALSE).

   PartHistoryBrowse:StartBody().

   IF AVAILABLE Part THEN
   DO:
      /*List the PartHistorys for the Part*/
      FOR EACH PartHistory NO-LOCK
         WHERE PartHistory.PartID = Part.PartID
         BY    PartHistory.PartHistoryID: /*idx = PartByPartHistory*/

         FIND GateUser      OF PartHistory NO-LOCK NO-ERROR.
         FIND OperationType OF PartHistory NO-LOCK NO-ERROR.

         PartHistoryBrowse:startRow(PartHistory.PartHistoryID, "selectPartHistoryRow(this," + '"' 
                                      + STRING(PartHistory.PartHistoryID) + '"' + ");", "").

         PartHistoryBrowse:insertData(PartHistory.PartHistoryID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartHistory}

         PartHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         PartHistoryBrowse:insertData(PartHistory.PartDescr, "left").
         PartHistoryBrowse:insertData(STRING(PartHistory.ShelfLife), "right").
         PartHistoryBrowse:insertData(STRING(PartHistory.UnitWeight,"->,>>9.9999"), "right").
         PartHistoryBrowse:insertData(STRING(PartHistory.UnitValue,"->>>,>>9.99"), "right").
         PartHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
         PartHistoryBrowse:insertData(fDisplayDate&Time(PartHistory.Created,"y/m/d H:M:S"), "right").
         PartHistoryBrowse:insertData(STRING(PartHistory.Active, "Yes/No")).
         
         PartHistoryBrowse:endRow().

      END. /* FOR EACH PartHistory OF Part NO-LOCK, EACH PartHistory OF PartHistory NO-LOCK */
   END. /*IF AVAILABLE Part THEN*/

   PartHistoryBrowse:endTable().

   PartHistoryBrowseForm:insertHiddenField("popup_parthistory_browse", "").
   PartHistoryBrowseForm:insertHiddenField("PartHistoryID",            "").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartHistoryBrowseForm}

   /* Create Button Bar */
   PartHistoryBrowseButtons = NEW buttonBar().
   
   PartHistoryBrowseButtons:addButton("parthistory_browse_form_btn_view",
                                      fTL("View"),
                                      "viewPartHistoryDetails('parthistory_details_form');",
                                      "Disabled").

   PartHistoryBrowseButtons:addButton("parthistory_browse_form_btn_cancel",
                                      fTL("Cancel"),
                                      "disablePopup('parthistory_browse_form_popup');").

   PartHistoryBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartHistoryBrowseForm:FormBrowse  = PartHistoryBrowse.
   PartHistoryBrowseForm:FormButtons = PartHistoryBrowseButtons.
   
   PartHistoryBrowseForm:endForm().

   PartHistoryBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartHistoryDetails Procedure 
PROCEDURE pPartHistoryDetails :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   ASSIGN chrDisplayFieldList  = "PartHistoryID,PartID,PartRef,BusinessUnitID,UpcCode,EanCode,ItfCode,PartTypeID,"
                                    + "PartDescr,VendorID,DefaultStockEntityID,UnitWeight,UnitValue,SerialPrefix,"
                                    + "SerialisedPart,Active,CommitSerialsWithoutScanning,ShelfLife,FileID,MinimumStockLevel,CreatedDate,"
                                    + "CreatedHour,CreatedMins,GateUserID,OperationTypeID,InventoryPart,FinishedGoodPart,ComponentPart"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   PartHistoryDetailsForm           = NEW dataForm("parthistory_details_form").
   PartHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   PartHistoryDetailsForm:FormWidth  = 860.
   PartHistoryDetailsForm:FormHeight = 530.
   PartHistoryDetailsForm:FormTitle  = fTL("Part History Details").
   PartHistoryDetailsForm:FormType   = "xxl_large".

   /* Column Layout */
   PartHistoryDetailsForm:insertPaddingColumn(40).
   PartHistoryDetailsForm:insertColumn(170).
   PartHistoryDetailsForm:insertColumn(120).
   PartHistoryDetailsForm:insertColumn(4).
   PartHistoryDetailsForm:insertColumn(4).

   /* Fields */
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("History ID").
   PartHistoryDetailsForm:insertTextField("PartHistoryID", "", 110, TRUE).
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Part ID").
   PartHistoryDetailsForm:insertTextField("PartID", "", 110, TRUE).   
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Part Ref").
   PartHistoryDetailsForm:insertTextField("PartRef", "", 180, TRUE).

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("BusinessUnit").
   PartHistoryDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).
   /* Insert BusinessUnit */
   FOR EACH BusinessUnit NO-LOCK /*idx=ActiveListingSequence*/
      WHERE BusinessUnit.Active
      BY    BusinessUnit.UnitName:

      PartHistoryDetailsForm:insertComboPairs("BusinessUnitID", 
                                              STRING(BusinessUnit.BusinessUnitID), 
                                              BusinessUnit.UnitName).
   END.

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("UPC Code").
   PartHistoryDetailsForm:insertTextField("UpcCode", "", 110, TRUE).

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Ean Code").
   PartHistoryDetailsForm:insertTextField("EanCode", "", 110, TRUE).
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("ITF Code").
   PartHistoryDetailsForm:insertTextField("ItfCode", "", 110, TRUE).
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Part Type").
   PartHistoryDetailsForm:insertComboField("PartTypeID", "", 110, TRUE).
   /* Insert PartType */
   FOR EACH PartType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PartType.Active:

      PartHistoryDetailsForm:insertComboPairs("PartTypeID", 
                                              STRING(PartType.PartTypeID), 
                                              PartType.TypeName).
   END.
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Part Descr").
   PartHistoryDetailsForm:insertTextField("PartDescr", "", 250, TRUE).
            
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Default Stock Entity").
   PartHistoryDetailsForm:insertComboField("DefaultStockEntityID", "", 110, TRUE).
   /* Insert StockEntity */
   FOR EACH StockEntity NO-LOCK /*idx=ActiveListingSequence*/
      WHERE StockEntity.Active:

      PartHistoryDetailsForm:insertComboPairs("DefaultStockEntityID", 
                                              STRING(StockEntity.StockEntityID), 
                                              StockEntity.EntityName).
   END.
   
    /*   PartHistoryDetailsFormForm:startRow().*/
   PartHistoryDetailsForm:insertLabel("Width").
   PartHistoryDetailsForm:insertTextField("Width", "", 150, TRUE).
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Serial Prefix").
   PartHistoryDetailsForm:insertTextField("SerialPrefix", "", 110, TRUE).
   
   /*   PartHistoryDetailsFormForm:startRow().*/
   PartHistoryDetailsForm:insertLabel("Height").
   PartHistoryDetailsForm:insertTextField("Height", "", 150, TRUE).

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel(fTL("Serialised Part")).
   PartHistoryDetailsForm:insertComboField("SerialisedPart", "", 110, TRUE).
   PartHistoryDetailsForm:insertComboPairs("SerialisedPart", "yes", "Serialised").
   PartHistoryDetailsForm:insertComboPairs("SerialisedPart", "no",  "Not Serialised").
   
   /*   PartHistoryDetailsForm:startRow().*/
   PartHistoryDetailsForm:insertLabel("Depth").
   PartHistoryDetailsForm:insertTextField("Depth", "", 150, TRUE).

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel(fTL("Finished Good Part")).
   PartHistoryDetailsForm:insertComboField("FinishedGoodPart", "", 110, TRUE).
   PartHistoryDetailsForm:insertComboPairs("FinishedGoodPart", "yes", "Yes").
   PartHistoryDetailsForm:insertComboPairs("FinishedGoodPart", "no",  "No").
   
   /*   PartHistoryDetailsForm:startRow().*/
   PartHistoryDetailsForm:insertLabel("Volume").
   PartHistoryDetailsForm:insertTextField("WeightVolumetric", "", 150, TRUE).
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel(fTL("Component Part")).
   PartHistoryDetailsForm:insertComboField("ComponentPart", "", 110, TRUE).
   PartHistoryDetailsForm:insertComboPairs("ComponentPart", "yes", "Yes").
   PartHistoryDetailsForm:insertComboPairs("ComponentPart", "no",  "No").

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel(fTL("Inventory Part")).
   PartHistoryDetailsForm:insertComboField("InventoryPart", "", 110, TRUE).
   PartHistoryDetailsForm:insertComboPairs("InventoryPart", "yes", "Inventory Part").
   PartHistoryDetailsForm:insertComboPairs("InventoryPart", "no",  "Not Inventory Part").

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel(fTL("Active")).
   PartHistoryDetailsForm:insertComboField("Active", "", 110, TRUE).
   PartHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartHistoryDetailsForm:insertComboPairs("Active", "no",  "Active").
  
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel(fTL("Commit Serials w/o Scanning")).
   PartHistoryDetailsForm:insertComboField("CommitSerialsWithoutScanning", "", 110, TRUE).
   PartHistoryDetailsForm:insertComboPairs("CommitSerialsWithoutScanning", "yes", "Yes").
   PartHistoryDetailsForm:insertComboPairs("CommitSerialsWithoutScanning", "no",  "No").

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Shelf Life").
   PartHistoryDetailsForm:insertTextField("ShelfLife", "", 110, TRUE).
      
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("FileID").
   PartHistoryDetailsForm:insertTextField("FileID", "", 110, TRUE).
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("Min Stock Level").
   PartHistoryDetailsForm:insertTextField("MinimumStockLevel", "", 110, TRUE).
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("OperationType").
   PartHistoryDetailsForm:insertComboField("OperationTypeID", "", 110, TRUE).
   /* Insert OperationType */
   FOR EACH OperationType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE OperationType.Active:
      
      PartHistoryDetailsForm:insertComboPairs("OperationTypeID", 
                                              STRING(OperationType.OperationTypeID), 
                                              OperationType.TypeCode).
   END.

   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel("User").
   PartHistoryDetailsForm:insertComboField("GateUserID", "", 110, TRUE).
   /* Insert GateUser */
   FOR EACH GateUser NO-LOCK /*idx=ActiveListingSequence*/
      WHERE GateUser.Active:
      
      PartHistoryDetailsForm:insertComboPairs("GateUserID", 
                                              STRING(GateUser.GateUserID), 
                                              GateUser.FullName).
   END.
   
   PartHistoryDetailsForm:startRow().
   PartHistoryDetailsForm:insertLabel(fTL("Created")).
   PartHistoryDetailsForm:insertDateField("CreatedDate", "", 80, TRUE).
   PartHistoryDetailsForm:insertTextField("CreatedHour", "", 18, TRUE).
   PartHistoryDetailsForm:insertLabel(":").
   PartHistoryDetailsForm:insertTextField("CreatedMins", "", 18, TRUE).

   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartHistoryDetailsForm}
   
   /* Create Button Bar */
   PartHistoryDetailsButtons = NEW buttonBar().

   PartHistoryDetailsButtons:addButton("parthistory_details_form_btn_cancel",
                                       fTL("Cancel"),
                                       "disablePopup('parthistory_details_form_popup');").
                                       
   PartHistoryDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartHistoryDetailsForm:FormButtons = PartHistoryDetailsButtons.

   PartHistoryDetailsForm:endForm().

   PartHistoryDetailsForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartLocationSizeFit) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartLocationSizeFit Procedure
PROCEDURE pPartLocationSizeFit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   PartLocationSizeFitBrowseForm            = NEW dataForm("partlocationsizefit_browse_form").
   PartLocationSizeFitBrowseForm:WebStream  = STREAM WebStream:HANDLE.

   /* Setup */
   PartLocationSizeFitBrowseForm:FormWidth  = 700.
   PartLocationSizeFitBrowseForm:FormHeight = 490.
   PartLocationSizeFitBrowseForm:FormTitle  = fTL("Location Size Fit for Part: ")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;ID: " + string(Part.PartID))
                                                                       ELSE "").
   PartLocationSizeFitBrowseForm:FormType   = "xl_large".

   PartLocationSizeFitBrowse              = NEW browseTable("partlocationsizefit_browse").
   PartLocationSizeFitBrowse:BrowseWidth  = 680.
   PartLocationSizeFitBrowse:BrowseHeight = 432.

   PartLocationSizeFitBrowse:insertColumn(fTL("PLSF ID"),             70, "INTEGER"       ).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartLocationSizeFit}

   PartLocationSizeFitBrowse:insertColumn(fTL("LocationSize"),     100, "INTEGER", FALSE).
   PartLocationSizeFitBrowse:insertColumn(fTL("MaxQty"),              70, "INTEGER", FALSE).
   PartLocationSizeFitBrowse:insertColumn(fTL("CountByUnitQty"),     110, "LOGICAL", FALSE).
   PartLocationSizeFitBrowse:insertColumn(fTL("SuitabilityRanking"), 110, "INTEGER", FALSE).
   PartLocationSizeFitBrowse:insertColumn(fTL("Sequence"),           110, "INTEGER", FALSE).
   PartLocationSizeFitBrowse:insertColumn(fTL("Active"),              70, "LOGICAL", FALSE).
    
   PartLocationSizeFitBrowse:StartBody().
   
   IF AVAILABLE Part THEN
   DO:   
      FOR EACH PartLocationSizeFit OF Part NO-LOCK: /* idx=PartID */
      
         FIND FIRST LocationSize OF PartLocationSizeFit NO-LOCK NO-ERROR. /*idx=PartLocationSizeFitID*/

         PartLocationSizeFitBrowse:startRow(PartLocationSizeFit.PartLocationSizeFitID, "selectPartLocationSizeFitRow(this," + '"'
                                               + STRING(PartLocationSizeFit.PartLocationSizeFitID) + '"' + ");", "").

         PartLocationSizeFitBrowse:insertData(PartLocationSizeFit.PartLocationSizeFitID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartLocationSizeFit}

         PartLocationSizeFitBrowse:insertData((IF AVAILABLE LocationSize THEN LocationSize.SizeName ELSE ""), "left").
         PartLocationSizeFitBrowse:insertData(PartLocationSizeFit.MaxQty).
         PartLocationSizeFitBrowse:insertData(STRING(PartLocationSizeFit.CountByUnitQty,"YES/NO")).
         PartLocationSizeFitBrowse:insertData(PartLocationSizeFit.SuitabilityRanking).
         PartLocationSizeFitBrowse:insertData(PartLocationSizeFit.ListingSequence).
         PartLocationSizeFitBrowse:insertData(STRING(PartLocationSizeFit.Active,"Yes/No")).

         /* Add hidden fields */
         PartLocationSizeFitBrowse:insertHiddenData("PartLocationSizeFitID", PartLocationSizeFit.PartLocationSizeFitID).
         PartLocationSizeFitBrowse:insertHiddenData("PartLocationSizeFitVersionID", PartLocationSizeFit.VersionID).

         PartLocationSizeFitBrowse:endRow().

      END. /*  FOR EACH PartLocationSizeFit OF Part NO-LOCK */
   END. /*IF AVAILABLE Part THEN*/

   PartLocationSizeFitBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PartLocationSizeFitBrowse:getErrors().                         

   /* Hidden Variables for Popup and Browse attributes */
   PartLocationSizeFitBrowseForm:insertHiddenField("form_name", "partlocationsizefit_browse_form").
   PartLocationSizeFitBrowseForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartLocationSizeFitBrowseForm:insertHiddenField("part_browse_scroll", "").
   PartLocationSizeFitBrowseForm:insertHiddenField("popup_partlocationsizefit_browse", "").
   PartLocationSizeFitBrowseForm:insertHiddenField("popup_partlocationsizefithistory_browse",  "").

   /* Hidden Variables for Key Values */
   PartLocationSizeFitBrowseForm:insertHiddenField("PartLocationSizeFitID",        "").
   PartLocationSizeFitBrowseForm:insertHiddenField("PartLocationSizeFitVersionID", "").
   PartLocationSizeFitBrowseForm:insertHiddenField("PartID",                       chrPartID).
   PartLocationSizeFitBrowseForm:insertHiddenField("filtering",                    "no").                            
                                  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartLocationSizeFitBrowseForm}

   /* Create Button Bar */
   PartLocationSizeFitBrowseButtons = NEW buttonBar().
   
   PartLocationSizeFitBrowseButtons:addButton("partlocationsizefit_browse_form_btn_create",
                                             fTL("Create"),
                                             "createPartLocationSizeFit('partlocationsizefit_details_form');").
   
   PartLocationSizeFitBrowseButtons:addButton("partlocationsizefit_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewPartLocationSizeFitDetails('partlocationsizefit_details_form');",
                                             "Disabled").

   PartLocationSizeFitBrowseButtons:addButton("partlocationsizefit_browse_form_btn_history",
                                             fTL("History"),
                                             "viewPartLocationSizeFitHistoryBrowse();",
                                             "Disabled").

   PartLocationSizeFitBrowseButtons:addButton("partlocationsizefit_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('partlocationsizefit_browse_form_popup');").

   PartLocationSizeFitBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartLocationSizeFitBrowseForm:FormBrowse  = PartLocationSizeFitBrowse.
   PartLocationSizeFitBrowseForm:FormButtons = PartLocationSizeFitBrowseButtons.

   PartLocationSizeFitBrowseForm:endForm().

   PartLocationSizeFitBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pPartLocationSizeFitDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartLocationSizeFitDetails Procedure
PROCEDURE pPartLocationSizeFitDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
/*   {webGetWebForm.i "serialscanrequired_details_form"}*/
   
   ASSIGN chrDisplayFieldList  = "PartLocationSizeFitID,PartID,LocationSizeID,MaxQty,CountByUnitQty,SuitabilityRanking" 
                                     + ",ListingSequence,Active"
          chrEditFieldList     = "LocationSizeID,MaxQty,CountByUnitQty,SuitabilityRanking,ListingSequence,Active"
          chrNewFieldList      = "LocationSizeID,MaxQty,CountByUnitQty,SuitabilityRanking,ListingSequence,Active"
          chrRequiredFieldList = "LocationSizeID,MaxQty,CountByUnitQty,SuitabilityRanking,ListingSequence,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "MaxQty:INTEGER,SuitabilityRanking:INTEGER,ListingSequence:INTEGER".
   
   PartLocationSizeFitDetailsForm           = NEW dataForm("partlocationsizefit_details_form").
   PartLocationSizeFitDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PartLocationSizeFitDetailsForm:FormAction = "dbPartUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PartLocationSizeFitDetailsForm:FormWidth  = 460.
   PartLocationSizeFitDetailsForm:FormHeight = 300.
   PartLocationSizeFitDetailsForm:FormTitle  = "SerialScanRequired Details".
   PartLocationSizeFitDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   PartLocationSizeFitDetailsForm:insertPaddingColumn(10).
   PartLocationSizeFitDetailsForm:insertColumn(140).
   PartLocationSizeFitDetailsForm:insertColumn(120).
   PartLocationSizeFitDetailsForm:insertColumn(20).
   PartLocationSizeFitDetailsForm:insertColumn(4).
   PartLocationSizeFitDetailsForm:insertColumn(110).
   
   /* Fields */
   PartLocationSizeFitDetailsForm:startRow().
   PartLocationSizeFitDetailsForm:insertLabel("PartLocationSizeFitID").
   PartLocationSizeFitDetailsForm:insertTextField("PartLocationSizeFitID", "", 110, TRUE).
   
   PartLocationSizeFitDetailsForm:startRow().
   PartLocationSizeFitDetailsForm:insertLabel(fTL("Part")).
   PartLocationSizeFitDetailsForm:insertComboField("PartID", "", 110, TRUE).   
   PartLocationSizeFitDetailsForm:insertComboPairs("PartID", "0","None").
   /* Insert the Part */
   FOR EACH Part NO-LOCK /*idx=PartTypeIDActive*/
      WHERE Part.Active:
       
       PartLocationSizeFitDetailsForm:insertComboPairs("PartID", STRING(Part.PartID), Part.PartRef).  
   
   END.
   
   PartLocationSizeFitDetailsForm:startRow().
   PartLocationSizeFitDetailsForm:insertLabel(fTL("LocationSize")).
   PartLocationSizeFitDetailsForm:insertComboField("LocationSizeID", "", 110, TRUE).   
   PartLocationSizeFitDetailsForm:insertComboPairs("LocationSizeID", "0","None").
   /* Insert the LocationSize */
   FOR EACH LocationSize NO-LOCK /*idx=ActiveListingSequence*/
      WHERE LocationSize.Active:
       
       PartLocationSizeFitDetailsForm:insertComboPairs("LocationSizeID", STRING(LocationSize.LocationSizeID), LocationSize.SizeName).  
   
   END.  

   PartLocationSizeFitDetailsForm:startRow().   
   PartLocationSizeFitDetailsForm:insertLabel(fTL("MaxQty")).
   PartLocationSizeFitDetailsForm:insertTextField("MaxQty", "", 110, TRUE). 

   PartLocationSizeFitDetailsForm:startRow().
   PartLocationSizeFitDetailsForm:insertLabel(fTL("CountByUnitQty")). 
   PartLocationSizeFitDetailsForm:insertComboField("CountByUnitQty", "", 110, TRUE).  
   PartLocationSizeFitDetailsForm:insertComboPairs("CountByUnitQty", "yes", "Yes").
   PartLocationSizeFitDetailsForm:insertComboPairs("CountByUnitQty", "no",  "No").
   
   PartLocationSizeFitDetailsForm:startRow().
   PartLocationSizeFitDetailsForm:insertLabel("SuitabilityRanking").
   PartLocationSizeFitDetailsForm:insertTextField("SuitabilityRanking", "", 110, TRUE).
   
   PartLocationSizeFitDetailsForm:startRow().   
   PartLocationSizeFitDetailsForm:insertLabel(fTL("Sequence")).
   PartLocationSizeFitDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).

   PartLocationSizeFitDetailsForm:startRow().
   PartLocationSizeFitDetailsForm:insertLabel(fTL("Active")). 
   PartLocationSizeFitDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PartLocationSizeFitDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartLocationSizeFitDetailsForm:insertComboPairs("Active", "no",  "Not Active"). 
   
   {webGetOptionalFormFields.i pPartLocationSizeFitDetailsFields}
   
   /* Add Hidden Fields*/
   PartLocationSizeFitDetailsForm:insertHiddenField("part_browse_scroll", "").
   PartLocationSizeFitDetailsForm:insertHiddenField("form_name", "partlocationsizefit_details_form").
   PartLocationSizeFitDetailsForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartLocationSizeFitDetailsForm:insertHiddenField("popup_partlocationsizefit_browse", "").
   PartLocationSizeFitDetailsForm:insertHiddenField("PartLocationSizeFitID", "").
   PartLocationSizeFitDetailsForm:insertHiddenField("PartLocationSizeFitVersionID", "").
   PartLocationSizeFitDetailsForm:insertHiddenField("filtering", "no").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartLocationSizeFitDetailsForm}
   
   /* Create Button Bar */
   PartLocationSizeFitDetailsButtons = NEW buttonBar().
   
   PartLocationSizeFitDetailsButtons:addButton("partlocationsizefit_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updatePartLocationSizeFit('partlocationsizefit_details_form');").
   
   PartLocationSizeFitDetailsButtons:addButton("partlocationsizefit_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('partlocationsizefit_details_form_popup');").
   
   PartLocationSizeFitDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PartLocationSizeFitDetailsForm:FormButtons = PartLocationSizeFitDetailsButtons.
   
   PartLocationSizeFitDetailsForm:endForm(). 
   
   PartLocationSizeFitDetailsForm:displayForm(). 


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pPartLocationSizeFitHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartLocationSizeFitHistoryBrowse Procedure
PROCEDURE pPartLocationSizeFitHistoryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
   FIND FIRST PartLocationSizeFit NO-LOCK 
      WHERE PartLocationSizeFit.PartLocationSizeFitID = intSelectedPartLocationSizeFitID NO-ERROR. /*idx=PartLocationSizeFitID*/
   
   PartLocationSizeFitHistoryBrowseForm = NEW dataForm("partlocationsizefithistory_browse_form").
   PartLocationSizeFitHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   PartLocationSizeFitHistoryBrowseForm:FormWidth  = 860.
   PartLocationSizeFitHistoryBrowseForm:FormHeight = 530.
   PartLocationSizeFitHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE PartLocationSizeFit THEN " for PartLocationSizeFitID: "
                                                                    + STRING(PartLocationSizeFit.PartLocationSizeFitID) ELSE "").
   PartLocationSizeFitHistoryBrowseForm:FormType   = "xxl_large".

   PartLocationSizeFitHistoryBrowse              = NEW browseTable("partlocationsizefithistory_browse").
   PartLocationSizeFitHistoryBrowse:BrowseWidth  = 840.
   PartLocationSizeFitHistoryBrowse:BrowseHeight = 490.

   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("HistoryID"),          70, "INTEGER",           FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartLocationSizeFitHistory}
  
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("User"),               90, "CHARACTER", "left", FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("Part"),               90, "CHARACTER", "left", FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("LocationSize"),       90, "CHARACTER", "left", FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("MaxQty"),             60, "INTEGER",           FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("CountByQty"),         90, "LOGICAL",           FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("Ranking"),            70, "INTEGER",           FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("Sequence"),           70, "INTEGER",           FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("Created"),           120, "CHARACTER", "left", FALSE).
   PartLocationSizeFitHistoryBrowse:insertColumn(fTL("Active"),             50, "LOGICAL",           FALSE).

   PartLocationSizeFitHistoryBrowse:StartBody().
  
   /*List the PartLocationSizeFitHistory for the Part*/
   FOR EACH PartLocationSizeFitHistory NO-LOCK
      WHERE PartLocationSizeFitHistory.PartLocationSizeFitID = intSelectedPartLocationSizeFitID
      BY    PartLocationSizeFitHistory.PartLocationSizeFitHistoryID: /*idx = PartLocationSizeFitHistoryID*/

      FIND FIRST GateUser     OF PartLocationSizeFitHistory NO-LOCK NO-ERROR. /*idx=PartLocationSizeFitHistoryID*/
      FIND FIRST Part         OF PartLocationSizeFitHistory NO-LOCK NO-ERROR. /*idx=PartLocationSizeFitHistoryID*/
      FIND FIRST LocationSize OF PartLocationSizeFitHistory NO-LOCK NO-ERROR. /*idx=PartLocationSizeFitHistoryID*/

      PartLocationSizeFitHistoryBrowse:startRow(PartLocationSizeFitHistory.PartLocationSizeFitHistoryID, "selectPartLocationSizeFitHistoryRow(this," + '"' 
                                   + STRING(PartLocationSizeFitHistory.PartLocationSizeFitHistoryID) + '"' + ");", "").

      PartLocationSizeFitHistoryBrowse:insertData(PartLocationSizeFitHistory.PartLocationSizeFitHistoryID).

      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i PartLocationSizeFitHistory}

      PartLocationSizeFitHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
      PartLocationSizeFitHistoryBrowse:insertData((IF AVAILABLE Part THEN Part.PartRef ELSE ""), "left").
      PartLocationSizeFitHistoryBrowse:insertData((IF AVAILABLE LocationSize THEN LocationSize.SizeName ELSE ""), "left").
      PartLocationSizeFitHistoryBrowse:insertData(STRING(PartLocationSizeFitHistory.MaxQty)).
      PartLocationSizeFitHistoryBrowse:insertData(STRING(PartLocationSizeFitHistory.CountByUnitQty, "Yes/No")).
      PartLocationSizeFitHistoryBrowse:insertData(STRING(PartLocationSizeFitHistory.SuitabilityRanking)).
      PartLocationSizeFitHistoryBrowse:insertData(STRING(PartLocationSizeFitHistory.ListingSequence)).
      PartLocationSizeFitHistoryBrowse:insertData(fDisplayDate&Time(PartLocationSizeFitHistory.Created,"y/m/d H:M:S"), "left").
      PartLocationSizeFitHistoryBrowse:insertData(STRING(PartLocationSizeFitHistory.Active, "Yes/No")).
      
      PartLocationSizeFitHistoryBrowse:endRow().

   END. /* FOR EACH PartLocationSizeFitHistory OF Part NO-LOCK, EACH PartLocationSizeFitHistory OF PartLocationSizeFitHistory NO-LOCK */

   PartLocationSizeFitHistoryBrowse:endTable().

   PartLocationSizeFitHistoryBrowseForm:insertHiddenField("popup_partlocationsizefithistory_browse", "").
   PartLocationSizeFitHistoryBrowseForm:insertHiddenField("PartLocationSizeFitHistoryID",            "").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartLocationSizeFitHistoryBrowseForm}

   /* Create Button Bar */
   PartLocationSizeFitHistoryBrowseButtons = NEW buttonBar().

   PartLocationSizeFitHistoryBrowseButtons:addButton("partlocationsizefithistory_browse_form_btn_cancel",
                                                     fTL("Cancel"),
                                                     "disablePopup('partlocationsizefithistory_browse_form_popup');").

   PartLocationSizeFitHistoryBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartLocationSizeFitHistoryBrowseForm:FormBrowse  = PartLocationSizeFitHistoryBrowse.
   PartLocationSizeFitHistoryBrowseForm:FormButtons = PartLocationSizeFitHistoryBrowseButtons.
   
   PartLocationSizeFitHistoryBrowseForm:endForm().

   PartLocationSizeFitHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pPartProfile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartProfile Procedure 
PROCEDURE pPartProfile :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   ASSIGN chrDisplayFieldList  = "PartID,AvgNumReplenLocations,AvgNumStorageLocations,AvgQtyPickedPerDay,AvgQtyStoredPerDay,Created"
                                    + ",CycleCountRanking,HighRunnerRanking,NumDaysToReplenFor,OptimumNumReplenLocations"
                                    + ",OverrideCalculatedRanking,PartProfileID,ProfileDate"
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   PartProfileForm           = NEW dataForm("part_profile_form").
   PartProfileForm:WebStream = STREAM WebStream:HANDLE.

   PartProfileForm:FormAction = "".

   /* Setup */
   PartProfileForm:FormWidth  = 580.
   PartProfileForm:FormHeight = 420.
   PartProfileForm:FormTitle  = fTL("Part Profile").
   PartProfileForm:FormType   = "large".

   /* Column Layout */
   PartProfileForm:insertPaddingColumn(40).
   PartProfileForm:insertColumn(120).
   PartProfileForm:insertColumn(320).
   PartProfileForm:insertColumn(20).
   PartProfileForm:insertColumn(4).
   PartProfileForm:insertColumn(10).

   /* Fields */
   PartProfileForm:startRow().
   PartProfileForm:insertLabel("PartID").
   PartProfileForm:insertTextField("PartID", "", 180, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("ProfileID").
   PartProfileForm:insertTextField("PartProfileID", "", 180, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("Profile Date").
   PartProfileForm:insertTextField("ProfileDate", "", 110, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("HighRunner Ranking").
   PartProfileForm:insertTextField("HighRunnerRanking", "", 180, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("CycleCount Ranking").
   PartProfileForm:insertTextField("CycleCountRanking", "", 180, TRUE).
   
   PartProfileForm:startRow().
   PartProfileForm:insertLabel("AvgNumStorageLocations").
   PartProfileForm:insertTextField("AvgNumStorageLocations", "", 110, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("AvgNumReplenLocations").
   PartProfileForm:insertTextField("AvgNumReplenLocations", "", 110, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("OptimumNumReplenLocations").
   PartProfileForm:insertTextField("OptimumNumReplenLocations", "", 110, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("AvgQtyPickedPerDay").
   PartProfileForm:insertTextField("AvgQtyPickedPerDay", "", 110, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel("AvgQtyStoredPerDay").
   PartProfileForm:insertTextField("AvgQtyStoredPerDay", "", 110, TRUE).

   PartProfileForm:startRow().
   PartProfileForm:insertLabel(fTL("OverrideCalculatedRanking")).
   PartProfileForm:insertComboField("OverrideCalculatedRanking", "", 110, TRUE).
   PartProfileForm:insertComboPairs("OverrideCalculatedRanking", "yes", "Override On").
   PartProfileForm:insertComboPairs("OverrideCalculatedRanking", "no",  "Override Off").

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   /*{webGetOptionalFormFields.i pPartProfileFields}*/

   /* Add Hidden Fields*/
   PartProfileForm:insertHiddenField("part_browse_scroll", "").
   PartProfileForm:insertHiddenField("form_name", "part_profile_form").
   PartProfileForm:insertHiddenField("prog_name", "adPartAdmin.p").

   /* Hidden Variables for Filter */
   PartProfileForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   PartProfileForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   PartProfileForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   PartProfileForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   PartProfileForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   PartProfileForm:insertHiddenField("filtering",              "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartProfileForm}

   /* Create Button Bar */
   PartProfileButtons = NEW buttonBar().

   PartProfileButtons:addButton("part_profile_form_btn_cancel",
                                fTL("Cancel"),
                                "disablePopup('part_profile_form_popup');").

   PartProfileButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartProfileForm:FormButtons = PartProfileButtons.

   PartProfileForm:endForm().

   PartProfileForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartStockEntityLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartStockEntityLinkBrowse Procedure 
PROCEDURE pPartStockEntityLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   DEFINE BUFFER parentStockEntity FOR StockEntity.

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "partstockentitylink_details_form"}

   PartStockEntityLinkBrowseForm            = NEW dataForm("partstockentitylink_browse_form").
   PartStockEntityLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   PartStockEntityLinkBrowseForm:FormAction = "dbPartStockEntityLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartStockEntityLinkBrowseForm:FormWidth  = 700.
   PartStockEntityLinkBrowseForm:FormHeight = 490.
   PartStockEntityLinkBrowseForm:FormTitle  = fTL("Stock Entities assigned to Part:")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;ID:" + string(Part.PartID))
                                                                       ELSE "").
   PartStockEntityLinkBrowseForm:FormType   = "xl_large".

   PartStockEntityLinkBrowse              = NEW browseTable("partstockentitylink_browse").
   PartStockEntityLinkBrowse:BrowseWidth  = 680.
   PartStockEntityLinkBrowse:BrowseHeight = 450.

   PartStockEntityLinkBrowse:insertColumn(fTL("ID"),             60, "INTEGER"                  ).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartStockEntityLink}

   PartStockEntityLinkBrowse:insertColumn(fTL("Stock Entity"),  110, "CHARACTER", "LEFT",  FALSE).
   PartStockEntityLinkBrowse:insertColumn(fTL("Parent Entity"), 100, "CHARACTER", "LEFT",  FALSE).
   PartStockEntityLinkBrowse:insertColumn(fTL("Sequence"),       60, "INTEGER",   "RIGHT", FALSE).
   PartStockEntityLinkBrowse:insertColumn(fTL("Height"),         60, "INTEGER",            FALSE).
   PartStockEntityLinkBrowse:insertColumn(fTL("Weight(Act)"),    70, "INTEGER",   "RIGHT", FALSE).
   PartStockEntityLinkBrowse:insertColumn(fTL("MaxQty"),         60, "INTEGER",   "RIGHT", FALSE).
   PartStockEntityLinkBrowse:insertColumn(fTL("Can Overbuild"), 100, "LOGICAL",            FALSE).
   PartStockEntityLinkBrowse:insertColumn("",                    35, "",                   FALSE). 
   
   PartStockEntityLinkBrowse:StartBody().

   IF AVAILABLE Part THEN
   DO:
      /*List the Part Lines for the Part*/
      FOR EACH PartStockEntityLink OF Part NO-LOCK: /* idx=PartID */

         FIND FIRST StockEntity OF PartStockEntityLink NO-LOCK NO-ERROR.

         FIND FIRST parentStockEntity NO-LOCK /*idx=StockEntityID*/
            WHERE parentStockEntity.StockEntityID = PartStockEntityLink.ParentStockEntityID NO-ERROR.

         PartStockEntityLinkBrowse:startRow(PartStockEntityLink.PartStockEntityLinkID, "selectPartStockEntityLinkRow(this," + '"'
                                               + STRING(PartStockEntityLink.PartStockEntityLinkID) + '"' + ");", "").

         PartStockEntityLinkBrowse:insertData(PartStockEntityLink.PartStockEntityLinkID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartStockEntityLink}

         PartStockEntityLinkBrowse:insertData((IF AVAILABLE StockEntity THEN StockEntity.EntityName ELSE ""), "left").
         PartStockEntityLinkBrowse:insertData((IF AVAILABLE parentStockEntity THEN parentStockEntity.EntityName ELSE ""), "left").
         PartStockEntityLinkBrowse:insertData(STRING(PartStockEntityLink.ListingSequence)).
         PartStockEntityLinkBrowse:insertData(STRING(PartStockEntityLink.Height), "right").
         PartStockEntityLinkBrowse:insertData(STRING(PartStockEntityLink.WeightActual), "right").
         PartStockEntityLinkBrowse:insertData(STRING(PartStockEntityLink.MaxQty), "right").
         PartStockEntityLinkBrowse:insertData(STRING(PartStockEntityLink.AllowOverbuild, "Yes/No")).
         PartStockEntityLinkBrowse:insertImage("view", "View Change History", 'viewPartStockEntityLinkHistoryBrowse("' 
                                    + STRING(PartStockEntityLink.PartStockEntityLinkID) + '", this.parentNode);').
         
         /* Add hidden fields */
         PartStockEntityLinkBrowse:insertHiddenData("PartStockEntityLinkVersionID", PartStockEntityLink.VersionID).

         PartStockEntityLinkBrowse:endRow().

      END. /*  FOR EACH PartStockEntityLink OF Part NO-LOCK */
   END. /*IF AVAILABLE Part THEN*/

   PartStockEntityLinkBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PartStockEntityLinkBrowse:getErrors().

   /* Hidden Fields for Filter */
   PartStockEntityLinkBrowseForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   PartStockEntityLinkBrowseForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   PartStockEntityLinkBrowseForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   PartStockEntityLinkBrowseForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   PartStockEntityLinkBrowseForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   PartStockEntityLinkBrowseForm:insertHiddenField("filtering",              "no").

   /* Hidden Variables for Popup and Browse attributes */
   PartStockEntityLinkBrowseForm:insertHiddenField("form_name", "partstockentitylink_browse_form").
   PartStockEntityLinkBrowseForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartStockEntityLinkBrowseForm:insertHiddenField("partstockentitylink_browse_scroll", "").
   PartStockEntityLinkBrowseForm:insertHiddenField("popup_partstockentitylink_browse", "").
   PartStockEntityLinkBrowseForm:insertHiddenField("popup_relocatepartstockentityrule_browse", "").
   PartStockEntityLinkBrowseForm:insertHiddenField("popup_partstockentitylinkhistory_browse", "").

   /* Hidden Variables for Key Values */
   PartStockEntityLinkBrowseForm:insertHiddenField("PartID",                        chrPartID).
   PartStockEntityLinkBrowseForm:insertHiddenField("PartVersionID",                 IF AVAILABLE Part THEN STRING(Part.VersionID) ELSE "").
   PartStockEntityLinkBrowseForm:insertHiddenField("PartStockEntityLinkID",         chrPartStockEntityLinkID).
   PartStockEntityLinkBrowseForm:insertHiddenField("PartStockEntityLinkVersionID",  "").
   PartStockEntityLinkBrowseForm:insertHiddenField("RelocatePartStockEntityRuleID", chrRelocatePartStockEntityRuleID).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartStockEntityLinkBrowseForm}

   /* Create Button Bar */
   PartStockEntityLinkBrowseButtons = NEW buttonBar().

   PartStockEntityLinkBrowseButtons:addButton("partstockentitylink_browse_form_btn_create",
                                              fTL("Create"),
                                              "createPartStockEntityLink('partstockentitylink_details_form');").

   PartStockEntityLinkBrowseButtons:addButton("partstockentitylink_browse_form_btn_relocate",
                                              fTL("Relocate Rules"),
                                              "viewRelocatePartStockEntityRuleBrowse();",
                                              "Disabled").

   PartStockEntityLinkBrowseButtons:addButton("partstockentitylink_browse_form_btn_view",
                                              fTL("View"),
                                              "viewPartStockEntityLink('partstockentitylink_details_form');",
                                              "Disabled").

   PartStockEntityLinkBrowseButtons:addButton("partstockentitylink_browse_form_btn_delete",
                                              fTL("Delete"),
                                              "confirmDeletePartStockEntityLink('partstockentitylink_browse_form');",
                                              "Disabled").

   PartStockEntityLinkBrowseButtons:addButton("partstockentitylink_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('partstockentitylink_browse_form_popup');").

   PartStockEntityLinkBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartStockEntityLinkBrowseForm:FormBrowse  = PartStockEntityLinkBrowse.
   PartStockEntityLinkBrowseForm:FormButtons = PartStockEntityLinkBrowseButtons.

   PartStockEntityLinkBrowseForm:endForm().

   PartStockEntityLinkBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartStockEntityLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartStockEntityLinkDetails Procedure 
PROCEDURE pPartStockEntityLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "partstockentitylink_details_form"}

   ASSIGN chrDisplayFieldList  = "PartStockEntityLinkID,PartID,ParentStockEntityID,StockEntityID,ListingSequence,Active,"
                                    + "MaxQty,WeightActual,Depth,Width,Height,AllowOverbuild"
          chrEditFieldList     = "ParentStockEntityID,ListingSequence,Active,MaxQty,WeightActual,Depth,Width,Height,AllowOverbuild"
          chrNewFieldList      = "ParentStockEntityID,StockEntityID,ListingSequence,Active,MaxQty,WeightActual,Depth,Width,Height,"
                                    + "AllowOverbuild"
          chrRequiredFieldList = "PartID,StockEntityID,ListingSequence,MaxQty,WeightActual"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER>0,MaxQty:INTEGER>0,WeightActual:DECIMAL,Depth:DECIMAL,Width:DECIMAL,"
                                    + "Height:DECIMAL".

   PartStockEntityLinkDetailsForm           = NEW dataForm("partstockentitylink_details_form").
   PartStockEntityLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.

   PartStockEntityLinkDetailsForm:FormAction = "dbPartStockEntityLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartStockEntityLinkDetailsForm:FormWidth  = 460.
   PartStockEntityLinkDetailsForm:FormHeight = 300.
   PartStockEntityLinkDetailsForm:FormTitle  = "Part - StockEntity Details".
   PartStockEntityLinkDetailsForm:FormType   = "medium".

   /* Column Layout */
   PartStockEntityLinkDetailsForm:insertPaddingColumn(20).
   PartStockEntityLinkDetailsForm:insertColumn(120).
   PartStockEntityLinkDetailsForm:insertColumn(120).
   PartStockEntityLinkDetailsForm:insertColumn(30).
   PartStockEntityLinkDetailsForm:insertColumn(90).

   /* Fields */
   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("PartStockEntityID")).
   PartStockEntityLinkDetailsForm:insertTextField("PartStockEntityLinkID", "", 110, TRUE).

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Part ID")).
   PartStockEntityLinkDetailsForm:insertTextField("PartID", "0", 110, TRUE).

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("ParentStockEntity")).
   PartStockEntityLinkDetailsForm:insertComboField("ParentStockEntityID", "", 110, TRUE).
   PartStockEntityLinkDetailsForm:insertComboPairs("ParentStockEntityID", "0", "None Assigned...").
   /* Insert StockEntity */
   StockEntityLoop:
   FOR EACH StockEntity NO-LOCK /*idx=ActiveListingSequence*/
      WHERE StockEntity.Active
      AND   StockEntity.Virtual = FALSE
      BY    StockEntity.ListingSequence:
         
      PartStockEntityLinkDetailsForm:insertComboPairs("ParentStockEntityID", 
                                                      STRING(StockEntity.StockEntityID), 
                                                      StockEntity.EntityName).
   END.

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("StockEntity")).
   PartStockEntityLinkDetailsForm:insertComboField("StockEntityID", "", 110, TRUE).
   /* Insert StockEntity */
   StockEntityLoop:
   FOR EACH StockEntity NO-LOCK /*idx=ActiveListingSequence*/
      WHERE StockEntity.Active
      AND   StockEntity.Virtual = FALSE
      BY    StockEntity.ListingSequence:

      PartStockEntityLinkDetailsForm:insertComboPairs("StockEntityID", 
                                                      STRING(StockEntity.StockEntityID), 
                                                      StockEntity.EntityName).
   END.

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Listing Sequence")).
   PartStockEntityLinkDetailsForm:insertTextField("ListingSequence", "0", 110, TRUE).

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Active")).
   PartStockEntityLinkDetailsForm:insertComboField("Active", "", 110, TRUE).
   PartStockEntityLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   PartStockEntityLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").

   /* For them to fill in @ FAI */
   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Max Qty")).
   PartStockEntityLinkDetailsForm:insertTextField("MaxQty", "0", 110, TRUE).

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Weight Actual")).
   PartStockEntityLinkDetailsForm:insertTextField("WeightActual", "0", 110, TRUE).

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Depth")).
   PartStockEntityLinkDetailsForm:insertTextField("Depth", "0", 110, TRUE).

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Width")).
   PartStockEntityLinkDetailsForm:insertTextField("Width", "0", 110, TRUE).

   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Height")).
   PartStockEntityLinkDetailsForm:insertTextField("Height", "0", 110, TRUE).
   /* End to fill in @ FAI */
 
   PartStockEntityLinkDetailsForm:startRow().
   PartStockEntityLinkDetailsForm:insertLabel(fTL("Allow Overbuild") + "?").
   PartStockEntityLinkDetailsForm:insertComboField("AllowOverbuild", "", 110, TRUE).
   PartStockEntityLinkDetailsForm:insertComboPairs("AllowOverbuild", "yes", "Allow Overbuild").
   PartStockEntityLinkDetailsForm:insertComboPairs("AllowOverbuild", "no",  "Don't Allow Overbuild").

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pPartStockEntityLinkDetailsFields}

   /* Hidden Fields for Filter */
   PartStockEntityLinkDetailsForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   PartStockEntityLinkDetailsForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   PartStockEntityLinkDetailsForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   PartStockEntityLinkDetailsForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   PartStockEntityLinkDetailsForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   PartStockEntityLinkDetailsForm:insertHiddenField("filtering",              "no").

   /* Hidden Variables for Popup and Browse attributes */
   PartStockEntityLinkDetailsForm:insertHiddenField("popup_partstockentitylink_browse", "").
   PartStockEntityLinkDetailsForm:insertHiddenField("popup_relocatepartstockentityrule_browse", "").
   PartStockEntityLinkDetailsForm:insertHiddenField("form_name", "partstockentitylink_details_form").
   PartStockEntityLinkDetailsForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartStockEntityLinkDetailsForm:insertHiddenField("partstockentitylink_browse_scroll", "").

   /* Hidden Variables for Key Values */
   PartStockEntityLinkDetailsForm:insertHiddenField("PartID",                chrPartID).
   PartStockEntityLinkDetailsForm:insertHiddenField("PartStockEntityLinkID", chrPartStockEntityLinkID).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartStockEntityLinkDetailsForm}

   /* Create Button Bar */
   PartStockEntityLinkDetailsButtons = NEW buttonBar().

   PartStockEntityLinkDetailsButtons:addButton("partstockentitylink_details_form_btn_save",
                                               fTL("Save"),
                                               "updatePartStockEntityLink('partstockentitylink_details_form');").

   PartStockEntityLinkDetailsButtons:addButton("partstockentitylink_details_form_btn_cancel",
                                               fTL("Cancel"),
                                               "cancelUpdate('UserCancelled','process_mode');
                                               disablePopup('partstockentitylink_details_form_popup');").

   PartStockEntityLinkDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartStockEntityLinkDetailsForm:FormButtons = PartStockEntityLinkDetailsButtons.

   PartStockEntityLinkDetailsForm:endForm().

   PartStockEntityLinkDetailsForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartStockEntityLinkHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartStockEntityLinkHistoryBrowse Procedure 
PROCEDURE pPartStockEntityLinkHistoryBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcesEvent which is linked to the form associated with this browse */     
   
   PartStockEntityLinkHistoryBrowseForm           = NEW dataForm("partstockentitylinkhistory_browse_form").
   PartStockEntityLinkHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   PartStockEntityLinkHistoryBrowseForm:FormWidth  = 860.
   PartStockEntityLinkHistoryBrowseForm:FormHeight = 530.
   PartStockEntityLinkHistoryBrowseForm:FormTitle  = fTL("PartStockEntityLink History") + (IF AVAILABLE PartStockEntityLink THEN ": " 
                                                                                        + STRING (PartStockEntityLink.PartStockEntityLinkID) ELSE "").
   PartStockEntityLinkHistoryBrowseForm:FormType   = "xxl_large".
   
   PartStockEntityLinkHistoryBrowse              = NEW browseTable("partstockentitylinkhistory_browse").
   PartStockEntityLinkHistoryBrowse:BrowseWidth  = 840.
   PartStockEntityLinkHistoryBrowse:BrowseHeight = 490.
   
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("ID"),            60, "INTEGER"          ).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartStockEntityLinkHistory}
   
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("Gate User"),     90, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("Created"),       90, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("ParentEntity"),  80, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("Height"),        80, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("Weight(Act)"),   90, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("MaxQty"),        80, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("CanOverBuild"), 100, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("Active"),        60, "CHARACTER", "left").
   PartStockEntityLinkHistoryBrowse:insertColumn(fTL("Operation"),     90, "CHARACTER", "left").
   
   PartStockEntityLinkHistoryBrowse:StartBody().
   
   IF AVAILABLE PartStockEntityLink THEN
   DO:
      /* List the PartStockEntityLinkHistorys for the PartStockEntityLink */
      FOR EACH PartStockEntityLinkHistory NO-LOCK /* idx=PartStockEntityLinkID */
         WHERE PartStockEntityLinkHistory.PartStockEntityLinkID = PartStockEntityLink.PartStockEntityLinkID 
         BY    PartStockEntityLinkHistory.PartStockEntityLinkID
         BY    PartStockEntityLinkHistory.Created DESCENDING:

         FIND FIRST GateUser      OF PartStockEntityLinkHistory NO-LOCK NO-ERROR.        
         FIND FIRST OperationType OF PartStockEntityLinkHistory NO-LOCK NO-ERROR.
         
         FIND FIRST parentStockEntity NO-LOCK /*idx=StockEntityID*/
            WHERE parentStockEntity.StockEntityID = PartStockEntityLinkHistory.ParentStockEntityID NO-ERROR. 
         
         PartStockEntityLinkHistoryBrowse:startRow(PartStockEntityLinkHistory.PartStockEntityLinkHistoryID, "selectPartStockEntityLinkHistoryRow(this," 
                                                   + '"' + STRING(PartStockEntityLinkHistory.PartStockEntityLinkHistoryID)
                                                   + '","adPartAdmin.p","partstockentitylinkhistory_browse_form"' + ");", "").

         PartStockEntityLinkHistoryBrowse:insertData(PartStockEntityLinkHistory.PartStockEntityLinkHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartStockEntityLinkHistory}
         
         PartStockEntityLinkHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         PartStockEntityLinkHistoryBrowse:insertData(fDisplayDate&Time(PartStockEntityLinkHistory.Created, "d/m/y H:M")).
         PartStockEntityLinkHistoryBrowse:insertData(IF AVAILABLE parentStockEntity THEN parentStockEntity.EntityCode ELSE "", "left").
         PartStockEntityLinkHistoryBrowse:insertData(PartStockEntityLinkHistory.Height).
         PartStockEntityLinkHistoryBrowsE:insertData(PartStockEntityLinkHistory.WeightActual, "left").
         PartStockEntityLinkHistoryBrowsE:insertData(PartStockEntityLinkHistory.MaxQty, "left").
         PartStockEntityLinkHistoryBrowsE:insertData(PartStockEntityLinkHistory.AllowOverBuild, "left").
         PartStockEntityLinkHistoryBrowsE:insertData(PartStockEntityLinkHistory.Active, "left").
         PartStockEntityLinkHistoryBrowsE:insertData(IF AVAILABLE OperationType THEN OperationType.TypeCode ELSE "", "left").
         
         PartStockEntityLinkHistoryBrowse:endRow().
      
      END. /* FOR EACH PartStockEntityLinkHistory OF PartStockEntityLink NO-LOCK */
   END. /*IF AVAILABLE PartStockEntityLink THEN*/
   
   PartStockEntityLinkHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + PartStockEntityLinkHistoryBrowse:getErrors().

   PartStockEntityLinkHistoryBrowseForm:insertHiddenField("popup_partstockentitylinkhistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartStockEntityLinkHistoryBrowseForm}
   
   /* Create Button Bar */
   PartStockEntityLinkHistoryBrowseButtons = NEW buttonBar().
          
   PartStockEntityLinkHistoryBrowseButtons:addButton("partstockentitylinkhistory_browse_form_btn_cancel",
                                                     fTL("Cancel"),
                                                     "disablePopup('partstockentitylinkhistory_browse_form_popup');").
   
   PartStockEntityLinkHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PartStockEntityLinkHistoryBrowseForm:FormBrowse  = PartStockEntityLinkHistoryBrowse.
   PartStockEntityLinkHistoryBrowseForm:FormButtons = PartStockEntityLinkHistoryBrowseButtons.
   
   PartStockEntityLinkHistoryBrowseForm:endForm(). 
   
   PartStockEntityLinkHistoryBrowseForm:displayForm().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pRelocatePartStockEntityRuleBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRelocatePartStockEntityRuleBrowse Procedure 
PROCEDURE pRelocatePartStockEntityRuleBrowse :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "relocatepartstockentityrule_details_form"}

   IF AVAILABLE PartStockEntityLink THEN
   DO:
     FIND FIRST StockEntity OF PartStockEntityLink NO-LOCK NO-ERROR.

     IF AVAILABLE Part AND AVAILABLE StockEntity THEN
        chrFormTitle = fTL("Relocate Rule for Part: ") + Part.PartRef + "&nbsp;&nbsp;StockEntity: " + StockEntity.EntityName.
     ELSE
        "".
   END.

   RelocatePartStockEntityRuleBrowseForm            = NEW dataForm("relocatepartstockentityrule_browse_form").
   RelocatePartStockEntityRuleBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   RelocatePartStockEntityRuleBrowseForm:FormAction = "dbRelocatePartStockEntityRuleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   RelocatePartStockEntityRuleBrowseForm:FormWidth  = 580.
   RelocatePartStockEntityRuleBrowseForm:FormHeight = 420.
   RelocatePartStockEntityRuleBrowseForm:FormTitle  = chrFormTitle.
   RelocatePartStockEntityRuleBrowseForm:FormType   = "large".

   RelocatePartStockEntityRuleBrowse              = NEW browseTable("relocatepartstockentityrule_browse").
   RelocatePartStockEntityRuleBrowse:BrowseWidth  = 560.
   RelocatePartStockEntityRuleBrowse:BrowseHeight = 375.

   RelocatePartStockEntityRuleBrowse:insertColumn(fTL("ID"),            80, "INTEGER"                  ).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i RelocatePartStockEntityRule}

   RelocatePartStockEntityRuleBrowse:insertColumn(fTL("LocationType"), 110, "CHARACTER", "LEFT",  FALSE).
   RelocatePartStockEntityRuleBrowse:insertColumn(fTL("Location"),     100, "CHARACTER", "LEFT",  FALSE).
   RelocatePartStockEntityRuleBrowse:insertColumn(fTL("Sequence"),      60, "INTEGER",   "RIGHT", FALSE).
   RelocatePartStockEntityRuleBrowse:insertColumn(fTL("Rule Action"),  100, "CHARACTER", "LEFT",  FALSE).
   RelocatePartStockEntityRuleBrowse:insertColumn(fTL("Active"),        70, "LOGICAL",            FALSE).

   RelocatePartStockEntityRuleBrowse:StartBody().

   IF AVAILABLE PartStockEntityLink THEN
   DO:
      /*List the Part Lines for the Part*/
      FOR EACH RelocatePartStockEntityRule NO-LOCK /* idx=PartStockEntityLinkIDActive */
         WHERE RelocatePartStockEntityRule.PartStockEntityLinkID = PartStockEntityLink.PartStockEntityLinkID
         BY    RelocatePartStockEntityRule.Active DESCENDING:

         FIND FIRST LocationType OF RelocatePartStockEntityRule NO-LOCK NO-ERROR.
         FIND FIRST Location     OF RelocatePartStockEntityRule NO-LOCK NO-ERROR.

         RelocatePartStockEntityRuleBrowse:startRow(RelocatePartStockEntityRule.RelocatePartStockEntityRuleID,
                                                    "selectRelocatePartStockEntityRuleRow(this," + '"'
                                                    + STRING(RelocatePartStockEntityRule.RelocatePartStockEntityRuleID) + '"' + ");", "").

         RelocatePartStockEntityRuleBrowse:insertData(RelocatePartStockEntityRule.RelocatePartStockEntityRuleID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i RelocatePartStockEntityRule}

         RelocatePartStockEntityRuleBrowse:insertData((IF AVAILABLE LocationType THEN LocationType.TypeName ELSE ""), "left").
         RelocatePartStockEntityRuleBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE ""), "left").
         RelocatePartStockEntityRuleBrowse:insertData(STRING(RelocatePartStockEntityRule.ListingSequence), "right").
         RelocatePartStockEntityRuleBrowse:insertData(RelocatePartStockEntityRule.RuleAction, "left").
         RelocatePartStockEntityRuleBrowse:insertData(STRING(RelocatePartStockEntityRule.Active, "Yes/No")).

         /* Add hidden fields */
         RelocatePartStockEntityRuleBrowse:insertHiddenData("RelocatePartStockEntityRuleVersionID",   RelocatePartStockEntityRule.VersionID).
         RelocatePartStockEntityRuleBrowse:insertHiddenData("RuleType",                               IF AVAILABLE LocationType THEN 
                                                                                                         "ByLocationType" ELSE "ByLocationRef").
         RelocatePartStockEntityRuleBrowse:inserthiddenData("RelocatePartStockEntityRuleLocationRef", IF AVAILABLE Location THEN 
                                                                                                         Location.LocationRef ELSE "").

         RelocatePartStockEntityRuleBrowse:endRow().

      END. /*  FOR EACH RelocatePartStockEntityRule OF Part NO-LOCK */
   END. /*IF AVAILABLE Part THEN*/

   RelocatePartStockEntityRuleBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + RelocatePartStockEntityRuleBrowse:getErrors().

   /* Hidden Fields for Filter */
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("filtering",              "no").

   /* Hidden Variables for Popup and Browse attributes */
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("form_name", "relocatepartstockentityrule_browse_form").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("prog_name", "adPartAdmin.p").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("relocatepartstockentityrule_browse_scroll", "").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("popup_partstockentitylink_browse", "").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("popup_relocatepartstockentityrule_browse", "").

   /* Hidden Variables for Key Values */
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("PartID",                                 chrPartID).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("PartVersionID",                          IF AVAILABLE Part THEN 
                                                                                                        STRING(Part.VersionID) ELSE "").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("PartStockEntityLinkID",                  chrPartStockEntityLinkID).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("PartStockEntityLinkVersionID",           IF AVAILABLE PartStockEntityLink THEN 
                                                                                                        STRING(PartStockEntityLink.VersionID) ELSE "").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("RelocatePartStockEntityRuleID",          chrRelocatePartStockEntityRuleID).
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("RelocatePartStockEntityRuleVersionID",   "").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("RelocatePartStockEntityRuleRuleType",    "").
   RelocatePartStockEntityRuleBrowseForm:insertHiddenField("RelocatePartStockEntityRuleLocationRef", "").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i RelocatePartStockEntityRuleBrowseForm}

   /* Create Button Bar */
   RelocatePartStockEntityRuleBrowseButtons = NEW buttonBar().

   RelocatePartStockEntityRuleBrowseButtons:addButton("relocatepartstockentityrule_browse_form_btn_create",
                                                      fTL("Create"),
                                                      "createRelocatePartStockEntityRule('relocatepartstockentityrule_details_form');").

   RelocatePartStockEntityRuleBrowseButtons:addButton("relocatepartstockentityrule_browse_form_btn_view",
                                                      fTL("View"),
                                                      "viewRelocatePartStockEntityRule('relocatepartstockentityrule_details_form');",
                                                      "Disabled").

   RelocatePartStockEntityRuleBrowseButtons:addButton("relocatepartstockentityrule_browse_form_btn_cancel",
                                                      fTL("Cancel"),
                                                      "disablePopup('relocatepartstockentityrule_browse_form_popup');").

   RelocatePartStockEntityRuleBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   RelocatePartStockEntityRuleBrowseForm:FormBrowse  = RelocatePartStockEntityRuleBrowse.
   RelocatePartStockEntityRuleBrowseForm:FormButtons = RelocatePartStockEntityRuleBrowseButtons.

   RelocatePartStockEntityRuleBrowseForm:endForm().

   RelocatePartStockEntityRuleBrowseForm:displayForm().

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pRelocatePartStockEntityRuleDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRelocatePartStockEntityRuleDetails Procedure 
PROCEDURE pRelocatePartStockEntityRuleDetails :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "relocatepartstockentityrule_details_form"}

   ASSIGN chrDisplayFieldList  = "RelocatePartStockEntityRuleID,PartStockEntityLinkID,RuleType,LocationTypeID,LocationRef,ListingSequence,"
                                    + "RuleAction,Active"
          chrEditFieldList     = "ListingSequence,RuleAction,Active"
          chrNewFieldList      = "RuleType,LocationTypeID,LocationRef,ListingSequence,RuleAction,Active"
          chrRequiredFieldList = "RuleType,ListingSequence,RuleAction"
          chrExtraFieldList    = ""
          chrValidateFieldList = "ListingSequence:INTEGER>0".

   RelocatePartStockEntityRuleDetailsForm           = NEW dataForm("relocatepartstockentityrule_details_form").
   RelocatePartStockEntityRuleDetailsForm:WebStream = STREAM WebStream:HANDLE.

   RelocatePartStockEntityRuleDetailsForm:FormAction = "dbRelocatePartStockEntityRuleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   RelocatePartStockEntityRuleDetailsForm:FormWidth  = 460.
   RelocatePartStockEntityRuleDetailsForm:FormHeight = 300.
   RelocatePartStockEntityRuleDetailsForm:FormTitle  = "Relocate Stock Entity Rule Details".
   RelocatePartStockEntityRuleDetailsForm:FormType   = "medium".

   /* Column Layout */
   RelocatePartStockEntityRuleDetailsForm:insertPaddingColumn(20).
   RelocatePartStockEntityRuleDetailsForm:insertColumn(120).
   RelocatePartStockEntityRuleDetailsForm:insertColumn(120).
   RelocatePartStockEntityRuleDetailsForm:insertColumn(30).
   RelocatePartStockEntityRuleDetailsForm:insertColumn(90).

   /* Fields */
   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTL("Rule ID")).
   RelocatePartStockEntityRuleDetailsForm:insertTextField("RelocatePartStockEntityRuleID", "", 110, TRUE).

   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTL("Link ID")).
   RelocatePartStockEntityRuleDetailsForm:insertTextField("PartStockEntityLinkID", "0", 110, TRUE).

   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTl("Rule Type")).
   RelocatePartStockEntityRuleDetailsForm:insertComboField("RuleType","", 110, TRUE,"onRuleTypeChange()").
   RelocatePartStockEntityRuleDetailsForm:insertComboPairs("RuleType","ByLocationType", "By LocationType").
   RelocatePartStockEntityRuleDetailsForm:insertComboPairs("RuleType","ByLocationRef","By LocationRef").

   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTL("Location Type")).
   RelocatePartStockEntityRuleDetailsForm:insertComboField("LocationTypeID", "", 110, TRUE,"onLocationTypeChange()").
   RelocatePartStockEntityRuleDetailsForm:insertComboPairs("LocationTypeID", "0", "None Assigned...").
   /*Insert LocationType*/
   FOR EACH LocationType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE LocationType.ACTIVE
      BY    LocationType.ListingSequence:

      RelocatePartStockEntityRuleDetailsForm:insertComboPairs("LocationTypeID", 
                                                              STRING(LocationType.LocationTypeID), 
                                                              LocationType.TypeName).
   END.

   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTL("Location Ref")).
   RelocatePartStockEntityRuleDetailsForm:insertTextField("LocationRef", "", 150, TRUE).

   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTL("Listing Sequence")).
   RelocatePartStockEntityRuleDetailsForm:insertTextField("ListingSequence", "0", 110, TRUE).

   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTL("Rule Action")).
   RelocatePartStockEntityRuleDetailsForm:insertComboField("RuleAction", "", 110, TRUE).
   RelocatePartStockEntityRuleDetailsForm:insertComboPairs("RuleAction", "Direct", "Direct").
   RelocatePartStockEntityRuleDetailsForm:insertComboPairs("RuleAction", "Prevent", "Prevent").

   RelocatePartStockEntityRuleDetailsForm:startRow().
   RelocatePartStockEntityRuleDetailsForm:insertLabel(fTL("Active")).
   RelocatePartStockEntityRuleDetailsForm:insertComboField("Active", "", 110, TRUE).
   RelocatePartStockEntityRuleDetailsForm:insertComboPairs("Active", "yes", "Active").
   RelocatePartStockEntityRuleDetailsForm:insertComboPairs("Active", "no",  "Not Active").


   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pRelocatePartStockEntityRuleDetailsFields}

   /* Hidden Fields for Filter */
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("PartRef",                chrSelectedPartRef).
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("TopNumParts",            chrSelectedTopNumParts).
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("FilteredVendorID",       STRING(intSelectedVendorID)).
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("FilteredPartTypeID",     STRING(intSelectedPartTypeID)).
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("FilteredBusinessUnitID", STRING(intSelectedBusinessUnitID)).
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("filtering",              "no").

   /* Hidden Variables for Popup and Browse attributes */
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("popup_partstockentitylink_browse", "").
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("popup_relocatepartstockentityrule_browse", "").
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("form_name", "relocatepartstockentityrule_details_form").
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("prog_name", "adPartAdmin.p").
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("relocatepartstockentityrule_browse_scroll", "").

   /* Hidden Variables for Key Values */
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("PartID",                        chrPartID).
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("PartStockEntityLinkID",         chrPartStockEntityLinkID).
   RelocatePartStockEntityRuleDetailsForm:insertHiddenField("RelocatePartStockEntityRuleID", chrRelocatePartStockEntityRuleID).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i RelocatePartStockEntityRuleDetailsForm}

   /* Create Button Bar */
   RelocatePartStockEntityRuleDetailsButtons = NEW buttonBar().

   RelocatePartStockEntityRuleDetailsButtons:addButton("relocatepartstockentityrule_details_form_btn_save",
                                                       fTL("Save"),
                                                       "updateRelocatePartStockEntityRule('relocatepartstockentityrule_details_form');").

   RelocatePartStockEntityRuleDetailsButtons:addButton("relocatepartstockentityrule_details_form_btn_cancel",
                                                       fTL("Cancel"),
                                                       "cancelUpdate('UserCancelled','process_mode');
                                                       disablePopup('relocatepartstockentityrule_details_form_popup');").

   RelocatePartStockEntityRuleDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   RelocatePartStockEntityRuleDetailsForm:FormButtons = RelocatePartStockEntityRuleDetailsButtons.

   RelocatePartStockEntityRuleDetailsForm:endForm().

   RelocatePartStockEntityRuleDetailsForm:displayForm().

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

   ASSIGN chrPartID                              = get-value("PartID")
          chrPartStockEntityLinkID               = get-value("PartStockEntityLinkID")
          chrRelocatePartStockEntityRuleID       = get-value("RelocatePartStockEntityRuleID")
          chrPartDangerousGoodsLinkID            = get-value("PartDangerousGoodsLinkID")
          intSelectedPart                        = INTEGER(chrPartID)
          intSelectedPartStockEntityLink         = INTEGER(chrPartStockEntityLinkID)
          intSelectedRelocatePartStockEntityRule = INTEGER(chrRelocatePartStockEntityRuleID)
          intSelectedPartDangerousGoodsLink      = INTEGER(chrPartDangerousGoodsLinkID)
          chrScrollToPartRow                     = STRING(INTEGER(get-value("part_browse_scroll"))) + ";"
          logFilterIsPoppedUp                    = (get-value('filtering') <> "no")
          chrSelectedPartRef                     = get-value('PartRef')
          chrSelectedTopNumParts                 = get-value('TopNumParts')
          intSelectedVendorID                    = INTEGER(get-value('FilteredVendorID'))
          intSelectedPartTypeID                  = INTEGER(get-value('FilteredPartTypeID'))
          intSelectedBusinessUnitID              = INTEGER(get-value('FilteredBusinessUnitID'))
          chrSelectedSerialisedPart              = get-value('SerialisedID')
          chrSelectedFinishedGoodPart            = get-value('FinishedGoodPartID')
          chrSelectedComponentPart               = get-value('ComponentPartID')
          chrPartLocationSizeFitID               = get-value('PartLocationSizeFitID')
          intSelectedPartLocationSizeFitID       = INTEGER(chrPartLocationSizeFitID)
          chrPartProfileOrderStreamLinkID        = get-value('PartProfileOrderStreamLinkID')
          intSelectedPartProfileOrderStreamLink  = INTEGER(chrPartProfileOrderStreamLinkID).   
          

   IF chrSelectedPartRef <> "" THEN
   DO:
      FIND FIRST Part NO-LOCK 
         WHERE Part.PartRef = chrSelectedPartRef NO-ERROR.
      IF AVAILABLE Part THEN
         ASSIGN intFilteredPart = Part.PartID
                intSelectedPart = Part.PartID
                chrPartID       = STRING(Part.PartID).
   END.

   /* Process URL values */
   IF logFilterIsPoppedUp THEN
      chrPopupFilters = 'viewPartFilter("part_filter_form");'.

   IF chrPartID <> "" THEN
      chrSelectPartRow = 'selectPartRow(document.getElementById("part_browse_row_' + chrPartID + '"),"' + chrPartID + '");'.

   IF chrPartStockEntityLinkID <> "" THEN
      chrSelectPartStockEntityLinkRow = 'selectPartStockEntityLinkRow(document.getElementById("partstockentitylink_browse_row_'
                                           + chrPartStockEntityLinkID + '"),"' + chrPartStockEntityLinkID + '");'.
    
   IF chrPartLocationSizeFitID <> "" THEN
       chrSelectPartLocationSizeFitRow = 'selectPartLocationSizeFitRow(document.getElementById("partlocationsizefit_browse_row_'
                                           + chrPartLocationSizeFitID + '"),"' + chrPartLocationSizeFitID + '");'.
    
   IF chrRelocatePartStockEntityRuleID <> "" THEN
      chrSelectRelocatePartStockEntityRuleRow = 'selectRelocatePartStockEntityRuleRow(document.getElementById(
                                                   "relocatepartstockentityrule_browse_row_'
                                                   + chrRelocatePartStockEntityRuleID + '"),"' + chrRelocatePartStockEntityRuleID + '");'.

   IF chrPartDangerousGoodsLinkID <> "" THEN 
      chrSelectPartDangerousGoodsLinkRow = 'selectPartDangerousGoodsLinkRow(document.getElementById("partdangerousgoodslink_browse_row_'
                                              + chrPartDangerousGoodsLinkID + '"),"' + chrPartDangerousGoodsLinkID + '");'. 
                                              
   IF chrPartProfileOrderStreamLinkID <> "" THEN
       chrSelectPartProfileOrderStreamLinkRow = 'selectPartProfileOrderStreamLinkRow(document.getElementById("partprofileorderstreamlink_browse_row_'
                                           + chrPartProfileOrderStreamLinkID + '"),"' + chrPartProfileOrderStreamLinkID + '");'.                                                  

   IF get-value('popup_partstockentitylink_browse') = "yes" THEN
      chrPopupPartStockEntityLink = 'enablePopup("partstockentitylink_browse_form_popup");'.

   IF get-value('popup_relocatepartstockentityrule_browse') = "yes" THEN
      chrPopupRelocatePartStockEntityRule = 'enablePopup("relocatepartstockentityrule_browse_form_popup");'.
      
   IF get-value('popup_partdangerousgoodslink_browse') = "yes" THEN
      chrPopupPartDangerousGoodsLink = 'enablePopup("partdangerousgoodslink_browse_form_popup");'.        

   IF get-value('popup_parthistory_browse') = "yes" THEN
      chrPopupPartHistory = 'enablePopup("parthistory_browse_form_popup");'.
      
   IF get-value('popup_partlocationsizefithistory_browse') = "yes" THEN
      chrPopupPartLocationSizeFitHistory = 'enablePopup("partlocationsizefithistory_browse_form_popup");'.   

   IF get-value('popup_partstockentitylinkhistory_browse') = "yes" THEN 
      chrPopupPartStockEntityLinkHistory = 'enablePopup("partstockentitylinkhistory_browse_form_popup");'.
      
   IF get-value('popup_partlocationsizefit_browse') = "yes" THEN
      chrPopupPartLocationSizeFit = 'enablePopup("partlocationsizefit_browse_form_popup");'.
   
   IF get-value('popup_partprofileorderstreamlink_browse') = "yes" THEN
      chrPopupPartProfileOrderStreamLink = 'enablePopup("partprofileorderstreamlink_browse_form_popup");'.

   IF get-value('popup_partprofileorderstreamlinklocation_browse') = "yes" THEN
      chrPopupPartProfileOrderStreamLinkLocation = 'enablePopup("partprofileorderstreamlinklocation_browse_form_popup");'.
      
   IF get-value('popup_partprofileorderstreamlinkhistory_browse') = "yes" THEN
      chrPopupPartProfileOrderStreamLinkHistory = 'enablePopup("partprofileorderstreamlinkhistory_browse_form_popup");'.
     
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("part_browse").scrollTop=' + chrScrollToPartRow + chrSelectPartRow
                             + chrSelectPartStockEntityLinkRow + chrPopupPartStockEntityLink
                             + chrSelectRelocatePartStockEntityRuleRow + chrPopupRelocatePartStockEntityRule
                             + chrSelectPartDangerousGoodsLinkRow + chrPopupPartDangerousGoodsLink
                             + chrPopupPartHistory + chrPopupPartLocationSizeFit + chrSelectPartLocationSizeFitRow
                             + chrPopupPartStockEntityLinkHistory 
                             + chrPopupFilters + chrPopupPartLocationSizeFitHistory
                             + chrPopupPartProfileOrderStreamLink + chrPopupPartProfileOrderStreamLinkLocation
                             + chrPopupPartProfileOrderStreamLinkHistory.

   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Part Admin".
   ThisPage:FrameTitle    = "Part Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.

   /* Include CSS Files */
   {webCssFiles.i}

   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("part.js").
   ThisPage:PageHeader().

   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pPartBrowse.

   /******* Popup Browsers and Forms ********/
   IF intSelectedPart <> 0 THEN
      FIND FIRST Part NO-LOCK /*idx=PartID*/ 
         WHERE Part.PartID = intSelectedPart NO-ERROR.

   RUN pPartDetails.
   RUN pPartProfile.
   RUN pPartStockEntityLinkBrowse.
   RUN pPartStockEntityLinkDetails.
   RUN pStreamProfiles.
   RUN pStreamProfileHistory.
   RUN pStreamProfileDetails.
   RUN pStreamProfileLocations.

   IF intSelectedPartStockEntityLink <> 0 THEN
      FIND FIRST PartStockEntityLink NO-LOCK /*idx=PartStockEntityLinkID*/
         WHERE PartStockEntityLink.PartStockEntityLinkID = intSelectedPartStockEntityLink NO-ERROR.

   RUN pPartStockEntityLinkHistoryBrowse.

   RUN pRelocatePartStockEntityRuleBrowse.
   RUN pRelocatePartStockEntityRuleDetails.
   
   IF chrPopupPartDangerousGoodsLink <> "" THEN
   DO: 
      RUN pPartDangerousGoodsLinkBrowse.
      RUN pPartDangerousGoodsLinkDetails.
   END.   
     
   IF chrPopupPartHistory <> "" THEN 
   DO:
      RUN pPartHistoryBrowse.
      RUN pPartHistoryDetails.
   END.   
      
   RUN pPartFilter.
   RUN pPartLocationSizeFit.
   RUN pPartLocationSizeFitDetails.
   
   IF chrPopupPartLocationSizeFitHistory <> "" THEN 
   DO:
      RUN pPartLocationSizeFitHistoryBrowse.
   END.
   

   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}

   ThisPage:PageFooter().

   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}

   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}

   /* Main Browse Object */
   DELETE OBJECT PartBrowse                                       NO-ERROR.
   DELETE OBJECT PartBrowseButtons                                NO-ERROR.
   DELETE OBJECT PartBrowseFrame                                  NO-ERROR.
   DELETE OBJECT PartDetailsButtons                               NO-ERROR.
   DELETE OBJECT PartDetailsForm                                  NO-ERROR.

   /* Part Filter Object */
   DELETE OBJECT PartBrowseFilterForm                             NO-ERROR.
   DELETE OBJECT PartBrowseFilterButtons                          NO-ERROR.

   /* Part History Object */
   DELETE OBJECT PartHistoryBrowse                                NO-ERROR.
   DELETE OBJECT PartHistoryBrowseButtons                         NO-ERROR.
   DELETE OBJECT PartHistoryBrowseForm                            NO-ERROR.
   DELETE OBJECT PartHistoryDetailsButtons                        NO-ERROR.
   DELETE OBJECT PartHistoryDetailsForm                           NO-ERROR.
   
   /* PartLocationsSizeFit History Object */
   DELETE OBJECT PartLocationSizeFitHistoryBrowse                 NO-ERROR.
   DELETE OBJECT PartLocationSizeFitHistoryBrowseButtons          NO-ERROR.
   DELETE OBJECT PartLocationSizeFitHistoryBrowseForm             NO-ERROR.

   /* Stock Entity Link Object */
   DELETE OBJECT PartStockEntityLinkBrowse                        NO-ERROR.
   DELETE OBJECT PartStockEntityLinkBrowseButtons                 NO-ERROR.
   DELETE OBJECT PartStockEntityLinkBrowseForm                    NO-ERROR.
   DELETE OBJECT PartStockEntityLinkDetailsButtons                NO-ERROR.
   DELETE OBJECT PartStockEntityLinkDetailsForm                   NO-ERROR.

   /* Part Stock Entity Relocate Rules */
   DELETE OBJECT RelocatePartStockEntityRuleBrowse                NO-ERROR.
   DELETE OBJECT RelocatePartStockEntityRuleBrowseButtons         NO-ERROR.
   DELETE OBJECT RelocatePartStockEntityRuleBrowseForm            NO-ERROR.
   DELETE OBJECT RelocatePartStockEntityRuleDetailsButtons        NO-ERROR.
   DELETE OBJECT RelocatePartStockEntityRuleDetailsForm           NO-ERROR.

   /* PartStockEntityLinkHistory Objects */
   DELETE OBJECT PartStockEntityLinkHistoryBrowse                 NO-ERROR.                                                                       
   DELETE OBJECT PartStockEntityLinkHistoryBrowseButtons          NO-ERROR.
   DELETE OBJECT PartStockEntityLinkHistoryBrowseForm             NO-ERROR.
   
   /* PartDangerousGoodsLink Objects */
   DELETE OBJECT PartDangerousGoodsLinkBrowse                     NO-ERROR.
   DELETE OBJECT PartDangerousGoodsLinkBrowseButtons              NO-ERROR.
   DELETE OBJECT PartDangerousGoodsLinkBrowseForm                 NO-ERROR.
   DELETE OBJECT PartDangerousGoodsLinkDetailsButtons             NO-ERROR.
   DELETE OBJECT PartDangerousGoodsLinkDetailsForm                NO-ERROR.   
   
   /* PartProfileOrderStreamLink Object */
   DELETE OBJECT PartProfileOrderStreamLinkBrowse                 NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkBrowseButtons          NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkBrowseForm             NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkDetailsButtons         NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkDetailsForm            NO-ERROR.

   /* PartProfileOrderStreamLinkLocation Object */
   DELETE OBJECT PartProfileOrderStreamLinkLocationBrowse         NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkLocationBrowseButtons  NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkLocationBrowseForm     NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkLocationDetailsButtons NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkLocationDetailsForm    NO-ERROR.

   /* PartProfileOrderStreamLinkHistory Object */
   DELETE OBJECT PartProfileOrderStreamLinkHistoryBrowse          NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkHistoryBrowseButtons   NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkHistoryBrowseForm      NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkHistoryDetailsButtons  NO-ERROR.
   DELETE OBJECT PartProfileOrderStreamLinkHistoryDetailsForm     NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetPartRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetPartRows Procedure 
PROCEDURE pSetPartRows :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   PartBrowse:startRow(Part.PartID, "selectPartRow(this," + '"' + STRING(Part.PartID) + '"' + ");", "").
   
   PartBrowse:insertData((IF AVAILABLE Part THEN Part.PartRef ELSE ""), "left").

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalBrowseFields.i Part}

   PartBrowse:insertData((IF AVAILABLE Vendor THEN Vendor.VendorName ELSE ""), "left").
   PartBrowse:insertData((IF AVAILABLE PartType THEN PartType.TypeDescr ELSE ""), "left").
   PartBrowse:insertData(Part.PartDescr, "left").
   PartBrowse:insertData((IF AVAILABLE PartProfile THEN STRING(PartProfile.HighRunnerRanking) ELSE "")).
   PartBrowse:insertData(STRING(Part.SerialisedPart, "Yes/No")).
   PartBrowse:insertData(STRING(Part.Active,"Yes/No")).

   /* Add hidden fields */
   PartBrowse:insertHiddenData("PartVersionID",Part.VersionID).

   intRecordCount = intRecordCount + 1.

   PartBrowse:endRow().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pStreamProfileDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStreamProfileDetails Procedure
PROCEDURE pStreamProfileDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

 /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
/*   {webGetWebForm.i "partprofileorderstreamlink_details_form"}*/
   
   ASSIGN chrDisplayFieldList  = "PartProfileOrderStreamLinkID,PartProfileID,ShipOrderStreamID,HighRunnerRanking,AvgQtyPickedPerDay"
                                 + ",AvgNumReplenLocations,Completed,OverrideCalculatedRanking,OptimumNumReplenLocations"
          chrEditFieldList     = "HighRunnerRanking,AvgQtyPickedPerDay,AvgNumReplenLocations,OptimumNumReplenLocations"
                                 + ",OverrideCalculatedRanking,Completed"
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "HighRunnerRanking:INTEGER,AvgQtyPickedPerDay:DECIMAL,AvgNumReplenLocations:DECIMAL"
                                 + ",OptimumNumReplenLocations:INTEGER".

   PartProfileOrderStreamLinkDetailsForm           = NEW dataForm("partprofileorderstreamlink_details_form").
   PartProfileOrderStreamLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.

   PartProfileOrderStreamLinkDetailsForm:FormAction = "dbPartUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   PartProfileOrderStreamLinkDetailsForm:FormWidth  = 460.
   PartProfileOrderStreamLinkDetailsForm:FormHeight = 300.
   PartProfileOrderStreamLinkDetailsForm:FormTitle  = "Stream Profile Details".
   PartProfileOrderStreamLinkDetailsForm:FormType   = "medium".

   /* Column Layout */
   PartProfileOrderStreamLinkDetailsForm:insertPaddingColumn(10).
   PartProfileOrderStreamLinkDetailsForm:insertColumn(190).
   PartProfileOrderStreamLinkDetailsForm:insertColumn(120).
   PartProfileOrderStreamLinkDetailsForm:insertColumn(20).
   PartProfileOrderStreamLinkDetailsForm:insertColumn(4).
   PartProfileOrderStreamLinkDetailsForm:insertColumn(110).

   /* Fields */
   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel("Link ID").
   PartProfileOrderStreamLinkDetailsForm:insertTextField("PartProfileOrderStreamLinkID", "", 110, TRUE).

   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel(fTL("Part ProfileID")).
   PartProfileOrderStreamLinkDetailsForm:insertTextField("PartProfileID", "", 110, TRUE).
/*   PartProfileOrderStreamLinkDetailsForm:insertComboPairs("PartProfileID", "0","None").*/
   /* Insert the Part */
/*   FOR EACH PartProfile NO-LOCK /*idx=PartTypeIDActive*/                                                                          */
/*      BY PartProfile.PartProfileID:                                                                                               */
/*                                                                                                                                  */
/*         PartProfileOrderStreamLinkDetailsForm:insertComboPairs("PartProfileID", STRING(PartProfile.PartProfileID), Part.PartRef).*/
/*   END.                                                                                                                           */

   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel(fTL("Stream")).
   PartProfileOrderStreamLinkDetailsForm:insertComboField("ShipOrderStreamID", "", 110, TRUE).
/*   PartProfileOrderStreamLinkDetailsForm:insertComboPairs("ShipOrderStreamID", "0","None").*/
   /* Insert the LocationSize */
   FOR EACH ShipOrderStream NO-LOCK /*idx=ShipOrderStreamID*/
      BY ShipOrderStream.ShipOrderStreamID:
       PartProfileOrderStreamLinkDetailsForm:insertComboPairs("ShipOrderStreamID", STRING(ShipOrderStream.ShipOrderStreamID), ShipOrderStream.StreamName).
   END.

   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel(fTL("Ranking")).
   PartProfileOrderStreamLinkDetailsForm:insertTextField("HighRunnerRanking", "", 110, TRUE).


   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel("Avg Qty Picked Per Day").
   PartProfileOrderStreamLinkDetailsForm:insertTextField("AvgQtyPickedPerDay", "", 110, TRUE).

   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel(fTL("Avg Num Replen Locations")).
   PartProfileOrderStreamLinkDetailsForm:insertTextField("AvgNumReplenLocations", "", 110, TRUE).

   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel(fTL("Optimum Num Replen Locations")).
   PartProfileOrderStreamLinkDetailsForm:insertTextField("OptimumNumReplenLocations", "", 110, TRUE).

   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel(fTL("Completed")).
   PartProfileOrderStreamLinkDetailsForm:insertTextField("Completed", "", 110, TRUE).

   PartProfileOrderStreamLinkDetailsForm:startRow().
   PartProfileOrderStreamLinkDetailsForm:insertLabel(fTL("Override Ranking")).
   PartProfileOrderStreamLinkDetailsForm:insertComboField("OverrideCalculatedRanking", "", 110, TRUE).
   PartProfileOrderStreamLinkDetailsForm:insertComboPairs("OverrideCalculatedRanking", "yes", "Yes").
   PartProfileOrderStreamLinkDetailsForm:insertComboPairs("OverrideCalculatedRanking", "no",  "No").

   {webGetOptionalFormFields.i pStreamProfileDetailsFields}

   /* Add Hidden Fields*/
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("part_browse_scroll", "").
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("form_name", "partprofileorderstreamlink_details_form").
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("popup_partprofileorderstreamlink_browse", "yes").
/*   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("popup_partprofileorderstreamlinkdetails_browse_form", "yes").*/
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("part_browse_scroll", "").
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("partprofileorderstreamlink_browse_scroll", "").
   
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("PartProfileOrderStreamLinkID", "").
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("PartProfileOrderStreamLinktVersionID", "").
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("PartID", chrPartID).
   PartProfileOrderStreamLinkDetailsForm:insertHiddenField("filtering", "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartProfileOrderStreamLinkDetailsForm}

   /* Create Button Bar */
   PartProfileOrderStreamLinkDetailsButtons = NEW buttonBar().

   PartProfileOrderStreamLinkDetailsButtons:addButton("partprofileorderstreamlink_details_form_btn_save",
                                              fTL("Save"),
                                              "updatePartProfileOrderStreamLink('partprofileorderstreamlink_details_form');").

   PartProfileOrderStreamLinkDetailsButtons:addButton("partprofileorderstreamlink_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('partprofileorderstreamlink_details_form_popup');").

   PartProfileOrderStreamLinkDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartProfileOrderStreamLinkDetailsForm:FormButtons = PartProfileOrderStreamLinkDetailsButtons.

   PartProfileOrderStreamLinkDetailsForm:endForm().

   PartProfileOrderStreamLinkDetailsForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pStreamProfileHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStreamProfileHistory Procedure
PROCEDURE pStreamProfileHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   PartProfileOrderStreamLinkHistoryBrowseForm            = NEW dataForm("partprofileorderstreamlinkhistory_browse_form").
   PartProfileOrderStreamLinkHistoryBrowseForm:WebStream  = STREAM WebStream:HANDLE.

   /* Setup */
   PartProfileOrderStreamLinkHistoryBrowseForm:FormWidth  = 860.
   PartProfileOrderStreamLinkHistoryBrowseForm:FormHeight = 530.
   PartProfileOrderStreamLinkHistoryBrowseForm:FormTitle  = fTL("History for Order Stream Profiles Fast Pick and Pack for Part: ")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;ID: " + string(Part.PartID))
                                                                       ELSE "")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;Descr: " + string(Part.PartDescr))
                                                                       ELSE "").
   PartProfileOrderStreamLinkHistoryBrowseForm:FormType   = "xxl_large".

   PartProfileOrderStreamLinkHistoryBrowse              = NEW browseTable("partprofileorderstreamlinkhistory_browse").
   PartProfileOrderStreamLinkHistoryBrowse:BrowseWidth  = 840.
   PartProfileOrderStreamLinkHistoryBrowse:BrowseHeight = 490.

   PartProfileOrderStreamLinkHistoryBrowse:insertColumn(fTL("ID"), 50, "INTEGER").

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartProfileOrderStreamLink}

   PartProfileOrderStreamLinkHistoryBrowse:insertColumn(fTL("Ranking"),            110, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamLinkHistoryBrowse:insertColumn(fTL("Qty Picked Per Day"), 125, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamLinkHistoryBrowse:insertColumn(fTL("Date"),               125, "CHARACTER", "LEFT", FALSE).
   PartProfileOrderStreamLinkHistoryBrowse:insertColumn(fTL("Active"),             70, "LOGICAL", "LEFT", FALSE).
   PartProfileOrderStreamLinkHistoryBrowse:insertColumn(fTL("Manually Set"),       90, "LOGICAL", "LEFT", FALSE).

   PartProfileOrderStreamLinkHistoryBrowse:StartBody().

   IF AVAILABLE Part THEN
   DO:

      FIND FIRST PartProfile NO-LOCK /*idx=PartID*/
         WHERE PartProfile.PartID = Part.PartID NO-ERROR.

      FOR EACH PartProfileOrderStreamLink NO-LOCK /* idx=PartProfileID */
            WHERE PartProfileOrderStreamLink.PartProfileID = PartProfile.PartProfileID
            AND PartProfileOrderStreamLink.Completed <> ""
            BY PartProfileOrderStreamLink.Completed:
                  

         FIND FIRST ShipOrderStream OF PartProfileOrderStreamLink NO-LOCK NO-ERROR.

         PartProfileOrderStreamLinkHistoryBrowse:startRow(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID, "selectPartProfileOrderStreamLinkHistoryRow(this," + '"'
                                               + STRING(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID) + '"' + ");", "").

         PartProfileOrderStreamLinkHistoryBrowse:insertData(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartProfileOrderStreamLink}

         PartProfileOrderStreamLinkHistoryBrowse:insertData(PartProfileOrderStreamLink.HighRunnerRanking,"LEFT").
         PartProfileOrderStreamLinkHistoryBrowse:insertData(PartProfileOrderStreamLink.AvgQtyPickedPerDay, "LEFT").
         PartProfileOrderStreamLinkHistoryBrowse:insertData(fDisplayDate&Time(PartProfileOrderStreamLink.Completed,"y/m/d"), "LEFT").
         PartProfileOrderStreamLinkHistoryBrowse:insertData(STRING(Part.Active,"Yes/No"), "LEFT").
         PartProfileOrderStreamLinkHistoryBrowse:insertData(STRING(PartProfileOrderStreamLink.OverrideCalculatedRanking,"Yes/No"), "LEFT").

         /* Add hidden fields */
         PartProfileOrderStreamLinkHistoryBrowse:insertHiddenData("PartProfileOrderStreamLinkID",PartProfileOrderStreamLink.PartProfileOrderStreamLinkID).
         PartProfileOrderStreamLinkHistoryBrowse:insertHiddenData("PartProfileOrderStreamLinkVersionID", PartProfileOrderStreamLink.VersionID).

         PartProfileOrderStreamLinkHistoryBrowse:endRow().

      END. /*  FOR EACH PartProfileOrderStreamLink OF Part NO-LOCK */
   END. /*IF AVAILABLE PartProfile THEN*/

   PartProfileOrderStreamLinkHistoryBrowse:endTable().

   chrPageBuildError = chrPageBuildError + PartProfileOrderStreamLinkHistoryBrowse:getErrors().

   /* Hidden Variables for Popup and Browse attributes */
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("form_name", "partprofileorderstreamlinkhistory_browse_form").
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("part_browse_scroll", "").
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("partprofileorderstreamlink_browse_scroll", "").
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("popup_partprofileorderstreamlink_browse", "yes").
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("popup_partprofileorderstreamlinkhistory_browse",  "yes").

   /* Hidden Variables for Key Values */
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("PartProfileOrderStreamLinkID", "").
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("PartProfileOrderStreamLinkVersionID", "").
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("PartID", chrPartID).
   PartProfileOrderStreamLinkHistoryBrowseForm:insertHiddenField("filtering", "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartProfileOrderStreamLinkBrowseForm}

   /* Create Button Bar */
   PartProfileOrderStreamLinkHistoryBrowseButtons = NEW buttonBar().

   PartProfileOrderStreamLinkHistoryBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('partprofileorderstreamlinkhistory_browse_form_popup');").

   PartProfileOrderStreamLinkHistoryBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   PartProfileOrderStreamLinkHistoryBrowseForm:FormBrowse  = PartProfileOrderStreamLinkHistoryBrowse.
   PartProfileOrderStreamLinkHistoryBrowseForm:FormButtons = PartProfileOrderStreamLinkHistoryBrowseButtons.

   PartProfileOrderStreamLinkHistoryBrowseForm:endForm().

   PartProfileOrderStreamLinkHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pStreamProfileLocations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStreamProfileLocations Procedure
PROCEDURE pStreamProfileLocations:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   PartProfileOrderStreamLinkLocationBrowseForm    = NEW dataForm("partprofileorderstreamlinklocation_browse_form").
   PartProfileOrderStreamLinkLocationBrowseForm:WebStream  = STREAM WebStream:HANDLE.

   /* Setup */
   PartProfileOrderStreamLinkLocationBrowseForm:FormWidth  = 860.
   PartProfileOrderStreamLinkLocationBrowseForm:FormHeight = 530.
   PartProfileOrderStreamLinkLocationBrowseForm:FormTitle  = fTL("Locations for Order Stream Profiles Fast Pick and Pack for Part: ")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;ID: " + string(Part.PartID))
                                                                       ELSE "")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;Descr: " + string(Part.PartDescr))
                                                                       ELSE "").
   
   
   PartProfileOrderStreamLinkLocationBrowseForm:FormType   = "xxl_large".

   PartProfileOrderStreamLinkLocationBrowse              = NEW browseTable("partprofileorderstreamlinklocation_browse").
   PartProfileOrderStreamLinkLocationBrowse:BrowseWidth  = 840.
   PartProfileOrderStreamLinkLocationBrowse:BrowseHeight = 490.
   
 
   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("Suggested to cover Pick Qty: "), 350, "CENTER", "INTEGER").
   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("Actual"),             350, "INTEGER", "CENTER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartProfileOrderStreamLink}
/*   PartProfileOrderStreamLinkLocationBrowse:insertRow(fTL(""), 820,"Character", FALSE).*/
/*   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("ID"),                 50, "INTEGER", "CENTER", FALSE).*/
/*   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("Ranking"),            70, "INTEGER", "LEFT", FALSE).  */
/*   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("Qty Picked Per Day"), 125, "LOGICAL", "LEFT", FALSE).  */
/*   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("Date"),               125, "CHARACTER", "LEFT", FALSE).*/
/*   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("Active"),             70, "LOGICAL", "LEFT", FALSE).   */
/*   PartProfileOrderStreamLinkLocationBrowse:insertColumn(fTL("Manually Set"),       90, "LOGICAL", "LEFT", FALSE).   */

   PartProfileOrderStreamLinkLocationBrowse:StartBody().

   IF AVAILABLE Part THEN
   DO:

      FIND FIRST PartProfile NO-LOCK /*idx=PartID*/
         WHERE PartProfile.PartID = Part.PartID NO-ERROR.

      FOR EACH PartProfileOrderStreamLink NO-LOCK /* idx=PartProfileID */
            WHERE PartProfileOrderStreamLink.PartProfileID = PartProfile.PartProfileID:

         FIND FIRST ShipOrderStream OF PartProfileOrderStreamLink NO-LOCK NO-ERROR.

         PartProfileOrderStreamLinkLocationBrowse:startRow(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID, "selectPartProfileOrderStreamLinkLocationRow(this," + '"'
                                               + STRING(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID) + '"' + ");", "").

/*         PartProfileOrderStreamLinkLocationBrowse:insertData(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID).*/

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartProfileOrderStreamLink}

/*         PartProfileOrderStreamLinkLocationBrowse:insertData((IF AVAILABLE ShipOrderStream THEN ShipOrderStream.StreamName ELSE ""), "left").*/
/*         PartProfileOrderStreamLinkLocationBrowse:insertData(PartProfileOrderStreamLink.HighRunnerRanking,"LEFT").                          */
/*         PartProfileOrderStreamLinkLocationBrowse:insertData(PartProfileOrderStreamLink.AvgQtyPickedPerDay, "LEFT").                        */
/*         PartProfileOrderStreamLinkLocationBrowse:insertData(fDisplayDate&Time(PartProfileOrderStreamLink.Completed,"y/m/d"), "LEFT").      */
/*         PartProfileOrderStreamLinkLocationBrowse:insertData(STRING(Part.Active,"Yes/No"), "LEFT").                                         */
/*         PartProfileOrderStreamLinkLocationBrowse:insertData(STRING(PartProfileOrderStreamLink.OverrideCalculatedRanking,"Yes/No"), "LEFT").*/

         /* Add hidden fields */
         PartProfileOrderStreamLinkLocationBrowse:insertHiddenData("PartProfileOrderStreamLinkID",PartProfileOrderStreamLink.PartProfileOrderStreamLinkID).
         PartProfileOrderStreamLinkLocationBrowse:insertHiddenData("PartProfileOrderStreamLinkVersionID", PartProfileOrderStreamLink.VersionID).

         PartProfileOrderStreamLinkLocationBrowse:endRow().

      END. /*  FOR EACH PartProfileOrderStreamLink OF Part NO-LOCK */
   END. /*IF AVAILABLE PartProfile THEN*/
   
   PartProfileOrderStreamLinkLocationBrowse:endTable().

   chrPageBuildError = chrPageBuildError + PartProfileOrderStreamLinkLocationBrowse:getErrors().

   /* Hidden Variables for Popup and Browse attributes */
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("form_name", "partprofileorderstreamlinklocation_browse_form").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("part_browse_scroll", "").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("partprofileorderstreamlink_browse_scroll", "").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("popup_partprofileorderstreamlink_browse", "yes").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("popup_partprofileorderstreamlinkhistory_browse",  "yes").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("popup_partprofileorderstreamlinklocation_browse",  "yes").

   /* Hidden Variables for Key Values */
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("PartProfileOrderStreamLinkID", "").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("PartProfileOrderStreamLinkVersionID", "").
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("PartID", chrPartID).
   PartProfileOrderStreamLinkLocationBrowseForm:insertHiddenField("filtering", "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartProfileOrderStreamLinkBrowseForm}

   /* Create Button Bar */
   PartProfileOrderStreamLinkLocationBrowseButtons = NEW buttonBar().

/*   PartProfileOrderStreamLinkLocationBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_create",             */
/*                                             fTL("Create"),                                                           */
/*                                             "createPartProfileOrderStreamLink('partprofileorderstreamlink_details_form');").*/

/*   PartProfileOrderStreamLinkLocationBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_details",                           */
/*                                             fTL("Details"),                                                                         */
/*                                             "viewStreamProfileDetails('partprofileorderstreamlink_details_form');",                 */
/*                                             "Disabled").                                                                            */
/*                                                                                                                                     */
/*   PartProfileOrderStreamLinkLocationBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_history",                           */
/*                                             fTL("History"),                                                                         */
/*                                             "viewStreamProfileHistory();",                                                          */
/*                                             "Disabled").                                                                            */
/*                                                                                                                                     */
/*   PartProfileOrderStreamLinkLocationBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_locations",                         */
/*                                             fTL("Locations"),                                                                       */
/*                                             "viewStreamProfileLocations();",                                                        */
/*                                             "Disabled").                                                                            */
/*                                                                                                                                     */
/*  PartProfileOrderStreamLinkLocationBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_excel",                              */
/*                                             fTL("Excel Export"),                                                                    */
/*                                             "excelExport('" + STRING(intGblSessionID) + "_partprofileorderstreamlink_browse.xml')").*/

   PartProfileOrderStreamLinkLocationBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('partprofileorderstreamlinklocation_browse_form_popup');").

   PartProfileOrderStreamLinkLocationBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   PartProfileOrderStreamLinkLocationBrowseForm:FormBrowse  = PartProfileOrderStreamLinkLocationBrowse.
   PartProfileOrderStreamLinkLocationBrowseForm:FormButtons = PartProfileOrderStreamLinkLocationBrowseButtons.

   PartProfileOrderStreamLinkLocationBrowseForm:endForm().

   PartProfileOrderStreamLinkLocationBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pStreamProfiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pStreamProfiles Procedure
PROCEDURE pStreamProfiles:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
                                                                  
   PartProfileOrderStreamLinkBrowseForm            = NEW dataForm("partprofileorderstreamlink_browse_form").
   PartProfileOrderStreamLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.

   /* Setup */
   PartProfileOrderStreamLinkBrowseForm:FormWidth  = 860.
   PartProfileOrderStreamLinkBrowseForm:FormHeight = 530.
   PartProfileOrderStreamLinkBrowseForm:FormTitle  = fTL("Order Stream Profiles for Part: ")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;ID: " + string(Part.PartID))
                                                                       ELSE "")
                                                  + (IF AVAILABLE Part THEN (Part.PartRef + "&nbsp;&nbsp;Descr: " + string(Part.PartDescr))
                                                                       ELSE "").
   PartProfileOrderStreamLinkBrowseForm:FormType   = "xxl_large".

   PartProfileOrderStreamLinkBrowse              = NEW browseTable("partprofileorderstreamlink_browse").
   PartProfileOrderStreamLinkBrowse:BrowseWidth  = 840.
   PartProfileOrderStreamLinkBrowse:BrowseHeight = 490.
   PartProfileOrderStreamLinkBrowse:ExcelExport  = TRUE.
   PartProfileOrderStreamLinkBrowse:SessionID    = intGblSessionID.

   PartProfileOrderStreamLinkBrowse:insertColumn(fTL("ID"), 50, "INTEGER").

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PartProfileOrderStreamLink}

   PartProfileOrderStreamLinkBrowse:insertColumn(fTL("Stream"),             150, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamLinkBrowse:insertColumn(fTL("Ranking"),            110, "INTEGER", "LEFT", FALSE).
   PartProfileOrderStreamLinkBrowse:insertColumn(fTL("Qty Picked Per Day"), 125, "DECIMAL", "LEFT", FALSE).
   PartProfileOrderStreamLinkBrowse:insertColumn(fTL("Date"),               125, "CHARACTER", "LEFT", FALSE).
   PartProfileOrderStreamLinkBrowse:insertColumn(fTL("Active"),             70, "LOGICAL", "LEFT", FALSE).
   PartProfileOrderStreamLinkBrowse:insertColumn(fTL("Manually Set"),       90, "LOGICAL", "LEFT", FALSE).

   PartProfileOrderStreamLinkBrowse:StartBody().

   IF AVAILABLE Part THEN
   DO:
      FIND FIRST PartProfile NO-LOCK /*idx=PartID*/
         WHERE PartProfile.PartID = Part.PartID NO-ERROR.

      FOR EACH PartProfileOrderStreamLink NO-LOCK /* idx=PartProfileID */
            WHERE PartProfileOrderStreamLink.PartProfileID = PartProfile.PartProfileID:

         FIND FIRST ShipOrderStream OF PartProfileOrderStreamLink NO-LOCK NO-ERROR.
         
         PartProfileOrderStreamLinkBrowse:startRow(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID, "selectPartProfileOrderStreamLinkRow(this," + '"'
                                               + STRING(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID) + '"' + ");", "").

         PartProfileOrderStreamLinkBrowse:insertData(PartProfileOrderStreamLink.PartProfileOrderStreamLinkID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PartProfileOrderStreamLink}

         PartProfileOrderStreamLinkBrowse:insertData((IF AVAILABLE ShipOrderStream THEN ShipOrderStream.StreamName ELSE ""), "left").
         PartProfileOrderStreamLinkBrowse:insertData(PartProfileOrderStreamLink.HighRunnerRanking,"LEFT").
         PartProfileOrderStreamLinkBrowse:insertData(PartProfileOrderStreamLink.AvgQtyPickedPerDay, "LEFT").
         PartProfileOrderStreamLinkBrowse:insertData(fDisplayDate&Time(PartProfileOrderStreamLink.Completed,"y/m/d"), "LEFT").
         PartProfileOrderStreamLinkBrowse:insertData(STRING(Part.Active,"Yes/No"), "LEFT").
         PartProfileOrderStreamLinkBrowse:insertData(STRING(PartProfileOrderStreamLink.OverrideCalculatedRanking,"Yes/No"), "LEFT").

         /* Add hidden fields */
         PartProfileOrderStreamLinkBrowse:insertHiddenData("PartProfileOrderStreamLinkID",PartProfileOrderStreamLink.PartProfileOrderStreamLinkID).
         PartProfileOrderStreamLinkBrowse:insertHiddenData("PartProfileOrderStreamLinkVersionID", PartProfileOrderStreamLink.VersionID).

         PartProfileOrderStreamLinkBrowse:endRow().

      END. /*  FOR EACH PartProfileOrderStreamLink OF Part NO-LOCK */
   END. /*IF AVAILABLE PartProfile THEN*/

   PartProfileOrderStreamLinkBrowse:endTable().

   chrPageBuildError = chrPageBuildError + PartProfileOrderStreamLinkBrowse:getErrors().

   /* Hidden Variables for Popup and Browse attributes */
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("form_name", "partprofileorderstreamlink_browse_form").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("prog_name", "adPartAdmin.p").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("part_browse_scroll", "").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("partprofileorderstreamlink_browse_scroll", "").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("popup_partprofileorderstreamlink_browse", "").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("popup_partprofileorderstreamlinkhistory_browse",  "").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("popup_partprofileorderstreamlinklocation_browse",  "").

   /* Hidden Variables for Key Values */                  
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("PartProfileOrderStreamLinkID", "").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("PartProfileOrderStreamLinkVersionID", "").
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("PartID", chrPartID).
   PartProfileOrderStreamLinkBrowseForm:insertHiddenField("filtering", "no").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PartProfileOrderStreamLinkBrowseForm}

   /* Create Button Bar */
   PartProfileOrderStreamLinkBrowseButtons = NEW buttonBar().


   PartProfileOrderStreamLinkBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewStreamProfileDetails('partprofileorderstreamlink_details_form');",
                                             "Disabled").

   PartProfileOrderStreamLinkBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_history",
                                             fTL("History"),
                                             "viewStreamProfileHistory();",
                                             "Disabled").

/*   PartProfileOrderStreamLinkBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_locations",*/
/*                                             fTL("Locations"),                                              */
/*                                             "viewStreamProfileLocations();",                               */
/*                                             "Disabled").                                                   */

  PartProfileOrderStreamLinkBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_excel",
                                              fTL("Excel Export"),
                                              "excelExport('" + STRING(intGblSessionID) + "_partprofileorderstreamlink_browse.xml')").
   
  PartProfileOrderStreamLinkBrowseButtons:addButton("partprofileorderstreamlink_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('partprofileorderstreamlink_browse_form_popup');").

  PartProfileOrderStreamLinkBrowseButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
  PartProfileOrderStreamLinkBrowseForm:FormBrowse  = PartProfileOrderStreamLinkBrowse.
  PartProfileOrderStreamLinkBrowseForm:FormButtons = PartProfileOrderStreamLinkBrowseButtons.

  PartProfileOrderStreamLinkBrowseForm:endForm().

  PartProfileOrderStreamLinkBrowseForm:displayForm().


END PROCEDURE.

	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

