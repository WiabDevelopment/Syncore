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

  Author: Lily Tran

  Created: November 25, 2014

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
{defWebDefinitions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Local Variables */
DEFINE VARIABLE chrFormTitle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSpaces         AS CHARACTER NO-UNDO INITIAL "&nbsp&nbsp&nbsp&nbsp".

/* Bom Filter Popup: 
      - FormName: bom_filter_form 
*/
DEFINE VARIABLE BomFilterForm             AS dataForm.
DEFINE VARIABLE BomFilterButtons          AS buttonBar.

DEFINE VARIABLE chrPopupFilters           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredFGPartRef      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intFilteredBusinessUnitID AS INTEGER   NO-UNDO.
DEFINE VARIABLE intFilteredVendorID       AS INTEGER   NO-UNDO.

/* Bom Browse: 
      - FormName: bom_browse_form
      - DetailFormName: bom_details_form
      - BrowseName: bom_browse
*/
DEFINE VARIABLE BomBrowseFrame       AS pageFrame.
DEFINE VARIABLE BomBrowse            AS browseTable.
DEFINE VARIABLE BomBrowseButtons     AS buttonBar.
DEFINE VARIABLE BomDetailsForm       AS dataForm.
DEFINE VARIABLE BomDetailsButtons    AS buttonBar.

DEFINE VARIABLE chrBomID             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToBomRow    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectBomRow      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedBom       AS INTEGER   NO-UNDO.
DEFINE VARIABLE logFilterIsPoppedUp  AS LOGICAL   NO-UNDO.

/* FG Part Lookup Browse:
      - FormName: part_lookup_browse_form
      - BrowseName: part_lookup_browse     
*/
DEFINE VARIABLE PartLookupBrowseForm    AS dataForm.
DEFINE VARIABLE PartLookupBrowse        AS browseTable.
DEFINE VARIABLE PartLookupBrowseButtons AS buttonBar.

/* BomLine Browse:
      - FormName: bomline_browse_form
      - DetailFormName: bomline_details_form
      - BrowseName: bomline_browse
*/

DEFINE VARIABLE BomLineBrowseForm     AS dataForm.
DEFINE VARIABLE BomLineBrowse         AS browseTable.
DEFINE VARIABLE BomLineBrowseButtons  AS buttonBar.
DEFINE VARIABLE BomLineDetailsForm    AS dataForm.
DEFINE VARIABLE BomLineDetailsButtons AS buttonBar.

DEFINE VARIABLE chrBomLineID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToBomLineRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectBomLineRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupBomLine       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrQtyRequired        AS CHARACTER NO-UNDO.



/* Buffers */
DEFINE BUFFER bfComponentPart FOR Part.

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

&IF DEFINED(EXCLUDE-pBomDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomDetails Procedure
PROCEDURE pBomDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   {webGetWebForm.i "bom_details_form"}
   
   ASSIGN chrDisplayFieldList  = "BomID,BusinessUnitID,BomTitle,BomTypeID,BomStatusID,FGPartRef"
                                    + ",FGLabelTemplate,VendorID,CreatedDate,CreatedHour"
                                    + ",CreatedMins,Revision,BoMTotalValue,Active"
                                    + ",SupercededDate,SupercededHour,SupercededMins" 
          chrEditFieldList     = "BomTitle,BomTypeID,FGLabelTemplate,BoMTotalValue,Active"
          chrNewFieldList      = "BusinessUnitID,BomTitle,BomTypeID,FGPartRef,FGLabelTemplate"
                                    + ",VendorID,BoMTotalValue,Active"
          chrRequiredFieldList = "BusinessUnitID,BomTitle,BomTypeID,FGPartRef,VendorID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "BoMTotalValue:DECIMAL".
          
   BomDetailsForm = NEW dataForm("bom_details_form").
   BomDetailsForm:WebStream = STREAM WebStream:HANDLE. 
   
   BomDetailsForm:FormAction = "dbBomUpdate.p?" + TRIM(chrGblDefaultUrlValues, "&").
      
   /* Setup */
   BomDetailsForm:FormWidth  = 560.
   BomDetailsForm:FormHeight = 410.
   BomDetailsForm:FormTitle  = fTL("BOM Details").
   BomDetailsForm:FormType   = "large".
   
   /* Column Layout */
   BomDetailsForm:insertPaddingColumn(40).
   BomDetailsForm:insertColumn(100).
   BomDetailsForm:insertColumn(120).
   BomDetailsForm:insertColumn(20).
   BomDetailsForm:insertColumn(4).
   BomDetailsForm:insertColumn(80).
   
   /* Fields */ 
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("BOM ID")).
   BomDetailsForm:insertTextField("BomID", "", 100, TRUE).
      
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("BusinessUnitID")).
   BomDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).
   FOR EACH BusinessUnit NO-LOCK
      WHERE BusinessUnit.Active:
         
      BomDetailsForm:insertComboPairs("BusinessUnitID",
                                      STRING(BusinessUnit.BusinessUnitID),
                                      BusinessUnit.UnitName ).               
   END. /* FOR EACH BusinessUnit */
   
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("BOM Title")).
   BomDetailsForm:insertTextField("BomTitle", "", 200, TRUE).
         
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("BOM Type")).
   BomDetailsForm:insertComboField("BomTypeID", "", 110, TRUE).
   FOR EACH BomType NO-LOCK
      WHERE BomType.Active:
      
      BomDetailsForm:insertComboPairs("BomTypeID",
                                      STRING(BomType.BomTypeID),
                                      BomType.TypeName).   
   END. /* FOR EACH BomType */

   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Status")).
   BomDetailsForm:insertComboField("BomStatusID", "", 110, TRUE).
   FOR EACH BomStatus NO-LOCK
      WHERE BomStatus.Active:
         
      BomDetailsForm:insertComboPairs("BomStatusID",
                                      STRING(BomStatus.BomStatusID),
                                      BomStatus.StatusName).         
   
   END. /* FOR EACH BomStatus */            
   
   /* PartLookup javascript passes(callingForm, fieldNameToPopulate, PartType) */      
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("FG Part Ref")).
   BomDetailsForm:insertTextField("FGPartRef", "", 150, TRUE).
   BomDetailsForm:insertButton("PartSearch",
                               "Search Parts",
                               "partLookup('bom_details_form','FGPartRef','FGPart');").
                               
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Label Template")).
   BomDetailsForm:insertTextField("FGLabelTemplate", "", 200, TRUE).
   
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Vendor")).
   BomDetailsForm:insertComboField("VendorID", "", 110, TRUE).
   FOR EACH Vendor NO-LOCK
      WHERE Vendor.Active:
         
      BomDetailsForm:insertComboPairs("VendorID",
                                      STRING(Vendor.VendorID),
                                      Vendor.VendorName).      
   
   END. /* FOR EACH Vendor */      
   
/*   BomDetailsForm:startRow().                                     */
/*   BomDetailsForm:insertLabel(fTL("Last WO BOM ID")).             */
/*   BomDetailsForm:insertTextField("LastWorkBoMID", "", 110, TRUE).*/
      
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Revision")).
   BomDetailsForm:insertTextField("Revision", "", 100, TRUE).
      
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Total Value")).
   BomDetailsForm:insertTextField("BoMTotalValue", "", 110, TRUE).
   
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Active")).
   BomDetailsForm:insertComboField("Active", "", 130, TRUE).
   BomDetailsForm:insertComboPairs("Active", "no", "Not Active").
   BomDetailsForm:insertComboPairs("Active", "yes", "Active").
   
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Created")).
   BomDetailsForm:insertDateField("CreatedDate", "", 130, TRUE).
   BomDetailsForm:insertTextField("CreatedHour", "", 18, TRUE).
   BomDetailsForm:insertLabel(":").
   BomDetailsForm:insertTextField("CreatedMins", "", 18, TRUE).
      
   BomDetailsForm:startRow().
   BomDetailsForm:insertLabel(fTL("Superceded")).
   BomDetailsForm:insertDateField("SupercededDate", "", 130, TRUE).
   BomDetailsForm:insertTextField("SupercededHour", "", 18, TRUE).
   BomDetailsForm:insertLabel(":").
   BomDetailsForm:insertTextField("SupercededMins", "", 18, TRUE).
       
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pBomDetailsFields}
   
   /* Add Hidden Fields */
   BomDetailsForm:insertHiddenField("form_name", "bom_details_form").
   BomDetailsForm:insertHiddenField("prog_name", "adBomAdmin.p").
   BomDetailsForm:insertHiddenField("bom_browse_scroll", "").
   
   /* Hidden variables for Filter */
   BomDetailsForm:insertHiddenField("filtering", "no").
   BomDetailsForm:insertHiddenField("FilteredFGPartRef", chrFilteredFGPartRef).
   BomDetailsForm:insertHiddenField("FilteredBusinessUnitID", STRING(intFilteredBusinessUnitID)).
   BomDetailsForm:insertHiddenField("FilteredVendorID", STRING(intFilteredVendorID)).
      
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomDetailsForm}
   
   /* Create Button Bar */
   BomDetailsButtons = NEW buttonBar().
   
   BomDetailsButtons:addButton("bom_details_form_btn_newrevision",
                               fTL("New Revision"),
                               "createNewRevision('bom_details_form');"). 
                               
   BomDetailsButtons:addButton("bom_details_form_btn_save",
                               fTL("Save"),
                               "updateBom('bom_details_form');").
                               
   BomDetailsButtons:addButton("bom_details_form_btn_cancel",
                               fTL("Cancel"),
                               "cancelUpdate('UserCancelled', 'process_mode');
                                  disablePopup('bom_details_form_popup');").
                                  
   BomDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   BomDetailsForm:FormButtons = BomDetailsButtons.
   
   /* Close and Display Form */   
   BomDetailsForm:endForm().
   BomDetailsForm:displayForm().
                                                                                                                                                                                             
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pBomDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomDetailsFields Procedure
PROCEDURE pBomDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         BomDetailsForm:startRow().
         BomDetailsForm:insertLabel(fTL("Field Label")).
         BomDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adBomAdmin_bom_details_form.i}
      
   END CASE. /*chrOption:*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pBomFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomFilter Procedure
PROCEDURE pBomFilter:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   BomFilterForm = NEW dataForm("bom_filter_form").
   BomFilterForm:WebStream  = STREAM WebStream:HANDLE.
   BomFilterForm:FormAction = "adBomAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BomFilterForm:FormWidth  = 350.
   BomFilterForm:FormHeight = 200.
   BomFilterForm:FormTitle  = "BOM Filter".
   BomFilterForm:FormType   = "small_wide".

   /* Column Layout */
   BomFilterForm:insertPaddingColumn(10).
   BomFilterForm:insertColumn(100).
   BomFilterForm:insertColumn(200).

   /* Fields */
   BomFilterForm:startRow().
   BomFilterForm:insertLabel(fTL("FG Part Ref")).
   BomFilterForm:insertTextField("FilteredFGPartRef", chrFilteredFGPartRef, 200, TRUE).

   BomFilterForm:startRow().
   BomFilterForm:insertLabel(fTL("Business Unit")).
   BomFilterForm:insertComboField("FilteredBusinessUnitID", STRING(intFilteredBusinessUnitID), 140, TRUE).
   BomFilterForm:insertComboPairs("FilteredBusinessUnitID", "0", "All Business Unit...").
   FOR EACH BusinessUnit NO-LOCK
      WHERE BusinessUnit.Active
      BY    BusinessUnit.UnitName:
      BomFilterForm:insertComboPairs("FilteredBusinessUnitID", 
                                     STRING(BusinessUnit.BusinessUnitID), 
                                     BusinessUnit.UnitName).
   END. /* FOR EACH BusinessUnit */

   BomFilterForm:startRow().
   BomFilterForm:insertLabel(fTL("Vendor")).
   BomFilterForm:insertComboField("FilteredVendorID", STRING(intFilteredVendorID), 140, TRUE).
   BomFilterForm:insertComboPairs("FilteredVendorID", "0", "All Vendors...").
   FOR EACH Vendor NO-LOCK
      WHERE Vendor.Active
      BY    Vendor.VendorName:
      BomFilterForm:insertComboPairs("FilteredVendorID", STRING(Vendor.VendorID), Vendor.VendorName).
   END. /* FOR EACH Vendor */

   /* Add Hidden Fields*/
   BomFilterForm:insertHiddenField("form_name", "bom_filter_form").
   BomFilterForm:insertHiddenField("prog_name", "adBomAdmin.p").
   
   /* Filtered Hidden Fields */
   BomFilterForm:insertHiddenField("filtering", "no").
   BomFilterForm:insertHiddenField("FilteredFGPartRef", chrFilteredFGPartRef).
   BomFilterForm:insertHiddenField("FilteredBusinessUnitID", STRING(intFilteredBusinessUnitID)).
   BomFilterForm:insertHiddenField("FilteredVendorID", STRING(intFilteredVendorID)).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomFilterForm}

   /* Create Button Bar */
   BomFilterButtons = NEW buttonBar().

   BomFilterButtons:addButton("bom_filter_form_btn_search",
                              fTL("Filter"),
                              "filterBom('bom_filter_form')").

   BomFilterButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   BomFilterForm:FormButtons = BomFilterButtons.

   BomFilterForm:endForm().
   BomFilterForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pBomLineBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomLineBrowse Procedure
PROCEDURE pBomLineBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   IF AVAILABLE Bom THEN
   DO:
      FIND FIRST Part WHERE Part.PartID = Bom.FGPartID NO-LOCK NO-ERROR.
      chrFormTitle = "Components for BOM Part " 
                        + (IF AVAILABLE Part THEN 
                             Part.PartRef 
                          ELSE Bom.BomTitle)  + chrSpaces + "ID:" + STRING(Bom.BomID).
   END.    
   ELSE 
      chrFormTitle = "Components for BOM".   
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "bomline_details_form"}
   
   BomLineBrowseForm = NEW dataForm("bomline_browse_form").
   BomLineBrowseForm:WebStream = STREAM WebStream:HANDLE. 
   
   BomLineBrowseForm:FormAction = "dbBomLineUpdate.p?" + TRIM(chrGblDefaultUrlValues, "&").
   
   /* Setup */
   BomLineBrowseForm:FormWidth  = 580.
   BomLineBrowseForm:FormHeight = 420.
   BomLineBrowseForm:FormTitle  = chrFormTitle.
   BomLineBrowseForm:FormType   = "large".
   
   BomLineBrowse = NEW browseTable("bomline_browse").
   BomLineBrowse:BrowseWidth  = 560.
   BomLineBrowse:BrowseHeight = 375.
   BomLineBrowse:SessionID    = intGblSessionID.
   
   BomLineBrowse:insertColumn(fTL("Line ID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i BomLine}

   BomLineBrowse:insertColumn(fTL("Component Part Ref"), 100, "CHARACTER", "left", FALSE).
   BomLineBrowse:insertColumn(fTL("Qty Required"),       100, "DECIMAL",   "left", FALSE).
   
   BomLineBrowse:StartBody().
   
   IF AVAILABLE Bom THEN
   DO:
      FOR EACH BomLine OF Bom NO-LOCK:
         
         FIND FIRST BomStatus       WHERE BomStatus.BomStatusID = BomLine.BomStatusID NO-LOCK NO-ERROR.
         FIND FIRST bfComponentPart WHERE bfComponentPart.PartID = BomLine.PartID NO-LOCK NO-ERROR. 
         
         BomLineBrowse:startRow(BomLine.BomLineID, 
                                "selectBomLineRow(this," + '"' + STRING(BomLine.BomLineID) + '");',
                                "").
         BomLineBrowse:insertData(BomLine.BomLineID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i BomLine}
         
         BomLineBrowse:insertData(IF AVAILABLE bfComponentPart THEN bfComponentPart.PartRef ELSE "", "left").
         BomLineBrowse:insertData(STRING(BomLine.QtyRequired, ">9.999999"), "right").
         
        
         /* Add Hidden Fields */
         BomLineBrowse:insertHiddenData("BomLineVersionID", STRING(BomLine.VersionID)).
         BomLineBrowse:insertHiddenData("PartRef", STRING(bfComponentPart.PartRef)).    
         
         BomLineBrowse:endRow().
                                                                       
      END. /* FOR EACH BomLine of Bom */   
   END. /* IF AVAILABLE Bom */ 
   
   BomLineBrowse:endTable().
   chrPageBuildError = chrPageBuildError + BomLineBrowse:getErrors().  
   
   /* Insert Hidden Form Data */
   BomLineBrowseForm:insertHiddenField("form_name", "bomline_browse_form").
   BomLineBrowseForm:insertHiddenField("prog_name", "adBomAdmin.p").
   BomLineBrowseForm:insertHiddenField("BomID", chrBomID).
   BomLineBrowseForm:insertHiddenField("BomLineID", "").
   BomLineBrowseForm:insertHiddenField("BomLineVersionID", "").
   BomLineBrowseForm:insertHiddenField("BomStatus", "").
   
   /* Hidden variables for Filter */
   BomLineBrowseForm:insertHiddenField("filtering", "no").
   BomLineBrowseForm:insertHiddenField("FilteredFGPartRef", chrFilteredFGPartRef).
   BomLineBrowseForm:insertHiddenField("FilteredBusinessUnitID", STRING(intFilteredBusinessUnitID)).
   BomLineBrowseForm:insertHiddenField("FilteredVendorID", STRING(intFilteredVendorID)).
      
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomLineBrowseForm}
   
   /* Create Button Bar */
   BomLineBrowseButtons = NEW buttonBar().
   
   BomLineBrowseButtons:addButton("bomline_browse_form_btn_details",
                                  fTL("Details"),
                                  "viewBomLineDetails('bomline_details_form');",
                                  "Disabled").
                                  
   BomLineBrowseButtons:addButton("bomline_browse_form_btn_delete",
                                  fTL("Delete"),
                                  "confirmDeleteBomLine('bomline_browse_form');",
                                  "Disabled").
                                  
   BomLineBrowseButtons:addButton("bomline_browse_form_btn_create",
                                  fTL("Create"),
                                  "createBomLine('bomline_details_form');",
                                  "").
                                 
   BomLineBrowseButtons:addButton("bomline_browse_form_btn_cancel",
                                  fTL("Cancel"),
                                  "disablePopup('bomline_browse_form_popup');").                                                                   
                                  
   BomLineBrowseButtons:closeBar().
   
   /* Assign the Buttons and Browse to the Form Object */
   BomLineBrowseForm:FormBrowse  = BomLineBrowse.  
   BomLineBrowseForm:FormButtons = BomLineBrowseButtons.
   BomLineBrowseForm:endForm().
   
   BomLineBrowseForm:displayForm().                                  
 
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pBomLineDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomLineDetails Procedure
PROCEDURE pBomLineDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "bomline_details_form"}
   
   ASSIGN chrDisplayFieldList  = "BomLineID,BomID,PartRef,QtyRequired"
          chrEditFieldList     = "QtyRequired"
          chrNewFieldList      = "PartRef,QtyRequired"
          chrRequiredFieldList = "PartRef,QtyRequired"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
          
   BomLineDetailsForm = NEW dataForm("bomline_details_form").
   BomLineDetailsForm:WebStream = STREAM WebStream:HANDLE. 
   
   BomLineDetailsForm:FormAction = "dbBomLineUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   BomLineDetailsForm:FormWidth  = 350.
   BomLineDetailsForm:FormHeight = 200.
   BomLineDetailsForm:FormTitle  = "Bom Line Details".
   BomLineDetailsForm:FormType   = "small_wide".
   
   /* Column Layout */
   BomLineDetailsForm:insertPaddingColumn(20).
   BomLineDetailsForm:insertColumn(90).
   BomLineDetailsForm:insertColumn(120).
   BomLineDetailsForm:insertColumn(30).
   BomLineDetailsForm:insertColumn(120).  
   
   /* Fields */
   BomLineDetailsForm:startRow().
   BomLineDetailsForm:insertLabel(fTL("BomLine ID")).
   BomLineDetailsForm:insertTextField("BomLineID", "", 110, TRUE).   
   
   BomLineDetailsForm:startRow().
   BomLineDetailsForm:insertLabel(fTL("Bom ID")).
   BomLineDetailsForm:insertTextField("BomID", "", 110, TRUE).
   
   /* PartLookup javascript passes(callingForm, fieldNameToPopulate, PartType) */      
   BomLineDetailsForm:startRow().
   BomLineDetailsForm:insertLabel(fTL("Part Ref")).
   BomLineDetailsForm:insertTextField("PartRef", "", 200, TRUE).
   BomLineDetailsForm:insertButton("PartSearch",
                                   "Search Parts",
                                   "partLookup('bomline_details_form','FGPartRef','Component');").
       
   BomLineDetailsForm:startRow().
   BomLineDetailsForm:insertLabel(fTL("Qty Required")).
   BomLineDetailsForm:insertTextField("QtyRequired", "", 110, TRUE). 
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pBomLineDetailsFields}
      
   /* Add Hidden Fields*/
   BomLineDetailsForm:insertHiddenField("form_name", "bomline_details_form").
   BomLineDetailsForm:insertHiddenField("prog_name", "adBomAdmin.p").
   BomLineDetailsForm:insertHiddenField("bomline_browse_scroll", "").
   
   /* Hidden variables for Filter */
   BomLineDetailsForm:insertHiddenField("filtering", "no").
   BomLineDetailsForm:insertHiddenField("FilteredFGPartRef", chrFilteredFGPartRef).
   BomLineDetailsForm:insertHiddenField("FilteredBusinessUnitID", STRING(intFilteredBusinessUnitID)).
   BomLineDetailsForm:insertHiddenField("FilteredVendorID", STRING(intFilteredVendorID)).
      
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomLineDetailsForm}
   
   /* Create Button Bar */
   BomLineDetailsButtons = NEW buttonBar().
   
   BomLineDetailsButtons:addButton("bomline_details_form_btn_save",
                                   fTL("Save"),
                                   "updateBomLine('bomline_details_form');").
                               
   BomLineDetailsButtons:addButton("bomline_details_form_btn_cancel",
                                   fTL("Cancel"),
                                   "cancelUpdate('UserCancelled', 'process_mode');
                                   disablePopup('bomline_details_form_popup');").

   BomLineDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   BomLineDetailsForm:FormButtons = BomLineDetailsButtons.
   
   /* Close and Display Form */   
   BomLineDetailsForm:endForm().
   BomLineDetailsForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pBomLineDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomLineDetailsFields Procedure
PROCEDURE pBomLineDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         BomLineDetailsForm:startRow().
         BomLineDetailsForm:insertLabel(fTL("Field Label")).
         BomLineDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adBomAdmin_bomline_details_form.i}
      
   END CASE. /*chrOption:*/

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

&IF DEFINED(EXCLUDE-pPartLookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartLookup Procedure
PROCEDURE pPartLookup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   PartLookupBrowseForm = NEW dataForm("part_lookup_browse_form").
   PartLookupBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   PartLookupBrowseForm:FormAction = "".
   
   /* Setup */
   PartLookupBrowseForm:FormWidth   = 580.
   PartLookupBrowseForm:FormHeight  = 420.
   PartLookupBrowseForm:FormTitle   = fTL("Parts Lookup"). 
   PartLookupBrowseForm:FormType    = "large".
   
   PartLookupBrowse = NEW browseTable("part_lookup_browse").    
   PartLookupBrowse:BrowseWidth  = 560.
   PartLookupBrowse:BrowseHeight = 375.
   
   /* Browse Column */
   PartLookupBrowse:insertColumn(fTL("Part ID"),     70, "CHARACTER", FALSE).
   PartLookupBrowse:insertColumn(fTL("Part Ref"),   120, "CHARACTER", FALSE).
   PartLookupBrowse:insertColumn(fTL("Part Descr"), 150, "CHARACTER", FALSE).
   
   PartLookupBrowse:startBody().
   
   PartLookupBrowse:endTable("DontFill").

   PartLookupBrowseForm:insertHiddenField("PartID", "").
   PartLookupBrowseForm:insertHiddenField("PartRef", "").
   PartLookupBrowseForm:insertHiddenField("PartDescr", "").
   PartLookupBrowseForm:insertHiddenField("FormToPopulate", "").
   
   /* Create Button Bar */
   PartLookupBrowseButtons = NEW buttonBar().
   
   PartLookupBrowseButtons:addButton("part_lookup_browse_form_btn_select",
                                     fTL("Select"),
                                     "partLookupSubmit('part_lookup_browse_form');",
                                     "Disabled").

   PartLookupBrowseButtons:addButton("part_lookup_browse_form_btn_cancel",
                                     fTL("Cancel"),
                                     "disablePopup('part_lookup_browse_form_popup');").
   
   PartLookupBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PartLookupBrowseForm:FormBrowse  = PartLookupBrowse.
   PartLookupBrowseForm:FormButtons = PartLookupBrowseButtons.
   
   PartLookupBrowseForm:endForm(). 
   
   PartLookupBrowseForm:displayForm().   
   
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
   
   ASSIGN chrBomID                  = get-value("BomID")
          chrBomLineID              = get-value("BomLineID")
          intSelectedBom            = INTEGER(chrBomID)
          chrFilteredFGPartRef      = get-value("FilteredFGPartRef")
          intFilteredBusinessUnitID = INTEGER(get-value("FilteredBusinessUnitID"))
          intFilteredVendorID       = INTEGER(get-value("FilteredVendorID"))          
          chrScrollToBomRow         = STRING(INTEGER(get-value("bom_browse_scroll"))) + ";"
          logFilterIsPoppedUp       = (get-value("filtering") <> "no").
   
   /* Process URL values */
   IF logFilterIsPoppedUp THEN
      chrPopupFilters = 'viewBomFilter("bom_filter_form");'.
      
   IF chrBomID <> "" THEN
      chrSelectBomRow = 'selectBomRow(document.getElementById("bom_browse_row_' + chrBomID + '"),"' + chrBomID + '");'.
      
   IF chrBomLineID <> "" THEN
      chrSelectBomLineRow = 'selectBomLineRow(document.getElementById("bomline_browse_row_' + chrBomLineID
                               + '"),"' + chrBomLineID + '");'.
      
   IF get-value("popup_bomline_browse") = "yes" THEN
      chrPopupBomLine = 'enablePopup("bomline_browse_form_popup");'.   
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad 
                    + 'document.getElementById("bom_browse").scrollTop=' + chrScrollToBomRow 
                    + chrSelectBomRow
                    + chrPopupBomLine
                    + chrSelectBomLineRow
                    + chrPopupFilters.
         
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Bom Admin".
   ThisPage:FrameTitle    = "Bom Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("bom.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pBomBrowse.
   
   /******* Pop-up Browsers and Forms ********/    
   RUN pBomDetails.
   RUN pPartLookup.
   
   FIND FIRST Bom NO-LOCK WHERE Bom.BomID = intSelectedBom NO-ERROR.   
   RUN pBomLineBrowse.
   RUN pBomLineDetails.
   RUN pBomFilter.
         
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT BomFilterForm           NO-ERROR.
   DELETE OBJECT BomFilterButtons        NO-ERROR.

   DELETE OBJECT BomBrowseFrame          NO-ERROR.
   DELETE OBJECT BomBrowse               NO-ERROR.
   DELETE OBJECT BomBrowseButtons        NO-ERROR.
   DELETE OBJECT BomDetailsForm          NO-ERROR.
   DELETE OBJECT BomDetailsButtons       NO-ERROR. 
   
   DELETE OBJECT BomLineBrowseForm       NO-ERROR.
   DELETE OBJECT BomLineBrowse           NO-ERROR.
   DELETE OBJECT BomLineBrowseButtons    NO-ERROR.
   DELETE OBJECT BomLineDetailsForm      NO-ERROR.
   DELETE OBJECT BomLineDetailsButtons   NO-ERROR.
   
   DELETE OBJECT PartLookupBrowseForm    NO-ERROR.
   DELETE OBJECT PartLookupBrowse        NO-ERROR.
   DELETE OBJECT PartLookupBrowseButtons NO-ERROR.

      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBomBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBomBrowse Procedure 
PROCEDURE pBomBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "bom_details_form"}
   
   BomBrowse              = NEW browseTable("bom_browse").
   BomBrowse:BrowseWidth  = 965.
   BomBrowse:BrowseHeight = 455.
   BomBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   BomBrowse:insertColumn(fTL("Bom ID"), 90, "INTEGER", FALSE).

   /* Customer Specific Columns */
   {webGetOptionalBrowseHeaders.i Bom}
   
   /* Standard Columns */
   BomBrowse:insertColumn(fTL("Bom Title"), 100, "CHARACTER", "left", FALSE).
   BomBrowse:insertColumn(fTL("Part Ref"),  120, "CHARACTER", "left", FALSE).
   BomBrowse:insertColumn(fTL("Vendor"),     90, "CHARACTER", "left", FALSE).
   BomBrowse:insertColumn(fTL("Revision"),   70, "CHARACTER", "left", FALSE).
   BomBrowse:insertColumn(fTL("Status"),    100, "CHARACTER", "left", FALSE).
   BomBrowse:insertColumn(fTL("Created"),    90, "DATE",    FALSE).
   BomBrowse:insertColumn(fTL("Superceded"), 90, "DATE",    FALSE).
   BomBrowse:insertColumn(fTL("Active"),     90, "LOGICAL", FALSE).
   
   /*Body*/
   BomBrowse:startBody().
   
   IF NOT logFilterIsPoppedUp THEN
   DO:
      IF chrFilteredFGPartRef <> "" THEN
      DO:
         FIND FIRST Part WHERE Part.PartRef = chrFilteredFGPartRef NO-LOCK NO-ERROR.
         IF AVAILABLE Part THEN
         DO:
            IF NOT Part.FinishedGoodPart THEN
            DO:
               chrPageBuildError = fTL("[" + chrFilteredFGPartRef 
                                       + "] is not a finished good Part. Please enter another PartRef.").
                                        
            END. /* IF NOT Part.FinishedGoodPart */
            ELSE 
            DO:
               FOR EACH Bom NO-LOCK
                  WHERE Bom.FGPartID = Part.PartID: 
                     
                  RUN pSetBomRows.      
                                          
               END. /* FOR EACH Bom */      
            END. /* IF AVAILABLE Part */
                        
         END. /* IF AVAILABLE Part */  
         ELSE
         DO:
            chrPageBuildError = fTL("[PartRef: " + chrFilteredFGPartRef 
                                   + "] does not exist. Please enter another PartRef.").
            
         END. /* IF NOT AVAILABLE Part */
               
      END. /* IF chrFilteredFGPartRef <> "" */
      ELSE
      DO: 
         BomLoop:
         FOR EACH Bom NO-LOCK
            WHERE Bom.Active = TRUE:
               
            IF intFilteredBusinessUnitID <> 0 AND 
               Bom.BusinessUnitID        <> intFilteredBusinessUnitID THEN
               NEXT BomLoop.
            
            IF intFilteredVendorID <> 0 AND
               Bom.VendorID        <> intFilteredVendorID THEN
               NEXT BomLoop. 
               
            FIND FIRST Part NO-LOCK 
               WHERE Part.PartID = Bom.FGPartID NO-ERROR.   
               
            RUN pSetBomRows.         
            
         END. /* FOR EACH Bom */   
      END. /* IF chrFilteredFGPartRef = "" */    
         
   END. /* IF NOT logFilteredIsPoppedUp */  
   
   BomBrowse:endTable().
   chrPageBuildError = chrPageBuildError + BomBrowse:getErrors().
   
   /* Create a new frame */
   BomBrowseFrame           = NEW pageFrame().
   BomBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   BomBrowseFrame:FormAction="dbBomUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   BomBrowseFrame:formOpen("bom_browse_form").
   
   /* Start the Frame Header */
   BomBrowseFrame:insertSpacer(5).
   BomBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   BomBrowse:displayBrowse().  
   
   /* End the Frame Header */
   BomBrowseFrame:frameClose().
   BomBrowseFrame:insertSpacer(10).
   
   /* Hidden Data */
   BomBrowseFrame:insertHiddenField("form_name", "bom_browse_form").
   BomBrowseFrame:insertHiddenField("prog_name", "adBomAdmin.p").   
   BomBrowseFrame:insertHiddenField("popup_bomline_browse", "").
   BomBrowseFrame:insertHiddenField("bom_browse_scroll", "").
   BomBrowseFrame:insertHiddenField("BomID", "").
   BomBrowseFrame:insertHiddenField("BomVersionID", "").
   BomBrowseFrame:insertHiddenField("BomStatus", "").
   
   /* Filtered Form Hidden Data */
   BomBrowseFrame:insertHiddenField("filtering", "no").
   BomBrowseFrame:insertHiddenField("FilteredFGPartRef", chrFilteredFGPartRef).
   BomBrowseFrame:insertHiddenField("FilteredBusinessUnitID", STRING(intFilteredBusinessUnitID)).
   BomBrowseFrame:insertHiddenField("FilteredVendorID", STRING(intFilteredVendorID)).   
         
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i BomBrowseFrame}
   
   BomBrowseFrame:formClose().
         
   /* Create Button Bar */
   BomBrowseButtons           = NEW buttonBar().
   BomBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   BomBrowseButtons:addButton("bom_browse_form_btn_filter",
                              fTL("Filter"),
                              "viewBomFilter('bom_filter_form');").
   
   BomBrowseButtons:addButton("bom_browse_form_btn_details",
                              fTL("Details"),
                              "viewBomDetails('bom_details_form');",
                              "Disabled").
   
   BomBrowseButtons:addButton("bom_browse_form_btn_create",
                              fTL("Create"),
                              "createBom('bom_details_form');").
                              
   BomBrowseButtons:addButton("bom_browse_form_btn_component",
                              fTL("Component"),
                              "viewBomLine();",
                              "Disabled").
                              
   BomBrowseButtons:addButton("bom_browse_form_btn_complete",
                              fTL("Complete Bom"),
                              "confirmCompleteBom('bom_browse_form');",
                              "Disabled").                              
                                                                                                                                   
   BomBrowseButtons:closeBar().  
   BomBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetBomRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetBomRows Procedure
PROCEDURE pSetBomRows:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   
   FIND FIRST Vendor NO-LOCK
      WHERE Vendor.VendorID = Bom.VendorID NO-ERROR.
      
   FIND FIRST BomStatus NO-LOCK
      WHERE BomStatus.BomStatusID = Bom.BomStatusID NO-ERROR.   
   
   BomBrowse:startRow(Bom.BomID,
                      "selectBomRow(this," + '"' + STRING(Bom.BomID) + '");',
                      "").
   BomBrowse:insertData(Bom.BomID).
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalBrowseFields.i Bom}
   
   BomBrowse:insertData(Bom.BomTitle, "left").   
   BomBrowse:insertData(IF AVAILABLE Part THEN Part.PartRef ELSE "", "left").
   BomBrowse:insertData(IF AVAILABLE Vendor THEN Vendor.VendorName ELSE "", "left").
   BomBrowse:insertData(STRING(Bom.Revision), "right").
   BomBrowse:insertData(IF AVAILABLE BomStatus THEN BomStatus.StatusName ELSE "", "left").
   BomBrowse:insertData(fDisplayDate&Time(Bom.Created, "y/m/d H:M:S"), "right").
   BomBrowse:insertData(fDisplayDate&Time(Bom.Superceded, "y/m/d H:M:S"), "right").
   BomBrowse:insertData(STRING(Bom.Active, "Yes/No")).
   
   /* Add Hidden Fields */
   BomBrowse:insertHiddenData("BomVersionID", Bom.VersionID).
   BomBrowse:insertHiddenData("BomStatus", IF AVAILABLE BomStatus THEN BomStatus.StatusCode ELSE "").
   
   BomBrowse:endRow().
                            
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

