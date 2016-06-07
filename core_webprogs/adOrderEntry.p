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

  Created: 07/04/2014
  
  Changes :
  --------------------------------------------------------------------------------------------------------------------------------------------
  Date       Who Project    Description
  ---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
  09/12/2014 SH  AmazonCa   Added Carrier, CarrierSortation, Currency, Language to details 
  10/12/2014 SH  AmazonCa   Added Ability to Change BU - In Status of BeingCreated only when no Orderlines
  11/12/2014 SH  AmazonCA   Adding Delete Button.
  09/05/2015 AL             Added Upload button/function
  ------------------------------------------------------------------------------------------------------------------------------------------*/
/*           This .W file was created with the Progress AppBuilder.     */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures 
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     clean up will occur on deletion of the procedure. */
     
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Standard Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{defWebDefinitions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{fncStatusTypeFunctions.i}

/* ShipOrder Local Variables */
DEFINE VARIABLE chrShipOrderID                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToShipOrderRow              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectShipOrderRow                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE decShipOrderValue                    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE intNumParts                          AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedShipOrder                 AS INTEGER   NO-UNDO.

/* ShipOrder Filter Variables */
DEFINE VARIABLE chrFilteredOrderRef                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredShipDate                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE intFilteredCustomerID                AS INTEGER   NO-UNDO.
DEFINE VARIABLE intFilteredStatusID                  AS INTEGER   NO-UNDO.

/* ShipOrderDetails Variables */
DEFINE VARIABLE chrOrderRef                          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intAddressID                         AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCustomerID                        AS INTEGER   NO-UNDO.

/* ShipOrderHistory Variables */
DEFINE VARIABLE chrPopupShipOrderHistory             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectShipOrderHistoryRow         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipOrderHistoryID                AS CHARACTER NO-UNDO.

/* ShipOrderLine Variables */
DEFINE VARIABLE chrOrderLineFormTitle                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupShipOrderLine                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectShipOrderLineRow            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipOrderLineID                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE intAwaitingApprovalStatusID          AS INTEGER   NO-UNDO.
DEFINE VARIABLE intBeingCreatedStatusID              AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCanCancelGroupID                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCancelledStatusID                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCanEditGroupID                    AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedShipOrderLine             AS INTEGER   NO-UNDO.
DEFINE VARIABLE logEnableApproveOrder                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logEnableCancelOrder                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logEnableDeleteOrderLine             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logEnableUnCancelOrder               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logEnableUncompleteOrder             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logIsBeingCreatedStatus              AS LOGICAL   NO-UNDO.

/* ShipOrderLineHistory Variables */
DEFINE VARIABLE chrOrderLineHistoryFormTitle         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupShipOrderLineHistory         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectShipOrderLineHistoryRow     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipOrderLineHistoryID            AS CHARACTER NO-UNDO.

/* Customer Variables */
DEFINE VARIABLE chrCustomerID                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCustomer                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCustomerRow                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCustomer                  AS INTEGER   NO-UNDO.

/* CustomerHistory Variables */
DEFINE VARIABLE chrCustomerHistoryID                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCustomerHistory              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCustomerHistoryRow          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCustomerHistory           AS INTEGER   NO-UNDO.

/* Address Variables */
DEFINE VARIABLE chrAddressID                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupAddress                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectAddressRow                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedAddress                   AS INTEGER   NO-UNDO.

/* AddressHistory Variables */
DEFINE VARIABLE chrAddressHistoryFormTitle           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupAddressHistory               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectAddressHistoryRow           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrAddressHistoryID                  AS CHARACTER NO-UNDO.

/* variables for file import */
DEFINE VARIABLE intFileLineSequence                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrImportedLine                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mptFile                              AS MEMPTR    NO-UNDO.
DEFINE VARIABLE chrFileName                          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrDestinationFile                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrColumnValue                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE intColumnsCount                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE logIsLogFile                         AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE intPickPackStationIDCol              AS INTEGER   NO-UNDO.
DEFINE VARIABLE intLocationIDCol                     AS INTEGER   NO-UNDO.
DEFINE VARIABLE intCurrentColumnsWidth               AS INTEGER   NO-UNDO.
DEFINE VARIABLE intDisplayedColumnsNo                AS INTEGER   NO-UNDO.
DEFINE VARIABLE intViewSkusFileID                    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrCustomerName                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipOrderType                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrOrderRefUpload                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrShipOrderStream                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCarrier                           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCarrierSortation                  AS CHARACTER NO-UNDO.

/*Upload Variables*/
DEFINE VARIABLE intSelectedFile                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectFileRow                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileID                            AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFile                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE intLocationUploadMasterID            AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrValidFile                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFileSelection                AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupShipOrderEntryUpload         AS CHARACTER NO-UNDO.


/* Form names are set to shiporderentry_browse_form and shiporderentry_details_form to integrate with dbShipOrderUpdate.p */
/* ShipOrder Objects */
DEFINE VARIABLE ShipOrderBrowse                      AS browseTable.
DEFINE VARIABLE ShipOrderBrowseButtons               AS buttonBar.
DEFINE VARIABLE ShipOrderBrowseFrame                 AS pageFrame.
DEFINE VARIABLE ShipOrderDetailsButtons              AS buttonBar.
DEFINE VARIABLE ShipOrderDetailsForm                 AS dataForm.

/* ShipOrderHistory Objects */
DEFINE VARIABLE ShipOrderHistoryBrowse               AS browseTable.
DEFINE VARIABLE ShipOrderHistoryBrowseButtons        AS buttonBar.
DEFINE VARIABLE ShipOrderHistoryBrowseForm           AS dataForm.

/* ShipOrder Filter Objects */
DEFINE VARIABLE ShipOrderBrowseFilterButtons         AS buttonBar.
DEFINE VARIABLE ShipOrderBrowseFilterForm            AS dataForm.

/* ShipOrderLine Objects */
DEFINE VARIABLE ShipOrderLineBrowse                  AS browseTable.
DEFINE VARIABLE ShipOrderLineBrowseButtons           AS buttonBar.
DEFINE VARIABLE ShipOrderLineBrowseForm              AS dataForm.
DEFINE VARIABLE ShipOrderLineDetailsButtons          AS buttonBar.
DEFINE VARIABLE ShipOrderLineDetailsForm             AS dataForm.

/* Part Lookup Objects */
DEFINE VARIABLE PartLookupBrowse                     AS browseTable.
DEFINE VARIABLE PartLookupBrowseButtons              AS buttonBar.
DEFINE VARIABLE PartLookupBrowseForm                 AS dataForm.

/* ShipOrderLineHistory Objects */
DEFINE VARIABLE ShipOrderLineHistoryBrowse           AS browseTable.
DEFINE VARIABLE ShipOrderLineHistoryBrowseButtons    AS buttonBar.
DEFINE VARIABLE ShipOrderLineHistoryBrowseForm       AS dataForm.

/* Customer Objects */
DEFINE VARIABLE CustomerBrowse                       AS browseTable.
DEFINE VARIABLE CustomerBrowseButtons                AS buttonBar.
DEFINE VARIABLE CustomerBrowseForm                   AS dataForm.
DEFINE VARIABLE CustomerDetailsButtons               AS buttonBar.
DEFINE VARIABLE CustomerDetailsForm                  AS dataForm.

/* CustomerHistory Objects */
DEFINE VARIABLE CustomerHistoryBrowse                AS browseTable.
DEFINE VARIABLE CustomerHistoryBrowseButtons         AS buttonBar.
DEFINE VARIABLE CustomerHistoryBrowseForm            AS dataForm.

/* Address Objects */
DEFINE VARIABLE AddressBrowse                        AS browseTable.
DEFINE VARIABLE AddressBrowseButtons                 AS buttonBar.
DEFINE VARIABLE AddressBrowseForm                    AS dataForm.
DEFINE VARIABLE AddressDetailsButtons                AS buttonBar.
DEFINE VARIABLE AddressDetailsForm                   AS dataForm.

/* AddressHistory Objects */
DEFINE VARIABLE AddressHistoryBrowse                 AS browseTable.
DEFINE VARIABLE AddressHistoryBrowseButtons          AS buttonBar.
DEFINE VARIABLE AddressHistoryBrowseForm             AS dataForm.

/* Objects for File Browse */
DEFINE VARIABLE FileSelectionForm                    AS dataForm.
DEFINE VARIABLE FileBrowseForm                       AS dataForm.
DEFINE VARIABLE FileBrowse                           AS browseTable.
DEFINE VARIABLE FileBrowseButtons                    AS buttonBar.
DEFINE VARIABLE FileDetailsButtons                   AS buttonBar.

/* Objects for UploadShipOrderEntry Browse */
DEFINE VARIABLE UploadShipOrderEntryBrowse           AS browseTable.
DEFINE VARIABLE UploadShipOrderEntryBrowseButtons    AS buttonBar.
DEFINE VARIABLE UploadShipOrderEntryBrowseForm       AS dataForm.


/*Upload Browse*/
DEFINE STREAM sUploadFile.

DEFINE TEMP-TABLE ttFileLine
   FIELD FileLineNo AS INTEGER 
   FIELD LineString AS CHARACTER 
   INDEX iFileLineNo FileLineNo.

DEFINE TEMP-TABLE ttFileLineMerge
   FIELD FileLineNo AS INTEGER 
   FIELD PartRef    AS CHARACTER
   FIELD Quantity   AS INTEGER 
   INDEX iFileLineNo FileLineNo.   
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
         HEIGHT             = 20.81
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

&IF DEFINED(EXCLUDE-pAddressBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddressBrowse Procedure 
PROCEDURE pAddressBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "address_details_form"}
   
   AddressBrowseForm            = NEW dataForm("address_browse_form").
   AddressBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   AddressBrowseForm:FormAction = "dbAddressUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /*
   /* Dev Layout Helper */
   AddressBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   AddressBrowseForm:FormWidth  = 860. 
   AddressBrowseForm:FormHeight = 530. 
   AddressBrowseForm:FormTitle  = fTL("Address Browse ") 
                                      + IF AVAILABLE Customer THEN "for " + Customer.CustomerName ELSE "". 
   AddressBrowseForm:FormType   = "xxl_large".
   
   /* Form Data */
   AddressBrowse              = NEW browseTable("address_browse").
   AddressBrowse:BrowseWidth  = 840.
   AddressBrowse:BrowseHeight = 490.
   
   AddressBrowse:insertColumn(fTL("ID"), 80, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Address Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Address}
   
   AddressBrowse:insertColumn(fTL("Type"),           90, "CHARACTER", "left", FALSE).
   AddressBrowse:insertColumn(fTL("Name"),          120, "CHARACTER", "left", FALSE).
   AddressBrowse:insertColumn(fTL("Address Line1"), 150, "CHARACTER", "left", FALSE).
   AddressBrowse:insertColumn(FTL("Postal Code"),   100, "CHARACTER", "left", FALSE).
   AddressBrowse:insertColumn(fTL("Country"),       120, "CHARACTER", "left", FALSE).
   AddressBrowse:insertColumn(fTL("Active"),         50, "LOGICAL", FALSE).
   
   AddressBrowse:StartBody().
   
   /* Show all the Address for Selected Customer */
   IF AVAILABLE Customer THEN
      FOR EACH Address OF Customer NO-LOCK
      BY Address.Active:

      FIND FIRST AddressType OF Address NO-LOCK NO-ERROR.
      FIND FIRST Country OF Address NO-LOCK NO-ERROR.

      AddressBrowse:startRow(Address.AddressID, "selectAddressRow(this," + '"' + STRING(Address.AddressID) + '"' + ");", "").
      AddressBrowse:insertData(Address.AddressID).
      
      /* Add in Optional & Address Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Address}

      AddressBrowse:insertData(IF AVAILABLE AddressType THEN AddressType.TypeName ELSE "", "left").
      AddressBrowse:insertData(Address.FullName,     "left").
      AddressBrowse:insertData(Address.AddressLine1, "left").
      AddressBrowse:insertData(Address.PostalCode,   "left").
      AddressBrowse:insertData(IF AVAILABLE Country THEN Country.CountryName ELSE "", "left").
      AddressBrowse:insertData(STRING(Address.Active,"Yes/No")).
      
      /* Add hidden fields */
      AddressBrowse:insertHiddenData("AddressVersionID",Address.VersionID).
      
      AddressBrowse:endRow().
      
   END. /*FOR EACH Address NO-LOCK */

   AddressBrowse:endTable().
   chrPageBuildError = chrPageBuildError + AddressBrowse:getErrors().
   
   /* Hidden Values - Browse Level */
   AddressBrowseForm:insertHiddenField("form_name", "customer_browse_form").
   AddressBrowseForm:insertHiddenField("prog_name", "adOrderEntry.p").
   AddressBrowseForm:insertHiddenField("popup_customer_browse", "").
   AddressBrowseForm:insertHiddenField("popup_address_browse", "").
   AddressBrowseForm:insertHiddenField("popup_addresshistory_browse", "").
   AddressBrowseForm:insertHiddenField("shiporder_browse_scroll", "").
   AddressBrowseForm:insertHiddenField("customer_browse_scroll", "").
   AddressBrowseForm:insertHiddenField("address_browse_scroll", "").

   /* Hidden Values - Filters */
   AddressBrowseForm:insertHiddenField("FilteredOrderRef",   chrFilteredOrderRef).
   AddressBrowseForm:insertHiddenField("FilteredShipDate",   chrFilteredShipDate).
   AddressBrowseForm:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   AddressBrowseForm:insertHiddenField("FilteredStatusID",   STRING(intFilteredStatusID)).
   
   /* Hidden Values - Keys */
   AddressBrowseForm:insertHiddenField("ShipOrderID", chrShipOrderID).
   AddressBrowseForm:insertHiddenField("CustomerID", chrCustomerID).
   AddressBrowseForm:insertHiddenField("CutomerVersionID", "").
   AddressBrowseForm:insertHiddenField("AddressID", chrAddressID).
   AddressBrowseForm:insertHiddenField("AddressVersionID", "").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddressBrowseForm}

   /* Create Button Bar */
   AddressBrowseButtons = NEW buttonBar().
   
   AddressBrowseButtons:addButton("address_browse_form_btn_create",
                                   fTL("Create"),
                                   "createAddress('address_details_form');"). 
   
   AddressBrowseButtons:addButton("address_browse_form_btn_details",
                                   fTL("Details"),
                                   "viewAddressDetails('address_details_form');",
                                   "Disabled"). 

   AddressBrowseButtons:addButton("address_browse_form_btn_history",
                                  fTL("History"),
                                  "viewAddressHistory()",
                                  "Disabled").
   
   AddressBrowseButtons:addButton("address_browse_form_btn_cancel",
                                   fTL("Cancel"),
                                   "disablePopup('address_browse_form_popup');").
   
   AddressBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddressBrowseForm:FormBrowse  = AddressBrowse.
   AddressBrowseForm:FormButtons = AddressBrowseButtons.
   AddressBrowseForm:endForm(). 
   
   AddressBrowseForm:displayForm().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAddressDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddressDetails Procedure 
PROCEDURE pAddressDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "address_details_form"}
   
   ASSIGN chrDisplayFieldList  = "AddressID,CustomerID,AddressTypeID,FullName,AddressLine1,AddressLine2,AddressLine3"
                                  + ",AddressLine4,AddressLine5,PostalCode,CountryID,Active"
          chrEditFieldList     = "FullName,AddressLine1,AddressLine2,AddressLine3,AddressLine4" 
                                  + ",AddressLine5,PostalCode,CountryID,Active"
          chrNewFieldList      = "FullName,AddressLine1,AddressLine2,AddressLine3,AddressLine4" 
                                  + ",AddressLine5,PostalCode,CountryID,Active"
          chrRequiredFieldList = "AddressLine1,PostalCode,CountryID,Active" 
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   AddressDetailsForm           = NEW dataForm("address_details_form").
   AddressDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   AddressDetailsForm:FormAction = "dbAddressUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   AddressDetailsForm:FormWidth  = 460.
   AddressDetailsForm:FormHeight = 300.
   AddressDetailsForm:FormTitle  = "Address Details".
   AddressDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   AddressDetailsForm:insertPaddingColumn(10).
   AddressDetailsForm:insertColumn(110).
   AddressDetailsForm:insertColumn(120).
   AddressDetailsForm:insertColumn(20).
   AddressDetailsForm:insertColumn(4).
   AddressDetailsForm:insertColumn(40).
   
   /* Fields */
   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Address ID").
   AddressDetailsForm:insertTextField("AddressID","", 110, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("CustomerID").
   AddressDetailsForm:insertTextField("CustomerID", "", 110, TRUE).
      
   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Address Type").
   AddressDetailsForm:insertComboField("AddressTypeID", "", 200, TRUE).
   FOR EACH AddressType NO-LOCK /*idx=AddressTypeID*/
      WHERE AddressType.Active
      AND   AddressType.TypeCode = "Delivery"
      BY    AddressType.TypeName:
      
      AddressDetailsForm:insertComboPairs("AddressTypeID", STRING(AddressType.AddressTypeID), AddressType.TypeName).
      
   END. /* FOR EACH AddressType */

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Full Name").
   AddressDetailsForm:insertTextField("FullName", "", 300, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Address Line1").
   AddressDetailsForm:insertTextField("AddressLine1", "", 300, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Address Line2").
   AddressDetailsForm:insertTextField("AddressLine2", "", 300, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Address Line3").
   AddressDetailsForm:insertTextField("AddressLine3", "", 300, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("City").
   AddressDetailsForm:insertTextField("AddressLine4", "", 300, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("State/Province").
   AddressDetailsForm:insertTextField("AddressLine5", "", 300, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Postal Code").
   AddressDetailsForm:insertTextField("PostalCode", "", 150, TRUE).

   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel("Country").
   AddressDetailsForm:insertComboField("CountryID", "", 300, TRUE).
   AddressDetailsForm:insertComboPairs("CountryID", "0", "Please select...").
   FOR EACH Country NO-LOCK /*idx=CountryName*/
      WHERE Country.Active
      BY    Country.CountryName:

      AddressDetailsForm:insertComboPairs("CountryID", STRING(Country.CountryID), Country.CountryName).
   END.
   
   AddressDetailsForm:startRow().
   AddressDetailsForm:insertLabel(fTL("Active")). 
   AddressDetailsForm:insertComboField("Active", "", 110, TRUE).  
   AddressDetailsForm:insertComboPairs("Active", "yes", "Active").
   AddressDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pAddressDetailsFields}
  
   /* Hidden Fields - Browse level*/
   AddressDetailsForm:insertHiddenField("shiporder_browse_scroll", "").
   AddressDetailsForm:insertHiddenField("form_name", "address_details_form").
   AddressDetailsForm:insertHiddenField("prog_name", "adOrderEntry.p").

   /* Hidden Fields - Filtered */
   AddressDetailsForm:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   AddressDetailsForm:insertHiddenField("FilteredShipDate", chrFilteredShipDate).
   AddressDetailsForm:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   AddressDetailsForm:insertHiddenField("FilteredStatusID", STRING(intFilteredStatusID)).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddressDetailsForm}
   
   /* Create Button Bar */
   AddressDetailsButtons = NEW buttonBar().
   
   AddressDetailsButtons:addButton("address_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateAddress('address_details_form');").
   
   AddressDetailsButtons:addButton("address_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); 
                                    disablePopup('address_details_form_popup');").
   
   AddressDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddressDetailsForm:FormButtons = AddressDetailsButtons.
   
   AddressDetailsForm:endForm(). 
   
   AddressDetailsForm:displayForm(). 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pAddressHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddressHistory Procedure 
PROCEDURE pAddressHistory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   chrAddressHistoryFormTitle = fTL("Address History").
   IF AVAILABLE Address THEN
   DO:
      chrOrderLineHistoryFormTitle = chrAddressHistoryFormTitle + "For: " + Address.FullName + "&nbspID:" + STRING(Address.AddressID).

      FIND FIRST Customer NO-LOCK
         WHERE   Customer.CustomerID  = Address.CustomerID NO-ERROR.
      IF AVAILABLE Customer THEN
         chrOrderLineHistoryFormTitle = chrAddressHistoryFormTitle + "&nbspCustomer: " + Customer.CustomerName .
         
   END. /* IF AVAILABLE Address */
   
   AddressHistoryBrowseForm = NEW dataForm("addresshistory_browse_form").
   AddressHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   AddressHistoryBrowseForm:FormWidth  = 860.
   AddressHistoryBrowseForm:FormHeight = 530.
   AddressHistoryBrowseForm:FormTitle  = chrAddressHistoryFormTitle.
   AddressHistoryBrowseForm:FormType   = "xxl_large".
   
   AddressHistoryBrowse = NEW browseTable("addresshistory_browse").
   AddressHistoryBrowse:BrowseWidth  = 840.
   AddressHistoryBrowse:BrowseHeight = 490.
   
   AddressHistoryBrowse:insertColumn(fTL("ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i AddressHistory}
   
   AddressHistoryBrowse:insertColumn(fTL("Gate User"),     100, "CHARACTER", "left").
   AddressHistoryBrowse:insertColumn(fTL("Created"),        90, "CHARACTER", "left").
   AddressHistoryBrowse:insertColumn(fTL("FullName"),      100, "CHARACTER", "left").
   AddressHistoryBrowse:insertColumn(fTL("AddressLine1"),  120, "CHARACTER", "left").
   AddressHistoryBrowse:insertColumn(fTL("City"),           80, "CHARACTER", "left").
   AddressHistoryBrowse:insertColumn(fTL("State/Province"), 90, "CHARACTER", "left").
   AddressHistoryBrowse:insertColumn(fTL("PostalCode"),     90, "CHARACTER", "left").
   AddressHistoryBrowse:insertColumn(fTL("Country"),        80, "CHARACTER", "left").
   
   AddressHistoryBrowse:StartBody().
   
   IF AVAILABLE Address THEN
   DO:
      /* List the AddressHistorys for the Address */
      FOR EACH AddressHistory NO-LOCK /* idx=AddressID */
         WHERE AddressHistory.AddressID = Address.AddressID 
         BY AddressHistory.AddressID
         BY AddressHistory.Created DESCENDING:

         FIND FIRST GateUser OF AddressHistory NO-LOCK NO-ERROR.
         FIND FIRST Country  OF AddressHistory NO-LOCK NO-ERROR.
                 
         AddressHistoryBrowse:startRow(AddressHistory.AddressHistoryID, "selectAddressHistoryRow(this," 
                                       + '"' + STRING(AddressHistory.AddressHistoryID)
                                       + '","adOrderEntry.p","addresshistory_browse_form"' + ");", "").

         AddressHistoryBrowse:insertData(AddressHistory.AddressHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i AddressHistory}
         
         AddressHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         AddressHistoryBrowse:insertData(fDisplayDate&Time(AddressHistory.Created, "d/m/y H:M")).
         AddressHistoryBrowse:insertData(AddressHistory.FullName,     "left").
         AddressHistoryBrowse:insertData(AddressHistory.AddressLine1, "left").
         AddressHistoryBrowse:insertData(AddressHistory.AddressLine4, "left").
         AddressHistoryBrowse:insertData(AddressHistory.AddressLine5, "left").
         AddressHistoryBrowse:insertData(AddressHistory.PostalCode,   "left"). 
         AddressHistoryBrowse:insertData(IF AVAILABLE Country THEN Country.CountryName ELSE "", "left").

         AddressHistoryBrowse:endRow().
      
      END. /* FOR EACH AddressHistory OF Address NO-LOCK */
   END. /*IF AVAILABLE Address THEN*/
   
   AddressHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + AddressHistoryBrowse:getErrors().

   /*  HiddenField */
   AddressHistoryBrowseForm:insertHiddenField("popup_address_browse","").
   AddressHistoryBrowseForm:insertHiddenField("popup_addresshistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i AddressHistoryBrowseForm}
   
   /* Create Button Bar */
   AddressHistoryBrowseButtons = NEW buttonBar().
          
   AddressHistoryBrowseButtons:addButton("addresshistory_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('addresshistory_browse_form_popup');").
   
   AddressHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   AddressHistoryBrowseForm:FormBrowse  = AddressHistoryBrowse.
   AddressHistoryBrowseForm:FormButtons = AddressHistoryBrowseButtons.
   AddressHistoryBrowseForm:endForm(). 
   
   AddressHistoryBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCustomerBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustomerBrowse Procedure 
PROCEDURE pCustomerBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "customer_details_form"}
   
   CustomerBrowseForm            = NEW dataForm("customer_browse_form").
   CustomerBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   CustomerBrowseForm:FormAction = "dbCustomerUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /*
   /* Dev Layout Helper */
   CustomerBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   CustomerBrowseForm:FormWidth  = 860. 
   CustomerBrowseForm:FormHeight = 530. 
   CustomerBrowseForm:FormTitle  = fTL("Customer Browse"). 
   CustomerBrowseForm:FormType   = "xxl_large".
   
   /* Form Data */
   CustomerBrowse              = NEW browseTable("customer_browse").
   CustomerBrowse:BrowseWidth  = 840.
   CustomerBrowse:BrowseHeight = 490.
   
   CustomerBrowse:insertColumn(fTL("CustomerID"), 80, "INTEGER", FALSE).

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Customer}
   
   CustomerBrowse:insertColumn(fTL("Customer Code"), 200, "CHARACTER", "left", FALSE).
   CustomerBrowse:insertColumn(fTL("Customer Name"), 220, "CHARACTER", "left", FALSE).
   CustomerBrowse:insertColumn(fTL("Active"),         50, "LOGICAL", FALSE).
   
   CustomerBrowse:StartBody().
   
   /* Show all the Customer for User's BusinessUnit */
   CustomerLoop:
   FOR EACH Customer NO-LOCK /*idx=BusinessUnitIDManuallyCreated*/
      WHERE Customer.ManuallyCreated 
      BY Customer.CustomerName:
     
      IF NOT fCanViewBusinessUnit(intGblSessionID,Customer.BusinessUnitID) AND Customer.BusinessUnitID <> 0 THEN 
         NEXT CustomerLoop.

      CustomerBrowse:startRow(Customer.CustomerID, "selectCustomerRow(this," + '"' + STRING(Customer.CustomerID) 
                                                                      + '"' + ");", "").
      CustomerBrowse:insertData(Customer.CustomerID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Customer}
      
      CustomerBrowse:insertData(Customer.CustomerCode,  "left").
      CustomerBrowse:insertData(Customer.CustomerName,  "left").
      CustomerBrowse:insertData(STRING(Customer.Active, "Yes/No")).
      
      /* Add hidden fields */
      CustomerBrowse:insertHiddenData("CustomerVersionID",Customer.VersionID).
      
      CustomerBrowse:endRow().
      
   END. /*FOR EACH Customer NO-LOCK */

   CustomerBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CustomerBrowse:getErrors().
   
   /* Hidden Values - Browse Level */
   CustomerBrowseForm:insertHiddenField("form_name", "customer_browse_form").
   CustomerBrowseForm:insertHiddenField("prog_name", "adOrderEntry.p").
   CustomerBrowseForm:insertHiddenField("popup_customer_browse",   "").
   CustomerBrowseForm:insertHiddenField("popup_customerhistory_browse", "").
   CustomerBrowseForm:insertHiddenField("popup_address_browse",    "").
   CustomerBrowseForm:insertHiddenField("shiporder_browse_scroll", "").
   CustomerBrowseForm:insertHiddenField("customer_browse_scroll",  "").
   CustomerBrowseForm:insertHiddenField("address_browse_scroll",   "").

   /* Hidden Values - Filters */
   CustomerBrowseForm:insertHiddenField("FilteredOrderRef",   chrFilteredOrderRef).
   CustomerBrowseForm:insertHiddenField("FilteredShipDate",   chrFilteredShipDate).
   CustomerBrowseForm:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   CustomerBrowseForm:insertHiddenField("FilteredStatusID",   STRING(intFilteredStatusID)).
   
   /* Hidden Values - Keys */
   CustomerBrowseForm:insertHiddenField("ShipOrderID", chrShipOrderID).
   CustomerBrowseForm:insertHiddenField("CustomerID",  chrCustomerID).
   CustomerBrowseForm:insertHiddenField("CustomerVersionID", "").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CustomerBrowseForm}

   /* Create Button Bar */
   CustomerBrowseButtons = NEW buttonBar().
   
   CustomerBrowseButtons:addButton("customer_browse_form_btn_create",
                                   fTL("Create"),
                                   "createCustomer('customer_details_form');"). 
   
   CustomerBrowseButtons:addButton("customer_browse_form_btn_details",
                                   fTL("Details"),
                                   "viewCustomerDetails('customer_details_form');",
                                   "Disabled"). 
   
   CustomerBrowseButtons:addButton("customer_browse_form_btn_history",
                                   fTL("History"),
                                   "viewCustomerHistory();",
                                   "Disabled").

   CustomerBrowseButtons:addButton("customer_browse_form_btn_address",
                                   fTL("Address"),
                                   "viewAddressBrowse();", 
                                   "Disabled").

   CustomerBrowseButtons:addButton("customer_browse_form_btn_cancel",
                                   fTL("Cancel"),
                                   "disablePopup('customer_browse_form_popup');").
   
   CustomerBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CustomerBrowseForm:FormBrowse  = CustomerBrowse.
   CustomerBrowseForm:FormButtons = CustomerBrowseButtons.
   CustomerBrowseForm:endForm(). 
   
   CustomerBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCustomerDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustomerDetails Procedure 
PROCEDURE pCustomerDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "customer_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CustomerID,BusinessUnitID,CustomerCode,CustomerName,Active" 
          chrEditFieldList     = "CustomerCode,BusinessUnitID,CustomerName,Active" 
          chrNewFieldList      = "CustomerCode,BusinessUnitID,CustomerName,Active" 
          chrRequiredFieldList = "CustomerCode,CustomerName,Active" 
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   CustomerDetailsForm           = NEW dataForm("customer_details_form").
   CustomerDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CustomerDetailsForm:FormAction = "dbCustomerUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CustomerDetailsForm:FormWidth  = 460.
   CustomerDetailsForm:FormHeight = 300.
   CustomerDetailsForm:FormTitle  = "Customer Details".
   CustomerDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CustomerDetailsForm:insertPaddingColumn(10).
   CustomerDetailsForm:insertColumn(110).
   CustomerDetailsForm:insertColumn(120).
   CustomerDetailsForm:insertColumn(20).
   CustomerDetailsForm:insertColumn(4).
   CustomerDetailsForm:insertColumn(40).
   
   /* Fields */
   CustomerDetailsForm:startRow().
   CustomerDetailsForm:insertLabel("Customer ID").
   CustomerDetailsForm:insertTextField("CustomerID","", 110, TRUE).

   CustomerDetailsForm:startRow().
   CustomerDetailsForm:insertLabel("Business Unit").
   CustomerDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).
   CustomerDetailsForm:insertComboPairs("BusinessUnitID", "0", "None Selected").
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK /*idx=ActiveListingSequence*/
      WHERE BusinessUnit.Active 
      BY    BusinessUnit.ListingSequence:

      IF NOT fCanViewBusinessUnit(intGblSessionID, BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.
      
      CustomerDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID) , BusinessUnit.UnitName).
   END.
      
   CustomerDetailsForm:startRow().
   CustomerDetailsForm:insertLabel("Customer Code").
   CustomerDetailsForm:insertTextField("CustomerCode", "", 200, TRUE).

   CustomerDetailsForm:startRow().
   CustomerDetailsForm:insertLabel("Customer Name").
   CustomerDetailsForm:insertTextField("CustomerName", "", 300, TRUE).
   
   CustomerDetailsForm:startRow().
   CustomerDetailsForm:insertLabel(fTL("Active")). 
   CustomerDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CustomerDetailsForm:insertComboPairs("Active", "yes", "Active").
   CustomerDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pCustomerDetailsFields}
  
   /* Hidden Fields - Browse level*/
   CustomerDetailsForm:insertHiddenField("shiporder_browse_scroll", "").
   CustomerDetailsForm:insertHiddenField("form_name", "customer_details_form").
   CustomerDetailsForm:insertHiddenField("prog_name", "adOrderEntry.p").

   /* Hidden Fields - Filtered */
   CustomerDetailsForm:insertHiddenField("FilteredOrderRef",   chrFilteredOrderRef).
   CustomerDetailsForm:insertHiddenField("FilteredShipDate",   chrFilteredShipDate).
   CustomerDetailsForm:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   CustomerDetailsForm:insertHiddenField("FilteredStatusID",   STRING(intFilteredStatusID)).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CustomerDetailsForm}
   
   /* Create Button Bar */
   CustomerDetailsButtons = NEW buttonBar().
   
   CustomerDetailsButtons:addButton("customer_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateCustomer('customer_details_form');").
   
   CustomerDetailsButtons:addButton("customer_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('customer_details_form_popup');").
   
   CustomerDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CustomerDetailsForm:FormButtons = CustomerDetailsButtons.
   
   CustomerDetailsForm:endForm(). 
   
   CustomerDetailsForm:displayForm(). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCustomerDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustomerDetailsFields Procedure 
PROCEDURE pCustomerDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCustomerHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCustomerHistory Procedure 
PROCEDURE pCustomerHistory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CustomerHistoryBrowseForm = NEW dataForm("customerhistory_browse_form").
   CustomerHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
                                          
   /* Setup */
   CustomerHistoryBrowseForm:FormWidth  = 860.
   CustomerHistoryBrowseForm:FormHeight = 530.
   CustomerHistoryBrowseForm:FormTitle  = fTL("Customer History Browse")
                                          + IF AVAILABLE Customer THEN " for " + Customer.CustomerName ELSE "".
   CustomerHistoryBrowseForm:FormType   = "xxl_large".
   
   CustomerHistoryBrowse = NEW browseTable("customerhistory_browse").
   CustomerHistoryBrowse:BrowseWidth  = 840.
   CustomerHistoryBrowse:BrowseHeight = 490.
   
   CustomerHistoryBrowse:insertColumn(fTL("ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CustomerHistory}
   
   CustomerHistoryBrowse:insertColumn(fTL("Gate User"),     100, "CHARACTER", "left").
   CustomerHistoryBrowse:insertColumn(fTL("Created"),       100, "CHARACTER", "left").
   CustomerHistoryBrowse:insertColumn(fTL("Customer Name"), 150, "CHARACTER", "left").
   CustomerHistoryBrowse:insertColumn(fTL("Customer Code"), 150, "CHARACTER", "left").
   CustomerHistoryBrowse:insertColumn(fTL("Active"),         80, "CHARACTER", "left").
   
   CustomerHistoryBrowse:StartBody().
   
   IF AVAILABLE Customer THEN
   DO:
      /* List the CustomerHistorys for the Customer */
      FOR EACH CustomerHistory NO-LOCK /* idx=CustomerID */
         WHERE CustomerHistory.CustomerID = Customer.CustomerID 
         BY CustomerHistory.CustomerID
         BY CustomerHistory.Created DESCENDING:

         FIND FIRST GateUser OF CustomerHistory NO-LOCK NO-ERROR.
                 
         CustomerHistoryBrowse:startRow(CustomerHistory.CustomerHistoryID, "selectCustomerHistoryRow(this," 
                                        + '"' + STRING(CustomerHistory.CustomerHistoryID)
                                        + '","adOrderEntry.p","customerhistory_browse_form"' + ");", "").

         CustomerHistoryBrowse:insertData(CustomerHistory.CustomerHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CustomerHistory}
         
         CustomerHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         CustomerHistoryBrowse:insertData(fDisplayDate&Time(CustomerHistory.Created, "d/m/y H:M")).
         CustomerHistoryBrowse:insertData(CustomerHistory.CustomerName,  "left").
         CustomerHistoryBrowse:insertData(CustomerHistory.CustomerCode,  "left").
         CustomerHistoryBrowse:insertData(STRING(CustomerHistory.Active, "Yes/No"), "left").

         CustomerHistoryBrowse:endRow().
      
      END. /* FOR EACH CustomerHistory OF Customer NO-LOCK */
   END. /*IF AVAILABLE Customer THEN*/
   
   CustomerHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + CustomerHistoryBrowse:getErrors().

   /*  HiddenField */
   CustomerHistoryBrowseForm:insertHiddenField("popup_customer_browse","").
   CustomerHistoryBrowseForm:insertHiddenField("popup_customerhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CustomerHistoryBrowseForm}
   
   /* Create Button Bar */
   CustomerHistoryBrowseButtons = NEW buttonBar().
          
   CustomerHistoryBrowseButtons:addButton("customerhistory_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('customerhistory_browse_form_popup');").
   
   CustomerHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CustomerHistoryBrowseForm:FormBrowse  = CustomerHistoryBrowse.
   CustomerHistoryBrowseForm:FormButtons = CustomerHistoryBrowseButtons.
   CustomerHistoryBrowseForm:endForm(). 
   
   CustomerHistoryBrowseForm:displayForm().




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

   /* Check to see if ShipOrderStatus, ShipOrderStatusGroup and ShipORderStatusGroupLink records exist so buttons 
      can be enabled based on data flow */

   FIND FIRST ShipOrderStatus NO-LOCK
      WHERE   ShipOrderStatus.StatusCode = "BeingCreated" NO-ERROR.
   IF AVAILABLE ShipOrderStatus THEN
     intBeingCreatedStatusID = ShipOrderStatus.ShipOrderStatusID.
   ELSE
     chrError = "ShipOrderStatus record needs to be created for StatusCode 'BeingCreated'.  ".

   FIND FIRST ShipOrderStatus NO-LOCK
      WHERE   ShipOrderStatus.StatusCode = "AwaitingApproval" NO-ERROR.
   IF AVAILABLE ShipOrderStatus THEN
      intAwaitingApprovalStatusID = ShipOrderStatus.ShipOrderStatusID.
   ELSE
      chrError = chrError + "ShipOrderStatus record needs to be created for StatusCode 'AwaitingApproval'.  ".

   FIND FIRST ShipOrderStatus NO-LOCK
      WHERE   ShipOrderStatus.StatusCode = "Cancelled" NO-ERROR.
   IF AVAILABLE ShipOrderStatus THEN
      intCancelledStatusID = ShipOrderStatus.ShipOrderStatusID.
   ELSE
      chrError = chrError + "ShipOrderStatus record needs to be created for StatusCode 'Cancelled'.  ".

   FIND FIRST ShipOrderStatusGroup NO-LOCK
      WHERE   ShipOrderStatusGroup.GroupCode = "CanBeEdited" NO-ERROR.
   IF AVAILABLE ShipOrderStatusGroup THEN
      intCanEditGroupID = ShipOrderStatusGroup.ShipOrderStatusGroupID.
   ELSE
      chrError = chrError + "ShipOrderStatusGroup and ShipOrderStatusGroupLink records needs to be created "
                 + "for ShipOrderStatusGroup 'CanBeEdited'.  ".

   FIND FIRST ShipOrderStatusGroup NO-LOCK
      WHERE   ShipOrderStatusGroup.GroupCode = "CanBeCancelled" NO-ERROR.
   IF AVAILABLE ShipOrderStatusGroup THEN
      intCanCancelGroupID = ShipOrderStatusGroup.ShipOrderStatusGroupID.
   ELSE
      chrError = chrError + "ShipOrderStatusGroup and ShipOrderStatusGroupLink records needs to be created "
                 + "for ShipOrderStatusGroup 'CanBeCancelled'.  ".

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPartLookUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPartLookUp Procedure 
PROCEDURE pPartLookUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
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
   
   PartLookupBrowse:insertColumn(fTL("Part ID"),     70, "INTEGER", FALSE).
   PartLookupBrowse:insertColumn(fTL("Part Ref"),   115, "CHARACTER", "left", FALSE).
   PartLookupBrowse:insertColumn(fTL("Part Descr"), 325, "CHARACTER", "left", FALSE).
   
   PartLookupBrowse:startBody().
   
   PartLookupBrowse:endTable("DontFill").
    
   PartLookupBrowseForm:insertHiddenField("PartDescr","").
   PartLookupBrowseForm:insertHiddenField("PartID","").
   PartLookupBrowseForm:insertHiddenField("PartRef","").
   
   /* Create Button Bar */
   PartLookupBrowseButtons = NEW buttonBar().
   
   PartLookupBrowseButtons:addButton("part_lookup_browse_form_btn_select",
                                     fTL("Select"),
                                     "partLookupSubmit('adOrderEntry.p');",
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
   
   FIND FIRST FileMaster NO-LOCK
      WHERE   FileMaster.MasterName = "ShipOrderLineUpload" NO-ERROR.
   IF NOT AVAILABLE FileMaster THEN
   DO:
      chrPageBuildError = "FileMaster ShipOrderLineUpload does not exist.".
   END.
   ELSE intLocationUploadMasterID = FileMaster.FileMasterID.
   

   /* Get the system options which relate to this program */
   RUN pGetSystemOptions(OUTPUT chrPageBuildError).  
   
   ASSIGN chrShipOrderID           = get-value("ShipOrderID")          
          intSelectedShipOrder     = INTEGER(chrShipOrderID)
          chrShipOrderLineID       = get-value("ShipOrderLineID")
          intSelectedShipOrderLine = INTEGER(chrShipOrderLineID)
          chrCustomerID            = get-value("CustomerID")
          intSelectedCustomer      = INTEGER(chrCustomerID)
          chrAddressID             = get-value("AddressID")
          intSelectedAddress       = INTEGER((chrAddressID))
          chrFilteredOrderRef      = get-value("FilteredOrderRef")
          chrFilteredShipDate      = get-value("FilteredShipDate")
          intFilteredCustomerID    = INTEGER(get-value("FilteredCustomerID"))
          intFilteredStatusID      = INTEGER(get-value("FilteredStatusID"))       
          chrScrollToShipOrderRow  = STRING(INTEGER(get-value("shiporderentry_browse_scroll"))) + ";"
          chrValidFile             = get-value("ValidFile")
          chrCustomerName          = get-value("CustomerName")
          chrShipOrderType         = get-value("ShipOrderTypeName")
          chrOrderRefUpload        = get-value("OrderRef")
          chrShipOrderStream       = get-value("ShipOrderStream")
          chrCarrier               = get-value("Carrier")
          chrCarrierSortation      = get-value("CarrierSortation").

   /* Process URL values */
   IF chrShipOrderID <> "" THEN
      chrSelectShipOrderRow = 'selectShipOrderRow(document.getElementById("shiporderentry_browse_row_' + chrShipOrderID + '"),"' 
                               + chrShipOrderID + '");'.

   IF chrShipOrderHistoryID <> "" THEN
      chrSelectShipOrderHistoryRow = 'selectShipOrderHistoryRow(document.getElementById("shiporderhistory_browse_row_' 
                                      + chrShipOrderHistoryID + '"),"' + chrShipOrderHistoryID + '");'.

   IF chrShipOrderLineID <> "" THEN
      chrSelectShipOrderLineRow = 'selectShipOrderLineRow(document.getElementById("shiporderline_browse_row_' + chrShipOrderLineID + '"),"' 
                                   + chrShipOrderLineID + '");'.

   IF chrCustomerID <> "" THEN
      chrSelectCustomerRow = 'selectCustomerRow(document.getElementById("customer_browse_row_' + chrCustomerID + '"),"' 
                              + chrCustomerID + '");'.

   IF chrAddressID <> "" AND 
      get-value('popup_address_browse') = 'yes' THEN
      chrSelectAddressRow = 'selectAddressRow(document.getElementById("address_browse_row_' + chrAddressID + '"),"' 
                             + chrAddressID + '");'.

   /* Popup */
   IF get-value('popup_shiporderhistory_browse') = "yes" THEN
      chrPopupShipOrderHistory = 'enablePopup("shiporderhistory_browse_form_popup");'.

   IF get-value('popup_shiporderline_browse') = "yes" THEN 
      chrPopupShipOrderLine = 'enablePopup("shiporderline_browse_form_popup");'.

   IF get-value('popup_shiporderlinehistory_browse') = "yes" THEN
      chrPopupShipOrderLineHistory = 'enablePopup("shiporderlinehistory_browse_form_popup");'.

   IF get-value('popup_customer_browse') = "yes" THEN
      chrPopupCustomer = 'enablePopup("customer_browse_form_popup");'.

   IF get-value('popup_customerhistory_browse') = 'yes' THEN
      chrPopupCustomerHistory = 'enablePopup("customerhistory_browse_form_popup");'.

   IF get-value('popup_address_browse') = "yes" THEN
      chrPopupAddress = 'enablePopup("address_browse_form_popup");'.

   IF get-value('popup_addresshistory_browse') = "yes" THEN
      chrPopupAddressHistory = 'enablePopup("addresshistory_browse_form_popup");'.
      
   /* Build the Popup uploadlocation browse command if requested */
   IF get-value('popup_shiporderentryupload_browse_form') = "yes" THEN
   DO:
      chrPopupShipOrderEntryUpload = 'enablePopup("shiporderentryupload_browse_form_popup");'.                                                            
   END.  
      
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad 
                 + 'document.getElementById("shiporderentry_browse").scrollTop=' 
                 + chrScrollToShipOrderRow + chrSelectShipOrderRow
                 + chrPopupShipOrderHistory + chrSelectShipOrderHistoryRow
                 + chrPopupShipOrderLine + chrSelectShipOrderLineRow
                 + chrPopupShipOrderLineHistory + chrSelectShipOrderLineHistoryRow
                 + chrPopupCustomer + chrSelectCustomerRow
                 + chrPopupCustomerHistory + chrSelectCustomerRow
                 + chrPopupAddress + chrSelectAddressRow
                 + chrPopupAddressHistory + chrSelectAddressHistoryRow
                 + chrPopupShipOrderEntryUpload.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Order Entry".
   ThisPage:FrameTitle    = "Order Entry".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("orderentry.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pShipOrderBrowse.

   /******* Pop-up Browsers and Forms ********/
   FIND FIRST ShipOrder WHERE ShipOrder.ShipOrderID = intSelectedShipOrder NO-LOCK NO-ERROR.

   RUN pShipOrderDetails.
   RUN pShipOrderHistory.
   RUN pShipOrderLineBrowse.
   RUN pShipOrderLineDetails.

   FIND ShipOrderLine WHERE ShipOrderLine.ShipOrderLineID = intSelectedShipOrderLine NO-LOCK NO-ERROR.

   RUN pShipOrderLineHistory.
   RUN pPartLookup.
   RUN pCustomerBrowse.
   RUN pCustomerDetails.
      
   FIND Customer WHERE Customer.CustomerID = intSelectedCustomer NO-LOCK NO-ERROR.

   RUN pCustomerHistory.
   RUN pAddressBrowse.
   RUN pAddressDetails.

   FIND Address WHERE Address.AddressID = intSelectedAddress NO-LOCK NO-ERROR.

   RUN pAddressHistory.
   RUN pShipOrderFilter.
   
   RUN pShipOrderEntryUpload.
   RUN pReadFile.
   RUN pUploadShipOrderEntryBrowse.
   
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   /* ShipOrder Objects */
   DELETE OBJECT ShipOrderBrowse                        NO-ERROR.
   DELETE OBJECT ShipOrderBrowseButtons                 NO-ERROR.   
   DELETE OBJECT ShipOrderBrowseFrame                   NO-ERROR.
   DELETE OBJECT ShipOrderDetailsButtons                NO-ERROR.
   DELETE OBJECT ShipOrderDetailsForm                   NO-ERROR.

   /* ShipOrderHistory Objects */
   DELETE OBJECT ShipOrderHistoryBrowse                 NO-ERROR.
   DELETE OBJECT ShipOrderHistoryBrowseButtons          NO-ERROR.
   DELETE OBJECT ShipOrderHistoryBrowseForm             NO-ERROR.
   
   /* ShipOrder Filter Objects */
   DELETE OBJECT ShipOrderBrowseFilterButtons           NO-ERROR.
   DELETE OBJECT ShipOrderBrowseFilterForm              NO-ERROR.

   /* ShipOrderLine Objects */
   DELETE OBJECT ShipOrderLineBrowse                    NO-ERROR.
   DELETE OBJECT ShipOrderLineBrowseButtons             NO-ERROR.
   DELETE OBJECT ShipOrderLineBrowseForm                NO-ERROR.
   DELETE OBJECT ShipOrderLineDetailsButtons            NO-ERROR.
   DELETE OBJECT ShipOrderLineDetailsForm               NO-ERROR.

   /* Part Lookup Objects */
   DELETE OBJECT PartLookupBrowse                       NO-ERROR.
   DELETE OBJECT PartLookupBrowseButtons                NO-ERROR.
   DELETE OBJECT PartLookupBrowseForm                   NO-ERROR.

   /* ShipOrderLineHistory Objects */
   DELETE OBJECT ShipOrderLineHistoryBrowse             NO-ERROR.
   DELETE OBJECT ShipOrderLineHistoryBrowseButtons      NO-ERROR.
   DELETE OBJECT ShipOrderLineHistoryBrowseForm         NO-ERROR.

   /* Customer Objects */
   DELETE OBJECT CustomerBrowse                         NO-ERROR.
   DELETE OBJECT CustomerBrowseButtons                  NO-ERROR.
   DELETE OBJECT CustomerBrowseForm                     NO-ERROR.
   DELETE OBJECT CustomerDetailsButtons                 NO-ERROR.
   DELETE OBJECT CustomerDetailsForm                    NO-ERROR.

   /* Address Objects */
   DELETE OBJECT AddressBrowse                          NO-ERROR.
   DELETE OBJECT AddressBrowseButtons                   NO-ERROR.
   DELETE OBJECT AddressBrowseForm                      NO-ERROR.
   DELETE OBJECT AddressDetailsButtons                  NO-ERROR.
   DELETE OBJECT AddressDetailsForm                     NO-ERROR.

   /* AddressHistory Objects */
   DELETE OBJECT AddressHistoryBrowse                   NO-ERROR.
   DELETE OBJECT AddressHistoryBrowseButtons            NO-ERROR.
   DELETE OBJECT AddressHistoryBrowseForm               NO-ERROR.
   
   /* Objects for File Browse */
   DELETE OBJECT  FileSelectionForm                    NO-ERROR.
   DELETE OBJECT  FileBrowseForm                       NO-ERROR.
   DELETE OBJECT  FileBrowse                           NO-ERROR.
   DELETE OBJECT  FileBrowseButtons                    NO-ERROR.
   DELETE OBJECT  FileDetailsButtons                   NO-ERROR.
    
   /* Objects for UploadShipOrderEntry Browse */
   DELETE OBJECT  UploadShipOrderEntryBrowse           NO-ERROR.
   DELETE OBJECT  UploadShipOrderEntryBrowseButtons    NO-ERROR.
   DELETE OBJECT  UploadShipOrderEntryBrowseForm       NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetShipOrderRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetShipOrderRows Procedure 
PROCEDURE pSetShipOrderRows :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   ASSIGN decShipOrderValue = 0
          intNumParts       = 0.

   FIND FIRST ShipOrderStatus NO-LOCK
      WHERE   ShipOrderStatus.ShipOrderStatusID = ShipOrder.ShipOrderStatusID NO-ERROR.

   FIND FIRST Address NO-LOCK 
      WHERE   Address.AddressID = ShipOrder.AddressID NO-ERROR.

   FOR EACH    ShipOrderLine OF ShipOrder NO-LOCK
      BREAK BY ShipOrderLine.PartID:
      
      FIND FIRST   Part OF ShipOrderLine NO-LOCK NO-ERROR.
      IF AVAILABLE Part THEN
         decShipOrderValue = decShipOrderValue + (Part.UnitValue * ShipOrderLine.QtyOrdered).

      IF FIRST-OF(ShipOrderLine.PartID) THEN
         intNumParts = intNumParts + 1.

   END. /* FOR EACH ShipOrderLine */
     
   ShipOrderBrowse:startRow(ShipOrder.ShipOrderID, "selectShipOrderRow(this," + '"' + STRING(ShipOrder.ShipOrderID) + '"' + ");", "").
   ShipOrderBrowse:insertData(ShipOrder.OrderRef).

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalBrowseFields.i ShipOrder}
         
   ShipOrderBrowse:insertData(IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusName ELSE "", "LEFT").
   ShipOrderBrowse:insertData(IF AVAILABLE Customer THEN Customer.CustomerName ELSE "", "LEFT").
   ShipOrderBrowse:insertData(IF AVAILABLE Address  THEN Address.AddressLine1  ELSE "", "LEFT").
   ShipOrderBrowse:insertData(STRING(intNumParts), "RIGHT").
   ShipOrderBrowse:insertData(STRING(ShipOrder.DateToShip,"99/99/9999"), "RIGHT").
   ShipOrderBrowse:insertData(STRING(ROUND(decShipOrderValue, 2)), "RIGHT").
   ShipOrderBrowse:insertData(fDisplayDate&Time(ShipOrder.Created,"d/m/y H:M"), "RIGHT").

   /* Add hidden fields */
   ShipOrderBrowse:insertHiddenData("ShipOrderVersionID",  ShipOrder.VersionID).
   ShipOrderBrowse:insertHiddenData("ShipOrderRef",         ShipOrder.OrderRef).
   ShipOrderBrowse:insertHiddenData("AddressID",           ShipOrder.AddressID).
   ShipOrderBrowse:insertHiddenData("BusinessUnitID", ShipOrder.BusinessUnitID).
   ShipOrderBrowse:insertHiddenData("ShipOrderStatusCode", IF AVAILABLE ShipOrderStatus THEN ShipOrderStatus.StatusCode ELSE "").
             
   ShipOrderBrowse:endRow().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderBrowse Procedure 
PROCEDURE pShipOrderBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "shiporderentry_details_form"}
        
   ShipOrderBrowse              = NEW browseTable("shiporderentry_browse").
   ShipOrderBrowse:BrowseWidth  = 965.
   ShipOrderBrowse:BrowseHeight = 455.
   ShipOrderBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   ShipOrderBrowse:insertColumn(fTL("OrderRef"),  120, "CHARACTER"). 

   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ShipOrder}
      
   ShipOrderBrowse:insertColumn(fTL("Status"),    120, "CHARACTER", "LEFT").
   ShipOrderBrowse:insertColumn(fTL("Customer"),  150, "CHARACTER", "LEFT").
   ShipOrderBrowse:insertColumn(fTL("Address"),   160, "CHARACTER", "LEFT").
   ShipOrderBrowse:insertColumn(fTL("Parts"),      40, "CHARACTER", "LEFT").
   ShipOrderBrowse:insertColumn(fTL("Ship Date"),  80, "CHARACTER", "LEFT").
   ShipOrderBrowse:insertColumn(fTL("Value"),      60, "INTEGER",   "LEFT").
   ShipOrderBrowse:insertColumn(fTL("Created"),   110, "CHARACTER", "LEFT").
     
   /*Body*/
   ShipOrderBrowse:startBody().

   IF chrFilteredOrderRef <> "" THEN
   DO:
      FIND FIRST ShipOrder NO-LOCK
         WHERE   ShipOrder.OrderRef = chrFilteredOrderRef NO-ERROR.
      IF AVAILABLE ShipOrder AND fCanViewBusinessUnit(intGblSessionID,ShipOrder.BusinessUnitID) THEN
         RUN pSetShipOrderRows.
      ELSE
         chrPageBuildError = chrPageBuildError + "OrderRef: " + chrFilteredOrderRef 
                                + " does not exist. Please try another.".

   END. /* IF chrFilteredOrderRef <> "" */
   ELSE
   DO:      
      /* Query Open ShipOrders for Business Unit of Logged in User */
      CustomerLoop:
      FOR EACH Customer NO-LOCK /* idx=BusinessUnitIDManuallyCreated */ 
         WHERE Customer.ManuallyCreated
         BY    Customer.CustomerName:

         IF NOT fCanViewBusinessUnit(intGblSessionID,Customer.BusinessUnitID) AND Customer.BusinessUnitID <> 0 THEN 
            NEXT CustomerLoop.
         
         IF intFilteredCustomerID <> 0 AND Customer.CustomerID <> intFilteredCustomerID THEN
            NEXT CustomerLoop.
         
         OrderLoop:
         FOR EACH ShipOrder NO-LOCK  /*idx=ShipOrderID*/
            WHERE ShipOrder.CustomerID = Customer.CustomerID
            AND   ShipOrder.Shipped = ""
            BY    ShipOrder.OrderRef:
            
            IF chrFilteredShipDate <> "" AND ShipOrder.DateToShip <> DATE(chrFilteredShipDate) THEN
               NEXT OrderLoop.
            
            IF intFilteredStatusID <> 0 AND ShipOrder.ShipOrderStatusID <> intFilteredStatusID THEN
               NEXT OrderLoop.
            
            RUN pSetShipOrderRows.
               
         END. /* FOR EACH ShipOrder NO-LOCK */
   
      END. /* FOR EACH Customer */   

   END. /* IF chrFilteredOrderRef = "" */

   ShipOrderBrowse:endTable().
   chrPageBuildError = chrPageBuildError + ShipOrderBrowse:getErrors().
   
   /* Create a new frame */
   ShipOrderBrowseFrame           = NEW pageFrame().
   ShipOrderBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   ShipOrderBrowseFrame:formOpen("shiporderentry_browse_form").
   
   /* Start the Frame Header */
   ShipOrderBrowseFrame:insertSpacer(5).
   ShipOrderBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   ShipOrderBrowse:displayBrowse().  
   
   /* End the Frame Header */
   ShipOrderBrowseFrame:frameClose().
   ShipOrderBrowseFrame:insertSpacer(10).
   
   /* Hidden Values - Browse Level*/
   ShipOrderBrowseFrame:insertHiddenField("form_name","shiporderentry_browse_form").
   ShipOrderBrowseFrame:insertHiddenField("prog_name","adOrderEntry.p").  
   ShipOrderBrowseFrame:insertHiddenField("shiporderentry_browse_scroll","").
   ShipOrderBrowseFrame:insertHiddenField("popup_shiporderhistory_browse", "").
   ShipOrderBrowseFrame:insertHiddenField("popup_shiporderline_browse","").
   ShipOrderBrowseFrame:insertHiddenField("popup_shiporderlinehistory_browse","").
   ShipOrderBrowseFrame:insertHiddenField("popup_customer_browse","").
   ShipOrderBrowseFrame:insertHiddenField("popup_address_browse","").
   ShipOrderBrowseFrame:insertHiddenField("popup_shiporderentryupload_browse_form","").
   
   /* Hidden Values - Filters */
   ShipOrderBrowseFrame:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   ShipOrderBrowseFrame:insertHiddenField("FilteredShipDate", chrFilteredShipDate).
   ShipOrderBrowseFrame:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   ShipOrderBrowseFrame:insertHiddenField("FilteredStatusID", STRING(intFilteredStatusID)).

   /* Hidden Values - Keys */
   ShipOrderBrowseFrame:insertHiddenField("ShipOrderID","").
   ShipOrderBrowseFrame:insertHiddenField("ShipOrderRef", "").
   ShipOrderBrowseFrame:insertHiddenField("ShipOrderVersionID","").
   ShipOrderBrowseFrame:insertHiddenField("ShipOrderLineID", "").
   ShipOrderBrowseFrame:insertHiddenField("AddressID", "").
   ShipOrderBrowseFrame:insertHiddenField("BusinessUnitID", "").
   ShipOrderBrowseFrame:insertHiddenField("ShipOrderStatusCode", "").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderBrowseFrame}
   
   ShipOrderBrowseFrame:formClose().
   
   /* Create Button Bar */
   ShipOrderBrowseButtons           = NEW buttonBar().
   ShipOrderBrowseButtons:WebStream = STREAM WebStream:HANDLE.
           
   ShipOrderBrowseButtons:addButton("shiporderentry_browse_form_btn_filter",
                                    fTL("Filter"),
                                    "viewShipOrderFilter('shiporder_filter_form');"). 
   
   ShipOrderBrowseButtons:addButton("shiporderentry_browse_form_btn_details",
                                    fTL("Create / Edit"),
                                    "viewShipOrderDetails('shiporderentry_details_form');").

   ShipOrderBrowseButtons:addButton("shiporderentry_browse_form_btn_delete",
                                    fTL("Delete"),
                                    "confirmDeleteShipOrder('shiporderentry_details_form');",
                                    "Disabled").
   
   ShipOrderBrowseButtons:addButton("shiporderentry_browse_form_btn_orderlines",
                                    fTL("Order Lines"),
                                    "viewShipOrderLineBrowse();",
                                    "Disabled").

   ShipOrderBrowseButtons:addButton("shiporderentry_browse_form_btn_customer",
                                    fTL("Customers"),
                                    "viewCustomerBrowse();").

   ShipOrderBrowseButtons:addButton("shiporderentry_browse_form_btn_history",
                                    fTL("History"),
                                    "viewShipOrderHistory();",
                                    "Disabled").
                                    
   ShipOrderBrowseButtons:addButton("shiporderentry_browse_form_btn_upload",
                                    fTL("Upload"),
                                    "viewShipOrderEntryUpload('shiporder_selection_form');").
      
   ShipOrderBrowseButtons:closeBar().  
   ShipOrderBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderDetails Procedure 
PROCEDURE pShipOrderDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "shiporderentry_details_form"}

   ASSIGN chrDisplayFieldList  = "BusinessUnitID,ShipOrderID,OrderRef,CustomerID,AddressID,AddressLine2,AddressLine3,AddressLine4"
                                 + ",AddressLine5,PostalCode,DateToShip,Priority,CarrierID,CarrierSortationID,LanguageID,CurrencyID"
                                 + ",ShipOrderTypeID,ShipOrderStreamID,CustShipOrderRef"
          chrEditFieldList     = "BusinessUnitID,OrderRef,DateToShip,Priority"
          chrNewFieldList      = "BusinessUnitID,OrderRef,CustomerID,AddressID,DateToShip,Priority," +
                                 "CarrierID,CarrierSortationID,LanguageID,CurrencyID,ShipOrderTypeID,ShipOrderStreamID"
          chrRequiredFieldList = "OrderRef,CustomerID,AddressID,DateToShip,CarrierID,CarrierSortationID,LanguageID,CurrencyID"
                                 + ",ShipOrderTypeID,ShipOrderStreamID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   ShipOrderDetailsForm             = NEW dataForm("shiporderentry_details_form").
   ShipOrderDetailsForm:WebStream   = STREAM WebStream:HANDLE.
   ShipOrderDetailsForm:FormAction  = "dbShipOrderUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

      /* Setup */
   ShipOrderDetailsForm:FormWidth   = 580.
   ShipOrderDetailsForm:FormHeight  = 442.
   ShipOrderDetailsForm:FormTitle   = "ShipOrder Details".
   ShipOrderDetailsForm:FormType    = "large".
   
   /* Column Layout */
   ShipOrderDetailsForm:insertPaddingColumn(20).
   ShipOrderDetailsForm:insertColumn(90).
   ShipOrderDetailsForm:insertColumn(120).
   ShipOrderDetailsForm:insertColumn(20).
   ShipOrderDetailsForm:insertColumn(4).
   ShipOrderDetailsForm:insertColumn(110).
   
   /* Fields */
   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel("ShipOrder ID").
   ShipOrderDetailsForm:insertTextField("ShipOrderID", "", 110, TRUE).

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel("Business Unit").
   ShipOrderDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK /*idx=ActiveListingSequence*/
      WHERE BusinessUnit.Active 
      BY    BusinessUnit.ListingSequence:

      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.
      
      ShipOrderDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID) , BusinessUnit.UnitName).
   END.


   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Order Ref")).
   ShipOrderDetailsForm:insertTextField("OrderRef", chrOrderRef, 300, TRUE).
   
   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Cust Order Ref")).
   ShipOrderDetailsForm:insertTextField("CustShipOrderRef", "", 300, TRUE).
   
   /* refreshSelect function is in syngate.js */
   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Customer")).
   ShipOrderDetailsForm:insertComboField("CustomerID", "0", 300, TRUE,
                                         "refreshAddress('shiporderentry_details_form', 'AddressID', 'Address',
                                         'CustomerID',this.value,'AddressLine1');").
   ShipOrderDetailsForm:insertComboPairs("CustomerID", "0", "Please select a Customer...").
   CustomerLoop:
   FOR EACH Customer NO-LOCK /* idx=BusinessUnitIDManuallyCreated */
      WHERE Customer.ManuallyCreated      
      AND   Customer.Active
      BY    Customer.CustomerName:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,Customer.BusinessUnitID) AND Customer.BusinessUnitID <> 0 THEN 
         NEXT CustomerLoop.

      ShipOrderDetailsForm:insertComboPairs("CustomerID", STRING(Customer.CustomerID), Customer.CustomerName).

   END.
                       
   /* Need Javascript so that on change will populate AddressLine2-5 and Postal Code */
   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Address Line1")).
   ShipOrderDetailsForm:insertComboField("AddressID", STRING(intAddressID), 300, FALSE,
                                         "populateAddress('shiporderentry_details_form', this.value);").
   ShipOrderDetailsForm:insertComboPairs("AddressID", "0", "None Selected").


   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTl("Address Line2")).
   ShipOrderDetailsForm:insertTextField("AddressLine2", "", 300, TRUE).

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Address Line3")).
   ShipOrderDetailsForm:insertTextField("AddressLine3", "", 300, TRUE).

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("City")).
   ShipOrderDetailsForm:insertTextField("AddressLine4", "", 300, TRUE).

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTl("State/Province")).
   ShipOrderDetailsForm:insertTextField("AddressLine5", "", 300, TRUE).

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Postal Code")).
   ShipOrderDetailsForm:insertTextField("PostalCode", "", 300, TRUE).

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Date To Ship")).
   ShipOrderDetailsForm:insertDateField("DateToShip", "", 100, TRUE).

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Priority Order")).
   ShipOrderDetailsForm:insertComboField("Priority", "", 300, TRUE).
   ShipOrderDetailsForm:insertComboPairs("Priority", "0", "Is Not Priority Order").
   ShipOrderDetailsForm:insertComboPairs("Priority", "1", "Is Priority Order").


   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Carrier")).
   ShipOrderDetailsForm:insertComboField("CarrierID", "", 300, TRUE, "changeSortation('shiporderentry_details_form')").
   ShipOrderDetailsForm:insertComboPairs("CarrierID", "", "None Selected"). 
   CarrierLoop:
   FOR EACH Carrier NO-LOCK /*idx=CarrierName*/
      WHERE Carrier.Active
      BY    Carrier.CarrierName:

      ShipOrderDetailsForm:insertComboPairs("CarrierID", STRING(Carrier.CarrierID), Carrier.CarrierName).
   END.
  
   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("CarrierSortation")).
   ShipOrderDetailsForm:insertComboField("CarrierSortationID", "", 300, TRUE).
   ShipOrderDetailsForm:insertComboPairs("CarrierSortationID", "", "None Selected").

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Currency")).
   ShipOrderDetailsForm:insertComboField("CurrencyID", "2", 300, TRUE).
   ShipOrderDetailsForm:insertComboPairs("CurrencyID", "", "None Selected").
   CurrencyLoop:
   FOR EACH Currency NO-LOCK /*idx=CurrencyName*/
      WHERE Currency.Active
      BY    Currency.CurrencyName:

      ShipOrderDetailsForm:insertComboPairs("CurrencyID", STRING(Currency.CurrencyID), Currency.CurrencyName).
   END.

   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Language")).
   ShipOrderDetailsForm:insertComboField("LanguageID", "1", 300, TRUE).
   ShipOrderDetailsForm:insertComboPairs("LanguageID", "", "None Selected").
   LanguageLoop:
   FOR EACH Language NO-LOCK /*idx=LanguageName*/
      WHERE Language.Active
      BY    Language.LanguageName:

      ShipOrderDetailsForm:insertComboPairs("LanguageID", STRING(Language.LanguageID), Language.LanguageName).
   END.
   
   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("ShipOrder Type")).
   ShipOrderDetailsForm:insertComboField("ShipOrderTypeID", "", 300, TRUE).
   ShipOrderDetailsForm:insertComboPairs("ShipOrderTypeID", "", "None Selected").
   FOR EACH ShipOrderType NO-LOCK /*idx=LanguageName*/
      WHERE ShipOrderType.Active
      BY    ShipOrderType.TypeCode:

      ShipOrderDetailsForm:insertComboPairs("ShipOrderTypeID", STRING(ShipOrderType.ShipOrderTypeID), ShipOrderType.TypeName).
   END.
   
   ShipOrderDetailsForm:startRow().
   ShipOrderDetailsForm:insertLabel(fTL("Stream")).
   ShipOrderDetailsForm:insertComboField("ShipOrderStreamID", "", 300, TRUE).
   ShipOrderDetailsForm:insertComboPairs("ShipOrderStreamID", "", "None Selected").
   FOR EACH ShipOrderStream NO-LOCK /*idx=LanguageName*/
      WHERE ShipOrderStream.Active
      BY    ShipOrderStream.StreamCode:

      ShipOrderDetailsForm:insertComboPairs("ShipOrderStreamID", STRING(ShipOrderStream.ShipOrderStreamID), ShipOrderStream.StreamName).
   END.

   /* Hidden Fields - Browse level*/
   ShipOrderDetailsForm:insertHiddenField("shiporderentry_browse_scroll", "").
   ShipOrderDetailsForm:insertHiddenField("form_name", "shiporderentry_details_form").
   ShipOrderDetailsForm:insertHiddenField("prog_name", "adOrderEntry.p").

   /* Hidden Fields - Filtered */
   ShipOrderDetailsForm:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   ShipOrderDetailsForm:insertHiddenField("FilteredShipDate", chrFilteredShipDate).
   ShipOrderDetailsForm:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   ShipOrderDetailsForm:insertHiddenField("FilteredStatusID", STRING(intFilteredStatusID)).
   ShipOrderDetailsForm:insertHiddenField("ShipOrderID","").
   ShipOrderDetailsForm:insertHiddenField("ShipOrderRef", "").
   ShipOrderDetailsForm:insertHiddenField("ShipOrderVersionID","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderDetailsForm}
                                               
   /* Create Button Bar */
   ShipOrderDetailsButtons = NEW buttonBar().

   ShipOrderDetailsButtons:addButton("shiporderentry_details_form_btn_create",
                                     fTL("Create"),
                                     "createShipOrder('shiporderentry_details_form');").
 
   ShipOrderDetailsButtons:addButton("shiporderentry_details_form_btn_save", 
                                     fTL("Save"), 
                                     "updateShipOrder('shiporderentry_details_form');").
   
   ShipOrderDetailsButtons:addButton("shiporderentry_details_form_btn_cancel", 
                                     fTL("Cancel"), 
                                     "cancelUpdate('UserCancelled','process_mode'); disablePopup('shiporderentry_details_form_popup');").
   
   ShipOrderDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   ShipOrderDetailsForm:FormButtons = ShipOrderDetailsButtons.
   ShipOrderDetailsForm:endForm(). 
   
   ShipOrderDetailsForm:displayForm(). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderEntryUpload) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderEntryUpload Procedure
PROCEDURE pShipOrderEntryUpload:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "shiporderentry_details_form"}

   FileSelectionForm            = NEW dataForm("shiporder_selection_form").
   FileSelectionForm:WebStream  = STREAM WebStream:HANDLE.
   FileSelectionForm:FormAction = "dbOrderEntryUploadUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileSelectionForm:FormWidth  = 460.
   FileSelectionForm:FormHeight = 200.
   FileSelectionForm:FormTitle  = "Select Input File".
   FileSelectionForm:FormType   = "small_xwide".
   
   /* Column Layout */
   FileSelectionForm:insertPaddingColumn(15).
   
   /* Fields */
/*   {webGetOptionalFormFields.i pShipOrderDetails}*/
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel(fTL("Customer")).
   FileSelectionForm:insertComboField("CustomerID", "0", 300, TRUE).
   FileSelectionForm:insertComboPairs("CustomerID", "0", "Please select a Customer...").
   CustomerLoop:
   FOR EACH Customer NO-LOCK /* idx=BusinessUnitIDManuallyCreated */
      WHERE Customer.ManuallyCreated      
      AND   Customer.Active
      BY    Customer.CustomerName:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,Customer.BusinessUnitID) AND Customer.BusinessUnitID <> 0 THEN 
         NEXT CustomerLoop.

      FileSelectionForm:insertComboPairs("CustomerID", STRING(Customer.CustomerID), Customer.CustomerName).

   END.
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel(fTL("Ship Order Type")).
   FileSelectionForm:insertComboField("ShipOrderTypeID", "0", 300, TRUE).
   FileSelectionForm:insertComboPairs("ShipOrderTypeID", "0", "Please select a Ship Order Type...").
   FOR EACH ShipOrderType NO-LOCK /*idx=ShipOrderID*/
      WHERE ShipOrderType.Active = TRUE:
      
      FileSelectionForm:insertComboPairs("ShipOrderTypeID", STRING(ShipOrderType.ShipOrderTypeID), 
                                          ShipOrderType.TypeName).    
   END.
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel(fTL("Ship Order Stream")).
   FileSelectionForm:insertComboField("ShipOrderStreamID", "0", 300, TRUE).
   FileSelectionForm:insertComboPairs("ShipOrderStreamID", "0", "Please select a Ship Order Stream...").
   FOR EACH ShipOrderStream NO-LOCK /*idx=ShipOrderStreamID*/
      WHERE ShipOrderStream.Active = TRUE:
      
      FileSelectionForm:insertComboPairs("ShipOrderStreamID", STRING(ShipOrderStream.ShipOrderStreamID), 
                                          ShipOrderStream.StreamName).    
   END.
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel(fTL("Carrier")).
   FileSelectionForm:insertComboField("CarrierID", "", 300, TRUE, "changeSortation('shiporder_selection_form')").
   FileSelectionForm:insertComboPairs("CarrierID", "0", "Please select a Carrier..."). 
   CarrierLoop:
   FOR EACH Carrier NO-LOCK /*idx=CarrierName*/
      WHERE Carrier.Active
      BY    Carrier.CarrierName:

      FileSelectionForm:insertComboPairs("CarrierID", STRING(Carrier.CarrierID), Carrier.CarrierName).
   END.
  
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel(fTL("Carrier Sortation")).
   FileSelectionForm:insertComboField("CarrierSortationID", "", 300, TRUE).
   FileSelectionForm:insertComboPairs("CarrierSortationID", "", "None Selected"). 
   
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel(fTL("Order Ref")).
   FileSelectionForm:insertTextField("OrderRefField", "", 300, TRUE).
      
   FileSelectionForm:startRow().
   FileSelectionForm:insertLabel("").
   FileSelectionForm:insertFileInput("UploadFile",300,TRUE).
   
   /* Add Hidden Fields*/
   FileSelectionForm:insertHiddenField("shiporder_browse_scroll", "").
   FileSelectionForm:insertHiddenField("process_mode", "").
   FileSelectionForm:insertHiddenField("popup_shiporder_selection_form","").
   FileSelectionForm:insertHiddenField("popup_shiporderentryupload_browse_form","").
   FileSelectionForm:insertHiddenField("shiporderID", "").
   FileSelectionForm:insertHiddenField("form_name", "shiporder_selection_form").
   FileSelectionForm:insertHiddenField("prog_name", "adOrderEntry.p").
   FileSelectionForm:insertHiddenField("CustomerName", chrCustomerName).
   FileSelectionForm:insertHiddenField("ShipOrderTypeName", chrShipOrderType).
   FileSelectionForm:insertHiddenField("ShipOrderStream", chrShipOrderStream).
   FileSelectionForm:insertHiddenField("OrderRef", chrOrderRefUpload).
   FileSelectionForm:insertHiddenField("Carrier", chrCarrier).
   FileSelectionForm:insertHiddenField("CarrierSortation", chrCarrierSortation).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileSelectionForm}
   
   /* Create Button Bar */
   FileDetailsButtons = NEW buttonBar().
   FileDetailsButtons:addButton("shiporder_selection_form_btn_save", 
                                fTL("Upload"), 
                                "viewShipOrderEntryBrowse('shiporder_selection_form');").
   
   FileDetailsButtons:addButton("shiporder_selection_form_btn_cancel", 
                                fTL("Cancel"), 
                                "cancelUpdate('UserCancelled','process_mode'); disablePopup('shiporder_selection_form_popup');").
   
   FileDetailsButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   FileSelectionForm:FormButtons = FileDetailsButtons.
   FileSelectionForm:endForm(). 
   FileSelectionForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pShipOrderFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderFilter Procedure 
PROCEDURE pShipOrderFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   ShipOrderBrowseFilterForm = NEW dataForm("shiporder_filter_form").
   ShipOrderBrowseFilterForm:Webstream  = STREAM WebStream:HANDLE.
   ShipOrderBrowseFilterForm:FormAction = "adOrderEntry.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   ShipOrderBrowseFilterForm:FormWidth  = 350.
   ShipOrderBrowseFilterForm:FormHeight = 200.
   ShipOrderBrowseFilterForm:FormTitle  = "ShipOrder Filter".
   ShipOrderBrowseFilterForm:FormType   = "small_wide".

   /* Column Layout */
   ShipOrderBrowseFilterForm:insertPaddingColumn(30).
   ShipOrderBrowseFilterForm:insertColumn(90).
   ShipOrderBrowseFilterForm:insertColumn(120).
   ShipOrderBrowseFilterForm:insertColumn(20).
   ShipOrderBrowseFilterForm:insertColumn(4).
   ShipOrderBrowseFilterForm:insertColumn(80).

   /* Fields */
   ShipOrderBrowseFilterForm:startRow().
   ShipOrderBrowseFilterForm:insertLabel(fTL("Order Ref")).
   ShipOrderBrowseFilterForm:insertTextField("FilteredOrderRef", chrFilteredOrderRef, 150, TRUE).

   ShipOrderBrowseFilterForm:startRow().
   ShipOrderBrowseFilterForm:insertLabel(fTL("Ship Date")).
   ShipOrderBrowseFilterForm:insertDateField("FilteredShipDate", chrFilteredShipDate, 150, TRUE).

   ShipOrderBrowseFilterForm:startRow().
   ShipOrderBrowseFilterForm:insertLabel(fTL("Customer")).
   ShipOrderBrowseFilterForm:insertComboField("FilteredCustomerID", STRING(intFilteredCustomerID), 150, TRUE).
   ShipOrderBrowseFilterForm:insertComboPairs("FilteredCustomerID", "0", "All Customers...").
   
   CustomerLoop:
   FOR EACH Customer NO-LOCK /*idx=CustomerID*/
      WHERE Customer.ManuallyCreated:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID, Customer.BusinessUnitID) AND Customer.BusinessUnitID <> 0 THEN 
         NEXT CustomerLoop.
      
      ShipOrderBrowseFilterForm:insertComboPairs("FilteredCustomerID", STRING(Customer.CustomerID), Customer.CustomerName).

   END. /* FOR EACH Customer */

   ShipOrderBrowseFilterForm:startRow().
   ShipOrderBrowseFilterForm:insertLabel(fTL("Order Status")).
   ShipOrderBrowseFilterForm:insertComboField("FilteredStatusID", STRING(intFilteredStatusID), 150, TRUE).
   ShipOrderBrowseFilterForm:insertComboPairs("FilteredStatusID", "0", "All Order Statuses...").
   FOR EACH ShipOrderStatus NO-LOCK /*idxShipOrderStatusID*/
      WHERE ShipOrderStatus.Active
      BY    ShipOrderStatus.ListingSequence:

      ShipOrderBrowseFilterForm:insertComboPairs("FilteredStatusID", STRING(ShipOrderStatus.ShipOrderStatusID), ShipOrderStatus.StatusName).

   END. /* FOR EACH ShipOrderStatus */

   /* Add Hidden Fields */
   ShipOrderBrowseFilterForm:insertHiddenField("form_name", "shiporder_filter_form").
   ShipOrderBrowseFilterForm:insertHiddenField("prog_name", "adOrderEntry.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderBrowseFilterForm}
   
   /* Create Button Bar */
   ShipOrderBrowseFilterButtons = NEW buttonBar().

   ShipOrderBrowseFilterButtons:addButton("shiporder_filter_form_btn_search",
                                          fTL("Filter"),
                                          "filterShipOrder('shiporder_filter_form')").

   ShipOrderBrowseFilterButtons:addButton("shiporder_filter_form_btn_cancel",
                                          fTL("Cancel"),
                                          "disablePopup('shiporder_filter_form_popup');").

   ShipOrderBrowseFilterButtons:closeBar().
      
   /* Assign the Button Bar Object to the Form Object */
   ShipOrderBrowseFilterForm:FormButtons = ShipOrderBrowseFilterButtons.

   ShipOrderBrowseFilterForm:endForm().
   ShipOrderBrowseFilterForm:displayForm().



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderHistory Procedure 
PROCEDURE pShipOrderHistory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   ShipOrderHistoryBrowseForm = NEW dataForm("shiporderhistory_browse_form").
   ShipOrderHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   ShipOrderHistoryBrowseForm:FormWidth  = 860.
   ShipOrderHistoryBrowseForm:FormHeight = 530.
   ShipOrderHistoryBrowseForm:FormTitle  = fTL("ShipOrder History") 
                                           + (IF AVAILABLE ShipOrder THEN
                                              ": " + ShipOrder.OrderRef + "&nbsp ID:" + STRING(ShipOrder.ShipOrderID)   
                                              ELSE "").
   ShipOrderHistoryBrowseForm:FormType   = "xxl_large".
   
   ShipOrderHistoryBrowse = NEW browseTable("shiporderhistory_browse").
   ShipOrderHistoryBrowse:BrowseWidth  = 840.
   ShipOrderHistoryBrowse:BrowseHeight = 490.
   
   ShipOrderHistoryBrowse:insertColumn(fTL("ID"), 80, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ShipOrderHistory}
   
   ShipOrderHistoryBrowse:insertColumn(fTL("Gate User"), 120, "CHARACTER", "left").
   ShipOrderHistoryBrowse:insertColumn(fTL("Created"),   100, "CHARACTER", "left").
   ShipOrderHistoryBrowse:insertColumn(fTL("ShipDate"),   65, "DATE",      "left").
   ShipOrderHistoryBrowse:insertColumn(fTL("Address"),   130, "CHARACTER", "left").   
   ShipOrderHistoryBrowse:insertColumn(fTL("Status"),    130, "CHARACTER", "left").
   ShipOrderHistoryBrowse:insertColumn(fTL("Approved"),   70, "LOGICAL",   "left").
   
   ShipOrderHistoryBrowse:StartBody().
   
   IF AVAILABLE ShipOrder THEN
   DO:
      /* List the ShipOrderHistorys for the ShipOrder */
      FOR EACH ShipOrderHistory NO-LOCK /* idx=ShipOrderID */
         WHERE ShipOrderHistory.ShipOrderID = ShipOrder.ShipOrderID 
         BY    ShipOrderHistory.ShipOrderID
         BY    ShipOrderHistory.Created DESCENDING:

         FIND FIRST GateUser OF ShipOrderHistory NO-LOCK NO-ERROR.         
         FIND FIRST Address  OF ShipOrderHistory NO-LOCK NO-ERROR.
         FIND FIRST ShipOrderStatus NO-LOCK
            WHERE   ShipOrderStatus.ShipOrderStatusID = ShipOrderHistory.ShipOrderStatusID NO-ERROR.
                 
         ShipOrderHistoryBrowse:startRow(ShipOrderHistory.ShipOrderHistoryID, "selectShipOrderHistoryRow(this," 
                                         + '"' + STRING(ShipOrderHistory.ShipOrderHistoryID)
                                         + '","adOrderEntry.p","shiporder_browse_form"' + ");", "").

         ShipOrderHistoryBrowse:insertData(ShipOrderHistory.ShipOrderHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i ShipOrderHistory}
         
         ShipOrderHistoryBrowse:insertData(IF AVAILABLE GateUser 
                                           THEN GateUser.FullName ELSE "", "left").
         ShipOrderHistoryBrowse:insertData(fDisplayDate&Time(ShipOrderHistory.Created, "d/m/y H:M")).
         ShipOrderHistoryBrowse:insertData(STRING(ShipOrderHistory.DateToShip), " right").
         ShipOrderHistoryBrowse:insertData(IF AVAILABLE Address 
                                           THEN STRING(ShipOrderHistory.AddressID) + ": " + Address.AddressLine1 
                                           ELSE STRING(ShipOrderHistory.AddressID), "left").
         ShipOrderHistoryBrowse:insertData(IF AVAILABLE ShipOrderStatus 
                                           THEN ShipOrderStatus.StatusName 
                                           ELSE "", "left"). 
         ShipOrderHistoryBrowse:insertData(STRING(ShipOrderHistory.ApproveOrderForGeneratePick, "Yes/No"), "left").

         ShipOrderHistoryBrowse:endRow().
      
      END. /* FOR EACH ShipOrderHistory OF ShipOrder NO-LOCK */
   END. /*IF AVAILABLE ShipOrder THEN*/
   
   ShipOrderHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + ShipOrderHistoryBrowse:getErrors().

   ShipOrderHistoryBrowseForm:insertHiddenField("popup_shiporderhistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderHistoryBrowseForm}
   
   /* Create Button Bar */
   ShipOrderHistoryBrowseButtons = NEW buttonBar().
          
   ShipOrderHistoryBrowseButtons:addButton("shiporderhistory_browse_form_btn_cancel",
                                           fTL("Cancel"),
                                           "disablePopup('shiporderhistory_browse_form_popup');").
   
   ShipOrderHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ShipOrderHistoryBrowseForm:FormBrowse  = ShipOrderHistoryBrowse.
   ShipOrderHistoryBrowseForm:FormButtons = ShipOrderHistoryBrowseButtons.
   ShipOrderHistoryBrowseForm:endForm(). 
   
   ShipOrderHistoryBrowseForm:displayForm().


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderLineBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderLineBrowse Procedure 
PROCEDURE pShipOrderLineBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "shiporderline_details_form"}

   chrOrderLineFormTitle = fTL("Lines of Order").
   IF AVAILABLE ShipOrder THEN
   DO:
     chrOrderLineFormTitle = chrOrderLineFormTitle + ": " + ShipOrder.OrderRef + "&nbsp ID: " 
                             + STRING(ShipOrder.ShipOrderID).

     FIND FIRST Customer OF ShipOrder NO-LOCK NO-ERROR.     
     IF AVAILABLE Customer THEN
        chrOrderLineFormTitle = chrOrderLineFormTitle + "&nbsp Customer: " + Customer.CustomerName.
   END. /* IF AVAILABLE ShipOrder */

   ShipOrderLineBrowseForm = NEW dataForm("shiporderline_browse_form").
   ShipOrderLineBrowseForm:WebStream = STREAM WebStream:HANDLE.

   ShipOrderLineBrowseForm:FormAction  = "dbShipOrderUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ShipOrderLineBrowseForm:FormWidth   = 860.
   ShipOrderLineBrowseForm:FormHeight  = 530.
   ShipOrderLineBrowseForm:FormTitle   = chrOrderLineFormTitle.
   ShipOrderLineBrowseForm:FormType    = "xxl_large".
   
   ShipOrderLineBrowse = NEW browseTable("shiporderline_browse").
   ShipOrderLineBrowse:BrowseWidth  = 840.
   ShipOrderLineBrowse:BrowseHeight = 490.
   
   ShipOrderLineBrowse:insertColumn(fTL("Order ID"), 80, "INTEGER", FALSE).  
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ShipOrderLine}
   
   ShipOrderLineBrowse:insertColumn(fTL("Part #"),      130, "CHARACTER", "left",  FALSE).  
   ShipOrderLineBrowse:insertColumn(fTL("Description"), 140, "CHARACTER", "left",  FALSE).  
   ShipOrderLineBrowse:insertColumn(fTL("Qty Ordered"),  80, "INTEGER",   "right", FALSE).
   ShipOrderLineBrowse:insertColumn(fTL("Qty Packed"),   80, "INTEGER",   "right", FALSE).
   ShipOrderLineBrowse:insertColumn(fTL("Line Status"), 120, "CHARACTER",          FALSE).
   
   ShipOrderLineBrowse:StartBody().
   
   IF AVAILABLE ShipOrder THEN
   DO:
      /* Enable/Disable Complete Order Button */
      IF ShipOrder.ShipOrderStatusID = intBeingCreatedStatusID AND
         CAN-FIND(FIRST ShipOrderLine OF ShipOrder) THEN
         logIsBeingCreatedStatus = YES.
      ELSE
         logIsBeingCreatedStatus = NO.

      /* Enable/Disable Uncomplete Order Button */
      IF CAN-FIND(FIRST ShipOrderStatusGroupLink 
                  WHERE ShipOrderStatusGroupLink.ShipOrderStatusGroupID  = intCanEditGroupID
                  AND   ShipOrderStatusGroupLink.ShipOrderStatusID       = ShipOrder.ShipOrderStatusID
                  AND   ShipOrderStatusGroupLink.Active) THEN
         logEnableUncompleteOrder = YES.
      ELSE
         logEnableUncompleteOrder = NO.

      /* Enable/Disable Cancel Order Button */
      IF CAN-FIND(FIRST ShipOrderStatusGroupLink
                  WHERE ShipOrderStatusGroupLink.ShipOrderStatusGroupID = intCanCancelGroupID
                  AND   ShipOrderStatusGroupLink.ShipOrderStatusID      = ShipOrder.ShipOrderStatusID
                  AND   ShipOrderStatusGroupLink.Active) THEN
         logEnableCancelOrder = YES.
      ELSE
         logEnableCancelOrder = NO.

     /* Enable/Disable UnCancel Order Button */
     IF ShipOrder.ShipOrderStatusID = intCancelledStatusID THEN
        logEnableUncancelOrder = YES.
     ELSE
        logEnableUncancelOrder = NO.

      /* Enable/Disable Approve Order Button */
      IF ShipOrder.ShipOrderStatusID = intAwaitingApprovalStatusID THEN
         logEnableApproveOrder = YES.
      ELSE
         logEnableApproveOrder = NO.

      FOR EACH ShipOrderLine OF ShipOrder NO-LOCK:

         IF ShipOrderLine.ShipOrderStatus = intBeingCreatedStatusID THEN
            logEnableDeleteOrderLine = YES.
         ELSE
            logEnableDeleteOrderLine = NO.
         
         FIND Part            OF ShipOrderLine NO-LOCK NO-ERROR.      
         FIND ShipOrderStatus OF ShipOrderLine NO-LOCK NO-ERROR.
         
         ShipOrderLineBrowse:startRow(ShipOrderLine.ShipOrderLineID, "selectShipOrderLineRow(this," + '"' 
                                      + STRING(ShipOrderLine.ShipOrderLineID) + '"' + ");", "").         
         ShipOrderLineBrowse:insertData(ShipOrderLine.ShipOrderLineID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i ShipOrderLine}
         
         ShipOrderLineBrowse:insertData(IF AVAILABLE Part THEN Part.PartRef  ELSE "",   "left").
         ShipOrderLineBrowse:insertData(IF AVAILABLE Part THEN Part.PartDesc ELSE "",   "left").
         ShipOrderLineBrowse:insertData(ShipOrderLine.QtyOrdered, "right").   
         ShipOrderLineBrowse:insertData(ShipOrderLine.QtyPacked,  "right").
         ShipOrderLineBrowse:insertData((IF AVAILABLE ShipOrderStatus THEN StatusName ELSE "")).

         /* Add Hidden Fields */
         ShipOrderLineBrowse:insertHiddenData("ShipOrderLineVersionID", ShipOrderLine.VersionID).
         ShipOrderLineBrowse:insertHiddenData("PartRef",   IF AVAILABLE Part THEN Part.PartRef   ELSE "").
         ShipOrderLineBrowse:insertHiddenData("PartDescr", IF AVAILABLE Part THEN Part.PartDescr ELSE "").
         ShipOrderLineBrowse:insertHiddenData("enable_delete", logEnableDeleteOrderLine).
         
         ShipOrderLineBrowse:endRow().

      END. /* FOR EACH ShipOrderLine of ShipOrder */
   END. /*IF AVAILABLE ShipOrder THEN*/
   
   ShipOrderLineBrowse:endTable().
   
   /* Hidden Values - Browse Level*/
   ShipOrderLineBrowseForm:insertHiddenField("form_name","shiporderline_browse_form").
   ShipOrderLineBrowseForm:insertHiddenField("prog_name","adOrderEntry.p").  
   ShipOrderLineBrowseForm:insertHiddenField("form_action", "").
   ShipOrderLineBrowseForm:insertHiddenField("shiporderentry_browse_scroll","").
   ShipOrderLineBrowseForm:insertHiddenField("popup_shiporderline_browse","").
   ShipOrderLineBrowseForm:insertHiddenField("popup_shiporderlinehistory_browse", "").
   ShipOrderLineBrowseForm:inserthiddenField("enable_delete","").

   /* Hidden Values - Filters */
   ShipOrderLineBrowseForm:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   ShipOrderLineBrowseForm:insertHiddenField("FilteredShipDate", chrFilteredShipDate).
   ShipOrderLineBrowseForm:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   ShipOrderLineBrowseForm:insertHiddenField("FilteredStatusID", STRING(intFilteredStatusID)).
      
   /* Hidden Values - Keys*/
   ShipOrderLineBrowseForm:insertHiddenField("ShipOrderID", chrShipOrderID).
   ShipOrderLineBrowseForm:insertHiddenField("ShipOrderRef", "").
   ShipOrderLineBrowseForm:insertHiddenField("ShipOrderLineID", "").
   ShipOrderLineBrowseForm:insertHiddenField("ShipOrderLineVersionID","").
   ShipOrderLineBrowseForm:insertHiddenField("PartRef","").
   ShipOrderLineBrowseForm:insertHiddenField("PartDescr","").
   ShipOrderLineBrowseForm:insertHiddenField("ExternalUser", STRING(logGblExternalSession)).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderLineBrowseForm}
   
   /* Create Button Bar */
   ShipOrderLineBrowseButtons = NEW buttonBar().
   
   ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_details",
                                        fTL("Create / Edit"),
                                        "viewShipOrderLineDetails('shiporderline_details_form'," 
                                        + "'" + STRING(intBeingCreatedStatusID) + "');").
   
   ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_delete",
                                        fTL("Delete OrderLine"),
                                        "confirmDeleteShipOrderLine();",
                                        "Disabled").

   ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_history",
                                        fTL("History"),
                                        "viewShipOrderLineHistory();",
                                        "Disabled").
   
   /* Make this conditional based on external or internal broker and ShipOrderStatus */
   IF logISBeingCreatedStatus /*AND logGblExternalSession */ THEN
      ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_complete",
                                           fTL("Complete Order"),
                                           "confirmCompleteOrder('shiporderline_browse_form');").

   /* Make this conditional based on external or internal broker and ShipOrderStatus */
   IF logEnableUncompleteOrder /* AND logGblExternalSession */ THEN
      ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_uncomplete",
                                           fTL("UnComplete Order"),
                                           "confirmUnCompleteOrder('shiporderline_browse_form');").

   /* Make this conditional based on external or internal broker and ShipOrderStatus */
   IF logEnableApproveOrder AND NOT logGblExternalSession THEN
      ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_approve",
                                           fTL("Approve Order"),
                                           "confirmApproveOrder('shiporderline_browse_form');").

   /* Make this conditional based on external or internal broker and ShipOrderStatus */
   IF logEnableCancelOrder AND logGblExternalSession THEN
      ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_cancel",
                                           fTL("Cancel Order"),
                                           "confirmCancelOrder('shiporderline_browse_form');").

   /* Make this conditional based on external or internal broker and ShipOrderStatus */
   IF logEnableUnCancelOrder AND LogGblExternalSession THEN
      ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_cancel",
                                           fTL("UnCancelOrder"),
                                           "confirmUnCancelOrder('shiporderline_browse_form');").

   ShipOrderLineBrowseButtons:addButton("shiporderline_browse_form_btn_close",
                                        fTL("Close"),
                                        "disablePopup('shiporderline_browse_form_popup');").
   
   ShipOrderLineBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ShipOrderLineBrowseForm:FormBrowse  = ShipOrderLineBrowse.
   ShipOrderLineBrowseForm:FormButtons = ShipOrderLineBrowseButtons.
   
   ShipOrderLineBrowseForm:endForm(). 
   
   ShipOrderLineBrowseForm:displayForm().   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderLineDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderLineDetails Procedure 
PROCEDURE pShipOrderLineDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "shiporderline_details_form"}

   ASSIGN chrDisplayFieldList  = "ShipOrderLineID,ShipOrderID,ShipOrderStatusID,PartRef,PartDescr,QtyOrdered"
          chrEditFieldList     = "QtyOrdered"
          chrNewFieldList      = "PartRef,QtyOrdered"
          chrRequiredFieldList = "PartRef,QtyOrdered"
          chrExtraFieldList    = ""
          chrValidateFieldList = "QtyOrdered:INTEGER>0".

   ShipOrderLineDetailsForm = NEW dataForm("shiporderline_details_form").
   ShipOrderLineDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   ShipOrderLineDetailsForm:FormAction  = "dbShipOrderLineUpdate.p?" + TRIM(chrGblDefaultUrlValues, "&").
   
   /* Setup */
   ShipOrderLineDetailsForm:FormWidth   = 460.
   ShipOrderLineDetailsForm:FormHeight  = 300.
   ShipOrderLineDetailsForm:FormTitle   = "Ship Order Line Details".
   ShipOrderLineDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   ShipOrderLineDetailsForm:insertPaddingColumn(20).
   ShipOrderLineDetailsForm:insertColumn(90).
   ShipOrderLineDetailsForm:insertColumn(110).
   ShipOrderLineDetailsForm:insertColumn(30).
   ShipOrderLineDetailsForm:insertColumn(120).  
   
   /* Fields */
   ShipOrderLineDetailsForm:startRow().
   ShipOrderLineDetailsForm:insertLabel(fTL("OrderLine ID")).
   ShipOrderLineDetailsForm:insertTextField("ShipOrderLineID", "", 110, TRUE).    

   ShipOrderLineDetailsForm:startRow().
   ShipOrderLineDetailsForm:insertLabel(fTL("Line Status")).
   ShipOrderLineDetailsForm:insertComboField("ShipOrderStatusID", "", 110, TRUE).
   FOR EACH ShipOrderStatus NO-LOCK
      WHERE ShipOrderStatus.Active
      BY    ShipOrderStatus.ListingSequence:

      ShipOrderLineDetailsForm:insertComboPairs("ShipOrderStatusID", STRING(ShipOrderStatusID),
                                                ShipOrderStatus.StatusName).
   END.

   ShipOrderLineDetailsForm:startRow().
   ShipOrderLineDetailsForm:insertLabel(fTL("Order ID")).
   ShipOrderLineDetailsForm:insertTextField("ShipOrderID", "", 110, TRUE).
   
   ShipOrderLineDetailsForm:startRow().
   ShipOrderLineDetailsForm:insertLabel(fTL("Part Ref")).
   ShipOrderLineDetailsForm:insertTextField("PartRef", "", 200, TRUE).     
   ShipOrderLineDetailsForm:insertButton("PartSearch", 
                                         "Search Parts", 
                                         "partLookup('shiporderline_details_form','PartRef'," + "'" + 
                                         STRING(intGblSessionID) + "');").  
   
   ShipOrderLineDetailsForm:startRow().   
   ShipOrderLineDetailsForm:insertLabel(fTL("Part Descr")).
   ShipOrderLineDetailsForm:insertTextField("PartDescr", "", 300, TRUE). 
   
   ShipOrderLineDetailsForm:startRow().   
   ShipOrderLineDetailsForm:insertLabel(fTL("Qty Ordered")).
   ShipOrderLineDetailsForm:insertTextField("QtyOrdered", "", 110, TRUE).  
              
   /* Add Hidden Fields*/
   ShipOrderLineDetailsForm:insertHiddenField("shiporderline_browse_scroll","").
   ShipOrderLineDetailsForm:insertHiddenField("popup_shiporderline_browse","").
   ShipOrderLineDetailsForm:insertHiddenField("form_name","shiporderline_details_form").
   ShipOrderLineDetailsForm:insertHiddenField("prog_name","adOrderEntry.p").
   ShipOrderLineDetailsForm:insertHiddenField("PartID", "").

   /* Hidden Values - Filters */
   ShipOrderLineDetailsForm:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   ShipOrderLineDetailsForm:insertHiddenField("FilteredShipDate", chrFilteredShipDate).
   ShipOrderLineDetailsForm:insertHiddenField("FilteredCustomerID", STRING(intFilteredCustomerID)).
   ShipOrderLineDetailsForm:insertHiddenField("FilteredStatusID", STRING(intFilteredStatusID)).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderLineDetailsForm}
   
   /* Create Button Bar */
   ShipOrderLineDetailsButtons = NEW buttonBar().

   ShipOrderLineDetailsButtons:addButton("shiporderline_details_form_btn_create",
                                         fTL("Create"),
                                         "createShipOrderLine('shiporderline_details_form',"
                                         + "'" + STRING(intBeingCreatedStatusID) + "');").

   ShipOrderLineDetailsButtons:addButton("shiporderline_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateShipOrderLine('shiporderline_details_form');").
   
   ShipOrderLineDetailsButtons:addButton("shiporderline_details_form_btn_cancel", 
                                         fTL("Cancel"), 
                                         "cancelUpdate('UserCancelled','process_mode'); disablePopup('shiporderline_details_form_popup');").
   
   ShipOrderLineDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ShipOrderLineDetailsForm:FormButtons = ShipOrderLineDetailsButtons.
   
   ShipOrderLineDetailsForm:endForm(). 
   ShipOrderLineDetailsForm:displayForm(). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pShipOrderLineHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShipOrderLineHistory Procedure 
PROCEDURE pShipOrderLineHistory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   chrOrderLineHistoryFormTitle = fTL("ShipOrderLine History").
   IF AVAILABLE ShipOrderLine THEN
   DO:
      FIND FIRST Part NO-LOCK
         WHERE   Part.PartID = ShipOrderLine.PartID NO-ERROR.

      IF AVAILABLE Part THEN
         chrOrderLineHistoryFormTitle = chrOrderLineHistoryFormTitle + ": " + Part.PartRef 
                                        + "&nbspID:" + STRING(ShipOrderLine.ShipOrderLineID).
      ELSE
         chrOrderLineHistoryFormTitle = chrOrderLineHistoryFormTitle + ":" + STRING(ShipOrderLine.ShipOrderLineID).
   END.
   
   ShipOrderLineHistoryBrowseForm = NEW dataForm("shiporderlinehistory_browse_form").
   ShipOrderLineHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   ShipOrderLineHistoryBrowseForm:FormWidth  = 860.
   ShipOrderLineHistoryBrowseForm:FormHeight = 530.
   ShipOrderLineHistoryBrowseForm:FormTitle  = chrOrderLineHistoryFormTitle.
   ShipOrderLineHistoryBrowseForm:FormType   = "xxl_large".
   
   ShipOrderLineHistoryBrowse = NEW browseTable("shiporderlinehistory_browse").
   ShipOrderLineHistoryBrowse:BrowseWidth  = 840.
   ShipOrderLineHistoryBrowse:BrowseHeight = 490.
   
   ShipOrderLineHistoryBrowse:insertColumn(fTL("ID"), 70, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ShipOrderLineHistory}
   
   ShipOrderLineHistoryBrowse:insertColumn(fTL("Gate User"),  120, "CHARACTER", "left").
   ShipOrderLineHistoryBrowse:insertColumn(fTL("Created"),    100, "CHARACTER", "left").
   ShipOrderLineHistoryBrowse:insertColumn(fTL("Part Ref"),   150, "CHARACTER", "left").
   ShipOrderLineHistoryBrowse:insertColumn(fTL("Qty Ordered"), 90, "INTEGER",   "right").
   ShipOrderLineHistoryBrowse:insertColumn(fTL("Qty Packed"),  90, "INTEGER",   "right").
   ShipOrderLineHistoryBrowse:insertColumn(fTL("Status"),     100, "CHARACTER", "left").
   
   ShipOrderLineHistoryBrowse:StartBody().
   
   IF AVAILABLE ShipOrderLine THEN
   DO:
      /* List the ShipOrderLineHistorys for the ShipOrderLine */
      FOR EACH ShipOrderLineHistory NO-LOCK /* idx=ShipOrderLineID */
         WHERE ShipOrderLineHistory.ShipOrderLineID = ShipOrderLine.ShipOrderLineID 
         BY    ShipOrderLineHistory.ShipOrderLineID
         BY    ShipOrderLineHistory.Created DESCENDING:

         FIND FIRST GateUser        OF ShipOrderLineHistory NO-LOCK NO-ERROR.
         FIND FIRST Part            OF ShipOrderLineHistory NO-LOCK NO-ERROR.
         FIND FIRST ShipOrderStatus NO-LOCK
            WHERE   ShipOrderStatus.ShipOrderStatusID = ShipOrderLineHistory.ShipOrderStatusID NO-ERROR.
                 
         ShipOrderLineHistoryBrowse:startRow(ShipOrderLineHistory.ShipOrderLineHistoryID, "selectShipOrderLineHistoryRow(this," 
                                             + '"' + STRING(ShipOrderLineHistory.ShipOrderLineHistoryID)
                                             + '","adOrderEntry.p","shiporderline_browse_form"' + ");", "").

         ShipOrderLineHistoryBrowse:insertData(ShipOrderLineHistory.ShipOrderLineHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i ShipOrderLineHistory}
         
         ShipOrderLineHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         ShipOrderLineHistoryBrowse:insertData(fDisplayDate&Time(ShipOrderLineHistory.Created, "d/m/y H:M")).
         ShipOrderLineHistoryBrowse:insertData(IF AVAILABLE Part THEN Part.PartRef ELSE "",          "left").
         ShipOrderLineHistoryBrowse:insertData(ShipOrderLineHistory.QtyOrdered, "right").
         ShipOrderLineHistoryBrowse:insertData(ShipOrderLineHistory.QtyPacked,  "right").
         ShipOrderLineHistoryBrowse:insertData(IF AVAILABLE ShipOrderStatus 
                                               THEN ShipOrderStatus.StatusName ELSE "", "left").         

         ShipOrderLineHistoryBrowse:endRow().
      
      END. /* FOR EACH ShipOrderLineHistory OF ShipOrderLine NO-LOCK */
   END. /*IF AVAILABLE ShipOrderLine THEN*/
   
   ShipOrderLineHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + ShipOrderLineHistoryBrowse:getErrors().

   ShipOrderLineHistoryBrowseForm:insertHiddenField("popup_shiporderlinehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ShipOrderLineHistoryBrowseForm}
   
   /* Create Button Bar */
   ShipOrderLineHistoryBrowseButtons = NEW buttonBar().
          
   ShipOrderLineHistoryBrowseButtons:addButton("shiporderlinehistory_browse_form_btn_cancel",
                                               fTL("Cancel"),
                                               "disablePopup('shiporderlinehistory_browse_form_popup');").
   
   ShipOrderLineHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ShipOrderLineHistoryBrowseForm:FormBrowse  = ShipOrderLineHistoryBrowse.
   ShipOrderLineHistoryBrowseForm:FormButtons = ShipOrderLineHistoryBrowseButtons.
   ShipOrderLineHistoryBrowseForm:endForm(). 
   
   ShipOrderLineHistoryBrowseForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pUploadLocationBrowse:) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUploadLocationBrowse: Procedure
PROCEDURE pUploadShipOrderEntryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   FIND FIRST Customer
       WHERE  Customer.CustomerID = INTEGER(chrCustomerName).

   UploadShipOrderEntryBrowseForm            = NEW dataForm("shiporderentryupload_browse_form").
   UploadShipOrderEntryBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   UploadShipOrderEntryBrowseForm:FormAction = "dbOrderEntryUploadUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   UploadShipOrderEntryBrowseForm:FormWidth  = 700.
   UploadShipOrderEntryBrowseForm:FormHeight = 490.
   UploadShipOrderEntryBrowseForm:FormTitle  = fTL("Confirm Ship Order Entry Upload for " + Customer.CustomerName).
   UploadShipOrderEntryBrowseForm:FormType   = "xl_large".
   
   UploadShipOrderEntryBrowse                = NEW browseTable("shiporderentryupload_browse").
   UploadShipOrderEntryBrowse:BrowseWidth    = 680.
   UploadShipOrderEntryBrowse:BrowseHeight   = 450.

   /* design the columns of the browse */
   ASSIGN intDisplayedColumnsNo  = 0
          intCurrentColumnsWidth = 0.
   
   UploadShipOrderEntryBrowse:insertColumn(fTL("Line #"),   100, "INTEGER",   "left",  FALSE).
   UploadShipOrderEntryBrowse:insertColumn(fTL("Part Ref"), 200, "CHARACTER", "left",  FALSE).
   UploadShipOrderEntryBrowse:insertColumn(fTL("Quantity"), 150, "INTEGER",   "left",  FALSE).
   
   UploadShipOrderEntryBrowse:startBody().
   
   DEFINE VARIABLE intTempPartRef  AS INTEGER NO-UNDO.
   DEFINE VARIABLE intTempQuantity AS INTEGER NO-UNDO.
   
   FIND FIRST FileMasterField
       WHERE  FileMasterField.FieldName = "PartRef".
   
   intTempPartRef = FileMasterField.PositionInFile.
   
   FIND FIRST FileMasterField
       WHERE  FileMasterField.FieldName = "PackageQty".
   
   intTempQuantity = FileMasterField.PositionInFile.
      
   intFileLineSequence = 0.
   FOR EACH ttFileLine NO-LOCK:
       
       FIND FIRST ttFileLineMerge
           WHERE ENTRY(intTempPartRef,ttFileLine.LineString, ",") = ttFileLineMerge.PartRef NO-ERROR.
            
       IF AVAILABLE ttFileLineMerge THEN  
           ttFileLineMerge.Quantity = ttFileLineMerge.Quantity + INTEGER(ENTRY(intTempQuantity,ttFileLine.LineString, ",")). 
       ELSE
       DO: 
           CREATE ttFileLineMerge.
           ASSIGN intFileLineSequence        = intFileLineSequence + 1
                  ttFileLineMerge.PartRef    = ENTRY(intTempPartRef,ttFileLine.LineString, ",")
                  ttFileLineMerge.Quantity   = INTEGER(ENTRY(intTempQuantity,ttFileLine.LineString, ","))
                  ttFileLineMerge.FileLineNo = intFileLineSequence.
       END.           
   END.
   
   /*Body*/
   FOR EACH ttFileLineMerge NO-LOCK:               
      UploadShipOrderEntryBrowse:startRow(ttFileLineMerge.FileLineNo,   "", "").
      UploadShipOrderEntryBrowse:insertData(ttFileLineMerge.FileLineNo, "left").
      UploadShipOrderEntryBrowse:insertData(ttFileLineMerge.PartRef,    "left").
      UploadShipOrderEntryBrowse:insertData(ttFileLineMerge.Quantity,   "left").

      /* Add hidden fields */
      UploadShipOrderEntryBrowse:insertHiddenData("FileLineNo",ttFileLineMerge.FileLineNo).
      
      UploadShipOrderEntryBrowse:endRow().
      
   END. /* FOR EACH ttFileLine NO-LOCK */
   
   UploadShipOrderEntryBrowse:endTable().
   
   UploadShipOrderEntryBrowseForm:insertHiddenField("shiporderentryupload_browse_scroll","").
   UploadShipOrderEntryBrowseForm:insertHiddenField("UploadShipOrderEntryID","").
   UploadShipOrderEntryBrowseForm:insertHiddenField("UploadShipOrderEntryVersionID","").
   UploadShipOrderEntryBrowseForm:insertHiddenField("UploadedFileName", chrFileName).
   UploadShipOrderEntryBrowseForm:insertHiddenField("popup_shiporderentryupload_browse_form","").
   UploadShipOrderEntryBrowseForm:insertHiddenField("popup_shiporderentry_selection_form","").
   UploadShipOrderEntryBrowseForm:insertHiddenField("form_name", "shiporderentryupload_browse_form").
   UploadShipOrderEntryBrowseForm:insertHiddenField("prog_name", "adOrderEntry.p").
   UploadShipOrderEntryBrowseForm:insertHiddenField("CustomerName", chrCustomerName).
   UploadShipOrderEntryBrowseForm:insertHiddenField("ShipOrderTypeName", chrShipOrderType).
   UploadShipOrderEntryBrowseForm:insertHiddenField("ShipOrderStream", chrShipOrderStream).
   UploadShipOrderEntryBrowseForm:insertHiddenField("OrderRef", chrOrderRefUpload).
   UploadShipOrderEntryBrowseForm:insertHiddenField("Carrier", chrCarrier).
   UploadShipOrderEntryBrowseForm:insertHiddenField("CarrierSortation", chrCarrierSortation).
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i UploadShipOrderEntryBrowseForm}
   
   /* Create Button Bar */
   UploadShipOrderEntryBrowseButtons = NEW buttonBar().
   
   UploadShipOrderEntryBrowseButtons:addButton("shiporderentryupload_browse_form_btn_save",
                                               fTL("Confirm"),
                                               "saveUploadShipOrderEntry('shiporderentryupload_browse_form');",
                                               (IF chrValidFile = "YES" THEN "" ELSE "DISABLED")).
   
   UploadShipOrderEntryBrowseButtons:addButton("shiporderentryupload_browse_form_btn_cancel",
                                               fTL("Cancel"),
                                               "cancelUpdate('UserCancelled','process_mode'); 
                                               disablePopup('shiporderentryupload_browse_form_popup');").
   
   UploadShipOrderEntryBrowseButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   UploadShipOrderEntryBrowseForm:FormBrowse  = UploadShipOrderEntryBrowse.
   UploadShipOrderEntryBrowseForm:FormButtons = UploadShipOrderEntryBrowseButtons.
   UploadShipOrderEntryBrowseForm:endForm().
   UploadShipOrderEntryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

