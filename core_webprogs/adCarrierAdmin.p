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
{defWebDefinitions.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}

/* Definitions for System Options for Receiving */
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedCarrier                            AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedCarrierSortation                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedCarrierService                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedCarrierMode                        AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedCarrierDeliveryStatus              AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedSortationShipLaneLink              AS INTEGER     NO-UNDO.
DEFINE VARIABLE intProcessMap                                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrProcessMapList                             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectCarrierRow                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectCarrierSortationRow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectCarrierServiceRow                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectCarrierModeRow                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectCarrierDeliveryStatusRow             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectSortationShipLaneRow                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToCarrierRow                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToCarrierSortationRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToCarrierServiceRow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToCarrierModeRow                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToCarrierDeliveryStatusRow           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCarrierID                                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCarrierSortationID                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCarrierServiceID                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCarrierModeID                              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrCarrierDeliveryStatusID                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSortationShipLaneLinkID                    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCarrierSortations                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCarrierServices                       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCarrierMode                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCarrierDeliveryStatus                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupSortationShipLaneLinks                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCarrierHistory                        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCarrierSortationHistory               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupCarrierServiceHistory                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logShipLanesAvailable                         AS LOGICAL     NO-UNDO.

/* Objects */
DEFINE VARIABLE CarrierBrowseFrame                            AS pageFrame.
DEFINE VARIABLE CarrierBrowse                                 AS browseTable.
DEFINE VARIABLE CarrierBrowseButtons                          AS buttonBar.
DEFINE VARIABLE CarrierBrowseMoreButtons                      AS buttonBar.
DEFINE VARIABLE CarrierDetailsForm                            AS dataForm.
DEFINE VARIABLE CarrierDetailsButtons                         AS buttonBar.
DEFINE VARIABLE CarrierSortationBrowseFrame                   AS pageFrame.
DEFINE VARIABLE CarrierSortationBrowse                        AS browseTable.
DEFINE VARIABLE CarrierSortationBrowseButtons                 AS buttonBar.
DEFINE VARIABLE CarrierSortationDetailsForm                   AS dataForm.
DEFINE VARIABLE CarrierSortationDetailsButtons                AS buttonBar.
DEFINE VARIABLE CarrierSortationBrowseForm                    AS dataForm.
DEFINE VARIABLE CarrierServiceBrowseFrame                     AS pageFrame.
DEFINE VARIABLE CarrierServiceBrowse                          AS browseTable.
DEFINE VARIABLE CarrierModeBrowse                             AS browseTable.
DEFINE VARIABLE CarrierDeliveryStatusBrowse                   AS browseTable.
DEFINE VARIABLE CarrierServiceBrowseButtons                   AS buttonBar.
DEFINE VARIABLE CarrierModeBrowseButtons                      AS buttonBar.
DEFINE VARIABLE CarrierDeliveryStatusBrowseButtons            AS buttonBar.
DEFINE VARIABLE CarrierServiceDetailsForm                     AS dataForm.
DEFINE VARIABLE CarrierServiceDetailsButtons                  AS buttonBar.
DEFINE VARIABLE CarrierModeDetailsForm                        AS dataForm.
DEFINE VARIABLE CarrierDeliveryStatusDetailsForm              AS dataForm.
DEFINE VARIABLE CarrierModeDetailsButtons                     AS buttonBar.
DEFINE VARIABLE CarrierDeliveryStatusDetailsButtons           AS buttonBar.
DEFINE VARIABLE CarrierServiceBrowseForm                      AS dataForm.
DEFINE VARIABLE CarrierModeBrowseForm                         AS dataForm.
DEFINE VARIABLE CarrierDeliveryStatusBrowseForm               AS dataForm.
DEFINE VARIABLE CarrierHistoryBrowseFrame                     AS pageFrame.
DEFINE VARIABLE CarrierHistoryBrowseForm                      AS dataForm.
DEFINE VARIABLE CarrierHistoryBrowse                          AS browseTable.
DEFINE VARIABLE CarrierHistoryBrowseButtons                   AS buttonBar.
DEFINE VARIABLE CarrierHistoryDetailsForm                     AS dataForm.
DEFINE VARIABLE CarrierHistoryDetailsButtons                  AS buttonBar.
DEFINE VARIABLE CarrierSortationHistoryBrowseFrame            AS pageFrame.
DEFINE VARIABLE CarrierSortationHistoryBrowseForm             AS dataForm.
DEFINE VARIABLE CarrierSortationHistoryBrowse                 AS browseTable.
DEFINE VARIABLE CarrierSortationHistoryBrowseButtons          AS buttonBar.
DEFINE VARIABLE CarrierSortationHistoryDetailsForm            AS dataForm.
DEFINE VARIABLE CarrierSortationHistoryDetailsButtons         AS buttonBar.
DEFINE VARIABLE CarrierServiceHistoryBrowseFrame              AS pageFrame.
DEFINE VARIABLE CarrierServiceHistoryBrowseForm               AS dataForm.
DEFINE VARIABLE CarrierServiceHistoryBrowse                   AS browseTable.
DEFINE VARIABLE CarrierServiceHistoryBrowseButtons            AS buttonBar.
DEFINE VARIABLE CarrierServiceHistoryDetailsForm              AS dataForm.
DEFINE VARIABLE CarrierServiceHistoryDetailsButtons           AS buttonBar.
DEFINE VARIABLE SortationShipLaneLinkBrowseFrame              AS pageFrame.
DEFINE VARIABLE SortationShipLaneLinkBrowse                   AS browseTable.
DEFINE VARIABLE SortationShipLaneLinkBrowseButtons            AS buttonBar.
DEFINE VARIABLE SortationShipLaneLinkDetailsForm              AS dataForm.
DEFINE VARIABLE SortationShipLaneLinkDetailsButtons           AS buttonBar.
DEFINE VARIABLE SortationShipLaneLinkBrowseForm               AS dataForm.


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

&IF DEFINED(EXCLUDE-pCarrierBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierBrowse Procedure 
PROCEDURE pCarrierBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "carrier_details_form"}
   
   CarrierBrowse              = NEW browseTable("carrier_browse").
   CarrierBrowse:BrowseWidth  = 965.
   CarrierBrowse:BrowseHeight = 420. /* 455 */
   CarrierBrowse:ExcelExport  = TRUE.
   CarrierBrowse:SessionID    = intGblSessionID.
   CarrierBrowse:WebStream    = STREAM WebStream:HANDLE.

   /* Add in the ID as first Column */
   CarrierBrowse:insertColumn(fTL("Carrier ID"), 85, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Carrier}
   
   CarrierBrowse:insertColumn(fTL("Carrier Code"), 120, "CHARACTER", "left", FALSE).
   CarrierBrowse:insertColumn(fTL("Carrier Name"), 280, "CHARACTER", "left", FALSE).
   CarrierBrowse:insertColumn(fTL("SCAC Code"),    180, "CHARACTER", "left", FALSE).
   CarrierBrowse:insertColumn(fTL("Active"),        70, "LOGICAL",           FALSE).
   
   /*Body*/
   CarrierBrowse:startBody().
   
   FOR EACH Carrier NO-LOCK /*ActiveCarrierName*/
      WHERE Carrier.Active
      BY    Carrier.CarrierName:
      
      CarrierBrowse:startRow(Carrier.CarrierID, "selectCarrierRow(this," + '"' + STRING(Carrier.CarrierID) + '"' + ");", "").
      CarrierBrowse:insertData(Carrier.CarrierID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Carrier}
      
      CarrierBrowse:insertData((IF AVAILABLE Carrier THEN Carrier.CarrierCode ELSE ""), "left").
      CarrierBrowse:insertData((IF AVAILABLE Carrier THEN Carrier.CarrierName ELSE ""), "left").
      CarrierBrowse:insertData((IF AVAILABLE Carrier THEN Carrier.ScacCode    ELSE ""), "left").
      CarrierBrowse:insertData((IF AVAILABLE Carrier THEN STRING(Carrier.Active,"Yes/No") ELSE "")).
      
      /* Add hidden fields */
      CarrierBrowse:insertHiddenData("CarrierVersionID",Carrier.VersionID).
      
      CarrierBrowse:endRow().
      
   END. /*FOR EACH Carrier NO-LOCK */
   
   CarrierBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierBrowse:getErrors().
   
   /* Create a new frame */
   CarrierBrowseFrame = NEW pageFrame().
   CarrierBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CarrierBrowseFrame:FormAction="dbCarrierUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CarrierBrowseFrame:formOpen("carrier_browse_form").
   
   /* Start the Frame Header */
   CarrierBrowseFrame:insertSpacer(5).
   CarrierBrowseFrame:frameOpen(985, 470, "").  /* 500 */
   
   /* This outputs the Browse Table */  
   CarrierBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CarrierBrowseFrame:frameClose().
   CarrierBrowseFrame:insertSpacer(10).
   
   CarrierBrowseFrame:insertHiddenField("carrier_browse_scroll","").
   CarrierBrowseFrame:insertHiddenField("CarrierID","").
   CarrierBrowseFrame:insertHiddenField("CarrierVersionID","").
   CarrierBrowseFrame:insertHiddenField("carriersortation_browse_scroll","").
   CarrierBrowseFrame:insertHiddenField("popup_carriersortation_browse","").
   CarrierBrowseFrame:insertHiddenField("popup_carrierservice_browse","").
   CarrierBrowseFrame:insertHiddenField("popup_carriermode_browse","").
   CarrierBrowseFrame:insertHiddenField("popup_carrierdeliverystatus_browse","").
   CarrierBrowseFrame:insertHiddenField("popup_carrierhistory_browse","").
   CarrierBrowseFrame:insertHiddenField("popup_carriersortationhistory_browse","").
   CarrierBrowseFrame:insertHiddenField("popup_carrierservicehistory_browse","").
   CarrierBrowseFrame:insertHiddenField("CarrierSortationID","").
   CarrierBrowseFrame:insertHiddenField("CarrierServiceID","").
   CarrierBrowseFrame:insertHiddenField("CarrierModeID","").
   CarrierBrowseFrame:insertHiddenField("CarrierDeliveryStatusID","").
   CarrierBrowseFrame:insertHiddenField("SortationShipLaneLinkID","").
   CarrierBrowseFrame:insertHiddenField("form_name","carrier_browse_form").
   CarrierBrowseFrame:insertHiddenField("prog_name","adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierBrowseFrame}
   
   CarrierBrowseFrame:formClose().
   
   /* Create Button Bar */
   CarrierBrowseButtons = NEW buttonBar().
   CarrierBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   CarrierBrowseButtons:addButton("carrier_browse_form_btn_details",
                                  fTL("Details"),
                                  "viewCarrierDetails('carrier_details_form');",
                                  (IF intSelectedCarrier > 0 THEN "" ELSE "Disabled")).
   
   CarrierBrowseButtons:addButton("carrier_browse_form_btn_sortations",
                                  fTL("Sortations"),
                                  "viewCarrierSortations('carrier_details_form');",
                                  (IF intSelectedCarrier > 0 THEN "" ELSE "Disabled")).

   CarrierBrowseButtons:addButton("carrier_browse_form_btn_services",
                                  fTL("Services"),
                                  "viewCarrierServices('carrier_details_form');",
                                  (IF intSelectedCarrier > 0 THEN "" ELSE "Disabled")).

   CarrierBrowseButtons:addButton("carrier_browse_form_btn_create",
                                  fTL("Create"),
                                  "createCarrier('carrier_details_form');",
                                  "").
   
   CarrierBrowseButtons:addButton("carrier_browse_form_btn_delete",
                                  fTL("Delete"),
                                  "confirmDeleteCarrier();",
                                  (IF intSelectedCarrier > 0 THEN "" ELSE "Disabled")).
   
   CarrierBrowseButtons:addButton("carrier_browse_form_btn_history",
                                  fTL("History"),
                                  "viewCarrierHistoryBrowse('carrier_details_form');",
                                  (IF intSelectedCarrier > 0 THEN "" ELSE "Disabled")).
   
   CarrierBrowseButtons:addButton("carrier_browse_form_btn_excel",
                                  fTL("Excel Export"),
                                  "excelExport('" + STRING(intGblSessionID) + "_carrier_browse.xml')").
                                  
   CarrierBrowseButtons:closeBar().  
   CarrierBrowseButtons:displayButtonBar().  
   
   /* Create the second Button Bar */
   
   CarrierBrowseFrame:insertSpacer(10).
   CarrierBrowseMoreButtons = NEW buttonBar().
   CarrierBrowseMoreButtons:WebStream = STREAM WebStream:HANDLE.

   CarrierBrowseMoreButtons:addButton("carrier_browse_form_btn_modes",
                                      fTL("Modes"),
                                      "viewCarrierMode('carrier_details_form');",
                                      (IF intSelectedCarrier > 0 THEN "" ELSE "Disabled")).
   
   CarrierBrowseMoreButtons:addButton("carrier_browse_form_btn_deliverystatus",
                                      fTL("Delivery Status"),
                                      "viewCarrierDeliveryStatus('carrier_details_form');",
                                      (IF intSelectedCarrier > 0 THEN "" ELSE "Disabled")).                                    

   CarrierBrowseMoreButtons:closeBar().
   CarrierBrowseMoreButtons:displayButtonBar().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCarrierDeliveryStatusBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierDeliveryStatusBrowse Procedure
PROCEDURE pCarrierDeliveryStatusBrowse:
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "carrierdeliverystatus_details_form"}
   
   CarrierDeliveryStatusBrowseForm = NEW dataForm("carrierdeliverystatus_browse_form").
   CarrierDeliveryStatusBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierDeliveryStatusBrowseForm:FormAction  = "dbCarrierDeliveryStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierDeliveryStatusBrowseForm:FormWidth   = 700.
   CarrierDeliveryStatusBrowseForm:FormHeight  = 490.
   CarrierDeliveryStatusBrowseForm:FormTitle   = fTL("Delivery Status for Carrier") + (IF AVAILABLE Carrier THEN " : " + Carrier.CarrierName ELSE "").
   CarrierDeliveryStatusBrowseForm:FormType    = "xl_large".
   
   CarrierDeliveryStatusBrowse = NEW browseTable("carrierdeliverystatus_browse").
   CarrierDeliveryStatusBrowse:BrowseWidth  = 680.
   CarrierDeliveryStatusBrowse:BrowseHeight = 432.
   
   CarrierDeliveryStatusBrowse:insertColumn(fTL("ID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CarrierDeliveryStatus}
   
   CarrierDeliveryStatusBrowse:insertColumn(fTL("DeliveryCode"), 120, "CHARACTER", "left", FALSE).
   CarrierDeliveryStatusBrowse:insertColumn(fTL("CustomerDeliveryCode"), 140, "CHARACTER", "left", FALSE).
   CarrierDeliveryStatusBrowse:insertColumn(fTL("FailedDelivery"),     80, "CHARACTER",         FALSE).
   CarrierDeliveryStatusBrowse:StartBody().
   
   IF AVAILABLE Carrier THEN
   DO:
      /*List the CarrierDeliveryStatuss for the Carrier*/
      FOR EACH CarrierDeliveryStatus OF Carrier NO-LOCK:
         
         CarrierDeliveryStatusBrowse:startRow(CarrierDeliveryStatus.CarrierDeliveryStatusID, "selectCarrierDeliveryStatusRow(this," + '"' 
            + STRING(CarrierDeliveryStatus.CarrierDeliveryStatusID) + '","adCarrierAdmin.p","carrierdeliverystatus_browse_form"' + ");", "").
            
         CarrierDeliveryStatusBrowse:insertData(CarrierDeliveryStatus.CarrierDeliveryStatusID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CarrierDeliveryStatus}
         
         CarrierDeliveryStatusBrowse:insertData(CarrierDeliveryStatus.DeliveryCode, "left").
         CarrierDeliveryStatusBrowse:insertData(CarrierDeliveryStatus.CustomerDeliveryCode, "left").
         CarrierDeliveryStatusBrowse:insertData(STRING(CarrierDeliveryStatus.FailedDelivery,"Yes/No")).
               
         /* Add hidden fields */
         CarrierDeliveryStatusBrowse:insertHiddenData("CarrierID",CarrierDeliveryStatus.CarrierID).
         CarrierDeliveryStatusBrowse:insertHiddenData("CarrierDeliveryStatusID",CarrierDeliveryStatus.CarrierDeliveryStatusID).
         CarrierDeliveryStatusBrowse:insertHiddenData("CarrierDeliveryStatusVersionID",CarrierDeliveryStatus.VersionID). 
         CarrierDeliveryStatusBrowse:insertHiddenData("CarrierDeliveryStatusDeliveryCode",CarrierDeliveryStatus.DeliveryCode).
         CarrierDeliveryStatusBrowse:endRow().
      
      END. /* FOR EACH CarrierDeliveryStatus*/
   END. /*IF AVAILABLE Carrier THEN*/
   
   CarrierDeliveryStatusBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierDeliveryStatusBrowse:getErrors().
   
   CarrierDeliveryStatusBrowseForm:insertHiddenField("CarrierID","").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("CarrierName","").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("CarrierDeliveryStatusDeliveryCode","").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("CarrierDeliveryStatusID","").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("CarrierDeliveryStatusVersionID",""). 
   CarrierDeliveryStatusBrowseForm:insertHiddenField("carrier_browse_scroll","").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("carrierdeliverystatus_browse_scroll","").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("popup_carrierdeliverystatus_browse","").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("form_name","carrierdeliverystatus_browse_form").
   CarrierDeliveryStatusBrowseForm:insertHiddenField("prog_name","adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierDeliveryStatusBrowseForm}
   
   /* Create Button Bar */
   CarrierDeliveryStatusBrowseButtons = NEW buttonBar().
   
   CarrierDeliveryStatusBrowseButtons:addButton("carrierdeliverystatus_browse_form_btn_create",
      fTL("Create"),
      "createCarrierDeliveryStatus('carrierdeliverystatus_details_form');"). 
   
   CarrierDeliveryStatusBrowseButtons:addButton("carrierdeliverystatus_browse_form_btn_view",
      fTL("Details"),
      "viewCarrierDeliveryStatusDetails('carrierdeliverystatus_details_form');",
      (IF intSelectedCarrierDeliveryStatus > 0 THEN "" ELSE "Disabled")).
       
   CarrierDeliveryStatusBrowseButtons:addButton("carrierdeliverystatus_browse_form_btn_cancel",
      fTL("Cancel"),
      "disablePopup('carrierdeliverystatus_browse_form_popup');").
   
   CarrierDeliveryStatusBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierDeliveryStatusBrowseForm:FormBrowse  = CarrierDeliveryStatusBrowse.
   CarrierDeliveryStatusBrowseForm:FormButtons = CarrierDeliveryStatusBrowseButtons.
   CarrierDeliveryStatusBrowseForm:endForm(). 
   
   CarrierDeliveryStatusBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierDeliveryStatusDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierDeliveryStatusDetails Procedure
PROCEDURE pCarrierDeliveryStatusDetails:
/*------------------------------------------------------------------------------
    Purpose:
    Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "carrierdeliverystatus_details_form"}
   
   ASSIGN 
      chrDisplayFieldList  = "CarrierDeliveryStatusID,DeliveryCode,CustomerDeliveryCode,FailedDelivery"
      chrEditFieldList     = "DeliveryCode,CustomerDeliveryCode,FailedDelivery"
      chrNewFieldList      = "DeliveryCode,CustomerDeliveryCode,FailedDelivery"
      chrRequiredFieldList = "DeliveryCode"
      chrExtraFieldList    = ""
      chrValidateFieldList = "".
   
   CarrierDeliveryStatusDetailsForm = NEW dataForm("carrierdeliverystatus_details_form").
   CarrierDeliveryStatusDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierDeliveryStatusDetailsForm:FormAction  = "dbCarrierDeliveryStatusUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierDeliveryStatusDetailsForm:FormWidth   = 460.
   CarrierDeliveryStatusDetailsForm:FormHeight  = 300.
   CarrierDeliveryStatusDetailsForm:FormTitle   = "Delivery Status Details".
   CarrierDeliveryStatusDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CarrierDeliveryStatusDetailsForm:insertPaddingColumn(70).
   CarrierDeliveryStatusDetailsForm:insertColumn(90).
   CarrierDeliveryStatusDetailsForm:insertColumn(120).
   CarrierDeliveryStatusDetailsForm:insertColumn(30).
   CarrierDeliveryStatusDetailsForm:insertColumn(120).  
   
   /* Fields */
   CarrierDeliveryStatusDetailsForm:startRow().
   CarrierDeliveryStatusDetailsForm:insertLabel(fTL("ID")).
   CarrierDeliveryStatusDetailsForm:insertTextField("CarrierDeliveryStatusID", "", 110, TRUE).    
   
   CarrierDeliveryStatusDetailsForm:startRow().
   CarrierDeliveryStatusDetailsForm:insertLabel(fTL("DeliveryCode")).
   CarrierDeliveryStatusDetailsForm:insertTextField("DeliveryCode", "", 200, TRUE). 
   
   CarrierDeliveryStatusDetailsForm:startRow().
   CarrierDeliveryStatusDetailsForm:insertLabel(fTL("CustomerDeliveryCode")).
   CarrierDeliveryStatusDetailsForm:insertTextField("CustomerDeliveryCode", "", 200, TRUE).  
   
   CarrierDeliveryStatusDetailsForm:startRow().
   CarrierDeliveryStatusDetailsForm:insertLabel(fTL("FailedDelivery")). 
   CarrierDeliveryStatusDetailsForm:insertComboField("FailedDelivery", "", 110, TRUE).  
   CarrierDeliveryStatusDetailsForm:insertComboPairs("FailedDelivery", "yes", "Yes").
   CarrierDeliveryStatusDetailsForm:insertComboPairs("FailedDelivery", "no",  "No").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pCarrierDeliveryStatusDetailsFields}
   
   /* Add Hidden Fields*/
   CarrierDeliveryStatusDetailsForm:insertHiddenField("carrier_browse_scroll", "").
   CarrierDeliveryStatusDetailsForm:insertHiddenField("popup_carrierdeliverystatus_browse", "").
   CarrierDeliveryStatusDetailsForm:insertHiddenField("CarrierID",        STRING(intSelectedCarrier)).
   CarrierDeliveryStatusDetailsForm:insertHiddenField("CarrierDeliveryStatusID", STRING(intSelectedCarrierDeliveryStatus)).
   CarrierDeliveryStatusDetailsForm:insertHiddenField("form_name",        "carrierdeliverystatus_details_form").
   CarrierDeliveryStatusDetailsForm:insertHiddenField("prog_name",        "adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierDeliveryStatusDetailsForm}
   
   /* Create Button Bar */
   CarrierDeliveryStatusDetailsButtons = NEW buttonBar().
   
   CarrierDeliveryStatusDetailsButtons:addButton("carrierdeliverystatus_details_form_btn_save", 
      fTL("Save"), 
      "updateCarrierDeliveryStatus('carrierdeliverystatus_details_form');").
   
   CarrierDeliveryStatusDetailsButtons:addButton("carrierdeliverystatus_details_form_btn_cancel", 
      fTL("Cancel"), 
      "cancelUpdate('UserCancelled','process_mode'); " 
      + "disablePopup('carrierdeliverystatus_details_form_popup');").
   CarrierDeliveryStatusDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierDeliveryStatusDetailsForm:FormButtons = CarrierDeliveryStatusDetailsButtons.
   
   CarrierDeliveryStatusDetailsForm:endForm(). 
   CarrierDeliveryStatusDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierDetails Procedure 
PROCEDURE pCarrierDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "carrier_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CarrierID,CarrierCode,CarrierName,InternalCustomerRef,MaxValuePerTrailer,Active,UseCarrierSpecificLabel" 
                                    + ",GenerateCarrierTrackingRef,DangerousGoodsLimit,ScacCode,TrackingRefUrl"
          chrEditFieldList     = "CarrierCode,CarrierName,InternalCustomerRef,MaxValuePerTrailer,Active,UseCarrierSpecificLabel" 
                                    + ",GenerateCarrierTrackingRef,DangerousGoodsLimit,ScacCode,TrackingRefUrl"
          chrNewFieldList      = "CarrierCode,CarrierName,InternalCustomerRef,MaxValuePerTrailer,Active,UseCarrierSpecificLabel"
                                    + ",GenerateCarrierTrackingRef,DangerousGoodsLimit,ScacCode,TrackingRefUrl"
          chrRequiredFieldList = "CarrierCode,CarrierName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CarrierDetailsForm = NEW dataForm("carrier_details_form").
   CarrierDetailsForm:WebStream = STREAM WebStream:HANDLE.
   /*CarrierDetailsForm:ShowLock = TRUE.*/
            
   CarrierDetailsForm:FormAction  = "dbCarrierUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierDetailsForm:FormWidth   = 460.
   CarrierDetailsForm:FormHeight  = 300.
   CarrierDetailsForm:FormTitle   = "Carrier Details".
   CarrierDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CarrierDetailsForm:insertPaddingColumn(10).
   CarrierDetailsForm:insertColumn(150).
   CarrierDetailsForm:insertColumn(120).
   CarrierDetailsForm:insertColumn(20).
   CarrierDetailsForm:insertColumn(4).
   CarrierDetailsForm:insertColumn(110).
   
   /* Fields */
   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel("Carrier ID").
   CarrierDetailsForm:insertTextField("CarrierID", "", 110, TRUE).  
   
   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel("Carrier Code").
   CarrierDetailsForm:insertTextField("CarrierCode", "", 180, TRUE).  
   
   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel("Carrier Name").
   CarrierDetailsForm:insertTextField("CarrierName", "", 280, TRUE).  
   
   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel(fTL("Internal Customer Ref")). 
   CarrierDetailsForm:insertTextField("InternalCustomerRef", "", 280, TRUE).

   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel("Trailer MaxValue").
   CarrierDetailsForm:insertTextField("MaxValuePerTrailer", "", 180, TRUE).  
   
   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel("Dangerous Goods Limit").
   CarrierDetailsForm:insertTextField("DangerousGoodsLimit", "", 180, TRUE).
   
   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel("SCAC Code").
   CarrierDetailsForm:insertTextField("ScacCode", "", 180, TRUE).    

   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel("Tracking URL").
   CarrierDetailsForm:insertTextField("TrackingRefUrl", "", 280, TRUE).   

   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel(fTL("Active")). 
   CarrierDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CarrierDetailsForm:insertComboPairs("Active", "yes", "Active").
   CarrierDetailsForm:insertComboPairs("Active", "no",  "Not Active").

   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel(fTL("Use Specific Label")). 
   CarrierDetailsForm:insertComboField("UseCarrierSpecificLabel", "", 210, TRUE).  
   CarrierDetailsForm:insertComboPairs("UseCarrierSpecificLabel", "yes", "Use Carrier Specific Label").
   CarrierDetailsForm:insertComboPairs("UseCarrierSpecificLabel", "no",  "Do Not Use Carrier Specific Label").

   CarrierDetailsForm:startRow().
   CarrierDetailsForm:insertLabel(fTL("Generate Tracking Ref")). 
   CarrierDetailsForm:insertComboField("GenerateCarrierTrackingRef", "", 235, TRUE).  
   CarrierDetailsForm:insertComboPairs("GenerateCarrierTrackingRef", "yes", "Generate Carrier Tracking Ref").
   CarrierDetailsForm:insertComboPairs("GenerateCarrierTrackingRef", "no",  "Do Not Generate Carrier Tracking Ref").

   {webGetOptionalFormFields.i pCarrierDetailsFields}
   
   /* Add Hidden Fields*/
   CarrierDetailsForm:insertHiddenField("carrier_browse_scroll", "").
   CarrierDetailsForm:insertHiddenField("form_name", "carrier_details_form").
   CarrierDetailsForm:insertHiddenField("prog_name", "adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierDetailsForm}
   
   /* Create Button Bar */
   CarrierDetailsButtons = NEW buttonBar().
   CarrierDetailsButtons:addButton("carrier_details_form_btn_save", 
                                   fTL("Save"), 
                                   "updateCarrier('carrier_details_form');").
   CarrierDetailsButtons:addButton("carrier_details_form_btn_cancel", 
                                   fTL("Cancel"), 
                                   "cancelUpdate('UserCancelled','process_mode'); disablePopup('carrier_details_form_popup');").
   CarrierDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierDetailsForm:FormButtons = CarrierDetailsButtons.
   
   CarrierDetailsForm:endForm(). 
   
   CarrierDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + CarrierDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCarrierDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierDetailsFields Procedure 
PROCEDURE pCarrierDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "CutOffTime" THEN
      DO:
         CarrierDetailsForm:startRow().
         CarrierDetailsForm:insertLabel(fTL("Cut-off Time")).
         CarrierDetailsForm:insertTextField("CutOffTime", "", 280, TRUE).  
      END. /*WHEN "CutOffTime" THEN*/
      
      /* This will be held in customer specific code repository */
      {adCarrierAdmin_carrier_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCarrierHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierHistoryBrowse Procedure
PROCEDURE pCarrierHistoryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   CarrierHistoryBrowseForm           = NEW dataForm("carrierhistory_browse_form").
   CarrierHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CarrierHistoryBrowseForm:FormWidth  = 860.
   CarrierHistoryBrowseForm:FormHeight = 530.
   CarrierHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE Carrier THEN " for Carrier: " 
                                          + STRING(Carrier.CarrierCode) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ID:" 
                                          + STRING(Carrier.CarrierID) ELSE "").
   CarrierHistoryBrowseForm:FormType   = "xxl_large".
   CarrierHistoryBrowse                = NEW browseTable("carrierhistory_browse").
   CarrierHistoryBrowse:BrowseWidth    = 840.
   CarrierHistoryBrowse:BrowseHeight   = 490.
   
   CarrierHistoryBrowse:insertColumn(fTL("HistoryID"),    60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CarrierHistory}
   
   CarrierHistoryBrowse:insertColumn(fTL("Code"),         50, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("Name"),        100, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("IntCustRef"),   70, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("MaxVal"),       70, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("Dangerous"),    70, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("SpecLbl"),      70, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("GenTrack"),     70, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("Active"),       50, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("User"),         70, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("Operation"),    70, "CHARACTER", "left", FALSE).
   CarrierHistoryBrowse:insertColumn(fTL("Created"),      70, "CHARACTER", "left", FALSE).
   
   CarrierHistoryBrowse:StartBody().
   
   IF AVAILABLE Carrier THEN
   DO:
      /*List the CarrierHistorys for the Carrier*/
      FOR EACH CarrierHistory NO-LOCK
         WHERE CarrierHistory.CarrierID = Carrier.CarrierID
         BY CarrierHistory.CarrierHistoryID: /*idxCarrierHistoryID*/
         
         FIND OperationType OF CarrierHistory NO-LOCK NO-ERROR.
         FIND GateUser      OF CarrierHistory NO-LOCK NO-ERROR.
         
         CarrierHistoryBrowse:startRow(CarrierHistory.CarrierHistoryID, "selectBrowseRow(this," + '"carrierhistory_browse"' + ");","").
         CarrierHistoryBrowse:insertData(CarrierHistory.CarrierHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CarrierHistory}
         
         CarrierHistoryBrowse:insertData(CarrierHistory.CarrierCode, "left").
         CarrierHistoryBrowse:insertData(CarrierHistory.CarrierName, "left").
         CarrierHistoryBrowse:insertData(CarrierHistory.InternalCustomerRef, "left").
         CarrierHistoryBrowse:insertData(CarrierHistory.MaxValuePerTrailer, "left").
         CarrierHistoryBrowse:insertData(CarrierHistory.DangerousGoodsLimit,"left").
         CarrierHistoryBrowse:insertData(CarrierHistory.UseCarrierSpecificLabel, "left").
         CarrierHistoryBrowse:insertData(CarrierHistory.GenerateCarrierTrackingRef, "left").
         CarrierHistoryBrowse:insertData(CarrierHistory.Active, "left").
         CarrierHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         CarrierHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
         CarrierHistoryBrowse:insertData(fDisplayDate&Time(CarrierHistory.Created,"y/m/d H:M:S"), "right").
         
         CarrierHistoryBrowse:endRow().
      
      END. /* FOR EACH CarrierHistory OF Carrier NO-LOCK */
   END. /*IF AVAILABLE Carrier THEN*/
   
   CarrierHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierHistoryBrowse:getErrors().
   
   CarrierHistoryBrowseForm:insertHiddenField("popup_carrierhistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierHistoryBrowseForm}
   
   /* Create Button Bar */
   CarrierHistoryBrowseButtons = NEW buttonBar().
   
   CarrierHistoryBrowseButtons:addButton("carrierhistory_browse_form_btn_cancel",
                                          fTL("Cancel"),
                                          "disablePopup('carrierhistory_browse_form_popup');").
   
   CarrierHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierHistoryBrowseForm:FormBrowse  = CarrierHistoryBrowse.
   CarrierHistoryBrowseForm:FormButtons = CarrierHistoryBrowseButtons.
   CarrierHistoryBrowseForm:endForm(). 
   
   CarrierHistoryBrowseForm:displayForm().
   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierModeBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierModeBrowse Procedure
PROCEDURE pCarrierModeBrowse:
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "carriermode_details_form"}
   
   CarrierModeBrowseForm = NEW dataForm("carriermode_browse_form").
   CarrierModeBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierModeBrowseForm:FormAction  = "dbCarrierModeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierModeBrowseForm:FormWidth   = 700.
   CarrierModeBrowseForm:FormHeight  = 490.
   CarrierModeBrowseForm:FormTitle   = fTL("Mode for Carrier") + (IF AVAILABLE Carrier THEN " : " + Carrier.CarrierName ELSE "").
   CarrierModeBrowseForm:FormType    = "xl_large".
   
   CarrierModeBrowse = NEW browseTable("carriermode_browse").
   CarrierModeBrowse:BrowseWidth  = 680.
   CarrierModeBrowse:BrowseHeight = 432.
   
   CarrierModeBrowse:insertColumn(fTL("ModeID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CarrierMode}
   
   CarrierModeBrowse:insertColumn(fTL("Mode Code"), 120, "CHARACTER", "left", FALSE).
   CarrierModeBrowse:insertColumn(fTL("Mode Name"), 140, "CHARACTER", "left", FALSE).
   CarrierModeBrowse:insertColumn(fTL("Active"),     80, "CHARACTER",         FALSE).
   CarrierModeBrowse:StartBody().
   
   IF AVAILABLE Carrier THEN
   DO:
      /*List the CarrierModes for the Carrier*/
      FOR EACH CarrierMode OF Carrier NO-LOCK:
         
         CarrierModeBrowse:startRow(CarrierMode.CarrierModeID, "selectCarrierModeRow(this," + '"' 
            + STRING(CarrierMode.CarrierModeID) + '","adCarrierAdmin.p","carriermode_browse_form"' + ");", "").
            
         CarrierModeBrowse:insertData(CarrierMode.CarrierModeID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CarrierMode}
         
         CarrierModeBrowse:insertData(CarrierMode.ModeCode, "left").
         CarrierModeBrowse:insertData(CarrierMode.ModeName, "left").
         CarrierModeBrowse:insertData(STRING(CarrierMode.Active,"Yes/No")).
               
         /* Add hidden fields */
         CarrierModeBrowse:insertHiddenData("CarrierID",CarrierMode.CarrierID).
         CarrierModeBrowse:insertHiddenData("CarrierModeID",CarrierMode.CarrierModeID).
         CarrierModeBrowse:insertHiddenData("CarrierModeVersionID",CarrierMode.VersionID).
         CarrierModeBrowse:insertHiddenData("CarrierModeName",CarrierMode.ModeName).
         CarrierModeBrowse:endRow().
      
      END. /* FOR EACH CarrierMode*/
   END. /*IF AVAILABLE Carrier THEN*/
   
   CarrierModeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierModeBrowse:getErrors().
   
   CarrierModeBrowseForm:insertHiddenField("CarrierID","").
   CarrierModeBrowseForm:insertHiddenField("CarrierName","").
   CarrierModeBrowseForm:insertHiddenField("CarrierModeName","").
   CarrierModeBrowseForm:insertHiddenField("CarrierModeID","").
   CarrierModeBrowseForm:insertHiddenField("CarrierModeVersionID","").
   CarrierModeBrowseForm:insertHiddenField("carrier_browse_scroll","").
   CarrierModeBrowseForm:insertHiddenField("carriermode_browse_scroll","").
   CarrierModeBrowseForm:insertHiddenField("popup_carriermode_browse","").
   CarrierModeBrowseForm:insertHiddenField("form_name","carriermode_browse_form").
   CarrierModeBrowseForm:insertHiddenField("prog_name","adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierModeBrowseForm}
   
   /* Create Button Bar */
   CarrierModeBrowseButtons = NEW buttonBar().
   
   CarrierModeBrowseButtons:addButton("carriermode_browse_form_btn_create",
      fTL("Create"),
      "createCarrierMode('carriermode_details_form');"). 
   
   CarrierModeBrowseButtons:addButton("carriermode_browse_form_btn_view",
      fTL("Details"),
      "viewCarrierModeDetails('carriermode_details_form');",
      (IF intSelectedCarrierMode > 0 THEN "" ELSE "Disabled")).
   
   CarrierModeBrowseButtons:addButton("carriermode_browse_form_btn_delete",
      fTL("Delete"),
      "confirmDeleteCarrierMode('carriermode_browse_form');", 
      (IF intSelectedCarrierMode > 0 THEN "" ELSE "Disabled")).
   
   CarrierModeBrowseButtons:addButton("carriermode_browse_form_btn_cancel",
      fTL("Cancel"),
      "disablePopup('carriermode_browse_form_popup');").
   
   CarrierModeBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierModeBrowseForm:FormBrowse  = CarrierModeBrowse.
   CarrierModeBrowseForm:FormButtons = CarrierModeBrowseButtons.
   CarrierModeBrowseForm:endForm(). 
   
   CarrierModeBrowseForm:displayForm().
   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierModeDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierModeDetails Procedure
PROCEDURE pCarrierModeDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "carriermode_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CarrierModeID,ModeName,ModeCode,Active"
          chrEditFieldList     = "ModeName,Active"
          chrNewFieldList      = "ModeName,ModeCode,Active"
          chrRequiredFieldList = "ModeName,ModeCode"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CarrierModeDetailsForm = NEW dataForm("carriermode_details_form").
   CarrierModeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierModeDetailsForm:FormAction  = "dbCarrierModeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierModeDetailsForm:FormWidth   = 460.
   CarrierModeDetailsForm:FormHeight  = 300.
   CarrierModeDetailsForm:FormTitle   = "Carrier Mode Details".
   CarrierModeDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CarrierModeDetailsForm:insertPaddingColumn(70).
   CarrierModeDetailsForm:insertColumn(90).
   CarrierModeDetailsForm:insertColumn(120).
   CarrierModeDetailsForm:insertColumn(30).
   CarrierModeDetailsForm:insertColumn(120).  
   
   /* Fields */
   CarrierModeDetailsForm:startRow().
   CarrierModeDetailsForm:insertLabel(fTL("Mode ID")).
   CarrierModeDetailsForm:insertTextField("CarrierModeID", "", 110, TRUE).    
   
   CarrierModeDetailsForm:startRow().
   CarrierModeDetailsForm:insertLabel(fTL("Mode Code")).
   CarrierModeDetailsForm:insertTextField("ModeCode", "", 200, TRUE). 
   
   CarrierModeDetailsForm:startRow().
   CarrierModeDetailsForm:insertLabel(fTL("Mode Name")).
   CarrierModeDetailsForm:insertTextField("ModeName", "", 200, TRUE).  
   
   CarrierModeDetailsForm:startRow().
   CarrierModeDetailsForm:insertLabel(fTL("Active")). 
   CarrierModeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CarrierModeDetailsForm:insertComboPairs("Active", "yes", "Active").
   CarrierModeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pCarrierModeDetailsFields}
   
   /* Add Hidden Fields*/
   CarrierModeDetailsForm:insertHiddenField("carrier_browse_scroll", "").
   CarrierModeDetailsForm:insertHiddenField("popup_carriermode_browse", "").
   CarrierModeDetailsForm:insertHiddenField("CarrierID",        STRING(intSelectedCarrier)).
   CarrierModeDetailsForm:insertHiddenField("CarrierModeID", STRING(intSelectedCarrierMode)).
   CarrierModeDetailsForm:insertHiddenField("form_name",        "carriermode_details_form").
   CarrierModeDetailsForm:insertHiddenField("prog_name",        "adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierModeDetailsForm}
   
   /* Create Button Bar */
   CarrierModeDetailsButtons = NEW buttonBar().
   
   CarrierModeDetailsButtons:addButton("carriermode_details_form_btn_save", 
                                          fTL("Save"), 
                                          "updateCarrierMode('carriermode_details_form');").
   
   CarrierModeDetailsButtons:addButton("carriermode_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode'); " 
                                          + "disablePopup('carriermode_details_form_popup');").
   CarrierModeDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierModeDetailsForm:FormButtons = CarrierModeDetailsButtons.
   
   CarrierModeDetailsForm:endForm(). 
   CarrierModeDetailsForm:displayForm(). 


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierServiceBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierServiceBrowse Procedure
PROCEDURE pCarrierServiceBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "carrierservice_details_form"}
   
   CarrierServiceBrowseForm = NEW dataForm("carrierservice_browse_form").
   CarrierServiceBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierServiceBrowseForm:FormAction  = "dbCarrierServiceUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierServiceBrowseForm:FormWidth   = 700.
   CarrierServiceBrowseForm:FormHeight  = 490.
   CarrierServiceBrowseForm:FormTitle   = fTL("Services for Carrier") + (IF AVAILABLE Carrier THEN " : " + Carrier.CarrierName ELSE "").
   CarrierServiceBrowseForm:FormType    = "xl_large".
   
   CarrierServiceBrowse = NEW browseTable("carrierservice_browse").
   CarrierServiceBrowse:BrowseWidth  = 680.
   CarrierServiceBrowse:BrowseHeight = 432.
   
   CarrierServiceBrowse:insertColumn(fTL("ServiceID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CarrierService}
   
   CarrierServiceBrowse:insertColumn(fTL("Service Code"), 120, "CHARACTER", "left", FALSE).
   CarrierServiceBrowse:insertColumn(fTL("Service Name"), 140, "CHARACTER", "left", FALSE).
   CarrierServiceBrowse:insertColumn(fTL("Symbol"),        80, "INTEGER",           FALSE).
   CarrierServiceBrowse:insertColumn(fTL("COD Service"),   80, "INTEGER",           FALSE).
   CarrierServiceBrowse:insertColumn(fTL("Active"),        80, "CHARACTER",         FALSE).
   CarrierServiceBrowse:insertColumn("",                   35, "",                  FALSE).
   
   CarrierServiceBrowse:StartBody().
   
   IF AVAILABLE Carrier THEN
   DO:
      /*List the CarrierServices for the Carrier*/
      FOR EACH CarrierService OF Carrier NO-LOCK:
         
         CarrierServiceBrowse:startRow(CarrierService.CarrierServiceID, "selectCarrierServiceRow(this," + '"' 
                         + STRING(CarrierService.CarrierServiceID) + '","adCarrierAdmin.p","carrierservice_browse_form"' + ");", "").
         CarrierServiceBrowse:insertData(CarrierService.CarrierServiceID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CarrierService}
         
         CarrierServiceBrowse:insertData(CarrierService.ServiceCode, "left").
         CarrierServiceBrowse:insertData(CarrierService.ServiceName, "left").
         CarrierServiceBrowse:insertData(CarrierService.Symbol, "right").
         CarrierServiceBrowse:insertData(STRING(CarrierService.CashOnDeliveryService), "Yes/No").
         CarrierServiceBrowse:insertData(STRING(CarrierService.Active,"Yes/No")).
         CarrierServiceBrowse:insertImage("view", "View Change History", 'viewCarrierServiceHistoryBrowse("' 
                                     + STRING(CarrierService.CarrierID) + '", "' + STRING(CarrierService.CarrierserviceID) + '", this.parentNode);').
         
         /* Add hidden fields */
         CarrierServiceBrowse:insertHiddenData("CarrierID",CarrierService.CarrierID).
         CarrierServiceBrowse:insertHiddenData("CarrierServiceID",CarrierService.CarrierServiceID).
         CarrierServiceBrowse:insertHiddenData("CarrierServiceVersionID",CarrierService.VersionID).
         CarrierServiceBrowse:insertHiddenData("CarrierServiceName",CarrierService.ServiceName).
         CarrierServiceBrowse:endRow().
      
      END. /* FOR EACH CarrierService OF Carrier NO-LOCK, EACH CarrierService OF CarrierService NO-LOCK */
   END. /*IF AVAILABLE Carrier THEN*/
   
   CarrierServiceBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierServiceBrowse:getErrors().
   
   CarrierServiceBrowseForm:insertHiddenField("CarrierID","").
   CarrierServiceBrowseForm:insertHiddenField("CarrierName","").
   CarrierServiceBrowseForm:insertHiddenField("CarrierServiceName","").
   CarrierServiceBrowseForm:insertHiddenField("CarrierServiceID","").
   CarrierServiceBrowseForm:insertHiddenField("CarrierServiceVersionID","").
   CarrierServiceBrowseForm:insertHiddenField("carrier_browse_scroll","").
   CarrierServiceBrowseForm:insertHiddenField("carrierservice_browse_scroll","").
   CarrierServiceBrowseForm:insertHiddenField("popup_carrierservice_browse","").
   CarrierServiceBrowseForm:insertHiddenField("popup_carrierservicehistory_browse","").
   CarrierServiceBrowseForm:insertHiddenField("form_name","carrierservice_browse_form").
   CarrierServiceBrowseForm:insertHiddenField("prog_name","adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierServiceBrowseForm}
   
   /* Create Button Bar */
   CarrierServiceBrowseButtons = NEW buttonBar().
   
   CarrierServiceBrowseButtons:addButton("carrierservice_browse_form_btn_create",
                                         fTL("Create"),
                                         "createCarrierService('carrierservice_details_form');"). 
   
   CarrierServiceBrowseButtons:addButton("carrierservice_browse_form_btn_view",
                                         fTL("Details"),
                                         "viewCarrierServiceDetails('carrierservice_details_form');",
                                         (IF intSelectedCarrierService > 0 THEN "" ELSE "Disabled")).
   
   CarrierServiceBrowseButtons:addButton("carrierservice_browse_form_btn_delete",
                                         fTL("Delete"),
                                         "confirmDeleteCarrierService('carrierservice_browse_form');", 
                                         (IF intSelectedCarrierService > 0 THEN "" ELSE "Disabled")).
   
   CarrierServiceBrowseButtons:addButton("carrierservice_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('carrierservice_browse_form_popup');").
   
   CarrierServiceBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierServiceBrowseForm:FormBrowse  = CarrierServiceBrowse.
   CarrierServiceBrowseForm:FormButtons = CarrierServiceBrowseButtons.
   CarrierServiceBrowseForm:endForm(). 
   
   CarrierServiceBrowseForm:displayForm().


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierServiceDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierServiceDetails Procedure
PROCEDURE pCarrierServiceDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "carrierservice_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CarrierServiceID,ServiceName,ServiceCode,Symbol,MinPackageWeight,MaxPackageWeight,CashOnDeliveryService," +
                                 "TmsServiceCode,CustomerServiceCode,Active"
          chrEditFieldList     = "ServiceName,Symbol,MinPackageWeight,MaxPackageWeight,CashOnDeliveryService,TmsServiceCode,CustomerServiceCode,Active"
          chrNewFieldList      = "ServiceName,ServiceCode,Symbol,MinPackageWeight,MaxPackageWeight,CashOnDeliveryService," +
                                 "TmsServiceCode,CustomerServiceCode,Active"
          chrRequiredFieldList = "ServiceName,ServiceCode"
          chrExtraFieldList    = ""
          chrValidateFieldList = "MaxPackageWeight:DECIMAL,MinPackageWeight:DECIMAL".
   
   CarrierServiceDetailsForm = NEW dataForm("carrierservice_details_form").
   CarrierServiceDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierServiceDetailsForm:FormAction  = "dbCarrierServiceUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierServiceDetailsForm:FormWidth   = 460.
   CarrierServiceDetailsForm:FormHeight  = 300.
   CarrierServiceDetailsForm:FormTitle   = "Carrier Service Details".
   CarrierServiceDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CarrierServiceDetailsForm:insertPaddingColumn(50).
   CarrierServiceDetailsForm:insertColumn(120).
   CarrierServiceDetailsForm:insertColumn(120).
   CarrierServiceDetailsForm:insertColumn(30).
   CarrierServiceDetailsForm:insertColumn(120).  
   
   /* Fields */
   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("Service ID")).
   CarrierServiceDetailsForm:insertTextField("CarrierServiceID", "", 110, TRUE).    
   
   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("Service Code")).
   CarrierServiceDetailsForm:insertTextField("ServiceCode", "", 200, TRUE). 
   
   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("Service Name")).
   CarrierServiceDetailsForm:insertTextField("ServiceName", "", 200, TRUE).  
   
   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("Symbol")).
   CarrierServiceDetailsForm:insertTextField("Symbol", "", 110, TRUE).  
         
   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("MinPackageWeight")).
   CarrierServiceDetailsForm:insertTextField("MinPackageWeight", "", 110, TRUE).  
   
   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("MaxPackageWeight")).
   CarrierServiceDetailsForm:insertTextField("MaxPackageWeight", "", 110, TRUE).  

   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("COD Service")). 
   CarrierServiceDetailsForm:insertComboField("CashOnDeliveryService", "", 200, TRUE).  
   CarrierServiceDetailsForm:insertComboPairs("CashOnDeliveryService", "yes", "CashOnDeliveryService").
   CarrierServiceDetailsForm:insertComboPairs("CashOnDeliveryService", "no",  "Not CashOnDeliveryService").

   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("TMS Service Code")).
   CarrierServiceDetailsForm:insertTextField("TmsServiceCode", "", 200, TRUE).
                                              
   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("CustServiceCode")).
   CarrierServiceDetailsForm:insertTextField("CustomerServiceCode", "", 200, TRUE).

   CarrierServiceDetailsForm:startRow().
   CarrierServiceDetailsForm:insertLabel(fTL("Active")). 
   CarrierServiceDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CarrierServiceDetailsForm:insertComboPairs("Active", "yes", "Active").
   CarrierServiceDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pCarrierServiceDetailsFields}
   
   /* Add Hidden Fields*/
   CarrierServiceDetailsForm:insertHiddenField("carrier_browse_scroll", "").
   CarrierServiceDetailsForm:insertHiddenField("popup_carrierservice_browse", "").
   CarrierServiceDetailsForm:insertHiddenField("CarrierID",        STRING(intSelectedCarrier)).
   CarrierServiceDetailsForm:insertHiddenField("CarrierServiceID", STRING(intSelectedCarrierService)).
   CarrierServiceDetailsForm:insertHiddenField("form_name",        "carrierservice_details_form").
   CarrierServiceDetailsForm:insertHiddenField("prog_name",        "adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierServiceDetailsForm}
   
   /* Create Button Bar */
   CarrierServiceDetailsButtons = NEW buttonBar().
   
   CarrierServiceDetailsButtons:addButton("carrierservice_details_form_btn_save", 
                                          fTL("Save"), 
                                          "updateCarrierService('carrierservice_details_form');").
   
   CarrierServiceDetailsButtons:addButton("carrierservice_details_form_btn_cancel", 
                                          fTL("Cancel"), 
                                          "cancelUpdate('UserCancelled','process_mode'); " 
                                          + "disablePopup('carrierservice_details_form_popup');").
   CarrierServiceDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierServiceDetailsForm:FormButtons = CarrierServiceDetailsButtons.
   
   CarrierServiceDetailsForm:endForm(). 
   CarrierServiceDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + CarrierServiceDetailsForm:getErrors().  */
   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierServiceDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierServiceDetailsFields Procedure
PROCEDURE pCarrierServiceDetailsFields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adCarrierAdmin_carrierservice_details_form.i}
      
   END CASE. /*chrOption:*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pCarrierServiceHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierServiceHistoryBrowse Procedure
PROCEDURE pCarrierServiceHistoryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   CarrierServiceHistoryBrowseForm           = NEW dataForm("carrierservicehistory_browse_form").
   CarrierServiceHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CarrierServiceHistoryBrowseForm:FormWidth  = 860.
   CarrierServiceHistoryBrowseForm:FormHeight = 530.
   CarrierServiceHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE CarrierService THEN " for Carrier Service: " 
                                          + STRING(CarrierService.ServiceCode) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ID:" 
                                          + STRING(CarrierService.CarrierServiceID) ELSE "").
   CarrierServiceHistoryBrowseForm:FormType   = "xxl_large".
   CarrierServiceHistoryBrowse                = NEW browseTable("carrierservicehistory_browse").
   CarrierServiceHistoryBrowse:BrowseWidth    = 840.
   CarrierServiceHistoryBrowse:BrowseHeight   = 490.
   
   CarrierServiceHistoryBrowse:insertColumn(fTL("HistoryID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CarrierServiceHistory}
   
   CarrierServiceHistoryBrowse:insertColumn(fTL("Code"),         60, "CHARACTER", "left", FALSE).
   CarrierServiceHistoryBrowse:insertColumn(fTL("Name"),        130, "CHARACTER", "left", FALSE).
   CarrierServiceHistoryBrowse:insertColumn(fTL("Symbol"),      120, "CHARACTER", "left", FALSE).
   CarrierServiceHistoryBrowse:insertColumn(fTL("CODService"),  120, "CHARACTER", "left", FALSE).
   CarrierServiceHistoryBrowse:insertColumn(fTL("Active"),       50, "CHARACTER", "left", FALSE).
   CarrierServiceHistoryBrowse:insertColumn(fTL("User"),         80, "CHARACTER", "left", FALSE).
   CarrierServiceHistoryBrowse:insertColumn(fTL("Operation"),    80, "CHARACTER", "left", FALSE).
   CarrierServiceHistoryBrowse:insertColumn(fTL("Created"),      90, "CHARACTER", "left", FALSE).
   
   CarrierServiceHistoryBrowse:StartBody().
   
   IF AVAILABLE CarrierService THEN
   DO:
      /*List the CarrierServiceHistorys for the CarrierService*/
      FOR EACH CarrierServiceHistory NO-LOCK
         WHERE CarrierServiceHistory.CarrierServiceID = CarrierService.CarrierServiceID
         BY CarrierServiceHistory.CarrierServiceHistoryID: /*idxCarrierServiceHistoryID*/
         
         FIND OperationType OF CarrierServiceHistory NO-LOCK NO-ERROR.
         FIND GateUser      OF CarrierServiceHistory NO-LOCK NO-ERROR.
         
         CarrierServiceHistoryBrowse:startRow(CarrierServiceHistory.CarrierServiceHistoryID, "selectBrowseRow(this," + '"carrierservicehistory_browse"' + ");","").
         CarrierServiceHistoryBrowse:insertData(CarrierServiceHistory.CarrierServiceHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CarrierServiceHistory}
         
         CarrierServiceHistoryBrowse:insertData(CarrierServiceHistory.ServiceCode, "left").
         CarrierServiceHistoryBrowse:insertData(CarrierServiceHistory.ServiceName, "left").
         CarrierServiceHistoryBrowse:insertData(CarrierServiceHistory.Symbol, "left").
         CarrierServiceHistoryBrowse:insertData(CarrierServiceHistory.CashOnDeliveryService, "left").
         CarrierServiceHistoryBrowse:insertData(CarrierServiceHistory.Active, "left").
         CarrierServiceHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         CarrierServiceHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
         CarrierServiceHistoryBrowse:insertData(fDisplayDate&Time(CarrierServiceHistory.Created,"y/m/d H:M:S"), "right").
         
         CarrierServiceHistoryBrowse:endRow().
      
      END. /* FOR EACH CarrierServiceHistory OF Carrier NO-LOCK */
   END. /*IF AVAILABLE Carrier THEN*/
   
   CarrierServiceHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierServiceHistoryBrowse:getErrors().
   
   CarrierServiceHistoryBrowseForm:insertHiddenField("popup_carrierservicehistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierServiceHistoryBrowseForm}
   
   /* Create Button Bar */
   CarrierServiceHistoryBrowseButtons = NEW buttonBar().
   
   CarrierServiceHistoryBrowseButtons:addButton("carrierservicehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('carrierservicehistory_browse_form_popup');").
   
   CarrierServiceHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierServiceHistoryBrowseForm:FormBrowse  = CarrierServiceHistoryBrowse.
   CarrierServiceHistoryBrowseForm:FormButtons = CarrierServiceHistoryBrowseButtons.
   CarrierServiceHistoryBrowseForm:endForm(). 
   
   CarrierServiceHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCarrierSortationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierSortationBrowse Procedure 
PROCEDURE pCarrierSortationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE VARIABLE intPossibleShipLanes         AS INTEGER     NO-UNDO.
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "carriersortation_details_form"}
   
   CarrierSortationBrowseForm = NEW dataForm("carriersortation_browse_form").
   CarrierSortationBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierSortationBrowseForm:FormAction  = "dbCarrierSortationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierSortationBrowseForm:FormWidth   = 700.
   CarrierSortationBrowseForm:FormHeight  = 490.
   CarrierSortationBrowseForm:FormTitle   = fTL("Sortations for Carrier") + (IF AVAILABLE Carrier THEN " : " + Carrier.CarrierName ELSE "").
   CarrierSortationBrowseForm:FormType    = "xl_large".
   
   CarrierSortationBrowse = NEW browseTable("carriersortation_browse").
   CarrierSortationBrowse:BrowseWidth  = 680.
   CarrierSortationBrowse:BrowseHeight = 432.
   
   CarrierSortationBrowse:insertColumn(fTL("SortationID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CarrierSortation}
   
   CarrierSortationBrowse:insertColumn(fTL("Sortation Code"), 100, "CHARACTER", "left", FALSE).
   CarrierSortationBrowse:insertColumn(fTL("Sortation Name"), 110, "CHARACTER", "left", FALSE).
   CarrierSortationBrowse:insertColumn(fTL("Cut off Time"),    80, "INTEGER",           FALSE).
   CarrierSortationBrowse:insertColumn(fTL("PickUp Time"),     80, "INTEGER",           FALSE).
   CarrierSortationBrowse:insertColumn(fTL("ShipLanes"),       80, "INTEGER",           FALSE).
   CarrierSortationBrowse:insertColumn(fTL("Active"),          80, "CHARACTER",         FALSE).
   CarrierSortationBrowse:insertColumn("",                     35, "",                  FALSE).

   CarrierSortationBrowse:StartBody().
   
   IF AVAILABLE Carrier THEN
   DO:
      /*List the CarrierSortations for the Carrier*/
      FOR EACH CarrierSortation OF Carrier NO-LOCK:
         
         CarrierSortationBrowse:startRow(CarrierSortation.CarrierSortationID, "selectCarrierSortationRow(this," + '"' 
                         + STRING(CarrierSortation.CarrierSortationID) + '","adCarrierAdmin.p","carriersortation_browse_form"' + ");", "").
         CarrierSortationBrowse:insertData(CarrierSortation.CarrierSortationID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CarrierSortation}
         
         intPossibleShipLanes = 0.
         FOR EACH SortationShipLaneLink OF CarrierSortation NO-LOCK 
            WHERE SortationShipLaneLink.Active,
            EACH ShipLane OF SortationShipLaneLink NO-LOCK
            WHERE ShipLane.Active:
            
            intPossibleShipLanes = intPossibleShipLanes + 1.
         END.
         
         CarrierSortationBrowse:insertData(CarrierSortation.SortationCode, "left").
         CarrierSortationBrowse:insertData(CarrierSortation.SortationName, "left").
         CarrierSortationBrowse:insertData(CarrierSortation.CutOffTime, "right").
         CarrierSortationBrowse:insertData(CarrierSortation.PickUpTime, "right").
         CarrierSortationBrowse:insertData(intPossibleShipLanes, "right").
         CarrierSortationBrowse:insertData(STRING(CarrierSortation.Active,"Yes/No")).
         CarrierSortationBrowse:insertImage("view", "View Change History", 'viewCarrierSortationHistoryBrowse("' 
                                     + STRING(CarrierSortation.CarrierID) + '", "' + STRING(CarrierSortation.CarrierSortationID) + '", this.parentNode);').

         
         /* Add hidden fields */
         CarrierSortationBrowse:insertHiddenData("CarrierID",CarrierSortation.CarrierID).
         CarrierSortationBrowse:insertHiddenData("CarrierSortationID",CarrierSortation.CarrierSortationID).
         CarrierSortationBrowse:insertHiddenData("CarrierSortationVersionID",CarrierSortation.VersionID).
         CarrierSortationBrowse:insertHiddenData("CarrierSortationName",CarrierSortation.SortationName).
         CarrierSortationBrowse:endRow().
      
      END. /* FOR EACH CarrierSortation OF Carrier NO-LOCK, EACH CarrierSortation OF CarrierSortation NO-LOCK */
   END. /*IF AVAILABLE Carrier THEN*/
   
   CarrierSortationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierSortationBrowse:getErrors().
   
   CarrierSortationBrowseForm:insertHiddenField("CarrierID","").
   CarrierSortationBrowseForm:insertHiddenField("CarrierName","").
   CarrierSortationBrowseForm:insertHiddenField("CarrierSortationName","").
   CarrierSortationBrowseForm:insertHiddenField("CarrierSortationID","").
   CarrierSortationBrowseForm:insertHiddenField("CarrierSortationVersionID","").
   CarrierSortationBrowseForm:insertHiddenField("SortationShipLaneLinkID","").
   CarrierSortationBrowseForm:insertHiddenField("carrier_browse_scroll","").
   CarrierSortationBrowseForm:insertHiddenField("carriersortation_browse_scroll","").
   CarrierSortationBrowseForm:insertHiddenField("popup_carriersortation_browse","").
   CarrierSortationBrowseForm:insertHiddenField("popup_carriersortationhistory_browse","").
   CarrierSortationBrowseForm:insertHiddenField("popup_sortationshiplanelink_browse","").
   CarrierSortationBrowseForm:insertHiddenField("form_name","carriersortation_browse_form").
   CarrierSortationBrowseForm:insertHiddenField("prog_name","adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierSortationBrowseForm}
   
   /* Create Button Bar */
   CarrierSortationBrowseButtons = NEW buttonBar().
   
   CarrierSortationBrowseButtons:addButton("carriersortation_browse_form_btn_create",
                                           fTL("Create"),
                                           "createCarrierSortation('carriersortation_details_form');"). 
   
   CarrierSortationBrowseButtons:addButton("carriersortation_browse_form_btn_view",
                                           fTL("Details"),
                                           "viewCarrierSortationDetails('carriersortation_details_form');",
                                           (IF intSelectedCarrierSortation > 0 THEN "" ELSE "Disabled")).
   
   CarrierSortationBrowseButtons:addButton("carriersortation_browse_form_btn_shiplanes",
                                           fTL("ShipLanes"),
                                           "viewSortationShipLaneLinks('carriersortation_details_form');",
                                           (IF intSelectedCarrierSortation > 0 THEN "" ELSE "Disabled")).
   
   CarrierSortationBrowseButtons:addButton("carriersortation_browse_form_btn_delete",
                                           fTL("Delete"),
                                           "confirmDeleteCarrierSortation('carriersortation_browse_form');", 
                                           (IF intSelectedCarrierSortation > 0 THEN "" ELSE "Disabled")).
   
   CarrierSortationBrowseButtons:addButton("carriersortation_browse_form_btn_cancel",
                                            fTL("Cancel"),
                                            "disablePopup('carriersortation_browse_form_popup');").
   
   CarrierSortationBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierSortationBrowseForm:FormBrowse  = CarrierSortationBrowse.
   CarrierSortationBrowseForm:FormButtons = CarrierSortationBrowseButtons.
   CarrierSortationBrowseForm:endForm(). 
   
   CarrierSortationBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCarrierSortationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierSortationDetails Procedure 
PROCEDURE pCarrierSortationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "carriersortation_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CarrierSortationID,SortationName,SortationCode,CutOffTimeHour,CutOffTimeMins,"+
                                 "PickUpTimeHour,PickUpTimeMins,Active,PickUpTime,CutOffTime"
          chrEditFieldList     = "SortationName,CutOffTimeHour,CutOffTimeMins,PickUpTimeHour,PickUpTimeMins,Active"
          chrNewFieldList      = "SortationName,SortationCode,CutOffTimeHour,CutOffTimeMins,PickUpTimeHour,PickUpTimeMins,Active"
          chrRequiredFieldList = "SortationName,SortationCode,CutOffTimeHour,CutOffTimeMins,PickUpTimeHour,PickUpTimeMins"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   CarrierSortationDetailsForm = NEW dataForm("carriersortation_details_form").
   CarrierSortationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CarrierSortationDetailsForm:FormAction  = "dbCarrierSortationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CarrierSortationDetailsForm:FormWidth   = 460.
   CarrierSortationDetailsForm:FormHeight  = 300.
   CarrierSortationDetailsForm:FormTitle   = "Carrier Sortation Details".
   CarrierSortationDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   CarrierSortationDetailsForm:insertPaddingColumn(70).
   CarrierSortationDetailsForm:insertColumn(90).
   CarrierSortationDetailsForm:insertColumn(20).
   CarrierSortationDetailsForm:insertColumn(4).
   CarrierSortationDetailsForm:insertColumn(20).
   CarrierSortationDetailsForm:insertColumn(30).
   CarrierSortationDetailsForm:insertColumn(120).  
   
   /* Fields */
   CarrierSortationDetailsForm:startRow().
   CarrierSortationDetailsForm:insertLabel(fTL("Sortation ID")).
   CarrierSortationDetailsForm:insertTextField("CarrierSortationID", "", 110, TRUE).    
   
   CarrierSortationDetailsForm:startRow().
   CarrierSortationDetailsForm:insertLabel(fTL("Sortation Code")).
   CarrierSortationDetailsForm:insertTextField("SortationCode", "", 200, TRUE). 
   
   CarrierSortationDetailsForm:startRow().
   CarrierSortationDetailsForm:insertLabel(fTL("Sortation Name")).
   CarrierSortationDetailsForm:insertTextField("SortationName", "", 200, TRUE).  
   
   CarrierSortationDetailsForm:startRow().
   CarrierSortationDetailsForm:insertLabel(fTL("Cut Off Time")).
   CarrierSortationDetailsForm:insertTextField("CutOffTimeHour", "", 20, TRUE).
   CarrierSortationDetailsForm:insertLabel(":").
   CarrierSortationDetailsForm:insertTextField("CutOffTimeMins", "", 20, TRUE).  

   CarrierSortationDetailsForm:startRow().
   CarrierSortationDetailsForm:insertLabel(fTL("Pick Up Time")).
   CarrierSortationDetailsForm:insertTextField("PickUpTimeHour", "", 20, TRUE).
   CarrierSortationDetailsForm:insertLabel(":").
   CarrierSortationDetailsForm:insertTextField("PickUpTimeMins", "", 20, TRUE).  

   CarrierSortationDetailsForm:startRow().
   CarrierSortationDetailsForm:insertLabel(fTL("Active")). 
   CarrierSortationDetailsForm:insertComboField("Active", "", 110, TRUE).  
   CarrierSortationDetailsForm:insertComboPairs("Active", "yes", "Active").
   CarrierSortationDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pCarrierSortationDetailsFields}
   
   /* Add Hidden Fields*/
   CarrierSortationDetailsForm:insertHiddenField("carrier_browse_scroll", "").
   CarrierSortationDetailsForm:insertHiddenField("popup_carriersortation_browse", "").
   CarrierSortationDetailsForm:insertHiddenField("CarrierID",          STRING(intSelectedCarrier)).
   CarrierSortationDetailsForm:insertHiddenField("CarrierSortationID", STRING(intSelectedCarrierSortation)).
   CarrierSortationDetailsForm:insertHiddenField("form_name",          "carriersortation_details_form").
   CarrierSortationDetailsForm:insertHiddenField("prog_name",          "adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierSortationDetailsForm}
   
   /* Create Button Bar */
   CarrierSortationDetailsButtons = NEW buttonBar().
   
   CarrierSortationDetailsButtons:addButton("carriersortation_details_form_btn_save", 
                                            fTL("Save"), 
                                            "updateCarrierSortation('carriersortation_details_form');").
   
   CarrierSortationDetailsButtons:addButton("carriersortation_details_form_btn_cancel", 
                                            fTL("Cancel"), 
                                            "cancelUpdate('UserCancelled','process_mode'); " 
                                               + "disablePopup('carriersortation_details_form_popup');").
   CarrierSortationDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierSortationDetailsForm:FormButtons = CarrierSortationDetailsButtons.
   
   CarrierSortationDetailsForm:endForm(). 
   CarrierSortationDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + CarrierSortationDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCarrierSortationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierSortationDetailsFields Procedure 
PROCEDURE pCarrierSortationDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adCarrierAdmin_carriersortation_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCarrierSortationHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCarrierSortationHistoryBrowse Procedure
PROCEDURE pCarrierSortationHistoryBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   CarrierSortationHistoryBrowseForm           = NEW dataForm("carriersortationhistory_browse_form").
   CarrierSortationHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CarrierSortationHistoryBrowseForm:FormWidth  = 860.
   CarrierSortationHistoryBrowseForm:FormHeight = 530.
   CarrierSortationHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE CarrierSortation THEN " for Carrier Sortation: " 
                                          + STRING(CarrierSortation.SortationCode) + "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ID:" 
                                          + STRING(CarrierSortation.CarrierSortationID) ELSE "").
   CarrierSortationHistoryBrowseForm:FormType   = "xxl_large".
   CarrierSortationHistoryBrowse                = NEW browseTable("carriersortationhistory_browse").
   CarrierSortationHistoryBrowse:BrowseWidth    = 840.
   CarrierSortationHistoryBrowse:BrowseHeight   = 490.
   
   CarrierSortationHistoryBrowse:insertColumn(fTL("HistoryID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CarrierSortationHistory}
   
   CarrierSortationHistoryBrowse:insertColumn(fTL("Code"),         60, "CHARACTER", "left", FALSE).
   CarrierSortationHistoryBrowse:insertColumn(fTL("Name"),        130, "CHARACTER", "left", FALSE).
   CarrierSortationHistoryBrowse:insertColumn(fTL("CutOffTime"),  120, "CHARACTER", "left", FALSE).
   CarrierSortationHistoryBrowse:insertColumn(fTL("PickUpTime"),  120, "CHARACTER", "left", FALSE).
   CarrierSortationHistoryBrowse:insertColumn(fTL("Active"),       50, "CHARACTER", "left", FALSE).
   CarrierSortationHistoryBrowse:insertColumn(fTL("User"),         80, "CHARACTER", "left", FALSE).
   CarrierSortationHistoryBrowse:insertColumn(fTL("Operation"),    80, "CHARACTER", "left", FALSE).
   CarrierSortationHistoryBrowse:insertColumn(fTL("Created"),      90, "CHARACTER", "left", FALSE).
   
   CarrierSortationHistoryBrowse:StartBody().
   
   IF AVAILABLE CarrierSortation THEN
   DO:
      /*List the CarrierSortationHistorys for the CarrierSortation*/
      FOR EACH CarrierSortationHistory NO-LOCK
         WHERE CarrierSortationHistory.CarrierSortationID = CarrierSortation.CarrierSortationID
         BY CarrierSortationHistory.CarrierSortationHistoryID: /*idxCarrierSortationHistoryID*/
         
         FIND OperationType OF CarrierSortationHistory NO-LOCK NO-ERROR.
         FIND GateUser      OF CarrierSortationHistory NO-LOCK NO-ERROR.
         
         CarrierSortationHistoryBrowse:startRow(CarrierSortationHistory.CarrierSortationHistoryID, "selectBrowseRow(this," + '"carriersortationhistory_browse"' + ");","").
         CarrierSortationHistoryBrowse:insertData(CarrierSortationHistory.CarrierSortationHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i CarrierSortationHistory}
         
         CarrierSortationHistoryBrowse:insertData(CarrierSortationHistory.SortationCode, "left").
         CarrierSortationHistoryBrowse:insertData(CarrierSortationHistory.SortationName, "left").
         CarrierSortationHistoryBrowse:insertData(CarrierSortationHistory.CutOffTime, "left").
         CarrierSortationHistoryBrowse:insertData(CarrierSortationHistory.PickUpTime, "left").
         CarrierSortationHistoryBrowse:insertData(CarrierSortationHistory.Active, "left").
         CarrierSortationHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         CarrierSortationHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
         CarrierSortationHistoryBrowse:insertData(fDisplayDate&Time(CarrierSortationHistory.Created,"y/m/d H:M:S"), "right").
         
         CarrierSortationHistoryBrowse:endRow().
      
      END. /* FOR EACH CarrierSortationHistory OF Carrier NO-LOCK */
   END. /*IF AVAILABLE Carrier THEN*/
   
   CarrierSortationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CarrierSortationHistoryBrowse:getErrors().
   
   CarrierSortationHistoryBrowseForm:insertHiddenField("popup_carriersortationhistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CarrierSortationHistoryBrowseForm}
   
   /* Create Button Bar */
   CarrierSortationHistoryBrowseButtons = NEW buttonBar().
   
   CarrierSortationHistoryBrowseButtons:addButton("carriersortationhistory_browse_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('carriersortationhistory_browse_form_popup');").
   
   CarrierSortationHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CarrierSortationHistoryBrowseForm:FormBrowse  = CarrierSortationHistoryBrowse.
   CarrierSortationHistoryBrowseForm:FormButtons = CarrierSortationHistoryBrowseButtons.
   CarrierSortationHistoryBrowseForm:endForm(). 
   
   CarrierSortationHistoryBrowseForm:displayForm().


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
   
   ASSIGN chrCarrierID                        = get-value("CarrierID")
          intSelectedCarrier                  = INTEGER(chrCarrierID)
          chrScrollToCarrierRow               = STRING(INTEGER(get-value("carrier_browse_scroll"))) + ";"
          chrCarrierSortationID               = get-value("CarrierSortationID")
          intSelectedCarrierSortation         = INTEGER(chrCarrierSortationID)
          chrScrollToCarrierSortationRow      = STRING(INTEGER(get-value("carriersortation_browse_scroll"))) + ";"
          chrCarrierServiceID                 = get-value("CarrierServiceID")
          intSelectedCarrierService           = INTEGER(chrCarrierServiceID)
          chrCarrierModeID                    = get-value("CarrierModeID")
          intSelectedCarrierMode              = INTEGER(chrCarrierModeID)    
          chrCarrierDeliveryStatusID          = get-value("CarrierDeliveryStatusID")
          intSelectedCarrierDeliveryStatus    = INTEGER(chrCarrierDeliveryStatusID)
          chrScrollToCarrierServiceRow        = STRING(INTEGER(get-value("carrierservice_browse_srcoll"))) + ";"
          chrScrollToCarrierModeRow           = STRING(INTEGER(get-value("carriermode_browse_srcoll"))) + ";"
          chrScrollToCarrierDeliveryStatusRow = STRING(INTEGER(get-value("carrierdeliverystatus_browse_srcoll"))) + ";"
          chrSortationShipLaneLinkID          = get-value("SortationShipLaneLinkID")
          intSelectedSortationShipLaneLink    = INTEGER(chrSortationShipLaneLinkID).
   
   /* Process URL values */
   IF chrCarrierID <> "" THEN
      chrSelectCarrierRow = 'selectCarrierRow(document.getElementById("carrier_browse_row_' + chrCarrierID + '"),"' + chrCarrierID +  '");'.
   
   IF chrCarrierSortationID <> "" THEN
      chrSelectCarrierSortationRow = 'selectCarrierSortationRow(document.getElementById("carriersortation_browse_row_' 
                                        + chrCarrierSortationID + '"),"' + chrCarrierSortationID +  '");'.
   
   IF chrSortationShipLaneLinkID <> "" THEN
      chrSelectSortationShipLaneRow = 'selectSortationShipLaneLinkRow(document.getElementById("sortationshiplanelink_browse_row_' 
                                        + chrSortationShipLaneLinkID + '"),"' + chrSortationShipLaneLinkID +  '");'.

   IF chrCarrierServiceID <> "" THEN
      chrSelectCarrierServiceRow = 'selectCarrierServiceRow(document.getElementById("carrierservice_browse_row_' 
                                        + chrCarrierServiceID + '"),"' + chrCarrierServiceID +  '");'.                                       
   IF chrCarrierModeID <> "" THEN
      chrSelectCarrierModeRow = 'selectCarrierModeRow(document.getElementById("carriermode_browse_row_' 
                                   + chrCarrierModeID + '"),"' + chrCarrierModeID +  '");'.                                     
   
   IF chrCarrierDeliveryStatusID <> "" THEN
      chrSelectCarrierDeliveryStatusRow = 'selectCarrierDeliveryStatusRow(document.getElementById("carrierdeliverystatus_browse_row_' 
                                   + chrCarrierDeliveryStatusID + '"),"' + chrCarrierDeliveryStatusID +  '");'.
      
   IF get-value('popup_carriersortation_browse') = "yes" THEN 
      chrPopupCarrierSortations = 'enablePopup("carriersortation_browse_form_popup");'.
   
   IF get-value('popup_sortationshiplanelink_browse') = "yes" THEN 
      chrPopupSortationShipLaneLinks = 'enablePopup("sortationshiplanelink_browse_form_popup");'.

   IF get-value('popup_carrierservice_browse') = "yes" THEN 
      chrPopupCarrierSortations = 'enablePopup("carrierservice_browse_form_popup");'.
      
   IF get-value('popup_carriermode_browse') = "yes" THEN 
      chrPopupCarrierMode = 'enablePopup("carriermode_browse_form_popup");'.
      
   IF get-value('popup_carrierdeliverystatus_browse') = "yes" THEN 
      chrPopupCarrierDeliveryStatus = 'enablePopup("carrierdeliverystatus_browse_form_popup");'.   

   IF get-value('popup_carrierhistory_browse') = "yes" THEN
      chrPopupCarrierHistory = 'enablePopup("carrierhistory_browse_form_popup");'.
   
   IF get-value('popup_carriersortationhistory_browse') = "yes" THEN
      chrPopupCarrierSortationHistory = 'enablePopup("carriersortationhistory_browse_form_popup");'.

   IF get-value('popup_carrierservicehistory_browse') = "yes" THEN
      chrPopupCarrierServiceHistory = 'enablePopup("carrierservicehistory_browse_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("carrier_browse").scrollTop=' + chrScrollToCarrierRow + chrSelectCarrierRow 
                             + chrSelectCarrierSortationRow + chrPopupCarrierSortations + chrSelectSortationShipLaneRow 
                             + chrPopupSortationShipLaneLinks + chrSelectCarrierServiceRow + chrSelectCarrierModeRow
                             + chrPopupCarrierServices + chrPopupCarrierHistory
                             + chrPopupCarrierSortationHistory + chrPopupCarrierServiceHistory + chrPopupCarrierMode + chrPopupCarrierDeliveryStatus.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Carrier Admin".
   ThisPage:FrameTitle    = "Carrier Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("carrier.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Program Output/Logic Start ********/
   RUN pCarrierBrowse.
   
   FIND FIRST Carrier WHERE Carrier.CarrierID = intSelectedCarrier NO-LOCK NO-ERROR.
   
   /******* Procedures to build Popups ********/
   RUN pCarrierDetails.
   
   RUN pCarrierSortationBrowse.
   FIND FIRST CarrierSortation WHERE CarrierSortation.CarrierSortationID = intSelectedCarrierSortation NO-LOCK NO-ERROR.
   
   RUN pCarrierSortationDetails.
   
   RUN pCarrierServiceBrowse.
   FIND FIRST CarrierService WHERE CarrierService.CarrierServiceID = intSelectedCarrierService NO-LOCK NO-ERROR. 
   
   RUN pCarrierServiceDetails.
   
   RUN pSortationShipLaneBrowse. 
   RUN pSortationShipLaneDetails.
   
   RUN pCarrierHistoryBrowse.
   
   RUN pCarrierSortationHistoryBrowse.
   
   RUN pCarrierServiceHistoryBrowse.
   
   RUN pCarrierModeBrowse.
   FIND FIRST CarrierMode WHERE CarrierMode.CarrierModeID = intSelectedCarrierMode NO-LOCK NO-ERROR. 
   
   RUN pCarrierModeDetails.
   
   RUN pCarrierDeliveryStatusBrowse.
   FIND FIRST CarrierDeliveryStatus WHERE CarrierDeliveryStatus.CarrierDeliveryStatusID = intSelectedCarrierDeliveryStatus NO-LOCK NO-ERROR. 
   
   RUN pCarrierDeliveryStatusDetails.
   
      
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT CarrierBrowseFrame                            NO-ERROR.
   DELETE OBJECT CarrierBrowse                                 NO-ERROR.
   DELETE OBJECT CarrierBrowseButtons                          NO-ERROR.
   DELETE OBJECT CarrierBrowseMoreButtons                      NO-ERROR.
   DELETE OBJECT CarrierDetailsForm                            NO-ERROR.
   DELETE OBJECT CarrierDetailsButtons                         NO-ERROR.
   DELETE OBJECT CarrierSortationBrowseFrame                   NO-ERROR.
   DELETE OBJECT CarrierSortationBrowse                        NO-ERROR.
   DELETE OBJECT CarrierSortationBrowseButtons                 NO-ERROR.
   DELETE OBJECT CarrierSortationDetailsForm                   NO-ERROR.
   DELETE OBJECT CarrierSortationDetailsButtons                NO-ERROR.
   DELETE OBJECT CarrierSortationBrowseForm                    NO-ERROR.
   DELETE OBJECT SortationShipLaneLinkBrowseFrame              NO-ERROR.
   DELETE OBJECT SortationShipLaneLinkBrowse                   NO-ERROR.
   DELETE OBJECT SortationShipLaneLinkBrowseButtons            NO-ERROR.
   DELETE OBJECT SortationShipLaneLinkDetailsForm              NO-ERROR.
   DELETE OBJECT SortationShipLaneLinkDetailsButtons           NO-ERROR.
   DELETE OBJECT SortationShipLaneLinkBrowseForm               NO-ERROR.
   DELETE OBJECT CarrierServiceBrowseFrame                     NO-ERROR.
   DELETE OBJECT CarrierServiceBrowse                          NO-ERROR.
   DELETE OBJECT CarrierServiceBrowseButtons                   NO-ERROR.
   DELETE OBJECT CarrierServiceDetailsForm                     NO-ERROR.
   DELETE OBJECT CarrierServiceDetailsButtons                  NO-ERROR.
   DELETE OBJECT CarrierServiceBrowseForm                      NO-ERROR.
   DELETE OBJECT CarrierSortationHistoryBrowseFrame            NO-ERROR.
   DELETE OBJECT CarrierSortationHistoryBrowseForm             NO-ERROR.
   DELETE OBJECT CarrierSortationHistoryBrowse                 NO-ERROR.
   DELETE OBJECT CarrierSortationHistoryBrowseButtons          NO-ERROR.
   DELETE OBJECT CarrierSortationHistoryDetailsForm            NO-ERROR.
   DELETE OBJECT CarrierSortationHistoryDetailsButtons         NO-ERROR.
   DELETE OBJECT CarrierServiceHistoryBrowseFrame              NO-ERROR.
   DELETE OBJECT CarrierServiceHistoryBrowseForm               NO-ERROR.
   DELETE OBJECT CarrierServiceHistoryBrowse                   NO-ERROR.
   DELETE OBJECT CarrierServiceHistoryBrowseButtons            NO-ERROR.
   DELETE OBJECT CarrierServiceHistoryDetailsForm              NO-ERROR.
   DELETE OBJECT CarrierServiceHistoryDetailsButtons           NO-ERROR.
   DELETE OBJECT CarrierHistoryBrowseFrame                     NO-ERROR.
   DELETE OBJECT CarrierHistoryBrowseForm                      NO-ERROR.
   DELETE OBJECT CarrierHistoryBrowse                          NO-ERROR.
   DELETE OBJECT CarrierHistoryBrowseButtons                   NO-ERROR.
   DELETE OBJECT CarrierHistoryDetailsForm                     NO-ERROR.
   DELETE OBJECT CarrierHistoryDetailsButtons                  NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSortationShipLaneBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSortationShipLaneBrowse Procedure 
PROCEDURE pSortationShipLaneBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "sortationshiplanelink_details_form"}
   
   SortationShipLaneLinkBrowseForm = NEW dataForm("sortationshiplanelink_browse_form").
   SortationShipLaneLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   SortationShipLaneLinkBrowseForm:FormAction  = "dbSortationShipLaneLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   SortationShipLaneLinkBrowseForm:FormWidth   = 580.
   SortationShipLaneLinkBrowseForm:FormHeight  = 420.
   SortationShipLaneLinkBrowseForm:FormTitle   = fTL("ShipLanes for Sortation") 
                                              + (IF AVAILABLE CarrierSortation THEN " : " + STRING(CarrierSortation.SortationName) ELSE "").
   SortationShipLaneLinkBrowseForm:FormType    = "large".
   
   SortationShipLaneLinkBrowse = NEW browseTable("sortationshiplanelink_browse").
   SortationShipLaneLinkBrowse:BrowseWidth  = 560.
   SortationShipLaneLinkBrowse:BrowseHeight = 375.
   
   SortationShipLaneLinkBrowse:insertColumn(fTL("ID"), 100, "CHARACTER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i SortationShipLaneLink}
   
   SortationShipLaneLinkBrowse:insertColumn(fTL("ShipLane"), 110, "CHARACTER", "LEFT", FALSE).
   SortationShipLaneLinkBrowse:insertColumn(fTL("Location"), 100, "CHARACTER", "LEFT", FALSE).
   SortationShipLaneLinkBrowse:insertColumn(fTL("Priority"), 70, "INTEGER", "right", FALSE).
   SortationShipLaneLinkBrowse:insertColumn(fTL("Active"), 100, "LOGICAL", FALSE).
   
   SortationShipLaneLinkBrowse:StartBody().
   
   IF AVAILABLE CarrierSortation THEN
   DO:
      /*List the CarrierSortation Lines for the CarrierSortation*/
      FOR EACH SortationShipLaneLink OF CarrierSortation NO-LOCK,
         EACH  ShipLane OF SortationShipLaneLink NO-LOCK
         BY    ShipLane.ListingSequence:
         
         FIND FIRST Location OF ShipLane NO-LOCK NO-ERROR.
         
         SortationShipLaneLinkBrowse:startRow(SortationShipLaneLink.SortationShipLaneLinkID, 
                                                     "selectSortationShipLaneLinkRow(this," + '"' 
                                                     + STRING(SortationShipLaneLink.SortationShipLaneLinkID) + '"' + ");","").
         
         SortationShipLaneLinkBrowse:insertData(TRIM(STRING(SortationShipLaneLink.SortationShipLaneLinkID)), "left").
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i SortationShipLaneLink}
         
         SortationShipLaneLinkBrowse:insertData(ShipLane.LaneName, "left").
         SortationShipLaneLinkBrowse:insertData((IF AVAILABLE Location THEN Location.LocationRef ELSE "No Location Assigned"),"left").
         SortationShipLaneLinkBrowse:insertData(STRING(SortationShipLaneLink.Priority)).
         SortationShipLaneLinkBrowse:insertData(STRING(SortationShipLaneLink.Active)).
         
         /* Add hidden fields */
         SortationShipLaneLinkBrowse:insertHiddenData("CarrierSortationID", CarrierSortation.CarrierSortationID).
         SortationShipLaneLinkBrowse:insertHiddenData("CarrierSortationVersionID", CarrierSortation.VersionID).
         SortationShipLaneLinkBrowse:insertHiddenData("SortationShipLaneLinkID", 
                                                              SortationShipLaneLink.SortationShipLaneLinkID).
         SortationShipLaneLinkBrowse:insertHiddenData("SortationShipLaneLinkVersionID", 
                                                              SortationShipLaneLink.VersionID).
         
         SortationShipLaneLinkBrowse:endRow().
      
      END. /*  FOR EACH SortationShipLaneLink OF CarrierSortation NO-LOCK */
   END. /*IF AVAILABLE CarrierSortation THEN*/
   
   SortationShipLaneLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + SortationShipLaneLinkBrowse:getErrors().
   
   SortationShipLaneLinkBrowseForm:insertHiddenField("CarrierID",STRING(intSelectedCarrier)).
   SortationShipLaneLinkBrowseForm:insertHiddenField("CarrierSortationID",STRING(intSelectedCarrierSortation)).
   SortationShipLaneLinkBrowseForm:insertHiddenField("CarrierSortationVersionID","").
   SortationShipLaneLinkBrowseForm:insertHiddenField("SortationShipLaneLinkID","").
   SortationShipLaneLinkBrowseForm:insertHiddenField("SortationShipLaneLinkVersionID","").
   SortationShipLaneLinkBrowseForm:insertHiddenField("part_browse_scroll","").
   SortationShipLaneLinkBrowseForm:insertHiddenField("popup_carriersortation_browse","").
   SortationShipLaneLinkBrowseForm:insertHiddenField("popup_sortationshiplanelink_browse","").
   SortationShipLaneLinkBrowseForm:insertHiddenField("form_name","sortationshiplanelink_details_form").
   SortationShipLaneLinkBrowseForm:insertHiddenField("prog_name","adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i SortationShipLaneLinkBrowseForm}
   
   /* Create Button Bar */
   SortationShipLaneLinkBrowseButtons = NEW buttonBar().
   
   SortationShipLaneLinkBrowseButtons:addButton("sortationshiplanelink_browse_form_btn_create",
                                                fTL("Create"),
                                                "createSortationShipLaneLink('sortationshiplanelink_details_form');"). 
   
   SortationShipLaneLinkBrowseButtons:addButton("sortationshiplanelink_browse_form_btn_view",
                                                fTL("View"),
                                                "viewSortationShipLaneLink('sortationshiplanelink_details_form');",
                                                (IF intSelectedSortationShipLaneLink > 0 THEN "" ELSE "Disabled")).
   
   SortationShipLaneLinkBrowseButtons:addButton("sortationshiplanelink_browse_form_btn_delete",
                                                fTL("Delete"),
                                                "confirmDeleteSortationShipLaneLink('sortationshiplanelink_browse_form');", 
                                                (IF intSelectedSortationShipLaneLink > 0 THEN "" ELSE "Disabled")).
   
   SortationShipLaneLinkBrowseButtons:addButton("sortationshiplanelink_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('sortationshiplanelink_browse_form_popup');").
   
   SortationShipLaneLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   SortationShipLaneLinkBrowseForm:FormBrowse  = SortationShipLaneLinkBrowse.
   SortationShipLaneLinkBrowseForm:FormButtons = SortationShipLaneLinkBrowseButtons.
   
   SortationShipLaneLinkBrowseForm:endForm(). 
   
   SortationShipLaneLinkBrowseForm:displayForm().   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSortationShipLaneDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSortationShipLaneDetails Procedure 
PROCEDURE pSortationShipLaneDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "sortationshiplanelink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "SortationShipLaneLinkID,CarrierSortationID,ShipLaneID,CarrierID,Priority,Active"
          chrEditFieldList     = "Priority,Active"
          chrNewFieldList      = "ShipLaneID,Priority,Active"
          chrRequiredFieldList = "ShipLaneID,Priority"
          chrExtraFieldList    = ""
          chrValidateFieldList = "Priority:INTEGER>0".
   
   SortationShipLaneLinkDetailsForm = NEW dataForm("sortationshiplanelink_details_form").
   SortationShipLaneLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   SortationShipLaneLinkDetailsForm:FormAction  = "dbSortationShipLaneLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   SortationShipLaneLinkDetailsForm:FormWidth   = 460.
   SortationShipLaneLinkDetailsForm:FormHeight  = 300.
   SortationShipLaneLinkDetailsForm:FormTitle   = "Sortation - ShipLane Link Details".
   SortationShipLaneLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   SortationShipLaneLinkDetailsForm:insertPaddingColumn(20).
   SortationShipLaneLinkDetailsForm:insertColumn(120).
   SortationShipLaneLinkDetailsForm:insertColumn(120).
   SortationShipLaneLinkDetailsForm:insertColumn(30).
   SortationShipLaneLinkDetailsForm:insertColumn(90).  
   
   /* Fields */
   SortationShipLaneLinkDetailsForm:startRow().
   SortationShipLaneLinkDetailsForm:insertLabel(fTL("LinkID")).
   SortationShipLaneLinkDetailsForm:insertTextField("SortationShipLaneLinkID", "", 110, TRUE).  
   
   SortationShipLaneLinkDetailsForm:startRow().
   SortationShipLaneLinkDetailsForm:insertLabel(fTL("Sortation ID")).
   SortationShipLaneLinkDetailsForm:insertTextField("CarrierSortationID", "0", 110, TRUE).  
   
   SortationShipLaneLinkDetailsForm:startRow().
   SortationShipLaneLinkDetailsForm:insertLabel(fTL("ShipLane")).
   SortationShipLaneLinkDetailsForm:insertComboField("ShipLaneID", "", 110, TRUE).  
   FOR EACH ShipLane NO-LOCK /*idx=ActiveListingSequence*/
      WHERE ShipLane.CarrierID = intSelectedCarrier
      AND   ShipLane.Active
      BY    ShipLane.ListingSequence:
      
      logShipLanesAvailable = TRUE.
      SortationShipLaneLinkDetailsForm:insertComboPairs("ShipLaneID", STRING(ShipLane.ShipLaneID), ShipLane.LaneName).
   END.   
   
   IF NOT logShipLanesAvailable THEN
      SortationShipLaneLinkDetailsForm:insertComboPairs("ShipLaneID", "", "No Active ShipLanes for this Carrier").
   
   SortationShipLaneLinkDetailsForm:startRow().
   SortationShipLaneLinkDetailsForm:insertLabel(fTL("Priority")).
   SortationShipLaneLinkDetailsForm:insertTextField("Priority", "", 110, TRUE).  
   
   SortationShipLaneLinkDetailsForm:startRow().
   SortationShipLaneLinkDetailsForm:insertLabel(fTL("Active")). 
   SortationShipLaneLinkDetailsForm:insertComboField("Active", "", 110, TRUE).  
   SortationShipLaneLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   SortationShipLaneLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalFormFields.i pSortationShipLaneLinkDetailsFields}
   
   /* Add Hidden Fields*/
   SortationShipLaneLinkDetailsForm:insertHiddenField("carrier_browse_scroll","").
   SortationShipLaneLinkDetailsForm:insertHiddenField("carriersortation_browse_scroll","").
   SortationShipLaneLinkDetailsForm:insertHiddenField("popup_sortationshiplanelink_browse", "").
   SortationShipLaneLinkDetailsForm:insertHiddenField("popup_carriersortation_browse", "").
   SortationShipLaneLinkDetailsForm:insertHiddenField("CarrierID", STRING(intSelectedCarrier)).
   SortationShipLaneLinkDetailsForm:insertHiddenField("CarrierSortationID", STRING(intSelectedCarrierSortation)).
   SortationShipLaneLinkDetailsForm:insertHiddenField("SortationShipLaneLinkID", STRING(intSelectedSortationShipLaneLink)).
   SortationShipLaneLinkDetailsForm:insertHiddenField("form_name","sortationshiplanelink_details_form").
   SortationShipLaneLinkDetailsForm:insertHiddenField("prog_name","adCarrierAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i SortationShipLaneLinkDetailsForm}
   
   /* Create Button Bar */
   SortationShipLaneLinkDetailsButtons = NEW buttonBar().
   
   SortationShipLaneLinkDetailsButtons:addButton("sortationshiplanelink_details_form_btn_save", 
                                                 fTL("Save"), 
                                                 "updateSortationShipLaneLink('sortationshiplanelink_details_form');").
   
   SortationShipLaneLinkDetailsButtons:addButton("sortationshiplanelink_details_form_btn_cancel", 
                                                 fTL("Cancel"), 
                                                 "cancelUpdate('UserCancelled','process_mode'); 
                                                  disablePopup('sortationshiplanelink_details_form_popup');").
   
   SortationShipLaneLinkDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   SortationShipLaneLinkDetailsForm:FormButtons = SortationShipLaneLinkDetailsButtons.
   
   SortationShipLaneLinkDetailsForm:endForm(). 
   
   SortationShipLaneLinkDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + SortationShipLaneLinkDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSortationShipLaneDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSortationShipLaneDetailsFields Procedure 
PROCEDURE pSortationShipLaneDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      /* This will be held in customer specific code repository */
      {adCarrierAdmin_sortationshiplanelink_details_form.i}
     
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

