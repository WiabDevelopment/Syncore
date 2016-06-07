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

/* Get System Options */
{getAdminOptions.i}  

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedPrinter                AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedPrinterType            AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedPrinterLocationLink    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectPrinterRow               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectPrinterLocationLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToPrinterRow             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToPrinterLocationLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPrinterID                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPrinterTypeID                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPrinterLocationLinkID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupPrinterLocationLinks      AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE PrinterBrowseFrame                AS pageFrame.
DEFINE VARIABLE PrinterBrowse                     AS browseTable.
DEFINE VARIABLE PrinterBrowseButtons              AS buttonBar.
DEFINE VARIABLE PrinterDetailsForm                AS dataForm.
DEFINE VARIABLE PrinterDetailsButtons             AS buttonBar.
DEFINE VARIABLE PrinterLocationLinkBrowseFrame    AS pageFrame.
DEFINE VARIABLE PrinterLocationLinkBrowse         AS browseTable.
DEFINE VARIABLE PrinterLocationLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE PrinterLocationLinkDetailsForm    AS dataForm.
DEFINE VARIABLE PrinterLocationLinkDetailsButtons AS buttonBar.
DEFINE VARIABLE PrinterLocationLinkBrowseForm     AS dataForm.
DEFINE VARIABLE LocationLookupBrowseForm          AS dataForm.
DEFINE VARIABLE LocationLookupBrowse              AS browseTable.
DEFINE VARIABLE LocationLookupBrowseButtons       AS buttonBar.

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
         HEIGHT             = 12.67
         WIDTH              = 57.8.
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

&IF DEFINED(EXCLUDE-pLocationLookup) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocationLookup Procedure 
PROCEDURE pLocationLookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   LocationLookupBrowseForm = NEW dataForm("location_lookup_browse_form").
   LocationLookupBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   LocationLookupBrowseForm:FormAction = "".
   
   /* Setup */
   LocationLookupBrowseForm:FormWidth   = 580.
   LocationLookupBrowseForm:FormHeight  = 420.
   LocationLookupBrowseForm:FormTitle   = fTL("Search for Locations").
   LocationLookupBrowseForm:FormType    = "large".
   
   LocationLookupBrowse = NEW browseTable("location_lookup_browse").    
   LocationLookupBrowse:BrowseWidth  = 560.
   LocationLookupBrowse:BrowseHeight = 375.
   
  
   LocationLookupBrowse:insertColumn(fTL("Location ID"), 120, "CHARACTER", "left", FALSE).
   LocationLookupBrowse:insertColumn(fTL("Location Ref"), 180, "CHARACTER", "left", FALSE).
   LocationLookupBrowse:insertColumn(fTL("Location Type"), 120, "INTEGER", FALSE).

   /*Body*/
   LocationLookupBrowse:startBody().
   
   LocationLookupBrowse:endTable("DontFill").
    

   LocationLookupBrowseForm:insertHiddenField("LocationID","").
   LocationLookupBrowseForm:insertHiddenField("LocationRef","").
   LocationLookupBrowseForm:insertHiddenField("TypeCode","").

   /* Create Button Bar */
   LocationLookupBrowseButtons = NEW buttonBar().
   
   LocationLookupBrowseButtons:addButton("location_lookup_browse_form_btn_select",
                                         fTL("Select"),
                                         "locationLookupSubmit('adPrinterAdmin.p','');",
                                         "Disabled").
   
   LocationLookupBrowseButtons:addButton("location_lookup_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('location_lookup_browse_form_popup');").
   
   LocationLookupBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LocationLookupBrowseForm:FormBrowse  = LocationLookupBrowse.
   LocationLookupBrowseForm:FormButtons = LocationLookupBrowseButtons.
   
   LocationLookupBrowseForm:endForm(). 
   
   LocationLookupBrowseForm:displayForm().   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPrinterBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterBrowse Procedure 
PROCEDURE pPrinterBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "printer_details_form"}
   
   PrinterBrowse = NEW browseTable("printer_browse").
   PrinterBrowse:BrowseWidth  = 965.
   PrinterBrowse:BrowseHeight = 455.
   PrinterBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   PrinterBrowse:insertColumn(fTL("Printer ID"), 85, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Printer }
   
   PrinterBrowse:insertColumn(fTL("Network Name"),  120, "CHARACTER", "left", FALSE).
   PrinterBrowse:insertColumn(fTL("Remote Name"),   120, "CHARACTER", "left", FALSE).
   PrinterBrowse:insertColumn(fTL("Printer Name"),  200, "CHARACTER", "left", FALSE).
   PrinterBrowse:insertColumn(fTL("Printing Mode"), 100, "CHARACTER", "left", FALSE).
   PrinterBrowse:insertColumn(fTL("Printer Type"),  100, "CHARACTER", "left", FALSE).
   PrinterBrowse:insertColumn(fTL("Active"),         70, "LOGICAL",           FALSE).
   
   /*Body*/
   PrinterBrowse:startBody().
   
   FOR EACH Printer NO-LOCK 
      BY Printer.PrinterName:
      
      FIND FIRST PrinterType OF Printer NO-LOCK NO-ERROR.
      
      PrinterBrowse:startRow(Printer.PrinterID, "selectPrinterRow(this," + '"' + STRING(Printer.PrinterID) + '"' + ");", "").
      PrinterBrowse:insertData(Printer.PrinterID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Printer}
      
      PrinterBrowse:insertData(Printer.OSNetworkPrinterName, "left").
      PrinterBrowse:insertData(Printer.OSRemotePrinterName, "left").
      PrinterBrowse:insertData(Printer.PrinterName, "left").
      PrinterBrowse:insertData(Printer.PrintingMode, "left").
      PrinterBrowse:insertData((IF AVAILABLE PrinterType THEN STRING(PrinterType.TypeCode) ELSE "No Printer Type Assigned"),"left").
      PrinterBrowse:insertData(STRING(Printer.Active,"Yes/No")).
      
      /* Add hidden fields */
      PrinterBrowse:insertHiddenData("PrinterVersionID",Printer.VersionID).
      PrinterBrowse:insertHiddenData("PrinterTypeCode",PrinterType.TypeCode).

      intRecordCount = intRecordCount + 1.

      PrinterBrowse:endRow().
      
   END. /*FOR EACH Printer NO-LOCK */
   
   PrinterBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PrinterBrowse:getErrors().
   
   /* Create a new frame */
   PrinterBrowseFrame = NEW pageFrame().
   PrinterBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   PrinterBrowseFrame:FormAction="dbPrinterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   PrinterBrowseFrame:formOpen("printer_browse_form").
   
   /* Start the Frame Header */
   PrinterBrowseFrame:insertSpacer(5).
   PrinterBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   PrinterBrowse:displayBrowse().  
   
   /* End the Frame Header */
   PrinterBrowseFrame:frameClose().
   PrinterBrowseFrame:insertSpacer(10).
   
   PrinterBrowseFrame:insertHiddenField("printer_browse_scroll","").
   PrinterBrowseFrame:insertHiddenField("PrinterID","").
   PrinterBrowseFrame:insertHiddenField("PrinterVersionID","").
   PrinterBrowseFrame:insertHiddenField("PrinterTypeCode","").
   PrinterBrowseFrame:insertHiddenField("printerlocationlink_browse_scroll","").
   PrinterBrowseFrame:insertHiddenField("popup_printerlocationlink_browse","").
   PrinterBrowseFrame:insertHiddenField("PrinterLocationLinkID","").
   PrinterBrowseFrame:insertHiddenField("form_name","printer_browse_form").
   PrinterBrowseFrame:insertHiddenField("prog_name","adPrinterAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PrinterBrowseFrame}

   PrinterBrowseFrame:formClose().
   
   /* Create Button Bar */
   PrinterBrowseButtons = NEW buttonBar().
   PrinterBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   PrinterBrowseButtons:addButton("printer_browse_form_btn_details",
                                  fTL("Details"),
                                  "viewPrinterDetails('printer_details_form');",
                                  (IF intSelectedPrinter > 0 THEN "" ELSE "Disabled")).
   
   PrinterBrowseButtons:addButton("printer_browse_form_btn_locations",
                                  fTL("Locations"),
                                  "viewPrinterLocationLinks('printer_details_form');",
                                  (IF intSelectedPrinter > 0 THEN "" ELSE "Disabled")).
   
   PrinterBrowseButtons:addButton("printer_browse_form_btn_test",
                                  fTL("Test Page"),
                                  "testPrinter('printer_browse_form');",
                                  (IF intSelectedPrinter > 0 THEN "" ELSE "Disabled")).

   PrinterBrowseButtons:addButton("printer_browse_form_btn_cancel",
                                  fTL("Clear Queue"),
                                  "cancelPrintQueue('printer_browse_form');",
                                  (IF intSelectedPrinter > 0 THEN "" ELSE "Disabled")).
   
   PrinterBrowseButtons:addButton("printer_browse_form_btn_create",
                                  fTL("Create"),
                                  "createPrinter('printer_details_form');",
                                  "").
   
   PrinterBrowseButtons:addButton("printer_browse_form_btn_delete",
                                  fTL("Delete"),
                                  "confirmDeletePrinter();",
                                  (IF intSelectedPrinter > 0 THEN "" ELSE "Disabled")).

   
   PrinterBrowseButtons:closeBar().  
   PrinterBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPrinterDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterDetails Procedure 
PROCEDURE pPrinterDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "printer_details_form"}
   
   ASSIGN chrDisplayFieldList  = "PrinterID,OSNetworkPrinterName,OSRemotePrinterName,PrinterName,PrintingMode,PrinterTypeID,Active"
          chrEditFieldList     = "OSNetworkPrinterName,OSRemotePrinterName,PrintingMode,PrinterTypeID,Active"
          chrNewFieldList      = "OSNetworkPrinterName,OSRemotePrinterName,PrinterName,PrintingMode,PrinterTypeID,Active"
          chrRequiredFieldList = "OSNetworkPrinterName,PrinterName,PrintingMode"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PrinterDetailsForm = NEW dataForm("printer_details_form").
   PrinterDetailsForm:WebStream = STREAM WebStream:HANDLE.
   /*PrinterDetailsForm:ShowLock = TRUE.*/
   
   PrinterDetailsForm:FormAction  = "dbPrinterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PrinterDetailsForm:FormWidth   = 460.
   PrinterDetailsForm:FormHeight  = 300.
   PrinterDetailsForm:FormTitle   = "Printer Details".
   PrinterDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PrinterDetailsForm:insertPaddingColumn(20).
   PrinterDetailsForm:insertColumn(100).
   PrinterDetailsForm:insertColumn(120).
   PrinterDetailsForm:insertColumn(20).
   PrinterDetailsForm:insertColumn(50).
   PrinterDetailsForm:insertColumn(110).
   
   /* Fields */
   PrinterDetailsForm:startRow().
   PrinterDetailsForm:insertLabel("Printer ID").
   PrinterDetailsForm:insertTextField("PrinterID", "", 110, TRUE).  
   
   PrinterDetailsForm:startRow().
   PrinterDetailsForm:insertLabel("Network Name").
   PrinterDetailsForm:insertTextField("OSNetworkPrinterName", "", 180, TRUE).  
   
   PrinterDetailsForm:startRow().
   PrinterDetailsForm:insertLabel("Remote Name").
   PrinterDetailsForm:insertTextField("OSRemotePrinterName", "", 180, TRUE).  
   
   PrinterDetailsForm:startRow().
   PrinterDetailsForm:insertLabel("Printer Name").
   PrinterDetailsForm:insertTextField("PrinterName", "", 280, TRUE).
      
   PrinterDetailsForm:startRow().
   PrinterDetailsForm:insertLabel("Printing Mode").
   PrinterDetailsForm:insertComboField("PrintingMode", "", 200, TRUE).  
   PrinterDetailsForm:insertComboPairs("PrintingMode", "Network", "Network").
   PrinterDetailsForm:insertComboPairs("PrintingMode", "Remote", "Remote").
   
   PrinterDetailsForm:startRow().
   PrinterDetailsForm:insertLabel("Printer Type").
   PrinterDetailsForm:insertComboField("PrinterTypeID", "", 180, TRUE).  
   FOR EACH PrinterType NO-LOCK /*idx=ActiveListingSequence*/
      WHERE PrinterType.Active:
      
      PrinterDetailsForm:insertComboPairs("PrinterTypeID", STRING(PrinterType.PrinterTypeID) , PrinterType.TypeCode).
   END.   

   PrinterDetailsForm:startRow().
   PrinterDetailsForm:insertLabel(fTL("Active")). 
   PrinterDetailsForm:insertComboField("Active", "", 110, TRUE).  
   PrinterDetailsForm:insertComboPairs("Active", "yes", "Active").
   PrinterDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pPrinterDetailsFields}
   
   /* Add Hidden Fields*/
   PrinterDetailsForm:insertHiddenField("printer_browse_scroll", "").
   PrinterDetailsForm:insertHiddenField("form_name", "printer_details_form").
   PrinterDetailsForm:insertHiddenField("prog_name", "adPrinterAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i PrinterDetailsForm}

   
   /* Create Button Bar */
   PrinterDetailsButtons = NEW buttonBar().
   
   PrinterDetailsButtons:addButton("printer_details_form_btn_save", 
                                   fTL("Save"), 
                                   "updatePrinter('printer_details_form');").
   
   PrinterDetailsButtons:addButton("printer_details_form_btn_cancel", 
                                   fTL("Cancel"), 
                                   "cancelUpdate('UserCancelled','process_mode'); disablePopup('printer_details_form_popup');").
   PrinterDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   PrinterDetailsForm:FormButtons = PrinterDetailsButtons.
   
   PrinterDetailsForm:endForm(). 
   
   PrinterDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + PrinterDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPrinterDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrinterDetailsFields Procedure 
PROCEDURE pPrinterDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         PrinterDetailsForm:startRow().
         PrinterDetailsForm:insertLabel(fTL("Field Label")).
         PrinterDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adPrinterAdmin_printer_details_form.i}
      
   END CASE. /*chrOption:*/
   
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
   
   PrinterLocationLinkBrowseForm:FormAction  = "dbPrinterLocationLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PrinterLocationLinkBrowseForm:FormWidth   = 700.
   PrinterLocationLinkBrowseForm:FormHeight  = 490.
   PrinterLocationLinkBrowseForm:FormTitle   = fTL("Locations for Printer") + 
                                               (IF AVAILABLE Printer THEN " : " + Printer.PrinterName ELSE "").
   PrinterLocationLinkBrowseForm:FormType    = "xl_large".
   
   PrinterLocationLinkBrowse = NEW browseTable("printerlocationlink_browse").
   PrinterLocationLinkBrowse:BrowseWidth  = 680.
   PrinterLocationLinkBrowse:BrowseHeight = 432.
   
   PrinterLocationLinkBrowse:insertColumn(fTL("LinkID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i PrinterLocationLink}
   
   PrinterLocationLinkBrowse:insertColumn(fTL("Location Ref"), 120, "CHARACTER", "left", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("Location Type"), 140, "CHARACTER", "left", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("Listing Sequence"), 80, "INTEGER", FALSE).
   PrinterLocationLinkBrowse:insertColumn(fTL("Active"), 80, "CHARACTER", FALSE).
   
   PrinterLocationLinkBrowse:StartBody().
   
   IF AVAILABLE Printer THEN
   DO:
      /* List the PrinterLocationLinks for the Printer */
      FOR EACH PrinterLocationLink OF Printer NO-LOCK
         BY PrinterLocationLink.ListingSequence
         BY PrinterLocationLink.PrinterID:
         
         FIND FIRST Location OF PrinterLocationLink NO-LOCK NO-ERROR.

         IF AVAILABLE Location THEN
            FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.

         PrinterLocationLinkBrowse:startRow(PrinterLocationLink.PrinterLocationLinkID, 
                                            "selectPrinterLocationLinkRow(this," + '"' + 
                                            STRING(PrinterLocationLink.PrinterLocationLinkID) + 
                                            '","adPrinterAdmin.p","printerlocationlink_browse_form"' + ");", "").
         PrinterLocationLinkBrowse:insertData(PrinterLocationLink.PrinterLocationLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i PrinterLocationLink}
         
         PrinterLocationLinkBrowse:insertData(IF AVAILABLE Location THEN Location.LocationRef ELSE "No Location Assigned", "left").
         PrinterLocationLinkBrowse:insertData(IF AVAILABLE LocationType THEN 
                                                 LocationType.TypeName 
                                              ELSE 
                                                 "No LocationType Assigned", "left").
         PrinterLocationLinkBrowse:insertData(STRING(PrinterLocationLink.ListingSequence), "right").
         PrinterLocationLinkBrowse:insertData(STRING(PrinterLocationLink.Active,"Yes/No")).
         
         /* Add hidden fields */
         PrinterLocationLinkBrowse:insertHiddenData("PrinterID",PrinterLocationLink.PrinterID).
         PrinterLocationLinkBrowse:insertHiddenData("LocationID",(IF AVAILABLE Location THEN STRING(Location.LocationID) ELSE "")).
         PrinterLocationLinkBrowse:insertHiddenData("LocationRef",(IF AVAILABLE Location THEN Location.LocationRef ELSE "")).
         PrinterLocationLinkBrowse:insertHiddenData("LocationVersionID",(IF AVAILABLE Location THEN STRING(Location.VersionID) ELSE "")).
         PrinterLocationLinkBrowse:insertHiddenData("PrinterLocationLinkID",PrinterLocationLink.PrinterLocationLinkID).
         PrinterLocationLinkBrowse:insertHiddenData("PrinterLocationLinkVersionID",PrinterLocationLink.VersionID).
         PrinterLocationLinkBrowse:endRow().
      
      END. /* FOR EACH PrinterLocationLink OF Printer NO-LOCK */
   END. /*IF AVAILABLE Printer THEN*/
   
   PrinterLocationLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + PrinterLocationLinkBrowse:getErrors().
   
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterName","").
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationVersionID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("LocationRef","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterLocationLinkID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("PrinterLocationLinkVersionID","").
   PrinterLocationLinkBrowseForm:insertHiddenField("printer_browse_scroll","").
   PrinterLocationLinkBrowseForm:insertHiddenField("printerlocationlink_browse_scroll","").
   PrinterLocationLinkBrowseForm:insertHiddenField("popup_printerlocationlink_browse","").
   PrinterLocationLinkBrowseForm:insertHiddenField("form_name","printerlocationlink_browse_form").
   PrinterLocationLinkBrowseForm:insertHiddenField("prog_name","adPrinterAdmin.p").
   
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
   
   ASSIGN chrDisplayFieldList  = "PrinterLocationLinkID,LocationRef,ListingSequence,Active"
          chrEditFieldList     = "ListingSequence,Active"
          chrNewFieldList      = "LocationRef,ListingSequence,Active"
          chrRequiredFieldList = "LocationRef"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   PrinterLocationLinkDetailsForm = NEW dataForm("printerlocationlink_details_form").
   PrinterLocationLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   PrinterLocationLinkDetailsForm:FormAction  = "dbPrinterLocationLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   PrinterLocationLinkDetailsForm:FormWidth   = 460.
   PrinterLocationLinkDetailsForm:FormHeight  = 300.
   PrinterLocationLinkDetailsForm:FormTitle   = "Printer Location Link Details".
   PrinterLocationLinkDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   PrinterLocationLinkDetailsForm:insertPaddingColumn(70).
   PrinterLocationLinkDetailsForm:insertColumn(120).
   PrinterLocationLinkDetailsForm:insertColumn(120).
   PrinterLocationLinkDetailsForm:insertColumn(30).
   PrinterLocationLinkDetailsForm:insertColumn(120).  
   
   /* Fields */
   PrinterLocationLinkDetailsForm:startRow().
   PrinterLocationLinkDetailsForm:insertLabel(fTL("Link ID")).
   PrinterLocationLinkDetailsForm:insertTextField("PrinterLocationLinkID", "", 110, TRUE).    
   
   PrinterLocationLinkDetailsForm:startRow().
   PrinterLocationLinkDetailsForm:insertLabel(fTL("Location")).
   PrinterLocationLinkDetailsForm:insertTextField("LocationRef", "", 200, TRUE). 
   
   PrinterLocationLinkDetailsForm:insertButton("PrinterLocationLinkSearch", 
                                               "Search Locations", 
                                               "printerLocationLinkLocationLookup"
                                               + "('printerlocationlink_details_form','LocationRef');").  

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
   PrinterLocationLinkDetailsForm:insertHiddenField("printer_browse_scroll",            "").
   PrinterLocationLinkDetailsForm:insertHiddenField("popup_printerlocationlink_browse", "").
   PrinterLocationLinkDetailsForm:insertHiddenField("PrinterID",                        STRING(intSelectedPrinter)).
   PrinterLocationLinkDetailsForm:insertHiddenField("PrinterLocationLinkID",            STRING(intSelectedPrinterLocationLink)).
   PrinterLocationLinkDetailsForm:insertHiddenField("LocationID",                       "").
   PrinterLocationLinkDetailsForm:insertHiddenField("form_name",                        "printerlocationlink_details_form").
   PrinterLocationLinkDetailsForm:insertHiddenField("prog_name",                        "adPrinterAdmin.p").
   
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
      {adPrinterAdmin_printerlocationlink_details_form.i}
      
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
   
   ASSIGN chrPrinterID                      = get-value("PrinterID")
          intSelectedPrinter                = INTEGER(chrPrinterID)
          chrScrollToPrinterRow             = STRING(INTEGER(get-value("printer_browse_scroll"))) + ";"
          chrPrinterLocationLinkID          = get-value("PrinterLocationLinkID")
          intSelectedPrinterLocationLink    = INTEGER(chrPrinterLocationLinkID)
          chrScrollToPrinterLocationLinkRow = STRING(INTEGER(get-value("printerlocationlink_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrPrinterID <> "" THEN
      chrSelectPrinterRow = 'selectPrinterRow(document.getElementById("printer_browse_row_' + chrPrinterID + '"),"' + chrPrinterID +  '");'.
   
   IF chrPrinterLocationLinkID <> "" THEN
      chrSelectPrinterLocationLinkRow = 'selectPrinterLocationLinkRow(document.getElementById("printerlocationlink_browse_row_' 
                                           + chrPrinterLocationLinkID + '"),"' + chrPrinterLocationLinkID +  '");'.

   IF get-value('popup_printerlocationlink_browse') = "yes" THEN 
      chrPopupPrinterLocationLinks = 'enablePopup("printerlocationlink_browse_form_popup");'.

   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("printer_browse").scrollTop=' + chrScrollToPrinterRow + chrSelectPrinterRow
                             + chrSelectPrinterLocationLinkRow + chrPopupPrinterLocationLinks.
   
   /* Mandatory include to set opening HTML tags and default files */
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Printer Admin".
   ThisPage:FrameTitle    = "Printer Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("printer.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Program Output/Logic Start ********/
   RUN pPrinterBrowse.
   
   FIND FIRST Printer NO-LOCK
     WHERE Printer.PrinterID = intSelectedPrinter NO-ERROR.
   
   /******* Procedures to build Popups ********/
   RUN pPrinterDetails.
   
   RUN pPrinterLocationLinkBrowse.
   FIND FIRST PrinterLocationLink NO-LOCK 
      WHERE   PrinterLocationLink.PrinterLocationLinkID = intSelectedPrinterLocationLink NO-ERROR.
   
   RUN pPrinterLocationLinkDetails.
   RUN pLocationLookup.

   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT PrinterBrowseFrame                NO-ERROR.
   DELETE OBJECT PrinterBrowse                     NO-ERROR.
   DELETE OBJECT PrinterBrowseButtons              NO-ERROR.
   DELETE OBJECT PrinterDetailsForm                NO-ERROR.
   DELETE OBJECT PrinterDetailsButtons             NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowseFrame    NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowse         NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowseButtons  NO-ERROR.
   DELETE OBJECT PrinterLocationLinkDetailsForm    NO-ERROR.
   DELETE OBJECT PrinterLocationLinkDetailsButtons NO-ERROR.
   DELETE OBJECT PrinterLocationLinkBrowseForm     NO-ERROR.
   DELETE OBJECT LocationLookupBrowseForm          NO-ERROR.
   DELETE OBJECT LocationLookupBrowse              NO-ERROR.
   DELETE OBJECT LocationLookupBrowseButtons       NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

