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
{getReceivingOptions.i}


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedOwner        AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectOwnerRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToOwnerRow     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrOwnerID              AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE OwnerBrowseFrame        AS pageFrame.
DEFINE VARIABLE OwnerBrowse             AS browseTable.
DEFINE VARIABLE OwnerBrowseButtons      AS buttonBar.
DEFINE VARIABLE OwnerDetailsForm        AS dataForm.
DEFINE VARIABLE OwnerDetailsButtons     AS buttonBar.

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
   
   ASSIGN chrOwnerID = get-value("OwnerID")
          intSelectedOwner = INTEGER(chrOwnerID)
          chrScrollToOwnerRow = STRING(INTEGER(get-value("owner_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrOwnerID <> "" THEN
     chrSelectOwnerRow = 'selectOwnerRow(document.getElementById("owner_browse_row_' + chrOwnerID + '"),"' + chrOwnerID +  '");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("owner_browse").scrollTop=' + chrScrollToOwnerRow + chrSelectOwnerRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Owner Admin".
   ThisPage:FrameTitle = "Owner Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("owner.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pOwnerBrowse.
   
   FIND Owner WHERE Owner.OwnerID = intSelectedOwner NO-LOCK NO-ERROR.
   
   /******* Popup Browsers and Forms ********/    
   RUN pOwnerDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT OwnerBrowseFrame        NO-ERROR.
   DELETE OBJECT OwnerBrowse             NO-ERROR.
   DELETE OBJECT OwnerBrowseButtons      NO-ERROR.
   DELETE OBJECT OwnerDetailsForm        NO-ERROR.
   DELETE OBJECT OwnerDetailsButtons     NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOwnerBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOwnerBrowse Procedure 
PROCEDURE pOwnerBrowse:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "owner_details_form"}
   
   OwnerBrowse = NEW browseTable("owner_browse").
   OwnerBrowse:BrowseWidth  = 965.
   OwnerBrowse:BrowseHeight = 455.
   OwnerBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   OwnerBrowse:insertColumn(fTL("Owner ID"), 80, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Owner}
   
   OwnerBrowse:insertColumn(fTL("Listing Seq"), 100, "CHARACTER",         FALSE).
   OwnerBrowse:insertColumn(fTL("Owner Code"),  160, "CHARACTER", "left", FALSE).
   OwnerBrowse:insertColumn(fTL("Owner Name"),  160, "CHARACTER", "left", FALSE).
   OwnerBrowse:insertColumn(fTL("Vendor"),      160, "CHARACTER", "left", FALSE).
   OwnerBrowse:insertColumn(fTL("Customer"),    160, "CHARACTER", "left", FALSE).
   OwnerBrowse:insertColumn(fTL("Active"),       80, "LOGICAL",           FALSE).
   
   /*Body*/
   OwnerBrowse:startBody().
   
   FOR EACH Owner NO-LOCK /*idx=ActiveOwnerName*/
      BY Owner.ListingSequence:
         
      FIND FIRST Vendor   OF Owner NO-LOCK NO-ERROR.
      FIND FIRST Customer OF Owner NO-LOCK NO-ERROR. 
      
      OwnerBrowse:startRow(Owner.OwnerID, "selectOwnerRow(this," + '"' + STRING(Owner.OwnerID) + '"' + ");", "").
      OwnerBrowse:insertData(Owner.OwnerID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Owner}
      
      OwnerBrowse:insertData(Owner.ListingSequence).
      OwnerBrowse:insertData(Owner.OwnerCode).
      OwnerBrowse:insertData(Owner.OwnerName, "left").
      OwnerBrowse:insertData((IF AVAILABLE Vendor THEN Vendor.VendorName ELSE ""), "left").
      OwnerBrowse:insertData((IF AVAILABLE Customer THEN Customer.CustomerName ELSE ""), "left").
      OwnerBrowse:insertData(Owner.Active,"Yes/No").
      
      /* Add hidden fields */
      OwnerBrowse:insertHiddenData("OwnerVersionID",Owner.VersionID).
      
      OwnerBrowse:endRow().
      
   END. /*FOR EACH Owner NO-LOCK */
   
   OwnerBrowse:endTable().
   
   /* Create a new frame */
   OwnerBrowseFrame = NEW pageFrame().
   OwnerBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   OwnerBrowseFrame:FormAction="dbOwnerUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   OwnerBrowseFrame:formOpen("owner_browse_form").
   
   /* Start the Frame Header */
   OwnerBrowseFrame:insertSpacer(5).
   OwnerBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   OwnerBrowse:displayBrowse().  
   
   /* End the Frame Header */
   OwnerBrowseFrame:frameClose().
   OwnerBrowseFrame:insertSpacer(10).
   
   OwnerBrowseFrame:insertHiddenField("owner_browse_scroll","").
   OwnerBrowseFrame:insertHiddenField("OwnerID","").
   OwnerBrowseFrame:insertHiddenField("OwnerVersionID","").
   OwnerBrowseFrame:insertHiddenField("form_name", "owner_browse_form").
   OwnerBrowseFrame:insertHiddenField("prog_name", "adOwnerAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OwnerBrowseFrame}
   
   OwnerBrowseFrame:formClose().
   
   /* Create Button Bar */
   OwnerBrowseButtons = NEW buttonBar().
   OwnerBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   OwnerBrowseButtons:addButton("owner_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewOwnerDetails('owner_details_form');",
                                 (IF intSelectedOwner > 0 THEN "" ELSE "Disabled")).
   
   OwnerBrowseButtons:addButton("owner_browse_form_btn_create",
                                 fTL("Create"),
                                 "createOwner('owner_details_form');",
                                 "").
   
   OwnerBrowseButtons:addButton("owner_browse_form_btn_delete",
                                 fTL("Delete"),
                                 "confirmDeleteOwner();",
                                 (IF intSelectedOwner > 0 THEN "" ELSE "Disabled")).
   
   OwnerBrowseButtons:closeBar().  
   OwnerBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOwnerDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOwnerDetails Procedure 
PROCEDURE pOwnerDetails:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "owner_details_form"}
   
   ASSIGN chrDisplayFieldList  = "OwnerID,ListingSequence,OwnerCode,OwnerName,VendorID,CustomerID,Active"
          chrEditFieldList     = "OwnerCode,ListingSequence,OwnerName,VendorID,CustomerID,Active"
          chrNewFieldList      = "OwnerCode,ListingSequence,OwnerName,VendorID,CustomerID,Active"
          chrRequiredFieldList = "OwnerCode,OwnerName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   OwnerDetailsForm = NEW dataForm("owner_details_form").
   OwnerDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   OwnerDetailsForm:FormAction  = "dbOwnerUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   OwnerDetailsForm:FormWidth   = 460.
   OwnerDetailsForm:FormHeight  = 300.
   OwnerDetailsForm:FormTitle   = "Owner Details".
   OwnerDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   OwnerDetailsForm:insertPaddingColumn(50).
   OwnerDetailsForm:insertColumn(90).
   OwnerDetailsForm:insertColumn(120).
   OwnerDetailsForm:insertColumn(20).
   OwnerDetailsForm:insertColumn(4).
   OwnerDetailsForm:insertColumn(110).
   
   /* Fields */
   OwnerDetailsForm:startRow().
   OwnerDetailsForm:insertLabel("Owner ID").
   OwnerDetailsForm:insertTextField("OwnerID", "", 110, TRUE).
    
   OwnerDetailsForm:startRow().
   OwnerDetailsForm:insertLabel("Listing").
   OwnerDetailsForm:insertTextField("ListingSequence", "", 110, TRUE). 
   
   OwnerDetailsForm:startRow().
   OwnerDetailsForm:insertLabel("Owner Code").
   OwnerDetailsForm:insertTextField("OwnerCode", "", 220, TRUE).  
   
   OwnerDetailsForm:startRow().
   OwnerDetailsForm:insertLabel("Owner Name").
   OwnerDetailsForm:insertTextField("OwnerName", "", 220, TRUE). 
   
   OwnerDetailsForm:startRow().
   OwnerDetailsForm:insertLabel(fTL("Vendor")).
   OwnerDetailsForm:insertComboField("VendorID", "", 110, TRUE).
   OwnerDetailsForm:insertComboPairs("VendorID", "0", "None").
   FOR EACH Vendor NO-LOCK
      WHERE Vendor.Active:
         
      OwnerDetailsForm:insertComboPairs("VendorID",STRING(Vendor.VendorID),Vendor.VendorName).               
   END. /* FOR EACH Vendor */ 
   
   OwnerDetailsForm:startRow().
   OwnerDetailsForm:insertLabel(fTL("Customer")).
   OwnerDetailsForm:insertComboField("CustomerID", "", 110, TRUE).
   OwnerDetailsForm:insertComboPairs("CustomerID", "0", "None").
   FOR EACH Customer NO-LOCK
      WHERE Customer.Active:
         
      OwnerDetailsForm:insertComboPairs("CustomerID",STRING(Customer.CustomerID),Customer.CustomerName).               
   END. /* FOR EACH Customer */
   
   OwnerDetailsForm:startRow().
   OwnerDetailsForm:insertLabel(fTL("Active")). 
   OwnerDetailsForm:insertComboField("Active", "", 110, TRUE).  
   OwnerDetailsForm:insertComboPairs("Active", "yes", "Active").
   OwnerDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pOwnerDetailsFields}
   
   /* Add Hidden Fields*/
   OwnerDetailsForm:insertHiddenField("ListingSequence", "").
   OwnerDetailsForm:insertHiddenField("owner_browse_scroll", "").
   OwnerDetailsForm:insertHiddenField("form_name", "owner_details_form").
   OwnerDetailsForm:insertHiddenField("prog_name", "adOwnerAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i OwnerDetailsForm}
   
   /* Create Button Bar */
   OwnerDetailsButtons = NEW buttonBar().
   
   OwnerDetailsButtons:addButton("owner_details_form_btn_save", 
                                  fTL("Save"), 
                                  "updateOwner('owner_details_form');").
   
   OwnerDetailsButtons:addButton("owner_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('owner_details_form_popup');").
   
   OwnerDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   OwnerDetailsForm:FormButtons = OwnerDetailsButtons.
   
   OwnerDetailsForm:endForm(). 
   
   OwnerDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + OwnerDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOwnerDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOwnerDetailsFields Procedure 
PROCEDURE pOwnerDetailsFields:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         OwnerDetailsForm:startRow().
         OwnerDetailsForm:insertLabel(fTL("Field Label")).
         OwnerDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      /*{adOwnerAdmin_owner_details_form.i}*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

