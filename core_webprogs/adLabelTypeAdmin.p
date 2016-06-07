&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adLabeltypeAdmin.p

  Description: Admin screen for Label Type

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Nicholas Diessner

  Created: 31/03/2015

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
DEFINE VARIABLE intSelectedLabelType    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrLabelTypeRow         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLabelTypeRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intLabelTypeID          AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrLabelTypeID          AS CHARACTER   NO-UNDO.


/* Objects */
DEFINE VARIABLE LabelTypeBrowseFrame    AS pageFrame.
DEFINE VARIABLE LabelTypeBrowse         AS browseTable.
DEFINE VARIABLE LabelTypeBrowseButtons  AS buttonBar.
DEFINE VARIABLE LabelTypeDetailsForm    AS dataForm.
DEFINE VARIABLE LabelTypeDetailsButtons AS buttonBar.

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
   
   ASSIGN 
      chrLabelTypeID          = get-value("LabelTypeID")
      intLabelTypeID          = INTEGER(chrLabelTypeID)
      intSelectedLabelType    = INTEGER(intLabelTypeID)
      chrScrollToLabelTypeRow = STRING(INTEGER(get-value("labeltype_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrLabelTypeID <> "" THEN
      chrLabelTypeRow = 'selectLabelTypeRow(document.getElementById("labeltype_browse_row_' + chrLabelTypeID + '"),"' 
         + chrLabelTypeID +  '");'.
                                                          
  
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("labeltype_browse").scrollTop=' + chrScrollToLabelTypeRow 
      + chrLabelTypeRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "LabelType Admin".
   ThisPage:FrameTitle    = "LabelType Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for LabelType */
   ThisPage:addJavaScript("labeltype.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pLabelTypeBrowse.
   
   /******* Popup Browsers and Forms ********/
   IF intLabelTypeID <> 0 THEN
      FIND FIRST LabelType NO-LOCK /*idx=LabelTypeID*/
         WHERE LabelType.LabelTypeID = intSelectedLabelType NO-ERROR.
         
   RUN pLabelTypeDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT LabelTypeBrowseFrame    NO-ERROR.
   DELETE OBJECT LabelTypeBrowse         NO-ERROR.
   DELETE OBJECT LabelTypeBrowseButtons  NO-ERROR.
   DELETE OBJECT LabelTypeDetailsForm    NO-ERROR.
   DELETE OBJECT LabelTypeDetailsButtons NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationBrowse Procedure 
PROCEDURE pLabelTypeBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "labeltype_details_form"}
   
   LabelTypeBrowse              = NEW browseTable("labeltype_browse").
   LabelTypeBrowse:BrowseWidth  = 965.
   LabelTypeBrowse:BrowseHeight = 455.
   LabelTypeBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the LabelType ID as first Column */
   LabelTypeBrowse:insertColumn(fTL("LabelType ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LabelType}
   
   LabelTypeBrowse:insertColumn(fTL("Type Name"),       200, "CHARACTER", "LEFT", FALSE).
   LabelTypeBrowse:insertColumn(fTL("Type Decription"), 300, "CHARACTER",         FALSE).
   LabelTypeBrowse:insertColumn(fTL("Extension"),       150, "CHARACTER",         FALSE).
   LabelTypeBrowse:insertColumn(fTL("Active"),          100, "LOGICAL",           FALSE).
   
  
   /*Body*/
   LabelTypeBrowse:startBody().
   
   
   /* Query for getting all the information from the table LabelType*/
   FOR EACH LabelType NO-LOCK /*idx=LabelTypeID*/
      BY    LabelType.LabelTypeID:   
            
      LabelTypeBrowse:startRow(LabelType.LabelTypeID, "selectLabelTypeRow(this," + '"' + STRING(LabelType.LabelTypeID) + '"' + ");", "").
      
      /* Set the first column value as LabelType ID*/
      LabelTypeBrowse:insertData(LabelType.LabelTypeID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i LabelType}
      
      LabelTypeBrowse:insertData(LabelType.TypeName, "LEFT").
      LabelTypeBrowse:insertData(LabelType.TypeDescr, "LEFT").
      LabelTypeBrowse:insertData(LabelType.Extension).
      LabelTypeBrowse:insertData(STRING(LabelType.Active, "Yes/No")).
      
      
      /* Add hidden fields */
      LabelTypeBrowse:insertHiddenData("LabelTypeVersionID",LabelType.VersionID).
      LabelTypeBrowse:insertHiddenData("LabelTypeID", LabelType.LabelTypeID).
      
      LabelTypeBrowse:endRow().
      
   END. /*FOR EACH LabelType NO-LOCK*/
   
   LabelTypeBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LabelTypeBrowse:getErrors().
   
   /* Create a new frame */
   LabelTypeBrowseFrame           = NEW pageFrame().
   LabelTypeBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LabelTypeBrowseFrame:FormAction="dbLabelTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LabelTypeBrowseFrame:formOpen("labeltype_browse_form").
   
   /* Start the Frame Header */
   LabelTypeBrowseFrame:insertSpacer(5).
   LabelTypeBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LabelTypeBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LabelTypeBrowseFrame:frameClose().
   LabelTypeBrowseFrame:insertSpacer(10).
   
   LabelTypeBrowseFrame:insertHiddenField("labeltype_browse_scroll","").
   LabelTypeBrowseFrame:insertHiddenField("LabelTypeID","").
   LabelTypeBrowseFrame:insertHiddenField("LabelTypeVersionID","").
   LabelTypeBrowseFrame:insertHiddenField("form_name","labeltype_browse_form").
   LabelTypeBrowseFrame:insertHiddenField("prog_name","adLabelTypeAdmin.p").
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LabelTypeBrowseFrame}
   
   LabelTypeBrowseFrame:formClose().
   
   /* Create Button Bar */
   LabelTypeBrowseButtons           = NEW buttonBar().
   LabelTypeBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   LabelTypeBrowseButtons:addButton("labeltype_browse_form_btn_details",
                                    fTL("Details"),
                                    "viewLabelTypeDetails('labeltype_details_form');",
                                    (IF intSelectedLabelType > 0 THEN "" ELSE "Disabled")).
   
   LabelTypeBrowseButtons:addButton("labeltype_browse_form_btn_create",
                                    fTL("Create"),
                                    "createLabelType('labeltype_details_form');",
                                    "").
   
   LabelTypeBrowseButtons:closeBar().  
   LabelTypeBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetails Procedure 
PROCEDURE pLabelTypeDetails :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "labeltype_details_form"}
   
   
   /* Some fields will have to be changed to fit into the LabelType table fields  */
   
   ASSIGN 
      chrDisplayFieldList  = "LabelTypeID,TypeName,TypeCode,TypeDescr,Extension,InkRibbonRequired,ListingSequence,"+
                             "LabelPaper,LabelWidth,LabelHeight,Active"
      chrEditFieldList     = "TypeName,TypeCode,TypeDescr,Extension,InkRibbonRequired,ListingSequence,LabelPaper,LabelWidth,LabelHeight,Active"
      chrNewFieldList      = "TypeName,TypeCode,TypeDescr,Extension,InkRibbonRequired,ListingSequence,LabelPaper,LabelWidth,LabelHeight,Active"
      chrRequiredFieldList = ""
      chrExtraFieldList    = ""
      chrValidateFieldList = "".
      
   
   LabelTypeDetailsForm = NEW dataForm("labeltype_details_form").
   LabelTypeDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LabelTypeDetailsForm:FormAction = "dbLabelTypeUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LabelTypeDetailsForm:FormWidth   = 460.
   LabelTypeDetailsForm:FormHeight  = 300.
   LabelTypeDetailsForm:FormTitle   = fTL("LabelType Details").
   LabelTypeDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   LabelTypeDetailsForm:insertPaddingColumn(30).
   LabelTypeDetailsForm:insertColumn(130).
   LabelTypeDetailsForm:insertColumn(120).
   LabelTypeDetailsForm:insertColumn(20).
   LabelTypeDetailsForm:insertColumn(4).
  
   /* Fields */
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("LabelType ID").
   LabelTypeDetailsForm:insertTextField("LabelTypeID", "", 110, TRUE). 
      
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Type Name").
   LabelTypeDetailsForm:insertTextField("TypeName", "", 110, TRUE).  
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Type Code").
   LabelTypeDetailsForm:insertTextField("TypeCode", "", 110, TRUE).
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Extension").
   LabelTypeDetailsForm:insertTextField("Extension", "", 110, TRUE).
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Type Description").
   LabelTypeDetailsForm:insertTextField("TypeDescr", "", 110, TRUE).
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel(fTL("InkRibbon Required")). 
   LabelTypeDetailsForm:insertComboField("InkRibbonRequired", "", 110, TRUE).  
   LabelTypeDetailsForm:insertComboPairs("InkRibbonRequired", "yes", "Yes").
   LabelTypeDetailsForm:insertComboPairs("InkRibbonRequired", "no",  "No").
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Label Paper").
   LabelTypeDetailsForm:insertTextField("LabelPaper", "", 110, TRUE).
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Label Width").
   LabelTypeDetailsForm:insertTextField("LabelWidth", "", 110, TRUE).
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Label Height").
   LabelTypeDetailsForm:insertTextField("LabelHeight", "", 110, TRUE).
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel("Listing Sequence").
   LabelTypeDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).
   
   LabelTypeDetailsForm:startRow().
   LabelTypeDetailsForm:insertLabel(fTL("Active")). 
   LabelTypeDetailsForm:insertComboField("Active", "", 110, TRUE).  
   LabelTypeDetailsForm:insertComboPairs("Active", "yes", "Active").
   LabelTypeDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pLabelTypeDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LabelTypeDetailsForm:insertHiddenField("labeltype_browse_scroll", "").
   LabelTypeDetailsForm:insertHiddenField("form_name", "labeltype_details_form").
   LabelTypeDetailsForm:insertHiddenField("prog_name", "adLabelTypeAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LabelTypeDetailsForm}
   
   /* Create Button Bar */
   LabelTypeDetailsButtons = NEW buttonBar().
   LabelTypeDetailsButtons:addButton("labeltype_details_form_btn_save", 
                                    fTL("Save"), 
                                    "updateLabelType('labeltype_details_form');").
   LabelTypeDetailsButtons:addButton("labeltype_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('labeltype_details_form_popup');").
   LabelTypeDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LabelTypeDetailsForm:FormButtons = LabelTypeDetailsButtons.
   
   LabelTypeDetailsForm:endForm(). 
   
   LabelTypeDetailsForm:displayForm(). 
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingStationDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingStationDetailsFields Procedure 
PROCEDURE pLabelTypeDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            LabelTypeDetailsForm:startRow().
            LabelTypeDetailsForm:insertLabel(fTL("Field Label")).
            LabelTypeDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
