&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adMasterProcessProgramAdmin.p

  Description: 
      
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Maria Chereches 

  Created: 13/01/2016

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
DEFINE VARIABLE intSelectedMasterProcessProgram     AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedQualitySampleLinehour    AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedProcessProgram           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectMasterProcessProgramRow    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToMasterProcessProgramRow  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrMasterProcessProgramID           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToProcessProgramRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrProcessProgramID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupProcessProgram              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupProcessProgramHistory       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectProcessProgramRow          AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE MasterProcessProgramBrowseFrame     AS pageFrame.
DEFINE VARIABLE MasterProcessProgramBrowse          AS browseTable.
DEFINE VARIABLE MasterProcessProgramBrowseButtons   AS buttonBar.
DEFINE VARIABLE MasterProcessProgramDetailsForm     AS dataForm.
DEFINE VARIABLE MasterProcessProgramDetailsButtons  AS buttonBar.

DEFINE VARIABLE ProcessProgramBrowseForm            AS dataForm.   
DEFINE VARIABLE ProcessProgramBrowse                AS browseTable.
DEFINE VARIABLE ProcessProgramBrowseButtons         AS buttonBar.

DEFINE VARIABLE ProcessProgramDetailsForm           AS dataForm.
DEFINE VARIABLE ProcessProgramDetailsButtons        AS buttonBar.

DEFINE VARIABLE ProcessProgramHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE ProcessProgramHistoryBrowse         AS browseTable.
DEFINE VARIABLE ProcessProgramHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE ProcessProgramHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE ProcessProgramHistoryDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pProcessProgramBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessProgramBrowse Procedure
PROCEDURE pProcessProgramBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "processprogram_details_form"}
   
   FIND FIRST ProcessProgram NO-LOCK /* idx=MasterProcessProgramID */
      WHERE ProcessProgram.ProcessProgramID = intSelectedProcessProgram NO-ERROR.
   
   ProcessProgramBrowseForm = NEW dataForm("processprogram_browse_form").
   ProcessProgramBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   ProcessProgramBrowseForm:FormAction="dbProcessProgramUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ProcessProgramBrowseForm:FormWidth   = 850.
   ProcessProgramBrowseForm:FormHeight  = 540.
   ProcessProgramBrowseForm:FormTitle   = fTL("ProcessProgram Browse ").
   ProcessProgramBrowseForm:FormType    = "xxl_large".
   
   ProcessProgramBrowse = NEW browseTable("processprogram_browse").
   ProcessProgramBrowse:BrowseWidth  = 830.
   ProcessProgramBrowse:BrowseHeight = 500.
   ProcessProgramBrowse:ExcelExport  = TRUE.
   ProcessProgramBrowse:SessionID    = intGblSessionID.
   
   
   ProcessProgramBrowse:insertColumn(fTL("ID"),                    90, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i ProcessProgram}
   
   ProcessProgramBrowse:insertColumn(fTL("Program Name"),         150, "CHARACTER", "left", FALSE).
   ProcessProgramBrowse:insertColumn(fTL("Program Descr"),        180, "CHARACTER", "left", FALSE).
   ProcessProgramBrowse:insertColumn(fTL("Listing Seq"),           90, "INTEGER",           FALSE).
   ProcessProgramBrowse:insertColumn(fTL("CreateOutsideSyncore"), 150, "LOGICAL",           FALSE).
   ProcessProgramBrowse:insertColumn(fTL("UpdateOutsideSyncore"), 150, "LOGICAL",           FALSE).
   
   ProcessProgramBrowse:StartBody().
   
      /*List the ProcessProgram*/
      FOR EACH ProcessProgram NO-LOCK 
         WHERE ProcessProgram.ListingSequence = MasterProcessProgram.ListingSequence
         BY    ProcessProgram.ProcessProgramID:
       
         ProcessProgramBrowse:startRow(ProcessProgram.ProcessProgramID, "selectProcessProgramRow(this," + '"' 
                                          + STRING(ProcessProgram.ProcessProgramID) + '"' + ");", "").                                                            
                                                                     
         ProcessProgramBrowse:insertData(ProcessProgram.ProcessProgramID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i ProcessProgram}
         
         ProcessProgramBrowse:insertData(ProcessProgram.ProgramName, "LEFT").
         ProcessProgramBrowse:insertData(ProcessProgram.ProgramDescr, "LEFT").
         ProcessProgramBrowse:insertData(ProcessProgram.ListingSequence).
         ProcessProgramBrowse:insertData(STRING(ProcessProgram.CanCreateDataOutsideOfSyncore, "Yes/No")).
         ProcessProgramBrowse:insertData(STRING(ProcessProgram.CanUpdateDataOutsideOfSyncore, "Yes/No")).
         
         
         /* Add hidden fields */         
         ProcessProgramBrowse:insertHiddenData("ProcessProgramID",ProcessProgram.ProcessProgramID).
         ProcessProgramBrowse:insertHiddenData("ProcessProgramVersionID",ProcessProgram.VersionID).
         
         ProcessProgramBrowse:endRow().
      
      END. /* FOR EACH ProcessProgram */
   
   ProcessProgramBrowse:endTable().
   chrPageBuildError = chrPageBuildError + ProcessProgramBrowse:getErrors().
   
   ProcessProgramBrowseForm:insertHiddenField("ProcessProgramID",chrProcessProgramID).
   ProcessProgramBrowseForm:insertHiddenField("ProcessProgramVersionID","").
   ProcessProgramBrowseForm:insertHiddenField("masterprocessprogram_browse_scroll","").
   ProcessProgramBrowseForm:insertHiddenField("processprogram_browse_scroll","").
   ProcessProgramBrowseForm:insertHiddenField("popup_processprogram_browse","").
   ProcessProgramBrowseForm:insertHiddenField("popup_processprogramhistory_browse","").
   ProcessProgramBrowseForm:insertHiddenField("form_name","processprogram_browse_form").
   ProcessProgramBrowseForm:insertHiddenField("prog_name","adMasterProcessProgramAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ProcessProgramBrowseForm}
   
   /* Create Button Bar */
   ProcessProgramBrowseButtons = NEW buttonBar().
   
   ProcessProgramBrowseButtons:addButton("processprogram_browse_form_btn_create",
                                             fTL("Create"),
                                             "createProcessProgram('processprogram_details_form');",
                                             "").                                                 
   
   ProcessProgramBrowseButtons:addButton("processprogram_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewProcessProgramDetails('processprogram_details_form');",
                                             "Disabled").
                                             
/*   ProcessProgramBrowseButtons:addButton("processprogram_browse_form_btn_history",*/
/*                                             fTL("History"),                      */
/*                                             "viewProcessProgramHistory();",      */
/*                                             "Disabled").                         */
     
     /*Button for later if needed*/                                 
/*   ProcessProgramBrowseButtons:addButton("processprogram_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_processprogram_browse.xml')").*/
   
   ProcessProgramBrowseButtons:addButton("processprogram_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('processprogram_browse_form_popup');").
   
   ProcessProgramBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   ProcessProgramBrowseForm:FormBrowse  = ProcessProgramBrowse.
   ProcessProgramBrowseForm:FormButtons = ProcessProgramBrowseButtons.
   ProcessProgramBrowseForm:endForm(). 
   
   ProcessProgramBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pProcessProgramDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessProgramDetails Procedure
PROCEDURE pProcessProgramDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "processprogram_details_form"}
   
   ASSIGN chrDisplayFieldList  = "ProcessProgramID,ProgramName,ProgramDescr,ListingSequence,CanCreateDataOutsideOfSyncore"
                               + ",CanUpdateDataOutsideOfSyncore" 
          chrEditFieldList     = "ProgramName,ProgramDescr,CanCreateDataOutsideOfSyncore,CanUpdateDataOutsideOfSyncore" 
          chrNewFieldList      = "ProgramName,ProgramDescr,ListingSequence,CanCreateDataOutsideOfSyncore,CanUpdateDataOutsideOfSyncore" 
          chrRequiredFieldList = "ProgramName,ProgramDescr,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
                             
   
   ProcessProgramDetailsForm = NEW dataForm("processprogram_details_form").
   ProcessProgramDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   ProcessProgramDetailsForm:FormAction = "dbProcessProgramUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   ProcessProgramDetailsForm:FormWidth   = 460.
   ProcessProgramDetailsForm:FormHeight  = 200.
   ProcessProgramDetailsForm:FormTitle   = "ProcessProgram Details".
   ProcessProgramDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   ProcessProgramDetailsForm:insertPaddingColumn(40).
   ProcessProgramDetailsForm:insertColumn(120).
   ProcessProgramDetailsForm:insertColumn(120).
   ProcessProgramDetailsForm:insertColumn(20).
   ProcessProgramDetailsForm:insertColumn(4).
   ProcessProgramDetailsForm:insertColumn(40).  
   
   /* Fields */
   ProcessProgramDetailsForm:startRow().
   ProcessProgramDetailsForm:insertLabel(fTL("Program ID")).
   ProcessProgramDetailsForm:insertTextField("ProcessProgramID", "", 200, TRUE).    
   
   ProcessProgramDetailsForm:startRow().
   ProcessProgramDetailsForm:insertLabel(fTL("Program Name")).
   ProcessProgramDetailsForm:insertTextField("ProgramName", "", 200, TRUE).
   
   ProcessProgramDetailsForm:startRow().
   ProcessProgramDetailsForm:insertLabel(fTL("Program Descr")).
   ProcessProgramDetailsForm:insertTextField("ProgramDescr", "", 200, TRUE).
   
   ProcessProgramDetailsForm:startRow().
   ProcessProgramDetailsForm:insertLabel(fTL("Listing Sequence")).
   ProcessProgramDetailsForm:insertTextField("ListingSequence", STRING(MasterProcessProgram.ListingSequence), 200, TRUE).
   
   ProcessProgramDetailsForm:startRow().
   ProcessProgramDetailsForm:insertLabel(fTL("CanCreateDataOutsideOfSyncore")).
   ProcessProgramDetailsForm:insertComboField("CanCreateDataOutsideOfSyncore", "", 200, TRUE).
   ProcessProgramDetailsForm:insertComboPairs("CanCreateDataOutsideOfSyncore", "yes", "Yes").
   ProcessProgramDetailsForm:insertComboPairs("CanCreateDataOutsideOfSyncore", "no",  "No").
   
   ProcessProgramDetailsForm:startRow().
   ProcessProgramDetailsForm:insertLabel(fTL("CanUpdateDataOutsideOfSyncore")).
   ProcessProgramDetailsForm:insertComboField("CanUpdateDataOutsideOfSyncore", "", 200, TRUE).
   ProcessProgramDetailsForm:insertComboPairs("CanUpdateDataOutsideOfSyncore", "yes", "Yes").
   ProcessProgramDetailsForm:insertComboPairs("CanUpdateDataOutsideOfSyncore", "no",  "No").
   
   /* Add Hidden Fields*/
   ProcessProgramDetailsForm:insertHiddenField("processprogram_browse_scroll","").
   ProcessProgramDetailsForm:insertHiddenField("popup_processprogram_browse", "").
   ProcessProgramDetailsForm:insertHiddenField("MasterProcessProgramID","").
   ProcessProgramDetailsForm:insertHiddenField("ProcessProgramID","").
   ProcessProgramDetailsForm:insertHiddenField("ProcessProgramVersionID","").
   ProcessProgramDetailsForm:insertHiddenField("MasterProcessProgramListingSequence","").
   ProcessProgramDetailsForm:insertHiddenField("form_name","processprogram_details_form").
   ProcessProgramDetailsForm:insertHiddenField("prog_name","adMasterProcessProgramAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i ProcessProgramDetailsForm}
   
   /* Create Button Bar */
   ProcessProgramDetailsButtons = NEW buttonBar().
   
   ProcessProgramDetailsButtons:addButton("processprogram_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateProcessProgram('processprogram_details_form');").
   
   ProcessProgramDetailsButtons:addButton("processprogram_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('processprogram_details_form_popup');").
                                        
   ProcessProgramDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   ProcessProgramDetailsForm:FormButtons = ProcessProgramDetailsButtons.
   
   ProcessProgramDetailsForm:endForm(). 
   ProcessProgramDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pProcessProgramHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessProgramHistory Procedure
PROCEDURE pProcessProgramHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pProcessProgramHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProcessProgramHistoryDetails Procedure
PROCEDURE pProcessProgramHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
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
   RUN pGetSystemOptions(OUTPUT chrPageBuildError).  
   
   ASSIGN chrMasterProcessProgramID          = get-value("MasterProcessProgramID")
          intSelectedMasterProcessProgram    = INTEGER(chrMasterProcessProgramID)
          chrProcessProgramID                = get-value("ProcessProgramID")
          intSelectedProcessProgram          = INTEGER(chrProcessProgramID)
          chrScrollToProcessProgramRow       = STRING(INTEGER(get-value("processprogram_browse_scroll"))) + ";"
          chrScrollToMasterProcessProgramRow = STRING(INTEGER(get-value("masterprocessprogram_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrMasterProcessProgramID <> "" THEN
     chrSelectMasterProcessProgramRow = 'selectMasterProcessProgramRow(document.getElementById("masterprocessprogram_browse_row_' + chrMasterProcessProgramID + '"),"' 
                                          + chrMasterProcessProgramID +  '");'.
                                          
   IF chrProcessProgramID <> "" THEN
     chrSelectProcessProgramRow = 'selectProcessProgramRow(document.getElementById("processprogram_browse_row_' 
                                       + chrProcessProgramID + '"),"' + chrProcessProgramID +  '");'.    
                                       
   IF get-value('popup_processprogram_browse') = "yes" THEN
      chrPopupProcessProgram  = 'enablePopup("processprogram_browse_form_popup");'.      
      
   IF get-value('popup_processprogramhistory_browse') = "yes" THEN
      chrPopupProcessProgramHistory  = 'enablePopup("processprogramhistory_browse_form_popup");'.                                                                                 
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("masterprocessprogram_browse").scrollTop=' + chrScrollToMasterProcessProgramRow 
                                    + chrSelectMasterProcessProgramRow
                                    + chrPopupProcessProgram
                                    + chrSelectProcessProgramRow
                                    + chrPopupProcessProgramHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "MasterProcessProgram Admin".
   ThisPage:FrameTitle    = "MasterProcessProgram Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("masterprocessprogram.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pMasterProcessProgramBrowse.
   
   FIND MasterProcessProgram NO-LOCK  
      WHERE MasterProcessProgram.MasterProcessProgramID = intSelectedMasterProcessProgram NO-ERROR.
   
   /******* Popup Browsers and Forms ********/    
   RUN pMasterProcessProgramDetails.
   RUN pProcessProgramBrowse.
   
   FIND ProcessProgram NO-LOCK  
      WHERE ProcessProgram.ProcessProgramID = intSelectedProcessProgram NO-ERROR.
   
   RUN pProcessProgramDetails.
   RUN pProcessProgramHistory.
   RUN pProcessProgramHistoryDetails.   
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT MasterProcessProgramBrowseFrame    NO-ERROR.
   DELETE OBJECT MasterProcessProgramBrowse         NO-ERROR.
   DELETE OBJECT MasterProcessProgramBrowseButtons  NO-ERROR.
   DELETE OBJECT MasterProcessProgramDetailsForm    NO-ERROR.
   DELETE OBJECT MasterProcessProgramDetailsButtons NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pMasterProcessProgramBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMasterProcessProgramBrowse Procedure 
PROCEDURE pMasterProcessProgramBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "masterprocessprogram_details_form"}
   */
   MasterProcessProgramBrowse = NEW browseTable("masterprocessprogram_browse").
   MasterProcessProgramBrowse:BrowseWidth  = 965.
   MasterProcessProgramBrowse:BrowseHeight = 455.
   MasterProcessProgramBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   MasterProcessProgramBrowse:insertColumn(fTL("ID"),             85, "INTEGER", "", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i MasterProcessProgram}
   
/* MasterProcessProgramBrowse:insertColumn(fTL("VersionID"),     200, "INTEGER",   "left", FALSE). */
   MasterProcessProgramBrowse:insertColumn(fTL("Program Name"),  240, "CHARACTER", "left", FALSE).
   MasterProcessProgramBrowse:insertColumn(fTL("Program Descr"), 250, "CHARACTER", "left", FALSE).
   MasterProcessProgramBrowse:insertColumn(fTL("Web Program"),    85, "LOGICAL",           FALSE).
   MasterProcessProgramBrowse:insertColumn(fTL("Create in App"), 100, "LOGICAL",           FALSE).
   MasterProcessProgramBrowse:insertColumn(fTL("Update in App"), 100, "LOGICAL",           FALSE).
   
   /*Body*/
   MasterProcessProgramBrowse:startBody().
   
   MasterProcessProgramLoop:
   FOR EACH MasterProcessProgram NO-LOCK /*idx=MasterProcessProgramID*/
      BY    MasterProcessProgram.MasterProcessProgramID:
      
      MasterProcessProgramBrowse:startRow(MasterProcessProgram.MasterProcessProgramID, "selectMasterProcessProgramRow(this," + '"' + STRING(MasterProcessProgram.MasterProcessProgramID) + '"' + ");", "").
      MasterProcessProgramBrowse:insertData(MasterProcessProgram.MasterProcessProgramID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i MasterProcessProgram}
      
   /* MasterProcessProgramBrowse:insertData(MasterProcessProgram.MasterProcessProgramVersionID, "left"). */
      MasterProcessProgramBrowse:insertData(MasterProcessProgram.ProgramName,                   "left").
      MasterProcessProgramBrowse:insertData(MasterProcessProgram.ProgramDescr,                  "left").
      MasterProcessProgramBrowse:insertData(STRING(MasterProcessProgram.IsWebProgram,                 "Yes/No")).
      MasterProcessProgramBrowse:insertData(STRING(MasterProcessProgram.CanCreateDataOutsideOfSyncore,"Yes/No")).
      MasterProcessProgramBrowse:insertData(STRING(MasterProcessProgram.CanUpdateDataOutsideOfSyncore,"Yes/No")).
      
      /* Add hidden fields */
      MasterProcessProgramBrowse:insertHiddenData("MasterProcessProgramID",       MasterProcessProgram.MasterProcessProgramID).
      MasterProcessProgramBrowse:insertHiddenData("MasterProcessProgramVersionID",MasterProcessProgram.VersionID).
      MasterProcessProgramBrowse:insertHiddenData("MasterProcessProgramListingSequence",MasterProcessProgram.ListingSequence).
      
      
      MasterProcessProgramBrowse:endRow().
      
   END. /*FOR EACH MasterProcessProgram NO-LOCK */
   
   MasterProcessProgramBrowse:endTable().
   
   /* Create a new frame */
   MasterProcessProgramBrowseFrame = NEW pageFrame().
   MasterProcessProgramBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   MasterProcessProgramBrowseFrame:FormAction="dbMasterProcessProgramUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   MasterProcessProgramBrowseFrame:formOpen("masterprocessprogram_browse_form").
   
   /* Start the Frame Header */
   MasterProcessProgramBrowseFrame:insertSpacer(5).
   MasterProcessProgramBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   MasterProcessProgramBrowse:displayBrowse().  
   
   /* End the Frame Header */
   MasterProcessProgramBrowseFrame:frameClose().
   MasterProcessProgramBrowseFrame:insertSpacer(10).
   
   MasterProcessProgramBrowseFrame:insertHiddenField("masterprocessprogram_browse_scroll","").
   MasterProcessProgramBrowseFrame:insertHiddenField("MasterProcessProgramID","").
   MasterProcessProgramBrowseFrame:insertHiddenField("MasterProcessProgramVersionID","").
   MasterProcessProgramBrowseFrame:insertHiddenField("MasterProcessProgramListingSequence","").
   MasterProcessProgramBrowseFrame:insertHiddenField("popup_processprogram_browse","").
   MasterProcessProgramBrowseFrame:insertHiddenField("form_name", "masterprocessprogram_browse_form").
   MasterProcessProgramBrowseFrame:insertHiddenField("prog_name", "adMasterProcessProgramAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i MasterProcessProgramBrowseFrame}
   
   MasterProcessProgramBrowseFrame:formClose().
   
   /* Create Button Bar */
   MasterProcessProgramBrowseButtons = NEW buttonBar().
   MasterProcessProgramBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   MasterProcessProgramBrowseButtons:addButton("masterprocessprogram_browse_form_btn_details",
                                               fTL("Details"),
                                               "viewMasterProcessProgramDetails('masterprocessprogram_details_form');",
                                               "Disabled").
   
   MasterProcessProgramBrowseButtons:addButton("masterprocessprogram_browse_form_btn_create",
                                               fTL("Create"),
                                               "createMasterProcessProgram('masterprocessprogram_details_form');",
                                               "").
                                               
   MasterProcessProgramBrowseButtons:addButton("masterprocessprogram_browse_form_btn_processprogram",
                                               fTL("Process Program"),
                                               "viewProcessProgramBrowse();",
                                               "Disabled").                                            
   /*
   MasterProcessProgramBrowseButtons:addButton("masterprocessprogram_browse_form_btn_delete",
                                               fTL("Delete"),
                                               "confirmDeleteMasterProcessProgram();",
                                               (IF intSelectedMasterProcessProgram > 0 THEN "" ELSE "Disabled")).
   */
   MasterProcessProgramBrowseButtons:closeBar().  
   MasterProcessProgramBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pMasterProcessProgramDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMasterProcessProgramDetails Procedure 
PROCEDURE pMasterProcessProgramDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "masterprocessprogram_details_form"}
   */
   ASSIGN chrDisplayFieldList  = "MasterProcessProgramID,ProgramName,ProgramDescr,IsWebProgram,CanCreateDataOutsideOfSyncore,CanUpdateDataOutsideOfSyncore"
          chrEditFieldList     = "CanCreateDataOutsideOfSyncore,CanUpdateDataOutsideOfSyncore,IsWebProgram,ProgramDescr,ProgramName"
          chrNewFieldList      = "CanCreateDataOutsideOfSyncore,CanUpdateDataOutsideOfSyncore,IsWebProgram,ProgramDescr,ProgramName"
          chrRequiredFieldList = "CanCreateDataOutsideOfSyncore,CanUpdateDataOutsideOfSyncore,IsWebProgram,ProgramDescr,ProgramName"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

   MasterProcessProgramDetailsForm            = NEW dataForm("masterprocessprogram_details_form").
   MasterProcessProgramDetailsForm:WebStream  = STREAM WebStream:HANDLE.
   MasterProcessProgramDetailsForm:FormAction = "dbMasterProcessProgramUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   MasterProcessProgramDetailsForm:FormWidth  = 460.
   MasterProcessProgramDetailsForm:FormHeight = 300.
   MasterProcessProgramDetailsForm:FormTitle  = "MasterProcessProgram Details".
   MasterProcessProgramDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   MasterProcessProgramDetailsForm:insertPaddingColumn(30).
   MasterProcessProgramDetailsForm:insertColumn(160).
   MasterProcessProgramDetailsForm:insertColumn(120).
   MasterProcessProgramDetailsForm:insertColumn(120).
   MasterProcessProgramDetailsForm:insertColumn(120).
   
   /* Fields */
   MasterProcessProgramDetailsForm:startRow().
   MasterProcessProgramDetailsForm:insertLabel("MasterProcessProgram ID").
   MasterProcessProgramDetailsForm:insertTextField("MasterProcessProgramID", "", 100, TRUE).
   
   MasterProcessProgramDetailsForm:startRow().
   MasterProcessProgramDetailsForm:insertLabel("Program Name").
   MasterProcessProgramDetailsForm:insertTextField("ProgramName", "", 220, TRUE).
   
   MasterProcessProgramDetailsForm:startRow().
   MasterProcessProgramDetailsForm:insertLabel("Program Description").
   MasterProcessProgramDetailsForm:insertTextAreaField("ProgramDescr", "", 220, TRUE).
   
   MasterProcessProgramDetailsForm:startRow().
   MasterProcessProgramDetailsForm:insertLabel("Is Web Program").
   MasterProcessProgramDetailsForm:insertComboField("IsWebProgram", "", 100, TRUE).
   MasterProcessProgramDetailsForm:insertComboPairs("IsWebProgram", "yes", "Yes").
   MasterProcessProgramDetailsForm:insertComboPairs("IsWebProgram",  "no",  "No").
   
   MasterProcessProgramDetailsForm:startRow().
   MasterProcessProgramDetailsForm:insertLabel("Create in Application").
   MasterProcessProgramDetailsForm:insertComboField("CanCreateDataOutsideOfSyncore", "", 100, TRUE).
   MasterProcessProgramDetailsForm:insertComboPairs("CanCreateDataOutsideOfSyncore", "yes", "Yes").
   MasterProcessProgramDetailsForm:insertComboPairs("CanCreateDataOutsideOfSyncore",  "no",  "No").
   
   MasterProcessProgramDetailsForm:startRow().
   MasterProcessProgramDetailsForm:insertLabel("Update in Application").
   MasterProcessProgramDetailsForm:insertComboField("CanUpdateDataOutsideOfSyncore", "", 100, TRUE).
   MasterProcessProgramDetailsForm:insertComboPairs("CanUpdateDataOutsideOfSyncore", "yes", "Yes").
   MasterProcessProgramDetailsForm:insertComboPairs("CanUpdateDataOutsideOfSyncore",  "no",  "No").
   
   {webGetOptionalFormFields.i pMasterProcessProgramDetailsFields}
   
   /* Add Hidden Fields*/
   MasterProcessProgramDetailsForm:insertHiddenField("masterprocessprogram_browse_scroll", "").
   MasterProcessProgramDetailsForm:insertHiddenField("form_name", "masterprocessprogram_details_form").
   MasterProcessProgramDetailsForm:insertHiddenField("prog_name", "adMasterProcessProgramAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i MasterProcessProgramDetailsForm}
   
   /* Create Button Bar */
   MasterProcessProgramDetailsButtons = NEW buttonBar().
   
   MasterProcessProgramDetailsButtons:addButton("masterprocessprogram_details_form_btn_save", 
                                                fTL("Save"), 
                                                "updateMasterProcessProgram('masterprocessprogram_details_form');").
   
   MasterProcessProgramDetailsButtons:addButton("masterprocessprogram_details_form_btn_cancel", 
                                                fTL("Cancel"), 
                                                "cancelUpdate('UserCancelled','process_mode'); disablePopup('masterprocessprogram_details_form_popup');").
   
   MasterProcessProgramDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   MasterProcessProgramDetailsForm:FormButtons = MasterProcessProgramDetailsButtons.
   
   MasterProcessProgramDetailsForm:endForm(). 
   
   MasterProcessProgramDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pMasterProcessProgramDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMasterProcessProgramDetailsFields Procedure 
PROCEDURE pMasterProcessProgramDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         MasterProcessProgramDetailsForm:startRow().
         MasterProcessProgramDetailsForm:insertLabel(fTL("Field Label")).
         MasterProcessProgramDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

