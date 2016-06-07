&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adLabelFileAdmin.p 

  Description: ad file for the Label File Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Evan Urzen

  Created: 12/03/2015
  
  Changes :
  --------------------------------------------------------------------------------------------------------------------------------------------
  Date       Who Project    Description
  ---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
  31/03/2015 EU  CanonTLB   Standards / Clean up 
  --------------------------------------------------------------------------------------------------------------------------------------------

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
{defDataMigrationVariables.i}

/* Definitions for System Options for Receiving */
{getShippingOptions.i}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE intSelectedLabelFile           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedLabelFileHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrLabelFileHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLabelFileRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLabelFileRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLabelFileHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLabelFileID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                AS CHARACTER   NO-UNDO.

/* Buffers */
/* Objects */
DEFINE VARIABLE LabelFileBrowseFrame           AS pageFrame.
DEFINE VARIABLE LabelFileBrowse                AS browseTable.
DEFINE VARIABLE LabelFileBrowseButtons         AS buttonBar.
DEFINE VARIABLE LabelFileDetailsForm           AS dataForm.
DEFINE VARIABLE LabelFileDetailsButtons        AS buttonBar.
DEFINE VARIABLE LabelFileHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE LabelFileHistoryBrowse         AS browseTable.
DEFINE VARIABLE LabelFileHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE LabelFileHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE LabelFileHistoryBrowseButtons  AS buttonBar.
DEFINE VARIABLE LabelFileHistoryButtons        AS buttonBar.


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
   
   ASSIGN chrLabelFileID                 = get-value("LabelFileID")
          intSelectedLabelFile           = INTEGER(chrLabelFileID)
          chrScrollToLabelFileRow        = STRING(INTEGER(get-value("labelfile_browse_scroll"))) + ";"
          /*History details button*/
          chrLabelFileHistoryID          = get-value("LabelFileHistoryID")
          intSelectedLabelFileHistory    = INTEGER(chrLabelFileHistoryID)
          chrScrollToLabelFileHistoryRow = STRING(INTEGER(get-value("labelfileHistory_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrLabelFileID <> "" THEN
      chrLabelFileRow = 'selectLabelFileRow(document.getElementById("labelfile_browse_row_' + chrLabelFileID + '"),"' 
                                                           + chrLabelFileID +  '");'.
                                                          
   IF get-value('popup_labelfilehistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("labelfilehistory_browse_form_popup");'.                                                          
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("labelfile_browse").scrollTop=' + chrScrollToLabelFileRow 
                                                           + chrLabelFileRow 
                                                           + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Label File Admin".
   ThisPage:FrameTitle    = "Label File Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("labelfile.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pLabelFileBrowse.
   
   /******* Popup Browsers and Forms ********/ 
   IF intSelectedLabelFile <> 0 THEN 
   DO:
      FIND FIRST LabelFile NO-LOCK 
         WHERE LabelFile.LabelFileID = intSelectedLabelFile NO-ERROR.
   END. 
/*   IF intSelectedLabelFileHistory <> 0 THEN                                        */
/*   DO:                                                                             */
/*      FIND FIRST LabelFileHistory NO-LOCK                                          */
/*         WHERE LabelFileHistory.LabelFileHistoryID = intSelectedLabelFile NO-ERROR.*/
/*   END.                                                                            */
   RUN pLabelFileDetails.   
   RUN pLabelFileHistoryBrowse.
   RUN pLabelFileHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT LabelFileBrowseFrame           NO-ERROR.
   DELETE OBJECT LabelFileBrowse                NO-ERROR.
   DELETE OBJECT LabelFileBrowseButtons         NO-ERROR.
   DELETE OBJECT LabelFileDetailsForm           NO-ERROR.
   DELETE OBJECT LabelFileDetailsButtons        NO-ERROR.   
   DELETE OBJECT LabelFileHistoryBrowseForm     NO-ERROR.  
   DELETE OBJECT LabelFileHistoryBrowse         NO-ERROR.
   DELETE OBJECT LabelFileHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT LabelFileHistoryDetailsButtons NO-ERROR.
   DELETE OBJECT LabelFileHistoryBrowseButtons  NO-ERROR.
   DELETE OBJECT LabelFileHistoryButtons        NO-ERROR.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pLabelFileBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "labelfile_details_form"}
   
   LabelFileBrowse              = NEW browseTable("labelfile_browse").
   LabelFileBrowse:BrowseWidth  = 965.
   LabelFileBrowse:BrowseHeight = 455.
   LabelFileBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   LabelFileBrowse:insertColumn(fTL("LabelFile ID"), 65, "INTEGER", FALSE). /*TODO COLUMN SIZES*/
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LabelFile}
   
   LabelFileBrowse:insertColumn(fTL("Label Name"),     165, "CHARACTER", "LEFT",   FALSE).
   LabelFileBrowse:insertColumn(fTL("File Path"),      175, "CHARACTER", "LEFT",   FALSE).
   LabelFileBrowse:insertColumn(fTL("File Name"),      170, "CHARACTER", "LEFT",   FALSE). 
   LabelFileBrowse:insertColumn(fTL("Operation Type"), 150, "CHARACTER", "LEFT",   FALSE).
   LabelFileBrowse:insertColumn(fTL("File Prefix"),    95,  "CHARACTER", "LEFT",   FALSE).
   LabelFileBrowse:insertColumn(fTL("Temp Label"),     75,  "LOGICAL",   "CENTER", FALSE).
   LabelFileBrowse:insertColumn(fTL("Active"),         50,  "LOGICAL",   "LEFT",   FALSE).
   
   
   
   /*Body*/
   LabelFileBrowse:startBody().
   
   FOR EACH LabelFile NO-LOCK,
      EACH OperationType OF LabelFile NO-LOCK: 
            
      LabelFileBrowse:startRow(LabelFile.LabelFileID, "selectLabelFileRow(this," + '"' + STRING(LabelFile.LabelFileID) + '"' + ");", "").
      LabelFileBrowse:insertData(LabelFile.LabelFileID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i LabelFile}      

      LabelFileBrowse:insertData(LabelFile.LabelName,                     "LEFT").
      LabelFileBrowse:insertData(LabelFile.FilePath,                      "LEFT").
      LabelFileBrowse:insertData(LabelFile.FileName,                      "LEFT").
      LabelFileBrowse:insertData(OperationType.TypeName,                  "LEFT").
      LabelFileBrowse:insertData(LabelFile.FilePrefix,                    "LEFT").
      LabelFileBrowse:insertData(STRING(LabelFile.IsTempLabel, "Yes/No"), "CENTER").
      LabelFileBrowse:insertData(STRING(LabelFile.Active,      "Yes/No"), "LEFT").
            
      /* Add hidden fields */
      LabelFileBrowse:insertHiddenData("LabelFileVersionID",LabelFile.VersionID  ).
      
      LabelFileBrowse:endRow().
      
   END. /*FOR EACH LabelFile NO-LOCK */
   
   LabelFileBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LabelFileBrowse:getErrors().
   
   /* Create a new frame */
   LabelFileBrowseFrame           = NEW pageFrame().
   LabelFileBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LabelFileBrowseFrame:FormAction="dbLabelFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LabelFileBrowseFrame:formOpen("labelfile_browse_form").
   
   /* Start the Frame Header */
   LabelFileBrowseFrame:insertSpacer(5).
   LabelFileBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LabelFileBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LabelFileBrowseFrame:frameClose().
   LabelFileBrowseFrame:insertSpacer(10).
   
   LabelFileBrowseFrame:insertHiddenField("labelfile_browse_scroll","").
   LabelFileBrowseFrame:insertHiddenField("LabelFileID","").
   LabelFileBrowseFrame:insertHiddenField("LabelFileVersionID","").
   LabelFileBrowseFrame:insertHiddenField("form_name","labelfile_browse_form").
   LabelFileBrowseFrame:insertHiddenField("popup_labelfilehistory_browse","").
   LabelFileBrowseFrame:insertHiddenField("prog_name","adLabelFileAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LabelFileBrowseFrame}
   
   LabelFileBrowseFrame:formClose().
   
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   LabelFileBrowseButtons           = NEW buttonBar().
   LabelFileBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   LabelFileBrowseButtons:addButton("labelfile_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewLabelFileDetails('labelfile_details_form');",
                                         (IF intSelectedLabelFile > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:      
      LabelFileBrowseButtons:addButton("labelfile_browse_form_btn_create",
                                            fTL("Create"),
                                            "createLabelFile('labelfile_details_form');",
                                            "").
   END.
   
   LabelFileBrowseButtons:addButton("labelfile_browse_form_btn_history",
                                         fTL("History"),
                                         "viewLabelFileHistory('labelfile_browse_form');",
                                         (IF intSelectedLabelFile > 0 THEN "" ELSE "Disabled")).

   LabelFileBrowseButtons:closeBar().  
   LabelFileBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pLabelFileDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "labelfile_details_form"}/*TODO PUT CORRECT VALUES*/
   ASSIGN chrDisplayFieldList  = "LabelFileID,LabelTypeID,LabelName,OperationTypeID,FilePath,FileName,FilePrefix,Height," + 
                                "IsTempLabel,Width,DeleteBlankBarCodes,DeleteWhenPrinted,Active"
          chrEditFieldList     = "LabelTypeID,LabelName,OperationTypeID,FilePath,FileName,FilePrefix,Height," +
                                "IsTempLabel,Width,DeleteBlankBarCodes,DeleteWhenPrinted,Active"
          chrNewFieldList      = "LabelTypeID,LabelName,OperationTypeID,FilePath,FileName,FilePrefix,Height," +
                                "IsTempLabel,Width,DeleteBlankBarCodes,DeleteWhenPrinted,Active"
          chrRequiredFieldList = "LabelTypeID,LabelName,OperationTypeID,FilePath,FileName,FilePrefix,Height," +
                                "IsTempLabel,Width,DeleteBlankBarCodes,DeleteWhenPrinted,Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
          /*"DelimiterASCIIValue:INTEGER".*/

   
   LabelFileDetailsForm = NEW dataForm("labelfile_details_form").
   LabelFileDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LabelFileDetailsForm:FormAction = "dbLabelFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LabelFileDetailsForm:FormWidth   = 580.
   LabelFileDetailsForm:FormHeight  = 420.
   LabelFileDetailsForm:FormTitle   = "Label File Details".
   LabelFileDetailsForm:FormType    = "large".
   
   /* Column Layout */
   LabelFileDetailsForm:insertPaddingColumn(20).
   
   /* Fields */
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Label File ID")).
   LabelFileDetailsForm:insertTextField("LabelFileID", "", 250, TRUE).  
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Label Type ID")).
   LabelFileDetailsForm:insertComboField("LabelTypeID", "", 250, TRUE). 
   FOR EACH LabelType NO-LOCK 
   WHERE LabelType.Active:    
      LabelFileDetailsForm:insertComboPairs("LabelTypeID", STRING(LabelType.LabelTypeID), LabelType.TypeName).
   END.
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Label Name")).
   LabelFileDetailsForm:insertTextField("LabelName", "", 250, TRUE). 
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Operation Type")).
   LabelFileDetailsForm:insertComboField("OperationTypeID", "", 250, TRUE). 
   FOR EACH OperationType NO-LOCK 
   WHERE OperationType.Active:    
      LabelFileDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END. 
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("File Path")).
   LabelFileDetailsForm:insertTextField("FilePath", "", 250, TRUE).  
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("File Name")).
   LabelFileDetailsForm:insertTextField("FileName", "", 250, TRUE).  
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("File Prefix")).
   LabelFileDetailsForm:insertTextField("FilePrefix", "", 250, TRUE). 
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Height")).
   LabelFileDetailsForm:insertTextField("Height", "", 250, TRUE). 
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Width")).
   LabelFileDetailsForm:insertTextField("Width", "", 250, TRUE). 
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Temp Label")).
   LabelFileDetailsForm:insertComboField("IsTempLabel", "no", 100, TRUE).  
   LabelFileDetailsForm:insertComboPairs("IsTempLabel", "yes", "Yes").
   LabelFileDetailsForm:insertComboPairs("IsTempLabel", "no",  "No").
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Delete Blank Barcodes")).
   LabelFileDetailsForm:insertComboField("DeleteBlankBarCodes", "no", 100, TRUE).  
   LabelFileDetailsForm:insertComboPairs("DeleteBlankBarCodes", "yes", "Yes").
   LabelFileDetailsForm:insertComboPairs("DeleteBlankBarCodes", "no",  "No"). 
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Delete When Printed")).
   LabelFileDetailsForm:insertComboField("DeleteWhenPrinted", "no", 100, TRUE).  
   LabelFileDetailsForm:insertComboPairs("DeleteWhenPrinted", "yes", "Yes").
   LabelFileDetailsForm:insertComboPairs("DeleteWhenPrinted", "no",  "No").
   
   LabelFileDetailsForm:startRow().
   LabelFileDetailsForm:insertLabel(fTL("Active")).
   LabelFileDetailsForm:insertComboField("Active", "no", 100, TRUE).  
   LabelFileDetailsForm:insertComboPairs("Active", "yes", "Active").
   LabelFileDetailsForm:insertComboPairs("Active", "no",  "Not Active").


   {webGetOptionalFormFields.i pLabelFileDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LabelFileDetailsForm:insertHiddenField("labelfile_browse_scroll", "").
   LabelFileDetailsForm:insertHiddenField("form_name", "labelfile_details_form").
   LabelFileDetailsForm:insertHiddenField("prog_name", "adLabelFileAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LabelFileDetailsForm}
   
   /* Create Button Bar */
   LabelFileDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataCreates THEN
      LabelFileDetailsButtons:addButton("labelfile_details_form_btn_save", 
                                       fTL("Save"), 
                                       "updateLabelFile('labelfile_details_form');").
   LabelFileDetailsButtons:addButton("labelfile_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "cancelUpdate('UserCancelled','process_mode'); disablePopup('labelfile_details_form_popup');").
   LabelFileDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LabelFileDetailsForm:FormButtons = LabelFileDetailsButtons.
   
   LabelFileDetailsForm:endForm(). 
   
   LabelFileDetailsForm:displayForm(). 
  
END PROCEDURE.

PROCEDURE pLabelFileHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {webGetWebForm.i "labelfilehistory_details_form"}

   LabelFileHistoryBrowseForm = NEW dataForm("labelfilehistory_browse_form").
   LabelFileHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   LabelFileHistoryBrowseForm:FormWidth  = 860.
   LabelFileHistoryBrowseForm:FormHeight = 530.
   LabelFileHistoryBrowseForm:FormTitle  = fTL("Label File History") + (IF AVAILABLE LabelFile THEN " : " 
                                                                                + STRING(LabelFile.LabelFileID) ELSE "").
   LabelFileHistoryBrowseForm:FormType   = "xxl_large".   
   LabelFileHistoryBrowse                = NEW browseTable("labelfilehistory_browse").
   LabelFileHistoryBrowse:BrowseWidth    = 840.
   LabelFileHistoryBrowse:BrowseHeight   = 490.
   
   LabelFileHistoryBrowse:insertColumn(fTL("History ID"), 60, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LabelFileHistory}

   LabelFileHistoryBrowse:insertColumn(fTL("Label Name"),  140, "CHARACTER", "LEFT", FALSE).
   LabelFileHistoryBrowse:insertColumn(fTL("File Path"),   150, "CHARACTER", "LEFT", FALSE).
   LabelFileHistoryBrowse:insertColumn(fTL("File Name"),   145, "CHARACTER", "LEFT", FALSE). 
   LabelFileHistoryBrowse:insertColumn(fTL("Active"),      50,  "LOGICAL",   "LEFT", FALSE).
   LabelFileHistoryBrowse:insertColumn(fTL("User"),        80,  "CHARACTER",         FALSE).
   LabelFileHistoryBrowse:insertColumn(fTL("Created"),     135, "CHARACTER",         FALSE).
   
   
   LabelFileHistoryBrowse:StartBody().
   
   IF AVAILABLE LabelFile THEN
   DO:
       
      FOR EACH LabelFileHistory NO-LOCK
         WHERE LabelFileHistory.LabelFileID = LabelFile.LabelFileID
         BY    LabelFileHistory.Created DESCENDING:
             
         FIND FIRST GateUser OF LabelFileHistory NO-LOCK NO-ERROR.

/*         FIND FIRST GateUser NO-LOCK                                         */
/*            WHERE GateUser.GateUserID = LabelFileHistory.GateUserID NO-ERROR.*/

         LabelFileHistoryBrowse:startRow(LabelFileHistory.LabelFileHistoryID, "selectLabelFileHistoryRow(this,"
                                            + '"' + STRING(LabelFileHistory.LabelFileHistoryID)
                                            + '","adFileLabelAdmin.p","labelfilehistory_browse_form"' + ");", "").

         LabelFileHistoryBrowse:insertData(LabelFileHistory.LabelFileHistoryID).

         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i LabelFileHistory}

         LabelFileHistoryBrowse:insertData(STRING(LabelFileHistory.LabelName),                     "", "LEFT").
         LabelFileHistoryBrowse:insertData(STRING(LabelFileHistory.FilePath),                      "", "LEFT").
         LabelFileHistoryBrowse:insertData(STRING(LabelFileHistory.FileName),                      "", "LEFT").
         LabelFileHistoryBrowse:insertData(STRING(LabelFileHistory.Active, "Yes/No"),              "", "LEFT").
         LabelFileHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE       "", "LEFT").
         LabelFileHistoryBrowse:insertData(fDisplayDate&Time(LabelFileHistory.Created,"y/m/d H:M:S") , "LEFT").
         
         LabelFileHistoryBrowse:endRow().

      END. /* FOR EACH LabelFileHistory OF FileMaster NO-LOCK */
   END. /*IF AVAILABLE LabelFileHistory THEN*/
   
   LabelFileHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + LabelFileHistoryBrowse:getErrors().

   LabelFileHistoryBrowseForm:insertHiddenField("popup_labelfilehistory_browse","").
   LabelFileHistoryBrowseForm:insertHiddenField("LabelFileHistoryID","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LabelFileHistoryBrowseForm}
   
   /* Create Button Bar */
   LabelFileHistoryBrowseButtons = NEW buttonBar().
   
   LabelFileHistoryBrowseButtons:addButton("labelfilehistory_browse_form_btn_details",
                                            fTL("Details"),
                                            "viewLabelFileHistoryDetails('labelfilehistory_details_form');",
                                            (IF intSelectedLabelFileHistory > 0 THEN "" ELSE "Disabled")).
          
   LabelFileHistoryBrowseButtons:addButton("labelfilehistory_browse_form_btn_cancel",
                                            fTL("Cancel"),
                                            "disablePopup('labelfilehistory_browse_form_popup');").
   
   LabelFileHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LabelFileHistoryBrowseForm:FormBrowse  = LabelFileHistoryBrowse.
   LabelFileHistoryBrowseForm:FormButtons = LabelFileHistoryBrowseButtons.
   LabelFileHistoryBrowseForm:endForm(). 
   
   LabelFileHistoryBrowseForm:displayForm().
   
END PROCEDURE.


PROCEDURE pLabelFileHistoryDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "labelfilehistory_details_form"}
   ASSIGN chrDisplayFieldList  ="LabelFileHistoryID,LabelFileID,LabelTypeID,LabelName,OperationTypeID,FilePath,FileName,FilePrefix,Height," + 
                                "UpdateOperationTypeID,Width,IsTempLabel,DeleteBlankBarCodes,DeleteWhenPrinted,Active" 
          chrEditFieldList     = ""
          chrNewFieldList      = ""
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   LabelFileHistoryDetailsForm = NEW dataForm("labelfilehistory_details_form").
   LabelFileHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LabelFileHistoryDetailsForm:FormAction = "dbLabelFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LabelFileHistoryDetailsForm:FormWidth   = 580.
   LabelFileHistoryDetailsForm:FormHeight  = 420.
   LabelFileHistoryDetailsForm:FormTitle   = "Label File History Details".
   LabelFileHistoryDetailsForm:FormType    = "large".
   
   LabelFileHistoryDetailsForm:insertPaddingColumn(20).         
   

   /* Fields */
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Label File History ID")).
   LabelFileHistoryDetailsForm:insertTextField("LabelFileHistoryID", "", 250, TRUE).
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Label File ID")).
   LabelFileHistoryDetailsForm:insertTextField("LabelFileID", "", 250, TRUE).  
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Label Type ID")).
   LabelFileHistoryDetailsForm:insertComboField("LabelTypeID", "", 250, TRUE). 
   FOR EACH LabelType NO-LOCK 
   WHERE LabelType.Active:    
      LabelFileHistoryDetailsForm:insertComboPairs("LabelTypeID", STRING(LabelType.LabelTypeID), LabelType.TypeName).
   END.
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Update Operation Type")).
   LabelFileHistoryDetailsForm:insertComboField("UpdateOperationTypeID", "", 250, TRUE). 
   FOR EACH OperationType NO-LOCK 
   WHERE OperationType.Active:    
      LabelFileHistoryDetailsForm:insertComboPairs("UpdateOperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END.
      
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Label Name")).
   LabelFileHistoryDetailsForm:insertTextField("LabelName", "", 250, TRUE).  
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Operation Type")).
   LabelFileHistoryDetailsForm:insertComboField("OperationTypeID", "", 250, TRUE). 
   FOR EACH OperationType NO-LOCK 
   WHERE OperationType.Active:    
      LabelFileHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName).
   END.
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("File Path")).
   LabelFileHistoryDetailsForm:insertTextField("FilePath", "", 250, TRUE).  
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("File Name")).
   LabelFileHistoryDetailsForm:insertTextField("FileName", "", 250, TRUE).  
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("File Prefix")).
   LabelFileHistoryDetailsForm:insertTextField("FilePrefix", "", 250, TRUE). 
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Height")).
   LabelFileHistoryDetailsForm:insertTextField("Height", "", 250, TRUE). 
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Width")).
   LabelFileHistoryDetailsForm:insertTextField("Width", "", 250, TRUE). 
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Temp Label")).
   LabelFileHistoryDetailsForm:insertComboField("IsTempLabel", "no", 100, TRUE).  
   LabelFileHistoryDetailsForm:insertComboPairs("IsTempLabel", "yes", "Yes").
   LabelFileHistoryDetailsForm:insertComboPairs("IsTempLabel", "no",  "No").
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Delete Blank Barcodes")).
   LabelFileHistoryDetailsForm:insertComboField("DeleteBlankBarCodes", "no", 100, TRUE).  
   LabelFileHistoryDetailsForm:insertComboPairs("DeleteBlankBarCodes", "yes", "Yes").
   LabelFileHistoryDetailsForm:insertComboPairs("DeleteBlankBarCodes", "no",  "No"). 
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Delete When Printed")).
   LabelFileHistoryDetailsForm:insertComboField("DeleteWhenPrinted", "no", 100, TRUE).  
   LabelFileHistoryDetailsForm:insertComboPairs("DeleteWhenPrinted", "yes", "Yes").
   LabelFileHistoryDetailsForm:insertComboPairs("DeleteWhenPrinted", "no",  "No").
   
   LabelFileHistoryDetailsForm:startRow().
   LabelFileHistoryDetailsForm:insertLabel(fTL("Active")).
   LabelFileHistoryDetailsForm:insertComboField("Active", "no", 100, TRUE).  
   LabelFileHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   LabelFileHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").                              

   {webGetOptionalFormFields.i pLabelFileHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LabelFileHistoryDetailsForm:insertHiddenField("labelfilehistory_browse_scroll", "").
   LabelFileHistoryDetailsForm:insertHiddenField("form_name", "labelfilehistory_details_form").
   LabelFileHistoryDetailsForm:insertHiddenField("prog_name", "adLabelFileAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LabelFileHistoryDetailsForm}
   
   /* Create Button Bar */
   LabelFileHistoryDetailsButtons = NEW buttonBar().

   LabelFileHistoryDetailsButtons:addButton("labelfilehistory_details_form_btn_cancel", 
                                    fTL("Cancel"), 
                                    "disablePopup('labelfilehistory_details_form_popup');").
   LabelFileHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LabelFileHistoryDetailsForm:FormButtons = LabelFileHistoryDetailsButtons.
   
   LabelFileHistoryDetailsForm:endForm(). 
   
   LabelFileHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 


PROCEDURE pLabelFileDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
  CASE chrOption:
    
    WHEN "FieldName" THEN
    DO:
      LabelFileDetailsForm:startRow().
      LabelFileDetailsForm:insertLabel(fTL("Field Label")).
      LabelFileDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
    END. /*WHEN "FieldName" THEN*/
    
  END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF