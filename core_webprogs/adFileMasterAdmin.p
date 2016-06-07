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
     clean up will occur on deletion of the procedure. */
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Custom Includes */
{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}
{defDataMigrationVariables.i}

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedFileMaster       AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedFileMasterField  AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectFileMasterRow      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToFileMasterRow    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectFileMasterFieldRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrFileMasterID             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFileMasterFieldID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupFileMasterField     AS CHARACTER NO-UNDO.

/* Local Variables for FileMasterHistory Browse */
DEFINE VARIABLE chrPopupFileMasterHistory AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE FileMasterBrowseFrame         AS pageFrame.
DEFINE VARIABLE FileMasterBrowse              AS browseTable.
DEFINE VARIABLE FileMasterBrowseButtons       AS buttonBar.
DEFINE VARIABLE FileMasterDetailsForm         AS dataForm.
DEFINE VARIABLE FileMasterDetailsButtons      AS buttonBar.
DEFINE VARIABLE FileMasterFieldDetailsForm    AS dataForm.
DEFINE VARIABLE FileMasterFieldDetailsButtons AS buttonBar.
DEFINE VARIABLE FileMasterFieldBrowseForm     AS dataForm.
DEFINE VARIABLE FileMasterFieldBrowse         AS browseTable.
DEFINE VARIABLE FileMasterFieldBrowseButtons  AS buttonBar.


/* Objects for History Popup */
DEFINE VARIABLE FileMasterHistoryBrowse        AS browseTable.
DEFINE VARIABLE FileMasterHistoryBrowseButtons AS buttonBar.
DEFINE VARIABLE FileMasterHistoryBrowseForm    AS dataForm.

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

  {prcDataMigrationProcedures.i}
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

&IF DEFINED(EXCLUDE-pFileMasterBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileMasterBrowse Procedure 
PROCEDURE pFileMasterBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "filemaster_details_form"}
   
   FileMasterBrowse              = NEW browseTable("filemaster_browse").
   FileMasterBrowse:BrowseWidth  = 965.
   FileMasterBrowse:BrowseHeight = 455.
   FileMasterBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   FileMasterBrowse:insertColumn(fTL("ID"),          50, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileMaster}
   
   FileMasterBrowse:insertColumn(fTL("Active"),       50, "LOGICAL", FALSE).
   FileMasterBrowse:insertColumn(fTL("FileType"),     60, "CHARACTER", "left", FALSE).
   FileMasterBrowse:insertColumn(fTL("Direction"),    60, "CHARACTER", "left", FALSE).
   FileMasterBrowse:insertColumn(fTL("MasterName"),  150, "CHARACTER", "left", FALSE).
   FileMasterBrowse:insertColumn(fTL("ProgName"),    160, "CHARACTER", "left", FALSE).
   FileMasterBrowse:insertColumn(fTL("Prefix/Mask"), 150, "CHARACTER", "left", FALSE).
   FileMasterBrowse:insertColumn(fTL("Path"),        120, "CHARACTER", "left", FALSE).
   FileMasterBrowse:insertColumn(fTL("Delete?"),      60, "LOGICAL", FALSE).
   FileMasterBrowse:insertColumn(fTL("ArchiveDays"),  80, "INTEGER", FALSE).
   
   /*Body*/
   FileMasterBrowse:startBody().
   
   /* Find all File Master then sort by Active, Listing Sequence, and ID in case Sequences are the same */
   FOR EACH FileMaster NO-LOCK
      BY    FileMaster.Active DESCENDING
      BY    FileMaster.FileMasterID:
      
      FileMasterBrowse:startRow(FileMaster.FileMasterID, "selectFileMasterRow(this," + '"' 
                                                                                     + STRING(FileMaster.FileMasterID) + '"' + ");", "").
      FileMasterBrowse:insertData(STRING(FileMaster.FileMasterID)).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i FileMaster}
      
      FileMasterBrowse:insertData(STRING(FileMaster.Active, "Yes/No")).

      FIND FIRST FileType OF FileMaster NO-LOCK NO-ERROR.
      IF AVAILABLE FileType THEN
      DO:
         FileMasterBrowse:insertData(FileType.TypeCode, "left").
      END. 
      ELSE 
      DO:
         FileMasterBrowse:insertData("", "left").
      END.
      
      FileMasterBrowse:insertData(FileMaster.Direction, "left").
      FileMasterBrowse:insertData(FileMaster.MasterName, "left").
      FileMasterBrowse:insertData(FileMaster.ProgramName, "left").
      FileMasterBrowse:insertData(FileMaster.FilePrefix + FileMaster.FileMask, "left").
      FileMasterBrowse:insertData(FileMaster.FilePath, "left").
      FileMasterBrowse:insertData(STRING(FileMaster.DeleteWhenProcessed, "Yes/No")).

      FIND FIRST EmailGroup NO-LOCK
         WHERE EmailGroup.EmailGroupID = FileMaster.InternalEmailGroupID NO-ERROR.
      IF AVAILABLE EmailGroup THEN 
      DO:
         FileMasterBrowse:insertHiddenData("IntEmailGrp",STRING(EmailGroup.GroupCode)).
      END.  
      ELSE 
      DO:
         FileMasterBrowse:insertHiddenData("IntEmailGrp","").
      END.
    
      FIND FIRST EmailGroup NO-LOCK
         WHERE EmailGroup.EmailGroupID = FileMaster.ExternalEmailGroupID NO-ERROR.
      IF AVAILABLE EmailGroup THEN 
      DO:
         FileMasterBrowse:insertHiddenData("ExtEmailGrp",STRING(EmailGroup.GroupCode)).
      END. 
      ELSE 
      DO:
         FileMasterBrowse:insertHiddenData("ExtEmailGrp","").
      END.
      
      FileMasterBrowse:insertData(STRING(FileMaster.DaysToArchive)).
      
      /* Add hidden fields */
      FileMasterBrowse:insertHiddenData("FileMasterFileMask",FileMaster.FileMask).
      FileMasterBrowse:insertHiddenData("FileMasterFilePrefix",FileMaster.FilePrefix).
      FileMasterBrowse:insertHiddenData("FileMasterFileBackupPath",FileMaster.FileBackupPath).
      FileMasterBrowse:insertHiddenData("FileMasterFileRejectionPath",FileMaster.FileRejectionPath).
      FileMasterBrowse:insertHiddenData("FileMasterFileDelimiter",FileMaster.FileDelimiter).
      FileMasterBrowse:insertHiddenData("FileMasterFileDebuggingOn",STRING(FileMaster.DebuggingOn, "Yes/No")).
      FileMasterBrowse:insertHiddenData("FileMasterVersionID",FileMaster.VersionID).
      
      FileMasterBrowse:endRow().
      
   END. /* FOR EACH FileMaster NO-LOCK */
   
   FileMasterBrowse:endTable().
   chrPageBuildError = chrPageBuildError + FileMasterBrowse:getErrors().
   
   /* Create a new frame */
   FileMasterBrowseFrame           = NEW pageFrame().
   FileMasterBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   FileMasterBrowseFrame:FormAction="dbFileMasterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FileMasterBrowseFrame:formOpen("filemaster_browse_form").
   
   /* Start the Frame Header */
   FileMasterBrowseFrame:insertSpacer(5).
   FileMasterBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   FileMasterBrowse:displayBrowse().  
   
   /* End the Frame Header */
   FileMasterBrowseFrame:frameClose().
   FileMasterBrowseFrame:insertSpacer(10).
   
   /* Hidden Variables for Popup and Browse attributes */
   FileMasterBrowseFrame:insertHiddenField("popup_filemasterhistory_browse","").
   FileMasterBrowseFrame:insertHiddenField("popup_filemasterfield_browse","").
   FileMasterBrowseFrame:insertHiddenField("form_name","filemaster_browse_form").
   FileMasterBrowseFrame:insertHiddenField("prog_name","adFileMasterAdmin.p").
   FileMasterBrowseFrame:insertHiddenField("filemaster_browse_scroll","").

   /* Hidden Variables for Key values */
   FileMasterBrowseFrame:insertHiddenField("FileMasterID",chrFileMasterID).
   FileMasterBrowseFrame:insertHiddenField("FileMasterVersionID","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileMasterBrowseFrame}
   
   FileMasterBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   FileMasterBrowseButtons           = NEW buttonBar().
   FileMasterBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   FileMasterBrowseButtons:addButton("filemaster_browse_form_btn_details",
                                     fTL("Details"),
                                     "viewFileMasterDetails('filemaster_details_form');",
                                     (IF intSelectedFileMaster > 0 THEN "" ELSE "Disabled")).

   FileMasterBrowseButtons:addButton("filemaster_browse_form_btn_fields",
                                     fTL("Fields"),
                                     "viewFileMasterFieldBrowse('filemaster_browse_form');",
                                     (IF intSelectedFileMaster > 0 THEN "" ELSE "Disabled")).
                                     
   IF NOT logPreventDataCreates THEN
   DO:
      FileMasterBrowseButtons:addButton("filemaster_browse_form_btn_create",
                                        fTL("Create"),
                                        "createFileMaster('filemaster_details_form');").
                                        
      FileMasterBrowseButtons:addButton("filemaster_browse_form_btn_applinks",
                                        fTL("App Links"),
                                        "viewAppLinkBrowse('filemaster_browse_form','FileMaster');",
                                        "Disabled").
                                                                                                                        
   END. /* IF NOT logPreventDataCreates */                                     

   FileMasterBrowseButtons:addButton("filemaster_browse_form_btn_history",
                                     fTL("History"),
                                     "viewFileMasterHistoryBrowse('filemaster_browse_form');",
                                     (IF intSelectedFileMaster > 0 THEN "" ELSE "Disabled")). 

/*   FileMasterBrowseButtons:addButton("filemaster_browse_form_btn_delete",*/
/*                                     fTL("Delete"),                      */
/*                                     "confirmDeleteFileMaster();",       */
/*                                     "Disabled").                        */
                                     
   FileMasterBrowseButtons:closeBar().  
   FileMasterBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileMasterDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileMasterDetails Procedure 
PROCEDURE pFileMasterDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE logSuccess     AS LOGICAL NO-UNDO.
   DEFINE VARIABLE hdlTableBuffer AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hdlTableQuery  AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hdlFieldBuffer AS HANDLE  NO-UNDO.
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "filemaster_details_form"}
   
   ASSIGN chrDisplayFieldList  = "FileMasterID,FileTypeID,Active,Direction,MasterName,ProgramName,FileMask,FilePrefix,"
                                   + "FilePath,FileBackupPath,FileRejectionPath,FileDelimiterID,AllowUploadFilesViaWebInterface,"
                                   + "InternalEmailGroupID,ExternalEmailGroupID,DebuggingOn,DaysToArchive,AddCarriageReturnAfterEachLine,"
                                   + "AddLineFeedAfterEachLine,EdiSenderID,EdiReceiverID,EdiSenderName,EdiReceiverName"
          chrEditFieldList     = "FileTypeID,Active,Direction,MasterName,ProgramName,FileMask,FilePrefix,FilePath,"
                                   + "FileBackupPath,FileRejectionPath,FileDelimiterID,AllowUploadFilesViaWebInterface,"
                                   + "DeleteWhenProcessed,InternalEmailGroupID,ExternalEmailGroupID,DebuggingOn,DaysToArchive,"
                                   + "AddCarriageReturnAfterEachLine,AddLineFeedAfterEachLine,EdiSenderID,EdiReceiverID,"
                                   + "EdiSenderName,EdiReceiverName"
          chrNewFieldList      = "FileTypeID,Active,Direction,MasterName,ProgramName,FileMask,FilePrefix,FilePath,FileBackupPath,"
                                   + "FileRejectionPath,FileDelimiterID,AllowUploadFilesViaWebInterface,DeleteWhenProcessed,"
                                   + "InternalEmailGroupID,ExternalEmailGroupID,DebuggingOn,DaysToArchive,AddCarriageReturnAfterEachLine,"
                                   + "AddLineFeedAfterEachLine,EdiSenderID,EdiReceiverID,EdiSenderName,EdiReceiverName"
          chrRequiredFieldList = "FileTypeID,Active,Direction,MasterName,DeleteWhenProcessed,"
                                   + "InternalEmailGroupID,ExternalEmailGroupID,DebuggingOn"
          chrExtraFieldList    = ""
          chrValidateFieldList = "DaysToArchive:INTEGER". 
   
   FileMasterDetailsForm           = NEW dataForm("filemaster_details_form").
   FileMasterDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileMasterDetailsForm:FormAction = "dbFileMasterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileMasterDetailsForm:FormWidth  = 860.
   FileMasterDetailsForm:FormHeight = 530.
   FileMasterDetailsForm:FormTitle  = "File Master Details".
   FileMasterDetailsForm:FormType   = "xxl_large".
   
   /* Column Layout */
   FileMasterDetailsForm:insertPaddingColumn(70).
   FileMasterDetailsForm:insertColumn(170).
   FileMasterDetailsForm:insertColumn(200).
   FileMasterDetailsForm:insertColumn(170).
   FileMasterDetailsForm:insertColumn(200).
   
   /* Fields */
   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel("ID").
   FileMasterDetailsForm:insertTextField("FileMasterID", "", 80, TRUE).  

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel("File Type").
   FileMasterDetailsForm:insertComboField("FileTypeID", "", 110, TRUE). 
   FOR EACH FileType NO-LOCK 
      WHERE FileType.Active
      BY    FileType.TypeCode:
      
      FileMasterDetailsForm:insertComboPairs("FileTypeID", STRING(FileType.FileTypeID), FileType.TypeName).
   END.

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Active")). 
   FileMasterDetailsForm:insertComboField("Active", "no", 110, TRUE).  
   FileMasterDetailsForm:insertComboPairs("Active", "yes", "Active").
   FileMasterDetailsForm:insertComboPairs("Active", "no",  "Not Active").

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Direction")). 
   FileMasterDetailsForm:insertComboField("Direction", "In", 110, TRUE).  
   FileMasterDetailsForm:insertComboPairs("Direction", "Out", "Out").
   FileMasterDetailsForm:insertComboPairs("Direction", "In",  "In").

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Master Name")).
   FileMasterDetailsForm:insertTextField("MasterName", "", 310, TRUE).

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Program Name")).
   FileMasterDetailsForm:insertTextField("ProgramName", "", 310, TRUE).
      
   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("File Mask")).
   FileMasterDetailsForm:insertTextField("FileMask", "", 310, TRUE).
   
   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("File Prefix")).
   FileMasterDetailsForm:insertTextField("FilePrefix", "", 310, TRUE).

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("File Path")).
   FileMasterDetailsForm:insertTextField("FilePath", "", 310, TRUE).

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("File Backup Path")).
   FileMasterDetailsForm:insertTextField("FileBackupPath", "", 310, TRUE).

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("File Rejection Path")).
   FileMasterDetailsForm:insertTextField("FileRejectionPath", "", 310, TRUE).

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("File Delimiter")).
   FileMasterDetailsForm:insertComboField("FileDelimiterID", "", 150, TRUE). /*TODO file delimiter*/
   FOR EACH FileDelimiter NO-LOCK:
      FileMasterDetailsForm:insertComboPairs("FileDelimiterID", STRING(FileDelimiter.FileDelimiterID),FileDelimiter.DelimiterName).
   END.

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Allow Upload Files Via Web")). 
   FileMasterDetailsForm:insertComboField("AllowUploadFilesViaWebInterface", "no", 110, TRUE).  
   FileMasterDetailsForm:insertComboPairs("AllowUploadFilesViaWebInterface", "yes", "Yes").
   FileMasterDetailsForm:insertComboPairs("AllowUploadFilesViaWebInterface", "no",  "No").

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Delete When Processed")). 
   FileMasterDetailsForm:insertComboField("DeleteWhenProcessed", "no", 110, TRUE).  
   FileMasterDetailsForm:insertComboPairs("DeleteWhenProcessed", "yes", "Yes").
   FileMasterDetailsForm:insertComboPairs("DeleteWhenProcessed", "no",  "No").

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Internal Email Group")). 
   FileMasterDetailsForm:insertComboField("InternalEmailGroupID", "", 250, TRUE).

   FileMasterDetailsForm:insertComboPairs("InternalEmailGroupID","0", "None Selected...").
   FOR EACH EmailGroup NO-LOCK 
      WHERE EmailGroup.Active
      AND   (EmailGroup.GroupType = "Internal" OR EmailGroup.GroupType = "Both")
      BY    EmailGroup.ListingSequence:
      
      FileMasterDetailsForm:insertComboPairs("InternalEmailGroupID", STRING(EmailGroup.EmailGroupID), EmailGroup.GroupName).

   END.
   
   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("External Email Group")). 
   FileMasterDetailsForm:insertComboField("ExternalEmailGroupID", "", 250, TRUE). 
   FileMasterDetailsForm:insertComboPairs("ExternalEmailGroupID","0", "None Selected...").
   FOR EACH EmailGroup NO-LOCK 
      WHERE EmailGroup.Active
      AND   (EmailGroup.GroupType = "External" OR EmailGroup.GroupType = "Both")
      BY    EmailGroup.ListingSequence:
      
      FileMasterDetailsForm:insertComboPairs("ExternalEmailGroupID", STRING(EmailGroup.EmailGroupID), EmailGroup.GroupName).

   END.

   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Debug Mode")). 
   FileMasterDetailsForm:insertComboField("DebuggingOn", "no", 110, TRUE).  
   FileMasterDetailsForm:insertComboPairs("DebuggingOn", "yes", "Active").
   FileMasterDetailsForm:insertComboPairs("DebuggingOn", "no",  "Not Active").
   
   /*   FileMasterDetailsForm:startRow().*/
   FileMasterDetailsForm:insertLabel(fTL("EDI Sender ID")).
   FileMasterDetailsForm:insertTextField("EdiSenderID", "", 150, TRUE). 
   
   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Archive Days")).
   FileMasterDetailsForm:insertTextField("DaysToArchive", "", 80, TRUE).
   
   /*   FileMasterDetailsForm:startRow().*/
   FileMasterDetailsForm:insertLabel(fTL("EDI Sender")).
   FileMasterDetailsForm:insertTextField("EdiSenderName", "", 150, TRUE).  
   
   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Line Feed After Each Line")). 
   FileMasterDetailsForm:insertComboField("AddLineFeedAfterEachLine", "no", 110, TRUE).  
   FileMasterDetailsForm:insertComboPairs("AddLineFeedAfterEachLine", "yes", "Yes").
   FileMasterDetailsForm:insertComboPairs("AddLineFeedAfterEachLine", "no",  "No").
   
   /*   FileMasterDetailsForm:startRow().*/
   FileMasterDetailsForm:insertLabel(fTL("EDI ReceiverID")).
   FileMasterDetailsForm:insertTextField("EdiReceiverID", "", 150, TRUE). 
   
   FileMasterDetailsForm:startRow().
   FileMasterDetailsForm:insertLabel(fTL("Carriage Return After Each Line")). 
   FileMasterDetailsForm:insertComboField("AddCarriageReturnAfterEachLine", "no", 110, TRUE).  
   FileMasterDetailsForm:insertComboPairs("AddCarriageReturnAfterEachLine", "yes", "Yes").
   FileMasterDetailsForm:insertComboPairs("AddCarriageReturnAfterEachLine", "no",  "No").
   
   /*   FileMasterDetailsForm:startRow().*/
   FileMasterDetailsForm:insertLabel(fTL("EDI Receiver")).
   FileMasterDetailsForm:insertTextField("EdiReceiverName", "", 150, TRUE). 
   
   {webGetOptionalFormFields.i pFileMasterDetailsFields}
   
   /* Add Hidden Fields*/
   FileMasterDetailsForm:insertHiddenField("filemaster_browse_scroll", "").
   FileMasterDetailsForm:insertHiddenField("form_name", "filemaster_details_form").
   FileMasterDetailsForm:insertHiddenField("prog_name", "adFileMasterAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileMasterDetailsForm}
   
   /* Create Button Bar */
   FileMasterDetailsButtons = NEW buttonBar().
   
   IF NOT logPreventDataUpdates THEN
      FileMasterDetailsButtons:addButton("filemaster_details_form_btn_save", 
                                         fTL("Save"), 
                                         "updateFileMaster('filemaster_details_form');").
   
   FileMasterDetailsButtons:addButton("filemaster_details_form_btn_cancel", 
                                      fTL("Cancel"), 
                                      "cancelUpdate('UserCancelled','process_mode'); disablePopup('filemaster_details_form_popup');").
   
   FileMasterDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileMasterDetailsForm:FormButtons = FileMasterDetailsButtons.
   
   FileMasterDetailsForm:endForm(). 
   
   FileMasterDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileMasterDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileMasterDetailsFields Procedure 
PROCEDURE pFileMasterDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         FileMasterDetailsForm:startRow().
         FileMasterDetailsForm:insertLabel(fTL("Field Label")).
         FileMasterDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
      /* This will be held in customer specific code repository */
      {adFileMasterAdmin_filemaster_details_form.i}
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileMasterFieldBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileMasterFieldBrowse Procedure 
PROCEDURE pFileMasterFieldBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "filemasterfield_browse_form"}

   FileMasterFieldBrowseForm = NEW dataForm("filemasterfield_browse_form").
   FileMasterFieldBrowseForm:WebStream = STREAM WebStream:HANDLE.

   /* Insert a form */
   FileMasterFieldBrowseForm:FormAction="dbFileMasterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
/*    FileMasterFieldBrowseForm:formOpen("filemasterfield_browse_form"). */
      
   /* Setup */
   FileMasterFieldBrowseForm:FormWidth  = 860.
   FileMasterFieldBrowseForm:FormHeight = 530.
   FileMasterFieldBrowseForm:FormTitle  = fTL("File Master Field") + (IF AVAILABLE FileMaster THEN " : " 
                                                                                + FileMaster.MasterName ELSE "").
   FileMasterFieldBrowseForm:FormType   = "xxl_large".
   
   FileMasterFieldBrowse = NEW browseTable("filemasterfield_browse").
   FileMasterFieldBrowse:BrowseWidth  = 840.
   FileMasterFieldBrowse:BrowseHeight = 490.
   
   /* Add in the ID as first Column */
   FileMasterFieldBrowse:insertColumn(fTL("Field ID"),        60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileMasterField}
   FileMasterFieldBrowse:insertColumn(fTL("Position in File"),  90, "INTEGER",   "right", FALSE).
   FileMasterFieldBrowse:insertColumn(fTL("Table Name"),       150, "CHARACTER", "left", FALSE).
   FileMasterFieldBrowse:insertColumn(fTL("Field Name"),       150, "CHARACTER", "left", FALSE).
   FileMasterFieldBrowse:insertColumn(fTL("Column Width"),      90, "INTEGER", "right", FALSE).
   FileMasterFieldBrowse:insertColumn(fTL("Mandatory Field"),  100, "LOGICAL", FALSE).
   FileMasterFieldBrowse:insertColumn(fTL("Unique Index"),      80, "LOGICAL", FALSE).
   FileMasterFieldBrowse:insertColumn(fTL("Active"),            60, "LOGICAL", FALSE).
   
   /*Body*/
   FileMasterFieldBrowse:startBody().
   
   IF AVAILABLE FileMaster THEN
   DO:
      /* Find all File Master Field then sort by Active, Listing Sequence, and ID in case Sequences are the same */
      FOR EACH FileMasterField NO-LOCK
         WHERE FileMasterField.FileMasterID = FileMaster.FileMasterID
         BY    FileMasterField.Active DESCENDING 
         BY    FileMasterField.FileMasterFieldID:
         
         FileMasterFieldBrowse:startRow(FileMasterField.FileMasterFieldID, "selectFileMasterFieldRow(this," + '"' + STRING(FileMasterField.FileMasterFieldID) + '",' + '"' + FileMasterField.TableName + '"' + ");", "").
         FileMasterFieldBrowse:insertData(FileMasterField.FileMasterFieldID).
         
         /* Add in Optional & Customer Specific field according to the ProcessOptionField associated with this ProcessEvent */
         {webGetOptionalBrowseFields.i FileMasterField}

         FileMasterFieldBrowse:insertData(STRING(FileMasterField.PositionInFile), "right").
         FileMasterFieldBrowse:insertData(FileMasterField.TableName, "left").
         FileMasterFieldBrowse:insertData(FileMasterField.FieldName, "left").
         FileMasterFieldBrowse:insertData(STRING(FileMasterField.BrowseColumnWidth), "right").
         FileMasterFieldBrowse:insertData(STRING(FileMasterField.IsMandatoryField, "Yes/No")).
         FileMasterFieldBrowse:insertData(STRING(FileMasterField.IsUniqueValue, "Yes/No")).
         FileMasterFieldBrowse:insertData(STRING(FileMasterField.Active, "Yes/No")).
         
         /* Add hidden field */
         FileMasterFieldBrowse:insertHiddenData("FileMasterFieldVersionID",FileMasterField.VersionID).
         
         FileMasterFieldBrowse:endRow().
         
      END. /* FOR EACH FileMasterField NO-LOCK */
   END. /*IF AVAILABLE FileMaster THEN*/
   
   FileMasterFieldBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + FileMasterFieldBrowse:getErrors().

   FileMasterFieldBrowseForm:insertHiddenField("filemaster_browse_scroll","").
   FileMasterFieldBrowseForm:insertHiddenField("FileMasterFieldID","").
   FileMasterFieldBrowseForm:insertHiddenField("FileMasterFieldVersionID","").
   FileMasterFieldBrowseForm:insertHiddenField("FileMasterFieldTableName","").
   FileMasterFieldBrowseForm:insertHiddenField("form_name","filemaster_browse_form").
   FileMasterFieldBrowseForm:insertHiddenField("prog_name","adFileMasterAdmin.p").
   FileMasterFieldBrowseForm:insertHiddenField("popup_filemasterfield_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileMasterFieldBrowseForm}
   
   /* Create Button Bar */
   FileMasterFieldBrowseButtons = NEW buttonBar().

   FileMasterFieldBrowseButtons:addButton("filemasterfield_browse_form_btn_details",
                                          fTL("Details"),
                                          "viewFileMasterFieldDetails('filemasterfield_details_form');",
                                          (IF intSelectedFileMasterField > 0 THEN "" ELSE "Disabled")).

   FileMasterFieldBrowseButtons:addButton("filemasterfield_browse_form_btn_create",
                                          fTL("Create"),
                                          "createFileMasterField('filemasterfield_details_form');",
                                          "").  

/*   FileMasterFieldBrowseButtons:addButton("filemasterfield_browse_form_btn_delete",                    */
/*                                          fTL("Delete"),                                               */
/*                                          "confirmDeleteFileMasterField();",                           */
/*                                          (IF intSelectedFileMasterField > 0 THEN "" ELSE "Disabled")).*/

   FileMasterFieldBrowseButtons:addButton("filemasterfield_browse_form_btn_cancel",
                                          fTL("Cancel"),
                                          "disablePopup('filemasterfield_browse_form_popup');").
   
   FileMasterFieldBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileMasterFieldBrowseForm:FormBrowse  = FileMasterFieldBrowse.
   FileMasterFieldBrowseForm:FormButtons = FileMasterFieldBrowseButtons.
   FileMasterFieldBrowseForm:endForm(). 
   
   FileMasterFieldBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileMasterFieldDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileMasterFieldDetails Procedure 
PROCEDURE pFileMasterFieldDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE logSuccess     AS LOGICAL NO-UNDO.
   DEFINE VARIABLE hdlTableBuffer AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hdlTableQuery  AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hdlFieldBuffer AS HANDLE  NO-UNDO.
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "filemasterfield_details_form"}
   
   ASSIGN chrDisplayFieldList  = "FileMasterFieldID,TableName,FieldName,IsUniqueValue,PositionInFile,Active,BrowseColumnWidth"
                                    + ",IsMandatoryField"
          chrEditFieldList     = "TableName,FieldName,IsUniqueValue,PositionInFile,Active,BrowseColumnWidth,IsMandatoryField"
          chrNewFieldList      = "TableName,FieldName,IsUniqueValue,PositionInFile,Active,BrowseColumnWidth,IsMandatoryField"
          chrRequiredFieldList = "TableName,FieldName,IsUniqueValue,PositionInFile,Active,BrowseColumnWidth,IsMandatoryField"
          chrExtraFieldList    = ""
          chrValidateFieldList = "PositionInFile:INTEGER".
   
   FileMasterFieldDetailsForm           = NEW dataForm("filemasterfield_details_form").
   FileMasterFieldDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   FileMasterFieldDetailsForm:FormAction = "dbFileMasterUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   FileMasterFieldDetailsForm:FormWidth  = 460.
   FileMasterFieldDetailsForm:FormHeight = 300.
   FileMasterFieldDetailsForm:FormTitle  = "File Master Field Details".
   FileMasterFieldDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   FileMasterFieldDetailsForm:insertPaddingColumn(50).
   FileMasterFieldDetailsForm:insertColumn(130).
   FileMasterFieldDetailsForm:insertColumn(150).
   FileMasterFieldDetailsForm:insertColumn(20).
   FileMasterFieldDetailsForm:insertColumn(4).
   FileMasterFieldDetailsForm:insertColumn(100).
   
   /* Field */
   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel("Field ID").
   FileMasterFieldDetailsForm:insertTextField("FileMasterFieldID", "", 110, TRUE). 

   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel("Position in File").
   FileMasterFieldDetailsForm:insertTextField("PositionInFile", "", 110, TRUE).

   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel("Browse Column Width").
   FileMasterFieldDetailsForm:insertTextField("BrowseColumnWidth", "", 110, TRUE).
   
   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel("Table Name").
   FileMasterFieldDetailsForm:insertComboField("TableName", "", 250, TRUE,'populateTableFieldsCombo(this.value)').
   /* Adding Blank Line to have unselected */
   FileMasterFieldDetailsForm:insertComboPairs("TableName", "", "None Selected...").
   FOR EACH SystemDb NO-LOCK
      WHERE SystemDb.ExcludeDataMigration = NO:
      /* SystemDb.SystemDbName */

      CREATE BUFFER hdlTableBuffer FOR TABLE SystemDb.SystemDbName + "._file" NO-ERROR.
      CREATE QUERY  hdlTableQuery NO-ERROR.
      hdlTableQuery:SET-BUFFERS(hdlTableBuffer).

      logSuccess = hdlTableQuery:QUERY-PREPARE("FOR EACH " + SystemDb.SystemDbName + "._file WHERE " + SystemDb.SystemDbName 
                                                  + "._file._tbl-type = 'T'").
      IF NOT logSuccess OR ERROR-STATUS:ERROR THEN
      DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
         chrPageBuildError = chrPageBuildError + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
      END.
      
      IF logSuccess THEN
      DO:
         hdlTableQuery:QUERY-OPEN().
         hdlTableQuery:GET-FIRST(NO-LOCK).
         
         REPEAT WHILE NOT hdlTableQuery:QUERY-OFF-END:
            
            FileMasterFieldDetailsForm:insertComboPairs("TableName",
                                                        SystemDb.SystemDbName + "." 
                                                          + STRING(hdlTableBuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE), 
                                                          /* + "." + STRING(hdlTableBuffer:RECID), */
                                                        STRING(hdlTableBuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE)).
            hdlTableQuery:GET-NEXT(NO-LOCK).

         END.

      END. /* logSuccess */

      hdlTableBuffer:BUFFER-RELEASE().
      hdlTableQuery:QUERY-CLOSE().

      IF ERROR-STATUS:ERROR THEN
         DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
            chrPageBuildError = chrPageBuildError + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
      END.

      /* Clearing up */
      DELETE OBJECT hdlTableBuffer NO-ERROR.
      DELETE OBJECT hdlTableQuery  NO-ERROR.
      DELETE OBJECT hdlFieldBuffer NO-ERROR.

   END.

   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel("Field Name").
   FileMasterFieldDetailsForm:insertComboField("FieldName", "", 250, TRUE).
   /* Adding Blank Line to have unselected */
   FileMasterFieldDetailsForm:insertComboPairs("FieldName","","None Selected..."). 

   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel(fTL("Unique Index")). 
   FileMasterFieldDetailsForm:insertComboField("IsUniqueValue", "no", 110, TRUE).  
   FileMasterFieldDetailsForm:insertComboPairs("IsUniqueValue", "yes", "Unique").
   FileMasterFieldDetailsForm:insertComboPairs("IsUniqueValue", "no",  "Not Unique").

   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel(fTL("Active")). 
   FileMasterFieldDetailsForm:insertComboField("Active", "", 110, TRUE).  
   FileMasterFieldDetailsForm:insertComboPairs("Active", "yes", "Active").
   FileMasterFieldDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   FileMasterFieldDetailsForm:startRow().
   FileMasterFieldDetailsForm:insertLabel(fTL("Mandatory Field")). 
   FileMasterFieldDetailsForm:insertComboField("IsMandatoryField", "", 110, TRUE).  
   FileMasterFieldDetailsForm:insertComboPairs("IsMandatoryField", "yes", "Mandatory").
   FileMasterFieldDetailsForm:insertComboPairs("IsMandatoryField", "no",  "Not Mandatory").
   
   /* Add Hidden Field*/
   FileMasterFieldDetailsForm:insertHiddenField("filemaster_browse_scroll", "").
   FileMasterFieldDetailsForm:insertHiddenField("filemasterfield_browse_scroll", "").
   FileMasterFieldDetailsForm:insertHiddenField("form_name", "filemasterfield_details_form").
   FileMasterFieldDetailsForm:insertHiddenField("FileMasterID", chrFileMasterID).
   FileMasterFieldDetailsForm:insertHiddenField("prog_name", "adFileMasterAdmin.p").

/*    END. */

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileMasterFieldDetailsForm}
   
   /* Create Button Bar */
   FileMasterFieldDetailsButtons = NEW buttonBar().
   
   FileMasterFieldDetailsButtons:addButton("filemasterfield_details_form_btn_save", 
                                           fTL("Save"), 
                                           "updateFileMasterField('filemasterfield_details_form');").
   
   FileMasterFieldDetailsButtons:addButton("filemasterfield_details_form_btn_cancel", 
                                           fTL("Cancel"), 
                                           "cancelUpdate('UserCancelled','process_mode'); disablePopup('filemasterfield_details_form_popup');").
   
   FileMasterFieldDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileMasterFieldDetailsForm:FormButtons = FileMasterFieldDetailsButtons.
   
   FileMasterFieldDetailsForm:endForm(). 
   
   FileMasterFieldDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFileMasterHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFileMasterHistoryBrowse Procedure 
PROCEDURE pFileMasterHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "filemasterhistory_details_form"}

   FileMasterHistoryBrowseForm = NEW dataForm("filemasterhistory_browse_form").
   FileMasterHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
      
   /* Setup */
   FileMasterHistoryBrowseForm:FormWidth  = 860.
   FileMasterHistoryBrowseForm:FormHeight = 530.
   FileMasterHistoryBrowseForm:FormTitle  = fTL("File Master Config History") + (IF AVAILABLE FileMaster THEN " : " 
                                                                                + STRING(FileMaster.FileMasterID) ELSE "").
   FileMasterHistoryBrowseForm:FormType   = "xxl_large".
   
   FileMasterHistoryBrowse = NEW browseTable("filemasterhistory_browse").
   FileMasterHistoryBrowse:BrowseWidth  = 840.
   FileMasterHistoryBrowse:BrowseHeight = 490.
   
   FileMasterHistoryBrowse:insertColumn(fTL("History ID"), 60, "INTEGER").
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i FileMasterHistory}

   FileMasterHistoryBrowse:insertColumn(fTL("Active"),        50, "INTEGER", FALSE).
   FileMasterHistoryBrowse:insertColumn(fTL("Master Name"),  110, "INTEGER", FALSE).
   FileMasterHistoryBrowse:insertColumn(fTL("Program Name"), 130, "INTEGER", FALSE).  
   FileMasterHistoryBrowse:insertColumn(fTL("FileMask"),     135, "CHARACTER", FALSE).
   FileMasterHistoryBrowse:insertColumn(fTL("File Path"),    100, "CHARACTER", FALSE).
   FileMasterHistoryBrowse:insertColumn(fTL("User"),          90, "CHARACTER", FALSE).
   FileMasterHistoryBrowse:insertColumn(fTL("Created"),      105, "CHARACTER", FALSE).
   
   FileMasterHistoryBrowse:StartBody().
   
   IF AVAILABLE FileMaster THEN
   DO:
      /* List the FileMasterHistorys for the ShipOrderPick */
      FOR EACH FileMasterHistory NO-LOCK
         WHERE FileMasterHistory.FileMasterID = FileMaster.FileMasterID 
         BY    FileMasterHistory.Created DESCENDING:
         
         FIND FIRST GateUser NO-LOCK 
            WHERE GateUser.GateUserID = FileMasterHistory.GateUserID NO-ERROR.

         FileMasterHistoryBrowse:startRow(FileMasterHistory.FileMasterHistoryID, "selectFileMasterHistoryRow(this," 
                                            + '"' + STRING(FileMasterHistory.FileMasterHistoryID)
                                            + '","adFileMasterAdmin.p","shiporderpickconfig_browse_form"' + ");", "").

         FileMasterHistoryBrowse:insertData(FileMasterHistory.FileMasterHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i FileMasterHistory}

         FileMasterHistoryBrowse:insertData(STRING(FileMasterHistory.Active), "center").
         FileMasterHistoryBrowse:insertData(STRING(FileMasterHistory.MasterName), "left"). 
         FileMasterHistoryBrowse:insertData(STRING(FileMasterHistory.ProgramName), "left"). 
         FileMasterHistoryBrowse:insertData(STRING(FileMasterHistory.FileMask), "left"). 
         FileMasterHistoryBrowse:insertData(STRING(FileMasterHistory.FilePath), "left").             
         FileMasterHistoryBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         FileMasterHistoryBrowse:insertData(fDisplayDate&Time(FileMasterHistory.Created,"y/m/d H:M:S"), "right").                  

         FileMasterHistoryBrowse:endRow().
      
      END. /* FOR EACH FileMasterHistory OF FileMaster NO-LOCK */
   END. /*IF AVAILABLE FileMasterHistory THEN*/
   
   FileMasterHistoryBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + FileMasterHistoryBrowse:getErrors().

   FileMasterHistoryBrowseForm:insertHiddenField("popup_filemasterhistory_browse", "").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FileMasterHistoryBrowseForm}
   
   /* Create Button Bar */
   FileMasterHistoryBrowseButtons = NEW buttonBar().
          
   FileMasterHistoryBrowseButtons:addButton("filemasterhistory_browse_form_btn_cancel",
                                            fTL("Cancel"),
                                            "disablePopup('filemasterhistory_browse_form_popup');").
   
   FileMasterHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   FileMasterHistoryBrowseForm:FormBrowse  = FileMasterHistoryBrowse.
   FileMasterHistoryBrowseForm:FormButtons = FileMasterHistoryBrowseButtons.
   FileMasterHistoryBrowseForm:endForm(). 
   
   FileMasterHistoryBrowseForm:displayForm().
   
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
   
   ASSIGN chrFileMasterID          = get-value("FileMasterID")
          chrFileMasterFieldID     = get-value("FileMasterFieldID")
          chrScrollToFileMasterRow = STRING(INTEGER(get-value("MasterName_browse_scroll"))) + ";".
   
   ASSIGN intSelectedFileMaster      = INTEGER(chrFileMasterID) 
          intSelectedFileMasterField = INTEGER(chrFileMasterFieldID) NO-ERROR.
          
   /* Process URL values */
   IF chrFileMasterID <> "" THEN
      chrSelectFileMasterRow = 'selectFileMasterRow(document.getElementById("filemaster_browse_row_' 
                                  + chrFileMasterID + '"),"' + chrFileMasterID +  '");'.
                                  
   /* Disabling Selection of Popup Line if not Belonging to Current */
   IF intSelectedFileMasterField > 0 THEN DO:
      FIND FIRST FileMasterField NO-LOCK 
         WHERE FileMasterField.FileMasterFieldID = intSelectedFileMasterField 
         AND   FileMasterField.FileMasterID = intSelectedFileMaster NO-ERROR.
      IF NOT AVAILABLE FileMasterField THEN 
      DO:
         chrFileMasterFieldID = "".
         intSelectedFileMasterField = 0.
      END.
      ELSE
      DO:
         chrSelectFileMasterFieldRow = 'selectFileMasterFieldRow(document.getElementById("filemasterfield_browse_row_' 
                                          + chrFileMasterFieldID + '"),"' + chrFileMasterFieldID + '","' + FileMasterField.TableName + '");'.
      END.
   END. /* intSelectedDataMigrationBrowseField > 0 */  

/*    IF chrFileMasterFieldID <> "" THEN                                                                                                                                                                            */
/*       chrSelectFileMasterFieldRow = 'selectFileMasterFieldRow(document.getElementById("filemasterfield_browse_row_' + chrFileMasterFieldID + '"),"' + chrFileMasterFieldID + ,"' + chrFileMasterFieldID + '");'. */
   IF get-value('popup_filemasterhistory_browse') = "yes" THEN 
      chrPopupFileMasterHistory = 'enablePopup("filemasterhistory_browse_form_popup");'. 

   IF get-value('popup_filemasterfield_browse') = "yes" THEN 
      chrPopupFileMasterField = 'enablePopup("filemasterfield_browse_form_popup");'. 

   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("filemaster_browse").scrollTop=' + chrScrollToFileMasterRow 
                             + chrSelectFileMasterRow
                             + chrSelectFileMasterFieldRow
                             + chrPopupFileMasterHistory
                             + chrPopupFileMasterField.
                             
   /* Add Body Load for AppLinks */
   {webDataMigrationBodyLoad.i}   

   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "File Master Admin".
   ThisPage:FrameTitle    = "File Master Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("filemaster.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i} 
   
   /******* Main Browser ********************/
   RUN pFileMasterBrowse.
   
   /******* Pop-up Browsers and Forms ********/
   IF intSelectedFileMaster <> 0 THEN 
   DO:
      FIND FIRST FileMaster NO-LOCK 
         WHERE FileMaster.FileMasterID = intSelectedFileMaster NO-ERROR.
   END.
      
   RUN pFileMasterHistoryBrowse.
   RUN pFileMasterDetails.

   RUN pFileMasterFieldBrowse.
   RUN pFileMasterFieldDetails.
   
   /* Data Migration Procedure runs */
   {webDataMigrationProceduresRun.i}
      
   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT FileMasterBrowseFrame          NO-ERROR.
   DELETE OBJECT FileMasterBrowse               NO-ERROR.
   DELETE OBJECT FileMasterBrowseButtons        NO-ERROR.
   DELETE OBJECT FileMasterDetailsForm          NO-ERROR.
   DELETE OBJECT FileMasterDetailsButtons       NO-ERROR.
   DELETE OBJECT FileMasterHistoryBrowse        NO-ERROR.
   DELETE OBJECT FileMasterHistoryBrowseForm    NO-ERROR.
   DELETE OBJECT FileMasterHistoryBrowseButtons NO-ERROR.
   DELETE OBJECT FileMasterFieldDetailsForm     NO-ERROR.
   DELETE OBJECT FileMasterFieldDetailsButtons  NO-ERROR.
   DELETE OBJECT FileMasterFieldBrowseForm      NO-ERROR.
   DELETE OBJECT FileMasterFieldBrowse          NO-ERROR.
   DELETE OBJECT FileMasterFieldBrowseButtons   NO-ERROR.

   /* Data Migration Objects Cleanup */
   {webDataMigrationObjectsCleanup.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

