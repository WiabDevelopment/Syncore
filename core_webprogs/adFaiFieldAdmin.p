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

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE chrSelectedFaiFieldTable  AS CHARACTER NO-UNDO INITIAL "0".
DEFINE VARIABLE intSelectedFaiField       AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectFaiFieldRow      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToFaiFieldRow    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFaiFieldID             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectedFaiField       AS CHARACTER NO-UNDO.

/* Objects */
DEFINE VARIABLE FaiFieldSelectionFrame    AS pageFrame.
DEFINE VARIABLE FaiFieldSelectionForm     AS dataForm.
DEFINE VARIABLE FaiFieldBrowseFrame       AS pageFrame.
DEFINE VARIABLE FaiFieldBrowse            AS browseTable.
DEFINE VARIABLE FaiFieldBrowseBuons       AS buttonBar.
DEFINE VARIABLE FaiFieldDetailsForm       AS dataForm.
DEFINE VARIABLE FaiFieldDetailsBuons      AS buttonBar.

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

&IF DEFINED(EXCLUDE-pFaiFieldBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFaiFieldBrowse Procedure 
PROCEDURE pFaiFieldBrowse :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form that is passed in */
   {webGetWebForm.i "faifield_details_form"}

   /* Browser on the FaiField table */
   FaiFieldBrowse = NEW browseTable("faifield_browse").
   FaiFieldBrowse:BrowseWidth  = 965.
   FaiFieldBrowse:BrowseHeight = 395.
   FaiFieldBrowse:WebStream    = STREAM WebStream:HANDLE.

   /* If there is a corresponding Rule table, then build RuleID and RuleDesc into the browse */
   FaiFieldBrowse:insertColumn(fTL("FaiFieldID"),    80, "INTEGER", FALSE).
   FaiFieldBrowse:insertColumn(fTL("TableName"),    100, "CHARACTER", "left", FALSE).
   FaiFieldBrowse:insertColumn(fTL("FieldName"),    100, "INTEGER", FALSE).
   FaiFieldBrowse:insertColumn(fTL("FieldLabel"),   100, "INTEGER", FALSE).
   FaiFieldBrowse:insertColumn(fTL("ListSeq"),       70, "INTEGER", FALSE).
   FaiFieldBrowse:insertColumn(fTL("ACTIVE"),        80, "LOGICAL", FALSE).

   /* build body of browse */
   FaiFieldBrowse:startBody().

   FOR EACH FaiField NO-LOCK
      WHERE TRUE:

      FaiFieldBrowse:startRow(FaiField.FaiFieldID,
                            "selectFaiFieldRow(this," + '"' + STRING(FaiField.FaiFieldID) + '","' + chrSelectedFaiFieldTable + '"' + ");",
                            "").

      FaiFieldBrowse:insertData(STRING(FaiField.FaiFieldID)).
      FaiFieldBrowse:insertData(FaiField.TableName, "left").
      FaiFieldBrowse:insertData(FaiField.FieldName, "left").
      FaiFieldBrowse:insertData(FaiField.FieldLabel, "left").
      FaiFieldBrowse:insertData(STRING(FaiField.ListingSequence)).
      FaiFieldBrowse:insertData(STRING(FaiField.ACTIVE,"Yes/No")).

      /* Add hidden fields */
      FaiFieldBrowse:insertHiddenData("FaiFieldID", FaiField.FaiFieldID).
      FaiFieldBrowse:insertHiddenData("FaiFieldVersionID", FaiField.VersionID).

      FaiFieldBrowse:endRow().
   END.

   FaiFieldBrowse:endTable().   

   /* --------------Create FaiField Browse Buon Bar --------------*/
   FaiFieldBrowseBuons           = NEW buttonBar().
   FaiFieldBrowseBuons:WebStream = STREAM WebStream:HANDLE.

   FaiFieldBrowseBuons:addButton("faifield_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewFaiFieldDetails('faifield_details_form');",
                                 ""). /* .(IF intSelectedFaiField > 0 THEN "" ELSE "Disabled")). */

   FaiFieldBrowseBuons:addButton("faifield_browse_form_btn_create",
                                 fTL("Create"),
                                 "createFaiField('faifield_details_form');",
                                 "").

/*   FaiFieldBrowseBuons:addButton("faifield_browse_form_btn_create",                           */
/*                                 fTL("Values"),                                               */
/*                                 "createFaiField('faifield_details_form');",                  */
/*                                 (IF chrSelectedFaiFieldTable > "0" THEN "" ELSE "Disabled")).*/

   FaiFieldBrowseBuons:addButton("faifield_browse_form_btn_create",
                                 fTL("History"),
                                 "viewFaiFieldHistory('faifield_details_form');",
                                 ""). /* (IF intSelectedFaiField > 0 THEN "" ELSE "Disabled")). */

   FaiFieldBrowseBuons:closeBar().


   /* Building page for adding pieces to main frame */
   FaiFieldSelectionFrame = NEW pageFrame().
   FaiFieldSelectionFrame:WebStream = STREAM WebStream:HANDLE.

   /* creating a new frame */
   FaiFieldSelectionFrame:frameOpen(985, 70, "").

   FaiFieldSelectionForm:displayForm().

   FaiFieldSelectionFrame:frameClose().

   /* Create a new frame */
   FaiFieldBrowseFrame = NEW pageFrame().
   FaiFieldBrowseFrame:Webstream = STREAM WebStream:HANDLE. 

   /* Insert a form */
   FaiFieldBrowseFrame:FormAction="dbFaiFieldUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   FaiFieldBrowseFrame:formOpen("faifield_browse_form").

   FaiFieldBrowseFrame:frameOpen(985, 435, "").

   FaiFieldBrowse:displayBrowse().

   FaiFieldBrowseFrame:frameClose().
   FaiFieldBrowseFrame:insertSpacer(10).

   /* insert hidden fields */
   FaiFieldBrowseFrame:insertHiddenField("FaiFieldTableName", chrSelectedFaiFieldTable).
   FaiFieldBrowseFrame:insertHiddenField("FaiFieldID", "").
   FaiFieldBrowseFrame:insertHiddenField("FaiFieldVersionID","").
   FaiFieldBrowseFrame:insertHiddenField("faifield_browse_scroll","").
   FaiFieldBrowseFrame:insertHiddenField("form_name", "faifield_browse_form").
   FaiFieldBrowseFrame:insertHiddenField("prog_name", "adFaiFieldAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FaiFieldBrowseFrame}

   FaiFieldBrowseFrame:formClose().

   FaiFieldBrowseBuons:displayButtonBar().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFaiFieldDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFaiFieldDetails Procedure 
PROCEDURE pFaiFieldDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "faifield_details_form"}

   DEFINE VARIABLE logSuccess     AS LOGICAL NO-UNDO.
   DEFINE VARIABLE hdlTableBuffer AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hdlTableQuery  AS HANDLE  NO-UNDO.
   DEFINE VARIABLE hdlFieldBuffer AS HANDLE  NO-UNDO.

   ASSIGN chrDisplayFieldList  = chrSelectedFaiFieldTable + ",PartTypeID,StockEntityID,DatabaseName,TableName,FieldLabel,FieldName,FieldDescr,CustomerSpecific,Active,PartCategoryID,"
                                    + "ListingSequence,IsUniqueValue,IsMandatoryField,PercentageChangeAllowed,DisplayType,DataType,DefaultValue,"
                                    + "ValidationString,LinkTableName"
          chrEditFieldList     = "PartTypeID,StockEntityID,DatabaseName,TableName,FieldLabel,FieldName,FieldDescr,CustomerSpecific,Active,PartCategoryID,"
                                    + "ListingSequence,IsUniqueValue,IsMandatoryField,PercentageChangeAllowed,DisplayType,DataType,DefaultValue,"
                                    + "ValidationString,LinkTableName"
          chrNewFieldList      = "PartTypeID,StockEntityID,DatabaseName,TableName,FieldLabel,FieldName,FieldDescr,CustomerSpecific,Active,PartCategoryID,"
                                    + "ListingSequence,IsUniqueValue,IsMandatoryField,PercentageChangeAllowed,DisplayType,DataType,DefaultValue,"
                                    + "ValidationString,LinkTableName"
          chrRequiredFieldList = "FieldName,FieldDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".

      
   
   FaiFieldDetailsForm            = NEW dataForm("faifield_details_form").
   FaiFieldDetailsForm:WebStream  = STREAM WebStream:HANDLE.
   FaiFieldDetailsForm:FormAction = "dbFaiFieldUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   FaiFieldDetailsForm:FormWidth  = 660.
   FaiFieldDetailsForm:FormHeight = 510.
   FaiFieldDetailsForm:FormTitle  = chrSelectedFaiFieldTable + " Details".
   FaiFieldDetailsForm:FormType   = "xl_large".

   /* Column Layout */
   FaiFieldDetailsForm:insertPaddingColumn(60).

   /* Fields */
   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("FaiField ID").
   FaiFieldDetailsForm:insertTextField(chrSelectedFaiFieldTable + "ID", "", 60, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("Listing Sequence").
   FaiFieldDetailsForm:insertTextField("ListingSequence", "", 60, TRUE).

/*    FaiFieldDetailsForm:startRow().                                  */
/*    FaiFieldDetailsForm:insertLabel("FaiField Name").                */
/*    FaiFieldDetailsForm:insertTextField("FieldName", "", 200, TRUE). */

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("PartType").
   FaiFieldDetailsForm:insertComboField("PartTypeID", "", 220, TRUE).
    FaiFieldDetailsForm:insertComboPairs("PartTypeID", "", "None Selected...").
   FOR EACH PartType NO-LOCK:
      FaiFieldDetailsForm:insertComboPairs("PartTypeID", STRING(PartType.PartTypeID), PartType.TypeCode).
   END.

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("StockEntity").
   FaiFieldDetailsForm:insertComboField("StockEntityID", "", 220, TRUE).
   FaiFieldDetailsForm:insertComboPairs("StockEntityID", "", "None Selected...").
   FOR EACH StockEntity NO-LOCK:
      FaiFieldDetailsForm:insertComboPairs("StockEntityID", STRING(StockEntity.StockEntityID), StockEntity.EntityCode).
   END.

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("DB Name").
   FaiFieldDetailsForm:insertComboField("DatabaseName", "", 110, TRUE,'populateTablesCombo(this.value)').
   /* Adding Blank Line to have unselected */
   FaiFieldDetailsForm:insertComboPairs("DatabaseName", "", "None Selected...").

   /* Read the DB's */
   FOR EACH SystemDb NO-LOCK:

      IF SystemDB.SystemDBName = "Temp" THEN NEXT.

      FaiFieldDetailsForm:insertComboPairs("DatabaseName", SystemDB.SystemDBName, SystemDB.SystemDBName).       
   END.

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("Table Name").
   FaiFieldDetailsForm:insertComboField("TableName", "", 110, TRUE,
                                        "populateTableFieldsCombo(document.getElementById('DatabaseName').value, this.value)").
   
   /* Adding Blank Line to have unselected */
   FaiFieldDetailsForm:insertComboPairs("TableName", "", "None Selected...").

   /* Read the DB structures*/
   FOR EACH SystemDb NO-LOCK:

      IF SystemDB.SystemDBName = "Temp" THEN NEXT.

      CREATE BUFFER hdlTableBuffer FOR TABLE SystemDb.SystemDbName + "._file".
      CREATE QUERY  hdlTableQuery NO-ERROR.
      hdlTableQuery:SET-BUFFERS(hdlTableBuffer).

      logSuccess = hdlTableQuery:QUERY-PREPARE("FOR EACH " + SystemDb.SystemDbName + "._file WHERE " + SystemDB.SystemDBName + "._file._hidden = FALSE") NO-ERROR.
                      
      IF NOT logSuccess OR ERROR-STATUS:ERROR THEN
      DO intErrorCount = 1 TO ERROR-STATUS:NUM-MESSAGES:
         chrPageBuildError = chrPageBuildError + ERROR-STATUS:GET-MESSAGE(intErrorCount) + ". ".
      END.
      
      IF logSuccess THEN
      DO:
         hdlTableQuery:QUERY-OPEN().
         hdlTableQuery:GET-FIRST(NO-LOCK).
         
         REPEAT WHILE NOT hdlTableQuery:QUERY-OFF-END:
            
            FaiFieldDetailsForm:insertComboPairs("TableName",
                                                 STRING(hdlTableBuffer:BUFFER-FIELD("_file-name"):BUFFER-VALUE),
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
  
   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("Field Name").
   FaiFieldDetailsForm:insertComboField("FieldName", "", 110, TRUE).
   /* Adding Blank Line to have unselected */
   FaiFieldDetailsForm:insertComboPairs("FieldName","","None Selected..."). 


   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("FieldLabel").
   FaiFieldDetailsForm:insertTextField("FieldLabel", "", 200, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel(fTL("Customer Specific")).
   FaiFieldDetailsForm:insertComboField("CustomerSpecific", "", 100, TRUE).
   FaiFieldDetailsForm:insertComboPairs("CustomerSpecific", "yes", "Is Customer Specific").
   FaiFieldDetailsForm:insertComboPairs("CustomerSpecific", "no",  "Is Not CustomerSpecific ").

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("PartCategory").
   FaiFieldDetailsForm:insertComboField("PartCategoryID", "", 220, TRUE).
   FaiFieldDetailsForm:insertComboPairs("PartCategoryID","","None Selected..."). 
   FOR EACH PartCategory NO-LOCK:
      FaiFieldDetailsForm:insertComboPairs("PartCategoryID", STRING(PartCategory.PartCategoryID), PartCategory.CategoryCode).
   END.

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel(fTL("Unique Value")).
   FaiFieldDetailsForm:insertComboField("IsUniqueValue", "", 100, TRUE).
   FaiFieldDetailsForm:insertComboPairs("IsUniqueValue", "yes", "Is Unique Value").
   FaiFieldDetailsForm:insertComboPairs("IsUniqueValue", "no",  "Is Not Unique Value").

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel(fTL("Mandatory Field")).
   FaiFieldDetailsForm:insertComboField("IsMandatoryField", "", 100, TRUE).
   FaiFieldDetailsForm:insertComboPairs("IsMandatoryField", "yes", "Is Mandatory Field").
   FaiFieldDetailsForm:insertComboPairs("IsMandatoryField", "no",  "Is Not Mandatory Field").

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("PercentageChangeAllowed").
   FaiFieldDetailsForm:insertTextField("PercentageChangeAllowed", "", 200, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("DisplayType").
   FaiFieldDetailsForm:insertTextField("DisplayType", "", 200, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("DataType").
   FaiFieldDetailsForm:insertTextField("DataType", "", 200, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("DefaultValue").
   FaiFieldDetailsForm:insertTextField("DefaultValue", "", 200, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("ValidationString").
   FaiFieldDetailsForm:insertTextField("ValidationString", "", 200, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("LinkTableName").
   FaiFieldDetailsForm:insertTextField("LinkTableName", "", 200, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel("FaiField Details").
   FaiFieldDetailsForm:insertTextAreaField("FieldDescr", "", 200, 2, TRUE).

   FaiFieldDetailsForm:startRow().
   FaiFieldDetailsForm:insertLabel(fTL("Active")).
   FaiFieldDetailsForm:insertComboField("Active", "", 100, TRUE).
   FaiFieldDetailsForm:insertComboPairs("Active", "yes", "Active").
   FaiFieldDetailsForm:insertComboPairs("Active", "no",  "Not Active").

   {webGetOptionalFormFields.i pFaiFieldDetailsFields}

   /* Add Hidden Fields*/
   FaiFieldDetailsForm:insertHiddenField("faifield_browse_scroll", "").
   FaiFieldDetailsForm:insertHiddenField("form_name", "faifield_details_form").
   FaiFieldDetailsForm:insertHiddenField("prog_name", "adFaiFieldAdmin.p").
   FaiFieldDetailsForm:insertHiddenField("FaiFieldTableName", chrSelectedFaiFieldTable).

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i FaiFieldDetailsForm}

   /* Create Buon Bar */
   FaiFieldDetailsBuons = NEW buttonBar().
   FaiFieldDetailsBuons:addButton("faifield_details_form_btn_save",
                                  fTL("Save"),
                                  "updateFaiField('faifield_details_form','" + chrSelectedFaiFieldTable + "');").

   FaiFieldDetailsBuons:addButton("faifield_details_form_btn_cancel",
                                  fTL("Cancel"),
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('faifield_details_form_popup');").
   FaiFieldDetailsBuons:closeBar().

   /* Assign the Buon Bar Object to the Form Object */
   FaiFieldDetailsForm:FormButtons = FaiFieldDetailsBuons.

   FaiFieldDetailsForm:endForm().

   FaiFieldDetailsForm:displayForm().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pFaiFieldSelectionDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFaiFieldSelectionDetails Procedure 
PROCEDURE pFaiFieldSelectionDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


   FaiFieldSelectionForm             = NEW dataForm("faifield_selection_form").
   FaiFieldSelectionForm:WebStream   = STREAM WebStream:HANDLE.
   FaiFieldSelectionForm:ShowAsPopup = FALSE.
   FaiFieldSelectionForm:FloatForm   = "left".
   FaiFieldSelectionForm:FormAction  = "adFirstArticleInspectionAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   FaiFieldSelectionForm:FormWidth  = 964.
   FaiFieldSelectionForm:FormHeight =  50.
   FaiFieldSelectionForm:FormTitle  = "Select FaiField Table".
   FaiFieldSelectionForm:FormType   = "medium".

   /* Column Layout */
   FaiFieldSelectionForm:insertPaddingColumn(10).
   FaiFieldSelectionForm:insertColumn(100).
   FaiFieldSelectionForm:insertColumn(200).
   FaiFieldSelectionForm:insertColumn(10).
   FaiFieldSelectionForm:insertColumn(100).
   FaiFieldSelectionForm:insertColumn(200).
   FaiFieldSelectionForm:insertColumn(10).
   FaiFieldSelectionForm:insertColumn(100).
   FaiFieldSelectionForm:insertColumn(200).

   /* Fields design */
   FaiFieldSelectionForm:startRow().
   FaiFieldSelectionForm:insertLabel(fTL("FaiField Table")).
   FaiFieldSelectionForm:insertComboField("FaiFieldTableName", chrSelectedFaiFieldTable, 200, TRUE, 'changeFaiField(this.value)').

   FaiFieldSelectionForm:insertComboPairs("FaiFieldTableName", "0", "Choose a table...").

   /* Build a list with all the FaiField tables in the core DB */
   FOR EACH core._file NO-LOCK
      WHERE core._file._file-Name MATCHES "*" + "FaiField" + "*":

      FaiFieldSelectionForm:insertComboPairs("FaiFieldTableName", core._file._file-Name, core._file._file-Name).

   END.

   FaiFieldSelectionForm:insertLabel(fTL("")).
   FaiFieldSelectionForm:insertLabel(fTL("FaiField Table2")).
   FaiFieldSelectionForm:insertComboField("FaiFieldTableName2", chrSelectedFaiFieldTable, 200, TRUE, 'changeFaiField(this.value)').

   FaiFieldSelectionForm:insertComboPairs("FaiFieldTableName2", "0", "Choose a table...").

   /* Build a list with all the FaiField tables in the core DB */
   FOR EACH core._file NO-LOCK
      WHERE core._file._file-Name MATCHES "*" + "FaiField" + "*":

      FaiFieldSelectionForm:insertComboPairs("FaiFieldTableName2", core._file._file-Name, core._file._file-Name).

   END.

   FaiFieldSelectionForm:insertLabel(fTL("")).
   FaiFieldSelectionForm:insertLabel(fTL("FaiField Table3")).
   FaiFieldSelectionForm:insertComboField("FaiFieldTableName3", chrSelectedFaiFieldTable, 200, TRUE, 'changeFaiField(this.value)').

   FaiFieldSelectionForm:insertComboPairs("FaiFieldTableName3", "0", "Choose a table...").

   /* Build a list with all the FaiField tables in the core DB */
   FOR EACH core._file NO-LOCK
      WHERE core._file._file-Name MATCHES "*" + "FaiField" + "*":

      FaiFieldSelectionForm:insertComboPairs("FaiFieldTableName3", core._file._file-Name, core._file._file-Name).

   END.

   FaiFieldSelectionForm:endForm().

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

   /* Get URL Values */
   ASSIGN chrSelectedFaiFieldTable = get-value("FaiFieldTableName").
          chrFaiFieldID            = get-value("FaiFieldID").
          intSelectedFaiField      = INTEGER(chrFaiFieldID).
          chrScrollToFaiFieldRow   = STRING(INTEGER(get-value("faifield_browse_scroll"))) + ";".


   IF chrSelectedFaiFieldTable = "" THEN chrSelectedFaiFieldTable = "0".

   /* Process URL values */
   IF chrFaiFieldID <> "" THEN
      chrSelectFaiFieldRow = 'selectFaiFieldRow(document.getElementById("faifield_browse_row_' + chrFaiFieldID + '"),"'
                                            + chrFaiFieldID + '");'.



   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("faifield_browse").scrollTop=' + chrScrollToFaiFieldRow
                             + chrSelectFaiFieldRow.

   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "FAI Field Admin".
   ThisPage:FrameTitle    = "FAI Field Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.

   /* Include CSS Files */
   {webCssFiles.i}

   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("faifield.js"). /* Append your file here */

   ThisPage:PageHeader().

   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}

   /******* Main Browser ********************/
   RUN pFaiFieldSelectionDetails.
   RUN pFaiFieldBrowse.

   /******* Pop-up Browsers and Forms ********/
   RUN pFaiFieldDetails.


   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}

   ThisPage:PageFooter().

   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}

   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}

   /* Delete objects defined locally */
   DELETE OBJECT FaiFieldSelectionFrame NO-ERROR.
   DELETE OBJECT FaiFieldSelectionForm  NO-ERROR.
   DELETE OBJECT FaiFieldBrowseFrame    NO-ERROR.
   DELETE OBJECT FaiFieldBrowse         NO-ERROR.
   DELETE OBJECT FaiFieldBrowseBuons    NO-ERROR.
   DELETE OBJECT FaiFieldDetailsForm    NO-ERROR.
   DELETE OBJECT FaiFieldDetailsBuons   NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

