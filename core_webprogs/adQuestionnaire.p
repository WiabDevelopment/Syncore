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

  Author: Anthony Ferrari

  Created: 01/02/2016

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
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncDataFunctions.i}



/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedQuestionnaire                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedQuestionnaireFlow               AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedQuestionnaireOpTypeLink         AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQuestionnaireFlowRow              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionnaireFlowRow            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionnaireFlowID                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionnaireFlow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectQuestionnaireRow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionnaireRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionnaireID                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionnaireHistory               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionnaireFlowHistory           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectQuestionnaireOpTypeLinkRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQuestionnaireOpTypeLinkRow      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQuestionnaireOpTypeLinkID               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionnaireOpTypeLink            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQuestionnaireOpTypeLinkHist        AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QuestionnaireBrowseFrame                   AS pageFrame.
DEFINE VARIABLE QuestionnaireBrowse                        AS browseTable.
DEFINE VARIABLE QuestionnaireBrowseButtons                 AS buttonBar.
DEFINE VARIABLE QuestionnaireDetailsForm                   AS dataForm.
DEFINE VARIABLE QuestionnaireDetailsButtons                AS buttonBar.

DEFINE VARIABLE QuestionnaireHistoryBrowseForm             AS dataForm.   
DEFINE VARIABLE QuestionnaireHistoryBrowse                 AS browseTable.
DEFINE VARIABLE QuestionnaireHistoryBrowseButtons          AS buttonBar.

DEFINE VARIABLE QuestionnaireHistoryDetailsForm            AS dataForm.
DEFINE VARIABLE QuestionnaireHistoryDetailsButtons         AS buttonBar.

DEFINE VARIABLE QuestionnaireFlowBrowseForm                AS dataForm.   
DEFINE VARIABLE QuestionnaireFlowBrowse                    AS browseTable.
DEFINE VARIABLE QuestionnaireFlowBrowseButtons             AS buttonBar.

DEFINE VARIABLE QuestionnaireFlowDetailsForm               AS dataForm.
DEFINE VARIABLE QuestionnaireFlowDetailsButtons            AS buttonBar.

DEFINE VARIABLE QuestionnaireFlowHistoryBrowseForm         AS dataForm.   
DEFINE VARIABLE QuestionnaireFlowHistoryBrowse             AS browseTable.
DEFINE VARIABLE QuestionnaireFlowHistoryBrowseButtons      AS buttonBar.

DEFINE VARIABLE QuestionnaireFlowHistoryDetailsForm        AS dataForm.
DEFINE VARIABLE QuestionnaireFlowHistoryDetailsButtons     AS buttonBar.

DEFINE VARIABLE QuestionnaireOpTypeLinkBrowseForm          AS dataForm.   
DEFINE VARIABLE QuestionnaireOpTypeLinkBrowse              AS browseTable.
DEFINE VARIABLE QuestionnaireOpTypeLinkBrowseButtons       AS buttonBar.

DEFINE VARIABLE QuestionnaireOpTypeLinkDetailsForm         AS dataForm.
DEFINE VARIABLE QuestionnaireOpTypeLinkDetailsButtons      AS buttonBar.

DEFINE VARIABLE QuestionnaireOpTypeLinkHistBrowseForm      AS dataForm.   
DEFINE VARIABLE QuestionnaireOpTypeLinkHistBrowse          AS browseTable.
DEFINE VARIABLE QuestionnaireOpTypeLinkHistBrowseButtons   AS buttonBar.

DEFINE VARIABLE QuestionnaireOpTypeLinkHistDetailsForm     AS dataForm.
DEFINE VARIABLE QuestionnaireOpTypeLinkHistDetailsButtons  AS buttonBar.

/* Buffers */
DEFINE BUFFER performedOperationType FOR OperationType.


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
         HEIGHT             = 14.1
         WIDTH              = 60.4.
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

&IF DEFINED(EXCLUDE-pQuestionnaireFlowBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireFlowBrowse Procedure
PROCEDURE pQuestionnaireFlowBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionnaireflow_details_form"}
   
   FIND FIRST QuestionnaireFlow NO-LOCK /* idx=QuestionnaireID */
      WHERE QuestionnaireFlow.QuestionnaireFlowID = intSelectedQuestionnaireFlow NO-ERROR.
   
   QuestionnaireFlowBrowseForm = NEW dataForm("questionnaireflow_browse_form").
   QuestionnaireFlowBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   QuestionnaireFlowBrowseForm:FormAction="dbQuestionnaireFlowUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireFlowBrowseForm:FormWidth   = 850.
   QuestionnaireFlowBrowseForm:FormHeight  = 540.
   QuestionnaireFlowBrowseForm:FormTitle   = fTL("Questionnaire Flows for Questionnaire: ") + (IF AVAILABLE Questionnaire THEN  
                                                                  Questionnaire.QuestionnaireName ELSE "").
   QuestionnaireFlowBrowseForm:FormType    = "xxl_large".
   
   QuestionnaireFlowBrowse = NEW browseTable("questionnaireflow_browse").
   QuestionnaireFlowBrowse:BrowseWidth  = 830.
   QuestionnaireFlowBrowse:BrowseHeight = 500.
   QuestionnaireFlowBrowse:ExcelExport  = TRUE.
   QuestionnaireFlowBrowse:SessionID    = intGblSessionID.
   
   
   QuestionnaireFlowBrowse:insertColumn(fTL("Flow ID"),            60, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionnaireFlow}
   
   QuestionnaireFlowBrowse:insertColumn(fTL("Question Category"), 125, "CHARACTER", "left", FALSE).
   QuestionnaireFlowBrowse:insertColumn(fTL("Asking Sequence"),   125, "INTEGER", "left",   FALSE).
   QuestionnaireFlowBrowse:insertColumn(fTL("Active"),             50, "LOGICAL",           FALSE).
   
   QuestionnaireFlowBrowse:StartBody().
   
   IF AVAILABLE Questionnaire THEN
   DO:
      /*List the QuestionnaireFlow*/
      FOR EACH QuestionnaireFlow NO-LOCK 
         WHERE QuestionnaireFlow.QuestionnaireID = intSelectedQuestionnaire
         BY    QuestionnaireFlow.QuestionnaireFlowID:
         
         FIND FIRST QuestionCategory OF QuestionnaireFlow NO-LOCK NO-ERROR. /* idx=QuestionCategoryID */
       
         QuestionnaireFlowBrowse:startRow(QuestionnaireFlow.QuestionnaireFlowID, "selectQuestionnaireFlowRow(this," + '"' 
                                          + STRING(QuestionnaireFlow.QuestionnaireFlowID) + '"' + ");", "").                                                            
                                                                     
         QuestionnaireFlowBrowse:insertData(QuestionnaireFlow.QuestionnaireFlowID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionnaireFlow}
         
         QuestionnaireFlowBrowse:insertData((IF AVAILABLE QuestionCategory THEN QuestionCategory.QuestionCategoryName ELSE ""), "left").
         QuestionnaireFlowBrowse:insertData(QuestionnaireFlow.AskingSequence).
         QuestionnaireFlowBrowse:insertData(STRING(QuestionnaireFlow.Active, "Yes/No")).
         
         
         /* Add hidden fields */         
         QuestionnaireFlowBrowse:insertHiddenData("QuestionnaireID",Questionnaire.QuestionnaireID).
         QuestionnaireFlowBrowse:insertHiddenData("QuestionnaireVerisonID",Questionnaire.VersionID).
         QuestionnaireFlowBrowse:insertHiddenData("QuestionnaireFlowID",QuestionnaireFlow.QuestionnaireFlowID).
         QuestionnaireFlowBrowse:insertHiddenData("QuestionnaireFlowVerisonID",QuestionnaireFlow.VersionID).
         
         QuestionnaireFlowBrowse:endRow().
      
      END. /* FOR EACH QuestionnaireFlow */
   END. /*IF AVAILABLE Questionnaire THEN*/
   
   QuestionnaireFlowBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireFlowBrowse:getErrors().
   
   QuestionnaireFlowBrowseForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireFlowBrowseForm:insertHiddenField("QuestionnaireVerisonID","").
   QuestionnaireFlowBrowseForm:insertHiddenField("QuestionnaireFlowID","").
   QuestionnaireFlowBrowseForm:insertHiddenField("QuestionnaireFlowVerisonID","").
   QuestionnaireFlowBrowseForm:insertHiddenField("questionnaireflow_browse_scroll","").
   QuestionnaireFlowBrowseForm:insertHiddenField("popup_questionnaireflow_browse","").
   QuestionnaireFlowBrowseForm:insertHiddenField("popup_questionnaireflowhistory_browse","").
   QuestionnaireFlowBrowseForm:insertHiddenField("form_name","questionnaireflow_browse_form").
   QuestionnaireFlowBrowseForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireFlowBrowseForm}
   
   /* Create Button Bar */
   QuestionnaireFlowBrowseButtons = NEW buttonBar().
   
   QuestionnaireFlowBrowseButtons:addButton("questionnaireflow_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQuestionnaireFlow('questionnaireflow_details_form');",
                                             "").                                                 
   
   QuestionnaireFlowBrowseButtons:addButton("questionnaireflow_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQuestionnaireFlowDetails('questionnaireflow_details_form');",
                                             "Disabled").
                                             
   QuestionnaireFlowBrowseButtons:addButton("questionnaireflow_browse_form_btn_history",
                                             fTL("History"),
                                             "viewQuestionnaireFlowHistory();",
                                             "Disabled").                                          
     
     /*Button for later if needed*/                                 
/*   QuestionnaireFlowBrowseButtons:addButton("questionnaireflow_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_questionnaireflow_browse.xml')").*/
   
   QuestionnaireFlowBrowseButtons:addButton("questionnaireflow_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('questionnaireflow_browse_form_popup');").
   
   QuestionnaireFlowBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireFlowBrowseForm:FormBrowse  = QuestionnaireFlowBrowse.
   QuestionnaireFlowBrowseForm:FormButtons = QuestionnaireFlowBrowseButtons.
   QuestionnaireFlowBrowseForm:endForm(). 
   
   QuestionnaireFlowBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionnaireFlowDetailsBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireFlowDetailsBrowse Procedure
PROCEDURE pQuestionnaireFlowDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questionnaireflow_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionnaireFlowID,QuestionCategoryID,AskingSequence,Active,QuestionnaireID" 
          chrEditFieldList     = "AskingSequence,Active" 
          chrNewFieldList      = "QuestionCategoryID,AskingSequence,Active" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
                             
   
   QuestionnaireFlowDetailsForm = NEW dataForm("questionnaireflow_details_form").
   QuestionnaireFlowDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   QuestionnaireFlowDetailsForm:FormAction = "dbQuestionnaireFlowUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireFlowDetailsForm:FormWidth   = 460.
   QuestionnaireFlowDetailsForm:FormHeight  = 200.
   QuestionnaireFlowDetailsForm:FormTitle   = "Questionnaire Flow Details".
   QuestionnaireFlowDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QuestionnaireFlowDetailsForm:insertPaddingColumn(40).
   QuestionnaireFlowDetailsForm:insertColumn(120).
   QuestionnaireFlowDetailsForm:insertColumn(120).
   QuestionnaireFlowDetailsForm:insertColumn(20).
   QuestionnaireFlowDetailsForm:insertColumn(4).
   QuestionnaireFlowDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionnaireFlowDetailsForm:startRow().
   QuestionnaireFlowDetailsForm:insertLabel(fTL("Flow ID")).
   QuestionnaireFlowDetailsForm:insertTextField("QuestionnaireFlowID", "", 200, TRUE).    
   
   QuestionnaireFlowDetailsForm:startRow().
   QuestionnaireFlowDetailsForm:insertLabel(fTL("Questionnaire")).
   QuestionnaireFlowDetailsForm:insertComboField("QuestionnaireID", "", 200, TRUE).
   /* Questionnaire DropDown Options */
   FOR EACH Questionnaire NO-LOCK /*idx=QuestionCategoryID*/
      WHERE Questionnaire.Active = TRUE:      
      QuestionnaireFlowDetailsForm:insertComboPairs("QuestionnaireID", STRING(Questionnaire.QuestionnaireID), Questionnaire.QuestionnaireName).    
   END.
   
   QuestionnaireFlowDetailsForm:startRow().
   QuestionnaireFlowDetailsForm:insertLabel(fTL("Question Category")).
   QuestionnaireFlowDetailsForm:insertComboField("QuestionCategoryID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH QuestionCategory NO-LOCK /*idx=QuestionCategoryID*/
      WHERE QuestionCategory.Active = TRUE:      
      QuestionnaireFlowDetailsForm:insertComboPairs("QuestionCategoryID", STRING(QuestionCategory.QuestionCategoryID), QuestionCategory.QuestionCategoryName).    
   END.
   
   QuestionnaireFlowDetailsForm:startRow().
   QuestionnaireFlowDetailsForm:insertLabel(fTL("AskingSequence")).
   QuestionnaireFlowDetailsForm:insertTextField("AskingSequence", "", 200, TRUE).
   
   QuestionnaireFlowDetailsForm:startRow().
   QuestionnaireFlowDetailsForm:insertLabel(fTL("Active")).
   QuestionnaireFlowDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionnaireFlowDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireFlowDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add Hidden Fields*/
   QuestionnaireFlowDetailsForm:insertHiddenField("questionnaireflow_browse_scroll","").
   QuestionnaireFlowDetailsForm:insertHiddenField("popup_questionnaireflow_browse", "").
   QuestionnaireFlowDetailsForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireFlowDetailsForm:insertHiddenField("QuestionnaireFlowID","").
   QuestionnaireFlowDetailsForm:insertHiddenField("QuestionnaireFlowVerisonID","").
   QuestionnaireFlowDetailsForm:insertHiddenField("form_name","questionnaireflow_details_form").
   QuestionnaireFlowDetailsForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireFlowDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireFlowDetailsButtons = NEW buttonBar().
   
   QuestionnaireFlowDetailsButtons:addButton("questionnaireflow_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQuestionnaireFlow('questionnaireflow_details_form');").
   
   QuestionnaireFlowDetailsButtons:addButton("questionnaireflow_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('questionnaireflow_details_form_popup');").
                                        
   QuestionnaireFlowDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireFlowDetailsForm:FormButtons = QuestionnaireFlowDetailsButtons.
   
   QuestionnaireFlowDetailsForm:endForm(). 
   QuestionnaireFlowDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionnaireFlowHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireFlowHistory Procedure
PROCEDURE pQuestionnaireFlowHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionnaireflowhistory_details_form"}
   
   FIND FIRST QuestionnaireFlow WHERE QuestionnaireFlow.QuestionnaireFlowID = intSelectedQuestionnaireFlow NO-LOCK NO-ERROR.
   
   QuestionnaireFlowHistoryBrowseForm = NEW dataForm("questionnaireflowhistory_browse_form").
   QuestionnaireFlowHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   QuestionnaireFlowHistoryBrowseForm:FormAction="dbQuestionnaireFlowUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireFlowHistoryBrowseForm:FormWidth   = 850.
   QuestionnaireFlowHistoryBrowseForm:FormHeight  = 540.
   QuestionnaireFlowHistoryBrowseForm:FormTitle   = fTL("History for Questionnarie Flow: ") + (IF AVAILABLE Questionnaireflow THEN  
                                                                  STRING(Questionnaireflow.QuestionnaireflowID) ELSE "").
   QuestionnaireFlowHistoryBrowseForm:FormType    = "xxl_large".
   
   QuestionnaireFlowHistoryBrowse = NEW browseTable("questionnaireflowhistory_browse").
   QuestionnaireFlowHistoryBrowse:BrowseWidth  = 830.
   QuestionnaireFlowHistoryBrowse:BrowseHeight = 500.
   QuestionnaireFlowHistoryBrowse:ExcelExport  = TRUE.
   QuestionnaireFlowHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QuestionnaireFlowHistoryBrowse:insertColumn(fTL("History ID"),         70, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionnaireFlowHistory}
   
   QuestionnaireFlowHistoryBrowse:insertColumn(fTL("Questionnaire"),     125, "CHARACTER", "left", FALSE).
   QuestionnaireFlowHistoryBrowse:insertColumn(fTL("Question Category"), 125, "CHARACTER", "left", FALSE).
   QuestionnaireFlowHistoryBrowse:insertColumn(fTL("Asking Sequence"),   125, "INTEGER", "left",   FALSE).
   QuestionnaireFlowHistoryBrowse:insertColumn(fTL("Active"),             50, "LOGICAL",           FALSE).
   QuestionnaireFlowHistoryBrowse:insertColumn(fTL("User"),              125, "CHARACTER", "left", FALSE).
   QuestionnaireFlowHistoryBrowse:insertColumn(fTL("Created"),           125, "CHARACTER", "left", FALSE).
   
   QuestionnaireFlowHistoryBrowse:StartBody().
   
   IF AVAILABLE QuestionnaireFlow THEN
   DO:
      /*List the QuestionnaireFlowHistory*/
      FOR EACH QuestionnaireFlowHistory NO-LOCK 
         WHERE  QuestionnaireFlowHistory.QuestionnaireFlowID = intSelectedQuestionnaireFlow
         BY QuestionnaireFlowHistory.QuestionnaireFlowHistoryID:
         
         FIND FIRST GateUser OF QuestionnaireFlowHistory NO-LOCK NO-ERROR. /* idx=GateUserID */
         FIND FIRST Questionnaire OF QuestionnaireFlowHistory NO-LOCK NO-ERROR. /* idx=QuestionnaireID */
         FIND FIRST QuestionCategory OF QuestionnaireFlowHistory NO-LOCK NO-ERROR. /* idx=QuestionCategoryID */
       
         QuestionnaireFlowHistoryBrowse:startRow(QuestionnaireFlowHistory.QuestionnaireFlowHistoryID, "selectQuestionnaireFlowHistoryRow(this," + '"' 
                                                   + STRING(QuestionnaireFlowHistory.QuestionnaireFlowHistoryID) + '","questionnaireFlowHistory"' + ");", "").
         
         QuestionnaireFlowHistoryBrowse:insertData(QuestionnaireFlowHistory.QuestionnaireFlowHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionnaireFlowHistory}
         
         QuestionnaireFlowHistoryBrowse:insertData((IF AVAILABLE Questionnaire THEN Questionnaire.QuestionnaireName ELSE ""), "left").
         QuestionnaireFlowHistoryBrowse:insertData((IF AVAILABLE QuestionCategory THEN QuestionCategory.QuestionCategoryName ELSE ""), "left").
         QuestionnaireFlowHistoryBrowse:insertData(QuestionnaireFlowHistory.AskingSequence).
         QuestionnaireFlowHistoryBrowse:insertData(STRING(QuestionnaireFlowHistory.Active, "Yes/No")).
         QuestionnaireFlowHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QuestionnaireFlowHistoryBrowse:insertData(fDisplayDate&Time(QuestionnaireFlowHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */    
         QuestionnaireFlowHistoryBrowse:insertHiddendata("QuestionnaireFlowHistoryID",QuestionnaireFlowHistory.QuestionnaireFlowHistoryID).     
         QuestionnaireFlowHistoryBrowse:insertHiddendata("QuestionnaireID",Questionnaire.QuestionnaireID).
         QuestionnaireFlowHistoryBrowse:insertHiddendata("QuestionnaireFlowID",QuestionnaireFlow.QuestionnaireFlowID).
         QuestionnaireFlowHistoryBrowse:insertHiddendata("QuestionnaireFlowVerisonID",QuestionnaireFlow.VersionID).
         
         QuestionnaireFlowHistoryBrowse:endRow().
      
      END. /* FOR EACH QuestionnaireFlow, */
   END. /*IF AVAILABLE Questionnaire THEN*/
   
   QuestionnaireFlowHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireFlowHistoryBrowse:getErrors().
   
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("QuestionnaireFlowHistoryID","").
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("QuestionnaireFlowID","").
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("questionnaire_browse_scroll","").
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("popup_questionnaireflow_browse","").
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("popup_questionnaireflowhistory_browse","").
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("form_name","questionnaireflowhistory_browse_form").
   QuestionnaireFlowHistoryBrowseForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireFlowHistoryBrowseForm}
   
   /* Create Button Bar */
   QuestionnaireFlowHistoryBrowseButtons = NEW buttonBar().
   
   QuestionnaireFlowHistoryBrowseButtons:addButton("questionnaireflowhistory_browse_form_btn_details",
                                                   fTL("Details"),
                                                   "viewQuestionnaireFlowHistoryDetails('questionnaireflowhistory_details_form');",
                                                   "Disabled").
                                             
     /*Button for later if needed*/                                 
/*   QuestionnaireFlowHistoryBrowseButtons:addButton("questionnaireflowhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_questionnaireflowhistory_browse.xml')").*/
   
   QuestionnaireFlowHistoryBrowseButtons:addButton("questionnaireflowhistory_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('questionnaireflowhistory_browse_form_popup');").
   
   QuestionnaireFlowHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireFlowHistoryBrowseForm:FormBrowse  = QuestionnaireFlowHistoryBrowse.
   QuestionnaireFlowHistoryBrowseForm:FormButtons = QuestionnaireFlowHistoryBrowseButtons.
   QuestionnaireFlowHistoryBrowseForm:endForm(). 
   
   QuestionnaireFlowHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionnaireFlowHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireFlowHistoryDetails Procedure
PROCEDURE pQuestionnaireFlowHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questionnaireflowhistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionnaireFlowID,QuestionCategoryID,AskingSequence,Active,QuestionnaireID,"
                               + "QuestionnaireFlowHistoryID,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QuestionnaireFlowHistoryDetailsForm = NEW dataForm("questionnaireflowhistory_details_form").
   QuestionnaireFlowHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   QuestionnaireFlowHistoryDetailsForm:FormAction = "dbQuestionnaireFlowUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireFlowHistoryDetailsForm:FormWidth   = 460.
   QuestionnaireFlowHistoryDetailsForm:FormHeight  = 300.
   QuestionnaireFlowHistoryDetailsForm:FormTitle   = "Questionnaire Flow History Details".
   QuestionnaireFlowHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QuestionnaireFlowHistoryDetailsForm:insertPaddingColumn(40).
   QuestionnaireFlowHistoryDetailsForm:insertColumn(120).
   QuestionnaireFlowHistoryDetailsForm:insertColumn(120).
   QuestionnaireFlowHistoryDetailsForm:insertColumn(20).
   QuestionnaireFlowHistoryDetailsForm:insertColumn(4).
   QuestionnaireFlowHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel(fTL("History ID")).
   QuestionnaireFlowHistoryDetailsForm:insertTextField("QuestionnaireFlowHistoryID", "", 200, TRUE).    
   
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel(fTL("Flow ID")).
   QuestionnaireFlowHistoryDetailsForm:insertTextField("QuestionnaireFlowID", "", 200, TRUE).    
   
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel(fTL("Questionnaire")).
   QuestionnaireFlowHistoryDetailsForm:insertComboField("QuestionnaireID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH Questionnaire NO-LOCK /*idx=QuestionCategoryID*/
      WHERE Questionnaire.Active = TRUE:      
      QuestionnaireFlowHistoryDetailsForm:insertComboPairs("QuestionnaireID", STRING(Questionnaire.QuestionnaireID), Questionnaire.QuestionnaireName).    
   END.
   
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel(fTL("Question Category")).
   QuestionnaireFlowHistoryDetailsForm:insertComboField("QuestionCategoryID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH QuestionCategory NO-LOCK /*idx=QuestionCategoryID*/
      WHERE QuestionCategory.Active = TRUE:      
      QuestionnaireFlowHistoryDetailsForm:insertComboPairs("QuestionCategoryID", STRING(QuestionCategory.QuestionCategoryID), QuestionCategory.QuestionCategoryName).    
   END.
   
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel(fTL("AskingSequence")).
   QuestionnaireFlowHistoryDetailsForm:insertTextField("AskingSequence", "", 200, TRUE).
   
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel(fTL("Active")).
   QuestionnaireFlowHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionnaireFlowHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireFlowHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel("User").
   QuestionnaireFlowHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QuestionnaireFlowHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QuestionnaireFlowHistoryDetailsForm:startRow().
   QuestionnaireFlowHistoryDetailsForm:insertLabel("Created").
   QuestionnaireFlowHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QuestionnaireFlowHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QuestionnaireFlowHistoryDetailsForm:insertLabel(":").
   QuestionnaireFlowHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("questionnaireflow_browse_scroll","").
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("questionnaireflowhistory_browse_scroll","").
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("popup_questionnaireflow_browse", "").
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("QuestionnaireFlowID","").
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("QuestionnaireFlowHistoryID","").
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("form_name","questionnaireflowhistory_details_form").
   QuestionnaireFlowHistoryDetailsForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireFlowHistoryDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireFlowHistoryDetailsButtons = NEW buttonBar().
   
   QuestionnaireFlowHistoryDetailsButtons:addButton("questionnaireflowhistory_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('questionnaireflowhistory_details_form_popup');").
                                        
   QuestionnaireFlowHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireFlowHistoryDetailsForm:FormButtons = QuestionnaireFlowHistoryDetailsButtons.
   
   QuestionnaireFlowHistoryDetailsForm:endForm(). 
   QuestionnaireFlowHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionnaireOpTypeLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireOpTypeLinkBrowse Procedure
PROCEDURE pQuestionnaireOpTypeLinkBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionnaireoptypelink_details_form"}
   
   FIND FIRST Questionnaire NO-LOCK /* idx=QuestionnaireID */
      WHERE Questionnaire.QuestionnaireID = intSelectedQuestionnaire NO-ERROR.
   
   QuestionnaireOpTypeLinkBrowseForm = NEW dataForm("questionnaireoptypelink_browse_form").
   QuestionnaireOpTypeLinkBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   QuestionnaireOpTypeLinkBrowseForm:FormAction="dbQuestionnaireOpTypeLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireOpTypeLinkBrowseForm:FormWidth   = 850.
   QuestionnaireOpTypeLinkBrowseForm:FormHeight  = 540.
   QuestionnaireOpTypeLinkBrowseForm:FormTitle   = fTL("Part and Opereration Links for Questionnaire: ") + (IF AVAILABLE Questionnaire THEN  
                                                                  Questionnaire.QuestionnaireName ELSE "").
   QuestionnaireOpTypeLinkBrowseForm:FormType    = "xxl_large".
   
   QuestionnaireOpTypeLinkBrowse = NEW browseTable("questionnaireoptypelink_browse").
   QuestionnaireOpTypeLinkBrowse:BrowseWidth  = 830.
   QuestionnaireOpTypeLinkBrowse:BrowseHeight = 500.
   QuestionnaireOpTypeLinkBrowse:ExcelExport  = TRUE.
   QuestionnaireOpTypeLinkBrowse:SessionID    = intGblSessionID.
   
   
   QuestionnaireOpTypeLinkBrowse:insertColumn(fTL("OpTypeLink ID"),  90, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionnaireOpTypeLink}
   
   QuestionnaireOpTypeLinkBrowse:insertColumn(fTL("PartRef"),       125, "CHARACTER", "left", FALSE).
   QuestionnaireOpTypeLinkBrowse:insertColumn(fTL("OperationType"), 125, "INTEGER", "left",   FALSE).
   QuestionnaireOpTypeLinkBrowse:insertColumn(fTL("Active"),         70, "LOGICAL",           FALSE).
   
   QuestionnaireOpTypeLinkBrowse:StartBody().
   
   IF AVAILABLE Questionnaire THEN
   DO:
      /*List the QuestionnaireOpTypeLink*/
      FOR EACH QuestionnaireOpTypeLink NO-LOCK 
         WHERE QuestionnaireOpTypeLink.QuestionnaireID = intSelectedQuestionnaire
         BY    QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID:
         
         FIND FIRST Part OF QuestionnaireOpTypeLink NO-LOCK NO-ERROR. /* idx=PartID */
         
         FIND FIRST performedOperationType NO-LOCK /* idx=OperationTypeID */
            WHERE performedOperationType.OperationTypeID = QuestionnaireOpTypeLink.PerformedOperationTypeID NO-ERROR.
       
         QuestionnaireOpTypeLinkBrowse:startRow(QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID, "selectQuestionnaireOpTypeLinkRow(this," + '"' 
                                          + STRING(QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID) + '"' + ");", "").                                                            
                                                                     
         QuestionnaireOpTypeLinkBrowse:insertData(QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionnaireOpTypeLink}
         
         QuestionnaireOpTypeLinkBrowse:insertData((IF AVAILABLE Part THEN Part.PartRef ELSE ""), "left").
         QuestionnaireOpTypeLinkBrowse:insertData((IF AVAILABLE performedOperationType THEN performedOperationType.TypeName ELSE "") ,"LEFT").
         QuestionnaireOpTypeLinkBrowse:insertData(STRING(QuestionnaireOpTypeLink.Active, "Yes/No")).
         
         
         /* Add hidden fields */         
         QuestionnaireOpTypeLinkBrowse:insertHiddenData("QuestionnaireID",Questionnaire.QuestionnaireID).
         QuestionnaireOpTypeLinkBrowse:insertHiddenData("QuestionnaireVerisonID",Questionnaire.VersionID).
         QuestionnaireOpTypeLinkBrowse:insertHiddenData("QuestionnaireOpTypeLinkID",QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID).
         QuestionnaireOpTypeLinkBrowse:insertHiddenData("QuestionnaireOpTypeLinkVersionID",QuestionnaireOpTypeLink.VersionID).
         
         QuestionnaireOpTypeLinkBrowse:endRow().
      
      END. /* FOR EACH QuestionnaireOpTypeLink */
   END. /*IF AVAILABLE Questionnaire THEN*/
   
   QuestionnaireOpTypeLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireOpTypeLinkBrowse:getErrors().
   
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("QuestionnaireVerisonID","").
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("QuestionnaireOpTypeLinkID","").
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("QuestionnaireOpTypeLinkVerisonID","").
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("questionnaireoptypelink_browse_scroll","").
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("popup_questionnaireoptypelink_browse","").
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("popup_questionnaireoptypelinkhist_browse","").
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("form_name","questionnaireoptypelink_browse_form").
   QuestionnaireOpTypeLinkBrowseForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireOpTypeLinkBrowseForm}
   
   /* Create Button Bar */
   QuestionnaireOpTypeLinkBrowseButtons = NEW buttonBar().
   
   QuestionnaireOpTypeLinkBrowseButtons:addButton("questionnaireoptypelink_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQuestionnaireOpTypeLink('questionnaireoptypelink_details_form');",
                                             "").                                                 
   
   QuestionnaireOpTypeLinkBrowseButtons:addButton("questionnaireoptypelink_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQuestionnaireOpTypeLinkDetails('questionnaireoptypelink_details_form');",
                                             "Disabled").
                                             
   QuestionnaireOpTypeLinkBrowseButtons:addButton("questionnaireoptypelink_browse_form_btn_history",
                                             fTL("History"),
                                             "viewQuestionnaireOpTypeLinkHist();",
                                             "Disabled").                                          
     
     /*Button for later if needed*/                                 
/*   QuestionnaireOpTypeLinkBrowseButtons:addButton("questionnaireoptypelink_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_questionnaireoptypelink_browse.xml')").*/
   
   QuestionnaireOpTypeLinkBrowseButtons:addButton("questionnaireoptypelink_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('questionnaireoptypelink_browse_form_popup');").
   
   QuestionnaireOpTypeLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireOpTypeLinkBrowseForm:FormBrowse  = QuestionnaireOpTypeLinkBrowse.
   QuestionnaireOpTypeLinkBrowseForm:FormButtons = QuestionnaireOpTypeLinkBrowseButtons.
   QuestionnaireOpTypeLinkBrowseForm:endForm(). 
   
   QuestionnaireOpTypeLinkBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionnaireOpTypeLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireOpTypeLinkDetails Procedure
PROCEDURE pQuestionnaireOpTypeLinkDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questionnaireoptypelink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionnaireOpTypeLinkID,QuestionnaireID,PartID,PerformedOperationTypeID,Active" 
          chrEditFieldList     = "Active" 
          chrNewFieldList      = "PartID,PerformedOperationTypeID,Active" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
                             
   
   QuestionnaireOpTypeLinkDetailsForm = NEW dataForm("questionnaireoptypelink_details_form").
   QuestionnaireOpTypeLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   QuestionnaireOpTypeLinkDetailsForm:FormAction = "dbQuestionnaireOpTypeLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireOpTypeLinkDetailsForm:FormWidth   = 460.
   QuestionnaireOpTypeLinkDetailsForm:FormHeight  = 200.
   QuestionnaireOpTypeLinkDetailsForm:FormTitle   = "Questionnaire OpTypeLink Details".
   QuestionnaireOpTypeLinkDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QuestionnaireOpTypeLinkDetailsForm:insertPaddingColumn(40).
   QuestionnaireOpTypeLinkDetailsForm:insertColumn(120).
   QuestionnaireOpTypeLinkDetailsForm:insertColumn(120).
   QuestionnaireOpTypeLinkDetailsForm:insertColumn(20).
   QuestionnaireOpTypeLinkDetailsForm:insertColumn(4).
   QuestionnaireOpTypeLinkDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionnaireOpTypeLinkDetailsForm:startRow().
   QuestionnaireOpTypeLinkDetailsForm:insertLabel(fTL("OpTypeLink ID")).
   QuestionnaireOpTypeLinkDetailsForm:insertTextField("QuestionnaireOpTypeLinkID", "", 200, TRUE).    
   
   QuestionnaireOpTypeLinkDetailsForm:startRow().
   QuestionnaireOpTypeLinkDetailsForm:insertLabel(fTL("Questionnaire")).
   QuestionnaireOpTypeLinkDetailsForm:insertComboField("QuestionnaireID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH Questionnaire NO-LOCK /*idx=QuestionCategoryID*/
      WHERE Questionnaire.Active = TRUE:      
         
      QuestionnaireOpTypeLinkDetailsForm:insertComboPairs("QuestionnaireID", STRING(Questionnaire.QuestionnaireID), Questionnaire.QuestionnaireName).    
   END.
   
   QuestionnaireOpTypeLinkDetailsForm:startRow().
   QuestionnaireOpTypeLinkDetailsForm:insertLabel(fTL("Part")).
   QuestionnaireOpTypeLinkDetailsForm:insertComboField("PartID", "", 200, TRUE).
   /* Part DropDown Options */
   FOR EACH Part NO-LOCK /*idx=PartID*/
      WHERE Part.Active = TRUE:  
             
      QuestionnaireOpTypeLinkDetailsForm:insertComboPairs("PartID", STRING(Part.PartID), Part.PartRef).    
   END.
   
   QuestionnaireOpTypeLinkDetailsForm:startRow().
   QuestionnaireOpTypeLinkDetailsForm:insertLabel(fTL("Operation Type")).
   QuestionnaireOpTypeLinkDetailsForm:insertComboField("PerformedOperationTypeID", "", 200, TRUE).
   FOR EACH OperationType NO-LOCK /*idx=OperationTypeID*/
      WHERE OperationType.Active
      BY    OperationType.TypeName:
      
      QuestionnaireOpTypeLinkDetailsForm:insertComboPairs("PerformedOperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName). 
   END. 
   
   QuestionnaireOpTypeLinkDetailsForm:startRow().
   QuestionnaireOpTypeLinkDetailsForm:insertLabel(fTL("Active")).
   QuestionnaireOpTypeLinkDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionnaireOpTypeLinkDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireOpTypeLinkDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add Hidden Fields*/
   QuestionnaireOpTypeLinkDetailsForm:insertHiddenField("questionnaireoptypelink_browse_scroll","").
   QuestionnaireOpTypeLinkDetailsForm:insertHiddenField("popup_questionnaireoptypelink_browse", "").
   QuestionnaireOpTypeLinkDetailsForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireOpTypeLinkDetailsForm:insertHiddenField("QuestionnaireOpTypeLinkID","").
   QuestionnaireOpTypeLinkDetailsForm:insertHiddenField("QuestionnaireOpTypeLinkVerisonID","").
   QuestionnaireOpTypeLinkDetailsForm:insertHiddenField("form_name","questionnaireoptypelink_details_form").
   QuestionnaireOpTypeLinkDetailsForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireOpTypeLinkDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireOpTypeLinkDetailsButtons = NEW buttonBar().
   
   QuestionnaireOpTypeLinkDetailsButtons:addButton("questionnaireoptypelink_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQuestionnaireOpTypeLink('questionnaireoptypelink_details_form');").
   
   QuestionnaireOpTypeLinkDetailsButtons:addButton("questionnaireoptypelink_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('questionnaireoptypelink_details_form_popup');").
                                        
   QuestionnaireOpTypeLinkDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireOpTypeLinkDetailsForm:FormButtons = QuestionnaireOpTypeLinkDetailsButtons.
   
   QuestionnaireOpTypeLinkDetailsForm:endForm(). 
   QuestionnaireOpTypeLinkDetailsForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionnaireOpTypeLinkHist) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireOpTypeLinkHist Procedure
PROCEDURE pQuestionnaireOpTypeLinkHist:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionnaireoptypelinkhist_details_form"}
   
   FIND FIRST QuestionnaireFlow WHERE QuestionnaireFlow.QuestionnaireFlowID = intSelectedQuestionnaireFlow NO-LOCK NO-ERROR.
   
   QuestionnaireOpTypeLinkHistBrowseForm = NEW dataForm("questionnaireoptypelinkhist_browse_form").
   QuestionnaireOpTypeLinkHistBrowseForm:WebStream = STREAM WebStream:HANDLE.
   QuestionnaireOpTypeLinkHistBrowseForm:FormAction="dbQuestionnaireOpTypeLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireOpTypeLinkHistBrowseForm:FormWidth   = 850.
   QuestionnaireOpTypeLinkHistBrowseForm:FormHeight  = 540.
   QuestionnaireOpTypeLinkHistBrowseForm:FormTitle   = fTL("History for Questionnarie OpTypeLink: ") + (IF AVAILABLE QuestionnaireOpTypeLink THEN  
                                                                  STRING(QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID) ELSE "").
   QuestionnaireOpTypeLinkHistBrowseForm:FormType    = "xxl_large".
   
   QuestionnaireOpTypeLinkHistBrowse = NEW browseTable("questionnaireoptypelinkhist_browse").
   QuestionnaireOpTypeLinkHistBrowse:BrowseWidth  = 830.
   QuestionnaireOpTypeLinkHistBrowse:BrowseHeight = 500.
   QuestionnaireOpTypeLinkHistBrowse:ExcelExport  = TRUE.
   QuestionnaireOpTypeLinkHistBrowse:SessionID    = intGblSessionID.
   
   
   QuestionnaireOpTypeLinkHistBrowse:insertColumn(fTL("History ID"),         70, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionnaireOpTypeLinkHist}
   
   QuestionnaireOpTypeLinkHistBrowse:insertColumn(fTL("Questionnaire"),     125, "CHARACTER", "left", FALSE).
   QuestionnaireOpTypeLinkHistBrowse:insertColumn(fTL("Part"), 125, "CHARACTER", "left", FALSE).
   QuestionnaireOpTypeLinkHistBrowse:insertColumn(fTL("OperationType"),   125, "INTEGER", "left",   FALSE).
   QuestionnaireOpTypeLinkHistBrowse:insertColumn(fTL("Active"),             50, "LOGICAL",           FALSE).
   QuestionnaireOpTypeLinkHistBrowse:insertColumn(fTL("User"),              125, "CHARACTER", "left", FALSE).
   QuestionnaireOpTypeLinkHistBrowse:insertColumn(fTL("Created"),           125, "CHARACTER", "left", FALSE).
   
   QuestionnaireOpTypeLinkHistBrowse:StartBody().
   
   IF AVAILABLE QuestionnaireOpTypeLink THEN
   DO:
      /*List the QuestionnaireOpTypeLinkHist*/
      FOR EACH QuestionnaireOpTypeLinkHist NO-LOCK 
         WHERE QuestionnaireOpTypeLinkHist.QuestionnaireOpTypeLinkID = intSelectedQuestionnaireOpTypeLink
         BY QuestionnaireOpTypeLinkHist.QuestionnaireOpTypeLinkHistID:
         
         FIND FIRST GateUser OF QuestionnaireOpTypeLinkHist NO-LOCK NO-ERROR. /* idx=GateUserID */
         FIND FIRST Part OF QuestionnaireOpTypeLinkHist NO-LOCK NO-ERROR. /* idx=PartID */
         FIND FIRST Questionnaire OF QuestionnaireOpTypeLinkHist NO-LOCK NO-ERROR. /* idx=QuestionnaireID */
         
         FIND FIRST performedOperationType NO-LOCK /* idx=OperationTypeID */
            WHERE performedOperationType.OperationTypeID = QuestionnaireOpTypeLinkHist.PerformedOperationTypeID NO-ERROR.
       
         QuestionnaireOpTypeLinkHistBrowse:startRow(QuestionnaireOpTypeLinkHist.QuestionnaireOpTypeLinkHistID, "selectQuestionnaireOpTypeLinkHistRow(this," + '"' 
                                                   + STRING(QuestionnaireOpTypeLinkHist.QuestionnaireOpTypeLinkHistID) + '","questionnaireOpTypeLinkHist"' + ");", "").
         
         QuestionnaireOpTypeLinkHistBrowse:insertData(QuestionnaireOpTypeLinkHist.QuestionnaireOpTypeLinkHistID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionnaireOpTypeLinkHist}
         
         QuestionnaireOpTypeLinkHistBrowse:insertData((IF AVAILABLE Questionnaire THEN Questionnaire.QuestionnaireName ELSE ""), "left").
         QuestionnaireOpTypeLinkHistBrowse:insertData((IF AVAILABLE Part THEN Part.PartRef ELSE ""), "left").
         QuestionnaireOpTypeLinkHistBrowse:insertData((IF AVAILABLE performedOperationType THEN performedOperationType.TypeName ELSE "") ,"LEFT").
         QuestionnaireOpTypeLinkHistBrowse:insertData(STRING(QuestionnaireOpTypeLink.Active, "Yes/No")).
         QuestionnaireOpTypeLinkHistBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QuestionnaireOpTypeLinkHistBrowse:insertData(fDisplayDate&Time(QuestionnaireOpTypeLinkHist.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */    
         QuestionnaireOpTypeLinkHistBrowse:insertHiddendata("QuestionnaireOpTypeLinkHistID",QuestionnaireOpTypeLinkHist.QuestionnaireOpTypeLinkHistID).     
         QuestionnaireOpTypeLinkHistBrowse:insertHiddendata("QuestionnaireID",Questionnaire.QuestionnaireID).
         QuestionnaireOpTypeLinkHistBrowse:insertHiddendata("QuestionnaireOpTypeLinkID",QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID).
         QuestionnaireOpTypeLinkHistBrowse:insertHiddendata("QuestionnaireOpTypeLinkVerisonID",QuestionnaireOpTypeLink.VersionID).
         
         QuestionnaireOpTypeLinkHistBrowse:endRow().
      
      END. /* FOR EACH QuestionnaireFlow, */
   END. /*IF AVAILABLE Questionnaire THEN*/
   
   QuestionnaireOpTypeLinkHistBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireOpTypeLinkHistBrowse:getErrors().
   
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("QuestionnaireOpTypeLinkHistID","").
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("QuestionnaireOpTypeLinkID","").
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("questionnaireoptypelink_browse_scroll","").
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("popup_questionnaireoptypelink_browse","").
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("popup_questionnaireoptypelinkhist_browse","").
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("form_name","questionnaireoptypelinkhist_browse_form").
   QuestionnaireOpTypeLinkHistBrowseForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireOpTypeLinkHistBrowseForm}
   
   /* Create Button Bar */
   QuestionnaireOpTypeLinkHistBrowseButtons = NEW buttonBar().
   
   QuestionnaireOpTypeLinkHistBrowseButtons:addButton("questionnaireoptypelinkhist_browse_form_btn_details",
                                                   fTL("Details"),
                                                   "viewQuestionnaireOpTypeLinkHistDetails('questionnaireoptypelinkhist_details_form');",
                                                   "Disabled").
                                             
     /*Button for later if needed*/                                 
/*   QuestionnaireOpTypeLinkHistBrowseButtons:addButton("questionnaireoptypelinkhist_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_questionnaireoptypelinkhist_browse.xml')").*/
   
   QuestionnaireOpTypeLinkHistBrowseButtons:addButton("questionnaireoptypelinkhist_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('questionnaireoptypelinkhist_browse_form_popup');").
   
   QuestionnaireOpTypeLinkHistBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireOpTypeLinkHistBrowseForm:FormBrowse  = QuestionnaireOpTypeLinkHistBrowse.
   QuestionnaireOpTypeLinkHistBrowseForm:FormButtons = QuestionnaireOpTypeLinkHistBrowseButtons.
   QuestionnaireOpTypeLinkHistBrowseForm:endForm(). 
   
   QuestionnaireOpTypeLinkHistBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQuestionnaireOpTypeLinkHistDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireOpTypeLinkHistDetails Procedure
PROCEDURE pQuestionnaireOpTypeLinkHistDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questionnaireoptypelinkhist_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionnaireOpTypeLinkID,PerformedOperationTypeID,Active,QuestionnaireID,PartID,"
                               + "QuestionnaireOpTypeLinkHistID,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QuestionnaireOpTypeLinkHistDetailsForm = NEW dataForm("questionnaireoptypelinkhist_details_form").
   QuestionnaireOpTypeLinkHistDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   QuestionnaireOpTypeLinkHistDetailsForm:FormAction = "dbQuestionnaireOpTypeLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireOpTypeLinkHistDetailsForm:FormWidth   = 460.
   QuestionnaireOpTypeLinkHistDetailsForm:FormHeight  = 300.
   QuestionnaireOpTypeLinkHistDetailsForm:FormTitle   = "Questionnaire OpTypeLink History Details".
   QuestionnaireOpTypeLinkHistDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QuestionnaireOpTypeLinkHistDetailsForm:insertPaddingColumn(40).
   QuestionnaireOpTypeLinkHistDetailsForm:insertColumn(120).
   QuestionnaireOpTypeLinkHistDetailsForm:insertColumn(120).
   QuestionnaireOpTypeLinkHistDetailsForm:insertColumn(20).
   QuestionnaireOpTypeLinkHistDetailsForm:insertColumn(4).
   QuestionnaireOpTypeLinkHistDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel(fTL("History ID")).
   QuestionnaireOpTypeLinkHistDetailsForm:insertTextField("QuestionnaireOpTypeLinkHistID", "", 200, TRUE).    
   
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel(fTL("OpTypeLink ID")).
   QuestionnaireOpTypeLinkHistDetailsForm:insertTextField("QuestionnaireOpTypeLinkID", "", 200, TRUE).    
   
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel(fTL("Questionnaire")).
   QuestionnaireOpTypeLinkHistDetailsForm:insertComboField("QuestionnaireID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH Questionnaire NO-LOCK /*idx=QuestionCategoryID*/
      WHERE Questionnaire.Active = TRUE:      
      QuestionnaireOpTypeLinkHistDetailsForm:insertComboPairs("QuestionnaireID", STRING(Questionnaire.QuestionnaireID), Questionnaire.QuestionnaireName).    
   END.
   
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel(fTL("PartID")).
   QuestionnaireOpTypeLinkHistDetailsForm:insertComboField("PartID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH Part NO-LOCK /*idx=PartID*/
      WHERE Part.Active = TRUE:      
      QuestionnaireOpTypeLinkHistDetailsForm:insertComboPairs("PartID", STRING(Part.PartID), Part.PartRef).    
   END.
   
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel(fTL("OperationType")).
   QuestionnaireOpTypeLinkHistDetailsForm:insertComboField("PerformedOperationTypeID", "", 200, TRUE).
    FOR EACH OperationType NO-LOCK /*idx=OperationTypeID*/
      WHERE OperationType.Active
      BY    OperationType.TypeName:
      
      QuestionnaireOpTypeLinkHistDetailsForm:insertComboPairs("PerformedOperationTypeID", STRING(OperationType.OperationTypeID), OperationType.TypeName). 
   END.
   
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel(fTL("Active")).
   QuestionnaireOpTypeLinkHistDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionnaireOpTypeLinkHistDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireOpTypeLinkHistDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel("User").
   QuestionnaireOpTypeLinkHistDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QuestionnaireOpTypeLinkHistDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QuestionnaireOpTypeLinkHistDetailsForm:startRow().
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel("Created").
   QuestionnaireOpTypeLinkHistDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QuestionnaireOpTypeLinkHistDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QuestionnaireOpTypeLinkHistDetailsForm:insertLabel(":").
   QuestionnaireOpTypeLinkHistDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("questionnaireflow_browse_scroll","").
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("questionnaireoptypelinkhist_browse_scroll","").
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("popup_questionnaireflow_browse", "").
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("QuestionnaireOpTypeLinkID","").
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("QuestionnaireOpTypeLinkHistID","").
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("form_name","questionnaireoptypelinkhist_details_form").
   QuestionnaireOpTypeLinkHistDetailsForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireOpTypeLinkHistDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireOpTypeLinkHistDetailsButtons = NEW buttonBar().
   
   QuestionnaireOpTypeLinkHistDetailsButtons:addButton("questionnaireoptypelinkhist_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('questionnaireoptypelinkhist_details_form_popup');").
                                        
   QuestionnaireOpTypeLinkHistDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireOpTypeLinkHistDetailsForm:FormButtons = QuestionnaireOpTypeLinkHistDetailsButtons.
   
   QuestionnaireOpTypeLinkHistDetailsForm:endForm(). 
   QuestionnaireOpTypeLinkHistDetailsForm:displayForm(). 

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
   
   ASSIGN chrQuestionnaireID                    = get-value("QuestionnaireID")
          intSelectedQuestionnaire              = INTEGER(chrQuestionnaireID)
          chrQuestionnaireFlowID                = get-value("QuestionnaireFlowID")
          intSelectedQuestionnaireFlow          = INTEGER(chrQuestionnaireFlowID)
          chrQuestionnaireOpTypeLinkID          = get-value("QuestionnaireOpTypeLinkID")
          intSelectedQuestionnaireOpTypeLink    = INTEGER(chrQuestionnaireOpTypeLinkID)
          chrScrollToQuestionnaireOpTypeLinkRow = STRING(INTEGER(get-value("questionnaireoptypelink_browse_scroll")))
          chrScrollToQuestionnaireFlowRow       = STRING(INTEGER(get-value("questionnaireflow_browse_scroll")))
          chrScrollToQuestionnaireRow           = STRING(INTEGER(get-value("questionnaire_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQuestionnaireID <> "" THEN
     chrSelectQuestionnaireRow = 'selectQuestionnaireRow(document.getElementById("questionnaire_browse_row_' + chrQuestionnaireID + '"),"' 
                                                         + chrQuestionnaireID +  '");'.
                                                         
   IF chrQuestionnaireFlowID <> "" THEN
     chrSelectQuestionnaireFlowRow = 'selectQuestionnaireFlowRow(document.getElementById("questionnaireflow_browse_row_' 
                                   + chrQuestionnaireFlowID + '"),"' + chrQuestionnaireFlowID +  '");'.     
                                   
   IF chrQuestionnaireOpTypeLinkID <> "" THEN
     chrSelectQuestionnaireOpTypeLinkRow = 'selectQuestionnaireOpTypeLinkRow(document.getElementById("questionnaireoptypelink_browse_row_' 
                                   + chrQuestionnaireOpTypeLinkID + '"),"' + chrQuestionnaireOpTypeLinkID +  '");'.                                                                                 
   
   IF get-value('popup_questionnairehistory_browse') = "yes" THEN
      chrPopupQuestionnaireHistory  = 'enablePopup("questionnairehistory_browse_form_popup");'.
      
   IF get-value('popup_questionnaireflow_browse') = "yes" THEN
      chrPopupQuestionnaireFlow  = 'enablePopup("questionnaireflow_browse_form_popup");'.   
      
   IF get-value('popup_questionnaireflowhistory_browse') = "yes" THEN
      chrPopupQuestionnaireFlowHistory  = 'enablePopup("questionnaireflowhistory_browse_form_popup");'.   
      
   IF get-value('popup_questionnaireoptypelink_browse') = "yes" THEN
      chrPopupQuestionnaireOpTypeLink  = 'enablePopup("questionnaireoptypelink_browse_form_popup");'.      
      
   IF get-value('popup_questionnaireoptypelinkhist_browse') = "yes" THEN
      chrPopupQuestionnaireOpTypeLinkHist  = 'enablePopup("questionnaireoptypelinkhist_browse_form_popup");'.   
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("questionnaire_browse").scrollTop=' + chrScrollToQuestionnaireRow 
                                                          + chrSelectQuestionnaireRow 
                                                          + chrPopupQuestionnaireHistory
                                                          + chrPopupQuestionnaireFlow
                                                          + chrSelectQuestionnaireFlowRow
                                                          + chrPopupQuestionnaireFlowHistory
                                                          + chrPopupQuestionnaireOpTypeLink
                                                          + chrSelectQuestionnaireOpTypeLinkRow
                                                          + chrPopupQuestionnaireOpTypeLinkHist.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Questionnaire Admin".
   ThisPage:FrameTitle    = "Questionnaire Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("questionnaire.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQuestionnaireBrowse.
   
   FIND FIRST Questionnaire NO-LOCK /* idx=QuestionnaireID */
      WHERE Questionnaire.QuestionnaireID = intSelectedQuestionnaire NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQuestionnaireDetails.
   RUN pQuestionnaireHistory.
   RUN pQuestionnaireHistoryDetails.
   RUN pQuestionnaireFlowBrowse.
   
   FIND FIRST QuestionnaireFlow NO-LOCK /* idx=QuestionnaireFlowID */
      WHERE QuestionnaireFlow.QuestionnaireFlowID = intSelectedQuestionnaireFlow NO-ERROR.
   
   RUN pQuestionnaireFlowDetails.
   RUN pQuestionnaireFlowHistory.
   RUN pQuestionnaireFlowHistoryDetails.
   RUN pQuestionnaireOpTypeLinkBrowse.
   
   FIND FIRST QuestionnaireOpTypeLink NO-LOCK /* idx=QuestionnaireFlowID */
      WHERE QuestionnaireOpTypeLink.QuestionnaireOpTypeLinkID = intSelectedQuestionnaireOpTypeLink NO-ERROR.
      
   RUN pQuestionnaireOpTypeLinkDetails.   
   RUN pQuestionnaireOpTypeLinkHist.
   RUN pQuestionnaireOpTypeLinkHistDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QuestionnaireBrowseFrame                  NO-ERROR.
   DELETE OBJECT QuestionnaireBrowse                       NO-ERROR.
   DELETE OBJECT QuestionnaireBrowseButtons                NO-ERROR.
   DELETE OBJECT QuestionnaireDetailsForm                  NO-ERROR.
   DELETE OBJECT QuestionnaireDetailsButtons               NO-ERROR.
   
   DELETE OBJECT QuestionnaireHistoryBrowseForm            NO-ERROR.   
   DELETE OBJECT QuestionnaireHistoryBrowse                NO-ERROR.
   DELETE OBJECT QuestionnaireHistoryBrowseButtons         NO-ERROR.
   
   DELETE OBJECT QuestionnaireHistoryDetailsForm           NO-ERROR.
   DELETE OBJECT QuestionnaireHistoryDetailsButtons        NO-ERROR.
   
   DELETE OBJECT QuestionnaireFlowBrowseForm               NO-ERROR.   
   DELETE OBJECT QuestionnaireFlowBrowse                   NO-ERROR.
   DELETE OBJECT QuestionnaireFlowBrowseButtons            NO-ERROR.
   
   DELETE OBJECT QuestionnaireFlowDetailsForm              NO-ERROR.
   DELETE OBJECT QuestionnaireFlowDetailsButtons           NO-ERROR.
   
   DELETE OBJECT QuestionnaireFlowHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT QuestionnaireFlowHistoryBrowse            NO-ERROR.
   DELETE OBJECT QuestionnaireFlowHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT QuestionnaireFlowHistoryDetailsForm       NO-ERROR.
   DELETE OBJECT QuestionnaireFlowHistoryDetailsButtons    NO-ERROR.
   
   DELETE OBJECT QuestionnaireOpTypeLinkBrowseForm         NO-ERROR.   
   DELETE OBJECT QuestionnaireOpTypeLinkBrowse             NO-ERROR.
   DELETE OBJECT QuestionnaireOpTypeLinkBrowseButtons      NO-ERROR.
   
   DELETE OBJECT QuestionnaireOpTypeLinkDetailsForm        NO-ERROR.
   DELETE OBJECT QuestionnaireOpTypeLinkDetailsButtons     NO-ERROR.
   
   DELETE OBJECT QuestionnaireOpTypeLinkHistBrowseForm     NO-ERROR.   
   DELETE OBJECT QuestionnaireOpTypeLinkHistBrowse         NO-ERROR.
   DELETE OBJECT QuestionnaireOpTypeLinkHistBrowseButtons  NO-ERROR.
   
   DELETE OBJECT QuestionnaireOpTypeLinkHistDetailsForm    NO-ERROR.
   DELETE OBJECT QuestionnaireOpTypeLinkHistDetailsButtons NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireBrowse Procedure 
PROCEDURE pQuestionnaireBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "questionnaire_details_form"}
   
   QuestionnaireBrowse = NEW browseTable("questionnaire_browse").
   QuestionnaireBrowse:BrowseWidth  = 965.
   QuestionnaireBrowse:BrowseHeight = 455.
   QuestionnaireBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QuestionnaireBrowse:insertColumn(fTL("QuestionnaireID"),     120, "INTEGER", "left",   FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i Questionnaire}
   
   QuestionnaireBrowse:insertColumn(fTL("Questionnaire Code"),  130, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Questionnaire Name"),  130, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Questionnaire Descr"), 180, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Starting Categoy"),    130, "CHARACTER", "left", FALSE).
   QuestionnaireBrowse:insertColumn(fTL("Active"),               50, "LOGICAL",           FALSE).
   
   /*Body*/
   QuestionnaireBrowse:startBody().
   
   FOR EACH Questionnaire NO-LOCK: /*idx=ActiveListingSequence*/
      
      QuestionnaireBrowse:startRow(Questionnaire.QuestionnaireID, "selectQuestionnaireRow(this," + '"' + STRING(Questionnaire.QuestionnaireID) 
                                                                  + '"' + ");", "").
      QuestionnaireBrowse:insertData(Questionnaire.QuestionnaireID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i Questionnaire}
      
      QuestionnaireBrowse:insertData(Questionnaire.QuestionnaireCode, "left").
      QuestionnaireBrowse:insertData(Questionnaire.QuestionnaireName, "left").
      QuestionnaireBrowse:insertData(Questionnaire.QuestionnaireDescr, "left").
      QuestionnaireBrowse:insertData(Questionnaire.QuestionnaireCode, "left").
      QuestionnaireBrowse:insertData(STRING(Questionnaire.Active,"Yes/No")).
      
      /* Add hidden fields */
      QuestionnaireBrowse:insertHiddenData("QuestionnaireVersionID",Questionnaire.VersionID).
      
      QuestionnaireBrowse:endRow().
      
   END. /*FOR EACH Questionnaire NO-LOCK */
   
   QuestionnaireBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireBrowse:getErrors().
   
   /* Create a new frame */
   QuestionnaireBrowseFrame = NEW pageFrame().
   QuestionnaireBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QuestionnaireBrowseFrame:FormAction="dbQuestionnaireUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QuestionnaireBrowseFrame:formOpen("questionnaire_browse_form").
   
   /* Start the Frame Header */
   QuestionnaireBrowseFrame:insertSpacer(5).
   QuestionnaireBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QuestionnaireBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QuestionnaireBrowseFrame:frameClose().
   QuestionnaireBrowseFrame:insertSpacer(10).
   
   QuestionnaireBrowseFrame:insertHiddenField("questionnaire_browse_scroll","").
   QuestionnaireBrowseFrame:insertHiddenField("QuestionnaireID",chrQuestionnaireID).
   QuestionnaireBrowseFrame:insertHiddenField("QuestionnaireVersionID","").
   QuestionnaireBrowseFrame:insertHiddenField("popup_questionnairehistory_browse","").
   QuestionnaireBrowseFrame:insertHiddenField("popup_questionnaireflow_browse","").
   QuestionnaireBrowseFrame:insertHiddenField("popup_questionnaireflowhistory_browse","").
   QuestionnaireBrowseFrame:insertHiddenField("popup_questionnaireoptypelink_browse","").
   QuestionnaireBrowseFrame:insertHiddenField("popup_questionnaireoptypelinkhist_browse","").
   QuestionnaireBrowseFrame:insertHiddenField("form_name","questionnaire_browse_form").
   QuestionnaireBrowseFrame:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireBrowseFrame}
   
   QuestionnaireBrowseFrame:formClose().
   
   /* Create Button Bar */
   QuestionnaireBrowseButtons = NEW buttonBar().
   QuestionnaireBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQuestionnaire('questionnaire_details_form');",
                                             "").
   
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQuestionnaireDetails('questionnaire_details_form');",
                                             "Disabled").
   
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_questionnaireflow",
                                             fTL("Question Flows"),
                                             "viewQuestionnaireFlowBrowse();",
                                             "Disabled").                                          
   
   QuestionnaireBrowseButtons:addButton("questionnaire_browse_form_btn_partoptypelink",
                                             fTL("Part OpType Links"),
                                             "viewQuestionnaireOpTypeLinkBrowse();",
                                             "Disabled").
                                             
   QuestionnaireBrowseButtons:closeBar().  
   QuestionnaireBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireDetails Procedure 
PROCEDURE pQuestionnaireDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "questionnaire_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QuestionnaireID,QuestionnaireCode,QuestionnaireName,QuestionnaireDescr,Active" 
          chrEditFieldList     = "QuestionnaireDescr,Active" 
          chrNewFieldList      = "QuestionnaireCode,QuestionnaireName,QuestionnaireDescr,Active" 
          chrRequiredFieldList = "QuestionnaireCode,QuestionnaireName,QuestionnaireDescr"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QuestionnaireDetailsForm = NEW dataForm("questionnaire_details_form").
   QuestionnaireDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QuestionnaireDetailsForm:FormAction = "dbQuestionnaireUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QuestionnaireDetailsForm:FormWidth   = 460.
   QuestionnaireDetailsForm:FormHeight  = 200.
   QuestionnaireDetailsForm:FormTitle   = "Questionnaire Details".
   QuestionnaireDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QuestionnaireDetailsForm:insertPaddingColumn(30).
   QuestionnaireDetailsForm:insertColumn(130).
   QuestionnaireDetailsForm:insertColumn(160).
   QuestionnaireDetailsForm:insertColumn(20).
   QuestionnaireDetailsForm:insertColumn(4).
   QuestionnaireDetailsForm:insertColumn(110).
   
   /* Fields */
   QuestionnaireDetailsForm:startRow().
   QuestionnaireDetailsForm:insertLabel("Questionnaire ID").
   QuestionnaireDetailsForm:insertTextField("QuestionnaireID", "", 200, TRUE).  
   
   QuestionnaireDetailsForm:startRow().
   QuestionnaireDetailsForm:insertLabel("Questionnaire Code").
   QuestionnaireDetailsForm:insertTextField("QuestionnaireCode", "", 200, TRUE).  
   
   QuestionnaireDetailsForm:startRow().
   QuestionnaireDetailsForm:insertLabel("Questionnaire Name").
   QuestionnaireDetailsForm:insertTextField("QuestionnaireName", "", 200, TRUE).
   
   QuestionnaireDetailsForm:startRow().
   QuestionnaireDetailsForm:insertLabel("Questionnaire Descr").
   QuestionnaireDetailsForm:insertTextAreaField("QuestionnaireDescr", "", 200, TRUE).  

   QuestionnaireDetailsForm:startRow().
   QuestionnaireDetailsForm:insertLabel(fTL("Active")). 
   QuestionnaireDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QuestionnaireDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQuestionnaireDetailsFields}
   
   /* Add Hidden Fields*/
   QuestionnaireDetailsForm:insertHiddenField("questionnaire_browse_scroll", "").
   QuestionnaireDetailsForm:insertHiddenField("form_name", "questionnaire_details_form").
   QuestionnaireDetailsForm:insertHiddenField("prog_name", "adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireDetailsButtons = NEW buttonBar().
   
   QuestionnaireDetailsButtons:addButton("questionnaire_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQuestionnaire('questionnaire_details_form');").
   
   QuestionnaireDetailsButtons:addButton("questionnaire_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('questionnaire_details_form_popup');").
   
   QuestionnaireDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireDetailsForm:FormButtons = QuestionnaireDetailsButtons.
   
   QuestionnaireDetailsForm:endForm(). 
   
   QuestionnaireDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QuestionnaireDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireDetailsFields Procedure 
PROCEDURE pQuestionnaireDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QuestionnaireDetailsForm:startRow().
         QuestionnaireDetailsForm:insertLabel(fTL("Field Label")).
         QuestionnaireDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireHistory Procedure
PROCEDURE pQuestionnaireHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "questionnairehistory_details_form"}
   
   FIND FIRST Questionnaire WHERE Questionnaire.QuestionnaireID = intSelectedQuestionnaire NO-LOCK NO-ERROR.
   
   QuestionnaireHistoryBrowseForm = NEW dataForm("questionnairehistory_browse_form").
   QuestionnaireHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QuestionnaireHistoryBrowseForm:FormWidth   = 850.
   QuestionnaireHistoryBrowseForm:FormHeight  = 540.
   QuestionnaireHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE Questionnaire THEN " for QuestionnaireID: " 
                                                                  + STRING(Questionnaire.QuestionnaireID) ELSE "").
   QuestionnaireHistoryBrowseForm:FormType    = "xxl_large".
   
   QuestionnaireHistoryBrowse = NEW browseTable("questionnairehistory_browse").
   QuestionnaireHistoryBrowse:BrowseWidth  = 830.
   QuestionnaireHistoryBrowse:BrowseHeight = 500.
   QuestionnaireHistoryBrowse:ExcelExport  = TRUE.
   QuestionnaireHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QuestionnaireHistoryBrowse:insertColumn(fTL("HistoryID"),            60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QuestionnaireHistory}
   
   QuestionnaireHistoryBrowse:insertColumn(fTL("Questionnaire Code"),  125, "CHARACTER", "left", FALSE).
   QuestionnaireHistoryBrowse:insertColumn(fTL("Questionnaire Name"),  125, "CHARACTER", "left", FALSE).
   QuestionnaireHistoryBrowse:insertColumn(fTL("Questionnaire Descr"), 130, "CHARACTER", "left", FALSE).
   QuestionnaireHistoryBrowse:insertColumn(fTL("Starting Category"),   120, "CHARACTER", "left", FALSE).
   QuestionnaireHistoryBrowse:insertColumn(fTL("Active"),               50, "LOGICAL",           FALSE).
   QuestionnaireHistoryBrowse:insertColumn(fTL("User"),                100, "CHARACTER", "left", FALSE).
   QuestionnaireHistoryBrowse:insertColumn(fTL("Created"),             100, "CHARACTER", "left", FALSE).
   
   QuestionnaireHistoryBrowse:StartBody().
   
   IF AVAILABLE Questionnaire THEN
   DO:
      /*List the QuestionnaireHistory*/
      FOR EACH QuestionnaireHistory NO-LOCK 
         WHERE  QuestionnaireHistory.QuestionnaireID = intSelectedQuestionnaire
         BY QuestionnaireHistory.QuestionnaireHistoryID:
         
         FIND FIRST GateUser OF QuestionnaireHistory NO-LOCK NO-ERROR.
       
         QuestionnaireHistoryBrowse:startRow(QuestionnaireHistory.QuestionnaireHistoryID, "selectHistoryRow(this," + '"' + STRING(QuestionnaireHistory.QuestionnaireHistoryID) 
                                                                     + '","questionnaireHistory"' + ");", "").
         QuestionnaireHistoryBrowse:insertData(QuestionnaireHistory.QuestionnaireHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QuestionnaireHistory}
         
         QuestionnaireHistoryBrowse:insertData(QuestionnaireHistory.QuestionnaireCode, "left").
         QuestionnaireHistoryBrowse:insertData(QuestionnaireHistory.QuestionnaireName, "left").
         QuestionnaireHistoryBrowse:insertData(QuestionnaireHistory.QuestionnaireDescr, "left").    
         QuestionnaireHistoryBrowse:insertData(QuestionnaireHistory.QuestionnaireCode, "left").                        
         QuestionnaireHistoryBrowse:insertData(STRING(QuestionnaireHistory.Active, "Yes/No")).
         QuestionnaireHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QuestionnaireHistoryBrowse:insertData(fDisplayDate&Time(QuestionnaireHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */         
         QuestionnaireHistoryBrowse:insertHiddendata("QuestionnaireHistoryID",QuestionnaireHistory.QuestionnaireHistoryID).
         
         QuestionnaireHistoryBrowse:endRow().
      
      END. /* FOR EACH QuestionnaireHistory, */
   END. /*IF AVAILABLE Questionnaire THEN*/
   
   QuestionnaireHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QuestionnaireHistoryBrowse:getErrors().
   
   QuestionnaireHistoryBrowseForm:insertHiddenField("QuestionnaireHistoryID","").
   QuestionnaireHistoryBrowseForm:insertHiddenField("popup_questionnairehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireHistoryBrowseForm}
   
   /* Create Button Bar */
   QuestionnaireHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QuestionnaireHistoryBrowseButtons:addButton("questionnairehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQuestionnaireHistoryDetails('questionnairehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QuestionnaireHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   QuestionnaireHistoryBrowseButtons:addButton("questionnairehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('questionnairehistory_browse_form_popup');").
   
   QuestionnaireHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireHistoryBrowseForm:FormBrowse  = QuestionnaireHistoryBrowse.
   QuestionnaireHistoryBrowseForm:FormButtons = QuestionnaireHistoryBrowseButtons.
   QuestionnaireHistoryBrowseForm:endForm(). 
   
   QuestionnaireHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQuestionnaireHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQuestionnaireHistoryDetails Procedure
PROCEDURE pQuestionnaireHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "questionnairehistory_details_form"}
   
   chrDisplayFieldList  = "QuestionnaireHistoryID,QuestionnaireID,QuestionnaireCode,QuestionnaireName,QuestionnaireDescr,"
                        + "Active,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QuestionnaireHistoryDetailsForm = NEW dataForm("questionnairehistory_details_form").
   QuestionnaireHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QuestionnaireHistoryDetailsForm:FormWidth   = 460.
   QuestionnaireHistoryDetailsForm:FormHeight  = 300.
   QuestionnaireHistoryDetailsForm:FormTitle   = "Questionnaire History Details".
   QuestionnaireHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QuestionnaireHistoryDetailsForm:insertPaddingColumn(40).
   QuestionnaireHistoryDetailsForm:insertColumn(120).
   QuestionnaireHistoryDetailsForm:insertColumn(120).
   QuestionnaireHistoryDetailsForm:insertColumn(20).
   QuestionnaireHistoryDetailsForm:insertColumn(4).
   QuestionnaireHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel(fTL("ID")).
   QuestionnaireHistoryDetailsForm:insertTextField("QuestionnaireHistoryID", "", 200, TRUE).    
   
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel(fTL("QuestionnaireID")).
   QuestionnaireHistoryDetailsForm:insertTextField("QuestionnaireID", "", 200, TRUE).
   
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel(fTL("QuestionnaireCode")).
   QuestionnaireHistoryDetailsForm:insertTextField("QuestionnaireCode", "", 200, TRUE).
   
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel(fTL("QuestionnaireName")).
   QuestionnaireHistoryDetailsForm:insertTextField("QuestionnaireName", "", 200, TRUE).
   
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel(fTL("QuestionnaireDescr")).
   QuestionnaireHistoryDetailsForm:insertTextAreaField("QuestionnaireDescr", "", 200, TRUE).
   
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel(fTL("Active")).
   QuestionnaireHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QuestionnaireHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QuestionnaireHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel("User").
   QuestionnaireHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QuestionnaireHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QuestionnaireHistoryDetailsForm:startRow().
   QuestionnaireHistoryDetailsForm:insertLabel("Created").
   QuestionnaireHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QuestionnaireHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QuestionnaireHistoryDetailsForm:insertLabel(":").
   QuestionnaireHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QuestionnaireHistoryDetailsForm:insertHiddenField("questionnaire_browse_scroll","").
   QuestionnaireHistoryDetailsForm:insertHiddenField("popup_questionnairehistory_browse", "").
   QuestionnaireHistoryDetailsForm:insertHiddenField("QuestionnaireHistoryID","").
   QuestionnaireHistoryDetailsForm:insertHiddenField("form_name","questionnairehistory_details_form").
   QuestionnaireHistoryDetailsForm:insertHiddenField("prog_name","adQuestionnaire.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QuestionnaireHistoryDetailsForm}
   
   /* Create Button Bar */
   QuestionnaireHistoryDetailsButtons = NEW buttonBar().
   
   QuestionnaireHistoryDetailsButtons:addButton("questionnairehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('questionnairehistory_details_form_popup');").
                                        
   QuestionnaireHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QuestionnaireHistoryDetailsForm:FormButtons = QuestionnaireHistoryDetailsButtons.
   
   QuestionnaireHistoryDetailsForm:endForm(). 
   QuestionnaireHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

