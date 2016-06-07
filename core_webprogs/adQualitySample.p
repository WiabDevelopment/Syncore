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

  Created: 02/02/2016

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

DEFINE VARIABLE intSelectedQualitySample                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedQualitySampleLinehour           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrSelectQualitySampleRow                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQualitySampleRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQualitySampleID                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQualitySampleHistory               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectQualitySampleLineHourRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToQualitySampleLineHourRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrQualitySampleLineHourID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQualitySampleLineHour              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupQualitySampleLineHourHistory       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE QualitySampleBrowseFrame                   AS pageFrame.
DEFINE VARIABLE QualitySampleBrowse                        AS browseTable.
DEFINE VARIABLE QualitySampleBrowseButtons                 AS buttonBar.
DEFINE VARIABLE QualitySampleDetailsForm                   AS dataForm.
DEFINE VARIABLE QualitySampleDetailsButtons                AS buttonBar.

DEFINE VARIABLE QualitySampleHistoryBrowseForm             AS dataForm.   
DEFINE VARIABLE QualitySampleHistoryBrowse                 AS browseTable.
DEFINE VARIABLE QualitySampleHistoryBrowseButtons          AS buttonBar.

DEFINE VARIABLE QualitySampleHistoryDetailsForm            AS dataForm.
DEFINE VARIABLE QualitySampleHistoryDetailsButtons         AS buttonBar.

DEFINE VARIABLE QualitySampleLineHourBrowseForm            AS dataForm.   
DEFINE VARIABLE QualitySampleLineHourBrowse                AS browseTable.
DEFINE VARIABLE QualitySampleLineHourBrowseButtons         AS buttonBar.

DEFINE VARIABLE QualitySampleLineHourDetailsForm           AS dataForm.
DEFINE VARIABLE QualitySampleLineHourDetailsButtons        AS buttonBar.

DEFINE VARIABLE QualitySampleLineHourHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE QualitySampleLineHourHistoryBrowse         AS browseTable.
DEFINE VARIABLE QualitySampleLineHourHistoryBrowseButtons  AS buttonBar.

DEFINE VARIABLE QualitySampleLineHourHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE QualitySampleLineHourHistoryDetailsButtons AS buttonBar.

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

&IF DEFINED(EXCLUDE-pQualitySampleLineHourBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleLineHourBrowse Procedure
PROCEDURE pQualitySampleLineHourBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "qualitysamplelinehour_details_form"}
   
   FIND FIRST QualitySampleLineHour NO-LOCK /* idx=QualitySampleID */
      WHERE QualitySampleLineHour.QualitySampleLineHourID = intSelectedQualitySampleLineHour NO-ERROR.
   
   QualitySampleLineHourBrowseForm = NEW dataForm("qualitysamplelinehour_browse_form").
   QualitySampleLineHourBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   QualitySampleLineHourBrowseForm:FormAction="dbQualitySampleLineHourUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualitySampleLineHourBrowseForm:FormWidth   = 850.
   QualitySampleLineHourBrowseForm:FormHeight  = 540.
   QualitySampleLineHourBrowseForm:FormTitle   = fTL("Samples and Hours for QualitySample: ") + (IF AVAILABLE QualitySample THEN  
                                                                  QualitySample.QualitySampleName ELSE "").
   QualitySampleLineHourBrowseForm:FormType    = "xxl_large".
   
   QualitySampleLineHourBrowse = NEW browseTable("qualitysamplelinehour_browse").
   QualitySampleLineHourBrowse:BrowseWidth  = 830.
   QualitySampleLineHourBrowse:BrowseHeight = 500.
   QualitySampleLineHourBrowse:ExcelExport  = TRUE.
   QualitySampleLineHourBrowse:SessionID    = intGblSessionID.
   
   
   QualitySampleLineHourBrowse:insertColumn(fTL("Sample Hour ID"),  100, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySampleLineHour}
   
   QualitySampleLineHourBrowse:insertColumn(fTL("Kitting Line"),   125, "CHARACTER", "left", FALSE).
   QualitySampleLineHourBrowse:insertColumn(fTL("Hour Of Day"),    100, "INTEGER", "left",   FALSE).
   QualitySampleLineHourBrowse:insertColumn(fTL("Sample Size"),    100, "INTEGER", "left",   FALSE).
   QualitySampleLineHourBrowse:insertColumn(fTL("Active"),          70, "LOGICAL",           FALSE).
   
   QualitySampleLineHourBrowse:StartBody().
   
   IF AVAILABLE QualitySample THEN
   DO:
      /*List the QualitySampleLineHour*/
      FOR EACH QualitySampleLineHour NO-LOCK 
         WHERE QualitySampleLineHour.QualitySampleID = intSelectedQualitySample
         BY    QualitySampleLineHour.QualitySampleLineHourID:
         
         FIND FIRST KittingLine OF QualitySampleLineHour NO-LOCK NO-ERROR. /* idx=KittingLineID */
       
         QualitySampleLineHourBrowse:startRow(QualitySampleLineHour.QualitySampleLineHourID, "selectQualitySampleLineHourRow(this," + '"' 
                                          + STRING(QualitySampleLineHour.QualitySampleLineHourID) + '"' + ");", "").                                                            
                                                                     
         QualitySampleLineHourBrowse:insertData(QualitySampleLineHour.QualitySampleLineHourID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QualitySampleLineHour}
         
         QualitySampleLineHourBrowse:insertData((IF AVAILABLE KittingLine THEN KittingLine.LineName ELSE ""), "left").
         QualitySampleLineHourBrowse:insertData(QualitySampleLineHour.HourOfDay).
         QualitySampleLineHourBrowse:insertData(QualitySampleLineHour.QtyRequired).
         QualitySampleLineHourBrowse:insertData(STRING(QualitySampleLineHour.Active, "Yes/No")).
         
         
         /* Add hidden fields */         
         QualitySampleLineHourBrowse:insertHiddenData("QualitySampleID",QualitySample.QualitySampleID).
         QualitySampleLineHourBrowse:insertHiddenData("QuestionnaireVerisonID",QualitySample.VersionID).
         QualitySampleLineHourBrowse:insertHiddenData("QualitySampleLineHourID",QualitySampleLineHour.QualitySampleLineHourID).
         QualitySampleLineHourBrowse:insertHiddenData("QualitySampleLineHourVerisonID",QualitySampleLineHour.VersionID).
         
         QualitySampleLineHourBrowse:endRow().
      
      END. /* FOR EACH QualitySampleLineHour */
   END. /*IF AVAILABLE QualitySample THEN*/
   
   QualitySampleLineHourBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleLineHourBrowse:getErrors().
   
   QualitySampleLineHourBrowseForm:insertHiddenField("QualitySampleID",chrQualitySampleID).
   QualitySampleLineHourBrowseForm:insertHiddenField("QuestionnaireVerisonID","").
   QualitySampleLineHourBrowseForm:insertHiddenField("QualitySampleLineHourID","").
   QualitySampleLineHourBrowseForm:insertHiddenField("QualitySampleLineHourVerisonID","").
   QualitySampleLineHourBrowseForm:insertHiddenField("qualitysamplelinehour_browse_scroll","").
   QualitySampleLineHourBrowseForm:insertHiddenField("popup_qualitysamplelinehour_browse","").
   QualitySampleLineHourBrowseForm:insertHiddenField("popup_qualitysamplelinehourhistory_browse","").
   QualitySampleLineHourBrowseForm:insertHiddenField("form_name","qualitysamplelinehour_browse_form").
   QualitySampleLineHourBrowseForm:insertHiddenField("prog_name","adQualitySample.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleLineHourBrowseForm}
   
   /* Create Button Bar */
   QualitySampleLineHourBrowseButtons = NEW buttonBar().
   
   QualitySampleLineHourBrowseButtons:addButton("qualitysamplelinehour_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQualitySampleLineHour('qualitysamplelinehour_details_form');",
                                             "").                                                 
   
   QualitySampleLineHourBrowseButtons:addButton("qualitysamplelinehour_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQualitySampleLineHourDetails('qualitysamplelinehour_details_form');",
                                             "Disabled").
                                             
   QualitySampleLineHourBrowseButtons:addButton("qualitysamplelinehour_browse_form_btn_history",
                                             fTL("History"),
                                             "viewQualitySampleLineHourHistory();",
                                             "Disabled").                                          
     
     /*Button for later if needed*/                                 
/*   QualitySampleLineHourBrowseButtons:addButton("qualitysamplelinehour_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_qualitysamplelinehour_browse.xml')").*/
   
   QualitySampleLineHourBrowseButtons:addButton("qualitysamplelinehour_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('qualitysamplelinehour_browse_form_popup');").
   
   QualitySampleLineHourBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleLineHourBrowseForm:FormBrowse  = QualitySampleLineHourBrowse.
   QualitySampleLineHourBrowseForm:FormButtons = QualitySampleLineHourBrowseButtons.
   QualitySampleLineHourBrowseForm:endForm(). 
   
   QualitySampleLineHourBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQualitySampleLineHourDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleLineHourDetails Procedure
PROCEDURE pQualitySampleLineHourDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "qualitysamplelinehour_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualitySampleLineHourID,KittingLineID,HourOfDay,Active,QualitySampleID,QtyRequired" 
          chrEditFieldList     = "HourOfDay,Active,QtyRequired,KittingLineID" 
          chrNewFieldList      = "KittingLineID,HourOfDay,Active,QtyRequired" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
                             
   
   QualitySampleLineHourDetailsForm = NEW dataForm("qualitysamplelinehour_details_form").
   QualitySampleLineHourDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   QualitySampleLineHourDetailsForm:FormAction = "dbQualitySampleLineHourUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualitySampleLineHourDetailsForm:FormWidth   = 460.
   QualitySampleLineHourDetailsForm:FormHeight  = 200.
   QualitySampleLineHourDetailsForm:FormTitle   = "QualitySample Flow Details".
   QualitySampleLineHourDetailsForm:FormType    = "small_xwide".
   
   /* Column Layout */
   QualitySampleLineHourDetailsForm:insertPaddingColumn(40).
   QualitySampleLineHourDetailsForm:insertColumn(120).
   QualitySampleLineHourDetailsForm:insertColumn(120).
   QualitySampleLineHourDetailsForm:insertColumn(20).
   QualitySampleLineHourDetailsForm:insertColumn(4).
   QualitySampleLineHourDetailsForm:insertColumn(40).  
   
   /* Fields */
   QualitySampleLineHourDetailsForm:startRow().
   QualitySampleLineHourDetailsForm:insertLabel(fTL("Flow ID")).
   QualitySampleLineHourDetailsForm:insertTextField("QualitySampleLineHourID", "", 200, TRUE).    
   
   QualitySampleLineHourDetailsForm:startRow().
   QualitySampleLineHourDetailsForm:insertLabel(fTL("QualitySample")).
   QualitySampleLineHourDetailsForm:insertComboField("QualitySampleID", "", 200, TRUE).
   /* QualitySample DropDown Options */
   FOR EACH QualitySample NO-LOCK /*idx=KittingLineID*/
      WHERE QualitySample.Active = TRUE:      
      QualitySampleLineHourDetailsForm:insertComboPairs("QualitySampleID", STRING(QualitySample.QualitySampleID), QualitySample.QualitySampleName).    
   END.
   
   QualitySampleLineHourDetailsForm:startRow().
   QualitySampleLineHourDetailsForm:insertLabel(fTL("Kitting Line")).
   QualitySampleLineHourDetailsForm:insertComboField("KittingLineID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH KittingLine NO-LOCK /*idx=KittingLineID*/
      WHERE KittingLine.Active = TRUE:      
      QualitySampleLineHourDetailsForm:insertComboPairs("KittingLineID", STRING(KittingLine.KittingLineID), KittingLine.LineName).    
   END.
   
   QualitySampleLineHourDetailsForm:startRow().
   QualitySampleLineHourDetailsForm:insertLabel(fTL("Hour Of Day")).
   QualitySampleLineHourDetailsForm:insertTextField("HourOfDay", "", 200, TRUE).
   
   QualitySampleLineHourDetailsForm:startRow().
   QualitySampleLineHourDetailsForm:insertLabel(fTL("Sample Size")).
   QualitySampleLineHourDetailsForm:insertTextField("QtyRequired", "", 200, TRUE).
   
   QualitySampleLineHourDetailsForm:startRow().
   QualitySampleLineHourDetailsForm:insertLabel(fTL("Active")).
   QualitySampleLineHourDetailsForm:insertComboField("Active", "", 200, TRUE).
   QualitySampleLineHourDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualitySampleLineHourDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   /* Add Hidden Fields*/
   QualitySampleLineHourDetailsForm:insertHiddenField("qualitysamplelinehour_browse_scroll","").
   QualitySampleLineHourDetailsForm:insertHiddenField("popup_qualitysamplelinehour_browse", "").
   QualitySampleLineHourDetailsForm:insertHiddenField("QualitySampleID",chrQualitySampleID).
   QualitySampleLineHourDetailsForm:insertHiddenField("QualitySampleLineHourID","").
   QualitySampleLineHourDetailsForm:insertHiddenField("QualitySampleLineHourVerisonID","").
   QualitySampleLineHourDetailsForm:insertHiddenField("form_name","qualitysamplelinehour_details_form").
   QualitySampleLineHourDetailsForm:insertHiddenField("prog_name","adQualitySample.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleLineHourDetailsForm}
   
   /* Create Button Bar */
   QualitySampleLineHourDetailsButtons = NEW buttonBar().
   
   QualitySampleLineHourDetailsButtons:addButton("qualitysamplelinehour_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQualitySampleLineHour('qualitysamplelinehour_details_form');").
   
   QualitySampleLineHourDetailsButtons:addButton("qualitysamplelinehour_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('qualitysamplelinehour_details_form_popup');").
                                        
   QualitySampleLineHourDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleLineHourDetailsForm:FormButtons = QualitySampleLineHourDetailsButtons.
   
   QualitySampleLineHourDetailsForm:endForm(). 
   QualitySampleLineHourDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQualitySampleLineHourHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleLineHourHistory Procedure
PROCEDURE pQualitySampleLineHourHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "qualitysamplelinehourhistory_details_form"}
   
   FIND FIRST QualitySampleLineHour WHERE QualitySampleLineHour.QualitySampleLineHourID = intSelectedQualitySampleLineHour NO-LOCK NO-ERROR.
   
   QualitySampleLineHourHistoryBrowseForm = NEW dataForm("qualitysamplelinehourhistory_browse_form").
   QualitySampleLineHourHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   QualitySampleLineHourHistoryBrowseForm:FormAction="dbQualitySampleLineHourUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualitySampleLineHourHistoryBrowseForm:FormWidth   = 850.
   QualitySampleLineHourHistoryBrowseForm:FormHeight  = 540.
   QualitySampleLineHourHistoryBrowseForm:FormTitle   = fTL("History for Questionnarie Flow: ") + (IF AVAILABLE Questionnaireflow THEN  
                                                                  STRING(Questionnaireflow.QuestionnaireflowID) ELSE "").
   QualitySampleLineHourHistoryBrowseForm:FormType    = "xxl_large".
   
   QualitySampleLineHourHistoryBrowse = NEW browseTable("qualitysamplelinehourhistory_browse").
   QualitySampleLineHourHistoryBrowse:BrowseWidth  = 830.
   QualitySampleLineHourHistoryBrowse:BrowseHeight = 500.
   QualitySampleLineHourHistoryBrowse:ExcelExport  = TRUE.
   QualitySampleLineHourHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("History ID"),      70, "INTEGER",           FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySampleLineHourHistory}
   
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("Kitting Line"),   120, "CHARACTER", "left", FALSE).
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("Quality Sample"), 120, "CHARACTER", "left", FALSE).
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("Hour Of Day"),     90, "CHARACTER", "left", FALSE).
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("Sample Size"),     90, "INTEGER", "left",   FALSE).
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("Active"),          60, "LOGICAL",           FALSE).
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("User"),           120, "CHARACTER", "left", FALSE).
   QualitySampleLineHourHistoryBrowse:insertColumn(fTL("Created"),        120, "CHARACTER", "left", FALSE).
   
   QualitySampleLineHourHistoryBrowse:StartBody().
   
   IF AVAILABLE QualitySampleLineHour THEN
   DO:
      /*List the QualitySampleLineHourHistory*/
      FOR EACH QualitySampleLineHourHistory NO-LOCK 
         WHERE  QualitySampleLineHourHistory.QualitySampleLineHourID = intSelectedQualitySampleLineHour
         BY QualitySampleLineHourHistory.QualitySampleLineHourHistoryID:
         
         FIND FIRST GateUser OF QualitySampleLineHourHistory NO-LOCK NO-ERROR. /* idx=GateUserID */
         FIND FIRST QualitySample OF QualitySampleLineHourHistory NO-LOCK NO-ERROR. /* idx=QualitySampleID */
         FIND FIRST KittingLine OF QualitySampleLineHourHistory NO-LOCK NO-ERROR. /* idx=KittingLineID */
       
         QualitySampleLineHourHistoryBrowse:startRow(QualitySampleLineHourHistory.QualitySampleLineHourHistoryID, "selectQualitySampleLineHourHistoryRow(this," + '"' 
                                                   + STRING(QualitySampleLineHourHistory.QualitySampleLineHourHistoryID) + '","qualitySampleLineHourHistory"' + ");", "").
         
         QualitySampleLineHourHistoryBrowse:insertData(QualitySampleLineHourHistory.QualitySampleLineHourHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QualitySampleLineHourHistory}
         
         QualitySampleLineHourHistoryBrowse:insertData((IF AVAILABLE QualitySample THEN QualitySample.QualitySampleName ELSE ""), "left").
         QualitySampleLineHourHistoryBrowse:insertData((IF AVAILABLE KittingLine THEN KittingLine.LineName ELSE ""), "left").
         QualitySampleLineHourHistoryBrowse:insertData(QualitySampleLineHourHistory.HourOfDay).
         QualitySampleLineHourHistoryBrowse:insertData(QualitySampleLineHourHistory.QtyRequired).
         QualitySampleLineHourHistoryBrowse:insertData(STRING(QualitySampleLineHourHistory.Active, "Yes/No")).
         QualitySampleLineHourHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QualitySampleLineHourHistoryBrowse:insertData(fDisplayDate&Time(QualitySampleLineHourHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */    
         QualitySampleLineHourHistoryBrowse:insertHiddendata("QualitySampleLineHourHistoryID",QualitySampleLineHourHistory.QualitySampleLineHourHistoryID).     
         QualitySampleLineHourHistoryBrowse:insertHiddendata("QualitySampleID",QualitySample.QualitySampleID).
         QualitySampleLineHourHistoryBrowse:insertHiddendata("QualitySampleLineHourID",QualitySampleLineHour.QualitySampleLineHourID).
         QualitySampleLineHourHistoryBrowse:insertHiddendata("QualitySampleLineHourVerisonID",QualitySampleLineHour.VersionID).
         
         QualitySampleLineHourHistoryBrowse:endRow().
      
      END. /* FOR EACH QualitySampleLineHour, */
   END. /*IF AVAILABLE QualitySample THEN*/
   
   QualitySampleLineHourHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleLineHourHistoryBrowse:getErrors().
   
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("QualitySampleLineHourHistoryID","").
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("QualitySampleID",chrQualitySampleID).
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("QualitySampleLineHourID","").
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("questionnaire_browse_scroll","").
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("popup_qualitysamplelinehour_browse","").
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("popup_qualitysamplelinehourhistory_browse","").
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("form_name","qualitysamplelinehourhistory_browse_form").
   QualitySampleLineHourHistoryBrowseForm:insertHiddenField("prog_name","adQualitySample.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleLineHourHistoryBrowseForm}
   
   /* Create Button Bar */
   QualitySampleLineHourHistoryBrowseButtons = NEW buttonBar().
   
   QualitySampleLineHourHistoryBrowseButtons:addButton("qualitysamplelinehourhistory_browse_form_btn_details",
                                                   fTL("Details"),
                                                   "viewQualitySampleLineHourHistoryDetails('qualitysamplelinehourhistory_details_form');",
                                                   "Disabled").
                                             
     /*Button for later if needed*/                                 
/*   QualitySampleLineHourHistoryBrowseButtons:addButton("qualitysamplelinehourhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_qualitysamplelinehourhistory_browse.xml')").*/
   
   QualitySampleLineHourHistoryBrowseButtons:addButton("qualitysamplelinehourhistory_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('qualitysamplelinehourhistory_browse_form_popup');").
   
   QualitySampleLineHourHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleLineHourHistoryBrowseForm:FormBrowse  = QualitySampleLineHourHistoryBrowse.
   QualitySampleLineHourHistoryBrowseForm:FormButtons = QualitySampleLineHourHistoryBrowseButtons.
   QualitySampleLineHourHistoryBrowseForm:endForm(). 
   
   QualitySampleLineHourHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pQualitySampleLineHourHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleLineHourHistoryDetails Procedure
PROCEDURE pQualitySampleLineHourHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "qualitysamplelinehourhistory_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualitySampleLineHourID,KittingLineID,HourOfDay,Active,QualitySampleID,QtyRequired,"
                               + "QualitySampleLineHourHistoryID,CreatedDate,CreatedHour,CreatedMins,GateUserID".
                             
   
   QualitySampleLineHourHistoryDetailsForm = NEW dataForm("qualitysamplelinehourhistory_details_form").
   QualitySampleLineHourHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   QualitySampleLineHourHistoryDetailsForm:FormAction = "dbQualitySampleLineHourUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualitySampleLineHourHistoryDetailsForm:FormWidth   = 460.
   QualitySampleLineHourHistoryDetailsForm:FormHeight  = 300.
   QualitySampleLineHourHistoryDetailsForm:FormTitle   = "QualitySample Flow History Details".
   QualitySampleLineHourHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QualitySampleLineHourHistoryDetailsForm:insertPaddingColumn(40).
   QualitySampleLineHourHistoryDetailsForm:insertColumn(120).
   QualitySampleLineHourHistoryDetailsForm:insertColumn(120).
   QualitySampleLineHourHistoryDetailsForm:insertColumn(20).
   QualitySampleLineHourHistoryDetailsForm:insertColumn(4).
   QualitySampleLineHourHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel(fTL("History ID")).
   QualitySampleLineHourHistoryDetailsForm:insertTextField("QualitySampleLineHourHistoryID", "", 200, TRUE).    
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel(fTL("Sample Hour ID")).
   QualitySampleLineHourHistoryDetailsForm:insertTextField("QualitySampleLineHourID", "", 200, TRUE).    
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel(fTL("Quality Sample")).
   QualitySampleLineHourHistoryDetailsForm:insertComboField("QualitySampleID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH QualitySample NO-LOCK /*idx=KittingLineID*/
      WHERE QualitySample.Active = TRUE:      
      QualitySampleLineHourHistoryDetailsForm:insertComboPairs("QualitySampleID", STRING(QualitySample.QualitySampleID), QualitySample.QualitySampleName).    
   END.
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel(fTL("Kitting Line")).
   QualitySampleLineHourHistoryDetailsForm:insertComboField("KittingLineID", "", 200, TRUE).
   /* Question Category DropDown Options */
   FOR EACH KittingLine NO-LOCK /*idx=KittingLineID*/
      WHERE KittingLine.Active = TRUE:      
      QualitySampleLineHourHistoryDetailsForm:insertComboPairs("KittingLineID", STRING(KittingLine.KittingLineID), KittingLine.LineName).    
   END.
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel(fTL("Hour Of Day")).
   QualitySampleLineHourHistoryDetailsForm:insertTextField("HourOfDay", "", 200, TRUE).
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel(fTL("Sample Size")).
   QualitySampleLineHourHistoryDetailsForm:insertTextField("QtyRequired", "", 200, TRUE).
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel(fTL("Active")).
   QualitySampleLineHourHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).
   QualitySampleLineHourHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualitySampleLineHourHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel("User").
   QualitySampleLineHourHistoryDetailsForm:insertComboField("GateUserID", "", 200, TRUE).
   /* Insert the Status Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QualitySampleLineHourHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QualitySampleLineHourHistoryDetailsForm:startRow().
   QualitySampleLineHourHistoryDetailsForm:insertLabel("Created").
   QualitySampleLineHourHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QualitySampleLineHourHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QualitySampleLineHourHistoryDetailsForm:insertLabel(":").
   QualitySampleLineHourHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("qualitysamplelinehour_browse_scroll","").
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("qualitysamplelinehourhistory_browse_scroll","").
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("popup_qualitysamplelinehour_browse", "").
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("QualitySampleID",chrQualitySampleID).
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("QualitySampleLineHourID","").
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("QualitySampleLineHourHistoryID","").
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("form_name","qualitysamplelinehourhistory_details_form").
   QualitySampleLineHourHistoryDetailsForm:insertHiddenField("prog_name","adQualitySample.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleLineHourHistoryDetailsForm}
   
   /* Create Button Bar */
   QualitySampleLineHourHistoryDetailsButtons = NEW buttonBar().
   
   QualitySampleLineHourHistoryDetailsButtons:addButton("qualitysamplelinehourhistory_details_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('qualitysamplelinehourhistory_details_form_popup');").
                                        
   QualitySampleLineHourHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleLineHourHistoryDetailsForm:FormButtons = QualitySampleLineHourHistoryDetailsButtons.
   
   QualitySampleLineHourHistoryDetailsForm:endForm(). 
   QualitySampleLineHourHistoryDetailsForm:displayForm(). 

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
   
   ASSIGN chrQualitySampleID                  = get-value("QualitySampleID")
          intSelectedQualitySample            = INTEGER(chrQualitySampleID)
          chrQualitySampleLineHourID          = get-value("QualitySampleLineHourID")
          intSelectedQualitySampleLineHour    = INTEGER(chrQualitySampleLineHourID)
          chrScrollToQualitySampleLineHourRow = STRING(INTEGER(get-value("qualitysamplelinehour_browse_scroll")))
          chrScrollToQualitySampleRow         = STRING(INTEGER(get-value("qualitysample_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrQualitySampleID <> "" THEN
     chrSelectQualitySampleRow = 'selectQualitySampleRow(document.getElementById("qualitysample_browse_row_' + chrQualitySampleID + '"),"' 
                                                         + chrQualitySampleID +  '");'.
                                                         
   IF chrQualitySampleLineHourID <> "" THEN
     chrSelectQualitySampleLineHourRow = 'selectQualitySampleLineHourRow(document.getElementById("qualitysamplelinehour_browse_row_' 
                                       + chrQualitySampleLineHourID + '"),"' + chrQualitySampleLineHourID +  '");'.                                                      
   
   IF get-value('popup_qualitysamplehistory_browse') = "yes" THEN
      chrPopupQualitySampleHistory  = 'enablePopup("qualitysamplehistory_browse_form_popup");'.
      
   IF get-value('popup_qualitysamplelinehour_browse') = "yes" THEN
      chrPopupQualitySampleLineHour  = 'enablePopup("qualitysamplelinehour_browse_form_popup");'.
      
   IF get-value('popup_qualitysamplelinehourhistory_browse') = "yes" THEN
      chrPopupQualitySampleLineHourHistory  = 'enablePopup("qualitysamplelinehourhistory_browse_form_popup");'.      
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("qualitysample_browse").scrollTop=' + chrScrollToQualitySampleRow 
                                                          + chrSelectQualitySampleRow 
                                                          + chrPopupQualitySampleHistory
                                                          + chrPopupQualitySampleLineHour
                                                          + chrSelectQualitySampleLineHourRow
                                                          + chrPopupQualitySampleLineHourHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "QualitySample Admin".
   ThisPage:FrameTitle    = "QualitySample Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}

   /* Include relevant Js files */
   ThisPage:addJavaScript("qualitysample.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pQualitySampleBrowse.
   
   FIND FIRST QualitySample NO-LOCK /* idx=QualitySampleID */
      WHERE QualitySample.QualitySampleID = intSelectedQualitySample NO-ERROR.

   /******* Popup Browsers and Forms ********/    
   RUN pQualitySampleDetails.
   RUN pQualitySampleHistory.
   RUN pQualitySampleHistoryDetails.
   RUN pQualitySampleLineHourBrowse.
   
   FIND FIRST QualitySampleLineHour NO-LOCK /* idx=QualitySampleID */
      WHERE QualitySampleLineHour.QualitySampleLineHourID = intSelectedQualitySampleLineHour NO-ERROR.
      
   RUN pQualitySampleLineHourDetails.
   RUN pQualitySampleLineHourHistory.
   RUN pQualitySampleLineHourHistoryDetails.   
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT QualitySampleBrowseFrame              NO-ERROR.
   DELETE OBJECT QualitySampleBrowse                   NO-ERROR.
   DELETE OBJECT QualitySampleBrowseButtons            NO-ERROR.
   DELETE OBJECT QualitySampleDetailsForm              NO-ERROR.
   DELETE OBJECT QualitySampleDetailsButtons           NO-ERROR.
   
   DELETE OBJECT QualitySampleHistoryBrowseForm        NO-ERROR.   
   DELETE OBJECT QualitySampleHistoryBrowse            NO-ERROR.
   DELETE OBJECT QualitySampleHistoryBrowseButtons     NO-ERROR.
   
   DELETE OBJECT QualitySampleHistoryDetailsForm       NO-ERROR.
   DELETE OBJECT QualitySampleHistoryDetailsButtons    NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleBrowse Procedure 
PROCEDURE pQualitySampleBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "qualitysample_details_form"}
   
   QualitySampleBrowse = NEW browseTable("qualitysample_browse").
   QualitySampleBrowse:BrowseWidth  = 965.
   QualitySampleBrowse:BrowseHeight = 455.
   QualitySampleBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QualitySampleBrowse:insertColumn(fTL("QualitySample ID"),    130, "INTEGER", "left",   FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySample}
   
   QualitySampleBrowse:insertColumn(fTL("QualitySample Code"),  150, "CHARACTER", "left", FALSE).
   QualitySampleBrowse:insertColumn(fTL("QualitySample Name"),  150, "CHARACTER", "left", FALSE).
   QualitySampleBrowse:insertColumn(fTL("QualitySample Descr"), 200, "CHARACTER", "left", FALSE).
   QualitySampleBrowse:insertColumn(fTL("Active"),               60, "LOGICAL",           FALSE).
   
   /*Body*/
   QualitySampleBrowse:startBody().
   
   FOR EACH QualitySample NO-LOCK: /*idx=ActiveListingSequence*/
      
      QualitySampleBrowse:startRow(QualitySample.QualitySampleID, "selectQualitySampleRow(this," + '"' + STRING(QualitySample.QualitySampleID) 
                                                                  + '"' + ");", "").
      QualitySampleBrowse:insertData(QualitySample.QualitySampleID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i QualitySample}
      
      QualitySampleBrowse:insertData(QualitySample.QualitySampleCode, "left").
      QualitySampleBrowse:insertData(QualitySample.QualitySampleName, "left").
      QualitySampleBrowse:insertData(QualitySample.QualitySampleDescr, "left").
      QualitySampleBrowse:insertData(STRING(QualitySample.Active,"Yes/No")).
      
      /* Add hidden fields */
      QualitySampleBrowse:insertHiddenData("QualitySampleVersionID",QualitySample.VersionID).
      
      QualitySampleBrowse:endRow().
      
   END. /*FOR EACH QualitySample NO-LOCK */
   
   QualitySampleBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleBrowse:getErrors().
   
   /* Create a new frame */
   QualitySampleBrowseFrame = NEW pageFrame().
   QualitySampleBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QualitySampleBrowseFrame:FormAction="dbQualitySampleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   QualitySampleBrowseFrame:formOpen("qualitysample_browse_form").
   
   /* Start the Frame Header */
   QualitySampleBrowseFrame:insertSpacer(5).
   QualitySampleBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QualitySampleBrowse:displayBrowse().  
   
   /* End the Frame Header */
   QualitySampleBrowseFrame:frameClose().
   QualitySampleBrowseFrame:insertSpacer(10).
   
   QualitySampleBrowseFrame:insertHiddenField("qualitysample_browse_scroll","").
   QualitySampleBrowseFrame:insertHiddenField("QualitySampleID","").
   QualitySampleBrowseFrame:insertHiddenField("QualitySampleVersionID","").
   QualitySampleBrowseFrame:insertHiddenField("popup_qualitysamplehistory_browse","").
   QualitySampleBrowseFrame:insertHiddenField("popup_qualitysamplelinehour_browse","").
   QualitySampleBrowseFrame:insertHiddenField("popup_qualitysamplelinehourhistory_browse","").
   QualitySampleBrowseFrame:insertHiddenField("form_name","qualitysample_browse_form").
   QualitySampleBrowseFrame:insertHiddenField("prog_name","adQualitySample.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleBrowseFrame}
   
   QualitySampleBrowseFrame:formClose().
   
   /* Create Button Bar */
   QualitySampleBrowseButtons = NEW buttonBar().
   QualitySampleBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   QualitySampleBrowseButtons:addButton("qualitysample_browse_form_btn_create",
                                             fTL("Create"),
                                             "createQualitySample('qualitysample_details_form');",
                                             "").
   
   QualitySampleBrowseButtons:addButton("qualitysample_browse_form_btn_details",
                                             fTL("Details"),
                                             "viewQualitySampleDetails('qualitysample_details_form');",
                                             "Disabled").
   
   QualitySampleBrowseButtons:addButton("qualitysample_browse_form_btn_history",
                                             fTL("History"),
                                             "viewHistory();",
                                             "Disabled").
                                             
   QualitySampleBrowseButtons:addButton("qualitysample_browse_form_btn_hours",
                                             fTL("Hours"),
                                             "viewQualitySampleLineHourBrowse();",
                                             "Disabled"). 
                                             
/*   QualitySampleBrowseButtons:addButton("qualitysample_browse_form_btn_complete",*/
/*                                             fTL("Complete"),                    */
/*                                             "viewComplete();",                  */
/*                                             "Disabled").                        */
   
   QualitySampleBrowseButtons:closeBar().  
   QualitySampleBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleDetails Procedure 
PROCEDURE pQualitySampleDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "qualitysample_details_form"}
   
   ASSIGN chrDisplayFieldList  = "QualitySampleID,QualitySampleCode,QualitySampleName,QualitySampleDescr,QualitySampleTypeID,Active,"
                               + "QualitySampleStatusID,SampleOperationTypeID,RevisionNumber" 
          chrEditFieldList     = "Active" 
          chrNewFieldList      = "QualitySampleCode,QualitySampleName,QualitySampleDescr,QualitySampleTypeID,Active," 
                               + "QualitySampleStatusID,SampleOperationTypeID,RevisionNumber" 
          chrRequiredFieldList = ""
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   QualitySampleDetailsForm = NEW dataForm("qualitysample_details_form").
   QualitySampleDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   QualitySampleDetailsForm:FormAction = "dbQualitySampleUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   QualitySampleDetailsForm:FormWidth   = 460.
   QualitySampleDetailsForm:FormHeight  = 300.
   QualitySampleDetailsForm:FormTitle   = "QualitySample Details".
   QualitySampleDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   QualitySampleDetailsForm:insertPaddingColumn(30).
   QualitySampleDetailsForm:insertColumn(150).
   QualitySampleDetailsForm:insertColumn(160).
   QualitySampleDetailsForm:insertColumn(20).
   QualitySampleDetailsForm:insertColumn(4).
   QualitySampleDetailsForm:insertColumn(110).
   
   /* Fields */
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("QualitySample ID").
   QualitySampleDetailsForm:insertTextField("QualitySampleID", "", 200, TRUE).  
   
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("QualitySample Code").
   QualitySampleDetailsForm:insertTextField("QualitySampleCode", "", 200, TRUE).  
   
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("QualitySample Name").
   QualitySampleDetailsForm:insertTextField("QualitySampleName", "", 200, TRUE).
   
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("QualitySample Descr").
   QualitySampleDetailsForm:insertTextAreaField("QualitySampleDescr", "", 200, TRUE).  
   
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("QualitySample Type").
   QualitySampleDetailsForm:insertComboField("QualitySampleTypeID", "", 200, TRUE).  
    /* QualitySampleType DropDown Options */
   FOR EACH QualitySampleType NO-LOCK /*idx=QualitySampleTypeID*/
      WHERE QualitySampleType.Active = TRUE:
               
      QualitySampleDetailsForm:insertComboPairs("QualitySampleTypeID", STRING(QualitySampleType.QualitySampleType), QualitySampleType.TypeName).    
   END.
   
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("QualitySample Status").
   QualitySampleDetailsForm:insertComboField("QualitySampleStatusID", "", 200, TRUE).  
    /* QualitySampleStatus DropDown Options */
   FOR EACH QualitySampleStatus NO-LOCK /*idx=QualitySampleStatusID*/
      WHERE QualitySampleStatus.Active = TRUE:
               
      QualitySampleDetailsForm:insertComboPairs("QualitySampleStatusID", STRING(QualitySampleStatus.QualitySampleStatus), QualitySampleStatus.StatusName).    
   END.
   
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("OperationType").
   QualitySampleDetailsForm:insertComboField("SampleOperationTypeID", "", 200, TRUE).  
    /* OperationType DropDown Options */
   FOR EACH OperationType NO-LOCK /*idx=OperationTypeID*/
      WHERE OperationType.Active = TRUE
         BY OperationType.TypeName:
               
      QualitySampleDetailsForm:insertComboPairs("SampleOperationTypeID", STRING(OperationType.OperationType), OperationType.TypeName).    
   END.

   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel("RevisionNumber").
   QualitySampleDetailsForm:insertTextField("RevisionNumber", "", 200, TRUE).
   
   QualitySampleDetailsForm:startRow().
   QualitySampleDetailsForm:insertLabel(fTL("Active")). 
   QualitySampleDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QualitySampleDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualitySampleDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pQualitySampleDetailsFields}
   
   /* Add Hidden Fields*/
   QualitySampleDetailsForm:insertHiddenField("qualitysample_browse_scroll", "").
   QualitySampleDetailsForm:insertHiddenField("form_name", "qualitysample_details_form").
   QualitySampleDetailsForm:insertHiddenField("prog_name", "adQualitySample.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleDetailsForm}
   
   /* Create Button Bar */
   QualitySampleDetailsButtons = NEW buttonBar().
   
   QualitySampleDetailsButtons:addButton("qualitysample_details_form_btn_save", 
                                              fTL("Save"), 
                                              "updateQualitySample('qualitysample_details_form');").
   
   QualitySampleDetailsButtons:addButton("qualitysample_details_form_btn_cancel", 
                                              fTL("Cancel"), 
                                              "cancelUpdate('UserCancelled','process_mode'); disablePopup('qualitysample_details_form_popup');").
   
   QualitySampleDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleDetailsForm:FormButtons = QualitySampleDetailsButtons.
   
   QualitySampleDetailsForm:endForm(). 
   
   QualitySampleDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + QualitySampleDetailsForm:getErrors().  */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleDetailsFields Procedure 
PROCEDURE pQualitySampleDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
     
      WHEN "FieldName" THEN
      DO:
         QualitySampleDetailsForm:startRow().
         QualitySampleDetailsForm:insertLabel(fTL("Field Label")).
         QualitySampleDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSerialMaskRuleTypeHistory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleHistory Procedure
PROCEDURE pQualitySampleHistory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "qualitysamplehistory_details_form"}
   
   FIND FIRST QualitySample WHERE QualitySample.QualitySampleID = intSelectedQualitySample NO-LOCK NO-ERROR.
   
   QualitySampleHistoryBrowseForm = NEW dataForm("qualitysamplehistory_browse_form").
   QualitySampleHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   QualitySampleHistoryBrowseForm:FormWidth   = 850.
   QualitySampleHistoryBrowseForm:FormHeight  = 540.
   QualitySampleHistoryBrowseForm:FormTitle   = fTL("History") + (IF AVAILABLE QualitySample THEN " for MaskRuleTypeID: " 
                                                                  + STRING(QualitySample.QualitySampleID) ELSE "").
   QualitySampleHistoryBrowseForm:FormType    = "xxl_large".
   
   QualitySampleHistoryBrowse = NEW browseTable("qualitysamplehistory_browse").
   QualitySampleHistoryBrowse:BrowseWidth  = 830.
   QualitySampleHistoryBrowse:BrowseHeight = 500.
   QualitySampleHistoryBrowse:ExcelExport  = TRUE.
   QualitySampleHistoryBrowse:SessionID    = intGblSessionID.
   
   
   QualitySampleHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i QualitySampleHistory}

   QualitySampleHistoryBrowse:insertColumn(fTL("QualitySample Code"),  100, "CHARACTER", "left", FALSE).
   QualitySampleHistoryBrowse:insertColumn(fTL("QualitySample Name"),  100, "CHARACTER", "left", FALSE).
   QualitySampleHistoryBrowse:insertColumn(fTL("QualitySample Descr"), 150, "CHARACTER", "left", FALSE).
   QualitySampleHistoryBrowse:insertColumn(fTL("Active"),               50, "LOGICAL",           FALSE).
   QualitySampleHistoryBrowse:insertColumn(fTL("User"),                100, "CHARACTER", "left", FALSE).
   QualitySampleHistoryBrowse:insertColumn(fTL("Created"),             120, "CHARACTER", "left", FALSE).
   
   QualitySampleHistoryBrowse:StartBody().
   
   IF AVAILABLE QualitySample THEN
   DO:
      /*List the QualitySampleHistory*/
      FOR EACH QualitySampleHistory NO-LOCK 
         WHERE  QualitySampleHistory.QualitySampleID = intSelectedQualitySample
         BY QualitySampleHistory.QualitySampleHistoryID:
         
         FIND FIRST GateUser OF QualitySampleHistory NO-LOCK NO-ERROR.
       
         QualitySampleHistoryBrowse:startRow(QualitySampleHistory.QualitySampleHistoryID, "selectHistoryRow(this," + '"' + STRING(QualitySampleHistory.QualitySampleHistoryID) 
                                                                     + '","qualitySampleHistory"' + ");", "").
         QualitySampleHistoryBrowse:insertData(QualitySampleHistory.QualitySampleHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
         {webGetOptionalBrowseFields.i QualitySampleHistory}
         
         QualitySampleHistoryBrowse:insertData(QualitySampleHistory.QualitySampleCode, "left").
         QualitySampleHistoryBrowse:insertData(QualitySampleHistory.QualitySampleName, "left").
         QualitySampleHistoryBrowse:insertData(QualitySampleHistory.QualitySampleDescr, "left").                            
         QualitySampleHistoryBrowse:insertData(STRING(QualitySampleHistory.Active, "Yes/No")).
         QualitySampleHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         QualitySampleHistoryBrowse:insertData(fDisplayDate&Time(QualitySampleHistory.Created,"y/m/d H:M:S"), "left").
         
         
         /* Add hidden fields */          
         QualitySampleHistoryBrowse:insertHiddendata("QualitySampleHistoryID",QualitySampleHistory.QualitySampleHistoryID).
         
         QualitySampleHistoryBrowse:endRow().
      
      END. /* FOR EACH QualitySampleHistory*/
   END. /*IF AVAILABLE QualitySample THEN*/
   
   QualitySampleHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + QualitySampleHistoryBrowse:getErrors().
   
   QualitySampleHistoryBrowseForm:insertHiddenField("QualitySampleHistoryID","").
   QualitySampleHistoryBrowseForm:insertHiddenField("popup_qualitysamplehistory_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleHistoryBrowseForm}
   
   /* Create Button Bar */
   QualitySampleHistoryBrowseButtons = NEW buttonBar().                                                 
   
   QualitySampleHistoryBrowseButtons:addButton("qualitysamplehistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewQualitySampleHistoryDetails('qualitysamplehistory_details_form');",
                                                "Disabled").
     
     /*Button for later if needed*/                                 
/*   QualitySampleHistoryBrowseButtons:addButton("maskhistory_browse_form_btn_excel",               */
/*                                                fTL("Excel Export"),                                                     */
/*                                                "excelExport('" + STRING(intGblSessionID) + "_taskhistory_browse.xml')").*/
   
   QualitySampleHistoryBrowseButtons:addButton("qualitysamplehistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('qualitysamplehistory_browse_form_popup');").
   
   QualitySampleHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleHistoryBrowseForm:FormBrowse  = QualitySampleHistoryBrowse.
   QualitySampleHistoryBrowseForm:FormButtons = QualitySampleHistoryBrowseButtons.
   QualitySampleHistoryBrowseForm:endForm(). 
   
   QualitySampleHistoryBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pQualitySampleHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualitySampleHistoryDetails Procedure
PROCEDURE pQualitySampleHistoryDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/* This finds the ProcessEvent which is linked to this form */
   {webGetWebForm.i "qualitysamplehistory_details_form"}
   
   chrDisplayFieldList  = "QualitySampleHistoryID,QualitySampleID,QualitySampleCode,QualitySampleName,QualitySampleDescr,QualitySampleTypeID,"
                        + "Active,CreatedDate,CreatedHour,CreatedMins,GateUserID,QualitySampleStatusID,SampleOperationTypeID,"
                        + "RevisionNumber".
                             
   
   QualitySampleHistoryDetailsForm = NEW dataForm("qualitysamplehistory_details_form").
   QualitySampleHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.  
   
   /* Setup */
   QualitySampleHistoryDetailsForm:FormWidth   = 545.
   QualitySampleHistoryDetailsForm:FormHeight  = 440.
   QualitySampleHistoryDetailsForm:FormTitle   = "Quality Sample History Details".
   QualitySampleHistoryDetailsForm:FormType    = "large".
   
   /* Column Layout */
   QualitySampleHistoryDetailsForm:insertPaddingColumn(40).
   QualitySampleHistoryDetailsForm:insertColumn(140).
   QualitySampleHistoryDetailsForm:insertColumn(120).
   QualitySampleHistoryDetailsForm:insertColumn(20).
   QualitySampleHistoryDetailsForm:insertColumn(4).
   QualitySampleHistoryDetailsForm:insertColumn(40).  
   
   /* Fields */
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel(fTL("History ID")).
   QualitySampleHistoryDetailsForm:insertTextField("QualitySampleHistoryID", "", 90, TRUE).   
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("QualitySample ID").
   QualitySampleHistoryDetailsForm:insertTextField("QualitySampleID", "", 200, TRUE).  
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("QualitySample Code").
   QualitySampleHistoryDetailsForm:insertTextField("QualitySampleCode", "", 200, TRUE).  
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("QualitySample Name").
   QualitySampleHistoryDetailsForm:insertTextField("QualitySampleName", "", 200, TRUE).
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("QualitySample Descr").
   QualitySampleHistoryDetailsForm:insertTextAreaField("QualitySampleDescr", "", 200, TRUE).  
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("QualitySample Type").
   QualitySampleHistoryDetailsForm:insertComboField("QualitySampleTypeID", "", 200, TRUE).  
    /* QualitySampleType DropDown Options */
   FOR EACH QualitySampleType NO-LOCK /*idx=QualitySampleTypeID*/
      WHERE QualitySampleType.Active = TRUE:
               
      QualitySampleHistoryDetailsForm:insertComboPairs("QualitySampleTypeID", STRING(QualitySampleType.QualitySampleType), QualitySampleType.TypeName).    
   END.
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("QualitySample Status").
   QualitySampleHistoryDetailsForm:insertComboField("QualitySampleStatusID", "", 200, TRUE).  
    /* QualitySampleStatus DropDown Options */
   FOR EACH QualitySampleStatus NO-LOCK /*idx=QualitySampleStatusID*/
      WHERE QualitySampleStatus.Active = TRUE:
               
      QualitySampleHistoryDetailsForm:insertComboPairs("QualitySampleStatusID", STRING(QualitySampleStatus.QualitySampleStatus), QualitySampleStatus.StatusName).    
   END.
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("OperationType").
   QualitySampleHistoryDetailsForm:insertComboField("SampleOperationTypeID", "", 200, TRUE).  
    /* OperationType DropDown Options */
   FOR EACH OperationType NO-LOCK /*idx=OperationTypeID*/
      WHERE OperationType.Active = TRUE
         BY OperationType.TypeName:
               
      QualitySampleHistoryDetailsForm:insertComboPairs("SampleOperationTypeID", STRING(OperationType.OperationType), OperationType.TypeName).    
   END.

   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("RevisionNumber").
   QualitySampleHistoryDetailsForm:insertTextField("RevisionNumber", "", 200, TRUE).
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel(fTL("Active")). 
   QualitySampleHistoryDetailsForm:insertComboField("Active", "", 200, TRUE).  
   QualitySampleHistoryDetailsForm:insertComboPairs("Active", "yes", "Active").
   QualitySampleHistoryDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("User").
   QualitySampleHistoryDetailsForm:insertComboField("GateUserID", "", 110, TRUE).
   /* Insert the User Codes */
   FOR EACH GateUser NO-LOCK
      BY GateUser.FullName:
      QualitySampleHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
   END.
   
   QualitySampleHistoryDetailsForm:startRow().
   QualitySampleHistoryDetailsForm:insertLabel("Created").
   QualitySampleHistoryDetailsForm:insertDateField("CreatedDate", "", 110, TRUE).  
   /* Time fields have no label */
   QualitySampleHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).  
   /* This has a label to separate the time */
   QualitySampleHistoryDetailsForm:insertLabel(":").
   QualitySampleHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).
   
   /* Add Hidden Fields*/
   QualitySampleHistoryDetailsForm:insertHiddenField("qualitysample_browse_scroll","").
   QualitySampleHistoryDetailsForm:insertHiddenField("popup_qualitysamplehistory_browse", "").
   QualitySampleHistoryDetailsForm:insertHiddenField("QualitySampleHistoryID","").
   QualitySampleHistoryDetailsForm:insertHiddenField("form_name","qualitysamplehistory_details_form").
   QualitySampleHistoryDetailsForm:insertHiddenField("prog_name","adQualitySample.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualitySampleHistoryDetailsForm}
   
   /* Create Button Bar */
   QualitySampleHistoryDetailsButtons = NEW buttonBar().
   
   QualitySampleHistoryDetailsButtons:addButton("qualitysamplehistory_details_form_btn_cancel",
                                                  fTL("Cancel"),
                                                  "disablePopup('qualitysamplehistory_details_form_popup');").
                                        
   QualitySampleHistoryDetailsButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   QualitySampleHistoryDetailsForm:FormButtons = QualitySampleHistoryDetailsButtons.
   
   QualitySampleHistoryDetailsForm:endForm(). 
   QualitySampleHistoryDetailsForm:displayForm(). 

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

