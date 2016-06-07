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
{fncUserAccessFunctions.i}

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

/* Count Group Browse */
DEFINE VARIABLE intSelectedCountTask                    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountTaskRow                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountTaskRow                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskID                          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupHistory                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskLocationHistoryPopup        AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskLocationPartHistoryPopup    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskLocationPackageHistoryPopup AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectedTaskType                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectedButtonMode                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrUserIDList                           AS CHARACTER NO-UNDO.
DEFINE VARIABLE intLoop                                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE logTaskCompleted                        AS LOGICAL   NO-UNDO.

DEFINE VARIABLE CountTaskBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskBrowse         AS browseTable.
DEFINE VARIABLE CountTaskBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskDetailsButtons AS buttonBar.

/* Generate tasks */
DEFINE VARIABLE CountGroupGenerateTasksForm    AS dataForm.
DEFINE VARIABLE CountGroupGenerateTasksButtons AS buttonBar.

/* CountTaskLocationLink Browse */
DEFINE VARIABLE chrPopupCountTaskUserLink       AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCountTaskUserLink    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrCountTaskUserLinkID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectCountTaskUserLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCountTaskUserLinks      AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountTaskUserlinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupCountTaskLocations      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCountTaskLocation    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountTaskLocationRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountTaskLocationRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskLocationID          AS CHARACTER NO-UNDO.

DEFINE VARIABLE CountTaskUserLinkBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskUserLinkBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskUserLinkBrowse         AS browseTable.
DEFINE VARIABLE CountTaskUserLinkBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskUserLinkDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskUserLinkDetailsButtons AS buttonBar.

DEFINE VARIABLE CountTaskLocationBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskLocationBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskLocationBrowse         AS browseTable.
DEFINE VARIABLE CountTaskLocationBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskLocationDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskLocationDetailsButtons AS buttonBar.


/* CountTaskLocationPart Browse */
DEFINE VARIABLE chrPopupCountTaskLocationParts      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCountTaskLocationPart    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountTaskLocationPartRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountTaskLocationPartRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskLocationPartID          AS CHARACTER NO-UNDO.

DEFINE VARIABLE CountTaskLocationPartBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskLocationPartBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskLocationPartBrowse         AS browseTable.
DEFINE VARIABLE CountTaskLocationPartBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskLocationPartDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskLocationPartDetailsButtons AS buttonBar.

/* CountTaskLocationPackage Browse */
DEFINE VARIABLE chrPopupCountTaskLocationPackages      AS CHARACTER NO-UNDO.
DEFINE VARIABLE intSelectedCountTaskLocationPackage    AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectCountTaskLocationPackageRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToCountTaskLocationPackageRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCountTaskLocationPackageID          AS CHARACTER NO-UNDO.

DEFINE VARIABLE CountTaskLocationPackageBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskLocationPackageBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskLocationPackageBrowse         AS browseTable.
DEFINE VARIABLE CountTaskLocationPackageBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskLocationPackageDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskLocationPackageDetailsButtons AS buttonBar.

/* CountTask History Browse */
DEFINE VARIABLE CountTaskHistoryBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskHistoryBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskHistoryBrowse         AS browseTable.
DEFINE VARIABLE CountTaskHistoryBrowseButtons  AS buttonBar.

/* CountTaskLocation History Browse */
DEFINE VARIABLE CountTaskLocationHistoryBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskLocationHistoryBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskLocationHistoryBrowse         AS browseTable.
DEFINE VARIABLE CountTaskLocationHistoryBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskLocationHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskLocationHistoryDetailsButtons AS buttonBar.

/* CountTaskLocationPart History Browse */
DEFINE VARIABLE CountTaskLocationPartHistBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskLocationPartHistBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskLocationPartHistBrowse         AS browseTable.
DEFINE VARIABLE CountTaskLocationPartHistBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskLocationPartHistDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskLocationPartHistDetailsButtons AS buttonBar.

/* CountTaskLocationPackage History Browse */
DEFINE VARIABLE CountTaskLocationPackageHistBrowseFrame    AS pageFrame.
DEFINE VARIABLE CountTaskLocationPackageHistBrowseForm     AS dataForm.
DEFINE VARIABLE CountTaskLocationPackageHistBrowse         AS browseTable.
DEFINE VARIABLE CountTaskLocationPackageHistBrowseButtons  AS buttonBar.
DEFINE VARIABLE CountTaskLocationPackageHistDetailsForm    AS dataForm.
DEFINE VARIABLE CountTaskLocationPackageHistDetailsButtons AS buttonBar.

/* CountTask Filter */
DEFINE VARIABLE logFilterIsPoppedUp          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrPopupFilters              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrCurrentTime               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFromTimestamp             AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrHistoricDate              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrHistoricHour              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrHistoricMins              AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrToTimestamp               AS CHARACTER NO-UNDO.
DEFINE VARIABLE intHistoryDays               AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedCountTaskStatusID AS INTEGER   NO-UNDO.

DEFINE VARIABLE CountTaskBrowseFilterForm    AS dataForm.
DEFINE VARIABLE CountTaskBrowseFilterButtons AS buttonBar.

DEFINE BUFFER countTaskGateUser        FOR GateUser.
DEFINE BUFFER countTaskHistoryGateUser FOR GateUser.

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
         HEIGHT             = 22.67
         WIDTH              = 68.2.
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

  /* DEFINE VARIABLE mptFile        AS MEMPTR     NO-UNDO.
   DEFINE VARIABLE chrFileName    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE chrDestination AS CHARACTER  NO-UNDO.
      
   chrFileName = get-value("UploadFile").

   /* 'filename' refers to the name of the field in the form. get-binary-data returns
      the contents of the file associated with the form field named 'filename'. */
   mptFile = get-binary-data("UploadFile").
   IF mptFile <> ? THEN DO:
      /* If mFile is a valid pointer, save the data to a file. The value of the field
         'filename' is the name of the file that was posted */
      chrDestination = get-config("fileUploadDirectory") + chrFileName.
      COPY-LOB FROM mptFile TO FILE chrDestination NO-CONVERT.
   END.*/
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskBrowse Procedure 
PROCEDURE pCountTaskBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "counttask_details_form"}
   
   CountTaskBrowse              = NEW browseTable("counttask_browse").
   CountTaskBrowse:BrowseWidth  = 965.
   CountTaskBrowse:BrowseHeight = 455.
   CountTaskBrowse:ExcelExport  = TRUE.
   CountTaskBrowse:SessionID    = intGblSessionID.
   CountTaskBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   CountTaskBrowse:insertColumn(fTL("TaskID"),       90, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTask}
   
   CountTaskBrowse:insertColumn(fTL("Group Code"),  100, "CHARACTER", "left", FALSE).
   CountTaskBrowse:insertColumn(fTL("Group Type"),  100, "CHARACTER", "left", FALSE).
   CountTaskBrowse:insertColumn(fTL("Task Type"),   100, "CHARACTER", "left", FALSE).
   CountTaskBrowse:insertColumn(fTL("Blind"),        50, "LOGICAL", FALSE).
   CountTaskBrowse:insertColumn(fTL("Priority"),     60, "INTEGER",FALSE).
   CountTaskBrowse:insertColumn(fTL("RecountID"),    90, "INTEGER",FALSE).
   CountTaskBrowse:insertColumn(fTL("BusinessUnit"),100, "CHARACTER", "left", FALSE).
   CountTaskBrowse:insertColumn(fTL("Status"),      100, "CHARACTER", "left", FALSE).
   CountTaskBrowse:insertColumn(fTL("Completed"),   110, "CHARACTER", "left", FALSE).
   CountTaskBrowse:insertColumn("",                  35, "", FALSE).
   
   /*Body*/
   CountTaskBrowse:startBody().
   /* Find all Operation Types then sort by Active, Listing Sequence, and Type ID in case Sequences are the same */
   IF NOT logFilterIsPoppedUp THEN
   DO:
      IF chrHistoricDate = "" THEN 
      DO:
         IF intSelectedCountTaskStatusID = 0 THEN /* All CountTask Statuses */
         DO:
            CountTaskLoop:
            FOR EACH CountTask NO-LOCK
               BY CountTask.CountTaskID:
               
               IF NOT fCanViewBusinessUnit(intGblSessionID,CountTask.BusinessUnitID) THEN 
                  NEXT CountTaskLoop.
               
               FIND FIRST CountGroup      OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST CountGroupType  OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST CountTaskType   OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST BusinessUnit    OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST CountTaskStatus OF CountTask NO-LOCK NO-ERROR.
   
               RUN pSetCountTaskRows.
   
            END. /* FOR EACH CountTask NO-LOCK */

         END. /* IF intSelectedCountTaskStatusID = 0 */
         ELSE
         DO:
            CountTaskLoop:
            FOR EACH CountTask NO-LOCK 
               WHERE CountTask.CountTaskStatusID = intSelectedCountTaskStatusID
               BY CountTask.CountTaskID:

               IF NOT fCanViewBusinessUnit(intGblSessionID,CountTask.BusinessUnitID) THEN 
                  NEXT CountTaskLoop.

               FIND FIRST CountGroup      OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST CountGroupType  OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST CountTaskType   OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST BusinessUnit    OF CountTask NO-LOCK NO-ERROR.
               FIND FIRST CountTaskStatus OF CountTask NO-LOCK NO-ERROR.
   
               RUN pSetCountTaskRows.
               
            END.
         END. /* ELSE */
         
      END. /* IF chrHistoricDate = "" */
      ELSE
      DO:
         CountTaskLoop:
         FOR EACH CountTask NO-LOCK /*idx=Completed*/
            WHERE CountTask.Completed         <= chrToTimeStamp 
            AND   CountTask.Completed         >= chrFromTimeStamp
            AND   CountTask.CountTaskStatusID = 5 /* Show Only Completed = 5 */
            BY CountTask.CountTaskID:

            IF NOT fCanViewBusinessUnit(intGblSessionID,CountTask.BusinessUnitID) THEN 
               NEXT CountTaskLoop.

            FIND FIRST CountGroup      OF CountTask NO-LOCK NO-ERROR.
            FIND FIRST CountGroupType  OF CountTask NO-LOCK NO-ERROR.
            FIND FIRST CountTaskType   OF CountTask NO-LOCK NO-ERROR.
            FIND FIRST BusinessUnit    OF CountTask NO-LOCK NO-ERROR.
            FIND FIRST CountTaskStatus OF CountTask NO-LOCK NO-ERROR.

            RUN pSetCountTaskRows.

         END.

      END.
      
   END. /*IF NOT logFilterIsPoppedUp THEN*/
   
   CountTaskBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskBrowse:getErrors().
   
   /* Create a new frame */
   CountTaskBrowseFrame           = NEW pageFrame().
   CountTaskBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   CountTaskBrowseFrame:FormAction="dbCountTaskUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   CountTaskBrowseFrame:formOpen("counttask_browse_form").
   
   /* Start the Frame Header */
   CountTaskBrowseFrame:insertSpacer(5).
   CountTaskBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   CountTaskBrowse:displayBrowse().  
   
   /* End the Frame Header */
   CountTaskBrowseFrame:frameClose().
   CountTaskBrowseFrame:insertSpacer(10).
   
   CountTaskBrowseFrame:insertHiddenField("counttask_browse_scroll","").
   CountTaskBrowseFrame:insertHiddenField("counttasklocationlink_browse_scroll","").
   CountTaskBrowseFrame:insertHiddenField("CountTaskID","").
   CountTaskBrowseFrame:insertHiddenField("CountTaskVersionID","").
   CountTaskBrowseFrame:insertHiddenField("CountTaskLocationID",chrCountTaskLocationID).
   CountTaskBrowseFrame:insertHiddenField("popup_counttaskuserlink_browse","").
   CountTaskBrowseFrame:insertHiddenField("popup_counttasklocation_browse","").
   CountTaskBrowseFrame:insertHiddenField("popup_counttaskhistory_browse","").
   CountTaskBrowseFrame:insertHiddenField("popup_counttasklocationpart_browse","").
   CountTaskBrowseFrame:insertHiddenField("popup_counttasklocationpackage_browse","").
   CountTaskBrowseFrame:insertHiddenField("popup_counttasklocationparthistory_browse","").
   CountTaskBrowseFrame:insertHiddenField("popup_counttasklocationpackagehistory_browse","").
   CountTaskBrowseFrame:insertHiddenField("CountTaskStatusID",STRING(intSelectedCountTaskStatusID)).
   CountTaskBrowseFrame:insertHiddenField("TaskType", "").
   CountTaskBrowseFrame:insertHiddenField("filtering", "no").
   CountTaskBrowseFrame:insertHiddenField("ButtonMode", "").
   CountTaskBrowseFrame:insertHiddenField("HistoricDate", chrHistoricDate).
   CountTaskBrowseFrame:insertHiddenField("HistoricHour", chrHistoricHour).
   CountTaskBrowseFrame:insertHiddenField("HistoricMins", chrHistoricMins).
   CountTaskBrowseFrame:insertHiddenField("HistoryDays", STRING(intHistoryDays)).
   CountTaskBrowseFrame:insertHiddenField("form_name","counttask_browse_form").
   CountTaskBrowseFrame:insertHiddenField("prog_name","adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskBrowseFrame}
   
   CountTaskBrowseFrame:formClose().
   
   /* Create Button Bar */
   CountTaskBrowseButtons           = NEW buttonBar().
   CountTaskBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_filter",
                                    fTL("Filter"),
                                    "viewCountTaskFilter('counttask_filter_form');").
   
   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_details",
                                    fTL("Details"),
                                    "viewCountTaskDetails('counttask_details_form');",
                                    (IF intSelectedCountTask > 0 THEN "" ELSE "Disabled")).

   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_users",
                                    fTL("Users"),
                                    "viewCountTaskUserLinks('counttask_browse_form');",
                                    (IF intSelectedCountTask > 0 THEN "" ELSE "Disabled")).
   
   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_locations",
                                    fTL("Locations"),
                                    "viewCountTaskLocations('counttask_browse_form');",
                                    (IF intSelectedCountTask > 0 THEN "" ELSE "Disabled")).

   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_results",
                                    fTL("Results"),
                                    "viewCountTaskResults('counttask_browse_form');",
                                    (IF intSelectedCountTask > 0 THEN "" ELSE "Disabled")).
   
   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_discrepancies",
                                    fTL("Discrepancies"),
                                    "viewCountTaskDiscrepancies('counttask_browse_form');",
                                    (IF intSelectedCountTask > 0 THEN "" ELSE "Disabled")).
   /*
   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_history",
                                    fTL("History"),
                                    "viewCountTaskHistory();",
                                    (IF intSelectedCountTask > 0 THEN "" ELSE "Disabled")).
   */
   CountTaskBrowseButtons:addButton("counttask_browse_form_btn_excel",
                                    fTL("Excel Export"),
                                    "excelExport('" + STRING(intGblSessionID) + "_counttask_browse.xml')").
   
   CountTaskBrowseButtons:closeBar().  
   CountTaskBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskDetails Procedure 
PROCEDURE pCountTaskDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "counttask_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountTaskID,CountTaskTypeID,CountGroupID,CountTaskStatusID,CountGroupTypeID,BusinessUnitID,CompletedDate,CompletedHour,CompletedMins,Priority,BlindCount,CountGroupRecountID"
          chrEditFieldList     = "Priority"
          chrNewFieldList      = "CountTaskID,CountTaskTypeID,CountGroupID,CountTaskStatusID,CountGroupTypeID,BusinessUnitID,CompletedDate,CompletedHour,CompletedMins,Priority,BlindCount,CountGroupRecountID"
          chrRequiredFieldList = "Priority"
          chrExtraFieldList    = ""
          chrValidateFieldList = "Priority:INTEGER".
   
   CountTaskDetailsForm           = NEW dataForm("counttask_details_form").
   CountTaskDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskDetailsForm:FormAction = "dbCountTaskUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskDetailsForm:FormWidth  = 460.
   CountTaskDetailsForm:FormHeight = 300.
   CountTaskDetailsForm:FormTitle  = " Count Task Details".
   CountTaskDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountTaskDetailsForm:insertPaddingColumn(50).
   CountTaskDetailsForm:insertColumn(120).
   CountTaskDetailsForm:insertColumn(120).
   CountTaskDetailsForm:insertColumn(20).
   CountTaskDetailsForm:insertColumn(4).
   CountTaskDetailsForm:insertColumn(110).

   /* Fields */
   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("CountTask ID").
   CountTaskDetailsForm:insertTextField("CountTaskID", "", 110, TRUE).  
   
   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("Count Group").
   CountTaskDetailsForm:insertComboField("CountGroupID", "", 110, TRUE).  
   CountGroupLoop:
   FOR EACH CountGroup NO-LOCK /* idx=ActiveListingSequence*/
      BY CountGroup.Active DESC
      BY CountGroup.ListingSequence:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,CountGroup.BusinessUnitID) THEN 
         NEXT CountGroupLoop.
      
      CountTaskDetailsForm:insertComboPairs("CountGroupID", STRING(CountGroup.CountGroupID), CountGroup.CountGroupName).
   END.

   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("Count Group Type").
   CountTaskDetailsForm:insertComboField("CountGroupTypeID", "", 110, TRUE).  
   FOR EACH CountGroupType NO-LOCK /* idx=ActiveListingSequence*/
      BY CountGroupType.Active DESC
      BY CountGroupType.ListingSequence:
      
      CountTaskDetailsForm:insertComboPairs("CountGroupTypeID", STRING(CountGroupType.CountGroupTypeID), CountGroupType.TypeName).
   END.

   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("Count Task Type").
   CountTaskDetailsForm:insertComboField("CountTaskTypeID", "", 110, TRUE).  
   FOR EACH CountTaskType NO-LOCK /* idx=ActiveListingSequence*/
      BY CountTaskType.Active DESC
      BY CountTaskType.ListingSequence:
      
      CountTaskDetailsForm:insertComboPairs("CountTaskTypeID", STRING(CountTaskType.CountTaskTypeID), CountTaskType.TypeName).
   END.

   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel(fTL("Blind Count")). 
   CountTaskDetailsForm:insertComboField("BlindCount", "", 110, TRUE).  
   CountTaskDetailsForm:insertComboPairs("BlindCount", "yes", "Yes").
   CountTaskDetailsForm:insertComboPairs("BlindCount", "no",  "No").
   
   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("Priority").
   CountTaskDetailsForm:insertTextField("Priority", "", 110, TRUE).  

   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("RecountID").
   CountTaskDetailsForm:insertTextField("CountGroupRecountID", "", 110, TRUE).  
   
   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("Business Unit").
   CountTaskDetailsForm:insertComboField("BusinessUnitID", "", 110, TRUE).  
   BusinessUnitLoop:
   FOR EACH BusinessUnit NO-LOCK /* idx=ActiveListingSequence*/
      BY BusinessUnit.Active DESC
      BY BusinessUnit.ListingSequence:
      
      IF NOT fCanViewBusinessUnit(intGblSessionID,BusinessUnit.BusinessUnitID) THEN 
         NEXT BusinessUnitLoop.
      
      CountTaskDetailsForm:insertComboPairs("BusinessUnitID", STRING(BusinessUnit.BusinessUnitID), BusinessUnit.UnitName).
   END.

   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel("Count Task Status").
   CountTaskDetailsForm:insertComboField("CountTaskStatusID", "", 110, TRUE).  
   FOR EACH CountTaskStatus NO-LOCK /* idx=ActiveListingSequence*/
      BY CountTaskStatus.Active DESC
      BY CountTaskStatus.ListingSequence:
      
      CountTaskDetailsForm:insertComboPairs("CountTaskStatusID", STRING(CountTaskStatus.CountTaskStatusID), CountTaskStatus.StatusName).
   END.

   CountTaskDetailsForm:startRow().
   CountTaskDetailsForm:insertLabel(fTL("Completed")).
   CountTaskDetailsForm:insertDateField("CompletedDate", "", 110, TRUE).
   CountTaskDetailsForm:insertTextField("CompletedHour", "", 18, TRUE).
   CountTaskDetailsForm:insertLabel(":").
   CountTaskDetailsForm:insertTextField("CompletedMins", "", 18, TRUE).

   {webGetOptionalFormFields.i pCountGroupDetailsFields}
   
   /* Add Hidden Fields*/
   CountTaskDetailsForm:insertHiddenField("counttask_browse_scroll", "").
   CountTaskDetailsForm:insertHiddenField("form_name", "counttask_details_form").
   CountTaskDetailsForm:insertHiddenField("prog_name", "adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskDetailsForm}
   
   /* Create Button Bar */
   CountTaskDetailsButtons = NEW buttonBar().
   
   CountTaskDetailsButtons:addButton("counttask_details_form_btn_save", 
                                     fTL("Save"), 
                                     "updateCountTask('counttask_details_form');").

   CountTaskDetailsButtons:addButton("counttask_details_form_btn_cancel", 
                                     fTL("Cancel"), 
                                     "cancelUpdate('UserCancelled','process_mode'); disablePopup('counttask_details_form_popup');").
   
   CountTaskDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskDetailsForm:FormButtons = CountTaskDetailsButtons.
   
   CountTaskDetailsForm:endForm(). 
   
   CountTaskDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskFilter Procedure 
PROCEDURE pCountTaskFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to this form */
   
   CountTaskBrowseFilterForm            = NEW dataForm("counttask_filter_form").
   CountTaskBrowseFilterForm:WebStream  = STREAM WebStream:HANDLE.
   CountTaskBrowseFilterForm:FormAction = "adCountTaskAdmin.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskBrowseFilterForm:FormWidth   = 350.
   CountTaskBrowseFilterForm:FormHeight  = 200.
   CountTaskBrowseFilterForm:FormTitle   = "Count Task Filter".
   CountTaskBrowseFilterForm:FormType    = "small_wide".
   
   /* Column Layout */   
   CountTaskBrowseFilterForm:insertPaddingColumn(10).
   CountTaskBrowseFilterForm:insertColumn(120).
   CountTaskBrowseFilterForm:insertColumn(120).
   CountTaskBrowseFilterForm:insertColumn(20).
   CountTaskBrowseFilterForm:insertColumn(4).  
   CountTaskBrowseFilterForm:insertColumn(80). 

   /* Fields */
   CountTaskBrowseFilterForm:startRow().
   CountTaskBrowseFilterForm:insertLabel("Count Task Status").
   CountTaskBrowseFilterForm:insertComboField("CountTaskStatusID", STRING(intSelectedCountTaskStatusID), 140, TRUE).  
   CountTaskBrowseFilterForm:insertComboPairs("CountTaskStatusID", "0", "All Statuses...").
   FOR EACH CountTaskStatus NO-LOCK /* idx=ActiveListingSequence*/
      BY CountTaskStatus.Active DESC
      BY CountTaskStatus.ListingSequence:
      
      CountTaskBrowseFilterForm:insertComboPairs("CountTaskStatusID", STRING(CountTaskStatus.CountTaskStatusID), CountTaskStatus.StatusName).
   END.

   CountTaskBrowseFilterForm:startRow().
   CountTaskBrowseFilterForm:insertLabel(fTL("Historic Date")).
   CountTaskBrowseFilterForm:insertDateField("HistoricDate", chrHistoricDate, 105 ,TRUE).
   CountTaskBrowseFilterForm:insertTextField("HistoricHour", chrHistoricHour, 18, TRUE).
   CountTaskBrowseFilterForm:insertLabel(":").
   CountTaskBrowseFilterForm:insertTextField("HistoricMins", chrHistoricMins, 18, TRUE).

   CountTaskBrowseFilterForm:startRow().
   CountTaskBrowseFilterForm:insertLabel(fTL("History Days")).
   CountTaskBrowseFilterForm:insertTextField("HistoryDays", STRING(intHistoryDays), 60, TRUE).

   /* Add Hidden Fields*/
   CountTaskBrowseFilterForm:insertHiddenField("filtering","no").
   CountTaskBrowseFilterForm:insertHiddenField("form_name","counttask_filter_form").
   CountTaskBrowseFilterForm:insertHiddenField("prog_name","adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskBrowseFilterForm}
   
   /* Create Button Bar */
   CountTaskBrowseFilterButtons = NEW buttonBar().
   
   CountTaskBrowseFilterButtons:addButton("location_filter_form_btn_search", 
                                          fTL("Filter"), 
                                          "filterCountTasks()").
   
   CountTaskBrowseFilterButtons:closeBar(). 
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskBrowseFilterForm:FormButtons = CountTaskBrowseFilterButtons.
   
   CountTaskBrowseFilterForm:endForm(). 
   CountTaskBrowseFilterForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskHistoryBrowse Procedure 
PROCEDURE pCountTaskHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   CountTaskHistoryBrowseForm           = NEW dataForm("counttaskhistory_browse_form").
   CountTaskHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   CountTaskHistoryBrowseForm:FormWidth  = 860.
   CountTaskHistoryBrowseForm:FormHeight = 530.
   CountTaskHistoryBrowseForm:FormTitle  = fTL("History") + (IF AVAILABLE CountTask THEN 
                                                               " for CountTask: " + STRING(CountTask.CountTaskID)
                                                             ELSE "").
   CountTaskHistoryBrowseForm:FormType   = "xxl_large".
   CountTaskHistoryBrowse                = NEW browseTable("counttaskhistory_browse").
   CountTaskHistoryBrowse:BrowseWidth    = 840.
   CountTaskHistoryBrowse:BrowseHeight   = 490.
   
   CountTaskHistoryBrowse:insertColumn(fTL("HistoryID"), 60, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskHistory}
   
   CountTaskHistoryBrowse:insertColumn(fTL("Group Code"),          75, "CHARACTER", "left", FALSE).
   CountTaskHistoryBrowse:insertColumn(fTL("Group Type"),          75, "CHARACTER", "left", FALSE).
   /**
   CountTaskHistoryBrowse:insertColumn(fTL("Task Type"),           80, "CHARACTER", "left", FALSE).
   CountTaskHistoryBrowse:insertColumn(fTL("Blind"),               50, "LOGICAL", FALSE).
   **/
   CountTaskHistoryBrowse:insertColumn(fTL("Completed"),          105, "CHARACTER", "left", FALSE).
   CountTaskHistoryBrowse:insertColumn(fTL("Priority"),            50, "INTEGER","left",FALSE).
   /* CountTaskHistoryBrowse:insertColumn(fTL("RecountID"),           60, "INTEGER",   "left", FALSE). */
   CountTaskHistoryBrowse:insertColumn(fTL("Business Unit"),       85, "CHARACTER",   "left", FALSE).
   CountTaskHistoryBrowse:insertColumn(fTL("Status"),              75, "CHARACTER", "left", FALSE).
   CountTaskHistoryBrowse:insertColumn(fTL("User"),               110, "CHARACTER", "left", FALSE).
   CountTaskHistoryBrowse:insertColumn(fTL("Operation"),           80, "CHARACTER", "left", FALSE).
   CountTaskHistoryBrowse:insertColumn(fTL("Created"),            105, "CHARACTER", "left", FALSE).
   
   CountTaskHistoryBrowse:StartBody().
   
   IF AVAILABLE CountTask THEN
   DO:
      FOR EACH CountTaskHistory NO-LOCK /* idx=CountTaskIDCreated */
         WHERE CountTaskHistory.CountTaskID = CountTask.CountTaskID
         BY CountTaskHistory.Created DESCENDING
         BY CountTaskHistory.CountTaskID:
         

         FIND FIRST CountGroup      OF CountTaskHistory NO-LOCK NO-ERROR.
         FIND FIRST CountGroupType  OF CountTaskHistory NO-LOCK NO-ERROR.
         FIND FIRST CountTaskType   OF CountTaskHistory NO-LOCK NO-ERROR.
         FIND FIRST BusinessUnit    OF CountTaskHistory NO-LOCK NO-ERROR.
         FIND FIRST CountTaskStatus OF CountTaskHistory NO-LOCK NO-ERROR.
         FIND FIRST BusinessUnit    OF CountTaskHistory NO-LOCK NO-ERROR.
         FIND FIRST OperationType   OF CountTaskHistory NO-LOCK NO-ERROR.
         FIND FIRST GateUser        OF CountTaskHistory NO-LOCK NO-ERROR.
         
         CountTaskHistoryBrowse:startRow(CountTaskHistory.CountTaskHistoryID, 
                                           "selectBrowseRow(this," + '"counttaskhistory_browse"' + ");","").
         CountTaskHistoryBrowse:insertData(CountTaskHistory.CountTaskHistoryID).
         
         /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
         {webGetOptionalBrowseFields.i CountTaskHistory}
         
         CountTaskHistoryBrowse:insertData(IF AVAILABLE CountGroup THEN CountGroup.CountGroupCode ELSE "", "left").
         CountTaskHistoryBrowse:insertData(IF AVAILABLE CountGroupType THEN CountGroupType.TypeCode ELSE "", "left").
         /**
         CountTaskHistoryBrowse:insertData(IF AVAILABLE CountTaskType THEN CountTaskType.TypeCode ELSE "","left").                                                                                               
         CountTaskHistoryBrowse:insertData(STRING(CountTask.BlindCount, "Yes/No")).
         **/
         CountTaskHistoryBrowse:insertData(fDisplayDate&Time(CountTask.Completed,"y/m/d H:M:S"), "right").
         CountTaskHistoryBrowse:insertData(STRING(CountTask.Priority)).
         /* CountTaskHistoryBrowse:insertData(STRING(CountTask.CountGroupRecountID)). */
         CountTaskHistoryBrowse:insertData(IF AVAILABLE BusinessUnit THEN BusinessUnit.UnitName ELSE "","left").
         CountTaskHistoryBrowse:insertData(IF AVAILABLE CountTaskStatus THEN CountTaskStatus.StatusCode ELSE "","left").
         CountTaskHistoryBrowse:insertData((IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "left").
         CountTaskHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
         CountTaskHistoryBrowse:insertData(fDisplayDate&Time(CountTaskHistory.Created,"y/m/d H:M:S"), "right").
         
         CountTaskHistoryBrowse:endRow().
      
      END. /* FOR EACH CountTaskHistory NO-LOCK */
   END. /*IF AVAILABLE CountTask THEN*/
   
   CountTaskHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskHistoryBrowse:getErrors().
   
   CountTaskHistoryBrowseForm:insertHiddenField("popup_counttaskhistory_browse","").
  
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskHistoryBrowseForm}
   
   /* Create Button Bar */
   CountTaskHistoryBrowseButtons = NEW buttonBar().
   
   CountTaskHistoryBrowseButtons:addButton("counttaskhistory_browse_form_btn_cancel",
                                           fTL("Cancel"),
                                           "disablePopup('counttaskhistory_browse_form_popup');").
   
   CountTaskHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskHistoryBrowseForm:FormBrowse  = CountTaskHistoryBrowse.
   CountTaskHistoryBrowseForm:FormButtons = CountTaskHistoryBrowseButtons.
   CountTaskHistoryBrowseForm:endForm(). 
   
   CountTaskHistoryBrowseForm:displayForm().
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationBrowse Procedure 
PROCEDURE pCountTaskLocationBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "counttasklocation_details_form"}
   
   CountTaskLocationBrowseForm            = NEW dataForm("counttasklocation_browse_form").
   CountTaskLocationBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   CountTaskLocationBrowseForm:FormAction = "dbCountTaskLocationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   CountTaskLocationBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   CountTaskLocationBrowseForm:FormWidth  = 860. 
   CountTaskLocationBrowseForm:FormHeight = 530. 
   CountTaskLocationBrowseForm:FormTitle  = fTL("Locations Linked to Count Task") + 
                                                      (IF AVAILABLE CountTask THEN " : " + STRING(CountTask.CountTaskID) ELSE "").
   CountTaskLocationBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountTaskLocationBrowse              = NEW browseTable("counttasklocation_browse").
   CountTaskLocationBrowse:BrowseWidth  = 840.
   CountTaskLocationBrowse:BrowseHeight = 490.
   CountTaskLocationBrowse:ExcelExport  = TRUE.
   CountTaskLocationBrowse:SessionID    = intGblSessionID.
   
   /* Add in the ID as first Column */
   CountTaskLocationBrowse:insertColumn(fTL("CountTaskLocation ID"),    115, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskLocation}
   
   CountTaskLocationBrowse:insertColumn(fTL("TaskID"),         75, "INTEGER", FALSE).
   CountTaskLocationBrowse:insertColumn(fTL("Location Ref"),  120, "CHARACTER", "left", FALSE).
   CountTaskLocationBrowse:insertColumn(fTL("Completed"),     130, "CHARACTER", "left", FALSE).
   CountTaskLocationBrowse:insertColumn(fTL("BlindCount"),     80, "LOGICAL", FALSE).
   CountTaskLocationBrowse:insertColumn(fTL("Status"),        100, "CHARACTER", "left", FALSE).
   CountTaskLocationBrowse:insertColumn(fTL("Priority"),       60, "INTEGER", FALSE).
   CountTaskLocationBrowse:insertColumn(fTL("Performed By"),  140, "CHARACTER", "left", FALSE).

   /*Body*/
   CountTaskLocationBrowse:StartBody().
   CountTaskLocationLoop:
   FOR EACH CountTaskLocation NO-LOCK
      WHERE CountTaskLocation.CountTaskID = CountTask.CountTaskID,
      EACH Location OF CountTaskLocation NO-LOCK:

      IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
         NEXT CountTaskLocationLoop.

      FIND FIRST countTaskGateUser NO-LOCK
         WHERE countTaskGateUser.GateUserID = CountTaskLocation.AssignedTo NO-ERROR.
      
      FIND FIRST CountTaskStatus OF CountTaskLocation NO-LOCK NO-ERROR.

      CountTaskLocationBrowse:startRow(CountTaskLocation.CountTaskLocationID, "selectCountTaskLocationRow(this," + '"' 
                                                                     + STRING(CountTaskLocation.CountTaskLocationID) + '"' + ");", "").
      CountTaskLocationBrowse:insertData(CountTaskLocation.CountTaskLocationID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountTaskLocation}
      
      CountTaskLocationBrowse:insertData(CountTaskLocation.CountTaskID).
      CountTaskLocationBrowse:insertData(STRING(Location.LocationRef),"left").
      CountTaskLocationBrowse:insertData(fDisplayDate&Time(CountTaskLocation.Completed,"y/m/d H:M:S"), "right").
      CountTaskLocationBrowse:insertData(STRING(CountTaskLocation.BlindCount, "Yes/No")).
      CountTaskLocationBrowse:insertData(IF AVAILABLE CountTaskStatus THEN STRING(CountTaskStatus.StatusName) ELSE "","left").
      CountTaskLocationBrowse:insertData(CountTaskLocation.Priority).
      CountTaskLocationBrowse:insertData(IF AVAILABLE countTaskGateUser THEN STRING(countTaskGateUser.FullName) ELSE "","left").
      
      /* Add hidden fields */
      CountTaskLocationBrowse:insertHiddenData("CountTaskVersionID",CountTask.VersionID).
      CountTaskLocationBrowse:insertHiddenData("CountTaskLocationVersionID",CountTaskLocation.VersionID).
      
      CountTaskLocationBrowse:endRow().
      
   END. /* FOR EACH CountGroup NO-LOCK */
   
   CountTaskLocationBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskLocationBrowse:getErrors().
   
   /* Hidden Fields */
   
   CountTaskLocationBrowseForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationBrowseForm:insertHiddenField("CountTaskLocationID","").
   CountTaskLocationBrowseForm:insertHiddenField("CountTaskLocationVersionID","").
   CountTaskLocationBrowseForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskLocationBrowseForm:insertHiddenField("counttasklocation_browse_scroll","").
   CountTaskLocationBrowseForm:insertHiddenField("popup_counttasklocation_browse","").
   CountTaskLocationBrowseForm:insertHiddenField("popup_addcounttasklocation_browse","").
   CountTaskLocationBrowseForm:insertHiddenField("popup_counttasklocationhistory_browse","").
   /*CountTaskLocationBrowseForm:insertHiddenField("filtering", "yes").*/
   CountTaskLocationBrowseForm:insertHiddenField("filtering", "no").
   CountTaskLocationBrowseForm:insertHiddenField("form_name","counttasklocation_browse_form").
   CountTaskLocationBrowseForm:insertHiddenField("prog_name","adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationBrowseForm}
   
   /* Create Button Bar */
   CountTaskLocationBrowseButtons = NEW buttonBar().

   CountTaskLocationBrowseButtons:addButton("counttasklocation_browse_form_btn_view",
                                            fTL("Details"),
                                            "viewCountTaskLocationDetails('counttasklocation_details_form');",
                                            (IF intSelectedCountTaskLocation > 0 THEN "" ELSE "Disabled")).

   CountTaskLocationBrowseButtons:addButton("counttasklocation_browse_form_btn_history",
                                            fTL("History"),
                                            "viewCountTaskLocationHistory('counttasklocationhistory_browse_form');",
                                            (IF intSelectedCountTaskLocation > 0 THEN "" ELSE "Disabled")).
   
   CountTaskLocationBrowseButtons:addButton("counttasklocation_browse_form_btn_cancel",
                                            fTL("Cancel"),
                                            "disablePopup('counttasklocation_browse_form_popup');").

   CountTaskLocationBrowseButtons:addButton("counttasklocation_browse_form_btn_excel",
                                            fTL("Excel Export"),
                                            "excelExport('" + STRING(intGblSessionID) + "_counttasklocation_browse.xml')").

   CountTaskLocationBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationBrowseForm:FormBrowse  = CountTaskLocationBrowse.
   CountTaskLocationBrowseForm:FormButtons = CountTaskLocationBrowseButtons.
   CountTaskLocationBrowseForm:endForm(). 
   
   CountTaskLocationBrowseForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationDetails Procedure 
PROCEDURE pCountTaskLocationDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "counttasklocation_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountTaskLocationID,CountTaskID,LocationRef,CountTaskStatusID,CompletedDate,CompletedHour,CompletedMins,Priority,BlindCount,AssignedTo"
          chrEditFieldList     = "Priority"
          chrNewFieldList      = "CountTaskLocationID,CountTaskID,LocationRef,CountTaskStatusID,CompletedDate,CompletedHour,CompletedMins,Priority,BlindCount,AssignedTo"
          chrRequiredFieldList = "Priority,AssignedTo"
          chrExtraFieldList    = ""
          chrValidateFieldList = "Priority:INTEGER".
   
   CountTaskLocationDetailsForm           = NEW dataForm("counttasklocation_details_form").
   CountTaskLocationDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskLocationDetailsForm:FormAction = "dbCountTaskLocationUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskLocationDetailsForm:FormWidth  = 460.
   CountTaskLocationDetailsForm:FormHeight = 300.
   CountTaskLocationDetailsForm:FormTitle  = " Count Task Location Details".
   CountTaskLocationDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountTaskLocationDetailsForm:insertPaddingColumn(40).
   CountTaskLocationDetailsForm:insertColumn(130).
   CountTaskLocationDetailsForm:insertColumn(120).
   CountTaskLocationDetailsForm:insertColumn(20).
   CountTaskLocationDetailsForm:insertColumn(4).
   CountTaskLocationDetailsForm:insertColumn(110).
   
   /* Fields */
   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("CountTaskLocation ID").
   CountTaskLocationDetailsForm:insertTextField("CountTaskLocationID", "", 110, TRUE).  
   
   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("CountTask ID").
   CountTaskLocationDetailsForm:insertTextField("CountTaskID", "", 110, TRUE).  
   
   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("Location Ref").
   CountTaskLocationDetailsForm:insertTextField("LocationRef", "", 110, TRUE).  
   
   /**
   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("Location").
   CountTaskLocationDetailsForm:insertComboField("LocationID", "", 110, TRUE).  
   
   FOR EACH Location NO-LOCK /* idx=ActiveListingSequence*/
      BY Location.Active DESC
      BY Location.CountSequence:
      
      CountTaskLocationDetailsForm:insertComboPairs("LocationID", STRING(Location.LocationID), Location.LocationRef).
   END.
   **/

   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel(fTL("Blind Count")). 
   CountTaskLocationDetailsForm:insertComboField("BlindCount", "", 110, TRUE).  
   CountTaskLocationDetailsForm:insertComboPairs("BlindCount", "yes", "Yes").
   CountTaskLocationDetailsForm:insertComboPairs("BlindCount", "no",  "No").
   
   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel(fTL("Completed")).
   CountTaskLocationDetailsForm:insertDateField("CompletedDate", "", 110, TRUE).
   CountTaskLocationDetailsForm:insertTextField("CompletedHour", "", 18, TRUE).
   CountTaskLocationDetailsForm:insertLabel(":").
   CountTaskLocationDetailsForm:insertTextField("CompletedMins", "", 18, TRUE).

   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("Count Task Status").
   CountTaskLocationDetailsForm:insertComboField("CountTaskStatusID", "", 110, TRUE).  
   FOR EACH CountTaskStatus NO-LOCK /* idx=ActiveListingSequence*/
      BY CountTaskStatus.Active DESC
      BY CountTaskStatus.ListingSequence:
      
      CountTaskLocationDetailsForm:insertComboPairs("CountTaskStatusID", STRING(CountTaskStatus.CountTaskStatusID), CountTaskStatus.StatusName).
   END.

   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("Priority").
   CountTaskLocationDetailsForm:insertTextField("Priority", "", 110, TRUE).  

   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("Performed By").
   CountTaskLocationDetailsForm:insertComboField("AssignedTo", "", 110, TRUE).  
   CountTaskLocationDetailsForm:insertComboPairs("AssignedTo","0","Any User").
   FOR EACH GateUser NO-LOCK /* idx=ActiveListingSequence*/
      BY GateUser.Active DESC
      BY GateUser.FullName:
      
      CountTaskLocationDetailsForm:insertComboPairs("AssignedTo", STRING(GateUser.GateUserID), GateUser.FullName).
   END.


   {webGetOptionalFormFields.i pCountGroupDetailsFields}
   
   /* Add Hidden Fields*/
   CountTaskLocationDetailsForm:insertHiddenField("counttask_browse_scroll", "").
   CountTaskLocationDetailsForm:insertHiddenField("counttasklocation_browse_scroll","").
   CountTaskLocationDetailsForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationDetailsForm:insertHiddenField("CountTaskLocationID","").
   CountTaskLocationDetailsForm:insertHiddenField("CountTaskLocationVersionID","").
   CountTaskLocationDetailsForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskLocationDetailsForm:insertHiddenField("counttasklocation_browse_scroll","").
   CountTaskLocationDetailsForm:insertHiddenField("popup_counttasklocation_browse","").
   CountTaskLocationDetailsForm:insertHiddenField("form_name", "counttasklocation_details_form").
   CountTaskLocationDetailsForm:insertHiddenField("prog_name", "adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationDetailsForm}
   
   /* Create Button Bar */
   CountTaskLocationDetailsButtons = NEW buttonBar().
   
   CountTaskLocationDetailsButtons:addButton("counttasklocation_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateCountTaskLocation('counttasklocation_details_form');").

   CountTaskLocationDetailsButtons:addButton("counttasklocation_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); disablePopup('counttasklocation_details_form_popup');").
   
   CountTaskLocationDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationDetailsForm:FormButtons = CountTaskLocationDetailsButtons.
   
   CountTaskLocationDetailsForm:endForm(). 
   
   CountTaskLocationDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationHistoryBrowse Procedure 
PROCEDURE pCountTaskLocationHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CountTaskLocationHistoryBrowseForm            = NEW dataForm("counttasklocationhistory_browse_form").
   CountTaskLocationHistoryBrowseForm:WebStream  = STREAM WebStream:HANDLE.

   /* Setup */
   CountTaskLocationHistoryBrowseForm:FormWidth  = 860. 
   CountTaskLocationHistoryBrowseForm:FormHeight = 530. 
   CountTaskLocationHistoryBrowseForm:FormTitle  = fTL("CountTaskLocationLink History") + 
                                                      (IF AVAILABLE CountTaskLocation THEN " : " + STRING(CountTaskLocation.CountTaskLocationID) ELSE "").
   CountTaskLocationHistoryBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountTaskLocationHistoryBrowse              = NEW browseTable("counttasklocationhistory_browse").
   CountTaskLocationHistoryBrowse:BrowseWidth  = 840.
   CountTaskLocationHistoryBrowse:BrowseHeight = 490.
   
   /* Add in the ID as first Column */
   CountTaskLocationHistoryBrowse:insertColumn(fTL("History ID"),  55, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskLocationHistory}
   
   CountTaskLocationHistoryBrowse:insertColumn(fTL("TaskID"),      50, "CHARACTER", "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Location Ref"),80, "CHARACTER", "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Pri"),         35, "INTEGER",   "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Completed"),  110, "CHARACTER", "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Blind"),       35, "LOGICAL", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Performed By"),105, "CHARACTER", "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Status"),      70, "CHARACTER", "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("User"),       105, "CHARACTER", "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Operation"),   65, "CHARACTER", "left", FALSE).
   CountTaskLocationHistoryBrowse:insertColumn(fTL("Created"),    110, "CHARACTER", "left", FALSE).
   
   /*Body*/
   CountTaskLocationHistoryBrowse:StartBody().
   
   FOR EACH CountTaskLocationHistory NO-LOCK
      WHERE CountTaskLocationHistory.CountTaskLocationID = CountTaskLocation.CountTaskLocationID
      BY CountTaskLocationHistory.Created DESCENDING
      BY CountTaskLocationHistory.CountTaskLocationHistoryID:

      FIND FIRST Location OF CountTaskLocationHistory NO-LOCK NO-ERROR.

      FIND FIRST countTaskGateUser NO-LOCK
         WHERE countTaskGateUser.GateUserID = CountTaskLocationHistory.AssignedTo NO-ERROR.
      
      FIND FIRST countTaskHistoryGateUser NO-LOCK
         WHERE countTaskHistoryGateUser.GateUserID = CountTaskLocationHistory.GateUserID NO-ERROR.

      FIND FIRST CountTaskStatus OF CountTaskLocationHistory NO-LOCK NO-ERROR.
      
      CountTaskLocationHistoryBrowse:startRow(CountTaskLocationHistory.CountTaskLocationHistoryID, "selectCountTaskLocationHistoryRow(this," + '"' 
                                                                     + STRING(CountTaskLocationHistory.CountTaskLocationHistoryID) + '"' + ");", "").
      CountTaskLocationHistoryBrowse:insertData(CountTaskLocationHistory.CountTaskLocationHistoryID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountTaskLocationHistory}
      
      CountTaskLocationHistoryBrowse:insertData(STRING(CountTaskLocationHistory.CountTaskID),"left").
      CountTaskLocationHistoryBrowse:insertData(IF AVAILABLE Location THEN STRING(Location.LocationRef) ELSE "","left").
      CountTaskLocationHistoryBrowse:insertData(CountTaskLocationHistory.Priority).
      CountTaskLocationHistoryBrowse:insertData(fDisplayDate&Time(CountTaskLocationHistory.Completed,"y/m/d H:M:S"), "right").
      CountTaskLocationHistoryBrowse:insertData(STRING(CountTaskLocationHistory.BlindCount, "Yes/No")).
      CountTaskLocationHistoryBrowse:insertData(IF AVAILABLE countTaskGateUser THEN STRING(countTaskGateUser.FullName) ELSE "","left").
      CountTaskLocationHistoryBrowse:insertData(IF AVAILABLE CountTaskStatus THEN STRING(CountTaskStatus.StatusName) ELSE "","left").
      CountTaskLocationHistoryBrowse:insertData((IF AVAILABLE countTaskHistoryGateUser THEN countTaskHistoryGateUser.FullName ELSE ""), "left").
      CountTaskLocationHistoryBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
      CountTaskLocationHistoryBrowse:insertData(fDisplayDate&Time(CountTaskLocationHistory.Created,"y/m/d H:M:S"), "right").
      
      CountTaskLocationHistoryBrowse:endRow().
      
   END. /* FOR EACH CountTaskLocationHistory NO-LOCK */
   
   CountTaskLocationHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskLocationHistoryBrowse:getErrors().
   
   /* Hidden Fields */
   /**
   CountTaskLocationHistoryBrowseForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationHistoryBrowseForm:insertHiddenField("CountTaskLocationHistoryID","").
   CountTaskLocationHistoryBrowseForm:insertHiddenField("CountTaskLocationHistoryVersionID","").
   CountTaskLocationHistoryBrowseForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskLocationHistoryBrowseForm:insertHiddenField("counttasklocation_browse_scroll","").
   CountTaskLocationHistoryBrowseForm:insertHiddenField("popup_counttasklocation_browse","").
   **/

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationHistoryBrowseForm}
   
   /* Create Button Bar */
   CountTaskLocationHistoryBrowseButtons = NEW buttonBar().
   
   
   CountTaskLocationHistoryBrowseButtons:addButton("counttasklocation_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('counttasklocationhistory_browse_form_popup');").

   CountTaskLocationHistoryBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationHistoryBrowseForm:FormBrowse  = CountTaskLocationHistoryBrowse.
   CountTaskLocationHistoryBrowseForm:FormButtons = CountTaskLocationHistoryBrowseButtons.
   CountTaskLocationHistoryBrowseForm:endForm(). 
   
   CountTaskLocationHistoryBrowseForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationPackageBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationPackageBrowse Procedure 
PROCEDURE pCountTaskLocationPackageBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "counttasklocationpackage_details_form"}
   
   CountTaskLocationPackageBrowseForm            = NEW dataForm("counttasklocationpackage_browse_form").
   CountTaskLocationPackageBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   CountTaskLocationPackageBrowseForm:FormAction = "dbCountTaskLocationPackageUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   CountTaskLocationPackageBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   CountTaskLocationPackageBrowseForm:FormWidth  = 860. 
   CountTaskLocationPackageBrowseForm:FormHeight = 530. 
   CountTaskLocationPackageBrowseForm:FormTitle  = (IF chrSelectedButtonMode = "Results" THEN fTL("Package Results for Count Task") ELSE fTL("Package Discrepancies for Count Task")) + 
                                                      (IF AVAILABLE CountTask THEN " : " + STRING(CountTask.CountTaskID) ELSE "").
   CountTaskLocationPackageBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountTaskLocationPackageBrowse              = NEW browseTable("counttasklocationpackage_browse").
   CountTaskLocationPackageBrowse:BrowseWidth  = 840.
   CountTaskLocationPackageBrowse:BrowseHeight = 490.
   CountTaskLocationPackageBrowse:ExcelExport  = TRUE.
   CountTaskLocationPackageBrowse:SessionID    = intGblSessionID.
   
   /* Add in the ID as first Column */
   CountTaskLocationPackageBrowse:insertColumn(fTL("ID"),             90, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskLocationPackage}
   
   CountTaskLocationPackageBrowse:insertColumn(fTL("Location"),      100, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageBrowse:insertColumn(fTL("Part"),          100, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageBrowse:insertColumn(fTL("Qty Expected"),   80, "INTEGER", FALSE).
   CountTaskLocationPackageBrowse:insertColumn(fTL("Qty Counted"),    80, "INTEGER", FALSE).
   CountTaskLocationPackageBrowse:insertColumn(fTL("Difference"),     80, "INTEGER", FALSE).
   CountTaskLocationPackageBrowse:insertColumn(fTL("AddedAfter"),     80, "LOGICAL", FALSE).
   CountTaskLocationPackageBrowse:insertColumn(fTL("Completed"),     100, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageBrowse:insertColumn(fTL("Status"),         80, "CHARACTER", "left", FALSE).
   /*Body*/
   CountTaskLocationPackageBrowse:StartBody().
   
   IF AVAILABLE CountTask THEN
   DO:
      FIND FIRST CountTaskType OF CountTask NO-LOCK NO-ERROR.

      IF AVAILABLE CountTaskType AND CountTaskType.TypeCode = "PackageScan" THEN
      DO:
         CountTaskLocationLoop:
         FOR EACH CountTaskLocation OF CountTask NO-LOCK,
            EACH CountTaskLocationPackage OF CountTaskLocation NO-LOCK,
               FIRST Location OF CountTaskLocation NO-LOCK,
               FIRST StockPackage OF CountTaskLocationPackage NO-LOCK,
               FIRST Part OF StockPackage NO-LOCK,
               FIRST CountTaskStatus OF CountTaskLocationPackage NO-LOCK
                  BY Location.LocationRef
                  BY Part.PartRef:
            
            IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
               NEXT CountTaskLocationLoop.

            /* Don't show records that don't have discrepancies when mode is discrepancies */
            IF chrSelectedButtonMode = "Discrepancies" AND CountTaskLocationPackage.QtyExpected = CountTaskLocationPackage.QtyCounted THEN NEXT.

            CountTaskLocationPackageBrowse:startRow(CountTaskLocationPackage.CountTaskLocationPackageID, "selectCountTaskLocationPackageRow(this," + '"' 
                                                                           + STRING(CountTaskLocationPackage.CountTaskLocationPackageID) + '"' + ");", "").
            CountTaskLocationPackageBrowse:insertData(CountTaskLocationPackage.CountTaskLocationPackageID).

            /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
            {webGetOptionalBrowseFields.i CountTaskLocationPackage}

            CountTaskLocationPackageBrowse:insertData(STRING(Location.LocationRef),"left").
            CountTaskLocationPackageBrowse:insertData(STRING(Part.PartRef),"left").
            CountTaskLocationPackageBrowse:insertData(CountTaskLocationPackage.QtyExpected).
            CountTaskLocationPackageBrowse:insertData(CountTaskLocationPackage.QtyCounted).
            CountTaskLocationPackageBrowse:insertData(CountTaskLocationPackage.QtyExpected - CountTaskLocationPackage.QtyCounted).
            CountTaskLocationPackageBrowse:insertData(STRING(CountTaskLocationPackage.AddedAfterCountBegan, "Yes/No")).
            CountTaskLocationPackageBrowse:insertData(fDisplayDate&Time(CountTaskLocationPackage.Completed,"y/m/d H:M:S"), "right").
            CountTaskLocationPackageBrowse:insertData(IF AVAILABLE CountTaskStatus THEN STRING(CountTaskStatus.StatusName) ELSE "","left").

            /* Add hidden fields */
            CountTaskLocationPackageBrowse:insertHiddenData("CountTaskVersionID",CountTask.VersionID).
            CountTaskLocationPackageBrowse:insertHiddenData("CountTaskLocationPackageVersionID",CountTaskLocationPackage.VersionID).

            CountTaskLocationPackageBrowse:endRow().
         
            
         END. /* FOR EACH CountTaskLocation OF CountTask NO-LOCK */

      END.

   END.

   CountTaskLocationPackageBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskLocationPackageBrowse:getErrors().
   
   /* Hidden Fields */
   
   CountTaskLocationPackageBrowseForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationPackageBrowseForm:insertHiddenField("CountTaskLocationPackageID","").
   CountTaskLocationPackageBrowseForm:insertHiddenField("CountTaskLocationPackageVersionID","").
   CountTaskLocationPackageBrowseForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskLocationPackageBrowseForm:insertHiddenField("counttasklocationpackage_browse_scroll","").
   CountTaskLocationPackageBrowseForm:insertHiddenField("popup_counttasklocationpackage_browse","").
   CountTaskLocationPackageBrowseForm:insertHiddenField("popup_counttasklocationpackagehistory_browse","").
   CountTaskLocationPackageBrowseForm:insertHiddenField("filtering","no").
   CountTaskLocationPackageBrowseForm:insertHiddenField("form_name","counttasklocationpackage_browse_form").
   CountTaskLocationPackageBrowseForm:insertHiddenField("prog_name","adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationPackageBrowseForm}
   
   /* Create Button Bar */
   CountTaskLocationPackageBrowseButtons = NEW buttonBar().
   
   CountTaskLocationPackageBrowseButtons:addButton("counttasklocationpackage_browse_form_btn_view",
                                                   fTL("Details"),
                                                   "viewCountTaskLocationPackageDetails('counttasklocationpackage_details_form');",
                                                   (IF intSelectedCountTaskLocationPackage > 0 THEN "" ELSE "Disabled")).

   CountTaskLocationPackageBrowseButtons:addButton("counttasklocationpackage_browse_form_btn_history",
                                                   fTL("History"),
                                                   "viewCountTaskLocationPackageHistory('counttasklocationpackagehistory_browse_form');",
                                                   (IF intSelectedCountTaskLocationPackage > 0 THEN "" ELSE "Disabled")).
   
   CountTaskLocationPackageBrowseButtons:addButton("counttasklocationpackage_browse_form_btn_cancel",
                                                   fTL("Cancel"),
                                                   "disablePopup('counttasklocationpackage_browse_form_popup');").

   CountTaskLocationPackageBrowseButtons:addButton("counttasklocationpackage_browse_form_btn_excel",
                                                   fTL("Excel Export"),
                                                   "excelExport('" + STRING(intGblSessionID) + "_counttasklocationpackage_browse.xml')").

   CountTaskLocationPackageBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationPackageBrowseForm:FormBrowse  = CountTaskLocationPackageBrowse.
   CountTaskLocationPackageBrowseForm:FormButtons = CountTaskLocationPackageBrowseButtons.
   CountTaskLocationPackageBrowseForm:endForm(). 
   
   CountTaskLocationPackageBrowseForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationPackageDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationPackageDetails Procedure 
PROCEDURE pCountTaskLocationPackageDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "counttasklocationpackage_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountTaskLocationPackageID,CountTaskLocationID,StockPackageID,QtyExpected,QtyCounted,CountTaskStatusID,CompletedDate,CompletedHour,CompletedMins,Priority,AddedAfterCountBegan"
          chrEditFieldList     = "QtyCounted,Priority"
          chrNewFieldList      = "CountTaskLocationPackageID,CountTaskLocationID,StockPackageID,QtyExpected,QtyCounted,CountTaskStatusID,CompletedDate,CompletedHour,CompletedMins,Priority,AddedAfterCountBegan"
          chrRequiredFieldList = "QtyCounted,Priority"
          chrExtraFieldList    = ""
          chrValidateFieldList = "QtyCounted:INTEGER,Priority:INTEGER".
   
   CountTaskLocationPackageDetailsForm           = NEW dataForm("counttasklocationpackage_details_form").
   CountTaskLocationPackageDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskLocationPackageDetailsForm:FormAction = "dbCountTaskLocationPackageUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskLocationPackageDetailsForm:FormWidth  = 460.
   CountTaskLocationPackageDetailsForm:FormHeight = 300.
   CountTaskLocationPackageDetailsForm:FormTitle  = " Count Task Location Package Details".
   CountTaskLocationPackageDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountTaskLocationPackageDetailsForm:insertPaddingColumn(40).
   CountTaskLocationPackageDetailsForm:insertColumn(130).
   CountTaskLocationPackageDetailsForm:insertColumn(120).
   CountTaskLocationPackageDetailsForm:insertColumn(20).
   CountTaskLocationPackageDetailsForm:insertColumn(4).
   CountTaskLocationPackageDetailsForm:insertColumn(110).
   
   /* Fields */
   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel("CountTaskLocationPackage ID").
   CountTaskLocationPackageDetailsForm:insertTextField("CountTaskLocationPackageID", "", 110, TRUE).  
   
   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel("CountTaskLocation ID").
   CountTaskLocationPackageDetailsForm:insertTextField("CountTaskLocationID", "", 110, TRUE).  
   
   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel("StockPackage ID").
   CountTaskLocationPackageDetailsForm:insertComboField("StockPackageID", "", 110, TRUE).  

   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel("Qty Expected").
   CountTaskLocationPackageDetailsForm:insertTextField("QtyExpected", "", 110, TRUE).  
   
   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel("Qty Counted").
   CountTaskLocationPackageDetailsForm:insertTextField("QtyCounted", "", 110, TRUE).  
   
   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel(fTL("Completed")).
   CountTaskLocationPackageDetailsForm:insertDateField("CompletedDate", "", 110, TRUE).
   CountTaskLocationPackageDetailsForm:insertTextField("CompletedHour", "", 18, TRUE).
   CountTaskLocationPackageDetailsForm:insertLabel(":").
   CountTaskLocationPackageDetailsForm:insertTextField("CompletedMins", "", 18, TRUE).

   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel("Count Task Status").
   CountTaskLocationPackageDetailsForm:insertComboField("CountTaskStatusID", "", 110, TRUE).  
   FOR EACH CountTaskStatus NO-LOCK /* idx=ActiveListingSequence*/
      BY CountTaskStatus.Active DESC
      BY CountTaskStatus.ListingSequence:
      
      CountTaskLocationPackageDetailsForm:insertComboPairs("CountTaskStatusID", STRING(CountTaskStatus.CountTaskStatusID), CountTaskStatus.StatusName).
   END.

   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel("Priority").
   CountTaskLocationPackageDetailsForm:insertTextField("Priority", "", 110, TRUE).  

   CountTaskLocationPackageDetailsForm:startRow().
   CountTaskLocationPackageDetailsForm:insertLabel(fTL("Added After Count Began")). 
   CountTaskLocationPackageDetailsForm:insertComboField("AddedAfterCountBegan", "", 110, TRUE).  
   CountTaskLocationPackageDetailsForm:insertComboPairs("AddedAfterCountBegan", "yes", "Yes").
   CountTaskLocationPackageDetailsForm:insertComboPairs("AddedAfterCountBegan", "no",  "No").
   
   {webGetOptionalFormFields.i pCountGroupDetailsFields}
   
   /* Add Hidden Fields*/
   CountTaskLocationPackageDetailsForm:insertHiddenField("counttask_browse_scroll", "").
   CountTaskLocationPackageDetailsForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationPackageDetailsForm:insertHiddenField("CountTaskLocationPackageID","").
   CountTaskLocationPackageDetailsForm:insertHiddenField("CountTaskLocationPackageVersionID","").
   CountTaskLocationPackageDetailsForm:insertHiddenField("counttasklocationpackage_browse_scroll","").
   CountTaskLocationPackageDetailsForm:insertHiddenField("popup_counttasklocationpackage_browse","").
   CountTaskLocationPackageDetailsForm:insertHiddenField("form_name", "counttasklocationpackage_details_form").
   CountTaskLocationPackageDetailsForm:insertHiddenField("prog_name", "adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationPackageDetailsForm}
   
   /* Create Button Bar */
   CountTaskLocationPackageDetailsButtons = NEW buttonBar().
   
   CountTaskLocationPackageDetailsButtons:addButton("counttasklocationpackage_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateCountTaskLocationPackage('counttasklocation_details_form');").

   CountTaskLocationPackageDetailsButtons:addButton("counttasklocationpackage_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); disablePopup('counttasklocationpackage_details_form_popup');").
   
   CountTaskLocationPackageDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationPackageDetailsForm:FormButtons = CountTaskLocationPackageDetailsButtons.
   
   CountTaskLocationPackageDetailsForm:endForm(). 
   
   CountTaskLocationPackageDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationPackageHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationPackageHistoryBrowse Procedure 
PROCEDURE pCountTaskLocationPackageHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CountTaskLocationPackageHistBrowseForm           = NEW dataForm("counttasklocationpackagehistory_browse_form").
   CountTaskLocationPackageHistBrowseForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   CountTaskLocationPackageHistBrowseForm:FormWidth  = 860. 
   CountTaskLocationPackageHistBrowseForm:FormHeight = 530. 
   CountTaskLocationPackageHistBrowseForm:FormTitle  = fTL("CountTaskLocationPackageLink History") + 
                                                             (IF AVAILABLE CountTaskLocationPackage THEN " : " + STRING(CountTaskLocationPackage.CountTaskLocationPackageID) ELSE "").
   CountTaskLocationPackageHistBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountTaskLocationPackageHistBrowse              = NEW browseTable("counttasklocationpackagehistory_browse").
   CountTaskLocationPackageHistBrowse:BrowseWidth  = 840.
   CountTaskLocationPackageHistBrowse:BrowseHeight = 490.
   
   /* Add in the ID as first Column */
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Hist ID"),  80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskLocationPackageHist}
   
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Location"),     80, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Part"),         80, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Qty Expected"), 80, "INTEGER", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Qty Counted"),  80, "INTEGER", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("AddedAfter"),   70, "LOGICAL", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Completed"),    70, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Status"),       70, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("User"),         70, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Operation"),    70, "CHARACTER", "left", FALSE).
   CountTaskLocationPackageHistBrowse:insertColumn(fTL("Created"),      70, "CHARACTER", "left", FALSE).
   
   /*Body*/
   CountTaskLocationPackageHistBrowse:StartBody().
   
   FOR EACH CountTaskLocationPackageHist NO-LOCK
      WHERE CountTaskLocationPackageHist.CountTaskLocationPackageID = CountTaskLocationPackage.CountTaskLocationPackageID
      BY CountTaskLocationPackageHist.Created DESCENDING
      BY CountTaskLocationPackageHist.CountTaskLocationPackageHistID:

      FIND FIRST CountTaskLocation OF CountTaskLocationPackage NO-LOCK NO-ERROR.
      
      FIND FIRST Location OF CountTaskLocation NO-LOCK NO-ERROR.
      
      FIND FIRST StockPackage OF CountTaskLocationPackageHist NO-LOCK NO-ERROR.

      FIND FIRST Part OF StockPackage NO-LOCK NO-ERROR.

      FIND FIRST countTaskHistoryGateUser NO-LOCK
         WHERE countTaskHistoryGateUser.GateUserID = CountTaskLocationPackageHist.GateUserID NO-ERROR.

      FIND FIRST CountTaskStatus OF CountTaskLocationPackageHist NO-LOCK NO-ERROR.
      
      CountTaskLocationPackageHistBrowse:startRow(CountTaskLocationPackageHist.CountTaskLocationPackageHistID, "selectCountTaskLocationPackageHistRow(this," + '"' 
                                                                     + STRING(CountTaskLocationPackageHist.CountTaskLocationPackageHistID) + '"' + ");", "").
      CountTaskLocationPackageHistBrowse:insertData(CountTaskLocationPackageHist.CountTaskLocationPackageHistID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountTaskLocationPackageHist}
      
      CountTaskLocationPackageHistBrowse:insertData(STRING(Location.LocationRef),"left").
      CountTaskLocationPackageHistBrowse:insertData(STRING(Part.PartRef),"left").
      CountTaskLocationPackageHistBrowse:insertData(CountTaskLocationPackage.QtyExpected).
      CountTaskLocationPackageHistBrowse:insertData(CountTaskLocationPackage.QtyCounted).
      CountTaskLocationPackageHistBrowse:insertData(CountTaskLocationPackage.QtyExpected - CountTaskLocationPackage.QtyCounted).
      CountTaskLocationPackageHistBrowse:insertData(STRING(CountTaskLocationPackage.AddedAfterCountBegan, "Yes/No")).
      CountTaskLocationPackageHistBrowse:insertData(fDisplayDate&Time(CountTaskLocation.Completed,"y/m/d H:M:S"), "right").
      CountTaskLocationPackageHistBrowse:insertData(IF AVAILABLE CountTaskStatus THEN STRING(CountTaskStatus.StatusName) ELSE "","left").
      CountTaskLocationPackageHistBrowse:insertData((IF AVAILABLE countTaskHistoryGateUser THEN countTaskHistoryGateUser.FullName ELSE ""), "left").
      CountTaskLocationPackageHistBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
      CountTaskLocationPackageHistBrowse:insertData(fDisplayDate&Time(CountTaskLocationPackageHist.Created,"y/m/d H:M:S"), "right").
      
      CountTaskLocationPackageHistBrowse:endRow().
      
   END. /* FOR EACH CountTaskLocationPackageHist NO-LOCK */
   
   CountTaskLocationPackageHistBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskLocationPackageHistBrowse:getErrors().
   
   /* Hidden Fields */
   /**
   CountTaskLocationPackageHistBrowseForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationPackageHistBrowseForm:insertHiddenField("CountTaskLocationPackageHistID","").
   CountTaskLocationPackageHistBrowseForm:insertHiddenField("CountTaskLocationPackageHistVersionID","").
   CountTaskLocationPackageHistBrowseForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskLocationPackageHistBrowseForm:insertHiddenField("counttasklocation_browse_scroll","").
   CountTaskLocationPackageHistBrowseForm:insertHiddenField("popup_counttasklocation_browse","").
   **/

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationPackageHistBrowseForm}
   
   /* Create Button Bar */
   CountTaskLocationPackageHistBrowseButtons = NEW buttonBar().
   
   
   CountTaskLocationPackageHistBrowseButtons:addButton("counttasklocationpackagehistory_browse_form_btn_cancel",
                                                       fTL("Cancel"),
                                                       "disablePopup('counttasklocationpackagehistory_browse_form_popup');").

   CountTaskLocationPackageHistBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationPackageHistBrowseForm:FormBrowse  = CountTaskLocationPackageHistBrowse.
   CountTaskLocationPackageHistBrowseForm:FormButtons = CountTaskLocationPackageHistBrowseButtons.
   CountTaskLocationPackageHistBrowseForm:endForm(). 
   
   CountTaskLocationPackageHistBrowseForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationPartBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationPartBrowse Procedure 
PROCEDURE pCountTaskLocationPartBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "counttasklocationpart_details_form"}
   
   CountTaskLocationPartBrowseForm            = NEW dataForm("counttasklocationpart_browse_form").
   CountTaskLocationPartBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   CountTaskLocationPartBrowseForm:FormAction = "dbCountTaskLocationPartUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   CountTaskLocationPartBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   CountTaskLocationPartBrowseForm:FormWidth  = 860. 
   CountTaskLocationPartBrowseForm:FormHeight = 530. 
   CountTaskLocationPartBrowseForm:FormTitle  = (IF chrSelectedButtonMode = "Results" THEN fTL("Part Results for Count Task") ELSE fTL("Part Discrepancies for Count Task")) + 
                                                   (IF AVAILABLE CountTask THEN " : " + STRING(CountTask.CountTaskID) ELSE "").
   CountTaskLocationPartBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountTaskLocationPartBrowse              = NEW browseTable("counttasklocationpart_browse").
   CountTaskLocationPartBrowse:BrowseWidth  = 840.
   CountTaskLocationPartBrowse:BrowseHeight = 490.
   CountTaskLocationPartBrowse:ExcelExport  = TRUE.
   CountTaskLocationPartBrowse:SessionID    = intGblSessionID.
   
   /* Add in the ID as first Column */
   CountTaskLocationPartBrowse:insertColumn(fTL("ID"),            80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskLocationPart}
   
   CountTaskLocationPartBrowse:insertColumn(fTL("Location"),       70, "CHARACTER", "left", FALSE).
   CountTaskLocationPartBrowse:insertColumn(fTL("Part"),          100, "CHARACTER", "left", FALSE).
   CountTaskLocationPartBrowse:insertColumn(fTL("Qty Expected"),   80, "INTEGER", FALSE).
   CountTaskLocationPartBrowse:insertColumn(fTL("Qty Counted"),    80, "INTEGER", FALSE).
   CountTaskLocationPartBrowse:insertColumn(fTL("Difference"),     80, "INTEGER", FALSE).
   CountTaskLocationPartBrowse:insertColumn(fTL("AddedAfter"),     80, "LOGICAL", FALSE).
   CountTaskLocationPartBrowse:insertColumn(fTL("Completed"),     120, "CHARACTER", "left", FALSE).
   CountTaskLocationPartBrowse:insertColumn(fTL("Status"),        100, "CHARACTER", "left", FALSE).
   /*Body*/
   CountTaskLocationPartBrowse:StartBody().
   
   IF AVAILABLE CountTask THEN
   DO:
      FIND FIRST CountTaskType OF CountTask NO-LOCK NO-ERROR.

      IF AVAILABLE CountTaskType AND CountTaskType.TypeCode = "PartScan" THEN
      DO:
         CountTaskLocationLoop:
         FOR EACH CountTaskLocation OF CountTask NO-LOCK,
            EACH CountTaskLocationPart OF CountTaskLocation NO-LOCK,
               FIRST Location OF CountTaskLocation NO-LOCK,
               FIRST Part OF CountTaskLocationPart NO-LOCK,
               FIRST CountTaskStatus OF CountTaskLocationPart NO-LOCK
                  BY Location.LocationRef
                  BY Part.PartRef:
            
            IF NOT fCanViewBusinessUnit(intGblSessionID,Location.BusinessUnitID) THEN 
               NEXT CountTaskLocationLoop.

            /* Don't show records that don't have discrepancies when mode is discrepancies */
            IF chrSelectedButtonMode = "Discrepancies" AND CountTaskLocationPart.QtyExpected = CountTaskLocationPart.QtyCounted THEN NEXT.

            CountTaskLocationPartBrowse:startRow(CountTaskLocationPart.CountTaskLocationPartID, "selectCountTaskLocationPartRow(this," + '"' 
                                                                           + STRING(CountTaskLocationPart.CountTaskLocationPartID) + '"' + ");", "").
            CountTaskLocationPartBrowse:insertData(CountTaskLocationPart.CountTaskLocationPartID).

            /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
            {webGetOptionalBrowseFields.i CountTaskLocationPart}

            CountTaskLocationPartBrowse:insertData(STRING(Location.LocationRef),"left").
            CountTaskLocationPartBrowse:insertData(STRING(Part.PartRef),"left").
            CountTaskLocationPartBrowse:insertData(CountTaskLocationPart.QtyExpected).
            CountTaskLocationPartBrowse:insertData(CountTaskLocationPart.QtyCounted).
            CountTaskLocationPartBrowse:insertData(CountTaskLocationPart.QtyExpected - CountTaskLocationPart.QtyCounted).
            CountTaskLocationPartBrowse:insertData(STRING(CountTaskLocationPart.AddedAfterCountBegan, "Yes/No")).
            CountTaskLocationPartBrowse:insertData(fDisplayDate&Time(CountTaskLocationPart.Completed,"y/m/d H:M:S"), "right").
            CountTaskLocationPartBrowse:insertData(IF AVAILABLE CountTaskStatus THEN STRING(CountTaskStatus.StatusName) ELSE "","left").

            /* Add hidden fields */
            CountTaskLocationPartBrowse:insertHiddenData("CountTaskVersionID",CountTask.VersionID).
            CountTaskLocationPartBrowse:insertHiddenData("CountTaskLocationPartVersionID",CountTaskLocationPart.VersionID).

            CountTaskLocationPartBrowse:endRow().


         END. /* FOR EACH CountTaskLocation OF CountTask NO-LOCK */

      END.

   END.

   CountTaskLocationPartBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskLocationPartBrowse:getErrors().
   
   /* Hidden Fields */
   
   CountTaskLocationPartBrowseForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationPartBrowseForm:insertHiddenField("CountTaskLocationPartID","").
   CountTaskLocationPartBrowseForm:insertHiddenField("CountTaskLocationPartVersionID","").
   CountTaskLocationPartBrowseForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskLocationPartBrowseForm:insertHiddenField("counttasklocationpart_browse_scroll","").
   CountTaskLocationPartBrowseForm:insertHiddenField("popup_counttasklocationpart_browse","").
   CountTaskLocationPartBrowseForm:insertHiddenField("popup_counttasklocationparthistory_browse","").
   CountTaskLocationPartBrowseForm:insertHiddenField("filtering","no").
   CountTaskLocationPartBrowseForm:insertHiddenField("form_name","counttasklocationpart_browse_form").
   CountTaskLocationPartBrowseForm:insertHiddenField("prog_name","adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationPartBrowseForm}
   
   /* Create Button Bar */
   CountTaskLocationPartBrowseButtons = NEW buttonBar().
   
   CountTaskLocationPartBrowseButtons:addButton("counttasklocationpart_browse_form_btn_view",
                                                fTL("Details"),
                                                "viewCountTaskLocationPartDetails('counttasklocationpart_details_form');",
                                                (IF intSelectedCountTaskLocationPart > 0 THEN "" ELSE "Disabled")).

   CountTaskLocationPartBrowseButtons:addButton("counttasklocationpart_browse_form_btn_history",
                                                fTL("History"),
                                                "viewCountTaskLocationPartHistory('counttasklocationparthistory_browse_form');",
                                                (IF intSelectedCountTaskLocationPart > 0 THEN "" ELSE "Disabled")).
   
   CountTaskLocationPartBrowseButtons:addButton("counttasklocationpart_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('counttasklocationpart_browse_form_popup');").

   CountTaskLocationPartBrowseButtons:addButton("counttasklocationpart_browse_form_btn_excel",
                                                fTL("Excel Export"),
                                                "excelExport('" + STRING(intGblSessionID) + "_counttasklocationpart_browse.xml')").

   CountTaskLocationPartBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationPartBrowseForm:FormBrowse  = CountTaskLocationPartBrowse.
   CountTaskLocationPartBrowseForm:FormButtons = CountTaskLocationPartBrowseButtons.
   CountTaskLocationPartBrowseForm:endForm(). 
   
   CountTaskLocationPartBrowseForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationPartDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationPartDetails Procedure 
PROCEDURE pCountTaskLocationPartDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "counttasklocationpart_details_form"}
   
   ASSIGN chrDisplayFieldList  = "CountTaskLocationPartID,CountTaskLocationID,PartID,QtyExpected,QtyCounted,CountTaskStatusID,CompletedDate,CompletedHour,CompletedMins,Priority,AddedAfterCountBegan"
          chrEditFieldList     = "QtyCounted,Priority"
          chrNewFieldList      = "CountTaskLocationPartID,CountTaskLocationID,PartID,QtyExpected,QtyCounted,CountTaskStatusID,CompletedDate,CompletedHour,CompletedMins,Priority,AddedAfterCountBegan"
          chrRequiredFieldList = "QtyCounted,Priority"
          chrExtraFieldList    = ""
          chrValidateFieldList = "QtyCounted:INTEGER,Priority:INTEGER".
   
   CountTaskLocationPartDetailsForm           = NEW dataForm("counttasklocationpart_details_form").
   CountTaskLocationPartDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskLocationPartDetailsForm:FormAction = "dbCountTaskLocationPartUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskLocationPartDetailsForm:FormWidth  = 460.
   CountTaskLocationPartDetailsForm:FormHeight = 300.
   CountTaskLocationPartDetailsForm:FormTitle  = " Count Task Location Part Details".
   CountTaskLocationPartDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountTaskLocationPartDetailsForm:insertPaddingColumn(40).
   CountTaskLocationPartDetailsForm:insertColumn(130).
   CountTaskLocationPartDetailsForm:insertColumn(120).
   CountTaskLocationPartDetailsForm:insertColumn(20).
   CountTaskLocationPartDetailsForm:insertColumn(4).
   CountTaskLocationPartDetailsForm:insertColumn(110).
   
   /* Fields */
   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel("CountTaskLocationPart ID").
   CountTaskLocationPartDetailsForm:insertTextField("CountTaskLocationPartID", "", 110, TRUE).  
   
   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel("CountTaskLocation ID").
   CountTaskLocationPartDetailsForm:insertTextField("CountTaskLocationID", "", 110, TRUE).  
   
   CountTaskLocationDetailsForm:startRow().
   CountTaskLocationDetailsForm:insertLabel("Part Ref").
   CountTaskLocationDetailsForm:insertTextField("PartRef", "", 110, TRUE).  

   /**
   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel("Part").
   CountTaskLocationPartDetailsForm:insertComboField("PartID", "", 110, TRUE).  
   
   FOR EACH Part NO-LOCK /* idx=ActiveListingSequence*/
      BY Part.ACTIVE
      BY Part.PartID:
      
      CountTaskLocationPartDetailsForm:insertComboPairs("PartID", STRING(Part.PartID), Part.PartRef).
   END.
   **/

   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel("Qty Expected").
   CountTaskLocationPartDetailsForm:insertTextField("QtyExpected", "", 110, TRUE).  
   
   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel("Qty Counted").
   CountTaskLocationPartDetailsForm:insertTextField("QtyCounted", "", 110, TRUE).  
   
   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel(fTL("Completed")).
   CountTaskLocationPartDetailsForm:insertDateField("CompletedDate", "", 110, TRUE).
   CountTaskLocationPartDetailsForm:insertTextField("CompletedHour", "", 18, TRUE).
   CountTaskLocationPartDetailsForm:insertLabel(":").
   CountTaskLocationPartDetailsForm:insertTextField("CompletedMins", "", 18, TRUE).

   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel("Count Task Status").
   CountTaskLocationPartDetailsForm:insertComboField("CountTaskStatusID", "", 110, TRUE).  
   FOR EACH CountTaskStatus NO-LOCK /* idx=ActiveListingSequence*/
      BY CountTaskStatus.Active DESC
      BY CountTaskStatus.ListingSequence:
      
      CountTaskLocationPartDetailsForm:insertComboPairs("CountTaskStatusID", STRING(CountTaskStatus.CountTaskStatusID), CountTaskStatus.StatusName).
   END.

   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel("Priority").
   CountTaskLocationPartDetailsForm:insertTextField("Priority", "", 110, TRUE).  

   CountTaskLocationPartDetailsForm:startRow().
   CountTaskLocationPartDetailsForm:insertLabel(fTL("Added After Count Began")). 
   CountTaskLocationPartDetailsForm:insertComboField("AddedAfterCountBegan", "", 110, TRUE).  
   CountTaskLocationPartDetailsForm:insertComboPairs("AddedAfterCountBegan", "yes", "Yes").
   CountTaskLocationPartDetailsForm:insertComboPairs("AddedAfterCountBegan", "no",  "No").
   
  {webGetOptionalFormFields.i pCountGroupDetailsFields}
   
   /* Add Hidden Fields*/
   CountTaskLocationPartDetailsForm:insertHiddenField("counttask_browse_scroll", "").
   CountTaskLocationPartDetailsForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationPartDetailsForm:insertHiddenField("CountTaskLocationPartID","").
   CountTaskLocationPartDetailsForm:insertHiddenField("CountTaskLocationPartVersionID","").
   CountTaskLocationPartDetailsForm:insertHiddenField("counttasklocationpart_browse_scroll","").
   CountTaskLocationPartDetailsForm:insertHiddenField("popup_counttasklocationpart_browse","").
   CountTaskLocationPartDetailsForm:insertHiddenField("form_name", "counttasklocationpart_details_form").
   CountTaskLocationPartDetailsForm:insertHiddenField("prog_name", "adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationPartDetailsForm}
   
   /* Create Button Bar */
   CountTaskLocationPartDetailsButtons = NEW buttonBar().
   
   CountTaskLocationPartDetailsButtons:addButton("counttasklocationpart_details_form_btn_save", 
                                             fTL("Save"), 
                                             "updateCountTaskLocationPart('counttasklocation_details_form');").

   CountTaskLocationPartDetailsButtons:addButton("counttasklocationpart_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); disablePopup('counttasklocationpart_details_form_popup');").
   
   CountTaskLocationPartDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationPartDetailsForm:FormButtons = CountTaskLocationPartDetailsButtons.
   
   CountTaskLocationPartDetailsForm:endForm(). 
   
   CountTaskLocationPartDetailsForm:displayForm(). 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskLocationPartHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskLocationPartHistoryBrowse Procedure 
PROCEDURE pCountTaskLocationPartHistoryBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CountTaskLocationPartHistBrowseForm           = NEW dataForm("counttasklocationparthistory_browse_form").
   CountTaskLocationPartHistBrowseForm:WebStream = STREAM WebStream:HANDLE.

   /* Setup */
   CountTaskLocationPartHistBrowseForm:FormWidth  = 860. 
   CountTaskLocationPartHistBrowseForm:FormHeight = 530. 
   CountTaskLocationPartHistBrowseForm:FormTitle  = fTL("CountTaskLocationPartLink History") + 
                                                             (IF AVAILABLE CountTaskLocationPart THEN " : " + STRING(CountTaskLocationPart.CountTaskLocationPartID) ELSE "").
   CountTaskLocationPartHistBrowseForm:FormType   = "xxl_large".

   /* Form Data */
   CountTaskLocationPartHistBrowse              = NEW browseTable("counttasklocationparthistory_browse").
   CountTaskLocationPartHistBrowse:BrowseWidth  = 840.
   CountTaskLocationPartHistBrowse:BrowseHeight = 490.
   
   /* Add in the ID as first Column */
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Hist ID"),  80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskLocationPartHist}
   

   CountTaskLocationPartHistBrowse:insertColumn(fTL("Location"),     80, "CHARACTER", "left", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Part"),         80, "CHARACTER", "left", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Qty Expected"), 80, "INTEGER", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Qty Counted"),  80, "INTEGER", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("AddedAfter"),   70, "LOGICAL", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Completed"),    70, "CHARACTER", "left", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Status"),       70, "CHARACTER", "left", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("User"),         70, "CHARACTER", "left", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Operation"),    70, "CHARACTER", "left", FALSE).
   CountTaskLocationPartHistBrowse:insertColumn(fTL("Created"),      70, "CHARACTER", "left", FALSE).
   
   /*Body*/
   CountTaskLocationPartHistBrowse:StartBody().
   
   FOR EACH CountTaskLocationPartHist NO-LOCK
      WHERE CountTaskLocationPartHist.CountTaskLocationPartID = CountTaskLocationPart.CountTaskLocationPartID
      BY CountTaskLocationPartHist.Created DESCENDING
      BY CountTaskLocationPartHist.CountTaskLocationPartHistID:

      FIND FIRST CountTaskLocation OF CountTaskLocationPart NO-LOCK NO-ERROR.
      
      FIND FIRST Location OF CountTaskLocation NO-LOCK NO-ERROR.
      
      FIND FIRST Part OF CountTaskLocationPartHist NO-LOCK NO-ERROR.

      FIND FIRST countTaskHistoryGateUser NO-LOCK
         WHERE countTaskHistoryGateUser.GateUserID = CountTaskLocationPartHist.GateUserID NO-ERROR.

      FIND FIRST CountTaskStatus OF CountTaskLocationPartHist NO-LOCK NO-ERROR.
      
      CountTaskLocationPartHistBrowse:startRow(CountTaskLocationPartHist.CountTaskLocationPartHistID, "selectCountTaskLocationPartHistRow(this," + '"' 
                                                                     + STRING(CountTaskLocationPartHist.CountTaskLocationPartHistID) + '"' + ");", "").
      CountTaskLocationPartHistBrowse:insertData(CountTaskLocationPartHist.CountTaskLocationPartHistID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i CountTaskLocationPartHist}
      
      CountTaskLocationPartHistBrowse:insertData(STRING(Location.LocationRef),"left").
      CountTaskLocationPartHistBrowse:insertData(STRING(Part.PartRef),"left").
      CountTaskLocationPartHistBrowse:insertData(CountTaskLocationPart.QtyExpected).
      CountTaskLocationPartHistBrowse:insertData(CountTaskLocationPart.QtyCounted).
      CountTaskLocationPartHistBrowse:insertData(CountTaskLocationPart.QtyExpected - CountTaskLocationPart.QtyCounted).
      CountTaskLocationPartHistBrowse:insertData(STRING(CountTaskLocationPart.AddedAfterCountBegan, "Yes/No")).
      CountTaskLocationPartHistBrowse:insertData(fDisplayDate&Time(CountTaskLocation.Completed,"y/m/d H:M:S"), "right").
      CountTaskLocationPartHistBrowse:insertData(IF AVAILABLE CountTaskStatus THEN STRING(CountTaskStatus.StatusName) ELSE "","left").
      CountTaskLocationPartHistBrowse:insertData((IF AVAILABLE countTaskHistoryGateUser THEN countTaskHistoryGateUser.FullName ELSE ""), "left").
      CountTaskLocationPartHistBrowse:insertData((IF AVAILABLE OperationType THEN OperationType.TypeName ELSE ""), "left").
      CountTaskLocationPartHistBrowse:insertData(fDisplayDate&Time(CountTaskLocationPartHist.Created,"y/m/d H:M:S"), "right").
      
      CountTaskLocationPartHistBrowse:endRow().
      
   END. /* FOR EACH CountTaskLocationPartHist NO-LOCK */
   
   CountTaskLocationPartHistBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskLocationPartHistBrowse:getErrors().
   
   /* Hidden Fields */
   /**
   CountTaskLocationPartHistBrowseForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskLocationPartHistBrowseForm:insertHiddenField("CountTaskLocationPartHistID","").
   CountTaskLocationPartHistBrowseForm:insertHiddenField("CountTaskLocationPartHistVersionID","").
   CountTaskLocationPartHistBrowseForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskLocationPartHistBrowseForm:insertHiddenField("counttasklocation_browse_scroll","").
   CountTaskLocationPartHistBrowseForm:insertHiddenField("popup_counttasklocation_browse","").
   **/

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskLocationPartHistBrowseForm}
   
   /* Create Button Bar */
   CountTaskLocationPartHistBrowseButtons = NEW buttonBar().
   
   
   CountTaskLocationPartHistBrowseButtons:addButton("counttasklocationparthistory_browse_form_btn_cancel",
                                                    fTL("Cancel"),
                                                    "disablePopup('counttasklocationparthistory_browse_form_popup');").

   CountTaskLocationPartHistBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskLocationPartHistBrowseForm:FormBrowse  = CountTaskLocationPartHistBrowse.
   CountTaskLocationPartHistBrowseForm:FormButtons = CountTaskLocationPartHistBrowseButtons.
   CountTaskLocationPartHistBrowseForm:endForm(). 
   
   CountTaskLocationPartHistBrowseForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskUserLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskUserLinkBrowse Procedure 
PROCEDURE pCountTaskUserLinkBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "counttaskuserlink_browse_form"}
   
   CountTaskUserLinkBrowseForm            = NEW dataForm("counttaskuserlink_browse_form").
   CountTaskUserLinkBrowseForm:WebStream  = STREAM WebStream:HANDLE.
   CountTaskUserLinkBrowseForm:FormAction = "dbCountTaskUserLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /*
   /* Dev Layout Helper */
   CountTaskUserLinkBrowseForm:ShowBorder = TRUE.
   */

   /* Setup */
   CountTaskUserLinkBrowseForm:FormWidth  = 600. 
   CountTaskUserLinkBrowseForm:FormHeight = 420. 
   CountTaskUserLinkBrowseForm:FormTitle  = fTL("Users Linked to Count Task") + 
                                                      (IF AVAILABLE CountTask THEN " : " + STRING(CountTask.CountTaskID) ELSE "").
   CountTaskUserLinkBrowseForm:FormType   = "large".

   /* Form Data */
   CountTaskUserLinkBrowse              = NEW browseTable("counttaskuserlink_browse").
   CountTaskUserLinkBrowse:BrowseWidth  = 580.
   CountTaskUserLinkBrowse:BrowseHeight = 380.
   CountTaskUserLinkBrowse:SessionID    = intGblSessionID.
   
   /* Add in the ID as first Column */
   CountTaskUserLinkBrowse:insertColumn(fTL("CountTaskUserLink ID"),    115, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i CountTaskUserLink}
   
   CountTaskUserLinkBrowse:insertColumn(fTL("TaskID"),         90, "INTEGER", FALSE).
   CountTaskUserLinkBrowse:insertColumn(fTL("User Name"),     290, "CHARACTER", "left", FALSE).

   /*Body*/
   CountTaskUserLinkBrowse:StartBody().
   IF AVAILABLE CountTask THEN
   DO: 
      /* Display diffrent depending if CountTask Was completed or not */ 
      IF CountTask.Completed = "" THEN 
      DO:
         FOR EACH CountTaskUserLink NO-LOCK
            WHERE CountTaskUserLink.CountTaskID = CountTask.CountTaskID
            AND   CountTaskUserLink.Completed = "",
            EACH GateUser OF CountTaskUserLink NO-LOCK:
      
            CountTaskUserLinkBrowse:startRow(CountTaskUserLink.CountTaskUserLinkID, "selectCountTaskUserLinkRow(this," + '"' 
                                                                           + STRING(CountTaskUserLink.CountTaskUserLinkID) + '"' + ");", "").
            CountTaskUserLinkBrowse:insertData(CountTaskUserLink.CountTaskUserLinkID).
            
            /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
            {webGetOptionalBrowseFields.i CountTaskUserLink}
            
            CountTaskUserLinkBrowse:insertData(CountTaskUserLink.CountTaskID).
            CountTaskUserLinkBrowse:insertData(STRING(GateUser.FullName),"left").
            
            /* Add hidden fields */
            CountTaskUserLinkBrowse:insertHiddenData("CountTaskVersionID",CountTask.VersionID).
            CountTaskUserLinkBrowse:insertHiddenData("CountTaskUserLinkVersionID",CountTaskUserLink.VersionID).
            
            CountTaskUserLinkBrowse:endRow().
            
         END. /* FOR EACH CountGroup NO-LOCK */
      END.
      ELSE /* Display all users ever assigned of completed task */
      DO:
         logTaskCompleted = YES.
         FOR EACH CountTaskUserLink NO-LOCK
            WHERE CountTaskUserLink.CountTaskID = CountTask.CountTaskID,
            EACH GateUser OF CountTaskUserLink NO-LOCK
            GROUP BY CountTaskUserLink.GateUserID:
         
            IF FIRST-OF(CountTaskUserLink.GateUserID) THEN
            DO:
               CountTaskUserLinkBrowse:startRow(CountTaskUserLink.CountTaskUserLinkID, "", "").
               CountTaskUserLinkBrowse:insertData(CountTaskUserLink.CountTaskUserLinkID).
               
               /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
               {webGetOptionalBrowseFields.i CountTaskUserLink}
               
               CountTaskUserLinkBrowse:insertData(CountTaskUserLink.CountTaskID).
               CountTaskUserLinkBrowse:insertData(STRING(GateUser.FullName),"left").
               
               /* Add hidden fields */
               CountTaskUserLinkBrowse:insertHiddenData("CountTaskVersionID",CountTask.VersionID).
               CountTaskUserLinkBrowse:insertHiddenData("CountTaskUserLinkVersionID",CountTaskUserLink.VersionID).
               
               CountTaskUserLinkBrowse:endRow().
            END.

         END. /* FOR EACH CountGroup NO-LOCK */
      END.

   END.
   CountTaskUserLinkBrowse:endTable().
   chrPageBuildError = chrPageBuildError + CountTaskUserLinkBrowse:getErrors().
   
   /* Hidden Fields */
   
   CountTaskUserLinkBrowseForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskUserLinkBrowseForm:insertHiddenField("CountTaskUserLinkID","").
   CountTaskUserLinkBrowseForm:insertHiddenField("GateUserID","").
   CountTaskUserLinkBrowseForm:insertHiddenField("CountTaskUserLinkVersionID","").
   CountTaskUserLinkBrowseForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskUserLinkBrowseForm:insertHiddenField("counttaskuserlink_browse_scroll","").
   CountTaskUserLinkBrowseForm:insertHiddenField("popup_counttaskuserlink_browse","").
   CountTaskUserLinkBrowseForm:insertHiddenField("popup_addcounttaskuserlink_browse","").
   CountTaskUserLinkBrowseForm:insertHiddenField("popup_counttaskuserhistory_browse","").
   /*CountTaskUserLinkBrowseForm:insertHiddenField("filtering", "yes").*/
   CountTaskUserLinkBrowseForm:insertHiddenField("filtering", "no").
   CountTaskUserLinkBrowseForm:insertHiddenField("form_name","counttaskuserlink_browse_form").
   CountTaskUserLinkBrowseForm:insertHiddenField("prog_name","adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskUserLinkBrowseForm}
   
   /* Create Button Bar */
   CountTaskUserLinkBrowseButtons = NEW buttonBar().

   CountTaskUserLinkBrowseButtons:addButton("counttaskuserlink_browse_form_btn_adduser",
                                            fTL("Add User"),
                                            "displayUserList('counttaskuserlink_browse_form');",
                                            (IF logTaskCompleted = FALSE THEN "" ELSE "Disabled")).

   CountTaskUserLinkBrowseButtons:addButton("counttaskuserlink_browse_form_btn_deactivateuser",
                                            fTL("Deactivate Link"),
                                            "deactivateCountTaskUserLink('counttaskuserlink_browse_form');",
                                            (IF logTaskCompleted = FALSE AND intSelectedCountTaskUserLink > 0 THEN "" ELSE "Disabled")).
   
   CountTaskUserLinkBrowseButtons:addButton("counttaskuserlink_browse_form_btn_cancel",
                                            fTL("Cancel"),
                                            "disablePopup('counttaskuserlink_browse_form_popup');").

   CountTaskUserLinkBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskUserLinkBrowseForm:FormBrowse  = CountTaskUserLinkBrowse.
   CountTaskUserLinkBrowseForm:FormButtons = CountTaskUserLinkBrowseButtons.
   CountTaskUserLinkBrowseForm:endForm(). 
   
   CountTaskUserLinkBrowseForm:displayForm().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCountTaskUserLinkDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCountTaskUserLinkDetails Procedure 
PROCEDURE pCountTaskUserLinkDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "counttaskuserlink_details_form"}
   
   ASSIGN chrDisplayFieldList  = "GateUserID"
          chrEditFieldList     = "GateUserID"
          chrNewFieldList      = "GateUserID"
          chrRequiredFieldList = "GateUserID"
          chrExtraFieldList    = ""
          chrValidateFieldList = "GateUserID:INTEGER".
   
   CountTaskUserLinkDetailsForm           = NEW dataForm("counttaskuserlink_details_form").
   CountTaskUserLinkDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   CountTaskUserLinkDetailsForm:FormAction = "dbCountTaskUserLinkUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   CountTaskUserLinkDetailsForm:FormWidth  = 440.
   CountTaskUserLinkDetailsForm:FormHeight = 300.
   CountTaskUserLinkDetailsForm:FormTitle  = "Add Count Task User".
   CountTaskUserLinkDetailsForm:FormType   = "medium".
   
   /* Column Layout */
   CountTaskUserLinkDetailsForm:insertPaddingColumn(40).
   CountTaskUserLinkDetailsForm:insertColumn(80).
   CountTaskUserLinkDetailsForm:insertColumn(260).
   CountTaskUserLinkDetailsForm:insertColumn(80).

   /* Fields */
   CountTaskUserLinkDetailsForm:startRow().
   CountTaskUserLinkDetailsForm:insertLabel("User").
   CountTaskUserLinkDetailsForm:insertComboField("GateUserID", "", 110, TRUE).  
   CountTaskUserLinkDetailsForm:insertComboPairs("GateUserID","0","Any User").
   FIND FIRST Environment NO-LOCK
      WHERE Environment.EnvironmentCode = chrGblEnvironment NO-ERROR.
   IF AVAILABLE Environment THEN
   DO:

      chrUserIDList = fGetUsersForApplication(UserSession.ApplicationID,Environment.EnvironmentID).

      DO intLoop = 1 TO NUM-ENTRIES(chrUserIDList):
         FIND FIRST GateUser NO-LOCK 
            WHERE GateUser.GateUserID = INTEGER(ENTRY(intLoop,chrUserIDList)) NO-ERROR.
         IF AVAILABLE GateUser THEN
         DO:
            CountTaskUserLinkDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).
         END.
      END.

   END. /* AVAILABLE Environment */

   {webGetOptionalFormFields.i pCountGroupDetailsFields}
   
   /* Add Hidden Fields*/
   CountTaskUserLinkDetailsForm:insertHiddenField("counttask_browse_scroll", "").
   CountTaskUserLinkDetailsForm:insertHiddenField("counttaskuserlink_browse_scroll","").
   CountTaskUserLinkDetailsForm:insertHiddenField("CountTaskID",chrCountTaskID).
   CountTaskUserLinkDetailsForm:insertHiddenField("CountTaskUserLinkID","").
   CountTaskUserLinkDetailsForm:insertHiddenField("CountTaskUserLinkVersionID","").
   CountTaskUserLinkDetailsForm:insertHiddenField("counttask_browse_scroll","").
   CountTaskUserLinkDetailsForm:insertHiddenField("counttaskuserlink_browse_scroll","").
   CountTaskUserLinkDetailsForm:insertHiddenField("popup_counttaskuserlink_browse","").
   CountTaskUserLinkDetailsForm:insertHiddenField("form_name", "counttaskuserlink_details_form").
   CountTaskUserLinkDetailsForm:insertHiddenField("prog_name", "adCountTaskAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i CountTaskUserLinkDetailsForm}
   
   /* Create Button Bar */
   CountTaskUserLinkDetailsButtons = NEW buttonBar().
   
   CountTaskUserLinkDetailsButtons:addButton("counttaskuserlink_details_form_btn_save", 
                                             fTL("Add"), 
                                             "addCountTaskUserLink('counttaskuserlink_browse_form');").

   CountTaskUserLinkDetailsButtons:addButton("counttaskuserlink_details_form_btn_cancel", 
                                             fTL("Cancel"), 
                                             "cancelUpdate('UserCancelled','process_mode'); disablePopup('counttaskuserlink_details_form_popup');").
   
   CountTaskUserLinkDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   CountTaskUserLinkDetailsForm:FormButtons = CountTaskUserLinkDetailsButtons.
   
   CountTaskUserLinkDetailsForm:endForm(). 
   
   CountTaskUserLinkDetailsForm:displayForm(). 
   
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
   
   ASSIGN chrCountTaskID                         = get-value("CountTaskID")
          intSelectedCountTask                   = INTEGER(chrCountTaskID)
          chrScrollToCountTaskRow                = STRING(INTEGER(get-value("counttask_browse_scroll"))) + ";"
          chrCountTaskLocationID                 = get-value("CountTaskLocationID")
          intSelectedCountTaskLocation           = INTEGER(chrCountTaskLocationID)
          chrCountTaskUserLinkID                 = get-value("CountTaskUserLinkID")
          intSelectedCountTaskUserLink           = INTEGER(chrCountTaskUserLinkID)
          chrScrollToCountTaskLocationRow        = STRING(INTEGER(get-value("counttasklocation_browse_scroll"))) + ";"
          chrCountTaskLocationPartID             = get-value("CountTaskLocationPartID")
          intSelectedCountTaskLocationPart       = INTEGER(chrCountTaskLocationPartID)
          chrScrollToCountTaskLocationPartRow    = STRING(INTEGER(get-value("counttasklocationpart_browse_scroll"))) + ";"
          chrCountTaskLocationPackageID          = get-value("CountTaskLocationPackageID")
          intSelectedCountTaskLocationPackage    = INTEGER(chrCountTaskLocationPackageID)
          chrScrollToCountTaskLocationPackageRow = STRING(INTEGER(get-value("counttasklocationpackage_browse_scroll"))) + ";"
          chrSelectedTaskType                    = get-value("TaskType")
          chrSelectedButtonMode                  = get-value("ButtonMode")
          logFilterIsPoppedUp                    = (get-value('filtering') <> "no" AND get-value('filtering') <> "")
          intSelectedCountTaskStatusID           = INTEGER(get-value("CountTaskStatusID"))
          chrHistoricDate                        = get-value("HistoricDate")
          chrHistoricHour                        = get-value("HistoricHour")
          chrHistoricMins                        = get-value("HistoricMins")
          intHistoryDays                         = INTEGER(get-value("HistoryDays"))
          chrCurrentTime                         = REPLACE(STRING(TIME, "HH:MM:SS"), ":", "").
   
   /* Process URL values */
   IF chrHistoricDate <> "" THEN
      ASSIGN chrToTimestamp   = fDateToTimestamp(DATE(chrHistoricDate)) + chrHistoricHour + chrHistoricMins + "59999"
             chrFromTimestamp = fOpeningTimestamp(DATE(chrHistoricDate) - intHistoryDays).
   ELSE
      ASSIGN chrToTimestamp = fDateToTimestamp(TODAY) + chrCurrentTime + "999"
             chrFromTimestamp = fOpeningTimestamp(TODAY - intHistoryDays).

   IF logFilterIsPoppedUp THEN
     chrPopupFilters = 'viewCountTaskFilter("counttask_filter_form");'.   
   
   /* Process URL values */
   IF chrCountTaskID <> "" THEN
      chrSelectCountTaskRow = 'selectCountTaskRow(document.getElementById("counttask_browse_row_' + chrCountTaskID + '"),"' 
                                                         + chrCountTaskID +  '");'.

   IF get-value('popup_counttaskuserlink_browse') = "yes" THEN DO:
      chrPopupCountTaskUserLinks = 'enablePopup("counttaskuserlink_browse_form_popup");'.
      IF chrCountTaskUserLinkID <> "" THEN
         chrSelectCountTaskUserLinkRow = 'selectCountTaskUserLinkRow(document.getElementById("counttaskuserlink_browse_row_' + chrCountTaskUserLinkID + '"),"'
                                                                 + chrCountTaskUserLinkID + '");'.
   END. 

   IF chrCountTaskLocationID <> "" THEN
      chrSelectCountTaskLocationRow = 'selectCountTaskLocationRow(document.getElementById("counttasklocation_browse_row_' + chrCountTaskLocationID + '"),"'
                                                              + chrCountTaskLocationID + '");'.
   
   IF chrCountTaskLocationPartID <> "" THEN
      chrSelectCountTaskLocationPartRow = 'selectCountTaskLocationPartRow(document.getElementById("counttasklocationpart_browse_row_' + chrCountTaskLocationPartID + '"),"'
                                                              + chrCountTaskLocationPartID + '");'.
   IF chrCountTaskLocationPackageID <> "" THEN
      chrSelectCountTaskLocationPackageRow = 'selectCountTaskLocationPackageRow(document.getElementById("counttasklocationpackage_browse_row_' + chrCountTaskLocationPackageID + '"),"'
                                                              + chrCountTaskLocationPackageID + '");'.
   /* Popups */
   IF get-value('popup_counttasklocation_browse') = "yes" THEN
      chrPopupCountTaskLocations = 'enablePopup("counttasklocation_browse_form_popup");'.

   IF get-value('popup_counttasklocationpart_browse') = "yes" THEN
      chrPopupCountTaskLocationParts = 'enablePopup("counttasklocationpart_browse_form_popup");'.

   IF get-value('popup_counttasklocationpackage_browse') = "yes" THEN
      chrPopupCountTaskLocationPackages = 'enablePopup("counttasklocationpackage_browse_form_popup");'.

   IF get-value('popup_counttaskhistory_browse') = "yes" THEN
      chrPopupHistory = 'enablePopup("counttaskhistory_browse_form_popup");'.

   IF get-value('popup_counttasklocationhistory_browse') = "yes" THEN
      chrCountTaskLocationHistoryPopup = 'enablePopup("counttasklocationhistory_browse_form_popup");'.

   IF get-value('popup_counttasklocationparthistory_browse') = "yes" THEN
      chrCountTaskLocationPartHistoryPopup = 'enablePopup("counttasklocationparthistory_browse_form_popup");'.

   IF get-value('popup_counttasklocationpackagehistory_browse') = "yes" THEN
      chrCountTaskLocationPackageHistoryPopup = 'enablePopup("counttasklocationpackagehistory_browse_form_popup");'.

   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("counttask_browse").scrollTop=' + chrScrollToCountTaskRow + chrSelectCountTaskRow
                             + chrPopupCountTaskUserLinks
                             + chrScrollToCountTaskUserLinkRow + chrSelectCountTaskUserLinkRow
                             + chrPopupCountTaskLocations
                             + 'document.getElementById("counttasklocation_browse").scrollTop=' + chrScrollToCountTaskLocationRow + chrSelectCountTaskLocationRow
                             + chrPopupCountTaskLocationParts
                             + 'document.getElementById("counttasklocationpart_browse").scrollTop=' + chrScrollToCountTaskLocationPartRow + chrSelectCountTaskLocationPartRow
                             + chrPopupCountTaskLocationPackages
                             + 'document.getElementById("counttasklocationpackage_browse").scrollTop=' + chrScrollToCountTaskLocationPackageRow + chrSelectCountTaskLocationPackageRow
                             + chrPopupFilters
                             + chrPopupHistory
                             + chrCountTaskLocationHistoryPopup
                             + chrCountTaskLocationPartHistoryPopup
                             + chrCountTaskLocationPackageHistoryPopup.

   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Count Task Admin".
   ThisPage:FrameTitle    = "Count Task Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("counttask.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pCountTaskBrowse.
   
   FIND FIRST CountTask NO-LOCK
      WHERE CountTask.CountTaskID = intSelectedCountTask NO-ERROR.
   
   IF AVAILABLE CountTask THEN
      FIND FIRST CountTaskType OF CountTask NO-LOCK NO-ERROR.

   /******* Pop-up Browsers and Forms ********/    
   RUN pCountTaskDetails.
   
   RUN pCountTaskHistoryBrowse.

   RUN pCountTaskUserLinkBrowse.

   RUN pCountTaskUserLinkDetails.

   RUN pCountTaskLocationBrowse.
   
   FIND FIRST CountTaskLocation NO-LOCK
      WHERE CountTaskLocation.CountTaskLocationID = intSelectedCountTaskLocation NO-ERROR.

   RUN pCountTaskLocationDetails.

   RUN pCountTaskLocationHistoryBrowse.

   RUN pCountTaskFilter.

   RUN pCountTaskLocationPartBrowse.

   FIND FIRST CountTaskLocationPart NO-LOCK
      WHERE CountTaskLocationPart.CountTaskLocationPartID = intSelectedCountTaskLocationPart NO-ERROR.
   
   RUN pCountTaskLocationPartDetails.

   RUN pCountTaskLocationPartHistoryBrowse.
   
   RUN pCountTaskLocationPackageBrowse.

   FIND FIRST CountTaskLocationPackage NO-LOCK
      WHERE CountTaskLocationPackage.CountTaskLocationPackageID = intSelectedCountTaskLocationPackage NO-ERROR.
   
   RUN pCountTaskLocationPackageDetails.

   RUN pCountTaskLocationPackageHistoryBrowse.

   /* Writes the HTML for 2 pop-up Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display pop-up errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects defined locally */
   DELETE OBJECT CountTaskBrowseFrame                   NO-ERROR.
   DELETE OBJECT CountTaskBrowse                        NO-ERROR.
   DELETE OBJECT CountTaskBrowseButtons                 NO-ERROR.
   DELETE OBJECT CountTaskDetailsForm                   NO-ERROR.
   DELETE OBJECT CountTaskDetailsButtons                NO-ERROR.

   DELETE OBJECT CountTaskUserLinkBrowseFrame           NO-ERROR.
   DELETE OBJECT CountTaskUserLinkBrowseForm            NO-ERROR.
   DELETE OBJECT CountTaskUserLinkBrowse                NO-ERROR.
   DELETE OBJECT CountTaskUserLinkBrowseButtons         NO-ERROR.
      
   DELETE OBJECT CountTaskLocationBrowseFrame           NO-ERROR.
   DELETE OBJECT CountTaskLocationBrowseForm            NO-ERROR.
   DELETE OBJECT CountTaskLocationBrowse                NO-ERROR.
   DELETE OBJECT CountTaskLocationBrowseButtons         NO-ERROR.
   DELETE OBJECT CountTaskLocationDetailsForm           NO-ERROR.
   DELETE OBJECT CountTaskLocationDetailsButtons        NO-ERROR.
   
   DELETE OBJECT CountTaskLocationPartBrowseFrame           NO-ERROR.
   DELETE OBJECT CountTaskLocationPartBrowseForm            NO-ERROR.
   DELETE OBJECT CountTaskLocationPartBrowse                NO-ERROR.
   DELETE OBJECT CountTaskLocationPartBrowseButtons         NO-ERROR.
   DELETE OBJECT CountTaskLocationPartDetailsForm           NO-ERROR.
   DELETE OBJECT CountTaskLocationPartDetailsButtons        NO-ERROR.
   
   DELETE OBJECT CountTaskLocationPackageBrowseFrame           NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageBrowseForm            NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageBrowse                NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageBrowseButtons         NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageDetailsForm           NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageDetailsButtons        NO-ERROR.


   DELETE OBJECT CountTaskBrowseFilterForm              NO-ERROR.
   DELETE OBJECT CountTaskBrowseFilterButtons           NO-ERROR.

   DELETE OBJECT CountTaskHistoryBrowseFrame            NO-ERROR.
   DELETE OBJECT CountTaskHistoryBrowseForm             NO-ERROR.
   DELETE OBJECT CountTaskHistoryBrowse                 NO-ERROR.
   DELETE OBJECT CountTaskHistoryBrowseButtons          NO-ERROR.

   DELETE OBJECT CountTaskLocationHistoryBrowseFrame    NO-ERROR.
   DELETE OBJECT CountTaskLocationHistoryBrowseForm     NO-ERROR.
   DELETE OBJECT CountTaskLocationHistoryBrowse         NO-ERROR.
   DELETE OBJECT CountTaskLocationHistoryBrowseButtons  NO-ERROR.
   DELETE OBJECT CountTaskLocationHistoryDetailsForm    NO-ERROR.
   DELETE OBJECT CountTaskLocationHistoryDetailsButtons NO-ERROR.

   DELETE OBJECT CountTaskLocationPartHistBrowseFrame    NO-ERROR.
   DELETE OBJECT CountTaskLocationPartHistBrowseForm     NO-ERROR.
   DELETE OBJECT CountTaskLocationPartHistBrowse         NO-ERROR.
   DELETE OBJECT CountTaskLocationPartHistBrowseButtons  NO-ERROR.
   DELETE OBJECT CountTaskLocationPartHistDetailsForm    NO-ERROR.
   DELETE OBJECT CountTaskLocationPartHistDetailsButtons NO-ERROR.
   
   DELETE OBJECT CountTaskLocationPackageHistBrowseFrame    NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageHistBrowseForm     NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageHistBrowse         NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageHistBrowseButtons  NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageHistDetailsForm    NO-ERROR.
   DELETE OBJECT CountTaskLocationPackageHistDetailsButtons NO-ERROR.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetCountTaskRows) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetCountTaskRows Procedure 
PROCEDURE pSetCountTaskRows :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   CountTaskBrowse:startRow(CountTask.CountTaskID, "selectCountTaskRow(this," + '"' 
                                                                         + STRING(CountTask.CountTaskID) + '"' + ");", "").
   CountTaskBrowse:insertData(CountTask.CountTaskID).

   /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
   {webGetOptionalBrowseFields.i CountTask}

   CountTaskBrowse:insertData(IF AVAILABLE CountGroup THEN CountGroup.CountGroupCode ELSE "", "left").
   CountTaskBrowse:insertData(IF AVAILABLE CountGroupType THEN CountGroupType.TypeCode ELSE "", "left").
   CountTaskBrowse:insertData(IF AVAILABLE CountTaskType THEN CountTaskType.TypeCode ELSE "","left").                                                                                               
   CountTaskBrowse:insertData(STRING(CountTask.BlindCount, "Yes/No")).
   CountTaskBrowse:insertData(STRING(CountTask.Priority)).
   CountTaskBrowse:insertData(STRING(CountTask.CountGroupRecountID)).
   CountTaskBrowse:insertData(IF AVAILABLE BusinessUnit THEN BusinessUnit.UnitName ELSE "","left").
   CountTaskBrowse:insertData(IF AVAILABLE CountTaskStatus THEN CountTaskStatus.StatusCode ELSE "","left").
   CountTaskBrowse:insertData(fDisplayDate&Time(CountTask.Completed,"y/m/d H:M:S"), "right").
   CountTaskBrowse:insertImage("view", "View Change History", 'viewCountTaskHistoryBrowse("' 
                                 + STRING(CountTask.CountTaskID) + '", this.parentNode);').
   /* Add hidden fields */
   CountTaskBrowse:insertHiddenData("CountTaskVersionID",CountTask.VersionID).

   IF CAN-FIND(FIRST CountTaskType OF CountTask NO-LOCK
               WHERE CountTaskType.TypeCode = "PartScan") THEN
      CountTaskBrowse:insertHiddenData("TaskType","PartScan").
   ELSE
      CountTaskBrowse:insertHiddenData("TaskType","PackageScan").

   CountTaskBrowse:endRow().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

