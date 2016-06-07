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
     
  Changes: 02/11/2015  ML  Adjusted logic for filtered action point.

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
{fncLoggingFunctions.i}
{fncStatusTypeFunctions.i}

/* Logging */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{defWebDefinitions.i}

DEFINE VARIABLE intSelectedQualityInspection            AS INTEGER   NO-UNDO.
DEFINE VARIABLE intSelectedQualityInspectionFileLink    AS INTEGER   NO-UNDO.
DEFINE VARIABLE intDisplayCount                         AS INTEGER   NO-UNDO.
DEFINE VARIABLE intAvailableStatus                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE intFilteredRecords                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrSelectQualityInspectionRow           AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToQualityInspectionRow         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrQualityInspectionID                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSelectQualityInspectionFileLinkRow   AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrScrollToQualityInspectionFileLinkRow AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrQualityInspectionFileLinkID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredQualityInspectionRef         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredOrderRef                     AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredLocationRef                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredStatus                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrFilteredRecords                      AS CHARACTER NO-UNDO INITIAL 500.
DEFINE VARIABLE chrSelectedDate                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrPopupQualityInspectionFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE logFiltering                            AS LOGICAL   NO-UNDO.

/* Definitions for System Options for Receiving */
{getReceivingOptions.i}

/* Objects */
/*DEFINE BUFFER bufQualityInspection FOR QualityInspection.*/

/* Main Browse */
DEFINE VARIABLE QualityInspectionBrowseFrame             AS pageFrame.
DEFINE VARIABLE QualityInspectionBrowse                  AS browseTable.
DEFINE VARIABLE QualityInspectionBrowseButtons           AS buttonBar.
DEFINE VARIABLE QualityInspectionDetailsForm             AS dataForm.
DEFINE VARIABLE QualityInspectionDetailsButtons          AS buttonBar.

/* Filter */
DEFINE VARIABLE QualityInspectionFilterForm              AS dataForm.
DEFINE VARIABLE QualityInspectionFilterButtons           AS buttonBar.

DEFINE VARIABLE QualityInspectionFileBrowseForm            AS dataForm.   
DEFINE VARIABLE QualityInspectionFileBrowse                AS browseTable.
DEFINE VARIABLE QualityInspectionFileBrowseButtons         AS buttonBar.

DEFINE VARIABLE QualityInspectionFileDetailsForm           AS dataForm.
DEFINE VARIABLE QualityInspectionFileDetailsButtons        AS buttonBar.

DEFINE VARIABLE QualityInspectionFileHistoryBrowseForm     AS dataForm.   
DEFINE VARIABLE QualityInspectionFileHistoryBrowse         AS browseTable.
DEFINE VARIABLE QualityInspectionFileHistoryBrowseButtons  AS buttonBar.

DEFINE STREAM strLog.


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
         HEIGHT             = 19.43
         WIDTH              = 67.4.
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-pInboundBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundBrowse Procedure 
PROCEDURE pQualityInspectionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   {webGetWebForm.i "qualityinspection_browse_form"}
   
   QualityInspectionBrowse = NEW browseTable("qualityinspection_browse").
   QualityInspectionBrowse:BrowseWidth  = 965.
   QualityInspectionBrowse:BrowseHeight = 455.
   QualityInspectionBrowse:ExcelExport  = TRUE.
   QualityInspectionBrowse:SessionID    = intGblSessionID.
   QualityInspectionBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   QualityInspectionBrowse:insertColumn(fTL("ID"), 50, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
/*   {webGetOptionalBrowseHeaders.i qualityinspection}*/
   
   QualityInspectionBrowse:insertColumn(fTL("KittingLine"),  150, "CHARACTER", "left", FALSE).
   QualityInspectionBrowse:insertColumn(fTL("HourOfDay"),     120, "CHARACTER", "left", FALSE).
   QualityInspectionBrowse:insertColumn(fTL("QualityStation"),  150, "CHARACTER", "left", FALSE).
   QualityInspectionBrowse:insertColumn(fTL("StockPackage"),    125, "CHARACTER", "left", FALSE).
   QualityInspectionBrowse:insertColumn(fTL("Units Inspected"), 100, "INTEGER", "left", FALSE).
   QualityInspectionBrowse:insertColumn(fTL("User"),    125, "CHARACTER", "left", FALSE).
   QualityInspectionBrowse:insertColumn(fTL("Completed"),    125, "DATE", "left", FALSE).
   /*Body*/
   QualityInspectionBrowse:startBody().
   
/*   intAvailableStatus = fGetStatusID("QualityInspection","Available").                                                                 */
/*   ASSIGN intFilteredRecords = INTEGER(chrFilteredRecords) NO-ERROR.                                                        */
/*   IF ERROR-STATUS:ERROR THEN                                                                                               */
/*   DO:                                                                                                                      */
/*      intFilteredRecords = 0.                                                                                               */
/*   END.                                                                                                                     */
/*   intDisplayCount = 0.                                                                                                     */
/*   IF logFiltering THEN                                                                                                     */
/*   DO:                                                                                                                      */
/*      FilterLoop:                                                                                                           */
/*      FOR EACH QualityInspection NO-LOCK:                                                                                              */
/*         IF intDisplayCount >= intFilteredRecords THEN LEAVE FilterLoop.                                                    */
/*         FIND FIRST QualityInspectionLineJourney NO-LOCK                                                                               */
/*            WHERE QualityInspectionLineJourney.QualityInspectionID = QualityInspection.QualityInspectionID AND QualityInspectionLineJourney.Completed = "" NO-ERROR.               */
/*         FIND FIRST TaskLineWorkQualityInspectionLink WHERE TaskLineWorkQualityInspectionLink.QualityInspectionID =  QualityInspection.QualityInspectionID                         */
/*                                           AND TaskLineWorkQualityInspectionLink.Completed = "" NO-LOCK NO-ERROR.                      */
/*         FIND FIRST TaskLineWork WHERE TaskLineWork.TaskLineWorkID = TaskLineWorkQualityInspectionLink.TaskLineWorkID NO-LOCK NO-ERROR.*/
/*         FIND FIRST Shiporder WHERE ShipOrder.ShipOrderID = TaskLineWork.ShipOrderID NO-LOCK NO-ERROR.                      */
/*         FIND FIRST Location   WHERE Location.LocationID = QualityInspection.LocationID NO-LOCK NO-ERROR.                              */
/*         FIND FIRST QualityInspectionStatus WHERE QualityInspectionStatus.QualityInspectionStatusID = QualityInspection.QualityInspectionStatusID NO-LOCK NO-ERROR.                */
/*                                                                                                                            */
/*         IF chrFilteredQualityInspectionRef <> "" AND QualityInspection.QualityInspectionRef <> chrFilteredQualityInspectionRef THEN                                    */
/*            NEXT FilterLoop.                                                                                                */
/*                                                                                                                            */
/*         IF AVAILABLE ShipOrder THEN                                                                                        */
/*         DO:                                                                                                                */
/*            IF chrFilteredOrderRef <> "" AND ShipOrder.OrderRef <> chrFilteredOrderRef THEN                                 */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*         ELSE DO:                                                                                                           */
/*            IF chrFilteredOrderRef <> "" THEN                                                                               */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*                                                                                                                            */
/*         IF AVAILABLE Location THEN                                                                                         */
/*         DO:                                                                                                                */
/*            IF chrFilteredLocationRef <> "" AND Location.LocationRef <> chrFilteredLocationRef THEN                         */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*         ELSE DO:                                                                                                           */
/*            IF chrFilteredLocationRef <> "" THEN                                                                            */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*                                                                                                                            */
/*         IF AVAILABLE QualityInspectionStatus THEN                                                                                     */
/*         DO:                                                                                                                */
/*            IF chrFilteredStatus <> "" AND QualityInspectionStatus.StatusName <> chrFilteredStatus THEN                                */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*         ELSE DO:                                                                                                           */
/*            IF chrFilteredStatus <> "" THEN                                                                                 */
/*               NEXT FilterLoop.                                                                                             */
/*         END.                                                                                                               */
/*                                                                                                                            */
/*         RUN pSetQualityInspectionRow.                                                                                                 */
/*         intDisplayCount = intDisplayCount + 1.                                                                             */
/*      END.                                                                                                                  */
/*   END. /*IF logFiltering THEN*/                                                                                            */
/*   ELSE                                                                                                                     */
/*   DO:                                                                                                                      */
/*      UnfilteredLoop:                                                                                                       */
      FOR EACH QualityInspection  NO-LOCK:
/*         IF intDisplayCount >= intFilteredRecords THEN LEAVE UnfilteredLoop.*/
/*         FIND FIRST QualityInspectionType WHERE QualityInspectionType.QualityInspectionTypeID = QualityInspection.QualityInspectionTypeID NO-LOCK NO-ERROR.*/
/*         IF QualityInspectionType.ReUsable = YES THEN                                                                                                      */
/*         DO:                                                                                                                                               */
            RUN pSetQualityInspectionRow.
/*            intDisplayCount = intDisplayCount + 1.*/
/*         END.*/
      END.
/*   END.*/
   
   QualityInspectionBrowse:endTable().
   
   /* Create a new frame */
   QualityInspectionBrowseFrame = NEW pageFrame().
   QualityInspectionBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   QualityInspectionBrowseFrame:formOpen("qualityinspection_browse_form").
   
   /* Start the Frame Header */
   QualityInspectionBrowseFrame:insertSpacer(5).
   QualityInspectionBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   QualityInspectionBrowse:displayBrowse().  
   
   /* This creates an Excel File */
   /* No point in adding this as there's no room for a button to display it */
   /*InboundBrowse:generateExcel(intGblSessionID, "inbound_browse").  */

   /* End the Frame Header */
   QualityInspectionBrowseFrame:frameClose().
   QualityInspectionBrowseFrame:insertSpacer(10).
   
   QualityInspectionBrowseFrame:insertHiddenField("qualityinspection_browse_scroll","").
   QualityInspectionBrowseFrame:insertHiddenField("qualityinspectionfile_browse_scroll","").
   QualityInspectionBrowseFrame:insertHiddenField("QualityInspectionID",chrQualityInspectionID).
   QualityInspectionBrowseFrame:insertHiddenField("FilteredQualityInspectionRef", chrFilteredQualityInspectionRef).
   QualityInspectionBrowseFrame:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   QualityInspectionBrowseFrame:insertHiddenField("FilteredLocationRef", chrFilteredLocationRef).
   QualityInspectionBrowseFrame:insertHiddenField("FilteredStatus", chrFilteredStatus).
   QualityInspectionBrowseFrame:insertHiddenField("filtering", STRING(logFiltering)).
   QualityInspectionBrowseFrame:insertHiddenField("popup_qualityinspectionfile_browse","").
  
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityInspectionBrowseFrame}
   
   QualityInspectionBrowseFrame:formClose().
   
   /* Create Button Bar */
   QualityInspectionBrowseButtons = NEW buttonBar().
   QualityInspectionBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   QualityInspectionBrowseButtons:addButton("qualityinspection_browse_form_btn_filter",
                                  fTL("Filter"),
                                  "viewQualityInspectionFilter('qualityinspection_filter_form');").
                                  
   QualityInspectionBrowseButtons:addButton("qualityinspection_browse_form_btn_files",
                                  fTL("Files"),
                                  "viewQualityInspectionFileBrowse();",
                                  "Disabled").                                
                                  
   QualityInspectionBrowseButtons:addButton("qualityinspection_browse_form_btn_excel",
                                  fTL("Excel Export"),
                                  "excelExport('" + STRING(intGblSessionID) + "_qualityinspection_browse.xml')").

   QualityInspectionBrowseButtons:closeBar().  
   QualityInspectionBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-pInboundDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundDetailsFields Procedure 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pQualityInspectionFileLinkBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQualityInspectionFileLinkBrowse Procedure
PROCEDURE pQualityInspectionFileBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   {webGetWebForm.i "qualityinspectionfile_browse_form"}
   
   QualityInspectionFileBrowseForm = NEW dataForm("qualityinspectionfile_browse_form").
   QualityInspectionFileBrowseForm:WebStream = STREAM WebStream:HANDLE.

   FIND FIRST QualityInspection NO-LOCK /* idx=QualityInspectionID */
      WHERE QualityInspection.QualityInspectionID = intSelectedQualityInspection NO-ERROR.
      
   /* Form Setup */
   QualityInspectionFileBrowseForm:FormWidth  = 580.
   QualityInspectionFileBrowseForm:FormHeight = 420.
   QualityInspectionFileBrowseForm:FormType   = "large".
   QualityInspectionFileBrowseForm:FormTitle   = fTL("Files for QualityInspectionID:  ") + (IF AVAILABLE QualityInspection THEN  
                                                                  STRING(QualityInspection.QualityInspectionID) ELSE "Files").                                   

   /* Browse Setup */
   QualityInspectionFileBrowse              = NEW browseTable("qualityinspectionfile_browse").
   QualityInspectionFileBrowse:BrowseWidth  = 560.
   QualityInspectionFileBrowse:BrowseHeight = 375.
   QualityInspectionFileBrowse:WebStream    = STREAM WebStream:HANDLE.
   
   QualityInspectionFileBrowse:insertColumn(fTL("FileName"),    150, "CHARACTER", "LEFT", FALSE).
   QualityInspectionFileBrowse:insertColumn(fTL("Uploaded By"), 130, "CHARACTER", FALSE).
   QualityInspectionFileBrowse:insertColumn(fTL("Uploaded"),    110, "CHARACTER", FALSE).
   QualityInspectionFileBrowse:insertColumn(fTL("View File"),    60, "CHARACTER", "LEFT", FALSE).
   
   QualityInspectionFileBrowse:StartBody().
   
   IF AVAILABLE QualityInspection THEN
   DO:
      FOR EACH QualityInspectionFileLink NO-LOCK /* idx=QualityInspectionID */
         WHERE QualityInspectionFileLink.QualityInspectionID = intSelectedQualityInspection
         BY QualityInspectionFileLink.QualityInspectionFileLinkID:

         QualityInspectionFileBrowse:startRow(QualityInspectionFileLink.QualityInspectionFileLinkID, "selectQualityInspectionFileLinkRow(this," + '"' 
                                          + STRING(QualityInspectionFileLink.QualityInspectionFileLinkID) + '"' + ");", "").    
         
         FIND FIRST File NO-LOCK /* idx=FileID */
            WHERE File.FileID = QualityInspectionFileLink.FileID NO-ERROR. 
         
         QualityInspectionFileBrowse:insertData(File.FileName, "left").
         
         IF AVAILABLE File THEN
         DO:
            FIND FIRST GateUser NO-LOCK /* Idx=GateUserID */
               WHERE GateUser.GateUserID = File.GateUserID NO-ERROR.
               
            IF AVAILABLE GateUser THEN 
            DO:
               QualityInspectionFileBrowse:insertData(GateUser.FullName, "left").
            END.
            ELSE
            DO: 
               QualityInspectionFileBrowse:insertData(File.GateUserID, "left").
            END.   
            
               QualityInspectionFileBrowse:insertData(fDisplayDate&Time(File.Completed,"d/m/y H:M"), "left").
         END.
         
         QualityInspectionFileBrowse:insertLink("download", "downloadQualityInspectionFile(" + '"' + TRIM(File.FilePath) + TRIM(File.FileName) + '"' + ")").
         
         /* Add hidden fields */
         QualityInspectionFileBrowse:insertHiddenData("QualityInspectionFileLinkID", QualityInspectionFileLink.QualityInspectionFileLinkID).
         QualityInspectionFileBrowse:insertHiddenData("QualityInspectionFileLinkVersionID", QualityInspectionFileLink.VersionID).
         QualityInspectionFileBrowse:insertHiddenData("QualityInspectionID", QualityInspection.QualityInspectionID).
         QualityInspectionFileBrowse:insertHiddenData("FileID", File.FileID).
         
         QualityInspectionFileBrowse:endRow().
         
      END. /* FOR EACH File OF ReportRun */
   END. /* IF AVAILABLE QualityInspection THEN */
   
   QualityInspectionFileBrowse:endTable().   
   chrPageBuildError = chrPageBuildError + QualityInspectionFileBrowse:getErrors().
 
   /* Hidden Fields */
   QualityInspectionFileBrowseForm:insertHiddenField("qualityinspectionfile_browse_scroll","").
   QualityInspectionFileBrowseForm:insertHiddenField("FileID","").
   QualityInspectionFileBrowseForm:insertHiddenField("QualityInspectionID",chrQualityInspectionID).
   QualityInspectionFileBrowseForm:insertHiddenField("QualityInspectionFileLinkID","").
   QualityInspectionFileBrowseForm:insertHiddenField("QualityInspectionFileLinkVersionID","").
   QualityInspectionFileBrowseForm:insertHiddenField("FilteredQualityInspectionRef", chrFilteredQualityInspectionRef).
   QualityInspectionFileBrowseForm:insertHiddenField("FilteredOrderRef", chrFilteredOrderRef).
   QualityInspectionFileBrowseForm:insertHiddenField("FilteredLocationRef", chrFilteredLocationRef).
   QualityInspectionFileBrowseForm:insertHiddenField("FilteredStatus", chrFilteredStatus).
   QualityInspectionFileBrowseForm:insertHiddenField("filtering", STRING(logFiltering)).
   QualityInspectionFileBrowseForm:insertHiddenField("form_name","qualityinspectionfile_browse_form").
   QualityInspectionFileBrowseForm:insertHiddenField("prog_name","adQualityInspectionEnquiry.p").  
   QualityInspectionFileBrowseForm:insertHiddenField("popup_qualityinspectionfile_browse","").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityInspectionFileBrowseForm}

   /* Create Button Bar */
   QualityInspectionFileBrowseButtons           = NEW buttonBar().
   QualityInspectionFileBrowseButtons:WebStream = STREAM WebStream:HANDLE.

   QualityInspectionFileBrowseButtons:addButton("qualityinspectionfile_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('qualityinspectionfile_browse_form_popup');").
   
   QualityInspectionFileBrowseButtons:closeBar().  

   QualityInspectionFileBrowseForm:FormBrowse  = QualityInspectionFileBrowse.
   QualityInspectionFileBrowseForm:FormButtons = QualityInspectionFileBrowseButtons.

   QualityInspectionFileBrowseForm:endForm(). 
   QualityInspectionFileBrowseForm:displayForm().  

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pInboundFilter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInboundFilter Procedure 
PROCEDURE pQualityInspectionFilter :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

   QualityInspectionFilterForm = NEW dataForm("qualityinspection_filter_form").
   QualityInspectionFilterForm:WebStream  = STREAM WebStream:HANDLE.
   QualityInspectionFilterForm:FormAction = "adQualityInspectionEnquiry.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   QualityInspectionFilterForm:FormWidth   = 350.
   QualityInspectionFilterForm:FormHeight  = 200.
   QualityInspectionFilterForm:FormTitle   = "QualityInspection Filter".
   QualityInspectionFilterForm:FormType    = "small_wide".

   /* Column Layout */
   QualityInspectionFilterForm:insertPaddingColumn(10).
   QualityInspectionFilterForm:insertColumn(100).
   QualityInspectionFilterForm:insertColumn(169).
   QualityInspectionFilterForm:insertColumn(20).
   QualityInspectionFilterForm:insertColumn(2).

   /* Fields */
   QualityInspectionFilterForm:startRow().
   QualityInspectionFilterForm:insertLabel(fTL("Sample Matrix")).
   QualityInspectionFilterForm:insertComboField("SampleMatrix", chrFilteredQualityInspectionRef, 200, TRUE).
   
   QualityInspectionFilterForm:startRow().
   QualityInspectionFilterForm:insertLabel(fTL("Stock Package")).
   QualityInspectionFilterForm:insertTextField("FilteredOrderRef", chrFilteredOrderRef, 200, TRUE).
   
   QualityInspectionFilterForm:startRow().
   QualityInspectionFilterForm:insertLabel(fTL("Kitting Line")).
   QualityInspectionFilterForm:insertComboField("KittingLine", chrFilteredQualityInspectionRef, 200, TRUE).
   
   QualityInspectionFilterForm:startRow().
   QualityInspectionFilterForm:insertLabel(fTL("User")).
   QualityInspectionFilterForm:insertComboField("GateUser", chrFilteredQualityInspectionRef, 200, TRUE).
   
   QualityInspectionFilterForm:startRow().
   QualityInspectionFilterForm:insertLabel(fTL("Date")).
   QualityInspectionFilterForm:insertDateField("FilteredRecords", chrFilteredRecords, 200, TRUE).

   /* Add Hidden Fields*/
   QualityInspectionFilterForm:insertHiddenField("form_name", "qualityinspection_filter_form").
   QualityInspectionFilterForm:insertHiddenField("prog_name", "adQualityInspectionEnquiry.p").
   QualityInspectionFilterForm:insertHiddenField("FilteredQualityInspectionRef", "").
   QualityInspectionFilterForm:insertHiddenField("FilteredOrderRef", "").
   QualityInspectionFilterForm:insertHiddenField("FilteredLocationRef", "").
   QualityInspectionFilterForm:insertHiddenField("FilteredStatus", "").
   QualityInspectionFilterForm:insertHiddenField("FilteredRecords", "").
   QualityInspectionFilterForm:insertHiddenField("filtering", "yes").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i QualityInspectionFilterForm}

   /* Create Button Bar */
   QualityInspectionFilterButtons = NEW buttonBar().

   QualityInspectionFilterButtons:addButton("qualityinspection_filter_form_btn_search",
                                   fTL("Filter"),
                                   "filterQualityInspection('qualityinspection_filter_form');").

   QualityInspectionFilterButtons:closeBar().
   
   /* Assign the Button Bar Object to the Form Object */
   QualityInspectionFilterForm:FormButtons = QualityInspectionFilterButtons.

   QualityInspectionFilterForm:endForm().
   QualityInspectionFilterForm:displayForm().

   /*   chrPageBuildError = chrPageBuildError + InboundFilterForm:getErrors().  */

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
   
   /* Process URL values */
   ASSIGN chrQualityInspectionID                  = get-value("QualityInspectionID")
          intSelectedQualityInspection            = INT(chrQualityInspectionID)
          chrQualityInspectionFileLinkID          = get-value("QualityInspectionFileLinkID")
          intSelectedQualityInspectionFileLink    = INT(chrQualityInspectionFileLinkID)
          chrFilteredQualityInspectionRef         = get-value("FilteredQualityInspectionRef")
          chrFilteredOrderRef                     = get-value("FilteredOrderRef")
          chrFilteredLocationRef                  = get-value("FilteredLocationRef")
          chrFilteredStatus                       = get-value("FilteredStatus")
          chrFilteredRecords                      = (IF get-value("FilteredRecords") <> "" THEN get-value("FilteredRecords") ELSE chrFilteredRecords)
          logFiltering                            = (get-value("filtering") = "yes")
          chrScrollToQualityInspectionFileLinkRow = STRING(INT(get-value("qualityinspectionfile_browse_scroll")))
          chrScrollToQualityInspectionRow         = STRING(INT(get-value("qualityinspection_browse_scroll"))) + ";" NO-ERROR.

   IF chrFilteredQualityInspectionRef     = "" AND
      chrFilteredOrderRef    = "" AND
      chrFilteredLocationRef = "" AND
      chrFilteredStatus      = "" THEN
      ASSIGN logFiltering    = FALSE.

   IF intSelectedQualityInspection > 0 THEN
      chrSelectQualityInspectionRow = 'selectQualityInspectionRow(document.getElementById("qualityinspection_browse_row_' + chrQualityInspectionID + '"),"'
                              + chrQualityInspectionID + '","adQualityInspectionEnquiry.p' + '");'.
                              
   IF chrQualityInspectionFileLinkID <> "" THEN
     chrSelectQualityInspectionFileLinkRow = 'selectQualityInspectionFileLinkRow(document.getElementById("qualityinspectionfile_browse_row_' 
                                       + chrQualityInspectionFileLinkID + '"),"' + chrQualityInspectionFileLinkID +  '");'.                           
                              
   IF get-value('popup_qualityinspectionfile_browse') = "yes" THEN
      chrPopupQualityInspectionFile = 'enablePopup("qualityinspectionfile_browse_form_popup");'.   
      
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("qualityinspection_browse").scrollTop=' + chrScrollToQualityInspectionRow 
                             + chrSelectQualityInspectionRow
                             + chrPopupQualityInspectionFile.
     
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "QualityInspection Enquiry".
   ThisPage:FrameTitle = "QualityInspection Enquiry".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("qualityinspectionenquiry.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
/*   IF logGblDebugging THEN                                                                                                    */
/*      fLog("GblSessionID:" + STRING(intGblSessionID) + " GblUserID:" + STRING(intGblUserID) + " GblUserName:" + chrGblUserName*/
/*                           + " GblLanguageID:" + STRING(intGblLanguageID) + " GblEnvironment:" + chrGblEnvironment).          */
   
   /******* Main Browser ********************/
   RUN pQualityInspectionBrowse.
   RUN pQualityInspectionFilter.
   
   /******* Popup Browsers and Forms ********/    
   FIND FIRST QualityInspection NO-LOCK /* QualityInspectionID */
      WHERE QualityInspection.QualityInspectionID = intSelectedQualityInspection NO-ERROR.
   
   RUN pQualityInspectionFileBrowse.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Errors from an Update program are most important - get and show these first */
   chrFromURLError = get-value("error").
   
   /* This will display errors that came in from an Update program via the URL */
   IF chrFromURLError <> "" THEN
      {&OUT} '<script>systemAlert("message_alert","' + chrFromURLError + '","Error")</script>' SKIP.
   
   /* This will display errors that were generated when building the page */
   IF chrPageBuildError <> "" THEN
      {&OUT} '<script>systemAlert("message_alert","' + chrPageBuildError + '","Error")</script>' SKIP.
   
   IF chrFromURLError = "" THEN
      {&OUT} '<script>footerMessage("' + get-value("return-message") + '");</script>' SKIP.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSetQualityInspectionRow) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetQualityInspectionRow Procedure
PROCEDURE pSetQualityInspectionRow:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        
/*         FIND FIRST KittingLine OF QualityInspection NO-LOCK NO-ERROR.*/
         FIND FIRST QualitySampleLineHour OF QualityInspection NO-LOCK NO-ERROR.
         FIND FIRST QualityStation OF QualityInspection NO-LOCK NO-ERROR.
         FIND FIRST StockPackage OF QualityInspection NO-LOCK NO-ERROR.
         FIND FIRST GateUser WHERE GateUserID = QualityInspection.InspectingGateUserID NO-LOCK NO-ERROR.
         QualityInspectionBrowse:startRow(QualityInspection.QualityInspectionID, "selectQualityInspectionRow(this," + '"' + STRING(QualityInspection.QualityInspectionID) + '"' + ");", "").
         QualityInspectionBrowse:insertData(QualityInspection.QualityInspectionID, "left").


         /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
/*         {webGetOptionalBrowseFields.i qualityinspection}*/

         QualityInspectionBrowse:insertData("KittingLine1", "left").
         QualityInspectionBrowse:insertData(IF AVAILABLE QualitySampleLineHour THEN QualitySampleLineHour.HourOfday ELSE 0, "left").
         QualityInspectionBrowse:insertData(IF AVAILABLE QualityStation THEN QualityStation.StationName ELSE "", "left").
         QualityInspectionBrowse:insertData(IF AVAILABLE StockPackage THEN StockPackage.PackageRef ELSE "", "left").
         QualityInspectionBrowse:insertData(QualityInspection.UnitsInspected, "left").
         QualityInspectionBrowse:insertData(IF AVAILABLE GateUser THEN GateUser.FullName ELSE "", "left").
         QualityInspectionBrowse:insertData(QualityInspection.Completed, "left").
         
         
         
/*         QualityInspectionBrowse:insertData((IF AVAILABLE ShipOrder       THEN ShipOrder.OrderRef          ELSE ""), "left").                          */
/*         QualityInspectionBrowse:insertData((IF AVAILABLE Location        THEN Location.LocationRef        ELSE ""), "left").                          */
/*         QualityInspectionBrowse:insertData((IF AVAILABLE QualityInspectionStatus      THEN QualityInspectionStatus.StatusName       ELSE ""), "left").*/
/*         QualityInspectionBrowse:insertData((IF AVAILABLE PickPackStation THEN PickPackStation.StationName ELSE ""), "left").                          */
         
         QualityInspectionBrowse:endRow().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF
