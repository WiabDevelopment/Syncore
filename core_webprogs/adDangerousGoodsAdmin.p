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
DEFINE VARIABLE chrDangerousGoodsID                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedDangerousGoods          AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrScrollToDangerousGoodsRow       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectDangerousGoodsRow         AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chrClassID                         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedClass                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrScrollToClassRow                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectClassRow                  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chrSubDivisionID                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE intSelectedSubDivision             AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrScrollToSubDivisionRow          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrSelectSubDivisionRow            AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chrPopupDangerousGoodsClassBrowse  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupDangerousGoodsClassDetails AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chrPopupDGSubDivisionBrowse        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupDGSubDivisionDetails       AS CHARACTER   NO-UNDO.

/* Objects */
DEFINE VARIABLE DangerousGoodsBrowseFrame                AS pageFrame.
DEFINE VARIABLE DangerousGoodsBrowse                     AS browseTable.
DEFINE VARIABLE DangerousGoodsBrowseButtons              AS buttonBar.
DEFINE VARIABLE DangerousGoodsDetailsForm                AS dataForm.
DEFINE VARIABLE DangerousGoodsDetailsButtons             AS buttonBar.

DEFINE VARIABLE DangerousGoodsClassBrowseForm            AS dataForm.
DEFINE VARIABLE DangerousGoodsClassBrowse                AS browseTable.
DEFINE VARIABLE DangerousGoodsClassBrowseButtons         AS buttonBar.
DEFINE VARIABLE DangerousGoodsClassDetailsForm           AS dataForm.
DEFINE VARIABLE DangerousGoodsClassDetailsButtons        AS buttonBar.

DEFINE VARIABLE DangerousGoodsSubDivisionBrowseForm      AS dataForm.
DEFINE VARIABLE DangerousGoodsSubDivisionBrowse          AS browseTable.
DEFINE VARIABLE DangerousGoodsSubDivisionBrowseButtons   AS buttonBar.
DEFINE VARIABLE DangerousGoodsSubDivisionDetailsForm     AS dataForm.
DEFINE VARIABLE DangerousGoodsSubDivisionDetailsButtons  AS buttonBar.

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

&IF DEFINED(EXCLUDE-pDangerousGoodsClassBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsClassBrowse Procedure
PROCEDURE pDangerousGoodsClassBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "dangerousgoodsclass_details_form"}
   
   DangerousGoodsClassBrowseForm = NEW dataForm("dangerousgoodsclass_browse_form").
   DangerousGoodsClassBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsClassBrowseForm:FormAction  = "dbDangerousGoodsClassUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DangerousGoodsClassBrowseForm:FormWidth   = 860.
   DangerousGoodsClassBrowseForm:FormHeight  = 530.
   DangerousGoodsClassBrowseForm:FormTitle   = fTL("Dangerous Goods Classes").
   DangerousGoodsClassBrowseForm:FormType    = "xxl_large".
   
   DangerousGoodsClassBrowse = NEW browseTable("dangerousgoodsclass_browse").
   DangerousGoodsClassBrowse:BrowseWidth  = 840.
   DangerousGoodsClassBrowse:BrowseHeight = 490.
   
   DangerousGoodsClassBrowse:insertColumn(fTL("DG ClassID"), 80, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DangerousGoodsClass}
   
   DangerousGoodsClassBrowse:insertColumn(fTL("Class Code"),        150, "CHARACTER", "left", FALSE).
   DangerousGoodsClassBrowse:insertColumn(fTL("Class Name"),        150, "CHARACTER", "left", FALSE).
   DangerousGoodsClassBrowse:insertColumn(fTL("Class Description"), 200, "CHARACTER", "left", FALSE).
   DangerousGoodsClassBrowse:insertColumn(fTL("List Seq"),           60, "INTEGER", FALSE).
   
   DangerousGoodsClassBrowse:StartBody().
   
   /* List all DangerousGoodsClasss */
   FOR EACH DangerousGoodsClass NO-LOCK
      BY DangerousGoodsClass.ListingSequence:
      
      DangerousGoodsClassBrowse:startRow(DangerousGoodsClass.DangerousGoodsClassID, 
                             "selectDangerousGoodsClassRow(this," + '"' +  
                             STRING(DangerousGoodsClass.DangerousGoodsClassID) + 
                             '","adDangerousGoodsAdmin.p","dangerousgoodsclass_browse_form"' + ");", "").
      
      DangerousGoodsClassBrowse:insertData(DangerousGoodsClass.DangerousGoodsClassID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i DangerousGoodsClass}
      
      DangerousGoodsClassBrowse:insertData(DangerousGoodsClass.ClassCode,               "left").
      DangerousGoodsClassBrowse:insertData(DangerousGoodsClass.ClassName,               "left").
      DangerousGoodsClassBrowse:insertData(DangerousGoodsClass.ClassDescr,              "left").
      DangerousGoodsClassBrowse:insertData(STRING(DangerousGoodsClass.ListingSequence), "right").
      
      /* Add hidden fields */
      DangerousGoodsClassBrowse:insertHiddenData("DangerousGoodsClassID",       DangerousGoodsClass.DangerousGoodsClassID).
      DangerousGoodsClassBrowse:insertHiddenData("DangerousGoodsClassVersionID",DangerousGoodsClass.VersionID).
      
      DangerousGoodsClassBrowse:endRow().
      
   END. /* FOR EACH DangerousGoodsClass NO-LOCK */
   
   DangerousGoodsClassBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + DangerousGoodsClassBrowse:getErrors().
   
   DangerousGoodsClassBrowseForm:insertHiddenField("DangerousGoodsClassID","").
   DangerousGoodsClassBrowseForm:insertHiddenField("DangerousGoodsClassVersionID","").
   DangerousGoodsClassBrowseForm:insertHiddenField("dangerousgoods_browse_scroll","").
   DangerousGoodsClassBrowseForm:insertHiddenField("dangerousgoodsclass_browse_scroll","").
   DangerousGoodsClassBrowseForm:insertHiddenField("popup_dangerousgoodsclass_details_form","").
   DangerousGoodsClassBrowseForm:insertHiddenField("form_name","dangerousgoodsclass_browse_form").
   DangerousGoodsClassBrowseForm:insertHiddenField("prog_name","adDangerousGoodsAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsClassBrowseForm}
   
   /* Create Button Bar */
   DangerousGoodsClassBrowseButtons = NEW buttonBar().
   
   DangerousGoodsClassBrowseButtons:addButton("dangerousgoodsclass_browse_form_btn_create",
                                              fTL("Create"),
                                              "createDangerousGoodsClass('dangerousgoodsclass_details_form');"). 
   
   DangerousGoodsClassBrowseButtons:addButton("dangerousgoodsclass_browse_form_btn_view",
                                              fTL("Details"),
                                              "viewDangerousGoodsClassDetails('dangerousgoodsclass_details_form');",
                                              (IF intSelectedClass > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsClassBrowseButtons:addButton("dangerousgoodsclass_browse_form_btn_delete",
                                              fTL("Delete"),
                                              "confirmDeleteDangerousGoodsClass('dangerousgoodsclass_browse_form');", 
                                              (IF intSelectedClass > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsClassBrowseButtons:addButton("dangerousgoodsclass_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('dangerousgoodsclass_browse_form_popup');").
   
   DangerousGoodsClassBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DangerousGoodsClassBrowseForm:FormBrowse  = DangerousGoodsClassBrowse.
   DangerousGoodsClassBrowseForm:FormButtons = DangerousGoodsClassBrowseButtons.
   DangerousGoodsClassBrowseForm:endForm(). 
   
   DangerousGoodsClassBrowseForm:displayForm().
   
   /*
**************************************************************************************************************************
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "dangerousgoods_details_form"}
   
   DangerousGoodsBrowse = NEW browseTable("dangerousgoodsclass_browse").
   DangerousGoodsBrowse:BrowseWidth  = 965.
   DangerousGoodsBrowse:BrowseHeight = 455.
   DangerousGoodsBrowse:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsDetailsForm:FormWidth   = 460.
   DangerousGoodsDetailsForm:FormHeight  = 300.
   DangerousGoodsDetailsForm:FormTitle   = "DangerousGoods Details".
   DangerousGoodsDetailsForm:FormType    = "medium".
   
   /* Add in the ID as first Column */
   DangerousGoodsBrowse:insertColumn(fTL("ID"), 100, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DangerousGoods}
   
   DangerousGoodsBrowse:insertColumn(fTL("Class"),             150, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("SubDivision"),       120, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("UNCode"),            100, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("Description"),       200, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("DangerValue"),        90, "INTEGER",   "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("Active"),            110, "LOGICAL", FALSE).
   
   /*Body*/
   DangerousGoodsBrowse:startBody().
   
   FOR EACH DangerousGoods NO-LOCK /*idx=ActiveDangerousGoodsDescr*/
      /* WHERE DangerousGoods.Active */
      BY DangerousGoodsID:
      
      /* FIND FIRST DangerousGoodsClass OF DangerousGoods NO-LOCK NO-ERROR. */
      FIND FIRST DangerousGoodsSubDivision OF DangerousGoods NO-LOCK NO-ERROR.
      FIND FIRST DangerousGoodsClass OF DangerousGoodsSubDivision NO-LOCK NO-ERROR.
      
      DangerousGoodsBrowse:startRow(DangerousGoods.DangerousGoodsID, "selectDangerousGoodsRow(this," + '"' + STRING(DangerousGoods.DangerousGoodsID) + '"' + ");", "").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerousGoodsID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i DangerousGoods}
      
      DangerousGoodsBrowse:insertData(DangerousGoodsClass.ClassCode, "left").
      DangerousGoodsBrowse:insertData(DangerousGoodsSubDivision.SubDivisionCode, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.UNCode, "left").
      DangerousGoodsBrowse:insertData(DangerousGoodsSubDivision.SubDivisionDescr, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerValue, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.Active).
      
      /* Add hidden fields */
      DangerousGoodsBrowse:insertHiddenData("DangerousGoodsVersionID",DangerousGoods.VersionID).
      
      DangerousGoodsBrowse:endRow().
      
   END. /*FOR EACH DangerousGoods NO-LOCK */
   
   DangerousGoodsBrowse:endTable().
   
   /* Create a new frame */
   DangerousGoodsBrowseFrame = NEW pageFrame().
   DangerousGoodsBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   DangerousGoodsBrowseFrame:FormAction="dbDangerousGoodsUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   DangerousGoodsBrowseFrame:formOpen("dangerousgoods_browse_form").
   
   /* Start the Frame Header */
   DangerousGoodsBrowseFrame:insertSpacer(5).
   DangerousGoodsBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   DangerousGoodsBrowse:displayBrowse().  
   
   /* End the Frame Header */
   DangerousGoodsBrowseFrame:frameClose().
   DangerousGoodsBrowseFrame:insertSpacer(10).
   
   DangerousGoodsBrowseFrame:insertHiddenField("dangerousgoods_browse_scroll","").
   DangerousGoodsBrowseFrame:insertHiddenField("DangerousGoodsID","").
   DangerousGoodsBrowseFrame:insertHiddenField("DangerousGoodsVersionID","").
   DangerousGoodsBrowseFrame:insertHiddenField("form_name", "dangerousgoods_browse_form").
   DangerousGoodsBrowseFrame:insertHiddenField("prog_name", "adDangerousGoods.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsBrowseFrame}
   
   DangerousGoodsBrowseFrame:formClose().
   
   /* Create Button Bar */
   DangerousGoodsBrowseButtons = NEW buttonBar().
   DangerousGoodsBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewDangerousGoodsDetails('dangerousgoods_details_form');",
                                 (IF intSelectedDangerousGoods > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_create",
                                 fTL("Create"),
                                 "createDangerousGoods('dangerousgoods_details_form');",
                                 "").
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_dangerousgoodsclass",
                                 fTL("DG Class"),
                                 "viewDangerousGoodsClassBrowse('dangerousgoods_details_form');",
                                 "Disabled").
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_dgsubdivision",
                                 fTL("DG SubDivision"),
                                 "viewDGSubDivisionBrowse('dangerousgoods_details_form');",
                                 "Disabled").
   /*
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_delete",
                                 fTL("Delete"),
                                 "confirmDeleteDangerousGoods();",
                                 (IF intSelectedDangerousGoods > 0 THEN "" ELSE "Disabled")).
   */
   DangerousGoodsBrowseButtons:closeBar().  
   DangerousGoodsBrowseButtons:displayButtonBar().  
   */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDangerousGoodsClassDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsClassDetails Procedure
PROCEDURE pDangerousGoodsClassDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "dangerousgoodsclass_details_form"}
   
   ASSIGN chrDisplayFieldList  = "DangerousGoodsClassID,ClassCode,ClassName,ClassDescr,ListingSequence"
          chrEditFieldList     = "ClassCode,ClassName,ClassDescr,ListingSequence"
          chrNewFieldList      = "ClassCode,ClassName,ClassDescr,ListingSequence"
          chrRequiredFieldList = "ClassCode,ClassName,ClassDescr,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   DangerousGoodsClassDetailsForm = NEW dataForm("dangerousgoodsclass_details_form").
   DangerousGoodsClassDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsClassDetailsForm:FormAction  = "dbDangerousGoodsClassUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DangerousGoodsClassDetailsForm:FormWidth   = 460.
   DangerousGoodsClassDetailsForm:FormHeight  = 300.
   DangerousGoodsClassDetailsForm:FormTitle   = "DangerousGoodsClass Details".
   DangerousGoodsClassDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   DangerousGoodsClassDetailsForm:insertPaddingColumn(30).
   DangerousGoodsClassDetailsForm:insertColumn(100).
   DangerousGoodsClassDetailsForm:insertColumn(250).
   
   /* Fields */
   DangerousGoodsClassDetailsForm:startRow().
   DangerousGoodsClassDetailsForm:insertLabel("ID").
   DangerousGoodsClassDetailsForm:insertTextField("DangerousGoodsClassID", "", 110, TRUE).
   
   DangerousGoodsClassDetailsForm:startRow().
   DangerousGoodsClassDetailsForm:insertLabel("Class Code").
   DangerousGoodsClassDetailsForm:insertTextField("ClassCode", "", 250, TRUE).
   
   DangerousGoodsClassDetailsForm:startRow().
   DangerousGoodsClassDetailsForm:insertLabel("Class Name").
   DangerousGoodsClassDetailsForm:insertTextField("ClassName", "", 250, TRUE).
   
   DangerousGoodsClassDetailsForm:startRow().
   DangerousGoodsClassDetailsForm:insertLabel("Class Descr").
   DangerousGoodsClassDetailsForm:insertTextAreaField("ClassDescr", "", 250, TRUE).
   
   DangerousGoodsClassDetailsForm:startRow().
   DangerousGoodsClassDetailsForm:insertLabel("Listing Sequence").
   DangerousGoodsClassDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).
   
   {webGetOptionalFormFields.i pDangerousGoodsDetailsFields}
   
   /* Add Hidden Fields*/
   DangerousGoodsClassDetailsForm:insertHiddenField("dangerousgoodsclass_browse_scroll", "").
   DangerousGoodsClassDetailsForm:insertHiddenField("form_name", "dangerousgoodsclass_details_form").
   DangerousGoodsClassDetailsForm:insertHiddenField("prog_name", "adDangerousGoodsAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsClassDetailsForm}
   
   /* Create Button Bar */
   DangerousGoodsClassDetailsButtons = NEW buttonBar().
   
   DangerousGoodsClassDetailsButtons:addButton("dangerousgoodsclass_details_form_btn_save", 
                                               fTL("Save"), 
                                               "updateDangerousGoodsClass('dangerousgoodsclass_details_form');").
   
   DangerousGoodsClassDetailsButtons:addButton("dangerousgoodsclass_details_form_btn_cancel", 
                                               fTL("Cancel"), 
                                               "cancelUpdate('UserCancelled','process_mode'); disablePopup('dangerousgoodsclass_details_form_popup');").
   
   DangerousGoodsClassDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DangerousGoodsClassDetailsForm:FormButtons = DangerousGoodsClassDetailsButtons.
   
   DangerousGoodsClassDetailsForm:endForm(). 
   
   DangerousGoodsClassDetailsForm:displayForm(). 
   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDangerousGoodsSubDivisionBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsSubDivisionBrowse Procedure
PROCEDURE pDangerousGoodsSubDivisionBrowse:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "dangerousgoodssubdivision_details_form"}
   
   DangerousGoodsSubDivisionBrowseForm = NEW dataForm("dangerousgoodssubdivision_browse_form").
   DangerousGoodsSubDivisionBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsSubDivisionBrowseForm:FormAction  = "dbDangerousGoodsSubDivisionUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DangerousGoodsSubDivisionBrowseForm:FormWidth   = 860.
   DangerousGoodsSubDivisionBrowseForm:FormHeight  = 530.
   DangerousGoodsSubDivisionBrowseForm:FormTitle   = fTL("Dangerous Goods SubDivisions").
   DangerousGoodsSubDivisionBrowseForm:FormType    = "xxl_large".
   
   DangerousGoodsSubDivisionBrowse = NEW browseTable("dangerousgoodssubdivision_browse").
   DangerousGoodsSubDivisionBrowse:BrowseWidth  = 840.
   DangerousGoodsSubDivisionBrowse:BrowseHeight = 490.
   
   DangerousGoodsSubDivisionBrowse:insertColumn(fTL("ID"), 70, "INTEGER", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DangerousGoodsSubDivision}
   
   
   DangerousGoodsSubDivisionBrowse:insertColumn(fTL("Class"),                   180, "CHARACTER", "left", FALSE).
   DangerousGoodsSubDivisionBrowse:insertColumn(fTL("SubDivision Code"),        200, "CHARACTER", "left", FALSE).
   DangerousGoodsSubDivisionBrowse:insertColumn(fTL("SubDivision Name"),        210, "CHARACTER", "left", FALSE).
   /* DangerousGoodsSubDivisionBrowse:insertColumn(fTL("SubDivision Description"), 180, "CHARACTER", "left", FALSE). */
   DangerousGoodsSubDivisionBrowse:insertColumn(fTL("List Seq"),                 60, "INTEGER", FALSE).
   
   DangerousGoodsSubDivisionBrowse:StartBody().
   
   /* List all DangerousGoodsSubDivisions */
   FOR EACH DangerousGoodsSubDivision NO-LOCK
      BY DangerousGoodsSubDivision.ListingSequence:
      
      FIND FIRST DangerousGoodsClass OF DangerousGoodsSubDivision NO-LOCK NO-ERROR.
      
      DangerousGoodsSubDivisionBrowse:startRow(DangerousGoodsSubDivision.DangerousGoodsSubDivisionID, 
                             "selectDangerousGoodsSubDivisionRow(this," + '"' +  
                             STRING(DangerousGoodsSubDivision.DangerousGoodsSubDivisionID) + 
                             '","adDangerousGoodsAdmin.p","dangerousgoodssubdivision_browse_form"' + ");", "").
      
      DangerousGoodsSubDivisionBrowse:insertData(DangerousGoodsSubDivision.DangerousGoodsSubDivisionID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i DangerousGoodsSubDivision}
      
      DangerousGoodsSubDivisionBrowse:insertData(IF AVAILABLE DangerousGoodsClass THEN DangerousGoodsClass.ClassCode ELSE "", "left").
      DangerousGoodsSubDivisionBrowse:insertData(DangerousGoodsSubDivision.SubDivisionCode,         "left").
      DangerousGoodsSubDivisionBrowse:insertData(DangerousGoodsSubDivision.SubDivisionName,         "left").
      /* DangerousGoodsSubDivisionBrowse:insertData(DangerousGoodsSubDivision.SubDivisionDescr,        "left"). */
      DangerousGoodsSubDivisionBrowse:insertData(STRING(DangerousGoodsSubDivision.ListingSequence), "right").
      
      /* Add hidden fields */
      DangerousGoodsSubDivisionBrowse:insertHiddenData("DangerousGoodsSubDivisionID",       DangerousGoodsSubDivision.DangerousGoodsSubDivisionID).
      DangerousGoodsSubDivisionBrowse:insertHiddenData("DangerousGoodsSubDivisionVersionID",DangerousGoodsSubDivision.VersionID).
      
      DangerousGoodsSubDivisionBrowse:endRow().
      
   END. /* FOR EACH DangerousGoodsSubDivision NO-LOCK */
   
   DangerousGoodsSubDivisionBrowse:endTable().
   
   chrPageBuildError = chrPageBuildError + DangerousGoodsSubDivisionBrowse:getErrors().
   
   DangerousGoodsSubDivisionBrowseForm:insertHiddenField("DangerousGoodsSubDivisionID","").
   DangerousGoodsSubDivisionBrowseForm:insertHiddenField("DangerousGoodsSubDivisionVersionID","").
   DangerousGoodsSubDivisionBrowseForm:insertHiddenField("dangerousgoods_browse_scroll","").
   DangerousGoodsSubDivisionBrowseForm:insertHiddenField("dangerousgoodssubdivision_browse_scroll","").
   DangerousGoodsSubDivisionBrowseForm:insertHiddenField("popup_dangerousgoodssubdivision_details_form","").
   DangerousGoodsSubDivisionBrowseForm:insertHiddenField("form_name","dangerousgoodssubdivision_browse_form").
   DangerousGoodsSubDivisionBrowseForm:insertHiddenField("prog_name","adDangerousGoodsAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsSubDivisionBrowseForm}
   
   /* Create Button Bar */
   DangerousGoodsSubDivisionBrowseButtons = NEW buttonBar().
   
   DangerousGoodsSubDivisionBrowseButtons:addButton("dangerousgoodssubdivision_browse_form_btn_create",
                                              fTL("Create"),
                                              "createDangerousGoodsSubDivision('dangerousgoodssubdivision_details_form');"). 
   
   DangerousGoodsSubDivisionBrowseButtons:addButton("dangerousgoodssubdivision_browse_form_btn_view",
                                              fTL("Details"),
                                              "viewDangerousGoodsSubDivisionDetails('dangerousgoodssubdivision_details_form');",
                                              (IF intSelectedSubDivision > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsSubDivisionBrowseButtons:addButton("dangerousgoodssubdivision_browse_form_btn_delete",
                                              fTL("Delete"),
                                              "confirmDeleteDangerousGoodsSubDivision('dangerousgoodssubdivision_browse_form');", 
                                              (IF intSelectedSubDivision > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsSubDivisionBrowseButtons:addButton("dangerousgoodssubdivision_browse_form_btn_cancel",
                                              fTL("Cancel"),
                                              "disablePopup('dangerousgoodssubdivision_browse_form_popup');").
   
   DangerousGoodsSubDivisionBrowseButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DangerousGoodsSubDivisionBrowseForm:FormBrowse  = DangerousGoodsSubDivisionBrowse.
   DangerousGoodsSubDivisionBrowseForm:FormButtons = DangerousGoodsSubDivisionBrowseButtons.
   DangerousGoodsSubDivisionBrowseForm:endForm(). 
   
   DangerousGoodsSubDivisionBrowseForm:displayForm().

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDangerousGoodsSubDivisionDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsSubDivisionDetails Procedure
PROCEDURE pDangerousGoodsSubDivisionDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "dangerousgoodssubdivision_details_form"}
   
   ASSIGN chrDisplayFieldList  = "DangerousGoodsSubDivisionID,DangerousGoodsClassID,SubDivisionCode,"
                                   + "SubDivisionName,SubDivisionDescr,ListingSequence"
          chrEditFieldList     = "DangerousGoodsClassID,SubDivisionCode,SubDivisionName,SubDivisionDescr,ListingSequence"
          chrNewFieldList      = "DangerousGoodsClassID,SubDivisionCode,SubDivisionName,SubDivisionDescr,ListingSequence"
          chrRequiredFieldList = "DangerousGoodsClassID,SubDivisionCode,SubDivisionName,SubDivisionDescr,ListingSequence"
          chrExtraFieldList    = ""
          chrValidateFieldList = "".
   
   DangerousGoodsSubDivisionDetailsForm = NEW dataForm("dangerousgoodssubdivision_details_form").
   DangerousGoodsSubDivisionDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsSubDivisionDetailsForm:FormAction  = "dbDangerousGoodsSubDivisionUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DangerousGoodsSubDivisionDetailsForm:FormWidth   = 460.
   DangerousGoodsSubDivisionDetailsForm:FormHeight  = 300.
   DangerousGoodsSubDivisionDetailsForm:FormTitle   = "DangerousGoodsSubDivision Details".
   DangerousGoodsSubDivisionDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   DangerousGoodsSubDivisionDetailsForm:insertPaddingColumn(30).
   DangerousGoodsSubDivisionDetailsForm:insertColumn(110).
   DangerousGoodsSubDivisionDetailsForm:insertColumn(260).
   
   /* Fields */
   DangerousGoodsSubDivisionDetailsForm:startRow().
   DangerousGoodsSubDivisionDetailsForm:insertLabel("ID").
   DangerousGoodsSubDivisionDetailsForm:insertTextField("DangerousGoodsSubDivisionID", "", 110, TRUE).
   
   DangerousGoodsSubDivisionDetailsForm:startRow().
   DangerousGoodsSubDivisionDetailsForm:insertLabel(fTL("Class")).
   DangerousGoodsSubDivisionDetailsForm:insertComboField("DangerousGoodsClassID", "", 220, TRUE).
   
   FOR EACH DangerousGoodsClass NO-LOCK:
      DangerousGoodsSubDivisionDetailsForm:insertComboPairs("DangerousGoodsClassID", 
                                                            STRING(DangerousGoodsClass.DangerousGoodsClassID), 
                                                            DangerousGoodsClass.ClassCode).
   END.
   
   DangerousGoodsSubDivisionDetailsForm:startRow().
   DangerousGoodsSubDivisionDetailsForm:insertLabel("SubDivision Code").
   DangerousGoodsSubDivisionDetailsForm:insertTextField("SubDivisionCode", "", 260, TRUE).
   
   DangerousGoodsSubDivisionDetailsForm:startRow().
   DangerousGoodsSubDivisionDetailsForm:insertLabel("SubDivision Name").
   DangerousGoodsSubDivisionDetailsForm:insertTextField("SubDivisionName", "", 260, TRUE).
   
   DangerousGoodsSubDivisionDetailsForm:startRow().
   DangerousGoodsSubDivisionDetailsForm:insertLabel("SubDivision Descr").
   DangerousGoodsSubDivisionDetailsForm:insertTextAreaField("SubDivisionDescr", "", 260, TRUE).
   
   DangerousGoodsSubDivisionDetailsForm:startRow().
   DangerousGoodsSubDivisionDetailsForm:insertLabel("Listing Sequence").
   DangerousGoodsSubDivisionDetailsForm:insertTextField("ListingSequence", "", 110, TRUE).
   
   {webGetOptionalFormFields.i pDangerousGoodsDetailsFields}
   
   /* Add Hidden Fields*/
   DangerousGoodsSubDivisionDetailsForm:insertHiddenField("dangerousgoodssubdivision_browse_scroll", "").
   DangerousGoodsSubDivisionDetailsForm:insertHiddenField("form_name", "dangerousgoodssubdivision_details_form").
   DangerousGoodsSubDivisionDetailsForm:insertHiddenField("prog_name", "adDangerousGoodsAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsSubDivisionDetailsForm}
   
   /* Create Button Bar */
   DangerousGoodsSubDivisionDetailsButtons = NEW buttonBar().
   
   DangerousGoodsSubDivisionDetailsButtons:addButton("dangerousgoodssubdivision_details_form_btn_save", 
                                               fTL("Save"), 
                                               "updateDangerousGoodsSubDivision('dangerousgoodssubdivision_details_form');").
   
   DangerousGoodsSubDivisionDetailsButtons:addButton("dangerousgoodssubdivision_details_form_btn_cancel", 
                                               fTL("Cancel"), 
                                               "cancelUpdate('UserCancelled','process_mode'); disablePopup('dangerousgoodssubdivision_details_form_popup');").
   
   DangerousGoodsSubDivisionDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DangerousGoodsSubDivisionDetailsForm:FormButtons = DangerousGoodsSubDivisionDetailsButtons.
   
   DangerousGoodsSubDivisionDetailsForm:endForm(). 
   
   DangerousGoodsSubDivisionDetailsForm:displayForm(). 
   

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
   
   ASSIGN chrDangerousGoodsID = get-value("DangerousGoodsID")
          intSelectedDangerousGoods = INTEGER(chrDangerousGoodsID)
          chrScrollToDangerousGoodsRow = STRING(INTEGER(get-value("dangerousgoods_browse_scroll"))) + ";"
   
          chrClassID = get-value("DangerousGoodsClassID")
          intSelectedClass = INTEGER(chrClassID)
          chrScrollToClassRow = STRING(INTEGER(get-value("dangerousgoodsclass_browse_scroll"))) + ";"
   
          chrSubDivisionID = get-value("DangerousGoodsSubDivisionID")
          intSelectedSubDivision = INTEGER(chrSubDivisionID)
          chrScrollToSubDivisionRow = STRING(INTEGER(get-value("dangerousgoodssubdivision_browse_scroll"))) + ";".
   
   /* Process URL values */
   IF chrDangerousGoodsID <> "" THEN
      chrSelectDangerousGoodsRow = 'selectDangerousGoodsRow(document.getElementById("dangerousgoods_browse_row_' 
                                     + chrDangerousGoodsID + '"),"' + chrDangerousGoodsID + '");'.
   
   IF chrClassID <> "" THEN 
      chrSelectClassRow = 'selectDangerousGoodsClassRow(document.getElementById("dangerousgoodsclass_browse_row_' 
                            + chrClassID + '"),"' + chrClassID + '");'.
   
   IF chrSubDivisionID <> "" THEN 
      chrSelectSubDivisionRow = 'selectDangerousGoodsSubDivisionRow(document.getElementById("dangerousgoodssubdivision_browse_row_' 
                                  + chrSubDivisionID + '"),"' + chrSubDivisionID + '");'.
   
   IF get-value('popup_dangerousgoodsclass_browse') = "yes" THEN 
      chrPopupDangerousGoodsClassBrowse = 'enablePopup("dangerousgoodsclass_browse_form_popup");'.
   
   IF get-value('popup_dangerousgoodsclass_details') = "yes" THEN 
      chrPopupDangerousGoodsClassDetails = 'enablePopup("dangerousgoodsclass_details_form_popup");'.
   
   IF get-value('popup_dangerousgoodssubdivision_browse') = "yes" THEN 
      chrPopupDGSubDivisionBrowse = 'enablePopup("dangerousgoodssubdivision_browse_form_popup");'.
   
   IF get-value('popup_dangerousgoodssubdivision_details') = "yes" THEN 
      chrPopupDGSubDivisionDetails = 'enablePopup("dangerousgoodssubdivision_details_form_popup");'.
   
   /* Build the Body Load commands before rendering tha page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("dangerousgoods_browse").scrollTop=' + chrScrollToDangerousGoodsRow 
                   + chrSelectDangerousGoodsRow 
                   + chrPopupDangerousGoodsClassBrowse + chrPopupDangerousGoodsClassDetails + chrSelectClassRow
                   + chrPopupDGSubDivisionBrowse + chrPopupDGSubDivisionDetails + chrSelectSubDivisionRow.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage = NEW HTMLPage().
   ThisPage:WebStream = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu = get-value("menu_no").
   ThisPage:PageTitle = "Dangerous Goods Admin".
   ThisPage:FrameTitle = "Dangerous Goods Admin".
   ThisPage:OnBodyLoad = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* Include relevant Js files */
   ThisPage:addJavaScript("dangerousgoods.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pDangerousGoodsBrowse.
   
   FIND DangerousGoods NO-LOCK 
      WHERE DangerousGoods.DangerousGoodsID = intSelectedDangerousGoods NO-ERROR.
   
   /******* Popup Browsers and Forms ********/    
   RUN pDangerousGoodsDetails.
   RUN pDangerousGoodsClassBrowse.
   RUN pDangerousGoodsClassDetails.
   
   RUN pDangerousGoodsSubDivisionBrowse.
   RUN pDangerousGoodsSubDivisionDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i} 
   
   /* Delete objects definied locally */
   DELETE OBJECT DangerousGoodsBrowseFrame      NO-ERROR.
   DELETE OBJECT DangerousGoodsBrowse           NO-ERROR.
   DELETE OBJECT DangerousGoodsBrowseButtons    NO-ERROR.
   DELETE OBJECT DangerousGoodsDetailsForm      NO-ERROR.
   DELETE OBJECT DangerousGoodsDetailsButtons   NO-ERROR.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDangerousGoodsBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsBrowse Procedure 
PROCEDURE pDangerousGoodsBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "dangerousgoods_details_form"}
   
   DangerousGoodsBrowse = NEW browseTable("dangerousgoods_browse").
   DangerousGoodsBrowse:BrowseWidth  = 965.
   DangerousGoodsBrowse:BrowseHeight = 455.
   DangerousGoodsBrowse:WebStream = STREAM WebStream:HANDLE.
   
   /* Add in the ID as first Column */
   DangerousGoodsBrowse:insertColumn(fTL("ID"), 100, "INTEGER", "left", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i DangerousGoods}
   
   DangerousGoodsBrowse:insertColumn(fTL("Class"),             150, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("SubDivision"),       180, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("UNCode"),            100, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("Description"),       180, "CHARACTER", "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("DangerValue"),        90, "INTEGER",   "left", FALSE).
   DangerousGoodsBrowse:insertColumn(fTL("Active"),            110, "LOGICAL", FALSE).
   
   /*Body*/
   DangerousGoodsBrowse:startBody().
   
   FOR EACH DangerousGoods NO-LOCK /*idx=ActiveDangerousGoodsDescr*/
      /* WHERE DangerousGoods.Active */
      BY DangerousGoodsID:
      
      /* FIND FIRST DangerousGoodsClass OF DangerousGoods NO-LOCK NO-ERROR. */
      FIND FIRST DangerousGoodsSubDivision OF DangerousGoods NO-LOCK NO-ERROR.
      FIND FIRST DangerousGoodsClass OF DangerousGoodsSubDivision NO-LOCK NO-ERROR.
      
      DangerousGoodsBrowse:startRow(DangerousGoods.DangerousGoodsID, "selectDangerousGoodsRow(this," + '"' + STRING(DangerousGoods.DangerousGoodsID) + '"' + ");", "").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerousGoodsID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i DangerousGoods}
      
      DangerousGoodsBrowse:insertData(DangerousGoodsClass.ClassCode, "left").
      DangerousGoodsBrowse:insertData(DangerousGoodsSubDivision.SubDivisionCode, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.UNCode, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerousGoodsDescr, "left").
      DangerousGoodsBrowse:insertData(DangerousGoods.DangerValue, "right").
      DangerousGoodsBrowse:insertData(DangerousGoods.Active).
      
      /* Add hidden fields */
      DangerousGoodsBrowse:insertHiddenData("DangerousGoodsVersionID",DangerousGoods.VersionID).
      
      DangerousGoodsBrowse:endRow().
      
   END. /*FOR EACH DangerousGoods NO-LOCK */
   
   DangerousGoodsBrowse:endTable().
   
   /* Create a new frame */
   DangerousGoodsBrowseFrame = NEW pageFrame().
   DangerousGoodsBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   DangerousGoodsBrowseFrame:FormAction="dbDangerousGoodsUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   DangerousGoodsBrowseFrame:formOpen("dangerousgoods_browse_form").
   
   /* Start the Frame Header */
   DangerousGoodsBrowseFrame:insertSpacer(5).
   DangerousGoodsBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   DangerousGoodsBrowse:displayBrowse().  
   
   /* End the Frame Header */
   DangerousGoodsBrowseFrame:frameClose().
   DangerousGoodsBrowseFrame:insertSpacer(10).
   
   DangerousGoodsBrowseFrame:insertHiddenField("dangerousgoods_browse_scroll","").
   DangerousGoodsBrowseFrame:insertHiddenField("DangerousGoodsID","").
   DangerousGoodsBrowseFrame:insertHiddenField("DangerousGoodsVersionID","").
   DangerousGoodsBrowseFrame:insertHiddenField("popup_dangerousgoodsclass_browse","").
   DangerousGoodsBrowseFrame:insertHiddenField("popup_dangerousgoodsclass_details","").
   DangerousGoodsBrowseFrame:insertHiddenField("popup_dangerousgoodssubdivision_browse","").
   DangerousGoodsBrowseFrame:insertHiddenField("popup_dangerousgoodssubdivision_details","").
   DangerousGoodsBrowseFrame:insertHiddenField("form_name", "dangerousgoods_browse_form").
   DangerousGoodsBrowseFrame:insertHiddenField("prog_name", "adDangerousGoodsAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsBrowseFrame}
   
   DangerousGoodsBrowseFrame:formClose().
   
   /* Create Button Bar */
   DangerousGoodsBrowseButtons = NEW buttonBar().
   DangerousGoodsBrowseButtons:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_details",
                                 fTL("Details"),
                                 "viewDangerousGoodsDetails('dangerousgoods_details_form');",
                                 (IF intSelectedDangerousGoods > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_create",
                                 fTL("Create"),
                                 "createDangerousGoods('dangerousgoods_details_form');",
                                 "").
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_dangerousgoodsclass",
                                 fTL("DG Class"),
                                 "viewDangerousGoodsClassBrowse('dangerousgoods_details_form');",
                                 "").
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_dgsubdivision",
                                 fTL("DG SubDivision"),
                                 "viewDangerousGoodsSubDivisionBrowse('dangerousgoods_details_form');",
                                 "").
   
   DangerousGoodsBrowseButtons:addButton("dangerousgoods_browse_form_btn_delete",
                                 fTL("Delete"),
                                 "confirmDeleteDangerousGoods();",
                                 (IF intSelectedDangerousGoods > 0 THEN "" ELSE "Disabled")).
   
   DangerousGoodsBrowseButtons:closeBar().  
   DangerousGoodsBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDangerousGoodsDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsDetails Procedure 
PROCEDURE pDangerousGoodsDetails :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "dangerousgoods_details_form"}
   
   ASSIGN chrDisplayFieldList  = "DangerousGoodsID,DangerousGoodsSubDivisionID,UNCode,DangerousGoodsDescr" +
                                 ",DangerousGoodsLabelDescr,DangerValue,Active"
          chrEditFieldList     = "DangerousGoodsSubDivisionID,UNCode,DangerousGoodsDescr,DangerousGoodsLabelDescr" +
                                 ",DangerValue,Active"
          chrNewFieldList      = "DangerousGoodsSubDivisionID,UNCode,DangerValue,DangerousGoodsDescr,DangerousGoodsLabelDescr" +
                                 ",Active"
          chrRequiredFieldList = "DangerousGoodsSubDivisionID,UNCode,DangerValue,DangerousGoodsDescr,DangerousGoodsLabelDescr" +
                                 ",Active"
          chrExtraFieldList    = ""
          chrValidateFieldList = "DangerValue:DECIMAL".
   
   DangerousGoodsDetailsForm = NEW dataForm("dangerousgoods_details_form").
   DangerousGoodsDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   DangerousGoodsDetailsForm:FormAction  = "dbDangerousGoodsUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   DangerousGoodsDetailsForm:FormWidth   = 460.
   DangerousGoodsDetailsForm:FormHeight  = 300.
   DangerousGoodsDetailsForm:FormTitle   = "DangerousGoods Details".
   DangerousGoodsDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   DangerousGoodsDetailsForm:insertPaddingColumn(50).
   DangerousGoodsDetailsForm:insertColumn(90).
   DangerousGoodsDetailsForm:insertColumn(120).
   DangerousGoodsDetailsForm:insertColumn(20).
   DangerousGoodsDetailsForm:insertColumn(4).
   DangerousGoodsDetailsForm:insertColumn(110).
   
   /* Fields */
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("ID").
   DangerousGoodsDetailsForm:insertTextField("DangerousGoodsID", "", 110, TRUE).
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel(fTL("Sub Division")).
   DangerousGoodsDetailsForm:insertComboField("DangerousGoodsSubDivisionID", "", 220, TRUE).
   
   FOR EACH DangerousGoodsSubDivision NO-LOCK
      BY DangerousGoodsSubDivision.SubDivisionCode:
         
      DangerousGoodsDetailsForm:insertComboPairs("DangerousGoodsSubDivisionID", 
                                                  STRING(DangerousGoodsSubDivision.DangerousGoodsSubDivisionID), 
                                                  STRING(DangerousGoodsSubDivision.SubDivisionCode)).
   END.
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("UN Code").
   DangerousGoodsDetailsForm:insertTextField("UNCode", "", 220, TRUE).
   
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("Descr").
   DangerousGoodsDetailsForm:insertTextField("DangerousGoodsDescr", "", 220, TRUE).
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("Label Descr").
   DangerousGoodsDetailsForm:insertTextField("DangerousGoodsLabelDescr", "", 220, TRUE).
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel("Danger Value").
   DangerousGoodsDetailsForm:insertTextField("DangerValue", "", 220, TRUE).
   
   DangerousGoodsDetailsForm:startRow().
   DangerousGoodsDetailsForm:insertLabel(fTL("Active")).
   DangerousGoodsDetailsForm:insertComboField("Active", "", 110, TRUE).
   DangerousGoodsDetailsForm:insertComboPairs("Active", "yes", "Active").
   DangerousGoodsDetailsForm:insertComboPairs("Active", "no",  "Not Active").
   
   {webGetOptionalFormFields.i pDangerousGoodsDetailsFields}
   
   /* Add Hidden Fields*/
   DangerousGoodsDetailsForm:insertHiddenField("dangerousgoods_browse_scroll", "").
   DangerousGoodsDetailsForm:insertHiddenField("form_name", "dangerousgoods_details_form").
   DangerousGoodsDetailsForm:insertHiddenField("prog_name", "adDangerousGoodsAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i DangerousGoodsDetailsForm}
   
   /* Create Button Bar */
   DangerousGoodsDetailsButtons = NEW buttonBar().
   
   DangerousGoodsDetailsButtons:addButton("dangerousgoods_details_form_btn_save", 
                                  fTL("Save"), 
                                  "updateDangerousGoods('dangerousgoods_details_form');").
   
   DangerousGoodsDetailsButtons:addButton("dangerousgoods_details_form_btn_cancel", 
                                  fTL("Cancel"), 
                                  "cancelUpdate('UserCancelled','process_mode'); disablePopup('dangerousgoods_details_form_popup');").
   
   DangerousGoodsDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   DangerousGoodsDetailsForm:FormButtons = DangerousGoodsDetailsButtons.
   
   DangerousGoodsDetailsForm:endForm(). 
   
   DangerousGoodsDetailsForm:displayForm(). 
   
   /*   chrPageBuildError = chrPageBuildError + DangerousGoodsDetailsForm:getErrors().  */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pDangerousGoodsDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDangerousGoodsDetailsFields Procedure 
PROCEDURE pDangerousGoodsDetailsFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
   
   CASE chrOption:
      
      WHEN "FieldName" THEN
      DO:
         DangerousGoodsDetailsForm:startRow().
         DangerousGoodsDetailsForm:insertLabel(fTL("Field Label")).
         DangerousGoodsDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
      END. /*WHEN "FieldName" THEN*/
      
   END CASE. /*chrOption:*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

