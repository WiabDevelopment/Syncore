&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

  File: adLaserFileAdmin.p 

  Description: ad file for the Laser File Admin screen

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Alex Litas

  Created: 28/04/2015

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
{defDataMigrationVariables.i}

DEFINE VARIABLE intSelectedLaserFile                      AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedLaserFileHistory               AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedLaserFileReplacement           AS INTEGER     NO-UNDO.
DEFINE VARIABLE intSelectedLaserFileReplacementHistory    AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrLaserFileHistoryID                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLaserFileRow                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLaserFileRow                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLaserFileHistoryRow            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLaserFileReplacementRow        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrScrollToLaserFileReplacementHistoryRow AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLaserFileID                            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLaserFileReplacementID                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrLaserFileReplacementHistoryID          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupHistory                           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrPopupReplacement                       AS CHARACTER   NO-UNDO.

/* Laser File Objects */
DEFINE VARIABLE LaserFileBrowseFrame                      AS pageFrame.
DEFINE VARIABLE LaserFileBrowse                           AS browseTable.
DEFINE VARIABLE LaserFileBrowseButtons                    AS buttonBar.
DEFINE VARIABLE LaserFileDetailsForm                      AS dataForm.
DEFINE VARIABLE LaserFileDetailsButtons                   AS buttonBar.

/* Laser File Hisory Objects*/
DEFINE VARIABLE LaserFileHistoryBrowseForm                AS dataForm.  
DEFINE VARIABLE LaserFileHistoryBrowse                    AS browseTable.
DEFINE VARIABLE LaserFileHistoryDetailsForm               AS dataForm.
DEFINE VARIABLE LaserFileHistoryDetailsButtons            AS buttonBar.
DEFINE VARIABLE LaserFileHistoryButtons                   AS buttonBar.

/* Laser File Replacement Objects*/
DEFINE VARIABLE LaserFileReplacementBrowseForm            AS dataForm.  
DEFINE VARIABLE LaserFileReplacementBrowse                AS browseTable.
DEFINE VARIABLE LaserFileReplacementDetailsForm           AS dataForm.
DEFINE VARIABLE LaserFileReplacementDetailsButtons        AS buttonBar.
DEFINE VARIABLE LaserFileReplacementButtons               AS buttonBar.

/* Laser File History Replacement Objects*/
DEFINE VARIABLE LaserFileReplacementHistoryBrowseForm     AS dataForm.  
DEFINE VARIABLE LaserFileReplacementHistoryBrowse         AS browseTable.
DEFINE VARIABLE LaserFileReplacementHistoryDetailsForm    AS dataForm.
DEFINE VARIABLE LaserFileReplacementHistoryDetailsButtons AS buttonBar.
DEFINE VARIABLE LaserFileReplacementHistoryButtons        AS buttonBar.

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

&IF DEFINED(EXCLUDE-pLaserFileReplacementDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLaserFileReplacementDetails Procedure
PROCEDURE pLaserFileReplacementDetails:
   /*------------------------------------------------------------------------------
     Purpose:
     Parameters:  <none>
     Notes:
   ------------------------------------------------------------------------------*/

   /*    This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below*/
   {webGetWebForm.i "laserfilereplacement_details_form"}
   ASSIGN 
      chrDisplayFieldList  = "LaserFileReplacementID,LaserFileID,PsFontID,TextToBeReplaced,ReplacementText," +
                             "CountryID,LanguageID,TransactionID,CustomerID,ReplaceBy,Active,CreatedDate," +
                             "CreatedHour,CreatedMins"
      chrEditFieldList     = ""
      chrNewFieldList      = ""
      chrRequiredFieldList = ""
      chrExtraFieldList    = ""
      chrValidateFieldList = "".

   LaserFileReplacementDetailsForm = NEW dataForm("laserfilereplacement_details_form").
   LaserFileReplacementDetailsForm:WebStream = STREAM WebStream:HANDLE.

   LaserFileReplacementDetailsForm:FormAction = "dbLaserFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   LaserFileReplacementDetailsForm:FormWidth   = 460.
   LaserFileReplacementDetailsForm:FormHeight  = 300.
   LaserFileReplacementDetailsForm:FormTitle   = "Laser File Admin Details".
   LaserFileReplacementDetailsForm:FormType    = "medium".

   /* Column Layout */
   LaserFileReplacementDetailsForm:insertPaddingColumn(30).
   LaserFileReplacementDetailsForm:insertColumn(150).
   LaserFileReplacementDetailsForm:insertColumn(125).
   LaserFileReplacementDetailsForm:insertColumn(20).
   LaserFileReplacementDetailsForm:insertColumn(4).
   LaserFileReplacementDetailsForm:insertColumn(20).

   /* Fields */
   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Laser File Replacement ID")).
   LaserFileReplacementDetailsForm:insertTextField("LaserFileReplacementID", "", 100, TRUE).

   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Laser File ID")).
   LaserFileReplacementDetailsForm:insertTextField("LaserFileID", "", 100, TRUE).

   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Ps Font ID")).
   LaserFileReplacementDetailsForm:insertComboField("PsFontID", "", 168, TRUE).                                                   
   FOR EACH PsFont NO-LOCK  /*idx=PsFontID*/                                                                                                      
      BY    PsFont.PsFontID:                                                                                                  
      LaserFileReplacementDetailsForm:insertComboPairs("PsFontID", STRING(PsFont.PsFontID),
                                                       PsFont.FontName).
   END. /*FOR EACH PsFont NO-LOCK*/

   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Text To Be Replaced")).
   LaserFileReplacementDetailsForm:insertTextField("TextToBeReplaced", "", 100, TRUE).

   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Replacement Text")).
   LaserFileReplacementDetailsForm:insertTextField("ReplacementText", "", 100, TRUE).

   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Country")).                                                                
   LaserFileReplacementDetailsForm:insertComboField("CountryID", "", 168, TRUE).                                                   
   FOR EACH Country NO-LOCK  /*idx=CountryID*/                                                                                                      
      BY    Country.CountryID:                                                                                                  
      LaserFileReplacementDetailsForm:insertComboPairs("CountryID", STRING(Country.CountryID),
                                                       Country.CountryName).
   END. /*FOR EACH Country NO-LOCK*/
            
   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Language")).
   LaserFileReplacementDetailsForm:insertComboField("LanguageID", "", 168, TRUE).                                                   
   FOR EACH gate.Language NO-LOCK  /*idx=LanguageID*/                                                                                                      
      BY    gate.Language.LanguageID:                                                                                                  
      LaserFileReplacementDetailsForm:insertComboPairs("LanguageID", STRING(gate.Language.LanguageID),
                                                       gate.Language.LanguageName).
   END. /*FOR EACH Language NO-LOCK*/

   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Transaction ID")).
   LaserFileReplacementDetailsForm:insertTextField("TransactionID", "", 100, TRUE).
   
   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Customer")).
   LaserFileReplacementDetailsForm:insertComboField("CustomerID", "", 168, TRUE).                                                   
   FOR EACH Customer NO-LOCK  /*idx=CustomerID*/                                                                                                      
      BY    Customer.CustomerID:                                                                                                  
      LaserFileReplacementDetailsForm:insertComboPairs("CustomerID", STRING(Customer.CustomerID),
                                                       Customer.CustomerName).
   END. /*FOR EACH Customer NO-LOCK*/
   
   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Replace By")).
   LaserFileReplacementDetailsForm:insertTextField("ReplaceBy", "", 100, TRUE).
   
   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Active")).
   LaserFileReplacementDetailsForm:insertComboField("Active", "", 168, TRUE).
   LaserFileReplacementDetailsForm:insertComboPairs("Active","yes","Yes").
   LaserFileReplacementDetailsForm:insertComboPairs("Active","no","No").
   
   LaserFileReplacementDetailsForm:startRow().
   LaserFileReplacementDetailsForm:insertLabel(fTL("Created")).
   LaserFileReplacementDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).
   /* Time fields have no label*/
   LaserFileReplacementDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).
   /* This has a label to separate the time */
   LaserFileReplacementDetailsForm:insertLabel(":").
   LaserFileReplacementDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).

   {webGetOptionalFormFields.i pLaserFileReplacementDetailsFields}

   chrExtraFieldList = TRIM(chrExtraFieldList,",").

   /* Add Hidden Fields*/
   LaserFileReplacementDetailsForm:insertHiddenField("laserfilereplacement_browse_scroll", "").
   LaserFileReplacementDetailsForm:insertHiddenField("form_name", "laserfilereplacement_details_form").
   LaserFileReplacementDetailsForm:insertHiddenField("prog_name", "adLaserFileAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileReplacementDetailsForm}

   /* Create Button Bar */
   LaserFileReplacementDetailsButtons = NEW buttonBar().

   LaserFileReplacementDetailsButtons:addButton("laserfilereplacement_details_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('laserfilereplacement_details_form_popup');").
   LaserFileReplacementDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   LaserFileReplacementDetailsForm:FormButtons = LaserFileReplacementDetailsButtons.

   LaserFileReplacementDetailsForm:endForm().

   LaserFileReplacementDetailsForm:displayForm().
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pLaserFileReplacementHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLaserFileReplacementHistoryBrowse Procedure
PROCEDURE pLaserFileReplacementHistoryBrowse:
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   LaserFileReplacementHistoryBrowseForm           = NEW dataForm("laserfilereplacementhistory_browse_form").
   LaserFileReplacementHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   LaserFileReplacementHistoryBrowseForm:FormWidth  = 700.
   LaserFileReplacementHistoryBrowseForm:FormHeight = 510.
   LaserFileReplacementHistoryBrowseForm:FormTitle  = fTL("Laser File Admin Replacement History").
   LaserFileReplacementHistoryBrowseForm:FormType   = "xl_large".
   LaserFileReplacementHistoryBrowse                = NEW browseTable("laserfilereplacementhistory_browse").
   LaserFileReplacementHistoryBrowse:BrowseWidth    = 680.
   LaserFileReplacementHistoryBrowse:BrowseHeight   = 470.
   
   LaserFileReplacementHistoryBrowse:insertColumn(fTL("Laser File Replacement History ID"), 120, "CHARACTER",   "LEFT", FALSE).
   LaserFileReplacementHistoryBrowse:insertColumn(fTL("Replacement Text"),                  120, "CHARACTER",   "LEFT", FALSE).
   LaserFileReplacementHistoryBrowse:insertColumn(fTL("Country"),                           100, "DECIMAL",     "LEFT", FALSE).
   LaserFileReplacementHistoryBrowse:insertColumn(fTL("Language"),                          100, "DECIMAL",     "LEFT", FALSE).
   LaserFileReplacementHistoryBrowse:insertColumn(fTL("Customer"),                          100, "DECIMAL",     "LEFT", FALSE).
     
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LaserFileReplacementHistory}
   
   LaserFileReplacementHistoryBrowse:StartBody().
   
   FOR EACH LaserFileReplacementHistory NO-LOCK /*idx=LaserFileID and LaserFileReplacementHistoryID*/
      WHERE LaserFileReplacementHistory.LaserFileID = intSelectedLaserFile
      BY    LaserFileReplacementHistory.LaserFileReplacementHistoryID:

      FIND FIRST Country       OF LaserFileReplacementHistory   NO-LOCK NO-ERROR. /*idx=CountryID*/
      FIND FIRST Customer      OF LaserFileReplacementHistory   NO-LOCK NO-ERROR. /*idx=CustomerID*/
      FIND FIRST gate.Language OF LaserFileReplacementHistory   NO-LOCK NO-ERROR. /*idx=LanguageID*/
      
      LaserFileReplacementHistoryBrowse:startRow (LaserFileReplacementHistory.LaserFileReplacementHistoryID,
                                                  "selectLaserFileReplacementHistoryRow(this," + '"' + 
                                                  STRING(LaserFileReplacementHistory.LaserFileReplacementHistoryID) + 
                                                  '"' + ");", "").
      
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i LaserFileReplacementHistory}

      LaserFileReplacementHistoryBrowse:insertData(LaserFileReplacementHistory.LaserFileReplacementHistoryID,                     "").
      LaserFileReplacementHistoryBrowse:insertData(STRING(LaserFileReplacementHistory.ReplacementText),                       "LEFT").
      LaserFileReplacementHistoryBrowse:insertData(STRING(IF AVAILABLE Country THEN Country.CountryName ELSE ""),             "LEFT").
      LaserFileReplacementHistoryBrowse:insertData(STRING(IF AVAILABLE gate.Language THEN gate.Language.LanguageName ELSE ""),"LEFT").
      LaserFileReplacementHistoryBrowse:insertData(STRING(IF AVAILABLE Customer THEN Customer.CustomerName ELSE ""),          "LEFT").

      LaserFileReplacementHistoryBrowse:endRow().
   END. /* FOR EACH LaserFileReplacementHistory */
   
   LaserFileReplacementHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LaserFileReplacementHistoryBrowse:getErrors().
   
   LaserFileReplacementHistoryBrowseForm:insertHiddenField("popup_laserfilereplacement_browse","").
   LaserFileReplacementHistoryBrowseForm:insertHiddenField("popup_laserfilereplacementhistory_browse","").
   LaserFileReplacementHistoryBrowseForm:insertHiddenField("LaserFileReplacementHistoryID","").
      
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileReplacementHistoryBrowseForm}
   
   /* Create Button Bar */
   LaserFileReplacementHistoryButtons = NEW buttonBar().
   
   LaserFileReplacementHistoryButtons:addButton("laserfilereplacementhistory_browse_form_btn_details",
                                                fTL("Details"),
                                                "viewLaserFileReplacementHistoryDetails('laserfilereplacementhistory_details_form');",
                                                (IF intSelectedLaserFileReplacement > 0 THEN "" ELSE "Disabled")).    
                                         
   LaserFileReplacementHistoryButtons:addButton("laserfilereplacementhistory_browse_form_btn_cancel",
                                                fTL("Cancel"),
                                                "disablePopup('laserfilereplacementhistory_browse_form_popup');").
   LaserFileReplacementHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LaserFileReplacementHistoryBrowseForm:FormBrowse  = LaserFileReplacementHistoryBrowse.
   LaserFileReplacementHistoryBrowseForm:FormButtons = LaserFileReplacementHistoryButtons.
   LaserFileReplacementHistoryBrowseForm:endForm(). 
   
   LaserFileReplacementHistoryBrowseForm:displayForm().   

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pLaserFileReplacementHistoryDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLaserFileReplacementHistoryDetails Procedure
PROCEDURE pLaserFileReplacementHistoryDetails:
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   /*------------------------------------------------------------------------------
     Purpose:
     Parameters:  <none>
     Notes:
   ------------------------------------------------------------------------------*/

   /*    This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below*/
   {webGetWebForm.i "laserfilereplacementhistory_details_form"}
   ASSIGN 
      chrDisplayFieldList  = "LaserFileReplacementID,LaserFileReplacementHistoryID,LaserFileID,PsFontID," + 
                             "TextToBeReplaced,ReplacementText,CountryID,LanguageID,CustomerID,ReplaceBy," +
                             "Active,CreatedDate,CreatedHour,CreatedMins,TransactionID,OperationTypeID," +
                             "GateUserID"                               
      chrEditFieldList     = ""
      chrNewFieldList      = ""
      chrRequiredFieldList = ""
      chrExtraFieldList    = ""
      chrValidateFieldList = "".

   LaserFileReplacementHistoryDetailsForm = NEW dataForm("laserfilereplacementhistory_details_form").
   LaserFileReplacementHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.

   LaserFileReplacementHistoryDetailsForm:FormAction = "dbLaserFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").

   /* Setup */
   LaserFileReplacementHistoryDetailsForm:FormWidth   = 580.
   LaserFileReplacementHistoryDetailsForm:FormHeight  = 420.
   LaserFileReplacementHistoryDetailsForm:FormTitle   = "Laser File Admin Replacement History Details".
   LaserFileReplacementHistoryDetailsForm:FormType    = "large".

   /* Column Layout */
   LaserFileReplacementHistoryDetailsForm:insertPaddingColumn(30).
   LaserFileReplacementHistoryDetailsForm:insertColumn(150).
   LaserFileReplacementHistoryDetailsForm:insertColumn(125).
   LaserFileReplacementHistoryDetailsForm:insertColumn(20).
   LaserFileReplacementHistoryDetailsForm:insertColumn(4).
   LaserFileReplacementHistoryDetailsForm:insertColumn(20).

   /* Fields */
   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Laser File Replacement ID")).
   LaserFileReplacementHistoryDetailsForm:insertTextField("LaserFileReplacementID", "", 100, TRUE).

   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Laser File ID")).
   LaserFileReplacementHistoryDetailsForm:insertTextField("LaserFileID", "", 100, TRUE).

   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Ps Font ID")).
   LaserFileReplacementHistoryDetailsForm:insertComboField("PsFontID", "", 168, TRUE).                                                   
   FOR EACH PsFont NO-LOCK  /*idx=PsFontID*/                                                                                                      
      BY    PsFont.PsFontID:                                                                                                  
      LaserFileReplacementHistoryDetailsForm:insertComboPairs("PsFontID", STRING(PsFont.PsFontID),
                                                              PsFont.FontName).
   END. /*FOR EACH PsFont NO-LOCK*/

   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Text To Be Replaced")).
   LaserFileReplacementHistoryDetailsForm:insertTextField("TextToBeReplaced", "", 100, TRUE).

   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Replacement Text")).
   LaserFileReplacementHistoryDetailsForm:insertTextField("ReplacementText", "", 100, TRUE).

   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Country")).                                                                
   LaserFileReplacementHistoryDetailsForm:insertComboField("CountryID", "", 168, TRUE).                                                   
   FOR EACH Country NO-LOCK  /*idx=CountryID*/                                                                                                      
      BY    Country.CountryID:                                                                                                  
      LaserFileReplacementHistoryDetailsForm:insertComboPairs("CountryID", STRING(Country.CountryID),
                                                              Country.CountryName).
   END. /*FOR EACH Country NO-LOCK*/
            
   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Language")).
   LaserFileReplacementHistoryDetailsForm:insertComboField("LanguageID", "", 168, TRUE).                                                   
   FOR EACH gate.Language NO-LOCK  /*idx=LanguageID*/                                                                                                      
      BY    gate.Language.LanguageID:                                                                                                  
      LaserFileReplacementHistoryDetailsForm:insertComboPairs("LanguageID", STRING(gate.Language.LanguageID),
                                                              gate.Language.LanguageName).
   END. /*FOR EACH Language NO-LOCK*/
   
   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Customer")).
   LaserFileReplacementHistoryDetailsForm:insertComboField("CustomerID", "", 168, TRUE).                                                   
   FOR EACH Customer NO-LOCK  /*idx=CustomerID*/                                                                                                      
      BY    Customer.CustomerID:                                                                                                  
      LaserFileReplacementHistoryDetailsForm:insertComboPairs("CustomerID", STRING(Customer.CustomerID),
                                                              Customer.CustomerName).
   END. /*FOR EACH Customer NO-LOCK*/
   
   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Replace By")).
   LaserFileReplacementHistoryDetailsForm:insertTextField("ReplaceBy", "", 100, TRUE).
   
   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Active")).
   LaserFileReplacementHistoryDetailsForm:insertComboField("Active", "", 168, TRUE).
   LaserFileReplacementHistoryDetailsForm:insertComboPairs("Active","yes","Yes").
   LaserFileReplacementHistoryDetailsForm:insertComboPairs("Active","no","No").
   
   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Created")).
   LaserFileReplacementHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).
   /* Time fields have no label*/
   LaserFileReplacementHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).
   /* This has a label to separate the time */
   LaserFileReplacementHistoryDetailsForm:insertLabel(":").
   LaserFileReplacementHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).

   LaserFileReplacementHistoryDetailsForm:startRow().
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Transaction ID")).
   LaserFileReplacementHistoryDetailsForm:insertTextField("TransactionID", "", 100, TRUE).
   
   LaserFileReplacementHistoryDetailsForm:startRow().                                                                                           
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   LaserFileReplacementHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*idx=OperationTypeID*/                                                                                                      
      BY    OperationType.OperationTypeID:                                                                                                  
      LaserFileReplacementHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                              OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   LaserFileReplacementHistoryDetailsForm:startRow().                                                                                           
   LaserFileReplacementHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   LaserFileReplacementHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*idx=GateUserID*/                                                                                                        
      BY    GateUser.FullName:                                                                                                              
      LaserFileReplacementHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   chrExtraFieldList = TRIM(chrExtraFieldList,",").
   
   {webGetOptionalFormFields.i pLaserFileReplacementHistoryDetailsFields}
   
   /* Add Hidden Fields*/
   LaserFileReplacementHistoryDetailsForm:insertHiddenField("laserfilereplacementhistory_browse_scroll", "").
   LaserFileReplacementHistoryDetailsForm:insertHiddenField("form_name", "laserfilereplacementhistory_details_form").
   LaserFileReplacementHistoryDetailsForm:insertHiddenField("prog_name", "adLaserFileAdmin.p").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileReplacementHistoryDetailsForm}

   /* Create Button Bar */
   LaserFileReplacementHistoryDetailsButtons = NEW buttonBar().

   LaserFileReplacementHistoryDetailsButtons:addButton("laserfilereplacementhistory_details_form_btn_cancel",
                                                       fTL("Cancel"),
                                                       "disablePopup('laserfilereplacementhistory_details_form_popup');").
   LaserFileReplacementHistoryDetailsButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   LaserFileReplacementHistoryDetailsForm:FormButtons = LaserFileReplacementHistoryDetailsButtons.

   LaserFileReplacementHistoryDetailsForm:endForm().

   LaserFileReplacementHistoryDetailsForm:displayForm().

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
      chrLaserFileID                            = get-value("LaserFileID")
      intSelectedLaserFile                      = INTEGER(chrLaserFileID)
      chrScrollToLaserFileRow                   = STRING(INTEGER(get-value("laserfile_browse_scroll"))) + ";"
      /*History details button*/
      chrLaserFileHistoryID                     = get-value("LaserFileHistoryID")
      intSelectedLaserFileHistory               = INTEGER(chrLaserFileHistoryID)
      chrScrollToLaserFileHistoryRow            = STRING(INTEGER(get-value("laserfilehistory_browse_scroll"))) + ";"
      /*Replacement details button*/
      chrLaserFileReplacementID                 = get-value("LaserFileReplacementID")
      intSelectedLaserFileReplacement           = INTEGER(chrLaserFileReplacementID)
      chrScrollToLaserFileReplacementRow        = STRING(INTEGER(get-value("laserfilereplacement_browse_scroll"))) + ";"
      /*Replacement history details button*/
      chrLaserFileReplacementHistoryID          = get-value("LaserFileReplacementHistoryID")
      intSelectedLaserFileReplacementHistory    = INTEGER(chrLaserFileReplacementHistoryID)
      chrScrollToLaserFileReplacementHistoryRow = STRING(INTEGER(get-value("laserfilereplacementhistory_browse_scroll"))) + ";".
          
   /* Process URL values */
   IF chrLaserFileID <> "" THEN
      chrLaserFileRow = 'selectLaserFileRow(document.getElementById("laserfile_browse_row_'
                        + chrLaserFileID + '"),"' + chrLaserFileID +  '");'.
                                                          
   IF get-value('popup_laserfilehistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("laserfilehistory_browse_form_popup");'.                                                          
   
   IF get-value('popup_laserfilereplacement_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("laserfilereplacement_browse_form_popup");'.
     
   IF get-value('popup_laserfilereplacementhistory_browse') = "Yes" THEN
      chrPopupHistory = 'enablePopup("laserfilereplacementhistory_browse_form_popup");'.                                                             
   
   /* Build the Body Load commands before rendering the page */
   chrBodyLoad = chrBodyLoad + 'document.getElementById("laserfile_browse").scrollTop=' 
                 + chrScrollToLaserFileRow + chrLaserFileRow + chrPopUpHistory.
   
   /*Mandatory include to set opening HTML tags and default files*/
   ThisPage               = NEW HTMLPage().
   ThisPage:WebStream     = STREAM WebStream:HANDLE.
   ThisPage:ThisSessionID = get-value("SESSIONID").
   ThisPage:ParentMenu    = get-value("menu_no").
   ThisPage:PageTitle     = "Laser File Admin".
   ThisPage:FrameTitle    = "Laser File Admin".
   ThisPage:OnBodyLoad    = chrBodyLoad.
   
   /* Include CSS Files */
   {webCssFiles.i}
   
   /* Include JavaScript Files */
   {webJavascriptFiles.i}
   
   /* js File especially for Kitting Station */
   ThisPage:addJavaScript("laserfileadmin.js").
   
   ThisPage:PageHeader().
   
   /* This gets the Session record and sets some Global variables - page validation and security check has already been done above */
   {webValidateWebSession.i}
   
   /******* Main Browser ********************/
   RUN pLaserFileBrowse.
   
   /******* Popup Browsers and Forms ********/    
   RUN pLaserFileDetails.
   
   RUN pLaserFileHistoryBrowse.  
   RUN pLaserFileHistoryDetails.
   
   RUN pLaserFileReplacementBrowse.
   RUN pLaserFileReplacementDetails.
   
   RUN pLaserFileReplacementHistoryBrowse. 
   RUN pLaserFileReplacementHistoryDetails.
   
   /* Writes the HTML for 2 popup Message Boxes - one with Yes/No buttons for confirmation and a standard Alert Box */
   {webGetAlertBoxes.i}
   
   ThisPage:PageFooter().  
   
   /* Calls JavaScript to display popup errors, either returning from a back end update program or from building the page. */
   /* Also displays an informative message being returned from a back end update in the info_message token.                */
   {webPageMessaging.i}
   
   /* Deletes objects defined in defWebDefinitions.i */
   {webObjectCleanup.i}
   
   /* Delete objects defined locally */
   DELETE OBJECT LaserFileBrowseFrame                       NO-ERROR.
   DELETE OBJECT LaserFileBrowse                            NO-ERROR.
   DELETE OBJECT LaserFileBrowseButtons                     NO-ERROR.
   DELETE OBJECT LaserFileDetailsForm                       NO-ERROR.
   DELETE OBJECT LaserFileDetailsButtons                    NO-ERROR.
   
   DELETE OBJECT LaserFileHistoryBrowseForm                 NO-ERROR.
   DELETE OBJECT LaserFileHistoryBrowse                     NO-ERROR.
   DELETE OBJECT LaserFileHistoryDetailsForm                NO-ERROR.
   DELETE OBJECT LaserFileHistoryDetailsButtons             NO-ERROR.
   DELETE OBJECT LaserFileHistoryButtons                    NO-ERROR.

   DELETE OBJECT LaserFileReplacementBrowseForm             NO-ERROR. 
   DELETE OBJECT LaserFileReplacementBrowse                 NO-ERROR.
   DELETE OBJECT LaserFileReplacementDetailsForm            NO-ERROR.
   DELETE OBJECT LaserFileReplacementDetailsButtons         NO-ERROR.
   DELETE OBJECT LaserFileReplacementButtons                NO-ERROR.

   DELETE OBJECT LaserFileReplacementHistoryBrowseForm      NO-ERROR. 
   DELETE OBJECT LaserFileReplacementHistoryBrowse          NO-ERROR.
   DELETE OBJECT LaserFileReplacementHistoryDetailsForm     NO-ERROR.
   DELETE OBJECT LaserFileReplacementHistoryDetailsButtons  NO-ERROR.
   DELETE OBJECT LaserFileReplacementHistoryButtons         NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigBrowse Procedure 
PROCEDURE pLaserFileBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
   
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var */
   {webGetWebForm.i "laserfile_details_form"}
   
   LaserFileBrowse               = NEW browseTable("laserfile_browse").
   LaserFileBrowse:BrowseWidth   = 965.
   LaserFileBrowse:BrowseHeight  = 455.
   LaserFileBrowse:WebStream     = STREAM WebStream:HANDLE.
   
   /* Add in the Station ID as first Column */
   LaserFileBrowse:insertColumn(fTL(" Laser File ID"), 100, "INTEGER", FALSE).
 
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LaserFile}
   
   LaserFileBrowse:insertColumn(fTL("File Name"),           120, "CHARACTER", "LEFT", FALSE).
   LaserFileBrowse:insertColumn(fTL("Laser Type"),          100, "CHARACTER", "LEFT", FALSE).
   LaserFileBrowse:insertColumn(fTL("Body Length"),         120, "DECIMAL",   "LEFT", FALSE).
   LaserFileBrowse:insertColumn(fTL("Num Columns"),         120, "DECIMAL",   "LEFT", FALSE).
   LaserFileBrowse:insertColumn(fTL("Active"),              100, "LOGICAL",   "LEFT", FALSE).
   LaserFileBrowse:insertColumn(fTL("Delete When Printed"), 130, "LOGICAL",   "LEFT", FALSE).
   LaserFileBrowse:insertColumn(fTL("Num Copies To Print"), 130, "DECIMAL",   "LEFT", FALSE).
   
   /*Body*/
   LaserFileBrowse:startBody().
   
   FOR EACH LaserFile NO-LOCK: /*idx=LaserFileID*/          
      LaserFileBrowse:startRow(LaserFile.LaserFileID, "selectLaserFileRow(this," + '"' 
                               + STRING(LaserFile.LaserFileID) + '"' + ");", "").
                                     
      LaserFileBrowse:insertData(LaserFile.LaserFileID).
      
      /* Add in Optional & Customer Specific fields according to the WebFormFields associated with this WebForm */
      {webGetOptionalBrowseFields.i LaserFile}      
 
      LaserFileBrowse:insertData(STRING(LaserFile.FileName),                    "LEFT").
      LaserFileBrowse:insertData(STRING(LaserFile.LaserType),                   "LEFT"). 
      LaserFileBrowse:insertData(LaserFile.BodyLength,                          "LEFT").
      LaserFileBrowse:insertData(LaserFile.NumColumns,                          "LEFT").
      LaserFileBrowse:insertData(STRING(LaserFile.Active,            "Yes/No"), "LEFT").
      LaserFileBrowse:insertData(STRING(LaserFile.DeleteWhenPrinted, "Yes/No"), "LEFT").
      LaserFileBrowse:insertData(LaserFile.NumCopiesToPrint,                    "LEFT").
            
      /* Add hidden fields */
      LaserFileBrowse:insertHiddenData("LaserFileVersionID",LaserFile.VersionID).
      
      LaserFileBrowse:endRow().
      
   END. /*FOR EACH LaserFileAdmin NO-LOCK */
   
   LaserFileBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LaserFileBrowse:getErrors().
   
   /* Create a new frame */
   LaserFileBrowseFrame = NEW pageFrame().
   LaserFileBrowseFrame:WebStream = STREAM WebStream:HANDLE.
   
   /* Insert a form */
   LaserFileBrowseFrame:FormAction="dbLaserFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   LaserFileBrowseFrame:formOpen("laserfile_browse_form").
   
   /* Start the Frame Header */
   LaserFileBrowseFrame:insertSpacer(5).
   LaserFileBrowseFrame:frameOpen(985, 500, "").  
   
   /* This outputs the Browse Table */  
   LaserFileBrowse:displayBrowse().  
   
   /* End the Frame Header */
   LaserFileBrowseFrame:frameClose().
   LaserFileBrowseFrame:insertSpacer(10).
   
   LaserFileBrowseFrame:insertHiddenField("laserfile_browse_scroll","").
   LaserFileBrowseFrame:insertHiddenField("LaserFileID","").
   LaserFileBrowseFrame:insertHiddenField("LaserFileVersionID","").
   LaserFileBrowseFrame:insertHiddenField("form_name","laserfile_browse_form").
   LaserFileBrowseFrame:insertHiddenField("popup_laserfilehistory_browse","").
   LaserFileBrowseFrame:insertHiddenField("popup_laserfilereplacement_browse","").
   LaserFileBrowseFrame:insertHiddenField("prog_name","adLaserFileAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileBrowseFrame}
   
   LaserFileBrowseFrame:formClose().
   
   /* DataMigration Security Settings */
   {webDataMigrationSecurity.i}
   
   /* Create Button Bar */
   LaserFileBrowseButtons = NEW buttonBar().
   LaserFileBrowseButtons:WebStream = STREAM WebStream:HANDLE.   
   
   LaserFileBrowseButtons:addButton("laserfile_browse_form_btn_details",
                                    fTL("Details"),
                                    "viewLaserFileDetails('laserfile_details_form');",
                                    (IF intSelectedLaserFile > 0 THEN "" ELSE "Disabled")).
   
   IF NOT logPreventDataCreates THEN
   DO:
      LaserFileBrowseButtons:addButton("laserfile_browse_form_btn_create",
                                       fTL("Create"),
                                       "createLaserFile('laserfile_details_form');","").
   END.
   
   LaserFileBrowseButtons:addButton("laserfile_browse_form_btn_history",
                                    fTL("History"),
                                    "viewLaserFileHistory('laserfilehistory_browse_form');",
                                    (IF intSelectedLaserFile > 0 THEN "" ELSE "Disabled")).
   
   LaserFileBrowseButtons:addButton("laserfile_browse_form_btn_replacement",
                                    fTL("Replacement"),
                                    "viewLaserFileReplacement('laserfilereplacement_browse_form');",
                                    (IF intSelectedLaserFile > 0 THEN "" ELSE "Disabled")).
   
   LaserFileBrowseButtons:closeBar().  
   LaserFileBrowseButtons:displayButtonBar().  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetails) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetails Procedure 
PROCEDURE pLaserFileDetails :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "laserfile_details_form"}
   ASSIGN 
      chrDisplayFieldList  = "LaserFileID,FileName,LaserType,TemplateFileName,FilePrefix,BodyLength," 
                             + "NumColumns,Extension,Active,DeleteWhenPrinted,NumCopiesToPrint"
      chrEditFieldList     = "LaserType,TemplateFileName,FilePrefix,BodyLength," 
                             + "NumColumns,Extension,Active,DeleteWhenPrinted,NumCopiesToPrint"
      chrNewFieldList      = "FileName,LaserType,TemplateFileName,FilePrefix,BodyLength," 
                             + "NumColumns,Extension,Active,DeleteWhenPrinted,NumCopiesToPrint"
      chrRequiredFieldList = ""
      chrExtraFieldList    = ""
      chrValidateFieldList = "".

   
   LaserFileDetailsForm = NEW dataForm("laserfile_details_form").
   LaserFileDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LaserFileDetailsForm:FormAction = "dbLaserFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LaserFileDetailsForm:FormWidth   = 460.
   LaserFileDetailsForm:FormHeight  = 300.
   LaserFileDetailsForm:FormTitle   = "Laser File Admin Details".
   LaserFileDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   LaserFileDetailsForm:insertPaddingColumn(30).
   LaserFileDetailsForm:insertColumn(185).
   
   /* Fields */
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Laser File ID")).
   LaserFileDetailsForm:insertTextField("LaserFileID", "", 100, TRUE).
    
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("File Name")).
   LaserFileDetailsForm:insertTextField("FileName", "", 100, TRUE).  
   
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Laser Type")).
   LaserFileDetailsForm:insertTextField("LaserType", "", 100, TRUE).  
   
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Template File Name")).
   LaserFileDetailsForm:insertTextField("TemplateFileName", "", 168, TRUE).

   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("File Prefix")).
   LaserFileDetailsForm:insertTextField("FilePrefix", "", 100, TRUE).
   
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Body Length")).
   LaserFileDetailsForm:insertTextField("BodyLength", "", 100, TRUE).
   
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Num Columns")).
   LaserFileDetailsForm:insertTextField("NumColumns", "", 100, TRUE).
   
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Extension")).
   LaserFileDetailsForm:insertTextField("Extension", "", 100, TRUE).
   
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Active")).
   LaserFileDetailsForm:insertComboField("Active", "", 168, TRUE).
   LaserFileDetailsForm:insertComboPairs("Active", "yes", "Yes").    
   LaserFileDetailsForm:insertComboPairs("Active", "no", "No"). 
   
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Delete When Printed")).
   LaserFileDetailsForm:insertComboField("DeleteWhenPrinted", "", 168, TRUE).
   LaserFileDetailsForm:insertComboPairs("DeleteWhenPrinted", "yes", "Yes").    
   LaserFileDetailsForm:insertComboPairs("DeleteWhenPrinted", "no", "No"). 
         
   LaserFileDetailsForm:startRow().
   LaserFileDetailsForm:insertLabel(fTL("Num Copies To Print")).
   LaserFileDetailsForm:insertTextfield("NumCopiesToPrint", "", 100, TRUE).
   
   {webGetOptionalFormFields.i pLaserFileDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LaserFileDetailsForm:insertHiddenField("laserfile_browse_scroll", "").
   LaserFileDetailsForm:insertHiddenField("form_name", "laserfile_details_form").
   LaserFileDetailsForm:insertHiddenField("prog_name", "adLaserFileAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileDetailsForm}
   
   /* Create Button Bar */
   LaserFileDetailsButtons = NEW buttonBar().
   IF NOT logPreventDataUpdates THEN
   DO:
      LaserFileDetailsButtons:addButton("laserfile_details_form_btn_save", 
                                        fTL("Save"), 
                                        "updateLaserFile('laserfile_details_form');").
   END.
   LaserFileDetailsButtons:addButton("laserfile_details_form_btn_cancel", 
                                     fTL("Cancel"), 
                                     "cancelUpdate('UserCancelled','process_mode');" + 
                                     "disablePopup('laserfile_details_form_popup');").
   LaserFileDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LaserFileDetailsForm:FormButtons = LaserFileDetailsButtons.
   
   LaserFileDetailsForm:endForm(). 
   
   LaserFileDetailsForm:displayForm(). 
  
END PROCEDURE.


PROCEDURE pLaserFileDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            LaserFileDetailsForm:startRow().
            LaserFileDetailsForm:insertLabel(fTL("Field Label")).
            LaserFileDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigHistoryBrowse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigHistoryBrowse Procedure 

PROCEDURE pLaserFileHistoryDetails :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   /* This finds the WebForm which is linked to the Form that is passed in and Program in chrThisProgramName var for use below */
   {webGetWebForm.i "laserfilehistory_details_form"}
   ASSIGN 
      chrDisplayFieldList  = "LaserFileHistoryID,FileName,LaserType,TemplateFileName,FilePrefix,BodyLength," 
                             + "NumColumns,Extension,Active,DeleteWhenPrinted,NumCopiesToPrint,CreatedDate,"
                             + "TransactionID,OperationTypeID,GateUserID,CreatedHour,CreatedMins"
      chrEditFieldList     = ""
      chrNewFieldList      = ""
      chrRequiredFieldList = ""
      chrExtraFieldList    = ""
      chrValidateFieldList = "".
   
   LaserFileHistoryDetailsForm = NEW dataForm("laserfilehistory_details_form").
   LaserFileHistoryDetailsForm:WebStream = STREAM WebStream:HANDLE.
   
   LaserFileHistoryDetailsForm:FormAction = "dbLaserFileUpdate.p?" + TRIM(chrGblDefaultUrlValues,"&").
   
   /* Setup */
   LaserFileHistoryDetailsForm:FormWidth   = 460.
   LaserFileHistoryDetailsForm:FormHeight  = 300.
   LaserFileHistoryDetailsForm:FormTitle   = "Laser File Admin Details".
   LaserFileHistoryDetailsForm:FormType    = "medium".
   
   /* Column Layout */
   LaserFileHistoryDetailsForm:insertPaddingColumn(30).
   LaserFileHistoryDetailsForm:insertColumn(150).
   LaserFileHistoryDetailsForm:insertColumn(125).
   LaserFileHistoryDetailsForm:insertColumn(20).
   LaserFileHistoryDetailsForm:insertColumn(4).
   LaserFileHistoryDetailsForm:insertColumn(20).
   
   /* Fields */
   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("Laser File History ID")).
   LaserFileHistoryDetailsForm:insertTextField("LaserFileHistoryID", "", 100, TRUE).
   
   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("File Name")).
   LaserFileHistoryDetailsForm:insertTextField("FileName", "", 100, TRUE).

   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("Body Length")).
   LaserFileHistoryDetailsForm:insertTextField("BodyLength", "", 100, TRUE).
   
   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("Num Columns")).
   LaserFileHistoryDetailsForm:insertTextField("NumColumns", "", 100, TRUE).
    
   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("Active")).
   LaserFileHistoryDetailsForm:insertComboField("Active", "", 168, TRUE).
   LaserFileHistoryDetailsForm:insertComboPairs("Active", "yes", "Yes").    
   LaserFileHistoryDetailsForm:insertComboPairs("Active", "no", "No"). 
   
   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("Delete When Printed")).
   LaserFileHistoryDetailsForm:insertComboField("DeleteWhenPrinted", "", 168, TRUE).
   LaserFileHistoryDetailsForm:insertComboPairs("DeleteWhenPrinted", "yes", "Yes").    
   LaserFileHistoryDetailsForm:insertComboPairs("DeleteWhenPrinted", "no", "No"). 
   
   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("Num Copies To Print")).
   LaserFileHistoryDetailsForm:insertTextField("NumCopiesToPrint", "", 100, TRUE).

   LaserFileHistoryDetailsForm:startRow().                                                                                           
   LaserFileHistoryDetailsForm:insertLabel(fTL("Created")).                                                                          
   LaserFileHistoryDetailsForm:insertDateField("CreatedDate", "", 100, TRUE).                                                        
   /* Time fields have no label*/                                                                                                        
   LaserFileHistoryDetailsForm:insertTextField("CreatedHour", "00", 18, TRUE).                                                       
   /* This has a label to separate the time */                                                                                           
   LaserFileHistoryDetailsForm:insertLabel(":").                                                                                     
   LaserFileHistoryDetailsForm:insertTextField("CreatedMins", "00", 18, TRUE).                                                       
                                                                                                                         
   LaserFileHistoryDetailsForm:startRow().
   LaserFileHistoryDetailsForm:insertLabel(fTL("Transaction ID")).
   LaserFileHistoryDetailsForm:insertTextField("TransactionID", "", 100, TRUE).
                                                                                                                                       
   LaserFileHistoryDetailsForm:startRow().                                                                                           
   LaserFileHistoryDetailsForm:insertLabel(fTL("Operation Type ID")).                                                                
   LaserFileHistoryDetailsForm:insertComboField("OperationTypeID", "", 168, TRUE).                                                   
   FOR EACH OperationType NO-LOCK  /*idx=OperationTypeID*/                                                                                                      
      BY    OperationType.OperationTypeID:                                                                                                  
      LaserFileHistoryDetailsForm:insertComboPairs("OperationTypeID", STRING(OperationType.OperationTypeID),
                                                   OperationType.TypeName).
   END. /*FOR EACH OperationType NO-LOCK*/                                                                                                                                 
                                                                                                                                         
   LaserFileHistoryDetailsForm:startRow().                                                                                           
   LaserFileHistoryDetailsForm:insertLabel(fTL("Gate User ID")).                                                                     
   LaserFileHistoryDetailsForm:insertComboField("GateUserID", "", 168, TRUE).                                                        
   FOR EACH GateUser NO-LOCK  /*idx=GateUserID*/                                                                                                        
      BY    GateUser.FullName:                                                                                                              
      LaserFileHistoryDetailsForm:insertComboPairs("GateUserID", STRING(GateUser.GateUserID), GateUser.FullName).                    
   END.  /*FOR EACH GateUser NO-LOCK*/                                                                                                                                

   {webGetOptionalFormFields.i pLaserFileHistoryDetailsFields}
   
   chrExtraFieldList = TRIM(chrExtraFieldList,",").  
   
   /* Add Hidden Fields*/
   LaserFileHistoryDetailsForm:insertHiddenField("laserfilehistory_browse_scroll", "").
   LaserFileHistoryDetailsForm:insertHiddenField("form_name", "laserfilehistory_details_form").
   LaserFileHistoryDetailsForm:insertHiddenField("prog_name", "adLaserFileAdmin.p").
   
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileHistoryDetailsForm}
   
   /* Create Button Bar */
   LaserFileHistoryDetailsButtons = NEW buttonBar().

   LaserFileHistoryDetailsButtons:addButton("laserfilehistory_details_form_btn_cancel", 
                                            fTL("Cancel"), 
                                            "disablePopup('laserfilehistory_details_form_popup');").
   LaserFileHistoryDetailsButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LaserFileHistoryDetailsForm:FormButtons = LaserFileHistoryDetailsButtons.
   
   LaserFileHistoryDetailsForm:endForm(). 
   
   LaserFileHistoryDetailsForm:displayForm(). 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pKittingConfigDetailsFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pKittingConfigDetailsFields Procedure 

PROCEDURE pLaserFileHistoryDetailsFields :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/
  
   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.
  
   CASE chrOption:
    
      WHEN "FieldName" THEN
         DO:
            LaserFileDetailsForm:startRow().
            LaserFileDetailsForm:insertLabel(fTL("Field Label")).
            LaserFileDetailsForm:insertTextField("FieldName", "", 200, TRUE).  
         END. /*WHEN "FieldName" THEN*/
    
   END CASE. /*chrOption:*/
  
END PROCEDURE.

PROCEDURE pLaserFileHistoryBrowse :
   /*------------------------------------------------------------------------------
     Purpose:     
     Parameters:  <none>
     Notes:       
   ------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   LaserFileHistoryBrowseForm           = NEW dataForm("laserfilehistory_browse_form").
   LaserFileHistoryBrowseForm:WebStream = STREAM WebStream:HANDLE.
   
   /* Setup */
   LaserFileHistoryBrowseForm:FormWidth  = 860.
   LaserFileHistoryBrowseForm:FormHeight = 530.
   LaserFileHistoryBrowseForm:FormTitle  = fTL("Laser File Admin History").
   LaserFileHistoryBrowseForm:FormType   = "xxl_large".
   LaserFileHistoryBrowse                = NEW browseTable("laserfilehistory_browse").
   LaserFileHistoryBrowse:BrowseWidth    = 840.
   LaserFileHistoryBrowse:BrowseHeight   = 490.
   
   LaserFileHistoryBrowse:insertColumn(fTL("History ID"),           80,   "INTEGER",             FALSE).
   LaserFileHistoryBrowse:insertColumn(fTL("File Name"),            130,  "CHARACTER",   "LEFT", FALSE).
   LaserFileHistoryBrowse:insertColumn(fTL("Laser Type"),           80,   "CHARACTER",   "LEFT", FALSE).
   LaserFileHistoryBrowse:insertColumn(fTL("Active"),               70,   "LOGICAL",     "LEFT", FALSE).
   LaserFileHistoryBrowse:insertColumn(fTL("Delete When Printed"),  130,  "LOGICAL",     "LEFT", FALSE).
   LaserFileHistoryBrowse:insertColumn(fTL("Num Copies To Print"),  130,  "DECIMAL",     "LEFT", FALSE).
   LaserFileHistoryBrowse:insertColumn(fTL("Gate User ID"),         100,  "CHARACTER",   "LEFT", FALSE).
   LaserFileHistoryBrowse:insertColumn(fTL("File Prefix"),          100,  "DATE",        "LEFT", FALSE).
   
   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LaserFileHistory}
   
   LaserFileHistoryBrowse:StartBody().
   
   FOR EACH LaserFileHistory NO-LOCK /*idx=LaserFileID and LaserFileHistoryID*/
      WHERE LaserFileHistory.LaserFileID = intSelectedLaserFile
      BY    LaserFileHistory.LaserFileHistoryID:

      FIND FIRST GateUser OF LaserFileHistory NO-LOCK NO-ERROR. /*idx=GateUserID*/

      LaserFileHistoryBrowse:startRow (LaserFileHistory.LaserFileHistoryID,
                                       "selectLaserFileHistoryRow(this," + '"' + 
                                       STRING(LaserFileHistory.LaserFileHistoryID)
                                       + '"' + ");", "").
      /* Add in Optional & Customer Specific fields according to the 
         ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i LaserFileHistory}

      LaserFileHistoryBrowse:insertData(LaserFileHistory.LaserFileHistoryID,                              "").
      LaserFileHistoryBrowse:insertData(STRING(LaserFileHistory.FileName),                            "LEFT").
      LaserFileHistoryBrowse:insertData(STRING(LaserFileHistory.LaserType),                           "LEFT").
      LaserFileHistoryBrowse:insertData(STRING(LaserFileHistory.Active,             "Yes/No"),        "LEFT").
      LaserFileHistoryBrowse:insertData(STRING(LaserFileHistory.DeleteWhenPrinted,  "Yes/No"),        "LEFT").
      LaserFileHistoryBrowse:insertData(LaserFileHistory.NumCopiesToPrint,                            "LEFT").
      LaserFileHistoryBrowse:insertData(STRING(IF AVAILABLE GateUser THEN GateUser.FullName ELSE ""), "LEFT").
      LaserFileHistoryBrowse:insertData(LaserFileHistory.FilePrefix,                                  "LEFT").

      LaserFileHistoryBrowse:endRow().
   END. /* FOR EACH LaserFileHistory */
   
   LaserFileHistoryBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LaserFileHistoryBrowse:getErrors().
   
   LaserFileHistoryBrowseForm:insertHiddenField("popup_laserfilehistory_browse","").
   LaserFileHistoryBrowseForm:insertHiddenField("LaserFileHistoryID","").
      
   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileHistoryBrowseForm}
   
   /* Create Button Bar */
   LaserFileHistoryButtons = NEW buttonBar().
   
   LaserFileHistoryButtons:addButton("laserfilehistory_browse_form_btn_details",
                                     fTL("Details"),
                                     "viewLaserFileHistoryDetails('laserfilehistory_details_form');",
                                     (IF intSelectedLaserFileHistory > 0 THEN "" ELSE "Disabled")).    
                                                                       
   LaserFileHistoryButtons:addButton("laserfilehistory_browse_form_btn_cancel",
                                     fTL("Cancel"),
                                     "disablePopup('laserfilehistory_browse_form_popup');").
   LaserFileHistoryButtons:closeBar().  
   
   /* Assign the Button Bar Object to the Form Object */
   LaserFileHistoryBrowseForm:FormBrowse  = LaserFileHistoryBrowse.
   LaserFileHistoryBrowseForm:FormButtons = LaserFileHistoryButtons.
   LaserFileHistoryBrowseForm:endForm(). 
   
   LaserFileHistoryBrowseForm:displayForm().   
END PROCEDURE.

PROCEDURE pLaserFileReplacementFields :
   /*------------------------------------------------------------------------------
     Purpose:
     Parameters:  <none>
     Notes:
   ------------------------------------------------------------------------------*/

   DEFINE INPUT PARAMETER chrOption AS CHARACTER NO-UNDO.

   CASE chrOption:

      WHEN "FieldName" THEN
         DO:
            LaserFileReplacementDetailsForm:startRow().
            LaserFileReplacementDetailsForm:insertLabel(fTL("Field Label")).
            LaserFileReplacementDetailsForm:insertTextField("FieldName", "", 200, TRUE).
         END. /*WHEN "FieldName" THEN*/

   END CASE. /*chrOption:*/

END PROCEDURE.

PROCEDURE pLaserFileReplacementBrowse :
   /*------------------------------------------------------------------------------
     Purpose:
     Parameters:  <none>
     Notes:
   ------------------------------------------------------------------------------*/

   /* This finds the ProcessEvent which is linked to the form associated with this browse */
   LaserFileReplacementBrowseForm            = NEW dataForm("laserfilereplacement_browse_form").
   LaserFileReplacementBrowseForm:WebStream  = STREAM WebStream:HANDLE.

   /* Setup */
   LaserFileReplacementBrowseForm:FormWidth  = 860.
   LaserFileReplacementBrowseForm:FormHeight = 530.
   LaserFileReplacementBrowseForm:FormTitle  = fTL("Laser File Admin Replacement").
   LaserFileReplacementBrowseForm:FormType   = "xxl_large".
   LaserFileReplacementBrowse                = NEW browseTable("laserfilereplacement_browse").
   LaserFileReplacementBrowse:BrowseWidth    = 840.
   LaserFileReplacementBrowse:BrowseHeight   = 490.

   LaserFileReplacementBrowse:insertColumn(fTL("Replacement ID"),   100, "INTEGER",           FALSE).
   LaserFileReplacementBrowse:insertColumn(fTL("Replaced Text"),    100, "CHARACTER", "LEFT", FALSE).
   LaserFileReplacementBrowse:insertColumn(fTL("Replacement Text"), 120, "CHARACTER", "LEFT", FALSE).
   LaserFileReplacementBrowse:insertColumn(fTL("Country"),          100, "CHARACTER", "LEFT", FALSE).
   LaserFileReplacementBrowse:insertColumn(fTL("Language"),         100, "CHARACTER", "LEFT", FALSE).
   LaserFileReplacementBrowse:insertColumn(fTL("Customer"),         100, "CHARACTER", "LEFT", FALSE).
   LaserFileReplacementBrowse:insertColumn(fTL("Replace By"),       100, "CHARACTER", "LEFT", FALSE).
   LaserFileReplacementBrowse:insertColumn(fTL("Active"),           100, "LOGICAL",   "LEFT", FALSE).


   /* Add in Core Db Optional field labels & Customer Specific Cust Db field labels according to the Process Options */
   {webGetOptionalBrowseHeaders.i LaserFileReplacement}

   LaserFileReplacementBrowse:StartBody().

   FOR EACH LaserFileReplacement NO-LOCK /*idx=LaserFileID and LaserFileReplacementID*/
      WHERE LaserFileReplacement.LaserFileID = intSelectedLaserFile
      BY    LaserFileReplacement.LaserFileReplacementID:
            
      FIND FIRST Country       OF LaserFileReplacement NO-LOCK NO-ERROR. /*idx=CountryID*/
      FIND FIRST Customer      OF LaserFileReplacement NO-LOCK NO-ERROR. /*idx=CustomerID*/
      FIND FIRST gate.Language OF LaserFileReplacement NO-LOCK NO-ERROR. /*idx=LanguageID*/
      
      LaserFileReplacementBrowse:startRow (LaserFileReplacement.LaserFileReplacementID,
                                           "selectLaserFileReplacementRow(this," + '"' + 
                                           STRING(LaserFileReplacement.LaserFileReplacementID) + 
                                           '"' + ");", "").
                                           
      /* Add in Optional & Customer Specific fields according to the ProcessOptionFields associated with this ProcessEvent */
      {webGetOptionalBrowseFields.i LaserFileReplacement}

      LaserFileReplacementBrowse:insertData(LaserFileReplacement.LaserFileReplacementID,                                    "").
      LaserFileReplacementBrowse:insertData(STRING(LaserFileReplacement.TextToBeReplaced),                              "LEFT").
      LaserFileReplacementBrowse:insertData(STRING(LaserFileReplacement.ReplacementText),                               "LEFT").
      LaserFileReplacementBrowse:insertData(STRING(IF AVAILABLE Country THEN Country.CountryName ELSE ""),              "LEFT").
      LaserFileReplacementBrowse:insertData(STRING(IF AVAILABLE gate.Language THEN gate.Language.LanguageName ELSE ""), "LEFT").
      LaserFileReplacementBrowse:insertData(STRING(IF AVAILABLE Customer THEN Customer.CustomerName ELSE ""),           "LEFT").
      LaserFileReplacementBrowse:insertData(STRING(LaserFileReplacement.ReplaceBy),                                     "LEFT").
      LaserFileReplacementBrowse:insertData(STRING(LaserFileReplacement.Active,            "Yes/No"),                   "LEFT").

      LaserFileReplacementBrowse:endRow().
   END. /* FOR EACH LaserFileReplacement */

   LaserFileReplacementBrowse:endTable().
   chrPageBuildError = chrPageBuildError + LaserFileReplacementBrowse:getErrors().

   LaserFileReplacementBrowseForm:insertHiddenField("popup_laserfilereplacement_browse","").
   LaserFileReplacementBrowseForm:insertHiddenField("popup_laserfilereplacementhistory_browse","").
   LaserFileReplacementBrowseForm:insertHiddenField("LaserFileReplacementID","").

   /* This adds all of the standard form field lists to the form */
   {webGetHiddenFormFields.i LaserFileReplacementBrowseForm}

   /* Create Button Bar */
   LaserFileReplacementButtons = NEW buttonBar().

   LaserFileReplacementButtons:addButton("laserfilereplacement_browse_form_btn_details",
                                         fTL("Details"),
                                         "viewLaserFileReplacementDetails('laserfilereplacement_details_form');",
                                         (IF intSelectedLaserFileReplacement > 0 THEN "" ELSE "Disabled")).
   
   LaserFileReplacementButtons:addButton("laserfilereplacement_browse_form_btn_history",
                                         fTL("History"),
                                         "viewLaserFileReplacementHistory('laserfilereplacementhistory_browse_form');",
                                         (IF intSelectedLaserFileReplacement > 0 THEN "" ELSE "Disabled")).                                     

   LaserFileReplacementButtons:addButton("laserfilereplacement_browse_form_btn_cancel",
                                         fTL("Cancel"),
                                         "disablePopup('laserfilereplacement_browse_form_popup');").
   LaserFileReplacementButtons:closeBar().

   /* Assign the Button Bar Object to the Form Object */
   LaserFileReplacementBrowseForm:FormBrowse  = LaserFileReplacementBrowse.
   LaserFileReplacementBrowseForm:FormButtons = LaserFileReplacementButtons.
   LaserFileReplacementBrowseForm:endForm().

   LaserFileReplacementBrowseForm:displayForm().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


