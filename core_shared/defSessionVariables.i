/*------------------------------------------------------------------------------------------------------------------------------------------
Program : usrSession.i
Purpose : This is a group of global chared VARs that will be used in both web and character. In web environment they are updated on each 
          page via webPageValidate.i so they behave as if they were truly global even in the stateless environment.
Author  : BG
Date    : 23rd May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
20/12/2013 BG  WiaB       Added chrGblEnvironmentID to link to the new Environment Table
10/02/2014 AB  WiaB       Added chrGblApplicationID
------------------------------------------------------------------------------------------------------------------------------------------*/

/*New Global Shared Variable to Hold Logged in User*/
DEFINE {2} {3} SHARED VARIABLE intGblUserID                AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE intGblSessionID             AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE intGblLanguageID            AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE intGblTransactionID         AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE intGblOperationTypeID       AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE intGblBusinessUnitID        AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblSessionID             AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblIP                    AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblAppNo                 AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE intGblApplicationID         AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblUserName              AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblSystemName            AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblEnvironment           AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE intGblEnvironmentID         AS INTEGER     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblChangeRequestNo       AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblDefaultUrlValues      AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE chrGblUpdateFileName        AS CHARACTER   NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE logGblExternalSession       AS LOGICAL     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE logGblDebugging             AS LOGICAL     NO-UNDO.
DEFINE {2} {3} SHARED VARIABLE logGblMapDebugging          AS LOGICAL     NO-UNDO.

/* Access to the data library */
DEFINE {2} {3} SHARED VARIABLE hdlGblLibrary               AS HANDLE      NO-UNDO.

/* Access to the Session Record TempTable */
DEFINE {2} {3} SHARED TEMP-TABLE ttSessionRecord
   FIELD SessionID AS CHARACTER
   FIELD RecordID  AS RECID
   FIELD TableName AS CHARACTER            
   INDEX RecordID IS UNIQUE RecordID
   INDEX TableName TableName.   

DEFINE {2} {3} SHARED TEMP-TABLE ttSessionField
   FIELD SessionID  AS CHARACTER
   FIELD RecordID   AS RECID
   FIELD TableName  AS CHARACTER         
   FIELD FieldName  AS CHARACTER         
   FIELD FieldValue AS CHARACTER
   FIELD DataType   AS CHARACTER   
   INDEX RecordID RecordID
   INDEX TableName TableName.

/* Defined with the Global Variables but Scope is always local - handles errors */
DEFINE                VARIABLE chrError                    AS CHARACTER   NO-UNDO.

&GLOBAL-DEFINE CLIENT_TYPE {1}

/* Global temp-tables */
DEFINE TEMP-TABLE ttMenuCheck
    FIELD MenuItemID AS INTEGER
    INDEX idx MenuItemID.

