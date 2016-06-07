/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncStatusTypeFunctions.i
Purpose : Functions to make StatusIDs and StatusNames easier to handle amd same for TypeIDs and TypeNames
Author  : BG
Date    : 26th April 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
30/08/2012 BG  WiaB       Changed the fGetStatusID & fGetTypeID functions to find using StatusCode instead of StatusName.
01/02/2013 BG  WiaB       Changed fStepBackStatus to allow the Cancellation of Inbounds.
------------------------------------------------------------------------------------------------------------------------------------------*/

FUNCTION fGetStatusID RETURNS INTEGER (INPUT chrTableName  AS CHARACTER,
                                       INPUT chrStatusCode AS CHARACTER):
   
   DEFINE VARIABLE hdlRecordBuffer        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE intReturnValue         AS INTEGER     NO-UNDO.
   
   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName + "Status".
   logRecordFound = hdlRecordBuffer:FIND-FIRST("WHERE StatusCode = '" + chrStatusCode + "'", NO-LOCK) NO-ERROR.
   
   IF logRecordFound THEN 
      intReturnValue = hdlRecordBuffer:BUFFER-FIELD(chrTableName + "StatusID"):BUFFER-VALUE.
   
   hdlRecordBuffer:BUFFER-RELEASE NO-ERROR.

   DELETE OBJECT hdlRecordBuffer NO-ERROR.
   
   RETURN intReturnValue.
   
END FUNCTION. /*fGetStatusID*/


FUNCTION fGetStatusCode RETURNS CHARACTER (INPUT chrTableName AS CHARACTER,
                                           INPUT intStatusID  AS INTEGER):
   
   DEFINE VARIABLE hdlRecordBuffer        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrReturnValue         AS CHARACTER   NO-UNDO.
   
   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName + "Status".
   logRecordFound = hdlRecordBuffer:FIND-FIRST("WHERE " + chrTableName + "StatusID = " + STRING(intStatusID), NO-LOCK) NO-ERROR.
   
   IF logRecordFound THEN 
      chrReturnValue = hdlRecordBuffer:BUFFER-FIELD("StatusCode"):BUFFER-VALUE.
   
   hdlRecordBuffer:BUFFER-RELEASE NO-ERROR.

   DELETE OBJECT hdlRecordBuffer NO-ERROR.
   
   RETURN chrReturnValue.
   
END FUNCTION. /*fGetStatusName*/


FUNCTION fGetStatusName RETURNS CHARACTER (INPUT chrTableName AS CHARACTER,
                                           INPUT intStatusID  AS INTEGER):
   
   DEFINE VARIABLE hdlRecordBuffer        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrReturnValue         AS CHARACTER   NO-UNDO.
   
   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName + "Status".
   logRecordFound = hdlRecordBuffer:FIND-FIRST("WHERE " + chrTableName + "StatusID = " + STRING(intStatusID), NO-LOCK) NO-ERROR.
   
   IF logRecordFound THEN 
      chrReturnValue = hdlRecordBuffer:BUFFER-FIELD("StatusName"):BUFFER-VALUE.
   
   hdlRecordBuffer:BUFFER-RELEASE NO-ERROR.

   DELETE OBJECT hdlRecordBuffer NO-ERROR.
   
   RETURN chrReturnValue.
   
END FUNCTION. /*fGetStatusName*/


FUNCTION fGetStatusNameList RETURNS CHARACTER (INPUT chrTableName  AS CHARACTER,
                                               INPUT chrStatusList AS CHARACTER):
   
   DEFINE VARIABLE hdlRecordBuffer        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logFound               AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrReturnValue         AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE intCount               AS INTEGER     NO-UNDO.
   
   DO intCount = 1 TO NUM-ENTRIES(chrStatusList):
      
      CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName + "Status".
      logFound = hdlRecordBuffer:FIND-FIRST("WHERE " + chrTableName + "StatusID = " + ENTRY(intCount, chrStatusList), NO-LOCK) NO-ERROR.
      
      IF logFound THEN 
         chrReturnValue = hdlRecordBuffer:BUFFER-FIELD("StatusName"):BUFFER-VALUE + "/" + chrReturnValue.
      
      hdlRecordBuffer:BUFFER-RELEASE NO-ERROR.

      DELETE OBJECT hdlRecordBuffer NO-ERROR.
      
   END.
   
   RETURN TRIM(chrReturnValue,"/").
   
END FUNCTION. /*fGetStatusName*/


FUNCTION fGetTypeID RETURNS INTEGER (INPUT chrTableName AS CHARACTER,
                                     INPUT chrTypeCode  AS CHARACTER):
   
   DEFINE VARIABLE hdlRecordBuffer        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE intReturnValue         AS INTEGER     NO-UNDO.
   
   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName + "Type".
   logRecordFound = hdlRecordBuffer:FIND-FIRST("WHERE TypeCode = '" + chrTypeCode + "'", NO-LOCK) NO-ERROR.
   
   IF logRecordFound THEN 
      intReturnValue = hdlRecordBuffer:BUFFER-FIELD(chrTableName + "TypeID"):BUFFER-VALUE.
   
   hdlRecordBuffer:BUFFER-RELEASE NO-ERROR.

   DELETE OBJECT hdlRecordBuffer NO-ERROR.
   
   RETURN intReturnValue.
   
END FUNCTION. /*fGetTypeID*/


FUNCTION fGetTypeCode RETURNS CHARACTER (INPUT chrTableName AS CHARACTER,
                                         INPUT intTypeID    AS INTEGER):
   
   DEFINE VARIABLE hdlRecordBuffer        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrReturnValue         AS CHARACTER   NO-UNDO.
   
   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName + "Type".
   logRecordFound = hdlRecordBuffer:FIND-FIRST("WHERE " + chrTableName + "TypeID = " + STRING(intTypeID), NO-LOCK) NO-ERROR.
   
   IF logRecordFound THEN 
      chrReturnValue = hdlRecordBuffer:BUFFER-FIELD("TypeCode"):BUFFER-VALUE.
   
   hdlRecordBuffer:BUFFER-RELEASE NO-ERROR.

   DELETE OBJECT hdlRecordBuffer NO-ERROR.
   
   RETURN chrReturnValue.
   
END FUNCTION. /*fGetTypeName*/


FUNCTION fGetTypeName RETURNS CHARACTER (INPUT chrTableName AS CHARACTER,
                                         INPUT intTypeID    AS INTEGER):
   
   DEFINE VARIABLE hdlRecordBuffer        AS HANDLE      NO-UNDO.
   DEFINE VARIABLE logRecordFound         AS LOGICAL     NO-UNDO.
   DEFINE VARIABLE chrReturnValue         AS CHARACTER   NO-UNDO.
   
   CREATE BUFFER hdlRecordBuffer FOR TABLE chrTableName + "Type".
   logRecordFound = hdlRecordBuffer:FIND-FIRST("WHERE " + chrTableName + "TypeID = " + STRING(intTypeID), NO-LOCK) NO-ERROR.
   
   IF logRecordFound THEN 
      chrReturnValue = hdlRecordBuffer:BUFFER-FIELD("TypeName"):BUFFER-VALUE.
   
   hdlRecordBuffer:BUFFER-RELEASE NO-ERROR.

   DELETE OBJECT hdlRecordBuffer NO-ERROR.
   
   RETURN chrReturnValue.
   
END FUNCTION. /*fGetTypeName*/


FUNCTION fGetStepBackStatus RETURNS CHARACTER (INPUT intInboundID       AS INTEGER,
                                               INPUT intCurrentStatusID AS INTEGER):
   
   DEFINE VARIABLE chrCurrentStatus    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrStepBackStatus   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE logPartAssigned     AS LOGICAL     NO-UNDO.
   
   /* Keep the scoping local to the function */
   DEFINE BUFFER Inbound  FOR Inbound.
   DEFINE BUFFER Asn      FOR Asn.
   DEFINE BUFFER AsnLine  FOR AsnLine.
   
   FIND FIRST Inbound WHERE Inbound.InboundID = intInboundID NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE Inbound THEN
   DO:
      chrStepBackStatus = "Error: Cannot find Inbound for:" + STRING(intInboundID) + ".".
      RETURN chrStepBackStatus.
   END.
   
   chrCurrentStatus = fGetStatusName("Inbound",intCurrentStatusID).
   
   CASE chrCurrentStatus:
      
      WHEN "Created" THEN 
      DO:
         /*chrStepBackStatus = "Error: Cannot StepBack from 'Created' status.".*/
         chrStepBackStatus = "Cancelled".
      END. /* WHEN "Created" THEN  */
      
      WHEN "PartsAssigned" THEN
      DO:
         PartFindLoop:
         FOR EACH Asn NO-LOCK OF Inbound:
            
            FOR EACH AsnLine NO-LOCK OF Asn:
               
               logPartAssigned = TRUE.
               LEAVE PartFindLoop.
            END.
         END.
         
         IF logPartAssigned THEN
         DO:
            chrStepBackStatus = "Error: Cannot StepBack from 'PartsAssigned' status as at least one Part is assigned.".
            RETURN chrStepBackStatus.
         END.
         
         ASSIGN chrStepBackStatus = "Created".
      END. /*WHEN "PartsAssigned" THEN */     
      
      WHEN "Arrived" THEN
      DO:
         IF CAN-FIND(FIRST StockPackage OF Inbound) THEN
         DO:
            chrStepBackStatus = "Error: Cannot StepBack from 'Arrived' status once Stock has been Received.".
            RETURN chrStepBackStatus.
         END.
         
         ASSIGN chrStepBackStatus = "PartsAssigned".
      END. /*WHEN "Arrived" THEN */     
      
      WHEN "Attached" THEN 
      DO:
         chrStepBackStatus = "Error: Cannot StepBack from 'Attached' Status. Please use 'Detach' option.".
      END. /*WHEN "Attached" THEN */
      
      WHEN "Unloading" THEN 
      DO:
         /*If there is Stock received against the Inbound then don't allow StepBack */
         IF CAN-FIND(FIRST StockPackage OF Inbound NO-LOCK) THEN
         DO:
            chrStepBackStatus = "Error: Cannot StepBack from 'Unloading' status once Stock has been Received.".
            RETURN chrStepBackStatus.
         END.
         
         /* We dunno if it was prev @ Attached (should be) or "Arrived" - a valid LocAttach record is the differentiator */
         IF Inbound.LocationID <> 0 THEN
            chrStepBackStatus = "Attached".
         ELSE
            chrStepBackStatus = "Arrived".
         
      END. /*WHEN "Unloadng" THEN */
      
      WHEN "PreAlert" OR WHEN "Cleared" OR WHEN "RedFlag" OR WHEN "CustomsWait" THEN
      DO:
         chrStepBackStatus = "Error: Cannot StepBack from '" + chrCurrentStatus + "' status.".
         RETURN chrStepBackStatus.
      END. /*WHEN "PreAlert" OR WHEN "Cleared" OR WHEN "RedFlag" THEN */
      
      WHEN "Reconciling" OR WHEN "Reconciled"  OR WHEN "Complete" THEN
      DO:
         chrStepBackStatus = "Error: Cannot StepBack from '" + chrCurrentStatus + "' status.".
         RETURN chrStepBackStatus.
      END.
      
      OTHERWISE
      DO:
         chrStepBackStatus = "Error: No StepBack functionality is avaiable for '" + chrCurrentStatus + "' status.".
         RETURN chrStepBackStatus.
      END.
      
   END CASE. /*CASE chrCurrentStatus:*/
   
   RETURN chrStepBackStatus.   /* Function return value. */
   
END FUNCTION. /*fGetStepBackStatus*/

