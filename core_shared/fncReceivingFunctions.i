/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncReceivingFunctions.i
Purpose : All functions to do with Receiving
Author  : BG
Date    : 7th Feb 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Temp Tables */
DEFINE TEMP-TABLE ttAsnLine 
   LIKE AsnLine.

/* Buffers */
DEFINE BUFFER checkInbound      FOR Inbound.
DEFINE BUFFER checkAsn          FOR Asn.
DEFINE BUFFER checkAsnLine      FOR AsnLine.
DEFINE BUFFER thisAsnLine       FOR AsnLine.
DEFINE BUFFER checkTask         FOR Task.
DEFINE BUFFER checkTaskLine     FOR TaskLine.
DEFINE BUFFER checkTaskLineWork FOR TaskLineWork.


FUNCTION fGetQtyAllocatedToAsnLine RETURNS INTEGER (INPUT intAsnLineID AS INTEGER):
   
   DEFINE VARIABLE intReceivedSoFar     AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intReconciledSoFar   AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intStillToAllocate   AS INTEGER     NO-UNDO.
   DEFINE VARIABLE intLaterLinesTotal   AS INTEGER     NO-UNDO.
   
   FIND FIRST thisAsnLine NO-LOCK
      WHERE thisAsnLine.AsnLineID = intAsnLineID NO-ERROR.
   IF NOT AVAILABLE thisAsnLine THEN
      RETURN 0.
   
   /* If its already reconciled then just return the QtyReceived */
   IF thisAsnLine.Reconciled <> "" THEN
      RETURN thisAsnLine.QtyReceived.
   
   FIND FIRST checkAsn NO-LOCK
      WHERE checkAsn.AsnID = thisAsnLine.AsnID NO-ERROR.
   IF NOT AVAILABLE checkAsn THEN
      RETURN 0.
   
   FIND FIRST checkInbound NO-LOCK
      WHERE checkInbound.InboundID = checkAsn.InboundID NO-ERROR.
   IF NOT AVAILABLE checkInbound THEN
      RETURN 0.
   
   FIND LAST checkTask OF checkInbound NO-LOCK NO-ERROR.
   IF NOT AVAILABLE checkTask THEN
      RETURN 0.
   
   /* Get the Total received so far for the Part on this Inbound */
   FOR EACH checkTaskLine OF checkTask NO-LOCK 
      WHERE checkTaskLine.PartID = AsnLine.PartID,
         EACH checkTaskLineWork OF checkTaskLine NO-LOCK:
         
         intReceivedSoFar = intReceivedSoFar + checkTaskLineWork.QtyCompleted.
   END.
   
   /* If nothing Received then just return zero */
   IF intReceivedSoFar <= 0 THEN
      RETURN 0.
   
   /* Go through the other AsnLines for the same Part on the Inbound */
   CheckLoop:
   FOR EACH checkAsn OF checkInbound  NO-LOCK, 
      EACH checkAsnLine OF checkAsn NO-LOCK /* idx=AsnIDPartIDAsnLineID */
         WHERE checkAsnLine.PartID     =  thisAsnLine.PartID 
         AND   checkAsnLine.AsnLineID  <> thisAsnLine.AsnLineID:
         
         IF checkAsnLine.Reconciled <> "" THEN
            intReconciledSoFar = intReconciledSoFar + checkAsnLine.QtyReceived.
         ELSE IF checkAsnLine.AsnLineID < thisAsnLine.AsnLineID THEN /* If an earlier line than the current one then subtract QtyExpected */
            intReceivedSoFar = intReceivedSoFar - checkAsnLine.QtyExpected.
         ELSE IF checkAsnLine.AsnLineID > thisAsnLine.AsnLineID THEN /* If a later line than the current one then total up QtyExpected */
            intLaterLinesTotal = intLaterLinesTotal + checkAsnLine.QtyExpected.
         
   END. /*FOR EACH checkAsn OF checkInbound  NO-LOCK, */
   
   intStillToAllocate = intReceivedSoFar - intReconciledSoFar.
   
   /* If nothing left after Allocation to other ealier or else later Reconciled Lines then return 0 */
   IF intStillToAllocate <= 0 THEN
      RETURN 0.
   
   /* Qty to allocate and no Later lines so all remaining must go on current line */
   IF intLaterLinesTotal = 0 THEN
      RETURN intStillToAllocate.
   
   /* Finally if we get to here then there are later lines which remaining Qty can be allocated to */
   IF intStillToAllocate >= thisAsnLine.QtyExpected THEN
      RETURN thisAsnLine.QtyExpected.
   ELSE
      RETURN intStillToAllocate.
   
END FUNCTION. /* fGetQtyAllocatedToAsnLine */


FUNCTION fIsPartReceiptComplete RETURNS CHARACTER (INPUT intAsnLineID AS INTEGER):
   
   FIND FIRST thisAsnLine NO-LOCK
      WHERE thisAsnLine.AsnLineID = intAsnLineID NO-ERROR.
   IF NOT AVAILABLE thisAsnLine THEN
      RETURN "No".
   
   FIND FIRST checkAsn NO-LOCK
      WHERE checkAsn.AsnID = thisAsnLine.AsnID NO-ERROR.
   IF NOT AVAILABLE checkAsn THEN
      RETURN "Cannot Find Asn for:" + STRING(thisAsnLine.AsnID).
   
   FIND FIRST checkInbound NO-LOCK
      WHERE checkInbound.InboundID = checkAsn.InboundID NO-ERROR.
   IF NOT AVAILABLE checkInbound THEN
      RETURN "Cannot Find Inbound for:" + STRING(checkAsn.InboundID).
   
   FIND LAST checkTask OF checkInbound NO-LOCK NO-ERROR.
   IF NOT AVAILABLE checkTask THEN
      RETURN "Cannot Find Task for Inbound:" + STRING(checkAsn.InboundID).
   
   /* Go through each TaskLineWork for the Part and if any are incomplete then return "No" */
   FOR EACH checkTaskLine OF checkTask NO-LOCK 
      WHERE checkTaskLine.PartID = AsnLine.PartID,
         EACH checkTaskLineWork OF checkTaskLine NO-LOCK:
      
         IF checkTaskLineWork.Completed = "" THEN
         DO:
            FIND Part OF checkTaskLine NO-LOCK.
            RETURN "Open Task remains for Part:" + Part.PartRef + ".".
         END.
   END.
   
   /* If we get this far then all TaskLineWorks are complete */
   RETURN "Yes".
   
END FUNCTION. /* fIsPartReceiptComplete */


FUNCTION fAllOtherLinesReconciled RETURNS LOGICAL (INPUT intAsnLineID AS INTEGER):
   
   FIND FIRST thisAsnLine NO-LOCK
      WHERE thisAsnLine.AsnLineID = intAsnLineID NO-ERROR.
   IF NOT AVAILABLE thisAsnLine THEN
      RETURN FALSE.
   
   FIND FIRST checkAsn NO-LOCK
      WHERE checkAsn.AsnID = thisAsnLine.AsnID NO-ERROR.
   IF NOT AVAILABLE checkAsn THEN
      RETURN FALSE.
   
   FIND FIRST checkInbound NO-LOCK
      WHERE checkInbound.InboundID = checkAsn.InboundID NO-ERROR.
   IF NOT AVAILABLE checkInbound THEN
      RETURN FALSE.
   
   /* See if there are any other unreconciled AsnLines for the same PArt on the same Inbound */
   FOR EACH checkAsn OF checkInbound  NO-LOCK, 
      EACH checkAsnLine OF checkAsn NO-LOCK /* idx=AsnIDPartIDAsnLineID */
         WHERE checkAsnLine.PartID    =  thisAsnLine.PartID
         AND   checkAsnLine.AsnLineID <> thisAsnLine.AsnLineID:
         
         IF checkAsnLine.Reconciled = "" THEN
            RETURN FALSE.
   END.
   
   /* If we get this far then all Lines are Reconciled */
   RETURN TRUE.
   
END FUNCTION. /* fAllOtherLinesReconciled */


FUNCTION fAllLinesReconciled RETURNS LOGICAL (INPUT intInboundID AS INTEGER):
   
   FIND FIRST checkInbound NO-LOCK
      WHERE checkInbound.InboundID = intInboundID NO-ERROR.
   IF NOT AVAILABLE checkInbound THEN
      RETURN FALSE.
   
   /* See if there are any other unreconciled AsnLines for the same PArt on the same Inbound */
   FOR EACH checkAsn OF checkInbound  NO-LOCK, 
      EACH checkAsnLine OF checkAsn NO-LOCK: /* idx=AsnIDPartIDAsnLineID */
      
      IF checkAsnLine.Reconciled = "" THEN
         RETURN FALSE.
   END.
   
   /* If we get this far then all Lines are Reconciled */
   RETURN TRUE.
   
END FUNCTION. /* fAllOtherLinesReconciled */
