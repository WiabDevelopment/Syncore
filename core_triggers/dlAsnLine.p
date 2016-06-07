TRIGGER PROCEDURE FOR DELETE OF AsnLine.

{trgValidateSession.i}

{trgCreateAudit.i "AsnLine" "DELETE"}

/* Bespoke Trigger Code goes here  */
FIND FIRST TaskStatus WHERE TaskStatus.StatusCode = "Cancelled" NO-LOCK NO-ERROR.
FIND FIRST TaskLine OF AsnLine NO-LOCK NO-ERROR.
IF AVAILABLE TaskLine THEN
DO:
   FIND FIRST Task OF TaskLine NO-LOCK NO-ERROR.
   IF AVAILABLE Task THEN
   DO:
      IF AVAILABLE TaskStatus THEN
      DO:
         IF Task.TaskStatusID <> TaskStatus.TaskStatusID THEN
            RETURN ERROR "Asn line already has a task associated with it.".
      END. /*IF AVAILABLE TaskStatus THEN*/
   END. /* FIND FIRST Task OF TaskLine NO-LOCK NO-ERROR.*/
END. /*IF AVAILABLE TaskLine THEN*/


/* Buffers */
DEFINE BUFFER checkInbound      FOR Inbound.
DEFINE BUFFER checkAsn          FOR Asn.
DEFINE BUFFER checkAsnLine      FOR AsnLine.
DEFINE BUFFER checkTask         FOR Task.
DEFINE BUFFER checkTaskLine     FOR TaskLine.
DEFINE BUFFER checkTaskLineWork FOR TaskLineWork.


DEFINE VARIABLE intReceivedSoFar     AS INTEGER     NO-UNDO.
DEFINE VARIABLE intReconciledSoFar   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intStillToAllocate   AS INTEGER     NO-UNDO.
DEFINE VARIABLE intLaterLinesTotal   AS INTEGER     NO-UNDO.

MainBlock:
DO:
   /* If its already reconciled then just return the QtyReceived */
   IF AsnLine.Reconciled <> "" THEN
   RETURN ERROR "Already received " + STRING(AsnLine.QtyReceived) + " units".
   
   FIND FIRST checkAsn NO-LOCK
      WHERE checkAsn.AsnID = AsnLine.AsnID NO-ERROR.
   IF NOT AVAILABLE checkAsn THEN
      LEAVE MainBlock.
   
   FIND FIRST checkInbound NO-LOCK
      WHERE checkInbound.InboundID = checkAsn.InboundID NO-ERROR.
   IF NOT AVAILABLE checkInbound THEN
      LEAVE MainBlock.
   
   FIND LAST checkTask OF checkInbound NO-LOCK NO-ERROR.
   IF NOT AVAILABLE checkTask THEN
      LEAVE MainBlock.
   
   /* Get the Total received so far for the Part on this Inbound */
   FOR EACH checkTaskLine OF checkTask NO-LOCK 
      WHERE checkTaskLine.PartID = AsnLine.PartID,
         EACH checkTaskLineWork OF checkTaskLine NO-LOCK:
         
         intReceivedSoFar = intReceivedSoFar + checkTaskLineWork.QtyCompleted.
   END.
   
   /* If nothing Received then just return zero */
   IF intReceivedSoFar <= 0 THEN
      LEAVE MainBlock.
   
   /* Go through the other AsnLines for the same Part on the Inbound */
   CheckLoop:
   FOR EACH checkAsn OF checkInbound  NO-LOCK, 
      EACH checkAsnLine OF checkAsn NO-LOCK /* idx=AsnIDPartIDAsnLineID */
         WHERE checkAsnLine.PartID     =  AsnLine.PartID 
         AND   checkAsnLine.AsnLineID  <> AsnLine.AsnLineID:
         
         IF checkAsnLine.Reconciled <> "" THEN
            intReconciledSoFar = intReconciledSoFar + checkAsnLine.QtyReceived.
         ELSE IF checkAsnLine.AsnLineID < AsnLine.AsnLineID THEN /* If an earlier line than the current one then subtract QtyExpected */
            intReceivedSoFar = intReceivedSoFar - checkAsnLine.QtyExpected.
         ELSE IF checkAsnLine.AsnLineID > AsnLine.AsnLineID THEN /* If a later line than the current one then total up QtyExpected */
            intLaterLinesTotal = intLaterLinesTotal + checkAsnLine.QtyExpected.
         
   END. /*FOR EACH checkAsn OF checkInbound  NO-LOCK, */
   
   intStillToAllocate = intReceivedSoFar - intReconciledSoFar.
   
   /* If nothing left after Allocation to other ealier or else later Reconciled Lines then return 0 */
   IF intStillToAllocate <= 0 THEN
      LEAVE MainBlock.
   
   /* Qty to allocate and no Later lines so all remaining must go on current line */
   IF intLaterLinesTotal = 0 THEN
      RETURN ERROR "Already received " + STRING(intStillToAllocate) + " units".
   
   /* Finally if we get to here then there are later lines which remaining Qty can be allocated to */
   IF intStillToAllocate >= AsnLine.QtyExpected THEN
      RETURN ERROR "Already received " + STRING(AsnLine.QtyExpected) + " units".
   ELSE
      RETURN ERROR "Already received " + STRING(intStillToAllocate) + " units".
END.