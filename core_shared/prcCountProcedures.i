/*------------------------------------------------------------------------------------------------------------------------------------------
Program : prcCountProcedures.i
Purpose : Group of generic procedures which can be called from anywhere
Author  : Ashwin Baliga
Date    : 23rd April 2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
19/09/2014 MC  HyperBR    Add Locationref to the LocationTypeGroups validation message
12/01/2014 SH  NextelBR   Add Check for Status.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* This include has dependencies on the following other includes */
/*
{defSessionVariables.i}
{fncGlobalFunctions.i}
{fncReplenFunctions.i}
{fncClassFunctions.i}
{fncStatusTypeFunctions.i}
*/

PROCEDURE pGenerateCountTasksForCountGroup:
   
   /* Parameters Definitions ---                                           */
   DEFINE INPUT  PARAMETER intCountGroupID         AS INTEGER        NO-UNDO.
   DEFINE INPUT  PARAMETER logBlindCount           AS LOGICAL        NO-UNDO.
   DEFINE INPUT  PARAMETER intPriority             AS INTEGER        NO-UNDO.
   DEFINE INPUT  PARAMETER chrAssignedTo           AS CHARACTER      NO-UNDO.
   DEFINE OUTPUT PARAMETER intCountTaskID          AS INTEGER        NO-UNDO.
   DEFINE OUTPUT PARAMETER chrReturnError          AS CHARACTER      NO-UNDO.
  
   /* Data Object Definitions */
   DEFINE VARIABLE newCountTask             AS newRecord.
   DEFINE VARIABLE newCustCountTask         AS newRecord.
   DEFINE VARIABLE newCountTaskLocation     AS newRecord.
   DEFINE VARIABLE newCustCountTaskLocation AS newRecord.
   DEFINE VARIABLE newCountTaskUserLink     AS newRecord.

   /* Local Variables */
   DEFINE VARIABLE intLoop AS INTEGER NO-UNDO.

   /* Buffers */
   DEFINE BUFFER groundLocationTypeGroup FOR LocationTypeGroup.
   DEFINE BUFFER upperLocationTypeGroup  FOR LocationTypeGroup.
   
   /* Check if LocationTypeGroups are setup */
   FIND FIRST groundLocationTypeGroup NO-LOCK 
      WHERE groundLocationTypeGroup.GroupCode = "GroundLocations" 
      AND groundLocationTypeGroup.Active NO-ERROR.
      
   IF NOT AVAILABLE groundLocationTypeGroup THEN 
   DO:
      chrReturnError = fTL("LocationTypeGroup for 'GroundLocations' NOT found OR inactive.  Cannot Continue") + ".".
      RETURN.
   END.
   
   FIND FIRST upperLocationTypeGroup NO-LOCK 
      WHERE upperLocationTypeGroup.GroupCode = "UpperLocations" 
      AND upperLocationTypeGroup.Active NO-ERROR.
      
   IF NOT AVAILABLE upperLocationTypeGroup THEN 
   DO:
      chrReturnError = fTL("LocationTypeGroup for 'UpperLocations' NOT found OR inactive.  Cannot Continue") + ".".
      RETURN.
   END.

   FIND FIRST CountTaskStatus NO-LOCK
      WHERE CountTaskStatus.StatusCode = 'Available'
      AND CountTaskStatus.Active NO-ERROR.

   IF NOT AVAILABLE CountTaskStatus THEN
   DO:
      chrReturnError = fTL("CountTaskStatus NOT found or inactive. Cannot Continue") + ".".
      RETURN.
   END.
   
   FIND FIRST CountGroup NO-LOCK
      WHERE CountGroup.CountGroupID = intCountGroupID NO-ERROR.

   IF NOT AVAILABLE CountGroup THEN
   DO:
      chrReturnError = "CountGroup: " + STRING(intCountGroupID) + fTL(" is NOT a valid CountGroup. Cannot Continue") + ".".
      RETURN.
   END.
 
   IF chrAssignedTo <> "0" THEN
   DO intLoop = 1 TO NUM-ENTRIES(chrAssignedTo):
      FIND FIRST GateUser NO-LOCK
         WHERE GateUser.GateUserID = INTEGER(ENTRY(intLoop,chrAssignedTo)) NO-ERROR.

      IF NOT AVAILABLE GateUser THEN
      DO:
         chrReturnError = ENTRY(intLoop,chrAssignedTo) + fTL(" is NOT a valid GateUser. Cannot Continue") + ".".
         RETURN.
      END.
   END.
   
   /* check to see if atleast one location link exists for the CountGroup */
   IF NOT CAN-FIND(FIRST CountGroupLocationLink OF CountGroup) THEN
   DO:
      chrReturnError = "CountGroup: " + STRING(intCountGroupID) + fTL(" has NO linked locations.  Cannot Continue") + ".".
      RETURN.
   END.

   CountTaskBlk:
   DO ON ERROR UNDO, LEAVE:

      /* Create a single CountTask and multiple CountTaskLocations 
         for each of the CountGroupLocationLinks linked to the CountGroup */
      
      newCountTask = fCreateRecord("CountTask").

      newCountTask:assignField("CountTaskTypeID",CountGroup.CountTaskTypeID).
      newCountTask:assignField("CountGroupID",CountGroup.CountGroupID).
      newCountTask:assignField("BusinessUnitID",CountGroup.BusinessUnitID).
      newCountTask:assignField("CountGroupTypeID",CountGroup.CountGroupTypeID).
      newCountTask:assignField("BlindCount",logBlindCount).
      newCountTask:assignField("Priority",intPriority).
      newCountTask:assignField("CountTaskStatusID", CountTaskStatus.CountTaskStatusID). /* Create with "Available Status" */

      chrReturnError = newCountTask:getErrors().

      IF chrReturnError <> "" THEN
         UNDO CountTaskBlk, LEAVE CountTaskBlk.
   
      /* Set this output parameter to return the CountTaskID to the calling program */
      intCountTaskID = newCountTask:NewRecordUniqueID.

      DELETE OBJECT newCountTask NO-ERROR.  
      
      /* Create CountTaskLocations */
      FOR EACH CountGroupLocationLink OF CountGroup NO-LOCK
         WHERE CountGroupLocationLink.Active = YES:
         
         FIND FIRST Location OF CountGroupLocationLink NO-LOCK NO-ERROR.
        
         IF NOT AVAILABLE Location THEN 
         DO:
            chrReturnError = STRING(intCountGroupID) + fTL(" has UNKNOWN locations.  Cannot Continue") + ".". 
            UNDO CountTaskBlk, LEAVE CountTaskBlk.
         END.
         
         FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
         
         IF NOT AVAILABLE LocationType THEN 
         DO:
            chrReturnError = "CountGroup: " + STRING(intCountGroupID) + fTL(" has locations with UNKNOWN Location Types.  Cannot Continue") + ".". 
            UNDO CountTaskBlk, LEAVE CountTaskBlk.
         END.
         
         /* Check to see if CountGroupLocationLink is active and belongs to one of the LocationTypeGroups below */
         IF NOT CAN-FIND(FIRST LocationTypeGroupLink OF LocationType NO-LOCK
                         WHERE LocationTypeGroupLink.LocationTypeGroupID = groundLocationTypeGroup.LocationTypeGroupID
                         AND LocationTypeGroupLink.Active) AND 
            NOT CAN-FIND(FIRST LocationTypeGroupLink OF LocationType NO-LOCK
                         WHERE LocationTypeGroupLink.LocationTypeGroupID = upperLocationTypeGroup.LocationTypeGroupID
                         AND LocationTypeGroupLink.Active) THEN 
         DO:
            chrReturnError = "CountGroup: " + STRING(intCountGroupID) + 
                              fTL(" has location " + Location.LocationRef + " which does not belong to either Ground or Upper LocationTypeGroups.  Cannot Continue") + ".".
            UNDO CountTaskBlk, LEAVE CountTaskBlk.
         END.
         
         newCountTaskLocation = fCreateRecord("CountTaskLocation").

         newCountTaskLocation:assignField("LocationID",CountGroupLocationLink.LocationID).
         newCountTaskLocation:assignField("CountTaskID",intCountTaskID).
         newCountTaskLocation:assignField("Priority",intPriority).
/*          newCountTaskLocation:assignField("AssignedTo",intAssignedTo). Not assigning at this stage as CountTaskUserLink was introduced */
         newCountTaskLocation:assignField("BlindCount",logBlindCount).
         newCountTaskLocation:assignField("CountTaskStatusID", CountTaskStatus.CountTaskStatusID). /* Create with "Available Status" */

         chrReturnError = newCountTaskLocation:getErrors().
         
         IF chrReturnError <> "" THEN 
            UNDO CountTaskBlk, LEAVE CountTaskBlk.
         
         DELETE OBJECT newCountTaskLocation NO-ERROR.

      END. /* FOR EACH CountGroupLocationLink */
      
      /* Only Create CountTaskUserLink records if chrAssignedTo is not 0 */
      IF chrAssignedTo <> "0" THEN 
      DO:
         DO intLoop = 1 TO NUM-ENTRIES(chrAssignedTo):
   
            /* Creatiing Task User Link */
            newCountTaskUserLink = fCreateRecord("CountTaskUserLink").
   
            newCountTaskUserLink:assignField("CountTaskID",intCountTaskID).
            newCountTaskUserLink:assignField("GateUserID",INTEGER(ENTRY(intLoop,chrAssignedTo))).
            newCountTaskUserLink:assignField("PreAssignedBy",intGblUserID).
            newCountTaskUserLink:assignField("Created",fTimestamp(NOW)).
   
            chrReturnError = newCountTaskUserLink:getErrors().
   
            IF chrReturnError <> "" THEN
               UNDO CountTaskBlk, LEAVE CountTaskBlk.
            
            DELETE OBJECT newCountTaskUserLink NO-ERROR.  
   
         END.
      END. /* if chrAssignedTo <> "0" */     
   
   END. /* CountTaskBlk:*/

END PROCEDURE. /* pGenerateCountTasksForCountGroup */
