/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncUserAccessFunctions.i
Purpose : All functions to do with User Access Access Groups and Access Group Permission Links and their recursive nature
Author  : AB
Date    : 8th Jan 2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* ttDef */
DEFINE TEMP-TABLE ttViewableAccessGroup LIKE AccessGroup.


FUNCTION fIsSuperUser RETURNS LOGICAL (INPUT intGateUserID AS INTEGER): 
   
   DEFINE BUFFER fncGateUser FOR GateUser.
   DEFINE BUFFER fncAccessUserLink FOR AccessUserLink.

   FIND FIRST fncGateUser NO-LOCK
      WHERE fncGateUser.GateUserID = intGateUserID NO-ERROR.
   
   IF NOT AVAILABLE fncGateUser THEN
      RETURN FALSE.

   IF AVAILABLE fncGateUser THEN
      FIND FIRST fncAccessUserLink OF fncGateUser NO-LOCK
         WHERE fncAccessUserLink.AccessGroupID = 1 NO-ERROR.

   IF AVAILABLE fncAccessUserLink THEN
      RETURN TRUE.
   ELSE
      RETURN FALSE.

END FUNCTION. /* fIsSuperUser */


FUNCTION fIsDeveloper RETURNS LOGICAL (INPUT intGateUserID AS INTEGER):

   DEFINE BUFFER fncGateUser FOR GateUser.
   
   DEFINE BUFFER fncAccessUserLink FOR AccessUserLink.

   FIND FIRST fncGateUser NO-LOCK
      WHERE fncGateUser.GateUserID = intGateUserID NO-ERROR.

   IF NOT AVAILABLE fncGateUser THEN
      RETURN FALSE.

   IF AVAILABLE fncGateUser THEN
      FIND FIRST fncAccessUserLink OF fncGateUser NO-LOCK
         WHERE fncAccessUserLink.AccessGroupID = 2 NO-ERROR.

   IF AVAILABLE fncAccessUserLink THEN
      RETURN TRUE.
   ELSE
      RETURN FALSE.

END FUNCTION. /* fIsDeveloper */


FUNCTION fCanCreateGroups RETURNS LOGICAL(INPUT intApplicationID AS INTEGER,
                                          INPUT intGateUserID    AS INTEGER):
   
   DEFINE VARIABLE logFoundOne AS LOGICAL NO-UNDO.

   DEFINE BUFFER fncAccessAppLink  FOR AccessAppLink.
   DEFINE BUFFER fncAccessUserLink FOR AccessUserLink.
   DEFINE BUFFER fncAccessGroup    FOR AccessGroup.
   
   /* check to see if user has one group linked directly to application */
   FOR EACH fncAccessAppLink NO-LOCK
      WHERE fncAccessAppLink.ApplicationID = intApplicationID,
      EACH fncAccessUserLink NO-LOCK
      WHERE fncAccessUserLink.AccessGroupID = fncAccessAppLink.AccessGroupID
      AND fncAccessUserLink.GateUserID = intGateUserID,
      EACH fncAccessGroup OF fncAccessUserLink NO-LOCK
      WHERE fncAccessGroup.CanCreateAccessGroups = YES
      AND fncAccessGroup.SpansMultipleApps = NO:
      logFoundOne = TRUE.
      LEAVE.
   END. /* FOR EACH fncAccessAppLink NO-LOCK */
    
   IF logFoundOne = TRUE THEN
     RETURN logFoundOne.
   
   /* second level check if User is part of SpansMultiple Apps */
   FOR EACH fncAccessUserLink NO-LOCK
      WHERE fncAccessUserLink.GateUserID = intGateUserID,
      EACH fncAccessGroup OF fncAccessUserLink NO-LOCK
      WHERE fncAccessGroup.CanCreateAccessGroups = YES
      AND fncAccessGroup.SpansMultipleApps = YES:
     logFoundOne = TRUE.
     LEAVE.
   END. /* FOR EACH fncAccessUserLink NO-LOCK */

   RETURN logFoundOne.

END FUNCTION. /* fCanCreateGroups */


FUNCTION fBelongsToSpansMultipleApps RETURNS LOGICAL (INPUT intGateUserID AS INTEGER):
   
   DEFINE VARIABLE logFoundOne AS LOGICAL NO-UNDO.

   DEFINE BUFFER fncAccessAppLink  FOR AccessAppLink.
   DEFINE BUFFER fncAccessUserLink FOR AccessUserLink.
   DEFINE BUFFER fncAccessGroup    FOR AccessGroup.
   
   FOR EACH fncAccessUserLink NO-LOCK
      WHERE fncAccessUserLink.GateUserID = intGateUserID,
      EACH fncAccessGroup OF fncAccessUserLink NO-LOCK
      WHERE fncAccessGroup.SpansMultipleApps = YES:
      logFoundOne = TRUE.
      LEAVE.
   END.
   RETURN logFoundOne.
END FUNCTION. /* fBelongsToSpansMultipleApps */


PROCEDURE pGetUserAccessGroupsAvailableToApplication:

   DEFINE INPUT PARAMETER intApplicationID AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER intGateUserID    AS INTEGER NO-UNDO.

   FOR EACH AccessUserLink NO-LOCK 
       WHERE AccessUserLink.GateUserID = intGateUserID,
       EACH AccessAppLink NO-LOCK
       WHERE AccessAppLink.ApplicationID = intApplicationID
       AND AccessAppLink.AccessGroupID = AccessUserLink.AccessGroupID:

       RUN pGetAllViewableAccessGroups (INPUT AccessAppLink.AccessGroupID).
    END.

END PROCEDURE. /* pGetUserAccessGroupsAvailableToApplication */


PROCEDURE pGetAllViewableAccessGroups:

   DEFINE INPUT PARAMETER intAccessGroupID AS INTEGER NO-UNDO.
   
   DEFINE BUFFER fromAccessGroupPermissionLink FOR AccessGroupPermissionLink.
   DEFINE BUFFER toAccessGroupPermissionLink FOR AccessGroupPermissionLink.
   
   DEFINE BUFFER fromAccessGroup FOR AccessGroup.
   
   DEFINE BUFFER bufAccessGroup   FOR AccessGroup.
   DEFINE BUFFER bufAccessAppLink FOR AccessAppLink.

   
   /* if either passed in application or group is not found as active then leave */
   IF NOT CAN-FIND(FIRST bufAccessGroup NO-LOCK
                   WHERE bufAccessGroup.AccessGroupID = intAccessGroupID
                   AND bufAccessGroup.Active) THEN
     RETURN.
   
   FOR EACH fromAccessGroupPermissionLink NO-LOCK
      WHERE fromAccessGroupPermissionLink.FromAccessGroupID = intAccessGroupID
      AND fromAccessGroupPermissionLink.Active:
      
      FIND fromAccessGroup NO-LOCK
         WHERE fromAccessGroup.AccessGroupID = fromAccessGroupPermissionLink.ToAccessGroupID NO-ERROR.
      /* if already there then don't create it */
      IF AVAILABLE fromAccessGroup
         AND NOT CAN-FIND(FIRST ttViewableAccessGroup NO-LOCK
                          WHERE ttViewableAccessGroup.AccessGroupID = fromAccessGroup.AccessGroupID) THEN
      DO:
         CREATE ttViewableAccessGroup.
         BUFFER-COPY fromAccessGroup TO ttViewableAccessGroup.
      END. /* IF AVAILABLE fromAccessGroup */
      
      IF CAN-FIND(FIRST toAccessGroupPermissionLink NO-LOCK
                  WHERE toAccessGroupPermissionLink.FromAccessGroupID = fromAccessGroupPermissionLink.ToAccessGroupID
                  AND  toAccessGroupPermissionLink.FromAccessGroupID <>  fromAccessGroupPermissionLink.FromAccessGroupID 
                  AND toAccessGroupPermissionLink.Active) THEN
      
      DO:
         /* Run recursively for the children */
         RUN pGetAllViewableAccessGroups (INPUT fromAccessGroupPermissionLink.ToAccessGroupID).
      END. /* IF CAN-FIND(FIRST AccessGroupPermissionLink */     
   END. /* FOR EACH fromAccessGroupPermissionLink */
END PROCEDURE. /* pGetAllViewableAccessGroups */


PROCEDURE pGetUserAccessGroupsLinkedToApplication:

  DEFINE INPUT PARAMETER intApplicationID AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER intGateUserID    AS INTEGER NO-UNDO.

  FOR EACH AccessUserLink NO-LOCK 
      WHERE AccessUserLink.GateUserID = intGateUserID,
      EACH AccessAppLink NO-LOCK
      WHERE AccessAppLink.ApplicationID = intApplicationID
      AND AccessAppLink.AccessGroupID = AccessUserLink.AccessGroupID:

      RUN pGetAllViewableAccessGroupsForApplication (INPUT intApplicationID,
                                                     INPUT AccessAppLink.AccessGroupID).
   END.

END PROCEDURE. /* pGetUserAccessGroupsLinkedToApplication */


PROCEDURE pGetAllViewableAccessGroupsForApplication:
   
   DEFINE INPUT PARAMETER intApplicationID AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER intAccessGroupID AS INTEGER NO-UNDO.
   
   DEFINE BUFFER fromAccessGroupPermissionLink FOR AccessGroupPermissionLink.
   DEFINE BUFFER toAccessGroupPermissionLink FOR AccessGroupPermissionLink.
   
   DEFINE BUFFER fromAccessGroup FOR AccessGroup.
   
   DEFINE BUFFER bufApplication   FOR Application.
   DEFINE BUFFER bufAccessGroup   FOR AccessGroup.
   DEFINE BUFFER bufAccessAppLink FOR AccessAppLink.

   
   /* if either passed in application or group is not found as active then leave */
   IF NOT CAN-FIND(FIRST bufApplication NO-LOCK
                   WHERE bufApplication.ApplicationID = intApplicationID
                   AND bufApplication.Active) OR
      NOT CAN-FIND(FIRST bufAccessGroup NO-LOCK
                   WHERE bufAccessGroup.AccessGroupID = intAccessGroupID
                   AND bufAccessGroup.Active) THEN
     RETURN.
   
   FOR EACH fromAccessGroupPermissionLink NO-LOCK
      WHERE fromAccessGroupPermissionLink.FromAccessGroupID = intAccessGroupID
      AND fromAccessGroupPermissionLink.Active:
      
      /* Check to see if group is linked to the application if not skip */
      IF CAN-FIND(FIRST bufAccessAppLink 
                  WHERE bufAccessAppLink.ApplicationID = intApplicationID
                  AND bufAccessAppLink.AccessGroupID = fromAccessGroupPermissionLink.ToAccessGroupID) THEN
      DO:
         FIND fromAccessGroup NO-LOCK
            WHERE fromAccessGroup.AccessGroupID = fromAccessGroupPermissionLink.ToAccessGroupID NO-ERROR.
         /* if already there then don't create it */
         IF AVAILABLE fromAccessGroup
            AND NOT CAN-FIND(FIRST ttViewableAccessGroup NO-LOCK
                             WHERE ttViewableAccessGroup.AccessGroupID = fromAccessGroup.AccessGroupID) THEN
         DO:
            CREATE ttViewableAccessGroup.
            BUFFER-COPY fromAccessGroup TO ttViewableAccessGroup.
         END. /* IF AVAILABLE fromAccessGroup */
         
         IF CAN-FIND(FIRST toAccessGroupPermissionLink NO-LOCK
                     WHERE toAccessGroupPermissionLink.FromAccessGroupID = fromAccessGroupPermissionLink.ToAccessGroupID
                     AND  toAccessGroupPermissionLink.FromAccessGroupID <>  fromAccessGroupPermissionLink.FromAccessGroupID 
                     AND toAccessGroupPermissionLink.Active) THEN
         
         DO:
            /* Run recursively for the children */
            RUN pGetAllViewableAccessGroupsForApplication (INPUT intApplicationID,
                                                           INPUT fromAccessGroupPermissionLink.ToAccessGroupID).
         END. /* IF CAN-FIND(FIRST AccessGroupPermissionLink */     
      END. /* IF CAN-FIND(FIRST AccessAppLink */
   END. /* FOR EACH fromAccessGroupPermissionLink */

END PROCEDURE. /* pGetAllViewableAccessGroupsForApplication */

FUNCTION fGetUsersForApplication RETURNS CHARACTER (INPUT intApplicationID AS INTEGER,
                                                    INPUT intEnvironmentID AS INTEGER):
   
   DEFINE VARIABLE chrUserIdList AS CHARACTER NO-UNDO.

   FOR EACH GateUser NO-LOCK
      WHERE GateUser.Active = YES, /* idx=ActiveListingSequence*/
      EACH AccessUserLink OF GateUser NO-LOCK
         WHERE AccessUserLink.Active = YES,
      EACH AccessGroup OF AccessUserLink NO-LOCK
         WHERE AccessGroup.ACTIVE = YES,
      EACH AccessGroupEnvironmentLink OF AccessGroup NO-LOCK
         WHERE AccessGroupEnvironmentLink.EnvironmentID = intEnvironmentID
         AND   AccessGroupEnvironmentLink.Active = YES,
      EACH AccessAppLink OF AccessGroup NO-LOCK
         WHERE AccessAppLink.ApplicationID = intApplicationID
         AND   AccessAppLink.Active = YES
         GROUP BY GateUser.FullName DESCENDING:

         IF FIRST-OF(GateUser.FullName) THEN DO:
            chrUserIdList = STRING(GateUser.GateUserID,">>>>>>>9") + "," + chrUserIdList.
         END.

   END. /* FOR EACH CountGroup NO-LOCK */

   chrUserIdList = RIGHT-TRIM(chrUserIdList,",").

   RETURN chrUserIdList.

END FUNCTION. /* fGetUsersForApplication */
