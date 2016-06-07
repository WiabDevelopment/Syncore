/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncMenuFunctions.i
Purpose : Menu functions used by character and web
Author  : Shane Conaty, Binoy Rajan
Date    : 26/11/13
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
  
DEFINE VARIABLE intDynamicItemID AS INTEGER NO-UNDO INITIAL 1.

DEFINE BUFFER checkMenuItem FOR MenuItem.

DEFINE TEMP-TABLE ttMenuItem 
   FIELD ApplicationID    AS INTEGER
   FIELD MenuItemID       AS INTEGER    
   FIELD DynamicItemID    AS INTEGER
   FIELD DynamicParentID  AS INTEGER
   FIELD ListingSequence   AS INTEGER
   FIELD ItemTitle        AS CHARACTER
   FIELD ItemProgram      AS CHARACTER
   FIELD HasChildren      AS CHARACTER.
  
/*Function called from usrLogin passing paramertrs GateUserID,Environment, Chr/Web Mode, Log & DynamicItemID, Outout error.*/
FUNCTION fGetMenuOptions RETURNS INTEGER (INPUT  intUserID        AS INTEGER,
                                          INPUT  chrEnviornment   AS CHARACTER,
                                          INPUT  chrMode          AS CHARACTER,
                                          INPUT  logInternal      AS LOGICAL,
                                          INPUT  intBrowseMenuID  AS INTEGER,
                                          OUTPUT chrMenuError     AS CHARACTER ):
   
   DEFINE VARIABLE intParentMenuID AS INTEGER.

   FIND FIRST ttMenuItem NO-LOCK 
      WHERE ttMenuItem.DynamicItemID = intBrowseMenuID NO-ERROR. 
   IF AVAILABLE ttMenuItem THEN
   DO:
      ASSIGN intParentMenuID = ttMenuItem.MenuItemID.
   END.
        
   FIND FIRST UserSession NO-LOCK
      WHERE UserSession.SessionID = chrGblSessionID NO-ERROR.         
   IF AVAILABLE UserSession THEN
   DO:
      /*This block populates the temptable ttMenuItem thats to be used and validated within usrlogin*/
      FOR EACH AccessUserLink NO-LOCK 
         WHERE AccessUserLink.GateUserID = intUserID,
         EACH  AccessAppLink NO-LOCK 
            WHERE AccessAppLink.ApplicationID = UserSession.ApplicationID
            AND   AccessAppLink.AccessGroupID = AccessUserLink.AccessGroupID,
            EACH  AppMenuItemLink NO-LOCK 
               WHERE AppMenuItemLink.ApplicationID = AccessAppLink.ApplicationID
               AND   AppMenuItemLink.Active, 
               EACH  AccessMenuItemLink NO-LOCK 
                  WHERE AccessMenuItemLink.AccessGroupID = AccessAppLink.AccessGroupID
                  AND   AccessMenuItemLink.MenuItemID = AppMenuItemLink.MenuItemID:  

                  FIND FIRST MenuItem OF AppMenuItemLink NO-LOCK NO-ERROR.
                  IF AVAILABLE MenuItem THEN
                  DO:
                     /*Check if the ProcessEventID within AppMenuItemLink is valid*/
                     FIND FIRST ProcessEvent NO-LOCK 
                        WHERE ProcessEvent.ProcessEventID = AppMenuItemLink.ProcessEventID NO-ERROR.
                     IF NOT AVAILABLE ProcessEvent THEN
                     DO:
                        /*Return an error to the calling program*/
                        ASSIGN chrMenuError = "AppMenuItemLink record does not have a valid ProcessEventID: " + STRING(AppMenuItemLink.ProcessEventID)
                                                 + "  Menu Name: " + MenuItem.MenuName.
                        LEAVE.
                     END.
                  END. /*Avail MenuItem of AppMenuItemLink */
                     
                  /*Validate MenuItem and create records to temptable ttMenuItem*/
                  FIND FIRST MenuItem OF AccessMenuItemLink NO-LOCK 
                     WHERE (MenuItem.MenuItemParentID = intParentMenuID) 
                     AND   (MenuItem.InterfaceType = "ALL" 
                     OR     MenuItem.InterfaceType = chrMode) NO-ERROR.
                  IF AVAILABLE MenuItem THEN 
                  DO:
                     FIND FIRST ttMenuItem NO-LOCK
                        WHERE ttMenuItem.MenuItemID = AccessMenuItemLink.MenuItemID NO-ERROR.
                     IF AVAILABLE ttMenuItem THEN NEXT. 
   
                     CREATE ttMenuItem.
                     ASSIGN ttMenuItem.DynamicItemID   = intDynamicItemID
                            ttMenuItem.MenuItemID      = AccessMenuItemLink.MenuItemID                 
                            ttMenuItem.DynamicParentID = intBrowseMenuID
                            ttMenuItem.ItemTitle       = MenuItem.MenuName
                            ttMenuItem.ItemProgram     = (IF AppMenuItemLink.ProcessEventID = 0 THEN "pChildBrowse" 
                                                          ELSE STRING(AppMenuItemLink.ProcessEventID)) /*ProcessEventID validated above*/
                         
                           /*Check the buffer if the current menuitem has children. If yes then indicate ">"*/
                            ttMenuItem.HasChildren     = (IF CAN-FIND(FIRST checkMenuItem NO-LOCK WHERE checkMenuItem.MenuItemParentID = MenuItem.MenuItemID 
                                                                                                  AND (checkMenuItem.InterfaceType = "ALL"
                                                                                                  OR   checkMenuItem.InterfaceType = chrMode)) THEN ">" ELSE "")
                            ttMenuItem.ListingSequence = AppMenuItemLink.ListingSequence.

                     intDynamicItemID = intDynamicItemID + 1.

                  END. /*IF AVAILABLE MenuItem THEN */
      END. /*FOR EACH AccessUserLink NO-LOCK */

      RETURN ttMenuItem.DynamicParentID.

   END. /*IF AVAILABLE UserSession*/
   ELSE
   DO:
      /*Return an error to the calling program*/
      ASSIGN chrMenuError = "No UserSession record available".
      LEAVE.
   END.

END FUNCTION. /* fGetMenuOptions */
