/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntScanTaskLocation.p
Purpose : Validate the scanned location is the expected location
 
          Possible Results: Continue

Author  : Christopher Shelley
Date    : 23/04/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
28/10/2014 BR  DaymenNL   Add scan height functionality.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:

   /* Character SessionValue Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i}
   {fncGlobalFunctions.i}
   {fncStatusTypeFunctions.i}
   
   /*Optional Includes*/ 
   {fncStringFunctions.i}
      
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Buffers */
   DEFINE BUFFER scanLocation FOR Location.
   
   /* Local Variables */     
   DEFINE VARIABLE chrScanLocation AS CHARACTER FORMAT "x(19)"  VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrScanHeight   AS CHARACTER FORMAT "x(4)"   VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrLocation     AS CHARACTER FORMAT "x(12)"  LABEL "Location"       NO-UNDO.
   DEFINE VARIABLE chrCountTask    AS CHARACTER FORMAT "x(24)"  VIEW-AS FILL-IN NATIVE NO-UNDO.   
   DEFINE VARIABLE chrScanMessage  AS CHARACTER FORMAT "x(23)"  VIEW-AS FILL-IN NATIVE NO-UNDO.
   DEFINE VARIABLE chrUserPrompt   AS CHARACTER FORMAT "x(24)"  VIEW-AS FILL-IN NATIVE NO-UNDO.
   
   /* Local Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.      
   DEFINE VARIABLE intSsnLocationID     AS sessionValue NO-UNDO.   
   DEFINE VARIABLE intSsnLocationRef    AS sessionValue NO-UNDO.  
   DEFINE VARIABLE chrSsnLastScan       AS sessionValue NO-UNDO.  

   /* Reset Data */   
   fClearSessionValue("LocationID").  
   fClearSessionValue("LocationRef"). 
   fClearSessionValue("LastScan").
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
        
   /* Set New Data */   
   intSsnLocationID  = fNewSessionValue("LocationID").
   intSsnLocationRef = fNewSessionValue("LocationRef").
   chrSsnLastScan    = fNewSessionValue("LastScan").   
   
   /* Frames and UI */
   DEFINE FRAME ScanLocationFrame                 
       chrCountTask    NO-LABEL COLON 1 
       SKIP(1)
       chrUserPrompt   NO-LABEL COLON 1 
       SKIP 
       chrLocation              COLON 12         
       SKIP(2)
       chrScanMessage  NO-LABEL COLON 1
       SKIP
       chrScanLocation NO-LABEL COLON 1
       chrScanHeight   NO-LABEL COLON 21
       SKIP(4)
   WITH SIDE-LABELS TITLE "Scan Location" COLOR MESSAGE ROW 8 COL 1 WIDTH 30.
   
   /* Events */
   ON 'PF4':U ANYWHERE
   DO:
      chrScanLocation:SCREEN-VALUE IN FRAME ScanLocationFrame = "COMPLETE".
      RETURN NO-APPLY.
   END.      
      
   Main_Block:
   DO ON ERROR UNDO:
      
      Scan_Block:
      REPEAT ON STOP UNDO, RETRY:       
         
         FIND FIRST CountTaskLocation NO-LOCK
            WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
         IF NOT AVAILABLE CountTaskLocation THEN
         DO:
            RUN DisplayError("Record Not Found",
                             "[CountTaskLocation ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
            LEAVE Main_Block.
         END.
         
         FIND FIRST CountTask OF CountTaskLocation NO-LOCK NO-ERROR.
         IF NOT AVAILABLE CountTask THEN
         DO:
            RUN DisplayError("Record Not Found",
                             "[CountTask ID] [" + STRING(CountTaskLocation.CountTaskID) + "] does not Exist.").
            LEAVE Main_Block.
         END. 
         
         FIND FIRST Location NO-LOCK
            WHERE Location.LocationID = CountTaskLocation.LocationID NO-ERROR.
         IF NOT AVAILABLE Location THEN
         DO:
            RUN DisplayError("Record Not Found",
                             "[Location ID] [" + STRING(CountTaskLocation.LocationID) + "] does not Exist.").
            LEAVE Main_Block.
         END.
         
         /* If This Task is for Recount */
         IF CountTask.CountGroupRecountID > 0 THEN 
         DO:
            chrUserPrompt = fCenterOnScreen("Scan Recount Location").
         END.
         ELSE 
         DO:
            chrUserPrompt = fCenterOnScreen("Scan Location").
         END.
         
         chrCountTask   = " Count TaskID: " + STRING(CountTask.CountTaskID,"999999").
         chrScanMessage = fCenterOnScreen("Scan Location/Height").

         /* Set Screen Data */
         chrLocation = Location.LocationRef. 
         chrScanLocation = "".         
         chrScanHeight   = "".
         
         /* Scan Count Location */
         DISPLAY chrUserPrompt chrLocation chrScanMessage chrCountTask WITH FRAME ScanLocationFrame.
         UPDATE chrScanLocation chrScanHeight WITH FRAME ScanLocationFrame.
         chrScanLocation = chrScanLocation + chrScanHeight.  /*Add height to location ref*/
         
         chrScanLocation = TRIM(chrScanLocation).
         
         /* Check for Complete */
         IF chrScanLocation = "COMPLETE" THEN
         DO:
            chrSsnLastScan:setValue(chrScanLocation).
            chrResult = "Continue".
            LEAVE Main_Block.
         END.

         /* Validate the Location Exists */
         FIND FIRST scanLocation NO-LOCK
              WHERE scanLocation.LocationRef = chrScanLocation NO-ERROR.
         IF NOT AVAILABLE scanLocation THEN
         DO:
            RUN DisplayMessage("Location Error",
                               "Location [" + chrScanLocation + "] does not exist. Scan another.").
            NEXT Scan_Block.
         END.        
         
         /* Validate the the User Scanned the Task Location */
         IF scanLocation.LocationID <> CountTaskLocation.LocationID THEN
         DO:
            RUN DisplayMessage("Location Error",
                               "Location [" + chrScanLocation + "] is NOT the next task location.").
            NEXT Scan_Block.
         END.

         /* Assign Location Data */
         intSsnLocationID:setValue(scanLocation.LocationID).
         intSsnLocationRef:setValue(Location.LocationRef).
         chrResult = "Continue".

         LEAVE Main_Block.
   
      END.  /* Scan Block */

   END.  /* Main Block */
      
   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   DELETE OBJECT intSsnLocationID     NO-ERROR. 
   DELETE OBJECT intSsnLocationRef    NO-ERROR.  
   DELETE OBJECT chrSsnLastScan       NO-ERROR.  

   /* Releases */
   RELEASE Location          NO-ERROR.
   RELEASE scanLocation      NO-ERROR.
   RELEASE CountTaskLocation NO-ERROR.   

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.  /* CTRL-C */
