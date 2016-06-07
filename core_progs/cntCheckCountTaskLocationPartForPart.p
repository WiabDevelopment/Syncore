/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCheckCountTaskLocationPartForPart.p
Purpose : Checks if the scanned part is on CountTaskLocation for this Count Task

          Possible Results : Yes, No

Author  : Christopher Shelley
Date    : 28/04/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:

   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i} 
   {fncGlobalFunctions.i}
   
   /* Optional Includes */  
   {fncStatusTypeFunctions.i} 
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i} 

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   DEFINE VARIABLE intSsnPartID         AS sessionValue NO-UNDO.
   
   /* DB Objects */   
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   intSsnPartID         = fGetSessionValue("PartID").

   Main_Block:
   DO ON ERROR UNDO:

      /* Validate Session Data */
      FIND FIRST CountTaskLocation NO-LOCK
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Task Location ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST Part NO-LOCK
         WHERE Part.PartID = intSsnPartID:intValue NO-ERROR.
      IF NOT AVAILABLE Part THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Part ID] [" + STRING(intSsnPartID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST CountTaskLocationPart NO-LOCK
         WHERE CountTaskLocationPart.PartID = Part.PartID
         AND   CountTaskLocationPart.CountTaskLocationID = CountTaskLocation.CountTaskLocationID NO-ERROR.
      IF AVAILABLE CountTaskLocationPart THEN
      DO:
         chrResult = "Yes".
         LEAVE Main_Block.
      END.

      chrResult = "No".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   DELETE OBJECT intSsnPartID         NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation        NO-ERROR.
   RELEASE CountTaskLocationPackage NO-ERROR.
   RELEASE Part                     NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
