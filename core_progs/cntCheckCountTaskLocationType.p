/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntCheckCountTaskLocationType.p
Purpose : Checks the CountTask Type if Parts are loose or non loose

          Possible Results : Loose, NotLoose

Author  : Christopher Shelley
Date    : 29/04/2014
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
   
   /* Local Variables */     

   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID AS sessionValue NO-UNDO.
   
   /* DB Objects */   
 
   /* Get Current Session Data */
   intSsnTaskLocationID  = fGetSessionValue("TaskLocationID").

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

      FIND FIRST Location OF CountTaskLocation NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Location THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Location ID] [" + STRING(CountTaskLocation.LocationID) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LocationType THEN DO:
         RUN DisplayError("Record Not Found",
                          "[LocationType ID] [" + STRING(Location.LocationTypeID) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      IF LocationType.LooseUnitStorage THEN
         chrResult = "Loose".
      ELSE
         chrResult = "NonLoose".
                     
   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation NO-ERROR.
   RELEASE Location          NO-ERROR.
   RELEASE LocationType      NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
