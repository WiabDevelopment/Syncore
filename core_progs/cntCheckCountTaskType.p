/*------------------------------------------------------------------------------------------------------------------------------------------
Program : stkCheckCountTaskType.p
Purpose : Checks if there is an open CountTask for an unassigned CountTaskLocation with matching EaseOfAcessRanking

          Possible Results : Part, Package

Author  : Christopher Shelley
Date    : 30/04/2014 
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

   /* DB Objects */
   
   /* Get Current Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").
   
   Main_Block:
   DO ON ERROR UNDO:
      FIND FIRST CountTaskLocation NO-LOCK
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskLocation ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.
       
      FIND FIRST CountTask NO-LOCK
         WHERE CountTask.CountTaskID = CountTaskLocation.CountTaskID NO-ERROR.
      IF NOT AVAILABLE CountTask THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTask ID] [" + STRING(CountTaskLocation.CountTaskID) + "] does not Exist.").
         LEAVE Main_Block.
      END.
        
      FIND FIRST CountTaskType NO-LOCK
         WHERE CountTaskType.CountTaskTypeID = CountTask.CountTaskTypeID NO-ERROR.
      IF NOT AVAILABLE CountTaskType THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskType ID] [" + STRING(CountTask.CountTaskTypeID) + "] does not Exist.").
         LEAVE Main_Block.
      END.

      CASE CountTaskType.TypeCode:
         WHEN "PartScan" THEN
            chrResult = "Part".
         WHEN "PackageScan" THEN
            chrResult = "Package".
         OTHERWISE
         DO:
            RUN DisplayError("Uknown [CountTaskType] Found",
                             "[CountTaskType Code] [" + STRING(CountTaskType.TypeCode) + "] does not Exist.").
         END.
      END CASE.
   END. /* Main_Block */
   
   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID NO-ERROR.

   /* Releases */
   RELEASE CountTaskLocation NO-ERROR.
   RELEASE CountTask         NO-ERROR.
   RELEASE CountTaskType     NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}
   
END. /* CTRL-C Catch */
