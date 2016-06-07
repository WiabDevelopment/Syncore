/*------------------------------------------------------------------------------------------------------------------------------------------
Program : locCheckLooseUnitStorage.p
Purpose : Check whether the Location is a Loose Unit Storage Location

          Possible Results : Yes/No 

Author  : MNogaj
Date    : 11/09/2013
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

   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}     

   /* Local Objects */   
   DEFINE VARIABLE intSsnLocationID AS sessionValue NO-UNDO.         
           
   /* Get Current Session Data */
   intSsnLocationID = fGetSessionValue("LocationID").   
     
   Main_Block:
   DO ON ERROR UNDO:
            
      /* Validate the Session Data */
      FIND FIRST Location NO-LOCK 
         WHERE Location.LocationID = intSsnLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE Location THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Location ID] [" + STRING(intSsnLocationID:intValue) + "] does not exist.").
         LEAVE Main_Block.
      END.

      FIND FIRST LocationType OF Location NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LocationType THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[LocationType ID] [" + STRING(Locationtype.LocationTypeID) + "] does not exist.").
         LEAVE Main_Block.
      END.

      /* Check the Loose Unit Storage on the Location Type */
      IF LocationType.LooseUnitStorage THEN
      DO:
         chrResult = "Yes".            
      END.
      ELSE
      DO:
         chrResult = "No".
      END.         

   END.  /* Main Block */

   /* Clean Up */   
   DELETE OBJECT intSsnLocationID NO-ERROR.   

   /* Releases */
   RELEASE Location     NO-ERROR.
   RELEASE LocationType NO-ERROR.

   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.  /* CTRL-C */

