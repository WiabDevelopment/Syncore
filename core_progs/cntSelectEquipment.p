/*------------------------------------------------------------------------------------------------------------------------------------------
Program : stkSelectEquipmentCycleCounting.p
Purpose : Prompts User to if they have Equipment

          Possible Results : Continue

Author  : Christopher Shelley
Date    : 22/04/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:
   
   /* Character SessionValue Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
{fncClassFunctions.i}
   {fncGlobalFunctions.i}

   /* Optional Includes */
{fncStatusTypeFunctions.i}
   /* fGetStatusID */
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}       

   /* Session Objects */
   DEFINE VARIABLE intSsnLocationTypeGroup AS sessionValue NO-UNDO.   
   
   /* Buffers */
   DEFINE BUFFER groundLocationTypeGroup FOR LocationTypeGroup.
   DEFINE BUFFER upperLocationTypeGroup  FOR LocationTypeGroup.
   
   /* Temp Tables */
   DEFINE TEMP-TABLE ttEquipment NO-UNDO
      FIELD LocationTypeGroup AS INTEGER 
      FIELD EquipmentName       AS CHARACTER.

   /* Frames and UI */
   DEFINE QUERY qryEquipmentTypes FOR ttEquipment.

   DEFINE BROWSE bseEquipmentTypes QUERY qryEquipmentTypes
      DISPLAY  ttEquipment.EquipmentName NO-LABEL FORMAT "x(18)"
      WITH NO-BOX 12 DOWN CENTERED COLOR MESSAGE ROW 1 COL 1 SIZE 26 BY 11.

   DEFINE FRAME SelectEquipmentTypeFrame          
      SKIP(1)
      bseEquipmentTypes
      WITH SIDE-LABELS TITLE " Select Equipment Type " COLOR MESSAGE ROW 8 COL 1 WIDTH 30.

   /* Events */
   ON RETURN OF bseEquipmentTypes IN FRAME SelectEquipmentTypeFrame 
      DO:      
         /* Set the Equipment  */
         intSsnLocationTypeGroup:setValue(ttEquipment.LocationTypeGroup).            

         chrResult = "CONTINUE".       
      END.

   ON 'PF4':U ANYWHERE
      DO:         
         chrResult = "ALLOW_EXIT".
         RETURN NO-APPLY.
      END.   

   ON END-ERROR OF bseEquipmentTypes IN FRAME SelectEquipmentTypeFrame
      DO:      
         chrResult = "ALLOW_EXIT".
         HIDE ALL.
         LEAVE.
      END.
   
   /* Clear Session Data */
   fClearSessionValue("LocationTypeGroup").
   
   /* Create New Session Data */
   intSsnLocationTypeGroup = fNewSessionValue("LocationTypeGroup").         
   
   Main_Block:
   DO ON ERROR UNDO, LEAVE:
      
      FIND FIRST groundLocationTypeGroup NO-LOCK 
         WHERE groundLocationTypeGroup.GroupCode = "GroundLocations" NO-ERROR.
      IF NOT AVAILABLE groundLocationTypeGroup THEN 
      DO:
         RUN DisplayMessage("Record Not Found",
                            "[LocationTypeGroup] [GroundLocations] does not exist.").
         LEAVE Main_Block.
      END.   
      
      FIND FIRST upperLocationTypeGroup NO-LOCK 
         WHERE upperLocationTypeGroup.GroupCode = "UpperLocations" NO-ERROR.
      IF NOT AVAILABLE upperLocationTypeGroup THEN 
      DO:
         RUN DisplayMessage("Record Not Found",
                            "[LocationTypeGroup] [UpperLocations] does not exist.").
         LEAVE Main_Block.                  
      END. 
 
      /* For now hard coding these */
      CREATE ttEquipment.
      ASSIGN ttEquipment.LocationTypeGroup = groundLocationTypeGroup.LocationTypeGroupID.
             ttEquipment.EquipmentName       = "None".
                 
      CREATE ttEquipment.
      ASSIGN ttEquipment.LocationTypeGroup = upperLocationTypeGroup.LocationTypeGroupID. 
             ttEquipment.EquipmentName       = "Lift".
                          
      OPEN QUERY qryEquipmentTypes FOR EACH ttEquipment NO-LOCK.
      ENABLE bseEquipmentTypes WITH FRAME SelectEquipmentTypeFrame.
      bseEquipmentTypes:SELECT-ROW(1).      

      WAIT-FOR RETURN, END-ERROR OF BROWSE bseEquipmentTypes.

   END.  /* Main Block */      

   /* Clean Up */
   DELETE OBJECT intSsnLocationTypeGroup NO-ERROR. 
   
   /* Releases */
   RELEASE groundLocationTypeGroup NO-ERROR.
   RELEASE upperLocationTypeGroup NO-ERROR.  
   
   /* Map Debugging */
   {prcProcessDebugging.i}

END.  /* CTRL-C */