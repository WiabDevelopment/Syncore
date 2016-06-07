DISABLE TRIGGERS FOR LOAD OF nex_core.ProcessMap.
DISABLE TRIGGERS FOR LOAD OF nex_core.ProcessEvent.
DISABLE TRIGGERS FOR LOAD OF nex_core.ProcessProgram.
DISABLE TRIGGERS FOR LOAD OF nex_core.ProcessRoute.
DISABLE TRIGGERS FOR LOAD OF nex_core.ProcessMessage.

DEF VAR logCompletedOk           AS LOG.

DEF BUFFER delProcessMap           FOR nex_core.ProcessMap.
DEF BUFFER delProcessEvent         FOR nex_core.ProcessEvent.
DEF BUFFER delProcessProgram       FOR nex_core.ProcessProgram.
DEF BUFFER delProcessRoute         FOR nex_core.ProcessRoute.
DEF BUFFER delProcessMessage       FOR nex_core.ProcessMessage.

UpdateBlk:
DO TRANS ON ERROR UNDO:
   
   FOR EACH nex_core.ProcessMap NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessMap EXCLUSIVE-LOCK
         WHERE ROWID(delProcessMap) = ROWID(nex_core.ProcessMap) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessMap THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessMap.
   END.
   
   FOR EACH MasterProcessMap NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
   
      CREATE nex_core.ProcessMap.
      BUFFER-COPY MasterProcessMap TO nex_core.ProcessMap.
      
      nex_core.ProcessMap.ProcessMapID = MasterProcessMap.MasterProcessMapID.
      nex_core.ProcessMap.StartingProcessEventID = MasterProcessMap.StartingMasterProcessEventID.      
      
   END.
      
   FOR EACH nex_core.ProcessEvent NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessEvent EXCLUSIVE-LOCK
         WHERE ROWID(delProcessEvent) = ROWID(nex_core.ProcessEvent) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessEvent THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessEvent.
   END.
   
   FOR EACH MasterProcessEvent WHERE (MasterProcessEvent.ApplicationID = 0 OR MasterProcessEvent.ApplicationID = 18) NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE nex_core.ProcessEvent.
      BUFFER-COPY MasterProcessEvent TO nex_core.ProcessEvent.
      
      nex_core.ProcessEvent.ProcessMapID = MasterProcessEvent.MasterProcessMapID.
      nex_core.ProcessEvent.ProcessEventID = MasterProcessEvent.MasterProcessEventID.
      
      /*IF MasterProcessEvent.NextCustMasterProcessEventID <> 0 THEN
         nex_core.ProcessEvent.NextAutoEventID = MasterProcessEvent.NextCustMasterProcessEventID.
      ELSE */
         nex_core.ProcessEvent.NextAutoEventID = MasterProcessEvent.NextMasterProcessEventID.
            
      nex_core.ProcessEvent.NextErrorEventID = MasterProcessEvent.NextErrorMasterProcessEventID.
      nex_core.ProcessEvent.ProcessProgramID = MasterProcessEvent.MasterProcessProgramID.

   END.
   
   
   FOR EACH nex_core.ProcessProgram NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessProgram EXCLUSIVE-LOCK
         WHERE ROWID(delProcessProgram) = ROWID(nex_core.ProcessProgram) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessProgram THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessProgram.
   END.
   
   FOR EACH MasterProcessProgram NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
/*      FIND FIRST MasterProcessEvent OF MasterProcessProgram NO-LOCK NO-ERROR.            
      IF AVAILABLE MasterProcessEvent AND MasterProcessEvent.ApplicationID = 0 OR MasterProcessEvent.ApplicationID = 18 THEN
      DO:      */
      
         CREATE nex_core.ProcessProgram.
         BUFFER-COPY MasterProcessProgram TO nex_core.ProcessProgram.
      
         nex_core.ProcessProgram.ProcessProgramID = MasterProcessProgram.MasterProcessProgramID.      
         
/*      END.*/
      
   END.
   
   
   FOR EACH nex_core.ProcessRoute NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessRoute EXCLUSIVE-LOCK
         WHERE ROWID(delProcessRoute) = ROWID(nex_core.ProcessRoute) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessRoute THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessRoute.
   END.
   
   FOR EACH MasterProcessRoute NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND FIRST MasterProcessEvent OF MasterProcessRoute NO-LOCK NO-ERROR.      
      IF AVAILABLE MasterProcessEvent AND MasterProcessEvent.ApplicationID = 0 OR MasterProcessEvent.ApplicationID = 18 THEN
      DO:
         CREATE nex_core.ProcessRoute.
         BUFFER-COPY MasterProcessRoute TO nex_core.ProcessRoute.
        
         nex_core.ProcessRoute.ProcessRouteID = MasterProcessRoute.MasterProcessRouteID.      
         nex_core.ProcessRoute.ProcessEventID = MasterProcessRoute.MasterProcessEventID.            
/*         IF MasterProcessRoute.NextCustMasterProcessEventID <> 0 THEN
            nex_core.ProcessRoute.NextProcessEventID = MasterProcessRoute.NextCustMasterProcessEventID.            
         ELSE*/
            nex_core.ProcessRoute.NextProcessEventID = MasterProcessRoute.NextMasterProcessEventID.            
      END.      
          
   END.
      
      
   FOR EACH nex_core.ProcessMessage NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessMessage EXCLUSIVE-LOCK
         WHERE ROWID(delProcessMessage) = ROWID(nex_core.ProcessMessage) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessMessage THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessMessage.
   END.
   
   FOR EACH MasterProcessMessage NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE nex_core.ProcessMessage.
      BUFFER-COPY MasterProcessMessage TO nex_core.ProcessMessage.
      
      nex_core.ProcessMessage.ProcessMessageID = MasterProcessMessage.MasterProcessMessageID.      
      nex_core.ProcessMessage.ProcessEventID = MasterProcessMessage.MasterProcessEventID.      
      
   END.
   
   logCompletedOk = TRUE.
   
   /*UNDO.*/
   
END. /*DO TRANS ON ERROR UNDO:*/


IF logCompletedOk = TRUE THEN
   MESSAGE SKIP "Data Migration Successful." VIEW-AS ALERT-BOX.
ELSE
   MESSAGE SKIP "Data Migration Failed." VIEW-AS ALERT-BOX.
   

