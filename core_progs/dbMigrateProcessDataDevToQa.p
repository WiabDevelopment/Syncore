DISABLE TRIGGERS FOR LOAD OF qa_core.ProcessMap.
DISABLE TRIGGERS FOR LOAD OF qa_core.ProcessEvent.
DISABLE TRIGGERS FOR LOAD OF qa_core.ProcessProgram.
DISABLE TRIGGERS FOR LOAD OF qa_core.ProcessRoute.
DISABLE TRIGGERS FOR LOAD OF qa_core.ProcessOption.
DISABLE TRIGGERS FOR LOAD OF qa_core.ProcessMessage.
DISABLE TRIGGERS FOR LOAD OF qa_core.WebForm.
DISABLE TRIGGERS FOR LOAD OF qa_core.WebFormField.

DEF VAR logCompletedOk           AS LOG.

DEF BUFFER delProcessMap           FOR qa_core.ProcessMap.
DEF BUFFER delProcessEvent         FOR qa_core.ProcessEvent.
DEF BUFFER delProcessProgram       FOR qa_core.ProcessProgram.
DEF BUFFER delProcessRoute         FOR qa_core.ProcessRoute.
DEF BUFFER delProcessOption        FOR qa_core.ProcessOption.
DEF BUFFER delProcessMessage       FOR qa_core.ProcessMessage.
DEF BUFFER delWebForm              FOR qa_core.WebForm.
DEF BUFFER delWebFormField         FOR qa_core.WebFormField.

UpdateBlk:
DO TRANS ON ERROR UNDO:
   
   FOR EACH qa_core.ProcessMap NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessMap EXCLUSIVE-LOCK
         WHERE ROWID(delProcessMap) = ROWID(qa_core.ProcessMap) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessMap THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessMap.
   END.
   
   FOR EACH core.ProcessMap NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.ProcessMap.
      BUFFER-COPY core.ProcessMap TO qa_core.ProcessMap.
   END.
   
   
   FOR EACH qa_core.ProcessEvent NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessEvent EXCLUSIVE-LOCK
         WHERE ROWID(delProcessEvent) = ROWID(qa_core.ProcessEvent) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessEvent THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessEvent.
   END.
   
   FOR EACH core.ProcessEvent NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.ProcessEvent.
      BUFFER-COPY core.ProcessEvent TO qa_core.ProcessEvent.
   END.
   
   
   FOR EACH qa_core.ProcessProgram NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessProgram EXCLUSIVE-LOCK
         WHERE ROWID(delProcessProgram) = ROWID(qa_core.ProcessProgram) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessProgram THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessProgram.
   END.
   
   FOR EACH core.ProcessProgram NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.ProcessProgram.
      BUFFER-COPY core.ProcessProgram TO qa_core.ProcessProgram.
   END.
   
   
   FOR EACH qa_core.ProcessRoute NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessRoute EXCLUSIVE-LOCK
         WHERE ROWID(delProcessRoute) = ROWID(qa_core.ProcessRoute) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessRoute THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessRoute.
   END.
   
   FOR EACH core.ProcessRoute NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.ProcessRoute.
      BUFFER-COPY core.ProcessRoute TO qa_core.ProcessRoute.
   END.
   
   
   FOR EACH qa_core.ProcessOption NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessOption EXCLUSIVE-LOCK
         WHERE ROWID(delProcessOption) = ROWID(qa_core.ProcessOption) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessOption THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessOption.
   END.
   
   FOR EACH core.ProcessOption NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.ProcessOption.
      BUFFER-COPY core.ProcessOption TO qa_core.ProcessOption.
   END.
   
      
   FOR EACH qa_core.ProcessMessage NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delProcessMessage EXCLUSIVE-LOCK
         WHERE ROWID(delProcessMessage) = ROWID(qa_core.ProcessMessage) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delProcessMessage THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delProcessMessage.
   END.
   
   FOR EACH core.ProcessMessage NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.ProcessMessage.
      BUFFER-COPY core.ProcessMessage TO qa_core.ProcessMessage.
   END.
   
   FOR EACH qa_core.WebForm NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delWebForm EXCLUSIVE-LOCK
         WHERE ROWID(delWebForm) = ROWID(qa_core.WebForm) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delWebForm THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delWebForm.
   END.
   
   FOR EACH core.WebForm NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.WebForm.
      BUFFER-COPY core.WebForm TO qa_core.WebForm.
   END.

   FOR EACH qa_core.WebFormField NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FIND delWebFormField EXCLUSIVE-LOCK
         WHERE ROWID(delWebFormField) = ROWID(qa_core.WebFormField) NO-ERROR NO-WAIT.
      
      IF NOT AVAIL delWebFormField THEN
        UNDO UpdateBlk, LEAVE UpdateBlk.
      
      DELETE delWebFormField.
   END.
   
   FOR EACH core.WebFormField NO-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      CREATE qa_core.WebFormField.
      BUFFER-COPY core.WebFormField TO qa_core.WebFormField.
   END.

   logCompletedOk = TRUE.
   
END. /*DO TRANS ON ERROR UNDO:*/

/* Update Sequences */
CURRENT-VALUE(ProcessMap,qa_core)           = CURRENT-VALUE(ProcessMap,core).
CURRENT-VALUE(ProcessEvent,qa_core)         = CURRENT-VALUE(ProcessEvent,core).
CURRENT-VALUE(ProcessProgram,qa_core)       = CURRENT-VALUE(ProcessProgram,core).
CURRENT-VALUE(ProcessRoute,qa_core)         = CURRENT-VALUE(ProcessRoute,core).
CURRENT-VALUE(ProcessOption,qa_core)        = CURRENT-VALUE(ProcessOption,core).
CURRENT-VALUE(ProcessMessage,qa_core)       = CURRENT-VALUE(ProcessMessage,core).
CURRENT-VALUE(WebForm,qa_core)              = CURRENT-VALUE(WebForm,core).
CURRENT-VALUE(WebFormField,qa_core)         = CURRENT-VALUE(WebFormField,core).

IF logCompletedOk = TRUE THEN
   MESSAGE SKIP "Data Migration Successful." VIEW-AS ALERT-BOX.
ELSE
   MESSAGE SKIP "Data Migration Failed." VIEW-AS ALERT-BOX.
   

