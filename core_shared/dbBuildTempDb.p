/*------------------------------------------------------------------------------------------------------------------------------------------
Program : dbBuildTempDb.p
Purpose : This program deletes the temp.db from the current environment and then programatically builds a new temp db schema from initially 
          the core and cust dbs. Add various bespoke indexes at the end which are needed to speed up queries and maintain data integrity.
Author  : BG
Date    : 23rd Feb 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
08/01/2016 CS  NextelBr   Added unique UserTransactionID to allow record tracking from each unique libDataManager call within the same 
                          session.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* UNDO Vars */
DEFINE VARIABLE recFileRecid         AS RECID       NO-UNDO.
DEFINE VARIABLE intCount             AS INTEGER     NO-UNDO.
DEFINE VARIABLE intFieldNum          AS INTEGER     NO-UNDO.
DEFINE VARIABLE intOrder             AS INTEGER     NO-UNDO.
DEFINE VARIABLE intHighestOrder      AS INTEGER     NO-UNDO.
DEFINE VARIABLE intIndexSeq          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCount               AS INTEGER     NO-UNDO.
DEFINE VARIABLE intDbCount           AS INTEGER     NO-UNDO.
DEFINE VARIABLE chrDbName            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hdlTableHandle       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlTableQuery        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlFieldQuery        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlIndexQuery        AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlIndexFieldQuery   AS HANDLE      NO-UNDO.
DEFINE VARIABLE chrWhereClause       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logValidQry          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE chrTableName         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hdlFieldHandle       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlIndexHandle       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hdlIndexFieldHandle  AS HANDLE      NO-UNDO.
DEFINE VARIABLE chrTempDbPrefix      AS CHARACTER   NO-UNDO INIT "ssn".
DEFINE VARIABLE logCompletedOK       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE intTableAreaNo       AS INTEGER     NO-UNDO.
DEFINE VARIABLE intIndexAreaNo       AS INTEGER     NO-UNDO.

/* Buffers */
DEFINE BUFFER indexArea FOR _area.
DEFINE BUFFER tableArea FOR _area.


/* Get the areas to be used for temp Db Tables and Indexes */
FIND FIRST Config NO-LOCK.

/* Should always be a "Schema Area" as a fall back position */
FIND temp._area NO-LOCK WHERE _area-name = "Schema Area" NO-ERROR.
IF AVAILABLE temp._area THEN
   ASSIGN intTableAreaNo = _area-number
          intIndexAreaNo = _area-number.

IF Config.DbAreaForTables <> "" THEN 
   FIND temp._area NO-LOCK WHERE _area-name = Config.DbAreaForTables NO-ERROR.
IF AVAILABLE temp._area THEN
   intTableAreaNo = _area-number.
RELEASE temp._area NO-ERROR.

IF Config.DbAreaForIndexes <> "" THEN 
   FIND temp._area NO-LOCK WHERE _area-name = Config.DbAreaForIndexes NO-ERROR.
IF AVAILABLE temp._area THEN
   intIndexAreaNo = _area-number.

/* Get the highest index sequence and add 1 */
FOR EACH core._index:
   
   IF core._index._idx-num > intIndexSeq THEN
      intIndexSeq = core._index._idx-num.
END.
intIndexSeq = intIndexSeq + 1.

FIND temp._db NO-LOCK.

UpdateBlk:
DO TRANSACTION ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
   
   /* First DELETE the current temp Db */
   FOR EACH temp._file EXCLUSIVE-LOCK
      WHERE _hidden = FALSE ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
      
      FOR EACH temp._index OF temp._file EXCLUSIVE-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
         
         FOR EACH temp._index-field OF temp._index EXCLUSIVE-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
         
            DELETE temp._index-field.
         END.
         DELETE temp._index.
      END. /*FOR EACH temp._index OF temp._file:*/
      
      FOR EACH temp._field OF temp._file EXCLUSIVE-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
         
         DELETE temp._field.
      END.
      
      FOR EACH temp._File-trig OF temp._file EXCLUSIVE-LOCK ON ERROR UNDO UpdateBlk, LEAVE UpdateBlk:
         
         DELETE temp._File-trig.
      END.
      
      DELETE temp._file.
      
   END. /*FOR EACH temp._file */
   
   
   /* Go through connected Dbs */
   DbLoop:
   DO intDbCount = 1 TO NUM-DBS ON ERROR UNDO, RETURN ERROR:
      
      /* Only replicate those Dbs that we're using classes to update - initially core & cust*/
      IF LOOKUP(LDBNAME(intDbCount),Config.DbsUsingClasses) = 0 THEN
         NEXT DbLoop.
      
      /* Dynamic Query for the _file table so we can prefix table with DbName */
      CREATE QUERY hdlTableQuery.
      CREATE BUFFER hdlTableHandle FOR TABLE LDBNAME(intDbCount) + "._file".
      hdlTableQuery:ADD-BUFFER(hdlTableHandle) NO-ERROR.
      
      logValidQry = hdlTableQuery:QUERY-PREPARE("FOR EACH _file WHERE _tbl-type = 'T' BY _file-number").
      IF NOT logValidQry THEN
      DO ON ERROR UNDO, RETURN ERROR:
         MESSAGE "Bad Query:" + "FOR EACH _file WHERE _tbl-type = 'T' BY _file-number" + "." VIEW-AS ALERT-BOX.
         UNDO UpdateBlk, LEAVE UpdateBlk.
      END.
      
      hdlTableQuery:QUERY-OPEN NO-ERROR.
      
      TableLoop:
      REPEAT ON ERROR UNDO, RETURN ERROR:
         
         hdlTableQuery:GET-NEXT.
         IF hdlTableQuery:QUERY-OFF-END THEN 
            LEAVE TableLoop.
         
         /* The field here is the actual Db Table name*/
         chrTableName = hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE.
         
         /* Skip these tables */
         IF (hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE = "UserSession"
            OR hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE = "UserSessionBusUnitLink"
            OR hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE = "UserSessionCrumb") THEN
            NEXT TableLoop.         

         ASSIGN intFieldNum     = 1
                intOrder        = 0
                intHighestOrder = 0.
         
         CREATE temp._file.
         ASSIGN temp._file._db-recid      = RECID(temp._db)
                temp._file._dump-name     = chrTempDbPrefix + hdlTableHandle:BUFFER-FIELD("_dump-name"):BUFFER-VALUE
                temp._file._file-name     = chrTempDbPrefix + hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE
                temp._file._ianum         = intTableAreaNo
                temp._file._desc          = hdlTableHandle:BUFFER-FIELD("_desc"):BUFFER-VALUE
                temp._file._valexp        = hdlTableHandle:BUFFER-FIELD("_valexp"):BUFFER-VALUE
                temp._file._valmsg        = hdlTableHandle:BUFFER-FIELD("_valmsg"):BUFFER-VALUE
                temp._file._hidden        = hdlTableHandle:BUFFER-FIELD("_hidden"):BUFFER-VALUE
                temp._file._frozen        = hdlTableHandle:BUFFER-FIELD("_frozen"):BUFFER-VALUE
                temp._file._can-dump      = hdlTableHandle:BUFFER-FIELD("_can-dump"):BUFFER-VALUE
                temp._file._can-load      = hdlTableHandle:BUFFER-FIELD("_can-load"):BUFFER-VALUE
                temp._file._file-label    = hdlTableHandle:BUFFER-FIELD("_file-label"):BUFFER-VALUE
                temp._file._file-label-sa = hdlTableHandle:BUFFER-FIELD("_file-label-sa"):BUFFER-VALUE
                temp._file._for-cnt1      = hdlTableHandle:BUFFER-FIELD("_for-cnt1"):BUFFER-VALUE
                temp._file._for-cnt2      = hdlTableHandle:BUFFER-FIELD("_for-cnt2"):BUFFER-VALUE
                temp._file._for-flag      = hdlTableHandle:BUFFER-FIELD("_for-flag"):BUFFER-VALUE
                temp._file._for-format    = hdlTableHandle:BUFFER-FIELD("_for-format"):BUFFER-VALUE
                temp._file._for-id        = hdlTableHandle:BUFFER-FIELD("_for-id"):BUFFER-VALUE
                temp._file._for-info      = hdlTableHandle:BUFFER-FIELD("_for-info"):BUFFER-VALUE
                temp._file._for-name      = hdlTableHandle:BUFFER-FIELD("_for-name"):BUFFER-VALUE
                temp._file._for-number    = hdlTableHandle:BUFFER-FIELD("_for-number"):BUFFER-VALUE
                temp._file._for-owner     = hdlTableHandle:BUFFER-FIELD("_for-owner"):BUFFER-VALUE
                temp._file._for-size      = hdlTableHandle:BUFFER-FIELD("_for-size"):BUFFER-VALUE
                temp._file._for-type      = hdlTableHandle:BUFFER-FIELD("_for-type"):BUFFER-VALUE
                temp._file._valmsg-sa     = hdlTableHandle:BUFFER-FIELD("_valmsg-sa"):BUFFER-VALUE
                temp._file._can-create    = hdlTableHandle:BUFFER-FIELD("_can-create"):BUFFER-VALUE
                temp._file._can-delete    = hdlTableHandle:BUFFER-FIELD("_can-delete"):BUFFER-VALUE
                temp._file._can-read      = hdlTableHandle:BUFFER-FIELD("_can-read"):BUFFER-VALUE
                temp._file._can-write     = hdlTableHandle:BUFFER-FIELD("_can-write"):BUFFER-VALUE
                temp._file._dft-pk        = hdlTableHandle:BUFFER-FIELD("_dft-pk"):BUFFER-VALUE.
         
         DO intCount = 1 TO 8 ON ERROR UNDO, RETURN ERROR:
            
            ASSIGN temp._file._fil-Misc1[intCount] = hdlTableHandle:BUFFER-FIELD("_fil-Misc1"):BUFFER-VALUE(intCount)
                   temp._file._fil-Misc2[intCount] = hdlTableHandle:BUFFER-FIELD("_fil-Misc2"):BUFFER-VALUE(intCount).
         END.  /* do intCount = 1 to 8 */
         
         recFileRecid = RECID(temp._file).
         
         /* Create a trigger for WRITE event of WebLock so that it will update its children */
         IF temp._file._file-name = "ssnWebLock" THEN
         DO ON ERROR UNDO, RETURN ERROR:
            
            CREATE temp._File-trig.
            ASSIGN temp._File-trig._File-Recid    = recFileRecid
                   temp._File-trig._Event         = "WRITE"
                   temp._File-trig._Proc-Name     = "wrssnWebLock.p"
                   temp._File-trig._Override      = NO
                   temp._File-trig._Trig-Crc      = ?
                   temp._File-trig._Mod-sequence  = ?.
         END.
         
         /* Create a trigger for DELETE event of WebLock so that it will delete its children */
         IF temp._file._file-name = "ssnWebLock" THEN
         DO ON ERROR UNDO, RETURN ERROR:
            
            CREATE temp._File-trig.
            ASSIGN temp._File-trig._File-Recid    = recFileRecid
                   temp._File-trig._Event         = "DELETE"
                   temp._File-trig._Proc-Name     = "dlssnWebLock.p"
                   temp._File-trig._Override      = NO
                   temp._File-trig._Trig-Crc      = ?
                   temp._File-trig._Mod-sequence  = ?.
         END.
         
         CREATE QUERY hdlFieldQuery.
         CREATE BUFFER hdlFieldHandle FOR TABLE LDBNAME(intDbCount) + "._field".
         hdlFieldQuery:ADD-BUFFER(hdlFieldHandle) NO-ERROR.
         
         logValidQry = hdlFieldQuery:QUERY-PREPARE("FOR EACH _field WHERE _file-recid = " + STRING(hdlTableHandle:RECID) 
                                                      + " BY _field-rpos").
         IF NOT logValidQry THEN
         DO ON ERROR UNDO, RETURN ERROR:
            MESSAGE SKIP "Bad Query:" + "FOR EACH _field WHERE _file-recid = " + STRING(hdlTableHandle:RECID) 
                            + " BY _field-rpos" + "." VIEW-AS ALERT-BOX.
            UNDO UpdateBlk, LEAVE UpdateBlk.
         END.
         
         hdlFieldQuery:QUERY-OPEN NO-ERROR.
         
         FieldLoop:
         REPEAT ON ERROR UNDO, RETURN ERROR:
            
            hdlFieldQuery:GET-NEXT.
            IF hdlFieldQuery:QUERY-OFF-END THEN 
               LEAVE FieldLoop.
            
            intFieldNum = intFieldNum + 1.
            
            /* Create dummy fields to preserve _file._numfld count */
            DO intFieldNum = intFieldNum TO (hdlFieldHandle:BUFFER-FIELD("_field-rpos"):BUFFER-VALUE - 1) ON ERROR UNDO, RETURN ERROR:
               
               intOrder = intOrder + 1.
               
               CREATE temp._field.
               ASSIGN temp._field._file-recid  = RECID(temp._file)
                      temp._field._field-name  = "xxxxxxxxxxxxx" + STRING(intOrder,"999999")
                      temp._field._order       = 10000000 - intOrder 
                      temp._field._mandatory   = no
                      temp._field._data-type   = "character" 
                      temp._field._decimals    = ?
                      temp._field._extent      = 0
                      temp._field._col-label   = ?
                      temp._field._fld-case    = no.
               
            END.  /* do intFieldNum = intFieldNum to hdlTableHandle:BUFFER-FIELD("_field._field-rpos - 1 */
            
            CREATE temp._field.
            ASSIGN temp._field._file-recid    = RECID(temp._file)
                   temp._field._field-name    = hdlFieldHandle:BUFFER-FIELD("_field-name"):BUFFER-VALUE
                   temp._field._order         = hdlFieldHandle:BUFFER-FIELD("_order"):BUFFER-VALUE
                   temp._field._mandatory     = hdlFieldHandle:BUFFER-FIELD("_mandatory"):BUFFER-VALUE
                   temp._field._data-type     = hdlFieldHandle:BUFFER-FIELD("_data-type"):BUFFER-VALUE
                   temp._field._format        = hdlFieldHandle:BUFFER-FIELD("_format"):BUFFER-VALUE
                   temp._field._initial       = hdlFieldHandle:BUFFER-FIELD("_initial"):BUFFER-VALUE
                   temp._field._label         = hdlFieldHandle:BUFFER-FIELD("_label"):BUFFER-VALUE
                   temp._field._decimals      = hdlFieldHandle:BUFFER-FIELD("_decimals"):BUFFER-VALUE
                   temp._field._extent        = hdlFieldHandle:BUFFER-FIELD("_extent"):BUFFER-VALUE
                   temp._field._valexp        = hdlFieldHandle:BUFFER-FIELD("_valexp"):BUFFER-VALUE
                   temp._field._valmsg        = hdlFieldHandle:BUFFER-FIELD("_valmsg"):BUFFER-VALUE
                   temp._field._help          = hdlFieldHandle:BUFFER-FIELD("_help"):BUFFER-VALUE
                   temp._field._desc          = hdlFieldHandle:BUFFER-FIELD("_desc"):BUFFER-VALUE
                   temp._field._col-label     = hdlFieldHandle:BUFFER-FIELD("_col-label"):BUFFER-VALUE
                   temp._field._fld-case      = hdlFieldHandle:BUFFER-FIELD("_fld-case"):BUFFER-VALUE
                   temp._field._can-read      = hdlFieldHandle:BUFFER-FIELD("_can-read"):BUFFER-VALUE
                   temp._field._can-write     = hdlFieldHandle:BUFFER-FIELD("_can-write"):BUFFER-VALUE
                   temp._field._col-label-sa  = hdlFieldHandle:BUFFER-FIELD("_col-label-sa"):BUFFER-VALUE
                   temp._field._fld-stdtype   = hdlFieldHandle:BUFFER-FIELD("_fld-stdtype"):BUFFER-VALUE
                   temp._field._fld-stlen     = hdlFieldHandle:BUFFER-FIELD("_fld-stlen"):BUFFER-VALUE
                   temp._field._fld-stoff     = hdlFieldHandle:BUFFER-FIELD("_fld-stoff"):BUFFER-VALUE
                   temp._field._for-allocated = hdlFieldHandle:BUFFER-FIELD("_for-allocated"):BUFFER-VALUE
                   temp._field._for-id        = hdlFieldHandle:BUFFER-FIELD("_for-id"):BUFFER-VALUE
                   temp._field._for-itype     = hdlFieldHandle:BUFFER-FIELD("_for-itype"):BUFFER-VALUE
                   temp._field._for-maxsize   = hdlFieldHandle:BUFFER-FIELD("_for-maxsize"):BUFFER-VALUE
                   temp._field._for-name      = hdlFieldHandle:BUFFER-FIELD("_for-name"):BUFFER-VALUE
                   temp._field._for-primary   = hdlFieldHandle:BUFFER-FIELD("_for-primary"):BUFFER-VALUE
                   temp._field._for-retrieve  = hdlFieldHandle:BUFFER-FIELD("_for-retrieve"):BUFFER-VALUE
                   temp._field._for-scale     = hdlFieldHandle:BUFFER-FIELD("_for-scale"):BUFFER-VALUE
                   temp._field._for-separator = hdlFieldHandle:BUFFER-FIELD("_for-separator"):BUFFER-VALUE
                   temp._field._for-spacing   = hdlFieldHandle:BUFFER-FIELD("_for-spacing"):BUFFER-VALUE
                   temp._field._for-type      = hdlFieldHandle:BUFFER-FIELD("_for-type"):BUFFER-VALUE
                   temp._field._for-xpos      = hdlFieldHandle:BUFFER-FIELD("_for-xpos"):BUFFER-VALUE
                   temp._field._format-sa     = hdlFieldHandle:BUFFER-FIELD("_format-sa"):BUFFER-VALUE
                   temp._field._help-sa       = hdlFieldHandle:BUFFER-FIELD("_help-sa"):BUFFER-VALUE
                   temp._field._initial-sa    = hdlFieldHandle:BUFFER-FIELD("_initial-sa"):BUFFER-VALUE
                   temp._field._label-sa      = hdlFieldHandle:BUFFER-FIELD("_label-sa"):BUFFER-VALUE
                   temp._field._valmsg-sa     = hdlFieldHandle:BUFFER-FIELD("_valmsg-sa"):BUFFER-VALUE
                   temp._field._view-as       = hdlFieldHandle:BUFFER-FIELD("_view-as"):BUFFER-VALUE.
            
            DO intCount = 1 TO 8 ON ERROR UNDO, RETURN ERROR:
               ASSIGN temp._field._fld-Misc1[intCount] = hdlFieldHandle:BUFFER-FIELD("_fld-Misc1"):BUFFER-VALUE(intCount)
                      temp._field._fld-Misc2[intCount] = hdlFieldHandle:BUFFER-FIELD("_fld-Misc2"):BUFFER-VALUE(intCount).
            END.  /* do intCount = 1 to 8 */
            
            IF temp._field._Order > intHighestOrder THEN
               intHighestOrder = temp._field._Order.
            
         END.  /* for each _field of _file */
         
         /*Extra fields*/
         /* Need to add these fields to every table so we can trace them on the web */
         CREATE temp._Field.
         ASSIGN temp._Field._File-recid = RECID(temp._file)
                temp._Field._Field-Name = "SessionID"
                temp._Field._Data-Type  = "Character"
                temp._Field._Order      = intHighestOrder + 10
                temp._Field._Format     = "x(60)"
                temp._Field._Initial    = ""
                temp._Field._Help       = "Session ID of the User who is Logged In".
         
         CREATE temp._Field.
         ASSIGN temp._Field._File-recid = RECID(temp._file)
                temp._Field._Field-Name = "Committed"
                temp._Field._Data-Type  = "Character"
                temp._Field._Order      = intHighestOrder + 20
                temp._Field._Format     = "x(17)"
                temp._Field._Initial    = ""
                temp._Field._Help       = "Timestamp when this Create/Update was committed to the Db".
         
         CREATE temp._Field.
         ASSIGN temp._Field._File-recid = RECID(temp._file)
                temp._Field._Field-Name = "ProcessMode"
                temp._Field._Data-Type  = "Character"
                temp._Field._Order      = intHighestOrder + 30
                temp._Field._Format     = "x(6)"
                temp._Field._Initial    = ""
                temp._Field._Help       = "Whether user is doing a Create, Update or Delete".
         
         CREATE temp._Field.
         ASSIGN temp._Field._File-recid = RECID(temp._file)
                temp._Field._Field-Name = "Outcome"
                temp._Field._Data-Type  = "Character"
                temp._Field._Order      = intHighestOrder + 40
                temp._Field._Format     = "x(30)"
                temp._Field._Initial    = ""
                temp._Field._Help       = "The outcome of the update whether written, cancelled, error etc".
                
         CREATE temp._Field.
         ASSIGN temp._Field._File-recid = RECID(temp._file)
                temp._Field._Field-Name = "UserTransactionID"
                temp._Field._Data-Type  = "Integer"
                temp._Field._Order      = intHighestOrder + 50
                temp._Field._Format     = ">>>>>>>>9"
                temp._Field._Initial    = ""
                temp._Field._Help       = "UserTransactionID sequence that was used to Create/Update to the Db".       
         
         /* Indexes */
         CREATE QUERY hdlIndexQuery.
         CREATE BUFFER hdlIndexHandle FOR TABLE LDBNAME(intDbCount) + "._Index".
         hdlIndexQuery:ADD-BUFFER(hdlIndexHandle) NO-ERROR.
         
         logValidQry = hdlIndexQuery:QUERY-PREPARE("FOR EACH _Index WHERE _file-recid = " + STRING(hdlTableHandle:RECID)).
         IF NOT logValidQry THEN
         DO ON ERROR UNDO, RETURN ERROR:
            MESSAGE SKIP "Bad Query:" + "FOR EACH _Index WHERE _file-recid = " + STRING(hdlTableHandle:RECID) + "." VIEW-AS ALERT-BOX.
            UNDO UpdateBlk, LEAVE UpdateBlk.
         END.
         
         hdlIndexQuery:QUERY-OPEN NO-ERROR.
         
         IndexLoop:
         REPEAT ON ERROR UNDO, RETURN ERROR:
            
            hdlIndexQuery:GET-NEXT.
            IF hdlIndexQuery:QUERY-OFF-END THEN 
               LEAVE IndexLoop.
            
            CREATE temp._index.
            ASSIGN temp._index._file-recid = RECID(temp._file)
                   temp._index._index-name = hdlIndexHandle:BUFFER-FIELD("_index-name"):BUFFER-VALUE
                   temp._index._unique     = /*_index._unique*/ FALSE /*Don't want unique indexes*/
                   intIndexSeq             = intIndexSeq + 1
                   temp._index._idx-num    = intIndexSeq
                   /*temp._index._idx-num    = hdlIndexHandle:BUFFER-FIELD("_idx-num"):BUFFER-VALUE*/
                   temp._index._active     = hdlIndexHandle:BUFFER-FIELD("_active"):BUFFER-VALUE
                   temp._index._wordidx    = hdlIndexHandle:BUFFER-FIELD("_wordidx"):BUFFER-VALUE
                   temp._index._desc       = hdlIndexHandle:BUFFER-FIELD("_desc"):BUFFER-VALUE
                   temp._index._for-name   = hdlIndexHandle:BUFFER-FIELD("_for-name"):BUFFER-VALUE
                   temp._index._for-type   = hdlIndexHandle:BUFFER-FIELD("_for-type"):BUFFER-VALUE
                   temp._index._ianum      = intIndexAreaNo.
            
            DO intCount = 1 TO 8 ON ERROR UNDO, RETURN ERROR:
               
               ASSIGN temp._index._i-Misc1[intCount] = hdlIndexHandle:BUFFER-FIELD("_i-Misc1"):BUFFER-VALUE[intCount]
                      temp._index._i-Misc2[intCount] = hdlIndexHandle:BUFFER-FIELD("_i-Misc2"):BUFFER-VALUE[intCount].
            END.  /* do intCount = 1 to 8 */
            
            IF hdlTableHandle:BUFFER-FIELD("_prime-index"):BUFFER-VALUE = hdlIndexHandle:RECID THEN
               temp._file._prime-index = RECID(temp._index).
            
            /* Index Fields */
            CREATE QUERY hdlIndexFieldQuery.
            CREATE BUFFER hdlIndexFieldHandle FOR TABLE LDBNAME(intDbCount) + "._Index-Field".
            hdlIndexFieldQuery:ADD-BUFFER(hdlIndexFieldHandle) NO-ERROR.
            
            logValidQry = hdlIndexFieldQuery:QUERY-PREPARE("FOR EACH _index-field WHERE _index-recid = " + STRING(hdlIndexHandle:RECID)).
            IF NOT logValidQry THEN
            DO ON ERROR UNDO, RETURN ERROR:
               MESSAGE SKIP "Bad Query:" + "FOR EACH _index-Field WHERE _index-recid = " 
                               + STRING(hdlIndexHandle:RECID) + "." VIEW-AS ALERT-BOX.
               UNDO UpdateBlk, LEAVE UpdateBlk.
            END.
            
            hdlIndexFieldQuery:QUERY-OPEN NO-ERROR.
            
            IndexFieldLoop:
            REPEAT ON ERROR UNDO, RETURN ERROR:
               
               hdlIndexFieldQuery:GET-NEXT.
               IF hdlIndexFieldQuery:QUERY-OFF-END THEN 
                  LEAVE IndexFieldLoop.
               
               hdlFieldHandle:FIND-UNIQUE("WHERE RECID(" + hdlFieldHandle:TABLE + ") = " 
                                             + STRING(hdlIndexFieldHandle:BUFFER-FIELD("_field-recid"):BUFFER-VALUE) + ""). 
               FIND temp._field OF temp._file NO-LOCK
                  WHERE temp._field._field-name = hdlFieldHandle:BUFFER-FIELD("_field-name"):BUFFER-VALUE.
               
               CREATE temp._index-field.
               ASSIGN temp._index-field._index-recid = RECID(temp._index)
                      temp._index-field._field-recid = RECID(temp._field)
                      temp._index-field._index-seq   = hdlIndexFieldHandle:BUFFER-FIELD("_index-seq"):BUFFER-VALUE
                      temp._index-field._ascending   = hdlIndexFieldHandle:BUFFER-FIELD("_ascending"):BUFFER-VALUE
                      temp._index-field._abbreviate  = hdlIndexFieldHandle:BUFFER-FIELD("_abbreviate"):BUFFER-VALUE
                      temp._index-field._unsorted    = hdlIndexFieldHandle:BUFFER-FIELD("_unsorted"):BUFFER-VALUE.
               
               DO intCount = 1 TO 8 ON ERROR UNDO, RETURN ERROR:
                  
                  ASSIGN temp._index-field._if-Misc1[intCount] = hdlIndexFieldHandle:BUFFER-FIELD("_if-Misc1"):BUFFER-VALUE[intCount]
                         temp._index-field._if-Misc2[intCount] = hdlIndexFieldHandle:BUFFER-FIELD("_if-Misc2"):BUFFER-VALUE[intCount].
               END.  /* do intCount = 1 to 8 */
               
            END. /* for each _index-field of _index */
            
         END. /* for each _index of _file */
         
         /* Need an extra Index on the ssnSessionVariable table for SessionID And VariableName - a lot of searches done on these fields */
         IF temp._file._file-name = "ssnSessionVariable" THEN
         DO ON ERROR UNDO, RETURN ERROR:
            
            CREATE temp._index.
            ASSIGN temp._index._File-recid = RECID(temp._file)
                   temp._index._index-Name = "SessionIDVariableName"
                   temp._index._Unique     = NO
                   intIndexSeq             = intIndexSeq + 1
                   temp._index._idx-num    = intIndexSeq
                   temp._index._ianum      = intIndexAreaNo.
            
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = "SessionID".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 1.
            
            FIND temp._field OF temp._file NO-LOCK
              WHERE temp._field._field-name = "VariableName".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 2.
                   
            FIND temp._field OF temp._file NO-LOCK
              WHERE temp._field._field-name = "UserTransactionID".
         
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 3.   
            
         END. /*IF temp._file._file-name = "ssnSessionVariable" THEN*/
         
         /* Need an extra Index on the ssnSessionVariable table for SessionID And ProcessID - a lot of searches done on these fields */
         IF temp._file._file-name = "ssnSessionVariable" THEN
         DO ON ERROR UNDO, RETURN ERROR:
            
            CREATE temp._index.
            ASSIGN temp._index._File-recid = RECID(temp._file)
                   temp._index._index-Name = "SessionIDProcessID"
                   temp._index._Unique     = NO
                   intIndexSeq             = intIndexSeq + 1
                   temp._index._idx-num    = intIndexSeq
                   temp._index._ianum      = intIndexAreaNo.
            
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = "SessionID".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 1.
                   
            FIND temp._field OF temp._file NO-LOCK
              WHERE temp._field._field-name = "ProcessID".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 2.
            
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = "UserTransactionID".
         
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 3.   
            
         END. /*IF temp._file._file-name = "ssnSessionVariable" THEN*/
         
         /* Need extra Index on the ssnWebLock table for Completed and SessionID - big table and a lot of searches done on these fields */
         IF temp._file._file-name = "ssnWebLock" THEN
         DO ON ERROR UNDO, RETURN ERROR:
            
            CREATE temp._index.
            ASSIGN temp._index._File-recid = RECID(temp._file)
                   temp._index._index-Name = "CompletedSessionID"
                   temp._index._Unique     = NO
                   temp._index._Desc       = "Index to be used in Commit All"
                   intIndexSeq             = intIndexSeq + 1
                   temp._index._idx-num    = intIndexSeq
                   temp._index._ianum      = intIndexAreaNo.
            
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = "Completed".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 1.
            
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = "SessionID".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 2.
                   
            FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = "UserTransactionID".
         
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 3.          
                   
         END. /*IF temp._file._file-name = "ssnWebLock" THEN*/
         
         /* Need an extra Index on the ssnWebTransaction table for SessionID and Completed */
         IF temp._file._file-name = "ssnWebTransaction" THEN
         DO ON ERROR UNDO, RETURN ERROR:
            
            CREATE temp._index.
            ASSIGN temp._index._File-recid = RECID(temp._file)
                   temp._index._index-Name = "SessionIDCompleted"
                   temp._index._Unique     = NO
                   intIndexSeq             = intIndexSeq + 1
                   temp._index._idx-num    = intIndexSeq
                   temp._index._ianum      = intIndexAreaNo.
            
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = "SessionID".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 1.
            
            FIND temp._field OF temp._file NO-LOCK
              WHERE temp._field._field-name = "Completed".
            
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 2.
                   
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = "UserTransactionID".
         
            CREATE temp._index-field.
            ASSIGN temp._index-field._index-recid = RECID(temp._index)
                   temp._index-field._Field-recid = RECID(temp._field)
                   temp._index-field._index-seq   = 3.   
            
         END. /*IF temp._file._file-name = "ssnWebTransaction" THEN*/
         
         /* Bespoke Index for Committed, SessionID, and UserTransactionID for all Tables to be used in pCommitAll */
         CREATE temp._index.
         ASSIGN temp._index._File-recid = RECID(temp._file)
                temp._index._index-Name = "CommittedSessionID"
                temp._index._Unique     = NO
                temp._index._Desc       = "Index to be used in Commit All"
                intIndexSeq             = intIndexSeq + 1
                temp._index._idx-num    = intIndexSeq
                temp._index._ianum      = intIndexAreaNo.
         
         FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = "Committed".
         
         CREATE temp._index-field.
         ASSIGN temp._index-field._index-recid = RECID(temp._index)
                temp._index-field._Field-recid = RECID(temp._field)
                temp._index-field._index-seq   = 1.
         
         FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = "SessionID".
         
         CREATE temp._index-field.
         ASSIGN temp._index-field._index-recid = RECID(temp._index)
                temp._index-field._Field-recid = RECID(temp._field)
                temp._index-field._index-seq   = 2.
                
         FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = "UserTransactionID".
         
         CREATE temp._index-field.
         ASSIGN temp._index-field._index-recid = RECID(temp._index)
                temp._index-field._Field-recid = RECID(temp._field)
                temp._index-field._index-seq   = 3.       
         
         /* Unique Index for ID, Committed, SessionID and UserTransactionID for all Tables so that no Session can have more than one live update at a time */
         CREATE temp._index.
         ASSIGN temp._index._File-recid = RECID(temp._file)
                temp._index._index-Name = "UniqueIDCommittedSessionTransID"
                temp._index._Unique     = TRUE
                temp._index._Desc       = "Unique Index for ID, Committed, SessionID, and UserTransactionID - restrict to a single live updates per session"
                intIndexSeq             = intIndexSeq + 1
                temp._index._idx-num    = intIndexSeq
                temp._index._ianum      = intIndexAreaNo.
         
         FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE  + "ID" NO-ERROR.
         
         IF NOT AVAILABLE temp._field THEN
         DO ON ERROR UNDO, RETURN ERROR:
            
            MESSAGE "Missing Field:" + temp._file._file-name hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE  
                       + "ID" VIEW-AS ALERT-BOX.
            FIND temp._field OF temp._file NO-LOCK
               WHERE temp._field._field-name = hdlTableHandle:BUFFER-FIELD("_file-name"):BUFFER-VALUE  + "ID" NO-ERROR.
         END.
         
         CREATE temp._index-field.
         ASSIGN temp._index-field._index-recid = RECID(temp._index)
                temp._index-field._Field-recid = RECID(temp._field)
                temp._index-field._index-seq   = 1.
         
         FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = "Committed".
         
         CREATE temp._index-field.
         ASSIGN temp._index-field._index-recid = RECID(temp._index)
                temp._index-field._Field-recid = RECID(temp._field)
                temp._index-field._index-seq   = 2.
         
         FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = "SessionID".
         
         CREATE temp._index-field.
         ASSIGN temp._index-field._index-recid = RECID(temp._index)
                temp._index-field._Field-recid = RECID(temp._field)
                temp._index-field._index-seq   = 3.
         
         FIND temp._field OF temp._file NO-LOCK
            WHERE temp._field._field-name = "UserTransactionID".
            
         CREATE temp._index-field.
         ASSIGN temp._index-field._index-recid = RECID(temp._index)
                temp._index-field._Field-recid = RECID(temp._field)
                temp._index-field._index-seq   = 4.
         
         RELEASE temp._file NO-ERROR.
         
         /* Delete temp files */
         FIND temp._file WHERE RECID(temp._file) = recFileRecid NO-LOCK.
         
         FOR EACH temp._field OF temp._file
            WHERE temp._field._field-name BEGINS "xxxxxxxxxxxx" EXCLUSIVE-LOCK ON ERROR UNDO, RETURN ERROR:
            
            DELETE temp._field.
         END. /*FOR EACH temp._field OF temp._file*/
         
      END. /*TableLoop: REPEAT: for _file records */
      
   END. /*DO intDbCount = 1 TO NUM-DBS ON ERROR UNDO, RETURN ERROR:*/
   
   logCompletedOK = TRUE.
   
END. /*DO TRANSACTION:*/

IF logCompletedOK = TRUE THEN
   MESSAGE SKIP "Virtual Db created successfully." VIEW-AS ALERT-BOX.
ELSE
   MESSAGE SKIP "Error: Virtual Db NOT created successfully." VIEW-AS ALERT-BOX.
