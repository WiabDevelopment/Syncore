/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pcsCheckOptionsWithMsg.p 
Purpose : Checks whether a system defined option is ON or OFF

          Possible Results : Yes/No        

Author  : DCummins
Date    : 20th June 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
24/4/2015  ND  CanonTlb    Remade pcsCheckOptionsWithMsg.p to include all config tables to phase out ProcessOptions table
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
   DEFINE VARIABLE chrSsnGetOptions AS SessionValue.
   
   DEFINE VARIABLE hdlTableQuery    AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlTableName     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlFieldRecord   AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlDataQuery     AS HANDLE      NO-UNDO.
   DEFINE VARIABLE hdlSourceDbTable AS HANDLE      NO-UNDO.
   DEFINE VARIABLE chrSourceDb      AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrTableWhere    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrDbTable       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrWhereClause   AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrOption        AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE chrGetOptions    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE logFound         AS LOGICAL     NO-UNDO.
   
   
   ASSIGN chrTableWhere = "FOR EACH _file WHERE _file._tbl-type = 'T'".
   
   CREATE BUFFER hdlFieldRecord FOR TABLE "core._field".
   
   
   /* Get Current Data */
   ASSIGN
      chrSsnGetOptions = fGetSessionValue("GetOptions")
      chrGetOptions    = chrSsnGetOptions:chrValue.
      
   

   CREATE QUERY hdlTableQuery.
   CREATE BUFFER hdlTableName FOR TABLE "core._file".
   hdlTableQuery:ADD-BUFFER(hdlTableName) NO-ERROR.
   hdlTableQuery:QUERY-PREPARE(chrTableWhere) NO-ERROR.
   hdlTableQuery:QUERY-OPEN NO-ERROR.

/*IF logGblDebugging THEN                                          */
/*   PUT STREAM sToCronLog UNFORMATTED NOW " Before MainBlk." SKIP.*/

   Main_Block:
   DO ON ERROR UNDO:

      TableLoop:
      REPEAT ON ERROR UNDO Main_Block, LEAVE Main_Block:

         hdlTableQuery:GET-NEXT.
         IF hdlTableQuery:QUERY-OFF-END THEN LEAVE.
         
         /* The _file-name here is the actual Db Table name */
         ASSIGN chrDbTable = hdlTableName:BUFFER-FIELD("_file-name"):BUFFER-VALUE.

         /* Don't want to copy down Session info as this will be handled locally */
         IF NOT chrDbTable MATCHES("*Config") THEN
           NEXT TableLoop.

         chrWhereClause = "FIND FIRST " + chrDbTable + " NO-LOCK NO-ERROR.".
         logFound = hdlFieldRecord:FIND-FIRST("WHERE _file-recid = '" + STRING(hdlTableName:RECID) + "'"
                                           + "AND _field-name = '" + STRING(chrGetOptions) + "'", NO-LOCK) NO-ERROR.

         IF NOT logFound THEN
            NEXT TableLoop.
            
         chrWhereClause = "FOR EACH " + chrDbTable + " NO-LOCK".

         /* Query for the Source Db table */
         CREATE QUERY hdlDataQuery.
         
         CREATE BUFFER hdlSourceDbTable FOR TABLE "core." + chrDbTable.
         hdlDataQuery:ADD-BUFFER(hdlSourceDbTable).
         hdlDataQuery:QUERY-PREPARE(chrWhereClause).
         hdlDataQuery:QUERY-OPEN.
         
         RecordLoop:
         REPEAT:
            hdlDataQuery:GET-NEXT.
            IF hdlDataQuery:QUERY-OFF-END THEN
               LEAVE RecordLoop.
               
            chrResult = hdlSourceDbTable:BUFFER-FIELD(chrGetOptions):STRING-VALUE.

            IF logFound THEN
               LEAVE Main_Block.
            
         END.
         
       END.
         
   END. /*Main_Block: DO ON ERROR UNDO:*/
   
   /* Clean Up */
   DELETE OBJECT chrSsnGetOptions NO-ERROR.
   hdlDataQuery:QUERY-CLOSE()    NO-ERROR.
   hdlTableQuery:QUERY-CLOSE()    NO-ERROR.
   DELETE OBJECT hdlTableName     NO-ERROR.
   DELETE OBJECT hdlSourceDbTable NO-ERROR.
   DELETE OBJECT hdlDataQuery     NO-ERROR.
   DELETE OBJECT hdlTableQuery    NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END. /* CTRL-C Catch */
