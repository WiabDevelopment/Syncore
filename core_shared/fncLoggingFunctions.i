/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncLoggingFunctions.i
Purpose : Really just a stream amd a single function for Logging. The stream is begun in {fncStartLogging.i}
Author  : BG
Date    : 20th March 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/*********************************** 
  This include has a dependancy on 
  {defSessionVariables.i} 
***********************************/

/* Local VARs */
DEFINE VARIABLE chrNewAgedDirectory AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chrTargetDirectory  AS CHARACTER   NO-UNDO.

/* Streams */
DEF STREAM sToLogFile.

/* Functions */
FUNCTION fLog RETURNS CHARACTER (INPUT chrStringToLog AS CHARACTER):
   
   IF logGblDebugging THEN
      PUT STREAM sToLogFile UNFORMATTED STRING(NOW) + " " + chrStringToLog SKIP.
   
END FUNCTION. /* fLog */




