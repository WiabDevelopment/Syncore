/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncStartLogging.i
Purpose : Outputs a stream to a LogFile and gets it ready for logging. Actual logging output is done in {fncLoggingFunctions.i}
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
  {fncServerFunctions.i} 
  {fncLoggingFunctions.i} 
***********************************/

IF intGblOperationTypeID <> 0 THEN
DO:
   FIND OperationType NO-LOCK
      WHERE OperationType.OperationTypeID = intGblOperationTypeID NO-ERROR.
END.

IF AVAILABLE OperationType AND OperationType.Debugging THEN
   logGblDebugging = TRUE.

IF logGblDebugging THEN
DO:
   /* Set the destination directory  */
   chrNewAgedDirectory = fGetAgedDirectory("../logs/", {1}).
   IF chrNewAgedDirectory BEGINS "Error" THEN
      chrTargetDirectory = "../logs/".              
   ELSE
      chrTargetDirectory = chrNewAgedDirectory.
   
   IF AVAILABLE UserSession THEN
      OUTPUT STREAM sToLogFile TO VALUE(chrTargetDirectory + STRING(UserSession.SessionID) + ".log") APPEND.
   IF AVAILABLE GateUser THEN
      PUT STREAM sToLogFile UNFORMATTED STRING(NOW) + " " + GateUser.FullName + " -> " + THIS-PROCEDURE:NAME SKIP.
END.
   
