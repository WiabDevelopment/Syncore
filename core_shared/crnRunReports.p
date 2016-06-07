{defSessionVariables.i}
{fncGlobalFunctions.i}

DEFINE BUFFER updReportRun FOR ReportRun.

ReportRunLoop:
FOR EACH ReportRun NO-LOCK 
   WHERE ReportRun.Completed = ""
   AND   ReportRun.StartedOnCron = ""
   BY    ReportRun.Created:
   
   FIND FIRST Report OF ReportRun NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Report THEN
      NEXT ReportRunLoop.
   
   FIND FIRST ProcessProgram OF Report NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ProcessProgram THEN
      NEXT ReportRunLoop.
   
   FIND FIRST updReportRun EXCLUSIVE-LOCK
      WHERE ROWID(updReportRun) = ROWID(ReportRun) NO-ERROR NO-WAIT.
   IF NOT AVAILABLE updReportRun THEN
      NEXT ReportRunLoop.
   
   ReportRun.StartedOnCron = ftimeStamp(NOW).
   RUN VALUE(ProcessProgram.ProgramName).
   
END.


