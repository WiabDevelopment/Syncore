/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncWorkOrderFunctions.i
Purpose : All functions to do with WorkOrder
Author  : AGL
Date    : 09/12/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* This include has a dependency on */
/*{fncDateFunctions.i}                                  */
/*{defSessionVariables.i SESSION:CLIENT-TYPE NEW GLOBAL}*/
/*{fncGlobalFunctions.i}                                */

FUNCTION fNewWorkOrderRef RETURNS CHAR ():
   
   /*Buffers*/
   DEFINE BUFFER newCustConfig FOR CustConfig.
   
   /*Integers*/
   DEFINE VARIABLE intSequence AS INTEGER NO-UNDO INITIAL 1.
   DEFINE VARIABLE chrDay      AS CHARACTER.
   DEFINE VARIABLE chrMonth    AS CHARACTER.
   DEFINE VARIABLE chrYear     AS CHARACTER.
   
   ASSIGN chrDay   = fDay(fDateToTimestamp(TODAY))
          chrMonth = fMonth(fDateToTimestamp(TODAY))
          chrMonth = fGetMonthName(chrShortMonths, INTEGER(chrMonth)).
         
   FIND FIRST newCustConfig NO-LOCK NO-ERROR.
   IF NOT AVAIL newCustConfig THEN
      RETURN "".
   
   FOR EACH WorkOrder NO-LOCK
      WHERE WorkOrder.OrderRef MATCHES ("*" + chrMonth + chrDay + "*"):
      intSequence = intSequence + 1.    
   END.
      
   RETURN newCustConfig.CurrentPurchaseOrderNo + chrMonth + chrDay + "-" + STRING(intSequence,"999").
   
END FUNCTION. /* fNewWorkOrderRef */

