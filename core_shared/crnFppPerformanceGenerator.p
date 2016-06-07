/*------------------------------------------------------------------------------------------------------------------------------------------
Program : crnFppPerformanceGenerator.p
Purpose : This file is called by the cron program every minute and back populates data using available orders and current uph
          per 30 seconds time interval

Author  : Alexander Dolotov
Date    : 14/09/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/
 
/******************************************* Functions **********************************************************/
{defSessionVariables.i GLOBAL NEW}
{fncGlobalFunctions.i}
{fncDateFunctions.i}

/******************************************* Main Block *********************************************************/

DEFINE VARIABLE intOrdersAvailable       AS INTEGER   NO-UNDO.
DEFINE VARIABLE intTimeInterval          AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrTimeInterval          AS CHARACTER NO-UNDO.
DEFINE VARIABLE intBeginFrom             AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrCurrentInterval       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrHourInAdvanceInterval AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrHourBehindInterval    AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrHours                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrMinutes               AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrIntervalSeconds       AS CHARACTER NO-UNDO.
DEFINE VARIABLE chrSeconds               AS CHARACTER NO-UNDO.
DEFINE VARIABLE intCurrentUph            AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrStartDate             AS CHARACTER NO-UNDO.
DEFINE VARIABLE intMidnightTimeInterval  AS INTEGER   NO-UNDO.
DEFINE VARIABLE chrCurrentTimeStamp      AS CHARACTER NO-UNDO.

/* Get Cancelled Status to Exclude Cancelled Orders */
FIND FIRST ShipOrderStatus WHERE ShipOrderStatus.StatusCode = "Cancelled" NO-LOCK NO-ERROR.
IF NOT AVAILABLE ShipOrderStatus THEN
   RETURN.

/* Get the FPP PickPackType */
FIND FIRST PickPackType NO-LOCK WHERE PickPackType.TypeCode = "FastPickPack" NO-ERROR.
IF AVAILABLE PickPackType THEN
DO:
   /* Loop Through Each FPP Station */
   FOR EACH PickPackStation NO-LOCK
      WHERE PickPackStation.PickPackTypeID = PickPackType.PickPackTypeID 
      AND   PickPackStation.Active:
      
      /* Check CRON for previous Run */
      FIND FIRST ProcessProgram WHERE ProcessProgram.ProgramName = "crnFppPerformanceGenerator.p" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ProcessProgram THEN RETURN.
      
      FIND FIRST CronEvent OF ProcessProgram NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CronEvent THEN RETURN.
      
      FIND FIRST CronEventParameter OF CronEvent NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CronEventParameter THEN RETURN.
      
      /* Set Timings */
      intBeginFrom = INTEGER(CronEventParameter.ParameterValue) * 60. /* Starts population from this time  */
      
      /* set correct timeInterval(taking into consideration midnight) */
      IF TIME < intBeginFrom THEN
      DO:
         intTimeInterval = (24 * 60 * 60) + TIME - intBeginFrom. /* Generate time interval range  */
         chrStartDate = fDateToTimestamp(TODAY - 1).
      END.
      ELSE
      DO:
         intTimeInterval = TIME - intBeginFrom. /* Generate time interval range  */
         chrStartDate = fDateToTimestamp(TODAY).
      END.
      
      chrSeconds = "00".
      
      chrHours = STRING(TRUNCATE(intTimeInterval / 3600, 0), "99").
      chrMinutes  = STRING(TRUNCATE((intTimeInterval MOD 3600) / 60, 0), "99").
      chrIntervalSeconds = STRING((intTimeInterval MOD 3600) MOD 60, "99").
      
      chrTimeInterval = chrStartDate + chrHours + chrMinutes + chrIntervalSeconds + "000".
      
      chrCurrentTimeStamp = fTimeStamp(NOW).
      
      /* Loop from the Last Run to Now */
      DO WHILE chrTimeInterval < chrCurrentTimeStamp:         
        
         /* Build Timestamp */   
         chrCurrentInterval = chrStartDate + chrHours + chrMinutes + chrSeconds + "000".         
         
         /* Loop that Counts Available Orders per Time Interval */
         FOR EACH ShipOrderStreamPickPackLink NO-LOCK /* idx=PickPackStationID */
            WHERE ShipOrderStreamPickPackLink.PickPackStationID = PickPackStation.PickPackStationID
            AND   ShipOrderStreamPickPackLink.Active:
                 
            FOR EACH ShipOrder NO-LOCK 
               WHERE ShipOrder.ShipOrderStreamID = ShipOrderStreamPickPackLink.ShipOrderStreamID 
               AND  (ShipOrder.PickConfirmed BEGINS chrStartDate OR  
                     ShipOrder.PickConfirmed = ""):
               
               /* Skip if Cancelled */
               IF ShipOrder.ShipOrderStatusID = ShipOrderStatus.ShipOrderStatusID THEN
                  NEXT.
                       
               /* Count the Orders not Pick Confirmed */                          
               IF ShipOrder.PickConfirmed = "" THEN 
               DO:
                  IF(ShipOrder.Created <= chrCurrentInterval) THEN
                  DO:
                      intOrdersAvailable = intOrdersAvailable + 1.
                  END.
               END.
               ELSE 
               DO:
                  /* Count Historic Orders Pick Confirmed */
                  IF(ShipOrder.Created <= chrCurrentInterval AND ShipOrder.PickConfirmed > chrCurrentInterval) THEN
                  DO:
                      intOrdersAvailable = intOrdersAvailable + 1.
                  END.
               END. /* IF ShipOrder.PickConfirmed*/       
                                  
            END. /* FOR EACH ShipOrder */
         END. /* FOR EACH ShipOrderStreamPickPackLink NO-LOCK */
         
         /* Loop that Counts Current UPH per Time Interval */
         FOR EACH ShipOrderStreamPickPackLink NO-LOCK /* idx=PickPackStationID */
            WHERE ShipOrderStreamPickPackLink.PickPackStationID = PickPackStation.PickPackStationID
              AND ShipOrderStreamPickPackLink.Active:
           
            FOR EACH ShipOrder NO-LOCK 
               WHERE ShipOrder.ShipOrderStreamID = ShipOrderStreamPickPackLink.ShipOrderStreamID
                 AND ShipOrder.PickConfirmed BEGINS chrStartDate:   
               
               /* Skip if Cancelled */
               IF ShipOrder.ShipOrderStatusID = ShipOrderStatus.ShipOrderStatusID THEN
                  NEXT.
                       
               FOR EACH ShipPackage OF ShipOrder NO-LOCK 
                  WHERE ShipPackage.PackedOut BEGINS fDateToTimestamp(TODAY)
                    AND ShipPackage.PickPackStationID = PickPackStation.PickPackStationID:                  
                  
                  /* When clock goes 00, the substracted hour will produce minus sign */
                  IF intTimeInterval >= 3600 THEN
                  DO:
                     chrHourBehindInterval = chrStartDate + STRING(TRUNCATE((intTimeInterval - 3600) / 3600, 0), "99") + chrMinutes + chrSeconds + "000".
                  END.
                  ELSE
                  DO:
                     /* as our clock shows midnight, hence we need 23:00 */
                     intMidnightTimeInterval = 23 * 60 * 60.
                     /* adding minutes passed since midnight */
                     intMidnightTimeInterval = intMidnightTimeInterval + intTimeInterval.
                     /* we don't need to substract hour as above */
                     chrHourBehindInterval = chrStartDate + STRING(TRUNCATE(intMidnightTimeInterval / 3600, 0), "99") + chrMinutes + chrSeconds + "000".
                  END.

                  IF(ShipPackage.PackedOut > chrHourBehindInterval AND ShipPackage.PackedOut <= chrCurrentInterval) THEN
                  DO:
                     /* counting qty for each shippackageline */         
                     FOR EACH ShipPackageLine OF ShipPackage NO-LOCK:
                                          
                        intCurrentUph = intCurrentUph + ShipPackageLine.LineQty.
                        
                     END. /* FOR EACH ShipPackageLine OF ShipPackage NO-LOCK: */                        
                     
                  END. 
                  
               END. /* FOR EACH ShipPackage OF ShipOrder NO-LOCK */
               
            END. /* For EACH ShipOrder */
            
         END. /* FOR EACH ShipOrderStreamPickPackLink NO-LOCK */
         
         
         /* Swap Time Interval Seconds for Timestamp*/
         IF chrSeconds = "00" THEN
         DO:
            chrSeconds = "30".  
         END.
         ELSE
         DO:
            chrSeconds = "00".
         END.                      
         
         /* Check Time Interval Record for Current Interval and Create */
         FIND FIRST PickPackStationUnitsProcessed NO-LOCK
            WHERE PickPackStationUnitsProcessed.TimeInterval      = chrCurrentInterval
            AND   PickPackStationUnitsProcessed.PickPackStationID = PickPackStation.PickPackStationID NO-ERROR.
         IF NOT AVAILABLE PickPackStationUnitsProcessed THEN
         DO:
            CREATE PickPackStationUnitsProcessed.
            ASSIGN PickPackStationUnitsProcessed.PickPackStationUnitsProcessedID = NEXT-VALUE(PickPackStationUnitsProcessed)
                   PickPackStationUnitsProcessed.PickPackStationID               = PickPackStation.PickPackStationID
                   PickPackStationUnitsProcessed.TimeInterval                    = chrCurrentInterval
                   PickPackStationUnitsProcessed.TargetUph                       = PickPackStation.DefaultTargetUph
                   PickPackStationUnitsProcessed.CurrentUph                      = intCurrentUph
                   PickPackStationUnitsProcessed.OrdersToProcess                 = intOrdersAvailable NO-ERROR.
            
         END.      
         
         /* if timeInterval above 24 hours */
         IF intTimeInterval >= 86400 THEN
         DO:
            intTimeInterval = 0.
            chrStartDate = fDateToTimestamp(TODAY).
         END.
         /* Increase Time Interval by 30 Seconds */
         intTimeInterval = intTimeInterval + 30.

         /* Converting timeInterval into Hours and Minutes */         
         chrHours = STRING(TRUNCATE(intTimeInterval / 3600, 0), "99").
         chrMinutes  = STRING(TRUNCATE((intTimeInterval MOD 3600) / 60, 0), "99").
         chrIntervalSeconds = STRING((intTimeInterval MOD 3600) MOD 60, "99").
         
         chrTimeInterval = chrStartDate + chrHours + chrMinutes + chrIntervalSeconds + "000".
         
         /* Reset Counters for Available Orders and Current UPH */
         intCurrentUph = 0.
         intOrdersAvailable = 0.
        
      END. /* DO WHILE */
      
   END. /* FOR EACH PickPackStation */  
             
END. /* IF NOT AVAILABLE PickPackType */
