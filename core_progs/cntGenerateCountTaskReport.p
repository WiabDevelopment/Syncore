/*------------------------------------------------------------------------------------------------------------------------------------------
Program : cntGenerateCountTaskReport.p
Purpose : Generates the Count Task Report

          Possible Results : Continue

Author  : Christopher Shelley
Date    : 29/04/2014
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Required to prevent CTRL-C or CRTL-BREAK */
DO ON STOP UNDO, RETRY:

   /* Character Parameter Include */
   {defProcessParameters.i}
   
   /* Standard Mandatory Includes */
   {defSessionVariables.i}
   {fncClassFunctions.i} 
   {fncGlobalFunctions.i}
   
   /* Optional Includes */  
   {fncStatusTypeFunctions.i} 
   
   /* Scan Gun Message/Confirm */
   {prcScannerMessaging.i}
   
   /* Map Debugging */
   {prcProcessDebugging.i}
   
   /* Optional Includes */
   {fncDateFunctions.i}
   /* fTimestamp */
   {fncServerFunctions.i}
   /* fValidWriteDirectory */
   {fncDataFunctions.i}
   /* fGetFieldValue */
   
   /* Local Variables */
   DEFINE VARIABLE chrFilePath      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrFileName      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrValidWriteDir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrCurrentCode   AS CHARACTER NO-UNDO.
   
   /* Session Objects */   
   DEFINE VARIABLE intSsnTaskLocationID      AS sessionValue NO-UNDO.   
   DEFINE VARIABLE intSsnFileMasterID        AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnFilePath            AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnEmailAddressList    AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnEmailSubjectLine    AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnPlainTextEmailBody  AS sessionValue NO-UNDO.
   DEFINE VARIABLE chrSsnHtmlEmailBody       AS sessionValue NO-UNDO. 
   
   /* Streams */
   DEFINE STREAM strToFile.
   
   fClearSessionValue("FileMasterID").
   fClearSessionValue("FilePath").
   fClearSessionValue("EmailAddressList").
   fClearSessionValue("EmailSubjectLine").
   fClearSessionValue("PlainTextEmailBody").
   fClearSessionValue("HtmlEmailBody").
   
   /* Set New Data */
   intSsnFileMasterID       = fNewSessionValue("FileMasterID").      
   chrSsnFilePath           = fNewSessionValue("FilePath").          
   chrSsnEmailAddressList   = fNewSessionValue("EmailAddressList").  
   chrSsnEmailSubjectLine   = fNewSessionValue("EmailSubjectLine").  
   chrSsnPlainTextEmailBody = fNewSessionValue("PlainTextEmailBody").
   chrSsnHtmlEmailBody      = fNewSessionValue("HtmlEmailBody"). 
   
   /* Get Current Session Data */
   intSsnTaskLocationID = fGetSessionValue("TaskLocationID").  
   
   /* Temp-Tables */
   DEFINE TEMP-TABLE ttCountTaskPartLocation
      FIELD LocationRef AS CHARACTER 
      FIELD PartRef     AS CHARACTER
      FIELD ExpectedQty AS INTEGER 
      FIELD CountedQty  AS INTEGER
      FIELD StatusCode  AS CHARACTER
      FIELD UserName    AS CHARACTER
      INDEX LocationRefPartRefStatusCode LocationRef PartRef StatusCode.
      
   DEFINE TEMP-TABLE ttCountTaskPackageLocation
      FIELD LocationRef AS CHARACTER 
      FIELD PackageRef  AS CHARACTER
      FIELD PartRef     AS CHARACTER
      FIELD ExpectedQty AS INTEGER 
      FIELD CountedQty  AS INTEGER
      FIELD StatusCode  AS CHARACTER
      FIELD UserName    AS CHARACTER
      INDEX LocationRefPackageRefStatusCode LocationRef PackageRef StatusCode.
   
    /* Get Current Session Data */
   Main_Block:
   DO ON ERROR UNDO Main_Block, LEAVE Main_Block:
      
      /* Validate Session Data */
      FIND FIRST CountTaskLocation NO-LOCK
         WHERE CountTaskLocation.CountTaskLocationID = intSsnTaskLocationID:intValue NO-ERROR.
      IF NOT AVAILABLE CountTaskLocation THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[Task Location ID] [" + STRING(intSsnTaskLocationID:intValue) + "] does not Exist.").
         LEAVE Main_Block.
      END.
            
      FIND FIRST CountTask OF CountTaskLocation NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountTask THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTask ID] [" + STRING(CountTaskLocation.CountTaskID) + "] does not Exist.").
         LEAVE Main_Block.
      END. 
      
      FIND FIRST CountTaskType OF CountTask NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CountTaskType THEN
      DO:
         RUN DisplayError("Record Not Found",
                          "[CountTaskType ID] [" + STRING(CountTask.CountTaskTypeID) + "] does not Exist.").
         LEAVE Main_Block.
      END. 
      
      /* Creating List of Locations visited */
      CountTaskLocationLoop:
      FOR EACH CountTaskLocation OF CountTask NO-LOCK:
         
         FIND FIRST Location OF CountTaskLocation NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Location THEN 
            NEXT CountTaskLocationLoop.
         
         IF  CountTaskType.TypeCode = "PartScan" THEN 
         DO:
            CountTaskLocationPartLoop:
            FOR EACH CountTaskLocationPart OF CountTaskLocation NO-LOCK:
               
               FIND FIRST Part OF CountTaskLocationPart NO-LOCK NO-ERROR.
               IF NOT AVAILABLE Part THEN 
                  NEXT CountTaskLocationPartLoop.
               
               chrCurrentCode = fGetStatusCode("CountTask", CountTaskLocationPart.CountTaskStatusID).
               
               FIND FIRST ttCountTaskPartLocation /* idx = LocationRefPartRefStatusCode1 */
                  WHERE ttCountTaskPartLocation.LocationRef = Location.LocationRef
                  AND   ttCountTaskPartLocation.PartRef     = Part.PartRef 
                  AND   ttCountTaskPartLocation.StatusCode  = chrCurrentCode NO-ERROR.
               IF NOT AVAILABLE ttCountTaskPartLocation THEN 
               DO:
                  CREATE ttCountTaskPartLocation.
                  ASSIGN ttCountTaskPartLocation.LocationRef = Location.LocationRef
                         ttCountTaskPartLocation.PartRef     = Part.PartRef
                         ttCountTaskPartLocation.StatusCode  = chrCurrentCode.
                  
                  FIND FIRST GateUser NO-LOCK
                     WHERE GateUser.GateUserID = CountTaskLocation.AssignedTo NO-ERROR.
                  IF AVAILABLE GateUser THEN
                     ttCountTaskPartLocation.UserName = GateUser.FullName.
               END.
               ASSIGN ttCountTaskPartLocation.ExpectedQty = ttCountTaskPartLocation.ExpectedQty + CountTaskLocationPart.QtyExpected
                      ttCountTaskPartLocation.CountedQty  = ttCountTaskPartLocation.CountedQty  + CountTaskLocationPart.QtyCounted.              
               
            END.  /* EACH CountTaskLocationPart */
            
            /* Reporting Empty/Skipped Location */
            IF NOT CAN-FIND(FIRST CountTaskLocationPart OF CountTaskLocation) THEN 
            DO:
               IF CountTaskLocation.CountTaskStatusID = fGetStatusID("CountTask", "Skipped") THEN 
               DO:    
                  CREATE ttCountTaskPartLocation.
                  ASSIGN ttCountTaskPartLocation.LocationRef = Location.LocationRef
                         ttCountTaskPartLocation.PartRef     = "Location Skipped"
                         ttCountTaskPartLocation.StatusCode  = fGetStatusCode("CountTask", CountTaskLocation.CountTaskStatusID).
               END.
               ELSE 
               DO:
                  CREATE ttCountTaskPartLocation.
                  ASSIGN ttCountTaskPartLocation.LocationRef = Location.LocationRef
                         ttCountTaskPartLocation.PartRef     = "Location Empty"
                         ttCountTaskPartLocation.StatusCode  = fGetStatusCode("CountTask", CountTaskLocation.CountTaskStatusID).
               END.
               FIND FIRST GateUser NO-LOCK
                  WHERE GateUser.GateUserID = CountTaskLocation.AssignedTo NO-ERROR.
               IF AVAILABLE GateUser THEN
                  ttCountTaskPartLocation.UserName = GateUser.FullName.                       
               
            END. /*IF NOT CAN-FIND(FIRST CountTaskLocationPart OF CountTaskLocation) THEN */
         END.
         ELSE  /*CountTaskType.TypeCode = "PackageScan" */ 
         DO:
            CountTaskLocationPackageLoop:
            FOR EACH CountTaskLocationPackage OF CountTaskLocation NO-LOCK:
               
               FIND FIRST StockPackage OF CountTaskLocationPackage NO-LOCK NO-ERROR.
               IF NOT AVAILABLE StockPackage THEN 
                  NEXT CountTaskLocationPackageLoop.
               
               FIND FIRST Part OF StockPackage NO-LOCK NO-ERROR.
               IF NOT AVAILABLE Part THEN 
                  NEXT CountTaskLocationPackageLoop.
               
               chrCurrentCode = fGetStatusCode("CountTask", CountTaskLocationPackage.CountTaskStatusID).
               
               FIND FIRST ttCountTaskPackageLocation /* idx = LocationRefPackageRefStatusCode1 */
                  WHERE ttCountTaskPackageLocation.LocationRef = Location.LocationRef
                  AND   ttCountTaskPackageLocation.PackageRef  = StockPackage.PackageRef
                  AND   ttCountTaskPackageLocation.StatusCode  = chrCurrentCode NO-ERROR.
               IF NOT AVAILABLE ttCountTaskPackageLocation THEN 
               DO:
                  CREATE ttCountTaskPackageLocation.
                  ASSIGN ttCountTaskPackageLocation.LocationRef = Location.LocationRef
                         ttCountTaskPackageLocation.PackageRef  = StockPackage.PackageRef
                         ttCountTaskPackageLocation.PartRef     = Part.PartRef
                         ttCountTaskPackageLocation.StatusCode  = chrCurrentCode.
                  
                  FIND FIRST GateUser NO-LOCK
                     WHERE GateUser.GateUserID = CountTaskLocation.AssignedTo NO-ERROR.
                  IF AVAILABLE GateUser THEN
                     ttCountTaskPackageLocation.UserName = GateUser.FullName.       
               END.
               ASSIGN ttCountTaskPackageLocation.ExpectedQty = ttCountTaskPackageLocation.ExpectedQty + CountTaskLocationPackage.QtyExpected
                      ttCountTaskPackageLocation.CountedQty  = ttCountTaskPackageLocation.CountedQty  + CountTaskLocationPackage.QtyCounted.              
               
            END.  /* EACH CountTaskLocationPackage */
            
            /* Reporting Empty/Skipped Location */
            IF NOT CAN-FIND(FIRST CountTaskLocationPackage OF CountTaskLocation) THEN 
            DO:
               IF CountTaskLocation.CountTaskStatusID = fGetStatusID("CountTask", "Skipped") THEN 
               DO:    
                  CREATE ttCountTaskPackageLocation.
                  ASSIGN ttCountTaskPackageLocation.LocationRef = Location.LocationRef
                         ttCountTaskPackageLocation.PackageRef  = "Location Skipped"
                         ttCountTaskPackageLocation.StatusCode  = fGetStatusCode("CountTask", CountTaskLocation.CountTaskStatusID).
               END.
               ELSE 
               DO:
                  CREATE ttCountTaskPackageLocation.
                  ASSIGN ttCountTaskPackageLocation.LocationRef = Location.LocationRef
                         ttCountTaskPackageLocation.PackageRef  = "Location Empty"
                         ttCountTaskPackageLocation.StatusCode  = fGetStatusCode("CountTask", CountTaskLocation.CountTaskStatusID).
               END.
               
               FIND FIRST GateUser NO-LOCK
                  WHERE GateUser.GateUserID = CountTaskLocation.AssignedTo NO-ERROR.
               IF AVAILABLE GateUser THEN
                  ttCountTaskPackageLocation.UserName = GateUser.FullName.
               
            END. /*IF NOT CAN-FIND(FIRST CountTaskLocationPackage OF CountTaskLocation) THEN */
         END. /*CountTaskType.TypeCode = "PckageScan" */ 
      END. /* FOR EACH CountTaskLocation */
      
      FIND FIRST FileMaster NO-LOCK 
         WHERE FileMaster.MasterName = "CycleCountTaskReport" NO-ERROR.
      IF NOT AVAILABLE FileMaster THEN 
      DO:
         RUN DisplayMessage("Record not found", 
                            "File Master [CountTaskReport] does not exist. CountTask Report Email cannot be sent.").
         LEAVE Main_Block.
      END.
      
      FIND FileType OF FileMaster NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FileType THEN 
      DO:
         RUN DisplayMessage("Record not found",
                            "File Type of [CountTaskReport File Master does not exist. " 
                              + "CountTask Report Email cannot be sent.").
         LEAVE Main_Block.
      END.
      
      chrFileName = FileMaster.FilePrefix + fTimestamp(NOW) + "." + FileType.Extension.
      chrFilePath = RIGHT-TRIM(FileMaster.FilePath,"/") + "/tmp/".
      
      chrValidWriteDir = fValidWriteDirectory(chrFilePath).
      IF chrValidWriteDir <> "Ok" THEN 
      DO:
         RUN DisplayMessage("Directory missing",
                            chrValidWriteDir).
         LEAVE Main_Block.
      END.
      
      chrFilePath = chrFilePath + chrFileName.
      
      OUTPUT STREAM strToFile TO VALUE(chrFilePath).
         
         PUT STREAM strToFile UNFORMATTED "Count Task Summary for Task ID " + STRING(CountTask.CountTaskID) SKIP.
         
         IF  CountTaskType.TypeCode = "PartScan" THEN 
         DO:
            PUT STREAM strToFile UNFORMATTED "Location Ref,Status,User,Part Ref,Expected Qty,Counted Qty" SKIP.
            
            FOR EACH ttCountTaskPartLocation:
               
               PUT STREAM strToFile UNFORMATTED ttCountTaskPartLocation.LocationRef 
                                                  + "," + ttCountTaskPartLocation.StatusCode    
                                                  + "," + ttCountTaskPartLocation.UserName
                                                  + "," + ttCountTaskPartLocation.PartRef 
                                                  + "," + STRING(ttCountTaskPartLocation.ExpectedQty,">>>>9") 
                                                  + "," + STRING(ttCountTaskPartLocation.CountedQty,">>>>9") SKIP.
               
            END.
         END.
         ELSE 
         DO:
            PUT STREAM strToFile UNFORMATTED "Location Ref,Status,User,Package Ref, Part Ref,Expected Qty,Counted Qty" SKIP.
            
            FOR EACH ttCountTaskPackageLocation:
               
               PUT STREAM strToFile UNFORMATTED ttCountTaskPackageLocation.LocationRef 
                                                  + "," + ttCountTaskPackageLocation.StatusCode    
                                                  + "," + ttCountTaskPackageLocation.UserName
                                                  + "," + ttCountTaskPackageLocation.PackageRef 
                                                  + "," + ttCountTaskPackageLocation.PartRef 
                                                  + "," + STRING(ttCountTaskPackageLocation.ExpectedQty,">>>>9") 
                                                  + "," + STRING(ttCountTaskPackageLocation.CountedQty,">>>>9") SKIP.
               
            END.
         END.         
         
         PUT STREAM strToFile UNFORMATTED " ".
         
      OUTPUT STREAM strToFile CLOSE.
      
      /* Set Session Variables for psSendMail.p */
      intSsnFileMasterID:setValue(FileMaster.FileMasterID).
      chrSsnFilePath:setValue(chrFilePath).
      chrSsnEmailAddressList:setValue("").
      chrSsnEmailSubjectLine:setValue("CountTask ID " + STRING(CountTask.CountTaskID) + " Confirmation").
      chrSsnPlainTextEmailBody:setValue("Count Task ID " + STRING(CountTask.CountTaskID) + " has been completed").
      chrSsnHtmlEmailBody:setValue("").
      /* Export Summary: finish */
      
      chrResult = "CONTINUE".

   END.   

   /* Clean Up */
   DELETE OBJECT intSsnTaskLocationID     NO-ERROR.
   DELETE OBJECT intSsnFileMasterID       NO-ERROR.
   DELETE OBJECT chrSsnFilePath           NO-ERROR.
   DELETE OBJECT chrSsnEmailAddressList   NO-ERROR.
   DELETE OBJECT chrSsnEmailSubjectLine   NO-ERROR.
   DELETE OBJECT chrSsnPlainTextEmailBody NO-ERROR.
   DELETE OBJECT chrSsnHtmlEmailBody      NO-ERROR.
   
   /* Releases */
   RELEASE CountTaskLocation     NO-ERROR.
   RELEASE CountTask             NO-ERROR.
   RELEASE CountTaskLocationPart NO-ERROR.
   RELEASE CountTaskType         NO-ERROR.
   RELEASE StockPackage          NO-ERROR.
   
   /* Map Result Debugging */
   {prcProcessDebugging.i}

END.
