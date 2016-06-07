/*------------------------------------------------------------------------------------------------------------------------------------------
Program : pacBoxLogicCalculation.p
Purpose : Calculate the number of ShipPackages required at Packout to Pack an Order

Author  : DCummins
Date    : 09/03/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* Includes */
{defSessionVariables.i}
{fncGlobalFunctions.i}
{fncDateFunctions.i}
{fncStatusTypeFunctions.i}
{fncServerFunctions.i}
{fncLockingFunctions.i}
{fncBoxLogicFunctions.i}

/* Temp-Tables */
DEFINE TEMP-TABLE ttShipOrder
   FIELD ShipOrderID LIKE ShipOrder.ShipOrderID.
   
DEFINE TEMP-TABLE ttPackMatrix
   FIELD PrimaryItem   AS CHARACTER
   FIELD SecondaryItem AS CHARACTER
   FIELD PackTogether  AS LOGICAL. 
   
DEFINE TEMP-TABLE ttBoxLogicError NO-UNDO
   FIELD ShipOrderID LIKE ShipOrder.ShipOrderID
   FIELD ErrorDescription AS CHARACTER.         

/* Buffers */
DEFINE BUFFER updateShipOrder     FOR ShipOrder.
DEFINE BUFFER updateShipOrderLine FOR ShipOrderLine.
DEFINE BUFFER currentSegment      FOR ttPackagingSegment.
DEFINE BUFFER packedItem          FOR ttItemToPack.
DEFINE BUFFER packedPart          FOR Part.

/* Local Definitions */
DEFINE VARIABLE intAwaitingBoxLogicStatus     AS INTEGER NO-UNDO.
DEFINE VARIABLE intBoxLogicFailureStatus      AS INTEGER NO-UNDO.
DEFINE VARIABLE intCancelledStatus            AS INTEGER NO-UNDO.
DEFINE VARIABLE intMasterPackagingID          AS INTEGER NO-UNDO.
DEFINE VARIABLE intItemToPackID               AS INTEGER NO-UNDO.
DEFINE VARIABLE intItemLargestSurfaceID       AS INTEGER NO-UNDO.
DEFINE VARIABLE intMasterPackagingSegmentID   AS INTEGER NO-UNDO.
DEFINE VARIABLE intPackagingSegmentID         AS INTEGER NO-UNDO.
DEFINE VARIABLE intShipCalculationID          AS INTEGER NO-UNDO.
DEFINE VARIABLE intPackageCount               AS INTEGER NO-UNDO.
DEFINE VARIABLE intPackedPartQty              AS INTEGER NO-UNDO.
DEFINE VARIABLE decUsedVolume                 AS DECIMAL NO-UNDO.
DEFINE VARIABLE decTotalUsedVolume            AS DECIMAL NO-UNDO.
DEFINE VARIABLE decTotalFreeVolume            AS DECIMAL NO-UNDO.
DEFINE VARIABLE decMaximumPackageWeight       AS DECIMAL NO-UNDO.
DEFINE VARIABLE decPackageWeight              AS DECIMAL NO-UNDO.
DEFINE VARIABLE logPacked                     AS LOGICAL NO-UNDO.
DEFINE VARIABLE logPackageWillExceedMaxWeight AS LOGICAL NO-UNDO.
DEFINE VARIABLE intTimeout                    AS INTEGER NO-UNDO.
DEFINE VARIABLE intBoxLogicFailureGroup       AS INTEGER NO-UNDO.
DEFINE VARIABLE chrPackingError               AS CHARACTER NO-UNDO.

/* Have a Max Timeout to Stop Cron getting Stuck */
intTimeout = 10000.

/* Build Pack Matrix */
RUN pBuildPackMatrix.

/* Get Defaults */
intAwaitingBoxLogicStatus = fGetStatusID("ShipOrder", "AwaitingBoxLogic").
intBoxLogicFailureStatus  = fGetStatusID("ShipOrder", "BoxLogicFailure").
intCancelledStatus        = fGetStatusID("ShipOrder", "Cancelled").

/* Set the Global OperationType */
intGblOperationTypeID = fGetTypeID("Operation", "BoxLogicTmsCalculation").

/* Get the TMS config for the Min Max weight if NO Carrier is Specified */
FIND FIRST TmsConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE TmsConfig THEN
DO:
   chrPackingError = "No TMS Config records exists.".
   RETURN ERROR chrPackingError.       
END.

/* Set the TmsConfig value as Default */
decMaximumPackageWeight = TmsConfig.MaxPackageWeight.

/* Get Box Logic Failure Group */
FIND FIRST EmailGroup NO-LOCK
   WHERE EmailGroup.GroupCode = "BoxLogicFailures" NO-ERROR.
IF NOT AVAILABLE EmailGroup THEN
DO:
   chrPackingError = "No EmailGroup for BoxLogicFailures exists.".
   RETURN ERROR chrPackingError.          
END.

intBoxLogicFailureGroup = EmailGroup.EmailGroupID.

Main_Block:
DO ON ERROR UNDO, LEAVE:

   /* Strat ETIME for Timeout */
   ETIME(TRUE).

   /* Collect ALL the Ship Orders as Awating Box Logic */
   FOR EACH ShipOrder NO-LOCK
      WHERE (ShipOrder.ShipOrderStatusID = intAwaitingBoxLogicStatus OR
             ShipOrder.ShipOrderStatusID = intBoxLogicFailureStatus):
      
      CREATE ttShipOrder.
      ASSIGN ttShipOrder.ShipOrderID = ShipOrder.ShipOrderID.
      
   END.   

   /* Start Processing each Order through Box Logic */
   ShipOrder_Loop:   
   FOR EACH ttShipOrder NO-LOCK:
      
      ShipOrder_Update:
      DO TRANS ON ERROR UNDO, LEAVE:
      
         /* Check the CarrierService */
         FIND FIRST CarrierService NO-LOCK
            WHERE CarrierService.CarrierServiceID = ShipOrder.CarrierServiceID NO-ERROR.
         IF AVAILABLE CarrierService AND CarrierService.MaxPackageWeight <> 0 THEN
         DO:
            decMaximumPackageWeight = CarrierService.MaxPackageWeight.
         END.   
        
         IF decMaximumPackageWeight = 0 THEN
         DO:
            chrPackingError = "Maximum Package Weight cannot be 0. 
                               Please Set the Carrier Service Max Weight Value for Carrier Service:" + CarrierService.ServiceName 
                                 + " or TmsConfig Max Weight Value".
            RETURN ERROR chrPackingError.        
         END.
        
         /* Reset Counters */
         decTotalFreeVolume = 0.
         decTotalUsedVolume = 0.
         intPackageCount    = 0.
        
         /* Clear out the Temp-Tables */
         ClearAllTempData().      
        
         /* Build Temp-Table of Master Packages Available */
         CreateMasterPackaging().      
                    
         FIND FIRST ShipOrder NO-LOCK
            WHERE ShipOrder.ShipOrderID = ttShipOrder.ShipOrderID NO-ERROR.
            
         /* Get the Dimensions of Items to Pack and Store in a Temp-Table */   
         FOR EACH ShipOrderLine OF ShipOrder NO-LOCK
            WHERE ShipOrderLine.ShipOrderStatusID <> intCancelledStatus:
            chrPackingError = CreateItemsToPack(INPUT ShipOrderLine.PartID,
                                                INPUT ShipOrderLine.QtyOrdered).
                                                
            /* Report Errors from Stock Dimension Check ONLY if not Already Failed */                                    
            IF chrPackingError <> "OK" THEN
            DO:
               IF ShipOrder.ShipOrderStatusID <> intBoxLogicFailureStatus THEN
                  RUN pCreateError(INPUT ShipOrder.ShipOrderID,
                                   INPUT chrPackingError).            
            
               UNDO ShipOrder_Update, NEXT ShipOrder_Loop.
            END.                                    
         END.   
           
         MasterPackage_Loop:            
         REPEAT:                        
           
            /* Timeout for Debug */
            /*IF ETIME > intTimeout THEN
               UNDO ShipOrder_Update, NEXT ShipOrder_Loop.*/     
              
            /* Resets */   
            decPackageWeight = 0.
            logPackageWillExceedMaxWeight = FALSE.
  
            /* Get a Suitable Package */         
            intMasterPackagingID = SelectMasterPackagingByVolume().         
            intMasterPackagingSegmentID = CreatePackagingSegmentFromMaster(INPUT intMasterPackagingID).
                         
            /* Start Packing Largest to Smallest */         
            ItemPacking_Loop:         
            FOR EACH ttItemToPack NO-LOCK
               WHERE ttItemToPack.Packed = FALSE BY ttItemToPack.PartVolume DESCENDING:                                            
                                                        
               /* If Any Dimensions are 0 UNDO and LEAVE */                                         
               IF ttItemToPack.PartHeight = 0 OR ttItemToPack.PartWidth = 0 OR ttItemToPack.PartDepth = 0 THEN
               DO:
                  UNDO ShipOrder_Update, LEAVE ShipOrder_Update. 
               END.
                                                                                      
               /* Going to Need to Check what is in the Package here against the Packing Matrix Cust Table */
               FIND FIRST Part OF ttItemToPack NO-LOCK NO-ERROR.
               IF AVAILABLE Part THEN
               DO:            
                  /* Check is the Package Empty */
                  
                  FIND FIRST PartType OF Part NO-LOCK NO-ERROR.
                  IF AVAILABLE PartType THEN 
                  DO:
                     /* Check the Current Contents against the Packing Rules */
                     FOR EACH CustPartTypePackingRule OF PartType NO-LOCK:
                       
                        FIND FIRST currentSegment NO-LOCK
                           WHERE currentSegment.PackagingSegmentID = intMasterPackagingSegmentID NO-ERROR.
                        IF AVAILABLE currentSegment THEN
                        DO:   
                           /* Loop through Everything in the Package */
                           FOR EACH packedItem OF currentSegment:
                          
                              FIND FIRST packedPart OF packedItem NO-LOCK NO-ERROR.
                              IF AVAILABLE packedPart THEN
                              DO:
                                 IF packedPart.PartTypeID = CustPartTypePackingRule.CustPartTypePackingRuleID AND
                                    CustPartTypePackingRule.CanPackTogether = NO THEN
                                 DO:
                                    NEXT ItemPacking_Loop.
                                 END.
                              END.                           
                           END.                        
                        END.                            
                     END.       
                  END.    
                 
                  /* Check if Suitable for Jiffy */
                  FIND FIRST CustPart NO-LOCK
                     WHERE CustPart.CustPartID = Part.PartID NO-ERROR.
                  IF CustPart.CanShipInJiffyEnvelope = FALSE AND IsShipPackageEnvelope(intMasterPackagingID) THEN
                  DO:
                     NEXT ItemPacking_Loop.
                  END.               
               END.
                                 
               /* Check we are not Exceeding any Weight Restrictions */       
               IF decPackageWeight > (decMaximumPackageWeight + ttItemToPack.PartWeight) THEN
               DO:
                  logPackageWillExceedMaxWeight = TRUE.
                  LEAVE ItemPacking_Loop.   
               END.
                                      
               REPEAT:
              
                  /* Timeout for Debug */
                  /*IF ETIME > intTimeout THEN
                     UNDO ShipOrder_Update, NEXT ShipOrder_Loop.*/
              
                  /* Nothing to Pack Into */
                  IF CheckIfAnySegmentsRemain() = FALSE THEN 
                     LEAVE ItemPacking_Loop.
                      
                  intPackagingSegmentID = SelectPackagingSegmentByVolume(ttItemToPack.ItemToPackID).
                 
                  /* Nothing Suitable Left */
                  IF intPackagingSegmentID = ? THEN 
                     NEXT ItemPacking_Loop.                                            
                                                                                       
                  intItemLargestSurfaceID = GetLargestSurfaceOfItemToPack(INPUT ttItemToPack.ItemToPackID).
           
                  logPacked = PackItem(INPUT intPackagingSegmentID,
                                       INPUT ttItemToPack.ItemToPackID,
                                       INPUT intItemLargestSurfaceID).
                                 
                  /* Item Packed */                                                   
                  IF logPacked THEN DO:
                     decPackageWeight = decPackageWeight + ttItemToPack.PartWeight.
                     NEXT ItemPacking_Loop.
                  END.   
                                   
               END.   
                                               
            END.    
           
            /* Check if Everything got Packed */
            IF CAN-FIND(FIRST ttItemToPack WHERE ttItemToPack.Packed = FALSE) THEN
            DO:
               /* Check if we are on the Largest Package */
               IF CheckIfLargestMasterPackage(intMasterPackagingID) THEN
               DO:
                  /* Reset All Packages to Suitable as Largest box is full */               
                  ResetMasterPackaging().                                            
                  DeleteAllUnUsedPackagingSegments().
                 
                  NEXT MasterPackage_Loop.
               END.
               ELSE
               DO:   
                  /* Want to reset the Current Packaging if NOT Over the Max Weight */
                  IF logPackageWillExceedMaxWeight = FALSE THEN
                  DO:
                     DeletePackagingSegmentAndResetItems(intMasterPackagingSegmentID).
                     
                     /* Need to Set Master to Not Suitable */
                     FIND FIRST ttMasterPackaging EXCLUSIVE-LOCK
                        WHERE ttMasterPackaging.MasterPackagingID = intMasterPackagingID NO-ERROR.
                     IF AVAILABLE ttMasterPackaging THEN
                     DO:
                        ttMasterPackaging.Suitable = FALSE.
                     END.   
                     
                  END.   
                    
                  NEXT MasterPackage_Loop.
               END.   
            END.  
            ELSE
            DO:
               LEAVE MasterPackage_Loop.
            END.                   
                                
         END.                              
              
         /* Dont Update if Anything is not Packed */
         IF CAN-FIND(FIRST ttItemToPack WHERE ttItemToPack.Packed = FALSE) THEN
         DO:
            FIND FIRST Part of ttItemToPack NO-LOCK NO-ERROR.
         
            RUN pCreateError(INPUT ShipOrder.ShipOrderID,
                             INPUT "Could NOT pack Part Ref: " + Part.PartRef + " for Ship Order Ref: " + ShipOrder.OrderRef).
         
            UNDO ShipOrder_Update, NEXT ShipOrder_Loop.                  
         END.      
                                
         /* Record the Box Calulation */
         CREATE BoxLogicCalculation.
         ASSIGN BoxLogicCalculation.BoxLogicCalculationID = NEXT-VALUE(BoxLogicCalculation)
                BoxLogicCalculation.ShipOrderID = ttShipOrder.ShipOrderID
                BoxLogicCalculation.Created = fTimeStamp(NOW) NO-ERROR.
               
         IF ERROR-STATUS:ERROR THEN DO:     
            RUN pCreateError(INPUT ShipOrder.ShipOrderID,
                             INPUT "BoxCalculation Records could NOT be created for Ship Order Ref: " + ShipOrder.OrderRef).
         
            UNDO ShipOrder_Update, NEXT ShipOrder_Loop.
         END.                
              
         /* Create Box Calculation Data */   
         FOR EACH ttItemToPack NO-LOCK BREAK BY ttItemToPack.OriginalPackagingSegmentID BY ttItemToPack.PartID:
       
            /* Accumulate Volume */
            decUsedVolume = decUsedVolume + ttItemToPack.PartVolume.
            decTotalUsedVolume = decTotalUsedVolume + ttItemToPack.PartVolume.         
       
            IF FIRST-OF(ttItemToPack.OriginalPackagingSegmentID) THEN
            DO:
               /* Get the Master Segment for the Package Type and Size */
               FIND FIRST ttPackagingSegment NO-LOCK
                  WHERE ttPackagingSegment.PackagingSegmentID = ttItemToPack.OriginalPackagingSegmentID NO-ERROR.
                 
               FIND FIRST ttMasterPackaging OF ttPackagingSegment NO-LOCK NO-ERROR.  
                          
               CREATE ShipPackageCalculation.
               ASSIGN ShipPackageCalculation.ShipPackageCalculationID = NEXT-VALUE(ShipPackageCalculation)
                      ShipPackageCalculation.BoxLogicCalculationID    = BoxLogicCalculation.BoxLogicCalculationID 
                      ShipPackageCalculation.CalculationRef           = NewTmsPackageRef(ShipPackageCalculation.ShipPackageCalculationID)
                      ShipPackageCalculation.Created                  = fTimeStamp(NOW)
                      ShipPackageCalculation.ShipPackageTypeID        = ttMasterPackaging.ShipPackageTypeID
                      ShipPackageCalculation.ShipPackageSizeID        = ttMasterPackaging.ShipPackageSizeID
                      ShipPackageCalculation.ShipOrderID              = ttShipOrder.ShipOrderID
                      ShipPackageCalculation.FreeVolume               = ((ttMasterPackaging.MasterHeight * ttMasterPackaging.MasterDepth 
                                                                        * ttMasterPackaging.MasterWidth) - decUsedVolume)
                      ShipPackageCalculation.UsedVolume               = decUsedVolume NO-ERROR.
                     
               IF ERROR-STATUS:ERROR THEN DO:     
                  RUN pCreateError(INPUT ShipOrder.ShipOrderID,
                                   INPUT "ShipPackageCalculation Records could NOT be created for Ship Order Ref: " + ShipOrder.OrderRef).
               
                  UNDO ShipOrder_Update, NEXT ShipOrder_Loop.
               END.   
                     
               decTotalFreeVolume = decTotalFreeVolume + ShipPackageCalculation.FreeVolume.       
               intPackageCount = intPackageCount + 1.
               intShipCalculationID = ShipPackageCalculation.ShipPackageCalculationID.
               decUsedVolume = 0.                                           
                     
            END.
           
            intPackedPartQty = intPackedPartQty + 1.
           
            IF LAST-OF(ttItemToPack.PartID) THEN
            DO:
               CREATE ShipPackageCalcLine.
               ASSIGN ShipPackageCalcLine.ShipPackageCalcLineID = NEXT-VALUE(ShipPackageCalcLine)
                      ShipPackageCalcLine.ShipPackageCalculationID = intShipCalculationID
                      ShipPackageCalcLine.PartID = ttItemToPack.PartID
                      ShipPackageCalcLine.QtyAssigned = intPackedPartQty NO-ERROR.   
                     
               IF ERROR-STATUS:ERROR THEN DO:     
                  RUN pCreateError(INPUT ShipOrder.ShipOrderID,
                                   INPUT "ShipPackageCalcLine Records could NOT be created for Ship Order Ref: " + ShipOrder.OrderRef).
               
                  UNDO ShipOrder_Update, NEXT ShipOrder_Loop.
               END.                          
                     
               intPackedPartQty = 0.
            
            END.
           
         END.          
         
         /* Dont Update Nothing got Created */
         IF NOT CAN-FIND(FIRST ShipPackageCalculation WHERE ShipPackageCalculation.ShipOrderID = ttShipOrder.ShipOrderID) THEN
         DO:            
            RUN pCreateError(INPUT ShipOrder.ShipOrderID,
                             INPUT "No ShipPackageCalculation Records exist for Ship Order Ref: " + ShipOrder.OrderRef).
                     
            UNDO ShipOrder_Update, NEXT ShipOrder_Loop.                  
         END.                                        
               
         /* Update Volume on BoxCalculation Record after Accumulations */
         ASSIGN BoxLogicCalculation.TotalFreeVolume = decTotalFreeVolume
                BoxLogicCalculation.TotalUsedVolume = decTotalUsedVolume
                BoxLogicCalculation.NumPackages     = intPackageCount.      
        
         /* Update the ShipOrder to ReadyForTmsExchange */
         FIND FIRST updateShipOrder EXCLUSIVE-LOCK 
            WHERE updateShipOrder.ShipOrderID = ttShipOrder.ShipOrderID NO-ERROR NO-WAIT.
         IF LOCKED updateShipOrder THEN
         DO:      
            UNDO ShipOrder_Update, NEXT ShipOrder_Loop.
         END.   
  
         updateShipOrder.ShipOrderStatusID = fGetStatusID("ShipOrder", "ReadyForTmsExchange").
         updateShipOrder.NumPackages = intPackageCount.
        
         /* Update the ShipOrderLines */
         FOR EACH ShipOrderLine OF updateShipOrder NO-LOCK:
        
            /* Skip Cancelled */
            IF ShipOrderLine.ShipOrderStatusID = fGetStatusID("ShipOrder", "Cancelled") THEN NEXT.
            
            FIND FIRST updateShipOrderLine EXCLUSIVE-LOCK 
               WHERE updateShipOrderLine.ShipOrderLineID = ShipOrderLine.ShipOrderLineID NO-ERROR NO-WAIT.
            IF LOCKED updateShipOrderLine THEN
            DO:      
               UNDO ShipOrder_Update, NEXT ShipOrder_Loop.
            END.
           
            updateShipOrderLine.ShipOrderStatusID = fGetStatusID("ShipOrder", "ReadyForTmsExchange").                  
        
         END.                         
              
      END. /* ShipOrder_Update Trans Block */  
            
   END. /* EACH ttShipOrder */   
      
   /* Report Failures */
   Failure_Update:
   FOR EACH ttBoxLogicError:
      
      /* Update the ShipOrder to BoxLogicFailure */
      FIND FIRST updateShipOrder EXCLUSIVE-LOCK 
         WHERE updateShipOrder.ShipOrderID = ttBoxLogicError.ShipOrderID NO-ERROR NO-WAIT.
      IF LOCKED updateShipOrder THEN
      DO:      
         UNDO Failure_Update, NEXT Failure_Update.
      END.  
          
      updateShipOrder.ShipOrderStatusID = fGetStatusID("ShipOrder", "BoxLogicFailure").
       
      /* Update the ShipOrderLines */
      FOR EACH ShipOrderLine OF updateShipOrder NO-LOCK:
       
         /* Skip Cancelled */
         IF ShipOrderLine.ShipOrderStatusID = fGetStatusID("ShipOrder", "Cancelled") THEN NEXT.
       
         FIND FIRST updateShipOrderLine EXCLUSIVE-LOCK 
            WHERE updateShipOrderLine.ShipOrderLineID = ShipOrderLine.ShipOrderLineID NO-ERROR NO-WAIT.
         IF LOCKED updateShipOrderLine THEN
         DO:      
            UNDO Failure_Update, NEXT Failure_Update.
         END.
      
         updateShipOrderLine.ShipOrderStatusID = fGetStatusID("ShipOrder", "BoxLogicFailure").                  
       
      END.                    
      
      /* Send Email */
      RUN pSendErrorEmail(INPUT "Box Logic Failure",
                          INPUT ttBoxLogicError.ErrorDescription,
                          INPUT "",
                          INPUT intBoxLogicFailureGroup).
      
   END.  /* ttBoxLogicError */   
      
END.  /* Main_Block */

/* Procedures */

PROCEDURE pCreateError:

   DEFINE INPUT PARAMETER intShipOrderID AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER chrErrorString AS CHARACTER NO-UNDO.

   CREATE ttBoxLogicError.
   ASSIGN ttBoxLogicError.ShipOrderID      = intShipOrderID
          ttBoxLogicError.ErrorDescription = chrErrorString.

END PROCEDURE.

PROCEDURE pSendErrorEmail:

   DEFINE INPUT PARAMETER chrSubjectString AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER chrErrorString   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER chrAttachment    AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER intEmailGroup    AS INTEGER   NO-UNDO.
                  
   RUN osSendMail.p(INPUT "",               /* Optional list of Users */
                    INPUT chrSubjectString, /* Email Subject */
                    INPUT chrErrorString,   /* Plain text message Body */
                    INPUT "",               /* Html format message Body */
                    INPUT chrAttachment,    /* File path ../files/file */
                    INPUT intEmailGroup,    /* EmailGroupID that you want to send this to */
                    INPUT 0).               /* File MasterID is it applies */   

END PROCEDURE. /*End of pSendErrorMail */

PROCEDURE pBuildPackMatrix:

   /* Printers */
   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Printer"
          ttPackMatrix.SecondaryItem = "Printer"
          ttPackMatrix.PackTogether = NO.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Printer"
          ttPackMatrix.SecondaryItem = "Paper"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Printer"
          ttPackMatrix.SecondaryItem = "Ink"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Printer"
          ttPackMatrix.SecondaryItem = "SelphyPrinter"
          ttPackMatrix.PackTogether = NO.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Printer"
          ttPackMatrix.SecondaryItem = "Optical"
          ttPackMatrix.PackTogether = NO.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Printer"
          ttPackMatrix.SecondaryItem = "Accessories"
          ttPackMatrix.PackTogether = NO.    

   /* Paper */
   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Paper"
          ttPackMatrix.SecondaryItem = "Printer"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Paper"
          ttPackMatrix.SecondaryItem = "Paper"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Paper"
          ttPackMatrix.SecondaryItem = "Ink"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Paper"
          ttPackMatrix.SecondaryItem = "SelphyPrinter"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Paper"
          ttPackMatrix.SecondaryItem = "Optical"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Paper"
          ttPackMatrix.SecondaryItem = "Accessories"
          ttPackMatrix.PackTogether = YES.   
          
   /* Ink */
   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Ink"
          ttPackMatrix.SecondaryItem = "Printer"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Ink"
          ttPackMatrix.SecondaryItem = "Paper"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Ink"
          ttPackMatrix.SecondaryItem = "Ink"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Ink"
          ttPackMatrix.SecondaryItem = "SelphyPrinter"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Ink"
          ttPackMatrix.SecondaryItem = "Optical"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Ink"
          ttPackMatrix.SecondaryItem = "Accessories"
          ttPackMatrix.PackTogether = YES.   

   /* Selphy */
   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "SelphyPrinter"
          ttPackMatrix.SecondaryItem = "Printer"
          ttPackMatrix.PackTogether = NO.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "SelphyPrinter"
          ttPackMatrix.SecondaryItem = "Paper"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "SelphyPrinter"
          ttPackMatrix.SecondaryItem = "Ink"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "SelphyPrinter"
          ttPackMatrix.SecondaryItem = "SelphyPrinter"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "SelphyPrinter"
          ttPackMatrix.SecondaryItem = "Optical"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "SelphyPrinter"
          ttPackMatrix.SecondaryItem = "Accessories"
          ttPackMatrix.PackTogether = YES.   
                    
   /* Optical */
   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Optical"
          ttPackMatrix.SecondaryItem = "Printer"
          ttPackMatrix.PackTogether = NO.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Optical"
          ttPackMatrix.SecondaryItem = "Paper"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Optical"
          ttPackMatrix.SecondaryItem = "Ink"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Optical"
          ttPackMatrix.SecondaryItem = "SelphyPrinter"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Optical"
          ttPackMatrix.SecondaryItem = "Optical"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Optical"
          ttPackMatrix.SecondaryItem = "Accessories"
          ttPackMatrix.PackTogether = YES.   
          
   /* Accessories */
   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Accessories"
          ttPackMatrix.SecondaryItem = "Printer"
          ttPackMatrix.PackTogether = NO.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Accessories"
          ttPackMatrix.SecondaryItem = "Paper"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Accessories"
          ttPackMatrix.SecondaryItem = "Ink"
          ttPackMatrix.PackTogether = YES.    

   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Accessories"
          ttPackMatrix.SecondaryItem = "SelphyPrinter"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Accessories"
          ttPackMatrix.SecondaryItem = "Optical"
          ttPackMatrix.PackTogether = YES.    


   CREATE ttPackMatrix.
   ASSIGN ttPackMatrix.PrimaryItem = "Accessories"
          ttPackMatrix.SecondaryItem = "Accessories"
          ttPackMatrix.PackTogether = YES.   
          


END PROCEDURE.






