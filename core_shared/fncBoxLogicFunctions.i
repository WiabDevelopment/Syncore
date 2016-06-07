/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncBoxLogicFunctions.i
Purpose : Library of functions to Calculate and Measure the Packing Box Logic process

Author  : DCummins
Date    : 09/03/20
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------------------*/

/* Temp-Tables */
DEFINE TEMP-TABLE ttMasterPackaging
   FIELD MasterPackagingID AS INTEGER   
   FIELD ShipPackageTypeID AS INTEGER      
   FIELD ShipPackageSizeID AS INTEGER   
   FIELD MasterHeight      AS DECIMAL
   FIELD MasterWidth       AS DECIMAL
   FIELD MasterDepth       AS DECIMAL
   FIELD MasterVolume      AS DECIMAL FORMAT ">>>>>>>>>>>9.999"
   FIELD Suitable          AS LOGICAL   
   INDEX MasterPackagingID IS UNIQUE MasterPackagingID.

DEFINE TEMP-TABLE ttPackagingSurface
   FIELD PackagingSurfaceID AS INTEGER
   FIELD PackagingSegmentID AS INTEGER
   FIELD SurfaceHeight      AS INTEGER
   FIELD SurfaceWidth       AS INTEGER   
   FIELD SurfaceTotal       AS DECIMAL FORMAT ">>>>>>>>>>>9.999"
   FIELD SurfaceDescription AS CHARACTER
   INDEX PackagingSegmentID PackagingSegmentID. 

DEFINE TEMP-TABLE ttPackagingSegment
   FIELD PackagingSegmentID         AS INTEGER  
   FIELD OriginalPackagingSegmentID AS INTEGER        
   FIELD ParentPackagingSegmentID   AS INTEGER     
   FIELD MasterPackagingID          AS INTEGER       
   FIELD SegmentHeight              AS DECIMAL
   FIELD SegmentWidth               AS DECIMAL
   FIELD SegmentDepth               AS DECIMAL
   FIELD SegmentVolume              AS DECIMAL FORMAT ">>>>>>>>>>>9.999"  
   FIELD Used                       AS LOGICAL
   FIELD Suitable                   AS LOGICAL      
   INDEX PackagingSegmentID IS UNIQUE PackagingSegmentID
   INDEX MasterPackagingID MasterPackagingID.
   
DEFINE TEMP-TABLE ttItemToPack
   FIELD ItemToPackID               AS INTEGER
   FIELD PartID                     LIKE Part.PartID
   FIELD PartHeight                 AS DECIMAL
   FIELD PartWidth                  AS DECIMAL
   FIELD PartDepth                  AS DECIMAL
   FIELD PartVolume                 AS DECIMAL FORMAT ">>>>>>>>>>>9.999"
   FIELD PartWeight                 AS DECIMAL FORMAT ">>>>>>>>>>>9.999"   
   FIELD Packed                     AS LOGICAL
   FIELD PackagingSegmentID         AS INTEGER
   FIELD OriginalPackagingSegmentID AS INTEGER
   INDEX ItemToPackID IS UNIQUE ItemToPackID
   INDEX PackagingSegmentID PackagingSegmentID.   
   
DEFINE TEMP-TABLE ttItemSurface
   FIELD ItemSurfaceID      AS INTEGER
   FIELD ItemToPackID       AS INTEGER
   FIELD SurfaceHeight      AS INTEGER
   FIELD SurfaceWidth       AS INTEGER      
   FIELD SurfaceTotal       AS DECIMAL
   FIELD SurfaceDescription AS CHARACTER
   INDEX ItemToPackID ItemToPackID.
   
/* Buffers */
DEFINE BUFFER checkParentSegment          FOR ttPackagingSegment.    
DEFINE BUFFER checkSegment                FOR ttPackagingSegment.    
DEFINE BUFFER createChildSegment          FOR ttPackagingSegment.  
DEFINE BUFFER createChildPackagingSurface FOR ttPackagingSurface.  
DEFINE BUFFER checkItemToPackVolume       FOR ttItemToPack.
DEFINE BUFFER resetItemToPack             FOR ttItemToPack.

/* Local Variables */
DEFINE VARIABLE intItemToPackCount AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE intPackagingCount  AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE chrLogicError      AS CHARACTER NO-UNDO.
DEFINE VARIABLE logPartWidthOK     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logPartHeightOK    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE logPartDepthOK     AS LOGICAL   NO-UNDO.

FUNCTION CreateMasterPackaging RETURNS CHARACTER():
  
   /* Check Packaging Exists */
   IF NOT CAN-FIND(FIRST ShipPackageSize WHERE ShipPackageSize.Active) THEN
   DO:
      chrLogicError = "No Active ShipPackageSize records exist.".
      RETURN ERROR chrLogicError.    
   END.

   /* Validate Packaging */
   FOR EACH ShipPackageType NO-LOCK
      WHERE ShipPackageType.Active:
      
      FOR EACH ShipPackageSize OF ShipPackageType NO-lOCK
         WHERE ShipPackageSize.Active:
    
         /* Validate the Dimensions are Set */
         IF ShipPackageSize.PackageHeight = 0 THEN
            chrLogicError = "ShipPackageSize Height is Mandatory for Packing Logic".
         IF ShipPackageSize.PackageWidth = 0 THEN
            chrLogicError = "ShipPackageSize Width is Mandatory for Packing Logic".
         IF ShipPackageSize.PackageDepth = 0 THEN
            chrLogicError = "ShipPackageSize Depth is Mandatory for Packing Logic".      
          
      END.
      
   END.   
   
   IF chrLogicError <> "" THEN
      RETURN ERROR chrLogicError.    
   
   /* Store Master Data in Temp-Table */
   FOR EACH ShipPackageType NO-LOCK
      WHERE ShipPackageType.Active:   
      FOR EACH ShipPackageSize OF ShipPackageType NO-lOCK
         WHERE ShipPackageSize.Active:
                  
         CREATE ttMasterPackaging.
         ASSIGN ttMasterPackaging.MasterPackagingID = ShipPackageSize.ShipPackageSizeID
                ttMasterPackaging.ShipPackageTypeID = ShipPackageSize.ShipPackageTypeID
                ttMasterPackaging.ShipPackageSizeID = ShipPackageSize.ShipPackageSizeID
                ttMasterPackaging.MasterHeight      = ShipPackageSize.PackageHeight
                ttMasterPackaging.MasterWidth       = ShipPackageSize.PackageWidth
                ttMasterPackaging.MasterDepth       = ShipPackageSize.PackageDepth
                ttMasterPackaging.MasterVolume      = (ShipPackageSize.PackageHeight * ShipPackageSize.PackageWidth * ShipPackageSize.PackageDepth)
                ttMasterPackaging.Suitable          = TRUE.
      
      END.
   END.  

END FUNCTION.

FUNCTION CreatePackagingSegmentFromMaster RETURNS INTEGER(INPUT intMasterPackagingID AS INTEGER):

   FIND FIRST ttMasterPackaging NO-LOCK
      WHERE ttMasterPackaging.MasterPackagingID = intMasterPackagingID NO-ERROR.              
                 
   CREATE ttPackagingSegment.
   ASSIGN ttPackagingSegment.PackagingSegmentID         = intPackagingCount
          ttPackagingSegment.MasterPackagingID          = ttMasterPackaging.MasterPackagingID
          ttPackagingSegment.OriginalPackagingSegmentID = ttPackagingSegment.PackagingSegmentID
          ttPackagingSegment.SegmentHeight              = ttMasterPackaging.MasterHeight
          ttPackagingSegment.SegmentWidth               = ttMasterPackaging.MasterWidth
          ttPackagingSegment.SegmentDepth               = ttMasterPackaging.MasterDepth
          ttPackagingSegment.SegmentVolume              = (ttMasterPackaging.MasterHeight * ttMasterPackaging.MasterWidth * ttMasterPackaging.MasterDepth)
          ttPackagingSegment.Suitable                   = TRUE.
   
   intPackagingCount = intPackagingCount + 1.
   
   /* Create Surface records */
   /* Surface 1 - Height * Width */
   CREATE ttPackagingSurface.
   ASSIGN ttPackagingSurface.PackagingSurfaceID = 1
          ttPackagingSurface.PackagingSegmentID = ttPackagingSegment.PackagingSegmentID
          ttPackagingSurface.SurfaceHeight      = ttPackagingSegment.SegmentHeight
          ttPackagingSurface.SurfaceWidth       = ttPackagingSegment.SegmentWidth          
          ttPackagingSurface.SurfaceTotal       = ttPackagingSegment.SegmentHeight * ttPackagingSegment.SegmentWidth
          ttPackagingSurface.SurfaceDescription = "Height by Width".
           
   /* Surface 2 - Height * Depth */
   CREATE ttPackagingSurface.
   ASSIGN ttPackagingSurface.PackagingSurfaceID = 2
          ttPackagingSurface.PackagingSegmentID = ttPackagingSegment.PackagingSegmentID
          ttPackagingSurface.SurfaceHeight      = ttPackagingSegment.SegmentHeight
          ttPackagingSurface.SurfaceWidth       = ttPackagingSegment.SegmentDepth                    
          ttPackagingSurface.SurfaceTotal       = ttPackagingSegment.SegmentHeight * ttPackagingSegment.SegmentDepth
          ttPackagingSurface.SurfaceDescription = "Height by Depth".

   /* Surface 3 - Width * Depth */
   CREATE ttPackagingSurface.
   ASSIGN ttPackagingSurface.PackagingSurfaceID = 3
          ttPackagingSurface.PackagingSegmentID = ttPackagingSegment.PackagingSegmentID
          ttPackagingSurface.SurfaceHeight      = ttPackagingSegment.SegmentWidth
          ttPackagingSurface.SurfaceWidth       = ttPackagingSegment.SegmentDepth                    
          ttPackagingSurface.SurfaceTotal       = ttPackagingSegment.SegmentWidth * ttPackagingSegment.SegmentDepth
          ttPackagingSurface.SurfaceDescription = "Width by Depth".     
          
   RETURN ttPackagingSegment.PackagingSegmentID.       

END.

FUNCTION CreateRemainingPackaging RETURNS LOGICAL(INPUT intParentSegmentID AS INTEGER,
                                                  INPUT decHeight     AS DECIMAL,
                                                  INPUT decWidth      AS DECIMAL,
                                                  INPUT decDepth      AS DECIMAL):
                                                  
   /* Dont Create if any Dimension is 0 */
   IF decHeight = 0 OR decWidth = 0 OR decDepth = 0 THEN
   DO:
      RETURN FALSE.
   END.
                                                  
   FIND FIRST checkParentSegment NO-LOCK
      WHERE checkParentSegment.PackagingSegmentID = intParentSegmentID NO-ERROR.              
                 
   CREATE createChildSegment.
   ASSIGN createChildSegment.PackagingSegmentID         = intPackagingCount
          createChildSegment.MasterPackagingID          = checkParentSegment.MasterPackagingID
          createChildSegment.OriginalPackagingSegmentID = checkParentSegment.OriginalPackagingSegmentID
          createChildSegment.ParentPackagingSegmentID   = checkParentSegment.PackagingSegmentID          
          createChildSegment.SegmentHeight              = decHeight
          createChildSegment.SegmentWidth               = decWidth
          createChildSegment.SegmentDepth               = decDepth
          createChildSegment.SegmentVolume              = (createChildSegment.SegmentHeight * createChildSegment.SegmentWidth * createChildSegment.SegmentDepth)
          createChildSegment.Suitable                   = TRUE.
   
   intPackagingCount = intPackagingCount + 1.   
   
   /* Create Surface records */
   /* Surface 1 - Height * Width */
   CREATE createChildPackagingSurface.
   ASSIGN createChildPackagingSurface.PackagingSurfaceID = 1
          createChildPackagingSurface.PackagingSegmentID = createChildSegment.PackagingSegmentID
          createChildPackagingSurface.SurfaceHeight      = createChildSegment.SegmentHeight
          createChildPackagingSurface.SurfaceWidth       = createChildSegment.SegmentWidth          
          createChildPackagingSurface.SurfaceTotal       = createChildSegment.SegmentHeight * createChildSegment.SegmentWidth
          createChildPackagingSurface.SurfaceDescription = "Height by Width".
           
   /* Surface 2 - Height * Depth */
   CREATE createChildPackagingSurface.
   ASSIGN createChildPackagingSurface.PackagingSurfaceID = 2
          createChildPackagingSurface.PackagingSegmentID = createChildSegment.PackagingSegmentID
          createChildPackagingSurface.SurfaceHeight      = createChildSegment.SegmentHeight
          createChildPackagingSurface.SurfaceWidth       = createChildSegment.SegmentDepth                    
          createChildPackagingSurface.SurfaceTotal       = createChildSegment.SegmentHeight * createChildSegment.SegmentDepth
          createChildPackagingSurface.SurfaceDescription = "Height by Depth".

   /* Surface 3 - Width * Depth */
   CREATE createChildPackagingSurface.
   ASSIGN createChildPackagingSurface.PackagingSurfaceID = 3
          createChildPackagingSurface.PackagingSegmentID = createChildSegment.PackagingSegmentID
          createChildPackagingSurface.SurfaceHeight      = createChildSegment.SegmentWidth
          createChildPackagingSurface.SurfaceWidth       = createChildSegment.SegmentDepth                    
          createChildPackagingSurface.SurfaceTotal       = createChildSegment.SegmentWidth * createChildSegment.SegmentDepth
          createChildPackagingSurface.SurfaceDescription = "Width by Depth".     

END.

FUNCTION CreateItemsToPack RETURNS CHARACTER(INPUT intPartID AS INTEGER,
                                             INPUT intPartQty AS INTEGER):

   DEFINE VARIABLE intPartQtyToCreate AS INTEGER.
   
   /* Reset Variables */
   chrLogicError = "".
   logPartHeightOK = FALSE.
   logPartWidthOK = FALSE.
   logPartDepthOK = FALSE.
   
   /* Validate Part Exists */
   FIND FIRST Part NO-LOCK 
      WHERE Part.PartID = intPartID NO-ERROR.
   IF NOT AVAIlABLE Part THEN
   DO:
     chrLogicError = "PartID " + STRING(intPartID) + " does NOT exist.".
     RETURN chrLogicError. 
   END.  
   
   /* Validate the Dimensions are Set */
   IF Part.Height = 0 THEN
      chrLogicError = "Part Height for " + Part.PartRef + " is Mandatory for Packing Logic".
   IF Part.Width = 0 THEN
      chrLogicError = "Part Width for " + Part.PartRef + " is Mandatory for Packing Logic".
   IF Part.Depth = 0 THEN
      chrLogicError = "Part Depth for " + Part.PartRef + " is Mandatory for Packing Logic".      
      
   /* Validate no Dims are to Big */
   FOR EACH ttMasterPackaging NO-LOCK:
      
      /* Check Height */
      IF Part.Height <= ttMasterPackaging.MasterHeight OR
         Part.Height <= ttMasterPackaging.MasterWidth OR
         Part.Height <= ttMasterPackaging.MasterDepth THEN
      DO:
         logPartHeightOK = TRUE. 
      END.   
      
      /* Check Width */
      IF Part.Width <= ttMasterPackaging.MasterHeight OR
         Part.Width <= ttMasterPackaging.MasterWidth OR
         Part.Width <= ttMasterPackaging.MasterDepth THEN 
      DO:
         logPartWidthOK = TRUE.
      END.   

      /* Check Depth */      
      IF Part.Depth <= ttMasterPackaging.MasterHeight OR
         Part.Depth <= ttMasterPackaging.MasterWidth OR
         Part.Depth <= ttMasterPackaging.MasterDepth THEN 
      DO:
         logPartDepthOK = TRUE.
      END.   
      
   END.  
   
   /* Check we can pack the part */
   IF NOT (logPartHeightOK AND logPartWidthOK AND logPartDepthOK) THEN
   DO:
      IF logPartHeightOK = FALSE THEN
         chrLogicError = "Height".   
      
      IF logPartWidthOK = FALSE THEN
         chrLogicError = "Width".   
   
      IF logPartDepthOK = FALSE THEN
         chrLogicError = "Depth".   
       
      chrLogicError = chrLogicError + " Dimension for Part " + Part.PartRef + " does NOT fit into any of the ShipPackageTypes Setup".      
   END.
      
   IF chrLogicError <> "" THEN
      RETURN chrLogicError.   
      
   /* Loop through the Qty of the Part to Pack */
   DO intPartQtyToCreate = 1 to intPartQty:
      CREATE ttItemToPack.
      ASSIGN ttItemToPack.ItemToPackID = intItemToPackCount
             ttItemToPack.PartID       = Part.PartID
             ttItemToPack.PartHeight   = Part.Height + ((Part.Height / 100) * 2)
             ttItemToPack.PartWidth    = Part.Width + ((Part.Height / 100) * 2)
             ttItemToPack.PartDepth    = Part.Depth + ((Part.Height / 100) * 2)
             ttItemToPack.PartVolume   = (ttItemToPack.PartHeight * ttItemToPack.PartWidth * ttItemToPack.PartDepth)
             ttItemToPack.PartWeight   = Part.UnitWeight.
             
      intItemToPackCount = intItemToPackCount + 1.       
             
      /* Create Surface records */
      /* Surface 1 - Height * Width */
      CREATE ttItemSurface.
      ASSIGN ttItemSurface.ItemSurfaceID      = 1
             ttItemSurface.ItemToPackID       = ttItemToPack.ItemToPackID 
             ttItemSurface.SurfaceHeight      = ttItemToPack.PartHeight
             ttItemSurface.SurfaceWidth       = ttItemToPack.PartWidth
             ttItemSurface.SurfaceTotal       = ttItemToPack.PartHeight * ttItemToPack.PartWidth
             ttItemSurface.SurfaceDescription = "Height by Width".
             
      /* Surface 2 - Height * Depth */
      CREATE ttItemSurface.
      ASSIGN ttItemSurface.ItemSurfaceID      = 2
             ttItemSurface.ItemToPackID       = ttItemToPack.ItemToPackID 
             ttItemSurface.SurfaceHeight      = ttItemToPack.PartHeight
             ttItemSurface.SurfaceWidth       = ttItemToPack.PartDepth             
             ttItemSurface.SurfaceTotal       = ttItemToPack.PartHeight * ttItemToPack.PartDepth
             ttItemSurface.SurfaceDescription = "Height by Depth".

      /* Surface 3 - Width * Depth */
      CREATE ttItemSurface.
      ASSIGN ttItemSurface.ItemSurfaceID      = 3
             ttItemSurface.ItemToPackID       = ttItemToPack.ItemToPackID 
             ttItemSurface.SurfaceHeight      = ttItemToPack.PartWidth
             ttItemSurface.SurfaceWidth       = ttItemToPack.PartDepth             
             ttItemSurface.SurfaceTotal       = ttItemToPack.PartWidth * ttItemToPack.PartDepth
             ttItemSurface.SurfaceDescription = "Width by Depth".             
             
   END.   
   
   RETURN "OK".

END FUNCTION.
   
FUNCTION GetLargestItemLeftToPack RETURNS INTEGER():

   /* Loop through the Unpacked Items by Size */
   FOR EACH ttItemToPack NO-LOCK
      WHERE ttItemToPack.Packed = FALSE BY ttItemToPack.PartVolume DESCENDING:
      
      RETURN ttItemToPack.ItemToPackID.
      
   END.    
  
END FUNCTION.
   
FUNCTION GetLargestSurfaceOfItemToPack RETURNS DECIMAL(INPUT intItemToPackID AS INTEGER):

   /* Loop through the Surfaces of the Item by Size */
   FOR EACH ttItemSurface OF ttItemToPack BY SurfaceTotal DESCENDING:
   
      RETURN ttItemSurface.ItemSurfaceID.
      
   END.    

END FUNCTION.

FUNCTION SelectMasterPackagingByVolume RETURNS INTEGER():

   DEFINE VARIABLE decTotalVolume AS DECIMAL NO-UNDO.
   DEFINE VARIABLE intLastPackage AS INTEGER NO-UNDO.


   /* Total up the Volume the Items Left to Pack*/ 
   FOR EACH ttItemToPack NO-LOCK
      WHERE ttItemToPack.Packed = FALSE:
      
      decTotalVolume = decTotalVolume + ttItemToPack.PartVolume.
       
   END.

   /* Release the Buffer */
   RELEASE ttMasterPackaging NO-ERROR.
     
   /* Loop through the Packaging By Volume that is NOT a sub segment, it must be Complete Package. 
      Select the best fit and if none fit take the largest Package */                  
   SelectPackage_Loop:   
   FOR EACH ttMasterPackaging NO-LOCK
      WHERE ttMasterPackaging.MasterVolume > decTotalVolume
      AND   ttMasterPackaging.Suitable = TRUE BY ttMasterPackaging.MasterVolume:             
      
      /* Leave on the First Fit */
      LEAVE SelectPackage_Loop.            
   END.    
   
   IF AVAILABLE ttMasterPackaging THEN
   DO:
      RETURN ttMasterPackaging.MasterPackagingID.
   END.
   ELSE
   DO:
      /* Get the Largest */  
      FOR EACH ttMasterPackaging BY ttMasterPackaging.MasterVolume:
          intLastPackage = ttMasterPackaging.MasterPackagingID.
      END.   
      
      RETURN intLastPackage.  
   END.

END FUNCTION.

FUNCTION GetLargestMasterPackage RETURNS INTEGER():

   FOR EACH ttMasterPackaging BY ttMasterPackaging.MasterVolume:
            
   END.   
   
   RETURN ttMasterPackaging.MasterPackagingID.
   
END FUNCTION.

FUNCTION CheckIfLargestMasterPackage RETURNS LOGICAL(INPUT intMasterPackagingID AS INTEGER):

   DEFINE VARIABLE decCurrentVolume as DECIMAL NO-UNDO. 

   FIND FIRST ttMasterPackaging NO-LOCK 
      WHERE ttMasterPackaging.MasterPackagingID = intMasterPackagingID NO-ERROR.
      
   decCurrentVolume = ttMasterPackaging.MasterVolume.

   FOR EACH ttMasterPackaging:
   
      IF ttMasterPackaging.MasterVolume > decCurrentVolume THEN
      DO:
         RETURN FALSE.
      END.
      
   END.   
   
   RETURN TRUE. 

END FUNCTION.

FUNCTION CheckIfAnySegmentsRemain RETURNS LOGICAL():

   FIND FIRST checkSegment NO-LOCK
      WHERE checkSegment.Suitable = TRUE NO-ERROR.
   IF AVAILABLE checkSegment THEN
      RETURN TRUE.
   ELSE   
      RETURN FALSE.
      
END FUNCTION.

FUNCTION SelectPackagingSegmentByVolume RETURNS INTEGER(INPUT intItemToPackID AS INTEGER):

   /* Get the Next largest ttItemToPack by Volume */ 
   FIND FIRST checkItemToPackVolume NO-LOCK
      WHERE checkItemToPackVolume.ItemToPackID = intItemToPackID.
            
   /* Loop through the Sub Packaging Segments By Volume that are not Used */                  
   SelectPackage_Loop:   
   FOR EACH ttPackagingSegment NO-LOCK
      WHERE ttPackagingSegment.SegmentVolume >= ttItemToPack.PartVolume
      AND   ttPackagingSegment.Used = FALSE
      AND   ttPackagingSegment.Suitable = TRUE BY ttPackagingSegment.SegmentVolume:
      /* Leave on the First Fit */
      LEAVE SelectPackage_Loop.            
   END.    
   
   IF AVAILABLE ttPackagingSegment THEN
   DO:
      RETURN ttPackagingSegment.PackagingSegmentID.
   END.
   
END FUNCTION.

FUNCTION IsShipPackageEnvelope RETURNS LOGICAL(INPUT intShipPackageSizeID AS INTEGER):

   FIND FIRST ShipPackageSize NO-LOCK
      WHERE ShipPackageSize.ShipPackageSizeID = intShipPackageSizeID NO-ERROR.
   IF AVAILABLE ShipPackageSize THEN
   DO:   
      FIND FIRST ShipPackageType OF ShipPackageSize NO-LOCK NO-ERROR.
      IF AVAILABLE ShipPackageType THEN
         RETURN ShipPackageType.IsEnvelope.
   END.
   
   RETURN TRUE.
        
END FUNCTION.

FUNCTION PackItem RETURNS LOGICAL(INPUT intPackagingSegmentID AS INTEGER,
                                  INPUT intItemToPackID AS INTEGER,
                                  INPUT intItemSurfaceID AS INTEGER):

   DEFINE VARIABLE decItemDepth  AS DECIMAL.
   DEFINE VARIABLE decItemHeight AS DECIMAL.
   DEFINE VARIABLE decItemWidth  AS DECIMAL.      
                                                                 
   /* Get the Item */ 
   FIND FIRST ttItemToPack NO-LOCK
      WHERE ttItemToPack.ItemToPackID = intItemToPackID NO-ERROR.
     
   /* Get the Item Surface */   
   FIND FIRST ttItemSurface NO-LOCK
      WHERE ttItemSurface.ItemToPackID  = ttItemToPack.ItemToPackID
      AND   ttItemSurface.ItemSurfaceID = intItemSurfaceID NO-ERROR.
      
   CASE ttItemSurface.ItemSurfaceID:
      WHEN 1 THEN DO:
         decItemDepth = ttItemToPack.PartDepth.
         decItemHeight = ttItemToPack.PartHeight.
         decItemWidth = ttItemToPack.PartWidth.         
      END.
      WHEN 2 THEN DO:
         decItemDepth = ttItemToPack.PartWidth.      
         decItemHeight = ttItemToPack.PartHeight.
         decItemWidth = ttItemToPack.PartDepth.                  
      END.
      WHEN 3 THEN DO:
         decItemDepth = ttItemToPack.PartHeight.
         decItemHeight = ttItemToPack.PartWidth.
         decItemWidth = ttItemToPack.PartDepth.                           
      END.      
   END CASE.     
      
   /* Get the Packaging Segment */   
   FIND FIRST ttPackagingSegment NO-LOCK
      WHERE ttPackagingSegment.PackagingSegmentID = intPackagingSegmentID NO-ERROR.
      
   /* Compare */
   SurfaceCompare_Loop:
   FOR EACH ttPackagingSurface OF ttPackagingSegment NO-LOCK
      WHERE ttPackagingSurface.SurfaceTotal > ttItemSurface.SurfaceTotal:

      /* Check 3rd dimension */
      CASE ttPackagingSurface.PackagingSurfaceID:
         WHEN 1 THEN DO:
            IF decItemDepth > ttPackagingSegment.SegmentDepth THEN 
            DO:
               NEXT SurfaceCompare_Loop.
            END.   
         END.
         WHEN 2 THEN DO:
            IF decItemDepth > ttPackagingSegment.SegmentWidth THEN
            DO:
               NEXT SurfaceCompare_Loop.
            END.   
         END.
         WHEN 3 THEN DO:
            IF decItemDepth > ttPackagingSegment.SegmentHeight THEN
            DO:
               NEXT SurfaceCompare_Loop.
            END.               
         END.      
      END CASE.   
      
      /* Check Dimensions against ttItemToPack largest surface */
      IF ttPackagingSurface.SurfaceHeight >= ttItemSurface.SurfaceHeight AND 
         ttPackagingSurface.SurfaceWidth >= ttItemSurface.SurfaceWidth THEN      
      DO:   
         LEAVE SurfaceCompare_Loop.
      END.   
      
      /* Check Reverse against ttItemToPack largest surface */
      IF ttPackagingSurface.SurfaceHeight >= ttItemSurface.SurfaceWidth AND 
         ttPackagingSurface.SurfaceWidth >= ttItemSurface.SurfaceHeight THEN      
      DO:            
         LEAVE SurfaceCompare_Loop.
      END.         
      
   END.            
   
   /* Set the ttPackagingSegment to Suitable = False if cannot fit on any Surface */
   IF NOT AVAILABLE ttPackagingSurface THEN
   DO:
      FIND FIRST ttMasterPackaging OF ttPackagingSegment EXCLUSIVE-LOCK NO-ERROR.
            
      ttMasterPackaging.Suitable = FALSE.
      ttPackagingSegment.Suitable = FALSE.
                  
      RETURN FALSE.
   END.

   /* Item was Packed */
   ttItemToPack.Packed = TRUE.
   ttItemToPack.PackagingSegmentID = ttPackagingSegment.PackagingSegmentID.
   ttItemToPack.OriginalPackagingSegmentID = ttPackagingSegment.OriginalPackagingSegmentID.
   ttPackagingSegment.Used = TRUE.

   /* Check which Surface to Paak against to Determine New Segments */   
   CASE ttPackagingSurface.PackagingSurfaceID:
      WHEN 1 THEN DO:
         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  decItemHeight,
                                  ttPackagingSegment.SegmentWidth,
                                  (ttPackagingSegment.SegmentDepth - decItemDepth)).                                  

         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  decItemHeight,
                                  (ttPackagingSegment.SegmentWidth - decItemWidth),
                                  decItemDepth).                                  
                                  
         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  (ttPackagingSegment.SegmentHeight - decItemHeight),
                                  ttPackagingSegment.SegmentWidth,
                                  ttPackagingSegment.SegmentDepth).
                                  
      END.
      WHEN 2 THEN DO:
         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  decItemHeight,
                                  ttPackagingSegment.SegmentWidth,
                                  (ttPackagingSegment.SegmentDepth - decItemWidth)).                                  

         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  decItemHeight,
                                  (ttPackagingSegment.SegmentWidth - decItemDepth),
                                  decItemWidth).                                  
                                  
         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  (ttPackagingSegment.SegmentHeight - decItemHeight),
                                  ttPackagingSegment.SegmentDepth,
                                  ttPackagingSegment.SegmentWidth).
         
      END.
      WHEN 3 THEN DO:
         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  decItemDepth,
                                  ttPackagingSegment.SegmentWidth,
                                  (ttPackagingSegment.SegmentDepth - decItemHeight)).                                  

         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  decItemDepth,
                                  (ttPackagingSegment.SegmentWidth - decItemWidth),
                                  decItemHeight).                                  
                                  
         CreateRemainingPackaging(ttPackagingSegment.PackagingSegmentID,
                                  (ttPackagingSegment.SegmentHeight - decItemDepth),
                                  ttPackagingSegment.SegmentWidth,
                                  ttPackagingSegment.SegmentDepth).         
      END.      
   END CASE.      
      
   RETURN TRUE.
           
END FUNCTION.

FUNCTION NewTmsPackageRef RETURNS CHAR (INPUT intNewPackageID  AS INTEGER):
      
   RETURN "TmsPck" + STRING(ShipPackageCalculation.ShipPackageCalculationID, "99999999").
   
END FUNCTION. /* fNewPackageRef */


FUNCTION DeletePackagingSegmentAndResetItems RETURNS LOGICAL(INPUT intPackagingSegmentID AS INTEGER):
 
   FIND FIRST ttPackagingSegment EXCLUSIVE-LOCK 
      WHERE ttPackagingSegment.PackagingSegmentID = intPackagingSegmentID NO-ERROR.
  
   DELETE ttPackagingSegment.
   
   FOR EACH ttPackagingSegment EXCLUSIVE-LOCK
      WHERE ttPackagingSegment.OriginalPackagingSegmentID = intPackagingSegmentID:
      
      DELETE ttPackagingSegment.
            
   END. 
   
   FOR EACH resetItemToPack EXCLUSIVE-LOCK
      WHERE resetItemToPack.OriginalPackagingSegmentID = intPackagingSegmentID:
      
      resetItemToPack.OriginalPackagingSegmentID = 0.   
      resetItemToPack.PackagingSegmentID = 0.
      resetItemToPack.Packed = FALSE.         
         
   END.   
      
END FUNCTION.

FUNCTION DeleteAllUnUsedPackagingSegments RETURNS LOGICAL():
    
   FOR EACH ttPackagingSegment EXCLUSIVE-LOCK
      WHERE ttPackagingSegment.Used = FALSE:
      
      DELETE ttPackagingSegment.
      
   END. 
      
END FUNCTION.

FUNCTION ResetMasterPackaging RETURNS LOGICAL():

   /* Set All Master Packaging back to Suitable */
   FOR EACH ttMasterPackaging EXCLUSIVE-LOCK:
   
      ttMasterPackaging.Suitable = TRUE.   
       
   END.

END FUNCTION.

FUNCTION ResetItemsToPack RETURNS LOGICAL():

   /* Clear the Packing Fields and Delete the Segments */
   FOR EACH ttItemToPack EXCLUSIVE-LOCK:
   
      ttItemToPack.OriginalPackagingSegmentID = 0.   
      ttItemToPack.PackagingSegmentID = 0.
      ttItemToPack.Packed = FALSE.   
      
   END.

   EMPTY TEMP-TABLE ttPackagingSegment.
   EMPTY TEMP-TABLE ttPackagingSurface.

END FUNCTION.

FUNCTION ClearAllTempData RETURNS LOGICAL():

   EMPTY TEMP-TABLE ttItemToPack.
   EMPTY TEMP-TABLE ttItemSurface.
   EMPTY TEMP-TABLE ttPackagingSegment.
   EMPTY TEMP-TABLE ttPackagingSurface.   
   EMPTY TEMP-TABLE ttMasterPackaging.
   
   /* Reset Setup Variables */
   intItemToPackCount = 0.
   intPackagingCount  = 0.
   chrLogicError      = "".   

END FUNCTION.

