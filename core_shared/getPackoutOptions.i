/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getPackoutOptions.i
Purpose : This sets a group of variables that will be used in Packout both in Character and Web enviroments. Included in all programs 
          in Packout.
Author  : BG
Date    : 4th Sept 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/11/2013 BG  HyperMerge Renamed logPromptForPackagingPart to logPromptAdditionOfPackagingPart to be consistent with ProcessOption naming
06/04/2015 ND  Canon      Changed the call to now include the Config file instead of ProcessOption
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on defSessionVariables.i */
/*
{defSessionVariables.i}
*/

/*This option will be deprecated this is here as a place holder only */
DEFINE VARIABLE logAllowMultiPackout                       AS LOGICAL NO-UNDO INITIAL TRUE.

/* At Packout should we prompt the User to scan a Packaging Part (like a Cardboard Box) that they're going to use for that Package. */
DEFINE VARIABLE logPromptAdditionOfPackagingPart           AS LOGICAL NO-UNDO.

/* At Packout should we print Temp labels during Packout and then relabel all boxes when Packout is complete e.g 1 of 3, 2 of 3, 3 of 3 */
/* The alternative - if set to "NO" is that the User will be asked the number of Packages at the beginning and final labels will Print  */
DEFINE VARIABLE logPrintTempLabelsAtPackout                AS LOGICAL NO-UNDO.

/* Only applies if above logPrintTempLabelsAtPackout is set to NO - if this one is set to YES then it will print Package label after    */
/* Package has been completed - if its set to NO then the Package will be printed once its created and they'll be asked to verify label */
DEFINE VARIABLE logPrintLabelWhenPackageIsComplete         AS LOGICAL NO-UNDO.

/* In Packout, should the system allow the User to choose to Auto Select a Qty of 1 and Auto Submit if they scan the EAN Code of a Part? */
DEFINE VARIABLE logAllowAutoSubmitOnEanCodeScan            AS LOGICAL NO-UNDO.

/* Are we using Nota Fiscal Customs procedures after Packout? If so ShipPackages and ShipOrder will move to PackedOut after Packout  */ 
/* and then will be relocated to NF Waiting area where they will move to Awaiting NotaFiscal. Then the NF number will be added using */
/* a Web Admin update once its received.  */
DEFINE VARIABLE logUsingNotaFiscalAfterPackout             AS LOGICAL NO-UNDO.

/* Do we need to scan Serials in Packout? Will apply in case where we need Serials and they haven't been scanned already at Receiving */
DEFINE VARIABLE logScanSerialsAtPackout                    AS LOGICAL NO-UNDO.

/* Do we need to enter weight after complete of packout? If so a weight popup will popup after Complete of Packout before label prints */
DEFINE VARIABLE logPromptForWeightAfterPackout             AS LOGICAL NO-UNDO.

/* Do we need to display a dangerous goods message after complete of packout? If so, an appropriate dangerous goods message is displayed in a popup */
DEFINE VARIABLE logPromptDangGoodsMessageAtPackout AS LOGICAL NO-UNDO.

/* Do we need to not scan serials for complete package at packout? If so, no prompts for serials to scan will popup during packout of complete package */
/* Used as a hidden field in pacPackoutOrder.p, pacFastpackout.p */
DEFINE VARIABLE logSerialScanRequiredForCompletePackageAtPackout AS LOGICAL NO-UNDO.

/* Do we need to print a packing slip at the beginning of packout? If so a label will be printed at the beginning. */
DEFINE VARIABLE logPrintPackSlipAtBegin AS LOGICAL NO-UNDO.

FIND FIRST PackoutConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE PackoutConfig THEN
DO:
   chrError = chrError + "No PackoutConfig exist. Please create PackoutConfig record.".
END.
ELSE
DO:
  ASSIGN  logPromptAdditionOfPackagingPart                 = PackoutConfig.PromptAdditionOfPackagingPart
          logPrintTempLabelsAtPackout                      = PackoutConfig.PrintTempLabelsAtPackout
          logPrintLabelWhenPackageIsComplete               = PackoutConfig.PrintLabelWhenPackageIsComplete
          logAllowAutoSubmitOnEanCodeScan                  = PackoutConfig.AllowAutoSubmitOnEanCodeScan
          logUsingNotaFiscalAfterPackout                   = PackoutConfig.UsingNotaFiscalAfterPackout
          logScanSerialsAtPackout                          = PackoutConfig.ScanSerialsAtPackout
          logPromptForWeightAfterPackout                   = PackoutConfig.PromptForWeightAfterPackout
          logPromptDangGoodsMessageAtPackout               = PackoutConfig.PromptDangGoodsMessageAtPackout
          logPrintPackSlipAtBegin                          = PackoutConfig.PrintPackSlipAtBegin.
END.


