/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getInventoryOptions.i
Purpose : This sets a group of variables that will be used in Inventory both in Character and Web enviroments. Included in all programs 
          in Inventory.
Author  : BG
Date    : 8th Oct 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/03/2015  ND  CanonTlb   No table named InventoryConfig and not used in any other file.       
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on usrSession.i */

/* Will we need to record more than one Serial per Inventory Unit ?*/
/*DEFINE VARIABLE logMultiSerialsPerUnit   AS LOGICAL NO-UNDO.*/
/*                                                                                           */
/*FIND FIRST InventoryConfig NO-LOCK NO-ERROR.                                               */
/*IF NOT AVAILABLE InventoryConfig THEN                                                      */
/*DO:                                                                                        */
/*   chrError = chrError + "No InventoryConfig exist. Please create InventoryConfig record.".*/
/*END.                                                                                       */
/*ELSE                                                                                       */
/*DO:                                                                                        */
/*   ASSIGN loglogMultiSerialsPerUnit = InventoryConfig.MultiSerialsPerUnit.                 */
/*END.                                                                                       */
