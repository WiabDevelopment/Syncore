/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getShippingOptions.i
Purpose : This sets a group of variables that will be used in shipping both in Character and Web enviroments. Included in all programs 
          in shipping.
Author  : ND
Date    : 17/04/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on usrSession.i */

/* Are we allowed Add Multiple ShipLanes to an Outbound Trailer? */
DEFINE VARIABLE logAllowMultiShipLanesPerOutbound      AS LOGICAL     NO-UNDO.

/* Allow Multiple Users Build Single ShipPallet at once? */
DEFINE VARIABLE logAllowMultiUsersBuildSinglePallet    AS LOGICAL     NO-UNDO.

/* Enforce Dangerous Goods Calcualtions? */
DEFINE VARIABLE logEnforceDangerousGoodsValueCalcs     AS LOGICAL     NO-UNDO.

FIND FIRST ShippingConfig NO-LOCK NO-ERROR.
IF NOT AVAILABLE ShippingConfig THEN
DO:
   chrError = chrError + "No ShippingConfig exist. Please create ShippingConfig record.".
END.
ELSE
DO:
   ASSIGN
    logAllowMultiUsersBuildSinglePallet = ShippingConfig.AllowMultiUsersBuildSinglePallet
    logAllowMultiShipLanesPerOutbound   = ShippingConfig.AllowMultiShipLanesPerOutbound
    logEnforceDangerousGoodsValueCalcs  = ShippingConfig.EnforceDangerousGoodsValueCalcs.
    
    
   
END.
