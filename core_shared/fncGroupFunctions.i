/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncGroupFunctions.i 
Purpose : Functions to handling arbitrary Groups easier 
Author  : Todd Wierzchowski
Date    : 14/07/2015
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
14/07/2015 TAW CanonTLB   add fInShipOrderStreamGroup
------------------------------------------------------------------------------------------------------------------------------------------*/

FUNCTION fInShipOrderStreamGroup RETURNS CHARACTER (INPUT intStreamID  AS INTEGER,
                                                    INPUT chrGroupCode AS CHARACTER):
             
   DEFINE BUFFER readShipOrderStreamGroup     FOR ShipOrderStreamGroup.
   DEFINE BUFFER readShipOrderStreamGroupLink FOR ShipOrderStreamGroupLink.
   DEFINE BUFFER readShipOrderStream          FOR ShipOrderStream.
   
   FIND FIRST readShipOrderStreamGroup NO-LOCK
      WHERE readShipOrderStreamGroup.GroupCode = chrGroupCode NO-ERROR.
      
   IF NOT AVAILABLE readShipOrderStreamGroup THEN
      RETURN "Error: ShipOrderStreamGroup does not exist with GroupCode: [" + chrGroupCode + "]. Cannot continue!!".
      
   IF CAN-FIND(FIRST readShipOrderStreamGroupLink /* idx=ShipOrderStreamGroupID */
                  WHERE readShipOrderStreamGroupLink.ShipOrderStreamGroupID = readShipOrderStreamGroup.ShipOrderStreamGroupID
                  AND   readShipOrderStreamGroupLink.ShipOrderStreamID = intStreamID
                  AND   readShipOrderStreamGroupLink.Active) THEN
       
      RETURN "YES".
   
   ELSE
      RETURN "NO".

END FUNCTION.