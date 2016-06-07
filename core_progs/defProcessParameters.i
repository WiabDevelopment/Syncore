/*------------------------------------------------------------------------------------------------------------------------------------------
Program : defProcessParameters.i
Purpose : Include to manage the parameters for all Character Programs that run
          from psProcessEngine.p
Author  : DC
Date    : 8th May 2012
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER intEventID    AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER chrResult     AS CHAR INITIAL "ERROR".

/* Get the Event so we Can Check the Complete and Message Suppress */
FIND FIRST ProcessEvent NO-LOCK
   WHERE ProcessEvent.ProcessEventID = intEventID NO-ERROR.

/*MESSAGE intEventID VIEW-AS ALERT-BOX.*/
