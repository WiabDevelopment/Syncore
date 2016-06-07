/*******************************************  NetworkDeviceReaders Delete Trigger  ***************************************

Author    : Anthony Ferrari
Descr     : Delete Trigger for NetworkDeviceReaders Admin screen.
Created   : 17/09/2015
Revisions :

**************************************************************************************************************************/
TRIGGER PROCEDURE FOR DELETE OF NetworkReader.

{trgValidateSession.i}

{trgCreateAudit.i "NetworkReader"}

/* Bespoke Trigger Code goes here */
