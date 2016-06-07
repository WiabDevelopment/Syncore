/*******************************************  NetworkDeviceActionPoints Delete Trigger  ***************************************

Author    : Anthony Ferrari
Descr     : Delete Trigger for NetworkDeviceActionPoints Admin screen.
Created   : 17/09/2015
Revisions :

**************************************************************************************************************************/
TRIGGER PROCEDURE FOR DELETE OF NetworkActionPoint.

{trgValidateSession.i}

{trgCreateAudit.i "NetworkActionPoint"}

/* Bespoke Trigger Code goes here */
