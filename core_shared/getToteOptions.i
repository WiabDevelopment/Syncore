/*------------------------------------------------------------------------------------------------------------------------------------------
Program : getToteOptions.i
Purpose : This sets a group of variables that will be used for Totes both in Character and Web environments. Included in all programs 
          using Tote manipulation.
Author  : BG
Date    : 24th Sep 2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
04/06/2015 ND   Canon     No config file and field is not used so all code is commented out for now. ProcessOption table is being removed
------------------------------------------------------------------------------------------------------------------------------------------*/
/* This include has a dependancy on defSessionVariables.i */

/* Will we be using Parent Totes such as a Multi-Tote Trolley */
/*DEFINE VARIABLE logUsingParentTotes               AS LOGICAL NO-UNDO.*/

/*FIND ProcessOption WHERE ProcessOption.OptionName = "UsingParentTotes" NO-LOCK NO-ERROR.*/
/*IF NOT AVAILABLE ProcessOption THEN                                                     */
/*   chrError = chrError + "No Process Option available for Tote & UsingParentTotes. ".   */
/*ELSE                                                                                    */
/*   logUsingParentTotes = ProcessOption.Active.                                          */





