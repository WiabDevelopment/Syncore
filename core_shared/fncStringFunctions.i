/*------------------------------------------------------------------------------------------------------------------------------------------
Program : fncStringFunctions.i 
Purpose : Functions to Make String Operations Easier
Author  : MN
Date    : 20/09/2013
--------------------------------------------------------------------------------------------------------------------------------------------
Changes :
--------------------------------------------------------------------------------------------------------------------------------------------
Date       Who Project    Description
---------- --- ---------- ------------------------------------------------------------------------------------------------------------------
16/01/2014 MN  CR1027     Adding extra TRIM to center text only not possbile white spaces, 
26/03/2014 BR  CR1052     New function that Splits the usrprompt label to multiple lines if it exceeds 25 characters
03/04/2015 BG  CanopnTlb  Added fGetDelimitedEntry function to extract entries from list where the delimiter is more than 1 char in length.
------------------------------------------------------------------------------------------------------------------------------------------*/

/* Temp Tables */
DEFINE TEMP-TABLE ttWordWrap NO-UNDO   
      FIELD LineLabel AS CHARACTER.   

/* Appends Spaces In Front Of String to Center on Scanner Screen */
FUNCTION fCenterOnScreen RETURNS CHARACTER (INPUT chrStringToCenter AS CHARACTER):
   
   DEFINE VARIABLE intCenterSpace AS INTEGER NO-UNDO.

   /* Calculating Centering 
      Screen is 28 Chars Width And The String Starts From Column 1 */
   intCenterSpace = ((26 - LENGTH(TRIM(chrStringToCenter))) / 2) - ((IF LENGTH(TRIM(chrStringToCenter)) MOD 2 = 0 THEN 1 ELSE 2)) NO-ERROR.
   
   IF intCenterSpace > 0 AND intCenterSpace + LENGTH(TRIM(chrStringToCenter)) <= 28 THEN
      chrStringToCenter = FILL(" ",intCenterSpace) + TRIM(chrStringToCenter).
   
   RETURN chrStringToCenter.
   
END FUNCTION. /* fCenterOnScreen */


/*Splits the usrprompt label to multiple lines if it exceeds 25 characters*/
FUNCTION fWordWrapLine RETURNS CHARACTER (INPUT chrString    AS CHARACTER,
                                          INPUT intMaxLength AS INTEGER):
   
   DEFINE VARIABLE chrWord         AS CHARACTER NO-UNDO.  
   DEFINE VARIABLE chrReturnString AS CHARACTER NO-UNDO FORMAT "x(140)".   
   DEFINE VARIABLE intWordCount    AS INTEGER   NO-UNDO.
   
   Count_loop:
   DO intWordCount = 1 TO NUM-ENTRIES(chrString," "):
      
      chrWord = ENTRY(intWordCount,chrString," ").
   
      /* Create the First Line */
      IF NOT AVAILABLE(ttWordWrap) THEN 
      DO:
         CREATE ttWordWrap.
         ASSIGN ttWordWrap.LineLabel = ttWordWrap.LineLabel + chrWord.   
         NEXT Count_loop.
      END.
   
      IF LENGTH(ttWordWrap.LineLabel + " " + chrWord) > intMaxLength THEN
      DO:       
         CREATE ttWordWrap.
         ASSIGN ttWordWrap.LineLabel = ttWordWrap.LineLabel + chrWord.        
      END.
      ELSE
      DO:
         ASSIGN ttWordWrap.LineLabel = ttWordWrap.LineLabel + " " + chrWord.  
      END.
   END. /*DO intWordCount = 1 TO NUM-ENTRIES(chrString," "):*/
   
   FOR EACH ttWordWrap NO-LOCK:
       
      chrReturnString = chrReturnString + LineLabel + "|".
   END.
   
   chrReturnString = RIGHT-TRIM(chrReturnString,"|").
   
   RETURN chrReturnString.

END FUNCTION. /* fWordWrapLine */


FUNCTION fStripString RETURNS CHARACTER (INPUT chrString           AS CHARACTER,
                                         INPUT intStartingPosition AS INTEGER,
                                         INPUT intNumCharacters    AS INTEGER):
   
   DEFINE VARIABLE chrPreString  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE chrPostString AS CHARACTER NO-UNDO.
   
   chrPreString  = SUBSTRING(chrString,1,intStartingPosition - 1).
   chrPostString = SUBSTRING(chrString,intStartingPosition + intNumCharacters).

   RETURN chrPreString + chrPostString.

END FUNCTION. /* fStringString */


/* Validates that a String only contains a Numeric value */
FUNCTION fValidateIsNumber RETURNS LOGICAL (INPUT chrInputString AS CHARACTER):
         
   DEFINE VARIABLE intValidInteger AS INTEGER NO-UNDO.
   
   DO WHILE LENGTH(chrInputString) <> 0:
      
      intValidInteger = INT(SUBSTRING(chrInputString, 1, 1)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN                                                               
      DO:
         RETURN NO.
      END.
      
      chrInputString = SUBSTRING(chrInputString, 2).   
   END.    
   
   RETURN YES.
END.


FUNCTION fGetDelimitedEntry RETURNS CHARACTER(INPUT  intDelimitedEntryNumber AS INTEGER,
                                              INPUT  chrDelimiter            AS CHARACTER,
                                              INPUT  chrFullString           AS CHARACTER):
   
   DEFINE VARIABLE intEntryStartingPosition AS INTEGER   EXTENT 10.
   DEFINE VARIABLE intEntryEndingPosition   AS INTEGER   EXTENT 10.
   DEFINE VARIABLE intEntriesArray          AS CHARACTER EXTENT 10.
   DEFINE VARIABLE chrStrippedString        AS CHARACTER.
   DEFINE VARIABLE intCount                 AS INTEGER.
   DEFINE VARIABLE intDelimiterLength       AS INTEGER.
   
   intDelimiterLength = LENGTH(chrDelimiter).
   
   IF INDEX(chrFullString, chrDelimiter) <= 0 THEN
      RETURN "Error: No instance of Delimiter:" +  chrDelimiter + " in String:" + chrFullString + ".".
   
   chrStrippedString = chrFullString.
   
   CountLoop:
   DO intCount = 1 TO 9:
      
      /* MESSAGE "Count:" + STRING(intCount) + " String:" + chrStrippedString VIEW-AS ALERT-BOX. */
      
      /* Sets the entry from the start of the stripped string to where we find the delimiter */
      intEntriesArray[intCount] = SUBSTRING(chrStrippedString, 1, (INDEX(chrStrippedString, chrDelimiter) - 1)).
      
      /* If it's the one they want then the journey ends here */
      IF intCount = intDelimitedEntryNumber THEN
         RETURN intEntriesArray[intCount].
      
      /* Strip this entry and the delimiter off the beginning of the String and leave what is remaining */
      chrStrippedString = SUBSTRING(chrStrippedString, INDEX(chrStrippedString, chrDelimiter) + intDelimiterLength).
      
      /* If there are no more Delimiters remaining in the String see if it's the last one they're looking for - if not return error */
      IF INDEX(chrStrippedString, chrDelimiter) <= 0 THEN
      DO:
         intEntriesArray[intCount + 1] = chrStrippedString .
         
         IF intCount + 1 = intDelimitedEntryNumber THEN
            RETURN intEntriesArray[intCount + 1].
         
         RETURN "Error: No Entry:" + STRING(intDelimitedEntryNumber) + " of Delimiter:" +  chrDelimiter + " in String:" + chrFullString + ".".
      END.
      
   END. /*DO intCount = 1 TO 9: */
   
END FUNCTION. /*fGetDelimitedEntry */

     

