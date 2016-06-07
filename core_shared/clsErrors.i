DO:
   chrErrors = chrErrors + "{&Err}".
   DO intError = 1 TO ERROR-STATUS:NUM-MESSAGES:
      chrErrors = chrErrors + ERROR-STATUS:GET-MESSAGE(intError) + ". ".
   END.
   LEAVE {2}.
END.
