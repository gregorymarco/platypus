!! String concatenation test
PLATYPUS{
  a$ = "Hello";
  b$ = " ";
  c$ = "World";
  d$ = "!";
  
  !! Simple concatenation
  msg$ = a$ # b$;
  WRITE(msg$);
  
  !! Multi-part concatenation
  full$ = a$ # b$ # c$ # d$;
  WRITE(full$);
  
  !! Mixed literals and variables
  greet$ = "Say: " # a$ # b$ # c$;
  WRITE(greet$);
}


