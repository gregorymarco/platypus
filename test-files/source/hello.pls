!! Simple PLATYPUS test program
PLATYPUS{
  a = 10;
  b = 20;
  c = a + b;
  WRITE("Hello from PLATYPUS!");
  WRITE("a + b = ");
  WRITE(c);
  
  IF TRUE(a < b) THEN {
    WRITE("a is less than b");
  } ELSE {
    WRITE("a is not less than b");
  };
}


