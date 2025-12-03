!! More error tests
PLATYPUS{
  a = 10;
  IF TRUE(a > 5) THEN {
    b = 20;
  } ELSE {
    c = 30;
  }
  !! Missing semicolon after IF
  d = 40;
}


