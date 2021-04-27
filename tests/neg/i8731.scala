object test:

  3 match
  case 3 => ???
  end match
  case _ => () // error: expected start of definition
  end match

  if 3 == 3 then
    ()
  end if
  else     // error: illegal start of definition
    ()
  end if   // error: misaligned end marker

  class Test {
    val test = 3
  end Test // error: misaligned end marker
  }  // error: eof expected, but unindent found