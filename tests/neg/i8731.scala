object test with

  3 match
  case 3 => ???
  end match
  case _ => () // error: missing parameter type
  end match

  if 3 == 3 then
    ()
  end if
  else     // error: illegal start of definition
    ()
  end if

  class Test {
    val test = 3
  end Test   // error: misaligned end marker
  }

  while
    3 == 3
  end while  // error: `do` expected
  do ()

  for
    a <- Seq()
  end for    // error: `yield` or `do` expected
  do ()