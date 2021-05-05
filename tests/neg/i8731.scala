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
  end if

  class Test {
    val test = 3
  end Test
  }