//> using options -Werror -explain

def test =
  451.synchronized {} // error

def test2 =
  val x: Integer = 451
  x.synchronized {} // error

def test3 =
  true.synchronized {} // error

def test4 =
  true.hashCode() // success
