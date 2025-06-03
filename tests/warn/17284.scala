//> using options  -explain

def test =
  451.synchronized {} // warn

def test2 =
  val x: Integer = 451
  x.synchronized {} // warn

def test3 =
  true.synchronized {} // warn

def test4 =
  true.hashCode() // success