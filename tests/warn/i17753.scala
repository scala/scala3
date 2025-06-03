//> using options -Wunused:all

class PartiallyApplied[A] {
  transparent inline def func[B](): Nothing = ???
}

def call[A] = new PartiallyApplied[A]

def good = call[Int].func[String]() // no warn inline proxy
def bad = { call[Int].func[String]() } // no warn inline proxy
