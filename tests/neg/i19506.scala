//> using options "-source:3.4-migration",

trait Reader[T]
def read[T: Reader](s: String, trace: Boolean = false): T = ???

def Test =
  read[Object]("") // error
  read[Object]("")() // error
