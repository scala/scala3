extension (tuple: Tuple)
  infix def **:[T >: tuple.type <: Tuple, H](x: H): H *: T = ???

def test1: (Int, String, Char) = 1 **: ("a", 'b')
def test2: (Int, String, Char) = ("a", 'b').**:(1)
def test3: (Int, String, Char) = **:("a", 'b')(1)
