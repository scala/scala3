object Num {
  opaque type One = 1
  inline val One: One = 1 // error

  opaque type Two = 2
  inline def Two: Two = 2
}

def test1 = Num.One
def test2 = Num.Two
