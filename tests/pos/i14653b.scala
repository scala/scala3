type Amount = Amount.Type
object Amount:
  opaque type Type = Int
  inline def twice(x: Type): Type = x + x

def a: Amount = ???
def b: Amount.Type = ???
def test(c: Amount, d: Amount.Type): Unit =
   Amount.twice(a)
   Amount.twice(b)
   Amount.twice(c)
   Amount.twice(d)
