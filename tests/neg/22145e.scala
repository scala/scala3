package foo

class C[T]:
  object Ext:
    extension (x: T) def f(): Int = 1

object O1 extends C[String]
object O2 extends C[Int]

def main =
  2.f() // error
