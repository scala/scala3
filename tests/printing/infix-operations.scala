class C(a: Int):
  def foo(b: Int): C = this
  def +(b: Int): C = this

@main def main =
  val v1 = 1 + 2 + 3
  val v2 = 1 + (2 + 3)
  val v3 = 1 + 2 * 3
  val v4 = (1 + 2) * 3
  val v5 = (1 + 2):Int
  val v6 = 1 + 2:Int // same as above
  val v7 = (1 < 2):Boolean
  val v8 = 1 < 2:Boolean // same as above

  val c = new C(2) // must not be printed in infix form
  val v9 = c.foo(3) // must not be printed in infix form
  val v10 = c + 3
