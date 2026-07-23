// Regression test for scala/bug#10393: the compiler must not prove `Nothing =:= Unit`.

object Test:
  def isUnit[A](code: => A)(using ev: A =:= Unit = null) =
    ev != null

  def main(args: Array[String]): Unit =
    assert(isUnit(()), "isUnit(()) should be true")
    assert(!isUnit(???), "isUnit(???) should be false: Nothing =:= Unit must not be proven")
end Test
