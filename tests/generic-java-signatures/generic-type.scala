// Used to crash the compiler while emitting the generic signature

class Outer[X, N]:
  class Inner

type X[A] = Outer[A, String]

object Test:
  val x: X[String] = null
  def foo(): x.Inner = null

  def main(args: Array[String]): Unit = ()
