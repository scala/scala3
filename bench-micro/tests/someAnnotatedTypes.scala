class Test:
  class FlagAnnot extends annotation.StaticAnnotation
  class StringAnnot(val s: String) extends annotation.StaticAnnotation
  class LambdaAnnot(val f: Int => Boolean) extends annotation.StaticAnnotation

  type SpecialInt <: Int

  val v1: Int @FlagAnnot = 42

  val v2: Int @StringAnnot("hello") = 42

  val v3: Int @LambdaAnnot(it => it == 42) = 42

  val v4: Int @LambdaAnnot(it => {
    def g(x: Int, y: Int) = x - y + 5
    g(it, 7) * 2 == 80
  }) = 42

  /*val v5: Int @LambdaAnnot(it => {
    class Foo(x: Int):
      def xPlus10 = x + 10
      def xPlus20 = x + 20
      def xPlus(y: Int) = x + y
    val foo = Foo(it)
    foo.xPlus10 - foo.xPlus20 + foo.xPlus(30) == 62
  }) = 42*/

  def main(args: Array[String]): Unit = ???
