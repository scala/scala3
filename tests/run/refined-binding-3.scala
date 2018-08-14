
sealed trait Foo {

  type X
  type Y <: X

  def x: X

  def f(y: Y) = println("ok")

  object Z {
    def unapply(arg: X) = new Opt {
      type Scrutinee = arg.type
      def refinedScrutinee: Y & Scrutinee = {
        println("refinedScrutinee")
        arg.asInstanceOf[Y & Scrutinee]
      }
    }
  }

  abstract class Opt {
    type Scrutinee <: Singleton
    def refinedScrutinee: Y & Scrutinee
    def get: Int = 9
    def isEmpty: Boolean = false
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    test(new Foo { type X = Int; type Y = Int; def x: X = 1 })
  }

  def test(foo: Foo): Unit = {
    foo.x match {
      case x @ foo.Z(i) => // `x` is refined to type `Y`
        foo.f(x)
        println(i)
    }
  }
}
