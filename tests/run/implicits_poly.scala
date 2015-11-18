class Foo[A](val x: String)
class Bar[A](x: String) extends Foo[A](x)

object Test {
  implicit def anyRefFoo[A <: AnyRef]: Foo[A] = new Foo("anyRefFoo")
  implicit def fooFoo[A]: Foo[Foo[A]] = new Foo("fooFoo")
  implicit def barFoo[A]: Bar[Foo[A]] = new Bar("barFoo")

  def getFooFoo(implicit ev: Foo[Foo[Int]]) = ev

  def main(args: Array[String]): Unit = {
    println(getFooFoo.x)
  }
}
