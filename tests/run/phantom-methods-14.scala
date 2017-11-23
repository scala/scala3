import scala.reflect.ClassTag

object Test {

  def main(args: Array[String]): Unit = {
    bar1(Foo.a)
    bar2(Foo.a)(null)
  }

  def bar1(unused ev: Foo.A) = ()
  def bar2(unused ev: Foo.A)(implicit c: ClassTag[Int]) = implicitly[ClassTag[Int]]
}

object Foo extends Phantom {
  type A <: this.Any
  unused def a: A = assume
}
