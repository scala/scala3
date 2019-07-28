import scala.compiletime.memo
trait Foo[A]


trait Qux {
  private[this] var x: Int | Null = null
  def f = {
    if (x == null) x = 22
    x.asInstanceOf[Int]
  }
  def g = memo(new Foo[Int] {})
//
  //given as Foo[Int] = new Foo[Int] {}
}