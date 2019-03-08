trait Foo[A] {
  def op(x: A): Int
}
trait Bar[A] extends Foo[A] {
  override def op(x: A): Int = 1
}
trait Baz[A] extends Foo[A] {
  override def op(x: A): Int = 2
}
trait Qux[A] extends Bar[A] with Baz[A]

class QInt extends Qux[Int]

object Test {
  def main(args: Array[String]): Unit = {
    val qint = new QInt
    assert(qint.op(1) == 2) // Used to fail with java.lang.IncompatibleClassChangeError: Conflicting default methods: Bar.op Baz.op
  }
}
