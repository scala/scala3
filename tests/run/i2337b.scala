/* 
Minimized from collection strawman 
This issue has a lot to do with both mixin and bridge generation and subtleties in JVM spec
if something breaks in this test, this is not a minor issue. Be careful. Here be dragons.
*/

trait Define[A] {
  protected def coll: Define[A]
  def s = coll
}

trait Iterable[A] extends Define[A] {
  protected def coll: this.type = this
}

trait Seq[A] extends Iterable[A]

trait Super1[A] {
  protected def coll: Iterable[A]
}

trait Super2[A] extends Super1[A] {
  override protected def coll: Seq[A]
  def bar = coll
}

class Foo[T] extends Seq[T] with Super2[T] {
}

object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo[Int]
    foo.s
    val su2: Super2[Int] = foo
    su2.bar
  }
}
