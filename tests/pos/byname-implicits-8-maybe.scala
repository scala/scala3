//> using options -Yexplicit-nulls
// shapeless's Lazy implemented in terms of byname implicits
import language.experimental.magic
import scala.magic.*
trait Lazy[T] {
  lazy val value: T
}

object Lazy {
  implicit def apply[T](implicit t: => T): Lazy[T] =
    new Lazy[T] {
      lazy val value = t
    }

  def unapply[T](lt: Lazy[T]): T? = Ok(lt.value)
}

trait Foo {
  type Out
  def out: Out
}

object Foo {
  type Aux[Out0] = Foo { type Out = Out0 }

  implicit val fooInt: Aux[Int] = new Foo { type Out = Int ; def out = 23 }
}

object Test {
  def bar[T](t: T)(implicit foo: Lazy[Foo.Aux[T]]): T = foo.value.out

  val i = bar(13)
  i: Int
}
