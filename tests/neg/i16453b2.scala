import scala.language.implicitConversions

sealed trait Ctx
given ct[T]: Conversion[Ctx => T, Ctx => Option[T]] = fn => fn.andThen(Option.apply)

def get[T](using fn: Ctx => Option[T]): Option[T] = ???

def Test = {
  given foo2[A]: (Ctx => Int) = _ => 42
  val ok = get[Int](using summon[Ctx => Int])
  val ko = get[Int] // error
}
