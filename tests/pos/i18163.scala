import scala.language.implicitConversions

trait Foo[A]
trait Bar[B]
trait Qux[C]
class Log[K[_]]

trait Inv[F[_]]
object Inv:
  given monFoo: Inv[Foo] = ???
  given monBar: Inv[Bar] = ???

trait InvOps[H[_], D] { def desc(s: String): H[D] = ??? }
trait LogOps[L[_]]    { def desc(s: String): Log[L] = ??? }

class Test:
  implicit def LogOps[Q[_]](l: Log[Q]): LogOps[Q] = ???
  implicit def InvOps[J[_], E](j11: J[E])(implicit z: Inv[J]): InvOps[J, E] = ???

  def fails = new Log[Qux].desc("fails")
  def works = LogOps[Qux](new Log[Qux]).desc("works")
