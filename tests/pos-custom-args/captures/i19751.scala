import language.experimental.captureChecking
import caps.{any, Shared, SharedCapability}

trait Ptr[A]
trait Scope extends caps.SharedCapability:
  def allocate(size: Int): Ptr[Unit]^{this}

object Scope:
  def confined[A](fn: Scope ?->{any.only[Shared]} A): A =
    val scope = new Scope:
      def allocate(size: Int) = new Ptr[Unit] {}
    fn(using scope)

def Test: Unit =
  val s = Scope.confined:
    val s2 = summon[Scope]
    Scope.confined:
      s2.allocate(5)
    5

