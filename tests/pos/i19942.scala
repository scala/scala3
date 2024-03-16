type LifecycleF = [_] =>> Any
trait Lifecycle[+F[_], +A]

trait LifecycleTag[R]
object LifecycleTag:
  implicit def resourceTag[R <: Lifecycle[F0, A0], F0[_], A0]: LifecycleTag[R] = ???

trait MakeDSL[T]:
  final def fromResource[R <: Lifecycle[LifecycleF, T]](implicit tag: LifecycleTag[R]): Any = ???

object distage:
  // Cannot be defined in the same scope as rest of code
  final type Identity[+A] = A
import distage.*

trait Resource
trait DependentResource() extends Lifecycle[Identity, Resource]

@main def Test =  {
  val dsl: MakeDSL[Resource] = ???
  val fails = dsl.fromResource[DependentResource]
  val works = dsl.fromResource[DependentResource](using LifecycleTag.resourceTag[DependentResource, Identity, Resource])
}
