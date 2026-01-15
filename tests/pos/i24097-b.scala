trait <[+A, -S]

trait TypeMap[+A]:
  def union[B](that: TypeMap[B]): TypeMap[A & B] = ???

trait Tag[E]
trait ContextEffect[+A]
object ContextEffect:
  def handle[A, E <: ContextEffect[A], B, S](effectTag: Tag[E], ifDefined: A => A)(
      v: B < (E & S)
  ): B < S = ???

sealed trait Env[+R] extends ContextEffect[TypeMap[R]]
object Env:
  def runAll[R >: Nothing, A, S, VR](
      env: TypeMap[R],
      tag: Tag[Env[R]]
  )(
      v: A < (Env[R & VR] & S)
  ) =
    ContextEffect.handle(
      tag,
      _.union(env)
    )(v): A < (Env[VR] & S)
    ???
