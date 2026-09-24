// Higher-kinded abstract type whose upper bound is itself a HKTypeLambda
// must not be naively intersected (via AndType) with the value-type alias.
// Reproduces the lucuma-itc/lucuma-odb regression where the inliner produced
// `cats.data.NonEmptyChainImpl.Type[$proxy.Wrap] & [A] =>> Base & Tag`.

trait NewType:
  type Base
  type Tag
  type Type[+A] <: Base & Tag

object Impl extends NewType:
  def create[A](a: A): Type[A] = a.asInstanceOf[Type[A]]

object Mod:
  opaque type Wrap = String
  def wrap(s: String): Wrap = s
  // Inline def lives inside an object that `contains opaques`, so the inliner
  // creates a proxy for `Mod` when widening the call's value-arg type.
  inline def consume[A](xs: Impl.Type[A]): String = xs.toString

object Test:
  val w: Mod.Wrap = Mod.wrap("hi")
  val xs: Impl.Type[Mod.Wrap] = Impl.create[Mod.Wrap](w)
  val s: String = Mod.consume(xs)
