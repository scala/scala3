// Regression test for scala/scala3#24537
// The compiler used to hang in `TypeComparer.compareAppliedTypeParamRef` when
// resolving chained extension methods on an opaque refinement type combined
// with a higher-kinded lower-bounded type parameter, as exercised by the
// Iron library (`:|`) together with `fs2.Stream.evalMapFilter`.

import scala.language.implicitConversions

opaque type IronType[A, C] <: A = A
type :|[A, C] = IronType[A, C]

final class Implication[C1, C2]
type ==>[C1, C2] = Implication[C1, C2]
object Implication:
  given [C]: (C ==> C) = Implication()
  given [C1, C2](using C1 <:< C2): (C1 ==> C2) = Implication()

implicit inline def autoCastIron[A, C1, C2](inline v: A :| C1)(using C1 ==> C2): A :| C2 =
  v.asInstanceOf[A :| C2]

class Stream[+F[_], +O]:
  def evalMapFilter[F2[x] >: F[x], O2](f: O => F2[Option[O2]]): Stream[F2, O2] = ???

class Path
final class A
final class D
type AD = D & A

object FExt:
  extension (p: Path)
    def asD[F[_]]: F[Option[Path :| D]] = ???

  extension (p: Path :| D)
    def ls[F[_]]: Stream[F, Path] = ???

  // This must compile without the compiler hanging.
  extension (p: Path :| AD)
    def lsD[F[_]]: Stream[F, Path :| D] =
      p.ls.evalMapFilter(_.asD)
