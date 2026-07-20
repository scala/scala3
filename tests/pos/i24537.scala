import scala.language.implicitConversions

object iron:
  opaque type IronType[A, C] <: A = A
  type :|[A, C] = IronType[A, C]

  final class Implication[C1, C2]
  type ==>[C1, C2] = Implication[C1, C2]
  given [C1, C2](using C1 <:< C2): (C1 ==> C2) = Implication()

  implicit inline def autoCastIron[A, C1, C2](inline value: A :| C1)(using C1 ==> C2): A :| C2 =
    value.asInstanceOf
  implicit inline def autoFactorize[A, I[_] <: Iterable[?], C1, C2](inline iterable: I[A :| C1])(using C1 ==> C2): I[A] :| C2 =
    iterable.asInstanceOf
import iron.*

trait Path
trait Stream[F[_], A]:
  def evalMapFilter[F2[_], O2](f: A => F2[O2]): Stream[F2, O2]

final class A
final class D
type AD = D & A

extension (p: Path)
  def asD[F[_]]: F[Path :| D] = ???

extension (p: Path :| D)
  def ls[F[_]]: Stream[F, Path] = ???

extension (p: Path :| AD)
  def lsDA[F[_]]: Stream[F, Path :| AD] = p.ls.evalMapFilter(_.asD)