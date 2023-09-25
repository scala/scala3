import scala.language.implicitConversions

// We do have 2 `contramap` functions, one provided via `LoggerSyntax` other via `Contravariant.Ops`
// `ContravariantMonoidal` given instances are not used, and they do not match our type. Code fails when we have at least 2 instances of them
// Removal of `import catsSyntax._` allow to compile code
// Removal of `import odinSyntax.LoggerSyntax` and remaining `catsSyntax` would fail to compile the `def fails`

trait Foo[A]
trait Bar[A]

trait WriterT[F[_]: Contravariant, L, V]:
  def contramap[Z](fn: Z => V): WriterT[F, L, Z] = ???
trait Logger[F[_]]
class WriterTLogger[F[_]] extends Logger[[G] =>> WriterT[F, List[String], G]]

trait ContravariantMonoidal[F[_]] extends Invariant[F] with Contravariant[F]
trait Invariant[F[_]]
object Invariant:
  given ContravariantMonoidal[Foo] = ???
  given ContravariantMonoidal[Bar] = ???

trait Contravariant[F[_]] extends Invariant[F]
object Contravariant:
  trait Ops[F[_], A]:
    def contramap[B](f: B => A): F[B] = ???

object catsSyntax:
  implicit def toContravariantOps[F[_]: Contravariant, A](target: F[A]): Contravariant.Ops[F, A] = ???

object odinSyntax:
  implicit class LoggerSyntax[F[_]](logger: Logger[F]):
    def contramap(f: String => String): Logger[F] = ???

import catsSyntax._
import odinSyntax.LoggerSyntax

class Test:
  def fails = new WriterTLogger[Option].contramap(identity)
  def works = LoggerSyntax(new WriterTLogger[Option]).contramap(identity)

