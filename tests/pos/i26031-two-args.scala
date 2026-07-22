package i26031twoargs

trait Ctx[F[_]]

trait Mon[F[_]]:
  type Context <: Ctx[F]

object Mon:
  type Aux[F[_], C <: Ctx[F]] = Mon[F] { type Context = C }

class Both[F[_], C1 <: Ctx[F], C2 <: Ctx[F]](
    using val am1: Mon.Aux[F, C1],
    val am2: Mon.Aux[F, C2]
)

transparent inline def both[F[_]](using am1: Mon[F], am2: Mon[F]) =
  new Both(using am1, am2)

class Pair[A, B]
class PairCtx1[E] extends Ctx[[A] =>> Pair[E, A]]
class PairCtx2[E] extends Ctx[[A] =>> Pair[E, A]]

class PairMon1[E] extends Mon[[A] =>> Pair[E, A]]:
  type Context = PairCtx1[E]

class PairMon2[E] extends Mon[[A] =>> Pair[E, A]]:
  type Context = PairCtx2[E]

object Test:
  val am1: Mon[[A] =>> Pair[String, A]] = new PairMon1[String]
  val am2: Mon[[A] =>> Pair[String, A]] = new PairMon2[String]
  val r = both[[A] =>> Pair[String, A]](using am1, am2)
