//> using options -source:3.3

trait Poly
trait -->[X, Y] extends Poly
trait +[X, Y] extends Poly
trait *[X, Y] extends Poly

type Hinze[X <: Fix[?]] = X#unfix match
  case (k1 + k2) --> v => Hinze[(k1 --> v)] * Hinze[(k2 --> v)]
  case (k1 * k2) --> v => k1 --> Hinze[(k2 --> v)]

trait Lambda[V]:
  type Abs[X] = V * X
  type App[X] = X * X
  type L[X] = V + Abs[X] + App[X]

trait Fix[F[X]]:
  type unfix = F[Fix[F]]

@main
def m =
  println(summon[((String --> Unit) * (String --> Unit)) =:= Hinze[(String + String) --> Unit]]) // error
  println(summon[String =:= Hinze[Fix[Lambda[String]#L] --> Unit]]) // error
