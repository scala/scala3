enum SUB[-A, +B]:
  case Refl[S]() extends SUB[S, S]

trait R {
  type Data
}
trait L extends R

def f(x: L): x.Data = ???

def g[T <: R](x: T, ev: T SUB L): x.Data = ev match
  case SUB.Refl() =>
    f(x)
