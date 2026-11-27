// Polymorphic function literals whose body is cast to its expected type using GADT
// constraints. The cast has no observable effect, so the body can still be evaluated
// once for all type applications.
enum IsInt[T]:
  case Yes extends IsInt[Int]

enum IsFun[T]:
  case Yes extends IsFun[Int => Int]

def f[T](ev: IsInt[T]): [A] => T => T = ev match
  case IsInt.Yes => [A] => (x: Int) => x + 1 // cast to `T => T`

def g[T](ev: IsFun[T]): [A] => T = ev match
  case IsFun.Yes => [A] => (x: Int) => x + 1 // cast to `T`

trait Sam:
  def apply(x: Int): Int

enum IsSam[T]:
  case Yes extends IsSam[Sam]

def h[T <: Sam](ev: IsSam[T]): [A] => T = ev match
  case IsSam.Yes => [A] => (x: Int) => x + 1 // converted to `Sam`, cast to `T`

@main def Test =
  assert(f(IsInt.Yes)[String](1) == 2)
  assert(g(IsFun.Yes)[String](1) == 2)
  assert(h(IsSam.Yes)[String](1) == 2)
