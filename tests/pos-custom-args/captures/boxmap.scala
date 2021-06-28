type Top = Any retains *
class Cap extends Retains[*]

infix type ==> [A, B] = (A => B) retains *

type Box[+T <: Top] = ([K <: Top] => (T ==> K) => K) retains T

def box[T <: Top](x: T): Box[T] =
  [K <: Top] => (k: T ==> K) => k(x)

def map[A <: Top, B <: Top](b: Box[A])(f: A ==> B): Box[B] =
  b[Box[B]]((x: A) => box(f(x)))

def lazymap[A <: Top, B <: Top](b: Box[A])(f: A ==> B): (() => Box[B]) retains b.type | f.type =
  () => b[Box[B]]((x: A) => box(f(x)))

def test[A <: Top, B <: Top] =
  def lazymap[A <: Top, B <: Top](b: Box[A])(f: A ==> B) =
    () => b[Box[B]]((x: A) => box(f(x)))
  val x: (b: Box[A]) => ((f: A ==> B) => (() => Box[B]) retains b.type | f.type) retains b.type = lazymap[A, B]
  ()
