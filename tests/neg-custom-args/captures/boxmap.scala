type Top = Any @retains(*)

infix type ==> [A, B] = (A => B) @retains(*)

type Box[+T <: Top] = ([K <: Top] => (T ==> K) => K)

def box[T <: Top](x: T): Box[T] =
  [K <: Top] => (k: T ==> K) => k(x)

def map[A <: Top, B <: Top](b: Box[A])(f: A ==> B): Box[B] =
  b[Box[B]]((x: A) => box(f(x)))

def lazymap[A <: Top, B <: Top](b: Box[A])(f: A ==> B): () => Box[B] =
  () => b[Box[B]]((x: A) => box(f(x)))  // error
