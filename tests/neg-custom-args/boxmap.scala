import annotation.retains
type Top = Any @retains[caps.cap.type]

type Box[+T <: Top] = ([K <: Top] -> (T => K) -> K)

def box[T <: Top](x: T): Box[T] =
  [K <: Top] => (k: T => K) => k(x)

def map[A <: Top, B <: Top](b: Box[A])(f: A => B): Box[B] =
  b[Box[B]]((x: A) => box(f(x)))

def lazymap[A <: Top, B <: Top](b: Box[A])(f: A => B): {f} (() -> Box[B]) =
  () => b[Box[B]]((x: A) => box(f(x)))

def test[A <: Top, B <: Top] =
  def lazymap[A <: Top, B <: Top](b: Box[A])(f: A => B) =
    () => b[Box[B]]((x: A) => box(f(x)))
  val x0: (b: Box[A]) -> (f: A => B) -> (() -> Box[B]) = lazymap[A, B]  // error
  val x: (b: Box[A]) -> (f: A => B) -> (() ->{b, f} Box[B]) = lazymap[A, B]  // works
  val y: (b: Box[A]) -> (f: A => B) -> (() ->{cap} Box[B]) = lazymap[A, B]  // works
  ()
