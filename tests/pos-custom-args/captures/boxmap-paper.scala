import caps.any

type Cell_orig[+T] = [K] -> (T => K) -> K

def cell_orig[T](x: T): Cell_orig[T] =
  [K] => (k: T => K) => k(x)

class Cell[+T](val value: [K] -> (T => K) -> K):
  def apply[K]: (T => K) -> K = value[K]

def cell[T](x: T): Cell[T] = Cell:
  [K] => (k: T => K) => k(x)

def get[T](c: Cell[T]): T = c[T](identity)

def map[A, B](c: Cell[A])(f: A => B): Cell[B]
  = c[Cell[B]]((x: A) => cell(f(x)))

def pureMap[A, B](c: Cell[A])(f: A -> B): Cell[B]
  = c[Cell[B]]((x: A) => cell(f(x)))

def lazyMap[A, B](c: Cell[A])(f: A ->{any.rd} B): () ->{f} Cell[B]
  = () => c[Cell[B]]((x: A) => cell(f(x)))

trait IO:
  def print(s: String): Unit

def test(io: IO^{any.rd}) =

  val loggedOne: () ->{io} Int = () => { io.print("1"); 1 }

  // We have a leakage of io because type arguments to alias type `Cell` are not boxed.
  val c_orig: Cell[() ->{io} Int]^{io}
      = cell[() ->{io} Int](loggedOne)

  val c: Cell[() ->{io} Int]
      = cell[() ->{io} Int](loggedOne)

  val g = (f: () ->{io} Int) =>
    val x = f(); io.print(" + ")
    val y = f(); io.print(s" = ${x + y}")

  val r = lazyMap[() ->{io} Int, Unit](c)(f => g(f))
  val r2 = lazyMap[() ->{io} Int, Unit](c)(g)
  val r3 = lazyMap(c)(g)
  val _ = r()
  val _ = r2()
  val _ = r3()
