infix type ==> [A, B] = {*} (A => B)

type Cell[+T] = [K] => (T ==> K) => K

def cell[T](x: T): Cell[T] =
  [K] => (k: T ==> K) => k(x)

def get[T](c: Cell[T]): T = c[T](identity)

def map[A, B](c: Cell[A])(f: A ==> B): Cell[B]
  = c[Cell[B]]((x: A) => cell(f(x)))

def pureMap[A, B](c: Cell[A])(f: A => B): Cell[B]
  = c[Cell[B]]((x: A) => cell(f(x)))

def lazyMap[A, B](c: Cell[A])(f: A ==> B): {f} () => Cell[B]
  = () => c[Cell[B]]((x: A) => cell(f(x)))

trait IO:
  def print(s: String): Unit

def test(io: {*} IO) =

  val loggedOne: {io} () => Int = () => { io.print("1"); 1 }

  val c: Cell[{io} () => Int]
      = cell[{io} () => Int](loggedOne)

  val g = (f: {io} () => Int) =>
    val x = f(); io.print(" + ")
    val y = f(); io.print(s" = ${x + y}")

  val r = lazyMap[{io} () => Int, Unit](c)(f => g(f))
  val r2 = lazyMap[{io} () => Int, Unit](c)(g)
  val r3 = lazyMap(c)(g)
  val _ = r()
  val _ = r2()
  val _ = r3()
