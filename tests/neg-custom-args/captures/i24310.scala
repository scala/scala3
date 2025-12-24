import caps.*

class Ref extends Mutable:
  private var value: Int = 0
  def get(): Int = value
  update def set(v: Int): Unit = value = v

class Matrix(val f: () => Int) extends Stateful:
  self: Matrix^ =>
    def run() = f() // error <- note the missing update
    update def add(): Unit = ()


def test(consume proc: () => Int) =
  val r: Ref^ = Ref()
  val m: Matrix^ = Matrix(() => 42)
  val m2: Matrix^ = Matrix(() => m.run())
  val m3: Matrix^ = Matrix(() => r.get())
  val m4: Matrix^ = Matrix(proc)

  def par(f1: () => Int, f2: () => Int): Unit =
    println(s"par results: ${f1()} and ${f2()}")

  def g(m: Matrix^): Unit =
    par(m.run, m.run) // <- should be rejected

  g(m2) // ok
  g(m3) // ok
