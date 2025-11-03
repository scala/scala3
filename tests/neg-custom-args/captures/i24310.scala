import caps.*

class Ref extends Mutable:
  private var value: Int = 0
  def get(): Int = value
  update def set(v: Int): Unit = value = v

class Matrix(val f: () => Int) extends Mutable:
  self: Matrix^ =>
    def run() = f() // error <- note the missing update
    update def add(): Unit = ()


@main def test =
  val r: Ref^ = Ref()
  val m: Matrix^ = Matrix(() => 42)
  val m2: Matrix^ = Matrix(() => m.run())
  val m3: Matrix^ = Matrix(() => r.get())

  def par(f1: () => Int, f2: () => Int): Unit =
    println(s"par results: ${f1()} and ${f2()}")

  def g(m: Matrix^): Unit =
    par(m.run, m.run) // <- should be rejected

  g(m2) // ok
  g(m3) // ok
