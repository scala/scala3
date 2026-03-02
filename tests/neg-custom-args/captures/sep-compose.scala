import caps.any

def seq1(x: () => Unit, y: () ->{x, any} Unit): Unit =
  x(); y()

def seq2(x: () => Unit)(y: () ->{x, any} Unit): Unit =
  x(); y()

def seq5(x: () ->{any} Unit)(y: () => Unit): Unit =
  x(); y()

def test(io: Object^, a: Object^{io}): Unit =

  def seq3(x: () => Unit)(y: () ->{a, any} Unit): Unit =
    x(); y()

  def seq4(x: () ->{a, any} Unit)(y: () => Unit): Unit =
    x(); y()

  def seq6(x: () => Unit, y: () ->{a, any} Unit): Unit =
    x(); y()

  def seq7(x: () ->{a, any} Unit, y: () => Unit): Unit =
    x(); y()

  def seq8(x: () => Unit)(y: () ->{a} Unit): Unit =
    x(); y()

  val f = () => println(a)
  seq1(f, f) // ok
  seq2(f)(f) // ok
  seq3(f)(f) // error
  seq4(f)(f) // error
  seq5(f)(f) // error
  seq6(f, f) // error
  seq7(f, f) // error
  seq8(f)(f) // error

  val p1 = (x: () => Unit) => seq1(f, x)
  p1(f) // error
  val p8 = (x: () ->{a} Unit) => seq8(f)(x) // error
  p8(f)



