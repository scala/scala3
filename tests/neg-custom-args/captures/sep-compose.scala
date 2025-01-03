import caps.cap

def seq1(x: () => Unit, y: () ->{x, cap} Unit): Unit =
  x(); y()

def seq2(x: () => Unit)(y: () ->{x, cap} Unit): Unit =
  x(); y()

def seq5(x: () ->{cap} Unit)(y: () => Unit): Unit =
  x(); y()

def test(io: Object^, a: Object^{io}): Unit =

  def seq3(x: () => Unit)(y: () ->{a, cap} Unit): Unit =
    x(); y()

  def seq4(x: () ->{a, cap} Unit)(y: () => Unit): Unit =
    x(); y()

  def seq6(x: () => Unit, y: () ->{a, cap} Unit): Unit =
    x(); y()

  def seq7(x: () ->{a, cap} Unit, y: () => Unit): Unit =
    x(); y()

  val f = () => println(a)
  seq1(f, f) // ok
  seq2(f)(f) // ok
  seq3(f)(f) // error
  seq4(f)(f) // error
  seq5(f)(f) // error
  seq6(f, f) // error
  seq7(f, f) // error

