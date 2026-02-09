import caps.any


def seq1(x: () => Unit, y: () ->{x, any} Unit): Unit =
  x(); y()

def seq2(x: () => Unit)(y: () ->{x, any} Unit): Unit =
  x(); y()

def test(io: Object^, a: Object^{io}): Unit =
  val f = () => println(a)
  val g = () => println(a)
  seq1(f, f)
  seq2(f)(f)
  seq1(g, g)
  seq2(g)(g)

  seq1(f, g)
  seq2(f)(g)
  seq1(g, f)
  seq2(g)(f)