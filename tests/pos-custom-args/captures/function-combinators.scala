class ContextClass
type Context = ContextClass^

def Test(using ctx1: Context, ctx2: Context) =
  val f: Int ->{cap[Test]} Int = identity
  val g1: Int ->{ctx1} Int = identity
  val g2: Int ->{ctx2} Int = identity
  val h: Int -> Int = identity
  val a1 = f.andThen(f); val _: Int ->{f} Int = a1
  val a2 = f.andThen(g1); val _: Int ->{f, g1} Int = a2
  val a3 = f.andThen(g2); val _: Int ->{f, g2} Int = a3
  val a4 = f.andThen(h); val _: Int ->{f} Int = a4
  val b1 = g1.andThen(f); val _: Int ->{f, g1} Int = b1
  val b2 = g1.andThen(g1); val _: Int ->{g1} Int = b2
  val b3 = g1.andThen(g2); val _: Int ->{g1, g2} Int = b3
  val b4 = g1.andThen(h); val _: Int ->{g1} Int = b4
  val c1 = h.andThen(f); val _: Int ->{f} Int = c1
  val c2 = h.andThen(g1); val _: Int ->{g1} Int = c2
  val c3 = h.andThen(g2); val _: Int ->{g2} Int = c3
  val c4 = h.andThen(h); val _: Int -> Int = c4

  val f2: (Int, Int) ->{cap[Test]} Int = _ + _
  val f2c = f2.curried; val _: Int -> Int ->{f2} Int = f2c
  val f2t = f2.tupled; val _: ((Int, Int)) ->{f2} Int = f2t

  val f3: (Int, Int, Int) => Int = ???
  val f3c = f3.curried; val _: Int -> Int -> Int ->{f3} Int = f3c
  val f3t = f3.tupled; val _: ((Int, Int, Int)) ->{f3} Int = f3t
