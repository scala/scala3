class IO

def f[c^](xs: List[() ->{c} Unit]): () ->{c} Unit = () =>
  println(xs.head) // was error, ok with captset vars

def test(io: IO^)(ys: List[() ->{io} Unit]) =
  val x = () =>
    val z = f(ys)
    z()
  val _: () -> Unit = x // error
  ()

def test(io: IO^) =
  def ys: List[() ->{io} Unit] = ???
  val x = () =>
    val z = f(ys)
    z()
  val _: () -> Unit = x // error
  ()



