class IO

def f(xs: List[() => Unit]): () ->{xs*} Unit = () =>
  println(xs.head) // error

def test(io: IO^)(ys: List[() ->{io} Unit]) =
  val x = () =>
    val z = f(ys)
    z()
  val _: () -> Unit = x // !!! ys* gets lost
  ()

def test(io: IO^) =
  def ys: List[() ->{io} Unit] = ???
  val x = () =>
    val z = f(ys)
    z()
  val _: () -> Unit = x // !!! io gets lost
  ()



