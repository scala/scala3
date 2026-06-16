


class IO

def f(consume xs: List[() => Unit]): () => Unit = () =>
  println(xs.head) // error

def test(io: IO^)(ys: List[() ->{io} Unit]) =
  val x = () =>
    val z = f(ys)   // was error consume failure, now OK since ys* is no longer produced.
    z()



