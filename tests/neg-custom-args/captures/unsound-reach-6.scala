
import caps.consume

class IO

def f(@consume xs: List[() => Unit]): () => Unit = () =>
  println(xs.head) // error

def test(io: IO^)(ys: List[() ->{io} Unit]) =
  val x = () =>
    val z = f(ys)   // error @consume failure
    z()
  val _: () -> Unit = x // !!! ys* gets lost
  ()

def test(io: IO^) =
  def ys: List[() ->{io} Unit] = ???
  val x = () =>
    val z = f(ys)  // error @consume failure
    z()
  val _: () -> Unit = x // !!! io gets lost
  ()



