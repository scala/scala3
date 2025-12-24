import caps.use

def foo[C^](x: List[() ->{C} Unit]): Unit =
  println(x.head)

def bar[C^](x: List[() ->{C} Unit]): Unit = ()

def test(io: Object^) =
  val xs: List[() ->{io} Unit] = ???
  val f = () => foo[{io}](xs)
  val g = () => bar[{io}](xs)
