def f[C^](xs: List[() ->{C} Unit]): List[() ->{C} Unit] =
  xs.reverse

def test(io: Object^, async: Object^): Unit =
  val ok = f[{io}](Nil)
  val x = f[{io}]        // was error
  val y = f[{io, async}] // was error
