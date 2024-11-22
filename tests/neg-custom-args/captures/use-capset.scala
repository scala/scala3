import caps.{use, CapSet}



def f[C^](@use xs: List[Object^{C^}]): Unit = ???

private def g[C^] = (xs: List[Object^{C^}]) => xs.head // error

private def g2[@use C^] = (xs: List[Object^{C^}]) => xs.head // ok

def test(io: Object^)(@use xs: List[Object^{io}]): Unit =
  val h = () => f(xs)
  val _: () -> Unit = h // error: should be ->{io}
  val h2 = () => g[CapSet^{io}]
  val _: () -> List[Object^{io}] -> Object^{io} = h2 // error, should be ->{io}

