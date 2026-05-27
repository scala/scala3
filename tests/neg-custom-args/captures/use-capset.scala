import caps.{use, CapSet}

def f[C^](xs: List[Object^{C}]): Unit = ???

private def g[C^] = (xs: List[Object^{C}]) => xs.head // error TODO: allow this
  // This fails currently since `C^` is not classified as used since it does
  // not appear in the deep capture set of a term parameter of `g`. See CaptureOps.isUseParam.
  // To classify it as used we'd also have to look at the RHS. But this would change again
  // if we go to non-monotonic currying.

private def g2[C^](xs: List[Object^{C}]) = xs.head // ok

def test(io: Object^)(xs: List[Object^{io}]): Unit =
  val h = () => f(xs)
  val _: () -> Unit = h // error: should be ->{io}
  val h2 = () => g[{io}]
  val _: () -> List[Object^{io}] -> Object^{io} = h2 // error, should be ->{io}

