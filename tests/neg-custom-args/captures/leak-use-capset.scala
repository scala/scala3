import caps.{use, CapSet}

def f[C^](xs: List[Object^{C}]): Unit = ???

private def g[C^] = (xs: List[Object^{C}]) => xs.head // error TODO: allow this
  // This fails currently since `C^` is not classified as used since it does
  // not appear in the deep capture set of a term parameter of `g`. See CaptureOps.isUseParam.
  // To classify it as used we'd also have to look at the RHS. But this would change again
  // if we go to non-monotonic currying.

