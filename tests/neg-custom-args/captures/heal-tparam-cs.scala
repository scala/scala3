import language.experimental.captureChecking

trait Cap { def use(): Unit }

def localCap[T](op: (cap: {*} Cap) => T): T = ???

def main(io: {*} Cap, net: {*} Cap): Unit = {
  val test1 = localCap { cap => // error
    () => { cap.use() }
  }

  val test2: (cap: {*} Cap) -> {cap} () -> Unit =
    localCap { cap =>  // should work
      (cap1: {*} Cap) => () => { cap1.use() }
    }

  val test3: (cap: {io} Cap) -> {io} () -> Unit =
    localCap { cap =>  // should work
      (cap1: {io} Cap) => () => { cap1.use() }
    }

  val test4: (cap: {io} Cap) -> {net} () -> Unit =
    localCap { cap =>  // error
      (cap1: {io} Cap) => () => { cap1.use() }
    }

  def localCap2[T](op: (cap: {io} Cap) => T): T = ???

  val test5: {io} () -> Unit =
    localCap2 { cap =>  // ok
      () => { cap.use() }
    }
}
