import language.experimental.captureChecking

trait Cap { def use(): Unit }

def localCap[sealed T](op: (cap: Cap^{cap}) => T): T = ???

def main(io: Cap^{cap}, net: Cap^{cap}): Unit = {
  val test1 = localCap { cap => // error
    () => { cap.use() }
  }

  val test2: (cap: Cap^{cap}) -> () ->{cap} Unit =
    localCap { cap =>  // should work
      (cap1: Cap^{cap}) => () => { cap1.use() }
    }

  val test3: (cap: Cap^{io}) -> () ->{io} Unit =
    localCap { cap =>  // should work
      (cap1: Cap^{io}) => () => { cap1.use() }
    }

  val test4: (cap: Cap^{io}) -> () ->{net} Unit =
    localCap { cap =>  // error
      (cap1: Cap^{io}) => () => { cap1.use() }
    }

  def localCap2[sealed T](op: (cap: Cap^{io}) => T): T = ???

  val test5: () ->{io} Unit =
    localCap2 { cap =>  // ok
      () => { cap.use() }
    }
}
