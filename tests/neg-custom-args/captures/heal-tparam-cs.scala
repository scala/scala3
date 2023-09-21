import language.experimental.captureChecking

trait Cap { def use(): Unit }

def localCap[T](op: (lcap: caps.Cap) ?-> (c: Cap^{lcap}) => T): T = ???

def main(io: Cap^{cap}, net: Cap^{cap}): Unit = {

  val test1 = localCap { c => // error
    () => { c.use() }
  }

  val test2: (c: Cap^{cap}) -> () ->{cap} Unit =
    localCap { c =>  // error, was: should work
      (c1: Cap^{cap}) => () => { c1.use() }
    }

  val test3: (c: Cap^{io}) -> () ->{io} Unit =
    localCap { c =>  // should work
      (c1: Cap^{io}) => () => { c1.use() }
    }

  val test4: (c: Cap^{io}) -> () ->{net} Unit =
    localCap { c =>  // error
      (c1: Cap^{io}) => () => { c1.use() }
    }

  def localCap2[T](op: (c: Cap^{io}) => T): T = ???

  val test5: () ->{io} Unit =
    localCap2 { c =>  // ok
      () => { c.use() }
    }
}
