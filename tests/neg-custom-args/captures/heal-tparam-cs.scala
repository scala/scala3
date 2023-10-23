import language.experimental.captureChecking

trait Capp { def use(): Unit }

def localCap[sealed T](op: (c: Capp^{cap}) => T): T = ???

def main(io: Capp^{cap}, net: Capp^{cap}): Unit = {

  val test1 = localCap { c => // error
    () => { c.use() }
  }

  val test2: (c: Capp^{cap}) -> () ->{cap} Unit =
    localCap { c =>  // should work
      (c1: Capp^{cap}) => () => { c1.use() }
    }

  val test3: (c: Capp^{io}) -> () ->{io} Unit =
    localCap { c =>  // should work
      (c1: Capp^{io}) => () => { c1.use() }
    }

  val test4: (c: Capp^{io}) -> () ->{net} Unit =
    localCap { c =>  // error
      (c1: Capp^{io}) => () => { c1.use() }
    }

  def localCap2[sealed T](op: (c: Capp^{io}) => T): T = ???

  val test5: () ->{io} Unit =
    localCap2 { c =>  // ok
      () => { c.use() }
    }
}
