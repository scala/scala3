import language.experimental.captureChecking
//import language.`3.8`

trait Capp { def use(): Unit }

def localCap[T](op: (c: Capp^) => T): T = ???

def main(io: Capp^, net: Capp^): Unit = {

  val test1 = localCap { c => // error
    () => { c.use() }
  }

  val test2: (c: Capp^) -> () => Unit =
    localCap { c =>  // error
      (c1: Capp^) => () => { c1.use() }
    }

  val test3: (c: Capp^{io}) -> () ->{io} Unit =
    localCap { c =>  // error (???) since change to cs mapping
      (c1: Capp^{io}) => () => { c1.use() }
    }

  val test4: (c: Capp^{io}) -> () ->{net} Unit =
    localCap { c =>  // error
      (c1: Capp^{io}) => () => { c1.use() }
    }

  def localCap2[T](op: (c: Capp^{io}) => T): T = ???

  val test5: () ->{io} Unit =
    localCap2 { c =>  // ok
      () => { c.use() }
    }

}

// Original issue from PR #16264
def main2() = {
  val f: (io: Capp^) -> () -> Unit =
    io => () => io.use()  // error

  val g: (Capp^) -> () -> Unit =
    io => () => io.use()  // error
}