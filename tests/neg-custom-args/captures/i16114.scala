trait Cap { def use(): Int; def close(): Unit }
def mkCap(): Cap^ = ???

def expect[T](x: T): x.type = x

def withCap[T](op: Cap^ => T): T = {
  val cap: Cap^ = mkCap()
  val result = op(cap)
  cap.close()
  result
}

def main(fs: Cap^): Unit = {
  def badOp(io: Cap^): Unit ->{} Unit = {
    val op1: Unit ->{io} Unit = (x: Unit) =>
      expect[Cap^] {
        io.use()
        fs  // error (limitation)
      }

    val op2: Unit ->{fs} Unit = (x: Unit) =>
      expect[Cap^] {
        fs.use()
        io  // error (limitation)
      }

    val op3: Unit ->{io} Unit = (x: Unit) =>  // ok
      expect[Cap^] {
        io.use()
        io
      }

    val op4: Unit ->{} Unit = (x: Unit) =>  // o k
      expect[Cap^](io) // error

    val op: Unit -> Unit = (x: Unit) =>
      expect[Cap^] {
        io.use()    // error
        io          // error
      }
    op
  }

  val leaked: Unit -> Unit = withCap(badOp)
  leaked(())
}
