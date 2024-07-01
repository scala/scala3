// example of cyclic initialization causing deadlock

package pkg

object Main extends App {
  val createPredef = new Runnable { def run = { val _ = Predef } }
  val createSeq = new Runnable { def run = { val _ = Seq } }
  new Thread(createPredef).start()
  new Thread(createSeq).start()
  Thread.sleep(100)
  val seq = Predef.seq
  val predef = Seq.predef
  println("done")
}

object Predef { // warn
  Thread.sleep(10)
  val seq = Seq
  println("done Predef")
}

object Seq {
  Thread.sleep(10)
  val predef = Predef
  println("done Seq")
}
