import language.experimental.captureChecking

def seq(x: () => Unit)(y: () ->{x} Unit): Unit =
  x()
  y()

def seqSameList(x: () => Unit, y: () ->{x} Unit): Unit =
  x()
  y()

def captureTest(io: Object^): Unit =
  lazy val f: () ->{io} Unit = () => println(io)
  seq(f)(f)
  seqSameList(f, f)
