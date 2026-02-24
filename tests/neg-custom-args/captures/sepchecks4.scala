import caps.any
import language.future
import language.experimental.captureChecking

def par(op1: () => Unit)(op2: () => Unit): Unit = ()

def bad(io: Object^): () => Unit =    // error
  val x: () => Unit = () => println(io) // error
  x

def test(io: Object^): Unit =
  par(() => println(io))(() => println(io)) // error // (1)

  val f = bad(io)
  par(f)(() => println(io))  // no error, but it is equivalent to (1) and should fail

