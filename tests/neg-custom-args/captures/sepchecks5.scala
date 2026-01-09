import caps.any
import language.future
import language.experimental.captureChecking

def par(op1: () => Unit)(op2: () => Unit): Unit = ()

def f(consume io: Object^): () => Unit =
  () => println(io)

def g(consume io: Object^): () => Unit = f(io) // ok

def bad(io: Object^): () => Unit = f(io) // error

def test(io: Object^): Unit =

  val f1 = bad(io)
  par(f1)(() => println(io))  // !!! separation failure

  val f2 = g(io)              // error
  par(f2)(() => println(io))  // error

