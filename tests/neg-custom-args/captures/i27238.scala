import language.experimental.captureChecking
import language.experimental.separationChecking
import scala.caps.{any, Classifier, Control, SharedCapability}

trait IOCap extends SharedCapability, Classifier
class Lbl extends Control:
  def break(): Unit = ()
class IO extends IOCap:
  def use(): Unit = ()

// The capture set of a consume by-name parameter is still enforced
def fork(consume t: ->{any.except[Control]} Unit): Unit = ???
def forkOnly(consume t: ->{any.only[Control]} Unit): Unit = ???

def test(l: Lbl^, io: IO^) =
  fork(l.break())     // error
  fork(io.use())      // ok
  forkOnly(io.use())  // error
  forkOnly(l.break()) // ok
