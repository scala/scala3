import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

// "Classifying Capabilities" §2.1: defining classifiers by extending caps.Classifier directly.
//   SharedCapability <- ThreadLocal <- Control <- CanThrow[-T]
trait ThreadLocal extends Classifier, SharedCapability
trait Control     extends Classifier, ThreadLocal
class CanThrow[-T] extends Control

class Ex1 extends Exception
class Ex2 extends Ex1

def use(ct: CanThrow[Ex1]^): Unit = ()

def test(ct1: CanThrow[Ex1]^, ct2: CanThrow[Ex2]^) =
  use(ct1)
  val widened: CanThrow[Ex2]^ = ct1       // contravariant in the exception type
  val asShared: SharedCapability^ = ct2   // CanThrow <: Control <: ThreadLocal <: SharedCapability
  ()
