import language.experimental.captureChecking
import caps.{any, Classifier, Control, SharedCapability, ExclusiveCapability}

// "Classifying Capabilities" §2.1: chaining only then except.
// only[SharedCapability].except[Control] admits exactly the shared, non-Control capabilities.
trait IOCap extends SharedCapability, Classifier   // shared, sibling of Control
class File extends IOCap:
  def read(): Unit = ()
class CanThrow[-T] extends Control                 // shared, but in the excluded subtree
class Buffer extends ExclusiveCapability:          // not shared
  def write(): Unit = ()

def runShared[T](task: () ->{any.only[SharedCapability].except[Control]} T): T = task()

def test(f: File^, exc: CanThrow[Int]^, buf: Buffer^): Unit =
  runShared(() => f.read())        // ok
  runShared(() => println(exc))    // error: exc is Control
  runShared(() => buf.write())     // error: Buffer is exclusive
