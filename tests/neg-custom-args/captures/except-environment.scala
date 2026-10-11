import caps.{any, Classifier, Control, SharedCapability}

// Section 2.1 of "Classifying Capabilities": a function that dispatches work to a
// new thread must forbid its closure from using thread-local Control capabilities.
// `File` is a resource capability classified disjoint from `Control`.
trait IOCap extends SharedCapability, Classifier
class File extends IOCap:
  def read(): Unit = ()

class CanThrow[-T] extends Control

case class Environment(file: File^, exc: CanThrow[Int]^)

def runOnNewThread(task: () ->{any.except[Control]} Unit): Unit = task()

def f(env: Environment^) =
  runOnNewThread(() => env.file.read())   // ok: File is not a Control capability
  runOnNewThread(() => println(env.exc))  // error: exc is Control
