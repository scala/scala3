import caps.{any, Classifier, SharedCapability}

// Section 2.3 of "Classifying Capabilities": a Future cannot capture thread-local
// resources. Following the paper, the control classifier sits *under* ThreadLocal,
// so a single exclusion of the ancestor ThreadLocal also excludes Control.
trait ThreadLocal extends SharedCapability, Classifier
trait Control     extends ThreadLocal, Classifier
trait Resource    extends SharedCapability, Classifier   // a sibling of ThreadLocal

class Label extends Control
class File  extends Resource:
  def read(): Unit = ()

class Future[T]
object Future:
  def apply[T](body: () ->{any.except[ThreadLocal]} T): Future[T]^{body} = ???

def test(label: Label^, file: File^) =
  Future(() => file.read())     // ok: File is a Resource, disjoint from ThreadLocal
  Future(() => println(label))  // error: Label is Control, a descendant of ThreadLocal
