import caps.{any, Classifier, Control, SharedCapability}

trait IOCap extends SharedCapability, Classifier

class File extends IOCap:
  def read(): Int = 1

def runOnNewThread[T](body: () ->{any.except[Control]} T): T = body()

def test(f: File^) =
  runOnNewThread: () =>
    f.read() // ok: f is classified as IOCap, which is unrelated to Control
