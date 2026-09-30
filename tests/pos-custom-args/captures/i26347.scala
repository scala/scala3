import language.experimental.captureChecking
import caps.*

class File extends ExclusiveCapability

// Receiver lifted into a val, as scoverage does: the closure parameter's
// inferred type adopts the root capability in `tmp`'s type.

def insideAnonymousFunction() =
  val tmp = List(File())
  tmp.map: external =>
    val f = { [C^] => (xs: List[File^{C}]) => (ys: List[File^{external}]) => ys }

def simpleLambda() =
  val tmp = List(File())
  tmp.map: external =>
    ()

def usedAsValue() =
  val tmp = List(File())
  tmp.map: external =>
    val g: File^{external} = external
    ()

def usedInCaptureSet() =
  val tmp = List(File())
  tmp.map: external =>
    val f = (ys: List[File^{external}]) => ys
    ()
