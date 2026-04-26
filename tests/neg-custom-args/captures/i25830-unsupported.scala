import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

// TODO these cases are currently restricted by the implementation, but we should consider allowing some of them in the future. See i25830 for details.
// The general case for capture-polymorphic lambdas is non-trivial to implement and we want to get the implementation right before allowing more cases.
// On the other hand, they are currently rarely needed in practice and the workaround is not too bad, so we can live with the restriction for now.

def mixedExternal() =
  val external = File()
  val f = // error
    { [C^] => (xs: List[File^{C, external}]) => xs }

def externalOnly() =
  val external = File()
  val f = // error
    { [C^] => (xs: List[File^{external}]) => xs }

def mixedExternalWithLowerBoundedParam() =
  val external = File()
  val f = // error
    { [C^, D^ >: {C}] => (xs: List[File^{D, external}]) => xs }

def mixedExternalInLaterParamList() =
  val external = File()
  val f = // error
    { [C^] => (xs: List[File^{C}]) => (ys: List[File^{C, external}]) => ys }

def enclosingParam(external: File^) =
  val f = // error
    { [C^] => (xs: List[File^{C}]) => (ys: List[File^{external}]) => ys }

def unsupportedDef() =
  val external = File()
  def f = // error
    { [C^] => (xs: List[File^{C, external}]) => xs }

def unsupportedInsideAnonymousFunction() =
  List(File()).map: external =>
    val f = // error
      { [C^] => (xs: List[File^{C}]) => (ys: List[File^{external}]) => ys }

def externalInBound() =
  val external = File()
  val f = // error
    { [C^, D^ <: {C, external}] => (xs: List[File^{D}]) => xs }

def nestedCapsetBinders() =
  val f = // error
    { [C^] => (xs: List[File^{C}]) => [D^] => (ys: List[File^{C, D}]) => ys }
