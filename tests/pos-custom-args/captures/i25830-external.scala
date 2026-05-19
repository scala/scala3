import language.experimental.captureChecking
import caps.*

class File extends ExclusiveCapability

// Capture-polymorphic lambdas whose retains mention enclosing capabilities
// in addition to (or instead of) the lambda's own capset binders. Because
// PostTyper now keeps the user-written parameter types and binder bounds
// verbatim ("explicify"), Setup processes them through transformExplicitType
// which preserves @retains. Only the inferred result type is cleaned up.

def mixedExternal() =
  val external = File()
  val f =
    { [C^] => (xs: List[File^{C, external}]) => xs }

def externalOnly() =
  val external = File()
  val f =
    { [C^] => (xs: List[File^{external}]) => xs }

def mixedExternalWithLowerBoundedParam() =
  val external = File()
  val f =
    { [C^, D^ >: {C}] => (xs: List[File^{D, external}]) => xs }

def mixedExternalInLaterParamList() =
  val external = File()
  val f =
    { [C^] => (xs: List[File^{C}]) => (ys: List[File^{C, external}]) => ys }

def enclosingParam(external: File^) =
  val f =
    { [C^] => (xs: List[File^{C}]) => (ys: List[File^{external}]) => ys }

def supportedDef() =
  val external = File()
  def f =
    { [C^] => (xs: List[File^{C, external}]) => xs }

def insideAnonymousFunction() =
  List(File()).map: external =>
    val f =
      { [C^] => (xs: List[File^{C}]) => (ys: List[File^{external}]) => ys }

def externalInBound() =
  val external = File()
  val f =
    { [C^, D^ <: {C, external}] => (xs: List[File^{D}]) => xs }

def nestedCapsetBinders() =
  val f =
    { [C^] => (xs: List[File^{C}]) => [D^] => (ys: List[File^{C, D}]) => ys }

def literalNestedInFunction1() =
  val external = File()
  val f =
    (i: Int) => { [C^] => (xs: List[File^{C, external}]) => xs }
