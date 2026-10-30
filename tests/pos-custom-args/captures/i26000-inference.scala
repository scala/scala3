import caps.*

class File
class Box[A](val value: A)
class Sh extends SharedCapability
class Ref extends Mutable:
  var x = 0

// Lambdas in the result of a closure
def inClosureResult(x: File^) =
  val p = [C^] => (a: File^{C}) => Box([D^ >: {C}] => (b: File^{D}) => (a: File^{D}))
  val _: File^{x} = p[{x}](x).value[{x}](File())
  val q = [C^] => (u: Unit) => Box((y: File^{C}) => y)
  val _: File^{x} = q[{x}](()).value(x)

// Several sources of function types: their parameters get the intersection
def merged(a: File^, b: File^, c: Boolean) =
  val f = Box(if c then [C^] => (y: File^{C, a}) => y else [C^] => (y: File^{C, a, b}) => y)
  val _ = f.value[{}](a)

// Inferred capture set arguments get the lower bound of their parameter
def inferredArg(io: File^) =
  def m[C^ >: {io}](x: File^{C}): File^{C} = io
  val r: File^{io} = m(File())

// Parameters of classified types
def classified(io: File^, sh: Sh^, r: Ref^) =
  val s = Box([C^] => (a: Sh^{C}) => a)
  val _: Sh^{sh} = s.value[{sh}](sh)
  val m = Box([C^ <: {r}] => (a: Ref^{C}) => a.x)
  val _ = m.value[{r}](r)
