class File
class Box[A](val value: A)

// Lambdas in the result of a closure keep their bounds
def inClosureResult(x: File^) =
  val p = [C^] => (a: File^{C}) => Box([D^ >: {C}] => (b: File^{D}) => (a: File^{D}))
  p[{x}](x).value[{}](File()) // error

// Several sources of function types: lower bounds are united
def merged(io: File^, c: Boolean) =
  val f = Box(if c then [C^ >: {io}] => (y: File^{C}) => y else [C^] => (y: File^{C}) => y)
  f.value[{}](File()) // error

// Inferred capture set arguments get the lower bound of their parameter
def inferredArg(io: File^) =
  def m[C^ >: {io}](x: File^{C}): File^{C} = io
  val r: File^{} = m(File()) // error
