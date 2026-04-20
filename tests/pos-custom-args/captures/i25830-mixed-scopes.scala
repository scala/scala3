import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

// Every local poly lambda below mentions at least one *outer* capture-set
// parameter in its own signature, and varies the lambda's own binder shape —
// multiple cap-sets, interleaved with plain types, nested poly, etc.
// Shapes where the local lambda does not reference anything from the outer
// scope are covered by tests/pos-custom-args/captures/i25830.scala.

// (1) val-form of the `def g[C^] = (xs) => xs.head` TODO line from
//     tests/neg-custom-args/captures/use-capset.scala. The def form still
//     errors (separate @use/classifier issue); the val form works because
//     the lambda binds its own C.
def useCapsetVal() =
  val g = { [C^] => (xs: List[File^{C}]) => xs.head }
  val io = File()
  val _ : File^{io} = g[{io}](List[File^{io}](io))

// (2) Baseline: simple local `[C^]` inside a def with `[OuterC^]`, the
//     lambda's signature mentions both.
def baseline[OuterC^](a: File^{OuterC}) =
  val mk = { [C^] => (x: File^{C}, y: File^{OuterC}) => x }
  val b = File()
  val _ : File^{b} = mk[{b}](b, a)

// (3) Fully interleaved local binders `[T, C^, U, D^]` around an outer cap-set.
def interleaved_local[OuterC^](a: File^{OuterC}) =
  val mk = { [T, C^, U, D^] =>
    (t: T, x: File^{C}, u: U, y: File^{D}, z: File^{OuterC}) => (t, u, x)
  }
  val b = File(); val c = File()
  val _ = mk[Int, {b}, String, {c}](1, b, "s", c, a)

// (4) Outer itself mixes plain + cap-set; local lambda does too.
def mixed_both_sides[T, OuterC^](t: T, a: File^{OuterC}) =
  val mk = { [U, C^] =>
    (u: U, x: File^{C}, t2: T, y: File^{OuterC}) => (u, x, t2)
  }
  val b = File()
  val _ = mk[Boolean, {b}](true, b, t, a)

// (5) Class field: lambda's interleaved signature mentions the class's
//     cap-set parameter.
class Holder[OuterC^](val outer: File^{OuterC}):
  val mk = { [T, C^, U] =>
    (t: T, x: File^{C}, u: U, y: File^{OuterC}) => x
  }

def useHolder() =
  val a = File(); val b = File()
  val h = Holder[{a}](a)
  val _ : File^{b} = h.mk[Int, {b}, String](1, b, "s", a)

// (6) Trait abstract member + subclass override, both mentioning the
//     enclosing cap-set parameter. Exercises the explicit-tpt path.
trait Ops[OuterC^]:
  val mk: [T, C^] -> (t: T, x: File^{C}, y: File^{OuterC}) -> File^{C}

class OpsImpl[X^](x0: File^{X}) extends Ops[X]:
  val mk = { [T, C^] => (t: T, x: File^{C}, y: File^{X}) => x }

def useOps() =
  val a = File(); val b = File()
  val ops = OpsImpl[{a}](a)
  val _ : File^{b} = ops.mk[Int, {b}](7, b, a)

// (7) Nested outer scopes (class + inner def) each contribute a cap-set;
//     the local lambda mentions both.
class Outer[X^](val xr: File^{X}):
  def inner[T, Y^](yr: File^{Y})(t0: T) =
    val mk = { [U, C^] =>
      (u: U, x: File^{C}, a: File^{X}, b: File^{Y}, t: T) => x
    }
    val c = File()
    val _ : File^{c} = mk[String, {c}]("s", c, xr, yr, t0)

// (8) Nested poly lambda: the body of the outer lambda is itself a poly
//     lambda, with interleaved binders at each level, and the inner level
//     mentions the enclosing method's OuterC.
def nested_poly_interleaved[OuterC^](a: File^{OuterC}) =
  val mk = { [T, C^] => (t: T, x: File^{C}) =>
    { [U, D^] => (u: U, y: File^{D}, z: File^{OuterC}) => (t, x) } }
  val b = File(); val c = File()
  val _ = mk[Int, {b}](1, b)[String, {c}]("s", c, a)
