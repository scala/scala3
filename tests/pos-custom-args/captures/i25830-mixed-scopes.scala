import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

// (1) val-form of use-capset.scala's TODO line — the def-form still errors
//     due to the `@use`/classifier issue, but this val-form works.
def useCapsetVal() =
  val g = { [C^] => (xs: List[File^{C}]) => xs.head }
  val io = File()
  val _ : File^{io} = g[{io}](List[File^{io}](io))

// (2) Curried poly lambda inside a def whose own type params are cap-set.
//     Local lambda doesn't reference the outer cap-set.
def insideDef[OuterC^](a: File^{OuterC}) =
  val g = { [C^] => (x: File^{C}) => x }
  val b = File()
  val _ : File^{b} = g[{b}](b)

// (3) Curried poly lambda inside a def whose type params are cap-set.
def insideDefCurried[OuterC^](a: File^{OuterC}) =
  val g = { [C^] => (x: File^{C}) => (y: File^{C}) => x }
  val b = File()
  val _ : File^{b} = g[{b}](b)(b)

// (4) Local lambda's type references the outer cap-set param
//     (non-curried, so no closure-capture classifier issue).
def outerReferenced[OuterC^](a: File^{OuterC}) =
  val mk = { [C^] => (x: File^{C}, y: File^{OuterC}) => x }
  val b = File()
  val _ : File^{b} = mk[{b}](b, a)

// (5) Def with an inferred return type that is itself a poly lambda
//     referencing the outer cap-set.
def mkLambdaInferred[OuterC^](a: File^{OuterC}) =
  [C^] => (x: File^{C}, y: File^{OuterC}) => x

def useMk() =
  val a = File(); val b = File()
  val mk = mkLambdaInferred[{a}](a)
  val _ : File^{b} = mk[{b}](b, a)

// (6) Inside a class with a cap-set type parameter.
class Holder[OuterC^](val outer: File^{OuterC}):
  val convert = { [C^] => (xs: List[File^{C}]) => xs.map(_ => ()) }
  def curried = { [C^] => (a: File^{C}) => (b: File^{C}) => a }

def useHolder() =
  val b = File()
  val h = Holder[{b}](b)
  val _ = h.convert[{b}](List[File^{b}](b))
  val _ : File^{b} = h.curried[{b}](b)(b)

// (7) Inside a trait with a cap-set type parameter.
trait Ops[OuterC^]:
  val mk = { [C^] => (xs: List[File^{C}]) => xs }

class OpsImpl[X^] extends Ops[X]:
  val specific = { [C^] => (a: File^{C}) => a }

def useOps() =
  val b = File()
  val ops = OpsImpl[{b}]()
  val _ = ops.mk[{b}](List[File^{b}](b))
  val _ : File^{b} = ops.specific[{b}](b)
