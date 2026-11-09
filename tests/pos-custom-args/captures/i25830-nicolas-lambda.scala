import language.experimental.captureChecking
import caps.*

// Lambda forms of the nicolas1 patterns: capset binder `B^` parameterising
// regular function types `Rand ->{B} A`. Verifies the explicify pipeline
// keeps `^{B}` flowing through to the result so the lambda can be applied
// with `B := {seed}` and used at the strict bound `Rand ->{seed} Int`.

trait Rand extends SharedCapability:
  def range(min: Int, max: Int): Int

val pickFirst =
  [A, B^] => (head: Rand ->{B} A, tail: Rand ->{B} A) => head

val oneOf =
  [A, B^] => (head: Rand ->{B} A, tail: Seq[Rand ->{B} A]) =>
    val all: Seq[Rand ->{B} A] = head +: tail
    all.head

def use =
  val seed: Rand = ???
  val f: Rand ->{seed} Int = (r: Rand) => r.range(0, 10)
  val r1: Rand ->{seed} Int = pickFirst[Int, {seed}](f, f)
  val r3: Rand ->{seed} Int = oneOf[Int, {seed}](f, Seq(f, f))
