import language.experimental.captureChecking
import caps.*

// Soundness regression: for nicolas1-shape poly-fn lambdas with capset
// binder `B^`, applying with `B := {seed}` must propagate `^{seed}` into
// the result. Assigning the result to a strict pure-bound (`Rand -> Int`)
// must be rejected.

trait Rand extends SharedCapability:
  def range(min: Int, max: Int): Int

val pickFirst =
  [A, B^] => (head: Rand ->{B} A, tail: Rand ->{B} A) => head

val oneOf =
  [A, B^] => (head: Rand ->{B} A, tail: Seq[Rand ->{B} A]) =>
    val all: Seq[Rand ->{B} A] = head +: tail
    all.head

def check =
  val seed: Rand = ???
  val f: Rand ->{seed} Int = (r: Rand) => r.range(0, 10)
  val r2: Rand -> Int = pickFirst[Int, {seed}](f, f)            // error
  val r4: Rand -> Int = oneOf[Int, {seed}](f, Seq(f, f))        // error
