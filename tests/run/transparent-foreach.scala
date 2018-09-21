// A demonstrator how `inline override` can be used to sepcialize inherited methods
trait Iterable[+A] {
  def foreach(f: A => Unit): Unit
}
// The unconventional definition of `Range` and `UntilRange` is needed so
// that `UntiRange` is recognized as a Noinits class. Our purity predictor
// for classes currently bails out for arguments passed to parent classes
// and for concrete `val`s. That's because the predictor runs on untyped trees
// so there's no way to predict whether an expression is pure or not.
// It would be nice if we could improve the predictor to work with typed trees.
// The tricky bit is doing this without causing cycles. That's the price we
// pay for making inlining a typelevel computation.
// One possible way to do it would be to make purity a property that's computed on demand,
// just like info, but evaluated later. Then we might still cause cycles, but these
// would be "justified" by inlining attempts. I.e. you could avoid a cycle by
// inlining less.
abstract class Range extends Iterable[Int]  {
  val start: Int
  val end: Int
  def step: Int
  def inclusive: Boolean
  def foreach(f: Int => Unit): Unit = {
    var idx = start
    while (
      if (step > 0)
        if (inclusive) idx <= end else idx < end
      else
        if (inclusive) idx >= end else idx > end
    ) {
      f(idx)
      idx = idx + step
    }
  }
}
class UntilRange(val start: Int, val end: Int) extends Range {
  def step = 1
  def inclusive = false
  inline override def foreach(f: Int => Unit): Unit = {
    var idx = start
    while (idx < end) {
      f(idx)
      idx += 1
    }
  }
}
object Test extends App {
  var x = 0
  new UntilRange(1, 10).foreach(x += _)
    // Expands to:
    //    var idx: Int = 1
    //    while idx < 10 do
    //      x = x * idx
    //      idx = idx + 1
    //    }

  class IntDeco(val x: Int) extends AnyVal {
    inline def until(y: Int) = new UntilRange(x, y)
  }
  implicit inline def intDeco(x: Int): IntDeco = new IntDeco(x)
    // So far, the decorator has to be an explicit def, since
    // we can inline only methods defined in source. So an implicit class
    // will not work. We can make this work by making `Desugar` smarter
    // and generate an inline method with pre-defined "BodyToInline" annotation.
    // It's doable, just extra work.

  (1 until 10).foreach(x += _)
    // expands to same as above

  assert(x == 90)
}
