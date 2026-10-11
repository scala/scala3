// Polymorphic functions whose runtime representation is not determined by their
// static type.
//
// A value of type `[X] => A` with `A` abstract erases to ErasedPolyFunction, a Function0
// at runtime, whereas a value of type `[X] => Object => Object` erases to Function1.
// Erasure adapts between the two representations when a value flows directly into
// a parameter or out of a result whose type mentions `[X] => A`. But once a value in
// the ErasedPolyFunction representation is stored in a generic container like a tuple
// or boxed to Any, the container's element is a Function0 while the static type of the
// extracted element says Function1, and the cast inserted by erasure fails.
object Test:
  def idPoly[A](ff: [X] => A): [X] => A = ff
  def twice[A](ff: [X] => A): ([X] => A, [X] => A) = (ff, ff)

  val f1 = [X] => (y: Object) => { println(y); y }
  val f2 = [X] => (a: Int, b: Int) => a + b

  def main(args: Array[String]): Unit =
    // Elements of a tuple built under the abstract type `[X] => A`
    val (t1, t2) = twice(f1)
    t1("t1")
    t2("t2")

    // Tuple elements with erased poly function type, passed as Object to Tuple2
    val tup: ([X] => Object => Object, [X] => (Int, Int) => Int) = (idPoly(f1), idPoly(f2))
    tup._1("tup._1")
    println(tup._2(5, 5))

    // Boxing to Any loses the FunctionN representation
    (idPoly(f1): Any) match
      case g: Function1[?, ?] => println(s"Function1 matched: ${g.asInstanceOf[Object => Object]("matched any")}")
      case other => println(s"unexpected: $other")
