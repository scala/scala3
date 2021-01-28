class Test {
  val tup: Char #: Int #: String #: TupleK = ???
  val x: String #: TupleK = (tup.tail: Int #: String #: TupleK).tail
  val a = tup.tail
  val b = a.tail
  val y: String #: TupleK = tup.tail.tail
  val z: Unit = tup.tail.tail
}

trait TupleK

object TupleK {
  type Tail[X <: NonEmptyTupleK] <: TupleK = X match {
    case _ #: xs => xs
  }
}

trait NonEmptyTupleK extends TupleK {
  /*inline*/ def tail[This >: this.type <: NonEmptyTupleK]: TupleK.Tail[This] = ???
}

abstract class #:[+H, +T <: TupleK] extends NonEmptyTupleK