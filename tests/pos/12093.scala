import scala.compiletime.ops.int.{`*`, +}

// HList
sealed trait Shape
final case class #:[H <: Int & Singleton, T <: Shape](head: H, tail: T) extends Shape
case object Ø extends Shape
type Ø = Ø.type

// Reduce
def reduce[T, S <: Shape, A <: Shape](shape: S, axes: A): Reduce[S, A, 0] = ???
type Reduce[S, Axes <: Shape, I <: Int] <: Shape = S match {
  case head #: tail => Contains[Axes, I] match {
    case true => Reduce[tail, Remove[Axes, I], I + 1]
    case false => head #: Reduce[tail, Axes, I + 1]
  }
  case Ø => Axes match {
    case Ø => Ø
    // otherwise, do not reduce further
  }
}
type Contains[Haystack <: Shape, Needle <: Int] <: Boolean = Haystack match {
  case Ø => false
  case head #: tail => head match {
    case Needle => true
    case _ => Contains[tail, Needle]
  }
}
type Remove[From <: Shape, Value <: Int] <: Shape = From match {
  case Ø => Ø
  case head #: tail => head match {
    case Value => Remove[tail, Value]
    case _ => head #: Remove[tail, Value]
  }
}

// Reshape
def reshape[From <: Shape, To <: Shape](from: From, to: To)
  (using ev: NumElements[From] =:= NumElements[To]): To = ???
type NumElements[X <: Shape] <: Int = X match {
  case Ø => 1
  case head #: tail => head * NumElements[tail]
}

// Test cases
val input = #:(25, #:(256, #:(256, #:(3, Ø))))
val reduced = reduce(input, #:(3, #:(1, #:(2, Ø))))
val reshaped: 5 #: 5 #: Ø = reshape(reduced, #:(5, #:(5, Ø)))
