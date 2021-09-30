object Meta:
  type Shape = String | Tuple

  type Demote[S <: Tuple]<: Shape = S match
    case Tuple1[t] => t & Shape
    case Tuple => S

  type If[T <: Boolean, R1, R2] <: R1 | R2 = T match
    case true => R1
    case false => R2

  type Contains[T <: Tuple, X] <: Boolean = T match
    case X *: r => true
    case _ *: r => Contains[r, X]
    case _ => false

  type RemoveStrict[T <: Tuple, X] <: Tuple = T match
    case head *: tail => head match
      case X => tail
      case _ => head *: RemoveStrict[tail, X]

  type WithoutStrict[T <: Tuple, T2 <: Tuple] <: Tuple = T2 match
    case head *: tail => WithoutStrict[RemoveStrict[T, head], tail]
    case EmptyTuple => T

  /** Removes all elems from ToReplace and replaces the first replaced elem with replacement */
  type ReplaceAllStrict[T <: Tuple, ToReplace <: Tuple, Replacement] <: Tuple = T match
    case head *: tail =>
      If[Contains[ToReplace, head],
          Replacement *: WithoutStrict[tail, RemoveStrict[ToReplace, head]],
          head *: ReplaceAllStrict[tail, ToReplace, Replacement]]
    case EmptyTuple => T

  type Sub[S <: Tuple, ToReplace <: Tuple, Replacement <: String] =
    Demote[ReplaceAllStrict[S, ToReplace, Replacement]]

object Foo:
  import Meta._
  val _0: Sub["batch" *: EmptyTuple, Int *: EmptyTuple, "batch"] = "batch"
  val _1: Sub[("batch", "len"), ("batch", "len"), "batch"] = "batch"
  val _2a: ReplaceAllStrict[("batch", "len", "embed"), "batch" *: EmptyTuple, "b"] = ("b", "len", "embed")
  type S = ("batch", "len")
  type ToReplace = "batch" *: EmptyTuple
  type Replacement = "b"
  val _2b: ReplaceAllStrict[S, ToReplace, Replacement] = ("b", "len") // ok
  val _2c: Demote[ReplaceAllStrict[S, ToReplace, Replacement]] = ("b", "len") // ok
  val _2d: Sub[S, ToReplace, Replacement] = ("b", "len") // error, see below
