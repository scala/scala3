import compiletime.*
import compiletime.ops.int.*

object TupleOps:

  /** The index of `Y` in tuple `X`, or `Size[X]` if `Y` does not occur in `X` */
  type IndexOf[X <: Tuple, Y] <: Int = X match
    case Y *: _ => 0
    case x *: xs => S[IndexOf[xs, Y]]
    case EmptyTuple => 0

  /** The `X` tuple, with its element at index `N` replaced by `Y`.
   *  If `N` is not an index of `X`, the element `Y` is appended instead
   */
  type UpdateOrAppend[X <: Tuple, N <: Int, Y] <: Tuple = X match
    case x *: xs =>
      N match
        case 0 => Y *: xs
        case S[n1] => x *: UpdateOrAppend[xs, n1, Y]
    case EmptyTuple => Y *: EmptyTuple

  inline def updateOrAppend[X <: Tuple, N <: Int, Y](xs: X, y: Y): UpdateOrAppend[X, N, Y] =
    def recur(xs: Tuple, n: Int): Tuple = xs match
      case x *: xs1 =>
        if n == 0 then y *: xs1 else x *: recur(xs1, n - 1)
      case EmptyTuple =>
        y *: EmptyTuple
    recur(xs, constValue[N]).asInstanceOf[UpdateOrAppend[X, N, Y]]

  /** If `Y` does not occur in tuple `X`, `X` with `Y` appended. Otherwise `X`. */
  type AppendIfDistinct[X <: Tuple, Y] <: Tuple = X match
    case Y *: xs => X
    case x *: xs => x *: AppendIfDistinct[xs, Y]
    case EmptyTuple => Y *: EmptyTuple

  /** `X` with all elements from `Y` that do not occur in `X` appended */
  type ConcatDistinct[X <: Tuple, Y <: Tuple] <: Tuple = Y match
    case y *: ys => ConcatDistinct[AppendIfDistinct[X, y], ys]
    case EmptyTuple => X

  // TODO: Implement appendIfDistinct, concatDistinct

object NamedTupleOps:
  import TupleOps.*

  opaque type NamedTuple[N <: Tuple, +X <: Tuple] >: X = X

  /** The names of the named tuple type `NT` */
  type Names[NT] <: Tuple = NT match
    case NamedTuple[n, _] => n

  /** The value types of the named tuple type `NT` */
  type DropNames[NT] <: Tuple = NT match
    case NamedTuple[_, x] => x

  extension [N <: Tuple, X <: Tuple](x: NamedTuple[N, X])
    transparent inline def dropNames: X = x.asInstanceOf
    transparent inline def names: N = ???

  /** Internal use only: Merge names and value components of two named tuple to
   *  impement `UpdateWith`.
   *  @param N  the names of the combined tuple
   *  @param X  the value types of the first named tuple
   *  @param N2 the names of the second named tuple
   *  @param Y  the value types of the second named tuple
   */
  type Merge[N <: Tuple, X <: Tuple, N2 <: Tuple, Y <: Tuple] = (N2, Y) match
    case (n *: ns, y *: ys) =>
      Merge[N, UpdateOrAppend[X, IndexOf[N, n], y], ns, ys]
    case (EmptyTuple, EmptyTuple) =>
      NamedTuple[N, X]

  /** A joint named tuple where
   *   - The names are the names of named tuple `NT1` followed by those names of `NT2` which
   *     do not appear in `NT1`
   *   - The values are the values of `NT1` and `NT2` corresponding to these names.
   *     If a name is present in both `NT1` and `NT2` the value in `NT2` is used.
   */
  type UpdateWith[NT1, NT2] =
    Merge[ConcatDistinct[Names[NT1], Names[NT2]], DropNames[NT1], Names[NT2], DropNames[NT2]]

  // TODO: Implement merge, updateWith

@main def Test =
  import TupleOps.*
  import NamedTupleOps.*

  type Names = "first" *: "last" *: "age" *: EmptyTuple
  type Values = "Bob" *: "Miller" *: 33 *: EmptyTuple

  val names: Names = ("first", "last", "age")
  val values: Values = ("Bob", "Miller", 33)

  val x1: IndexOf[Names, "first"] = constValue
  val _: 0 = x1

  val x2: IndexOf[Names, "age"] = constValue
  val _: 2 = x2

  val x3: IndexOf[Names, "what?"] = constValue
  val _: 3 = x3

  type Releases = "first" *: "middle" *: EmptyTuple
  type ReleaseValues = 1.0 *: true *: EmptyTuple

  val x4: UpdateOrAppend[Values, IndexOf[Names, "age"], 11] =
    updateOrAppend[Values, IndexOf[Names, "age"], 11](values, 11)
  val _: ("Bob", "Miller", 11) = x4
  assert(("Bob", "Miller", 11) == x4)

  val x5: UpdateOrAppend[Values, IndexOf[Names, "what"], true] =
    updateOrAppend[Values, IndexOf[Names, "what"], true](values, true)
  val _: ("Bob", "Miller", 33, true) = x5
  assert(("Bob", "Miller", 33, true) == x5)

  val x6: UpdateOrAppend[Values, IndexOf[Names, "first"], "Peter"] =
    updateOrAppend[Values, IndexOf[Names, "first"], "Peter"](values, "Peter")
  val _: ("Peter", "Miller", 33) = x6
  assert(("Peter", "Miller", 33) == x6)

  val x7: ConcatDistinct[Names, Releases] = ???
  val _: ("first", "last", "age", "middle") = x7

  val x8: ConcatDistinct[Releases, Names] = ???
  val _: ("first", "middle", "last", "age") = x8

  val x9: Merge[ConcatDistinct[Names, Releases], Values, Releases, ReleaseValues] = ???
  val _: NamedTuple[("first", "last", "age", "middle"), (1.0, "Miller", 33, true)] = x9

  val x10: UpdateWith[NamedTuple[Names, Values], NamedTuple[Releases, ReleaseValues]] = ???
  val _: ("first", "last", "age", "middle") = x10.names
  val _: (1.0, "Miller", 33, true) = x10.dropNames

  val x11: UpdateWith[NamedTuple[Releases, ReleaseValues], NamedTuple[Names, Values]] = ???
  val _: NamedTuple[("first", "middle", "last", "age"), ("Bob", true, "Miller", 33)] = x11

