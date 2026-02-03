import compiletime.*
import compiletime.ops.int.*
import compiletime.ops.boolean.!
import Tuple.*

object TupleOps:

  private object helpers:

    /** Used to implement IndicesWhere */
    type IndicesWhereHelper[X <: Tuple, P[_ <: Union[X]] <: Boolean, N <: Int] <: Tuple = X match
      case EmptyTuple => EmptyTuple
      case h *: t => P[h] match
        case true => N *: IndicesWhereHelper[t, P, S[N]]
        case false => IndicesWhereHelper[t, P, S[N]]

  end helpers

  /** A type level Boolean indicating whether the tuple `X` has an element
   *  that matches `Y`.
   *  @pre  The elements of `X` are assumed to be singleton types
   */
  type Contains[X <: Tuple, Y] <: Boolean = X match
    case Y *: _ => true
    case _ *: xs => Contains[xs, Y]
    case EmptyTuple => false

  /** The index of `Y` in tuple `X` as a literal constant Int,
   *  or `Size[X]` if `Y` is disjoint from all element types in `X`.
   */
  type IndexOf[X <: Tuple, Y] <: Int = X match
    case Y *: _ => 0
    case _ *: xs => S[IndexOf[xs, Y]]
    case EmptyTuple => 0

  /** A tuple consisting of those indices `N` of tuple `X` where the predicate `P`
   *  is true for `Elem[X, N]`. Indices are type level values <: Int.
   */
  type IndicesWhere[X <: Tuple, P[_ <: Union[X]] <: Boolean] =
    helpers.IndicesWhereHelper[X, P, 0]

  extension [X <: Tuple](inline x: X)

    /** The index (starting at 0) of the first occurrence of `y.type` in the type `X` of `x`
     *  or `Size[X]` if no such element exists.
     */
    inline def indexOf(y: Any): IndexOf[X, y.type] = constValue[IndexOf[X, y.type]]

    /** A boolean indicating whether there is an element `y.type` in the type `X` of `x` */
    inline def contains(y: Any): Contains[X, y.type] = constValue[Contains[X, y.type]]

  end extension


  /** The `X` tuple, with its element at index `N` replaced by `Y`.
   *  If `N` is equal to `Size[X]`, the element `Y` is appended instead
   */
  type UpdateOrAppend[X <: Tuple, N <: Int, Y] <: Tuple = X match
    case x *: xs =>
      N match
        case 0 => Y *: xs
        case S[n1] => x *: UpdateOrAppend[xs, n1, Y]
    case EmptyTuple =>
      N match
        case 0 => Y *: EmptyTuple

  inline def updateOrAppend[X <: Tuple, N <: Int, Y](xs: X, y: Y): UpdateOrAppend[X, N, Y] =
    locally:
      val n = constValue[N]
      val size = xs.size
      require(0 <= n && n <= xs.size, s"Index $n out of range 0..$size")
      if n == size then xs :* y
      else
        val elems = xs.toArray
        elems(n) = y.asInstanceOf[Object]
        fromArray(elems)
    .asInstanceOf[UpdateOrAppend[X, N, Y]]

  extension [X <: Tuple](inline xs: X)
    // Note: Y must be inferred precisely, or given explicitly. This means even though `updateOrAppend`
    // is clearly useful, we cannot yet move it to tuple since it is still too awkward to use.
    // Once we have precise inference, we could replace `Y <: Singleton` with `Y: Precise`
    // and then it should work beautifully.
    inline def updateOrAppend[N <: Int & Singleton, Y <: Singleton](inline n: N, inline y: Y): UpdateOrAppend[X, N, Y] =
      locally:
        val size = xs.size
        require(0 <= n && n <= size, s"Index $n out of range 0..$size")
        if n == size then xs :* y
        else
          val elems = xs.toArray
          elems(n) = y.asInstanceOf[Object]
          fromArray(elems)
      .asInstanceOf[UpdateOrAppend[X, N, Y]]

  /** If `Y` does not occur in tuple `X`, `X` with `Y` appended. Otherwise `X`. */
  type AppendIfDistinct[X <: Tuple, Y] <: Tuple = X match
    case Y *: xs => X
    case x *: xs => x *: AppendIfDistinct[xs, Y]
    case EmptyTuple => Y *: EmptyTuple

  inline def appendIfDistinct[X <: Tuple, Y](xs: X, y: Y): AppendIfDistinct[X, Y] =
    (if xs.contains(y) then xs else xs :* y).asInstanceOf[AppendIfDistinct[X, Y]]

  /** `X` with all elements from `Y` that do not occur in `X` appended */
  type ConcatDistinct[X <: Tuple, Y <: Tuple] <: Tuple = Y match
    case y *: ys => ConcatDistinct[AppendIfDistinct[X, y], ys]
    case EmptyTuple => X

  inline def concatDistinct[X <: Tuple, Y <: Tuple](xs: X, ys: Y): ConcatDistinct[X, Y] =
    (xs ++ filter[Y, [Elem] =>> ![Contains[X, Elem]]](ys)).asInstanceOf[ConcatDistinct[X, Y]]

  /** A tuple consisting of all elements of this tuple that have types
   *  for which the given type level predicate `P` reduces to the literal
   *  constant `true`.
   */
  inline def filter[X <: Tuple, P[_] <: Boolean](xs: X): Filter[X, P] =
    val toInclude = constValueTuple[IndicesWhere[X, P]].toArray
    val arr = new Array[Object](toInclude.length)
    for i <- toInclude.indices do
      arr(i) = xs.productElement(toInclude(i).asInstanceOf[Int]).asInstanceOf[Object]
    Tuple.fromArray(arr).asInstanceOf[Filter[X, P]]
end TupleOps

object NamedTupleDecomposition:
  import NamedTupleOps.*
  import TupleOps.ConcatDistinct

  /** The names of the named tuple type `NT` */
  type Names[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[n, _] => n

  /** The value types of the named tuple type `NT` */
  type DropNames[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[_, x] => x

  extension [N1 <: Tuple, V1 <: Tuple](nt1: NamedTuple[N1, V1])
    inline def updateWith[N2 <: Tuple, V2 <: Tuple](nt2: NamedTuple[N2, V2])
      : UpdateWith[NamedTuple[N1, V1], NamedTuple[N2, V2]] =
      val names = constValueTuple[ConcatDistinct[N1, N2]].toArray
      val names2 = constValueTuple[N2].toArray
      val values1 = NamedTupleOps.toTuple(nt1)
      val values2 = nt2.toTuple
      val values = new Array[Object](names.length)
      values1.toArray.copyToArray(values)
      for i <- 0 until values2.size do
        val idx = names.indexOf(names2(i))
        values(idx) = values2.productElement(i).asInstanceOf[Object]
      Tuple.fromArray(values).asInstanceOf[UpdateWith[NamedTuple[N1, V1], NamedTuple[N2, V2]]]

end NamedTupleDecomposition

object NamedTupleOps:
  import TupleOps.*

  opaque type AnyNamedTuple = Any

  opaque type NamedTuple[N <: Tuple, +X <: Tuple] >: X <: AnyNamedTuple = X

  export NamedTupleDecomposition.{Names, DropNames}

  object NamedTuple:
    def apply[N <: Tuple, X <: Tuple](x: X): NamedTuple[N, X] = x

  extension [N <: Tuple, V <: Tuple](x: NamedTuple[N, V])
    inline def toTuple: V = x.asInstanceOf

    inline def names: N = constValueTuple[N]

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
  type UpdateWith[NT1 <: AnyNamedTuple, NT2 <: AnyNamedTuple] =
    Merge[ConcatDistinct[Names[NT1], Names[NT2]], DropNames[NT1], Names[NT2], DropNames[NT2]]

  export NamedTupleDecomposition.updateWith
end NamedTupleOps

@main def Test =
  import TupleOps.*
  import NamedTupleOps.*

  type Names = "first" *: "last" *: "age" *: EmptyTuple
  type Values = "Bob" *: "Miller" *: 33 *: EmptyTuple

  val names: Names = ("first", "last", "age")
  val values: Values = ("Bob", "Miller", 33)

  val x1: IndexOf[Names, "first"] = constValue
  val _: 0 = x1

  val x2: IndexOf[Names, "age"] = names.indexOf("age")
  val _: 2 = x2

  val x3: IndexOf[Names, "what?"] = names.indexOf("what?")
  val _: 3 = x3

  type Releases = "first" *: "middle" *: EmptyTuple
  type ReleaseValues = 1.0 *: true *: EmptyTuple

  val releases: Releases = ("first", "middle")
  val releaseValues: ReleaseValues = (1.0, true)

  val x4 = values.updateOrAppend(names.indexOf("age"), 11)
    //updateOrAppend[Values](values)[IndexOf[Names, "age"], 11](indexOf[Names](names)["age"]("age"), 11)
  val _: ("Bob", "Miller", 11) = x4
  assert(("Bob", "Miller", 11) == x4)

  val x5 = updateOrAppend[Values, IndexOf[Names, "what"], true](values, true)
  val _: ("Bob", "Miller", 33, true) = x5
  assert(("Bob", "Miller", 33, true) == x5)

  val x6 = updateOrAppend[Values, IndexOf[Names, "first"], "Peter"](values, "Peter")
  val _: ("Peter", "Miller", 33) = x6
  assert(("Peter", "Miller", 33) == x6)

  val x7 = concatDistinct[Names, Releases](names, releases)
  val _: ("first", "last", "age", "middle") = x7
  assert(("first", "last", "age", "middle") == x7, x7)

  val x8 = concatDistinct[Releases, Names](releases, names)
  val _: ("first", "middle", "last", "age") = x8
  assert(("first", "middle", "last", "age") == x8)

  def x9: Merge[ConcatDistinct[Names, Releases], Values, Releases, ReleaseValues] = ???
  def x9c: NamedTuple[("first", "last", "age", "middle"), (1.0, "Miller", 33, true)] = x9

  val person = NamedTuple[Names, Values](values)
  val release = NamedTuple[Releases, ReleaseValues](releaseValues)

  val x10 = person.updateWith(release)
  val _: UpdateWith[NamedTuple[Names, Values], NamedTuple[Releases, ReleaseValues]] = x10
  val _: ("first", "last", "age", "middle") = x10.names
  val _: (1.0, "Miller", 33, true) = x10.toTuple
  assert((("first", "last", "age", "middle") == x10.names))
  assert((1.0, "Miller", 33, true) == x10.toTuple)

  val x11 = release.updateWith(person)
  val _: UpdateWith[NamedTuple[Releases, ReleaseValues], NamedTuple[Names, Values]] = x11
  val _: NamedTuple[("first", "middle", "last", "age"), ("Bob", true, "Miller", 33)] = x11
  assert(("first", "middle", "last", "age") == x11.names)
  assert(("Bob", true, "Miller", 33) == x11.toTuple)
