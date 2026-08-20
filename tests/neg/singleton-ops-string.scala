import scala.compiletime.ops.string.*

object Test {
  val t0: "Hello " + "world" = "Hello world"
  val t1: "" + "" = ""
  val t2: "3" + "" = "33" // error
  val t3: "Hello " + "world" = "error" // error

  val t4: Length["Hello"] = 5
  val t5: Length[""] = 0
  val t6: Length["1"] = 7 // error

  val t7: Substring["hamburger", 4, 8] = "urge"
  val t8: Substring["hamburger", 4, 8] = "urger" // error

  val t9: Matches["hamburger", "ham.*"] = true
  val t10: Matches["hamburger", "ham.*"] = false // error

  val t11: CharAt["String", 0] = 'S'
  val t12: CharAt["String", 1] = 't'
  val t13: CharAt["String", 2] = '!' // error
  //                             ^^^
  //                             Found:    ('!' : Char)
  //                             Required: ('r' : Char)
  val t14: CharAt["String", 3] = '!' // error
  //                             ^^^
  //                             Found:    ('!' : Char)
  //                             Required: ('i' : Char)
  val t15: CharAt["String", 4] = 'n'
  val t16: CharAt["String", 5] = 'g'
  val t17: CharAt["String", 6]  = '!' // error
  //       ^
  //       String index out of range: 6
  val t18: CharAt["String", -1] = '?' // error
  //       ^
  //       String index out of range: -1

  val t19: "hello" < "world" = true
  val t20: "hello" < "world" = false // error

  val t21: "hello" > "world" = false
  val t22: "hello" > "world" = true // error

  val t23: "hello" <= "world" = true
  val t24: "hello" <= "world" = false // error
  val t25: "hello" <= "hello" = true
  val t26: "hello" <= "hello" = false // error

  val t27: "hello" >= "world" = false
  val t28: "hello" >= "world" = true // error
  val t29: "hello" >= "hello" = true
  val t30: "hello" >= "hello" = false // error

  import scala.compiletime.ops.int./

  type If[C <: Boolean, Then, Else] = C match
    case true  => Then
    case false => Else

  type ZipTuples[First, Second] = (First, Second) match
    case (first *: EmptyTuple, second *: EmptyTuple) => (first, second) *: EmptyTuple
    case (first *: firstRest, second *: secondRest) => (first, second) *: ZipTuples[firstRest, secondRest]

  type TuplePairs[InputTuple] = InputTuple match
    case NamedTuple.NamedTuple[names, values] => ZipTuples[names, values]

  type PrependTuplePairs[First, Second, Pairs] = Pairs match
    case EmptyTuple => (First *: EmptyTuple) *: (Second *: EmptyTuple) *: EmptyTuple
    case left *: right *: EmptyTuple => (First *: left) *: (Second *: right) *: EmptyTuple

  type UnzipTuples[Input] = Input match
    case (first *: second *: EmptyTuple) *: rest => PrependTuplePairs[first, second, UnzipTuples[rest]]
    case EmptyTuple => EmptyTuple

  type NamedTupleFromPairs[Input] = UnzipTuples[Input] match
    case EmptyTuple => NamedTuple.NamedTuple[EmptyTuple, EmptyTuple]
    case left *: right *: EmptyTuple => NamedTuple.NamedTuple[left, right]

  type Merge[P1 <: Tuple, P2 <: Tuple] <: Tuple = (P1, P2) match
    case (EmptyTuple, h *: t) => h *: t
    case (h *: t, EmptyTuple) => h *: t
    case ((k1, v1) *: t1, (k2, v2) *: t2) => If[k1 <= k2,
      (k1, v1) *: Merge[t1, (k2, v2) *: t2],
      (k2, v2) *: Merge[(k1, v1) *: t1, t2]
    ]

  type SortPairs[P <: Tuple] <: Tuple = P match
    case EmptyTuple => EmptyTuple
    case h *: EmptyTuple => h *: EmptyTuple
    case _ => Merge[
      SortPairs[Tuple.Take[P, Tuple.Size[P] / 2]],
      SortPairs[Tuple.Drop[P, Tuple.Size[P] / 2]]
    ]

  type SortNamedTuple[NT <: NamedTuple.AnyNamedTuple] = NamedTupleFromPairs[SortPairs[TuplePairs[NT]]]

  val t31: SortNamedTuple[(foo: 45, bar: "x", baz: 6.0, quux: false)] = (bar = "x", baz = 6.0, foo = 45, quux = false)
  val t32: SortNamedTuple[(foo: 45, bar: "x", baz: 6.0, quux: false)] = (foo = 45, bar = "x", baz = 6.0, quux = false) // error
}
