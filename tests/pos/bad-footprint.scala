
object NamedTuple:

  opaque type AnyNamedTuple = Any
  opaque type NamedTuple[N <: Tuple, +V <: Tuple] >: V <: AnyNamedTuple = V

  export NamedTupleDecomposition.{Names, DropNames}

  /** The type of the named tuple `X` mapped with the type-level function `F`.
   *  If `X = (n1 : T1, ..., ni : Ti)` then `Map[X, F] = `(n1 : F[T1], ..., ni : F[Ti])`.
   */
  type Map[X <: AnyNamedTuple, F[_ <: Tuple.Union[DropNames[X]]]] =
    NamedTuple[Names[X], Tuple.Map[DropNames[X], F]]

end NamedTuple

object NamedTupleDecomposition:
  import NamedTuple.*

  /** The names of a named tuple, represented as a tuple of literal string values. */
  type Names[X <: AnyNamedTuple] <: Tuple = X match
    case NamedTuple[n, _] => n

  /** The value types of a named tuple represented as a regular tuple. */
  type DropNames[NT <: AnyNamedTuple] <: Tuple = NT match
    case NamedTuple[_, x] => x
end NamedTupleDecomposition

class Expr[Result]

object Expr:
  import NamedTuple.{NamedTuple, AnyNamedTuple}

  type Of[A] = Expr[A]

  type StripExpr[E] = E match
    case Expr.Of[b] => b

  case class Ref[A]($name: String = "") extends Expr.Of[A]

  case class Join[A <: AnyNamedTuple](a: A)
  extends Expr.Of[NamedTuple.Map[A, StripExpr]]
end Expr

trait Query[A]

object Query:
  // Extension methods to support for-expression syntax for queries
  extension [R](x: Query[R])
    def map[B](f: Expr.Ref[R] => Expr.Of[B]): Query[B] = ???

case class City(zipCode: Int, name: String, population: Int)

object Test:
  import Expr.StripExpr
  import NamedTuple.{NamedTuple, AnyNamedTuple}

  val cities: Query[City] = ???
  val q6 =
    cities.map: city =>
      val x: NamedTuple[
        ("name", "zipCode"),
        (Expr.Of[String], Expr.Of[Int])] = ???
      Expr.Join(x)

/* Was error:

-- [E007] Type Mismatch Error: bad-footprint.scala:60:16 -----------------------
60 |    cities.map: city =>
   |                ^
   |Found:    Expr.Ref[City] =>
   |  Expr[
   |    NamedTuple.NamedTuple[(("name" : String), ("zipCode" : String)), (String,
   |      Int)]
   |  ]
   |Required: Expr.Ref[City] =>
   |  Expr[
   |    NamedTuple.NamedTuple[
   |      NamedTupleDecomposition.Names[
   |        NamedTuple.NamedTuple[(("name" : String), ("zipCode" : String)), (
   |          Expr[String], Expr[Int])]
   |      ],
   |      Tuple.Map[
   |        NamedTupleDecomposition.DropNames[
   |          NamedTuple.NamedTuple[(("name" : String), ("zipCode" : String)), (
   |            Expr[String], Expr[Int])]
   |        ],
   |      Expr.StripExpr]
   |    ]
   |  ]
61 |      val x: NamedTuple[
62 |        ("name", "zipCode"),
63 |        (Expr.Of[String], Expr.Of[Int])] = ???
64 |      Expr.Join(x)
   |
   | longer explanation available when compiling with `-explain`
1 error found

*/