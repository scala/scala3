//> using options -source future
// !!! Needs to be compiled currently with -Ycheck:all since that avoids problem
// with illegal opaque types in match types (issue #19434).

import language.experimental.modularity
import language.experimental.namedTuples

/* This is a demonstrator that shows how to map regular for expressions to
 * internal data that can be optimized by a query engine. It needs NamedTuples
 * and type classes but no macros. It's so far very provisional and experimental,
 * intended as a basis for further exploration.
 */

/** The type of expressions in the query language */
trait Expr extends Selectable:

  /** The type of the expression's result.
   *  Note: This needs to be a type member if we want to have complex
   *  constructors that introduce dependencies on value parameters in Result.
   *  An example of such a constructor is the commented out version of Select below.
   */
  type Result

  /** This type is used to support selection with any of the field names
   *  defined by Fields.
   */
  type Fields = NamedTuple.Map[NamedTuple.From[Result], Expr.Of]

  /** A selection of a field name defined by Fields is implemented by `selectDynamic`.
   *  The implementation will add a cast to the right Expr type corresponding
   *  to the field type.
   */
  def selectDynamic(fieldName: String) = Expr.Select(this, fieldName)

  /** Member methods to implement universal equality on Expr level. */
  def == (other: Expr): Expr.Of[Boolean] = Expr.Eq(this, other)
  def != (other: Expr): Expr.Of[Boolean] = Expr.Ne(this, other)

object Expr:

  /** Convenience alias to regain something close to parameterized Expr types */
  type Of[A] = Expr { type Result = A }

  /** Sample extension methods for individual types */
  extension (x: Expr.Of[Int])
    def > (y: Expr.Of[Int]): Expr.Of[Boolean] = Gt(x, y)
    def > (y: Int): Expr.Of[Boolean] = Gt(x, IntLit(y))
  extension (x: Expr.Of[Boolean])
    def &&(y: Expr.Of[Boolean]): Expr.Of[Boolean] = And(x, y)
    def || (y: Expr.Of[Boolean]): Expr.Of[Boolean] = Or(x, y)

  // Note: All field names of constructors in the query language are prefixed with `$`
  // so that we don't accidentally pick a field name of a constructor class where we want
  // a name of the domain model instead.

  // Some sample constructors for Exprs
  case class Gt($x: Expr.Of[Int], $y: Expr.Of[Int]) extends Expr.Of[Boolean]
  case class Plus(x: Expr.Of[Int], y: Expr.Of[Int]) extends Expr.Of[Int]
  case class And($x: Expr.Of[Boolean], $y: Expr.Of[Boolean]) extends Expr.Of[Boolean]
  case class Or($x: Expr.Of[Boolean], $y: Expr.Of[Boolean]) extends Expr.Of[Boolean]

  // So far Select is weakly typed, so `selectDynamic` is easy to implement.
  // Todo: Make it strongly typed like the other cases, along the lines
  // of the commented out version below.
  case class Select[A]($x: Expr.Of[A], $name: String) extends Expr

  // Also weakly typed in the arguents since these two classes model universal equality */
  case class Eq($x: Expr, $y: Expr) extends Expr.Of[Boolean]
  case class Ne($x: Expr, $y: Expr) extends Expr.Of[Boolean]

  /** References are placeholders for parameters */
  private var refCount = 0

  case class Ref[A]($name: String = "") extends Expr.Of[A]:
    val id = refCount
    refCount += 1
    override def toString = s"ref$id(${$name})"

  /** Literals are type-specific, tailored to the types that the DB supports */
  case class IntLit($value: Int) extends Expr.Of[Int]

  /** Scala values can be lifted into literals by conversions */
  given Conversion[Int, IntLit] = IntLit(_)

  /** The internal representation of a function `A => B`
   *  Query languages are ususally first-order, so Fun is not an Expr
   */
  case class Fun[A, B](param: Ref[A], f: B)

  type Pred[A] = Fun[A, Expr.Of[Boolean]]
end Expr

/** The type of database queries. So far, we have queries
 *  that represent whole DB tables and queries that reify
 *  for-expressions as data.
 */
trait Query[A]

object Query:
  import Expr.{Pred, Fun, Ref}

  case class Filter[A]($q: Query[A], $p: Pred[A]) extends Query[A]
  case class Map[A, B]($q: Query[A], $f: Fun[A, Expr.Of[B]]) extends Query[B]
  case class FlatMap[A, B]($q: Query[A], $f: Fun[A, Query[B]]) extends Query[B]

  // Extension methods to support for-expression syntax for queries
  extension [R](x: Query[R])

    def withFilter(p: Ref[R] => Expr.Of[Boolean]): Query[R] =
      val ref = Ref[R]()
      Filter(x, Fun(ref, p(ref)))

    def map[B](f: Ref[R] => Expr.Of[B]): Query[B] =
      val ref = Ref[R]()
      Map(x, Fun(ref, f(ref)))

    def flatMap[B](f: Ref[R] => Query[B]): Query[B] =
      val ref = Ref[R]()
      FlatMap(x, Fun(ref, f(ref)))
end Query

/** The type of query references to database tables */
case class Table[R]($name: String) extends Query[R]

// Everything below is code using the model -----------------------------

// Some sample types
case class City(zipCode: Int, name: String, population: Int)
type Address = (city: City, street: String, number: Int)
type Person = (name: String, age: Int, addr: Address)

@main def Test =

  val cities: Query[City] = Table[City]("city")
  val q1 = cities.map: c =>
    c.zipCode
  val q2 = cities.withFilter: city =>
      city.population > 10_000
    .map: city =>
      city.name

  val q3 =
    for
      city <- cities
      if city.population > 10_000
    yield city.name

  val q4 =
    for
      city <- cities
      alt <- cities
      if city.name == alt.name && city.zipCode != alt.zipCode
    yield
      city

  println(q1)
  println(q2)
  println(q3)
  println(q4)

/* The following is not needed currently

/** A type class for types that can map to a database table */
trait Row:
  type Self
  type Fields = NamedTuple.From[Self]
  type FieldExprs = NamedTuple.Map[Fields, Expr.Of]

  //def toFields(x: Self): Fields = ???
  //def fromFields(x: Fields): Self = ???

*/