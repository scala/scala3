import language.experimental.modularity
import language.experimental.namedTuples
import NamedTuple.{NamedTuple, AnyNamedTuple}

class Expr extends Selectable:
  type Result
  type Fields = NamedTuple.Map[NamedTuple.From[Result], Expr.Of]

  /** A selection of a field name defined by Fields is implemented by `selectDynamic`.
   *  The implementation will add a cast to the right Expr type corresponding
   *  to the field type.
   */
  def selectDynamic(fieldName: String): Expr = ???


object Expr:
  type Of[A] = Expr { type Result = A }

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

  val cities: Query[City] = ???
  val q6 =
    cities.map: city =>
      //val tup = Expr.Join((name = city.name, zipCode = city.zipCode))
      //val _ : Expr.Of[(name: String, zipCode: Int)] = tup
      //tup
      Expr.Join((name = city.name, zipCode = city.zipCode))


  /*
  val x: StripExpr[Expr.Of[Int]] = ???
  val y: Int = x

  val x1: NamedTupleDecomposition.DropNames[(name : Expr.Of[String], zipCode : Expr.Of[Int])] = ???
  val y1: (Expr.Of[String], Expr.Of[Int]) = x1

  val x2: Tuple.Map[(Expr.Of[String], Expr.Of[Int]), StripExpr] = ???
  val y2: (String, Int) = x2

  var x3: Expr.Of[Tuple.Map[NamedTupleDecomposition.DropNames[(name : Expr.Of[String], zipCode : Expr.Of[Int])], StripExpr]] = ???
  val y3: Expr.Of[(String, Int)] = x3
  x3 = y3

  var x4: Expr.Of[(name : String, zipCode : Int)] = ???
  val y4: Expr.Of[
           NamedTuple.NamedTuple[
             NamedTupleDecomposition.Names[
               (name : Expr.Of[String], zipCode : Expr.Of[Int])],
             Tuple.Map[
               NamedTupleDecomposition.DropNames[
                 (name : Expr.Of[String], zipCode : Expr.Of[Int])],
             Expr.StripExpr]
           ]
         ] = x4
  x4 = y4

  var x4: Expr.Of[(name : String, zipCode : Int)] = ???
  val y4: Expr.Of[
           NamedTuple.NamedTuple[
             NamedTupleDecomposition.Names[
               (name : Expr.Of[String], zipCode : Expr.Of[Int])],
             Tuple.Map[
               NamedTupleDecomposition.DropNames[
                 (name : Expr.Of[String], zipCode : Expr.Of[Int])],
             Expr.StripExpr]
           ]
         ] = x4*/