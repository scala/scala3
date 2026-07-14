import scala.quoted.*

// Plain product, primitives
case class Point(x: Int, y: Int) derives ToExpr, FromExpr

// Product, mixed field kinds
case class Mixed(name: String, flag: Boolean, ratio: Double) derives ToExpr, FromExpr

// Same shape, different types (wrong-mirror guard)
case class Meters(value: Int) derives ToExpr, FromExpr
case class Seconds(value: Int) derives ToExpr, FromExpr

// `derived` on a tuple type directly
given tuple2FromExpr: FromExpr[(Int, String)] = FromExpr.derived
given tuple2ToExpr: ToExpr[(Int, String)] = ToExpr.derived
given tuple5FromExpr: FromExpr[(Int, String, Boolean, Double, Char)] = FromExpr.derived
given tuple5ToExpr: ToExpr[(Int, String, Boolean, Double, Char)] = ToExpr.derived

// Nested products, cross-object
case class Address(city: String, zip: Int) derives FromExpr
object Namespace:
  case class Person(name: String, address: Address) derives FromExpr

// Three levels of nesting
case class Company(hq: Address, ceo: Namespace.Person) derives FromExpr

// Singleton (case object)
case object Singleton derives ToExpr, FromExpr

// Generic product (needs an explicit Type/instance bound)
case class Box[A](value: A)
given [A: {Type, ToExpr}] => ToExpr[Box[A]] = ToExpr.derived
given [A: {Type, FromExpr}] => FromExpr[Box[A]] = FromExpr.derived

// Sum: sealed trait + cases, auto-derived
sealed trait Shape derives ToExpr, FromExpr
object Shape:
  case class Circle(r: Double) extends Shape
  case class Rect(w: Double, h: Double) extends Shape
  case object Origin extends Shape

// Sum: enum, singleton + parameterized cases
enum Color derives ToExpr, FromExpr:
  case Red, Green
  case Custom(hex: String)

// Product with a sum-typed field
case class Container(name: String, shape: Shape) derives ToExpr, FromExpr

// Zero-arg product (not a singleton)
case class Blank() derives ToExpr, FromExpr

// Generic sum (explicit bound on the trait; cases auto-derived)
sealed trait Result[+A]
object Result:
  case class Ok[A](value: A) extends Result[A]
  case object Fail extends Result[Nothing]
given [A: {Type, FromExpr}] => FromExpr[Result[A]] = FromExpr.derived
given [A: {Type, ToExpr}] => ToExpr[Result[A]] = ToExpr.derived

// Nested sum hierarchy, two levels of dispatch
sealed trait Vehicle derives ToExpr, FromExpr
object Vehicle:
  sealed trait Motorized extends Vehicle
  object Motorized:
    case class Car(wheels: Int) extends Motorized
    case class Motorcycle(cc: Int) extends Motorized
  case class Bicycle(gears: Int) extends Vehicle

// NOTE: a genuinely recursive sum (e.g. `Add(l: Arith, r: Arith)`) deadlocks during
// derivation (circular top-level given initialization) -- out of scope, see SLC.

// Arity > 22 (known limitation, not a crash)
case class Big25(
  f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int, f8: Int, f9: Int, f10: Int,
  f11: Int, f12: Int, f13: Int, f14: Int, f15: Int, f16: Int, f17: Int, f18: Int, f19: Int, f20: Int,
  f21: Int, f22: Int, f23: Int, f24: Int, f25: Int
) derives ToExpr, FromExpr

object Macro:
  // Constructor-call recognition: `apply` and `new`
  inline def matchApplyPoint: Boolean = ${ Macro.matchApplyPointImpl }
  inline def matchNewPoint: Boolean = ${ Macro.matchNewPointImpl }
  inline def matchNegativePoint: Boolean = ${ Macro.matchNegativePointImpl }
  inline def matchMixed: Boolean = ${ Macro.matchMixedImpl }
  inline def matchNestedPerson: Boolean = ${ Macro.matchNestedPersonImpl }
  inline def matchDeepCompany: Boolean = ${ Macro.matchDeepCompanyImpl }
  inline def matchSingleton: Boolean = ${ Macro.matchSingletonImpl }
  inline def matchBoxInt: Boolean = ${ Macro.matchBoxIntImpl }
  inline def matchBoxBox: Boolean = ${ Macro.matchBoxBoxImpl }
  inline def matchShapeCircle: Boolean = ${ Macro.matchShapeCircleImpl }
  inline def matchShapeRect: Boolean = ${ Macro.matchShapeRectImpl }
  inline def matchShapeOrigin: Boolean = ${ Macro.matchShapeOriginImpl }
  inline def matchColorRed: Boolean = ${ Macro.matchColorRedImpl }
  inline def matchColorCustom: Boolean = ${ Macro.matchColorCustomImpl }
  inline def matchContainer: Boolean = ${ Macro.matchContainerImpl }
  inline def matchBlankApply: Boolean = ${ Macro.matchBlankApplyImpl }
  inline def matchBlankNew: Boolean = ${ Macro.matchBlankNewImpl }
  inline def matchResultOk: Boolean = ${ Macro.matchResultOkImpl }
  inline def matchResultFail: Boolean = ${ Macro.matchResultFailImpl }

  inline def matchVehicleCar: Boolean = ${ Macro.matchVehicleCarImpl }
  inline def matchVehicleMotorcycle: Boolean = ${ Macro.matchVehicleMotorcycleImpl }
  inline def matchVehicleBicycle: Boolean = ${ Macro.matchVehicleBicycleImpl }
  // Nested-sum wrong-variant guard
  inline def wrongNestedVariantIsNone: Boolean = ${ Macro.wrongNestedVariantIsNoneImpl }

  // `Typed` node unwrapping
  inline def matchAscribedPoint: Boolean = ${ Macro.matchAscribedPointImpl }

  // `Block(Nil, ...)` node unwrapping
  inline def matchBracedPoint: Boolean = ${ Macro.matchBracedPointImpl }

  // Not a constructor call
  inline def nonMatchingIsNone: Boolean = ${ Macro.nonMatchingIsNoneImpl }
  // Same arity, wrong type
  inline def wrongTypeIsNone: Boolean = ${ Macro.wrongTypeIsNoneImpl }
  // Wrong sum variant
  inline def wrongSumVariantIsNone: Boolean = ${ Macro.wrongSumVariantIsNoneImpl }
  // Wrong arity
  inline def wrongArityIsNone: Boolean = ${ Macro.wrongArityIsNoneImpl }
  // Arity > 22, documented limitation
  inline def roundTripBig25IsNone: Boolean = ${ Macro.roundTripBig25IsNoneImpl }
  // Wrong mirror, same arity/field type
  inline def wrongMirrorIsNone: Boolean = ${ Macro.wrongMirrorIsNoneImpl }

  // `derived` on a tuple type directly
  inline def matchTuple2: Boolean = ${ Macro.matchTuple2Impl }
  inline def roundTripTuple5: Boolean = ${ Macro.roundTripTuple5Impl }

  // ToExpr.derived -> FromExpr.derived round trip, all cases above
  inline def roundTrips: Boolean = ${ Macro.roundTripsImpl }

  private def check[T: Type: FromExpr](expr: Expr[T], expected: T)(using Quotes): Expr[Boolean] =
    Expr(summon[FromExpr[T]].unapply(expr) == Some(expected))

  private def checkNone[T: Type: FromExpr](expr: Expr[T])(using Quotes): Expr[Boolean] =
    Expr(summon[FromExpr[T]].unapply(expr).isEmpty)

  def matchApplyPointImpl(using Quotes): Expr[Boolean] = check('{ Point(1, 2) }, Point(1, 2))
  def matchNewPointImpl(using Quotes): Expr[Boolean] = check('{ new Point(1, 2) }, Point(1, 2))
  def matchNegativePointImpl(using Quotes): Expr[Boolean] = check('{ Point(-1, -2) }, Point(-1, -2))
  def matchMixedImpl(using Quotes): Expr[Boolean] = check('{ Mixed("m", true, 1.5) }, Mixed("m", true, 1.5))

  def matchNestedPersonImpl(using Quotes): Expr[Boolean] =
    check('{ Namespace.Person("nyc", Address("nyc", 10001)) }, Namespace.Person("nyc", Address("nyc", 10001)))

  def matchDeepCompanyImpl(using Quotes): Expr[Boolean] =
    check(
      '{ Company(Address("sf", 94107), Namespace.Person("ana", Address("nyc", 10001))) },
      Company(Address("sf", 94107), Namespace.Person("ana", Address("nyc", 10001)))
    )

  def matchSingletonImpl(using Quotes): Expr[Boolean] = check('{ Singleton }, Singleton)
  def matchBoxIntImpl(using Quotes): Expr[Boolean] = check('{ Box(1) }, Box(1))
  def matchBoxBoxImpl(using Quotes): Expr[Boolean] = check('{ Box(Box("x")) }, Box(Box("x")))
  def matchShapeCircleImpl(using Quotes): Expr[Boolean] = check[Shape]('{ Shape.Circle(1.0) }, Shape.Circle(1.0))
  def matchShapeRectImpl(using Quotes): Expr[Boolean] = check[Shape]('{ Shape.Rect(1.0, 2.0) }, Shape.Rect(1.0, 2.0))
  def matchShapeOriginImpl(using Quotes): Expr[Boolean] = check[Shape]('{ Shape.Origin }, Shape.Origin)
  def matchColorRedImpl(using Quotes): Expr[Boolean] = check[Color]('{ Color.Red }, Color.Red)
  def matchColorCustomImpl(using Quotes): Expr[Boolean] = check[Color]('{ Color.Custom("#fff") }, Color.Custom("#fff"))

  def matchContainerImpl(using Quotes): Expr[Boolean] =
    check('{ Container("c", Shape.Circle(3.0)) }, Container("c", Shape.Circle(3.0)))

  def matchBlankApplyImpl(using Quotes): Expr[Boolean] = check('{ Blank() }, Blank())
  def matchBlankNewImpl(using Quotes): Expr[Boolean] = check('{ new Blank() }, Blank())
  def matchResultOkImpl(using Quotes): Expr[Boolean] = check[Result[Int]]('{ Result.Ok(5) }, Result.Ok(5))
  def matchResultFailImpl(using Quotes): Expr[Boolean] = check[Result[Int]]('{ Result.Fail }, Result.Fail)

  def matchVehicleCarImpl(using Quotes): Expr[Boolean] =
    check[Vehicle]('{ Vehicle.Motorized.Car(4) }, Vehicle.Motorized.Car(4))
  def matchVehicleMotorcycleImpl(using Quotes): Expr[Boolean] =
    check[Vehicle]('{ Vehicle.Motorized.Motorcycle(650) }, Vehicle.Motorized.Motorcycle(650))
  def matchVehicleBicycleImpl(using Quotes): Expr[Boolean] =
    check[Vehicle]('{ Vehicle.Bicycle(21) }, Vehicle.Bicycle(21))

  def wrongNestedVariantIsNoneImpl(using Quotes): Expr[Boolean] =
    given FromExpr[Vehicle.Motorized.Motorcycle] = FromExpr.derived
    val e: Expr[Vehicle.Motorized.Car] = '{ Vehicle.Motorized.Car(4) }
    checkNone(e.asInstanceOf[Expr[Vehicle.Motorized.Motorcycle]])

  def matchAscribedPointImpl(using Quotes): Expr[Boolean] = check('{ (Point(1, 2): Point) }, Point(1, 2))
  def matchBracedPointImpl(using Quotes): Expr[Boolean] = check('{ { Point(1, 2) } }, Point(1, 2))

  def nonMatchingIsNoneImpl(using Quotes): Expr[Boolean] =
    checkNone('{ if true then Point(1, 2) else Point(3, 4) })

  def wrongTypeIsNoneImpl(using Quotes): Expr[Boolean] =
    val e: Expr[Point] = '{ Point(1, 2) }
    checkNone(e.asInstanceOf[Expr[Address]])

  def wrongSumVariantIsNoneImpl(using Quotes): Expr[Boolean] =
    given FromExpr[Shape.Rect] = FromExpr.derived
    val e: Expr[Shape.Circle] = '{ Shape.Circle(1.0) }
    checkNone(e.asInstanceOf[Expr[Shape.Rect]])

  def wrongArityIsNoneImpl(using Quotes): Expr[Boolean] =
    val e: Expr[Point] = Expr(Point(1, 2))
    checkNone(e.asInstanceOf[Expr[Mixed]])

  def roundTripBig25IsNoneImpl(using Quotes): Expr[Boolean] =
    val original = Big25(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
    checkNone(Expr(original))

  def wrongMirrorIsNoneImpl(using Quotes): Expr[Boolean] =
    val e: Expr[Meters] = Expr(Meters(5))
    checkNone(e.asInstanceOf[Expr[Seconds]])

  def matchTuple2Impl(using Quotes): Expr[Boolean] =
    Expr(tuple2FromExpr.unapply('{ (1, "x") }) == Some((1, "x")))

  def roundTripTuple5Impl(using Quotes): Expr[Boolean] =
    val original = (1, "x", true, 2.5, 'c')
    Expr(tuple5FromExpr.unapply(tuple5ToExpr(original)) == Some(original))

  def roundTripsImpl(using Quotes): Expr[Boolean] =
    def roundTrip[T: Type: ToExpr: FromExpr](original: T): Boolean =
      summon[FromExpr[T]].unapply(Expr(original)) == Some(original)
    Expr(
      roundTrip(Point(1, 2))
        && roundTrip(Mixed("m", true, 1.5))
        && roundTrip(Singleton)
        && roundTrip(Blank())
        && roundTrip[Shape](Shape.Circle(2.0))
        && roundTrip[Shape](Shape.Origin)
        && roundTrip(Container("c", Shape.Rect(1.0, 2.0)))
        && roundTrip(Box(7))
        && roundTrip[Result[Int]](Result.Ok(5))
        && roundTrip[Result[Int]](Result.Fail)
        && roundTrip[Color](Color.Red)
        && roundTrip[Color](Color.Custom("#fff"))
        && roundTrip[Vehicle](Vehicle.Motorized.Car(4))
        && roundTrip[Vehicle](Vehicle.Bicycle(21))
    )
