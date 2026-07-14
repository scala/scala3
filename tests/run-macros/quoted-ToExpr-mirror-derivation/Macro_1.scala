import scala.quoted.*

// Plain product, primitives
case class Point(x: Int, y: Int) derives ToExprFactory

// Nested products, cross-object
case class Address(city: String, zip: Int) derives ToExprFactory
object Namespace:
  case class Person(name: String, address: Address) derives ToExprFactory

// Singleton (case object)
case object Singleton derives ToExprFactory

// Generic product (needs an explicit Type/instance bound)
case class Box[A](value: A) derives ToExprFactory

// Product, mixed field kinds
case class Mixed(name: String, flag: Boolean, ratio: Double) derives ToExprFactory

// Field type (List[Int]) whose ToExpr needs Quotes ambient, not just Type/instance
case class Tagged(tags: List[Int])
given taggedToExpr(using Quotes): ToExpr[Tagged] = ToExprFactory.derived[Tagged].apply()

// Sum: sealed trait + cases, auto-derived
sealed trait Shape derives ToExprFactory
object Shape:
  case class Circle(r: Double) extends Shape
  case class Rect(w: Double, h: Double) extends Shape
  case object Origin extends Shape

// Sum: enum, singleton + parameterized cases
enum Color derives ToExprFactory:
  case Red, Green
  case Custom(hex: String)

// Product with a sum-typed field
case class Container(name: String, shape: Shape) derives ToExprFactory

// Zero-arg product (not a singleton)
case class Blank() derives ToExprFactory

// Generic sum (explicit bound on the trait; cases auto-derived)
sealed trait Result[+A] derives ToExprFactory
object Result:
  case class Ok[A](value: A) extends Result[A]
  case object Fail extends Result[Nothing]

// Non-case singleton, lifted via valueOf[T] (no `derives`)
object Marker

object Macro:
  inline def liftPoint: Point = ${ Macro.liftPointImpl }
  inline def liftPerson: Namespace.Person = ${ Macro.liftPersonImpl }
  inline def liftSingleton: Singleton.type = ${ Macro.liftSingletonImpl }
  inline def liftBoxInt: Box[Int] = ${ Macro.liftBoxIntImpl }
  inline def liftBoxString: Box[String] = ${ Macro.liftBoxStringImpl }
  inline def liftBoxBox: Box[Box[Int]] = ${ Macro.liftBoxBoxImpl }
  inline def liftMixed: Mixed = ${ Macro.liftMixedImpl }
  inline def liftTagged: Tagged = ${ Macro.liftTaggedImpl }
  inline def liftCircle: Shape.Circle = ${ Macro.liftCircleImpl }
  inline def liftShapeCircle: Shape = ${ Macro.liftShapeCircleImpl }
  inline def liftShapeRect: Shape = ${ Macro.liftShapeRectImpl }
  inline def liftShapeOrigin: Shape = ${ Macro.liftShapeOriginImpl }
  inline def liftColorRed: Color = ${ Macro.liftColorRedImpl }
  inline def liftColorGreen: Color = ${ Macro.liftColorGreenImpl }
  inline def liftColorCustom: Color = ${ Macro.liftColorCustomImpl }
  inline def liftMarker: Marker.type = ${ Macro.liftMarkerImpl }
  inline def liftContainer: Container = ${ Macro.liftContainerImpl }
  inline def liftBlank: Blank = ${ Macro.liftBlankImpl }
  inline def liftResultOk: Result[Int] = ${ Macro.liftResultOkImpl }
  inline def liftResultFail: Result[Int] = ${ Macro.liftResultFailImpl }

  // Regression guard: derived code stays a plain `mirror.fromProduct(tuple)` call
  inline def showPointTree: String = ${ Macro.showPointTreeImpl }

  def liftPointImpl(using Quotes): Expr[Point] = Expr(Point(1, 2))
  def liftPersonImpl(using Quotes): Expr[Namespace.Person] = Expr(Namespace.Person("nyc", Address("nyc", 10001)))
  def liftSingletonImpl(using Quotes): Expr[Singleton.type] = Expr(Singleton)
  def liftBoxIntImpl(using Quotes): Expr[Box[Int]] = Expr(Box(1))
  def liftBoxStringImpl(using Quotes): Expr[Box[String]] = Expr(Box("s"))
  def liftBoxBoxImpl(using Quotes): Expr[Box[Box[Int]]] = Expr(Box(Box(1)))
  def liftMixedImpl(using Quotes): Expr[Mixed] = Expr(Mixed("m", true, 1.5))
  def liftTaggedImpl(using Quotes): Expr[Tagged] = Expr(Tagged(List(1, 2, 3)))
  def liftCircleImpl(using Quotes): Expr[Shape.Circle] =
    // `Circle` has no standalone top-level `ToExpr` (auto-derived via `Shape`), so derive locally
    given ToExpr[Shape.Circle] = ToExprFactory.derived[Shape.Circle].apply()
    Expr(Shape.Circle(1.0))
  def liftShapeCircleImpl(using Quotes): Expr[Shape] = Expr[Shape](Shape.Circle(2.0))
  def liftShapeRectImpl(using Quotes): Expr[Shape] = Expr[Shape](Shape.Rect(1.0, 2.0))
  def liftShapeOriginImpl(using Quotes): Expr[Shape] = Expr[Shape](Shape.Origin)
  def liftColorRedImpl(using Quotes): Expr[Color] = Expr[Color](Color.Red)
  def liftColorGreenImpl(using Quotes): Expr[Color] = Expr[Color](Color.Green)
  def liftColorCustomImpl(using Quotes): Expr[Color] = Expr[Color](Color.Custom("#fff"))
  def liftMarkerImpl(using Quotes): Expr[Marker.type] = Expr(Marker)
  def liftContainerImpl(using Quotes): Expr[Container] = Expr(Container("c", Shape.Circle(3.0)))
  def liftBlankImpl(using Quotes): Expr[Blank] = Expr(Blank())
  def liftResultOkImpl(using Quotes): Expr[Result[Int]] = Expr[Result[Int]](Result.Ok(5))
  def liftResultFailImpl(using Quotes): Expr[Result[Int]] = Expr[Result[Int]](Result.Fail)
  def showPointTreeImpl(using Quotes): Expr[String] = Expr(Expr(Point(1, 2)).show)
