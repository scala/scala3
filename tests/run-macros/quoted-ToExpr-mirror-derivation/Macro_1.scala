import scala.quoted.*

// Product derivation: plain case class with primitive fields.
case class Point(x: Int, y: Int) derives ToExpr

// Product derivation: nested case classes (field itself has a derived ToExpr), and the
// case class itself is nested inside another object's companion.
case class Address(city: String, zip: Int) derives ToExpr
object Namespace:
  case class Person(name: String, address: Address) derives ToExpr

// Product derivation: case object (singleton), reconstructed via `Mirror.fromProduct`
// applied to `EmptyTuple`.
case object Singleton derives ToExpr

// Product derivation: generic case class. `derives` only adds a `ToExpr[A]` bound to
// the generated given, not `Type[A]`, so a plain `derives ToExpr` clause on a generic
// class doesn't compile (the derived code needs to mention `A` inside a quote). Adding
// an explicit given with both bounds works.
case class Box[A](value: A)
given boxToExpr[A: Type: ToExpr]: ToExpr[Box[A]] = ToExpr.derived

// Product derivation: case class with a variety of primitive field kinds.
case class Mixed(name: String, flag: Boolean, ratio: Double) derives ToExpr

// Product derivation with a field type (`List[Int]`) whose `ToExpr` instance itself
// needs a `Type[Int]` (via `ListToExpr[T: Type: ToExpr]`), which can only be summoned
// where a `Quotes` is already ambient -- unlike the generic case above, adding a plain
// `Type`/`ToExpr` bound to the given doesn't help here (there's no type parameter to
// bind it to), so the given needs to be conditional on `Quotes` directly.
case class Tagged(tags: List[Int])
given taggedToExpr(using Quotes): ToExpr[Tagged] = ToExpr.derived

// Sum derivation: sealed hierarchy of case classes and case objects, nested inside the
// sealed trait's own companion object. Only the sealed trait itself needs `derives` -- its
// own cases are derived automatically since none of them already has an instance in scope;
// the sum's derived instance just dispatches by ordinal to whichever case it picks up.
sealed trait Shape derives ToExpr
object Shape:
  case class Circle(r: Double) extends Shape
  case class Rect(w: Double, h: Double) extends Shape
  case object Origin extends Shape

// Sum derivation over an enum with a mix of singleton and parameterized cases. Only the
// enum itself needs `derives` -- its cases are derived automatically, the same as `Shape`.
enum Color derives ToExpr:
  case Red, Green
  case Custom(hex: String)

// Product derivation with a sum-typed field: composes `derivedProduct` (for `Container`)
// and `derivedSum` (for its `shape` field) within a single derivation.
case class Container(name: String, shape: Shape) derives ToExpr

// Product derivation: zero-arg case class. Distinct from a singleton (`Singleton` above):
// this still goes through the constructor-call machinery, `fromProduct` is just applied
// to an already-empty tuple because the class happens to have no fields.
case class Blank() derives ToExpr

// Sum derivation over a generic sealed trait: combines the generics-need-an-explicit-given
// constraint (see `Box` above) with sum dispatch (see `Shape` above). The sealed trait
// itself still needs the explicit `Type[A]`/instance-bound given, but its cases (`Ok`,
// `Fail`) no longer need one of their own -- `A`'s own bound is already in scope from the
// outer given.
sealed trait Result[+A]
object Result:
  case class Ok[A](value: A) extends Result[A]
  case object Fail extends Result[Nothing]

given resultToExpr[A: Type: ToExpr]: ToExpr[Result[A]] = ToExpr.derived

// ConstToExpr: an arbitrary (non-case) object singleton type, lifted directly via
// `valueOf[T]`, found by ordinary implicit search (no `derives` involved at all).
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

  // Regression guard: the generated code for a derived product must stay a plain
  // `mirror.fromProduct(tuple)` call -- not silently regress into something that stages
  // reflection (`getClass`, `Symbol.requiredClass`, ...) into the generated tree itself.
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
    // `Circle` no longer has its own top-level `ToExpr` (it's derived automatically as part
    // of `Shape`'s own dispatch, not exposed standalone), so derive one locally to lift it
    // directly (as opposed to `liftShapeCircleImpl` below, which goes through `Shape`).
    given ToExpr[Shape.Circle] = ToExpr.derived
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
