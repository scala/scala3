import scala.quoted.*

// Product derivation: plain case class with primitive fields. Also derives `ToExpr`, so
// this doubles as the round-trip test type below.
case class Point(x: Int, y: Int) derives ToExpr, FromExpr

// Product derivation: case class with a variety of primitive field kinds. Also derives
// `ToExpr`, to exercise the round-trip test at an arity (3) beyond the smallest cases.
case class Mixed(name: String, flag: Boolean, ratio: Double) derives ToExpr, FromExpr

// Same shape (one Int field) as each other, but nominally distinct types: proves the
// `fromProduct` recognition branch checks the receiver is specifically `Mirror.ProductOf[T]`
// rather than accepting any same-arity `fromProduct` call (which would let a `Meters` tree
// be silently misread as a `Seconds` value, since field-level extraction alone can't catch
// the mismatch when the field types happen to coincide too).
case class Meters(value: Int) derives ToExpr, FromExpr
case class Seconds(value: Int) derives ToExpr, FromExpr

// `Mirror.ProductOf` exists for tuple types too, so `derived` works when `T` itself is a
// tuple -- not just when a tuple shows up as the argument to some other type's
// `fromProduct` call. Named explicitly (rather than via `derives`, which isn't valid syntax
// on a type alias) to avoid shadowing/ambiguity with the library's own hand-written
// `Tuple2FromExpr`/`Tuple5FromExpr` givens when summoned generically elsewhere.
given tuple2FromExpr: FromExpr[(Int, String)] = FromExpr.derived
given tuple2ToExpr: ToExpr[(Int, String)] = ToExpr.derived
given tuple5FromExpr: FromExpr[(Int, String, Boolean, Double, Char)] = FromExpr.derived
given tuple5ToExpr: ToExpr[(Int, String, Boolean, Double, Char)] = ToExpr.derived

// Product derivation: nested case classes, one nested inside another object's companion.
case class Address(city: String, zip: Int) derives FromExpr
object Namespace:
  case class Person(name: String, address: Address) derives FromExpr

// Three levels of nesting, to make sure recursion isn't hardcoded to depth 1-2.
case class Company(hq: Address, ceo: Namespace.Person) derives FromExpr

// Product derivation: case object (singleton). Matches a bare reference to the module,
// not a call. Also derives `ToExpr`, for the round-trip test below.
case object Singleton derives ToExpr, FromExpr

// Product derivation: generic case class. Just like `ToExpr`, `derives` only adds a
// `FromExpr[A]` bound (not `Type[A]`), so a generic class needs an explicit given. Also
// given a `ToExpr`, for the round-trip test below.
case class Box[A](value: A)
given boxFromExpr[A: Type: FromExpr]: FromExpr[Box[A]] = FromExpr.derived
given boxToExpr[A: Type: ToExpr]: ToExpr[Box[A]] = ToExpr.derived

// Sum derivation: sealed hierarchy of case classes and case objects, nested inside the
// sealed trait's own companion object. Each case needs its own instance; the sum type's
// own instance just tries each case's `unapply` in turn.
// Also derives `ToExpr` on the sum type and its variants, for the round-trip test below.
sealed trait Shape derives ToExpr, FromExpr
object Shape:
  case class Circle(r: Double) extends Shape derives ToExpr, FromExpr
  case class Rect(w: Double, h: Double) extends Shape derives ToExpr, FromExpr
  case object Origin extends Shape derives ToExpr, FromExpr

// Sum derivation over an enum with a mix of singleton and parameterized cases (mirrors
// the equivalent test in the ToExpr suite). Each case still needs its own instance. Also
// derives `ToExpr` (on the enum and each case), for the round-trip test below -- an enum
// singleton's `Type[T]` is `TermRef`-encoded (see the fix in `FromExpr.derivedProduct`),
// distinct from a real case object's `TypeRef`-encoded module class.
enum Color derives ToExpr, FromExpr:
  case Red, Green
  case Custom(hex: String)

given ToExpr[Color.Red.type] = ToExpr.derived
given ToExpr[Color.Green.type] = ToExpr.derived
given ToExpr[Color.Custom] = ToExpr.derived
given FromExpr[Color.Red.type] = FromExpr.derived
given FromExpr[Color.Green.type] = FromExpr.derived
given FromExpr[Color.Custom] = FromExpr.derived

// Product derivation with a sum-typed field: composes `derivedProduct` (for `Container`)
// and `derivedSum` (for its `shape` field) within a single derivation. Also derives
// `ToExpr`, so the round-trip test below exercises this composition recursively (the
// outer `fromProduct`-tuple contains an inner field that is itself either another
// `fromProduct` call, for `Circle`/`Rect`, or a bare singleton reference, for `Origin`).
case class Container(name: String, shape: Shape) derives ToExpr, FromExpr

// Product derivation: zero-arg case class. Distinct from a singleton (`Singleton` above):
// this is matched via the `Apply(fun, Nil)` branch (constructor-call recognition), not the
// bare-module-reference branch. Also derives `ToExpr`: `ToExpr.derived` produces
// `mirror.fromProduct(Tuple())` for `Blank` just as it does for a real singleton, so the
// round-trip test below confirms this correctly takes the tuple-unwrap branch rather than
// being mistaken for a singleton reference.
case class Blank() derives ToExpr, FromExpr

// Sum derivation over a generic sealed trait (mirrors the equivalent test in the ToExpr
// suite): combines the generics-need-an-explicit-given constraint (see `Box` above) with
// sum dispatch (see `Shape` above). Also given `ToExpr`s, for the round-trip test below.
sealed trait Result[+A]
object Result:
  case class Ok[A](value: A) extends Result[A]
  case object Fail extends Result[Nothing]

given resultOkFromExpr[A: Type: FromExpr]: FromExpr[Result.Ok[A]] = FromExpr.derived
given FromExpr[Result.Fail.type] = FromExpr.derived
given resultFromExpr[A: Type: FromExpr]: FromExpr[Result[A]] = FromExpr.derived
given resultOkToExpr[A: Type: ToExpr]: ToExpr[Result.Ok[A]] = ToExpr.derived
given ToExpr[Result.Fail.type] = ToExpr.derived
given resultToExpr[A: Type: ToExpr]: ToExpr[Result[A]] = ToExpr.derived

// A case class whose arity (25) exceeds `Tuple22`: `Expr.ofTupleFromSeq` produces a
// `Tuple.fromIArray(...).asInstanceOf[...]` shape for these, which `FromExpr.derived`'s
// tuple-unwrap branch does not recognize (`TupleN.apply` matching only covers arity 0-22).
// This is a known, deliberate limitation, not a crash -- pinned down by a dedicated test
// below rather than left undocumented.
case class Big25(
  f1: Int, f2: Int, f3: Int, f4: Int, f5: Int, f6: Int, f7: Int, f8: Int, f9: Int, f10: Int,
  f11: Int, f12: Int, f13: Int, f14: Int, f15: Int, f16: Int, f17: Int, f18: Int, f19: Int, f20: Int,
  f21: Int, f22: Int, f23: Int, f24: Int, f25: Int
) derives ToExpr, FromExpr

object Macro:
  // Matches "calls to `new X` or `X.apply`" (as `FromExpr`'s own contract puts it) --
  // both spellings of a case class construction should be recognized.
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

  // A tree wrapped in a `Typed` node (as produced by a type ascription) must be
  // unwrapped before the underlying constructor call is recognized.
  inline def matchAscribedPoint: Boolean = ${ Macro.matchAscribedPointImpl }

  // A tree wrapped in a `Block(Nil, ...)` node (as produced by explicit braces around a
  // value, real user-reachable syntax -- not merely a defensive-only case) must likewise
  // be unwrapped.
  inline def matchBracedPoint: Boolean = ${ Macro.matchBracedPointImpl }

  // Not a direct constructor call: no `FromExpr` should extract a value from this.
  inline def nonMatchingIsNone: Boolean = ${ Macro.nonMatchingIsNoneImpl }
  // Same field count as `Point`, but a different type: arity alone must not be enough.
  inline def wrongTypeIsNone: Boolean = ${ Macro.wrongTypeIsNoneImpl }
  // A `Circle` tree read back as a `Rect`: same sum type, wrong variant.
  inline def wrongSumVariantIsNone: Boolean = ${ Macro.wrongSumVariantIsNoneImpl }
  // A 2-element tuple (from a `Point`'s `fromProduct` call) read back as a `Mixed`
  // (arity 3): the tuple-unwrap branch must reject the arity mismatch, not crash.
  inline def wrongArityIsNone: Boolean = ${ Macro.wrongArityIsNoneImpl }
  // `Big25`'s `ToExpr.derived` output uses the `Tuple.fromIArray(...)` shape (arity > 22),
  // which the tuple-unwrap branch doesn't recognize -- a documented limitation, not a
  // crash.
  inline def roundTripBig25IsNone: Boolean = ${ Macro.roundTripBig25IsNoneImpl }
  // A `Meters`-shaped `fromProduct` tree read back as `Seconds`: same arity, same field
  // type, so only the receiver-mirror check (not field-level extraction) can catch this.
  inline def wrongMirrorIsNone: Boolean = ${ Macro.wrongMirrorIsNoneImpl }

  // `derived` applied directly to a tuple type: matches a literal tuple-literal quote
  // (arity 2), and round-trips through `ToExpr.derived` at a higher arity (5).
  inline def matchTuple2: Boolean = ${ Macro.matchTuple2Impl }
  inline def roundTripTuple5: Boolean = ${ Macro.roundTripTuple5Impl }

  // Round trip: a value lifted to code via `ToExpr.derived` must be readable back via
  // `FromExpr.derived` on the same type. `ToExpr.derived` always produces a
  // `mirror.fromProduct(tuple)`-shaped tree (never a `T(args*)`/`new T(args*)` call), so
  // this exercises the dedicated recognition branch for that shape: a product at several
  // arities, a singleton (both a real case object and a `TermRef`-encoded enum case), a
  // zero-arg non-singleton, a sum type, a generic type, and a product with a sum-typed
  // field (composing the recognition recursively).
  inline def roundTrips: Boolean = ${ Macro.roundTripsImpl }

  // Shared by (almost) every scenario above: extract `expr` via `T`'s `FromExpr` and
  // compare against the value it's expected to produce (or, for `checkNone`, expect
  // extraction to fail). Factored out since the comparison itself is always the same --
  // only the quoted syntax and type under test vary per scenario.
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
  def matchAscribedPointImpl(using Quotes): Expr[Boolean] = check('{ (Point(1, 2): Point) }, Point(1, 2))
  def matchBracedPointImpl(using Quotes): Expr[Boolean] = check('{ { Point(1, 2) } }, Point(1, 2))

  def nonMatchingIsNoneImpl(using Quotes): Expr[Boolean] =
    checkNone('{ if true then Point(1, 2) else Point(3, 4) })

  def wrongTypeIsNoneImpl(using Quotes): Expr[Boolean] =
    val e: Expr[Point] = '{ Point(1, 2) }
    checkNone(e.asInstanceOf[Expr[Address]])

  def wrongSumVariantIsNoneImpl(using Quotes): Expr[Boolean] =
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
    )
