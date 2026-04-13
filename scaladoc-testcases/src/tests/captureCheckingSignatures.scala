package tests.captureCheckingSignatures

import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

trait Nested:
  val c: AnyRef^
  val next: Nested

trait Arrows:
  val a: AnyRef^
  val b: AnyRef^
  val c: AnyRef^

  val purev: Int -> Int
  val purev2: Int ->{} Int //expected: val purev2: Int -> Int
  val impurev: Int => Int
  val impurev2: Int ->{a,b,c} Int //expected: val impurev2: Int ->{a, b, c} Int
  val impurev3: Int ->{a,b,c} Int => Int //expected: val impurev3: Int ->{a, b, c} Int => Int
  val impureAny: Int ->{any} Int //expected: val impureAny: Int => Int
  val impureAny2: Int ->{any, a, b, c} Int //expected: val impureAny2: Int ->{any, a, b, c} Int
  val contextPureV: Int ?-> Int
  val contextPureV2: Int ?->{} Int //expected: val contextPureV2: Int ?-> Int
  val contextImpureV: Int ?=> Int
  val contextImpureV2: Int ?->{a,b,c} Int //expected: val contextImpureV2: Int ?->{a, b, c} Int
  val contextImpureV3: Int ?->{a,b,c} Int ?=> Int //expected: val contextImpureV3: Int ?->{a, b, c} Int ?=> Int
  val contextImpureAny: Int ?->{any} Int //expected: val contextImpureAny: Int ?=> Int
  val contextImpureAny2: Int ?->{any, a, b, c} Int //expected: val contextImpureAny2: Int ?->{any, a, b, c} Int

  def pure(f: Int -> Int): Int
  def pure2(f: Int ->{} Int): Int //expected: def pure2(f: Int -> Int): Int
  def impure(f: Int => Int): Int
  def impure2(f: Int ->{a,b,c} Int): Int //expected: def impure2(f: Int ->{a, b, c} Int): Int
  def impure3(f: Int ->{a,b,c} Int => Int): Int //expected: def impure3(f: Int ->{a, b, c} Int => Int): Int

  def consumes(consume a: AnyRef^): Any
  def consumes2(consume x: AnyRef^{a}, consume y: AnyRef^{b}): Any

  def byNamePure(f: -> Int): Int
  def byNameImpure(f: ->{a,b,c} Int): Int //expected: def byNameImpure(f: ->{a, b, c} Int): Int
  def byNameImpure2(f: => Int): Int

  def pathDependent(n: Nested^)(g: AnyRef^{n.c} => Any): Any
  def pathDependent2(n: Nested^)(g: AnyRef^{n.next.c} => Any): Any
  def pathDependent3(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): Any
  def pathDependent4(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): AnyRef^{n.next.next.c}
  def pathDependent5(n: Nested^)(g: AnyRef^{n.c} => AnyRef^{n.next.c} ->{n.c} Any): AnyRef^{n.next.next.c*, n.c, any}

  def contextPure(f: AnyRef^{a} ?-> Int): Int
  def contextImpure(f: AnyRef^{a} ?=> Int): Int
  def contextImpure2(f: AnyRef^{a} ?->{b,c} Int): Int //expected: def contextImpure2(f: AnyRef^{a} ?->{b, c} Int): Int
  def contextImpure3(f: AnyRef^{a} ?->{b,c} Int => AnyRef^{a} ?=> Int): Int //expected: def contextImpure3(f: AnyRef^{a} ?->{b, c} Int => AnyRef^{a} ?=> Int): Int

  val noParams: () -> () -> Int
  val noParams2: () ->{} () ->{} Int //expected: val noParams2: () -> () -> Int
  val noParamsImpure: () => () => Int => Unit

  val uncurried: (x: AnyRef^, y: AnyRef^) -> AnyRef^{x,y} => Int ->{x,y} Int //expected: val uncurried: (x: AnyRef^, y: AnyRef^) -> AnyRef^{x, y} => Int ->{x, y} Int
  val uncurried2: (x: AnyRef^, y: AnyRef^) -> AnyRef => Int ->{x,y} Int //expected: val uncurried2: (x: AnyRef^, y: AnyRef^) -> AnyRef => Int ->{x, y} Int
  val uncurried3: (x: AnyRef^, y: AnyRef^) => AnyRef
  val uncurried4: (x: AnyRef^, y: AnyRef^) ->{a,b} AnyRef^ => Int ->{x,y} Int //expected: val uncurried4: (x: AnyRef^, y: AnyRef^) ->{a, b} AnyRef^ => Int ->{x, y} Int

  val contextUncurried: (x: AnyRef^{a}, y: AnyRef^{b}) ?-> AnyRef^{x,y} ?-> Int ?->{x,y} Int //expected: val contextUncurried: (x: AnyRef^{a}, y: AnyRef^{b}) ?-> AnyRef^{x, y} ?-> Int ?->{x, y} Int
  val contextUncurried2: (x: AnyRef^{a}, y: AnyRef^{b}) ?-> AnyRef ?-> Int ?->{x,y} Int //expected: val contextUncurried2: (x: AnyRef^{a}, y: AnyRef^{b}) ?-> AnyRef ?-> Int ?->{x, y} Int
  val contextUncurried3: (x: AnyRef^{a}, y: AnyRef^{b}) ?=> AnyRef //expected: val contextUncurried3: (AnyRef^{a}, AnyRef^{b}) ?=> AnyRef
  val contextUncurried4: (x: AnyRef^{a}, y: AnyRef^{b}) ?->{a,b} AnyRef^ ?=> Int ?->{x,y} Int //expected: val contextUncurried4: (x: AnyRef^{a}, y: AnyRef^{b}) ?->{a, b} AnyRef^ ?=> Int ?->{x, y} Int

  def polyPure[A](f: A -> Int): Int
  def polyPure2[A](f: A ->{} Int): Int //expected: def polyPure2[A](f: A -> Int): Int
  def polyImpure[A](f: A => Int): Int
  def polyImpure2[A](f: A ->{a,b,c} Int): Int //expected: def polyImpure2[A](f: A ->{a, b, c} Int): Int
  def polyImpure3[A](f: A ->{a,b,c} Int => Int): Int //expected: def polyImpure3[A](f: A ->{a, b, c} Int => Int): Int

  def polyContextPure[A](f: A ?-> Int): Int
  def polyContextPure2[A](f: A ?->{} Int): Int //expected: def polyContextPure2[A](f: A ?-> Int): Int
  def polyContextImpure[A](f: A ?=> Int): Int
  def polyContextImpure2[A](f: A ?->{a,b,c} Int): Int //expected: def polyContextImpure2[A](f: A ?->{a, b, c} Int): Int
  def polyContextImpure3[A](f: A ?->{a,b,c} Int => Int): Int //expected: def polyContextImpure3[A](f: A ?->{a, b, c} Int => Int): Int

  val polyPureV: [A] => A -> Int //expected: val polyPureV: [A] => A => Int
  val polyPureV2: [A] => Int => A ->{a,b,c} Int //expected: val polyPureV2: [A] => Int => A ->{a, b, c} Int
  val polyImpureV: [A] -> A => Int //expected: val polyImpureV: [A] => A => Int
  val polyImpureV2: [A] -> A => Int //expected: val polyImpureV2: [A] => A => Int

trait SelfTypeCaptures[+A]:
  self: SelfTypeCaptures[A]^ =>
  def concat[B >: A](xs: SelfTypeCaptures[B]^): SelfTypeCaptures[B]^{this, xs}

// {this} as sole capture set on non-pure traits
trait ThisCaptureOnly:
  self: ThisCaptureOnly^ =>
  def asRef: AnyRef^{this}
  def withOther(x: AnyRef^): AnyRef^{this, x}

trait MutableThisCapture extends Mutable:
  def asThis: MutableThisCapture^{this}

// --- Mutation tracking ---

import caps.{Mutable, Stateful, Separate, SharedCapability, Classifier}

class Ref[T](init: T) extends Mutable:
  private var x: T = init //unexpected
  def get: T = x //expected: def get: T
  update def set(v: T): Unit = x = v //expected: update def set(v: T): Unit

class MyStateful extends Stateful:
  private var count: Int = 0 //unexpected
  def value: Int = count //expected: def value: Int
  update def incr(): Unit = count += 1 //expected: update def incr(): Unit

class MySeparate(consume val inner: Ref[Int]^) extends Separate

// Read-only captures (.rd)
trait ReadOnlyExamples:
  val r: Ref[Int]^
  def readOnly: Ref[Int]^{any.rd} //expected: def readOnly: Ref[Int]^{any.rd}
  def readRef(x: Ref[Int]^{any.rd}): Int //expected: def readRef(x: Ref[Int]^{any.rd}): Int
  def specificRd: Ref[Int]^{r.rd}

// Consume on methods (not just params)
trait ConsumeMethodExamples extends Mutable:
  consume def sink: Unit
  consume def transfer: ConsumeMethodExamples^

// --- Classifiers ---

class MyIO extends SharedCapability
trait Control extends SharedCapability, Classifier

// .only[Classifier] restricted capabilities
trait ClassifierExamples:
  def restricted(f: () ->{any.only[Control]} Unit): Unit //expected: def restricted(f: () ->{any.only[Control]} Unit): Unit
  def sharedOnly: AnyRef^{any.only[Control]} //expected: def sharedOnly: AnyRef^{any.only[Control]}

// --- Capture set variables and capability members ---

class Box[X^](val value: AnyRef^{X})

trait CaptureSetVarExamples:
  def capSetVar[X^](x: AnyRef^{X}): AnyRef^{X}
  def multiCapSet[X^, Y^](x: AnyRef^{X}, y: AnyRef^{Y}): AnyRef^{X, Y} //expected: def multiCapSet[X^, Y^](x: AnyRef^{X}, y: AnyRef^{Y}): AnyRef^{X, Y}

// Capability members (type Cap^)
// Note: scaladoc strips the this. prefix from path-dependent capture set references.
trait HasCapMember:
  type Cap^

trait HasCapUpperBound:
  val io: AnyRef^
  val log: AnyRef^
  type Cap^ <: {io, log}

trait HasCapLowerBound:
  val io: AnyRef^
  type Cap^ >: {io}

trait HasCapBothBounds:
  val io: AnyRef^
  val log: AnyRef^
  type Cap^ >: {io} <: {io, log}

// Capability member used in method signatures
trait Reactor:
  type Cap^
  def onEvent(h: Event ->{this.Cap} Unit): Unit //expected: def onEvent(h: Event ->{Cap} Unit): Unit
  def getHandler: () ->{this.Cap} Unit //expected: def getHandler: () ->{Cap} Unit

class Event

// Capability member with upper bound used in signatures
trait BoundedReactor:
  val io: AnyRef^
  val log: AnyRef^
  type Cap^ <: {io, log}
  def onEvent(h: Event ->{this.Cap} Unit): Unit //expected: def onEvent(h: Event ->{Cap} Unit): Unit

// Capture-set parameters with bounds
trait CapSetBoundsExamples:
  val a: AnyRef^
  val b: AnyRef^
  def upperBound[X^ <: {a, b}](x: AnyRef^{X}): AnyRef^{X}
  def lowerBound[X^ >: {a}](x: AnyRef^{X}): AnyRef^{X}
  def bothBounds[X^ >: {a} <: {a, b}](x: AnyRef^{X}): AnyRef^{X}

// Multiple bounded capture-set params
trait MultiCapSetBounds:
  val a: AnyRef^
  val b: AnyRef^
  def multi[X^ <: {a}, Y^ <: {b}](x: AnyRef^{X}, y: AnyRef^{Y}): AnyRef^{X, Y} //expected: def multi[X^ <: {a}, Y^ <: {b}](x: AnyRef^{X}, y: AnyRef^{Y}): AnyRef^{X, Y}

// Class with bounded capture-set param
class BoundedBox[X^ <: {any}](val value: AnyRef^{X}) //expected: class BoundedBox[X^](val value: AnyRef^{X})

// --- Fresh capabilities ---

import caps.fresh

trait FreshExamples:
  val a: AnyRef^
  val b: AnyRef^

  // Basic: zero-param pure with fresh result
  def mkRef: () -> Ref[Int]^{fresh}

  // Dependent param, fresh in result (not syntactically dependent on param)
  val mkPair: (x: AnyRef^) -> AnyRef^{fresh}

  // Impure zero-param with fresh result
  val freshImpure: () => Ref[Int]^{fresh}

  // Multi-param dependent with fresh
  val freshMultiParam: (x: AnyRef^, y: AnyRef^) -> AnyRef^{fresh}

  // Fresh combined with param reference in capture set
  val freshAndParam: (x: AnyRef^) -> AnyRef^{x, fresh}

  // Nested function types with fresh at inner level
  val freshNested: (x: AnyRef^) -> () -> AnyRef^{fresh}

  // Context function with fresh
  val freshCtx: (x: AnyRef^) ?-> AnyRef^{fresh}

  // Method taking function-with-fresh as parameter
  def takesFresh(f: () -> Ref[Int]^{fresh}): Ref[Int]^

  // Method returning function type with fresh
  def returnsFresh: (x: AnyRef^) -> AnyRef^{fresh}

  // Fresh with named capture set in arrow
  val freshArrowCapture: (x: AnyRef^) ->{a} AnyRef^{fresh}

  // Fresh in impure (non-dependent) arrow
  val freshImpureNonDep: AnyRef^ => AnyRef^{fresh}

  // By-name with fresh (-> T^{cs} normalizes to ->{cs} T for by-name)
  def byNameFresh(f: ->{fresh} Ref[Int]): Ref[Int]^

  // Polymorphic function with fresh
  val freshPoly: [A] => (x: A) -> Ref[A]^{fresh}
