//> using options -Wunused:patvars

sealed trait Calc
sealed trait Const extends Calc
case class Sum(a: Calc, b: Calc) extends Calc
case class S(pred: Const) extends Const
case object Z extends Const

val a = Sum(S(S(Z)),Z) match {
  case Sum(x,Z) => Z // warn
  // case Sum(a @ _,Z) => Z // todo : this should pass in the future
  case Sum(x@S(_),Z) => Z // warn
  case Sum(x@S(_),Z) => x // warn unreachable
  case Sum(x@S(y@S(_)), Z) => x // warn
  case Sum(x@S(y@S(_)), Z) => x // warn
  case Sum(x@S(y@(S(_))), Z) => Sum(x,y) // warn unreachable
  case Sum(_,_) => Z // OK
  case _ => Z // warn unreachable
}

case class K(i: Int, j: Int)

class C(c0: Option[Int], k0: K):
  private val Some(c) = c0: @unchecked  // warn valdef from pattern
  private val K(i, j) = k0              // nowarn (name of case class element is nowarn)
  val K(v, w) = k0                      // nowarn nonprivate
  private val K(r, s) = k0              // warn // warn valdefs from pattern
  def f(x: Option[Int]) = x match
    case Some(y) => true                // warn Bind in pattern
    case _       => false
  def g(ns: List[Int]) =
    for x <- ns do println()            // warn valdef function param from for
  def g1(ns: List[Int]) =
    for x <- ns do println(x)           // x => println(x)
  def h(ns: List[Int]) =
    for x <- ns; y = x + 1              // warn tupling from for; x is used, y is unused
    do println()
  def k(x: Option[K]) =
    x match
    case Some(K(i, j)) =>               // nowarn canonical names
    case _ =>

  val m = Map(
    "first" -> Map((true, 1), (false, 2), (true, 3)),
    "second" -> Map((true, 1), (false, 2), (true, 3)),
  )
  def guardedUse =
    m.map: (a, m1) =>
      for (status, lag) <- m1 if status
      yield (a, status, lag)
  def guardedUseOnly =
    m.map: (a, m1) =>
      for (status, lag) <- m1 if status
      yield (a, lag)
  def guardedUseMissing =
    m.map: (a, m1) =>
      for (status, lag) <- m1           // warn
      yield (a, lag)
  def flatGuardedUse =
    for (a, m1) <- m; (status, lag) <- m1 if status
    yield (a, status, lag)
  def leading =
    for _ <- List("42"); i = 1; _ <- List("0", "27")(i)
    yield ()
  def optional =
    for case Some(x) <- List(Option(42))
    yield x
  def nonoptional =
    for case Some(x) <- List(Option(42))  // warn
    yield 27
  def optionalName =
    for case Some(value) <- List(Option(42))
    yield 27

class Wild:
  def f(x: Any) =
    x match
    case _: Option[?] => true
    case _ => false

def untuple(t: Tuple) =
  t match
  case Tuple() =>
  case h *: t => // no warn canonical names taken from tuple element types, (Head, Tail) -> (head, tail)
  //case head *: tail => // no warn canonical names taken from tuple element types, (Head, Tail) -> (head, tail)

// empty case class:
// def equals(other) = other match { case other => true } // exonerated name
object i15967:
  sealed trait A[-Z]
  final case class B[Y]() extends A[Y]

object `patvar is assignable`:
  var (i, j) = (42, 27) // no warn nonprivate
  j += 1
  println((i, j))

object `privy patvar is assignable`:
  private var (i, j) = (42, 27) // warn
  j += 1
  println((i, j))

object `local patvar is assignable`:
  def f() =
    var (i, j) = (42, 27) // warn
    j += 1
    println((i, j))

object `mutable patvar in for`:
  def f(xs: List[Int]) =
    for x <- xs; y = x + 1 if y > 10 yield
      var z :: Nil = y :: Nil: @unchecked // warn
      z + 10

class `unset var requires -Wunused`:
  private var i = 0 // no warn as we didn't ask for it
  def f = println(i)

class `i22743 lazy vals are defs`:
  def f: (Int, String) = (42, "hello, world")
  lazy val (i, s) = f // no warn because def is neither local nor private
  val (j, t) = f // existing no warn for val with attachment
  private lazy val (k, u) = f // warn // warn a warning so nice, they warn it twice

def `i25100 type vars are also pat vars` =
  ??? match { case _: List[t] => ??? }
