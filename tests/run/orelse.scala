// scalajs: --skip
// scalajs needs to be diasabled since `null` gives an Err(undefined) instead of an Err(())
// TODO: Figure out how to fix this under scalajs
//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*
import scala.util.Either

class C:
  def toEitherAny[T](x: Any): Either[Any, Any] = x match
    case Ok(y) => Right(y)
    case Err(e) => Left(e)

def toEitherIntStr(x: Int ? String): Either[String, Int] = x match
  case Ok(y) => Right(y)
  case Err(e) => Left(e)

def toEither[T, E](x: T ? E): Either[E, T] = x match
  case Err(e) => Left(e)
  case Ok(y) => Right(y)

def toEitherBooleanStr[T](x: T ? String): Either[String, Boolean] = x match
  case Ok(x: Boolean) => Right(x)
  case Err(e) => Left(e)
  case _ => Left("not a boolean")

extension [T, E](x: T ? E)
  def withErr[E1](e1: => E1): T ? E1 = x match
    case Ok(y) => Ok(y)
    case Err(e) => Err(e1)

object Pos:
  def unapply(x: Int): Int? =
    if x >= 0 then x else null

object WithTail:
  def unapply(s: String): String? =
    if s.isEmpty then null else s.substring(1)

object Poly:
  def unapply[T](x: T): T ? String = x match
    case Pos(y: T @unchecked) => Ok(y)
    case WithTail(s: T @unchecked) => Ok(s)
    case _ => Err("no match")

def f[T, E](x: T, e: E) =
  val x1 = toEither(Ok("s"))
  val x2 = toEither(null)
  val x3 = toEither("s")
  val x4 = toEither(Err("bad"))

  val y1 = toEitherIntStr(Ok(1))
  val y3 = toEitherIntStr(Ok(22))
  val y4 = toEitherIntStr(Err("bad"))

  val z1 = toEither(Ok(x))
  val z2 = toEither(Err(e))

  val a1 = toEitherBooleanStr(Ok(true))
  val a2 = toEitherBooleanStr(Ok(1))
  val a3 = toEitherBooleanStr(Err("bad"))

  println("==== x")
  println(x1)
  println(x2)
  println(x3)
  println(x4)
  println("==== y")
  println(y1)
  println(y3)
  println(y4)
  println("==== z")
  println(z1)
  println(z2)
  println("==== a")
  println(a1)
  println(a2)
  println(a3)

def posTest(x: Int) = x match
  case Pos(y) => println(s"pos $x $y")
  case _ => println(s"neg $x")

def strTest(x: String) = x match
  case WithTail(y) => println(s"nonempty $x $y")
  case _ => println(s"empty $x")

def polyTest(s: String, n: Int) =
  s match
    case Poly(s1) =>
      val _: String = s1
      println(s"poly $s = $s1")
    case _ =>
      println(s"nopoly $s")
  n match
    case Poly(n1) =>
      val _: Int = n1
      println(s"poly $n = $n1")
    case _ =>
      println(s"nopoly $n")
  true match
    case Poly(x) =>
      assert(false)
    case _ =>

def polyTest2[T](x: T) = x match
  case Poly(x1) =>
    println(s"poly2 $x = $x1")
  case _ =>
    println(s"nopoly2 $x")

@main def Test =
  f("ss", "BAD")
  println("==== pos")
  posTest(1)
  posTest(-1)
  println("==== str")
  strTest("abc")
  strTest("")
  println("==== poly")
  polyTest("abc", 22)
  polyTest("", -1)
  println("==== poly2")
  polyTest2("abc")
  polyTest2(22)
  polyTest2("")
  polyTest2(-1)
  polyTest2(true)






