//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*

class C:
  def toOptionAny[T](x: Any): Option[Any] = x match
    case Ok(y) => Some(y)
    case null => None

def toOptionStr(x: String?): Option[String] = x match
  case Ok(y) => Some(y)
  case null => None

def toOption[T](x: T?): Option[T] = x match
  case Ok(y) => Some(y)
  case null => None

def toOptionAny[T](x: Any): Option[Any] = x match
  case Ok(y) => Some(y)
  case null => None

object Pos:
  def unapply(x: Int): Int? =
    if x >= 0 then x else null

object WithTail:
  def unapply(s: String): String? =
    if s.isEmpty then null else s.substring(1)

object Poly:
  def unapply[T](x: T): T? = x match
    case Pos(y) => y
    case WithTail(s) => s
    case _ => null

def f[T](x: T) =
  val x1 = toOption(Ok("s"))
  val x2 = toOption(null)
  val x3 = toOption("s")

  val y1 = toOptionStr(Ok("s"))
  val y2 = toOptionStr(null)

  val z1 = toOption(Ok(x))
  println(x1)
  println(x2)
  println(x3)
  println(y1)
  println(y2)
  println(z1)

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
  f("ss")
  posTest(1)
  posTest(-1)
  strTest("abc")
  strTest("")
  polyTest("abc", 22)
  polyTest("", -1)
  polyTest2("abc")
  polyTest2(22)
  polyTest2("")
  polyTest2(-1)
  polyTest2(true)






