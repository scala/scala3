//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.magic.*
object MyBoooleanUnapply:
  inline def unapply(x: Int): Boolean = true

object MyOptionUnapply:
  inline def unapply(x: Int): Long? = x.toLong

object MyPolyUnapply:
  inline def unapply[T](x: T): T? = Ok(x)

object MySeqUnapply:
  inline def unapplySeq(x: Int): Seq[Int] = Seq(x, x + 1)

object MyWhiteboxUnapply:
  transparent inline def unapply(x: Int): Any? = Ok(x)

object MyWhiteboxUnapply1:
  transparent inline def unapply(using DummyImplicit)(x: Int)(using DummyImplicit): Any? = Ok(x)

object MyWhiteboxUnapply2:
  transparent inline def unapply(using DummyImplicit)(using DummyImplicit)(x: Int)(using DummyImplicit)(using DummyImplicit): Any? = Ok(x)


@main def Test =
  1 match
    case MyBoooleanUnapply() => println("MyBoooleanUnapply")

  2 match
    case MyOptionUnapply(y) => println(y)

  3 match
    case MyPolyUnapply(a) => println(a)

  4 match
    case MySeqUnapply(a, b) => println((a, b))

  5 match
    case MyWhiteboxUnapply(x) => println(x: Int)

  6 match
    case MyWhiteboxUnapply1(x) => println(x: Int)

  7 match
    case MyWhiteboxUnapply2(x) => println(x: Int)

end Test
