//> using options -Yexplicit-nulls
import annotation.targetName

class A
class B(x: A)

object Test:
  def convert(x: A): B = B(x)
  @targetName("inlineConvert")
  inline def convert(x: A | Null): B | Null =
    if x == null then null else convert(x)

  val x = convert(A())
  val an: A | Null = A()
  val y = convert(null)
  val z = convert(an)

