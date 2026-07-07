case class Foo(val value: String) extends Comparable[Integer]:
  override def compareTo(other: Integer) = 0

case class Bar(val value: String) extends Comparable[Bar]:
  override def compareTo(other: Bar) = 0

case class Baz(val s: String, val i: Int)

object Baz:
  def unapply(b: Baz): Rec2 = Rec2(b.i + 1, b.s + "j")

@main def Test =
  val r0 = Rec0()
  r0 match { case Rec0() => println("empty") }

  val r1 = Rec1("hello")
  r1 match { case Rec1(s) => println(s) }

  val r2 = Rec2(3, "ha")
  r2 match { case Rec2(i, s) => println(s * i) }

  // type param (no bounds)
  val r3a = Rec3(3, "he")
  r3a match { case Rec3(i, s) => println(s * i) }
  val r3b = Rec3(3, 7)
  r3b match { case Rec3(i, j) => println(i * j) }

  // type param with simple bounds
  val r4 = Rec4(3, Foo("hi"))
  r4 match { case Rec4(i, f) => println(f.value * i) }

  // type params with recursion / mutual reference
  val r5 = Rec5(3 : Integer, Foo("h"), Bar("o"))
  r5 match { case Rec5(i, f, b) => println((f.value + b.value) * i) }

  // predefined unapply takes precedence
  val r6 = RecUnapply(3, "x")
  r6 match { case RecUnapply(i, s) => println(s * i) }

  // scala class returning record from unapply
  val r7 = Baz("he", 3)
  r7 match { case Baz(i, s) => println(s * i) }
