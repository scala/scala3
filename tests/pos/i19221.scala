class T1

class P1
final class P2
class P3

class E1
class E2 extends E1
class E3 extends E1

object VarExt:
  def unapplySeq(t1: T1): U1 = new U1

class U1 extends Product1[P1]:
  def canEqual(that: Any): Boolean = ???

  val _1: P1      = new P1
  val _2: P2      = new P2
  val _3: Seq[P3] = Seq(new P3)

  def length: Int           = ???
  def apply(i: Int): E1     = ???
  def drop(n: Int): Seq[E2] = ???
  def toSeq: Seq[E3]        = ???

class Test:
  def m1(t1: T1): Unit = t1 match
    case VarExt(c1, cs*) => // CCE: class P1 cannot be cast to class E1
      val e1: E1       = c1
      val e1s: Seq[E1] = cs

object Main:
  def main(args: Array[String]): Unit =
    new Test().m1(new T1)

