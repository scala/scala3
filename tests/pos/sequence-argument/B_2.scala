import scala.reflect.ClassTag
import scala.language.implicitConversions

object B {
  def doubleSeq[T](x: T): Seq[T] = Seq(x, x)
  def doubleArray[T: ClassTag](x: T): Array[T] = Array(x, x)

  def box(args: Integer*): Unit = {}
  def widen(args: Long*): Unit = {}
  def conv(args: Y*): Unit = {}

  box(doubleSeq(1): _*)
  box(doubleArray(1): _*)
  Java_2.box(doubleSeq(1): _*)
  Java_2.box(doubleArray(1): _*)

  widen(doubleSeq(1): _*)
  widen(doubleArray(1): _*)
  Java_2.widen(doubleSeq(1): _*)
  Java_2.widen(doubleArray(1): _*)

  implicit def xToY(x: X): Y = new Y
  val x: X = new X
  conv(doubleSeq(x): _*)
  conv(doubleArray(x): _*)
  Java_2.conv(doubleSeq(x): _*)
  Java_2.conv(doubleArray(x): _*)
}
