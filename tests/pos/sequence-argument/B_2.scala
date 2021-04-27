import scala.reflect.ClassTag
import scala.language.implicitConversions

object B {
  def doubleSeq[T](x: T): Seq[T] = Seq(x, x)
  def doubleArray[T: ClassTag](x: T): Array[T] = Array(x, x)

  def box(args: Integer*): Unit = {}
  def widen(args: Long*): Unit = {}
  def conv(args: Y*): Unit = {}

  box(doubleSeq(1)*)
  box(doubleArray(1)*)
  Java_2.box(doubleSeq(1)*)
  Java_2.box(doubleArray(1)*)

  widen(doubleSeq(1)*)
  widen(doubleArray(1)*)
  Java_2.widen(doubleSeq(1)*)
  Java_2.widen(doubleArray(1)*)

  implicit def xToY(x: X): Y = new Y
  val x: X = new X
  conv(doubleSeq(x)*)
  conv(doubleArray(x)*)
  Java_2.conv(doubleSeq(x)*)
  Java_2.conv(doubleArray(x)*)
}
