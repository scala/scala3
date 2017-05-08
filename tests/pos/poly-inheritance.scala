// A strawman collection design study in a nutshell
import reflect.ClassTag
import Predef.???

class It[+A] extends Ops[A, It, It[A]]

trait Ops[+A, +CC[_], +C] extends Any {
  def filter(f: A => Boolean): C = ???
  def map[B](f: A => B): CC[B] = ???
  def toSeq: It[A] = ???
}

class ArraySeq[A] extends It[A] with Ops[A, ArraySeq, ArraySeq[A]]


class LP {
  implicit def arrayToSeq[A](xs: Array[A]): ArraySeq[A] = ???
}
object Test extends LP {

  implicit class ArrOps[A](val xs: Array[A]) extends AnyVal with Ops[A, ArraySeq, Array[A]] {
    def map[B: ClassTag](f: A => B): Array[B] = ???
    override def toSeq: ArraySeq[A] = arrayToSeq(xs)
  }

  val xs1 = Array(1, 2, 3)
  val xs2 = xs1.filter(_ % 2 == 0)
  val xs2a: Array[Int] = xs2
  val xs3 = xs1.map(_ + "!")
  val xs3a: Array[String] = xs3

  def f[T](f: Int => T) = {
    val xs1 = Array(1, 2, 3)
    val xs2 = xs1.filter(_ % 2 == 0)
    val xs2a: Array[Int] = xs2
    val xs3 = xs1.toSeq.map(f)
    val xs3a: ArraySeq[T] = xs3
  }
}





