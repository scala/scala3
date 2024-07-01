package tests

package enumSignatures

enum Enum1
{
  case A //expected: case A extends Enum1
  case B //expected: case B extends Enum1
  case C //expected: case C extends Enum1
}
enum Enum2(val i: Int):
  case A(s: String) extends Enum2(1) //expected: final case class A(s: String) extends Enum2
  case B(t: String) extends Enum2(2) //expected: final case class B(t: String) extends Enum2
  case C(u: String) extends Enum2(3) //expected: final case class C(u: String) extends Enum2

enum Enum3(val param: Int):
  case A extends Enum3/*<-*/(1)/*->*/ with A
  case B extends Enum3/*<-*/(2)/*->*/
  case C extends Enum3/*<-*/(3)/*->*/

enum Enum4[+T]:
  case G(s: String) //expected: final case class G[+T](s: String)
  case B extends Enum4[Int] with A
  case C[V](s: String) extends Enum4[V] //expected: final case class C[V](s: String) extends Enum4[V]
  case D[T](s: String) extends Enum4[T] //expected: final case class D[T](s: String) extends Enum4[T]

trait A