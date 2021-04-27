package tests

package enumSignatures

enum Enum1
{
  case A
  case B
  case C
}
enum Enum2(val i: Int):
  case A(s: String) extends Enum2/*<-*/(1)/*->*/
  case B(t: String) extends Enum2/*<-*/(2)/*->*/
  case C(u: String) extends Enum2/*<-*/(3)/*->*/

enum Enum3(val param: Int):
  case A extends Enum3/*<-*/(1)/*->*/ with A
  case B extends Enum3/*<-*/(2)/*->*/
  case C extends Enum3/*<-*/(3)/*->*/

enum Enum4[+T]:
  case G(s: String)
  case B extends Enum4[Int] with A
  case C[V](s: String) extends Enum4[V]
  case D[T](s: String) extends Enum4[T]

trait A