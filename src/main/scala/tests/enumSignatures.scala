package tests

package enumSignatures

enum Enum1
{
    case A
    case B
    case C
}
enum Enum2(val i: Int):
    case A(val s: String) extends Enum2(1)
    case B(val t: String) extends Enum2(2)
    case C(val u: String) extends Enum2(3)

enum Enum3(val param: Int):
    case A extends Enum3(1)
    case B extends Enum3(2)
    case C extends Enum3(3)
