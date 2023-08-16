//> using options -Werror
sealed trait T
class C extends T

class AClass
type AType = AClass {
  type TypeMember <: T
}

def list2Product(
  atype: AType,
  opt: atype.TypeMember
): Unit =
  opt match {
    case _: C => ()
  }
