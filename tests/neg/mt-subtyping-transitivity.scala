final class A
final class B

type MT[X] = X match
  case A => String
  case B => Int

def test: MT[A | B] = ??? : MT[A] // error
// testing that
//    MT[A]  !<: MT[A | B]
// otherwise
//    String <: MT[A] <: MT[A | B]
// but
//    String !<: MT[A | B]
