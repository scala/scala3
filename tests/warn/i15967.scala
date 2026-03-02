//> using options -Werror
sealed trait A[-Z]
final case class B[Y]() extends A[Y]

class Test:
  def t1[X](a: A[X]) = a match // was inexhaustive
    case _: B[X] @unchecked =>

//def t2[X](a: A[X]) = a match // was inexhaustive
//  case _: B[X] => // expected unchecked warning
