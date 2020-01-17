object Test {
  type M[X] = X match {
    case Int => String
    case Double => Int
  }

  def m_OK[X](x: X): M[X] = x match {
    case _: Int    => ""
    case _: Double => 1
  }

  def m_error0[X](x: X): M[X] = x match {
    case Some(i: Int) => "" // error
    case _: Double => 1     // error
  }

  def m_error1[X](x: X): M[X] = x match {
    case s: String => "" // error
    case _: Double => 1  // error
  }

  def m_error2[X](x: X): M[X] = x match {
    case _: Double => 1  // error
    case _: Int    => "" // error
  }

  def m_error3[X](x: X): M[X] = x match {
    case _: Int => "" // error
  }

  def m_error4[X](x: X): M[X] = x match {
    case _: Int    => ""   // error
    case _: Double => 1    // error
    case _: String => true // error
  }

  def m_error5[X](x: X): M[X] = x match {
    case _: Int if true => "" // error
    case _: Double      => 1  // error
  }

  case class Blah[A, B](a: A, b: B)

  type LeafElem[X] = X match {
    case Array[t]   => t
    case Blah[a, b] => a
  }

  def leafElem_ok[X](x: X): LeafElem[X] = x match {
    case y: Array[t]   => y.apply(0)
    case y: Blah[a, b] => y.a
  }

  def leafElem_error[X](x: X): LeafElem[X] = x match {
    case y: Array[t]   => 0   // error
    case y: Blah[a, b] => y.b // error
  }
}
