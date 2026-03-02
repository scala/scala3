
trait UsingTypeTest[B](using reflect.TypeTest[Int, B]):

  type M[U <: Int] = U match
    case B => String

  def m(t: Int): M[Int] = t match
    case _: B => "hello"