object subtype_specialization {

  class Seq[+A]

  case class FirstName[T](name: String) extends Seq[Char] {}

  def foo[@specialized(Char) A](stuff: Seq[A]): Seq[A] = {
    stuff
  }

  val s: Seq[FirstName] = foo[FirstName](new Seq[FirstName])

}
