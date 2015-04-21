class A(val f : String)

class B(a : Option[String], f : String) extends A(f) {
  a match {
    case Some(`f`) => print(f)
  }
}
