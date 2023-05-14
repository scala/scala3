import reflect.Generic // error: Generic is not a member of reflect

sealed trait A derives Generic // error: cannot take shape, it has anonymous or inaccessible subclasses

object A {
  def f() = {
    println(new A {})
    println(new A {})
  }
}

sealed trait B derives Generic // error: cannot take shape, its subclass class D is not a case class

class D(x: Int, y: String) extends B

class E derives Generic // error: cannot take shape, it is neither sealed nor a case class

sealed trait F derives Generic // error: cannot take shape, it has anonymous or inaccessible subclasses

object G {
  def f() = {
    case class H() extends F
  }
}

