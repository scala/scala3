inline trait A(implicit val imp: Int) // error

implicit val x: Int = 1
class B extends A()