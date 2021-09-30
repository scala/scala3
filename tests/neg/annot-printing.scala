import scala.annotation.*
class Foo() extends Annotation
class Bar(s: String) extends Annotation

def x: Int @nowarn @main @Foo @Bar("hello") = "abc" // error

