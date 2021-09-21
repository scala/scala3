import scala.annotation.*

class Foo() extends Annotation
class Bar(s: String) extends Annotation
class Xyz(i: Int = 23) extends Annotation

def x: Int @nowarn @main @Xyz() @Foo @Bar("hello") = ???
