abstract class A()(implicit val x: Int = 3)
trait B extends A

abstract class C()(using y: Int = 9)
class D extends C()