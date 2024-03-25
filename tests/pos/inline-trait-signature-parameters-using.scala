inline trait A(using private[A] val usng: Int)

given x: Int = 1
class B extends A
