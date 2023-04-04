inline trait A(using usng: Int) // error

given x: Int = 1
class B extends A()