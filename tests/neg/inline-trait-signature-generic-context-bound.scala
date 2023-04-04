inline trait A[T: List] // error

given List[Int] = Nil
class B extends A