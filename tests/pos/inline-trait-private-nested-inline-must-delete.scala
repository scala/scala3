inline trait A[T: Numeric]:
    private val num = summon[Numeric[T]]
    private val x = 1

class B extends A[Float]
