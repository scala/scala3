trait E[T]

@annotation.implicitAmbiguous("cba".reverse) // error
given E[Int] = ???

def f(using @annotation.implicitNotFound("cba".reverse) e: E[Int]): Unit = () // error
