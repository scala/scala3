trait B

def f(x: B) = s"B" // error: has already been compiled

private def g(): Unit = () // OK, since it is private