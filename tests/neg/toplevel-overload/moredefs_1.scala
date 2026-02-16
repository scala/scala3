trait B

def f(x: B) = s"B" // error: has already been compiled

private def g(): Unit = () // error: has already been compiled (def is visible in package)