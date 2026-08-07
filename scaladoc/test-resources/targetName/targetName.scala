def m(x: Int)(y: Int): Int = x + y

@scala.annotation.targetName("m2")
def m(x: Int, y: Int): Int = x - y

/**
 * Refers to [[m]] and [[m2]]
 */
def user(): Unit = ()