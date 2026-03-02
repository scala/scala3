
class MyAnnotation(a: Int, b: Int) extends annotation.StaticAnnotation

@MyAnnotation(1, 2) // don't require named arguments as it is Scala Defined
class Test