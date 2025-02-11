def id[T](x: T): T = x

class ann(x: Int) extends annotation.Annotation

@ann(id(22)) @main def blop = ()
