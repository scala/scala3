import util.chaining.*

class ann(x: Int = 1, y: Int) extends annotation.Annotation

@ann(y = 22.tap(println)) @main def blop = ()

