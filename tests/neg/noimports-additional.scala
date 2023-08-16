//> using options -Yno-imports -Yimports:scala.annotation,scala.util.matching
class annotation extends Annotation
val s: String = "str" // error
val regex: Regex = new Regex("str")
