//> using options -Yno-predef -Yimports:java.lang,scala.annotation,scala.util.matching
class annotation extends Annotation
val s: String = "str"
val regex: Regex = s.r // error
