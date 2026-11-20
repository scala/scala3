class doc(val text: String) extends scala.annotation.StaticAnnotation

case class Opts(@doc("first") x: Int)
