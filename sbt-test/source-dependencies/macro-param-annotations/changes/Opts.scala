class doc(val text: String) extends scala.annotation.StaticAnnotation

case class Opts(@doc("second") x: Int)
