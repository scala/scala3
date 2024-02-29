//> using options -Werror
object PartialFunctionNoWarning {
  // nice warning
  "abc" match {
    case "abc" =>
    case "abc" => // error
  }
  
  // no warnings
  val pf: PartialFunction[String, Unit] = {
    case "abc" =>
    case "abc" => // error
  }
}