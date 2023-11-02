//> using options -Werror
object PartialFunctionNoWarning {
  // nice warning
  "abc" match {
    case "abc" =>
    case "abc" => // warn
  }
  
  // no warnings
  val pf: PartialFunction[String, Unit] = {
    case "abc" =>
    case "abc" => // warn
  }
}
// nopos-error: No warnings can be incurred under -Werror.