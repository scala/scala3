object T5 {
  case class Abc(x: String*, c: String*) // error: varargs parameter must come last
}
