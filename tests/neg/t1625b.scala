object T5 {
  case class Abc(x: String*, c: String*) // error // error: varargs parameter must come last AND found: String* required: String
}
