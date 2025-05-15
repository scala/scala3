import language.experimental.namedTuples

@main def Test =
  val namedTple = Macro.macr()
  println(namedTple.intValue)
  println(namedTple.stringValue)
