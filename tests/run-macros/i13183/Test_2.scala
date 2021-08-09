@main def Test =
  println(Macro_1.stringLiteral("\u001b")) // "\u001b"
  println(Macro_1.stringLiteral("\u0000\u0001\u0003")) // "\u0000\u0001\u0003"
  println(Macro_1.stringLiteral("A\u0042C")) // "ABC"
  println(Macro_1.stringLiteral("\u0080\u0081\u7fff")) // "\u0080\u0081翿"
  println(Macro_1.stringLiteral("\t\n\r👋👌🥳")) // "\t\n\r👋👌🥳"
