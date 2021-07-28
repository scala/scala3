@main def Test = assert(scala.compiletime.codeOf(1+2) == "1.+(2)")
