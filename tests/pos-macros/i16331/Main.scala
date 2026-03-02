object Main:
  inline def inlineDef[A](): Any = Macro.macroDef()
  def main(args: Array[String]) = inlineDef()
