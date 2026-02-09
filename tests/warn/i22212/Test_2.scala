object Test:
  def main(args: Array[String]): Unit =
    Macro.makeMatch() // warn: match may not be exhaustive.
