// object A:
  val x: Int = 1
  inline def myMacro(): x.type = ${myMacroExpr}
  def test = myMacro()

@main def main() = ()