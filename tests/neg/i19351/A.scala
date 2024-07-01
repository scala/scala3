//object A:
  val x: Int = 1
  inline def myMacro(): x.type = ${myMacroExpr}  // error
  def test = myMacro()

