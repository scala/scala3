import Package.DependentCaseClass2

def test: Unit =
  val d: Package.DependentCaseClass2 = DependentCaseClass2()
  Macro.myMacro()
