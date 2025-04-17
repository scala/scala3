//> using options -Yimplicit-as-given
case class ImplicitClauseInCaseClass(dummy: Int)(implicit num: Int)
def Test() = Macro.test[ImplicitClauseInCaseClass]
