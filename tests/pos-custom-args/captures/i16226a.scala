class Name
class TermName extends Name
class TypeName extends Name

trait ParamInfo:
  type ThisName <: Name
  def variance: Long
object ParamInfo:
  type Of[N <: Name] = ParamInfo { type ThisName = N }

def test(tparams1: List[ParamInfo{ type ThisName = TypeName }], tparams2: List[ParamInfo.Of[TypeName]]) =
  tparams1.lazyZip(tparams2).map((p1, p2) => p1.variance + p2.variance)

