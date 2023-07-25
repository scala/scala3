class ContextClass
type Context = ContextClass^
class ParamRef:
  def isTracked(using Context): Boolean = ???
trait Lam[PR <: ParamRef]:
  val paramRefs: List[PR] = ???
inline def atPhase[T]()(inline op: Context ?=> T)(using ctx: Context): T =
  op(using ctx)

def Test(using ctx: Context) =
  val info: Lam[ParamRef] = ???
  info.paramRefs.filter(_.isTracked)
  val p = atPhase()((_: ParamRef).isTracked)
  val _: ParamRef ->{ctx} Boolean = p
