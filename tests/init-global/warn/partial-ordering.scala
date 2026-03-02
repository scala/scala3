object Names: // warn
  val ctorString = "<init>"
  val ctorName: MethodName = MethodName.apply(ctorString)

class MethodName(encoded: String)
object MethodName:
  val ctor: MethodName = new MethodName(Names.ctorString)
  def apply(name: String): MethodName = new MethodName(name)