trait Type
case class TermParamRef(binder: Type, index: Int) extends Type
case class TypeRef(prefix: Type, name: String) extends Type

class LambdaType extends Type { thisLambdaType =>
  def foo(tp: Type): Unit = tp match {
    case TermParamRef(`thisLambdaType`, _) =>
    case tp: TypeRef =>
    case _ =>
  }
}